/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::cmp;
use std::error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::{Seek, Read, Write};
use std::mem;
use std::path::{Path, PathBuf};
use std::ptr;
use std::result;

use self::PageTreeNode::*;

#[cfg(not(test))]
const PAGE_SIZE: usize = 1024;  // ish
#[cfg(test)]
const PAGE_SIZE: usize = 16;

// a rather soft limit where it's good time to split a page in two
const MAX_PAGE_SIZE: usize = (PAGE_SIZE as f64 * 1.5) as usize;

/*
 * File contents when stored in memory is paginated for quick modification.
 * Pages are indexed by a balanced binary tree reading left to right. That is,
 * to write any part of the file back to sequent memory, just traverse the
 * appropriate subtree left to right.
 * Pages exist in the leaf nodes of a page tree. Each tree node of a page tree
 * caches the length and number of newlines in its part (left + right) of a
 * possibly larger tree. This allows for quick lookup of any character in a file
 * either by offset or line/column.
 * Length refers to characters, size refers to bytes. Offset refers to
 * characters unless otherwise specified.
 */
enum PageTreeNode {
  Tree(PageTree),
  Leaf(Page),
}

impl PageTreeNode {
  fn length(&self) -> usize {
    match *self {
      Tree(ref tree) => tree.length,
      Leaf(ref page) => page.length,
    }
  }

  fn newlines(&self) -> usize {
    match *self {
      Tree(ref tree) => tree.newlines,
      Leaf(ref page) => page.newline_offsets.len(),
    }
  }

  fn height(&self) -> usize {
    match *self {
      Tree(ref tree) => tree.height,
      Leaf(_)        => 1
    }
  }
}

type PageTreeLink = Option<Box<PageTreeNode>>;

// This trait is a convenience that allows us to query a PageTreeLink for
// certain information whether there's a node there or not.
trait PageTreeLinkFuncs {
  fn length(&self) -> usize;
  fn newlines(&self) -> usize;
  fn height(&self) -> usize;
}

impl PageTreeLinkFuncs for PageTreeLink {
  fn length(&self) -> usize {
    self.as_ref().map(|node| node.length()).unwrap_or(0)
  }

  fn newlines(&self) -> usize {
    self.as_ref().map(|node| node.newlines()).unwrap_or(0)
  }

  fn height(&self) -> usize {
    self.as_ref().map(|node| node.height()).unwrap_or(0)
  }
}

struct PageTree {
  left: PageTreeLink,
  right: PageTreeLink,
  length: usize,  // cached length of pages down the subtrees
  newlines: usize,  // cached numer of newlines in the pages down the subtrees
  height: usize,  // cached height of the tree
}

impl PageTree {
  fn new() -> PageTree {
    PageTree { left: None, right: None, length: 0, newlines: 0, height: 1 }
  }

  fn build(mut stream: PageStream) -> io::Result<PageTree> {
    let mut tree = PageTree::new();
    for page in stream.by_ref() { tree.append_page(page); }
    return stream.error.map_or(Ok(tree), |err| Err(err));
  }

  // See comment for offset_of_line_start and add to that quirk the requirement
  // that |column| == 0.
  fn line_column_to_offset(&self, line: usize, column: usize) -> Option<usize> {
    self.line_start_and_end_offset(line).and_then(
      |(line_start_offset, line_end_offset)| {
        let line_length = line_end_offset - line_start_offset;
        let offset = line_start_offset + column;
        if column == 0 || column < line_length { Some(offset) } else { None }
      })
  }

  // end may equal start if the file is empty
  fn line_start_and_end_offset(&self, line: usize) -> Option<(usize, usize)> {
    self.offset_of_line_start(line).map(
      |line_start_offset| {
        let line_end_offset =
          self.offset_of_line_start(line + 1).unwrap_or(self.length);
        assert!(line_end_offset >= line_start_offset);
        (line_start_offset, line_end_offset)
      })
  }

  // May return an offset one position past the end of the file if either:
  //  * L == 0 and the file is empty, or
  //  * L == N+1 and line N ended with a newline
  // where L is the zero-indexed line number being sought for and N is the
  // number of newlines in the file.
  fn offset_of_line_start(&self, line: usize) -> Option<usize> {
    let (go_left, new_line) = self.decide_branch_by_line(line);
    let (branch, other) = if go_left { (&self.left, &self.right) }
                          else       { (&self.right, &self.left) };
    branch.as_ref().and_then(|boxed_node|
        match **boxed_node {
          Tree(ref tree) => tree.offset_of_line_start(new_line),
          Leaf(ref page) =>
            if new_line == 0 { Some(0) }
            else { page.offset_of_newline(new_line - 1).map(|ofs| ofs + 1) }
        }).
      or(if new_line == 0 { Some(0) } else { None }).
      map(|offset| offset + if go_left { 0 } else { other.length() })
  }

  fn get_char_by_line_column(&self, line: usize, column: usize)
      -> Option<char> {
    self.line_column_to_offset(line, column).and_then(
      |offset| self.get_char_by_offset(offset))
  }

  fn get_char_by_offset(&self, offset: usize) -> Option<char> {
    let (go_left, new_offset) = self.decide_branch_by_offset(offset);
    let branch = if go_left { &self.left } else { &self.right };
    branch.as_ref().and_then(|boxed_node|
        match **boxed_node {
          Tree(ref tree) => tree.get_char_by_offset(new_offset),
          Leaf(ref page) => page.data.chars().nth(new_offset),
        })
  }

  fn insert_string_at_offset(&mut self, string: String, offset: usize) {
    // We may only split a page once, otherwise rebalancing may not be possible
    // while returning. Therefore we must avoid inserting too large strings.
    assert!(string.len() <= PAGE_SIZE);
    assert!(offset <= self.length);
    {
      let (go_left, new_offset) = self.decide_branch_by_offset(offset);
      let (branch, other) = if go_left { (&mut self.left, &mut self.right) }
                            else       { (&mut self.right, &mut self.left) };

      // perform the insertion
      match *branch {
        None                     =>
          *branch = Some(Box::new(Leaf(Page::new(string)))),
        Some(ref mut boxed_node) => match **boxed_node {
          Tree(ref mut tree) =>
            tree.insert_string_at_offset(string, new_offset),
          Leaf(ref mut page) =>
            page.insert_string_at_offset(string, new_offset),
        }
      }

      // decide whether to split a page
      let split_page = match *branch {
        None                 => false,
        Some(ref boxed_node) => match **boxed_node {
          Leaf(ref page) => page.data.as_bytes().len() > MAX_PAGE_SIZE,
          _              => false,
        }
      };

      // split the page if it got too big
      if split_page {
        branch.as_mut().or_else(|| unreachable!()).map(|boxed_node| {
            let node = mem::replace(&mut **boxed_node, Tree(PageTree::new()));
            **boxed_node = if let Leaf(page) = node {
              let (page, next_page) = page.split();
              assert!(page.data.len() <= MAX_PAGE_SIZE);
              assert!(next_page.data.len() <= MAX_PAGE_SIZE);
              // avoid splitting the leaf if there's place in the other branch
              if other.is_none() {
                let (page, other_page) =
                  if go_left { (page, next_page) } else { (next_page, page) };
                *other = Some(Box::new(Leaf(other_page)));
                Leaf(page)
              }
              else {
                let mut new_subtree = PageTree::new();
                let right_offset = page.length;
                new_subtree.insert_page_at_offset(page, 0);
                new_subtree.insert_page_at_offset(next_page, right_offset);
                Tree(new_subtree)
              }
            } else { unreachable!(); };
          });
      }
    }
    self.update_caches();
    self.ensure_balanced();
  }

  #[cfg(test)]
  fn prepend_page(&mut self, page: Page) {
    self.insert_page_at_offset(page, 0);
  }

  fn append_page(&mut self, page: Page) {
    let offset = self.length;
    self.insert_page_at_offset(page, offset);
  }

  // Inserting pages directly is useful for efficiently building a well balanced
  // tree when reading a file.
  fn insert_page_at_offset(&mut self, page: Page, offset: usize) {
    assert!(offset <= self.length);

    // insert the page in the appropriate branch
    {
      let (go_left, new_offset) = self.decide_branch_by_offset(offset);
      let (branch, other) = if go_left { (&mut self.left, &mut self.right) }
                            else       { (&mut self.right, &mut self.left) };

      #[derive(PartialEq)]
      enum InsertMode {
        SetBranch,
        UpdateTree,
        SplitLeaf,
      }

      let insert_mode = branch.as_ref().map(|boxed_node|
        match **boxed_node {
          Tree(_) => InsertMode::UpdateTree,
          Leaf(_) => InsertMode::SplitLeaf,
        }).unwrap_or(InsertMode::SetBranch);

      match insert_mode {
        InsertMode::SetBranch  => *branch = Some(Box::new(Leaf(page))),
        InsertMode::UpdateTree => {
          branch.as_mut().map(|boxed_node|
            match **boxed_node {
              Tree(ref mut tree) =>
                tree.insert_page_at_offset(page, new_offset),
              Leaf(_)            => unreachable!(),
            }).or_else(|| unreachable!());
        }
        InsertMode::SplitLeaf  => {
          // this offset must be on a page boundary, no splitting of pages
          assert!(new_offset == 0 || new_offset == branch.length());
          // make a new subtree only if necessary
          if other.is_none() {
            // put page in the other branch and swap it to where it wants to be
            *other = Some(Box::new(Leaf(page)));
            mem::swap(branch, other);
          }
          else {
            let mut new_subtree = PageTree::new();
            mem::swap(branch, &mut new_subtree.left);
            new_subtree.update_caches();
            new_subtree.insert_page_at_offset(page, new_offset);
            *branch = Some(Box::new(Tree(new_subtree)));
          }
        }
      }
    }

    self.update_caches();
    self.ensure_balanced();

    // if left is none, right is none
    assert!(!self.left.is_none() || self.right.is_none());
  }

  // Will delete from the start offset to either the end offset, or if the range
  // is spanning multiple pages, to the end of the page containing the start
  // offset. This way only one page is affected by a call to delete_range,
  // meaning no merges but possible deletion of single leafs, and the tree can
  // rebalance itself as necessary. Returns the deleted amount.
  fn delete_range(&mut self, start: usize, end: usize) -> usize {
    assert!(start < end && start <= self.length);
    let deleted = {
      let (go_left, new_start) = self.decide_branch_by_offset(start);
      let new_end = end - (start - new_start);
      let branch = if go_left { &mut self.left } else { &mut self.right };
      if let Some(mut boxed_node) = branch.take() {
        // perform the deletion
        let deleted = match *boxed_node {
          Tree(ref mut tree) => tree.delete_range(new_start, new_end),
          Leaf(ref mut page) => page.delete_range(new_start, new_end),
        };
        // decide whether we need to shrink the tree by lifting a node up
        let page_emptied = match *boxed_node {
          Tree(ref tree) => tree.left.is_none() || tree.right.is_none(),
          Leaf(ref page) => page.length == 0,
        };
        // put node back, or lift other node up if a page got empty
        *branch = if !page_emptied { Some(boxed_node) } else {
          match *boxed_node {
            Tree(tree) => { assert!(tree.right.is_none()); tree.left }
            Leaf(_)    => None,
          }
        };
        deleted
      } else { panic!("Attempted deleting from None node"); }
    };
    if self.left.is_none() { mem::swap(&mut self.left, &mut self.right); }
    self.update_caches();
    self.ensure_balanced();
    return deleted;
  }

  // There are two cases when the branches' heights can differ by too much:
  // 1) One branch is None and the other is a Tree. Simply replace self with the
  // tree branch.
  // 2) The higher branch has two tree nodes down its highest path. Further,
  // those two tree nodes along with self has in total four affected subtrees
  // which must keep their left-to-right order relative to each other. In this
  // case self will be replaced with a new tree with left and right each being a
  // tree node holding the affected subtrees in their appropriate order.
  fn ensure_balanced(&mut self) {
    let left_height = self.left.height();
    let right_height = self.right.height();
    let height_diff =
      if left_height >= right_height { left_height - right_height }
      else                           { right_height - left_height };

    // assuming the tree was well balanced before some recent insert/removal
    assert!(height_diff <= 2);
    if height_diff == 2 && (left_height == 0 || right_height == 0) {
      let mut branch = self.left.take().or_else(|| self.right.take());
      branch.take().map(|boxed_node| match *boxed_node {
          Tree(new_self) => *self = new_self,
          _              => unreachable!(),
        }).or_else(|| unreachable!());
    }
    else if height_diff == 2 {

      // keep placeholders for the affected subtrees (left to right)
      let mut t0 = None;
      let mut t1 = None;
      let mut t2 = None;
      let mut t3 = None;

      // also keep the tree nodes around to avoid allocating new ones
      let mut mid_tree = None;
      let mut low_tree = None;

      // go down the higher branch and stash the affected trees
      let first_go_left = left_height > right_height;
      match if first_go_left { &mut self.left } else { &mut self.right } {
        &mut None                     => unreachable!(),
        &mut Some(ref mut boxed_node) => {
          match **boxed_node {
            Leaf(_)           => unreachable!(),
            Tree(ref mut mid) => {
              let left_height = mid.left.height();
              let right_height = mid.right.height();
              let then_go_left = left_height > right_height;
              match if then_go_left { &mut mid.left } else { &mut mid.right } {
                &mut None                     => unreachable!(),
                &mut Some(ref mut boxed_node) => {
                  match **boxed_node {
                    Leaf(_)           => unreachable!(),
                    Tree(ref mut low) => {
                      // stash the branches of the lower subtree in appropriate
                      // order
                      let (left, right) = match (first_go_left, then_go_left) {
                        (false, false) => (&mut t2, &mut t3),
                        (false, true)  => (&mut t1, &mut t2),
                        (true,  false) => (&mut t1, &mut t2),
                        (true,  true)  => (&mut t0, &mut t1),
                      };
                      mem::swap(&mut low.left, left);
                      mem::swap(&mut low.right, right);
                    }
                  }
                }
              }
              // stash the lower subtree as well as the middle subtree's other
              // branch in the appropriate order
              let placeholder = match (first_go_left, then_go_left) {
                (false, false) => &mut t1,
                (false, true)  => &mut t3,
                (true,  false) => &mut t0,
                (true,  true)  => &mut t2,
              };
              let (branch, other) =
                if then_go_left { (&mut mid.right, &mut mid.left) }
                else            { (&mut mid.left, &mut mid.right) };
              mem::swap(branch, placeholder);
              mem::swap(&mut low_tree, other);
            }
          }
        }
      }

      // stash the other branch of self in appropriate order
      if first_go_left { mem::swap(&mut self.right, &mut t3); }
      else             { mem::swap(&mut self.left, &mut t0); }

      // finally stash the middle tree as well
      mem::swap(&mut mid_tree,
        if first_go_left { &mut self.left } else { &mut self.right });

      // lift the two trees into self
      mem::swap(&mut self.left, &mut mid_tree);
      mem::swap(&mut self.right, &mut low_tree);

      // place t0-t3 into the left and right tree in appropriate order
      self.left.as_mut().or_else(|| unreachable!()).map(|boxed_tree_node|
          if let Tree(ref mut left) = **boxed_tree_node {
            mem::swap(&mut left.left, &mut t0);
            mem::swap(&mut left.right, &mut t1);
            left.update_caches();
          } else { unreachable!() });
      self.right.as_mut().or_else(|| unreachable!()).map(|boxed_tree_node|
          if let Tree(ref mut right) = **boxed_tree_node {
            mem::swap(&mut right.left, &mut t2);
            mem::swap(&mut right.right, &mut t3);
            right.update_caches();
          } else { unreachable!() });
      self.update_caches();
    }
  }

  fn update_caches(&mut self) {
    self.length = self.left.length() + self.right.length();
    self.newlines = self.left.newlines() + self.right.newlines();
    self.height = 1 + cmp::max(self.left.height(), self.right.height());
  }

  // Optionally returns the page containing a certain offset along with total
  // offset building up to the start of it.
  fn find_page_by_offset(&self, offset: usize) -> Option<(&Page, usize)> {
    if offset >= self.length { return None; }
    let (go_left, new_offset) = self.decide_branch_by_offset(offset);
    let branch = if go_left { &self.left } else { &self.right };
    branch.as_ref().and_then(|node|
        match **node {
          Tree(ref tree) => tree.find_page_by_offset(new_offset),
          Leaf(ref page) => Some((page, 0)),
        }).
      map(|(page, offset)|
        (page, offset + if go_left { 0 } else { self.left.length() }))
  }

  fn decide_branch_by_offset(&self, offset: usize) -> (bool, usize) {
    let left_length = self.left.length();
    let go_left = left_length == 0 || offset < left_length;
    return (go_left, if go_left { offset } else { offset - left_length });
  }

  fn decide_branch_by_line(&self, line: usize) -> (bool, usize) {
    let left_newlines = self.left.newlines();
    let go_left = line <= left_newlines;
    return (go_left, if go_left { line } else { line - left_newlines });
  }

  fn iter(&self) -> PageTreeIterator {
    PageTreeIterator::new(self, 0)
  }
}

/*
 * PageTreeIterator iterates the pages of a page tree from left to right.
 */
struct PageTreeIterator<'l> {
  tree: &'l PageTree,
  next_offset: usize,
}

impl<'l> PageTreeIterator<'l> {
  fn new(tree: &'l PageTree, start_offset: usize) -> PageTreeIterator<'l> {
    PageTreeIterator { tree: tree, next_offset: start_offset }
  }
}

impl<'l> Iterator for PageTreeIterator<'l> {
  type Item = &'l Page;

  fn next(&mut self) -> Option<Self::Item> {
    self.tree.find_page_by_offset(self.next_offset).map(
      |(page, offset)| { self.next_offset = offset + page.length; page })
  }
}

/*
 * CharIterator iterates the characters of a page tree between the given start
 * and end offsets.
 */
pub struct CharIterator<'l> {
  counter: usize,
  pages: PageTreeIterator<'l>,
  chars: Option<::std::str::Chars<'l>>,
}

impl<'l> CharIterator<'l> {
  fn new(tree: &'l PageTree, start: usize, end: usize) -> CharIterator<'l> {
    assert!(start < end && end <= tree.length);
    let mut pages = PageTreeIterator::new(tree, start);
    let page = pages.next().unwrap();
    let mut chars = page.data.chars();
    for _ in 0..((start + page.length) - pages.next_offset) { chars.next(); }
    CharIterator {
      counter: end - start,
      pages: pages,
      chars: Some(chars),
    }
  }
}

impl<'l> Iterator for CharIterator<'l> {
  type Item = char;

  fn next(&mut self) -> Option<Self::Item> {
    if self.counter == 0 { None } else {
      self.counter -= 1;
      self.chars.as_mut().and_then(|ref mut chars| chars.next()).or_else(|| {
        self.chars = self.pages.next().map(|page| page.data.chars());
        self.chars.as_mut().and_then(|ref mut chars| chars.next()) })
    }
  }
}

/*
 * LineIterator iterates the lines of a page tree, yielding a CharIterator for
 * each line.
 */
pub struct LineIterator<'l> {
  tree: &'l PageTree,
  next_line: usize
}

impl<'l> LineIterator<'l> {
  fn new(tree: &'l PageTree) -> LineIterator {
    LineIterator { tree: tree, next_line: 0 }
  }

  pub fn from(mut self, line: usize) -> LineIterator<'l> {
    self.next_line = line;
    self
  }
}

impl<'l> Iterator for LineIterator<'l> {
  type Item = CharIterator<'l>;

  fn next(&mut self) -> Option<Self::Item> {
    self.tree.line_start_and_end_offset(self.next_line).
    and_then(|(start, end)| if start != end { Some((start, end)) }
                            else            { None }).
    map(|(start, end)| CharIterator::new(self.tree, start, end)).
    map(|it| { self.next_line += 1; it })
  }
}

/*
 * The data of a page is for convenience kept as a string. The page may hold a
 * string of any length or size, but should for performance keep it lagom.
 * A single line may span multiple pages.
 */
struct Page {
  data: String,
  length: usize,
  newline_offsets: Vec<usize>,  // cached offsets to newlines within the page
}

impl Page {
  fn new(data: String) -> Page {
    assert!(data.len() <= MAX_PAGE_SIZE);
    let mut page = Page { data: data, length: 0, newline_offsets: Vec::new() };
    page.update_caches();
    return page;
  }

  fn byte_offset(&self, offset: usize) -> usize {
    self.data.char_indices().take(offset).last().map(
      |(i, c)| i + c.len_utf8()).unwrap_or(0)
  }

  fn insert_string_at_offset(&mut self, string: String, offset: usize) {
    assert!(offset <= self.data.chars().count());
    let byte_offset = self.byte_offset(offset) as isize;
    let original_size = self.data.len();
    let string_size = string.len();
    self.data.push_str(&string);  // first grow organically
    unsafe {
      let bytes_mut = self.data.as_mut_vec().as_mut_ptr();
      // make place by shifting some original data to the side
      ptr::copy(self.data.as_bytes().as_ptr().offset(byte_offset),
                bytes_mut.offset(byte_offset + string_size as isize),
                original_size - byte_offset as usize);
      // plunk that chunk in there
      ptr::copy(string.as_bytes().as_ptr(),
                bytes_mut.offset(byte_offset),
                string_size);
    }
    self.update_caches();
  }

  fn delete_range(&mut self, start: usize, end: usize) -> usize {
    assert!(start <= self.data.chars().count());
    let deleted = cmp::min(end, self.length) - start;
    unsafe {
      let start = self.byte_offset(start);
      let end = if end < self.length { self.byte_offset(end) }
                else                 { self.data.len() };
      // before truncating, shift down the string's ending if anything is left
      if end < self.data.len() {
        ptr::copy(self.data.as_bytes().as_ptr().offset(end as isize),
                  self.data.as_mut_vec().as_mut_ptr().offset(start as isize),
                  self.data.len() - end);
      }
      let new_len = self.data.len() - end + start;
      self.data.as_mut_vec().truncate(new_len);
    }
    self.update_caches();
    return deleted;
  }

  fn update_caches(&mut self) {
    self.length = self.data.chars().count();
    self.newline_offsets.clear();
    let mut offset = 0;
    for character in self.data.chars() {
      if character == '\n' { self.newline_offsets.push(offset); }
      offset += 1;
    }
  }

  fn offset_of_newline(&self, newline: usize) -> Option<usize> {
    self.newline_offsets.get(newline).map(|&offset| offset)
  }

  fn split(mut self) -> (Page, Page) {
    let half_plus_buffer_zone = self.data.len() / 2 + 2;
    let mut chunker = StringChunkerator::new(self.data, half_plus_buffer_zone);
    self.data = chunker.next().expect("Split should have got a first chunk.");
    let rest = chunker.next().expect("Split should have got a second chunk.");
    assert!(chunker.next().is_none());
    self.update_caches();
    return (self, Page::new(rest));
  }
}

/*
 * The StringChunkerator eats strings and leaves chunks no larger than the
 * specified size.
 */
struct StringChunkerator {
  data: Vec<u8>,
  chunk_size: usize,
}

impl StringChunkerator {
  fn new(string: String, chunk_size: usize) -> StringChunkerator {
    StringChunkerator { data: string.into_bytes(), chunk_size: chunk_size }
  }
}

impl Iterator for StringChunkerator {
  type Item = String;

  fn next(&mut self) -> Option<Self::Item> {
    // cut out a chunk of |self.chunk_size| bytes and hope it's a valid string
    let mut chunk: Vec<u8> =
      self.data.iter().take(self.chunk_size).map(|&b| b).collect();
    self.data = self.data.iter().skip(self.chunk_size).map(|&b| b).collect();
    while self.data.len() > 0 || chunk.len() > 0 {
      chunk = match String::from_utf8(chunk) {
        Ok(good_chunk) => return Some(good_chunk),
        Err(err)       => {
          // shift a byte back to data before trying again
          let mut broken_chunk = err.into_bytes();
          broken_chunk.pop().map(|byte| self.data.insert(0, byte)).unwrap();
          broken_chunk
        }
      }
    }
    return None;
  }
}

/*
 * PageStream is an iterator which takes a file path and spits out pages until
 * all the file content has been consumed.
 */
struct PageStream {
  file: File,
  error: Option<io::Error>,
}

impl PageStream {
  fn new(path: &Path) -> io::Result<PageStream> {
    File::open(path).and_then(|f| Ok(PageStream { file: f, error: None }))
  }

  // Assumes |data| is well formed utf8 with the possible exception of a broken
  // last character
  fn raw_data_to_utf8_string(data: &[u8]) -> (String, usize) {
    let mut string = String::from_utf8_lossy(data).into_owned();
    let mut num_truncated_bytes = 0;
    // adjust for multi-byte code-points spanning page boundaries
    let replacement_char = '\u{FFFD}';
    if string.chars().last() == Some(replacement_char) {
      let string_len = string.len();
      string.truncate(string_len - replacement_char.len_utf8());
      num_truncated_bytes = data.len() - string.len();
    }
    return (string, num_truncated_bytes);
  }
}

impl Iterator for PageStream {
  type Item = Page;

  fn next(&mut self) -> Option<Self::Item> {
    use std::ops::DerefMut;
    use std::io::SeekFrom;
    let mut data = Box::new([0; PAGE_SIZE]);
    self.file.read(data.deref_mut()).
    and_then(|bytes| if bytes == 0 { Ok(None) } else {
      let (string, num_truncated_bytes) =
        PageStream::raw_data_to_utf8_string(&(*data)[0..bytes]);
      try!(self.file.seek(SeekFrom::Current(-(num_truncated_bytes as i64))));
      Ok(Some(Page::new(string))) }).
    map_err(|err| self.error = Some(err)).
    ok().and_then(|result| result)
  }
}

/*
 * The various errors that may result from usage of the buffer.
 */
#[derive(Debug)]
pub enum Error {
  IoError(io::Error),
  NoPath,
  BadLocation,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", *self)
  }
}

impl error::Error for Error {
  fn description(&self) -> &str {
    match *self {
      Error::IoError(ref err) => error::Error::description(err),
      Error::NoPath           => "The buffer had no path.",
      Error::BadLocation      =>
        "The line/column or offset did not specify a valid location",
    }
  }
}

pub type Result<T> = result::Result<T, Error>;

/*
 * The buffer is used to open, modify and write files back to disk.
 */
pub struct Buffer {
  path: Option<PathBuf>,
  tree: PageTree,
}

impl Buffer {
  #[cfg(test)]
  pub fn new() -> Buffer {
    let mut buffer = Buffer { path: None, tree: PageTree::new() };
    buffer.insert_at_offset("\n".to_string(), 0);
    return buffer;
  }

  pub fn open(path: &Path) -> Result<Buffer> {
    PageStream::new(path).
    and_then(PageTree::build).
    and_then(|tree| Ok(Buffer { path: Some(path.to_path_buf()), tree: tree })).
    map(|mut buffer| { buffer.ensure_ends_with_newline(); buffer }).
    map_err(|io_err| Error::IoError(io_err))
  }

  fn ensure_ends_with_newline(&mut self) {
    let ends_with_newline = self.tree.length > 0 &&
      self.tree.get_char_by_offset(self.tree.length - 1).map(|c| c == '\n').
      expect("Found no last character in buffer of non-zero length.");
    if !ends_with_newline {
      let offset = self.tree.length;
      self.insert_at_offset("\n".to_string(), offset);
    }
  }

  pub fn write(&self) -> Result<()> {
    self.path.as_ref().
    map_or(Err(Error::NoPath), |path| self.write_to(path))
  }

  pub fn write_to(&self, path: &Path) -> Result<()> {
    File::create(path).
    and_then(|mut file|
      self.tree.iter().
      map(|page| file.write_all(page.data.as_bytes())).
      fold(Ok(()),
        |ok, err| if ok.is_ok() && err.is_err() { err } else { ok })).
    map_err(|io_err| Error::IoError(io_err))
  }

  #[cfg(not(test))]
  pub fn path(&self) -> Result<&Path> {
    self.path.as_ref().map(|path| path.as_path()).ok_or(Error::NoPath)
  }

  pub fn insert_at_line_column(&mut self, string: String, line: usize,
                               column: usize) -> Result<()> {
    self.tree.line_column_to_offset(line, column).
    map(|offset| self.insert_at_offset(string, offset)).
    ok_or(Error::BadLocation)
  }

  pub fn insert_at_offset(&mut self, string: String, mut offset: usize) {
    if string.len() > PAGE_SIZE {
      for chunk in StringChunkerator::new(string, PAGE_SIZE) {
        let chunk_length = chunk.chars().count();
        self.tree.insert_string_at_offset(chunk, offset);
        offset += chunk_length;
      }
    }
    else {
      self.tree.insert_string_at_offset(string, offset);
    }
  }

  pub fn delete_range(&mut self, start_line: usize, start_column: usize,
                      end_line: usize, end_column: usize) -> Result<()> {
    self.tree.line_column_to_offset(start_line, start_column).
    and_then(|start|
      self.tree.line_column_to_offset(end_line, end_column).
      and_then(|end| if end < self.tree.length { Some(end) } else { None }).
      map(|end| (start, end))).
    and_then(|(start, end)|
      if start < end { Some((start, end)) } else { None }).
    map(|(start, mut end)|
      while start < end { end -= self.tree.delete_range(start, end); } ).
    ok_or(Error::BadLocation)
  }

  pub fn get_char_by_line_column(&self, line: usize, column: usize)
      -> Option<char> {
    self.tree.get_char_by_line_column(line, column)
  }

  pub fn num_lines(&self) -> usize {
    self.tree.newlines
  }

  // excludes newline character from the count
  pub fn line_length(&self, line: usize) -> Option<usize> {
    self.tree.line_start_and_end_offset(line).and_then(|(start, end)|
      if start >= end { None } else { Some(end - start - 1) })
  }

  pub fn line_iter(&self) -> LineIterator {
    LineIterator::new(&self.tree)
  }
}

#[cfg(test)]
mod test {
  use std::fs::File;
  use std::path::Path;

  use super::*;

  // Opens a buffer (new or loaded file), performs some operation on it,
  // dumps buffer content to disk, compares results to expectations.
  // Also throws in a balance check on the resulting page tree, because why not.
  fn buffer_test<O, M: ?Sized>(test: &String, operation: O, make_buffer: Box<M>)
      where O: Fn(&mut Buffer) -> (),
            M: Fn() -> Result<Buffer> {
    use std::error::Error;
    use std::io::Read;
    let result_path_string = format!("tests/buffer/{}-result.txt", test);
    let result_path = Path::new(&result_path_string);
    let expect_path_string = format!("tests/buffer/{}-expect.txt", test);
    let expect_path = Path::new(&expect_path_string);

    let result = make_buffer().
      map(|mut buffer| { operation(&mut buffer); buffer }).
      map(|buffer| { assert!(is_balanced(&buffer.tree)); buffer }).
      and_then(|buffer| buffer.write_to(&result_path));

    let file_contents = |path| File::open(path).and_then(|mut file| {
      let mut content = String::new();
      try!(file.read_to_string(&mut content));
      Ok(content) });

    let result_content = file_contents(&result_path);
    let expect_content = file_contents(&expect_path);

    match (result_content, result, expect_content) {
      (Err(e),     _,      _         ) => panic!("{}", Error::description(&e)),
      (_,          Err(e), _         ) => panic!("{}", Error::description(&e)),
      (_,          _,      Err(e)    ) => panic!("{}", Error::description(&e)),
      (Ok(result), Ok(_),  Ok(expect)) => assert_eq!(result, expect),
    };
  }

  #[test]
  fn write_without_path() {
    assert!(Buffer::new().write().is_err());
  }

  macro_rules! buffer_test {
    ($name:ident, $new_file:expr, $operation:expr) => (
      #[test]
      fn $name() {
        let test = stringify!($name).to_string();
        let buffer_maker: Box<Fn() -> Result<Buffer>> =
          if $new_file { Box::new(|| Ok(Buffer::new())) }
          else { Box::new(|| {
            let test_path_string = format!("tests/buffer/{}.txt", &test);
            let test_path = Path::new(&test_path_string);
            return Buffer::open(&test_path);
          }) };
        buffer_test(&test, $operation, buffer_maker);
      }
    );
  }

  buffer_test!(utf8_one_byte_overflow, false, |_| ());
  buffer_test!(utf8_two_byte_overflow, false, |_| ());
  buffer_test!(one_full_page, false, |_| ());
  buffer_test!(multiple_pages, false, |_| ());

  macro_rules! simple_balance_test {
    ($name:ident, $fun:ident, $num_pages:expr) => (
      #[test]
      fn $name() {
        let mut tree = PageTree::new();
        for _ in 0..$num_pages {
          tree.$fun(Page::new("a".to_string()));
        }
        assert!(is_balanced(&tree));
      }
    );
  }

  simple_balance_test!(balanced_append_32, append_page, 32);
  simple_balance_test!(balanced_append_33, append_page, 33);
  simple_balance_test!(balanced_append_9001, append_page, 9001);
  simple_balance_test!(balanced_prepend_32, prepend_page, 32);
  simple_balance_test!(balanced_prepend_33, prepend_page, 33);
  simple_balance_test!(balanced_prepend_9001, prepend_page, 9001);

  macro_rules! balance_test {
    ($name:ident, $num_pages:expr) => (
      #[test]
      fn $name() {
        let mut tree = PageTree::new();
        let denominator: usize = 4;
        let mut numerator: usize = 0;
        for i in 0..$num_pages {
          let page = Page::new("abc".to_string());
          let fraction = (numerator as f32) / (denominator as f32);
          let offset = ((i as f32) * fraction) as usize * page.length;
          tree.insert_page_at_offset(page, offset);
          numerator = (numerator + 1) % (denominator + 1);
        }
        assert!(is_balanced(&tree));
      }
    );
  }

  balance_test!(balanced_insert_32, 32);
  balance_test!(balanced_insert_33, 33);
  balance_test!(balanced_insert_9001, 9001);

  fn is_balanced(tree: &PageTree) -> bool {
    let branch_is_balanced = |branch: &PageTreeLink| {
      match *branch {
        Some(ref boxed_node) =>
          match **boxed_node {
            PageTreeNode::Tree(ref tree) => is_balanced(tree),
            _                            => true,
          },
        _                                      => true,
      }
    };
    let left_height = tree.left.height();
    let right_height = tree.right.height();
    let height_diff =
      if left_height >= right_height { left_height - right_height }
      else                           { right_height - left_height };
    return height_diff < 2 &&
      branch_is_balanced(&tree.left) && branch_is_balanced(&tree.right);
  }

  buffer_test!(existing_file_insert, false, existing_file_insert_operation);
  buffer_test!(new_file_insert, true, new_file_insert_operation);
  buffer_test!(page_split_utf8_insert, false, page_split_utf8_insert_operation);
  buffer_test!(long_string_insert, true, long_string_insert_operation);

  fn existing_file_insert_operation(buffer: &mut Buffer) {
    buffer.insert_at_offset("more".to_string(), 0);
    buffer.insert_at_offset(" than ".to_string(), 4);
    buffer.insert_at_offset(".".to_string(), 25);
    let buffer_end = buffer.tree.length - 1;
    buffer.insert_at_offset(" and then some".to_string(), buffer_end);
  }

  fn new_file_insert_operation(buffer: &mut Buffer) {
    buffer.insert_at_offset("Here's a second line".to_string(), 1);
    buffer.insert_at_offset(" with a newline\n".to_string(), 21);
    buffer.insert_at_offset("First line go here".to_string(), 0);
    buffer.insert_at_offset(", and it even has a dot.".to_string(), 18);
  }

  fn page_split_utf8_insert_operation(buffer: &mut Buffer) {
    buffer.insert_at_offset("boop".to_string(), 5);
    buffer.insert_at_offset("boop".to_string(), 22);
    buffer.insert_at_offset("boop".to_string(), 36);
  }

  fn long_string_insert_operation(buffer: &mut Buffer) {
    buffer.insert_at_offset(
      include_str!("../tests/buffer/long_string_insert.txt").to_string(), 1);
  }

  #[test]
  fn line_column_to_offset_test() {
    let tests = [
       ((0, 0), Some(0)), ((0, 15), Some(15)), ((0, 16), None),
       ((1, 15), Some(31)), ((1, 28), Some(44)),
       ((5, 0), Some(51)), ((5, 1), None),
       ((7, 0), Some(53)),
       ((8, 0), Some(62)), ((8, 1), None),
       ((9, 0), None), ((9, 1), None)
    ];

    let test_path = Path::new("tests/buffer/line_column_offset.txt");
    let buffer = Buffer::open(&test_path).unwrap();
    for &((line, column), expected_offset) in tests.iter() {
      assert_eq!(buffer.tree.line_column_to_offset(line, column),
                 expected_offset);
    }
  }

  #[test]
  fn get_char_by_line_column_test() {
    let tests = [
      ((0, 0), Some('a')), ((0, 15), Some('\n')), ((0, 16), None),
      ((1, 15), Some('p')), ((1, 28), Some('ö')),
      ((5, 0), Some('\n')), ((5, 1), None),
      ((7, 0), Some('2')),
      ((8, 0), None), ((8, 1), None),
      ((9, 0), None), ((9, 1), None)
    ];

    let test_path = Path::new("tests/buffer/line_column_offset.txt");
    let buffer = Buffer::open(&test_path).unwrap();
    for &((line, column), expect_char) in tests.iter() {
      assert_eq!(buffer.get_char_by_line_column(line, column), expect_char);
    }
  }

  #[test]
  fn line_length() {
    let path = Path::new("tests/buffer/long_string_insert.txt");
    let expect = [141, 48, 152];
    line_length_test(&path, &expect);
  }

  #[test]
  fn line_length_when_lacking_newline() {
    let path = Path::new("tests/buffer/lacking_newline.txt");
    let expect = [3, 0, 4];
    line_length_test(&path, &expect);
  }

  fn line_length_test(path: &Path, expect: &[usize]) {
    let buffer = Buffer::open(path).unwrap();
    assert_eq!(buffer.num_lines(), expect.len());
    for line in 0..buffer.num_lines() {
      assert_eq!(buffer.line_length(line).unwrap(), expect[line]);
    }
  }

  #[test]
  fn line_iterator_yields_all_lines() {
    let path = Path::new("tests/buffer/lacking_newline.txt");
    let buffer = Buffer::open(&path).unwrap();
    assert_eq!(buffer.line_iter().count(), buffer.num_lines());
  }

  #[test]
  fn line_iterator_yields_correct_line_lengths() {
    let path = Path::new("tests/buffer/long_string_insert.txt");
    let buffer = Buffer::open(&path).unwrap();
    let mut line = 0;
    for chars in buffer.line_iter() {
      assert_eq!(buffer.line_length(line), Some(chars.count() - 1));
      line += 1;
    }
    assert_eq!(line, buffer.num_lines());
  }

  #[test]
  fn char_iterator_yields_all_chars() {
    let path = Path::new("tests/buffer/lacking_newline.txt");
    let buffer = Buffer::open(&path).unwrap();
    let mut offset = 0;
    for chars in buffer.line_iter() {
      for character in chars {
        assert_eq!(buffer.tree.get_char_by_offset(offset), Some(character));
        offset += 1;
      }
    }
    assert_eq!(offset, buffer.tree.length);
  }

  buffer_test!(delete_char_by_char_start, false, delete_char_by_char_start_op);
  buffer_test!(delete_char_by_char_mid, false, delete_char_by_char_mid_op);
  buffer_test!(delete_char_by_char_end, false, delete_char_by_char_end_op);
  buffer_test!(delete_big_range, false, delete_big_range_op);
  buffer_test!(delete_utf8, false, delete_utf8_op);

  fn delete_char_by_char_start_op(buffer: &mut Buffer) {
    for _ in 0..100 { buffer.tree.delete_range(0, 1); }
  }

  fn delete_char_by_char_mid_op(buffer: &mut Buffer) {
    for _ in 0..100 { buffer.tree.delete_range(68, 69); }
  }

  fn delete_char_by_char_end_op(buffer: &mut Buffer) {
    for i in 0..100 { buffer.tree.delete_range(255 - i, 256 - i); }
  }

  fn delete_big_range_op(buffer: &mut Buffer) {
    buffer.delete_range(0, 227, 6, 91).ok().unwrap();
  }

  fn delete_utf8_op(buffer: &mut Buffer) {
    buffer.delete_range(0, 18, 2, 144).ok().unwrap();
  }

  #[test]
  fn delete_with_bad_input() {
    let path = Path::new("tests/buffer/lacking_newline.txt");
    let mut buffer = Buffer::open(&path).unwrap();
    assert!(buffer.delete_range(0, 100, 1, 0).is_err());
    assert!(buffer.delete_range(0, 0, 4, 0).is_err());
    assert!(buffer.delete_range(2, 0, 0, 0).is_err());
  }
}
