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
use std::num::SignedInt;
use std::path::{Path, PathBuf};
use std::ptr;

use self::PageTreeNode::*;

#[cfg(not(test))]
const PAGE_SIZE: uint = 1024;  // ish
#[cfg(test)]
const PAGE_SIZE: uint = 16;

// a rather soft limit where it's good time to split a page in two
const MAX_PAGE_SIZE: uint = (PAGE_SIZE as f32 * 1.5) as uint;

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
  Tree(Box<PageTree>),
  Leaf(Page),
  Nil,
}

impl PageTreeNode {
  fn is_nil(&self) -> bool {
    match *self {
      Nil => true,
      _   => false,
    }
  }

  fn length(&self) -> uint {
    match *self {
      Nil            => 0,
      Tree(ref tree) => tree.length,
      Leaf(ref page) => page.length,
    }
  }

  fn newlines(&self) -> uint {
    match *self {
      Nil            => 0,
      Tree(ref tree) => tree.newlines,
      Leaf(ref page) => page.newline_offsets.len(),
    }
  }

  fn height(&self) -> uint {
    match *self {
      Nil            => 0,
      Tree(ref tree) => tree.height,
      Leaf(_)        => 1
    }
  }
}

struct PageTree {
  left: PageTreeNode,
  right: PageTreeNode,
  length: uint,  // cached length of pages down the subtrees
  newlines: uint,  // cached numer of newlines in the pages down the subtrees
  height: uint,  // cached height of the tree
}

impl PageTree {
  fn new() -> PageTree {
    PageTree { left: Nil, right: Nil, length: 0, newlines: 0, height: 1 }
  }

  fn build(mut stream: PageStream) -> io::Result<PageTree> {
    let mut tree = PageTree::new();
    for page in stream.by_ref() { tree.append_page(page); }
    return stream.error.map_or(Ok(tree), |err| Err(err));
  }

  // See comment for offset_of_line_start and add to that quirk the requirement
  // that |column| == 0.
  fn line_column_to_offset(&self, line: uint, column: uint) -> Option<uint> {
    self.line_start_and_end_offset(line).and_then(
      |(line_start_offset, line_end_offset)| {
        let line_length = line_end_offset - line_start_offset;
        let offset = line_start_offset + column;
        if column == 0 || column < line_length { Some(offset) } else { None }
      })
  }

  // end may equal start if the file is empty
  fn line_start_and_end_offset(&self, line: uint) -> Option<(uint, uint)> {
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
  fn offset_of_line_start(&self, line: uint) -> Option<uint> {
    let (go_left, new_line) = self.decide_branch_by_line(line);
    let (branch, other) = if go_left { (&self.left, &self.right) }
                          else       { (&self.right, &self.left) };
    match branch {
      &Nil            => if new_line == 0 { Some(0) } else { None },
      &Tree(ref tree) => tree.offset_of_line_start(new_line),
      &Leaf(ref page) =>
        if new_line == 0 { Some(0) }
        else { page.offset_of_newline(new_line - 1).map(|offset| offset + 1) }
    }.map(|offset| offset + if go_left { 0 } else { other.length() })
  }

  fn get_char_by_line_column(&self, line: uint, column: uint) -> Option<char> {
    self.line_column_to_offset(line, column).and_then(
      |offset| self.get_char_by_offset(offset))
  }

  fn get_char_by_offset(&self, offset: uint) -> Option<char> {
    let (go_left, new_offset) = self.decide_branch_by_offset(offset);
    match if go_left { &self.left } else { &self.right } {
      &Nil            => None,
      &Tree(ref tree) => tree.get_char_by_offset(new_offset),
      &Leaf(ref page) => page.data.as_slice().chars().nth(new_offset),
    }
  }

  fn insert_string_at_offset(&mut self, string: String, offset: uint) {
    // We may only split a page once, otherwise rebalancing may not be possible
    // while returning. Therefore we must avoid inserting too large strings.
    assert!(string.len() <= PAGE_SIZE);
    assert!(offset <= self.length);
    {
      let (go_left, new_offset) = self.decide_branch_by_offset(offset);
      let (branch, other) = if go_left { (&mut self.left, &mut self.right) }
                            else       { (&mut self.right, &mut self.left) };

      let new_branch = match mem::replace(branch, Nil) {
        Nil            => Leaf(Page::new(string)),
        Tree(mut tree) => {
          tree.insert_string_at_offset(string, new_offset);
          Tree(tree)
        },
        Leaf(mut page) => {
          page.insert_string_at_offset(string, new_offset);
          // split the page if it got too big
          if page.data.as_bytes().len() > MAX_PAGE_SIZE {
            let (page, next_page) = page.split();
            assert!(page.data.len() <= MAX_PAGE_SIZE);
            assert!(next_page.data.len() <= MAX_PAGE_SIZE);
            // avoid splitting the leaf if there's place in the other branch
            if other.is_nil() {
              let (page, other_page) =
                if go_left { (page, next_page) } else { (next_page, page) };
              *other = Leaf(other_page);
              Leaf(page)
            }
            else {
              let mut new_subtree = PageTree::new();
              let right_offset = page.length;
              new_subtree.insert_page_at_offset(page, 0);
              new_subtree.insert_page_at_offset(next_page, right_offset);
              Tree(Box::new(new_subtree))
            }
          }
          else {
            Leaf(page)
          }
        }
      };
      mem::replace(branch, new_branch);
    }
    self.update_caches();
    self.ensure_balanced();
  }

  fn prepend_page(&mut self, page: Page) {
    self.insert_page_at_offset(page, 0);
  }

  fn append_page(&mut self, page: Page) {
    let offset = self.length;
    self.insert_page_at_offset(page, offset);
  }

  fn insert_page_at_offset(&mut self, page: Page, offset: uint) {
    assert!(offset <= self.length);

    // insert the page in the appropriate branch
    {
      let (go_left, new_offset) = self.decide_branch_by_offset(offset);
      let (branch, other) = if go_left { (&mut self.left, &mut self.right) }
                            else       { (&mut self.right, &mut self.left) };

      match *branch {
        Nil                => *branch = Leaf(page),
        Tree(ref mut tree) => tree.insert_page_at_offset(page, new_offset),
        Leaf(_)            => {
          // this offset must be on a page boundary, no splitting of pages
          assert!(new_offset == 0 || new_offset == branch.length());
          // make a new subtree only if necessary
          if other.is_nil() {
            // put page in the other branch and swap it to where it wants to be
            *other = Leaf(page);
            mem::swap(branch, other);
          }
          else {
            let mut new_subtree = Box::new(PageTree::new());
            mem::swap(branch, &mut new_subtree.left);
            new_subtree.update_caches();
            new_subtree.insert_page_at_offset(page, new_offset);
            *branch = Tree(new_subtree);
          }
        }
      }
    }

    self.update_caches();
    self.ensure_balanced();

    // if left is nil, right is nil
    assert!(!self.left.is_nil() || self.right.is_nil());
  }

  // If the branches' heights differ by too much, the higher branch will have
  // two tree nodes down its highest path. Further, those two tree nodes along
  // with |self| has in total four affected subtrees which must keep their
  // left-to-right order relative to each other.
  // This method will replace |self| with a new tree with left and right each
  // being a tree node holding the affected subtrees in their appropriate order.
  fn ensure_balanced(&mut self) {
    let left_height = self.left.height();
    let right_height = self.right.height();
    let height_diff = (left_height as int - right_height as int).abs();

    // assuming the tree was well balanced before some recent insert/removal
    assert!(height_diff <= 2);
    if height_diff == 2 {

      // keep placeholders for the affected subtrees (left to right)
      let mut t0 = Nil;
      let mut t1 = Nil;
      let mut t2 = Nil;
      let mut t3 = Nil;

      // go down the higher branch and stash the affected trees
      let first_go_left = left_height > right_height;
      match if first_go_left { &mut self.left } else { &mut self.right } {
        &mut Nil | &mut Leaf(_) => unreachable!(),
        &mut Tree(ref mut mid)  => {
          let left_height = mid.left.height();
          let right_height = mid.right.height();
          assert!(left_height != right_height);
          let then_go_left = left_height > right_height;
          match if then_go_left { &mut mid.left } else { &mut mid.right } {
            &mut Nil | &mut Leaf(_) => unreachable!(),
            &mut Tree(ref mut low)  => {
              // stash the branches of the lower subtree in appropriate order
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
          // stash the other branch of the middle subtree in appropriate order
          let (branch, placeholder) = match (first_go_left, then_go_left) {
            (false, false) => (&mut mid.left, &mut t1),
            (false, true)  => (&mut mid.right, &mut t3),
            (true,  false) => (&mut mid.left, &mut t0),
            (true,  true)  => (&mut mid.right, &mut t2),
          };
          mem::swap(branch, placeholder);
        }
      }
      // stash the other branch of self in appropriate order
      if first_go_left { mem::swap(&mut self.right, &mut t3); }
      else             { mem::swap(&mut self.left, &mut t0); }

      // assemble a new tree using the subtrees we stashed above
      let mut balanced_tree = PageTree::new();
      let mut left = PageTree::new();
      let mut right = PageTree::new();
      mem::swap(&mut left.left, &mut t0);
      mem::swap(&mut left.right, &mut t1);
      mem::swap(&mut right.left, &mut t2);
      mem::swap(&mut right.right, &mut t3);
      left.update_caches();
      right.update_caches();
      balanced_tree.left = Tree(Box::new(left));
      balanced_tree.right = Tree(Box::new(right));
      balanced_tree.update_caches();

      // finally replace self with the balanced tree
      mem::swap(self, &mut balanced_tree);
    }
  }

  fn update_caches(&mut self) {
    self.length = self.left.length() + self.right.length();
    self.newlines = self.left.newlines() + self.right.newlines();
    self.height = 1 + cmp::max(self.left.height(), self.right.height());
  }

  // Optionally returns the page containing a certain offset along with total
  // offset building up to the start of it.
  fn find_page_by_offset<'l>(&'l self, offset: uint)
      -> Option<(&'l Page, uint)> {
    if offset >= self.length { return None; }
    let (go_left, new_offset) = self.decide_branch_by_offset(offset);
    let res = match if go_left { &self.left } else { &self.right } {
      &Nil            => None,
      &Tree(ref tree) => tree.find_page_by_offset(new_offset),
      &Leaf(ref page) => Some((&*page, 0)),
    };
    res.map(|(page, offset)|
      (page, offset + if go_left { 0 } else { self.left.length() }))
  }

  fn decide_branch_by_offset(&self, offset: uint) -> (bool, uint) {
    let left_length = self.left.length();
    let go_left = left_length == 0 || offset < left_length;
    return (go_left, if go_left { offset } else { offset - left_length });
  }

  fn decide_branch_by_line(&self, line: uint) -> (bool, uint) {
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
  next_offset: uint,
}

impl<'l> PageTreeIterator<'l> {
  fn new(tree: &'l PageTree, start_offset: uint) -> PageTreeIterator<'l> {
    PageTreeIterator { tree: tree, next_offset: start_offset }
  }
}

impl<'l> Iterator for PageTreeIterator<'l> {
  type Item = &'l Page;

  fn next(&mut self) -> Option<&'l Page> {
    self.tree.find_page_by_offset(self.next_offset).map(
      |(page, offset)| { self.next_offset = offset + page.length; page })
  }
}

/*
 * CharIterator iterates the characters of a page tree between the given start
 * and end offsets.
 */
struct CharIterator<'l> {
  counter: uint,
  pages: PageTreeIterator<'l>,
  chars: Option<::std::str::Chars<'l>>,
}

impl<'l> CharIterator<'l> {
  fn new(tree: &'l PageTree, start: uint, end: uint) -> CharIterator<'l> {
    assert!(start < end && end <= tree.length);
    let mut pages = PageTreeIterator::new(tree, start);
    let page = pages.next().unwrap();
    let mut chars = page.data.as_slice().chars();
    for _ in range(0, start - pages.next_offset + page.length) {
      chars.next();
    }
    CharIterator {
      counter: end - start,
      pages: pages,
      chars: Some(chars),
    }
  }
}

impl<'l> Iterator for CharIterator<'l> {
  type Item = char;

  fn next(&mut self) -> Option<char> {
    if self.counter == 0 { None } else {
      self.counter -= 1;
      self.chars.as_mut().and_then(|ref mut chars| chars.next()).or_else(|| {
        self.chars = self.pages.next().map(|page| page.data.as_slice().chars());
        self.chars.as_mut().and_then(|ref mut chars| chars.next()) })
    }
  }
}

/*
 * LineIterator iterates the lines of a page tree, yielding a CharIterator for
 * each line.
 */
struct LineIterator<'l> {
  tree: &'l PageTree,
  next_line: uint
}

impl<'l> LineIterator<'l> {
  fn new(tree: &'l PageTree) -> LineIterator {
    LineIterator { tree: tree, next_line: 0 }
  }
}

impl<'l> Iterator for LineIterator<'l> {
  type Item = CharIterator<'l>;

  fn next(&mut self) -> Option<CharIterator<'l>> {
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
  length: uint,
  newline_offsets: Vec<uint>,  // cached offsets to newlines within the page
}

impl Page {
  fn new(data: String) -> Page {
    assert!(data.len() <= MAX_PAGE_SIZE);
    let mut page = Page { data: data, length: 0, newline_offsets: Vec::new() };
    page.update_caches();
    return page;
  }

  fn insert_string_at_offset(&mut self, string: String, offset: uint) {
    assert!(offset <= self.data.chars().count());
    let byte_offset = self.data.as_slice().slice_chars(0, offset).len() as int;
    let original_size = self.data.len();
    let string_size = string.len();
    self.data.push_str(string.as_slice());  // first grow organically
    unsafe {
      let bytes_mut = self.data.as_mut_vec().as_mut_ptr();
      // make place by shifting some original data to the side
      ptr::copy(bytes_mut.offset(byte_offset + string_size as int),
                self.data.as_bytes().as_ptr().offset(byte_offset),
                original_size - byte_offset as uint);
      // plunk that chunk in there
      ptr::copy(bytes_mut.offset(byte_offset),
                string.as_bytes().as_ptr(),
                string_size);
    }
    self.update_caches();
  }

  fn update_caches(&mut self) {
    self.length = self.data.chars().count();
    self.newline_offsets.clear();
    let mut offset = 0;
    for character in self.data.as_slice().chars() {
      if character == '\n' { self.newline_offsets.push(offset); }
      offset += 1;
    }
  }

  fn offset_of_newline(&self, newline: uint) -> Option<uint> {
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
  chunk_size: uint,
}

impl StringChunkerator {
  fn new(string: String, chunk_size: uint) -> StringChunkerator {
    StringChunkerator { data: string.into_bytes(), chunk_size: chunk_size }
  }
}

impl Iterator for StringChunkerator {
  type Item = String;

  fn next(&mut self) -> Option<String> {
    // cut out a chunk of |self.chunk_size| bytes and hope it's a valid string
    let mut chunk: Vec<u8> =
      self.data.iter().take(self.chunk_size).map(|&b| b).collect();
    self.data = self.data.iter().skip(self.chunk_size).map(|&b| b).collect();
    while self.data.len() > 0 || chunk.len() > 0 {
      chunk = match String::from_utf8(chunk) {
        Ok(good_chunk) => return Some(good_chunk),
        Err(err)       => {
          assert_eq!(err.utf8_error(), ::std::str::Utf8Error::TooShort);
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
  fn raw_data_to_utf8_string(data: &[u8]) -> (String, uint) {
    let mut string = String::from_utf8_lossy(data).into_owned();
    let mut num_truncated_bytes = 0;
    // adjust for multi-byte code-points spanning page boundaries
    let replacement_char = '\u{FFFD}';
    let string_len = string.len();
    if string.as_slice().char_at_reverse(string_len - 1) == replacement_char {
      string.truncate(string_len - replacement_char.len_utf8());
      num_truncated_bytes = data.len() - string.len();
    }
    return (string, num_truncated_bytes);
  }
}

impl Iterator for PageStream {
  type Item = Page;

  fn next(&mut self) -> Option<Page> {
    use std::io::SeekFrom;
    let mut data = Box::new([0; PAGE_SIZE]);
    self.file.read(data.as_mut_slice()).
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
pub enum BufferError {
  IoError(io::Error),
  NoPath,
}

impl fmt::Display for BufferError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", *self)
  }
}

impl error::Error for BufferError {
  fn description(&self) -> &str {
    match *self {
      BufferError::IoError(ref err) => err.description(),
      BufferError::NoPath           => "The buffer had no path.",
    }
  }
}

type BufferResult<T> = Result<T, BufferError>;

/*
 * The buffer is used to open, modify and write files back to disk.
 */
pub struct Buffer {
  path: Option<PathBuf>,
  tree: PageTree,
}

impl Buffer {
  pub fn new() -> Buffer {
    let mut buffer = Buffer { path: None, tree: PageTree::new() };
    buffer.insert_at_offset(String::from_str("\n"), 0);
    return buffer;
  }

  pub fn open(path: &Path) -> BufferResult<Buffer> {
    PageStream::new(path).
    and_then(PageTree::build).
    and_then(|tree| Ok(Buffer { path: Some(path.to_path_buf()), tree: tree })).
    map_err(|io_err| BufferError::IoError(io_err))
  }

  pub fn write(&self) -> BufferResult<()> {
    self.path.as_ref().
    map_or(Err(BufferError::NoPath), |path| self.write_to(path))
  }

  pub fn write_to(&self, path: &Path) -> BufferResult<()> {
    File::create(path).
    and_then(|mut file|
      self.tree.iter().
      map(|page| file.write_all(page.data.as_bytes().as_slice())).
      fold(Ok(()),
        |ok, err| if ok.is_ok() && err.is_err() { err } else { ok })).
    map_err(|io_err| BufferError::IoError(io_err))
  }

  pub fn insert_at_offset(&mut self, string: String, mut offset: uint) {
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

  pub fn get_char_by_line_column(&self, line: uint, column: uint)
      -> Option<char> {
    self.tree.get_char_by_line_column(line, column)
  }

  // if the file doesn't end with a newline, the characters after the last
  // newline will still be counted as just another line
  pub fn num_lines(&self) -> uint {
    if self.tree.length == 0 { 0 } else {
      let borked_last_line = self.tree.get_char_by_offset(self.tree.length - 1).
                             map(|c| c != '\n').unwrap();
      self.tree.newlines + if borked_last_line { 1 } else { 0 }
    }
  }

  // excludes newline character from the count if the line has one
  pub fn line_length(&self, line: uint) -> Option<uint> {
    if self.tree.length == 0 { None } else {
      self.tree.line_start_and_end_offset(line).
      map(|(start_offset, end_offset)| {
        assert!(start_offset < end_offset);
        let borked_line = self.tree.get_char_by_offset(end_offset - 1).
                          map(|c| c != '\n').unwrap();
        end_offset - start_offset - if borked_line { 0 } else { 1 } })
    }
  }

  pub fn line_iter(&self) -> LineIterator {
    LineIterator::new(&self.tree)
  }
}

#[cfg(test)]
mod test {
  use std::fs::File;
  use std::io;
  use std::num::SignedInt;
  use std::path::Path;

  // Opens a buffer (new or loaded file), performs some operation on it,
  // dumps buffer content to disk, compares results to expectations.
  // Also throws in a balance check on the resulting page tree, because why not.
  fn buffer_test<O, M: ?Sized>(test: &String, operation: O, make_buffer: Box<M>)
      where O: Fn(&mut super::Buffer) -> (),
            M: Fn() -> super::BufferResult<super::Buffer> {
    use std::error::Error;
    use std::io::Read;
    let result_path_string = format!("tests/buffer/{}-result.txt", test);
    let result_path = Path::new(result_path_string.as_slice());
    let expect_path_string = format!("tests/buffer/{}-expect.txt", test);
    let expect_path = Path::new(expect_path_string.as_slice());

    let result = make_buffer().
      map(|mut buffer| { operation(&mut buffer); buffer }).
      map(|buffer| { assert!(is_balanced(&buffer.tree)); buffer }).
      and_then(|buffer| buffer.write_to(&result_path));

    let file_contents = |&: path| File::open(path).and_then(|mut file| {
      let mut content = String::new();
      try!(file.read_to_string(&mut content));
      Ok(content) });

    let result_content = file_contents(&result_path);
    let expect_content = file_contents(&expect_path);

    match (result_content, result, expect_content) {
      (Err(err),   _,        _         ) => panic!("{}", err.description()),
      (_,          Err(err), _         ) => panic!("{}", err.description()),
      (_,          _,        Err(err)  ) => panic!("{}", err.description()),
      (Ok(result), Ok(_),    Ok(expect)) => assert_eq!(result, expect),
    };
  }

  #[test]
  fn write_without_path() {
    assert!(super::Buffer::new().write().is_err());
  }

  macro_rules! buffer_test {
    ($name:ident, $new_file:expr, $operation:expr) => (
      #[test]
      fn $name() {
        let test = String::from_str(stringify!($name));
        let buffer_maker: Box<Fn() -> super::BufferResult<super::Buffer>> =
          if $new_file { Box::new(|&:| Ok(super::Buffer::new())) }
          else { Box::new(|&:| {
            let test_path_string = format!("tests/buffer/{}.txt", &test);
            let test_path = Path::new(test_path_string.as_slice());
            return super::Buffer::open(&test_path);
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
        let mut tree = super::PageTree::new();
        for _ in range(0, $num_pages) {
          tree.$fun(super::Page::new(String::from_str("a")));
        }
        assert!(is_balanced(&tree));
      }
    );
  }

  simple_balance_test!(balanced_append_32, append_page, 32u);
  simple_balance_test!(balanced_append_33, append_page, 33u);
  simple_balance_test!(balanced_append_9001, append_page, 9001u);
  simple_balance_test!(balanced_prepend_32, prepend_page, 32u);
  simple_balance_test!(balanced_prepend_33, prepend_page, 33u);
  simple_balance_test!(balanced_prepend_9001, prepend_page, 9001u);

  macro_rules! balance_test {
    ($name:ident, $num_pages:expr) => (
      #[test]
      fn $name() {
        let mut tree = super::PageTree::new();
        let denominator = 4u;
        let mut numerator = 0u;
        for i in range(0, $num_pages) {
          let page = super::Page::new(String::from_str("abc"));
          let fraction = (numerator as f32) / (denominator as f32);
          let offset = ((i as f32) * fraction) as uint * page.length;
          tree.insert_page_at_offset(page, offset);
          numerator = (numerator + 1) % (denominator + 1);
        }
        assert!(is_balanced(&tree));
      }
    );
  }

  balance_test!(balanced_insert_32, 32u);
  balance_test!(balanced_insert_33, 33u);
  balance_test!(balanced_insert_9001, 9001u);

  fn is_balanced(tree: &super::PageTree) -> bool {
    let branch_is_balanced = |&: branch: &super::PageTreeNode| {
      match branch {
        &super::PageTreeNode::Tree(ref tree) => is_balanced(&**tree),
        _                                    => true,
      }
    };
    let left_height = tree.left.height() as int;
    let right_height = tree.right.height() as int;
    return (left_height - right_height).abs() < 2 &&
      branch_is_balanced(&tree.left) && branch_is_balanced(&tree.right);
  }

  buffer_test!(existing_file_insert, false, existing_file_insert_operation);
  buffer_test!(new_file_insert, true, new_file_insert_operation);
  buffer_test!(page_split_utf8_insert, false, page_split_utf8_insert_operation);
  buffer_test!(long_string_insert, true, long_string_insert_operation);

  fn existing_file_insert_operation(buffer: &mut super::Buffer) {
    buffer.insert_at_offset(String::from_str("more"), 0);
    buffer.insert_at_offset(String::from_str(" than "), 4);
    buffer.insert_at_offset(String::from_str("."), 25);
    let buffer_end = buffer.tree.length - 1;
    buffer.insert_at_offset(String::from_str(" and then some"), buffer_end);
  }

  fn new_file_insert_operation(buffer: &mut super::Buffer) {
    buffer.insert_at_offset(String::from_str("Here's a second line"), 1);
    buffer.insert_at_offset(String::from_str(" with a newline\n"), 21);
    buffer.insert_at_offset(String::from_str("First line go here"), 0);
    buffer.insert_at_offset(String::from_str(", and it even has a dot."), 18);
  }

  fn page_split_utf8_insert_operation(buffer: &mut super::Buffer) {
    buffer.insert_at_offset(String::from_str("boop"), 5);
    buffer.insert_at_offset(String::from_str("boop"), 22);
    buffer.insert_at_offset(String::from_str("boop"), 36);
  }

  fn long_string_insert_operation(buffer: &mut super::Buffer) {
    buffer.insert_at_offset(String::from_str(
      include_str!("../tests/buffer/long_string_insert.txt")), 1);
  }

  #[test]
  fn line_column_to_offset_test() {
    let tests = [
       ((0u, 0u), Some(0u)), ((0u, 15u), Some(15u)), ((0u, 16u), None),
       ((1u, 15u), Some(31u)), ((1u, 28u), Some(44u)),
       ((5u, 0u), Some(51u)), ((5u, 1u), None),
       ((7u, 0u), Some(53u)),
       ((8u, 0u), Some(62u)), ((8u, 1u), None),
       ((9u, 0u), None), ((9u, 1u), None)
    ];

    let test_path = Path::new("tests/buffer/line_column_offset.txt");
    let buffer = super::Buffer::open(&test_path).unwrap();
    for &((line, column), expected_offset) in tests.iter() {
      assert_eq!(buffer.tree.line_column_to_offset(line, column),
                 expected_offset);
    }
  }

  #[test]
  fn get_char_by_line_column_test() {
    let tests = [
      ((0u, 0u), Some('a')), ((0u, 15u), Some('\n')), ((0u, 16u), None),
      ((1u, 15u), Some('p')), ((1u, 28u), Some('ö')),
      ((5u, 0u), Some('\n')), ((5u, 1u), None),
      ((7u, 0u), Some('2')),
      ((8u, 0u), None), ((8u, 1u), None),
      ((9u, 0u), None), ((9u, 1u), None)
    ];

    let test_path = Path::new("tests/buffer/line_column_offset.txt");
    let buffer = super::Buffer::open(&test_path).unwrap();
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

  fn line_length_test(path: &Path, expect: &[uint]) {
    let buffer = super::Buffer::open(path).unwrap();
    assert_eq!(buffer.num_lines(), expect.len());
    for line in range(0, buffer.num_lines()) {
      assert_eq!(buffer.line_length(line).unwrap(), expect[line]);
    }
  }

  #[test]
  fn line_iterator_yields_all_lines() {
    let path = Path::new("tests/buffer/lacking_newline.txt");
    let buffer = super::Buffer::open(&path).unwrap();
    assert_eq!(buffer.line_iter().count(), buffer.num_lines());
  }

  #[test]
  fn line_iterator_yields_correct_line_lengths() {
    let path = Path::new("tests/buffer/long_string_insert.txt");
    let buffer = super::Buffer::open(&path).unwrap();
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
    let buffer = super::Buffer::open(&path).unwrap();
    let mut offset = 0;
    for chars in buffer.line_iter() {
      for character in chars {
        assert_eq!(buffer.tree.get_char_by_offset(offset), Some(character));
        offset += 1;
      }
    }
    assert_eq!(offset, buffer.tree.length);
  }
}
