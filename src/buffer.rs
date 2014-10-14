/*
 * Copyright (c) 2014 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::cmp;
use std::io::{File, IoError, IoResult};
use std::mem;
use std::num;

#[cfg(not(test))]
static PAGE_SIZE: uint = 1024;  // ish
#[cfg(test)]
static PAGE_SIZE: uint = 16;

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
      Leaf(ref page) => page.newlines,
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

  fn build(mut stream: PageStream) -> IoResult<PageTree> {
    let mut tree = PageTree::new();
    for page in stream { tree.append_page(page); }
    return stream.error.map_or(Ok(tree), |err| Err(err));
  }

  fn prepend_page(&mut self, page: Page) {
    self.insert_page_at_offset(page, 0);
  }

  fn append_page(&mut self, page: Page) {
    let offset = self.length;
    self.insert_page_at_offset(page, offset);
  }

  fn insert_page_at_offset(&mut self, page: Page, offset: uint) {
    assert!(offset <= self.length)

    // insert the page in the appropriate branch
    {
      let (go_left, new_offset) = self.decide_branch_by_offset(offset);
      let (branch, other) = if go_left { (&mut self.left, &mut self.right) }
                            else       { (&mut self.right, &mut self.left) };

      match branch {
        &Nil                => *branch = Leaf(page),
        &Tree(ref mut tree) => tree.insert_page_at_offset(page, new_offset),
        &Leaf(_)            => {
          // this offset must be on a page boundary, no splitting of pages
          assert!(new_offset == 0 || new_offset == branch.length())
          // make a new subtree only if necessary
          if other.is_nil() {
            // put page in the other branch and swap it to where it wants to be
            *other = Leaf(page);
            mem::swap(branch, other);
          }
          else {
            let mut new_subtree = box PageTree::new();
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
    let height_diff = num::abs(left_height as int - right_height as int);

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
        &Nil | &Leaf(_)    => assert!(false),
        &Tree(ref mut mid) => {
          let left_height = mid.left.height();
          let right_height = mid.right.height();
          assert!(left_height != right_height);
          let then_go_left = left_height > right_height;
          match if then_go_left { &mut mid.left } else { &mut mid.right } {
            &Nil | &Leaf(_)    => assert!(false),
            &Tree(ref mut low) => {
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
      balanced_tree.left = Tree(box left);
      balanced_tree.right = Tree(box right);
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

  fn find_page_by_offset<'l>(&'l self, offset: uint) -> Option<&'l Page> {
    if offset >= self.length { return None; }
    let (go_left, new_offset) = self.decide_branch_by_offset(offset);
    match if go_left { &self.left } else { &self.right } {
      &Nil            => None,
      &Tree(ref tree) => tree.find_page_by_offset(new_offset),
      &Leaf(ref page) => Some(&*page),
    }
  }

  fn decide_branch_by_offset(&self, offset: uint) -> (bool, uint) {
    let left_length = self.left.length();
    let go_left = left_length == 0 || offset < left_length;
    return (go_left, if go_left { offset } else { offset - left_length });
  }

  fn iter(&self) -> PageTreeIterator {
    PageTreeIterator::new(self)
  }
}

/*
 * PageTreeIterator iterates the pages of a page tree from left to right.
 */
struct PageTreeIterator<'l> {
  tree: &'l PageTree,
  offset: uint,
}

impl<'l> PageTreeIterator<'l> {
  fn new(tree: &'l PageTree) -> PageTreeIterator<'l> {
    PageTreeIterator { tree: tree, offset: 0 }
  }
}

impl<'l> Iterator<&'l Page> for PageTreeIterator<'l> {
  fn next(&mut self) -> Option<&'l Page> {
    self.tree.find_page_by_offset(self.offset).map(
      |page| { self.offset += page.length; page })
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
  newlines: uint,
}

impl Page {
  fn new(data: String) -> Page {
    let length = data.as_slice().len();
    let newlines = match data.as_slice().lines().count() {
      0 => 0,
      line_count => line_count - 1,
    };
    Page { data: data, length: length, newlines: newlines }
  }
}

/*
 * PageStream is an iterator which takes a file path and spits out pages until
 * all the file content has been consumed.
 */
struct PageStream {
  file: File,
  error: Option<IoError>,
}

impl PageStream {
  fn new(path: &Path) -> IoResult<PageStream> {
    File::open(path).and_then(|f| Ok(PageStream { file: f, error: None }))
  }

  // Assumes |data| is well formed utf8 with the possible exception of a broken
  // last character
  fn raw_data_to_utf8_string(data: &[u8]) -> (String, uint) {
    let mut string = String::from_utf8_lossy(data).into_string();
    let mut num_truncated_bytes = 0;
    // adjust for multi-byte code-points spanning page boundaries
    let replacement_char = '\uFFFD';
    let string_len = string.len();
    if string.as_slice().char_at_reverse(string_len - 1) == replacement_char {
      string.truncate(string_len - replacement_char.len_utf8_bytes());
      num_truncated_bytes = data.len() - string.len();
    }
    return (string, num_truncated_bytes);
  }
}

impl Iterator<Page> for PageStream {
  fn next(&mut self) -> Option<Page> {
    use std::io::{EndOfFile, SeekCur};
    let mut data = box [0, ..PAGE_SIZE];
    let result = self.file.read(*data).
      map(|bytes| PageStream::raw_data_to_utf8_string((*data)[0..bytes])).
      and_then(|(string, num_truncated_bytes)|
        match self.file.seek(-(num_truncated_bytes as i64), SeekCur) {
          Err(err) => Err(err),
          Ok(())   => Ok(Page::new(string)),
        });

    match result {
      Ok(page) => return Some(page),
      Err(err) => {
        self.error = if err.kind == EndOfFile { None } else { Some(err) };
        return None;
      }
    };
  }
}

/*
 * The buffer is used to open, modify and write files back to disk.
 */
pub struct Buffer {
  path: Option<Path>,
  tree: PageTree,
}

impl Buffer {
  pub fn open(path: &Path) -> IoResult<Buffer> {
    PageStream::new(path).and_then(PageTree::build).and_then(
      |tree| Ok(Buffer { path: Some(path.clone()), tree: tree }))
  }

  pub fn write(&self) -> IoResult<()> {
    self.path.as_ref().map_or(
      Err(Buffer::no_path_error()), |path| self.write_to(path))
  }

  pub fn write_to(&self, path: &Path) -> IoResult<()> {
    use std::io::{Truncate, Write};
    File::open_mode(path, Truncate, Write).and_then(|mut file| self.tree.iter().
      map(|page| file.write(page.data.as_bytes().as_slice())).
      fold(Ok(()), |ok, err| if ok.is_ok() && err.is_err() { err } else { ok }))
  }

  fn no_path_error() -> IoError {
    use std::io::OtherIoError;
    IoError { kind: OtherIoError, desc: "no path specified", detail: None }
  }
}

mod testing {
  use std::io::{File, IoResult};
  use std::num;

  macro_rules! load_and_dump_test(
    ($fun:ident) => (
      #[test]
      fn $fun() {
        load_and_dump(&Path::new(
          format!("tests/buffer/{}.txt", stringify!($fun))));
      }
    );
  )

  load_and_dump_test!(utf8_one_byte_overflow)
  load_and_dump_test!(utf8_two_byte_overflow)
  load_and_dump_test!(one_full_page)
  load_and_dump_test!(multiple_pages)

  fn load_and_dump(test: &Path) {
    let content_before = file_contents(test);
    let result = super::Buffer::open(test).and_then(|buf| buf.write());
    let content_after = file_contents(test);

    match (content_before, result, content_after) {
      (Err(err),   _,        _        ) => fail!(err.desc),
      (_,          Err(err), _        ) => fail!(err.desc),
      (_,          _,        Err(err) ) => fail!(err.desc),
      (Ok(before), Ok(_),    Ok(after)) => assert!(before == after),
    };
  }

  fn file_contents(test: &Path) -> IoResult<String> {
    File::open(test).and_then(|mut file| file.read_to_string())
  }

  macro_rules! simple_balance_test(
    ($name:ident, $fun:ident, $num_pages:expr) => (
      #[test]
      fn $name() {
        let mut tree = super::PageTree::new();
        for _ in range(0, $num_pages) {
          tree.$fun(super::Page::new(String::from_str("a")));
        }
        assert!(is_balanced(&tree))
      }
    );
  )

  simple_balance_test!(balanced_append_32, append_page, 32u)
  simple_balance_test!(balanced_append_33, append_page, 33u)
  simple_balance_test!(balanced_append_9001, append_page, 9001u)
  simple_balance_test!(balanced_prepend_32, prepend_page, 32u)
  simple_balance_test!(balanced_prepend_33, prepend_page, 33u)
  simple_balance_test!(balanced_prepend_9001, prepend_page, 9001u)

  macro_rules! balance_test(
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
        assert!(is_balanced(&tree))
      }
    );
  )

  balance_test!(balanced_insert_32, 32u)
  balance_test!(balanced_insert_33, 33u)
  balance_test!(balanced_insert_9001, 9001u)

  fn is_balanced(tree: &super::PageTree) -> bool {
    let branch_is_balanced = |branch: &super::PageTreeNode| {
      match branch {
        &super::Nil | &super::Leaf(_) => true,
        &super::Tree(ref tree)        => is_balanced(&**tree),
      }
    };
    let left_height = tree.left.height() as int;
    let right_height = tree.right.height() as int;
    return num::abs(left_height - right_height) < 2 &&
      branch_is_balanced(&tree.left) && branch_is_balanced(&tree.right);
  }
}
