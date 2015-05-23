/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate unicode_width;

use std::cmp;
use std::ops::Add;

use self::unicode_width::UnicodeWidthChar as CharWidth;

use buffer;

/*
 * LineUp/Down: move caret a line up or down while trying to preserve the
 *   screen space column
 * CharNext/Prev: move forward/backward on a line
 * Char*Flat: like CharNext/Prev but treating the file as "flat" (disregarding
 *   line breaks)
 * CharNextAppending: like CharNext but allows an extra column, effectively
 *   standing on the newline character
 * Set: just set a position
 * WeakSet: like Set but preserving a saved column
 * Clamp: clamps first to a valid line then to a valid column on that line
 */
#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(test, derive(Debug))]
pub enum Adjustment {
  LineUp,
  LineDown,
  CharNext,
  CharNextFlat,
  CharNextAppending,
  CharPrev,
  CharPrevFlat,
  Set(usize, usize),
  WeakSet(usize, usize),
  Clamp,
}

/*
 * While the caret is in buffer coordinates, the saved column is in screen cell
 * coordinates.
 */
#[derive(Clone, Copy)]
pub struct Caret {
  line: usize,
  column: usize,
  saved_column: Option<usize>,
}

impl Caret {
  pub fn new() -> Caret {
    Caret { line: 0, column: 0, saved_column: None }
  }

  pub fn line(&self) -> usize {
    self.line
  }

  pub fn column(&self) -> usize {
    self.column
  }

  // some adjustments may assume that the caret is in a valid position
  pub fn adjust(&mut self, adjustment: Adjustment, buffer: &buffer::Buffer) {
    let clamp = |val, max| cmp::min(val, cmp::max(0, max) as usize);
    let clamped_column = |line, column, buffer: &buffer::Buffer|
      clamp(column, buffer.line_length(line).unwrap_or(0) as isize - 1);
    let clamped_column_appending = |line, column, buffer: &buffer::Buffer|
      clamp(column, buffer.line_length(line).unwrap_or(0) as isize);
    let (line, column) = (self.line, self.column);
    let (new_line, new_column, new_saved_column) = match adjustment {
      Adjustment::CharPrev              =>
        (line, cmp::max(0, column as isize - 1) as usize, None),
      Adjustment::CharNext              =>
        (line, clamped_column(line, column + 1, buffer), None),
      Adjustment::CharNextAppending     =>
        (line, clamped_column_appending(line, column + 1, buffer), None),
      Adjustment::CharPrevFlat          => {
        let (line, column) =
          if self.column == 0 && self.line == 0 { (0, 0) }
          else if self.column > 0 { (self.line, self.column - 1) }
          else {
            let line = self.line - 1;
            buffer.line_length(line).map(|line_len| (line, line_len)).
            unwrap_or((self.line, self.column))
          };
        (line, column, None)
      }
      Adjustment::CharNextFlat          => {
        let is_last_line = self.line + 1 == buffer.num_lines();
        let (line, column) =
          buffer.line_length(self.line).map(|line_len|
            if self.column < line_len { (self.line, self.column + 1) }
            else if is_last_line { (self.line, self.column) }
            else { (self.line + 1, 0) }).
          unwrap_or((self.line, self.column));
        (line, column, None)
      }
      Adjustment::LineUp                =>
        if line == 0 { (line, column, self.saved_column) }
        else { self.vertical_caret_movement(line, line - 1, buffer) },
      Adjustment::LineDown              => {
        let max_line = cmp::max(0, buffer.num_lines() as isize - 1) as usize;
        if line == max_line { (line, column, self.saved_column) }
        else { self.vertical_caret_movement(line, line + 1, buffer) }
      }
      Adjustment::Set(line, column)     => (line, column, None),
      Adjustment::WeakSet(line, column) => (line, column, self.saved_column),
      Adjustment::Clamp                 => {
        let line = clamp(self.line, buffer.num_lines() as isize - 1);
        (line, clamped_column(line, self.column, buffer), self.saved_column)
      }
    };
    if line != new_line || column != new_column {
      self.line = new_line;
      self.column = new_column;
      self.saved_column = new_saved_column;
    }
  }

  // helper function to adjust, restricts the caret column to valid
  // character positions in screen space
  fn vertical_caret_movement(&self, from_line: usize, to_line: usize,
                             buffer: &buffer::Buffer)
      -> (usize, usize, Option<usize>) {
    // find maximum column in screen space
    let to_line_length = buffer.line_length(to_line).unwrap();
    let to_line_screen_length =
      buffer_to_screen_column(to_line, to_line_length, buffer);
    let max_column = cmp::max(0, to_line_screen_length as isize - 1) as usize;
    // find where we want to be on the next line in screen space
    let current_screen_column =
      buffer_to_screen_column(from_line, self.column, buffer);
    let desired_column = self.saved_column.
      map(|saved_column| cmp::max(saved_column, current_screen_column)).
      unwrap_or(current_screen_column);
    // clamp it to maximum and go back to buffer space
    let screen_column = cmp::min(max_column, desired_column);
    let buffer_column =
      screen_to_buffer_column(to_line, screen_column, buffer).unwrap();
    // determine whether to save the desired column
    let final_screen_column =
      buffer_to_screen_column(to_line, buffer_column, buffer);
    let saved_column = if final_screen_column >= desired_column { None }
                       else { Some(desired_column) };
    return (to_line, buffer_column, saved_column);
  }
}

// sums up the widths of the characters before the given buffer column
pub fn buffer_to_screen_column(line: usize, column: usize,
                               buffer: &buffer::Buffer) -> usize {
  buffer.line_iter().from(line).next().map(|chars|
    chars.take(column).map(|c| CharWidth::width(c).unwrap_or(0)).
                       fold(0, Add::add)).
  unwrap_or(0)
}

// scans a line, counting characters up to the given screen column
pub fn screen_to_buffer_column(row: usize, screen_column: usize,
                               buffer: &buffer::Buffer) -> Option<usize> {
  buffer.line_iter().from(row).next().map(|chars|
    chars.filter(|&c| c != '\n').scan(0, |sum, c| {
      *sum += CharWidth::width(c).unwrap_or(0);
      Some(*sum) }).
    take_while(|&sum| sum <= screen_column).count())
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use buffer;

  #[test]
  fn adjust() {
    let buffer = buffer::Buffer::open(
      &Path::new("tests/caret/hokey_pokey_caret.txt")).unwrap();
    let mut caret = super::Caret::new();
    // move to empty line
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 1); assert_eq!(caret.column, 0);
    assert!(caret.saved_column.is_none());
    // move to end of double width character then back again
    caret.adjust(super::Adjustment::Set(3, 3), &buffer);
    assert_eq!(caret.line, 3); assert_eq!(caret.column, 3);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 4); assert_eq!(caret.column, 1);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert_eq!(caret.line, 3); assert_eq!(caret.column, 3);
    assert!(caret.saved_column.is_none());
    // move to shorter lines then back again
    caret.adjust(super::Adjustment::Set(6, 30), &buffer);
    assert_eq!(caret.line, 6); assert_eq!(caret.column, 30);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 7); assert_eq!(caret.column, 14);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 8); assert_eq!(caret.column, 20);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert_eq!(caret.line, 6); assert_eq!(caret.column, 30);
    assert!(caret.saved_column.is_none());
    // move to shorter line, step sideways, then back again
    caret.adjust(super::Adjustment::Set(10, 75), &buffer);
    assert_eq!(caret.line, 10); assert_eq!(caret.column, 75);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 11); assert_eq!(caret.column, 68);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::CharPrev, &buffer);
    assert_eq!(caret.line, 11); assert_eq!(caret.column, 67);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::CharNext, &buffer);
    assert_eq!(caret.line, 11); assert_eq!(caret.column, 68);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert_eq!(caret.line, 10); assert_eq!(caret.column, 68);
    assert!(caret.saved_column.is_none());
    // move to end of line lacking newline
    caret.adjust(super::Adjustment::Set(13, 34), &buffer);
    assert_eq!(caret.line, 13); assert_eq!(caret.column, 34);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 14); assert_eq!(caret.column, 34);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::CharNext, &buffer);
    assert_eq!(caret.line, 14); assert_eq!(caret.column, 34);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::CharNextAppending, &buffer);
    assert_eq!(caret.line, 14); assert_eq!(caret.column, 35);
    assert!(caret.saved_column.is_none());
  }

  #[test]
  fn weak_set() {
    let insertion = "somewhat lengthy string".to_string();
    let mut buffer = buffer::Buffer::open(
      &Path::new("tests/caret/hokey_pokey_caret.txt")).unwrap();
    let mut caret = super::Caret::new();
    // move to shorter line, weak set left, then move up again
    caret.adjust(super::Adjustment::Set(6, 20), &buffer);
    assert_eq!(caret.line, 6); assert_eq!(caret.column, 20);
    assert!(caret.saved_column.is_none());
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 7); assert_eq!(caret.column, 14);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::WeakSet(7, 10), &buffer);
    assert_eq!(caret.line, 7); assert_eq!(caret.column, 10);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert_eq!(caret.line, 6); assert_eq!(caret.column, 20);
    assert!(caret.saved_column.is_none());
    // move to shorter line, lengthen it, weak set right, then move up again
    caret.adjust(super::Adjustment::LineDown, &buffer);
    assert_eq!(caret.line, 7); assert_eq!(caret.column, 14);
    assert!(caret.saved_column.is_some());
    buffer.insert_at_line_column(insertion, 7, 5).unwrap();
    caret.adjust(super::Adjustment::WeakSet(7, 30), &buffer);
    assert_eq!(caret.line, 7); assert_eq!(caret.column, 30);
    assert!(caret.saved_column.is_some());
    caret.adjust(super::Adjustment::LineUp, &buffer);
    assert_eq!(caret.line, 6); assert_eq!(caret.column, 30);
    assert!(caret.saved_column.is_none());
  }

  #[test]
  fn adjust_flat() {
    let buffer = buffer::Buffer::open(
      &Path::new("tests/caret/hokey_pokey_caret.txt")).unwrap();
    let mut caret = super::Caret::new();
    // try backing out of file
    caret.adjust(super::Adjustment::CharPrevFlat, &buffer);
    assert_eq!(caret.line, 0); assert_eq!(caret.column, 0);
    assert!(caret.saved_column.is_none());
    // move forward
    caret.adjust(super::Adjustment::CharNextFlat, &buffer);
    assert_eq!(caret.line, 0); assert_eq!(caret.column, 1);
    assert!(caret.saved_column.is_none());
    // move forward to next line
    caret.adjust(super::Adjustment::Set(0, 18), &buffer);
    caret.adjust(super::Adjustment::CharNextFlat, &buffer);
    assert_eq!(caret.line, 1); assert_eq!(caret.column, 0);
    assert!(caret.saved_column.is_none());
    // move back again
    caret.adjust(super::Adjustment::CharPrevFlat, &buffer);
    assert_eq!(caret.line, 0); assert_eq!(caret.column, 18);
    assert!(caret.saved_column.is_none());
    // move back
    caret.adjust(super::Adjustment::CharPrevFlat, &buffer);
    assert_eq!(caret.line, 0); assert_eq!(caret.column, 17);
    assert!(caret.saved_column.is_none());
    // try skipping out ot file
    caret.adjust(super::Adjustment::Set(14, 35), &buffer);
    caret.adjust(super::Adjustment::CharNextFlat, &buffer);
    assert_eq!(caret.line, 14); assert_eq!(caret.column, 35);
    assert!(caret.saved_column.is_none());
  }
}
