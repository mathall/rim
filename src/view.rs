/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::cmp;
use std::iter::AdditiveIterator;

use buffer;
use screen;

const MIN_VIEW_SIZE: u16 = 1;

pub enum CaretMovement {
  LineUp,
  LineDown,
  CharNext,
  CharPrev,
}

/*
 * View handles the caret and presentation of a buffer.
 * Scroll and size are both measured in screen cell coordinates. While the caret
 * is in buffer coordinates, the saved column is in screen coordinates.
 */
pub struct View {
  scroll_line: uint,
  scroll_column: uint,
  size: screen::Size,
  caret_line: uint,
  caret_column: uint,
  saved_column: Option<uint>,
}

impl View {
  pub fn new() -> View {
    View {
      scroll_line: 0,
      scroll_column: 0,
      size: screen::Size(MIN_VIEW_SIZE, MIN_VIEW_SIZE),
      caret_line: 0,
      caret_column: 0,
      saved_column: None,
    }
  }

  fn scroll_into_view(&mut self, line: uint, column: uint,
      buffer: &buffer::Buffer) {
    let screen::Size(rows, cols) = self.size;
    let rows = rows as uint;
    let cols = cols as uint;

    self.scroll_line =
      if line < self.scroll_line { line }
      else if line >= self.scroll_line + rows { line - rows + 1 }
      else { self.scroll_line };

    // make sure wider characters are scrolled in entirely as well
    let character = buffer.get_char_by_line_column(line, column).unwrap();
    let start = buffer_to_screen_column(line, column, buffer);
    let end = start + CharExt::width(character, false).unwrap_or(1) - 1;
    self.scroll_column =
      if start < self.scroll_column { start }
      else if end >= self.scroll_column + cols { end - cols + 1 }
      else { self.scroll_column };
  }

  // Moves the caret and preserves its old screen space column when moving to a
  // line that can't have the caret at the same column.
  pub fn move_caret(&mut self, movement: CaretMovement,
      buffer: &buffer::Buffer) {
    let (line, column) = (self.caret_line, self.caret_column);
    let (new_line, new_column, new_saved_column) = match movement {
      CaretMovement::CharPrev =>
        (line, cmp::max(0, column as int - 1) as uint, None),
      CaretMovement::CharNext => {
        let line_length = buffer.line_length(line).unwrap();
        let max_column = cmp::max(0, line_length as int - 1) as uint;
        (line, cmp::min(max_column, column + 1), None)
      }
      CaretMovement::LineUp   =>
        if line == 0 { (line, column, self.saved_column) }
        else { self.vertical_caret_movement(line, line - 1, buffer) },
      CaretMovement::LineDown => {
        let max_line = cmp::max(0, buffer.num_lines() as int - 1) as uint;
        if line == max_line { (line, column, self.saved_column) }
        else { self.vertical_caret_movement(line, line + 1, buffer) }
      }
    };
    if line != new_line || column != new_column {
      self.caret_line = new_line;
      self.caret_column = new_column;
      self.saved_column = new_saved_column;
      self.scroll_into_view(new_line, new_column, buffer);
    }
  }

  // helper function to move_caret, restricts the caret column to valid
  // character positions in screen space
  fn vertical_caret_movement(&self, from_line: uint, to_line: uint,
      buffer: &buffer::Buffer) -> (uint, uint, Option<uint>) {
    let to_line_length = buffer.line_length(to_line).unwrap();
    let to_line_screen_length =
      buffer_to_screen_column(to_line, to_line_length, buffer);
    let max_column = cmp::max(0, to_line_screen_length as int - 1) as uint;
    let desired_column = self.saved_column.unwrap_or(
      buffer_to_screen_column(from_line, self.caret_column, buffer));
    let screen_column = cmp::min(max_column, desired_column);
    let buffer_column =
      screen_to_buffer_column(to_line, screen_column, buffer).unwrap();
    (to_line, buffer_column,
      if buffer_column == desired_column { None } else { Some(desired_column) })
  }

  pub fn set_size(&mut self, size: screen::Size, buffer: &buffer::Buffer) {
    let screen::Size(rows, cols) = size;
    assert!(rows >= MIN_VIEW_SIZE && cols >= MIN_VIEW_SIZE);
    self.size = size;
    let (line, column) = (self.caret_line, self.caret_column);
    self.scroll_into_view(line, column, buffer);
  }

  #[cfg(not(test))]
  pub fn draw(&self, buffer: &buffer::Buffer, focused: bool,
      position: screen::Cell, screen: &mut screen::Screen) {
    // calculate caret screen position
    let caret_row = (self.caret_line - self.scroll_line) as u16;
    let caret_column = buffer_to_screen_column(
      self.caret_line, self.caret_column, buffer) - self.scroll_column;
    let caret_cell = position + screen::Cell(caret_row, caret_column as u16);

    // helper to put a character on the screen
    let put = |character, cell: screen::Cell, screen: &mut screen::Screen| {
      use screen::Color::*;
      let (fg, bg) = if focused && cell != caret_cell { (Black, White) }
                     else                             { (White, Black) };
      screen.put(cell, character, fg, bg);
    };

    let screen::Size(rows, cols) = self.size;
    // draw line by line
    let mut row: u16 = 0;
    for chars in buffer.line_iter().from(self.scroll_line).take(rows as uint) {
      let line_offset = screen::Cell(row, 0) + position;
      // draw character by character
      let mut col = -(self.scroll_column as int);
      for character in chars {
        if col >= cols as int || character == '\n' { break }
        let char_width = CharExt::width(character, false).unwrap_or(0) as int;
        let end_col = col + char_width;
        if (col < 0 && end_col >= 0) || end_col > cols as int {
          // blank out partially visible characters
          for col in range(cmp::max(0, col), cmp::min(end_col, cols as int)) {
            put(' ', line_offset + screen::Cell(0, col as u16), screen);
          }
        }
        else if col >= 0 {
          put(character, line_offset + screen::Cell(0, col as u16), screen);
        }
        col += char_width;
      }
      // blank out the rest of the row if the line didn't fill it
      for col in range(cmp::max(0, col) as u16, cols) {
        put(' ', line_offset + screen::Cell(0, col), screen);
      }
      row += 1;
    }
    // fill in the rest of the view below the buffer content
    for row in range(row, rows) {
      let line_offset = screen::Cell(row, 0) + position;
      put(if self.scroll_column == 0 { '~' } else { ' ' }, line_offset, screen);
      for col in range(1, cols) {
        put(' ', line_offset + screen::Cell(0, col), screen);
      }
    }
  }
}

// sums up the widths of the characters before the given buffer column
fn buffer_to_screen_column(line: uint, column: uint, buffer: &buffer::Buffer)
    -> uint {
  buffer.line_iter().from(line).next().map(|chars|
    chars.take(column).map(|c| CharExt::width(c, false).unwrap_or(0)).sum()).
  unwrap_or(0)
}

// scans a line, counting characters up to the given screen column
fn screen_to_buffer_column(row: uint, screen_column: uint,
    buffer: &buffer::Buffer) -> Option<uint> {
  buffer.line_iter().from(row).next().map(|chars|
    chars.filter(|&c| c != '\n').scan(0, |sum, c| {
      *sum += CharExt::width(c, false).unwrap_or(0);
      Some(*sum) }).
    take_while(|&sum| sum <= screen_column).count())
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use buffer;
  use screen;

  #[test]
  fn scroll_into_view_double_width() {
    let buffer = buffer::Buffer::open(
      &Path::new("tests/view/scroll_into_view_double_width.txt")).unwrap();
    let mut view = super::View::new();
    view.set_size(screen::Size(1, 15), &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 0);
    view.scroll_into_view(0, 12, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 3);
    view.scroll_into_view(0, 16, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 9);
    view.scroll_into_view(0, 3, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 6);
    view.scroll_into_view(3, 10, &buffer);
    assert_eq!(view.scroll_line, 3); assert_eq!(view.scroll_column, 6);
  }

  #[test]
  fn move_caret() {
    let buffer = buffer::Buffer::open(
      &Path::new("tests/view/hokey_pokey_caret.txt")).unwrap();
    let mut view = super::View::new();
    // move to empty line
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 1); assert_eq!(view.caret_column, 0);
    // move to end of double width character then back again
    view.caret_line = 3; view.caret_column = 3;
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 4); assert_eq!(view.caret_column, 1);
    view.move_caret(super::CaretMovement::LineUp, &buffer);
    assert_eq!(view.caret_line, 3); assert_eq!(view.caret_column, 3);
    // move to shorter lines then back again
    view.caret_line = 6; view.caret_column = 30;
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 7); assert_eq!(view.caret_column, 14);
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 8); assert_eq!(view.caret_column, 20);
    view.move_caret(super::CaretMovement::LineUp, &buffer);
    view.move_caret(super::CaretMovement::LineUp, &buffer);
    assert_eq!(view.caret_line, 6); assert_eq!(view.caret_column, 30);
    // move to shorter line, step sideways, then back again
    view.caret_line = 10; view.caret_column = 75;
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 11); assert_eq!(view.caret_column, 68);
    view.move_caret(super::CaretMovement::CharPrev, &buffer);
    assert_eq!(view.caret_line, 11); assert_eq!(view.caret_column, 67);
    view.move_caret(super::CaretMovement::CharNext, &buffer);
    assert_eq!(view.caret_line, 11); assert_eq!(view.caret_column, 68);
    view.move_caret(super::CaretMovement::LineUp, &buffer);
    assert_eq!(view.caret_line, 10); assert_eq!(view.caret_column, 68);
    // move to end of line lacking newline
    view.caret_line = 13; view.caret_column = 34;
    view.move_caret(super::CaretMovement::LineDown, &buffer);
    assert_eq!(view.caret_line, 14); assert_eq!(view.caret_column, 34);
    view.move_caret(super::CaretMovement::CharNext, &buffer);
    assert_eq!(view.caret_line, 14); assert_eq!(view.caret_column, 34);
  }
}
