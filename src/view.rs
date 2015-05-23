/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate unicode_width;

use std::cmp;

use self::unicode_width::UnicodeWidthChar as CharWidth;

use buffer;
use caret;
use screen;

const MIN_VIEW_SIZE: u16 = 1;

/*
 * View handles the presentation of a buffer.
 * Everything is measured in screen cell coordinates.
 */
#[derive(Clone, Copy)]
pub struct View {
  scroll_line: usize,
  scroll_column: usize,
  size: screen::Size,
}

impl View {
  pub fn new() -> View {
    View {
      scroll_line: 0,
      scroll_column: 0,
      size: screen::Size(MIN_VIEW_SIZE, MIN_VIEW_SIZE),
    }
  }

  pub fn scroll_line(&self) -> usize {
    self.scroll_line
  }

  pub fn scroll_column(&self) -> usize {
    self.scroll_column
  }

  // assumes caret is in view
  pub fn caret_position(&self, caret: caret::Caret, buffer: &buffer::Buffer)
      -> screen::Cell {
    let caret_row = (caret.line() - self.scroll_line) as u16;
    let caret_column = caret::buffer_to_screen_column(
      caret.line(), caret.column(), buffer) - self.scroll_column;
    screen::Cell(caret_row, caret_column as u16)
  }

  pub fn set_scroll(&mut self, line: usize, column: usize) {
    self.scroll_line = line;
    self.scroll_column = column;
  }

  pub fn scroll_into_view(&mut self, caret: caret::Caret,
                          buffer: &buffer::Buffer) {
    let (line, column) = (caret.line(), caret.column());
    let screen::Size(rows, cols) = self.size;
    let rows = rows as usize;
    let cols = cols as usize;

    self.scroll_line =
      if line < self.scroll_line { line }
      else if line >= self.scroll_line + rows { line - rows + 1 }
      else { self.scroll_line };

    // make sure wider characters are scrolled in entirely as well
    let start = caret::buffer_to_screen_column(line, column, buffer);
    let end = start + buffer.get_char_by_line_column(line, column).and_then(|c|
      CharWidth::width(c)).unwrap_or(1) - 1;
    self.scroll_column =
      if start < self.scroll_column { start }
      else if end >= self.scroll_column + cols { end - cols + 1 }
      else { self.scroll_column };
  }

  pub fn line_clamped_to_view(&self, line: usize) -> usize {
    let screen::Size(rows, _) = self.size;
    assert!(rows >= MIN_VIEW_SIZE);
    let last_line = self.scroll_line + rows as usize - 1;
    cmp::min(cmp::max(line, self.scroll_line), last_line)
  }

  pub fn set_size(&mut self, size: screen::Size) {
    let screen::Size(rows, cols) = size;
    assert!(rows >= MIN_VIEW_SIZE && cols >= MIN_VIEW_SIZE);
    self.size = size;
  }

  #[cfg(not(test))]
  pub fn draw(&self, buffer: &buffer::Buffer, caret: caret::Caret,
              focused: bool, position: screen::Cell,
              screen: &mut screen::Screen) {
    // calculate caret screen position if focused
    let caret_cell =
      if focused { Some(position + self.caret_position(caret, buffer)) }
      else       { None };

    // helper to put a character on the screen
    let put = |character, cell: screen::Cell, screen: &mut screen::Screen| {
      use screen::Color::*;
      let highlight = caret_cell.map(|c| c != cell).unwrap_or(false);
      let (fg, bg) = if highlight { (Black, White) } else { (White, Black) };
      screen.put(cell, character, fg, bg);
    };

    let screen::Size(rows, cols) = self.size;
    // draw line by line
    let mut row: u16 = 0;
    for chars in buffer.line_iter().from(self.scroll_line).take(rows as usize) {
      let line_offset = screen::Cell(row, 0) + position;
      // draw character by character
      let mut col = -(self.scroll_column as isize);
      for character in chars {
        if col >= cols as isize || character == '\n' { break }
        let char_width = CharWidth::width(character).unwrap_or(0) as isize;
        let end_col = col + char_width;
        if (col < 0 && end_col >= 0) || end_col > cols as isize {
          // blank out partially visible characters
          for col in cmp::max(0, col)..cmp::min(end_col, cols as isize) {
            put(' ', line_offset + screen::Cell(0, col as u16), screen);
          }
        }
        else if col >= 0 {
          put(character, line_offset + screen::Cell(0, col as u16), screen);
        }
        col += char_width;
      }
      // blank out the rest of the row if the line didn't fill it
      for col in cmp::max(0, col) as u16..cols {
        put(' ', line_offset + screen::Cell(0, col), screen);
      }
      row += 1;
    }
    // fill in the rest of the view below the buffer content
    for row in row..rows {
      let line_offset = screen::Cell(row, 0) + position;
      put(if self.scroll_column == 0 { '~' } else { ' ' }, line_offset, screen);
      for col in 1..cols {
        put(' ', line_offset + screen::Cell(0, col), screen);
      }
    }
  }
}

#[cfg(test)]
mod test {
  use std::path::Path;

  use buffer;
  use caret;
  use screen;

  #[test]
  fn scroll_into_view_double_width() {
    let mut caret = caret::Caret::new();
    let buffer = buffer::Buffer::open(
      &Path::new("tests/view/scroll_into_view_double_width.txt")).unwrap();
    let mut view = super::View::new();
    view.set_size(screen::Size(1, 15));
    assert_eq!(view.scroll_line(), 0); assert_eq!(view.scroll_column(), 0);
    caret.adjust(caret::Adjustment::Set(0, 12), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line(), 0); assert_eq!(view.scroll_column(), 3);
    caret.adjust(caret::Adjustment::Set(0, 16), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line(), 0); assert_eq!(view.scroll_column(), 9);
    caret.adjust(caret::Adjustment::Set(0, 3), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line(), 0); assert_eq!(view.scroll_column(), 6);
    caret.adjust(caret::Adjustment::Set(3, 10), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line(), 3); assert_eq!(view.scroll_column(), 6);
  }

  #[test]
  fn caret_position() {
    let mut caret = caret::Caret::new();
    let buffer = buffer::Buffer::open(
      &Path::new("tests/view/caret_position.txt")).unwrap();
    let mut view = super::View::new();
    view.set_scroll(1, 1);
    caret.adjust(caret::Adjustment::Set(1, 1), &buffer);
    assert_eq!(view.caret_position(caret, &buffer), screen::Cell(0, 1));
    caret.adjust(caret::Adjustment::Set(2, 1), &buffer);
    assert_eq!(view.caret_position(caret, &buffer), screen::Cell(1, 0));
  }

  #[test]
  fn line_clamped_to_view() {
    let mut view = super::View::new();
    view.set_size(screen::Size(5, 5));
    view.set_scroll(5, 5);
    assert_eq!(view.line_clamped_to_view(1), 5);
    assert_eq!(view.line_clamped_to_view(7), 7);
    assert_eq!(view.line_clamped_to_view(10), 9);
  }
}
