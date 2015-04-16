/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use buffer;
use caret;
use screen;

const MIN_VIEW_SIZE: u16 = 1;

/*
 * View handles the presentation of a buffer.
 * Everything is measured in screen cell coordinates.
 */
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
    let character = buffer.get_char_by_line_column(line, column).unwrap();
    let start = caret::buffer_to_screen_column(line, column, buffer);
    let end = start + character.width(false).unwrap_or(1) - 1;
    self.scroll_column =
      if start < self.scroll_column { start }
      else if end >= self.scroll_column + cols { end - cols + 1 }
      else { self.scroll_column };
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
    use std::cmp;
    // calculate caret screen position
    let caret_row = (caret.line() - self.scroll_line) as u16;
    let caret_column = caret::buffer_to_screen_column(
      caret.line(), caret.column(), buffer) - self.scroll_column;
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
    for chars in buffer.line_iter().from(self.scroll_line).take(rows as usize) {
      let line_offset = screen::Cell(row, 0) + position;
      // draw character by character
      let mut col = -(self.scroll_column as isize);
      for character in chars {
        if col >= cols as isize || character == '\n' { break }
        let char_width = character.width(false).unwrap_or(0) as isize;
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
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 0);
    caret.adjust(caret::Adjustment::Set(0, 12), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 3);
    caret.adjust(caret::Adjustment::Set(0, 16), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 9);
    caret.adjust(caret::Adjustment::Set(0, 3), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line, 0); assert_eq!(view.scroll_column, 6);
    caret.adjust(caret::Adjustment::Set(3, 10), &buffer);
    view.scroll_into_view(caret, &buffer);
    assert_eq!(view.scroll_line, 3); assert_eq!(view.scroll_column, 6);
  }
}
