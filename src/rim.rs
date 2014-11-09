/*
 * Copyright (c) 2014 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(macro_rules)]
#![feature(slicing_syntax)]
#![feature(struct_variant)]

#[allow(dead_code, unused_imports)]  // temporary until buffer is used for real
mod buffer;
mod input;
mod keymap;
#[cfg(not(test))]
mod screen;

#[cfg(not(test))]
fn main() {
  let mut screen = screen::Screen::setup().unwrap();

  let (key_tx, key_rx) = std::comm::channel();
  let _term_input = input::start(key_tx);

  let buffer = buffer::Buffer::open(&Path::new("src/rim.rs")).unwrap();

  // just keep the buffer on the screen until ctrl+c is pressed
  loop {
    match key_rx.try_recv() {
      Ok(keymap::Unicode{codepoint: 'c', mods}) =>
        if mods.contains(keymap::MOD_CTRL) { break },
      _                                         => (),
    }

    if screen.update_size() {
      screen.clear();
      let mut skip_cols = 0u;
      for cell in screen::CellIterator::new(screen::Cell(0, 0), screen.size()) {
        let screen::Cell(line, column) = cell;
        if column == 0 { skip_cols = 0; }
        if skip_cols > 0 { skip_cols -= 1; continue; }
        let character =
          buffer.get_char_by_line_column(line as uint, column as uint).
          map(|character| if character == '\n' { ' ' } else { character }).
          unwrap_or(if column == 0 { '~' } else { ' ' });
        let fg = screen::color::Black;
        let bg = screen::color::White;
        screen.put(cell, character, fg, bg);
        skip_cols = std::char::width(character, false).unwrap_or(1) - 1;
      }
      screen.flush();
    }
  }

  buffer.write().unwrap();  // what could go wrong!
}
