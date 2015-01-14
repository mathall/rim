/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![allow(unstable)]

#![feature(int_uint)]
#![feature(slicing_syntax)]

#[cfg(not(test))]
use std::collections::HashSet;

#[allow(dead_code, unused_imports)]  // temporary until buffer is used for real
mod buffer;
mod frame;
mod input;
mod keymap;
mod screen;

#[cfg(not(test))]
struct Rim {
  frame: frame::Frame,
  frame_ctx: frame::FrameContext,
  windows: HashSet<frame::WindowId>,
  focus: frame::WindowId,
  buffer: buffer::Buffer,
}

#[cfg(not(test))]
impl Rim {
  fn new() -> Rim {
    let (frame, frame_ctx, first_window) = frame::Frame::new();
    let mut windows: HashSet<frame::WindowId> = HashSet::new();
    windows.insert(first_window.clone());
    Rim {
      frame: frame,
      frame_ctx: frame_ctx,
      windows: windows,
      focus: first_window,
      buffer: buffer::Buffer::open(&Path::new("src/rim.rs")).unwrap(),
    }
  }

  fn switch_focus(&mut self, direction: frame::Direction)
      -> Option<(frame::WindowId, frame::WindowId)> {
    self.frame.get_adjacent_window(&self.frame_ctx, &self.focus, direction).
    map(|window| self.move_focus(window)).ok()
  }

  fn shift_focus(&mut self, order: frame::WindowOrder)
      -> Option<(frame::WindowId, frame::WindowId)> {
    self.frame.get_sequent_window(&self.frame_ctx, &self.focus, order, true).
    map(|window| self.move_focus(window)).ok()
  }

  fn move_focus(&mut self, window: frame::WindowId)
      -> (frame::WindowId, frame::WindowId) {
    let old_focus = std::mem::replace(&mut self.focus, window);
    (old_focus, self.focus.clone())
  }

  fn split_window(&mut self, orientation: frame::Orientation) {
    self.frame.split_window(&mut self.frame_ctx, &self.focus, orientation).
    map(|new_window| self.windows.insert(new_window)).unwrap();
  }

  fn resize_window(&mut self, orientation: frame::Orientation, amount: int)
     -> bool {
    self.frame.resize_window(&self.frame_ctx, &self.focus, orientation, amount).
    map(|absorbed| absorbed != 0).unwrap()
  }

  fn close_window(&mut self) -> bool {
    self.frame.get_closest_neighbouring_window(&self.frame_ctx, &self.focus).
    map(|neighbour| {
      let (old_focus, _) = self.move_focus(neighbour);
      self.frame.close_window(&mut self.frame_ctx, &old_focus).unwrap();
      self.windows.remove(&old_focus); }).
    is_ok()
  }

  fn draw_window(&self, window: &frame::WindowId, screen: &mut screen::Screen) {
    let focused = self.focus == *window;
    self.frame.get_window_rect(&self.frame_ctx, window).
    map(|rect| self.draw_buffer(focused, rect, screen)).
    unwrap();
    screen.flush();
  }

  fn draw_buffer(&self, focused: bool, rect: screen::Rect,
                 screen: &mut screen::Screen) {
    let mut skip_cols = 0u;
    for screen_cell in screen::CellIterator::new(rect) {
      let screen::Rect(position, _) = rect;
      let screen::Cell(line, column) = screen_cell - position;
      if column == 0 { skip_cols = 0; }
      if skip_cols > 0 { skip_cols -= 1; continue; }
      let character =
        self.buffer.get_char_by_line_column(line as uint, column as uint).
        map(|character| if character == '\n' { ' ' } else { character }).
        unwrap_or(if column == 0 { '~' } else { ' ' });
      let (fg, bg) = if focused {
        (screen::Color::Black, screen::Color::White)
      }
      else {
        (screen::Color::White, screen::Color::Black)
      };
      screen.put(screen_cell, character, fg, bg);
      skip_cols = CharExt::width(character, false).unwrap_or(1) - 1;
    }
  }

  fn draw_all(&self, screen: &mut screen::Screen) {
    screen.clear();
    self.frame.draw_borders(screen);
    for window in self.windows.iter() {
      self.draw_window(window, screen);
    }
    screen.flush();
  }
}

#[cfg(not(test))]
impl Drop for Rim {
  fn drop(&mut self) {
    self.buffer.write().unwrap();  // what could go wrong!
  }
}

#[cfg(not(test))]
fn main() {
  let mut screen = screen::Screen::setup().unwrap();

  let mut rim = Rim::new();

  let (key_tx, key_rx) = std::sync::mpsc::channel();
  let _term_input = input::start(key_tx);

  let draw_window_pair =
    |&: (win1, win2), rim: &Rim, screen: &mut screen::Screen|
      { rim.draw_window(&win1, screen); rim.draw_window(&win2, screen); };

  loop {
    match key_rx.try_recv() {
      Ok(keymap::Key::Unicode{codepoint: 'h', mods:_}) => {
        rim.switch_focus(frame::Direction::Left).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: 'l', mods:_}) => {
        rim.switch_focus(frame::Direction::Right).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: 'k', mods:_}) => {
        rim.switch_focus(frame::Direction::Up).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: 'j', mods:_}) => {
        rim.switch_focus(frame::Direction::Down).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: 'n', mods:_}) => {
        rim.shift_focus(frame::WindowOrder::NextWindow).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: 'N', mods:_}) => {
        rim.shift_focus(frame::WindowOrder::PreviousWindow).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Ok(keymap::Key::Unicode{codepoint: '=', mods:_}) => {
        rim.frame.reset_layout();
        rim.draw_all(&mut screen);
      }
      Ok(keymap::Key::Unicode{codepoint: 'v', mods:_}) => {
        rim.split_window(frame::Orientation::Vertical);
        rim.draw_all(&mut screen);
      }
      Ok(keymap::Key::Unicode{codepoint: 's', mods:_}) => {
        rim.split_window(frame::Orientation::Horizontal);
        rim.draw_all(&mut screen);
      }
      Ok(keymap::Key::Unicode{codepoint: 'y', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        if rim.resize_window(frame::Orientation::Horizontal, amount) {
          rim.draw_all(&mut screen);
        }
      }
      Ok(keymap::Key::Unicode{codepoint: 'u', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        if rim.resize_window(frame::Orientation::Vertical, amount) {
          rim.draw_all(&mut screen);
        }
      }
      Ok(keymap::Key::Unicode{codepoint: 'c', mods:_}) =>
        if rim.close_window() {
          rim.draw_all(&mut screen);
        },
      Ok(keymap::Key::Unicode{codepoint: 'q', mods:_}) =>
        break,
      _                                                =>
        (),
    }

    if screen.update_size() {
      rim.frame.set_size(screen.size());
      rim.draw_all(&mut screen);
    }
  }
}
