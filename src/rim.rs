/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(collections)]
#![feature(core)]
#![feature(hash)]
#![feature(fs)]
#![feature(int_uint)]
#![feature(io)]
#![feature(libc)]
#![feature(old_io)]
#![feature(path)]
#![feature(rustc_private)]
#![feature(unicode)]

#![cfg_attr(test, feature(os))]
#![cfg_attr(test, feature(std_misc))]

#[macro_use]
extern crate bitflags;

#[cfg(not(test))]
use std::collections::HashMap;
#[cfg(not(test))]
use std::path::Path;

#[allow(dead_code, unused_imports)]  // temporary until buffer is used for real
mod buffer;
mod frame;
mod input;
mod keymap;
mod screen;
mod view;

#[cfg(not(test))]
struct Rim {
  frame: frame::Frame,
  frame_ctx: frame::FrameContext,
  windows: HashMap<frame::WindowId, view::View>,
  focus: frame::WindowId,
  buffer: buffer::Buffer,
}

#[cfg(not(test))]
impl Rim {
  fn new() -> Rim {
    let (frame, frame_ctx, first_window) = frame::Frame::new();
    let mut windows = HashMap::new();
    windows.insert(first_window.clone(), view::View::new());
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
    map(|new_window| self.windows.insert(new_window, view::View::new())).
    ok().expect("Failed to split window.");
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
    self.windows.get(window).
    map(|view| self.draw_view(window, view, screen)).
    expect("Couldn't find view for window.");
    screen.flush();
  }

  fn draw_view(&self, window: &frame::WindowId, view: &view::View,
               screen: &mut screen::Screen) {
    self.frame.get_window_rect(&self.frame_ctx, window).
    map(|rect| { let screen::Rect(position, _) = rect; position }).
    map(|pos| view.draw(&self.buffer, self.focus == *window, pos, screen)).
    ok().expect("Couldn't find rect for window.");
  }

  fn draw_all(&self, screen: &mut screen::Screen) {
    screen.clear();
    self.frame.draw_borders(screen);
    for (window, view) in self.windows.iter() {
      self.draw_view(window, view, screen);
    }
    screen.flush();
  }

  fn update_view_sizes(&mut self) {
    let window_sizes: Vec<(frame::WindowId, screen::Size)> =
      self.windows.iter().
      map(|(win, _)|
        self.frame.get_window_rect(&self.frame_ctx, win).
        map(|rect| { let screen::Rect(_, size) = rect; (win.clone(), size) }).
        ok().expect("Couldn't find rect for window.")).
      collect();
    for &(ref win, size) in window_sizes.iter() {
      self.windows.remove(win).
      map(|mut view| {
        view.set_size(size, &self.buffer);
        self.windows.insert(win.clone(), view); }).
      expect("Couldn't find view for window.");
    }
  }

  fn handle_key_by_view(&mut self, key: keymap::Key) -> bool {
     self.windows.remove(&self.focus).
     map(|mut view| {
       let swallowed = match key {
         keymap::Key::Sym{sym: keymap::KeySym::Left, mods:_}  => {
           view.move_caret(view::CaretMovement::CharPrev, &self.buffer); true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Right, mods:_} => {
           view.move_caret(view::CaretMovement::CharNext, &self.buffer); true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Up, mods:_}    => {
           view.move_caret(view::CaretMovement::LineUp, &self.buffer); true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Down, mods:_}  => {
           view.move_caret(view::CaretMovement::LineDown, &self.buffer); true
         }
         _                                                    => false,
       };
       self.windows.insert(self.focus.clone(), view);
       swallowed }).
     expect("Found no view for focused window.")
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
    match key_rx.try_recv().ok().and_then(|key|
        if !rim.handle_key_by_view(key) { Some(key) }
        else { rim.draw_window(&rim.focus, &mut screen); None }) {
      Some(keymap::Key::Unicode{codepoint: 'h', mods:_}) => {
        rim.switch_focus(frame::Direction::Left).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: 'l', mods:_}) => {
        rim.switch_focus(frame::Direction::Right).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: 'k', mods:_}) => {
        rim.switch_focus(frame::Direction::Up).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: 'j', mods:_}) => {
        rim.switch_focus(frame::Direction::Down).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: 'n', mods:_}) => {
        rim.shift_focus(frame::WindowOrder::NextWindow).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: 'N', mods:_}) => {
        rim.shift_focus(frame::WindowOrder::PreviousWindow).
        map(|pair| draw_window_pair(pair, &rim, &mut screen));
      }
      Some(keymap::Key::Unicode{codepoint: '=', mods:_}) => {
        rim.frame.reset_layout();
        rim.update_view_sizes();
        rim.draw_all(&mut screen);
      }
      Some(keymap::Key::Unicode{codepoint: 'v', mods:_}) => {
        rim.split_window(frame::Orientation::Vertical);
        rim.update_view_sizes();
        rim.draw_all(&mut screen);
      }
      Some(keymap::Key::Unicode{codepoint: 's', mods:_}) => {
        rim.split_window(frame::Orientation::Horizontal);
        rim.update_view_sizes();
        rim.draw_all(&mut screen);
      }
      Some(keymap::Key::Unicode{codepoint: 'y', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        if rim.resize_window(frame::Orientation::Horizontal, amount) {
          rim.update_view_sizes();
          rim.draw_all(&mut screen);
        }
      }
      Some(keymap::Key::Unicode{codepoint: 'u', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        if rim.resize_window(frame::Orientation::Vertical, amount) {
          rim.update_view_sizes();
          rim.draw_all(&mut screen);
        }
      }
      Some(keymap::Key::Unicode{codepoint: 'c', mods:_}) =>
        if rim.close_window() {
          rim.update_view_sizes();
          rim.draw_all(&mut screen);
        },
      Some(keymap::Key::Unicode{codepoint: 'q', mods:_}) =>
        break,
      _                                                  =>
        (),
    }

    if screen.update_size() {
      rim.frame.set_size(screen.size());
      rim.update_view_sizes();
      rim.draw_all(&mut screen);
    }
  }
}
