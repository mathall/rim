/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(collections)]
#![feature(core)]
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
struct Window {
  view: view::View,
  rect: screen::Rect,
  needs_redraw: bool,
}

#[cfg(not(test))]
impl Window {
  fn new() -> Window {
    Window {
      view: view::View::new(),
      rect: screen::Rect(screen::Cell(0, 0), screen::Size(0, 0)),
      needs_redraw: true,
    }
  }
}

#[cfg(not(test))]
struct Rim {
  frame: frame::Frame,
  frame_ctx: frame::FrameContext,
  frame_needs_redraw: bool,
  windows: HashMap<frame::WindowId, Window>,
  focus: frame::WindowId,
  buffer: buffer::Buffer,
}

#[cfg(not(test))]
impl Rim {
  fn new() -> Rim {
    let (frame, frame_ctx, first_win_id) = frame::Frame::new();
    let mut windows = HashMap::new();
    windows.insert(first_win_id.clone(), Window::new());
    Rim {
      frame: frame,
      frame_ctx: frame_ctx,
      frame_needs_redraw: true,
      windows: windows,
      focus: first_win_id,
      buffer: buffer::Buffer::open(&Path::new("src/rim.rs")).unwrap(),
    }
  }

  fn move_focus(&mut self, direction: frame::Direction) {
    self.frame.get_adjacent_window(&self.frame_ctx, &self.focus, direction).
    map(|win_id| self.set_focus(win_id)).ok();
  }

  fn shift_focus(&mut self, order: frame::WindowOrder) {
    self.frame.get_sequent_window(&self.frame_ctx, &self.focus, order, true).
    map(|win_id| self.set_focus(win_id)).ok();
  }

  fn set_focus(&mut self, win_id: frame::WindowId) {
    self.windows.get_mut(&win_id).map(|win| win.needs_redraw = true);
    self.windows.get_mut(&self.focus).map(|win| win.needs_redraw = true);
    self.focus = win_id;
  }

  fn split_window(&mut self, orientation: frame::Orientation) {
    self.frame.split_window(&mut self.frame_ctx, &self.focus, orientation).
    map(|new_win_id| {
      self.windows.insert(new_win_id, Window::new());
      self.invalidate_frame(); }).
    ok().expect("Failed to split window.");
  }

  fn resize_window(&mut self, orientation: frame::Orientation, amount: int) {
    self.frame.resize_window(&self.frame_ctx, &self.focus, orientation, amount).
    map(|absorbed| if absorbed != 0 { self.invalidate_frame(); }).
    ok().expect("Failed to resize window");
  }

  fn close_window(&mut self) {
    self.frame.get_closest_neighbouring_window(&self.frame_ctx, &self.focus).
    map(|neighbour| {
      let old_focus = self.focus.clone();
      self.set_focus(neighbour);
      self.frame.close_window(&mut self.frame_ctx, &old_focus).unwrap();
      self.windows.remove(&old_focus);
      self.invalidate_frame(); }).ok();
  }

  fn draw_window(&self, win_id: &frame::WindowId, screen: &mut screen::Screen) {
    self.windows.get(win_id).
    map(|win| {
      let screen::Rect(position, _) = win.rect;
      win.view.draw(&self.buffer, self.focus == *win_id, position, screen) }).
    expect("Couldn't find window.");
  }

  fn invalidate_frame(&mut self) {
    let window_rects: Vec<(frame::WindowId, screen::Rect)> =
      self.windows.iter().
      map(|(win_id, ref win)| {
        let old_rect = win.rect;
        (win_id.clone(), old_rect,
          self.frame.get_window_rect(&self.frame_ctx, win_id).
          ok().expect("Couldn't find rect for window.")) }).
      filter(|&(_, ref old_rect, ref new_rect)| *old_rect != *new_rect).
      map(|(win_id, _, new_rect)| (win_id, new_rect)).
      collect();
    for &(ref win_id, new_rect) in window_rects.iter() {
      self.windows.remove(win_id).
      map(|mut win| {
        let screen::Rect(_, old_size) = win.rect;
        let screen::Rect(_, new_size) = new_rect;
        if old_size != new_size { win.view.set_size(new_size, &self.buffer); }
        win.rect = new_rect;
        win.needs_redraw = true;
        self.windows.insert(win_id.clone(), win); }).
      expect("Couldn't find window.");
    }
    self.frame_needs_redraw = true;
  }

  fn handle_key_by_window(&mut self, key: keymap::Key) -> bool {
     self.windows.remove(&self.focus).
     map(|mut win| {
       let swallowed = match key {
         keymap::Key::Sym{sym: keymap::KeySym::Left, mods:_}  => {
           win.view.move_caret(view::CaretMovement::CharPrev, &self.buffer);
           true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Right, mods:_} => {
           win.view.move_caret(view::CaretMovement::CharNext, &self.buffer);
           true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Up, mods:_}    => {
           win.view.move_caret(view::CaretMovement::LineUp, &self.buffer);
           true
         }
         keymap::Key::Sym{sym: keymap::KeySym::Down, mods:_}  => {
           win.view.move_caret(view::CaretMovement::LineDown, &self.buffer);
           true
         }
         _                                                    => false,
       };
       if swallowed { win.needs_redraw = true; }
       self.windows.insert(self.focus.clone(), win);
       swallowed }).
     expect("Couldn't find focused window.")
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

  loop {
    match key_rx.try_recv().ok().and_then(|key|
        if rim.handle_key_by_window(key) { None } else { Some(key) }) {
      Some(keymap::Key::Unicode{codepoint: 'h', mods:_}) =>
        rim.move_focus(frame::Direction::Left),
      Some(keymap::Key::Unicode{codepoint: 'l', mods:_}) =>
        rim.move_focus(frame::Direction::Right),
      Some(keymap::Key::Unicode{codepoint: 'k', mods:_}) =>
        rim.move_focus(frame::Direction::Up),
      Some(keymap::Key::Unicode{codepoint: 'j', mods:_}) =>
        rim.move_focus(frame::Direction::Down),
      Some(keymap::Key::Unicode{codepoint: 'n', mods:_}) =>
        rim.shift_focus(frame::WindowOrder::NextWindow),
      Some(keymap::Key::Unicode{codepoint: 'N', mods:_}) =>
        rim.shift_focus(frame::WindowOrder::PreviousWindow),
      Some(keymap::Key::Unicode{codepoint: '=', mods:_}) => {
        rim.frame.reset_layout();
        rim.invalidate_frame();
      }
      Some(keymap::Key::Unicode{codepoint: 'v', mods:_}) =>
        rim.split_window(frame::Orientation::Vertical),
      Some(keymap::Key::Unicode{codepoint: 's', mods:_}) =>
        rim.split_window(frame::Orientation::Horizontal),
      Some(keymap::Key::Unicode{codepoint: 'y', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        rim.resize_window(frame::Orientation::Horizontal, amount);
      }
      Some(keymap::Key::Unicode{codepoint: 'u', mods})   => {
        let amount = if mods.contains(keymap::MOD_CTRL) { -10 }
                     else                               { 10 };
        rim.resize_window(frame::Orientation::Vertical, amount);
      }
      Some(keymap::Key::Unicode{codepoint: 'c', mods:_}) =>
        rim.close_window(),
      Some(keymap::Key::Unicode{codepoint: 'q', mods:_}) =>
        break,
      _                                                  =>
        (),
    }

    // clear/redraw/update/invalidate everything if the screen size changed
    if screen.update_size() {
      rim.frame.set_size(screen.size());
      rim.invalidate_frame();
      for (_, win) in rim.windows.iter_mut() { win.needs_redraw = true; }
      screen.clear();
    }

    let mut flush_screen = rim.frame_needs_redraw;

    // draw frame if necessary
    if rim.frame_needs_redraw {
      rim.frame.draw_borders(&mut screen);
      rim.frame_needs_redraw = false;
    }

    // draw windows if necessary
    for (win_id, win) in rim.windows.iter() {
      if win.needs_redraw {
        rim.draw_window(win_id, &mut screen);
        flush_screen = true;
      }
    }

    // mark windows as not needing redraw
    for (_, win) in rim.windows.iter_mut() { win.needs_redraw = false; }

    // flush screen if we did any drawing
    if flush_screen { screen.flush(); }
  }
}
