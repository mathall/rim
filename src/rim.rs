/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(collections)]
#![feature(core)]
#![feature(libc)]
#![feature(rustc_private)]
#![feature(slice_patterns)]
#![feature(std_misc)]
#![feature(str_char)]

#[macro_use]
extern crate bitflags;

#[cfg(not(test))]
use std::collections::HashMap;
#[cfg(not(test))]
use std::path::{Path, PathBuf};
#[cfg(not(test))]
use std::thread;

#[allow(dead_code, unused_imports)]  // temporary until buffer is used for real
mod buffer;
mod caret;
mod command;
mod frame;
mod input;
mod keymap;
mod screen;
mod view;

type BufferId = usize;

#[cfg(not(test))]
const INVALID_BUFFER_ID: BufferId = 0;

#[cfg(not(test))]
#[derive(Clone)]
struct Window {
  buf_id: BufferId,
  states: HashMap<BufferId, (caret::Caret, view::View)>,
  rect: screen::Rect,
  needs_redraw: bool,
  normal_mode: command::Mode,
  insert_mode: command::Mode,
}

#[cfg(not(test))]
impl Window {
  fn new() -> Window {
    let mut win = Window {
      buf_id: INVALID_BUFFER_ID,
      states: HashMap::new(),
      rect: screen::Rect(screen::Cell(0, 0), screen::Size(0, 0)),
      needs_redraw: true,
      normal_mode: default_normal_mode(),
      insert_mode: default_insert_mode(),
    };
    win.set_buf_id(INVALID_BUFFER_ID);
    return win;
  }

  fn caret_for(&self, buf_id: BufferId) -> Option<&caret::Caret> {
    self.states.get(&buf_id).map(|&(ref c, _)| c)
  }

  fn caret(&self) -> &caret::Caret {
    self.caret_for(self.buf_id).expect("Window lacked state for its buffer id.")
  }

  fn caret_mut_for(&mut self, buf_id: BufferId) -> Option<&mut caret::Caret> {
    self.states.get_mut(&buf_id).map(|&mut (ref mut c, _)| c)
  }

  fn caret_mut(&mut self) -> &mut caret::Caret {
    let buf_id = self.buf_id;
    self.caret_mut_for(buf_id).expect("Window lacked state for its buffer id.")
  }

  fn view_for(&self, buf_id: BufferId) -> Option<&view::View> {
    self.states.get(&buf_id).map(|&(_, ref v)| v)
  }

  fn view(&self) -> &view::View {
    self.view_for(self.buf_id).expect("Window lacked state for its buffer id.")
  }

  fn view_mut_for(&mut self, buf_id: BufferId) -> Option<&mut view::View> {
    self.states.get_mut(&buf_id).map(|&mut (_, ref mut v)| v)
  }

  fn view_mut(&mut self) -> &mut view::View {
    let buf_id = self.buf_id;
    self.view_mut_for(buf_id).expect("Window lacked state for its buffer id.")
  }

  fn set_buf_id(&mut self, buf_id: BufferId) {
    if !self.has_buf_id(buf_id) {
      self.states.insert(buf_id, (caret::Caret::new(), view::View::new()));
    }
    self.buf_id = buf_id;
    self.needs_redraw = true;
  }

  fn has_buf_id(&self, buf_id: BufferId) -> bool {
    self.states.contains_key(&buf_id)
  }
}

#[cfg(not(test))]
struct Rim {
  frame: frame::Frame,
  frame_ctx: frame::FrameContext,
  frame_needs_redraw: bool,
  windows: HashMap<frame::WindowId, Window>,
  focus: frame::WindowId,
  buffers: HashMap<BufferId, buffer::Buffer>,
  next_buf_id: BufferId,
  cmd_thread: command::CmdThread,
  quit: bool,
}

#[cfg(not(test))]
impl Rim {
  fn new(cmd_thread: command::CmdThread) -> Rim {
    let (frame, frame_ctx, first_win_id) = frame::Frame::new();
    let mut windows = HashMap::new();
    let first_win = Window::new();
    cmd_thread.set_mode(default_mode(), 0).ok().expect("Command thread died.");
    cmd_thread.set_mode(first_win.normal_mode.clone(), 1).ok().expect(
      "Command thread died.");
    windows.insert(first_win_id.clone(), first_win);
    Rim {
      frame: frame,
      frame_ctx: frame_ctx,
      frame_needs_redraw: true,
      windows: windows,
      focus: first_win_id,
      buffers: HashMap::new(),
      next_buf_id: INVALID_BUFFER_ID + 1,
      cmd_thread: cmd_thread,
      quit: false,
    }
  }

  fn load_buffer(&mut self, path: &Path) -> Option<BufferId> {
    for (buf_id, buf) in self.buffers.iter() {
      if let Ok(buf_path) = buf.path() {
        if path == buf_path { return Some(*buf_id) }
      }
    }
    buffer::Buffer::open(path).map(|buf| {
      let id = self.next_buf_id;
      self.next_buf_id += 1;
      self.buffers.insert(id, buf);
      return id; }).ok()
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
    assert!(self.windows.contains_key(&win_id));
    self.windows.get(&win_id).map(|win|
      self.cmd_thread.set_mode(win.normal_mode.clone(), 1).ok().expect(
        "Command thread died."));
    self.windows.get_mut(&self.focus).map(|win| win.needs_redraw = true);
    self.windows.remove(&win_id).map(|mut win| {
      win.needs_redraw = true;
      self.buffers.get(&win.buf_id).map(|buffer| {
        let caret = *win.caret();
        win.view_mut().scroll_into_view(caret, buffer) });
      self.windows.insert(win_id.clone(), win); });
    self.focus = win_id;
  }

  fn split_window(&mut self, orientation: frame::Orientation) {
    self.frame.split_window(&mut self.frame_ctx, &self.focus, orientation).
    map(|new_win_id| {
      let win = self.windows.get(&self.focus).map(|win| win.clone()).
        expect("Couldn't find focused window.");
      self.windows.insert(new_win_id, win);
      self.invalidate_frame(); }).
    ok().expect("Failed to split window.");
  }

  fn resize_window(&mut self, orientation: frame::Orientation, amount: isize) {
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
      let focused = self.focus == *win_id;
      self.buffers.get(&win.buf_id).map(|buffer|
        win.view().draw(buffer, *win.caret(), focused, position, screen)) }).
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
        if old_size != new_size {
          win.view_mut().set_size(new_size);
          self.buffers.get(&win.buf_id).map(|buffer| {
            let caret = *win.caret();
            win.view_mut().scroll_into_view(caret, buffer) });
        }
        win.rect = new_rect;
        win.needs_redraw = true;
        self.windows.insert(win_id.clone(), win); }).
      expect("Couldn't find window.");
    }
    self.frame_needs_redraw = true;
  }

  fn handle_cmd(&mut self, cmd: command::Cmd) {
    match cmd {
      command::Cmd::MoveFocus(direction)      =>
        self.move_focus(direction),
      command::Cmd::ShiftFocus(window_order)  =>
        self.shift_focus(window_order),
      command::Cmd::ResetLayout               => {
        self.frame.reset_layout();
        self.invalidate_frame();
      }
      command::Cmd::SplitWindow(orientation)  =>
        self.split_window(orientation),
      command::Cmd::GrowWindow(orientation)   =>
        self.resize_window(orientation, 10),
      command::Cmd::ShrinkWindow(orientation) =>
        self.resize_window(orientation, -10),
      command::Cmd::CloseWindow               =>
        self.close_window(),
      command::Cmd::Quit                      =>
        { self.quit = true; }
      command::Cmd::WinCmd(cmd)               => {
        self.windows.remove(&self.focus).
        map(|mut win| {
          self.handle_win_cmd(cmd, &mut win);
          self.windows.insert(self.focus.clone(), win); }).
        expect("Couldn't find focused window.");
      }
    }
    self.cmd_thread.ack_cmd().ok().expect("Command thread died.");
  }

  fn handle_win_cmd(&mut self, cmd: command::WinCmd, win: &mut Window) {
    match cmd {
      command::WinCmd::MoveCaret(adjustment) => {
        self.buffers.get(&win.buf_id).map(|buffer| {
          win.caret_mut().adjust(adjustment, buffer);
          let caret = *win.caret();
          win.view_mut().scroll_into_view(caret, buffer); });
        win.needs_redraw = true;
      }
      command::WinCmd::EnterNormalMode       => {
        self.cmd_thread.set_mode(win.normal_mode.clone(), 1).ok().expect(
          "Command thread died.");
        let id = win.buf_id;
        self.buffers.remove(&id).map(|buffer| {
          win.caret_mut().adjust(caret::Adjustment::Clamp, &buffer);
          for (_, win) in self.windows.iter_mut() {
            win.caret_mut_for(id).map(|caret|
              caret.adjust(caret::Adjustment::Clamp, &buffer));
          }
          self.buffers.insert(id, buffer); });
        win.needs_redraw = true;
      }
      command::WinCmd::EnterInsertMode       =>
        self.cmd_thread.set_mode(win.insert_mode.clone(), 1).ok().expect(
          "Command thread died."),
      command::WinCmd::OpenBuffer(path)      => {
        self.load_buffer(path.as_path()).map(|buf_id| {
          win.set_buf_id(buf_id);
          let screen::Rect(_, size) = win.rect;
          win.view_mut().set_size(size);
          self.buffers.get(&win.buf_id).map(|buffer| {
            let caret = *win.caret();
            win.view_mut().scroll_into_view(caret, buffer) }); });
      }
      command::WinCmd::Insert(string)        => {
        self.buffers.remove(&win.buf_id).map(|mut buffer| {
          let (insert_line, insert_col) =
            (win.caret().line(), win.caret().column());
          // update windows displaying the buffer, character by character
          let (mut c_line, mut c_col) = (insert_line, insert_col);
          for c in string.chars() {
            let newline = c == '\n';
            // update the caret of the focused window
            let (new_line, new_col) =
              if newline { (win.caret().line() + 1, 0) }
              else       { (win.caret().line(), win.caret().column() + 1) };
            win.caret_mut().adjust(
              caret::Adjustment::Set(new_line, new_col), &buffer);
            // update other windows which has viewed the buffer
            let id = win.buf_id;
            for (_, win) in self.windows.iter_mut() {
              if !win.has_buf_id(id) { continue }
              // keep the content of other views still if possible
              if win.view_for(id).unwrap().scroll_line() > c_line && newline {
                win.view_mut_for(id).unwrap().add_scroll(1, 0);
              }
              // update caret of other window
              let (cur_line, cur_col) =
                win.caret_for(id).map(|c| (c.line(), c.column())).unwrap();
              let (new_line, new_col) =
                if c_line < cur_line && newline { (cur_line + 1, cur_col) }
                else if cur_line == c_line && c_col <= cur_col {
                  if !newline { (cur_line, cur_col + 1) }
                  else { (cur_line, if c_col == 0 { 0 } else { c_col - 1 }) } }
                else { (cur_line, cur_col) };
              win.caret_mut_for(id).unwrap().adjust(
                caret::Adjustment::WeakSet(new_line, new_col), &buffer);
              win.needs_redraw = true;
            }
            if newline { c_line += 1; } else { c_col += 1; }
          }
          // insert string into buffer
          buffer.insert_at_line_column(string, insert_line, insert_col).ok().
            expect("View had invalid caret.");
          // ensure the caret is in the view
          let caret = *win.caret();
          win.view_mut().scroll_into_view(caret, &buffer);
          win.needs_redraw = true;
          self.buffers.insert(win.buf_id, buffer); });
      }
    }
  }
}

#[cfg(not(test))]
fn main() {
  let mut screen = screen::Screen::setup().unwrap();

  let (key_tx, key_rx) = std::sync::mpsc::channel();
  let _term_input = input::start(key_tx);

  let (cmd_tx, cmd_rx) = std::sync::mpsc::channel();
  cmd_tx.send(command::Cmd::ResetLayout).unwrap();
  cmd_tx.send(command::Cmd::WinCmd(command::WinCmd::OpenBuffer(
    PathBuf::from("src/rim.rs")))).unwrap();
  let cmd_thread = command::start(cmd_tx);

  let mut rim = Rim::new(cmd_thread);

  rim.cmd_thread.set_key_rx(key_rx).ok().expect("Command thread died.");

  // attempt to redraw at a regular interval
  let (draw_pulse_tx, draw_pulse_rx) = std::sync::mpsc::channel();
  thread::spawn(move || {
    loop {
      thread::sleep_ms(33);
      if draw_pulse_tx.send(()).is_err() { break; }
    }
  });

  loop {
    select!(
      cmd = cmd_rx.recv()       => {
        rim.handle_cmd(cmd.ok().expect("Command receiver died."));
        if rim.quit { break } else { continue }
      },
      _ = draw_pulse_rx.recv()  => {}
    );

    // clear/redraw/update/invalidate everything if the screen size changed
    if screen.update_size() {
      rim.frame.set_size(screen.size());
      rim.invalidate_frame();
      for (_, win) in rim.windows.iter_mut() { win.needs_redraw = true; }
      screen.clear();
    }

    let mut did_draw = rim.frame_needs_redraw;

    // draw frame if necessary
    if rim.frame_needs_redraw {
      rim.frame.draw_borders(&mut screen);
      rim.frame_needs_redraw = false;
    }

    // draw windows if necessary
    for (win_id, win) in rim.windows.iter() {
      if win.needs_redraw {
        rim.draw_window(win_id, &mut screen);
        did_draw = true;
      }
    }

    // mark windows as not needing redraw
    for (_, win) in rim.windows.iter_mut() { win.needs_redraw = false; }

    // set caret position and flush screen if we did any drawing
    if did_draw {
      rim.windows.get(&rim.focus).map(|win|
        rim.buffers.get(&win.buf_id).map(|buffer| {
          let screen::Rect(win_position, _) = win.rect;
          screen.set_cursor_position(win_position +
            win.view().caret_position(*win.caret(), buffer)); })).
      expect("Couldn't find focused window.");
      screen.flush();
    }
  }
}

#[cfg(not(test))]
fn default_mode() -> command::Mode {
  use keymap::Key;
  use command::Cmd;
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'h', mods: keymap::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Left));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'l', mods: keymap::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Right));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'k', mods: keymap::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Up));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'j', mods: keymap::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Down));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'v', mods: keymap::MOD_NONE}],
    Cmd::SplitWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 's', mods: keymap::MOD_NONE}],
    Cmd::SplitWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE}],
    Cmd::CloseWindow);
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: keymap::MOD_CTRL},
                       Key::Unicode{codepoint: '=', mods: keymap::MOD_NONE}],
    Cmd::ResetLayout);
  mode.keychain.bind(&[Key::Unicode{codepoint: 'y', mods: keymap::MOD_NONE}],
    Cmd::GrowWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'y', mods: keymap::MOD_CTRL}],
    Cmd::ShrinkWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'u', mods: keymap::MOD_NONE}],
    Cmd::GrowWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'u', mods: keymap::MOD_CTRL}],
    Cmd::ShrinkWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'n', mods: keymap::MOD_NONE}],
    Cmd::ShiftFocus(frame::WindowOrder::NextWindow));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'N', mods: keymap::MOD_NONE}],
    Cmd::ShiftFocus(frame::WindowOrder::PreviousWindow));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'q', mods: keymap::MOD_NONE}],
    Cmd::Quit);
  return mode;
}

#[cfg(not(test))]
fn default_normal_mode() -> command::Mode {
  use keymap::{Key, KeySym};
  use command::{Cmd, WinCmd};
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Left, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrev)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Right, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNext)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Up, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineUp)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Down, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineDown)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'h', mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrev)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'l', mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNext)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'k', mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineUp)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'j', mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineDown)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'i', mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertMode));
  mode.keychain.bind(&[Key::Fn{num: 1, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/rim.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 2, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/buffer.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 3, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/command.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 4, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/frame.rs"))));
  return mode;
}

#[cfg(not(test))]
fn default_insert_mode() -> command::Mode {
  use keymap::{Key, KeySym};
  use command::{Cmd, WinCmd};
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Escape, mods: keymap::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterNormalMode));
  fn fallback(key: keymap::Key) -> Option<command::Cmd> {
    match key {
      Key::Unicode{codepoint, mods: _}      => Some(format!("{}", codepoint)),
      Key::Sym{sym: KeySym::Enter, mods: _} => Some("\n".to_string()),
      _                                     => None,
    }.map(|string| command::Cmd::WinCmd(command::WinCmd::Insert(string)))
  }
  mode.fallback = fallback;
  return mode;
}
