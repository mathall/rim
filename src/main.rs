/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#[macro_use]
extern crate bitflags;
extern crate docopt;
extern crate futures;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate tokio_timer;

mod buffer;
mod caret;
mod command;
mod frame;
mod input;
mod keymap;
mod screen;
mod view;

#[cfg(not(test))]
use std::collections::HashMap;
#[cfg(not(test))]
use std::path::{Path, PathBuf};
#[cfg(not(test))]
use std::time::Duration;

#[cfg(not(test))]
use futures::{Future, Stream};

#[cfg(not(test))]
use buffer::Buffer;
#[cfg(not(test))]
use caret::Caret;
#[cfg(not(test))]
use command::{Cmd, CmdThread, WinCmd};
#[cfg(not(test))]
use frame::{Frame, FrameContext};
#[cfg(not(test))]
use keymap::{Key, KeySym, KeyMod};
#[cfg(not(test))]
use screen::Screen;
#[cfg(not(test))]
use view::View;

#[cfg(not(test))]
type BufferId = usize;

#[cfg(not(test))]
const INVALID_BUFFER_ID: BufferId = 0;

#[cfg(not(test))]
#[derive(Clone)]
struct Window {
  buf_id: BufferId,
  states: HashMap<BufferId, (Caret, View)>,
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

  fn caret_for(&self, buf_id: BufferId) -> Option<&Caret> {
    self.states.get(&buf_id).map(|&(ref c, _)| c)
  }

  fn caret(&self) -> &Caret {
    self.caret_for(self.buf_id).expect("Window lacked state for its buffer id.")
  }

  fn caret_mut_for(&mut self, buf_id: BufferId) -> Option<&mut Caret> {
    self.states.get_mut(&buf_id).map(|&mut (ref mut c, _)| c)
  }

  fn caret_mut(&mut self) -> &mut Caret {
    let buf_id = self.buf_id;
    self.caret_mut_for(buf_id).expect("Window lacked state for its buffer id.")
  }

  fn view_for(&self, buf_id: BufferId) -> Option<&View> {
    self.states.get(&buf_id).map(|&(_, ref v)| v)
  }

  fn view(&self) -> &View {
    self.view_for(self.buf_id).expect("Window lacked state for its buffer id.")
  }

  fn view_mut_for(&mut self, buf_id: BufferId) -> Option<&mut View> {
    self.states.get_mut(&buf_id).map(|&mut (_, ref mut v)| v)
  }

  fn view_mut(&mut self) -> &mut View {
    let buf_id = self.buf_id;
    self.view_mut_for(buf_id).expect("Window lacked state for its buffer id.")
  }

  fn set_buf_id(&mut self, buf_id: BufferId) {
    if !self.has_buf_id(buf_id) {
      self.states.insert(buf_id, (Caret::new(), View::new()));
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
  frame: Frame,
  frame_ctx: FrameContext,
  frame_needs_redraw: bool,
  windows: HashMap<frame::WindowId, Window>,
  focus: frame::WindowId,
  buffers: HashMap<BufferId, Buffer>,
  next_buf_id: BufferId,
  cmd_thread: CmdThread,
  quit: bool,
}

#[cfg(not(test))]
impl Rim {
  fn new(cmd_thread: CmdThread) -> Rim {
    let (frame, frame_ctx, first_win_id) = Frame::new();
    let mut windows = HashMap::new();
    let first_win = Window::new();
    cmd_thread.set_mode(default_mode(), 0);
    cmd_thread.set_mode(first_win.normal_mode.clone(), 1);
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
    Buffer::open(path).map(|buf| {
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
      self.cmd_thread.set_mode(win.normal_mode.clone(), 1));
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

  fn draw_window(&self, win_id: &frame::WindowId, screen: &mut Screen) {
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

  fn handle_cmd(&mut self, cmd: Cmd) {
    match cmd {
      Cmd::MoveFocus(direction)      => self.move_focus(direction),
      Cmd::ShiftFocus(window_order)  => self.shift_focus(window_order),
      Cmd::ResetLayout               => {
        self.frame.reset_layout();
        self.invalidate_frame();
      }
      Cmd::SplitWindow(orientation)  => self.split_window(orientation),
      Cmd::GrowWindow(orientation)   => self.resize_window(orientation, 10),
      Cmd::ShrinkWindow(orientation) => self.resize_window(orientation, -10),
      Cmd::CloseWindow               => self.close_window(),
      Cmd::QuitWindow                =>
        if self.windows.len() == 1 { self.quit = true; }
        else                       { self.close_window(); },
      Cmd::Quit                      => { self.quit = true; }
      Cmd::WinCmd(cmd)               => {
        self.windows.remove(&self.focus).
        map(|mut win| {
          self.handle_win_cmd(cmd, &mut win);
          self.windows.insert(self.focus.clone(), win); }).
        expect("Couldn't find focused window.");
      }
    }
    self.cmd_thread.ack_cmd();
  }

  fn handle_win_cmd(&mut self, cmd: WinCmd, win: &mut Window) {
    match cmd {
      WinCmd::MoveCaret(adjustment)          => {
        self.move_caret(adjustment, win);
      }
      WinCmd::PageUp                         => {
        let screen::Rect(_, screen::Size(rows, _)) = win.rect;
        self.scroll_view(-(rows as isize), win);
      }
      WinCmd::PageDown                       => {
        let screen::Rect(_, screen::Size(rows, _)) = win.rect;
        self.scroll_view(rows as isize, win);
      }
      WinCmd::HalfPageUp                     => {
        let screen::Rect(_, screen::Size(rows, _)) = win.rect;
        self.scroll_view(-(rows as isize) / 2, win);
      }
      WinCmd::HalfPageDown                   => {
        let screen::Rect(_, screen::Size(rows, _)) = win.rect;
        self.scroll_view(rows as isize / 2, win);
      }
      WinCmd::EnterNormalMode                => {
        self.set_win_cmd_mode(&win.normal_mode);
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
      WinCmd::EnterReplaceMode(replace_line) => {
        self.set_win_cmd_mode(&replace_mode(replace_line));
      }
      WinCmd::EnterInsertMode                => {
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::EnterInsertModeStartOfLine     => {
        self.move_caret(caret::Adjustment::Set(win.caret().line(), 0), win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::EnterInsertModeAppend          => {
        self.move_caret(caret::Adjustment::CharNextAppending, win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::EnterInsertModeAppendEndOfLine => {
        let col = self.buffers.get(&win.buf_id).map(|buf|
          buf.line_length(win.caret().line()).unwrap()).unwrap();
        self.move_caret(caret::Adjustment::Set(win.caret().line(), col), win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::EnterInsertModeNextLine        => {
        let col = self.buffers.get(&win.buf_id).map(|buf|
          buf.line_length(win.caret().line()).unwrap()).unwrap();
        self.move_caret(caret::Adjustment::Set(win.caret().line(), col), win);
        self.insert("\n".to_string(), win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::EnterInsertModePreviousLine    => {
        let line = win.caret().line();
        self.move_caret(caret::Adjustment::Set(line, 0), win);
        self.insert("\n".to_string(), win);
        self.move_caret(caret::Adjustment::Set(line, 0), win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
      WinCmd::OpenBuffer(path)               => {
        self.load_buffer(path.as_path()).map(|buf_id| {
          win.set_buf_id(buf_id);
          let screen::Rect(_, size) = win.rect;
          win.view_mut().set_size(size);
          self.buffers.get(&win.buf_id).map(|buffer| {
            let caret = *win.caret();
            win.view_mut().scroll_into_view(caret, buffer) }); });
      }
      WinCmd::SaveBuffer                     => {
        self.buffers.get(&win.buf_id).map(|buffer|
          buffer.write().ok().expect("Failed to save buffer."));
      }
      WinCmd::Insert(string)                 => {
        self.insert(string, win);
      }
      WinCmd::ReplaceLine(string)            => {
        self.replace(string, win);
      }
      WinCmd::Replace(string)                => {
        self.replace(string, win);
        self.set_win_cmd_mode(&win.normal_mode);
        self.move_caret(caret::Adjustment::CharPrev, win);
      }
      WinCmd::Backspace                      => {
        let mut start = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer|
          start.adjust(caret::Adjustment::CharPrevFlat, buffer));
        self.delete_range(start, win.caret().clone(), win);
      }
      WinCmd::Delete                         => {
        let mut end = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer|
          end.adjust(caret::Adjustment::CharNextFlat, buffer));
        self.delete_range(win.caret().clone(), end, win);
      }
      WinCmd::BackspaceOnLine                => {
        let mut start = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer|
          start.adjust(caret::Adjustment::CharPrev, buffer));
        self.delete_range(start, win.caret().clone(), win);
      }
      WinCmd::DeleteOnLine                   => {
        let mut end = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer|
          end.adjust(caret::Adjustment::CharNextAppending, buffer));
        self.delete_range(win.caret().clone(), end, win);
        self.move_caret(caret::Adjustment::Clamp, win);
      }
      WinCmd::DeleteLine                     => {
        let mut start = win.caret().clone();
        let mut end = win.caret().clone();
        let mut last_line = false;
        self.buffers.get(&win.buf_id).map(|buffer| {
          let line = win.caret().line();
          let line_len = buffer.line_length(line).unwrap();
          last_line = line + 1 == buffer.num_lines();
          start.adjust(caret::Adjustment::Set(line, 0), buffer);
          end.adjust(caret::Adjustment::Set(line, line_len), buffer);
          if !last_line { end.adjust(caret::Adjustment::CharNextFlat, buffer); }
          else { start.adjust(caret::Adjustment::CharPrevFlat, buffer); } });
        self.delete_range(start, end, win);
        if last_line {
          self.move_caret(caret::Adjustment::Set(win.caret().line(), 0), win);
        }
      }
      WinCmd::DeleteRestOfLine               => {
        let mut end = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer| {
          let line = win.caret().line();
          let line_len = buffer.line_length(line).unwrap();
          end.adjust(caret::Adjustment::Set(line, line_len), buffer) });
        self.delete_range(win.caret().clone(), end, win);
        self.move_caret(caret::Adjustment::Clamp, win);
      }
      WinCmd::ChangeRestOfLine               => {
        let mut end = win.caret().clone();
        self.buffers.get(&win.buf_id).map(|buffer| {
          let line = win.caret().line();
          let line_len = buffer.line_length(line).unwrap();
          end.adjust(caret::Adjustment::Set(line, line_len), buffer) });
        self.delete_range(win.caret().clone(), end, win);
        self.set_win_cmd_mode(&win.insert_mode);
      }
    }
  }

  fn set_win_cmd_mode(&mut self, mode: &command::Mode) {
    self.cmd_thread.set_mode(mode.clone(), 1);
  }

  fn scroll_view(&mut self, amount: isize, win: &mut Window) {
    let line = win.view().scroll_line();
    let new_line = std::cmp::max(line as isize + amount, 0) as usize;
    win.view_mut().set_scroll(new_line, 0);
    let caret_line = win.view().line_clamped_to_view(win.caret().line());
    self.move_caret(caret::Adjustment::Set(caret_line, 0), win);
    self.move_caret(caret::Adjustment::Clamp, win);
  }

  fn move_caret(&mut self, adjustment: caret::Adjustment, win: &mut Window) {
     self.buffers.get(&win.buf_id).map(|buffer| {
       win.caret_mut().adjust(adjustment, buffer);
       let caret = *win.caret();
       win.view_mut().scroll_into_view(caret, buffer); });
     win.needs_redraw = true;
  }

  fn replace(&mut self, string: String, win: &mut Window) {
    let mut end = win.caret().clone();
    self.buffers.get(&win.buf_id).map(|buffer|
      end.adjust(caret::Adjustment::CharNextAppending, buffer));
    self.delete_range(win.caret().clone(), end, win);
    self.insert(string, win);
  }

  fn insert(&mut self, string: String, win: &mut Window) {
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
          let (scroll_line, scroll_col) = win.view_for(id).map(|v|
            (v.scroll_line(), v.scroll_column())).unwrap();
          if scroll_line > c_line && newline {
            win.view_mut_for(id).unwrap().set_scroll(
              scroll_line + 1, scroll_col);
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

  fn delete_range(&mut self, start: Caret, end: Caret, win: &mut Window) {
    self.buffers.remove(&win.buf_id).map(|mut buffer| {
      let (start_line, start_col) = (start.line(), start.column());
      let (end_line, end_col) = (end.line(), end.column());
      if buffer.delete_range(start_line, start_col, end_line, end_col).is_ok() {
        // update other windows which has viewed the buffer
        let id = win.buf_id;
        for (_, win) in self.windows.iter_mut() {
          if !win.has_buf_id(id) { continue }
          // keep the content of other views still if possible
          let (scroll_line, scroll_col) = win.view_for(id).map(|v|
            (v.scroll_line(), v.scroll_column())).unwrap();
          let new_scroll_line = if scroll_line <= start_line { scroll_line }
                                else if scroll_line <= end_line { start_line }
                                else { scroll_line - end_line + start_line };
          win.view_mut_for(id).unwrap().set_scroll(new_scroll_line, scroll_col);
          // update caret of other window
          let (cur_line, cur_col) =
            win.caret_for(id).map(|c| (c.line(), c.column())).unwrap();
          let clamped_start_col = if start_col > 0 { start_col - 1 } else { 0 };
          let new_col =
            if cur_line < start_line || cur_line > end_line { cur_col }
            else if start_line == end_line {
              if cur_col < start_col { cur_col }
              else if cur_col < end_col { clamped_start_col }
              else { cur_col - end_col + start_col } }
            else {
              if cur_line == start_line {
                if cur_col < start_col { cur_col } else { clamped_start_col } }
              else if cur_line == end_line && cur_col >= end_col {
                cur_col - end_col + start_col }
              else { 0 } };
          let new_line =
            if cur_line >= start_line && cur_line <= end_line { start_line }
            else if cur_line > end_line { cur_line - end_line + start_line }
            else { cur_line };
          win.caret_mut_for(id).unwrap().adjust(
            caret::Adjustment::WeakSet(new_line, new_col), &buffer);
          win.needs_redraw = true;
        }
        // update the caret of the focused window and scroll it into view
        win.caret_mut().adjust(
          caret::Adjustment::Set(start_line, start_col), &buffer);
        let caret = *win.caret();
        win.view_mut().scroll_into_view(caret, &buffer);
        win.needs_redraw = true;
      }
      self.buffers.insert(win.buf_id, buffer); });
  }
}

#[cfg(not(test))]
const USAGE: &'static str = "
Rim - Vim-style text editor.

Usage:
  rim [<file>]
  rim -h | --help
  rim --version

Options:
  -h --help        Show this screen.
  --version        Show version.
";

#[cfg(not(test))]
#[derive(Deserialize)]
struct Args {
  arg_file: Option<String>,
  flag_version: bool,
}

/*
 * Events to the main loop.
 */
#[cfg(not(test))]
#[derive(Clone)]
enum Event {
  HandleCmd(Cmd),
  Draw,
}

#[cfg(not(test))]
fn main() {
  let args: Args = docopt::Docopt::new(USAGE).and_then(|d| d.deserialize()).
                   unwrap_or_else(|e| e.exit());
  if args.flag_version {
    println!("Rim - {}", env!("CARGO_PKG_VERSION"));
    return;
  }
  let mut screen = Screen::setup().unwrap();

  let (key_tx, key_rx) = futures::sync::mpsc::unbounded();
  let _term_input = input::start(key_tx);

  let (cmd_tx, cmd_rx) = futures::sync::mpsc::unbounded();
  cmd_tx.unbounded_send(Cmd::ResetLayout).unwrap();
  let filename = args.arg_file.unwrap_or("src/rim.rs".to_string());
  cmd_tx.unbounded_send(Cmd::WinCmd(WinCmd::OpenBuffer(
    PathBuf::from(&filename)))).unwrap();
  let cmd_thread = command::start(key_rx, cmd_tx);

  let mut rim = Rim::new(cmd_thread);

  // attempt to redraw at a regular interval
  let draw_pulse =
    tokio_timer::wheel().tick_duration(Duration::from_millis(10)).build().
    interval(Duration::from_millis(33)).map(|_| Event::Draw).map_err(|_| ());

  let cmd_stream = cmd_rx.map(Event::HandleCmd);

  let rim_loop = cmd_stream.select(draw_pulse).for_each(|event| {
    match event {
      Event::HandleCmd(cmd) => rim.handle_cmd(cmd),
      Event::Draw           => (),
    }

    if rim.quit { return Err(()); }

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

    Ok(())
  });

  rim_loop.wait().ok();
}

#[cfg(not(test))]
fn default_mode() -> command::Mode {
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'h', mods: KeyMod::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Left));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'l', mods: KeyMod::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Right));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'k', mods: KeyMod::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Up));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'j', mods: KeyMod::MOD_NONE}],
    Cmd::MoveFocus(frame::Direction::Down));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'v', mods: KeyMod::MOD_NONE}],
    Cmd::SplitWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 's', mods: KeyMod::MOD_NONE}],
    Cmd::SplitWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'c', mods: KeyMod::MOD_NONE}],
    Cmd::CloseWindow);
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: '=', mods: KeyMod::MOD_NONE}],
    Cmd::ResetLayout);
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'q', mods: KeyMod::MOD_CTRL}],
    Cmd::QuitWindow);
  mode.keychain.bind(&[Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_CTRL},
                       Key::Unicode{codepoint: 'q', mods: KeyMod::MOD_NONE}],
    Cmd::QuitWindow);
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Left, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrev)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Right, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNext)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Up, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineUp)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Down, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineDown)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Home, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::StartOfLine)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::End, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::EndOfLine)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Pageup, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::PageUp));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Pagedown, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::PageDown));
  // for testing purposes
  mode.keychain.bind(&[Key::Unicode{codepoint: 'y', mods: KeyMod::MOD_NONE}],
    Cmd::GrowWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'y', mods: KeyMod::MOD_CTRL}],
    Cmd::ShrinkWindow(frame::Orientation::Horizontal));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'u', mods: KeyMod::MOD_NONE}],
    Cmd::GrowWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'u', mods: KeyMod::MOD_CTRL}],
    Cmd::ShrinkWindow(frame::Orientation::Vertical));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'n', mods: KeyMod::MOD_NONE}],
    Cmd::ShiftFocus(frame::WindowOrder::NextWindow));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'N', mods: KeyMod::MOD_NONE}],
    Cmd::ShiftFocus(frame::WindowOrder::PreviousWindow));
  // for convenience until a proper command line is implemented
  mode.keychain.bind(&[Key::Unicode{codepoint: ':', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'q', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'a', mods: KeyMod::MOD_NONE},
                       Key::Sym{sym: KeySym::Enter, mods: KeyMod::MOD_NONE}],
    Cmd::Quit);
  mode.keychain.bind(&[Key::Unicode{codepoint: ':', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'q', mods: KeyMod::MOD_NONE},
                       Key::Sym{sym: KeySym::Enter, mods: KeyMod::MOD_NONE}],
    Cmd::QuitWindow);
  mode.keychain.bind(&[Key::Unicode{codepoint: ':', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'w', mods: KeyMod::MOD_NONE},
                       Key::Sym{sym: KeySym::Enter, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::SaveBuffer));
  return mode;
}

#[cfg(not(test))]
fn default_normal_mode() -> command::Mode {
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Unicode{codepoint: 'h', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrev)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'l', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNext)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'k', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineUp)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'j', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LineDown)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Insert, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertMode));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'i', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertMode));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'I', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertModeStartOfLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'a', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertModeAppend));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'A', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertModeAppendEndOfLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'o', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertModeNextLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'O', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterInsertModePreviousLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'g', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'g', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::FirstLine)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'G', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::LastLine)));
  mode.keychain.bind(&[Key::Unicode{codepoint: ' ', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNextFlat)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Space, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharNextFlat)));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Del, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrevFlat)));
  mode.keychain.bind(
    &[Key::Sym{sym: KeySym::Backspace, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::CharPrevFlat)));
  mode.keychain.bind(&[Key::Unicode{codepoint: '0', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::StartOfLine)));
  mode.keychain.bind(&[Key::Unicode{codepoint: '$', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::MoveCaret(caret::Adjustment::EndOfLine)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: KeyMod::MOD_CTRL}],
    Cmd::WinCmd(WinCmd::PageUp));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'f', mods: KeyMod::MOD_CTRL}],
    Cmd::WinCmd(WinCmd::PageDown));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'u', mods: KeyMod::MOD_CTRL}],
    Cmd::WinCmd(WinCmd::HalfPageUp));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'd', mods: KeyMod::MOD_CTRL}],
    Cmd::WinCmd(WinCmd::HalfPageDown));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Delete, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::DeleteOnLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'x', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::DeleteOnLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'X', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::BackspaceOnLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'd', mods: KeyMod::MOD_NONE},
                       Key::Unicode{codepoint: 'd', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::DeleteLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'D', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::DeleteRestOfLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'C', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::ChangeRestOfLine));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'r', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterReplaceMode(false)));
  mode.keychain.bind(&[Key::Unicode{codepoint: 'R', mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterReplaceMode(true)));
  // for testing purposes
  mode.keychain.bind(&[Key::Fn{num: 1, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/rim.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 2, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/buffer.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 3, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/command.rs"))));
  mode.keychain.bind(&[Key::Fn{num: 4, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::OpenBuffer(PathBuf::from("src/frame.rs"))));
  return mode;
}

#[cfg(not(test))]
fn key_to_string(key: Key) -> Option<String> {
  match key {
    Key::Unicode{codepoint, .. }      => Some(format!("{}", codepoint)),
    Key::Sym{sym: KeySym::Enter, .. } => Some("\n".to_string()),
    _                                 => None,
  }
}

#[cfg(not(test))]
fn default_insert_mode() -> command::Mode {
  let mut mode = command::Mode::new();
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Escape, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::EnterNormalMode));
  mode.keychain.bind(
    &[Key::Sym{sym: KeySym::Backspace, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::Backspace));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Del, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::Backspace));
  mode.keychain.bind(&[Key::Sym{sym: KeySym::Delete, mods: KeyMod::MOD_NONE}],
    Cmd::WinCmd(WinCmd::Delete));
  fn fallback(key: keymap::Key) -> Option<Cmd> {
    key_to_string(key).map(|string| Cmd::WinCmd(WinCmd::Insert(string)))
  }
  mode.fallback = fallback;
  return mode;
}

#[cfg(not(test))]
fn replace_mode(replace_line: bool) -> command::Mode {
  let mut mode = command::Mode::new();
  fn replace_line_fallback(key: Key) -> Option<Cmd> {
    key_to_string(key).map(|string| Cmd::WinCmd(WinCmd::ReplaceLine(string))).
    or(Some(Cmd::WinCmd(WinCmd::EnterNormalMode)))
  }
  fn replace_fallback(key: Key) -> Option<Cmd> {
    key_to_string(key).map(|string| Cmd::WinCmd(WinCmd::Replace(string))).
    or(Some(Cmd::WinCmd(WinCmd::EnterNormalMode)))
  }
  mode.fallback = if replace_line { replace_line_fallback }
                  else            { replace_fallback };
  return mode;
}
