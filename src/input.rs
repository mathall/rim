/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate futures;
extern crate libc;
extern crate termkey;
extern crate tokio_core;

use std::thread;

use self::termkey::{TermKeyEvent, TermKeyResult};
use self::termkey::c::TermKeySym;

use futures::{Future, Stream};
use futures::sync::mpsc;
use futures::sync::oneshot;

use keymap;

#[cfg(not(test))]
const STDIN_FILENO: libc::c_int = 0;

/*
 * TermInput is returned when starting listening for key events on a file
 * descriptor. It controls the life time of the terminal input loop. When the
 * TermInput is dropped, the terminal input loop will finish.
 */
pub struct TermInput {
  kill_tx: Option<oneshot::Sender<()>>,
  died_rx: Option<oneshot::Receiver<()>>,
}

impl Drop for TermInput {
  fn drop(&mut self) {
    self.kill_tx.take().expect("TermInput already killed.").complete(());
    self.died_rx.take().expect("TermInput already killed.").wait().expect(
      "Input thread died prematurely.");
  }
}

// start listening for terminal input on stdin
#[cfg(not(test))]
pub fn start(key_tx: mpsc::UnboundedSender<keymap::Key>) -> TermInput {
  start_on_fd(STDIN_FILENO, key_tx)
}

// start listening for terminal input on the specified file descriptor
pub fn start_on_fd(fd: libc::c_int, key_tx: mpsc::UnboundedSender<keymap::Key>)
    -> TermInput {
  let (kill_tx, kill_rx) = oneshot::channel();
  let (died_tx, died_rx) = oneshot::channel();
  thread::spawn(move || { input_loop(kill_rx, died_tx, key_tx, fd); });
  TermInput { kill_tx: Some(kill_tx), died_rx: Some(died_rx) }
}

/*
 * Helper module for detecting available bytes on the file descriptor being
 * listened to.
 */
mod libc_poll {
  use super::libc::{c_int, c_long, c_short};

  #[repr(C)]
  struct Pollfd {
    fd: c_int,
    events: c_short,
    revents: c_short
  }

  extern {
    fn poll(fds: *mut Pollfd, nfds: c_long, timeout: c_int) -> c_int;
  }

  #[derive(PartialEq)]
  pub enum PollResult {
    Ready,
    Timeout,
  }

  pub fn poll_fd(fd: c_int, timeout_ms: u16) -> PollResult {
    const POLLIN: c_short = 1;
    let mut pollfd = Pollfd { fd: fd, events: POLLIN, revents: 0 };
    let num_fds = 1;
    let poll_result = unsafe {
      poll(&mut pollfd, num_fds, timeout_ms as c_int)
    };
    if poll_result < 0      { panic!("Unable to poll stdin."); }
    else if poll_result > 0 { PollResult::Ready }
    else                    { PollResult::Timeout }
  }
}

/*
 * Events to the input loop.
 */
#[derive(Clone)]
enum InputEvent {
  Continue,
  Break,
}

fn input_loop(kill_rx: oneshot::Receiver<()>, died_tx: oneshot::Sender<()>,
              key_tx: mpsc::UnboundedSender<keymap::Key>, fd: libc::c_int) {
  let mut tk = termkey::TermKey::new(fd, termkey::c::TERMKEY_FLAG_CTRLC);

  let mut core = tokio_core::reactor::Core::new().expect(
    "Couln't create a reactor core for the input loop.");

  let inf = futures::stream::repeat::<_, ()>(InputEvent::Continue);
  let killer = kill_rx.into_stream().map(|_| InputEvent::Break).map_err(|_| ());

  let input_loop = inf.select(killer).for_each(|event| {
    match event {
      InputEvent::Continue => (),
      InputEvent::Break    => return Err(()),
    }

    // check for available input
    let poll_timeout_ms = 10;
    if libc_poll::poll_fd(fd, poll_timeout_ms) == libc_poll::PollResult::Ready {
      // tell termkey that there's input available
      tk.advisereadable();
    }

    // empty the fd, translating and sending key events
    loop {
      match tk.getkey() {
        TermKeyResult::None_      => break,  // got nothing, done here
        TermKeyResult::Eof        =>
          panic!("stdin closed, you're on your own."),
        TermKeyResult::Error{err} =>
          panic!("termkey::geykey failed with error code {}", err),
        TermKeyResult::Key(key)   => translate_key(key),
        TermKeyResult::Again      =>
          // There's input available, but not enough to make a key event. Likely
          // escape has been pushed, which we want to force out and interpret as
          // a key on its own, otherwise oops.
          match tk.getkey_force() {
            TermKeyResult::Key(key)   => translate_key(key),
            TermKeyResult::Eof        =>
              panic!("stdin closed, you're on your own."),
            TermKeyResult::Error{err} =>
              panic!("termkey::geykey_force failed with error code {}", err),
            _                         => unreachable!(),
          },
      }.map(|key| key_tx.send(key).expect("Key channel died."));
    }

    Ok(())
  });

  core.run(input_loop).ok();

  died_tx.complete(());
}

fn translate_key(key: TermKeyEvent) -> Option<keymap::Key> {
  match key {
    TermKeyEvent::FunctionEvent{num, mods}           =>
      Some(keymap::Key::Fn{num: num, mods: translate_mods(mods)}),
    TermKeyEvent::KeySymEvent{sym, mods}             =>
      Some(keymap::Key::Sym{
        sym: translate_sym(sym), mods: translate_mods(mods)}),
    TermKeyEvent::UnicodeEvent{codepoint, mods, .. } =>
      Some(keymap::Key::Unicode{
        codepoint: codepoint, mods: translate_mods(mods)}),
    _                                                =>
      None,
  }
}

fn translate_sym(sym: TermKeySym) -> keymap::KeySym {
  match sym {
    TermKeySym::TERMKEY_SYM_UNKNOWN   => keymap::KeySym::Unknown,
    TermKeySym::TERMKEY_SYM_NONE      => keymap::KeySym::None,
    TermKeySym::TERMKEY_SYM_BACKSPACE => keymap::KeySym::Backspace,
    TermKeySym::TERMKEY_SYM_TAB       => keymap::KeySym::Tab,
    TermKeySym::TERMKEY_SYM_ENTER     => keymap::KeySym::Enter,
    TermKeySym::TERMKEY_SYM_ESCAPE    => keymap::KeySym::Escape,
    TermKeySym::TERMKEY_SYM_SPACE     => keymap::KeySym::Space,
    TermKeySym::TERMKEY_SYM_DEL       => keymap::KeySym::Del,
    TermKeySym::TERMKEY_SYM_UP        => keymap::KeySym::Up,
    TermKeySym::TERMKEY_SYM_DOWN      => keymap::KeySym::Down,
    TermKeySym::TERMKEY_SYM_LEFT      => keymap::KeySym::Left,
    TermKeySym::TERMKEY_SYM_RIGHT     => keymap::KeySym::Right,
    TermKeySym::TERMKEY_SYM_BEGIN     => keymap::KeySym::Begin,
    TermKeySym::TERMKEY_SYM_FIND      => keymap::KeySym::Find,
    TermKeySym::TERMKEY_SYM_INSERT    => keymap::KeySym::Insert,
    TermKeySym::TERMKEY_SYM_DELETE    => keymap::KeySym::Delete,
    TermKeySym::TERMKEY_SYM_SELECT    => keymap::KeySym::Select,
    TermKeySym::TERMKEY_SYM_PAGEUP    => keymap::KeySym::Pageup,
    TermKeySym::TERMKEY_SYM_PAGEDOWN  => keymap::KeySym::Pagedown,
    TermKeySym::TERMKEY_SYM_HOME      => keymap::KeySym::Home,
    TermKeySym::TERMKEY_SYM_END       => keymap::KeySym::End,
    TermKeySym::TERMKEY_SYM_CANCEL    => keymap::KeySym::Cancel,
    TermKeySym::TERMKEY_SYM_CLEAR     => keymap::KeySym::Clear,
    TermKeySym::TERMKEY_SYM_CLOSE     => keymap::KeySym::Close,
    TermKeySym::TERMKEY_SYM_COMMAND   => keymap::KeySym::Command,
    TermKeySym::TERMKEY_SYM_COPY      => keymap::KeySym::Copy,
    TermKeySym::TERMKEY_SYM_EXIT      => keymap::KeySym::Exit,
    TermKeySym::TERMKEY_SYM_HELP      => keymap::KeySym::Help,
    TermKeySym::TERMKEY_SYM_MARK      => keymap::KeySym::Mark,
    TermKeySym::TERMKEY_SYM_MESSAGE   => keymap::KeySym::Message,
    TermKeySym::TERMKEY_SYM_MOVE      => keymap::KeySym::Move,
    TermKeySym::TERMKEY_SYM_OPEN      => keymap::KeySym::Open,
    TermKeySym::TERMKEY_SYM_OPTIONS   => keymap::KeySym::Options,
    TermKeySym::TERMKEY_SYM_PRINT     => keymap::KeySym::Print,
    TermKeySym::TERMKEY_SYM_REDO      => keymap::KeySym::Redo,
    TermKeySym::TERMKEY_SYM_REFERENCE => keymap::KeySym::Reference,
    TermKeySym::TERMKEY_SYM_REFRESH   => keymap::KeySym::Refresh,
    TermKeySym::TERMKEY_SYM_REPLACE   => keymap::KeySym::Replace,
    TermKeySym::TERMKEY_SYM_RESTART   => keymap::KeySym::Restart,
    TermKeySym::TERMKEY_SYM_RESUME    => keymap::KeySym::Resume,
    TermKeySym::TERMKEY_SYM_SAVE      => keymap::KeySym::Save,
    TermKeySym::TERMKEY_SYM_SUSPEND   => keymap::KeySym::Suspend,
    TermKeySym::TERMKEY_SYM_UNDO      => keymap::KeySym::Undo,
    TermKeySym::TERMKEY_SYM_KP0       => keymap::KeySym::KP0,
    TermKeySym::TERMKEY_SYM_KP1       => keymap::KeySym::KP1,
    TermKeySym::TERMKEY_SYM_KP2       => keymap::KeySym::KP2,
    TermKeySym::TERMKEY_SYM_KP3       => keymap::KeySym::KP3,
    TermKeySym::TERMKEY_SYM_KP4       => keymap::KeySym::KP4,
    TermKeySym::TERMKEY_SYM_KP5       => keymap::KeySym::KP5,
    TermKeySym::TERMKEY_SYM_KP6       => keymap::KeySym::KP6,
    TermKeySym::TERMKEY_SYM_KP7       => keymap::KeySym::KP7,
    TermKeySym::TERMKEY_SYM_KP8       => keymap::KeySym::KP8,
    TermKeySym::TERMKEY_SYM_KP9       => keymap::KeySym::KP9,
    TermKeySym::TERMKEY_SYM_KPENTER   => keymap::KeySym::KPEnter,
    TermKeySym::TERMKEY_SYM_KPPLUS    => keymap::KeySym::KPPlus,
    TermKeySym::TERMKEY_SYM_KPMINUS   => keymap::KeySym::KPMinus,
    TermKeySym::TERMKEY_SYM_KPMULT    => keymap::KeySym::KPMult,
    TermKeySym::TERMKEY_SYM_KPDIV     => keymap::KeySym::KPDiv,
    TermKeySym::TERMKEY_SYM_KPCOMMA   => keymap::KeySym::KPComma,
    TermKeySym::TERMKEY_SYM_KPPERIOD  => keymap::KeySym::KPPeriod,
    TermKeySym::TERMKEY_SYM_KPEQUALS  => keymap::KeySym::KPEquals,
    TermKeySym::TERMKEY_N_SYMS        => keymap::KeySym::NSyms,
  }
}

fn translate_mods(mods: termkey::c::X_TermKey_KeyMod) -> keymap::KeyMod {
  let mut ret = keymap::MOD_NONE;
  if mods.contains(termkey::c::TERMKEY_KEYMOD_SHIFT) {
    ret.insert(keymap::MOD_SHIFT);
  }
  if mods.contains(termkey::c::TERMKEY_KEYMOD_ALT) {
    ret.insert(keymap::MOD_ALT);
  }
  if mods.contains(termkey::c::TERMKEY_KEYMOD_CTRL) {
    ret.insert(keymap::MOD_CTRL);
  }
  return ret;
}

#[cfg(test)]
mod test {
  extern crate tokio_timer;

  use super::libc;
  use std::mem;
  use std::thread;
  use std::time::Duration;

  use futures;
  use futures::{Future, Stream};
  use futures::sync::{mpsc, oneshot};

  use keymap;

  // Simulates stdin by writing bytes to a pipe, then listens for the key
  // outputs and matches with expectations.
  #[test]
  fn test_input() {
    // pairs of input bytes on "stdin" and corresponding expected key output
    let input_output_pairs = vec!(
      (vec!(0x61),
        keymap::Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}),
      (vec!(0x1B, 0x61),
        keymap::Key::Unicode{codepoint: 'a', mods: keymap::MOD_ALT}),
      (vec!(0x1B),
        keymap::Key::Sym{sym: keymap::KeySym::Escape, mods: keymap::MOD_NONE}),
      (vec!(0x61),
        keymap::Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}),
      (vec!(0x03),
        keymap::Key::Unicode{codepoint: 'c', mods: keymap::MOD_CTRL}),
      (vec!(0x1B, 0x5B, 0x41),
        keymap::Key::Sym{sym: keymap::KeySym::Up, mods: keymap::MOD_NONE}),
      (vec!(0x1B, 0x4F, 0x53),
        keymap::Key::Fn{num: 4, mods: keymap::MOD_NONE}),
      (vec!(0xE3, 0x81, 0x82),
        keymap::Key::Unicode{codepoint: 'あ', mods: keymap::MOD_NONE}),
    );

    let inputs: Vec<Vec<u8>> =
      input_output_pairs.iter().map(|&(ref input, _)| input.clone()).collect();
    let outputs: Vec<keymap::Key> =
      input_output_pairs.iter().map(|&(_, output)| output).collect();

    // set up communication channels
    let (key_tx, key_rx) = mpsc::unbounded();
    let (reader_fd, writer_fd) = unsafe {
      let mut fds = [0; 2];
      if libc::pipe(fds.as_mut_ptr()) != 0 { panic!("Failed to create pipe"); }
      (fds[0], fds[1])
    };

    // make sure we quit in an orderly fashion even at failure
    let (close_writer_tx, close_writer_rx) = oneshot::channel();
    struct PipeKiller {
      close_writer_tx: Option<oneshot::Sender<()>>,
      reader_fd: libc::c_int,
    }
    impl Drop for PipeKiller {
      fn drop(&mut self) {
        unsafe { libc::close(self.reader_fd); }
        self.close_writer_tx.take().unwrap().complete(());
      }
    }
    let _pipe_killer = PipeKiller {
      close_writer_tx: Some(close_writer_tx),
      reader_fd: reader_fd
    };

    // start input listener
    let _term_input = super::start_on_fd(reader_fd, key_tx);

    // simulate keyboard input
    thread::spawn(move || {
      for input in inputs.iter() {
        unsafe {
          let buffer = mem::transmute(&input[0]);
          let count = input.len() as libc::size_t;
          libc::write(writer_fd, buffer, count);
        }

        // give termkey a chance to parse escape as a standalone key
        thread::sleep(Duration::from_millis(1));
      }
      // keep the pipe alive until we're finished with it
      close_writer_rx.wait().unwrap();
      unsafe { libc::close(writer_fd); }
    });

    let timeout =
      tokio_timer::wheel().tick_duration(Duration::from_millis(10)).build().
      sleep(Duration::from_millis(100)).then(|_| Err(()));

    // match up received keys with the expected output
    let expected_output = futures::stream::iter(outputs.iter().map(Ok));
    let check = key_rx.zip(expected_output).for_each(|(key, output)| {
        assert_eq!(key, *output);
        Ok(())
      });

    check.select(timeout).wait().ok().expect("Timeout waiting for key event.");
  }
}
