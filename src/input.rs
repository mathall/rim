/*
 * Copyright (c) 2014 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate libc;
extern crate termkey;

use std::comm;

use keymap;

#[cfg(not(test))]
const STDIN_FILENO: libc::c_int = 0;

/*
 * TermInput is returned when starting listening for key events on a file
 * descriptor. It controls the life time of the terminal input loop. When the
 * TermInput is dropped, the terminal input loop will finish.
 */
pub struct TermInput {
  kill_tx: Sender<()>,
  died_rx: Receiver<()>,
}

impl Drop for TermInput {
  fn drop(&mut self) {
    self.kill_tx.send(());
    self.died_rx.recv();
  }
}

// start listening for terminal input on stdin
#[cfg(not(test))]
pub fn start(key_tx: Sender<keymap::Key>) -> TermInput {
  start_on_fd(STDIN_FILENO, key_tx)
}

// start listening for terminal input on the specified file descriptor
pub fn start_on_fd(fd: libc::c_int, key_tx: Sender<keymap::Key>) -> TermInput {
  let (kill_tx, kill_rx) = comm::channel();
  let (died_tx, died_rx) = comm::channel();
  spawn(proc() { input_loop(kill_rx, died_tx, key_tx, fd); });
  TermInput { kill_tx: kill_tx, died_rx: died_rx }
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

  #[deriving(PartialEq)]
  pub enum PollResult {
    PollReady,
    PollTimeout,
  }

  pub fn poll_fd(fd: c_int, timeout_ms: int) -> PollResult {
    const POLLIN: c_short = 1;
    let mut pollfd = Pollfd { fd: fd, events: POLLIN, revents: 0 };
    let num_fds = 1;
    let poll_result = unsafe {
      poll(&mut pollfd, num_fds, timeout_ms as c_int)
    };
    if poll_result < 0 { panic!("Unable to poll stdin."); }
    return if poll_result > 0 { PollReady } else { PollTimeout };
  }
}

fn input_loop(kill_rx: Receiver<()>, died_tx: Sender<()>,
              key_tx: Sender<keymap::Key>, fd: libc::c_int) {
  let is_alive = || kill_rx.try_recv() == Err(comm::Empty);
  let mut tk = termkey::TermKey::new(fd, termkey::c::TERMKEY_FLAG_CTRLC);

  while is_alive() {
    // check for available input
    let poll_timeout_ms = 10;
    if libc_poll::poll_fd(fd, poll_timeout_ms) == libc_poll::PollReady {
      // tell termkey that there's input available
      tk.advisereadable();
    }

    // empty the fd, translating and sending key events
    loop {
      match tk.getkey() {
        termkey::None_    => break,  // got nothing, done here
        termkey::Eof      => panic!("stdin closed, you're on your own."),
        termkey::Error{errno} =>
          panic!("termkey::geykey failed with error code {}", errno),
        termkey::Key(key) => translate_key(key),
        termkey::Again    =>
          // There's input available, but not enough to make a key event. Likely
          // escape has been pushed, which we want to force out and interpret as
          // a key on its own, otherwise oops.
          match tk.getkey_force() {
            termkey::Key(key) => translate_key(key),
            termkey::Eof      => panic!("stdin closed, you're on your own."),
            termkey::Error{errno} =>
              panic!("termkey::geykey_force failed with error code {}", errno),
            _                 => unreachable!(),
          },
      }.map(|key| key_tx.send(key));
    }
  }

  died_tx.send(());
}

fn translate_key(key: termkey::TermKeyEvent) -> Option<keymap::Key> {
  match key {
    termkey::FunctionEvent{num, mods}              =>
      Some(keymap::Fn{num: num, mods: translate_mods(mods)}),
    termkey::KeySymEvent{sym, mods}                =>
      Some(keymap::Sym{sym: translate_sym(sym), mods: translate_mods(mods)}),
    termkey::UnicodeEvent{codepoint, mods, utf8:_} =>
      Some(keymap::Unicode{codepoint: codepoint, mods: translate_mods(mods)}),
    _                                              =>
      None,
  }
}

fn translate_sym(sym: termkey::c::TermKeySym) -> keymap::KeySym {
  match sym {
    termkey::c::TERMKEY_SYM_UNKNOWN   => keymap::SymUnknown,
    termkey::c::TERMKEY_SYM_NONE      => keymap::SymNone,
    termkey::c::TERMKEY_SYM_BACKSPACE => keymap::SymBackspace,
    termkey::c::TERMKEY_SYM_TAB       => keymap::SymTab,
    termkey::c::TERMKEY_SYM_ENTER     => keymap::SymEnter,
    termkey::c::TERMKEY_SYM_ESCAPE    => keymap::SymEscape,
    termkey::c::TERMKEY_SYM_SPACE     => keymap::SymSpace,
    termkey::c::TERMKEY_SYM_DEL       => keymap::SymDel,
    termkey::c::TERMKEY_SYM_UP        => keymap::SymUp,
    termkey::c::TERMKEY_SYM_DOWN      => keymap::SymDown,
    termkey::c::TERMKEY_SYM_LEFT      => keymap::SymLeft,
    termkey::c::TERMKEY_SYM_RIGHT     => keymap::SymRight,
    termkey::c::TERMKEY_SYM_BEGIN     => keymap::SymBegin,
    termkey::c::TERMKEY_SYM_FIND      => keymap::SymFind,
    termkey::c::TERMKEY_SYM_INSERT    => keymap::SymInsert,
    termkey::c::TERMKEY_SYM_DELETE    => keymap::SymDelete,
    termkey::c::TERMKEY_SYM_SELECT    => keymap::SymSelect,
    termkey::c::TERMKEY_SYM_PAGEUP    => keymap::SymPageup,
    termkey::c::TERMKEY_SYM_PAGEDOWN  => keymap::SymPagedown,
    termkey::c::TERMKEY_SYM_HOME      => keymap::SymHome,
    termkey::c::TERMKEY_SYM_END       => keymap::SymEnd,
    termkey::c::TERMKEY_SYM_CANCEL    => keymap::SymCancel,
    termkey::c::TERMKEY_SYM_CLEAR     => keymap::SymClear,
    termkey::c::TERMKEY_SYM_CLOSE     => keymap::SymClose,
    termkey::c::TERMKEY_SYM_COMMAND   => keymap::SymCommand,
    termkey::c::TERMKEY_SYM_COPY      => keymap::SymCopy,
    termkey::c::TERMKEY_SYM_EXIT      => keymap::SymExit,
    termkey::c::TERMKEY_SYM_HELP      => keymap::SymHelp,
    termkey::c::TERMKEY_SYM_MARK      => keymap::SymMark,
    termkey::c::TERMKEY_SYM_MESSAGE   => keymap::SymMessage,
    termkey::c::TERMKEY_SYM_MOVE      => keymap::SymMove,
    termkey::c::TERMKEY_SYM_OPEN      => keymap::SymOpen,
    termkey::c::TERMKEY_SYM_OPTIONS   => keymap::SymOptions,
    termkey::c::TERMKEY_SYM_PRINT     => keymap::SymPrint,
    termkey::c::TERMKEY_SYM_REDO      => keymap::SymRedo,
    termkey::c::TERMKEY_SYM_REFERENCE => keymap::SymReference,
    termkey::c::TERMKEY_SYM_REFRESH   => keymap::SymRefresh,
    termkey::c::TERMKEY_SYM_REPLACE   => keymap::SymReplace,
    termkey::c::TERMKEY_SYM_RESTART   => keymap::SymRestart,
    termkey::c::TERMKEY_SYM_RESUME    => keymap::SymResume,
    termkey::c::TERMKEY_SYM_SAVE      => keymap::SymSave,
    termkey::c::TERMKEY_SYM_SUSPEND   => keymap::SymSuspend,
    termkey::c::TERMKEY_SYM_UNDO      => keymap::SymUndo,
    termkey::c::TERMKEY_SYM_KP0       => keymap::SymKP0,
    termkey::c::TERMKEY_SYM_KP1       => keymap::SymKP1,
    termkey::c::TERMKEY_SYM_KP2       => keymap::SymKP2,
    termkey::c::TERMKEY_SYM_KP3       => keymap::SymKP3,
    termkey::c::TERMKEY_SYM_KP4       => keymap::SymKP4,
    termkey::c::TERMKEY_SYM_KP5       => keymap::SymKP5,
    termkey::c::TERMKEY_SYM_KP6       => keymap::SymKP6,
    termkey::c::TERMKEY_SYM_KP7       => keymap::SymKP7,
    termkey::c::TERMKEY_SYM_KP8       => keymap::SymKP8,
    termkey::c::TERMKEY_SYM_KP9       => keymap::SymKP9,
    termkey::c::TERMKEY_SYM_KPENTER   => keymap::SymKPEnter,
    termkey::c::TERMKEY_SYM_KPPLUS    => keymap::SymKPPlus,
    termkey::c::TERMKEY_SYM_KPMINUS   => keymap::SymKPMinus,
    termkey::c::TERMKEY_SYM_KPMULT    => keymap::SymKPMult,
    termkey::c::TERMKEY_SYM_KPDIV     => keymap::SymKPDiv,
    termkey::c::TERMKEY_SYM_KPCOMMA   => keymap::SymKPComma,
    termkey::c::TERMKEY_SYM_KPPERIOD  => keymap::SymKPPeriod,
    termkey::c::TERMKEY_SYM_KPEQUALS  => keymap::SymKPEquals,
    termkey::c::TERMKEY_N_SYMS        => keymap::SymNSyms,
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
  extern crate time;

  use super::libc;
  use std::comm;
  use std::io::pipe::PipeStream;
  use std::io::timer;
  use std::os;
  use std::time::duration::Duration;

  use keymap;

  // Simulates stdin by writing bytes to a pipe, then listens for the key
  // outputs and matches with expectations.
  #[test]
  fn test_input() {
    // pairs of input bytes on "stdin" and corresponding expected key output
    let input_output_pairs = vec!(
      (vec!(0x61),
        keymap::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}),
      (vec!(0x1B, 0x61),
        keymap::Unicode{codepoint: 'a', mods: keymap::MOD_ALT}),
      (vec!(0x1B),
        keymap::Sym{sym: keymap::SymEscape, mods: keymap::MOD_NONE}),
      (vec!(0x61),
        keymap::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}),
      (vec!(0x03),
        keymap::Unicode{codepoint: 'c', mods: keymap::MOD_CTRL}),
      (vec!(0x1B, 0x5B, 0x41),
        keymap::Sym{sym: keymap::SymUp, mods: keymap::MOD_NONE}),
      (vec!(0x1B, 0x4F, 0x53),
        keymap::Fn{num: 4, mods: keymap::MOD_NONE}),
      (vec!(0xE3, 0x81, 0x82),
        keymap::Unicode{codepoint: 'あ', mods: keymap::MOD_NONE}),
    );

    let inputs: Vec<Vec<u8>> =
      input_output_pairs.iter().map(|&(ref i, _)| i.clone()).collect();
    let outputs: Vec<keymap::Key> =
      input_output_pairs.iter().map(|&(_, ref o)| o.clone()).collect();

    // set up communication channels
    let (key_tx, key_rx) = comm::channel();
    let os::Pipe { reader, writer } = unsafe { os::pipe().unwrap() };

    // start input listener
    let _term_input = super::start_on_fd(reader, key_tx);

    // make sure we quit in an orderly fashion even at failure
    let (close_writer_tx, close_writer_rx) = channel();
    struct PipeKiller { close_writer_tx: Sender<()>, reader: libc::c_int }
    impl Drop for PipeKiller {
      fn drop(&mut self) {
        unsafe { libc::close(self.reader); }
        self.close_writer_tx.send(());
      }
    }
    let _pipe_killer =
      PipeKiller { close_writer_tx: close_writer_tx, reader: reader };

    // simulate keyboard input
    spawn(proc() {
      let mut pipe_out = PipeStream::open(writer);
      for input in inputs.iter() {
        pipe_out.write(input.as_slice()).unwrap();
        // give termkey a chance to parse escape as a standalone key
        timer::sleep(Duration::milliseconds(1));
      }
      // keep the pipe alive until we're finished with it
      close_writer_rx.recv();
    });

    // receive key outputs and match with expectations
    let timeout_at = time::get_time() + Duration::milliseconds(100);
    for output in outputs.iter() {
      // just keep looping until the key arrives
      loop {
        match key_rx.try_recv() {
          Ok(key) => { assert_eq!(key, *output); break; }
          _       => (),
        }
        let time_now = time::get_time();
        if time_now > timeout_at { panic!("Timeout waiting for key event."); }
      }
    }
  }
}
