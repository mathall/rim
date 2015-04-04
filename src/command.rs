/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::collections::{HashMap, VecDeque, VecMap};
use std::mem;
use std::sync::mpsc::{channel, Receiver, SendError, Sender};
use std::thread;

use frame;
use keymap;
use view;

#[cfg(not(test))]
const TIMEOUT: u32 = 3000;
#[cfg(test)]
const TIMEOUT: u32 = 100;

/*
 * CmdThread is the interface for instructing the command thread. It is returned
 * when the thread is started and it will tear down the thread when dropped.
 * Modes must be set with priority order (high number, high priority) for
 * arriving keys to form commands. A higher prioritized mode will always be
 * given a first chance to process keys. A mode's keychain may decide it wants
 * to wait for more keys before possibly producing a command, however, it may
 * only do so for a set time. After a timeout, the keys arrived so far will be
 * drained from the buffer by forcing keychains to only match keys to complete
 * commands.
 * Between each command produced and sent to the client, the client must ack the
 * arrival of the command before the command thread can proceed to process
 * further keys. This allows the client to for instance set a new mode to be
 * used for the keys ahead.
 * A key receiver must be set after that the thread has been setup to process
 * keys as desired for the first time.
 */
pub struct CmdThread {
  kill_tx: Sender<()>,
  died_rx: Receiver<()>,
  msg_tx: Sender<Msg>,
}

impl CmdThread {
  pub fn set_mode(&self, mode: Mode, num: usize) -> Result<(), ()> {
    self.msg_tx.send(Msg::SetMode(mode, num)).map_err(|_| ())
  }

  pub fn set_key_rx(&self, key_rx: Receiver<keymap::Key>)
      -> Result<(), Receiver<keymap::Key>> {
    self.msg_tx.send(Msg::SetKeyRx(key_rx)).map_err(|SendError(msg)|
      if let Msg::SetKeyRx(rx) = msg { rx } else { unreachable!() })
  }

  pub fn ack_cmd(&self) -> Result<(), ()> {
    self.msg_tx.send(Msg::AckCmd).map_err(|_| ())
  }
}

impl Drop for CmdThread {
  fn drop(&mut self) {
    self.kill_tx.send(()).ok().expect("Command thread died prematurely.");
    self.died_rx.recv().ok().expect("Command thread died prematurely.");
  }
}

/*
 * Messages used by CmdThread to instruct the command thread.
 */
enum Msg {
  SetMode(Mode, usize),
  SetKeyRx(Receiver<keymap::Key>),
  AckCmd,
}

// start the command thread
pub fn start(cmd_tx: Sender<Cmd>) -> CmdThread {
  let (kill_tx, kill_rx) = channel();
  let (died_tx, died_rx) = channel();
  let (msg_tx, msg_rx) = channel();
  thread::spawn(move || { cmd_thread(kill_rx, died_tx, msg_rx, cmd_tx); });
  CmdThread { kill_tx: kill_tx, died_rx: died_rx, msg_tx: msg_tx }
}

fn cmd_thread(kill_rx: Receiver<()>, died_tx: Sender<()>, msg_rx: Receiver<Msg>,
              cmd_tx: Sender<Cmd>) {
  // Set up a helper thread along with communication channels for handling
  // timeout requests.
  let (timeout_tx, timeout_rx) = channel();
  let (oneshot_tx, oneshot_rx) = channel();
  thread::spawn(move || {
    let oneshot = |ms: u32| -> Receiver<()> {
      let (tx, rx) = channel();
      thread::spawn(move || { thread::sleep_ms(ms); tx.send(()).ok(); });
      return rx;
    };
    let mut timeout = oneshot(TIMEOUT);
    let mut response = None;
    loop {
      select!(
        res = oneshot_rx.recv() => match res {
          Ok(r) => { response = Some(r); },
          _     => break,
        },
        res = timeout.recv()    => match res {
          Ok(_) => { response.take().map(|r| timeout_tx.send(r).ok()); },
          _     => break,
        }
      );
      timeout = oneshot(TIMEOUT);
    }
  });

  // placeholder channel until a proper key receiver is set by a message, or if
  // the key channel dies and we need to fall back on a placeholder again
  let (mut _key_tx, mut key_rx) = channel();

  // assures that no commands are sent until the previous one has been
  // acknowledged
  let mut cmd_acknowledged = true;

  // unprocessed keys along with sequence numbers for the first key to be
  // processed (front) and one beyond the last arrived key (back)
  let mut keys = VecDeque::new();
  let mut back_seq: usize = 0;
  let mut front_seq: usize = 0;

  // modes used to make commands out of a stream of keys, a higher index/key
  // implies higher priority
  let mut modes = VecMap::new();

  // keeps track of what we want to drain right now and, if we've already
  // requested a timeout, the seq we want to drain when that timeout hits
  let mut drain_seq = back_seq;
  let mut requested_drain_seq = None;

  loop {
    let mut new_key_rx = None;
    let mut drop_key_rx = false;
    select!(
      _ = kill_rx.recv()      => break,
      seq = timeout_rx.recv() => {
        drain_seq = seq.ok().expect("Timeout channel died.");
      },
      key = key_rx.recv()     => {
        key.map_err(|_| drop_key_rx = true).map(|key| {
          keys.push_back(key);
          back_seq += 1;
          requested_drain_seq = None; }).ok();
      },
      msg = msg_rx.recv()     => {
        match msg.ok().expect("Message channel died.") {
          Msg::SetMode(mode, num) => { modes.insert(num, mode); }
          Msg::SetKeyRx(rx)       => { new_key_rx = Some(rx); }
          Msg::AckCmd             => { cmd_acknowledged = true; }
        }
      }
    );
    if drop_key_rx { let (tx, rx) = channel(); _key_tx = tx; key_rx = rx; }
    new_key_rx.map(|rx| key_rx = rx);

    // work through the arrived keys
    while keys.len() > 0 {
      assert!(back_seq >= front_seq && back_seq >= drain_seq);
      // determine amount of keys to consider and whether to drain those keys
      let drain = drain_seq > front_seq;
      let num_keys = if drain { drain_seq } else { back_seq } - front_seq;
      // match keys with modes in priority order
      let mut match_result = MatchResult::None;
      for (_, mode) in modes.iter().rev() {
        // first match by keychain
        match_result =
          mode.keychain.match_keys(&mut keys.iter().take(num_keys), drain);
        // use the mode's fallback if the keychain didn't match anything
        if match_result == MatchResult::None {
          (mode.fallback)(keys[0]).map(|cmd|
            match_result = MatchResult::Complete(cmd, 1));
        }
        // proceed to next mode only if no match was made
        if match_result != MatchResult::None { break; }
      }

      // act on the match result
      match match_result {
        MatchResult::None               => { keys.pop_front(); front_seq += 1; }
        MatchResult::Partial(num)       => {
          let new_seq = front_seq + num;
          if requested_drain_seq.map(|seq| new_seq > seq).unwrap_or(true) {
            oneshot_tx.send(new_seq).ok().expect("Oneshot channel died.");
            requested_drain_seq = Some(new_seq);
          }
          break;
        }
        MatchResult::Complete(cmd, num) => {
          if !cmd_acknowledged { break; } else {
            for _ in 0..num { keys.pop_front(); front_seq += 1; }
            cmd_tx.send(cmd).ok().expect("Command channel died.");
            cmd_acknowledged = false;
          }
        }
      }
    }
  }

  died_tx.send(()).unwrap();
}

/*
 * A Mode is what the command thread use to form commands out of keys. It
 * consist of a keychain and a fallback command contructor for when the keychain
 * doesn't match on a string of keys.
 */
#[derive(Clone)]
pub struct Mode {
  pub keychain: Keychain,
  pub fallback: fn(keymap::Key) -> Option<Cmd>
}

impl Mode {
  pub fn new() -> Mode {
    fn fallback(_: keymap::Key) -> Option<Cmd> { None }
    Mode { keychain: Keychain::new(), fallback: fallback }
  }
}

/*
 * Result type for matching a string of keys against a Keychain. A complete or
 * partial match result will also contain the number of keys that matched.
 */
#[derive(PartialEq)]
#[cfg_attr(test, derive(Debug))]
enum MatchResult {
  None,
  Partial(usize),
  Complete(Cmd, usize),
}

/*
 * Keychain maps strings of keys to commands. It is a trie tree with commands in
 * the leafs as well as optional commands in the internal nodes, allowing
 * shorter complete matchings to overshadow longer partial ones depending on
 * mode.
 */
pub enum Keychain {
  Node(HashMap<keymap::Key, Keychain>, Option<Cmd>),
  Cmd(Cmd),
}

impl Keychain {
  pub fn new() -> Keychain {
    Keychain::Node(HashMap::new(), None)
  }

  pub fn bind(&mut self, keys: &[keymap::Key], new_cmd: Cmd) {
    let new_self = match mem::replace(self, Keychain::new()) {
      Keychain::Node(mut map, opt_cmd) => {
        match keys {
          []            =>
            if map.len() > 0 { Keychain::Node(map, Some(new_cmd)) }
            else             { Keychain::Cmd(new_cmd) },
          [key, keys..] => {
            let mut subchain = map.remove(&key).unwrap_or(Keychain::new());
            subchain.bind(keys, new_cmd);
            map.insert(key, subchain);
            Keychain::Node(map, opt_cmd)
          }
        }
      }
      Keychain::Cmd(old_cmd)           => {
        match keys {
          []     => Keychain::Cmd(new_cmd),
          keys   => {
            let mut chain = Keychain::Node(HashMap::new(), Some(old_cmd));
            chain.bind(keys, new_cmd);
            chain
          }
        }
      }
    };
    *self = new_self;
  }

  fn match_keys<'l, It>(&self, keys: &mut It, force: bool) -> MatchResult
      where It: Iterator<Item=&'l keymap::Key> {
    let res_from_opt = |opt: Option<Cmd>|
      opt.map(|cmd| MatchResult::Complete(cmd, 0)).unwrap_or(MatchResult::None);
    match self {
      &Keychain::Cmd(cmd)               => MatchResult::Complete(cmd, 0),
      &Keychain::Node(ref map, opt_cmd) =>
        keys.next().map(|key|
          map.get(key).map(|chain|
            match chain.match_keys(keys, force) {
              MatchResult::None               => res_from_opt(opt_cmd),
              MatchResult::Partial(num)       =>
                if force { res_from_opt(opt_cmd) }
                else     { MatchResult::Partial(num + 1) },
              MatchResult::Complete(cmd, num) =>
                MatchResult::Complete(cmd, num + 1),
            }).
          unwrap_or_else(|| res_from_opt(opt_cmd))).
        unwrap_or_else(||
          match if force { res_from_opt(opt_cmd) } else { MatchResult::None } {
            MatchResult::None if map.len() > 0 => MatchResult::Partial(0),
            otherwise                          => otherwise,
          }),
    }
  }
}

impl Clone for Keychain {
  fn clone(&self) -> Keychain {
    match self {
      &Keychain::Node(ref map, cmd) =>
        Keychain::Node(map.iter().map(|(k, v)| (*k, v.clone())).collect(), cmd),
      &Keychain::Cmd(cmd)           => Keychain::Cmd(cmd),
    }
  }
}

/*
 * Commands for rim.
 */
#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(test, derive(Debug))]
#[cfg_attr(test, allow(dead_code))]  // the tests don't make use of all commands
pub enum Cmd {
  MoveFocus(frame::Direction),
  ShiftFocus(frame::WindowOrder),
  ResetLayout,
  SplitWindow(frame::Orientation),
  GrowWindow(frame::Orientation),
  ShrinkWindow(frame::Orientation),
  CloseWindow,
  Quit,
  WinCmd(WinCmd),
}

/*
 * Commands intended for the focused window.
 */
#[derive(Clone, Copy, PartialEq)]
#[cfg_attr(test, derive(Debug))]
#[cfg_attr(test, allow(dead_code))]  // the tests don't make use of all commands
pub enum WinCmd {
  MoveCaret(view::CaretMovement),
  EnterNormalMode,
  EnterInsertMode,
}

#[cfg(test)]
mod test {
  use frame;
  use keymap;
  use keymap::Key;

  // Sends keys on one thread and receives commands and compares with
  // expectations on another. Each vector of keys will be sent in one go, with a
  // sleep inbetween to trigger a timeout.
  fn run_test<S, C>(inputs: Vec<Vec<Key>>, outputs: Vec<super::Cmd>, setup: S,
                    callback: C)
      where S: Fn(&super::CmdThread) -> (),
            C: Fn(super::Cmd, &super::CmdThread) -> () {
    use std::sync::mpsc::channel;
    use std::thread;

    // set up communication channels
    let (key_tx, key_rx) = channel();
    let (cmd_tx, cmd_rx) = channel();

    // start and setup command thread
    let cmd_thread = super::start(cmd_tx);
    setup(&cmd_thread);
    cmd_thread.set_key_rx(key_rx).ok().unwrap();

    // send off key events on separate thread
    thread::spawn(move || {
      for keys in inputs.iter() {
        for key in keys.iter() { key_tx.send(*key).unwrap(); }
        thread::sleep_ms(super::TIMEOUT + 10);
      }
    });

    // receive commands and match with expectations
    let (timeout_tx, timeout_rx) = channel();
    thread::spawn(move || {
      thread::sleep_ms(1000); timeout_tx.send(()).ok();
    });
    for output in outputs.iter() {
      select!(
        cmd = cmd_rx.recv()   => {
          assert_eq!(cmd.unwrap(), *output);
          callback(cmd.unwrap(), &cmd_thread);
          cmd_thread.ack_cmd().unwrap();
        },
        _ = timeout_rx.recv() => { panic!("Timeout waiting for command."); }
      );
    }
  }

  fn mode_0() -> super::Mode {
    let mut mode = super::Mode::new();
    mode.keychain.bind(&[Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::Quit);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::ResetLayout);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::CloseWindow);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'd', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'd', mods: keymap::MOD_NONE}],
      super::Cmd::CloseWindow);
    return mode;
  }

  fn mode_1() -> super::Mode {
    let mut mode = super::Mode::new();
    mode.keychain.bind(&[Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::MoveFocus(frame::Direction::Left));
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::MoveFocus(frame::Direction::Right));
    mode.keychain.bind(&[Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::MoveFocus(frame::Direction::Up));
    return mode;
  }

  fn mode_2() -> super::Mode {
    let mut mode = super::Mode::new();
    mode.keychain.bind(&[Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::Quit);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::ResetLayout);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}],
      super::Cmd::CloseWindow);
    return mode;
  }

  fn mode_3() -> super::Mode {
    let mut mode = super::Mode::new();
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}],
      super::Cmd::MoveFocus(frame::Direction::Right));
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}],
      super::Cmd::MoveFocus(frame::Direction::Up));
    return mode;
  }

  fn mode_4() -> super::Mode {
    let mut mode = super::Mode::new();
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}],
      super::Cmd::Quit);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}],
      super::Cmd::ResetLayout);
    mode.keychain.bind(&[Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
                         Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}],
      super::Cmd::CloseWindow);
    return mode;
  }

  #[test]
  fn single_mode() {
    let inputs = vec!(vec!(
      // Quit
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      // Nothing
      Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'y', mods: keymap::MOD_NONE},
      // ResetLayout
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      // Nothing
      Key::Unicode{codepoint: 'z', mods: keymap::MOD_NONE},
      // CloseWindow
      Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      // Nothing (partial CloseWindow)
      Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      // ResetLayout
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::Quit,
      super::Cmd::ResetLayout,
      super::Cmd::CloseWindow,
      super::Cmd::ResetLayout);
    let setup = |cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_0(), 0).unwrap(); };
    let callback = |_: super::Cmd, _: &super::CmdThread| {};
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn multiple_modes() {
    let inputs = vec!(vec!(
      // MoveFocus(Left)
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      // Nothing
      Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE},
      // MoveFocus(Left)
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      // mistype cbabaa, get CloseWindow from mode0 (cba), MoveFocus(Right)
      // overriding from mode1 (ba), leaving c in the buffer
      Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
      // MoveFocus(Up) by properly typing cbabaa (c left from above)
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::MoveFocus(frame::Direction::Left),
      super::Cmd::MoveFocus(frame::Direction::Left),
      super::Cmd::CloseWindow,
      super::Cmd::MoveFocus(frame::Direction::Right),
      super::Cmd::MoveFocus(frame::Direction::Up));
    let setup = |cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_0(), 0).unwrap();
      cmd_thread.set_mode(mode_1(), 1).unwrap(); };
    let callback = |_: super::Cmd, _: &super::CmdThread| {};
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn single_mode_timeout() {
    let inputs = vec!(
      vec!(
        // CloseWindow
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        // Timeout: ResetLayout
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}),
      vec!(
        // Timeout: Nothing
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE}),
      vec!(
        // Quit
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::CloseWindow,
      super::Cmd::ResetLayout,
      super::Cmd::Quit);
    let setup = |cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_2(), 0).unwrap(); };
    let callback = |_: super::Cmd, _: &super::CmdThread| {};
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn multiple_mode_timeout() {
    let inputs = vec!(vec!(
      // Timeout: MoveFocus(Right) (bab override on mode1), throw c away,
      // Quit (mode0), ResetLayout (mode0)
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'c', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::MoveFocus(frame::Direction::Right),
      super::Cmd::Quit,
      super::Cmd::ResetLayout);
    let setup = |cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_2(), 0).unwrap();
      cmd_thread.set_mode(mode_3(), 1).unwrap(); };
    let callback = |_: super::Cmd, _: &super::CmdThread| {};
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn change_mode() {
    let inputs = vec!(vec!(
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::Quit,
      super::Cmd::MoveFocus(frame::Direction::Left));
    let setup = |cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_0(), 0).unwrap(); };
    let callback = |_: super::Cmd, cmd_thread: &super::CmdThread| {
      cmd_thread.set_mode(mode_1(), 0).unwrap(); };
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn mode_fallback_command_constructor() {
    let inputs = vec!(
      vec!(
        // ResetLayout, normal match
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
        // Fallback, plain miss
        Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE},
        // mistype dbad, triggering first miss then match on ba, then miss again
        Key::Unicode{codepoint: 'd', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE}),
      vec!(
        // timeout on partial dbad, triggering first miss then match on ba
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
        Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE}));
    let outputs = vec!(
      super::Cmd::ResetLayout,
      super::Cmd::Quit,
      super::Cmd::Quit,
      super::Cmd::ResetLayout,
      super::Cmd::Quit,
      super::Cmd::Quit,
      super::Cmd::ResetLayout);
    let setup = |cmd_thread: &super::CmdThread| {
      let mut mode = mode_0();
      fn fallback(_: Key) -> Option<super::Cmd> { Some(super::Cmd::Quit) }
      mode.fallback = fallback;
      cmd_thread.set_mode(mode, 0).unwrap(); };
    let callback = |_: super::Cmd, _: &super::CmdThread| {};
    run_test(inputs, outputs, setup, callback);
  }

  #[test]
  fn keychain_matching() {
    let mode = mode_4();

    let match_test = |keys: Vec<Key>, regular, forced| {
      assert_eq!(mode.keychain.match_keys(&mut keys.iter(), false), regular);
      assert_eq!(mode.keychain.match_keys(&mut keys.iter(), true), forced); };

    // no matches
    let keys = vec!(Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::None, super::MatchResult::None);
    let keys = vec!(
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::None, super::MatchResult::None);

    // force a no match from a patial match
    let keys = vec!(Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Partial(1), super::MatchResult::None);

    // force a shorter complete match from a prefix of a longer command, the
    // shorter complete match must be the longest available
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Partial(2),
      super::MatchResult::Complete(super::Cmd::Quit, 2));
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Partial(3),
      super::MatchResult::Complete(super::Cmd::ResetLayout, 3));
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Partial(4),
      super::MatchResult::Complete(super::Cmd::ResetLayout, 3));

    // mistype a longer command to trigger a complete match of a shorter command
    // that prefix the longer one, the shorter complete match must be the
    // longest available
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Complete(super::Cmd::Quit, 2),
      super::MatchResult::Complete(super::Cmd::Quit, 2));
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Complete(super::Cmd::ResetLayout, 3),
      super::MatchResult::Complete(super::Cmd::ResetLayout, 3));
    let keys = vec!(
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'b', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'a', mods: keymap::MOD_NONE},
      Key::Unicode{codepoint: 'x', mods: keymap::MOD_NONE});
    match_test(keys, super::MatchResult::Complete(super::Cmd::ResetLayout, 3),
      super::MatchResult::Complete(super::Cmd::ResetLayout, 3));
  }
}
