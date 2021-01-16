/*
 * Copyright (c) 2015-2021 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::collections::{HashMap, VecDeque};
use std::mem;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

use futures::sync::{mpsc, oneshot};
use futures::{Future, Stream};
use vec_map::VecMap;

use crate::caret;
use crate::frame;
use crate::keymap::Key;

#[cfg(not(test))]
const TIMEOUT: u64 = 3000;
#[cfg(test)]
const TIMEOUT: u64 = 100;

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
 */
pub struct CmdThread {
    kill_tx: Option<oneshot::Sender<()>>,
    died_rx: Option<oneshot::Receiver<()>>,
    msg_tx: mpsc::UnboundedSender<Msg>,
}

impl CmdThread {
    pub fn set_mode(&self, mode: Mode, num: usize) {
        self.send(Msg::SetMode(mode, num));
    }

    pub fn ack_cmd(&self) {
        self.send(Msg::AckCmd);
    }

    fn send(&self, msg: Msg) {
        self.msg_tx
            .unbounded_send(msg)
            .expect("command thread died");
    }
}

impl Drop for CmdThread {
    fn drop(&mut self) {
        self.kill_tx
            .take()
            .expect("CmdThread already killed.")
            .send(())
            .expect("Command thread died prematurely.");
        self.died_rx
            .take()
            .expect("CmdThread already killed.")
            .wait()
            .expect("Command thread died prematurely.");
    }
}

/*
 * Messages used by CmdThread to instruct the command thread.
 */
enum Msg {
    SetMode(Mode, usize),
    AckCmd,
}

// start the command thread
pub fn start(
    key_rx: mpsc::UnboundedReceiver<Key>,
    cmd_tx: mpsc::UnboundedSender<Cmd>,
) -> CmdThread {
    let (kill_tx, kill_rx) = oneshot::channel();
    let (died_tx, died_rx) = oneshot::channel();
    let (msg_tx, msg_rx) = mpsc::unbounded();
    thread::spawn(move || cmd_thread(kill_rx, died_tx, msg_rx, key_rx, cmd_tx));
    CmdThread {
        kill_tx: Some(kill_tx),
        died_rx: Some(died_rx),
        msg_tx,
    }
}

/*
 * Events to the command loop.
 */
enum Event {
    Key(Key),
    CmdMsg(Msg),
    Timeout(usize),
    Kill,
}

fn cmd_thread(
    kill_rx: oneshot::Receiver<()>,
    died_tx: oneshot::Sender<()>,
    msg_rx: mpsc::UnboundedReceiver<Msg>,
    key_rx: mpsc::UnboundedReceiver<Key>,
    cmd_tx: mpsc::UnboundedSender<Cmd>,
) {
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

    // setup the command loop
    let mut core = tokio_core::reactor::Core::new().unwrap();

    // setup a channel through which timeouts may be requested
    let timeout_core_handle = core.handle();
    let (timeout_tx, timeout_rx) = mpsc::unbounded();
    let request_timeout = |seq| {
        let tx = timeout_tx.clone();
        let timeout = tokio_timer::wheel()
            .tick_duration(Duration::from_millis(10))
            .build()
            .sleep(Duration::from_millis(TIMEOUT))
            .then(move |_| {
                tx.unbounded_send(seq).expect("Oneshot channel died.");
                Ok(())
            });
        timeout_core_handle.spawn(timeout);
    };

    let killer = kill_rx.into_stream().map(|_| Event::Kill).map_err(|_| ());
    let timeout = timeout_rx.map(Event::Timeout);
    let msg_stream = msg_rx.map(Event::CmdMsg);
    let key_stream = key_rx.map(Event::Key);

    let cmd_loop = key_stream
        .select(msg_stream)
        .select(timeout)
        .select(killer)
        .for_each(|event| {
            let mut drain = false;

            match event {
                Event::Key(key) => {
                    keys.push_back(key);
                    back_seq += 1;
                }
                Event::CmdMsg(msg) => match msg {
                    Msg::SetMode(mode, num) => {
                        modes.insert(num, mode);
                    }
                    Msg::AckCmd => {
                        cmd_acknowledged = true;
                    }
                },
                Event::Timeout(seq) => drain = seq == back_seq,
                Event::Kill => return Err(()),
            }

            // work through the arrived keys
            while !keys.is_empty() {
                assert!(back_seq >= front_seq);
                // determine amount of keys to consider
                let num_keys = back_seq - front_seq;
                // match keys with modes in priority order
                let mut match_result = MatchResult::None;
                for (_, mode) in modes.iter().rev() {
                    // first match by keychain
                    match_result = mode
                        .keychain
                        .match_keys(&mut keys.iter().take(num_keys), drain);
                    // use the mode's fallback if the keychain didn't match anything
                    if match_result == MatchResult::None {
                        if let Some(cmd) = (mode.fallback)(keys[0]) {
                            match_result = MatchResult::Complete(cmd, 1);
                        }
                    }
                    // proceed to next mode only if no match was made
                    if match_result != MatchResult::None {
                        break;
                    }
                }

                // act on the match result
                match match_result {
                    MatchResult::None => {
                        keys.pop_front();
                        front_seq += 1;
                    }
                    MatchResult::Partial(num) => {
                        assert_eq!(front_seq + num, back_seq);
                        request_timeout(back_seq);
                        break;
                    }
                    MatchResult::Complete(cmd, num) => {
                        if !cmd_acknowledged {
                            break;
                        } else {
                            for _ in 0..num {
                                keys.pop_front();
                                front_seq += 1;
                            }
                            cmd_tx.unbounded_send(cmd).expect("Command channel died.");
                            cmd_acknowledged = false;
                        }
                    }
                }
            }

            Ok(())
        });

    core.run(cmd_loop).ok();

    died_tx.send(()).expect("Main thread died prematurely.");
}

/*
 * A Mode is what the command thread use to form commands out of keys. It
 * consist of a keychain and a fallback command contructor for when the keychain
 * doesn't match on a string of keys.
 */
#[derive(Clone)]
pub struct Mode {
    pub keychain: Keychain,
    pub fallback: fn(Key) -> Option<Cmd>,
}

impl Mode {
    pub fn new() -> Mode {
        fn fallback(_: Key) -> Option<Cmd> {
            None
        }
        Mode {
            keychain: Keychain::new(),
            fallback,
        }
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
#[derive(Clone)]
pub enum Keychain {
    Node(HashMap<Key, Keychain>, Option<Cmd>),
    Cmd(Cmd),
}

impl Keychain {
    pub fn new() -> Keychain {
        Keychain::Node(HashMap::new(), None)
    }

    pub fn bind(&mut self, keys: &[Key], new_cmd: Cmd) {
        let new_self = match mem::replace(self, Keychain::new()) {
            Keychain::Node(mut map, opt_cmd) => match keys.split_first() {
                None => {
                    if !map.is_empty() {
                        Keychain::Node(map, Some(new_cmd))
                    } else {
                        Keychain::Cmd(new_cmd)
                    }
                }
                Some((key, ref keys)) => {
                    let mut subchain = map.remove(&key).unwrap_or_else(Keychain::new);
                    subchain.bind(keys, new_cmd);
                    map.insert(*key, subchain);
                    Keychain::Node(map, opt_cmd)
                }
            },
            Keychain::Cmd(old_cmd) => {
                if keys.is_empty() {
                    Keychain::Cmd(new_cmd)
                } else {
                    let mut chain = Keychain::Node(HashMap::new(), Some(old_cmd));
                    chain.bind(keys, new_cmd);
                    chain
                }
            }
        };
        *self = new_self;
    }

    fn match_keys<'l, It>(&self, keys: &mut It, force: bool) -> MatchResult
    where
        It: Iterator<Item = &'l Key>,
    {
        let res_from_opt = |opt: Option<Cmd>| {
            opt.map(|cmd| MatchResult::Complete(cmd, 0))
                .unwrap_or(MatchResult::None)
        };
        match self {
            Keychain::Cmd(ref cmd) => MatchResult::Complete(cmd.clone(), 0),
            Keychain::Node(ref map, ref opt_cmd) => keys
                .next()
                .map(|key| {
                    map.get(key)
                        .map(|chain| match chain.match_keys(keys, force) {
                            MatchResult::None => res_from_opt(opt_cmd.clone()),
                            MatchResult::Partial(num) => {
                                if force {
                                    res_from_opt(opt_cmd.clone())
                                } else {
                                    MatchResult::Partial(num + 1)
                                }
                            }
                            MatchResult::Complete(cmd, num) => MatchResult::Complete(cmd, num + 1),
                        })
                        .unwrap_or_else(|| res_from_opt(opt_cmd.clone()))
                })
                .unwrap_or_else(|| {
                    match if force {
                        res_from_opt(opt_cmd.clone())
                    } else {
                        MatchResult::None
                    } {
                        MatchResult::None if !map.is_empty() => MatchResult::Partial(0),
                        otherwise => otherwise,
                    }
                }),
        }
    }
}

/*
 * Commands for rim.
 */
#[derive(Clone, PartialEq)]
#[cfg_attr(test, derive(Debug))]
#[cfg_attr(test, allow(dead_code))] // the tests don't make use of all commands
pub enum Cmd {
    MoveFocus(frame::Direction),
    ShiftFocus(frame::WindowOrder),
    ResetLayout,
    SplitWindow(frame::Orientation),
    GrowWindow(frame::Orientation),
    ShrinkWindow(frame::Orientation),
    CloseWindow,
    QuitWindow,
    Quit,
    WinCmd(WinCmd),
}

/*
 * Commands intended for the focused window.
 */
#[derive(Clone, PartialEq)]
#[cfg_attr(test, derive(Debug))]
#[cfg_attr(test, allow(dead_code))] // the tests don't make use of all commands
pub enum WinCmd {
    MoveCaret(caret::Adjustment),
    EnterNormalMode,
    EnterReplaceMode(bool),
    EnterInsertMode,
    EnterInsertModeStartOfLine,
    EnterInsertModeAppend,
    EnterInsertModeAppendEndOfLine,
    EnterInsertModeNextLine,
    EnterInsertModePreviousLine,
    OpenBuffer(PathBuf),
    SaveBuffer,
    Replace(String),
    ReplaceLine(String),
    Insert(String),
    Delete,
    Backspace,
    DeleteOnLine,
    BackspaceOnLine,
    DeleteLine,
    DeleteRestOfLine,
    ChangeRestOfLine,
    PageUp,
    PageDown,
    HalfPageUp,
    HalfPageDown,
}

#[cfg(test)]
mod test {
    use std::thread;
    use std::time::Duration;

    use futures::sync::mpsc;
    use futures::{Future, Stream};

    use crate::keymap::{Key, KeyMod};

    use super::*;

    // Sends keys on one thread and receives commands and compares with
    // expectations on another. Each vector of keys will be sent in one go, with a
    // sleep inbetween to trigger a timeout.
    fn run_test<S, C>(inputs: Vec<Vec<Key>>, outputs: Vec<Cmd>, setup: S, callback: C)
    where
        S: Fn(&CmdThread),
        C: Fn(Cmd, &CmdThread),
    {
        // set up communication channels
        let (key_tx, key_rx) = mpsc::unbounded();
        let (cmd_tx, cmd_rx) = mpsc::unbounded();

        // start and setup command thread
        let cmd_thread = start(key_rx, cmd_tx);
        setup(&cmd_thread);

        // unfortunately tokio seems to miss keys if we start blastimg them off
        // right after setting up the command thread
        thread::sleep(Duration::from_millis(1));

        // send off key events on separate thread
        thread::spawn(move || {
            for keys in inputs.iter() {
                for key in keys.iter() {
                    key_tx.unbounded_send(*key).unwrap();
                }
                thread::sleep(Duration::from_millis(TIMEOUT + 10));
            }
        });

        let timeout = tokio_timer::wheel()
            .tick_duration(Duration::from_millis(10))
            .build()
            .sleep(Duration::from_millis(1000))
            .then(|_| Err(()));

        // match up received commands with the expected output
        let expected_output = futures::stream::iter_result(outputs.iter().map(Ok));
        let check = cmd_rx.zip(expected_output).for_each(|(cmd, output)| {
            assert_eq!(cmd, *output);
            callback(cmd, &cmd_thread);
            cmd_thread.ack_cmd();
            Ok(())
        });

        check
            .select(timeout)
            .wait()
            .ok()
            .expect("Timeout waiting for command.")
            .1
            .wait()
            .ok();
    }

    fn mode_0() -> Mode {
        let mut mode = Mode::new();
        mode.keychain.bind(
            &[Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            }],
            Cmd::Quit,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::ResetLayout,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'c',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::CloseWindow,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'd',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'd',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::CloseWindow,
        );
        mode
    }

    fn mode_1() -> Mode {
        let mut mode = Mode::new();
        mode.keychain.bind(
            &[Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            }],
            Cmd::MoveFocus(frame::Direction::Left),
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::MoveFocus(frame::Direction::Right),
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'c',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::MoveFocus(frame::Direction::Up),
        );
        mode
    }

    fn mode_2() -> Mode {
        let mut mode = Mode::new();
        mode.keychain.bind(
            &[Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            }],
            Cmd::Quit,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::ResetLayout,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::CloseWindow,
        );
        mode
    }

    fn mode_3() -> Mode {
        let mut mode = Mode::new();
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::MoveFocus(frame::Direction::Right),
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'c',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::MoveFocus(frame::Direction::Up),
        );
        mode
    }

    fn mode_4() -> Mode {
        let mut mode = Mode::new();
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::Quit,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::ResetLayout,
        );
        mode.keychain.bind(
            &[
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            Cmd::CloseWindow,
        );
        mode
    }

    #[test]
    fn single_mode() {
        let inputs = vec![vec![
            // Quit
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            // Nothing
            Key::Unicode {
                codepoint: 'x',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'y',
                mods: KeyMod::MOD_NONE,
            },
            // ResetLayout
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            // Nothing
            Key::Unicode {
                codepoint: 'z',
                mods: KeyMod::MOD_NONE,
            },
            // CloseWindow
            Key::Unicode {
                codepoint: 'c',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            // Nothing (partial CloseWindow)
            Key::Unicode {
                codepoint: 'c',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            // ResetLayout
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ]];
        let outputs = vec![
            Cmd::Quit,
            Cmd::ResetLayout,
            Cmd::CloseWindow,
            Cmd::ResetLayout,
        ];
        let setup = |cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_0(), 0);
        };
        let callback = |_: Cmd, _: &CmdThread| {};
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn multiple_modes() {
        let inputs = vec![vec![
            // MoveFocus(Left)
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            // Nothing
            Key::Unicode {
                codepoint: 'x',
                mods: KeyMod::MOD_NONE,
            },
            // MoveFocus(Left)
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            // mistype cbabaa, get CloseWindow from mode0 (cba), MoveFocus(Right)
            // overriding from mode1 (ba), leaving c in the buffer
            Key::Unicode {
                codepoint: 'c',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'c',
                mods: KeyMod::MOD_NONE,
            },
            // MoveFocus(Up) by properly typing cbabaa (c left from above)
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ]];
        let outputs = vec![
            Cmd::MoveFocus(frame::Direction::Left),
            Cmd::MoveFocus(frame::Direction::Left),
            Cmd::CloseWindow,
            Cmd::MoveFocus(frame::Direction::Right),
            Cmd::MoveFocus(frame::Direction::Up),
        ];
        let setup = |cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_0(), 0);
            cmd_thread.set_mode(mode_1(), 1);
        };
        let callback = |_: Cmd, _: &CmdThread| {};
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn single_mode_timeout() {
        let inputs = vec![
            vec![
                // CloseWindow
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                // Timeout: ResetLayout
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            vec![
                // Timeout: Nothing
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            vec![
                // Quit
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
        ];
        let outputs = vec![Cmd::CloseWindow, Cmd::ResetLayout, Cmd::Quit];
        let setup = |cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_2(), 0);
        };
        let callback = |_: Cmd, _: &CmdThread| {};
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn multiple_mode_timeout() {
        let inputs = vec![vec![
            // Timeout: MoveFocus(Right) (bab override on mode1), throw c away,
            // Quit (mode0), ResetLayout (mode0)
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'c',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ]];
        let outputs = vec![
            Cmd::MoveFocus(frame::Direction::Right),
            Cmd::Quit,
            Cmd::ResetLayout,
        ];
        let setup = |cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_2(), 0);
            cmd_thread.set_mode(mode_3(), 1);
        };
        let callback = |_: Cmd, _: &CmdThread| {};
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn change_mode() {
        let inputs = vec![vec![
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ]];
        let outputs = vec![Cmd::Quit, Cmd::MoveFocus(frame::Direction::Left)];
        let setup = |cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_0(), 0);
        };
        let callback = |_: Cmd, cmd_thread: &CmdThread| {
            cmd_thread.set_mode(mode_1(), 0);
        };
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn mode_fallback_command_constructor() {
        let inputs = vec![
            vec![
                // ResetLayout, normal match
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                // Fallback, plain miss
                Key::Unicode {
                    codepoint: 'x',
                    mods: KeyMod::MOD_NONE,
                },
                // mistype dbad, triggering first miss then match on ba, then miss again
                Key::Unicode {
                    codepoint: 'd',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'x',
                    mods: KeyMod::MOD_NONE,
                },
            ],
            vec![
                // timeout on partial dbad, triggering first miss then match on ba
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'b',
                    mods: KeyMod::MOD_NONE,
                },
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ],
        ];
        let outputs = vec![
            Cmd::ResetLayout,
            Cmd::Quit,
            Cmd::Quit,
            Cmd::ResetLayout,
            Cmd::Quit,
            Cmd::Quit,
            Cmd::ResetLayout,
        ];
        let setup = |cmd_thread: &CmdThread| {
            let mut mode = mode_0();
            fn fallback(_: Key) -> Option<Cmd> {
                Some(Cmd::Quit)
            }
            mode.fallback = fallback;
            cmd_thread.set_mode(mode, 0);
        };
        let callback = |_: Cmd, _: &CmdThread| {};
        run_test(inputs, outputs, setup, callback);
    }

    #[test]
    fn keychain_matching() {
        let mode = mode_4();

        let match_test = |keys: Vec<Key>, regular, forced| {
            assert_eq!(mode.keychain.match_keys(&mut keys.iter(), false), regular);
            assert_eq!(mode.keychain.match_keys(&mut keys.iter(), true), forced);
        };

        // no matches
        let keys = vec![Key::Unicode {
            codepoint: 'a',
            mods: KeyMod::MOD_NONE,
        }];
        match_test(keys, MatchResult::None, MatchResult::None);
        let keys = vec![
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(keys, MatchResult::None, MatchResult::None);

        // force a no match from a patial match
        let keys = vec![Key::Unicode {
            codepoint: 'b',
            mods: KeyMod::MOD_NONE,
        }];
        match_test(keys, MatchResult::Partial(1), MatchResult::None);

        // force a shorter complete match from a prefix of a longer command, the
        // shorter complete match must be the longest available
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Partial(2),
            MatchResult::Complete(Cmd::Quit, 2),
        );
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Partial(3),
            MatchResult::Complete(Cmd::ResetLayout, 3),
        );
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Partial(4),
            MatchResult::Complete(Cmd::ResetLayout, 3),
        );

        // mistype a longer command to trigger a complete match of a shorter command
        // that prefix the longer one, the shorter complete match must be the
        // longest available
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'x',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Complete(Cmd::Quit, 2),
            MatchResult::Complete(Cmd::Quit, 2),
        );
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'x',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Complete(Cmd::ResetLayout, 3),
            MatchResult::Complete(Cmd::ResetLayout, 3),
        );
        let keys = vec![
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'b',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'a',
                mods: KeyMod::MOD_NONE,
            },
            Key::Unicode {
                codepoint: 'x',
                mods: KeyMod::MOD_NONE,
            },
        ];
        match_test(
            keys,
            MatchResult::Complete(Cmd::ResetLayout, 3),
            MatchResult::Complete(Cmd::ResetLayout, 3),
        );
    }
}
