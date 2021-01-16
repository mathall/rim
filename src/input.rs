/*
 * Copyright (c) 2014-2021 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::thread;

use futures::channel::mpsc;
use futures::channel::oneshot;
use futures::{FutureExt, StreamExt};

use crate::keymap::{Key, KeyMod, KeySym};

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
        self.kill_tx
            .take()
            .expect("TermInput already killed.")
            .send(())
            .expect("Input thread died prematurely.");
        tokio::runtime::Builder::new_current_thread()
            .build()
            .expect("Runtime started.")
            .block_on(self.died_rx.take().expect("TermInput already killed."))
            .expect("Input thread died prematurely.");
    }
}

// start listening for terminal input on stdin
#[cfg(not(test))]
pub fn start(key_tx: mpsc::UnboundedSender<Key>) -> TermInput {
    start_on_fd(STDIN_FILENO, key_tx)
}

// start listening for terminal input on the specified file descriptor
pub fn start_on_fd(fd: libc::c_int, key_tx: mpsc::UnboundedSender<Key>) -> TermInput {
    let (kill_tx, kill_rx) = oneshot::channel();
    let (died_tx, died_rx) = oneshot::channel();
    thread::spawn(move || input_loop(kill_rx, died_tx, key_tx, fd));
    TermInput {
        kill_tx: Some(kill_tx),
        died_rx: Some(died_rx),
    }
}

/*
 * Helper module for detecting available bytes on the file descriptor being
 * listened to.
 */
mod libc_poll {
    use libc::{c_int, c_long, c_short};

    #[repr(C)]
    struct Pollfd {
        fd: c_int,
        events: c_short,
        revents: c_short,
    }

    extern "C" {
        fn poll(fds: *mut Pollfd, nfds: c_long, timeout: c_int) -> c_int;
    }

    #[derive(PartialEq)]
    pub enum PollResult {
        Ready,
        Timeout,
    }

    pub fn poll_fd(fd: c_int, timeout_ms: u16) -> PollResult {
        const POLLIN: c_short = 1;
        let mut pollfd = Pollfd {
            fd,
            events: POLLIN,
            revents: 0,
        };
        let num_fds = 1;
        match (unsafe { poll(&mut pollfd, num_fds, timeout_ms as c_int) }).cmp(&0) {
            std::cmp::Ordering::Less => panic!("Unable to poll stdin."),
            std::cmp::Ordering::Greater => PollResult::Ready,
            std::cmp::Ordering::Equal => PollResult::Timeout,
        }
    }
}

/*
 * Events to the input loop.
 */
#[derive(Clone)]
enum Event {
    Continue,
    Break,
}

fn input_loop(
    kill_rx: oneshot::Receiver<()>,
    died_tx: oneshot::Sender<()>,
    key_tx: mpsc::UnboundedSender<Key>,
    fd: libc::c_int,
) {
    let mut tk = termkey::TermKey::new(fd, termkey::c::Flag::CTRLC);

    let inf = futures::stream::repeat::<Event>(Event::Continue);
    let killer = kill_rx.into_stream().map(|_| Event::Break);

    let mut event_stream = futures::stream::select(inf, killer);

    let mut input_loop = || {
        // check for available input
        let poll_timeout_ms = 10;
        if libc_poll::poll_fd(fd, poll_timeout_ms) == libc_poll::PollResult::Ready {
            // tell termkey that there's input available
            tk.advisereadable();
        }

        // empty the fd, translating and sending key events
        loop {
            if let Some(key) = match tk.getkey() {
                termkey::Result::None_ => break, // got nothing, done here
                termkey::Result::Eof => panic!("stdin closed, you're on your own."),
                termkey::Result::Error { err } => {
                    panic!("termkey::geykey failed with error code {}", err)
                }
                termkey::Result::Key(key) => translate_key(key),
                termkey::Result::Again =>
                // There's input available, but not enough to make a key event. Likely
                // escape has been pushed, which we want to force out and interpret as
                // a key on its own, otherwise oops.
                {
                    match tk.getkey_force() {
                        termkey::Result::Key(key) => translate_key(key),
                        termkey::Result::Eof => panic!("stdin closed, you're on your own."),
                        termkey::Result::Error { err } => {
                            panic!("termkey::geykey_force failed with error code {}", err)
                        }
                        _ => unreachable!(),
                    }
                }
            } {
                key_tx.unbounded_send(key).expect("Key channel died.");
            }
        }
    };

    tokio::runtime::Builder::new_current_thread()
        .build()
        .expect("Runtime started.")
        .block_on(async {
            while let Some(event) = event_stream.next().await {
                match event {
                    Event::Continue => input_loop(),
                    Event::Break => break,
                }
            }
        });

    died_tx.send(()).expect("Main thread died prematurely.");
}

fn translate_key(key: termkey::Event) -> Option<Key> {
    match key {
        termkey::Event::Function { num, mods } => Some(Key::Fn {
            num,
            mods: translate_mods(mods),
        }),
        termkey::Event::KeySym { sym, mods } => Some(Key::Sym {
            sym: translate_sym(sym),
            mods: translate_mods(mods),
        }),
        termkey::Event::Unicode {
            codepoint, mods, ..
        } => Some(Key::Unicode {
            codepoint,
            mods: translate_mods(mods),
        }),
        _ => None,
    }
}

fn translate_sym(sym: termkey::c::Sym) -> KeySym {
    match sym {
        termkey::c::Sym::UNKNOWN => KeySym::Unknown,
        termkey::c::Sym::NONE => KeySym::None,
        termkey::c::Sym::BACKSPACE => KeySym::Backspace,
        termkey::c::Sym::TAB => KeySym::Tab,
        termkey::c::Sym::ENTER => KeySym::Enter,
        termkey::c::Sym::ESCAPE => KeySym::Escape,
        termkey::c::Sym::SPACE => KeySym::Space,
        termkey::c::Sym::DEL => KeySym::Del,
        termkey::c::Sym::UP => KeySym::Up,
        termkey::c::Sym::DOWN => KeySym::Down,
        termkey::c::Sym::LEFT => KeySym::Left,
        termkey::c::Sym::RIGHT => KeySym::Right,
        termkey::c::Sym::BEGIN => KeySym::Begin,
        termkey::c::Sym::FIND => KeySym::Find,
        termkey::c::Sym::INSERT => KeySym::Insert,
        termkey::c::Sym::DELETE => KeySym::Delete,
        termkey::c::Sym::SELECT => KeySym::Select,
        termkey::c::Sym::PAGEUP => KeySym::Pageup,
        termkey::c::Sym::PAGEDOWN => KeySym::Pagedown,
        termkey::c::Sym::HOME => KeySym::Home,
        termkey::c::Sym::END => KeySym::End,
        termkey::c::Sym::CANCEL => KeySym::Cancel,
        termkey::c::Sym::CLEAR => KeySym::Clear,
        termkey::c::Sym::CLOSE => KeySym::Close,
        termkey::c::Sym::COMMAND => KeySym::Command,
        termkey::c::Sym::COPY => KeySym::Copy,
        termkey::c::Sym::EXIT => KeySym::Exit,
        termkey::c::Sym::HELP => KeySym::Help,
        termkey::c::Sym::MARK => KeySym::Mark,
        termkey::c::Sym::MESSAGE => KeySym::Message,
        termkey::c::Sym::MOVE => KeySym::Move,
        termkey::c::Sym::OPEN => KeySym::Open,
        termkey::c::Sym::OPTIONS => KeySym::Options,
        termkey::c::Sym::PRINT => KeySym::Print,
        termkey::c::Sym::REDO => KeySym::Redo,
        termkey::c::Sym::REFERENCE => KeySym::Reference,
        termkey::c::Sym::REFRESH => KeySym::Refresh,
        termkey::c::Sym::REPLACE => KeySym::Replace,
        termkey::c::Sym::RESTART => KeySym::Restart,
        termkey::c::Sym::RESUME => KeySym::Resume,
        termkey::c::Sym::SAVE => KeySym::Save,
        termkey::c::Sym::SUSPEND => KeySym::Suspend,
        termkey::c::Sym::UNDO => KeySym::Undo,
        termkey::c::Sym::KP0 => KeySym::KP0,
        termkey::c::Sym::KP1 => KeySym::KP1,
        termkey::c::Sym::KP2 => KeySym::KP2,
        termkey::c::Sym::KP3 => KeySym::KP3,
        termkey::c::Sym::KP4 => KeySym::KP4,
        termkey::c::Sym::KP5 => KeySym::KP5,
        termkey::c::Sym::KP6 => KeySym::KP6,
        termkey::c::Sym::KP7 => KeySym::KP7,
        termkey::c::Sym::KP8 => KeySym::KP8,
        termkey::c::Sym::KP9 => KeySym::KP9,
        termkey::c::Sym::KPENTER => KeySym::KPEnter,
        termkey::c::Sym::KPPLUS => KeySym::KPPlus,
        termkey::c::Sym::KPMINUS => KeySym::KPMinus,
        termkey::c::Sym::KPMULT => KeySym::KPMult,
        termkey::c::Sym::KPDIV => KeySym::KPDiv,
        termkey::c::Sym::KPCOMMA => KeySym::KPComma,
        termkey::c::Sym::KPPERIOD => KeySym::KPPeriod,
        termkey::c::Sym::KPEQUALS => KeySym::KPEquals,
        termkey::c::Sym::N_SYMS => KeySym::NSyms,
    }
}

fn translate_mods(mods: termkey::c::KeyMod) -> KeyMod {
    let mut ret = KeyMod::MOD_NONE;
    if mods.contains(termkey::c::KeyMod::SHIFT) {
        ret.insert(KeyMod::MOD_SHIFT);
    }
    if mods.contains(termkey::c::KeyMod::ALT) {
        ret.insert(KeyMod::MOD_ALT);
    }
    if mods.contains(termkey::c::KeyMod::CTRL) {
        ret.insert(KeyMod::MOD_CTRL);
    }
    ret
}

#[cfg(test)]
mod test {
    use std::mem;
    use std::thread;
    use std::time::Duration;

    use futures::channel::{mpsc, oneshot};
    use futures::StreamExt;

    use crate::keymap::{Key, KeySym};

    use super::*;

    // Simulates stdin by writing bytes to a pipe, then listens for the key
    // outputs and matches with expectations.
    #[test]
    fn test_input() {
        // pairs of input bytes on "stdin" and corresponding expected key output
        let input_output_pairs = vec![
            (
                vec![0x61],
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ),
            (
                vec![0x1B, 0x61],
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_ALT,
                },
            ),
            (
                vec![0x1B],
                Key::Sym {
                    sym: KeySym::Escape,
                    mods: KeyMod::MOD_NONE,
                },
            ),
            (
                vec![0x61],
                Key::Unicode {
                    codepoint: 'a',
                    mods: KeyMod::MOD_NONE,
                },
            ),
            (
                vec![0x03],
                Key::Unicode {
                    codepoint: 'c',
                    mods: KeyMod::MOD_CTRL,
                },
            ),
            (
                vec![0x1B, 0x5B, 0x41],
                Key::Sym {
                    sym: KeySym::Up,
                    mods: KeyMod::MOD_NONE,
                },
            ),
            (
                vec![0x1B, 0x4F, 0x53],
                Key::Fn {
                    num: 4,
                    mods: KeyMod::MOD_NONE,
                },
            ),
            (
                vec![0xE3, 0x81, 0x82],
                Key::Unicode {
                    codepoint: 'あ',
                    mods: KeyMod::MOD_NONE,
                },
            ),
        ];

        let inputs: Vec<Vec<u8>> = input_output_pairs
            .iter()
            .map(|&(ref input, _)| input.clone())
            .collect();
        let outputs: Vec<Key> = input_output_pairs
            .iter()
            .map(|&(_, output)| output)
            .collect();

        // set up communication channels
        let (key_tx, key_rx) = mpsc::unbounded();
        let (reader_fd, writer_fd) = unsafe {
            let mut fds = [0; 2];
            if libc::pipe(fds.as_mut_ptr()) != 0 {
                panic!("Failed to create pipe");
            }
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
                unsafe {
                    libc::close(self.reader_fd);
                }
                self.close_writer_tx.take().unwrap().send(()).unwrap();
            }
        }
        let _pipe_killer = PipeKiller {
            close_writer_tx: Some(close_writer_tx),
            reader_fd,
        };

        // start input listener
        let _term_input = start_on_fd(reader_fd, key_tx);

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
            tokio::runtime::Builder::new_current_thread()
                .build()
                .expect("Runtime started.")
                .block_on(close_writer_rx)
                .unwrap();
            unsafe {
                libc::close(writer_fd);
            }
        });

        // match up received keys with the expected output
        let expected_output = futures::stream::iter(outputs.iter());
        let check = key_rx.zip(expected_output).for_each(|(key, output)| {
            assert_eq!(key, *output);
            futures::future::ready(())
        });

        tokio::runtime::Builder::new_current_thread()
            .enable_time()
            .build()
            .expect("Runtime started.")
            .block_on(async { tokio::time::timeout(Duration::from_millis(10), check).await })
            .expect("Timeout waiting for key event.");
    }
}
