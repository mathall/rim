/*
 * Copyright (c) 2014-2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * A key can be of any of the following types:
 *   - Function key such as F1 or F24
 *   - Symbolic key such as enter or escape
 *   - Unicode codepoint such as a, ä, or あ
 * The key also records key modifier information.
 */
#[derive(Copy, Eq, Hash, PartialEq)]
#[cfg_attr(test, derive(Debug))]
pub enum Key {
  Fn{num: isize, mods: KeyMod},
  Sym{sym: KeySym, mods: KeyMod},
  Unicode{codepoint: char, mods: KeyMod},
}

#[derive(Copy, Eq, Hash, PartialEq)]
#[cfg_attr(test, derive(Debug))]
pub enum KeySym
{
  Unknown = -1,
  None = 0,

  /* Special names in C0 */
  Backspace,
  Tab,
  Enter,
  Escape,

  /* Special names in G0 */
  Space,
  Del,

  /* Special keys */
  Up,
  Down,
  Left,
  Right,
  Begin,
  Find,
  Insert,
  Delete,
  Select,
  Pageup,
  Pagedown,
  Home,
  End,

  /* Special keys from terminfo */
  Cancel,
  Clear,
  Close,
  Command,
  Copy,
  Exit,
  Help,
  Mark,
  Message,
  Move,
  Open,
  Options,
  Print,
  Redo,
  Reference,
  Refresh,
  Replace,
  Restart,
  Resume,
  Save,
  Suspend,
  Undo,

  /* Numeric keypad special keys */
  KP0,
  KP1,
  KP2,
  KP3,
  KP4,
  KP5,
  KP6,
  KP7,
  KP8,
  KP9,
  KPEnter,
  KPPlus,
  KPMinus,
  KPMult,
  KPDiv,
  KPComma,
  KPPeriod,
  KPEquals,

  /* et cetera ad nauseum */
  NSyms,
}

bitflags! {
  #[cfg_attr(test, derive(Debug))]
  flags KeyMod: u8 {
    const MOD_NONE  = 0,
    const MOD_SHIFT = 1 << 0,
    const MOD_ALT   = 1 << 1,
    const MOD_CTRL  = 1 << 2,
  }
}

#[allow(dead_code)]  // as the name suggests, this function should not be called
fn totally_useless_function_just_to_suppress_warnings_about_dead_code() {
  let mut a = MOD_NONE;
  a.toggle(MOD_NONE); a.remove(MOD_NONE); a.intersects(MOD_NONE);
  a.is_all(); KeyMod::from_bits_truncate(0); KeyMod::from_bits(0);
  a.insert(MOD_NONE); a.contains(MOD_NONE);
}
