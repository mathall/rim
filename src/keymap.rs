/*
 * Copyright (c) 2014 Mathias Hällman
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
#[deriving(Clone, PartialEq, Show)]
pub enum Key {
  Fn{pub num: int, pub mods: KeyMod},
  Sym{pub sym: KeySym, pub mods: KeyMod},
  Unicode{pub codepoint: char, pub mods: KeyMod},
}

#[deriving(Clone, PartialEq, Show)]
pub enum KeySym
{
  SymUnknown = -1,
  SymNone = 0,

  /* Special names in C0 */
  SymBackspace,
  SymTab,
  SymEnter,
  SymEscape,

  /* Special names in G0 */
  SymSpace,
  SymDel,

  /* Special keys */
  SymUp,
  SymDown,
  SymLeft,
  SymRight,
  SymBegin,
  SymFind,
  SymInsert,
  SymDelete,
  SymSelect,
  SymPageup,
  SymPagedown,
  SymHome,
  SymEnd,

  /* Special keys from terminfo */
  SymCancel,
  SymClear,
  SymClose,
  SymCommand,
  SymCopy,
  SymExit,
  SymHelp,
  SymMark,
  SymMessage,
  SymMove,
  SymOpen,
  SymOptions,
  SymPrint,
  SymRedo,
  SymReference,
  SymRefresh,
  SymReplace,
  SymRestart,
  SymResume,
  SymSave,
  SymSuspend,
  SymUndo,

  /* Numeric keypad special keys */
  SymKP0,
  SymKP1,
  SymKP2,
  SymKP3,
  SymKP4,
  SymKP5,
  SymKP6,
  SymKP7,
  SymKP8,
  SymKP9,
  SymKPEnter,
  SymKPPlus,
  SymKPMinus,
  SymKPMult,
  SymKPDiv,
  SymKPComma,
  SymKPPeriod,
  SymKPEquals,

  /* et cetera ad nauseum */
  SymNSyms,
}

bitflags! {
  #[deriving(Show)]
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
