/*
 * Copyright (c) 2014 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![feature(macro_rules)]
#![feature(slicing_syntax)]
#![feature(struct_variant)]

mod buffer;
mod input;
mod keymap;

#[cfg(not(test))]
fn main() {
  println!("Not much here..");
}
