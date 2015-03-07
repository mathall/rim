/*
 * Copyright (c) 2015 Mathias Hällman
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate uuid;

use std::cmp;
use std::collections::{HashMap, vec_deque, VecDeque};
use std::error;
use std::fmt;
use std::mem;

use screen;

use self::Orientation::*;
use self::SectionSide::*;
use self::WindowOrder::*;

const MIN_SECTION_SIZE: u16 = 1;
const BORDER_SIZE: u16 = 1;

/*
 * A SectionPath is a sequence of decisions used to navigate the binary tree
 * representing sections of a frame. As such, Frame uses SectionPath as an
 * internal id for its sections.
 */
#[derive(Clone, Hash, PartialEq)]
#[cfg_attr(test, derive(Debug, Eq))]
struct SectionPath(VecDeque<SectionSide>);

impl SectionPath {
  fn new() -> SectionPath {
    SectionPath(VecDeque::new())
  }

  fn len(&self) -> uint {
    let &SectionPath(ref internals) = self;
    internals.len()
  }

  fn prepend(self, side: SectionSide) -> SectionPath {
    let SectionPath(mut internals) = self;
    internals.push_front(side);
    SectionPath(internals)
  }

  fn append(self, side: SectionSide) -> SectionPath {
    let SectionPath(mut internals) = self;
    internals.push_back(side);
    SectionPath(internals)
  }

  fn remove(&mut self, idx: uint) {
    let &mut SectionPath(ref mut internals) = self;
    internals.remove(idx);
  }

  fn pop(&mut self) -> Option<SectionSide> {
    let &mut SectionPath(ref mut internals) = self;
    internals.pop_back()
  }

  fn first(&self) -> Option<SectionSide> {
    let &SectionPath(ref internals) = self;
    internals.front().map(|&side| side)
  }

  fn common_base(&self, other: &SectionPath) -> SectionPath {
    let &SectionPath(ref self_internals) = self;
    let &SectionPath(ref other_internals) = other;
    SectionPath(self_internals.iter().zip(other_internals.iter()).
        take_while(|&(self_side, other_side)| self_side == other_side).
        map(|(&x, _)| x).collect())
  }

  fn does_prefix(&self, other: &SectionPath) -> bool {
    return self.common_base(other) == *self;
  }

  fn iter(&self) -> vec_deque::Iter<SectionSide> {
    let &SectionPath(ref internals) = self;
    internals.iter()
  }
}

/*
 * Represents the sides of a section split.
 */
#[derive(Clone, Copy, Hash, PartialEq)]
#[cfg_attr(test, derive(Debug, Eq))]
enum SectionSide {
  Fst, // Top / Left
  Snd,  // Bottom / Right
}

/*
 * May represent the orientation of a split or an operation to carry out on the
 * section tree.
 */
#[derive(Copy, PartialEq)]
pub enum Orientation {
  Vertical,
  Horizontal,
}

impl Orientation {
  fn opposite(&self) -> Orientation {
    match *self {
      Vertical   => Horizontal,
      Horizontal => Vertical,
    }
  }
}

struct SectionSplit {
  fst: Box<Section>,
  snd: Box<Section>,
  orientation: Orientation,
}

impl SectionSplit {
  fn new(orientation: Orientation) -> SectionSplit {
    SectionSplit {
      fst: Box::new(Section::new()),
      snd: Box::new(Section::new()),
      orientation: orientation,
    }
  }
}

/*
 * A section is a rectangular portion of the frame. A section may be split into
 * two subsections either horizontally or vertically. This way the sections form
 * a binary tree describing how the frame is split. It is the leafs of this tree
 * that represent the windows of the frame.
 * A sections screen space position is determined by its position in the tree
 * and the sizes of the sections that come before it.
 */
struct Section {
  size: screen::Size,
  split: Option<SectionSplit>,
}

impl Section {
  fn new() -> Section {
    Section {
      size: screen::Size(MIN_SECTION_SIZE, MIN_SECTION_SIZE),
      split: None,
    }
  }

  // Split the section defined by |path| and return the ids for the two new
  // sections.
  // The path must refer to a leaf section with enough size to fit the split.
  fn split<'l, It>(&mut self, path: &mut It, orientation: Orientation)
      -> (SectionPath, SectionPath)
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None                => panic!("split: Bad path"),
        Some(ref mut split) => {
          let (fst_path, snd_path) =
            (if go == Fst { &mut split.fst } else { &mut split.snd }).
              split(path, orientation);
          (fst_path.prepend(go), snd_path.prepend(go))
        }
      }).
    unwrap_or_else(|| {
      // found our section, assert that there's no split already then make one
      assert!(self.split.is_none());
      let mut split = SectionSplit::new(orientation);
      let screen::Size(rows, cols) = self.size;
      let self_size = if orientation == Vertical { cols } else { rows };
      assert!(self_size >= MIN_SECTION_SIZE * 2 + BORDER_SIZE);
      let distributable_size = self_size - BORDER_SIZE;
      let fst_size = distributable_size / 2;
      let snd_size = distributable_size - fst_size;
      split.fst.size =
        if orientation == Vertical { screen::Size(rows, fst_size) }
        else                       { screen::Size(fst_size, cols) };
      split.snd.size =
        if orientation == Vertical { screen::Size(rows, snd_size) }
        else                       { screen::Size(snd_size, cols) };
      self.split = Some(split);
      (SectionPath::new().prepend(Fst), SectionPath::new().prepend(Snd)) })
  }

  // Redistributes available size along one orientation in a top down fashion.
  fn reset_sizes_along(&mut self, orientation: Orientation) {
    let screen::Size(rows, cols) = self.size;
    self.split.as_mut().map(|ref mut split| {
      let self_size = if orientation == Vertical { rows } else { cols };

      // set size of immediate subsections
      if split.orientation == orientation {
        split.fst.set_size_along(orientation, self_size);
        split.snd.set_size_along(orientation, self_size);
      }
      else {
        let fst_splits = split.fst.num_splits_along(orientation);
        let snd_splits = split.snd.num_splits_along(orientation);
        let tot_splits = fst_splits + snd_splits + 1;
        assert!(self_size >= tot_splits * BORDER_SIZE);
        let distributable_size = self_size - tot_splits * BORDER_SIZE;
        let fst_sections = fst_splits + 1;
        let snd_sections = snd_splits + 1;
        let tot_sections = fst_sections + snd_sections;
        assert!(distributable_size >= tot_sections * MIN_SECTION_SIZE);
        let fst_portion = (distributable_size * fst_sections) / tot_sections;
        let fst_size = fst_splits * BORDER_SIZE + fst_portion;
        let snd_size =
          snd_splits * BORDER_SIZE + distributable_size - fst_portion;
        assert!(fst_size >=
          fst_sections * MIN_SECTION_SIZE + fst_splits * BORDER_SIZE);
        assert!(snd_size >=
          snd_sections * MIN_SECTION_SIZE + snd_splits * BORDER_SIZE);
        split.fst.set_size_along(orientation, fst_size);
        split.snd.set_size_along(orientation, snd_size);
      }

      // reset recursively
      split.fst.reset_sizes_along(orientation);
      split.snd.reset_sizes_along(orientation);
    });
  }

  fn num_splits_along_at<'l, It>(&self, orientation: Orientation, path: &mut It)
      -> u16
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None            => panic!("num_splits_along_at: Bad path"),
        Some(ref split) =>
          (if go == Fst { &split.fst } else { &split.snd }).
            num_splits_along_at(orientation, path),
      }).
    unwrap_or_else(|| self.num_splits_along(orientation))
  }

  fn num_splits_along(&self, orientation: Orientation) -> u16 {
    self.split.as_ref().map_or(0, |ref split| {
      let fst_splits = split.fst.num_splits_along(orientation);
      let snd_splits = split.snd.num_splits_along(orientation);
      if split.orientation == orientation { cmp::max(fst_splits, snd_splits) }
      else                                { fst_splits + snd_splits + 1 }
    })
  }

  fn cut_branch<'l, It>(&mut self, path: &mut It, side: SectionSide)
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None                => panic!("cut_branch: Bad path"),
        Some(ref mut split) =>
          (if go == Fst { &mut split.fst } else { &mut split.snd }).
            cut_branch(path, side),
      }).
    unwrap_or_else(|| {
      // branch found, shift the edge of the other branch to take up the size of
      // the branch being cut, then cut it by replacing the current split with
      // that of the other branch
      self.split = mem::replace(&mut self.split, None).and_then(|mut split| {
        let (either, other) =
          if side == Fst { (&mut split.fst, &mut split.snd) }
          else           { (&mut split.snd, &mut split.fst) };
        let screen::Size(rows, cols) = either.size;
        let size = if split.orientation == Vertical { cols } else { rows };
        let shift = if side == Fst { -1 } else { 1 } *
          (BORDER_SIZE as int + size as int);
        other.shift_edge(split.orientation.opposite(), side, shift);
        assert_eq!(self.size, other.size);
        mem::replace(&mut other.split, None) }) });
  }

  // Gets the aligning base of the section identified by |path|.
  // A section's aligning base is its topmost ancestor section which split in
  // the same orientation as the section's direct parent, with no other split
  // orientations between them.
  fn get_aligning_base<'l, It>(&self, path: &mut It)
      -> Option<(SectionPath, Orientation, bool)>
      where It: Iterator<Item=&'l SectionSide> {
    path.next().and_then(|&go|
      self.split.as_ref().and_then(|ref split|
        (if go == Fst { &split.fst } else { &split.snd }).
          get_aligning_base(path).
          map(|(path, align_orientation, committed)| {
            let new_base = !committed && align_orientation == split.orientation;
            let new_path = if new_base { SectionPath::new() }
                           else        { path.prepend(go) };
            (new_path, align_orientation, !new_base) }).
          or(Some((SectionPath::new(), split.orientation, false)))))
  }

  // Gets the "|side|-most" leaf from the starting point defined by |path|.
  fn get_leaf_from<'l, It>(&self, path: &mut It, side: SectionSide)
      -> SectionPath
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None            => panic!("get_leaf_from: Bad path"),
        Some(ref split) =>
          (if go == Fst { &split.fst } else { &split.snd }).
            get_leaf_from(path, side).prepend(go),
      }).
    unwrap_or_else(|| self.get_leaf(side))
  }

  // Gets the "|side|-most" leaf under this section.
  fn get_leaf(&self, side: SectionSide) -> SectionPath {
    match self.split {
      None            => SectionPath::new(),
      Some(ref split) =>
        (if side == Fst { &split.fst } else { &split.snd }).
          get_leaf(side).prepend(side),
    }
  }

  // Performs hit test to get the section containing |point|. Hitting a border
  // counts towards the first subsection.
  fn get_section_at_point(&self, position: screen::Cell, point: screen::Cell)
      -> Option<SectionPath> {
    match self.split {
      None            => {
        let hit = screen::Rect(position, self.size).contains(point);
        if hit { Some(SectionPath::new()) } else { None }
      }
      Some(ref split) => {
        // calculate the rects for subsections and border area
        let fst_rect = screen::Rect(position, split.fst.size);
        let border_rect =
          border_rect(position, split.orientation, split.fst.size);
        let snd_pos = snd_position(position, split.fst.size, split.orientation);
        let snd_rect = screen::Rect(snd_pos, split.snd.size);

        // perform hit test and recurse deeper
        if fst_rect.contains(point) || border_rect.contains(point) {
          split.fst.get_section_at_point(position, point).
          or_else(|| Some(split.fst.get_leaf(Snd))).
          map(|path| path.prepend(Fst))
        }
        else if snd_rect.contains(point) {
          split.snd.get_section_at_point(snd_pos, point).
          map(|path| path.prepend(Snd))
        }
        else {
          None
        }
      }
    }
  }

  fn get_rect<'l, It>(&self, path: &mut It, position: screen::Cell)
      -> screen::Rect
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None            => panic!("get_rect: Bad path"),
        Some(ref split) => {
          let next_position = if go == Fst { position } else {
            snd_position(position, split.fst.size, split.orientation)
          };
          (if go == Fst { &split.fst } else { &split.snd }).
            get_rect(path, next_position)
        }
      }).
    unwrap_or(screen::Rect(position, self.size))
  }

  // Gets an adjacent section to the section defined by |path|. The
  // |orientation|,|side| pair essentially defines a direction in which to look
  // for the adjacent section.
  fn get_adjacent<'l, It>(&self, path: &mut It, orientation: Orientation,
                          side: SectionSide) -> Option<SectionPath>
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    and_then(|&go|
      match self.split {
        None            => panic!("get_adjacent: Bad path"),
        Some(ref split) =>
          (if go == Fst { &split.fst } else { &split.snd }).
            get_adjacent(path, orientation, side).
            map(|path| path.prepend(go)).
            or(if split.orientation == orientation || go == side { None }
               else { Some(SectionPath::new().prepend(side)) }),
      })
  }

  // Attempts to shift the split of the section defined by |path| by |amount|.
  // A negative amount implies shifting in the top/left direction.
  // Returns the absorbed amount.
  fn shift_split<'l, It>(&mut self, path: &mut It, amount: int) -> int
      where It: Iterator<Item=&'l SectionSide> {
    path.next().
    map(|&go|
      match self.split {
        None                => panic!("shift_split: Bad path."),
        Some(ref mut split) =>
          (if go == Fst { &mut split.fst } else { &mut split.snd }).
            shift_split(path, amount),
      }).
    unwrap_or_else(||
      match self.split {
        None                => 0,
        Some(ref mut split) => {
          // first shift the edge of the shrinking section, then shift the other
          // section's edge as much as the first section was able to shrink
          let (either, other, either_side, other_side) =
            if amount < 0 { (&mut split.fst, &mut split.snd, Snd, Fst) }
            else          { (&mut split.snd, &mut split.fst, Fst, Snd) };

          let shift_orientation = split.orientation.opposite();
          let either_took =
            either.shift_edge(shift_orientation, either_side, amount);
          let other_took =
            other.shift_edge(shift_orientation, other_side, either_took);
          assert_eq!(either_took, other_took);

          either_took
        }
      })
  }

  // Attempts to shift the edge defined by |orientation| and |side| by |amount|.
  // A negative amount implies shifting in the top/left direction.
  // Returns the absorbed amount.
  fn shift_edge(&mut self, orientation: Orientation, side: SectionSide,
                amount: int) -> int {
    let screen::Size(rows, cols) = self.size;
    let size = if orientation == Vertical { rows } else { cols } as int;

    // let the subsections absorb the amount by shifting them first, then
    // set the size of self according to how much was absorbed
    let took = match self.split {
      None                =>
        // no subsections to be concerned about, limit the absorbed amount only
        // if the section is shrinking
        if (side == Fst && amount < 0) || (side == Snd && amount > 0) {
          amount
        }
        else {
          if amount > 0 { cmp::min(amount, size - MIN_SECTION_SIZE as int) }
          else          { cmp::max(amount, MIN_SECTION_SIZE as int - size) }
        },
      Some(ref mut split) =>
        if split.orientation == orientation {
          // Shifting an edge alongside the split, meaning both subsections
          // share the edge. First shift the edge of the subsection containing
          // the most splits, then shift the other section's edge as much as the
          // first section was able to absorb.
          let fst_splits = split.fst.num_splits_along(orientation);
          let snd_splits = split.snd.num_splits_along(orientation);
          let (either, other) =
            if fst_splits > snd_splits { (&mut split.fst, &mut split.snd) }
            else                       { (&mut split.snd, &mut split.fst) };
          let either_took = either.shift_edge(orientation, side, amount);
          let other_took = other.shift_edge(orientation, side, either_took);
          assert_eq!(either_took, other_took);
          either_took
        }
        else {
          // Shifting an edge against the split, meaning only one subsection has
          // the actual edge of interest. First shift that subsection's edge,
          // then shift the same edge of the other subsection only as much as
          // the first subsection wasn't able to absorb.
          let (either, other) =
            if side == Fst { (&mut split.fst, &mut split.snd) }
            else           { (&mut split.snd, &mut split.fst) };
          let either_took = either.shift_edge(orientation, side, amount);
          let remains = amount - either_took;
          let other_took = other.shift_edge(orientation, side, remains);
          either_took + other_took
        },
    };

    let size_add = if side == Fst { -took } else { took };
    self.set_size_along(orientation, (size + size_add) as u16);

    took
  }

  fn set_size_along(&mut self, orientation: Orientation, size: u16) {
    let &mut screen::Size(ref mut rows, ref mut cols) = &mut self.size;
    if orientation == Vertical { *rows = size } else { *cols = size };
  }

  #[cfg(not(test))]
  fn draw_borders(&self, position: screen::Cell, screen: &mut screen::Screen) {
    self.split.as_ref().map(|ref split| {
      split.fst.draw_borders(position, screen);
      split.snd.draw_borders(
        snd_position(position, split.fst.size, split.orientation), screen);

      for screen_cell in screen::CellIterator::new(
          border_rect(position, split.orientation, split.fst.size)) {
        let border_char = ' ';
        let border_color = screen::Color::Cyan;
        screen.put(screen_cell, border_char, border_color, border_color);
      }
    });
  }
}

fn border_rect(position: screen::Cell, orientation: Orientation,
               screen::Size(fst_rows, fst_cols): screen::Size) -> screen::Rect {
  let border_position = position +
    if orientation == Vertical { screen::Cell(0, fst_cols) }
    else                       { screen::Cell(fst_rows, 0) };
  let border_size =
    if orientation == Vertical { screen::Size(fst_rows, BORDER_SIZE) }
    else                       { screen::Size(BORDER_SIZE, fst_cols) };
  screen::Rect(border_position, border_size)
}

fn snd_position(fst_position: screen::Cell, fst_size: screen::Size,
                orientation: Orientation) -> screen::Cell {
    let screen::Size(fst_rows, fst_cols) = fst_size;
    return fst_position +
      if orientation == Vertical { screen::Cell(0, fst_cols + BORDER_SIZE) }
      else                       { screen::Cell(fst_rows + BORDER_SIZE, 0) };
}

/*
 * LeafSectionIterator iterates the leafs of a section tree in either direction,
 * determined by |side|. (side == Fst -> Snd-most to Fst-most)
 */
struct LeafSectionIterator<'l> {
  main_section: &'l Section,
  side: SectionSide,
  next: Option<SectionPath>,
}

impl<'l> LeafSectionIterator<'l> {
  fn new(main_section: &'l Section, side: SectionSide)
      -> LeafSectionIterator<'l> {
    LeafSectionIterator {
      main_section: main_section,
      side: side,
      next: Some(main_section.get_leaf(if side == Fst { Snd } else { Fst })),
    }
  }

  // Starts the iterator from a given, already explored, path.
  fn from(mut self, path: SectionPath) -> LeafSectionIterator<'l> {
    self.next = self.find_next(path.clone());
    self
  }

  fn find_next(&self, mut path: SectionPath) -> Option<SectionPath> {
    let done_with_root = path.first().map(|go| go == self.side).unwrap_or(true);

    // unwind the path until there's an unexplored section of interest waiting
    loop { if path.pop().map(|v| v != self.side).unwrap_or(true) { break } }

    if path.len() == 0 && done_with_root {
      // we got back to the root and we've already been down each side, all done
      None
    }
    else {
      // reach down to the next leaf
      Some(self.main_section.get_leaf_from(&mut path.append(self.side).iter(),
        if self.side == Fst { Snd } else { Fst }))
    }
  }
}

impl<'l> Iterator for LeafSectionIterator<'l> {
  type Item = SectionPath;

  fn next(&mut self) -> Option<SectionPath> {
    let ret = self.next.clone();
    self.next =
      self.next.as_ref().and_then(|last| self.find_next(last.clone()));
    return ret;
  }
}

/*
 * The various errors that may result from usage of the frame.
 */
#[derive(Copy, Debug, PartialEq)]
pub enum FrameError {
  NoSuchWindow,
  CantCloseLastWindow,
  NoNeighbouringWindow,
  NoSuchSequentWindow,
  NoSuchAdjacentWindow,
}

impl fmt::Display for FrameError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", *self)
  }
}

impl error::Error for FrameError {
  fn description(&self) -> &str {
    match *self {
      FrameError::NoSuchWindow         =>
        "The provided WindowId does not refer to a window known by the frame.",
      FrameError::CantCloseLastWindow  =>
        "Attempted to close the last window in the frame.",
      FrameError::NoNeighbouringWindow =>
        "The window has no neighbours. (it's the last window)",
      FrameError::NoSuchSequentWindow  =>
        "The window had no sequent window in the provided window order.",
      FrameError::NoSuchAdjacentWindow =>
        "The window had no adjacent window in the provided direction.",
    }
  }
}

type FrameResult<T> = Result<T, FrameError>;

#[derive(Copy)]
pub enum Direction {
  Left,
  Right,
  Up,
  Down,
}

#[derive(Copy, PartialEq)]
pub enum WindowOrder {
  NextWindow,
  PreviousWindow,
}

/*
 * Identifier for the windows managed by the frame.
 */
pub type WindowId = uuid::Uuid;

/*
 * The FrameContext keeps track of the mapping of WindowIds to SectionPaths.
 */
pub struct FrameContext {
  window_to_section_path: HashMap<WindowId, SectionPath>,
}

impl FrameContext {
  fn add_section_path(&mut self, path: SectionPath) -> WindowId {
    let window = uuid::Uuid::new_v4();
    self.set_section_path(&window, path);
    return window;
  }

  fn set_section_path(&mut self, window: &WindowId, path: SectionPath) {
    self.window_to_section_path.insert(window.clone(), path);
  }

  fn get_section_path(&self, window: &WindowId) -> FrameResult<&SectionPath> {
    self.window_to_section_path.get(window).ok_or(FrameError::NoSuchWindow)
  }

  fn get_window(&self, path: &SectionPath) -> WindowId {
    self.window_to_section_path.iter().
      find(|&(_,val)| *val == *path).
      map(|(key,_)| key.clone()).
      expect("Tried to get WindowId of unexisting window.")
  }
}

/*
 * A Frame consist of one main section which may be split in any number of ways.
 * It is the leafs of the section tree rooted by the main section which are the
 * windows referred to by the WindowIds. A Frame has an accompanying
 * FrameContext used to map these WindowIds to the SectionPaths leading to the
 * leaf sections.
 * The size of the main section may exceed that of the Frame when the
 * subsections don't fit in the desired space, in which case the Frame will
 * realign the sizes again whenever possible.
 */
pub struct Frame {
  size: screen::Size,
  main_section: Section,
}

impl Frame {
  pub fn new() -> (Frame, FrameContext, WindowId) {
    let frame = Frame {
      size: screen::Size(0, 0),
      main_section: Section::new(),
    };
    let mut ctx = FrameContext { window_to_section_path: HashMap::new() };
    let main_window = ctx.add_section_path(SectionPath::new());
    (frame, ctx, main_window)
  }

  pub fn split_window(&mut self, ctx: &mut FrameContext, window: &WindowId,
                      orientation: Orientation) -> FrameResult<WindowId> {
    ctx.get_section_path(window).
    map(|path| {
      // first make sure the window is large enough to fit the split
      let screen::Rect(_, screen::Size(rows, cols)) =
        self.get_section_rect(path);
      let size = if orientation == Vertical { cols } else { rows };
      let min_size = MIN_SECTION_SIZE * 2 + BORDER_SIZE;
      if size < min_size {
        // try to resize the window to fit the split
        let grow_orientation = orientation.opposite();
        let increase = min_size as int - size as int;
        let resized =
          self.resize_window(ctx, window, grow_orientation, increase).unwrap();
        if resized < increase {
          // make room in the section, guaranteeing the window resize to succeed
          let force_increase = increase - resized;
          self.main_section.shift_edge(grow_orientation, Snd, force_increase);
          self.resize_window(
            ctx, window, grow_orientation, force_increase).unwrap();
        }
      }
      self.main_section.split(&mut path.iter(), orientation) }).
    map(|(fst, snd)| {
      // reset layout and update the context with the two new sections
      self.main_section.reset_sizes_along(orientation.opposite());
      ctx.set_section_path(window, fst);
      return ctx.add_section_path(snd); })
  }

  pub fn close_window(&mut self, ctx: &mut FrameContext, window: &WindowId)
      -> FrameResult<()> {
    ctx.get_section_path(window).
    map(|path| path.clone()).
    and_then(|path| {
      let mut parent_section_path = path.clone();
      parent_section_path.pop().
      ok_or(FrameError::CantCloseLastWindow).
      map(|side| {
        // get orientation we want to reset layout along after closing window
        let reset_orientation =
          self.main_section.get_aligning_base(&mut path.iter()).
          map(|(_, orientation, _)| orientation).unwrap().opposite();

        // remove the section
        self.main_section.cut_branch(&mut parent_section_path.iter(), side);

        // update the context
        ctx.window_to_section_path.remove(window);
        for (_, path) in ctx.window_to_section_path.iter_mut() {
          if parent_section_path.does_prefix(path) {
            path.remove(parent_section_path.len());
          }
        }

        // reset the layout
        let screen::Size(section_rows, section_cols) = self.main_section.size;
        let screen::Size(frame_rows, frame_cols) = self.size;
        let (section_size, frame_size) =
          if reset_orientation == Vertical { (section_rows, frame_rows) }
          else                             { (section_cols, frame_cols) };
        if section_size > frame_size {
          let size_diff = frame_size as int - section_size as int;
          self.main_section.shift_edge(reset_orientation, Snd, size_diff);
        }
        else {
          self.main_section.reset_sizes_along(reset_orientation);
        } }) })
  }

  // The closest neighbour of a window is defined as the next window unless that
  // window doesn't exist or is under a different aligning base, in which case
  // the closest neighbour is the previous window.
  pub fn get_closest_neighbouring_window(&self, ctx: &FrameContext,
                                         window: &WindowId)
      -> FrameResult<WindowId> {
    ctx.get_section_path(window).
    map(|path| path.clone()).
    and_then(|path|
      self.main_section.get_aligning_base(&mut path.iter()).
      ok_or(FrameError::NoNeighbouringWindow).
      and_then(|(aligning_base_path, _, _)|
        self.get_sequent_window(ctx, window, NextWindow, false).
        and_then(|next_window|
          ctx.get_section_path(&next_window).
          map(|next_path| next_path.clone()).
          or_else(|_| panic!("Couldn't get section path for existing window.")).
          and_then(|next_path| {
            let next_is_neighbour = aligning_base_path.does_prefix(&next_path);
            if next_is_neighbour { Ok(ctx.get_window(&next_path)) }
            else                 { Err(FrameError::NoNeighbouringWindow) }})).
        or_else(|_|
          self.get_sequent_window(ctx, window, PreviousWindow, false))))
  }

  // Gets next or previous window in sequence determined by the section tree.
  pub fn get_sequent_window(&self, ctx: &FrameContext, window: &WindowId,
                            order: WindowOrder, wrap: bool)
      -> FrameResult<WindowId> {
    let side = if order == NextWindow { Snd } else { Fst };
    ctx.get_section_path(window).
    and_then(|path|
      LeafSectionIterator::new(&self.main_section, side).from(path.clone()).
        next().or_else(||
          if wrap { LeafSectionIterator::new(&self.main_section, side).next() }
          else    { None }).
        ok_or(FrameError::NoSuchSequentWindow)).
    map(|path| ctx.get_window(&path))
  }

  // Picks a point next to the window's rect depending on |direction| and
  // performs hit tests down the section tree to find the adjacent window.
  pub fn get_adjacent_window(&self, ctx: &FrameContext, window: &WindowId,
                             direction: Direction) -> FrameResult<WindowId> {
    self.get_window_rect(ctx, window).
    and_then(|rect|
      adjacent_window_point(rect, direction).
      and_then(|point|
        self.main_section.get_section_at_point(screen::Cell(0, 0), point)).
      ok_or(FrameError::NoSuchAdjacentWindow)).
    map(|path| ctx.get_window(&path))
  }

  pub fn get_window_rect(&self, ctx: &FrameContext, window: &WindowId)
      -> FrameResult<screen::Rect> {
    ctx.get_section_path(window).map(|path| self.get_section_rect(path))
  }

  fn get_section_rect(&self, path: &SectionPath) -> screen::Rect {
    self.main_section.get_rect(&mut path.iter(), screen::Cell(0, 0))
  }

  // Tries to resize |window| by |amount| in |orientation| by first pushing the
  // bottom/right edges of that window, then pushing the top/left edges if there
  // wasn't enough space for the desired resize-amount.
  // Returns the amount the resize was able to absorb.
  pub fn resize_window(&mut self, ctx: &FrameContext, window: &WindowId,
                       orientation: Orientation, amount: int)
      -> FrameResult<int> {
    ctx.get_section_path(window).
    map(|path| {
      let clamped_amount = if amount > 0 { amount } else {
        let screen::Rect(_, screen::Size(rows, cols)) =
          self.get_section_rect(path);
        let size = if orientation == Vertical { rows } else { cols };
        let min_size = self.window_min_size(ctx, window, orientation);
        cmp::max(amount, min_size as int - size as int)
      };
      let snd_took = self.resize_section_recursive(
        path, orientation, Snd, clamped_amount);
      let fst_took = self.resize_section_recursive(
        path, orientation, Fst, clamped_amount - snd_took);
      fst_took + snd_took })
  }

  // Gets the minimum size a window may have along an orientation, paying
  // respect to surrounding windows.
  fn window_min_size(&mut self, ctx: &FrameContext, window: &WindowId,
                     orientation: Orientation) -> u16 {
    ctx.get_section_path(window).ok().
    and_then(|path| self.main_section.get_aligning_base(&mut path.iter())).
    map(|(aligning_base_path, aligning_base_orientation, _)|
      if aligning_base_orientation != orientation { MIN_SECTION_SIZE } else {
        let num_splits = self.main_section.num_splits_along_at(
          orientation, &mut aligning_base_path.iter());
        num_splits * BORDER_SIZE + (num_splits + 1) * MIN_SECTION_SIZE
      }).
    unwrap_or(MIN_SECTION_SIZE)
  }

  // Resizes a section by pushing one of its edges by as much as an ancestor
  // section allows, then possibly recurses up to that ancestor to do the same
  // thing there, and finally tries to push its edge again if the ancestor now
  // is less restrictive.
  // Returns the amount the resize was able to absorb.
  fn resize_section_recursive(&mut self, path: &SectionPath,
                              orientation: Orientation, side: SectionSide,
                              amount: int) -> int {
    self.resize_section(path, orientation, side, amount).
    map(|(absorbed, ancestor_path)|
      if absorbed == amount { absorbed } else {
        let ancestor_took = self.resize_section_recursive(
          &ancestor_path, orientation, side, amount - absorbed);
        if ancestor_took == 0 { absorbed } else {
          self.resize_section(path, orientation, side, ancestor_took).
            map(|(v, _)| v).unwrap() + absorbed
        }
      }).
    unwrap_or(0)
  }

  // Attempts to resize a section by shifting the split of the ancestor which
  // make up the relevant edge of that section. The ancestor of interest is the
  // base between the section and any adjacent section on the opposite side of
  // that edge.
  // Returns the amount the resize was able to absorb along with the path to the
  // relevant ancestor.
  fn resize_section(&mut self, path: &SectionPath, orientation: Orientation,
                    side: SectionSide, amount: int)
      -> Option<(int, SectionPath)> {
    self.main_section.get_adjacent(&mut path.iter(), orientation, side).
    map(|adjacent_path| path.common_base(&adjacent_path)).
    map(|base_path| {
      let adjusted_amount = if side == Snd { amount } else { -amount };
      let shifted = self.main_section.shift_split(
        &mut base_path.iter(), adjusted_amount);
      let adjusted_shifted = if side == Snd { shifted } else { -shifted };
      (adjusted_shifted, base_path.clone()) })
  }

  // Attempts to resize the main section by shifting its bottom/right edges.
  pub fn set_size(&mut self, size: screen::Size) {
    let screen::Size(new_rows, new_cols) = size;
    let screen::Size(section_rows, section_cols) = self.main_section.size;
    let row_diff = new_rows as int - section_rows as int;
    self.main_section.shift_edge(Vertical, Snd, row_diff);
    let col_diff = new_cols as int - section_cols as int;
    self.main_section.shift_edge(Horizontal, Snd, col_diff);
    self.size = size;
  }

  pub fn reset_layout(&mut self) {
    self.main_section.reset_sizes_along(Vertical);
    self.main_section.reset_sizes_along(Horizontal);
  }

  #[cfg(not(test))]
  pub fn draw_borders(&self, screen: &mut screen::Screen) {
    self.main_section.draw_borders(screen::Cell(0, 0), screen);
  }
}

fn adjacent_window_point(rect: screen::Rect, direction: Direction)
    -> Option<screen::Cell> {
  let screen::Rect(screen::Cell(row, col), screen::Size(rows, cols)) = rect;
  let sub = BORDER_SIZE + 1;
  match direction {
    Direction::Left  => if col >= sub { Some(screen::Cell(row, col - sub)) }
                        else          { None },
    Direction::Right => Some(screen::Cell(row, col + cols + BORDER_SIZE)),
    Direction::Up    => if row >= sub { Some(screen::Cell(row - sub, col)) }
                        else          { None },
    Direction::Down  => Some(screen::Cell(row + rows + BORDER_SIZE, col)),
  }
}

/*
 * Testing for the frame is somewhat lax in that we don't expect any exact
 * layout after an operation. Most tests ensure that certain invariants always
 * hold, and at that some operations on windows of course expect specific
 * outcomes for the window in question.
 */
#[cfg(test)]
mod test {
  use std::collections::VecMap;

  use screen;

  use super::Orientation::*;
  use super::SectionSide::*;
  use super::WindowOrder::*;

  // sanity check the state of a frame
  fn check_frame_invariants(frame: &super::Frame, ctx: &super::FrameContext) {
    check_size_invariant(frame);
    check_window_to_section_map_invariant(frame, ctx);
  }

  // The main section is never smaller than the frame. Either the main section's
  // size equal the frame's size, or the main section is larger and all
  // subsections are as small as they can be. In either case, the size of a
  // section with a split does always equal the union of the subsections' sizes
  // along with border size.
  fn check_size_invariant(frame: &super::Frame) {
    check_size_invariant_along(frame, Vertical);
    check_size_invariant_along(frame, Horizontal);
  }

  fn check_size_invariant_along(frame: &super::Frame,
                                orientation: super::Orientation) {
    let screen::Size(frame_rows, frame_cols) = frame.size;
    let screen::Size(section_rows, section_cols) = frame.main_section.size;
    let (frame_size, section_size) =
      if orientation == Vertical { (frame_rows, section_rows) }
      else                       { (frame_cols, section_cols) };

    assert!(
      (frame_size == section_size ||
        (frame_size < section_size &&
          all_minimized_along(&frame.main_section, orientation))) &&
      equal_subsection_size_along(&frame.main_section, orientation));
  }

  fn equal_subsection_size_along(section: &super::Section,
                                 orientation: super::Orientation) -> bool {
    match section.split {
      None            => true,
      Some(ref split) => {
        let screen::Size(rows, cols) = section.size;
        let screen::Size(fst_rows, fst_cols) = split.fst.size;
        let screen::Size(snd_rows, snd_cols) = split.snd.size;
        let (size, fst_size, snd_size) =
          if orientation == Vertical { (rows, fst_rows, snd_rows) }
          else                       { (cols, fst_cols, snd_cols) };

        equal_subsection_size_along(&*split.fst, orientation) &&
          equal_subsection_size_along(&*split.snd, orientation) &&
            if orientation == split.orientation {
              fst_size == size && size == snd_size
            }
            else {
              size == (fst_size + snd_size + super::BORDER_SIZE)
            }
      }
    }
  }

  fn all_minimized_along(section: &super::Section,
                         orientation: super::Orientation) -> bool {
    match section.split {
      None            => {
        let screen::Size(rows, cols) = section.size;
        let size = if orientation == Vertical { rows } else { cols };
        size == super::MIN_SECTION_SIZE
      }
      Some(ref split) => {
        if orientation == split.orientation {
          let fst_splits = split.fst.num_splits_along(orientation);
          let snd_splits = split.snd.num_splits_along(orientation);
          if fst_splits > snd_splits {
            return all_minimized_along(&*split.fst, orientation);
          }
          else if snd_splits > fst_splits {
            return all_minimized_along(&*split.snd, orientation);
          }
        }

        all_minimized_along(&*split.fst, orientation) &&
            all_minimized_along(&*split.snd, orientation)
      }
    }
  }

  // All windows in the frame context should lead to a leaf in the section tree,
  // and all leafs in the section tree should have a corresponding window id.
  // There may also be no dupes in the map.
  fn check_window_to_section_map_invariant(frame: &super::Frame,
                                           ctx: &super::FrameContext) {
    use std::collections::HashSet;
    let actual_leafs: HashSet<super::SectionPath> =
      super::LeafSectionIterator::new(&frame.main_section, Snd).collect();
    let leafs_in_context: HashSet<super::SectionPath> =
      ctx.window_to_section_path.values().map(|v| v.clone()).collect();
    assert_eq!(actual_leafs, leafs_in_context);
    assert_eq!(leafs_in_context.len(), ctx.window_to_section_path.len());
  }

  // A split descriptor here is an array of (window number, orientation) pairs
  // used to describe a sequence of splits resulting in a desired frame state.
  // Starting with one window and assigning it the number 0 then splitting
  // sequentially and assigning each new window the next number results in the
  // frames and section trees depicted along with the split descriptors below.
  const SPLIT_DESCRIPTORS: [&'static  [(uint, super::Orientation)]; 3] =
    [
      // -----------------                     V
      // |     0     |   |                    / \
      // |-----------|   |                   H   1
      // |   |   | 5 |   |                  / \
      // |   |   |---|   |                 H   2
      // | 3 | 4 | 7 | 1 |                / \
      // |   |   |---|   |               0   V
      // |   |   | 6 |   |                  / \
      // |-----------|   |                 3   V
      // |     2     |   |                    / \
      // -----------------                   4   H
      //                                        / \
      //                                       H   6
      //                                      / \
      //                                     5   7
      &[(0, Vertical), (0, Horizontal), (0, Horizontal), (3, Vertical),
        (4, Vertical), (5, Horizontal), (5, Horizontal)],

      // ---------                       ______H______
      // | 0 | 2 |                      /             \
      // |---|---|                   __V__           __H__
      // | 5 | 3 |                  /     \         /     \
      // ---------                 H       H       V       V
      // | 1 | 7 |                / \     / \     / \     / \
      // ---------               0   5   2   3   1   7   4   6
      // | 4 | 6 |
      // ---------
      &[(0, Horizontal), (0, Vertical), (2, Horizontal), (1, Horizontal),
        (0, Horizontal), (4, Vertical), (1, Vertical)],

      // ---------                        _____H_____
      // | 0 | 4 |                       /           \
      // ---------                      V         ___H___
      // | 1 | 5 |                     / \       /       \
      // ---------                    0   4   __H__       V
      // | 3 | 6 |                           /     \     / \
      // ---------                          V       V   2   7
      // | 2 | 7 |                         / \     / \
      // ---------                        1   5   3   6
      &[(0, Horizontal), (1, Horizontal), (1, Horizontal), (0, Vertical),
        (1, Vertical), (3, Vertical), (2, Vertical)],
    ];

  fn setup_frame(descriptor_nr: uint)
      -> (super::Frame, super::FrameContext, Vec<super::WindowId>) {
    let mut windows = Vec::new();
    let (mut frame, mut ctx, main_window) = super::Frame::new();
    frame.set_size(screen::Size(100, 100));
    windows.push(main_window);

    for &(window_nr, orientation) in SPLIT_DESCRIPTORS[descriptor_nr].iter() {
      let new_window = frame.split_window(
        &mut ctx, &windows[window_nr], orientation).unwrap();
      windows.push(new_window);
      check_frame_invariants(&frame, &ctx);
    }

    return (frame, ctx, windows);
  }

  // For every split descriptor, setup then tear down the frame in every
  // possible way.. limited to 100 ways. Because come on. We don't got all day.
  // And of course check invariants along the way.
  #[test]
  fn split_and_close() {
    for descriptor_nr in range(0, SPLIT_DESCRIPTORS.len()) {
      let num_windows = SPLIT_DESCRIPTORS[descriptor_nr].len() + 1;
      let window_nrs: Vec<uint> = range(0, num_windows).collect();
      for window_nrs in window_nrs.as_slice().permutations().take(100) {
        let (mut frame, mut ctx, windows) = setup_frame(descriptor_nr);
        let close_last = window_nrs[0];
        for &window_nr in window_nrs.iter().skip(1) {
          frame.close_window(&mut ctx, &windows[window_nr]).unwrap();
          check_frame_invariants(&frame, &ctx);
        }
        assert_eq!(Err(super::FrameError::CantCloseLastWindow),
          frame.close_window(&mut ctx, &windows[close_last]));
      }
    }
  }

  #[test]
  fn split_beyond_frame_size_and_close() {
    let (mut frame, mut ctx, mut windows) = setup_frame(0);

    for window_nr in range(0, 100) {
      let new_window = frame.split_window(
        &mut ctx, &windows[window_nr], Vertical).unwrap();
      windows.push(new_window);
      check_frame_invariants(&frame, &ctx);
    }

    for window_nr in range(50, windows.len()).chain(range(1, 50)) {
      frame.close_window(&mut ctx, &windows[window_nr]).unwrap();
      check_frame_invariants(&frame, &ctx);
    }
  }

  // Setup a frame and perform a number of resize operations on it. Inspect one
  // window and its size at a time. Reset frame size, then do it over again with
  // another window.
  // Beware of ugly constants, but fear not, for they are all from the |adds|
  // array below, or a number of windows/borders in the frame.
  #[test]
  fn set_size_frame_0() {
    let (mut frame, ctx, windows) = setup_frame(0);
    let start_size = 100;

    let adds = [(-50, 20), (-50, -60), (130, -60), (0, 100)];

    let win = 1;
    let screen::Rect(_, screen::Size(win_rows, win_cols)) =
      frame.get_window_rect(&ctx, &windows[win]).unwrap();
    let win_sizes = [
      screen::Size(win_rows - 50, win_cols + 20),
      screen::Size(5 * super::MIN_SECTION_SIZE + 4 * super::BORDER_SIZE,
        super::MIN_SECTION_SIZE),
      screen::Size(130, super::MIN_SECTION_SIZE),
      screen::Size(130,
        100 - 3 * super::MIN_SECTION_SIZE - 3 * super::BORDER_SIZE)
    ];
    do_frame_resizes(&mut frame, &ctx, &windows[win], &adds, &win_sizes);

    frame.set_size(screen::Size(start_size, start_size));
    check_frame_invariants(&frame, &ctx);
    frame.reset_layout();
    check_frame_invariants(&frame, &ctx);

    let win = 0;
    let screen::Rect(_, screen::Size(win_rows, win_cols)) =
      frame.get_window_rect(&ctx, &windows[win]).unwrap();
    let win_sizes = [
      screen::Size(win_rows, win_cols),
      screen::Size(super::MIN_SECTION_SIZE,
        start_size + 20 - 60 - super::MIN_SECTION_SIZE - super::BORDER_SIZE),
      screen::Size(super::MIN_SECTION_SIZE,
        3 * super::MIN_SECTION_SIZE + 2 * super::BORDER_SIZE),
      screen::Size(super::MIN_SECTION_SIZE,
        3 * super::MIN_SECTION_SIZE + 2 * super::BORDER_SIZE),
    ];
    do_frame_resizes(&mut frame, &ctx, &windows[win], &adds, &win_sizes);
  }

  // Same-ish as above, different window setup, different resizing.
  #[test]
  fn set_size_frame_2() {
    let (mut frame, ctx, windows) = setup_frame(2);
    let start_size = 100;

    let adds = [(0, -70), (10, 0), (-50, -30), (40, 50)];

    let win = 3;
    let screen::Rect(_, screen::Size(win_rows, _)) =
      frame.get_window_rect(&ctx, &windows[win]).unwrap();
    let screen::Rect(_, screen::Size(win_2_rows, _)) =
      frame.get_window_rect(&ctx, &windows[2]).unwrap();
    let win_sizes = [
      screen::Size(win_rows,
        start_size - 70 - super::MIN_SECTION_SIZE - super::BORDER_SIZE),
      screen::Size(win_rows,
        start_size - 70 - super::MIN_SECTION_SIZE - super::BORDER_SIZE),
      screen::Size(win_rows + win_2_rows + 10 - 50 - super::MIN_SECTION_SIZE,
        super::MIN_SECTION_SIZE),
      screen::Size(win_rows + win_2_rows + 10 - 50 - super::MIN_SECTION_SIZE,
        super::MIN_SECTION_SIZE),
    ];
    do_frame_resizes(&mut frame, &ctx, &windows[win], &adds, &win_sizes);

    frame.set_size(screen::Size(start_size, start_size));
    check_frame_invariants(&frame, &ctx);
    frame.reset_layout();
    check_frame_invariants(&frame, &ctx);

    let win = 6;
    let screen::Rect(_, screen::Size(win_rows, _)) =
      frame.get_window_rect(&ctx, &windows[win]).unwrap();
    let win_sizes = [
      screen::Size(win_rows, super::MIN_SECTION_SIZE),
      screen::Size(win_rows, super::MIN_SECTION_SIZE),
      screen::Size(win_rows + win_2_rows + 10 - 50 - super::MIN_SECTION_SIZE,
        super::MIN_SECTION_SIZE),
      screen::Size(win_rows + win_2_rows + 10 - 50 - super::MIN_SECTION_SIZE,
        50 - super::MIN_SECTION_SIZE - super::BORDER_SIZE),
    ];
    do_frame_resizes(&mut frame, &ctx, &windows[win], &adds, &win_sizes);
  }

  // Add to the frame's size, as described by the row/col pairs in |adds|, and
  // compare the size of |window| to the expected sizes in |win_sizes| after
  // each resize. As always, also check invariants.
  fn do_frame_resizes(frame: &mut super::Frame, ctx: &super::FrameContext,
                      window: &super::WindowId, adds: &[(i16, i16)],
                      win_sizes: &[screen::Size]) {
    for (&(add_rows, add_cols), win_size_expect) in
        adds.iter().zip(win_sizes.iter()) {
      let screen::Size(frame_rows, frame_cols) = frame.size;
      frame.set_size(screen::Size((frame_rows as i16 + add_rows) as u16,
                                  (frame_cols as i16 + add_cols) as u16));
      check_frame_invariants(frame, ctx);
      let screen::Rect(_, win_size) =
        frame.get_window_rect(ctx, window).unwrap();
      assert_eq!(win_size, *win_size_expect);
    }
  }

  // Setup a frame and resize a few windows, each time expecting certain size
  // changes in surrounding windows.
  #[test]
  fn window_resize_frame_0() {
    let (mut frame, ctx, windows) = setup_frame(0);

    let resize = (6, Vertical, 10);
    let mut window_changes = VecMap::new();
    window_changes.insert(2, -10);
    window_changes.insert(3, 10);
    window_changes.insert(4, 10);
    window_changes.insert(6, 10);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let resize = (7, Vertical, 5);
    let mut window_changes = VecMap::new();
    window_changes.insert(6, -5);
    window_changes.insert(7, 5);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let resize = (5, Horizontal, 15);
    let mut window_changes = VecMap::new();
    window_changes.insert(0, 15);
    window_changes.insert(1, -15);
    window_changes.insert(2, 15);
    window_changes.insert(5, 15);
    window_changes.insert(6, 15);
    window_changes.insert(7, 15);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let screen::Rect(_, screen::Size(win2_rows, _)) =
      frame.get_window_rect(&ctx, &windows[2]).unwrap();

    let win6_resize = win2_rows as i16 + 5;
    let win2_resize = super::MIN_SECTION_SIZE as i16 - win2_rows as i16;
    let resize = (6, Vertical, win6_resize as int);
    let mut window_changes = VecMap::new();
    window_changes.insert(2, win2_resize);
    window_changes.insert(3, -win2_resize);
    window_changes.insert(4, -win2_resize);
    window_changes.insert(6, win6_resize);
    window_changes.insert(7, -win6_resize - win2_resize);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    frame.reset_layout();
    check_frame_invariants(&frame, &ctx);
  }

  // Same-ish as above, different window setup, different resizing.
  #[test]
  fn window_resize_frame_1() {
    let (mut frame, ctx, windows) = setup_frame(1);

    let screen::Rect(_, screen::Size(win0_rows, _)) =
      frame.get_window_rect(&ctx, &windows[0]).unwrap();
    let screen::Rect(_, screen::Size(win1_rows, _)) =
      frame.get_window_rect(&ctx, &windows[1]).unwrap();
    let screen::Rect(_, screen::Size(win4_rows, _)) =
      frame.get_window_rect(&ctx, &windows[4]).unwrap();

    let resize = (5, Vertical, 1000);
    let win0_change = super::MIN_SECTION_SIZE as i16 - win0_rows as i16;
    let win1_change = super::MIN_SECTION_SIZE as i16 - win1_rows as i16;
    let win4_change = super::MIN_SECTION_SIZE as i16 - win4_rows as i16;
    let mut window_changes = VecMap::new();
    window_changes.insert(0, win0_change);
    window_changes.insert(1, win1_change);
    window_changes.insert(3, -win1_change - win4_change);
    window_changes.insert(4, win4_change);
    window_changes.insert(5, -win0_change - win1_change - win4_change);
    window_changes.insert(6, win4_change);
    window_changes.insert(7, win1_change);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let screen::Rect(_, screen::Size(_, win0_cols)) =
      frame.get_window_rect(&ctx, &windows[0]).unwrap();

    let resize = (2, Horizontal, 1000);
    let win0_change = super::MIN_SECTION_SIZE as i16 - win0_cols as i16;
    let mut window_changes = VecMap::new();
    window_changes.insert(0, win0_change);
    window_changes.insert(2, -win0_change);
    window_changes.insert(3, -win0_change);
    window_changes.insert(5, win0_change);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let resize = (4, Vertical, 50);
    let mut window_changes = VecMap::new();
    window_changes.insert(3, -50);
    window_changes.insert(4, 50);
    window_changes.insert(5, -50);
    window_changes.insert(6, 50);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let resize = (4, Vertical, -10);
    let mut window_changes = VecMap::new();
    window_changes.insert(1, 10);
    window_changes.insert(4, -10);
    window_changes.insert(6, -10);
    window_changes.insert(7, 10);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    frame.reset_layout();
    check_frame_invariants(&frame, &ctx);
  }

  #[test]
  fn resize_bloated_minimized_window() {
    let (mut frame, ctx, windows) = setup_frame(0);

    let screen::Rect(_, screen::Size(win3_rows, _)) =
      frame.get_window_rect(&ctx, &windows[3]).unwrap();
    let screen::Rect(_, screen::Size(win5_rows, _)) =
      frame.get_window_rect(&ctx, &windows[5]).unwrap();
    let screen::Rect(_, screen::Size(win6_rows, _)) =
      frame.get_window_rect(&ctx, &windows[6]).unwrap();
    let screen::Rect(_, screen::Size(win7_rows, _)) =
      frame.get_window_rect(&ctx, &windows[7]).unwrap();

    let win3_min_size = 3 * super::MIN_SECTION_SIZE + 2 * super::BORDER_SIZE;

    let win3_resize = win3_min_size as i16 - win3_rows as i16;
    let resize = (3, Vertical, win3_resize as int);
    let mut window_changes = VecMap::new();
    window_changes.insert(2, -win3_resize);
    window_changes.insert(3, win3_resize);
    window_changes.insert(4, win3_resize);
    window_changes.insert(5, super::MIN_SECTION_SIZE as i16 - win5_rows as i16);
    window_changes.insert(6, super::MIN_SECTION_SIZE as i16 - win6_rows as i16);
    window_changes.insert(7, super::MIN_SECTION_SIZE as i16 - win7_rows as i16);
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    let resize = (3, Vertical, -100);
    let window_changes = VecMap::new();
    do_window_resize(&mut frame, &ctx, &windows, &window_changes, resize);

    frame.reset_layout();
    check_frame_invariants(&frame, &ctx);
  }

  // Assert that all windows of the frame has the expected size after resizing
  // one window.
  fn do_window_resize(frame: &mut super::Frame, ctx: &super::FrameContext,
                      windows: &Vec<super::WindowId>,
                      window_changes: &VecMap<i16>,
                      (win, orientation, amount):
                        (uint, super::Orientation, int)) {
    // calculate and collect expected sizes after resizing a window
    let expectations: Vec<(super::WindowId, screen::Size)> =
      range(0, windows.len()).map(|win| {
        let window = windows[win].clone();
        let change = window_changes.get(&win).map(|&x| x).unwrap_or(0);
        let screen::Rect(_, screen::Size(rows, cols)) =
          frame.get_window_rect(ctx, &window).unwrap();
        (window, if orientation == Vertical {
          screen::Size((rows as i16 + change) as u16, cols)
        }
        else {
          screen::Size(rows, (cols as i16 + change) as u16)
        })
      }).collect();

    // perform window resize
    frame.resize_window(ctx, &windows[win], orientation, amount).unwrap();
    check_frame_invariants(frame, ctx);

    // compare resulting window sizes to expectations
    assert!(expectations.iter().map(|&(ref window, expected_size)| {
      let screen::Rect(_, size) = frame.get_window_rect(ctx, window).unwrap();
      size == expected_size
    }).all(|x| x));
  }

  #[test]
  fn get_closest_neighbouring_window_frame_0() {
    try_closest_neighbours(0,
      &[(0, 3), (1, 2), (2, 6), (3, 4), (4, 5), (5, 7), (6, 7), (7, 6)]);
  }

  #[test]
  fn get_closest_neighbouring_window_frame_1() {
    try_closest_neighbours(1,
      &[(0, 5), (1, 7), (2, 3), (3, 2), (4, 6), (5, 0), (6, 4), (7, 1)]);
  }

  #[test]
  fn get_closest_neighbouring_window_frame_2() {
    try_closest_neighbours(2,
      &[(0, 4), (1, 5), (2, 7), (3, 6), (4, 0), (5, 1), (6, 3), (7, 2)]);
  }

  #[test]
  fn get_no_closest_neighbour() {
    let (frame, ctx, main_window) = super::Frame::new();
    assert_eq!(Err(super::FrameError::NoNeighbouringWindow),
      frame.get_closest_neighbouring_window(&ctx, &main_window));
  }

  fn try_closest_neighbours(frame_num: uint,
                            closest_neighbours: &[(uint, uint)]) {
    let (frame, ctx, windows) = setup_frame(frame_num);
    for &(win, neighbour) in closest_neighbours.iter() {
      assert_eq!(windows[neighbour],
        frame.get_closest_neighbouring_window(&ctx, &windows[win]).unwrap());
    }
  }

  #[test]
  fn get_sequent_window_frame_0() {
    try_window_sequence(0, &[0, 3, 4, 5, 7, 6, 2, 1]);
  }

  #[test]
  fn get_sequent_window_frame_1() {
    try_window_sequence(1, &[0, 5, 2, 3, 1, 7, 4, 6]);
  }

  #[test]
  fn get_sequent_window_frame_2() {
    try_window_sequence(2, &[0, 4, 1, 5, 3, 6, 2, 7]);
  }

  fn try_window_sequence(frame_num: uint, sequence: &[uint]) {
    let (frame, ctx, windows) = setup_frame(frame_num);

    let first_window = windows[sequence[0]].clone();
    let last_window = windows[sequence[sequence.len() - 1]].clone();

    assert_eq!(Err(super::FrameError::NoSuchSequentWindow),
      frame.get_sequent_window(&ctx, &first_window, PreviousWindow, false));
    assert_eq!(Err(super::FrameError::NoSuchSequentWindow),
      frame.get_sequent_window(&ctx, &last_window, NextWindow, false));

    let order = NextWindow;
    let mut window = last_window;
    for &next_num in sequence.iter() {
      window = frame.get_sequent_window(&ctx, &window, order, true).unwrap();
      assert_eq!(window, windows[next_num]);
    }

    let order = PreviousWindow;
    let mut window = first_window;
    for &next_num in sequence.iter().rev() {
      window = frame.get_sequent_window(&ctx, &window, order, true).unwrap();
      assert_eq!(window, windows[next_num]);
    }
  }

  #[test]
  fn get_adjacent_window_frame_0() {
    let (frame, ctx, windows) = setup_frame(0);
    try_adjacent_windows(&frame, &ctx, &windows,
      &[(0, None, Some(1), None, Some(3)),
        (1, Some(0), None, None, None),
        (2, None, Some(1), Some(3), None),
        (3, None, Some(4), Some(0), Some(2)),
        (4, Some(3), Some(5), Some(0), Some(2)),
        (5, Some(4), Some(1), Some(0), Some(7)),
        (6, Some(4), Some(1), Some(7), Some(2)),
        (7, Some(4), Some(1), Some(5), Some(6))]);
  }

  #[test]
  fn get_adjacent_window_frame_1() {
    let (frame, ctx, windows) = setup_frame(1);
    try_adjacent_windows(&frame, &ctx, &windows,
      &[(0, None, Some(2), None, Some(5)),
        (1, None, Some(7), Some(5), Some(4)),
        (2, Some(0), None, None, Some(3)),
        (3, Some(5), None, Some(2), Some(7)),
        (4, None, Some(6), Some(1), None),
        (5, None, Some(3), Some(0), Some(1)),
        (6, Some(4), None, Some(7), None),
        (7, Some(1), None, Some(3), Some(6))]);
  }

  #[test]
  fn get_adjacent_window_frame_2() {
    let (frame, ctx, windows) = setup_frame(2);
    try_adjacent_windows(&frame, &ctx, &windows,
      &[(0, None, Some(4), None, Some(1)),
        (1, None, Some(5), Some(0), Some(3)),
        (2, None, Some(7), Some(3), None),
        (3, None, Some(6), Some(1), Some(2)),
        (4, Some(0), None, None, Some(5)),
        (5, Some(1), None, Some(4), Some(6)),
        (6, Some(3), None, Some(5), Some(7)),
        (7, Some(2), None, Some(6), None)]);
  }

  #[test]
  fn get_adjacent_window_border_inclusive() {
    let (mut frame, ctx, windows) = setup_frame(1);

    frame.resize_window(&ctx, &windows[0], Vertical, -1).unwrap();
    check_frame_invariants(&frame, &ctx);
    frame.resize_window(&ctx, &windows[0], Horizontal, 1).unwrap();
    check_frame_invariants(&frame, &ctx);

    try_adjacent_windows(&frame, &ctx, &windows,
      &[(5, None, Some(2), Some(0), Some(1)),
        (7, Some(1), None, Some(5), Some(6))]);
  }

  fn try_adjacent_windows(frame: &super::Frame, ctx: &super::FrameContext,
                          windows: &Vec<super::WindowId>,
                          adjacencies: &[(uint, Option<uint>, Option<uint>,
                                          Option<uint>, Option<uint>)]) {
    let err = super::FrameError::NoSuchAdjacentWindow;
    let expectation_as_frame_result = |opt: Option<uint>|
      opt.map(|adj| windows[adj].clone()).ok_or(err);
    for &(win, left, right, up, down) in adjacencies.iter() {
      assert_eq!(expectation_as_frame_result(left),
        frame.get_adjacent_window(ctx, &windows[win], super::Direction::Left));
      assert_eq!(expectation_as_frame_result(right),
        frame.get_adjacent_window(ctx, &windows[win], super::Direction::Right));
      assert_eq!(expectation_as_frame_result(up),
        frame.get_adjacent_window(ctx, &windows[win], super::Direction::Up));
      assert_eq!(expectation_as_frame_result(down),
        frame.get_adjacent_window(ctx, &windows[win], super::Direction::Down));
    }
  }
}
