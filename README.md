# Rim
Rim is an aspiring Vim-like text editor written in Rust.

## Current state
Focus has been put on building solid foundations free of code tangling rather than reaching quick goals. Thus many buffer operations required for rim to be called a text editor are lacking but will be easily implemented at the appropriate time. All you can do for now is navigate a buffer in multiple split views. There's also support for insertion, although it is not yet "hooked up."

## Try it out

Simply clone and `cargo run`.

Note: All key bindings are for testing purposes only.
- `q` - Quit
- `Arrow keys` - Move caret
- `v/s` - Split focused window
- `c` - Close focused window
- `h/j/k/l` - Move window focus left/down/up/right
- `n/N` - Move window focus forward/backward in window order
- `(Ctrl)+y/u` - Resize focused window
- `=` - Reset window sizes

## What's next?
Keychaining probably. Maybe some of those buffer operations after that.

## Trouble shooting
Rustc updates are still a bit of a pain point. If things are not compiling, you're probably using a different version than I. I try to update at least once a week.
