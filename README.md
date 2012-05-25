# My emacs settings

An ever-changing set of emacs settings. Micro-optimizations are super fun.
These are used in the [Emacs Rocks](http://emacsrocks.com) screencasts.

## Setup

To grab all the dependencies, either:

    git clone git://github.com/magnars/.emacs.d.git
    cd .emacs.d
    git submodule init
    git submodule update

or on git v1.6.5 or later:

    git clone --recursive git://github.com/magnars/.emacs.d.git


## Magit

I don't keep magit in my .emacs.d. To install magit, do

    M-x package-list-packages

Search for `magit` and install it there.


## Install emacs on mac

I use Cocoa Emacs, installed like this:

    brew install emacs --cocoa --use-git-head --HEAD

To open it with Alfred or Quicksilver, you have to copy `Emacs.app` into
`/Applications` instead of the symlink that brew places there.

## Tips for using these emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * The key bindings are optimized for a norwegian keyboard layout.

 * Start by reading up on all the cool stuff in key-bindings.el.

 * You quit emacs with `C-x r q`, mnemonic *Really Quit*.

 * Find file in project with `C-x o`, in dir with `C-x C-f`, recent with `C-x f`

 * Add your user- and project-specific stuff in .emacs.d/users/[machine name]/*.el

 * `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

 * Autocomplete with `C-.`

 * expand-region is your friend. Find its bound key by doing `F1 f er/expand-region`

 * annoying-arrows suggests alternative ways of moving around if you use the
   arrow keys excessively.

 * Undo with `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * Easily add functions to the F-keys with `M-: (f5 <sexps here>)`, example `(f5 (flush-lines "console.log"))`

 * Quickly jump anywhere in the buffer by pressing `fn` plus the starting letter of a word.

 * Indent and clean up white space in the entire buffer with `C-c n`

 * On a mac, the Meta key `M` is bound to Command.

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

 * Watch [emacsrocks.com](http://emacsrocks.com)
