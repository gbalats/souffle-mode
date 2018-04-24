# souffle-mode [![Build Status](https://secure.travis-ci.org/gbalats/souffle-mode.png)](http://travis-ci.org/gbalats/souffle-mode)


An Emacs mode for the Soufflé Datalog Language.

### Author
[George Balatsouras](mailto:gbalats@di.uoa.gr)

### License
MIT license (see `LICENSE`)


Manual Installation
-------------------

This project uses the [Cask][cask] project management tool for Emacs.


### Install `Cask`:

To install [Cask][cask] simply run:

    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

or:

    make setup


### Install `souffle-mode`:

Run the following to compile, create a package (tarball), and install
it to `emacs`:

    make
    make install

Finallly, add the following into your `~/.emacs`:

    (require 'souffle-mode)


Keyboard Shortcuts
------------------

### souffle-mode

Keyboard shortcut        | Description
-------------------------|-------------------------------
<kbd>M-a</kbd>           | Move backwards by one atom.
<kbd>M-e</kbd>           | Move forward by one atom.
<kbd>C-M-a</kbd>         | Move backwards by one clause.
<kbd>C-M-e</kbd>         | Move forward by one clause.
<kbd>C-:</kbd>           | Rename local variable (at clause scope).


[yasnippet]: https://github.com/capitaomorte/yasnippet/
[cask]: https://github.com/cask/cask
