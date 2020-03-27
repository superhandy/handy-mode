# handy-mode

> Copyright (C) 2015-2019 David Capello

## What's handy-mode?

`handy-mode` is a progressive way to migrate your Emacs keyboard
bindings to a more friendly keyboard layout:
the [`handy` layout](https://github.com/superhandy/intro).

It contains several levels to make it easy to go from the original
Emacs shortcuts to the new ones. The first `handy` level will give you
the most power with the less effort, it's really recommended to start
in this level.

## How to start? Level 1 JLIK

You should start with the handy level one:

```elisp
(add-to-list 'load-path "~/handy-mode")
(require 'handy-mode)
(setq handy-level 1)
(handy-mode 1)
```

This will enable the `Alt+JLIK` keyboard shortcuts which can be
visualized as a set of arrow keys in a QWERTY keyboard where `J` is
the left arrow, `L` the right arrow, `I` the up arrow, and `K` the
down arrow, just like this graph:

          .-----.
          |  I  |
    .-----'-----'-----.
    |  J  |  K  |  L  |
    '-----------------'

In this level, `Alt+JLIK` will be associated to these commands:

* `M-j` : `backward-char`
* `M-l` : `forward-char`
* `M-i` : `previous-line`
* `M-k` : `next-line`

And Alt+Shift+JLIK to:

* `M-J` : `backward-sexp`
* `M-L` : `forward-sexp`
* `M-I` : `scroll-down-command`
* `M-K` : `scroll-up-command`

We offer some alternatives to the Emacs commands associated with these
overriden keybindings:

* ~~`M-j`~~ : `indent-new-comment-line`, use `ESC M-j` instead
* ~~`M-l`~~ : `downcase-word`, use `ESC M-l` instead
* ~~`M-i`~~ : `tab-to-tab-stop`, use `ESC M-i` instead
* ~~`M-k`~~ : `kill-sentence`, use `ESC M-k` instead

If you learn these new shortcuts, you will already be able to replace
a big chunk of ackward Emacs keyboard shortcuts. After you get used to
these keys, you can forgot the old keybindings:

* ~~`C-b`~~ : `backward-char` now is `M-j`
* ~~`C-f`~~ : `forward-char` now is `M-l`
* ~~`C-p`~~ : `previous-line` now is `M-i`
* ~~`C-n`~~ : `next-line` now is `M-k`
* ~~`C-M-b`~~ : `backward-sexp` now is `M-J`
* ~~`C-M-f`~~ : `forward-sexp` now is `M-L`
* ~~`M-v`~~ : `scroll-down-command` now is `M-I`
* ~~`C-v`~~ : `scroll-up-command` now is `M-K`

This mode is defined in [`handy-mode-jlik.el`](handy-mode-jlik.el) file.

## Continue with Level 2 NM

```elisp
(add-to-list 'load-path "~/handy-mode")
(require 'handy-mode)
(setq handy-level 2)
(handy-mode 1)
```

The level 1 ([handy-mode-m](handy-mode-m.el)) enables the Alt+M prefix
which opens a lot of new commands.

                .-----.
                |     |
          .-----'     '-----.
          |                 |
    .-----|-----------------'
    |  N  |  M  |
    '-----------'


This prefix will be used in next levels to replace overriden Emacs
shortcuts. You can learn the prefixed commands progressively, the most
important ones at this moment are:

* `M-n` : `handy-keyboard-quit` (like `C-g`)
* `M-m` : prefix
* `M-M` : `execute-extended-command`

* `M-m h` : `back-to-indentation`
* `M-m c` : `handy-change-case` (a way to replace `downcase-word`/`upcase-word`/`capitalize-word` with one command)

Overriden shortcuts:

* ~~`M-m`~~ : `back-to-indentation`, use `M-m h` instead
* ~~`ESC M-l`~~ : this one was introduced to replace the old ~~`M-l`~~ (`downcase-word`), now you can use `M-m c k`

Get used to the new `M-M` so you don't have to use `M-x` to execute
commands in the future (`execute-extended-command`).

This level is defined in [`handy-mode-nm.el`](handy-mode-nm.el) file.

## Level 3 UO

```elisp
(add-to-list 'load-path "~/handy-mode")
(require 'handy-mode)
(setq handy-level 3)
(handy-mode 1)
```

Enables Alt+UO keys to jump between words or paragraphs (or functions on progmodes):

          .-----.-----.-----.
          |  U  |     |  O  |
          |-----'     '-----|
          |                 |
    .-----|-----------------'
    |     |     |
    '-----------'

New ways to jump between words and paragraphs/functions:

* `M-u` : `backward-word`
* `M-o` : `forward-word`
* `M-U` : `handy-beginning-of-block` (`backward-paragraph`/`beginning-of-defun`)
* `M-O` : `handy-end-of-block` (`forward-paragraph`/`end-of-defun`)

Overridden keybindings:

* ~~`M-u`~~ : `upcase-word`, you can use `M-m c i` from `handy-mode-nm`
* ~~`M-o`~~ : `facemenu-set-face`, you can use `M-m M-O` from `handy-mode-nm`

You can forgot about:

* ~~`M-f`~~ : `backward-word` now is `M-u`
* ~~`M-b`~~ : `forward-word` now is `M-o`
* ~~`M-{`~~ : `backward-paragraph` now is `M-U` in textmodes
* ~~`M-}`~~ : `forward-paragraph` now is `M-O` in textmodes
* ~~`M-{`~~ : `beginning-of-defun` now is `M-U` in progmodes
* ~~`M-}`~~ : `end-of-defun` now is `M-O` in progmodes

This level is defined in [`handy-mode-uo.el`](handy-mode-uo.el) file.

## Level 4 ZB

                                        .-----.-----.-----.
                                        |     |     |     |
                                        |-----'     '-----|
                                        |                 |
    .-----------------------------.-----'-----------------'
    |  Z     X     C     V     B  |           |
    '-----------.                 '-----------|
                |             SPC             |
                '-----------------------------'

This level is defined in [`handy-mode-zb.el`](handy-mode-zb.el) file.

## Level 5 YH

                                  .-----.-----.-----.-----.
                                  |  Y  |     |     |     |
                                  |     |-----'     '-----|
                                  |  H  |                 |
    .-----------------------------|-----'-----------------'
    |                             |           |
    '-----------.                 '-----------|
                |                             |
                '-----------------------------'

This level is defined in [`handy-mode-yh.el`](handy-mode-yh.el) file.

## Level 6 WERD

          .-----------------.     .-----.-----.-----.-----.
          |  W     E     R  |     |     |     |     |     |
          '-----.           '-----|     |-----'     '-----|
                |  D     F     G  |     |                 |
    .-----------'-----------------|-----'-----------------'
    |                             |           |
    '-----------.                 '-----------|
                |                             |
                '-----------------------------'

This level is defined in [`handy-mode-werd.el`](handy-mode-werd.el) file.

## Level 7 Modal/RET

This level is defined in [`handy-mode-modal.el`](handy-mode-modal.el)
file.  Pressing `M-p` you can lock the `Alt` key so there is no need
to press `M-` modifier for keyboard shortcuts in all other modes.

This is still a work-in-progress, doesn't work in several modes inside
the minibuffer, etc.

## Enable all levels

This is recommended only when you already know about all the
`handy-mode` keyboard shortcuts:

```elisp
(add-to-list 'load-path "~/handy-mode")
(require 'handy-mode)
(handy-mode 1)
```

## License

Distributed under the [MIT license](LICENSE.txt).
