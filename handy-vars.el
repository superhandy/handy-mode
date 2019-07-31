;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defcustom handy-level 7
  "Handy level to be enabled when `handy-mode' is called.

  Level 1 includes Alt+JLIK movement (`handy-mode-jlik')
  Level 2 includes Alt+NM (`handy-mode-nm')
  Level 3 includes Alt+UO (`handy-mode-uo')
  Level 4 includes Alt+ZB (`handy-mode-zb')
  Level 5 includes Alt+YH (`handy-mode-yh')
  Level 6 includes Alt+WERD (`handy-mode-werd')
  Level 7 includes Alt+Enter modal mode (`handy-mode-modal')

Use `handy-switch-level' funtion to change the active level when
the `handy-mode' is enabled.
"
  :group 'handy-mode)

;; TODO Add new keyboard layouts
(defcustom handy-layout nil
  "Keyboard layout. Default value is nil which means that the
layout is a standard US QWERTY keyboard.
"
  :group 'handy-mode)

(defcustom handy-text-modes '(text-mode
                              markdown-mode)
  "List of major modes that are related to text modes (non-programming modes).

Used by `handy-is-text-mode' to known if
`handy-beginning-of-block'/`handy-end-of-block' should jump
between paragraphs (in text modes) or functions (in non-text
modes, i.e. progmodes).
"
  :group 'handy-mode)

(defvar handy-new-file-count 0
  "Counter to create new documents.")
