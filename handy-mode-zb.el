;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defvar handy-mode-zb-map
  (handy-make-keymap
   ;; Z
   "M-z" 'handy-undo
   "M-Z" 'handy-redo

   ;; XCV
   "M-x" 'handy-cut
   "M-c" 'handy-copy
   "M-C" 'handy-copy-all
   "M-v" 'handy-paste
   "M-V" 'handy-paste-rotate

   ;; B
   "M-b" 'ido-switch-buffer
   "M-B" 'ibuffer

   ;; SPC
   "M-SPC" 'handy-set-mark
   "M-S-SPC" 'mark-sexp

   ;; Alternatives to overridden keys
   "ESC M-z" 'zap-to-char
   "ESC M-c" 'capitalize-word
   "ESC M-SPC" 'just-one-space))

(define-minor-mode handy-mode-zb
  "Enables Alt+ZXCVB and Alt+Spacebar.

                                        .-----.-----.-----.
                                        |     |     |     |
                                        |-----'     '-----|
                                        |                 |
    .-----------------------------.-----'-----------------'
    |  Z     X     C     V     B  |           |
    '-----------.                 '-----------|
                |             SPC             |
                '-----------------------------'

Undo/Redo:

  <M-z>  `handy-undo'
  <M-Z>  `handy-redo'

Copy, Cut, and Paste (kill/yank):

  <M-x>  `handy-cut'
  <M-c>  `handy-copy'
  <M-v>  `handy-paste'

  <M-C>  `handy-copy-all'
  <M-V>  `handy-paste-rotate'

Buffers:

  <M-b>  `switch-to-buffer'
  <M-B>  `ibuffer'

Set mark:

  <M-SPC>    `handy-set-mark'
  <M-S-SPC>  `mark-sexp'

Overridden keybindings:

  <M-z>  `zap-to-char'
    Use `ESC M-z' instead

  <M-x>  `execute-extended-command'
    You should be using `M-M' from `handy-mode-nm' at this moment

  <M-c>  `capitalize-word'
    You can use <ESC M-c> or <M-m c j> from `handy-mode-nm'

  <M-v>  `scroll-down-command'
    You should be using `M-K' from `handy-mode-jlik' at this moment

  <M-b>  `backward-word'
    You should be using `M-u' from `handy-mode-uo' at this moment

  <M-SPC>  `just-one-space'
    You can use <ESC M-SPC>. This can be replaced then with
    `M-w' when `handy-mode-werd' is introduced.

"
  :global t
  :group 'handy-mode
  :keymap handy-mode-zb-map

  (if handy-mode-zb
      ;; Remove conflict with Alt+V from cua--cua-keys-keymap
      (if (and (symbolp 'cua-mode) cua-mode)
          (define-key cua--cua-keys-keymap [(meta v)] nil))
    ;; Restore Alt+V key in cua--cua-keys-keymap
    (define-key cua--cua-keys-keymap [(meta v)]
      'delete-selection-repeat-replace-region)))
