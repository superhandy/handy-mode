;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defvar handy-mode-werd-map
  (handy-make-keymap
   "M-w" 'handy-shrink-whitespace
   "M-e" 'backward-kill-word
   "M-r" 'kill-word
   "M-d" 'delete-backward-char
   "M-f" 'delete-char
   "M-g" 'kill-line
   "M-W" 'handy-close-file
   "M-D" 'backward-kill-sexp
   "M-F" 'kill-sexp
   "M-G" 'handy-backward-kill-line

   ;; Overridden keybindings
   "ESC M-e" 'forward-sentence
   "ESC M-r" 'move-to-window-line-top-bottom
   ))

(define-minor-mode handy-mode-werd
  "Enable WERDFG keys.

          .-----------------.     .-----.-----.-----.-----.
          |  W     E     R  |     |     |     |     |     |
          '-----.           '-----|     |-----'     '-----|
                |  D     F     G  |     |                 |
    .-----------'-----------------|-----'-----------------'
    |                             |           |
    '-----------.                 '-----------|
                |                             |
                '-----------------------------'

  M-w : `handy-shrink-whitespace'
  M-e : `backward-kill-word'
  M-r : `kill-word'
  M-d : `delete-backward-char'
  M-f : `delete-char'
  M-g : `kill-char'

  M-W : `handy-close-file'
  M-D : `backward-kill-sexp'
  M-F : `kill-sexp'
  M-G : `handy-backward-kill-line'

Overridden keybindings:

  M-w : `kill-ring-save'
        You should be using `M-c' from `handy-mode-zb' at this moment

  M-e : `forward-sentence'
        Use `ESC M-e' instead

  M-r : `move-to-window-line-top-bottom'
         Use `ESC M-r' instead

  M-d : `kill-word'
        Use `M-r' instead

  M-f : `forward-word'
        You should be using `M-o' from `handy-mode-jlik' at this moment
"
  :global t
  :group 'handy-mode
  :keymap handy-mode-werd-map)
