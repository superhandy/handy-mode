;; handy-mode
;; Copyright (C) 2019 David Capello

(require 'edmacro)

;; Default state is normal (where keystrokes insert chars)
(defvar handy-state 'normal)

(defun handy-switch-state ()
  (interactive)
  (if (eq handy-state 'normal)
      (progn
        ;; Enable Alt+locked mode
        (message "ALT-LOCKED HANDY-MODE")
        (setq handy-state 'locked)
        (handy-mode-locked-state 1))
    ;; Disable Alt+locked mode
    (message "normal handy-mode")
    (setq handy-state 'normal)
    (handy-mode-locked-state 0)))

(defvar handy-mode-modal-map
  (handy-make-keymap
   "M-RET" 'handy-switch-state))

(defvar handy-mode-locked-map
  ;; TODO remove the duplication of all keys, iterate all keymaps and
  ;;      create the map without the M- prefix here
  (handy-make-keymap
   ;; jlik

   "j" 'backward-char
   "l" 'forward-char
   "i" 'previous-line
   "k" 'next-line
   "J" 'backward-sexp
   "L" 'forward-sexp
   "I" 'scroll-down-command
   "K" 'scroll-up-command

   ;; nm

   "n" 'handy-keyboard-quit
   "M" 'execute-extended-command

   "m 1" 'delete-other-windows
   "m 2" 'split-window-vertically
   "m M-2" 'split-window-horizontally
   "m 3" 'handy-three-windows
   "m TAB" 'handy-next-window
   "m <backtab>" 'handy-previous-window

   "N" 'handy-new-file
   "m s" 'save-buffer
   "m d" 'ido-dired
   "m f" 'find-file
   "m g" 'goto-line

   "m M-s" 'save-some-buffers
   "m M-f" 'projectile-find-file
   "m M-g" 'grep-find

   "m l" 'kmacro-end-and-call-macro
   "m j" 'handy-switch-macro-editing

   "m i" 'bookmark-jump
   "m k" 'bookmark-set
   "m b" 'bookmark-bmenu-list

   "m M-i" 'jump-to-register
   "m M-k" 'point-to-register
   "m M-c" 'copy-to-register
   "m M-v" 'insert-register
   "m M-b" 'list-registers

   "m M-SPC" 'mark-paragraph
   "m a" 'sort-lines
   "m c" 'handy-change-case
   "m t" 'transpose-chars
   "m M-t" 'transpose-words
   "m M-T" 'transpose-sexps

   "m h" 'back-to-indentation
   "m M-l" 'handy-indent
   "m M-e" 'handy-eshell
   "m M-m" 'handy-compile
   "m M-u" 'previous-error
   "m M-o" 'next-error

   ;; uo

   "u" 'backward-word
   "o" 'forward-word
   "U" 'handy-beginning-of-block
   "O" 'handy-end-of-block

   ;; zb

   "z" 'handy-undo
   "Z" 'handy-redo

   "x" 'handy-cut
   "c" 'handy-copy
   "C" 'handy-copy-all
   "v" 'handy-paste
   "V" 'handy-paste-rotate

   "b" 'ido-switch-buffer
   "B" 'ibuffer

   "SPC" 'handy-set-mark
   "S-SPC" 'mark-sexp

   ;; yh

   "h" 'handy-beginning-of-line-and-buffer
   "H" 'handy-end-of-line-and-buffer
   "y" 'isearch-forward                 ; replaces yank
   "Y" 'isearch-backward                ; replaces yank

   ;; werd

   "w" 'handy-shrink-whitespace
   "e" 'backward-kill-word
   "r" 'kill-word
   "d" 'delete-backward-char
   "f" 'delete-char
   "g" 'kill-line
   "W" 'handy-close-file
   "D" 'backward-kill-sexp
   "F" 'kill-sexp
   "G" 'handy-backward-kill-line

   ))

(define-key handy-mode-locked-map [remap self-insert-command] 'ignore)

(define-minor-mode handy-mode-modal
  "Enables the modal handy-mode.

  M-m RET : `handy-switch-state'
"
  :global t
  :group 'handy-mode
  :keymap handy-mode-modal-map)

(define-minor-mode handy-mode-locked-state
  "It is the Alt+locked state of modal handy-mode."
  :global t
  :group 'handy-mode
  :lighter "+ALT"
  :keymap handy-mode-locked-map)
