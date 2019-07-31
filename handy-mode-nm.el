;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defvar handy-mode-nm-map
  (handy-make-keymap

   "M-n" 'handy-keyboard-quit
   "M-M" 'execute-extended-command

   ;; Windows
   "M-m 1" 'delete-other-windows
   "M-m 2" 'split-window-vertically
   "M-m M-2" 'split-window-horizontally
   "M-m 3" 'handy-three-windows
   "M-m TAB" 'handy-next-window
   "M-m <backtab>" 'handy-previous-window

   ;; Files
   "M-N" 'handy-new-file
   "M-m s" 'save-buffer
   "M-m d" 'ido-dired
   "M-m f" 'find-file
   "M-m g" 'goto-line

   ;; Multiple-files
   "M-m M-s" 'save-some-buffers
   "M-m M-f" 'projectile-find-file
   "M-m M-g" 'grep-find

   ;; Macros

   "M-m m" 'handy-switch-macro-recording
   "M-m l" 'kmacro-end-and-call-macro
   "M-m j" 'handy-switch-macro-editing

   ;; Bookmarks
   "M-m i" 'bookmark-jump
   "M-m k" 'bookmark-set
   "M-m b" 'bookmark-bmenu-list

   ;; Registers
   "M-m M-i" 'jump-to-register
   "M-m M-k" 'point-to-register
   "M-m M-c" 'copy-to-register
   "M-m M-v" 'insert-register
   "M-m M-b" 'list-registers

   ;; Edit functions
   "M-m M-SPC" 'mark-paragraph
   "M-m a" 'sort-lines
   "M-m c" 'handy-change-case
   "M-m t" 'transpose-chars
   "M-m M-t" 'transpose-words
   "M-m M-T" 'transpose-sexps

   ;; M-m Programming functions
   "M-m h" 'back-to-indentation
   "M-m o" 'ff-get-other-file
   "M-m M-l" 'handy-indent
   "M-m M-e" 'handy-eshell
   "M-m M-m" 'handy-compile
   "M-m M-u" 'previous-error
   "M-m M-o" 'next-error
   ))

(define-minor-mode handy-mode-nm
  "Enables Alt+M key as a prefix and Alt+N to cancel commands.

                .-----.
                |     |
          .-----'     '-----.
          |                 |
    .-----'-----------------'
    |  N     M  |
    '-----------'

  M-n : `handy-keyboard-quit'
  M-m : prefix
  M-M : `execute-extended-command'

Files

  M-N   : `handy-new-file'
  M-m s : `save-buffer'
  M-m d : `ido-dired'
  M-m f : `find-file'
  M-m g : `goto-line'

Macros:

  M-m m : `handy-switch-macro-recording'
  M-m l : `kmacro-end-and-call-macro'
  M-m j : `handy-switch-macro-editing'

Bookmarks:

  M-m i : `bookmark-jump'
  M-m k : `bookmark-set'
  M-m b : `bookmark-bmenu-list'

Registers:

  M-m M-i : `jump-to-register'
  M-m M-k : `point-to-register'
  M-m M-c : `copy-to-register'
  M-m M-v : `insert-register'

Overridden keybindings:

  M-m : `back-to-indentation'    Use `M-m h' instead

"
  :global t
  :group 'handy-mode
  :keymap handy-mode-nm-map)
