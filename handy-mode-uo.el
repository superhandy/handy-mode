;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defun handy-ido-minibuffer-hook ()
  (defvar handy-ido-keymap
    (handy-make-keymap
     "M-u" 'ido-prev-match
     "M-o" 'ido-next-match))
  (add-to-list 'minor-mode-overriding-map-alist
               (cons 'handy-mode handy-ido-keymap)))

(defvar handy-mode-uo-map
  (handy-make-keymap
   "M-u" 'backward-word
   "M-o" 'forward-word
   "M-U" 'handy-beginning-of-block
   "M-O" 'handy-end-of-block

   "ESC M-u" 'upcase-word
   "ESC M-o" 'facemenu-set-face
   ))

(define-minor-mode handy-mode-uo
  "Enable words movement with U and O keys.

          .-----.-----.-----.
          |  U  |     |  O  |
          |-----'     '-----|
          |                 |
    .-----'-----------------'
    |           |
    '-----------'

  M-u : `backward-word'
  M-o : `forward-word'

  M-U : `handy-beginning-of-block'
  M-O : `handy-end-of-block'

Overridden keybindings:

  M-u : `upcase-word'         You can use `ESC M-u' instead
  M-o : `facemenu-set-face'   You can use `ESC M-o' instead
"
  :global t
  :group 'handy-mode
  :keymap handy-mode-uo-map
  (if handy-mode-uo
      (add-hook 'ido-minibuffer-setup-hook 'handy-ido-minibuffer-hook)
    (remove-hook 'ido-minibuffer-setup-hook 'handy-ido-minibuffer-hook)))
