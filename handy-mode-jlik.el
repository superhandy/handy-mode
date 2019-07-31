;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defun handy-minibuffer-hook ()
  (defvar handy-minibuffer-keymap
    (handy-make-keymap
     "M-i" 'previous-history-element
     "M-k" 'next-history-element))

  (add-to-list 'minor-mode-overriding-map-alist
               (cons 'handy-mode handy-minibuffer-keymap)))

(defvar handy-mode-jlik-map
  (handy-make-keymap
   "M-j" 'backward-char
   "M-l" 'forward-char
   "M-i" 'previous-line
   "M-k" 'next-line
   "M-J" 'backward-sexp
   "M-L" 'forward-sexp
   "M-I" 'scroll-down-command
   "M-K" 'scroll-up-command

   ;; Alternatives to overridden keys
   "ESC M-j" 'indent-new-comment-line
   "ESC M-l" 'downcase-word
   "ESC M-i" 'tab-to-tab-stop
   "ESC M-k" 'kill-sentence))

(define-minor-mode handy-mode-jlik
  "Enable char/line movement with Alt+JLIK keys.

          .-----.
          |  I  |
    .-----'     '-----.
    |  J     K     L  |
    '-----------------'

  <M-j>  `backward-char'
  <M-l>  `forward-char'
  <M-i>  `previous-line'
  <M-k>  `next-line'

  <M-J>  `backward-sexp'
  <M-L>  `forward-sexp'
  <M-I>  `scroll-down-command'
  <M-K>  `scroll-up-command'

Overridden keybindings:

  <M-j>  `indent-new-comment-line'   Use <ESC M-j> instead
  <M-l>  `downcase-word'             Use <ESC M-l> instead
  <M-i>  `tab-to-tab-stop'           Use <ESC M-i> instead
  <M-k>  `kill-sentence'             Use <ESC M-k> instead
"
  :global t
  :group 'handy-mode
  :keymap handy-mode-jlik-map

  (if handy-mode-jlik
      (add-hook 'minibuffer-setup-hook 'handy-minibuffer-hook)
    (remove-hook 'minibuffer-setup-hook 'handy-minibuffer-hook)))
