;; handy-mode
;; Copyright (C) 2015-2019 David Capello

(defun handy-isearch-hook ()
  ;; Overriden:
  ;; M-n     isearch-ring-advance
  ;; M-p     isearch-ring-retreat

  (handy-define-keys
   isearch-mode-map
   "M-n" 'isearch-abort
   "M-y" 'isearch-repeat-forward
   "M-Y" 'isearch-repeat-backward
   "M-v" 'isearch-yank-kill
   "M-w" 'handy-isearch-yank-sexp))

(defvar handy-mode-yh-map
  (handy-make-keymap
   "M-h" 'handy-beginning-of-line-and-buffer
   "M-H" 'handy-end-of-line-and-buffer
   "M-y" 'isearch-forward               ; replaces yank
   "M-Y" 'isearch-backward              ; replaces yank
   ))

(define-minor-mode handy-mode-yh
  "Enable Alt+YH keys.

                                  .-----.-----.-----.-----.
                                  |  Y  |     |     |     |
                                  |     |-----'     '-----|
                                  |  H  |                 |
    .-----------------------------|-----'-----------------'
    |                             |           |
    '-----------.                 '-----------|
                |                             |
                '-----------------------------'

You can press M-h one time to go to the beginning of line, and a
second time to go to the beginning of buffer. The same with M-H
and end of line/buffer.

  M-y : `isearch-forward'
  M-h : `handy-beginning-of-line-and-buffer'

  M-Y : `isearch-backward'
  M-H : `handy-end-of-line-and-buffer'

Overridden keybindings:

  M-y : `yank-pop'        Use `M-Y' from `handy-mode-zb'
  M-h : `mark-paragraph'  Use `M-m SPC' from `handy-mode-nm'
"
  :global t
  :group 'handy-mode
  :keymap handy-mode-yh-map
  (if handy-mode-yh
      (add-hook 'isearch-mode-hook 'handy-isearch-hook)
    (remove-hook 'isearch-mode-hook 'handy-isearch-hook)))
