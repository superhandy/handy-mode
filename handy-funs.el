;; handy-mode
;; Copyright (C) 2015-2019 David Capello

;; ----------------------------------------
;; General commands

(defun handy-keyboard-quit ()
  "Cancels the current minibuf or command."
  (interactive)
  (if isearch-mode
      (isearch-abort))
  (keyboard-escape-quit))

;; ----------------------------------------
;; File handling

(defun handy-new-file ()
  (interactive)
  (setq handy-new-file-count (+ handy-new-file-count 1))
  (switch-to-buffer (get-buffer-create (format "Untitled%d" handy-new-file-count))))

(defun handy-close-file ()
  (interactive)
  (kill-buffer))

;; ----------------------------------------
;; Undo/redo

(defun handy-undo ()
  (interactive)
  (if (bound-and-true-p undo-tree-mode)
      (undo-tree-undo)
    (undo)))

(defun handy-redo ()
  (interactive)
  (if (bound-and-true-p undo-tree-mode)
      (undo-tree-redo)))

;; ----------------------------------------
;; Paragraph/function movement

(defun handy-is-text-mode ()
  "Returns true if the current major-mode is a text
mode (is inside the `handy-text-modes' list).
"
  (interactive)
  (if (memq major-mode handy-text-modes) t nil))

(defun handy-beginning-of-block ()
  "In text modes (when `handy-is-text-mode' returns true), it
calls `backward-paragraph'. In programming modes it calls
`beginning-of-defun'.
"
  (interactive)
  (if (handy-is-text-mode)
      (backward-paragraph)
    (beginning-of-defun)))

(defun handy-end-of-block ()
"In text modes (when `handy-is-text-mode' returns true), it
calls `forward-paragraph'. In programming modes it calls
`end-of-defun'.
"
  (interactive)
  (if (handy-is-text-mode)
      (forward-paragraph)
    (end-of-defun)))

;; ----------------------------------------
;; Line/buffer movement

(defun handy-beginning-of-line-and-buffer ()
  (interactive)
  (if (eq last-command this-command)
      (beginning-of-buffer)
    (beginning-of-line)))

(defun handy-end-of-line-and-buffer ()
  (interactive)
  (if (eq last-command this-command)
      (end-of-buffer)
    (end-of-line)))

;; ----------------------------------------
;; Macros

(defun handy-switch-macro-recording ()
  (interactive)
  (if (not defining-kbd-macro)
      (kmacro-start-macro 0)
    (kmacro-end-macro 1)))

(defun handy-switch-macro-editing ()
  (interactive)
  (if (eq major-mode 'edmacro-mode)
      (edmacro-finish-edit)
    (kmacro-edit-macro)))

;; ----------------------------------------
;; Editing commands

(defun handy-backward-kill-line (arg)
  "Kill from the current position to the beginning of line."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun handy-shrink-whitespace ()
  "Replaces three Emacs functions in just one:
- Calls `delete-blank-lines' when used in a blank line
- Calls `just-one-space' when used in a sequence of whitespaces
- Calls `delete-horizontal-space' when used in one whitespace
"
  (interactive)
  (skip-chars-forward " \t")
  (let ((endcol (current-column))
        (iseol (eolp)))
    (skip-chars-backward " \t")
    (let ((isbol (bolp)))
      (if (and iseol isbol)
          (delete-blank-lines)
        (if (> (- endcol (current-column)) 1)
            (just-one-space)
          (delete-horizontal-space))))))

(defun handy-indent ()
  (interactive)
  (if (or (eq major-mode 'list-mode)
          (eq major-mode 'emacs-lisp-mode))
      (indent-pp-sexp)
    (if (or (eq major-mode 'c++-mode))
        (c-indent-exp)
      (prog-indent-sexp))))

(defun handy-change-case ()
  "Show options to change the current word, sexp or selection."
  (interactive)
  (let* ((oldbuf (current-buffer))
         (oldwnd (selected-window))
         (oldpos (window-point))
         (bounds (bounds-of-thing-at-point 'sexp))
         (lower (downcase (buffer-substring (car bounds) (cdr bounds))))
         (upper (upcase (buffer-substring (car bounds) (cdr bounds))))
         (capit (capitalize (buffer-substring (car bounds) (cdr bounds))))
         (wnd (select-window (split-window (frame-root-window) -1 'below)))
         (buf (get-buffer-create "*Handy Help*")))

    (switch-to-buffer buf)
    (setq mode-line-format nil)
    (erase-buffer)
    (insert "i: " upper "\nj: " capit "\nk: " lower)
    (fit-window-to-buffer nil nil 1)
    (unwind-protect
        (let ((key (read-key-sequence "? ")))
          (switch-to-buffer oldbuf)
          (let ((result (cond
                         ((equal key (handy-kbd "k")) lower)
                         ((equal key (handy-kbd "i")) upper)
                         ((equal key (handy-kbd "j")) capit)
                         (t nil))))
            (if (not result)
                (message "(Quit)")
              (message result)
              (delete-region (car bounds) (cdr bounds))
              (insert result)
              (set-window-point oldwnd oldpos))))
      (delete-window wnd))))

(defun handy-set-mark (arg)
  (interactive "P")
  (if (and (symbolp 'cua-mode) cua-mode)
      (cua-set-mark arg)
    (set-mark-command arg)))

(defun handy-cut (arg)
  (interactive "P")
  (if (and (symbolp 'cua-mode) cua-mode)
      (cua-cut-region arg)
    (kill-region (mark) (point))))

(defun handy-copy (arg)
  (interactive "P")
  (if (and (symbolp 'cua-mode) cua-mode)
      (cua-copy-region arg))
    (kill-ring-save (mark) (point)))

(defun handy-copy-all ()
  (interactive)
  (kill-ring-save 0 (eobp)))

;; The 'delete-selection property is needed by
;; delete-selection-pre-hook to delete the selection when handy-paste
;; is called.
(put 'handy-paste 'delete-selection 'yank)
(defun handy-paste (arg)
  (interactive "P")
  (if (and (symbolp 'cua-mode) cua-mode)
      (cua-paste arg)
    (yank)))

(defun handy-paste-rotate (arg)
  (interactive "P")
  (if (and (symbolp 'cua-mode) cua-mode)
      (cua-paste-pop arg)
    (yank-pop)))

;; ----------------------------------------
;; Keyboard commands

(defun handy-convert-from-qwerty-kbd (str)
  (if handy-layout
      (handy-layout str)
    str))

(defun handy-kbd (str)
  "Like `kbd' function but STR is specified in QWERTY keyboard
layout and it's preprocessed to the final keyboard layout
specified in `handy-layout' variable."
  (kbd (handy-convert-from-qwerty-kbd str)))

(defun handy-define-keys (map &rest args)
  (let ((x args))
    (while x
      (define-key map (handy-kbd (car x)) (cadr x))
      (setq x (cddr x)))
    map))

(defun handy-make-keymap (&rest args)
  (apply 'handy-define-keys (make-keymap) args))

;; ----------------------------------------
;; isearch helpers

(defun handy-isearch-yank-sexp ()
  "Pull next sexp from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-sexp 1) (point))))

;; ----------------------------------------
;; Windows

(defun handy-next-window ()
  (interactive)
  (other-window 1))

(defun handy-previous-window ()
  (interactive)
  (other-window -1))

(defun handy-three-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (split-window-vertically)
  (other-window -1))

;; ----------------------------------------
;; eshell

(defun handy-focus-eshell ()
  (let ((list (window-list))
        (found nil))
    ;; First we try to find the *eshell* window
    (while (and list (equal nil found))
      (if (equal (buffer-name (window-buffer (car list))) "*eshell*")
          (let ()
            (setq found (car list))))
      (setq list (cdr list)))
    (when found
      (select-window found)
      t))
  nil)

(defun handy-eshell (&optional ARG)
  (interactive "P")
  (or (handy-focus-eshell) ; Focus *eshell* window
      (eshell ARG)))

;; ----------------------------------------
;; handy-compile

(defun handy-compile ()
  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (compile command))

;; ----------------------------------------
;; Switch Level/Modes

(defun handy-switch-level (level)
  "Switch to another `handy-mode' level (`handy-level') enabling/disabling sub-modes."
  (interactive "P")
  (let* ((oldbuf (current-buffer))
         (oldwnd (selected-window))
         (oldpos (window-point))
         (wnd (select-window (split-window (frame-root-window) -1 'below)))
         (buf (get-buffer-create "*Handy Help*")))

    (switch-to-buffer buf)
    (setq mode-line-format nil)
    (erase-buffer)
    (insert "1: includes Alt+JLIK   (`handy-mode-jlik')
2: includes Alt+NM     (`handy-mode-nm')
3: includes Alt+UP     (`handy-mode-uo')
4: includes Alt+ZB     (`handy-mode-zb')
5: includes Alt+YH     (`handy-mode-yh')
6: includes Alt+WERD   (`handy-mode-werd')
7: includes Alt+Enter  (`handy-mode-modal')")
    (fit-window-to-buffer nil nil 7)
    (unwind-protect
        (let ((key (read-key-sequence "? ")))
          (switch-to-buffer oldbuf)
          (let ((result (cond
                         ((equal key (handy-kbd "1")) 1)
                         ((equal key (handy-kbd "2")) 2)
                         ((equal key (handy-kbd "3")) 3)
                         ((equal key (handy-kbd "4")) 4)
                         ((equal key (handy-kbd "5")) 5)
                         ((equal key (handy-kbd "6")) 6)
                         ((equal key (handy-kbd "7")) 7)
                         (t nil))))
            (if (not result)
                (message "(Quit)")
              (progn
                (handy-mode 0)
                (setq handy-level result)
                (handy-mode 1)
                (set-window-point oldwnd oldpos)))))
      (delete-window wnd))))
