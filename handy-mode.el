;;; handy-mode.el --- handy-mode

;; Copyright (C) 2015-2019 David Capello
;;
;; Author: David Capello <davidcapello@gmail.com>
;; Maintainer: David Capello

(defgroup handy-mode nil
  "Customization group for handy-mode."
  :package-version '(handy-mode . "0.1")
  :group 'local
  :prefix 'handy-)

(load (concat (file-name-directory load-file-name) "handy-vars.el"))
(load (concat (file-name-directory load-file-name) "handy-funs.el"))

(load (concat (file-name-directory load-file-name) "handy-mode-jlik.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-nm.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-uo.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-zb.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-yh.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-werd.el"))
(load (concat (file-name-directory load-file-name) "handy-mode-modal.el"))

(defvar handy-level--installed -1
  "Internal variable used to store the current `handy-level' when
`handy-mode' is enabled.")

(defvar handy-mode-all-modes
  [handy-mode-jlik                      ; Level 1
   handy-mode-nm                        ; Level 2
   handy-mode-uo                        ; Level 3
   handy-mode-zb                        ; Level 4
   handy-mode-yh                        ; Level 5
   handy-mode-werd                      ; Level 6
   handy-mode-modal]                    ; Level 7
  "List of modes ordered by `handy-level' used by
`handy-mode' to know in which order and what modes
it should enable/disable for each level.")

(define-minor-mode handy-mode
  "Handy keyboard bindings.

You can change your current `handy-level' using `handy-switch-level'
command so you can start using handy-mode progressively. E.g. To start
in the first level:

  M-x handy-switch-level

And then choose level interactively pressing key 1, or you can
just execute the following expression:

  (handy-switch-level 1)

"
  :global t
  :version "0.1"
  :lighter " handy"
  :group 'handy-mode
  (let (i level)
    (if handy-mode
        (progn
          ;; installing handy-mode
          (message "handy-mode level %d enabled" handy-level)
          (setq i 1)
          (while (<= i handy-level)
            (funcall (aref handy-mode-all-modes (- i 1)) 1)
            (setq i (+ i 1)))
          (setq handy-level--installed handy-level))

      ;; removing handy-mode
      (message (format "handy-mode level %d disabled" handy-level--installed))
      (setq i handy-level--installed)
      (while (> i 0)
        (funcall (aref handy-mode-all-modes (- i 1)) 0)
        (setq i (- i 1)))
      (setq level handy-level--installed)
      (setq handy-level--installed -1))))

(provide 'handy-mode)
