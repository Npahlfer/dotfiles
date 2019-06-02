;;; ~/.doom.d/lisp/timewarrior-mode.el -*- lexical-binding: t; -*-

;;; Timewarrior -- timewarrior minor mode
;;; Commentary:
;;; Interact with timewarrior
;;; Code:


;; (defun get-current-tag-duration ()
;;   "Get timewarrior tag duratiuon."
;;   (substring
;;    (car
;;     (split-string
;;      (shell-command-to-string "timew get dom.active.duration")
;;      "M")
;;     )
;;    2))

(defvar *is-active* nil "Cached active state.")
(defvar *active-tag* nil "Cached active tag.")

(defun is-active ()
  "Get timewarrior active state."
  (equal (string-trim-right (shell-command-to-string "timew get dom.active")) "1"))

(defun stop-tracker ()
  "Get timewarrior tag duratiuon."
  (shell-command "timew stop :quiet >> /dev/null"))

(defun get-active-tag ()
  "Gets active timewarrior tag."
  (if *active-tag*
      *active-tag*
    (string-trim-right (shell-command-to-string "timew get dom.active.tag.1"))))

(defun set-active-tag (tag)
  "Set active timewarrior tag."
  (progn
    (shell-command (format "timew start %s :quiet >> /dev/null" tag))
    (setq *active-tag* tag)
    tag))

(defun get-current-workspace ()
  "Get current workspace name."
  (safe-persp-name (+workspace-current)))

(defun correct-tag (active-tag)
  "Set active tag to the workspace name and set state to active."
  (if (and *is-active* *active-tag*)
      active-tag
      (set-active-tag (get-current-workspace))))

(defun get-mode-text ()
  "Get minor mode text"
  (concat " TW: " (correct-tag (get-active-tag))))

(defun stop-timewarrior ()
  (progn
    (setq *is-active* nil)
    (stop-tracker)))

(defun start-timewarrior ()
  (if (bound-and-true-p timewarrior-mode)
      (progn
        (setq *is-active* t)
        (correct-tag (get-active-tag)))))

;; Timewarrior Minor Mode
(define-minor-mode timewarrior-mode
  "Timewarrior time tracker"
  1
  ; :lighter (:eval (get-mode-text))
  (get-mode-text)
  `(
    (,(kbd "C-c C-1") . (lambda () (interactive) (start-timewarrior)))
    (,(kbd "C-c C-2") . (lambda () (interactive) (stop-timewarrior)))
   )
   :global 1
)

;; Make this async to prevent slow saves.
;; And look for another hook
(add-hook 'after-save-hook #'start-timewarrior)


(provide 'timewarrior)
;;; timewarrior ends here
