;;; ~/.doom.d/lisp/timewarrior-mode.el -*- lexical-binding: t; -*-

;;; Timewarrior -- timewarrior minor mode
;;; Commentary:
;;; Interact with timewarrior
;;; Author: Niclas Pahlfer
;;; Code:
;;;

(defvar *is-active* nil "Cached active state.")
(defvar *active-tag* nil "Cached active tag.")
(defvar *tw-is-active* t "Global tw active state")

(defun is-active ()
  "Get timewarrior active state."
  (equal (string-trim-right (shell-command-to-string "timew get dom.active")) "1"))

(defun stop-tracker ()
  "Get timewarrior tag duration."
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

(defun annotate-tag (text)
  "Annotates timewarrior tag."
  (progn
    (shell-command (format "timew annotate '%s'" text))
    text))

(defun get-current-workspace ()
  "Get current workspace name."
  (safe-persp-name (+workspace-current)))

(defun correct-tag (active-tag)
  "Set active tag to the workspace name and set state to active."
  (let ((workspace (get-current-workspace)))
    (if (and *is-active* *active-tag* (equal workspace *active-tag*))
        active-tag
      (set-active-tag workspace))))

(defun get-mode-text ()
  "Get minor mode text"
  (concat " TW: " (correct-tag (get-active-tag))))

(defun correct-timewarrior ()
  (if (and *tw-is-active* (bound-and-true-p timewarrior-mode))
      (progn
        (setq *is-active* t)
        (correct-tag (get-active-tag)))))

(defun stop-timewarrior ()
  (if (bound-and-true-p timewarrior-mode)
      (progn
        (setq *is-active* nil)
        (setq *active-tag* nil)
        (stop-tracker)
        (message "TW stopped tracking"))))

(defun start-timewarrior ()
  (progn
    (setq *tw-is-active* t)
    (correct-timewarrior)
    (message "TW tracking")))

(defun annotate-timewarrior (annotation)
  (progn
    (stop-timewarrior)
    (start-timewarrior)
    (annotate-tag annotation)))

(defun cancel-timewarrior ()
  (progn
    (setq *tw-is-active* nil)
    (message "TW auto tracking cancelled")))

;; Timewarrior Minor Mode
(define-minor-mode timewarrior-mode
  "Timewarrior time tracker"
  1
  (get-mode-text)
  `(
    (,(kbd "C-c C-1") . (lambda () "TW Start" (interactive) (start-timewarrior)))
    (,(kbd "C-c C-2") . (lambda () "TW Stop" (interactive) (stop-timewarrior)))
    (,(kbd "C-c C-3") . (lambda (text) "TW Annotate" (interactive "sEnter annotate: ") (annotate-timewarrior text)))
    (,(kbd "C-c C-4") . (lambda () "TW Cancel auto tracking" (interactive) (cancel-timewarrior)))
   )
   :global 1
)

;; Make this async to prevent the first save from being slow
;; & look for another hook
(add-hook 'after-save-hook #'correct-timewarrior)


(provide 'timewarrior)
;;; timewarrior ends here
