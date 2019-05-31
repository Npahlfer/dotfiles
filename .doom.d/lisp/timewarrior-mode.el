;;; ~/.doom.d/lisp/timewarrior-mode.el -*- lexical-binding: t; -*-

;;; Timewarrior -- timewarrior minor mode
;;; Commentary:
;;; Interact with timewarrior
;;; Code:


; (defun get-current-tag-duration ()
;   "Get timewarrior tag duratiuon."
;   (shell-command "timew get dom.active.duration"))

(defun is-active ()
  "Get timewarrior active state."
  (equal (string-trim-right (shell-command-to-string "timew get dom.active")) "1"))

(defun stop-tracker ()
  "Get timewarrior tag duratiuon."
  (shell-command "timew stop"))

(defun get-active-tag ()
  "Gets active timewarrior tag."
  (string-trim-right (shell-command-to-string "timew get dom.active.tag.1")))

(defun set-active-tag (tag)
  "Set active timewarrior tag."
  (progn (shell-command (format "timew start %s :quiet" tag)))
    tag)

(defun get-current-workspace ()
  "Get current workspace name."
  (safe-persp-name (+workspace-current)))

(defun correct-tag (active-tag)
  "Set active tag to the workspace name if it's not the same."
  "Also check if the tracker is active otherwise activate it"
  (let ((workspace-tag (get-current-workspace)))
    (cond ((not (equal active-tag workspace-tag))
           (set-active-tag workspace-tag))
          ((not (is-active)) (set-active-tag active-tag)))
    workspace-tag))

(defun get-mode-text ()
  "Get minor mode text"
  (concat " TW: " (correct-tag (get-active-tag))))

(defun stop-timewarrior ()
     (stop-tracker))

(defun start-timewarrior ()
  (if (bound-and-true-p timewarrior-mode)
        (correct-tag (get-active-tag))))

(let ((workspace-tag (get-current-workspace)))
  (if (not (equal "" workspace-tag))
      (set-active-tag workspace-tag)))

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

(add-hook 'after-save-hook #'start-timewarrior)


(provide 'timewarrior)
;;; timewarrior ends here
