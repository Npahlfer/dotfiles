;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Set theme
(load-theme 'doom-nord-light t)

;; Load private packages
(load! "lisp/timewarrior-mode")

;; Set which-key delay
(setq which-key-idle-delay 0)

;; Emacs window move keybindings
(windmove-default-keybindings 'meta)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Title
(setq-default frame-title-format '("%b [%m]"))

;; Font
(set-frame-font "Iosevka Term 14" nil t)

;; Relative line numbers
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)

(global-display-line-numbers-mode t)

(map! (:map override
       :i "C-j"           #'eval-print-last-sexp))

(map! :leader
      :desc "Kill buffer"                     "d" #'kill-this-buffer

      ;; (:prefix ("r" . "Run")
      ;;   "p" (exec! "go build")
      ;;   "P" (exec! "go run")
      ;;   "c" (exec! "go clean"))

      :desc "Search project"                  "/" #'+default/search-project
      (:prefix ("\\" . "search")
        :desc "Search buffer"                 "b" #'swiper
        :desc "Search current directory"      "d" #'+default/search-from-cwd
        :desc "Jump to symbol"                "i" #'imenu
        :desc "Jump to symbol across buffers" "I" #'imenu-anywhere
        :desc "Jump to link"                  "l" #'ace-link
        :desc "Look up online"                "o" #'+lookup/online-select
        :desc "Search project"                "p" #'+default/search-project)

      (:prefix-map ("f" . "file")
                   :desc "Find tag"           "t"   #'rgrep)

      (:prefix ("g" . "git")
        :desc "Git revert file"               "R"   #'vc-revert
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"             "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"              "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"            "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"           "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"       "["   #'git-gutter:previous-hunk)
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"              "/"   #'magit-dispatch
          :desc "Forge dispatch"              "'"   #'forge-dispatch
          :desc "Magit status"                "g"   #'magit-status
          :desc "Magit file delete"           "x"   #'magit-file-delete
          :desc "Magit blame"                 "B"   #'magit-blame-addition
          :desc "Magit commit"                "c"   #'magit-commit-create
          :desc "Magit fetch"                 "F"   #'magit-fetch
          :desc "Magit buffer log"            "L"   #'magit-log
          :desc "Git stage file"              "S"   #'magit-stage-file
          :desc "Git unstage file"            "U"   #'magit-unstage-file
          (:prefix ("m" . "make (pull, etc.)")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "R"   #'+magit/clone
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
          :desc "Display tab bar"             "."   #'+workspace/display
          :desc "Switch workspace"            "TAB" #'+workspace/switch-to)))


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

(when (require 'eslint nil 'noerror)
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

;; Org project notes
(defvar my/project-path "~/Dropbox/Documents/orgNotes/projects")

(defun my/pick-project ()
  "Prompt user to pick a choice from a list."
  (let ((choices (directory-files my/project-path)))
    (message "%s" (completing-read "Open bookmark:" choices ))))

(defun my/choose-note-name ()
  "Prompt user to choose a note name"
  (read-string "Choose the note name: "))


(defun my/create-note-name ()
  (let ((project-name (my/pick-project))
    (note-name (my/choose-note-name)))
    (concatenate 'string
         me/project-path
         "/"
         project-name
         "/"
         note-name
         ".org")))

(defun my/create-new-project-note ()
  (interactive)
  (let ((filename (my/create-note-name)))
    (find-file-other-window filename)
    (org-mode)))


(add-to-list 'company-backends #'company-tabnine)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(defun doom-dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'font-lock-comment-face) " ")
            (insert "\n"))
          '("                         E M A C S                           "
            "                             ♥                               "))
    (when (and (stringp +doom-dashboard-banner-file)
               (display-graphic-p)
               (file-exists-p! +doom-dashboard-banner-file +doom-dashboard-banner-dir))
      (let* ((image (create-image (expand-file-name +doom-dashboard-banner-file
                                                    +doom-dashboard-banner-dir)
                                  'png nil))
             (size (image-size image nil))
             (margin (+ 1 (/ (- +doom-dashboard--width (car size)) 2))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (when (> margin 0)
          (save-excursion
            (goto-char point)
            (insert (make-string (truncate margin) ? )))))
      (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) ?\n)))))

(after! helm
  (setq helm-default-display-buffer-functions '(+popup-display-buffer))
  (set-popup-rule! "^\\*helm" :ignore t))

(setq neo-window-fixed-size nil)
; (setq neo-window-width 40)

; Set the neo-window-width to the current width of the
; neotree window, to trick neotree into resetting the
; width back to the actual window width.
; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
 (eval-after-load "neotree"
                  '(add-to-list 'window-size-change-functions
                                (lambda (frame)
                                  (let ((neo-window (neo-global--get-window)))
                                    (unless (null neo-window)
                                      (setq neo-window-width (window-width neo-window)))))))

