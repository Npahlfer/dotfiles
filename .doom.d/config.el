;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Set theme
; (load-theme 'apropospriate-light t)
;; (load-theme 'doom-solarized-light t)
;; (load-theme 'doom-one-light t)
(load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
;; (load-theme 'doom-nord t)
;; (load-theme 'doom-nord-light t)
;; (load-theme 'doom-outrun-electric t)
;; (load-theme 'doom-peacock t)

;; Load private packages
(load! "lisp/timewarrior-mode")

;; Set which-key variables
(setq which-key-idle-delay 0.0
      which-key-idle-secondary-delay 0.0
      which-key-use-C-h-commands nil
      which-key-side-window-max-height 1.0)

(defun my-which-key-delay (prefix length)
  (unless (or (string-match-p "^\\(SPC\\|M-SPC\\|C-c\\)" prefix)
              (> length 1))
    1.0))

(add-hook 'which-key-delay-functions 'my-which-key-delay)

(setq *win-width* 100)
(setq *win-height* 50)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (setq *win-width* (round (* (x-display-pixel-width) 0.3)))
        (setq *win-height* (round (- (* (x-display-pixel-height) 0.5) 60)))
        (add-to-list 'default-frame-alist `(width . ,(round (/ *win-width* (frame-char-width)))))
        (add-to-list 'default-frame-alist `(height . ,(round (/ *win-height* (frame-char-height)))))

        ;; (setq initial-frame-alist
        ;;       `((left . ,(round (- (/ (x-display-pixel-width) 2) (/ *win-width* 2))))
        ;;         (top . ,(round (- (/ (x-display-pixel-height) 2) (/ *win-height* 2))))))
        *win-height*
        *win-width*
        )))

(add-hook 'after-init-hook #'set-frame-size-according-to-resolution)

;; Emacs window move keybindings
;; (windmove-default-keybindings 'meta)

(evil-set-initial-state 'term-mode 'emacs)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-evil-matchit-mode 1)

;; Title
(setq-default frame-title-format '("%b [%m]"))

;; Font
(set-frame-font "Iosevka Term 14" nil t)

;; Relative line numbers
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)

; (global-display-line-numbers-mode t)

;; Disable evil in nav-mode
;; (add-to-list 'evil-emacs-state-modes 'nav-mode)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun my-insert-dirname ()
  (interactive)
  (insert (file-name-nondirectory
   (directory-file-name
    (file-name-directory buffer-file-name)))))

(global-set-key "\C-c\C-d" 'my-insert-dirname)

(defun my-insert-filename (filename &optional args)
  (interactive "*fInsert file name: \nP")
  (let ((insert-name (file-relative-name (file-name-sans-extension filename))))
    (if (member insert-name
         '("index" "styles" "sagas" "actions" "reducer" "selectors" "constants" ))
        (insert (my-insert-dirname))
      (insert insert-name))))

(global-set-key "\C-ci" 'my-insert-filename)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))

(setq projectile-project-search-path '("~/Projects/" ))

(map! (:map override
            :i "C-j"                          #'eval-print-last-sexp
            :i [M-right]                      #'windmove-right
            :i [M-left]                       #'windmove-left))

(map! :leader
      :desc "Eval defun"                      "e" #'eval-defun
      :desc "Kill buffer"                     "d" #'kill-this-buffer
      :desc "Reopen buffer"                   "1" #'reopen-killed-file

      (:when (featurep! :term term)
             :desc "Terminal in popup"        "2" #'+term/toggle
             :desc "Terminal"                 "3" #'+term/here)

      (:prefix ("r" . "Run")
        :desc "Yarn build"                    "r" (lambda ()
                                                    ;; TODO Look for yarn.lock or package-json.lock
                                                    ;; And run `yarn build` or `npm run build`
                                                     (let ((default-directory (locate-dominating-file "./" "package.json")))
                                                       (exec! "yarn build")
                                                       )))

      ; :desc "Find file in project"            "SPC" #'fzf
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
          :desc "Git stage modified"          "a"   #'magit-stage-modified
          :desc "Git stage file"              "S"   #'magit-stage-file
          :desc "Git unstage file"            "U"   #'magit-unstage-file
          (:prefix ("m" . "make (pull, etc.)")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "R"   #'+magit/clone
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      (:prefix-map ("p" . "project") 
                   :desc "Which key abort"    "ESC" #'which-key-abort)

      (:when (featurep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
          :desc "Display tab bar"             ","   #'+workspace/display
          :desc "Switch workspace"            "TAB" #'+workspace/switch-to
          :desc "Which key abort"             "ESC" #'which-key-abort))

      :desc "Which key abort"                 "ESC" #'which-key-abort)


;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(flycheck-php)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

(defun has-project-prettier-config ()
  (or
   (equal (locate-dominating-file default-directory ".prettierrc") "~/")
   (equal (locate-dominating-file default-directory ".prettier.config.js") "~/")))

(when (require 'eslint nil 'noerror)
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(when (require 'eslint nil 'noerror)
  (eval-after-load 'rjsx-mode
    '(add-hook 'rjsx-mode-hook
               (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(when (require 'eslint nil 'noerror)
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook
               (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

;; (when (require 'eslint nil 'noerror)
;;   (eval-after-load 'js2-mode
;;     '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;;   (eval-after-load 'rjsx-mode
;;     '(add-hook 'rjsx-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;;   (eval-after-load 'js-mode
;;     '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

(add-hook! 'js2-mode-hook
  (unless
      (has-project-prettier-config)
    (format-all-mode -1)))

(add-hook! 'js-mode-hook
  (unless
      (has-project-prettier-config)
    (format-all-mode -1)))

(add-hook! 'rjsx-mode-hook
  (unless
      (has-project-prettier-config)
    (format-all-mode -1)))

;; Org project notes
;; (defvar my/project-path "~/Dropbox/Documents/orgNotes/projects")

;; (defun my/pick-project ()
;;   "Prompt user to pick a choice from a list."
;;   (let ((choices (directory-files my/project-path)))
;;     (message "%s" (completing-read "Open bookmark:" choices ))))

;; (defun my/choose-note-name ()
;;   "Prompt user to choose a note name"
;;   (read-string "Choose the note name: "))


;; (defun my/create-note-name ()
;;   (let ((project-name (my/pick-project))
;;     (note-name (my/choose-note-name)))
;;     (concatenate 'string
;;          me/project-path
;;          "/"
;;          project-name
;;          "/"
;;          note-name
;;          ".org")))

;; (defun my/create-new-project-note ()
;;   (interactive)
;;   (let ((filename (my/create-note-name)))
;;     (find-file-other-window filename)
;;     (org-mode)))


;; if company-tabnine stops working, try kill-local-variable RET company-backends

(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

(defun add-company-tabnine ()
  (add-to-list (make-local-variable 'company-backends) 'company-tabnine))

(use-package! company-tabnine
  :config
  (setq company-idle-delay 0
        company-show-numbers t)
  (add-hook! (web-mode-local-vars
              php-mode-local-vars
              js2-mode-local-vars
              rjsx-mode-local-vars
              emacs-lisp-mode-local-vars)
    #'add-company-tabnine))

; (use-package company-tabnine :ensure t)
;;
;; (setq company-backends '(company-tabnine company-capf))

;; (after! company
;;   (add-to-list 'company-backends 'company-tabnine))

;; (setq company-backends '(company-tabnine company-capf))

;; (defun add-company-tabnine ()
;;   (add-to-list (make-local-variable 'company-backends) 'company-tabnine))

;;
;; (after! company
;;   (setq company-idle-delay 0
;;         company-show-numbers t))

;; Trigger completion immediately.
;; (setq company-idle-delay 0.2)

;; Number the candidates (use M-1, M-2 etc to select completions).
;; (setq company-show-numbers t)

;; (use-package! company-tabnine
;;   ;; company-mode completion
;;   :commands company-tabnine
;;   :config (push 'company-tabnine company-backends))

;; (use-package! company-tabnine
;;   :config
  ;; (add-hook! (web-mode
  ;;             rjsx-mode
  ;;             js2-mode
  ;;             lua-mode
  ;;             php-mode
  ;;             emacs-lisp-mode
  ;;             python-mode)
  ;;   (lambda ()
  ;;   (setq company-backends '(company-tabnine))))

;; (require 'company-tabnine)
;; (use-package! company-tabnine
;;   :after company
;;   ; :ensure t
;;   :config
;;   ;; (add-to-list 'company-backends 'company-tabnine))
;;   (add-to-list 'company-backends #'company-tabnine))
;;   ; (cl-pushnew 'company-tabnine (default-value 'company-backends)))

; (use-package company-tabnine :ensure t)
;; (add-to-list 'company-backends #'company-tabnine)

;; (after! php-mode
;;   (set-company-backend! 'php-mode '(company-tabnine :with +php-company-backend)))

;; (after! web-mode
;;   (set-company-backend! 'web-mode #'company-tabnine))

(after! js2-mode
  (set-company-backend! 'js2-mode '(company-tabnine :with company-tide)))

(after! rjsx-mode
  (set-company-backend! 'rjsx-mode '(company-tabnine :with company-tide)))

;; (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))

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
            (insert "\n")) '("                           ♥                           "))
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

(after! helm-rg
  (setq
   helm-ag-base-command "rg --no-heading"))


(setq neo-window-fixed-size nil)
(setq neo-window-width 50)

; Set the neo-window-width to the current width of the
; neotree window, to trick neotree into resetting the
; width back to the actual window width.
; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
 ; (eval-after-load "neotree"
 ;                  '(add-to-list 'window-size-change-functions
 ;                                (lambda (frame)
 ;                                  (let ((neo-window (neo-global--get-window)))
 ;                                    (progn
 ;                                      (setq neo-window-width 50)
 ;                                    (unless (null neo-window)
 ;                                      (setq neo-window-width (window-width neo-window))))))))

(defun neotree-resize-window (&rest _args)
    "Resize neotree window.
    https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (add-hook 'neo-change-root-hook #'neotree-resize-window)
  (add-hook 'neo-enter-hook #'neotree-resize-window)

(setq max-specpdl-size 500
      max-lisp-eval-depth 300)

(setq undo-limit 40000
      undo-outer-limit 8000000
      undo-strong-limit 100000)

;; (add-hook 'go-mode-hook 'lsp-deferred)
;; (setq gofmt-command "goimports")
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/golang.org/x/lint/misc/emacs/"))
;; (require 'golint)


;; (add-to-list 'load-path "~/.doom.d/lisp/spotify.el")
;; (require 'spotify)

;; Settings
;; (setq spotify-oauth2-client-secret (getenv "SPOTIFY_SECRET"))
;; (setq spotify-oauth2-client-id (getenv "SPOTIFY_ID"))

;; (setq spotify-transport 'connect)
;; (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

;; (setq spotify-player-status-format "[%a - %t %r%s]")
;; (setq spotify-player-status-not-repeating-text "")
;; (setq spotify-player-status-not-shuffling-text "")
