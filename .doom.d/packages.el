;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
;;
(package! evil-matchit)
(package! editorconfig)
(package! eslint-fix)
(package! helm-rg)
(package! ripgrep)
(package! projectile-ripgrep)
(package! rg)
(package! php-cs-fixer)
(package! exec-path-from-shell)

(when (featurep! :completion company)
  (package! company-tabnine))
