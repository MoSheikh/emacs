;;; init.el

;;; Code:

;; disable byte-compile warnings for older packages
(setq byte-compile-warnings '(cl-functions))

;; initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(declare-function package-installed-p ())

;; use-package.el bootstrap
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-ensure t
	      use-package-always-defer t
	      use-package-verbose nil
	      use-package-expand-minimally t
	      use-package-enable-imenu-support t)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))

;; straight.el bootstrap
(declare-function straight-use-package ())
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package helm
  :straight t
  :config
  (setq helm-input-idle-delay                     0.01
        helm-reuse-last-window-split-state        t
        helm-always-two-windows                   t
        helm-split-window-inside-p                nil
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-dedicated-window      t
        helm-frame-background-color               "DarkSlateGray"
        helm-show-action-window-other-window      'left
        helm-allow-mouse                          t
        helm-move-to-line-cycle-in-source         t
        helm-autoresize-max-height                38 ; it is %.
        helm-autoresize-min-height                0  ; it is %.
        helm-debug-root-directory                 "~/.emacs.d/.tmp/helm-debug"
        helm-follow-mode-persistent               t
        helm-candidate-number-limit               500
        helm-visible-mark-prefix                  "✓")
  (use-package helm-ag)
  :init
  (helm-mode 1)
  (helm-autoresize-mode 1))

;; company.el
(use-package company
  :commands
  (global-company-mode
   company-idle-delay
   minimum-prefix-length
   company-dabbrev-other-buffers
   company-dabbrev-code-other-buffers)
  :init
  (global-company-mode)
  (setq company-idle-delay 0
	minimum-prefix-length 0
	company-dabbrev-other-buffers t
	company-dabbrev-code-other-buffers t)
  :hook
  ((text-mode . company-mode)
   (prog-mode . company-mode)))

;; helm-company.el
(use-package helm-company
  :bind
  (:map company-mode-map
	("C-:" . helm-company)
	("C-M-i" . company-complete)))

;; helm-xref.el
(use-package helm-xref)

;; lsp-mode.el
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  (typescript-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :defer t
  :commands lsp
  :config
  (advice-add 'json-parse-string :around
              (lambda (orig string &rest rest)
		(apply orig (s-replace "\\u0000" "" string)
                       rest)))
  (advice-add 'json-parse-buffer :around
              (lambda (oldfn &rest args)
		(save-excursion
                  (while (search-forward "\\u0000" nil t)
                    (replace-match "" nil t)))
		(apply oldfn args))))

;; lsp-ui.el
(use-package lsp-ui
  :commands lsp-ui-mode lsp-ui-mode-map
  :init
  (setq lsp-log-io nil
	lsp-restart 'auto-restart
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-doc-enable t
	lsp-ui-doc-position "right"
	lsp-ui-doc-delay 0
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-show-with-mouse t)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c i" . lsp-ui-imenu)))

;; helm-lsp.el
(use-package helm-lsp
  :bind
  (:map lsp-mode-map
	([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

;; flycheck.el
(use-package flycheck
  :init
  (global-flycheck-mode))

;; dap-mode.el
(use-package dap-mode
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1))

;; yasnippet.el
(use-package yasnippet
  :init
  (yas-global-mode 1)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets")))

(use-package yasnippet-snippets)

;; projectile.el
(use-package projectile
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :config
  (setq projectile-indexing-method 'alien)
  :init
  (helm-projectile-on))

;; doom-themes.el
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-tomorrow-night t)
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-org-config)
  (doom-themes-visual-bell-config))

;; magit.el
(use-package magit)

;; git-gutter.el
(use-package git-gutter
  :init
  (custom-set-variables
   `(git-gutter:update-interval 1))
  (global-git-gutter-mode +1))

;; which-key.el
(use-package which-key
  :init
  (which-key-mode))

;; ace-jump-mode.el
(use-package ace-jump-mode
  :bind
  ("C-," . ace-jump-mode))

;; ace-window.el
(use-package ace-window
  :bind
  ("M-o" . ace-window))

;; expand-region.el
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; multiple-cursors.el
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines))

;; golden-ratio.el
(defun pl-helm-alive-p ()
  "Prevent golden-ratio from interfering with helm."
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(use-package golden-ratio
  :init
  (golden-ratio-mode 1)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-inhibit-functions 'pl-helm-alive-p))

;; golden-ratio-scroll-screen.el
(defvar golden-ratio-extra-commands)
(use-package golden-ratio-scroll-screen
			:init
			(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
			(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;; telephone-line.el
(use-package telephone-line
  :init
  (setq telephone-line-lhs
	`((accent . (telephone-line-vc-segment))
	  (nil . (telephone-line-buffer-name-segment))))
  (telephone-line-mode 1))

;; all-the-icons.el
(use-package all-the-icons)

;; org
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

;; docker
(use-package dockerfile-mode
  :mode "\\Dockerfile\\..*")

;; dotenv
(use-package dotenv-mode
  :mode "\\.env\\..*")

;; markdown
(use-package markdown-mode
  :mode "\\.md$"
  :init (setq markdown-command "multimarkdown"))

;; json
(setq-default json-indent-level 2)

;; javascript
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default css-indent-level 2)

;; typescript
(use-package typescript-mode
  :mode "\\.tsx?$"
  :config
  (setq typescript-indent-level 2)
  :hook
  (typescript-mode . lsp-deferred))

;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml$")

;; styled-components
(use-package web-mode)

(use-package polymode
  :init
  (define-hostmode poly-typescript-hostmode
    :mode 'typescript-mode)
  (define-innermode poly-css-innermode
    :mode 'css-mode
    :head-matcher "\\(styled\\|css\\)[.{}()<>;:\n\s\t[:alnum:]]?+`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-web-innermode
    :mode 'web-mode
    :head-matcher "\\(styled\\|css\\)[.{}()<>;:\n\s\t[:alnum:]]?+`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-typescript-mode
    :innermodes '(poly-css-innermode
		  poly-web-innermode)))

;; prettier.el
(use-package prettier
  :hook
  ((typescript-mode json-mode) . prettier-mode))

;; jest
(use-package jest
     :after (typescript-mode)
     :hook (typescript-mode . jest-minor-mode))

;; string-inflection
(use-package string-inflection
  :bind
  ("C-c s" . string-inflection-snakecase)
  ("C-c c" . string-inflection-lower-camelcase))

;;; Ctl-x-5 map
;;
(define-key ctl-x-5-map (kbd "C-x c t") 'helm-top-in-frame)
(define-key ctl-x-5-map (kbd "C-x c i") 'helm-imenu-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-f") 'helm-find-files-in-frame)
(define-key ctl-x-5-map (kbd "M-x")     'helm-M-x-in-frame)
(define-key ctl-x-5-map (kbd "C-s")     'helm-occur-in-frame)
(define-key ctl-x-5-map (kbd "C-x C-b") 'helm-mini-in-frame)
(define-key ctl-x-5-map (kbd "M-g a")   'helm-do-grep-ag-in-frame)
(define-key ctl-x-5-map (kbd "M-g g")   'helm-do-git-grep-in-frame)


;;; Helm-command map
;;
(define-key helm-command-map (kbd "g") 'helm-apt)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "w") 'helm-w3m-bookmarks)
(define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)


;;; Global-map
;;
(global-set-key (kbd "C-M-c")                        'undefined)
(global-set-key (kbd "M-c")                          'undefined)
(global-set-key (kbd "C-z")                          'undo)
(global-set-key (kbd "C-x M-k")                      'kill-buffer-and-window)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x M-f")                      'helm-do-ag)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-h d")                        'helm-info-at-point)
(global-set-key (kbd "C-h i")                        'helm-info)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c C-i")                      'helm-imenu)
(global-set-key (kbd "<f11>")                        nil)
(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "M-s")                          nil)
(global-set-key (kbd "C-s")                          'helm-occur)
(global-set-key (kbd "M-s")                          'helm-occur-visible-buffers)
(global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g i")                 'helm-gid)
(define-key global-map (kbd "C-x r p")               'helm-projects-history)
(define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r")               'helm-dictionary)

;; customization
(global-display-line-numbers-mode)
(column-number-mode 1)
(scroll-bar-mode 1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode t)

(prefer-coding-system 'utf-8)

;; sessions
(defvar desktop-buffers-not-to-save)
(defvar desktop-path)
(defvar desktop-base-file-name)

(desktop-save-mode t)
(setq desktop-buffers-not-to-save "#*#")
(setq desktop-path '("~/.emacs.d"))
(setq desktop-base-file-name ".emacs-desktop")

;; backup
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup" t)))
(setq backup-by-copying t)

;; startup
(setq inhibit-startup-screen t)

;; fullscreen
(set-frame-parameter nil 'fullscreen 'fullboth)
(defun toggle-fullscreen()
  "Toggle fullscren mode for X11."
  (interactive)
  (when (eq window-system 'x)
    (cond ((not (eq (frame-parameter nil 'fullscreen) 'fullheight))
	   (progn
		  (set-frame-parameter nil 'fullscreen 'maximized)
		  (set-frame-parameter nil 'fullscreen 'fullheight)))
	  ((not (eq (frame-parameter nil 'fullscreen) 'fullboth))
	   (set-frame-parameter nil 'fullscreen 'fullboth)))))
(global-set-key (kbd "<f11> <f11>") 'toggle-fullscreen)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" default))
 '(git-gutter:update-interval 1)
 '(lsp-headerline-breadcrumb-icons-enable nil)
 '(telephone-line-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-background ((t (:foreground "gray30"))))
 '(ace-jump-face-foreground ((t (:foreground "coral" :underline nil))))
 '(aw-background-face ((t (:foreground "gray30"))))
 '(aw-leading-char-face ((t (:foreground "coral"))))
 '(helm-ff-file-extension ((t (:extend t :foreground "coral1"))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:inherit lsp-headerline-breadcrumb-path-face))))
 '(telephone-line-unimportant ((t (:inherit mode-line :background "gray10" :foreground "dim grey")))))



;;; init.el ends here
