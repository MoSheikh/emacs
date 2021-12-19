;;; init.el --- Mo's emacs init.el

;;; Commentary:
;;
;;  Second config file
;;
;;  TODO:
;;    - Python support
;;

;;; Code:

;; disable byte-compile warnings for older packages
(setq byte-compile-warnings '(cl-functions))

;; initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(declare-function package-installed-p ())

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

;; use-package.el
(straight-use-package 'use-package)
(setq-default use-package-always-ensure t
	      use-package-always-defer t
	      use-package-verbose nil
	      use-package-expand-minimally t
	      use-package-enable-imenu-support t)


(use-package helm
  :straight t
  :config
  (require 'helm-config)
  (setq helm-input-idle-delay                     0.01
        helm-reuse-last-window-split-state        t
        helm-always-two-windows                   nil
        helm-split-window-inside-p                t
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts
						    helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows nil
        helm-use-frame-when-dedicated-window      t
        helm-frame-background-color               "#45534f"
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
(defvar company-idle-delay)
(defvar minimum-prefix-length)
(defvar company-dabbrev-other-buffers)
(defvar company-dabbrev-code-other-buffers)
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
	("C-:" . helm-company))
  (:map company-active-map
	("C-:" . helm-company)))

;; helm-xref.el
(use-package helm-xref)

;; lsp-mode.el
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  (typescript-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

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

;; helm-projectile.el
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

;;; org
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

;;; markdown
(use-package markdown-mode
  :mode "\\.md$"
  :init (setq markdown-command "multimarkdown"))

;;; json
(setq-default json-indent-level 2)

;;;  javascript
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default css-indent-level 2)

;; ;; typescript
;; (use-package typescript-mode
;;   :mode "\\.tsx?$"
;;   :config
;;   (setq typescript-indent-level 2)
;;   :hook
;;   (typescript-mode . lsp-deferred))

;; prettier.el
;; (use-package prettier
;;   :defer t
;;   :hook
;;   ((typescript-mode json-mode) . prettier-mode))

;; jest
(use-package jest
  :after (typescript-mode)
  :hook (typescript-mode . jest-minor-mode))

;; terraform-mode
(use-package terraform-mode
  :mode "\\.tf$"
  :hook
  (terraform-mode-hook . terraform-format-on-save-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(setq split-width-threshold 0)
(setq split-height-threshold nil)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts$"
  :init
  (define-derived-mode tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx$\\'" . tsx-mode)))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
	 (typescript-tsx-mode . tree-sitter-hl-mode)))

(defvar tree-sitter-major-mode-language-alist)
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))

(use-package web-mode
  :hook ((web-mode . lsp)
         (tsx-mode . lsp))
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.tera\\'" . web-mode)
         ("\\.tsx\\'" . tsx-mode))
  :init
  (define-derived-mode tsx-mode typescript-mode "tsx")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package prettier
  :defer t
  :hook ((tsx-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (js-mode . prettier-mode)
         (json-mode . prettier-mode)
         (css-mode . prettier-mode)
         (scss-mode . prettier-mode)))

;; ;; web-mode
;; (use-package web-mode
;;   :ensure t
;;   :mode (("\\.jsx?$" . web-mode)
;;          ("\\.tsx?$" . web-mode)
;;          ("\\.html\\'" . web-mode)
;;          ("\\.vue\\'" . web-mode)
;; 	 ("\\.json\\'" . web-mode))
;;   ;; :hook
;;   ;; (web-mode . typescript-mode)
;;   :commands web-mode
;;   :hook
;;   (web-mode-hook . (lambda ()
;; 		     (when (string-equal "tsx" (file-name-extension buffer-file-name))
;; 		       (typescript-mode))))
;;   :config
;;   (setq web-mode-content-types-alist
;; 	'(("(j|t)sx" . "\\.(j|t)s[x]?\\'"))))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t)
  (exec-path-from-shell-initialize))

(use-package exec-path-from-shell)

;; disable revert-buffer confirmation when no changes are made
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;; alternatively:
;; (setq revert-without-query ’(".*"))

(global-set-key (kbd "C-M-r") 'revert-buffer-no-confirm)

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
;;
(define-key helm-command-map (kbd "g")  'helm-apt)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "w") 'helm-w3m-bookmarks)
(define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)


;;; Global-map
;;
;;
(global-set-key (kbd "C-M-c")                        'undefined)
(global-set-key (kbd "M-c")                          'undefined)
(global-set-key (kbd "C-z")                          'undo)
(global-set-key (kbd "C-M-l")                        'recenter-top-bottom)
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
(electric-pair-mode 1)
(electric-quote-mode 1)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" default))
 '(git-gutter:update-interval 1)
 '(helm-always-two-windows nil)
 '(helm-commands-using-frame
   '(completion-at-point helm-apropos helm-eshell-prompts helm-imenu helm-imenu-in-all-buffers))
 '(helm-frame-background-color "#353535")
 '(helm-reuse-last-window-split-state t)
 '(helm-show-action-window-other-window 'left)
 '(helm-split-window-inside-p t)
 '(helm-use-frame-when-dedicated-window nil)
 '(lsp-clients-verilog-executable '("hdl_checker" "--lsp"))
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
