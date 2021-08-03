;;; init.el --- Mo's Initialization file for Emacs

;;; Commentary:
;;
;;
;; WIP configuration for Emacs
;;
;;

;;; Code:

(global-set-key (kbd "M-p") 'yank)
(global-set-key (kbd "C-c q") 'kill-buffer-and-window)


(defvar desktop-buffers-not-to-save)
(defvar desktop-path)
(defvar desktop-base-file-name)
;; persist sessions
(desktop-save-mode t)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "\\.log\\|#*#"
	      "\\)$"))

(setq desktop-path '("~/.emacs.d"))
(setq desktop-base-file-name ".emacs.desktop")

;; suppress ace-jump-mode warnings
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-message t)
(defvar ispell-program-name "/usr/local/bin/aspell")

;; configure backup to ~/.emacs.d/saves
(setq backup-directory-alist '((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/saves" t)))
(setq backup-by-copying t)

;; (add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; (add-to-list 'default-frame-alist '(fullscreen . fullheight))

(if (eq system-type 'darwin)
    (setq mac-right-command-modifier 'meta)
    (setq ns-use-native-fullscreen t))


;; (defun create-global-gitignore ()
;;   "Save autosave ignore data in the '~/.config/git/ignore' location."
;;   (progn
;;     (make-directory "~/.config/git" :parents)
;;     (write-region
;;      "# ignore .emacs autosaves\n#*#\n"
;;      nil
;;      "~/.config/git/ignore"
;;      :excl)))

;; (if (not (file-exists-p "~/.config/git/ignore"))
;;     (progn (create-global-gitignore)
;; 	   (message "Created global gitignore")))

;; WIP
;; (global-set-key (kbd "C-v") 'View-scroll-half-page-up)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-down)

(global-display-line-numbers-mode)
(flyspell-prog-mode)
(column-number-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)
;; (set-visual-bell -1)

;; window switching via <S-Arrow> - replaced by ace-jump-window
;; (windmove-default-keybindings)

(defvar flycheck-emacs-lisp-load-path)
(setq flycheck-emacs-lisp-load-path 'inherit)
(add-to-list 'load-path "~/.emacs.d/plugins/")
(require 'package)
(load "~/.emacs.d/plugins/config.el")
(load "~/.emacs.d/plugins/css-in-js.el")
(require 'css-in-js)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; manual theme installation path
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq-default use-package-always-ensure t
	      use-package-always-defer t
	      use-package-verbose nil
	      use-pacakge-expand-minimally t
	      use-package-enable-imenu-support t)

;; utilities
(use-package which-key
  :init
  (which-key-mode))


;; manual installation to work around bug
;; enables quick navigation using via searching

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-,") 'ace-jump-mode)

(declare-function ace-jump-mode-enable-mark-sync 'ext:ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; git interface
(use-package magit)

;; allow quicker switching between windows
(use-package ace-window
  :init
  (global-set-key (kbd "M-o") 'ace-window))

;; allow for .618 * width scroll + highlight after scroll
(use-package golden-ratio-scroll-screen
  :init
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;; use 0.618 proportion for new windows
(use-package golden-ratio
  :init (golden-ratio-mode 1))

(defvar golden-ratio-extra-commands)
(add-to-list 'golden-ratio-extra-commands 'ace-window)

(defun pl-helm-alive-p ()
  "Prevent golden-ratio from interfering with helm."
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl-helm-alive-p)

(defvar telephone-line-lhs
  '((accent . (telephone-line-vc-segment))))

;; cleaner bottom info ribbon
(use-package telephone-line
  :init (telephone-line-mode 1))

;; (use-package moom)
;; (with-eval-after-load "moom"
;;   (define-key moom-mode-map (kbd "<f2>") 'moom-cycle-frame-height)
;;   (setq moom-use-font-module nil)
;;   (moom-mode 1))

;; (moom-fill-left)

;; (with-eval-after-load "moom"
;;   (setq moom-use-font-module nil))

;; (moom-toggle-font-module)

(use-package helm :config (require 'helm-config) :commands helm-autoresize-mode)

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-projectile)

(defvar helm-autoresize-mode)
(helm-mode 1)
(helm-autoresize-mode t)

(use-package helm-ag)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x M-f") (lambda ()
				  (interactive)
				  (let ((current-prefix-arg '(4)))
				    (call-interactively #'helm-find))))

(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-z") 'helm-persistent-action)

;; (global-set-key (kbd "C-c C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-b") 'helm-mini)

(define-prefix-command 'custom)
(global-set-key (kbd "C-c C-c") 'custom)
(global-set-key (kbd "C-c C-c t") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x F") 'helm-projectile-find-file)
(global-set-key (kbd "C-x G") 'helm-do-grep-ag)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c 1")
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (python-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
	lsp-ui-doc-delay 2)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

(use-package python
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable nil)
  (cond
   ((executable-find "ipython")
   (progn
     (setq python-shell-buffer-name "IPython")
     (setq python-shell-interpreter "ipython")
     (setq python-shell-interpreter-args "-i --simple-prompt")))
  ((executable-find "python3")
   (setq python-shell-interpreter "python3"))
  ((executable-find "python2")
   (setq python-shell-interpreter "python2"))
  (t
   (setq python-shell-interpreter "python"))))

(defvar pipenv-projectile-after-switch-function)
(defvar pipenv-projectile-after-switch-extended)
(declare-function pipenv-projectile-after-switch-extended ())
(autoload 'pipenv-projectile-after-switch-extended "pipenv")
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)
   :commands pipenv-projectile-after-switch-extended)

(use-package python-pytest)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package flycheck :init (global-flycheck-mode))

(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-headerline-breadcrumb-enable nil)

;; dap for debugging
(use-package dap-mode :commands dap-register-debug-template)
(require 'dap-chrome)
(require 'dap-python)
(add-hook 'dap-stopped-hook
	  (lambda (arg) (call-interactively #'dap-hydra)))
(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)
(dap-ui-controls-mode 1)


;; (defvar dap-chrome-url)
;; (setq dap-chrome-url "http://localhost:3002")
(dap-register-debug-template "Chrome"
			     (list :type "chrome"
				   :cwd nil
				   :mode "url"
				   :request "launch"
				   :webRoot "~/dev/unit21/lumos"
				   :url "http://localhost:3002"))


(use-package web-mode)

(use-package jest
  :after (js2-mode)
  :hook (js2-mode . jest-minor-mode))

(use-package json-mode
  :mode "\\.json$"
  :config
  (add-to-list 'flycheck-disabled-checkers 'json-python-json))

;; js
(setq-default js-indent-level 2)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; prettier
(use-package prettier
  :hook
  ((typescript-mode json-mode) . prettier-mode))

;; ts
(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . lsp))

(setq typescript-indent-level 2)


(use-package mmm-mode)
(use-package scss-mode)
(autoload
  'css-in-js
  "css-in-js"
  "Add syntax highlighting for css snippets of js files"
  t)

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package docker)
(use-package dockerfile-mode)

(defvar company-idle-delay)
(defvar minimum-prefix-length)
(defvar company-dabbrev-other-buffers)
(defvar company-dabbrev-code-other-buffers)
(declare-function global-company-mode ())
(use-package company
  :init
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq minimum-prefix-length 0)
  (setq company-dabbrev-other-buffers t
	company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
	 (prog-mode . company-mode)))

(use-package jedi)

(use-package treemacs
  :init
  (setq treemacs-width 40))

(use-package all-the-icons)
(use-package treemacs-magit)
(use-package treemacs-icons-dired)
(use-package treemacs-all-the-icons)

(defvar doom-themes-enable-bold)
(defvar doom-themes-enable-italic)
(defvar doom-themes-treemacs-theme)
(declare-function doom-themes-visual-bell-config ())
(declare-function doom-themes-treemacs-config ())
(declare-function doom-themes-org-config ())
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-spacegrey t)
  (doom-themes-visual-bell-config)t
  (setq doom-themes-treemacs-theme "doom-1337")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


(defvar centaur-tabs-style)
(use-package centaur-tabs
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "bar")
  (global-set-key (kbd "C-<") 'centaur-tabs-backward)
  (global-set-key (kbd "C->") 'centaur-tabs-forward))

;; (use-package company-jedi
;;   :hook ('python-mode-hook . 'jedi:setup)
;;   :init
;;   (add-to-list 'company-backends 'company-jedi)
;;   (setq jedi:complete-on-dot t))

(use-package elpy :init (elpy-enable))


;; (use-package rjsx-mode
;;   :mode "\\.tsx?$")

;; (use-package ivy)

;; (ivy-mode)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; (global-set-key "\C-s" 'swiper)

;; minimap currently disabled
;; (minimap-mode)

;; non-working ace-jump-mode configuration

;; (use-package ace-jump-mode
;;   :config
;;   (define-key global-map (kbd "M-SPC") 'ace-jump-mode));

;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; themes
(use-package monokai-pro-theme
  :init
  (load-theme 'monokai-pro t))

(use-package jetbrains-darcula-theme
  :init
  (load-theme 'jetbrains-darcula t))

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
	       (enable-theme 'ample)))

(load-theme 'monokai-pro t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"] t)
 '(custom-safe-themes
   '("1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" default))
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays t)
 '(minimap-hide-fringes t)
 '(minimap-hide-scroll-bar nil)
 '(minimap-width-fraction 0.05)
 '(minimap-window-location 'right)
 '(package-selected-packages
   '(helm-ag doom-themes inferior-python-mode python-pytest cl pytest centaur-tabs elpy helm-projectile company-jedi jedi company pipenv dockerfile-mode docker gitignore-mode gitconfig-mode gitattributes-mode gitattributes-modes projectile git-modes css-in-js rjsx-mode exec-path-from-shell dap-mode helm-lsp lsp-ui lsp-mode typescript-mode prettier json-mode helm-searcher ivy helm moom golden-ratio magit telephone-line golden-ratio-scroll-screen golden-ratoi-scroll-screen which-key use-package sublimity powerline monokai-pro-theme minimap jetbrains-darcula-theme ample-zen-theme ample-theme ace-window)))

; LocalWords:  aspell monokai
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "#fc9867"))))
 '(company-preview-common ((t (:background "#403e41" :foreground "#bdbdb3"))))
 '(company-tooltip ((t (:inherit nil :background "gray14" :foreground "dark gray"))))
 '(company-tooltip-common ((t (:background "gray35" :foreground "gray90" :weight bold))))
 '(company-tooltip-common-selection ((t (:background "#fc9867" :foreground "gray13"))))
 '(golden-ratio-scroll-highlight-line-face ((t (:extend t :background "dark gray" :foreground "white" :weight bold))))
 '(highlight ((t (:background "#353236" :foreground "#727072"))))
 '(lsp-face-highlight-read ((t nil)))
 '(minimap-active-region-background ((t (:extend t :background "#292A2B")))))



;;; init.el ends here
