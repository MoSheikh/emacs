;; suppress ace-jump-mode warnings
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-message t)
(setq ispell-program-name "/usr/local/bin/aspell")
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

;; (add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; (add-to-list 'default-frame-alist '(fullscreen . fullheight))

(if (eq system-type 'darwin)
    (setq mac-right-command-modifier 'meta)
    (setq ns-use-native-fullscreen t))

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

(require 'package)
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

(add-to-list 'load-path "/Users/mo/.emacs.d/plugins/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "M-SPC") 'ace-jump-mode)

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

(setq telephone-line-lhs
      '((accent . (telephone-line-vc-segment))))

;; cleaner bottom info ribbon
(use-package telephone-line
  :init (telephone-line-mode 1))

(use-package moom)
(with-eval-after-load "moom"
  (define-key moom-mode-map (kbd "<f2>") 'moom-cycle-frame-height)
  (setq moom-use-font-module nil)
  (moom-mode 1))

(moom-fill-left)

;; (with-eval-after-load "moom"
;;   (setq moom-use-font-module nil))

;; (moom-toggle-font-module)

(use-package helm :config (require 'helm-config))
(use-package helm-searcher)
(global-set-key (kbd "M-x") 'helm-M-x)

(use-package json-mode
  :mode "\\.json$"
  :config
  (add-to-list 'flycheck-disabled-checkers 'json-python-json))

;; js
(setq-default js-indent-level 2)

;; prettier
(use-package prettier
  :hook
  ((typescript-mode json-mode) . prettier-mode))

(use-package typescript-mode
  :mode "\\.tsx?$"
  :hook
  (typescript-mode . lsp))

(setq typescript-indent-level 2)
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
   '("24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" default))
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays t)
 '(minimap-hide-fringes t)
 '(minimap-hide-scroll-bar nil)
 '(minimap-width-fraction 0.05)
 '(minimap-window-location 'right)
 '(package-selected-packages
   '(typescript-mode prettier json-mode helm-searcher ivy helm moom golden-ratio magit telephone-line golden-ratio-scroll-screen golden-ratoi-scroll-screen which-key use-package sublimity powerline monokai-pro-theme minimap jetbrains-darcula-theme ample-zen-theme ample-theme ace-window)))

; LocalWords:  aspell monokai
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(golden-ratio-scroll-highlight-line-face ((t (:extend t :background "dark gray" :foreground "white" :weight bold))))
 '(minimap-active-region-background ((t (:extend t :background "#292A2B")))))
