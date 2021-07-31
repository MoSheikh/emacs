;; suppress ace-jump-mode warnings
(setq byte-compile-warnings '(cl-functions))
(setq inhibit-startup-message t)
(setq ispell-program-name "/usr/local/bin/aspell")

;; WIP
;; (global-set-key (kbd "C-v") 'View-scroll-half-page-up)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-down)

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
  :config
  (which-key-mode))


;; manual installation to workaround bug
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


(use-package ace-window
  :init
  (global-set-key (kbd "M-o") 'ace-window))

(minimap-mode)


;; non-working ace-jump-mode configuration
;;
;; (use-package ace-jump-mode
;;   :config
;;   (define-key global-map (kbd "M-SPC") 'ace-jump-mode))
;;
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; causes endless recursive call when using "load-theme"
;;
;; (use-package sublimity
;;   :config
;;   (sublimity-mode 1))

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (require 'sublimity-attractive)

;; (setq sublimity-scroll-weight 10
;;       sublimity-scroll-drift-length 5)
;; (sublimity-map-set-delay 0)


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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(custom-safe-themes
   '("24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" default))
 '(minimap-dedicated-window nil)
 '(minimap-display-semantic-overlays t)
 '(minimap-hide-fringes t)
 '(minimap-hide-scroll-bar nil)
 '(minimap-width-fraction 0.05)
 '(minimap-window-location 'right))

; LocalWords:  aspell monokai
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:extend t :background "#292A2B")))))
