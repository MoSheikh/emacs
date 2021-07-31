(setq inhibit-startup-message t)
(setq ispell-program-name "/usr/local/bin/aspell")

(flyspell-prog-mode 1))
(column-number-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)	       
;; (set-visual-bell -1)

;; window switching via <S-Arrow>
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

(use-package ace-jump-mode
  :config
  (define-key global-map (kbd "M-SPC") 'ace-jump-mode))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; themes
(use-package monokai-pro-theme
  :config
  (load-theme 'monokai-pro t))

(use-package jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

(use-package ample-theme
  :init (progn (load-theme 'ample t t)
	       (enable-theme 'ample)))






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window ace-jump-mode which-key use-package monokai-pro-theme jetbrains-darcula-theme ample-zen-theme ample-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; LocalWords:  aspell monokai
