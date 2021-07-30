(setq inhibit-startup-message t)

(column-number-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)	       
;; (set-visual-bell -1)

;; window switching via <S-Arrow>
(windmove-default-keybindings)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; manual theme installation path
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; monokai-pro install via use-package
(use-package
  monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro t))

;; (use-package
;;   jetbrains-darcula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'jetbrains-darcula t))

;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t t)
;; 	       (enable-theme 'ample))
;;   :defer t
;;   :ensure t)









