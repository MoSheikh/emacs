(package-initialize)
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (require 'use-package))
(require 'doom-themes)
(load-theme 'doom-tomorrow-night t)
