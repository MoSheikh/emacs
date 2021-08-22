;;; early-init.el

;; Temporarily increase garbage collection limits
;; (setq gc-cons-theshold most-positive-fixnum
;;       gc-cons-percentage 0.6)

;; Delay runtime compilation for gccemacs
;; (setq comp-deferred-compilation nil
;;       native-comp-deferred-compilation nil)

;; Prioritize non-btye-compiled source files
;; (setq load-prefer-newer noninteractive)

;; (add-hook 'emacs-startup-hook
;; 	  (lambda ()
;; 	    (setq gc-cons-threshold 16777216 ; 16mb
;; 		  gc-cons-percentage 0.1)))

;; (defun doom-defer-garbage-collection-h ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun doom-restore-gatbage-collection-h ()
;;   (run-at-time
;;    1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))

;; (add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
;; (add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
