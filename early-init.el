;;; early-init.el --- Mo's emacs init.el

;;; Commentary:
;;
;;  Second config file
;;
;;  TODO:
;;    - Python support
;;

;;; Code:

(setq byte-compile-warnings '(cl-functions))


(defvar gc-cons-threshold)
(defvar gc-cons-percentage)
(defvar comp-deferred-compilation)
(defvar native-comp-deferred-compilation)
;; temporarily increase garbage collection limits
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; delay runtime compilation for gccemacs
(setq comp-deferred-compilation nil
      native-comp-deferred-compilation nil)

;; prioritize non-btye-compiled source files
(setq load-prefer-newer noninteractive)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

(defun defer-garbage-collection-h ()
  "Unset garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(declare-function restore-garbage-collection-h ())
(defun restore-gatbage-collection-h ()
  "Reset garbage collection to reasonable value."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;;; early-init.el ends here
