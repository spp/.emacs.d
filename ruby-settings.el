(require 'ruby-electric)
(ruby-electric-mode t)

;; Ruby mode settings
(add-hook 'ruby-mode-hook
	  (lambda()
	    (add-hook 'local-write-file-hooks
		      '(lambda()
			 (save-excursion
			   (untabify (point-min) (point-max))
			   (delete-trailing-whitespace))))
	    (set (make-local-variable 'indent-tabs-mode) 'nil)
	    (set (make-local-variable 'tab-width) 2)
	    (imenu-add-to-menubar "IMENU")
	    (define-key ruby-mode-map "\C-m" 'newline-and-indent)))

;; Install mode-compile to give friendlier compiling support!
(require 'mode-compile)
(require 'mode-compile-kill)
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; Ruby block mode
(require 'ruby-block)
(ruby-block-mode t)
;; display to minibuffer and do overlay
(setq ruby-block-highlight-toggle t)

;;; FIX ME
;;Rspec mode
;(require 'rspec-mode)
;(setq rspec-use-rvm t)

;;; FIX ME
;; Shoulda mode
;(require 'shoulda-mode)

;; rhtml mode
(require 'rhtml-mode)
; put rhtml templates into rhtml-mode
(setq auto-mode-alist  (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
; put any rjs scripts into ruby-mode, as they are basically ruby
(setq auto-mode-alist  (cons '("\\.rjs$" . ruby-mode) auto-mode-alist))

;; RVM
(require 'rvm)
(rvm-use-default)

;; Haml mode
(require 'haml-mode)
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

(provide 'ruby-settings)