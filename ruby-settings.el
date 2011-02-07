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

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(define-key ruby-mode-map "{" 'paredit-open-curly)
(define-key ruby-mode-map "}" 'paredit-close-curly-and-newline)
(define-key ruby-mode-map "\"" 'paredit-doublequote)

(define-key ruby-mode-map "[" 'paredit-open-square)
(define-key ruby-mode-map "]" 'paredit-close-square)
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

;; Rinari mode
(require 'rinari)
(setq rinari-tags-file-name "TAGS")
(setq rinari-major-modes
      (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
            'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; Cucumber feature
;; language if .feature doesn't have "# language: fi"
(setq feature-default-language "en")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(require 'feature-mode)
(yas/load-directory "~/.emacs.d/packages/feature-mode/snippets")

(provide 'ruby-settings)
