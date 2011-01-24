;; Color theme
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-robin-hood)
;(load-file "~/.emacs.d/color-themes/color-theme-tango.el")
;(load-file "~/.emacs.d/color-themes/color-theme-blackboard.el")
;(load-file "~/.emacs.d/color-themes/zenburn.el")

;(zenburn)

;(color-theme-tango)
;(color-theme-blackboard)

;; I prefer tabs to be set at 4
(setq default-tab-width 4)

;; Use spaces for indentation (t)
(setq indent-tabs-mode nil)

;; Whitespace mode is still too annoying
; Make whitespace-mode use just basic coloring
;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; Make whitespace-mode use "¶" for newline and "⇥" for tab.
;; together with the rest of its defaults
;(setq whitespace-display-mappings
;  '((space-mark 32 [183] [46]) ; normal space
;		(space-mark 160 [164] [95])
;		(space-mark 2208 [2212] [95])
;		(space-mark 2336 [2340] [95])
;		(space-mark 3616 [3620] [95])
;		(space-mark 3872 [3876] [95])
;		(newline-mark 10 [182 10])
;		(tab-mark 9 [8677 9] [92 9])))

;; Remove the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; I don't like to type yes. y should suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; Vim style
(set-default 'indicate-empty-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Configure SLIME
;; (add-to-list 'load-path "~/.emacs.d/slime/")
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (require 'slime-autoloads)
;; (slime-setup '(slime-scratch slime-editing-commands slime-repl slime-fuzzy slime-autodoc slime-banner slime-editing-commands slime-asdf slime-presentations slime-tramp slime-references slime-xref-browser slime-highlight-edits))

;; Specify modes for Lisp file extensions so that we can turn on syntax highlighting for Lisp files with non-standard extensions
(setq auto-mode-alist
      (append '(("\.lisp$"   . lisp-mode)
				("\.lsp$"    . lisp-mode)
				("\.cl$"     . lisp-mode)
				("\.asd$"    . lisp-mode)
				("\.system$" . lisp-mode)) auto-mode-alist))

;; Redefine Enter to be new-line and then enter for lisp mode
(add-hook 'lisp-mode-hook
		  '(lambda ()
			 (define-key lisp-mode-map [?\C-m] 'newline-and-indent)
			 (define-key lisp-mode-map [?\C-j] 'newline)))

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
			(define-key ruby-mode-map "\C-m" 'newline-and-indent)
			(require 'ruby-electric)
			(ruby-electric-mode t)))

;; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/packages/rhtml")
(require 'rhtml-mode)
; put rhtml templates into rhtml-mode
(setq auto-mode-alist  (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
; put any rjs scripts into ruby-mode, as they are basically ruby
(setq auto-mode-alist  (cons '("\\.rjs$" . ruby-mode) auto-mode-alist))

;; Paredit
(add-to-list 'load-path "~/.emacs.d/paredit/")
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(mapc (lambda (mode)
		(let ((hook (intern (concat (symbol-name mode)
									"-mode-hook"))))
		  (add-hook hook (lambda ()
						   (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp slime slime-repl repl))

;; Load up Tramp
(require 'tramp)

;; Startup ido
(require 'ido)
(ido-mode t)

;; js2-mode
(add-to-list 'load-path "~/.emacs.d/packages/js2/")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;; Textmate mode
(add-to-list 'load-path "~/.emacs.d/textmate/")
(require 'textmate)
(textmate-mode)

;; For XML files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; Temporary files aren't put in the same directory
(setq make-backup-files nil)
(setq backup-directory-alist
			`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
			`((".*" ,temporary-file-directory t)))

;; Use cperl-mode instead of perl-mode
(mapc (lambda (pair)
		(if (eq (cdr pair) 'perl-mode)
			(setcdr pair 'cperl-mode)))
	  (append auto-mode-alist interpreter-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t)
 '(global-hl-line-mode 1)
 '(global-linum-mode 1)
 '(inhibit-startup-screen t)
 '(mouse-yank-at-point t)
 '(recentf-mode 1)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visible-bell t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#141312" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

;; Allow Emacs to switch to full-screen mode
(add-to-list 'load-path "~/.emacs.d/")
(require 'fullscreen)
;(fullscreen)

;; Start Maximized
;; (defun x-maximize-frame ()
;;   "Maximize the current frame (to full screen)"
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

;; (defun maximize-frame ()
;;   (x-maximize-frame))

;; (if window-system
;;     (add-hook 'window-setup-hook 'maximize-frame t))
