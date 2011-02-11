;; When started as a GUI app on Mac OS X, Emacs doesn't pick up environment variables
(add-to-list 'exec-path (getenv "PATH"))
;; Mac Hack
(push "/usr/local/bin" exec-path)

;; All the packages should be placed under the packages folder. Add them all.
(mapc (lambda (dir)
	(add-to-list 'load-path dir))
      (directory-files "~/.emacs.d/packages/" 'full))

;; Color theme
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/zenburn.el")
(zenburn)

;; I prefer tabs to be set at 4
(setq tab-width 4)

;; Use spaces for indentation
(setq indent-tabs-mode nil)

;; Remove the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; I don't like to type yes. y should suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; Vim style
(set-default 'indicate-empty-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Configure SLIME
(setq inferior-lisp-program (executable-find "sbcl"))
(require 'slime-autoloads)
(slime-setup '(slime-scratch slime-editing-commands slime-repl slime-fuzzy slime-autodoc slime-banner slime-editing-commands slime-asdf slime-presentations slime-tramp slime-references slime-xref-browser slime-highlight-edits))

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

;; Use magit
(require 'magit)

;; Load up Tramp
(require 'tramp)

;; Startup IDo
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-max-prospects 10)
(defun ido-complete-hook ()
  (define-key ido-completion-map [tab] 'ido-complete))
(add-hook 'ido-setup-hook 'ido-complete-hook)

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Textmate mode
(require 'textmate)
(textmate-mode)

;; Autopair mode
(require 'autopair)
(autopair-global-mode)
; Ruby has ruby-electric mode that does the same thing
(add-hook 'ruby-mode-hook (lambda () (setq autopair-dont-activate t)))

;; Modes in which Paredit should be active and Autopair shouldn't
(setf modes-for-paredit '(emacs-lisp lisp inferior-lisp slime slime-repl repl))

;; Paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (setq autopair-dont-activate t)))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      modes-for-paredit)

;; Yasnippet mode
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/packages/yasnippets")

;; Auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/dict")
(ac-config-default)

;; For XML files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; Temporary files aren't put in the same directory
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(auto-save-mode t)
(global-auto-revert-mode t)

;; Android mode
(require 'android-mode)

;; Emacs IDE
;; Requires ctags and cscope to be installed
;; This mode doesn't gel well with IDo mode
;(require 'eide)
;(eide-start)

;; Ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;; In Debian, ack is installed as ack-grep
(if (equal system-type 'gnu/linux)
  (defcustom ack-executable (executable-find "ack-grep")
    "*The location of the ack executable."
    :group 'full-ack
    :type 'file))

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
 '(blink-cursor-mode t)
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

;; Allow Emacs to switch to full-screen mode
(require 'fullscreen)
;(fullscreen)

;; Doesn't work in Mac OS X. Alternative required.
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

;; Move to the previous or next window
;; Copied from http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)

;; rainbow mode - Highlights colors within a file, such as "#FF00FF"
;; or "rgba(1,2,3,0.5)"
(require 'rainbow-mode)

;; Sass-mode
(require 'sass-mode)
(autoload 'sass-mode "sass-mode" nil t)

;; CSS mode
(autoload 'css-mode "css-mode" nil t)
(add-hook 'css-mode-hook '(lambda ()
			    (setq css-indent-level 2)
			    (setq css-indent-offset 2)))

;; Load up Ruby and its related settings
(require 'ruby-settings)

;; Load up JavaScript settings
(require 'javascript-settings)
