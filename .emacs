;;; .emacs
;;; -*- coding: shift-jis -*-
;;; -*- copyright: 2005 sahara -*-

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;;; set language and input method
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

(if (eq system-type 'windows-nt)
    (let ()
      (setq default-input-method "W32-IME")
      (setq-default w32-ime-mode-line-state-indicator "[--]")
      (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
      (w32-ime-initialize)
      (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
      (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

      ;; disable ime in isearch
      (add-hook 'isearch-mode-hook
		'(lambda ()
		   (deactivate-input-method)
		   (setq w32-ime-composition-window (minibuffer-window))))
      (add-hook 'isearch-mode-end-hook
		'(lambda () (setq w32-ime-composition-window nil)))

      ;; disable ime in helm
      (advice-add 'helm :around
		  '(lambda (orig-fun &rest args)
		     (let ((select-window-functions nil)
			   (w32-ime-composition-window (minibuffer-window)))
		       (deactivate-input-method)
		       (apply orig-fun args))))
      ))

;; disable ime on mini buffer
(add-hook 'minibuffer-setup-hook 'deactivate-input-method)

;;; theme
(load-theme 'manoj-dark t)

(if (eq system-type 'windows-nt)
    (let ()
      ;;(set-face-attribute 'default nil :family "Consolas" :height 130)
      (add-to-list 'default-frame-alist '(font . "MyricaM M-13"))
      ))

;;; window and display
;; tool bar
(tool-bar-mode 0)

;; wheel mouse
(mouse-wheel-mode t)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
(setq mouse-wheel-progressive-speed t) ; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; keyboard scroll one line at a time

;; line and column number
(line-number-mode 1)
(column-number-mode 1)

;; bell
;;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; scratch-log
(setq sl-scratch-log-file "~/.emacs.d/.scratch-log")
(setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
(setq sl-restore-scratch-p t)
(setq sl-prohibit-kill-scratch-buffer-p nil)
(require 'scratch-log)

;; save frame size and position
(desktop-save-mode 1)

;;; saving
;; do not save automatically
(setq auto-save-default nil)
(setq make-backup-files nil)

;; auto save mini buffer history
;;(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;; key
(global-set-key "\C-h" 'delete-backward-char)

;;; mode
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

;; groovy-mode
(add-hook 'groovy-mode-hook
          '(lambda ()
	     (setq c-basic-offset 4)
	     (setq tab-width 8)
	     (setq indent-tabs-mode nil)
             ))

;; gradle-mode
(if (>= emacs-major-version 25)
    (progn
      (add-to-list 'auto-minor-mode-alist '("\\.gradle\\'" . gradle-mode))
      ))

;; web-mode
(add-hook 'web-mode-hook
	  '(lambda ()
	     ;; indent
	     ;;(setq web-mode-attr-indent-offset nil)
	     (setq web-mode-markup-indent-offset 2)
	     (setq web-mode-css-indent-offset 2)
	     (setq web-mode-style-padding 2)
	     (setq web-mode-code-indent-offset 4)
	     (setq web-mode-script-padding 2)
	     (setq web-mode-sql-indent-offset 2)
	     (setq indent-tabs-mode nil)
	     (setq tab-width 2)

	     ;; auto tag closing
	     ;; 0: no auto-closing
	     ;; 1: auto-close with </
	     ;; 2: auto-close with > and </
	     (setq web-mode-tag-auto-close-style 2)
	     ))

(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . web-mode))
(setq web-mode-engines-alist '(("riot" . "\\.tag\\'")))

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (scratch-log typescript-mode arjen-grey-theme gradle-mode groovy-mode web-mode)))
 '(safe-local-variable-values (quote ((syntax . elisp)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "#587F35"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
 '(web-mode-html-tag-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-server-comment-face ((t (:foreground "#587F35")))))

;;;
;;; end of file
;;;
