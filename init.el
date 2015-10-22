;; Package initialize
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives)
(push '("tromey" . "http://tromey.com/elpa")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)

(package-initialize)

; init packages if not installed (useful in migration)
(unless package-archive-contents
  (package-refresh-contents))

(setq package-list '(cider
		     clojure-mode
                     clojure-mode-extra-font-locking
		     company
		     evil
		     golden-ratio
		     helm
		     magit
		     markdown-mode
		     paredit
		     projectile
		     rainbow-delimiters
		     yasnippet))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load languages
(add-to-list 'load-path "~/.emacs.d/lang")
(load "clojure-init.el")

;; Enable line numbering in all documents
(global-linum-mode 1)

;; Ibuffer-mode (used to display open buffers as a list)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Company mode (used to provide auto-completion)
(global-company-mode)

;; Yasnippet mode (used to provide code snippets)
(yas-global-mode 1)

;; Parenthesis help
; Paredit mode (generate matching parenthesis on the fly)
(require 'paredit)
(add-hook 'scheme-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

; Show paren mode (highlight matching parenthesis)
(show-paren-mode 1)
(setq show-paren-delay 0)

; Rainbow delimiters (color nested block delimiters with same color)
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (rainbow-delimiters-mode 1)))
;(global-rainbow-delimiters-mode)

;; Golden ratio mode for auto-resizing of multiple windows
(require 'golden-ratio)
(golden-ratio-mode 1)

;; Uniquify (used to prepend folder name to files with same names so as to differentiate them)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Markdown mode
(require 'markdown-mode)

; To work with yasnippet (try [tab] to perform yas-expand first, if no expansion,
; fallback to the original behavior of markdown-cycle)
(add-hook 'markdown-mode-hook
	  (let ((original-command (lookup-key markdown-mode-map [tab])))
	    `(lambda ()
	       (setq yas-fallback-behavior
		     '(apply ,original-command))
	       (local-set-key [tab] 'yas-expand))))

; Start fly-check in markdown
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))

;; Scheme mode used to run scheme
(setq scheme-program-name "mit-scheme-x86-64")

;; Projectile mode (to manage projects)
(projectile-global-mode)

;; Helm-mode (used to provide auto-complete)
(require 'helm-config)
(helm-mode 1)

;; Evil-mode (emulate vim in emacs)
(require 'evil)
(evil-mode 1)

;; Org-mode (for taking notes)
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Recentf-mode (used to find recently opened files)
;; From https://www.masteringemacs.org/article/find-files-faster-recent-files-package
;; modified to use helm instead of ido
(require 'recentf)
(global-set-key (kbd "C-x C-r") 'helm-recentf-open)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(defun helm-recentf-open ()
  "Use `helm-comp-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (helm-comp-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Reconfigure search bindings to use regex search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Use space instead of tabs
(setq-default indent-tabs-mode nil)

;; Saveplace (preserve the last location at each file)
(require 'saveplace)
(setq-default save-place t)

;; Disable extra stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)

;; Set default theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

(add-to-list 'load-path "~/.emacs.d/themes/nyan-mode-master/")
(require 'nyan-mode)
(nyan-mode 1)

;; Set default font
(set-face-attribute 'default nil
		    :family "Source Code Pro"
		    :height 90
		    :weight 'normal
		    :width 'normal)
