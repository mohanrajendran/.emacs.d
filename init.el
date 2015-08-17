;; Package initialize
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)

; init packages if not installed (useful in migration)
(unless package-archive-contents
  (package-refresh-contents))

(setq package-list '(evil 
		     helm
		     magit
		     markdown-mode 
		     yasnippet))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Ibuffer-mode (used to display open buffers as a list)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Yasnippet mode (used to provide code snippets)
(yas-global-mode 1)

;; Markdown mode to work with yasnippet (try [tab] to perform
;; yas-expand first fallback to the original behavior in this
;; mode)
(require 'markdown-mode)
(add-hook 'markdown-mode-hook
	  (let ((original-command (lookup-key markdown-mode-map [tab])))
	    `(lambda ()
	       (setq yas-fallback-behavior
		     '(apply ,original-command))
	       (local-set-key [tab] 'yas-expand))))

;; Helm-mode (used to provide auto-complete)
(require 'helm-config)
(helm-mode 1)

;; Evil-mode (emulate vim in emacs)
(require 'evil)
(evil-mode 1)

;; Disable extra stuff
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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
