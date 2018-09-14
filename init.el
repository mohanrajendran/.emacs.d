;;; Package initialize
(require 'package)
(push '("marmalade" . "https://marmalade-repo.org/packages/")
      package-archives)
(push '("melpa" . "https://melpa.org/packages/")
      package-archives)

(package-initialize)


;; Package installation
(unless package-archive-contents
  (package-refresh-contents))

(defun install-packages (packages-list)
  "Go through PACKAGES-LIST and install them if not installed."
  (dolist (package packages-list)
    (unless (package-installed-p package)
      (package-install package))))

(defvar core-package-list '(aggressive-indent
                            company
                            evil
                            evil-numbers
                            flycheck
                            golden-ratio
                            helm
                            helm-projectile
                            magit
                            markdown-mode
                            paredit
                            projectile
                            rainbow-delimiters
                            window-numbering
                            yasnippet))

(install-packages core-package-list)
                     
;; Load other stuff
; Languages
(add-to-list 'load-path "~/.emacs.d/lang")
(load "clojure-init.el")
(load "rust-init.el")
(load "c-c++-init.el")
(load "js-init.el")
(load "elm-init.el")

; Other modes
(load "org-init.el")


;; Enable line numbering in all documents
(global-linum-mode 1)


;; Ibuffer-mode (used to display open buffers as a list)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Flycheck mode (used to provide syntax checking)
(global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)


;; Company mode (used to provide auto-completion)
;; (global-company-mode)


;; Yasnippet mode (used to provide code snippets)
(yas-global-mode 1)


;; Parenthesis
; Paredit mode (generate matching parenthesis on the fly)
(require 'paredit)
(add-hook 'scheme-mode-hook (lambda () (paredit-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

; Show paren mode (highlight matching parenthesis)
(show-paren-mode 1)
(setq-default show-paren-delay 0)

; Rainbow delimiters (color nested block delimiters with same color)
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (rainbow-delimiters-mode 1)))


;; Golden ratio mode for auto-resizing of multiple windows
(require 'golden-ratio)
(golden-ratio-mode 1)
; Refresh window sizes after these functions are called
(setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(select-window-0
                select-window-1
                select-window-2
                select-window-3
                select-window-4
                select-window-5
                select-window-6
                select-window-7
                select-window-8
                select-window-9)))


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
(setq-default scheme-program-name "mit-scheme-x86-64")


;; Projectile mode (to manage projects)
(projectile-global-mode)
; Integrate with Helm
(require 'helm-projectile)
(helm-projectile-on)


;; Helm-mode (used to provide auto-complete)
(require 'helm-config)
(helm-mode 1)
; New change removes automatic helm for find-file and execute-extended-command. Add it back.
(assq-delete-all 'find-file helm-completing-read-handlers-alist)
(assq-delete-all 'execute-extended-command helm-completing-read-handlers-alist)


;; Magit-mode (git layer in emacs)
(global-set-key (kbd "C-x g") 'magit-status)


;; Evil-mode (emulate vim in emacs)
(require 'evil)
(evil-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)


;; Recentf-mode (used to find recently opened files)
;; From https://www.masteringemacs.org/article/find-files-faster-recent-files-package
;; modified to use helm instead of ido
(require 'recentf)
(global-set-key (kbd "C-x C-r") 'helm-recentf-open)
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(defun helm-recentf-open ()
  "Use `helm-comp-read' to \\[find-file] a recent file."
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


;; Auto-indentation
(global-set-key (kbd "RET") 'newline-and-indent)


;; Saveplace (preserve the last location at each file)
(require 'saveplace)
(setq-default save-place t)


;; Window numbering
(require 'window-numbering)
(window-numbering-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))


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
		    :family "Fira Code"
		    :height 120)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (flycheck-elm elm-mode yasnippet window-numbering web-beautify toml-mode rainbow-delimiters racer paredit org-trello org-pomodoro markdown-mode magit json-mode js2-mode helm-projectile golden-ratio flycheck-rust evil-org evil-numbers company-tern company-racer company-c-headers clojure-mode-extra-font-locking clang-format cider cargo aggressive-indent))))
