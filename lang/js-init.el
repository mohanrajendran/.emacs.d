;;;;;
;; JavaScript
;;;;;

(setq js-packages-list '(company-tern
                         js2-mode
                         json-mode
                         tern
                         web-beautify))

(install-packages js-packages-list)

;; Auto mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; JS completion
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

;; JS Beautify
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'web-beautify-js)
            (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
(add-hook 'json-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'web-beautify-js)
            (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
