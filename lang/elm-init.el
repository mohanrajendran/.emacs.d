;;; Inspired by http://www.lambdacat.com/post-modern-emacs-setup-for-elm/

;;;;;
;; Elm
;;;;;

(defvar elm-packages-list '(elm-mode
                            flycheck-elm))

(install-packages elm-packages-list)

;; Trigger elm-mode on opening elm files
(require 'elm-mode)

;; Add syntax checking
(require 'flycheck)
(with-eval-after-load 'flycheck
  '(add-hood 'flycheck-mode-hook #'flycheck-elm-setup))

;; Add auto completion
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))

(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(provide 'elm-init)
