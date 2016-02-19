;;; Org mode functionality

(setq org-package-list '(evil-org
                         org
                         org-pomodoro
                         org-trello))

(install-packages org-package-list)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; TODO workflow
; keywords
(setq org-todo-keywords '("TODO" "STARTED" "DONE"))
; faces
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("STARTED" . "yellow")
        ("DONE" . "green")))
(setq org-log-done 'time)
