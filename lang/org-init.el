;;; Org mode functionality

(setq org-package-list '(evil-org
                         org
                         org-pomodoro
                         org-trello))

(install-packages org-package-list)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;; Remove leading stars
(setq org-hide-leading-stars t)


;; Org shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;; TODO workflow
; keywords
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE")))
; faces
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("STARTED" . "yellow")
        ("DONE" . "green")))
(setq org-log-done 'time)


;; Capture workflow
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-work-notes-file (concat org-directory "/work.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-work-notes-file "New Tasks")
             "* TODO %?\n")))
