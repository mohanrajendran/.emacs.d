;; Inspired by bassam.co/emacs/2015/08/24/rust-with-emacs

;;;;
;; Rust
;;;;

(setq rust-packages-list '(company-racer
                           cargo
                           racer
                           flycheck-rust
                           rust-mode
                           toml-mode))

(install-packages rust-packages-list)

;;; Rust mode for rust files

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Set up company-racer
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'eldoc-mode)

;; Flycheck mode
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; Toml mode for toml files
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; Cargo mode (used to manage Rust projects)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'toml-mode-hook 'cargo-minor-mode)
