;;; init.el --- My init file  -*- lexical-binding:t -*-

;; Try really hard not to run gc during startup.
(setq gc-cons-threshold most-positive-fixnum)
;; After startup, reset to something reasonable.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; Manually managed packages go under .emacs.d/lisp -- this is just use-package for now
;; so we only bother to eval/load when compiling this.
(eval-when-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   (directory-files (expand-file-name "lisp" user-emacs-directory) t "^[[:lower:]]"))
  (require 'use-package))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; After startup, do other useful things.
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (eq system-type 'darwin)
              ;; Mac keyboard bindings.
              (setq mac-command-modifier 'meta)
              (setq mac-option-modifier 'super)
              (setq mac-control-modifier 'control)
              (setq ns-function-modifier 'hyper)
              ;; Mac utility options.
              (setq dired-use-ls-dired nil))

            (global-hl-line-mode)
            (global-auto-revert-mode)

            (run-with-idle-timer 1 nil
                                 (lambda ()
                                   ;; Do package things.
                                   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)))
            (message (emacs-init-time))))

;; For when using emacs over a tty
(local-set-key (kbd "TAB") 'tab-to-tab-stop)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'blink-cursor-mode nil)

(show-paren-mode 1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(toggle-frame-maximized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq vc-follow-symlinks t)
(setq-default c-default-style "stroustrup"
              c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default python-indent-offset 4)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default show-paren-delay 0)
(setq-default show-trailing-whitespace t)
(setq-default sql-set-product 'postgres)
(setq-default tab-width 4)
(setq-default fill-column 80)
(electric-pair-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

(set-face-attribute 'default nil :family "Fira Code" :height 170)
(setq-default line-spacing 3)

;; Set the exec path to local binaries.
(let ((local-exec-path '("/usr/local/bin" "/usr/bin" "/bin"
                         "/usr/local/opt/llvm/bin"
                         "/Users/mbg/.cargo/bin")))
  (setq exec-path local-exec-path)
  (setenv "PATH" (mapconcat 'identity local-exec-path ":")))

(use-package nerd-icons :ensure t)

(use-package doom-modeline
  :ensure t
  :demand
  :config (doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(use-package doom-themes
  :ensure t
  :demand
  :after (doom-modeline)
  :config (progn
            (setq doom-one-brighter-comments t)
            (setq doom-one-brighter-modeline t)
            (setq doom-one-padded-modeline t)
            (setq doom-one-comment-bg t)
            (doom-themes-visual-bell-config)
            (doom-themes-org-config)
            (load-theme 'doom-one t)))

(use-package company
  :defer t
  :hook prog-mode
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-quick-access t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-idle-delay 0.1)
    (global-company-mode)))

(use-package ivy
  :config
  (progn (ivy-mode)))

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (progn
    (setq org-hide-leading-stars t)
    (setq org-log-done t)
    (setq org-agenda-files (list "~/org/work.org"
                                 "~/org/home.org"))))

(use-package org-bullets
  :hook org-mode)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package paredit
  :hook emacs-lisp-mode)

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode))

(use-package lsp-ui
  :defer t)

(use-package yasnippet
  :defer t)
