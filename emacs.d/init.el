(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(customize-set-variable 'visible-bell t)
(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'blink-cursor-mode nil)

(global-hl-line-mode)
(global-auto-revert-mode)
(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle `show-trailing-whitespace` between t and nil."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; hippie expand - don't try to complete with file names
(setq-default hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq-default hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))
(setq-default ido-use-filename-at-point nil)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control)
  (setq ns-function-modifier 'hyper))

;; Save here instead of littering emacs backup files
(setq backup-directory-alist '(("." . "~/.emacs-backups/")))
(setq create-lockfiles nil)

(set-face-attribute 'default nil :family "mononoki" :height 170)
(setq-default line-spacing 3)
(let ((local-exec-path '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"
                         "/usr/local/go/bin"
                         "/Users/mgumport/go/bin"
                         "/Users/mgumport/misc"
                         "/Users/mgumport/git/perf-tools/bin"
                         "/Users/mgumport/git/FlameGraph"
                         "/usr/local/opt/llvm/bin")))
  (setq exec-path local-exec-path)
  (setenv "PATH" (mapconcat 'identity local-exec-path ":")))

(setenv "GOPATH" "/Users/mgumport/go")

(server-start)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; For when using emacs over a tty
(local-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Place downloaded elisp files in this directory.
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("lisp" "lisp/use-package" "lisp/rtags")))

(require 'use-package)

(use-package gruvbox-theme :ensure t
  :config (load-theme 'gruvbox-dark-hard t))

;; (use-package spacemacs-theme :ensure t
  ;; :config (load-theme spacemacs-dark))
;; (load-theme 'spacemacs-dark)

(use-package idle-highlight-mode :ensure t
  :config (idle-highlight-mode))

(use-package rainbow-mode :ensure t
  :diminish ""
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)))

(use-package company :ensure t
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence)
          company-idle-delay 0.1)
    (global-company-mode)))

(use-package company-ghci :ensure t
 :config
  (progn
    (push 'company-ghci company-backends)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-interactive-mode-hook 'company-mode)))

(use-package go-mode
  :ensure t
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
    (add-hook 'go-mode-hook #'electric-pair-local-mode)
    (use-package go-guru :ensure t)
    (progn
      (bind-key "M-." 'godef-jump go-mode-map)
      (bind-key "M-]" 'next-error go-mode-map)
      (bind-key "M-[" 'previous-error go-mode-map))))

(use-package company-go :ensure t
  :config
  (progn
    (add-hook 'go-mode-hook 'company-mode)
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode)))))

(use-package powerline :ensure t
  :config
  (powerline-default-theme))

(use-package projectile :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-completion-system 'ivy)
    (defun set-gopath-smart ()
      "Reset GOPATH if a vendor dir exists in the project root"
      (let ((vendor-dir (expand-file-name "vendor" (projectile-project-root))))
        (when (file-exists-p vendor-dir)
          (setenv "GOPATH" (concat vendor-dir path-separator (getenv "GOPATH"))))))
    (add-hook 'projectile-after-switch-project-hook 'set-gopath-smart))
  :bind
  (("C-c p h" . projectile-find-file)
   ("C-c p s" . projectile-switch-project)))

(use-package buffer-move
  :bind
  (("<C-s-up>"    . buf-move-up)
   ("<C-s-down>"  . buf-move-down)
   ("<C-s-left>"  . buf-move-left)
   ("<C-s-right>" . buf-move-right)))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (progn
    (setq org-log-done t)
    (setq org-agenda-files (list "~/org/work.org"
                                 "~/org/home.org"))))

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-hide-leading-stars t)

(use-package magit :ensure t
  :bind ("C-x g" . magit-status))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-}"         . mc/mark-next-like-this)
         ("C-{"         . mc/mark-previous-like-this)
         ("C-c C-}"     . mc/mark-all-like-this)))

(use-package counsel :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-s" . swiper-isearch)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . ivy-switch-buffer)
   ("C-x l" . counsel-locate)
   ("C-c J" . counsel-file-jump)
   :map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  ;; Show recently killed buffers when calling `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)   ;Show the full virtual file paths

  (use-package counsel-etags :ensure t
    :config
    ;; (add-hook 'c-mode-common-hook
    ;;           (lambda ()
    ;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
    ;;               (counsel-etags-mode))))))
    ))

(use-package paredit :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package markdown-mode
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))

(use-package clang-format
  :ensure t
  :config
  (progn
    (defun clang-format-buffer-smart ()
      "Reformat buffer if .clang-format exists in the project root."
      (when (and (or (eq major-mode 'c++-mode)
                     (eq major-mode 'c-mode))
                 (file-exists-p (expand-file-name ".clang-format" (projectile-project-root))))
        (clang-format-buffer)))

    (add-hook 'before-save-hook 'clang-format-buffer-smart)))

(use-package haskell-mode
  :init
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))
  :config
  (progn
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-process-type 'stack-ghci)))
  :bind
  (:map haskell-mode-map
        ("C-`"     . haskell-interactive-bring)
        ("C-c C-t" . haskell-process-do-type)
        ("C-c C-i" . haskell-process-do-info)
        ("C-c C-k" . haskell-interactive-mode-clear)
        ("C-c C-l" . haskell-process-load-file)))


;; (use-package ggtags
;;   :ensure t
;;   :init

                ;; (ggtags-mode 1)))))

(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" "\\.ts\\'" "\\.jsx\\'" "\\.tsx\\'" "\\.html\\'" "\\.json\\'")
  :bind
  (:map web-mode-map
        ("M-q" . web-mode-buffer-indent))
  :config (progn
            (defun eslint-fix-file ()
              (interactive)
              (message "eslint --fixing the file %s" (buffer-file-name))
              (shell-command (concat "eslint --fix " (buffer-file-name))))

            (defun eslint-fix-file-and-revert ()
              (interactive)
              (eslint-fix-file)
              (revert-buffer t t))

            (add-hook 'web-mode-hook
                      (lambda ()
                        (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))))

(use-package flycheck
  :ensure t
  :config (progn
            (defun my/use-eslint-from-node-modules ()
              "Pick the eslint tracked with the project if node_modules exists in the project root"
              (let ((eslintfile (expand-file-name "node_modules/eslint/bin/eslint.js"
                                                   (projectile-project-root))))
                (when (file-executable-p eslintfile)
                  (setq-local flycheck-javascript-eslint-executable eslintfile))))
            (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers '(javascript-jshint)))
            (flycheck-add-mode 'javascript-eslint 'web-mode)))

;; Assumes you have the rtags *el files in
;; ~/.emacs.d/lisp/rtags
;; https://github.com/Andersbakken/rtags
(progn
  (require 'rtags)
  (require 'company-rtags)
  (push 'company-rtags company-backends)
  (let ((keybinding-lamba (lambda ()
                             (progn
                               (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                               (local-set-key (kbd "M-,") 'rtags-find-references-at-point)
                               (local-set-key (kbd "M-;") 'rtags-find-file)
                               (local-set-key (kbd "C-.") 'rtags-find-symbol)
                               (local-set-key (kbd "C-,") 'rtags-find-references)
                               (local-set-key (kbd "C-<") 'rtags-find-virtuals-at-point)
                               (local-set-key (kbd "C-i") 'rtags-imenu)
                               (local-set-key (kbd "C-*") 'rtags-location-stack-back)
                               (local-set-key (kbd "TAB") 'indent-for-tab-command)))))
    (add-hook 'c++-mode-hook keybinding-lamba)
    (add-hook 'c-mode-hook keybinding-lamba))

  (setq rtags-completions-enabled t)
  (setq rtags-display-result-backend 'ivy)
  (setq rtags-path "/Users/mgumport/git/rtags/build/bin")
  (rtags-diagnostics))

;; Undo the work of helm-config, rtags
(progn
  (global-unset-key (kbd "C-x c"))
  (bind-key "M-]" 'find-tag)
  (bind-key "M-[" 'pop-tag-mark)
  (bind-key "C-*" 'pop-tag-mark)
  (bind-key "TAB" 'kindent-for-tab-command)
  (bind-key "C-c o" 'ff-get-other-file)
  (bind-key "<f5>" 'kmacro-end-or-call-macro))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(clang-format-executable "/usr/local/opt/llvm/bin/clang-format")
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (markdown-mode+ spacemacs-theme counsel-etags counsel-gtags gruvbox counsel helm-gtags ggtags go-guru js2-mode react-mode web-mode tide protobuf-mode company-go typescript-mode dockerfile-mode elisp--witness--lisp company-lua lua-mode graphviz-dot-mode cmake-mode hive salt-mode yaml-mode json-mode bazel-mode go-mode emojify elpy yascroll vagrant-tramp rainbow-mode rainbow-delimiters powerline paredit p4 org-bullets multiple-cursors markdown-mode magit idle-highlight-mode helm-projectile gruvbox-theme company-ghci buffer-move base16-theme)))
 '(rtags-path "/Users/mgumport/git/rtags/build/bin")
 '(sgml-basic-offset 4)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
