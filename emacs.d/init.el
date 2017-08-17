(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(customize-set-variable 'visible-bell t)

(customize-set-variable 'auto-save-default nil)
(customize-set-variable 'make-backup-files nil)

(customize-set-variable 'blink-cursor-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(server-start)

;; env PATH

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setq exec-path (split-string path-from-shell path-separator))
    (setenv "PATH" path-from-shell)))

(set-exec-path-from-shell-PATH)

;; Place downloaded elisp files in this directory.
(eval-and-compile
  (mapc
    #'(lambda (path)
	(push (expand-file-name path user-emacs-directory) load-path))
    '("lisp" "lisp/use-package" "lisp/rtags")))

(require 'use-package)
(require 'rtags)
(require 'company-rtags)

(use-package gruvbox-theme
  :ensure t)

(use-package idle-highlight-mode
  :ensure t
  :config (idle-highlight-mode))

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1))

(use-package rainbow-mode
  :ensure rainbow-mode
  :diminish ""
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)))

(use-package vagrant-tramp
  :ensure t
  :config (eval-after-load 'tramp
            '(vagrant-tramp-enable)))

(use-package company
  :ensure t
  :config
  (progn
    (setq company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-show-numbers t
          company-tooltip-align-annotations t
          company-require-match nil
          company-transformers '(company-sort-by-occurrence))
    (global-company-mode)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package buffer-move
  :ensure t
  :bind
  (("<C-s-up>"    . buf-move-up)
   ("<C-s-down>"  . buf-move-down)
   ("<C-s-left>"  . buf-move-left)
   ("<C-s-right>" . buf-move-right)))

(use-package multi-web-mode
  :ensure t
  :config
  (progn
    (setq mweb-default-major-mode 'html-mode)
    (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                      (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                      (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
    (multi-web-global-mode 1)))

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (progn
    (setq org-log-done t)
    (setq org-agenda-files (list "~/org/work.org"
                                 "~/org/home.org"
                                 "~/Documents/worldview/worldview.org"))))

(use-package magit
  :ensure t
  :bind ("C-x M-g" . magit-status))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-}"         . mc/mark-next-like-this)
         ("C-{"         . mc/mark-previous-like-this)
         ("C-c C-}"     . mc/mark-all-like-this)))

(use-package p4
  :ensure t
  :config (require 'p4))

(use-package helm
  :ensure t
  :init
  (progn
    (setq helm-autoresize-max-height 70)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-ff-file-name-history-use-recentf t
          ;; Wraparound top and bottom
          helm-move-to-line-cycle-in-source     t
          ;; Search for library in `require' and `declare-function' sexp
          helm-ff-search-library-in-sexp        t
          ;; Scroll 8 lines in other window using M-<next>/M-<prior>
          helm-scroll-amount                    8
          ;; open helm buffer inside current window, don't occupy the
          ;; whole other window
          helm-spit-window-in-side-p t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t))
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (progn
    (require 'helm-config)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (helm-autoresize-mode t)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    (helm-mode 1)))

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.text\\'" "\\.markdown\\'" "\\.md\\'"))

(use-package haskell-mode
  :ensure t
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
  (("C-`"     . haskell-interactive-bring)
   ("C-c C-t" . haskell-process-do-type)
   ("C-c C-i" . haskell-process-do-info)
   ("C-c C-k" . haskell-interactive-mode-clear)))

(use-package rtags
  :ensure t
  :init
  (custom-set-variables
   '(rtags-path "/home/tsi/mbgumport/git/rtags/bin"))
  :bind
  (("M-." . rtags-find-symbol-at-point)
   ("M-," . rtags-find-references-at-point)
   ("M-;" . rtags-find-file)
   ("C-." . rtags-find-symbol)
   ("C-," . rtags-find-references)
   ("C-<" . rtags-find-virtuals-at-point)
   ("M-i" . rtags-imenu)
   ("C-*" . rtags-location-stack-back))
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (setq rtags-completions-enabled t)
    (rtags-diagnostics)))

(use-package company-rtags
  :ensure t
  :config
  (push 'company-rtags company-backends))

;; Undo the work of helm-config
(global-unset-key (kbd "C-x c"))

;; Look and feel
(scroll-bar-mode -1)
(global-hl-line-mode)
(setq-default show-trailing-whitespace t)

(setq-default sql-set-product 'postgres)

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(show-paren-mode 1)
(setq-default show-paren-delay 0)
(global-auto-revert-mode t)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)
(setq-default line-spacing 1)
(setq-default indent-tabs-mode nil)
(idle-highlight-mode)
(setq-default tab-width 4)

(setq-default c-default-style "stroustrup"
              c-basic-offset 4)

;; Key bindings
(progn
  (bind-key "M-*" 'pop-tag-mark))

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".hpp" ".ipp" ".h"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".c" ".cpp"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

(set-face-attribute 'default nil :family "mononoki" :height 130)

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; hippie expand - don't try to complete with file names
(setq-default hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq-default hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq-default ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist '(("." . "~/.emacs-backups/")))
(setq create-lockfiles nil)

(defalias 'list-buffers 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(cmake-tab-width 4)
 '(custom-enabled-themes (quote (gruvbox)))
 '(custom-safe-themes
   (quote
    ("3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "1abda075ebacaa3795d675bb2be0a905322ac856f9c0c259da63f9ccfe1962ec" default)))
 '(global-auto-revert-mode t)
 '(global-linum-mode nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(line-number-mode nil)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (rust-mode 0blayout company-rtags helm-rtags rtags base16-theme clang-format yaml-mode gruvbox-theme groovy-mode ninja-mode yascroll vagrant-tramp rainbow-mode rainbow-delimiters powerline paredit p4 multi-web-mode markdown-mode magit idle-highlight-mode helm-projectile company cmake-mode buffer-move gruvbox)))
 '(rtags-path "/home/tsi/mbgumport/git/rtags/bin")
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "sienna2"))))
 '(fringe ((t (:background "#3f393a"))))
 '(hl-line ((t (:background "gray27"))))
 '(linum ((t (:background "nil" :foreground "#f4f1ed" :height 0.7))))
 '(region ((t (:background "#647E5A"))))
 '(trailing-whitespace ((t (:background "light sky blue")))))

(load-theme 'base16-gruvbox-dark-hard)
(load-theme 'gruvbox)
