;;; -*- no-byte-compile: t -*-

(setq-default gc-cons-threshold (* 256 1024 1024))

;; turn off mouse interface early in startup to avoid momentary display
(if (display-graphic-p) (tool-bar-mode -1))
(if (display-graphic-p) (scroll-bar-mode -1))

(menu-bar-mode -1)

(setq-default frame-title-format
      '(:eval
        (if (buffer-file-name)
            (replace-regexp-in-string
             (concat "/home/" user-login-name) "~" buffer-file-name)
          "%b")))
(set-frame-font "Fira Code-11")
(add-to-list 'default-frame-alist '(font . "Fira Code-11"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(defvar zenburn-override-colors-alist
  `(("zenburn-bg" . nil)))
(load-theme 'zenburn t)

(prefer-coding-system 'utf-8-unix)
(setq-default
 initial-major-mode 'text-mode
 bidi-display-reordering nil
 echo-keystrokes 0.1
 message-log-max t
 mouse-wheel-mode t
 xterm-mouse-mode t
 color-theme-is-global t
 delete-by-moving-to-trash t
 font-lock-maximum-decoration t
 blink-cursor-mode -1
 visible-bell nil
 inhibit-startup-screen t
 initial-scratch-message nil
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t
 vc-make-backup-files t
 custom-file (concat user-emacs-directory "custom.el")
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 create-lockfiles nil
 indent-tabs-mode nil
 tab-width 4
 fill-column 120
 global-auto-revert-mode t
 require-final-newline t
 fit-window-to-buffer-horizontally "only"
 read-process-output-max (* 16 1024 1024)
 server-client-instructions nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; set up package.el and use-package
(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq-default
 use-package-verbose t
 use-package-always-ensure t)
(setq use-package-compute-statistics t)

(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
  (use-package benchmark-init
    :config
    (require 'benchmark-init-modes)                                     ; explicitly required
    (add-hook 'after-init-hook #'benchmark-init/deactivate)))

;; load local packages, then package.el dependencies
(defvar local-packages-path (concat user-emacs-directory "vendor"))
(let ((base local-packages-path))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; miscellaneous config
(load (concat user-emacs-directory "utils"))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; hippie-expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(defun bash ()
  (interactive)
  (term "/bin/bash"))

(defun ashc (cmd)
  (interactive "sCall command: ")
  (let ((output-buffer (generate-new-buffer (format "*async:%s*" cmd)))
        (error-buffer  (generate-new-buffer (format "*error:%s*" cmd))))
    (async-shell-command cmd output-buffer error-buffer)))

;; package-specific configs follow

(require 'shell)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'js)
(setq-default js-indent-level 2)
(add-hook 'js-mode-hook 'subword-mode)
(define-key js-mode-map (kbd "M-.") nil)

(require 'lisp-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)

(require 'prog-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

(require 'wgsl-mode)
(add-to-list 'auto-mode-alist '("\\.wgsl$'" . wgsl-mode))

(require 'comint)
;; sets the current buffer process to not pop up an annoying notification on Emacs exit
(add-hook
 'comint-exec-hook
 (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(require 'uniquify)
(setq-default
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*")

(require 'eldoc)
(setq-default eldoc-idle-delay 0)

(require 'ido)
(setq-default
 ido-max-directory-size 100000
 ido-max-prospects 10
 ido-enable-flex-matching t
 ido-enable-prefix nil
 ido-enable-last-directory-history t
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-use-virtual-buffers t
 ido-save-directory-list-file (concat user-emacs-directory "ido.hist"))
(ido-mode 1)
(ido-everywhere 1)

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(require 'saveplace)
(setq-default
 save-place-file (concat user-emacs-directory "places")
 save-place-mode 1)

(require 'recentf)
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
(setq-default
 recentf-max-saved-items 500
 recentf-max-menu-items 15
 recentf-save-file (concat user-emacs-directory "recentf"))
(recentf-mode 1)

(require 'savehist)
(setq-default
 savehist-additional-variables '(search ring regexp-search-ring)
 savehist-autosave-interval 60
 savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode 1)

(require 'paren)
(setq-default show-paren-style 'parenthesis)
(show-paren-mode 1)

(setq-default css-indent-offset 2)

(use-package so-long)
(require 'so-long)
(global-so-long-mode 1)

(require 'fsr-mode)
(add-to-list 'auto-mode-alist '("firestore\\.rules$" . fsr-mode))

(use-package typescript-mode
  :after tree-sitter
  :mode "\\.\\(ts\\|tsx\\)$"
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX"
    "Major mode for editing TSX files.")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-hook 'typescript-mode-hook 'lsp)
  (setq-default typescript-indent-level 2))

(use-package amx
  :config
  (amx-mode 1))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package lsp-mode
  :commands lsp
  :custom
  (setq-default lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.next\\'")
  (setq-default lsp-eslint-trace-server t)
  (setq-default lsp-enable-snippet nil))

(use-package prettier)
(require 'prettier)
(add-hook 'after-init-hook #'global-prettier-mode)

(use-package magit
  :defer t
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill)
  (setq-default magit-completing-read-function 'magit-ido-completing-read))

(use-package forge :after magit)

(use-package rustic
  :defer t
  :config
  (add-hook 'rust-mode-hook 'lsp)
  (setq-default
   lsp-rust-analyzer-proc-macro-enable t
   lsp-enable-symbol-highlighting nil
   rustic-format-on-save nil))

(use-package diminish
  :init
  (eval-after-load "eldoc" '(diminish 'eldoc-mode))
  (eval-after-load "subword" '(diminish 'subword-mode)))

(use-package flycheck
  :config
  (global-flycheck-mode 1))

(use-package sudo-edit
  :commands sudo-edit)

(use-package yaml-mode
  :mode "\\.\\(yaml\\|yml\\)$")

(use-package company
  :config
  (setq-default company-idle-delay nil)
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package popup)

(use-package nginx-mode)
(use-package systemd)

(use-package dockerfile-mode
  :mode "^Dockerfile")

(use-package php-mode
  :mode "\\.\\(php\\|inc\\)$")

(use-package csharp-mode
  :mode "\\.cs$")

(use-package lua-mode
  :mode "\\.lua$")

(use-package glsl-mode
  :mode "\\.\\(glsl\\|vert\\|frag\\)$")

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("README\\.md$" . gfm-mode)))

(use-package web-mode
  :mode "\\.\\(html\\|js\\)$"
  :config
  (add-hook 'web-mode-hook 'lsp)
  (setq-default
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-ac-sources-alist
   '(("css" . (ac-source-css-property))
     ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(use-package json-mode
  :mode "\\.json$"
  :config
  (add-hook 'json-mode-hook 'lsp)
  ;; disable json-jsonlist checking for json files
  (setq-default
   flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist))))

(use-package handlebars-mode
  :mode "\\.hbs$")

(use-package ssh-config-mode
  :mode ((".ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("known_hosts\\'" . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(load (concat user-emacs-directory "bindings/bindings-general"))
