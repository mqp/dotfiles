;;; -*- no-byte-compile: t -*-

;; turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
      '(:eval
        (if (buffer-file-name)
            (replace-regexp-in-string
             (concat "/home/" user-login-name) "~" buffer-file-name)
          "%b")))
(set-frame-font "Consolas-11")

(prefer-coding-system 'utf-8-unix)

(setq-default
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
 tab-width 8
 fill-column 120
 global-auto-revert-mode t
 fit-window-to-buffer-horizontally "only")

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

(load (concat user-emacs-directory "utils"))
(load (concat user-emacs-directory "dependencies"))
(dependencies-initialize)

;; set up use-package, miscellaneous config
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; hippie-expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; package-specific configs follow

(use-package lisp-mode
  :config
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'pretty-lambdas)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'pretty-lambdas))

(use-package prog-mode
  :bind ("RET" . newline-and-indent)
  :init
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (add-hook 'prog-mode-hook 'electric-indent-mode))

(use-package comint
  :defer t
  :init
  ;; sets the current buffer process to not pop up an annoying notification on Emacs exit
  (add-hook
   'comint-exec-hook
   (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (setq eldoc-idle-delay 0))

(use-package ido
  :init
  (setq
   ido-max-directory-size 100000
   ido-max-prospects 10
   ido-enable-flex-matching t
   ido-enable-prefix nil
   ido-enable-last-directory-history t
   ido-use-filename-at-point nil
   ido-use-url-at-point nil
   ido-use-virtual-buffers t
   ido-save-directory-list-file (concat user-emacs-directory "ido.hist")
   ido-ignore-buffers '("\\` " "*Messages*" "*Compile-Log*"))
  (ido-mode t)
  (ido-ubiquitous-mode 1))

(use-package uniquify
  :init
  (setq-default
   uniquify-buffer-name-style 'forward
   uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :init
  (setq-default
   save-place-file (concat user-emacs-directory "places")
   save-place t))

(use-package recentf
  :init
  (setq-default
   recentf-max-saved-items 500
   recentf-max-menu-items 15
   recentf-save-file (concat user-emacs-directory "recentf"))
  (recentf-mode 1))

;; savehist keeps track of some history
(use-package savehist
  :init
  (setq-default
   savehist-additional-variables '(search ring regexp-search-ring)
   savehist-autosave-interval 60
   savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode 1))

(use-package paren
  :init
  (setq-default show-paren-style 'parenthesis)
  (show-paren-mode 1))

(use-package smex
  :bind ("M-x" . smex)
  :init
  (setq smex-save-file (concat user-emacs-directory "smex-items"))
  :config
  (smex-initialize))

(use-package flycheck
  :config
  (global-flycheck-mode 1))

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(use-package auto-complete
  :init
  (setq-default
   ac-auto-start nil
   ac-auto-show-menu nil
   ac-use-quick-help t
   ac-use-menu-map t
   ac-quick-help-delay 0.2
   ac-modes '(clojure-mode cider-mode css-mode emacs-lisp-mode web-mode))
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (global-auto-complete-mode t)
  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function))

(use-package auto-complete-config
  :config
  (define-key ac-completing-map "\M-/" 'ac-stop)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict")))

(use-package subword
  :defer t
  :diminish subword-mode)

(use-package clojure-mode
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'clojure-mode-hook 'pretty-fn)
  (add-hook 'clojure-mode-hook 'subword-mode)
  ;; adjust indents for core.logic macros
  (put-clojure-indent 'run* 'defun)
  (put-clojure-indent 'run 'defun)
  (put-clojure-indent 'fresh 'defun))

(use-package ac-cider
  :defer t
  :config
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup))

(use-package cider
  :defer t
  :init
  (setq nrepl-hide-special-buffers t)
  (setq cider-show-error-buffer t)
  (setq cider-repl-history-file (concat user-emacs-directory "cider-history"))
  (setq cider-repl-use-pretty-printing t)
  :config
  (add-hook 'cider-repl-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
  (define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(use-package ruby-mode :mode "\\.\\(arb\\|rabl\\)$")
(use-package dockerfile-mode :mode "Dockerfile")
(use-package php-mode :mode "\\.\\(php\\|inc\\)$")
(use-package scss-mode :mode "\\.scss$")
(use-package csharp-mode :mode "\\.cs$")
(use-package glsl-mode :mode "\\.\\(glsl\\|vert\\|frag\\)$")
(use-package markdown-mode :mode (("\\.md$" . markdown-mode)
                                  ("README\\.md$" . gfm-mode)))

(use-package fn-mode :defer t :commands fn-mode)

(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package web-mode
  :mode "\\.\\(jsx\\|html\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(use-package json-mode
  :mode "\\.json$"
  :config
  ;; disable json-jsonlist checking for json files
  (setq-default
   flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist))))

(use-package js2-mode
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook 'fn-mode)
  (add-hook 'js2-mode-hook 'subword-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default
   flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint))))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(use-package ssh-config-mode
  :mode ((".ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("known_hosts\\'" . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(make-variable-buffer-local 'erc-fill-column)
(use-package erc
  :defer t
  :init
  (setq
   erc-autojoin-channels-alist '(("freenode.net" "#lesswrong" "#go" "#clojure" "#emacs"))
   erc-autojoin-delay 1
   erc-email-userid "cata"
   erc-modules '(autojoin button completion irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)
   erc-prompt ">"
   erc-prompt-for-password nil
   erc-server "irc.freenode.net"
   erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-services-mode 1)
  (erc-autojoin-mode 1)
  :config
  (add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))
  (add-hook
   'window-configuration-change-hook
   '(lambda ()
      (save-excursion
        (walk-windows
         (lambda (w)
           (let ((buffer (window-buffer w)))
             (set-buffer buffer)
             (when (eq major-mode 'erc-mode)
               (setq erc-fill-column (- (window-width w) 2))))))))))

(use-package erc-services
  :defer t
  :init
  (setq
   erc-nickserv-passwords '((freenode (("cata" . "sikhianr"))))
   erc-prompt-for-nickserv-password nil))

(load (concat user-emacs-directory "bindings/bindings-general"))
