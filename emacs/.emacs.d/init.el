;;; -*- no-byte-compile: t; lexical-binding: t; -*-

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default
 completion-styles '(substring)
 dired-listing-switches "-alh"
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
 server-client-instructions nil
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 tab-always-indent 'complete
 line-spacing 0
 electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
 )


(bind-keys
 ("C-h a" . apropos)
 ("M-f" . forward-word)
 ("M-b" . backward-word)
 ("M-/" . hippie-expand)
 ("C-c q" . join-line)
 ("C-+" . text-scale-increase)
 ("C--" . text-scale-decrease)
 ("C-s" . isearch-forward-regexp)
 ("C-r" . isearch-backward-regexp)
 ("C-M-s" . isearch-forward)
 ("C-M-r" . isearch-backward)
 ("C-x g" . magit-status))

(when (file-exists-p custom-file) (load custom-file))
(load (concat user-emacs-directory "package-bootstrap"))
(load (concat user-emacs-directory "utils"))


(pixel-scroll-mode)
(use-package nordic-night-theme :init (load-theme 'nordic-night))
(set-frame-parameter nil 'alpha-background 90)
(set-background-color "black")

(use-package mood-line :init (mood-line-mode))
(set-face-attribute 'header-line nil :box nil)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-highlight nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box)
(set-face-foreground 'vertical-border (face-attribute 'default :background))
(set-face-background 'vertical-border (face-attribute 'default :background))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; hippie-expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

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

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (sql "https://github.com/m-novikov/tree-sitter-sql")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))


(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

(require 'eglot)
(setq-default eglot-ignored-server-capabilities '(:inlayHintProvider))

(require 'typescript-ts-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'typescript-ts-mode-hook 'subword-mode)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'subword-mode)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(setq-default typescript-indent-level 2)

(require 'json-ts-mode)

(require 'yaml-ts-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yaml\\|yml\\)\\'" . yaml-ts-mode))

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("README\\.md$" . gfm-mode)))

(setq-default css-indent-offset 2)

(use-package wgsl-mode
  :straight (:host github :repo "acowley/wgsl-mode")
  :mode "\\.wgsl$")

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :init (global-ligature-mode t)
  :config
  ;; fira code has so many ligatures but they are ugly... I only like these ones
  (ligature-set-ligatures 'prog-mode '("/*" "*/" "//" "///" ";;"
                                       "||" "&&" "??" "::" ">>" "<<" "++" "--")))

(use-package rainbow-mode
  :custom (rainbow-x-colors nil)
  :hook prog-mode)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config
  (setq vertico-count 10)
  (setq vertico-multiform-commands
                '((consult-buffer (vertico-count . 20))
                  (execute-extended-command flat)
                  ("magit-" flat)))
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package vertico-flat
  :after vertico
  :bind (:map vertico-flat-map ("TAB" . vertico-next)))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")
  (setq consult-narrow-key "<"))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom (marginalia-max-relative-age 0)
  :init (marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult :after (embark consult))

(use-package so-long :init (global-so-long-mode))

(require 'fsr-mode)
(add-to-list 'auto-mode-alist '("firestore\\.rules$" . fsr-mode))

(use-package vterm
  :config
  (setq-default vterm-buffer-name-string "vterm %s"
                vterm-always-compile-module t
                vterm-max-scrollback 10000))

;; (use-package lsp-mode
;;   :commands lsp
;;   :custom
;;   (lsp-completion-provider :none)
;;   (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
;;   :config
;;   (defvar lsp-file-watch-ignored-directories-additional nil
;;     "Additional ignored directories added to lsp-file-watch-ignored-directories.")
;;   (put 'lsp-file-watch-ignored-directories-additional 'safe-local-variable #'lsp--string-listp)
;;   (add-function :around (symbol-function 'lsp-file-watch-ignored-directories)
;;                 (lambda (orig)
;;                   (print "appending")
;;                   (append lsp-file-watch-ignored-directories-additional (funcall orig))))
;;   (add-to-list 'lsp-file-watch-ignored-directories "/\\.docusaurus$")
;;   (add-to-list 'lsp-file-watch-ignored-directories "/\\.next$")
;;   (setq-default lsp-eslint-trace-server t)
;;   (setq-default lsp-enable-snippet nil)
;;   (setq-default lsp-modeline-code-actions-enable nil)
;;   (setq-default lsp-modeline-diagnostics-enable nil))

(use-package prettier
  :init
  (setq-default prettier-mode-sync-config-flag nil)
  (add-hook 'js-ts-mode-hook 'prettier-mode)
  (add-hook 'typescript-ts-mode-hook 'prettier-mode)
  (add-hook 'tsx-ts-mode-hook 'prettier-mode)
  (add-hook 'json-ts-mode-hook 'prettier-mode))

(use-package magit
  :init
  ;; (add-hook 'magit-popup-mode-hook #'fit-window-to-buffer)
  ;; (add-hook 'magit-create-buffer-hook #'fit-window-to-buffer)
  ;; (add-hook 'magit-popup-help-mode-hook #'fit-window-to-buffer)
  ;; (add-hook 'magit-refresh-popup-buffer-hook #'fit-window-to-buffer)
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-auto-fill))

(use-package git-modes)

(use-package rustic
  :config
  (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
  (setq-default
   rustic-lsp-client 'eglot
   rustic-format-trigger 'on-save))

(use-package sudo-edit :commands sudo-edit)

(use-package corfu
  :bind (
         :map corfu-mode-map
         ("M-/" . completion-at-point))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-echo-documentation t)
  :init (global-corfu-mode))


(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package popup)

(use-package nginx-mode)

(use-package dockerfile-mode :mode "^Dockerfile")

(use-package glsl-mode :mode "\\.\\(glsl\\|vert\\|frag\\)$")

(use-package web-mode
  :mode "\\.html$"
  :config
  (setq-default
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-ac-sources-alist
   '(("css" . (ac-source-css-property))
     ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(use-package handlebars-mode :mode "\\.hbs$")

(use-package ssh-config-mode
  :mode ((".ssh/config\\'" . ssh-config-mode)
         ("sshd?_config\\'" . ssh-config-mode)
         ("known_hosts\\'" . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package sql-indent
  :hook sql-mode)
