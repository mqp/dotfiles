;; turn off mouse interface early in startup to avoid momentary display
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; work around a bug where system-name is FQDN on OS X
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

(setq frame-title-format
      '(:eval
        (if (buffer-file-name)
            (replace-regexp-in-string
             (concat "/home/" user-login-name) "~" buffer-file-name)
          "%b")))
(set-frame-font "Consolas-11")

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
 package-user-dir (concat user-emacs-directory "elpa")
 backup-directory-alist `(("." . ,(concat user-emacs-directory
                                          "backups")))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 create-lockfiles nil
 indent-tabs-mode nil
 tab-width 8
 global-auto-revert-mode t)

(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

;; hippie-expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(defun set-background-process ()
  "Sets the current buffer process to not pop up an annoying notification on Emacs exit."
  (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'comint-exec-hook 'set-background-process)

(require 'ido)
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

(require 'uniquify)
(setq-default
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*")

(require 'saveplace)
(setq-default
  save-place-file (concat user-emacs-directory "places")
  save-place t)

(require 'recentf)
(setq-default
 recentf-max-saved-items 500
 recentf-max-menu-items 15)
 recentf-save-file (concat user-emacs-directory "recentf")
(recentf-mode 1)

;; savehist keeps track of some history
(require 'savehist)
(setq-default
 savehist-additional-variables '(search ring regexp-search-ring)
 savehist-autosave-interval 60)
 savehist-file (concat user-emacs-directory "savehist")
(savehist-mode 1)

(require 'paren)
(setq-default
 show-paren-style 'parenthesis)
(show-paren-mode 1)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun pretty-fn ()
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    "\u0192"
				    'decompose-region)))))))

;; Emacs Lisp
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)

;; elpa packages
(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete
    cider
    ac-nrepl
    popup
    fuzzy
    flymake-cursor
    ido-ubiquitous
    zenburn-theme
    quack
    git-commit-mode
    git-rebase-mode
    gitignore-mode
    gitconfig-mode
    clojure-mode
    markdown-mode
    nginx-mode
    scss-mode
    nrepl
    smex
    json-mode
    csharp-mode
    php-mode
    haskell-mode
    web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(let ((base (concat user-emacs-directory "vendor")))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(load-theme 'zenburn t)
(ido-ubiquitous-mode 1)

;; smex
(require 'smex)
(setq smex-save-file (concat user-emacs-directory "smex-items"))
(smex-initialize)

(require 'auto-complete-config)
(ac-config-default)
(setq-default
 ac-auto-start nil
 ac-auto-show-menu nil
 ac-use-quick-help t
 ac-use-menu-map t
 ac-quick-help-delay 0.2)
(ac-set-trigger-key "TAB")
(define-key ac-completing-map "\M-/" 'ac-stop)
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t))))
  (message "Refreshed open files."))

(defun kill-other-buffers ()
  "Kill all buffers except the current buffer."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun pretty-fn ()
  (font-lock-add-keywords
   nil `(("(\\(\\<fn\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    "\u0192"
				    'decompose-region)))))))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Clojure
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'pretty-fn)
(add-hook 'clojure-mode-hook 'subword-mode)
(rename-modeline "clojure-mode" clojure-mode "Clj")

;; adjust indents for core.logic macros
(eval-after-load "clojure-mode"
  '(progn
     (put-clojure-indent 'run* 'defun)
     (put-clojure-indent 'run 'defun)
     (put-clojure-indent 'fresh 'defun)))

;; nREPL
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(setq nrepl-history-file (concat user-emacs-directory "nrepl-history"))
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces t)
(setq nrepl-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*nrepl*")

(require 'ac-nrepl)
(dolist (hook '(cider-mode-hook cider-repl-mode-hook))
  (add-hook hook 'ac-nrepl-setup)
  (add-hook hook 'set-auto-complete-as-completion-at-point-function))

(require 'auto-complete)
(add-to-list 'ac-modes 'cider-repl-mode)

(eval-after-load "cider"
  '(progn
     (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
     (define-key cider-repl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)))

;; Extempore
(autoload 'extempore-mode (concat user-emacs-directory "vendor/extempore-mode/extempore.el"))
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.aspx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jt$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; C#
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; PHP
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))

;; Smali
(require 'smali-mode)
(add-to-list 'auto-mode-alist '("\\.smali$" . smali-mode))

;; Javascript
(require 'flymake-node-jshint)
(autoload 'fn-mode (concat user-emacs-directory "vendor/fn-mode/fn-mode.el"))
(add-hook 'js-mode-hook 'fn-mode)
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook (lambda () (flymake-mode 1)))
(rename-modeline "js-mode" javascript-mode "JS")
(add-to-list 'auto-mode-alist '("\\.avsc$" . javascript-mode))

;; Python
(setq py-install-directory (concat user-emacs-directory "vendor/python-mode/"))
(setq py-shell-name "ipython")

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ERC
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#lesswrong" "#go" "#clojure" "#emacs")))
(setq erc-autojoin-delay 1)
(setq erc-email-userid "cata")
(setq erc-modules '(autojoin button completion irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track))
(setq erc-nickserv-passwords '((freenode (("cata" . "sikhianr")))))
(setq erc-prompt ">")
(setq erc-prompt-for-nickserv-password nil)
(setq erc-prompt-for-password nil)
(setq erc-server "irc.freenode.net")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(erc-services-mode 1)
(erc-autojoin-mode 1)

(require 'tls)
(defun erc-connect ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000 :nick "cata"))

(load-file (concat user-emacs-directory "bindings/bindings-general.el"))
