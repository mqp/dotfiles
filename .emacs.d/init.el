;; Turn off mouse interface early in startup to avoid momentary
;; display

(menu-bar-mode -1)
(tool-bar-mode -1)
(recentf-mode 1)
(setq echo-keystrokes 0.01)
(setq visible-bell nil)
(set-default-font "Consolas-13")

;; Don't clutter up directories with backup files~
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; ...or #autosave files
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq frame-title-format
      '(:eval
        (if (buffer-file-name)
            (replace-regexp-in-string
             (concat "/home/" user-login-name) "~" buffer-file-name)
          "%b")))

(setq initial-scratch-message nil)

(require 'tramp)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; elpa packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell
    auto-complete
    ac-nrepl
    popup
    fuzzy
    zenburn-theme
    slime
    slime-repl
    clojure-mode
    nrepl
    csharp-mode
    php-mode
    haskell-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(let ((base "~/.emacs.d/vendor"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(load-theme 'zenburn t)

(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(define-key ac-completing-map "\M-/" 'ac-stop)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-auto-start nil)
(setq ac-auto-show-menu nil)
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.2)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(require 'clojure-mode)
;; adjust indents for core.logic macros
(add-hook 'clojure-mode-hook 'subword-mode)
(put-clojure-indent 'run* 'defun)
(put-clojure-indent 'run 'defun)
(put-clojure-indent 'fresh 'defun)

(require 'nrepl)
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl nil)
(add-to-list 'same-window-buffer-names "*nrepl*")

(require 'ac-nrepl)
(dolist (hook '(nrepl-mode-hook nrepl-interaction-mode-hook))
  (add-hook hook 'ac-nrepl-setup)
  (add-hook hook 'set-auto-complete-as-completion-at-point-function))
(define-key nrepl-mode-map  (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

(require 'quack)
(require 'extempore)

(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(add-to-list 'auto-mode-alist '("\\.aspx\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.jt\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

(require 'powershell-mode)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

(require 'dos)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

(require 'drupal-mode)
(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))

(require 'x12-mode)
(add-to-list 'auto-mode-alist '("\\.\\([xX]12\\|850\\|810\\|856\\|894\\)$" . x12-mode))

;; javascript
(require 'js)
(require 'fn-mode)
(require 'flymake)
(require 'flymake-cursor)
(require 'flymake-node-jshint)
(setq flymake-node-jshint-config "~/.emacs.d/vendor/flymake-node-jshint/quixey.json")
(add-hook 'js-mode-hook (lambda () (flymake-mode 1)))
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'js-mode-hook 'fn-mode)

(setq auto-fill-mode -1)
(setq ido-max-directory-size 100000)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

;; python
(setq py-install-directory "~/.emacs.d/vendor/python-mode/")

(require 'python-mode)
(setq py-shell-name "ipython")

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list "--ignore=E111" local-file))))

(defun flymake-pep8-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pep8" (list "--repeat" local-file))))

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))


(defun flymake-lintrunner-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "lintrunner" (list local-file))))

(defun flymake-python-init ()
  (flymake-lintrunner-init))

(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; erc
(require 'erc-join)
(require 'erc-services)
(require 'tls)
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

(setq erc-autojoin-channels-alist '(("quixey.com" "#eng")
                                    ("freenode.net" "#lesswrong" "#lw-minicamp" "#go" "#clojure" "#emacs" "#javascript" "#meteor")))
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

(defun erc-connect ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "cata")
  ;;(erc-tls :server "ircd.quixey.com" :port 6697 :nick "mquander" :password "focus")
  )

(load-file "~/.emacs.d/bindings/bindings-general.el")
(add-hook 'prog-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'sudo)
(defun sudo-before-save-hook ()
  (set (make-local-variable 'sudo:file) (buffer-file-name))
  (when sudo:file
    (unless (file-writable-p sudo:file)
      (set (make-local-variable 'sudo:old-owner-uid) (nth 2 (file-attributes sudo:file)))
      (when (numberp sudo:old-owner-uid)
        (unless (= (user-uid) sudo:old-owner-uid)
          (when (y-or-n-p
                 (format "File %s is owned by %s, save it with sudo? "
                         (file-name-nondirectory sudo:file)
                         (user-login-name sudo:old-owner-uid)))
            (sudo-chown-file (int-to-string (user-uid)) (sudo-quoting sudo:file))
            (add-hook 'after-save-hook
                      (lambda ()
                        (sudo-chown-file (int-to-string sudo:old-owner-uid)
                                         (sudo-quoting sudo:file))
                        (if sudo-clear-password-always
                            (sudo-kill-password-timeout)))
                      nil   ;; not append
                      t    ;; buffer local hook
                      )))))))


(add-hook 'before-save-hook 'sudo-before-save-hook)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
