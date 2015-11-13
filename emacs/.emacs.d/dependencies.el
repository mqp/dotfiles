;;; Code to load third-party dependencies (both from ELPA and the vendor directory.)
(require 'package)
(require 'dash)

(defvar dependencies-archives
  '(("melpa-stable" . "http://stable.melpa.org/packages/")
    ("melpa-unstable" . "http://melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar dependencies-packages
  '(auto-complete
    ac-cider
    cider
    popup
    flycheck
    fuzzy
    ido-ubiquitous
    magit
    gitignore-mode
    gitconfig-mode
    ssh-config-mode
    clojure-mode
    markdown-mode
    nginx-mode
    scss-mode
    smex
    json-mode
    csharp-mode
    php-mode
    haskell-mode
    web-mode
    dockerfile-mode))

(defun dependencies-initialize ()
  "Install and initialize all dependencies, local and remote."
  (interactive)

  ;; set stuff up
  (setq package-archives dependencies-archives)
  (setq package-load-list '(all))
  (setq-default
   package-user-dir (concat user-emacs-directory "elpa")
   package-enable-at-startup nil)
  (package-initialize)

  (unless (--all? (package-installed-p it) dependencies-packages)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    ;; install the missing packages
    (dolist (p dependencies-packages)
      (unless (package-installed-p p))
        (package-install p))))
