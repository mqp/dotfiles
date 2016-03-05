;;; Code to load third-party dependencies (both from ELPA and the vendor directory.)
(require 'package)
(require 'cl-macs)

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
    js2-mode
    magit
    gitignore-mode
    gitconfig-mode
    ssh-config-mode
    clojure-mode
    glsl-mode
    markdown-mode
    lua-mode
    nginx-mode
    haml-mode
    feature-mode
    editorconfig
    powershell
    scss-mode
    smex
    json-mode
    csharp-mode
    php-mode
    haskell-mode
    web-mode
    dockerfile-mode
    use-package))

(defun dependencies-installed (packages)
  (cl-loop for p in packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(defun dependencies-initialize ()
  "Install and initialize all dependencies."
  (interactive)

  ;; set stuff up
  (setq load-prefer-newer t)
  (setq package-archives dependencies-archives)
  (setq package-load-list '(all))
  (setq-default
   package-user-dir (concat user-emacs-directory "elpa")
   package-enable-at-startup nil)
  (package-initialize)

  (unless (dependencies-installed dependencies-packages)
    ;; check for new packages (package versions)
    (message "Refreshing package database...")
    (package-refresh-contents)
    ;; install the missing packages
    (dolist (p dependencies-packages)
      (unless (package-installed-p p))
        (package-install p)))
   (message "Dependencies up to date."))
