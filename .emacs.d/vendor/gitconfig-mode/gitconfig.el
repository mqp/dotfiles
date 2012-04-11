;;; gitconfig.el --- A user interface for git

;; Usage:
;;
;; (load-file "~/path/to/gitconfig.el")
;; (require 'gitconfig-mode)

(require 'font-lock)

(defvar version "0.0.1")

(defvar gitconfig-mode-hook nil)

(defvar gitconfig-mode-syntax-table nil "Syntax table for gitconfig-mode.")
(setq gitconfig-mode-syntax-table nil)

(setq auto-mode-alist
      (append '(("\\.gitconfig" . gitconfig-mode))
              auto-mode-alist))


(defvar gitconfig-mode-font-lock-keywords
  (list '("^\\[\\(.*\\)\\]$" 1 font-lock-function-name-face)
        '("\\(.+?\\)=\\([^\r\n]*\\)"
          (1 font-lock-variable-name-face)
          (2 font-lock-type-face)))
  "Highlighting expressions for GitConfig mode.")

(defun gitconfig-mode ()
  "Major mode for editing GitConfig file."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gitconfig-mode)
  (setq mode-name "GitConfig")

                                        ; Create the syntax table
  (setq gitconfig-mode-syntax-table (make-syntax-table))
  (set-syntax-table gitconfig-mode-syntax-table)
  (modify-syntax-entry ?_  "w" gitconfig-mode-syntax-table)
  (modify-syntax-entry ?\" "w" gitconfig-mode-syntax-table)
  (modify-syntax-entry ?\(  "()" gitconfig-mode-syntax-table)
  (modify-syntax-entry ?\)  ")(" gitconfig-mode-syntax-table)
  (modify-syntax-entry ?# "<" gitconfig-mode-syntax-table)
  (modify-syntax-entry ?\n ">" gitconfig-mode-syntax-table)

                                        ; Setup font-lock mode.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gitconfig-mode-font-lock-keywords))
                                        ; Setup comment syntax.
  (make-local-variable 'comment-start)
  (setq comment-start "#")

                                        ; Run user hooks.
  (run-hooks 'gitconfig-mode-hook))

(provide 'gitconfig-mode)
