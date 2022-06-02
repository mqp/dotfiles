;;; fsr-mode.el --- major mode for editing Firestore rules files.

(setq fsr-keywords '("service" "match" "rules_version" "allow" "deny" "if" "null" "function" "return" "let"))
(setq fsr-constants '("read" "create" "update" "delete" "write" "list" "get"))
(setq fsr-font-lock-keywords
      `(("'\.\*'"  . font-lock-string-face)
        (,(regexp-opt fsr-keywords 'words) . font-lock-keyword-face)
        (,(regexp-opt fsr-constants 'words) . font-lock-constant-face)))

;;;###autoload
(define-derived-mode fsr-mode c-mode
  "FSR-mode"
  "Major mode for editing Firestore rules files."
  (setq font-lock-defaults '((fsr-font-lock-keywords)))
  (setq comment-start "//")
  (setq comment-end "")

  (modify-syntax-entry ?/ ". 12b" fsr-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" fsr-mode-syntax-table))

(provide 'fsr-mode)

;;; fsr-mode.el ends here
