(require 'tls)

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

(defun erc-connect ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000 :nick "cata"))

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
