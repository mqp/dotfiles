(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "aa742450bc84284415b398be20bfe1c7e63b58fbbc4beb4f2709ce08f2ca3c92" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "de43637da82e6127fd76472ae58682927f25693fcccb16161be12f2331bcc7cc" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" default))
 '(magit-section-visibility-indicator '("…" . t))
 '(package-selected-packages
   '(magit quelpa tree-sitter minimap ligature mood-line moody cyberpunk-theme material-theme material gruvbox-theme consult marginalia vertico apheleia treemacs neotree so-long yaml-mode web-mode vterm typescript-mode tsi tree-sitter-langs systemd sudo-edit ssh-config-mode rustic quelpa-use-package prettier popup php-mode nginx-mode lua-mode lsp-mode json-mode ido-completing-read+ handlebars-mode glsl-mode forge flycheck eglot dockerfile-mode diminish csharp-mode company benchmark-init amx))
 '(safe-local-variable-values
   '((eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (lsp-file-watch-ignored-directories-additional "/common/lib$" "/docs/build$" "/functions/lib$" "/functions/dist$" "/functions/firestore_export$")
     (lsp-file-watch-ignored-directories-additional "[////]\\lib\\'")
     (lsp-file-watch-ignored-directories-additional "[////]\\.next\\'")))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-annotations ((t (:foreground "sandy brown"))))
 '(line-number ((t (:inherit default :background "black" :foreground "white smoke"))))
 '(magit-diff-added ((t (:extend t :background "#335533"))))
 '(magit-diff-added-highlight ((t (:extend t :background "#336633"))))
 '(magit-diff-removed ((t (:extend t :background "#553333"))))
 '(magit-diff-removed-highlight ((t (:extend t :background "#663333"))))
 '(mood-line-buffer-status-read-only ((t (:inherit shadow :foreground "light gray" :weight normal))))
 '(mood-line-status-neutral ((t (:inherit mood-line-unimportant :foreground "light gray"))))
 '(mood-line-unimportant ((t (:inherit shadow :foreground "light gray" :weight normal)))))
