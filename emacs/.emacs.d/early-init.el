;;; -*- no-byte-compile: t; lexical-binding: t; -*-
;; there's currently some issue where prettier can't get the version
;; of this that it wants from ELPA, so vendor it locally and mark it here
(add-to-list 'package--builtin-versions `(iter2 1 2 0))
