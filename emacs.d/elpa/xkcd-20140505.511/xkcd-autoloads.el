;;; xkcd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (xkcd-get-latest xkcd-get xkcd-mode) "xkcd" "xkcd.el"
;;;;;;  (21438 14188 304093 324000))
;;; Generated autoloads from xkcd.el

(autoload 'xkcd-mode "xkcd" "\
Major mode for viewing xkcd (http://xkcd.com/) comics.

\(fn)" t nil)

(autoload 'xkcd-get "xkcd" "\
Get the xkcd number NUM.

\(fn NUM)" t nil)

(autoload 'xkcd-get-latest "xkcd" "\
Get the latest xkcd.

\(fn)" t nil)

(defalias 'xkcd 'xkcd-get-latest)

;;;***

;;;### (autoloads nil nil ("xkcd-pkg.el") (21438 14188 324834 858000))

;;;***

(provide 'xkcd-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xkcd-autoloads.el ends here
