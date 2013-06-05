;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; el-get, this should be at the top of the config
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(el-get 'sync)

;;; emacs own package system
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agenda and orgmode config
(setq org-agenda-files (list "~/documents/promotion/gedanken.org"
                             "~/Dropbox/uni/uni-plan.org"
                             "~/Dropbox/privat.org"
                             "~/Dropbox/notes.org"
                             "~/Dropbox/gfxpro.org"
                             "~/Dropbox/isavision.org"
                             "~/Dropbox/pav-plan.org")
      org-agenda-include-all-todo t
      org-agenda-include-diary t
      org-log-done t
)
(setq org-default-notes-file "~/Dropbox/notes.org")
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((latex . t)
    (R . t)
    (ruby . t)
    (haskell . t)
   )
)

; ESS emacs speaks statistics
(add-to-list 'load-path "~/.emacs.d/ess-site/lisp")
(require 'ess-site)


; solarized theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/")
(load-theme 'solarized-light t)

;;;;;;;;;;;;;;;;;;;;;;
;;; calendar and diary
(eval-after-load "calendar"
  '(european-calendar))
(setq number-of-diary-entries 5
      mark-diary-entries-in-calendar t
      calendar-offset -1
      calendar-location-name "Kiel"
      calendar-latitude 54.33
      calendar-longitude 10.13
      calendar-time-display-form '(24-hours ":" minutes
                                            (if time-zone " (")
                                            time-zone
                                            (if time-zone ")"))
      calendar-holidays '((holiday-fixed 01 01 "Gesetzlicher Feiertag (Neujahr)")
                          (holiday-fixed 05 01 "Gesetzlicher Feiertag (Maifeiertag)")
                          (holiday-fixed 10 03 "Gesetzlicher Feiertag (Tag der Deutschen Einheit)")
                          (holiday-fixed 12 25 "Gesetzlicher Feiertag (1. Weihnachtstag)")
                          (holiday-fixed 12 26 "Gesetzlicher Feiertag (2. Weihnachtstag)")
                          (holiday-easter-etc -2 "Gesetzlicher Feiertag (Karfreitag)")
                          (holiday-easter-etc  1 "Gesetzlicher Feiertag (Ostermontag)")
                          (holiday-easter-etc 39 "Gesetzlicher Feiertag (Christi Himmelfahrt)")
                          (holiday-easter-etc 50 "Gesetzlicher Feiertag (Pfingstmontag)")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("007b69ffec046a5842e34fea287b23c49175dfd6c6d5a0d9cdf150a2e8a8979f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(ido-enable-flex-matching t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "adobe" :family "Source Code Pro")))))

; insert spaces when pressing tab
(setq-default indent-tabs-mode nil)
; set indenting width
(setq tab-width 2)
; set other offsets to tab-width
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

; put temp files in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
; disable toolbar
(tool-bar-mode -1)

; disable menubar
(menu-bar-mode -1)

; disable scrollbar
(scroll-bar-mode -1)

; show agenda on start
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

