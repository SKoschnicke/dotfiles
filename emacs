(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
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
(add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

; init installed packages
; this should avoid "definition void" errors on startup
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agenda and orgmode config

(setq diary-file "~/Dropbox/diary")

(add-to-list 'load-path "~/org-mode/lisp")

(setq org-agenda-files (list "~/documents/promotion/gedanken.org"
                             "~/Dropbox/uni/uni-plan.org"
                             "~/Dropbox/privat.org"
                             "~/Dropbox/notes.org"
                             "~/Dropbox/gfxpro.org"
                             "~/Dropbox/isavision.org"
                             "~/Dropbox/pav-plan.org"
                             "~/Dropbox/getdigital.org"
                             "~/documents/promotion/qantrade.org"
                             )
      org-agenda-include-all-todo t
      org-agenda-include-diary t
      org-agenda-log-mode-items (list 'closed 'clock 'state)
      org-log-done t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-agenda-span 21
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
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
    (C . t)
   )
)

(load "~/.emacs.d/plugins/cdlatex.el")
(require 'cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setq org-todo-keywords
  '((sequence "TODO" "WAITING" "|" "DONE")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; latex export

(require 'ox-latex)
(setq ieeetran-class
      '("IEEEtran"
        "\\documentclass[11pt]{IEEEtran}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq article-class
      '("article"
        "\\documentclass[11pt]{article}"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq beamer-class
  '("beamer"
"\\documentclass{beamer}
\\usepackage[german]{babel}
\\usepackage{listings}
\\usepackage{color}

\\definecolor{red}{rgb}{0.6,0,0} % for strings
\\definecolor{green}{rgb}{0.25,0.5,0.35} % comments
\\definecolor{purple}{rgb}{0.5,0,0.35} % keywords
\\definecolor{docblue}{rgb}{0.25,0.35,0.75} % doc
 
\\lstset{basicstyle=\\small\\ttfamily,
keywordstyle=\\color{purple},
stringstyle=\\color{red},
commentstyle=\\color{green},
morecomment=[s][\\color{docblue}]{/**}{*/},
numbers=left,
numberstyle=\\tiny\\color{gray},
stepnumber=1,
numbersep=10pt,
tabsize=2,
showspaces=false,
showstringspaces=false,
otherkeywords={define,include,\\#}}
\\usetheme{hsrm}
     [NO-DEFAULT-PACKAGES]
     [NO-PACKAGES]"
        ("\\section{%s}" . "\\section*{%s}")
        ("\\subsection{%s}" . "\\subsection*{%s}")
        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        ("\\paragraph{%s}" . "\\paragraph*{%s}")
        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes ieeetran-class t)
(add-to-list 'org-latex-classes article-class t)
(add-to-list 'org-latex-classes beamer-class t)

(add-to-list 'org-latex-classes
  '("djcb-org-article"
"\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx} 
\\usepackage{hyperref} 
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Gentium}
\\setromanfont [BoldFont={Gentium Basic Bold},
                ItalicFont={Gentium Basic Italic}]{Gentium Basic}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'ox-beamer)

(setq org-latex-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(eval-after-load "org"
  '(progn
     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(setq org-export-latex-hyperref-format "\\ref{%s}")
(setq org-latex-listings t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org mode bibtex integration
(defun my-rtcite-export-handler (path desc format)
  (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc) 
                   (equal 0 (search "rtcite:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search))))))


(require 'org)

(org-add-link-type "rtcite" 
                   'org-bibtex-open
                   'my-rtcite-export-handler)

; function to insert code block in org-mode
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "javascript")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

; key binding for above
(add-hook 'org-mode-hook '(lambda ()
                            ;; turn on flyspell-mode by default
                            (flyspell-mode 1)
                            ;; C-TAB for expanding
                            (local-set-key (kbd "C-<tab>")
                                           'yas/expand-from-trigger-key)
                            ;; keybinding for editing source code blocks
                            (local-set-key (kbd "C-c s e")
                                           'org-edit-src-code)
                            ;; keybinding for inserting code blocks
                            (local-set-key (kbd "C-c s i")
                                           'org-insert-src-block)
                            ))

; enable syntax highlighting in soruce blocks
(setq org-src-fontify-natively t)


; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/")
;(load-theme 'solarized-light t)
;(load-theme 'molokai t)
(load "~/.emacs.d/plugins/color-theme-molokai.el")
(color-theme-molokai)

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
 '(ido-enable-flex-matching t)
 '(minimap-always-recenter t)
 '(minimap-hide-fringes t)
 '(minimap-update-delay 0.3)
 '(pivotal-api-token "28249de15fafe38c0351196088262df5"))
(if macosx-p
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "adobe" :family "Source Code Pro")))))
    ; else
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "adobe" :family "Source Code Pro")))))
)

; cycle through spelling dictionaries
(defun fd-switch-dictionary()
(interactive)
(let* ((dic ispell-current-dictionary)
   (change (if (string= dic "de_DE") "english" "de_DE")))
  (ispell-change-dictionary change)
  (message "Dictionary switched from %s to %s" dic change)
  ))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)

; insert spaces when pressing tab
(setq-default indent-tabs-mode nil)
; set indenting width
(setq tab-width 2)
; set other offsets to tab-width
(setq-default c-basic-offset tab-width)
(setq-default cperl-indent-level tab-width)

; put temp files in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; clipboard
; (transient-mark-mode 1)  ; Now on by default: makes the region act quite like the text "highlight" in many apps.
; (setq shift-select-mode t) ; Now on by default: allows shifted cursor-keys to control the region.
(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection
;; these will probably be already set to these values, leave them that way if so!
; (setf interprogram-cut-function 'x-select-text)
; (setf interprogram-paste-function 'x-cut-buffer-or-selection-value)
; You need an emacs with bug #902 fixed for this to work properly. It has now been fixed in CVS HEAD.
; it makes "highlight/middlebutton" style (X11 primary selection based) copy-paste work as expected
; if you're used to other modern apps (that is to say, the mere act of highlighting doesn't
; overwrite the clipboard or alter the kill ring, but you can paste in merely highlighted
; text with the mouse if you want to)
(setq select-active-regions t) ;  active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.
;; with this, doing an M-y will also affect the X11 clipboard, making emacs act as a sort of clipboard history, at
;; least of text you've pasted into it in the first place.
; (setq yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.

; disable toolbar
(tool-bar-mode -1)

; disable menubar (available as popup on C-mouse-3)
(menu-bar-mode -99)

; disable scrollbar
(scroll-bar-mode -1)

; show agenda on start
(setq inhibit-splash-screen t)
(org-agenda-list)
(delete-other-windows)

; enable evil mode
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)
; nerd commenter
(global-evil-leader-mode 1)

; helm
(require 'helm-cmd-t)
(global-set-key (kbd "M-p") 'helm-cmd-t)
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "adobe" :family "Source Code Pro")))))

; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
;(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
