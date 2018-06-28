;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(csv
     (go :variables
         go-use-gometalinter t
         go-tab-width 2)
     helm
     php
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     typography
     emoji
     emacs-lisp
     git
     github
     (org :variables
          org-enable-github-support t)
     evil-snipe
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-use-unicode-arrows t
            scala-auto-start-ensime t)
     (ruby :variables
           ruby-version-manager 'rbenv)
     ruby-on-rails
     html
     javascript
     java
     haskell
     yaml
     asciidoc
     markdown
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     c-c++
     sql
     typescript
     python
     (gtags :variables gtags-enable-by-default t)
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp")
                                        ;spacemacs-purpose seems not to be available yet
     elixir
     (restclient :variables restclient-use-org t)
   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(yafolding key-chord helm-org-rifle string-inflection)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 9.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; Modeline theme
   dotspacemacs-mode-line-theme 'spacemacs
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (require 'helm-bookmark) ; workaround until https://github.com/syl20bnr/spacemacs/issues/9549 is fixed in stable
  (with-eval-after-load 'org

    (setq org-log-done t
          org-completion-use-ido t
          org-edit-timestamp-down-means-later t
          org-agenda-start-on-weekday nil
          org-agenda-start-day "-1d"
          org-agenda-span 14
          org-agenda-include-diary t
          org-agenda-window-setup 'current-window
          org-fast-tag-selection-single-key 'expert
          org-export-kill-product-buffer-when-displayed t
          org-pretty-entities t
          org-pretty-entities-include-sub-superscripts t
          org-agenda-log-mode-items (list 'clock 'state)
          org-agenda-start-with-log-mode t
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-tags-column 80
          org-enforce-todo-dependencies t
          org-agenda-dim-blocked-tasks t
          org-src-fontify-natively t)

                                        ; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
                                        ; Targets start with the file name - allows creating level 1 tasks
    (setq org-refile-use-outline-path (quote file))
                                        ; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
    (setq org-outline-path-complete-in-steps t)


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persistence-insinuate t)
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    ;; Show clock sums as hours and minutes, not "n days" etc.
    (setq org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

    ;; CUSTOM AGENDA
    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          (quote (("N" "Notes" tags "NOTE"
                   ((org-agenda-overriding-header "Notes")
                    (org-tags-match-list-sublevels t)))
                  ;; ("h" "Habits" tags-todo "STYLE=\"habit\""
                  ;;  ((org-agenda-overriding-header "Habits")
                  ;;   (org-agenda-sorting-strategy
                  ;;    '(todo-state-down effort-up category-keep))))
                  ("A" "Agenda"
                   ((agenda "" ((org-agenda-span 2) (org-agenda-start-day "0d")))
                    (tags-todo "-CANCELLED/!STARTED"
                               ((org-agenda-overriding-header "Started Tasks")
                                (org-tags-match-list-sublevels t)
                                (org-agenda-sorting-strategy
                                 '(todo-state-down effort-up category-keep))))
                    (tags-todo "-CANCELLED/!NEXT"
                               ((org-agenda-overriding-header "Next Tasks")
                                (org-tags-match-list-sublevels t)
                                (org-agenda-sorting-strategy
                                 '(todo-state-down effort-up category-keep))))
                    (tags "REFILE"
                          ((org-agenda-overriding-header "Tasks to Refile")
                           (org-tags-match-list-sublevels nil)))
                    (tags-todo "-CANCELLED+WAITING|HOLD/!"
                               ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                (org-tags-match-list-sublevels nil))
                               nil)))
                  ("t" todo-tree "TODO")
                  ("T" tags-todo "/TODO|NEXT" ((org-agenda-todo-ignore-scheduled 'future)
                                               (org-agenda-tags-todo-honor-ignore-options t)
                                               (org-agenda-todo-list-sublevels t)))
                  ("s" todo-tree "STARTED")
                  ("w" todo-tree "WAITING")
                  ("p" . "Project Agendas")
                  ("pp" "Perfavo" ((tags-todo "pav/STARTED" ((org-agenda-overriding-header "Started Tasks")))
                                   (tags-todo "pav/NEXT" ((org-agenda-overriding-header "Next Tasks")))
                  ))
                  ("ps" "Software-Challenge" ((tags-todo "swc/STARTED" ((org-agenda-overriding-header "Started Tasks")))
                                   (tags-todo "swc/NEXT" ((org-agenda-overriding-header "Next Tasks")))
                                   ))
                  ("ps" tags-todo "+swc")
                  ("pg" tags-todo "+gxp")
; Weekly Review block agenda
  ("r" . "Weekly Review")
  ("r1" "Get Clear: Collect loose materials and process Inbox"
    tags "+in+LEVEL>1"
    ((org-agenda-overriding-header "Inbox items to process:")
     (org-agenda-prefix-format "")))
  ("r2" "Get Current: Review Next Actions\n    Archive completed actions, review for further action steps."
    ((todo "DONE|DROPPED|COMPLETE" ((org-agenda-overriding-header "Done/Dropped Items (to archive):")
                                    (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                                    (org-agenda-sorting-strategy '(user-defined-up))))
     (tags-todo "-sm/NEXT" ((org-agenda-overriding-header "Next Actions:")
                            (org-agenda-sorting-strategy '(time-up category-up alpha-up))
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
     (tags-todo "-sm/NEXT" ((org-agenda-overriding-header "Scheduled Actions:")
                            (org-agenda-sorting-strategy '(time-up category-up alpha-up))
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled)))))
     ((org-agenda-prefix-format "%-12:c ")))
  ("r3" "Get Current: Review Previous Calendar"
    ((agenda "" ((org-agenda-start-day (concat "-" (number-to-string (+ 13 (nth 6 (decode-time)))) "d"))
                 (org-agenda-span (+ 14 (nth 6 (decode-time))))
                 (org-agenda-repeating-timestamp-show-all t)
                 (org-agenda-entry-types '(:deadline :timestamp :sexp)) ; show due tasks, meetings
                 (org-agenda-show-log t)
                 (org-agenda-prefix-format "%-12t% s")))))

                  )))

    ;; ;; CUSTOM AGENDA END

    ;;   (org-babel-do-load-languages
    ;;    'org-babel-load-languages
    ;;    '((R . t)
    ;;      (ditaa . t)
    ;;      (dot . t)
    ;;      (emacs-lisp . t)
    ;;      (gnuplot . t)
    ;;      (haskell . nil)
    ;;      (latex . t)
    ;;      (ledger . t)
    ;;      (ocaml . nil)
    ;;      (octave . t)
    ;;      (python . t)
    ;;      (ruby . t)
    ;;      (screen . nil)
    ;;      (sh . t)
    ;;      (sql . nil)
    ;;      (sqlite . t)))

    ; function to insert code block in org-mode
    (defun org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite" "javascript" "scala")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (when (string= system-name "sven-uni")
      (defconst my-org-file-path "~/SpiderOak Hive/org"))
    (when (string= system-name "palanthas")
      (defconst my-org-file-path "~/SpiderOak Hive/org"))
    (when (string-prefix-p "losarcum" system-name)
      (defconst my-org-file-path "~/mainsync/org"))
    (when (string= system-name "daltigoth")
      (defconst my-org-file-path "~/sync/org"))

    (when (file-accessible-directory-p my-org-file-path)
      (setq diary-file (concat my-org-file-path "/diary")))

    (when (file-accessible-directory-p my-org-file-path)
      (setq org-agenda-files (list my-org-file-path))
      (setq org-directory my-org-file-path)
      (setq org-default-notes-file (concat my-org-file-path "/refile.org"))
      (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
      (setq org-mobile-inbox-for-pull (concat my-org-file-path "/from-mobile.org"))
      (setq org-download-method 'attach))

    ;; I use C-c c to start capture mode
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "refile.org")
                   "* TODO %?\n%U\n%a\n")
                  ("c" "Cooldown" entry (file+datetree "gtd-daily-cooldown.org") (file "tmp-daily-cooldown.org"))
                  ("r" "Weekly Review" entry (file+datetree "gtd-weekly-reviews.org") (file "tmp-weekly-review.org"))
                  ("n" "note" entry (file "refile.org")
                   "* %? :NOTE:\n%U\n%a\n")
                  ("b" "bug" entry (file "bugs.org")
                   "* %?\n%U\n%a\n\n** Symptom\n\n** Ursache\n\n** Wie gefunden\n\n** Fix\n\n** Projekt\n\n** Commit\n\n** Bug selbst verursacht?\n\n** Zeit bis zum Fix\n\n** Lektionen\n")
                  ("g" "respond" entry (file "refile.org")
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
                  ("j" "Journal" entry (file+datetree "diary.org")
                   "* %?\n%U\n")
                  ("m" "Meeting" entry (file "refile.org")
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("p" "Phone call" entry (file "refile.org")
                   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit" entry (file "refile.org")
                   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                  ("B" "Brain" plain (function org-brain-goto-end)
                   "* %i%?" :empty-lines 1)
                  )))

    ;;; org mode beamer


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

    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -shell-escape %f"
            "xelatex -interaction nonstopmode -shell-escape %f")) ;; for multiple passes

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


    (org-add-link-type "rtcite"
                       'org-bibtex-open
                       'my-rtcite-export-handler)
    )

  ; This enables binding the custom agenda to keys and showing it on startup.
  ; Has to be defined outside of org mode hook, or else it would not be
  ; available until org mode was loaded by opening an org buffer.
  (defun org-agenda-show-mine (&optional arg)
    (interactive "P")
    (org-agenda arg "A"))

  ;;;;;;;;;;;; org-mode end

  ;;;;;;;;;;;;;;;;;;;;;;
  ;;; calendar and diary
  (add-hook 'calendar-load-hook
            (lambda ()
              (calendar-set-date-style 'european)
              (setq diary-number-of-entries 5
                    calendar-mark-diary-entries-flag t
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
                                        (holiday-easter-etc 50 "Gesetzlicher Feiertag (Pfingstmontag)")))))

  (setq-default git-magit-status-fullscreen t)

  ; use 2 spaces in web mode for everything (instead of 4)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq-default typescript-indent-level 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-indent-style 2)
  (setq-default typescript-indent-level 2)
  ; NOTE that indent when using beautify (SPC m =) is determined by the external
  ; .jsbeautifyrc file, see https://github.com/yasuyk/web-beautify

  ; never indent with tabs
  (setq-default indent-tabs-mode nil)

  ; how can the default config not set this?!
  (global-set-key (kbd "C-i") 'evil-jump-forward)

  ; DocView renders PDFs as PNGs to display them. The default resolution (DPI)
  ; for this makes the PDFs look rasterized on hiDPI displays.
  (setq-default doc-view-resolution 300)

  ; Reload document when it changes on disk
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;(define-key evil-insert-state-map <deletechar> 'evil-normal-state)
  ;(define-key evil-visual-state-map <deletechar> 'evil-normal-state)

  ; bind custom agenda to SPC-A
  (spacemacs/set-leader-keys "A" 'org-agenda-show-mine)

  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1)

  ; show custom agenda after start
  (add-hook 'after-init-hook 'org-agenda-show-mine)

  ;(setq rubocop-check-command "rbenv exec rubocop --format emacs")
  (setq rubocop-check-command "rubocop --format emacs")

  ; set simple shell to speed up / fix projectile, see
  ; https://github.com/syl20bnr/spacemacs/issues/4207
  (setq shell-file-name "/bin/sh")

  (require 'mu4e)
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300 ;; in seconds
        mu4e-compose-format-flowed t
        )

  (setq mu4e-sent-folder "/GFXpro/INBOX.Sent"
        mu4e-drafts-folder "/GFXpro/INBOX.Drafts"
        user-mail-address "s.koschnicke@gfxpro.com"
        smtpmail-local-domain "gfxpro.com"
        smtpmail-smtp-server "mail.jpberlin.de"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-auth-credentials "~/.netrc"
        message-send-mail-function 'smtpmail-send-it
        )

  (defvar my-mu4e-account-alist
    '(("GFXpro"
       (mu4e-sent-folder "/GFXpro/Saved Items")
       (mu4e-drafts-folder "/GFXpro/Drafts")
       (user-mail-address "s.koschnicke@gfxpro.com")
       (smtpmail-local-domain "gfxpro.com")
       (smtpmail-smtp-server "mail.jpberlin.de")
       (smtpmail-smtp-service 465)
       (smtpmail-stream-type ssl)
       (smtpmail-auth-credentials "~/.netrc"))
      ("Uni"
       (mu4e-sent-folder "/Uni/sent")
       (mu4e-drafts-folder "/Uni/drafts")
       (user-mail-address "svk@informatik.uni-kiel.de")
       (smtpmail-local-domain "informatik.uni-kiel.de")
       (smtpmail-smtp-server "mail.informatik.uni-kiel.de")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-auth-credentials "~/.netrc"))
      ("Privat"
       (mu4e-sent-folder "/Privat/INBOX.Sent")
       (mu4e-drafts-folder "/Privat/INBOX.Drafts")
       (mu4e-trash-folder "/Privat/INBOX.Trash")
       (mu4e-refile-folder "/Privat/INBOX.Archive")
       (user-mail-address "sven@koschnicke.de")
       (smtpmail-local-domain "koschnicke.de")
       (smtpmail-smtp-server "sslout.df.eu")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls)
       (smtpmail-auth-credentials "~/.netrc"))
      )
    )

  (defun my-mu4e-set-account ()
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  (require 'mu4e)
  (add-to-list 'mu4e-bookmarks
               '("flag:flagged" "Flagged messages" ?f)
  )
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; use ensime in java mode (requires installed sbt)
  ;(add-hook 'java-mode-hook 'scala/configure-ensime)
  ;(add-hook 'java-mode-hook 'scala/maybe-start-ensime)

  ;; to avoid "The TLS connection was non-properly terminated"
  ;; see https://github.com/syl20bnr/spacemacs/issues/6638
  (setq dotspacemacs-elpa-https nil)

  ;; needs scalastyle installed
  (setq-default flycheck-scalastylerc "~/development/pa/psi/conf/scalastyle_config.xml")

  ;; Does not work yet, have to use M-/
  ;(evil-define-key 'insert org-mode-map (kbd "TAB") 'hippie-expand)

  (with-eval-after-load 'helm-org-rifle
    (spacemacs/set-leader-keys "aos" 'helm-org-rifle))

  ;; Keybindings for string inflection package
  (define-key evil-normal-state-map (kbd "C-w C-c") 'string-inflection-ruby-style-cycle)

  ;; 2017-09-18 workaround for issue https://github.com/syl20bnr/spacemacs/issues/9549 may be removed soon
  (require 'helm-bookmark)

  (setq helm-ag-base-command "rg --vimgrep --no-heading")

  ;; automatically sync mobile org on start and stop of emacs
  (add-hook 'after-init-hook 'org-mobile-pull)
  (add-hook 'kill-emacs-hook 'org-mobile-push)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(exec-path
   (quote
    ("/home/svk/.rbenv/shims/" "/usr/local/sbin/" "/usr/local/bin/" "/usr/bin/" "/opt/android-sdk/platform-tools/" "/opt/android-sdk/tools/" "/usr/lib/jvm/default/bin/" "/usr/bin/site_perl/" "/usr/bin/vendor_perl/" "/usr/bin/core_perl/" "/usr/lib/emacs/25.1/x86_64-unknown-linux-gnu/" "/home/svk/.gem/ruby/2.3.0/bin" "/home/svk/.rbenv/versions/2.3.1/bin")))
 '(flycheck-disabled-checkers (quote (ruby ruby-rubylint javascript-jshint)))
 '(haskell-tags-on-save t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(mu4e-view-show-addresses t)
 '(mu4e-view-show-images t)
 '(org-babel-load-languages (quote ((ruby . t) (emacs-lisp . t))))
 '(org-list-allow-alphabetical t)
 '(package-selected-packages
   (quote
    (org-category-capture string-inflection winum fuzzy flycheck-credo helm-org-rifle eclim phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode ob-elixir flycheck-mix alchemist elixir-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic alert log4e gntp markdown-mode simple-httpd json-snatcher json-reformat parent-mode haml-mode gitignore-mode fringe-helper git-gutter+ marshal logito pcache pkg-info epl flx evil goto-chg f diminish web-completion-data dash-functional tern pos-tip ghc s bind-map bind-key packed markup-faces avy popup package-build powerline rake spinner org hydra scala-mode auto-complete company iedit highlight git-gutter request skewer-mode gh pcre2el helm-gtags ggtags minitest multiple-cursors hide-comnt anzu undo-tree flyspell-correct ht inflections inf-ruby sql-indent tide typescript-mode pug-mode sbt-mode smartparens helm helm-core haskell-mode flycheck yasnippet magit magit-popup git-commit with-editor async projectile js2-mode company-quickhelp yaml-mode yafolding xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package typo toc-org tagedit spacemacs-theme spaceline solarized-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa projectile-rails popwin persp-mode paradox ox-gfm orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file noflet neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode key-chord json-mode js2-refactor js-doc jade-mode intero info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word company-web company-tern company-statistics company-ghci company-ghc company-emoji company-cabal company-c-headers column-enforce-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(rbenv-modeline-function (quote rbenv--modeline-plain)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(variable-pitch ((t (:weight semi-light :family "Fira Sans")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources '("~/.authinfo.gpg" "~/.netrc"))
 '(c-basic-offset 2)
 '(exec-path
   '("/home/svk/.rbenv/shims/" "/usr/local/sbin/" "/usr/local/bin/" "/usr/bin/" "/opt/android-sdk/platform-tools/" "/opt/android-sdk/tools/" "/usr/lib/jvm/default/bin/" "/usr/bin/site_perl/" "/usr/bin/vendor_perl/" "/usr/bin/core_perl/" "/usr/lib/emacs/25.1/x86_64-unknown-linux-gnu/" "/home/svk/.gem/ruby/2.3.0/bin" "/home/svk/.rbenv/versions/2.3.1/bin"))
 '(flycheck-disabled-checkers '(ruby ruby-rubylint javascript-jshint))
 '(haskell-tags-on-save t)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(mu4e-view-show-addresses t)
 '(mu4e-view-show-images t)
 '(org-babel-load-languages '((ruby . t) (emacs-lisp . t)))
 '(org-list-allow-alphabetical t)
 '(package-selected-packages
   '(zenburn-theme org-mime org-brain meghanada magithub ghub+ apiwrap importmagic impatient-mode helm-xref groovy-mode flycheck-rtags emojify dante counsel-projectile counsel swiper ivy ac-php-core window-purpose imenu-list rtags ghub org-category-capture string-inflection winum fuzzy flycheck-credo helm-org-rifle eclim phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode ob-elixir flycheck-mix alchemist elixir-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic alert log4e gntp markdown-mode simple-httpd json-snatcher json-reformat parent-mode haml-mode gitignore-mode fringe-helper git-gutter+ marshal logito pcache pkg-info epl flx evil goto-chg f diminish web-completion-data dash-functional tern pos-tip ghc s bind-map bind-key packed markup-faces avy popup package-build powerline rake spinner org hydra scala-mode auto-complete company iedit highlight git-gutter request skewer-mode gh pcre2el helm-gtags ggtags minitest multiple-cursors hide-comnt anzu undo-tree flyspell-correct ht inflections inf-ruby sql-indent tide typescript-mode pug-mode sbt-mode smartparens helm helm-core haskell-mode flycheck yasnippet magit magit-popup git-commit with-editor async projectile js2-mode company-quickhelp yaml-mode yafolding xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package typo toc-org tagedit spacemacs-theme spaceline solarized-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa projectile-rails popwin persp-mode paradox ox-gfm orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file noflet neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode key-chord json-mode js2-refactor js-doc jade-mode intero info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word company-web company-tern company-statistics company-ghci company-ghc company-emoji company-cabal company-c-headers column-enforce-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(rbenv-modeline-function 'rbenv--modeline-plain))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(variable-pitch ((t (:weight semi-light :family "Fira Sans")))))
)
