;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(nginx
     protobuf
     rust
     ansible
     react
     csv
     (go :variables
         go-use-gometalinter t
         go-tab-width 2
         go-use-gocheck-for-testing t
         go-format-before-save t
         go-use-golangci-lint t
         go-backend 'lsp)
     ;ivy
     helm
     (php :variables
          php-backend 'lsp-deferred)
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-complete-with-key-sequence-delay 0.1
                      )
     typography
     ;emoji ;; breaks org mode repeat
     emacs-lisp
     git
     github
     (org :variables
          org-enable-github-support t
          org-enable-roam-support nil)
     evil-snipe
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     shell-scripts
     (scala :variables
            scala-backend 'scala-metals)
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-backend 'lsp-deferred)
     ruby-on-rails
     html
     javascript
     tern
     (java :variables
           java-backend 'lsp-deferred)
     (kotlin :variables
           kotlin-backend 'lsp-deferred)
     ;haskell ;; currently results in emacs freezing because it waits for some stack command
     yaml
     asciidoc
     markdown
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     sql
     (typescript :variables
                 company-tooltip-align-annotations t)
     python
     (gtags :variables gtags-enable-by-default t)
     (mu4e :variables
           mu4e-installation-path "/usr/share/emacs/site-lisp")
;     spacemacs-purpose
     elixir
     (restclient :variables restclient-use-org t)
     (elfeed :variables rmh-elfeed-org-files (list "~/SpiderOak Hive/org/newsfeeds.org"))
     (lsp :variables lsp-treemacs-sync-mode 1)
     dash ; requires zeal installed on the machine
     plantuml
   )
   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(yafolding
     key-chord
     string-inflection
     vue-mode
     zpresent
     company-box
     org-randomnote
     org-ql
     helm-org-rifle
     helm-org-ql
                                        ;org-variable-pitch ;; seen no difference to using mixed-pitch
     mixed-pitch
     edit-server
     org-sidebar
     )


   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 8 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         spacemacs-dark
                         spacemacs-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Victor Mono"
                               :size 10.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts t

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first.")


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump.")


(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (require 'window-purpose) ; workaround until https://github.com/bmag/emacs-purpose/issues/158 is fixed

  (when (string= system-name "sven-uni")
    (defconst my-sync-path "~/SpiderOak Hive"))
  (when (string= system-name "palanthas")
    (defconst my-sync-path "~/SpiderOak Hive"))
  (when (string-prefix-p "losarcum" system-name)
    (defconst my-sync-path "~/sync"))
  (when (string= system-name "daltigoth")
    (defconst my-sync-path "~"))

  (defconst my-org-file-path (concat my-sync-path "/org"))
  (setq org-roam-directory (concat my-org-file-path "/roam"))

  (setq browse-url-browser-function 'browse-url-firefox)

  ;(global-company-mode t)

  (with-eval-after-load 'company-box
    (add-hook 'company-mode-hook 'company-box-mode)
  )

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; use imagemagick, if available (for displaying inline images, e.g. in mu4e)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (evil-leader/set-key "ah" 'harvest)
;; (add-hook 'org-clock-in-hook 'harvest)
;; (add-hook 'org-clock-out-hook 'harvest-clock-out)

  ; seems to make problems with tab complete
;  (setq-default evil-escape-key-sequence "kj")
;  (setq-default evil-escape-delay 0.2)

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  (setq mmm-submode-decoration-level 0)

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
          org-agenda-dim-blocked-tasks nil
          org-src-fontify-natively t
          org-id-track-globally t
          org-agenda-columns-add-appointments-to-effort-sum t
          org-agenda-default-appointment-duration nil  ; this also makes all scheduled items last for this duration instead of taking the efford
          org-id-link-to-org-use-id t
          )

    ; Refile targets include this file (5 levels deep) and any file contributing to the agenda (only 1 level, top level headlines)
    (setq org-refile-targets (quote ((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1))))
                                        ; Targets start with the file name - allows creating level 1 tasks
    (setq org-refile-use-outline-path (quote file))
                                        ; Works only with helm and ivy when nil
    (setq org-outline-path-complete-in-steps nil)

    (setq org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "MAYBE(m)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))


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

    ;(setq org-agenda-overriding-columns-format "%TODO %7EFFORT{:} %7CLOCKSUM_T(SPENT){:} %3PRIORITY     %100ITEM 100%TAGS")
    ;; CUSTOM AGENDA
    ;; Custom agenda command definitions

    (defun my/org-agenda-skip-scheduled ()
      (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

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
                  ("R" tags "REFILE")
                  ("o" tags-todo "-pav-swc-gxp/TODO")
                  ("p" . "Project Agendas")


        ;;           (mapcar (lambda (l)
        ;;                     (let ((first-member (car l))
        ;;                           (second-member (cadr l))
        ;;                           (third-member (nth 2 l) ))
        ;;                       `(,first-member ,second-member ;;First and second
        ;;                                       ( (tags-todo ,(concat third-member "/STARTED")
        ;;                                                     `((org-agenda-overriding-header "Started Tasks")))
        ;;                                         (tags-todo ,(concat third-member "/NEXT")
        ;;                                                     `((org-agenda-overriding-header "Next Tasks")))
        ;;                                         (tags-todo ,(concat third-member "/WAITING")
        ;;                                                     `((org-agenda-overriding-header "Waiting")))
        ;;                                         (tags-todo ,(concat third-member "/TODO")
        ;;                                                     `((org-agenda-overriding-header "Unscheduled Tasks")
        ;;                                                       (org-agenda-skip-function 'my/org-agenda-skip-scheduled)))))))
        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                   '( ("pp" "Perfavo" "pav") ("ps" "Software-Challenge" "swc")))


                  ("pp" "Perfavo" ((tags-todo "pav/STARTED" ((org-agenda-overriding-header "Started Tasks")))
                                   (tags-todo "pav/NEXT" ((org-agenda-overriding-header "Next Tasks")))
                                   (tags-todo "pav/WAITING" ((org-agenda-overriding-header "Waiting")))
                                   (tags-todo "pav/TODO" ((org-agenda-overriding-header "Unscheduled Tasks") (org-agenda-skip-function 'my/org-agenda-skip-scheduled)))
                  ))
                  ("ps" "Software-Challenge" ((tags-todo "swc/STARTED" ((org-agenda-overriding-header "Started Tasks")))
                                   (tags-todo "swc/NEXT" ((org-agenda-overriding-header "Next Tasks")))
                                   (tags-todo "swc/WAITING" ((org-agenda-overriding-header "Waiting")))
                                   (tags-todo "swc/TODO" ((org-agenda-overriding-header "Unscheduled Tasks") (org-agenda-skip-function 'my/org-agenda-skip-scheduled)))
                                   ))
                  ("pg" "GFXpro" ((tags-todo "gxp/STARTED" ((org-agenda-overriding-header "Started Tasks")))
                                              (tags-todo "gxp/NEXT" ((org-agenda-overriding-header "Next Tasks")))
                                              (tags-todo "gxp/WAITING" ((org-agenda-overriding-header "Waiting")))
                                              (tags-todo "gxp/TODO" ((org-agenda-overriding-header "Unscheduled Tasks") (org-agenda-skip-function 'my/org-agenda-skip-scheduled)))
                                              ))
; Weekly Review block agenda
  ("r" . "Weekly Review")
  ("r1" "Get Clear: Collect loose materials and process Inbox"
    tags "+in+LEVEL>1"
    ((org-agenda-overriding-header "Inbox items to process:")
     (org-agenda-prefix-format "")))
  ("r2" "Get Current: Review Next Actions\n    Archive completed actions, review for further action steps."
    (;(todo "DONE|DROPPED|COMPLETE" ((org-agenda-overriding-header "Done/Dropped Items (to archive):")
     ;                               (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
     ;                               (org-agenda-sorting-strategy '(user-defined-up))))
    ; (above gives error because cmp-date-property doesn't exist)
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

  ("j" "Planning Table"
    agenda ""
    ((org-agenda-overriding-header "")
     (org-agenda-span 1)
     (org-agenda-use-time-grid nil)
     (org-agenda-view-columns-initially t)
     (org-columns-default-format-for-agenda
      "%11AGENDA_TIME(When) %4TODO(Type) %40ITEM(What) %5AGENDA_DURATION(Takes){:}")
     ;; do not show wardings, overdue and overscheduled
     (org-scheduled-past-days 0)
     (org-deadline-past-days 0)
     (org-deadline-warning-days 0)
     ;; skip finished entries
     (org-agenda-skip-deadline-if-done t)
     (org-agenda-skip-scheduled-if-done t)))

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
                  ("c" "Cooldown" entry (file+datetree "gtd-daily-cooldown.org") (file "tmp-daily-cooldown.org") :immediate-finish t :jump-to-captured t :clock-in t)
                  ("r" "Weekly Review" entry (file+datetree "gtd-weekly-reviews.org") (file "tmp-weekly-review.org") :immediate-finish t :jump-to-captured t :clock-in t)
                  ("n" "note" entry (file "refile.org")
                   "* %? :NOTE:\n%U\n%a\n")
                  ("b" "bug" entry (file "bugs.org")
                   "* %?\n%U\n%a\n\n** Symptom\n\n** Ursache\n\n** Wie gefunden\n\n** Fix\n\n** Projekt\n\n** Commit\n\n** Bug selbst verursacht?\n\n** Zeit bis zum Fix\n\n** Lektionen\n")
                  ("a" "automate" entry (file "refile.org")
                   "* %? :AUTO:\n%U\n%a\n")
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
                  ("f" "Frontastic Weekly" entry (file+olp "gxp-frontastic.org" "Meetings" "All-Hands Weekly") (file "tmp-frontastic-meeting.org"))
                  ("F" "Frontastic Report" entry (file+headline "gxp-frontastic.org" "Wochenbericht Frontastic") (file "tmp-frontastic-weekly-report.org"))
                  )))

    ;;; org mode beamer


    (setq org-latex-pdf-process
          '("xelatex -interaction nonstopmode -shell-escape %f"
            "xelatex -interaction nonstopmode -shell-escape %f")) ;; for multiple passes

    (setq org-export-latex-hyperref-format "\\ref{%s}")
    (setq org-latex-listings t)

    ;; insert creation date into all headings
    (defun insert-created-date(&rest ignore)
      (insert "\n")
      (org-insert-time-stamp (current-time) 't 't)
      (org-back-to-heading) ; in org-capture, this folds the entry; when inserting a heading, this moves point back to the heading line
      (move-end-of-line()) ; when inserting a heading, this moves point to the end of the line
      )

    ; add to the org-capture hook to insert a creation date
    ; deactivated because it does not work well with the org-capture function (there, the date is inserted twice)
    ;(add-hook 'org-capture-before-finalize-hook
    ;          #'insert-created-date
    ;          )

    (advice-add 'org-insert-heading :after #'insert-created-date)


  ) ;; org-mode eval after load end

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

  ;; Enable JavaScript completion between <script>...</script> etc.
  ;; company-tern seems to be unavailable
;  (advice-add 'company-tern :before
;              #'(lambda (&rest _)
;                  (if (equal major-mode 'web-mode)
;                      (let ((web-mode-cur-language
;                             (web-mode-language-at-pos)))
;                        (if (or (string= web-mode-cur-language "javascript")
;                                (string= web-mode-cur-language "jsx"))
;                            (unless tern-mode (tern-mode))
;                          (if tern-mode (tern-mode -1)))))))

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

  (with-eval-after-load 'mu4e

    (setq mu4e-maildir "~/Mail"
          mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 300 ;; in seconds
          mu4e-compose-format-flowed t
          mu4e-context-policy 'pick-first
          mu4e-compose-context-policy 'ask
          )

    (defun my-mu4e-contact-filter-function (addr)
      (if (string-match-p
           (concat "\\(?:no-?reply\\|.*\\.unwanted\\.domain\\.com\\|"
                   "^vgr@informatik\\.uni-kiel\\.d$\\)")
           addr)
          nil
        addr))
    (setq mu4e-contact-process-function 'my-mu4e-contact-filter-function)

    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "Privat"
               :enter-func (lambda () (mu4e-message "Switch to the Private context"))
               ;; leave-func not defined
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg
                                                                   :to "sven@koschnicke.de")))
               :vars '(  ( user-mail-address      . "sven@koschnicke.de"  )
                         ( user-full-name     . "Sven Koschnicke" )
                         ( mu4e-compose-signature .
                                                  (concat
                                                   "Viele Gruesse\n"
                                                   "  Sven Koschnicke\n"))
                         (mu4e-sent-folder . "/Privat/INBOX.Sent")
                         (mu4e-drafts-folder . "/Privat/INBOX.Drafts")
                         (mu4e-trash-folder . "/Privat/INBOX.Trash")
                         (mu4e-refile-folder . "/Privat/INBOX.Archive")
                         (smtpmail-local-domain . "koschnicke.de")
                         (smtpmail-smtp-server . "sslout.df.eu")
                         (smtpmail-smtp-service . 465)
                         (smtpmail-stream-type . ssl)
                         (smtpmail-auth-credentials . "~/.netrc")))
             ,(make-mu4e-context
               :name "GFXpro"
               :enter-func (lambda () (mu4e-message "Switch to the GFXpro context"))
               ;; leave-fun not defined
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg
                                                                   :to "s.koschnicke@gfxpro.com")))
               :vars '(  ( user-mail-address      . "s.koschnicke@gfxpro.com" )
                         ( user-full-name     . "Sven Koschnicke" )
                         ( mu4e-compose-signature .
                                                  (concat
                                                   "Viele Gruesse\n"
                                                   "  Sven Koschnicke\n"))
                         (mu4e-sent-folder . "/GFXpro/INBOX.Sent")
                         (mu4e-drafts-folder . "/GFXpro/INBOX.Drafts")
                         (mu4e-trash-folder . "/GFXpro/INBOX.Trash")
                         (mu4e-refile-folder . "/GFXpro/INBOX.Archives")
                         (smtpmail-local-domain . "gfxpro.com")
                         (smtpmail-smtp-server . "mail.jpberlin.de")
                         (smtpmail-smtp-service . 465)
                         (smtpmail-stream-type . ssl)
                         (smtpmail-auth-credentials . "~/.netrc"))
               )
             ,(make-mu4e-context
               :name "Uni"
               :enter-func (lambda () (mu4e-message "Switch to the Uni context"))
               ;; leave-fun not defined
               :match-func (lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg
                                                                   :to "svk@informatik.uni-kiel.de")))
               :vars '(  ( user-mail-address      . "svk@informatik.uni-kiel.de" )
                         ( user-full-name     . "Sven Koschnicke" )
                         ( mu4e-compose-signature .
                                                  (concat
                                                   "Viele Gruesse\n"
                                                   "  Sven Koschnicke\n"))
                         (mu4e-sent-folder . "/Uni/sent")
                         (mu4e-drafts-folder . "/Uni/drafts")
                         (mu4e-trash-folder . "/Uni/trash")
                         (mu4e-refile-folder . "/Uni/archive")
                         (user-mail-address . "svk@informatik.uni-kiel.de")
                         (smtpmail-local-domain . "informatik.uni-kiel.de")
                         (smtpmail-smtp-server . "mailin.informatik.uni-kiel.de")
                         (smtpmail-smtp-service . 587)
                         (smtpmail-stream-type . starttls)
                         (smtpmail-auth-credentials . "~/.netrc"))
               )
             )
          )

    ;; this is not working yet (netrc-parse seems not to be available anymore)
    ;; try python .offlineimap.py in the shell
    (defun offlineimap-get-password (host port)
      (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
             (hostentry (netrc-machine netrc host port port)))
        (when hostentry (netrc-get hostentry "password"))))

    (add-to-list 'mu4e-bookmarks
                 '("flag:flagged" "Flagged messages" ?f)
                 )
    ;(require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text)
    (add-hook 'mu4e-view-mode-hook
              (lambda()
                ;; try to emulate some of the eww key-bindings
                (local-set-key (kbd "<tab>") 'shr-next-link)
                (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  )
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

  ;; Keybindings for string inflection package
  (define-key evil-normal-state-map (kbd "C-w C-c") 'string-inflection-ruby-style-cycle)

  ;; automatically sync mobile org on start and stop of emacs
  ;(add-hook 'after-init-hook 'org-mobile-pull)
  ;(add-hook 'kill-emacs-hook 'org-mobile-push)

  (defun my-php-mode-setup ()
    "My PHP-mode hook."
    ;(require 'flycheck-phpstan)
    (flycheck-mode t)
    (setq flycheck-checker-error-threshold 5000)
    (setq phpstan-working-dir ".")
    ;(flycheck-add-next-checker 'phpstan 'php-phpcs)
  )

  (add-hook 'php-mode-hook 'my-php-mode-setup)

  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-auto-save-persps-to-their-file-before-kill t
        persp-auto-resume-time 1
        persp-auto-save-fname "autosave"
        persp-auto-save-opt 1
        persp-nil-hidden t
        persp-nil-name "Default"
        persp-save-dir (concat my-sync-path "/emacs-perspectives/"))

  (spacemacs|define-custom-layout "Start layout"
    :binding "s"
    :body
    (find-file (concat my-org-file-path "/refile.org"))
    (split-window-right-and-focus) ;; Split and move to the right
    (org-agenda-show-mine) ;; load agenda in upper right window
    (split-window-below-and-focus) ;; Split the right side into two and move focus
    (mu4e) ;; start mail in lower right part
    (winum-select-window-2) ;; Move focus back to agenda
  )

  (spacemacs|define-custom-layout "Cooldown layout"
    :binding "c"
    :body
    (find-file (concat my-org-file-path "/gtd-daily-cooldown.org"))
    (split-window-right)
    (org-agenda-show-mine)
    (split-window-below-and-focus)
    (org-todo-list "STARTED")
    (split-window-below-and-focus)
    (org-todo-list "NEXT")
    (winum-select-window-1)
    (split-window-below-and-focus)
    (org-ql-view-recent-items 3)
    (winum-select-window-1) ;; Move focus back to agenda
  )

  (defun good-morning ()
    "Setup windows in the morning"
    (interactive)
    (persp-switch (concat my-org-file-path "/"))
    (spacemacs/window-split-single-column t)
    (find-file (concat my-org-file-path "/gtd-daily-cooldown.org"))
    (split-window-right-and-focus) ;; Split and move to the right
    (org-agenda-show-mine) ;; load agenda in upper right window
    (split-window-below-and-focus) ;; Split the right side into two and move focus
    (mu4e) ;; start mail in lower right part
    (winum-select-window-2) ;; Move focus back to agenda
    )

  (defun well-done ()
    "Setup windows for cooldown"
    (interactive)
    (persp-switch (concat my-org-file-path "/"))
    (spacemacs/window-split-single-column t)
    (find-file (concat my-org-file-path "/gtd-daily-cooldown.org"))
    (split-window-right-and-focus) ;; Split and move to the right
    (org-agenda-show-mine) ;; load agenda in upper right window
    ;; (split-window-below-and-focus)
    ;; (org-todo-list "STARTED")
    ;; (split-window-below-and-focus)
    ;; (org-todo-list "NEXT")
    ;; (winum-select-window-1)
    (split-window-below-and-focus)
    (org-ql-view-recent-items 3)
    (winum-select-window-1)
    )


  (evil-leader/set-key "am" 'good-morning)
  (evil-leader/set-key "aw" 'well-done)

  ;(evil-leader/set-key "aor" 'counsel-org-goto)
  ;(evil-leader/set-key "aos" 'counsel-org-goto-all)

  (add-to-list 'projectile-project-root-files "go.mod")
  (projectile-register-project-type 'go '("go.mod")
                                    :project-file "go.mod"
                                    :test-suffix "_test")

  (add-hook 'org-mode-hook 'mixed-pitch-mode)
;  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'after-init-hook 'org-roam-mode)

  (use-package edit-server
    :ensure t
    :commands edit-server-start
    :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start))))
    :config (setq edit-server-new-frame-alist
                  '((name . "Edit with Emacs FRAME")
                    (top . 200)
                    (left . 200)
                    (width . 80)
                    (height . 25)
                    (minibuffer . t)
                    (menu-bar-lines . t)
                    (window-system . x))))

  (setq create-lockfiles nil) ; webpack can't handle lockfiles
  (setq counsel-dash-common-docsets '("Javascript" "HTML" "Go" "PHP"))
  (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

  ;; ;; Enable the www ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))

  ;; ;; Enable ligatures in programming modes
  ;; (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
  ;;                                      ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
  ;;                                      "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
  ;;                                      "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
  ;;                                      "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
  ;;                                      "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
  ;;                                      "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
  ;;                                      "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
  ;;                                      "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
  ;;                                      "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  ;; (global-ligature-mode 't)

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

  ;; make highlighting symbol under cursor less annoying
  ;; https://github.com/syl20bnr/spacemacs/issues/14880
  ;; https://github.com/jcs-elpa/auto-highlight-symbol/issues/7
  ;; can be removed after https://github.com/syl20bnr/spacemacs/pull/14892 is merged
  ;; (require 'auto-highlight-symbol)
  ;; (set-face-attribute 'ahs-plugin-defalt-face nil
  ;;                     :background nil
  ;;                     :foreground nil)

  ;; (set-face-attribute 'ahs-plugin-default-face nil
  ;;                     :background nil
  ;;                     :foreground nil)

  ;; (set-face-attribute 'ahs-face nil
  ;;                     :background nil
  ;;                     :foreground nil)

  ;; (set-face-attribute 'ahs-face-unfocused nil
  ;;                     :background nil
  ;;                     :foreground nil)

  ;; (set-face-attribute 'ahs-plugin-default-face-unfocused nil
  ;;                     :background nil
  ;;                     :foreground nil)
)

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
 '(auth-source-save-behavior nil)
 '(c-basic-offset 2)
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(evil-want-Y-yank-to-eol t)
 '(exec-path
   '("/usr/local/sbin/" "/usr/local/bin/" "/usr/bin/" "/opt/android-sdk/platform-tools/" "/opt/android-sdk/tools/" "/usr/lib/jvm/default/bin/" "/usr/bin/site_perl/" "/usr/bin/vendor_perl/" "/usr/bin/core_perl/" "/home/sven/.rbenv/shims/"))
 '(flycheck-disabled-checkers '(ruby ruby-rubylint javascript-jshint))
 '(flycheck-phpstan-executable "/home/sven/.config/composer/vendor/bin/phpstan")
 '(haskell-tags-on-save t)
 '(highlight-parentheses-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-go-gopls-server-path "/home/sven/go/bin/gopls")
 '(lsp-gopls-server-path "/home/sven/go/bin/gopls")
 '(mu4e-view-show-addresses t)
 '(mu4e-view-show-images t)
 '(org-agenda-files nil)
 '(org-babel-load-languages '((ruby . t) (emacs-lisp . t)))
 '(org-list-allow-alphabetical t)
 '(org-modules '(org-habit org-protocol id))
 '(package-selected-packages
   '(plantuml-mode nginx-mode protobuf-mode dap-mode bui restclient-helm org-mime ob-restclient ob-http jinja2-mode hackernews go-guru go-eldoc flycheck-gometalinter transient org-category-capture string-inflection winum fuzzy flycheck-credo helm-org-rifle eclim phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode ob-elixir alchemist elixir-mode yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic alert log4e gntp markdown-mode simple-httpd json-snatcher json-reformat parent-mode haml-mode gitignore-mode fringe-helper git-gutter+ marshal logito pcache pkg-info epl flx evil goto-chg f diminish web-completion-data dash-functional tern pos-tip ghc s bind-map bind-key packed markup-faces avy popup package-build powerline rake spinner org hydra scala-mode auto-complete company iedit highlight git-gutter request skewer-mode gh pcre2el helm-gtags ggtags minitest multiple-cursors hide-comnt anzu undo-tree flyspell-correct ht inflections inf-ruby sql-indent tide typescript-mode pug-mode sbt-mode smartparens helm helm-core haskell-mode flycheck yasnippet magit magit-popup git-commit with-editor async projectile js2-mode company-quickhelp yaml-mode yafolding xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package typo toc-org tagedit spacemacs-theme spaceline solarized-theme smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters quelpa projectile-rails popwin persp-mode paradox ox-gfm orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file noflet neotree multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode key-chord json-mode js2-refactor js-doc jade-mode intero info+ indent-guide ido-vertical-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word company-web company-statistics company-ghci company-ghc company-emoji company-cabal company-c-headers column-enforce-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(php-mode-enable-project-coding-style t)
 '(rbenv-modeline-function 'rbenv--modeline-plain)
 '(select-enable-primary t)
 '(send-mail-function 'smtpmail-send-it)
 '(writeroom-width 144))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3"))))
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(variable-pitch ((t (:family "Linux Biolinum" :height 120 :weight thin)))))
)
