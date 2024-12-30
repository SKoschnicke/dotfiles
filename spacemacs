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
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(python
     rust
     haskell
     nginx
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
     ;; helm
     compleseus
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
                      auto-completion-private-snippets-directory "~/.dotfiles/snippets"
                      )
     typography
     ;;emoji ;; breaks org mode repeat
     emacs-lisp
     (git :variables
          git-enable-magit-delta-plugin nil; breaks stuff
          git-enable-magit-todos-plugin nil ;; breaks stuff and/or makes git status slow
          )
     (org :variables
          org-enable-github-support t
          org-enable-roam-support nil
          org-enable-jira-support t
          jiralib-url "https://commercetools.atlassian.net"
          org-enable-org-journal-support t
          org-enable-asciidoc-support t
          org-enable-verb-support t
          org-enable-valign f
          org-enable-transclusion-support t
          org-enable-sticky-header t
          org-enable-modern-support t
          org-enable-hugo-support t
          )
     evil-snipe
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     syntax-checking
     version-control
     shell-scripts
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-backend 'lsp-deferred
           ruby-prettier-on-save t)
     ruby-on-rails
     html
     (javascript :variables javascript-fmt-on-save t)
     tern
     yaml
     asciidoc
     markdown
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     sql
     prettier
     (typescript :variables
                 company-tooltip-align-annotations t
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'prettier)
     (restclient :variables restclient-use-org t)
     (lsp :variables
          lsp-treemacs-sync-mode 1
          lsp-lens-enable t
          lsp-enable-file-watchers nil) ;; only causes trouble on macOS, but keep on Linux!
     (dash :variables ;; requires zeal installed on the machine
           dash-docs-docset-newpath "~/.local/share/Zeal/Zeal/docsets"
           dash-docs-enable-debugging nil)
     bm
     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))
     treemacs
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak
                        persp-auto-save-persps-to-their-file-before-kill t
                        persp-auto-resume-time 1
                        persp-auto-save-fname "autosave"
                        persp-auto-save-opt 2
                        persp-nil-hidden t
                        persp-nil-name "Default"
                        persp-save-dir (concat my-sync-path "/emacs-perspectives/"))
     (llm-client :variables
                 llm-client-enable-gptel t)
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
     org-ql
     ;; helm-org-ql
     mixed-pitch
     org-sidebar
     org-super-agenda
     yasnippet-snippets
     editorconfig
     (copilot :location (recipe :fetcher github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
     fireplace
     code-review
     elpher
     org-recur
     gptel
     (conventional-commit :location (recipe :fetcher github :repo "akirak/conventional-commit.el"))
     (jwt :location (recipe :fetcher github :repo "joshbax189/jwt-el"))
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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

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

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

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

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Victor Mono"
                               :size 16.0
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
   dotspacemacs-large-file-size 3

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
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
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
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

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
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
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
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (require 'window-purpose) ;; workaround until https://github.com/bmag/emacs-purpose/issues/158 is fixed
  (require 'gptel)
  (require 'gptel-curl)
  (setq gptel-model "gpt-4"
        gptel-default-mode "org-mode"
        gptel-org-branching-context t
        )

  (add-to-list 'gptel-directives '(performance-review . "Revise the given performance review text. The revised text should maintain the original meaning and key points, while improving the grammar, spelling, and alignment with Radical Candor principles."))
  ;; map SPC a g to gptel-send invoked with universal argument
  (spacemacs/set-leader-keys "ag" (lambda () (interactive) (gptel-send '(4))))

  (when (string= system-name "sven-uni")
    (defconst my-sync-path "~/SpiderOak Hive"))
  (when (string= system-name "palanthas")
    (defconst my-sync-path "~/SpiderOak Hive"))
  (when (string-prefix-p "losarcum" system-name)
    (defconst my-sync-path "~"))
  (when (string= system-name "daltigoth")
    (defconst my-sync-path "~"))
  (when (string= system-name "istar")
    (defconst my-sync-path "~"))
  (defconst my-sync-path "~")

  (defconst my-org-file-path (concat my-sync-path "/org"))


  (when (equal system-type 'darwin)
    (setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox"))
  (setq browse-url-browser-function 'browse-url-firefox)


  ;; seems to make problems with tab complete
  ;;  (setq-default evil-escape-key-sequence "kj")
  ;;  (setq-default evil-escape-delay 0.2)

  ;;;;;;;;;;;; org mode setq settings ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; put them outside of eval-after-load to have them set before org is loaded
  ;; at least org-todo-keywords won't work otherwise!

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'mixed-pitch-mode)
  (add-hook 'org-mode-hook 'org-recur-mode)
  (add-hook 'after-init-hook 'org-roam-mode)
  ;; (add-hook 'org-clock-in-hook 'harvest)
  ;; (add-hook 'org-clock-out-hook 'harvest-clock-out)
  ;; show custom agenda after start
  (add-hook 'after-init-hook 'org-agenda-show-mine)
  (add-hook 'org-agenda-mode-hook 'org-recur-agenda-mode)



  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "MAYBE(m)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))

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
        org-agenda-default-appointment-duration nil  ;; this also makes all scheduled items last for this duration instead of taking the efford
        org-id-link-to-org-use-id t
        org-journal-dir (concat my-org-file-path "/journal/")
        org-confirm-babel-evaluate nil
        org-confirm-elisp-link-function nil
        org-confirm-shell-link-function t
        )

  ;; Refile targets include this file (5 levels deep) and any file contributing to the agenda (only 1 level, top level headlines)
  (setq org-refile-targets (quote ((nil :maxlevel . 2) (org-agenda-files :maxlevel . 1))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Works only with helm and ivy when nil
  (setq org-outline-path-complete-in-steps nil)

  (setq org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM")

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

  ;; put everything which calls org functions inside the eval-after-load block, to avoid loading bundled org mode
  (with-eval-after-load 'org


    (org-babel-do-load-languages
     'org-babel-load-languages
     '((R . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . t)
       (latex . t)
       (ledger . t)
       (ocaml . t)
       (octave . t)
       (python . t)
       (ruby . t)
       (screen . nil)
       (shell . t)
       (sql . nil)
       (verb . nil)
       (sqlite . t)
       (js . t)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                   ((agenda "" ((org-agenda-span 'day)
                                (org-agenda-start-day "0d")
                                (org-super-agenda-groups
                                 '((:name "Today"
                                          :time-grid t
                                          :todo "TODAY"
                                          :scheduled today
                                          :order 0)
                                   (:name "Main"
                                          :tag "main"
                                          :order 1)
                                        ;(:habit t) ;; this currently breaks Agenda, see https://github.com/alphapapa/org-super-agenda/issues/247 2024-10-24
                                   (:name "Work"
                                          :tag "frontastic"
                                          :order 2)
                                   (:name "Due Today"
                                          :deadline today
                                          :order 3)
                                   (:name "Overdue"
                                          :deadline past
                                          :order 7)
                                   (:name "Due Soon"
                                          :deadline future
                                          :order 8)
                                   (:name "Privat"
                                          :tag "prv"
                                          :order 9)
                                   )
                                 )
                                ))
                    )
                   )
                  ("t" todo-tree "TODO")
                  ("T" tags-todo "/TODO|NEXT" ((org-agenda-todo-ignore-scheduled 'future)
                                               (org-agenda-tags-todo-honor-ignore-options t)
                                               (org-agenda-todo-list-sublevels t)))
                  ("s" todo-tree "STARTED")
                  ("w" todo-tree "WAITING")
                  ("R" tags "REFILE")
                  ("o" tags-todo "-pav-swc-gxp/TODO")
                  ("S" tags-todo "sprint/TODO")
                  ("p" . "Project Agendas")


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
                  ;; Weekly Review block agenda
                  ("r" . "Weekly Review")
                  ("r1" "Get Clear: Collect loose materials and process Inbox"
                   tags "+REFILE"
                   ((org-agenda-overriding-header "Inbox items to process:")
                    (org-agenda-prefix-format "")))
                  ("r2" "Get Current: Review Next Actions\n    Archive completed actions, review for further action steps."
                   (;(todo "DONE|DROPPED|COMPLETE" ((org-agenda-overriding-header "Done/Dropped Items (to archive):")
                    ;;                               (org-agenda-cmp-user-defined (cmp-date-property "CLOSED"))
                    ;;                               (org-agenda-sorting-strategy '(user-defined-up))))
                    ;; (above gives error because cmp-date-property doesn't exist)
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
                                (org-agenda-entry-types '(:deadline :timestamp :sexp)) ;; show due tasks, meetings
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

                  ))
          )

    ;; ;; CUSTOM AGENDA END

    (org-super-agenda-mode)

    (when (file-accessible-directory-p my-org-file-path)
      (setq diary-file (concat my-org-file-path "/diary")))

    (when (file-accessible-directory-p my-org-file-path)
      (setq org-agenda-files (list my-org-file-path))
      (setq org-directory my-org-file-path)
      (setq org-default-notes-file (concat my-org-file-path "/refile.org"))
      (setq org-download-method 'attach))

    ;; I use C-c c to start capture mode
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "refile.org")
                   "* TODO %?\n%U\n%a\n")
                  ("c" "Cooldown" entry (file+olp+datetree "gtd-daily-cooldown.org") (file "tmp-daily-cooldown.org") :immediate-finish t :jump-to-captured t :clock-in t)
                  ("e" "Abend-Review" entry (file+olp+datetree "gtd-evening-review.org") (file "tmp-evening-review.org") :immediate-finish t :jump-to-captured t)
                  ("r" "Weekly Review" entry (file+olp+datetree "gtd-weekly-reviews.org") (file "tmp-weekly-review.org") :immediate-finish t :jump-to-captured t :clock-in t)
                  ("n" "note" entry (file "refile.org")
                   "* %? :NOTE:\n%U\n%a\n")
                  ("b" "bug" entry (file "bugs.org")
                   "* %?\n%U\n%a\n\n** Symptom\n\n** Ursache\n\n** Wie gefunden\n\n** Fix\n\n** Projekt\n\n** Commit\n\n** Bug selbst verursacht?\n\n** Zeit bis zum Fix\n\n** Lektionen\n")
                  ("a" "automate" entry (file "refile.org")
                   "* %? :AUTO:\n%U\n%a\n")
                  ("g" "respond" entry (file "refile.org")
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n")
                  ("j" "Journal" entry (file+olp+datetree "diary.org")
                   "* %?\n%U\n")
                  ("m" "Meeting" entry (file "refile.org")
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("p" "Phone call" entry (file "refile.org")
                   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit" entry (file "refile.org")
                   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                  ("B" "Brain" plain (function org-brain-goto-end)
                   "* %i%?" :empty-lines 1)
                  ("f" "Frontastic Report" entry (file+headline "gxp-frontastic.org" "Wochenbericht Frontastic") (file "tmp-frontastic-weekly-report.org"))
                  )))

    (setq org-use-sub-superscripts "{}") ;; x_i is not interpreted as subscript, but x_{i} is

    ;; insert creation date into all headings
    (defun insert-created-date(&rest ignore)
      (insert "\n")
      (org-insert-time-stamp (current-time) 't 't)
      (org-back-to-heading) ;; in org-capture, this folds the entry; when inserting a heading, this moves point back to the heading line
      (move-end-of-line()) ;; when inserting a heading, this moves point to the end of the line
      )

    (advice-add 'org-insert-heading :after #'insert-created-date)

    ;; This enables binding the custom agenda to keys and showing it on startup.
    ;; Has to be defined outside of org mode hook, or else it would not be
    ;; available until org mode was loaded by opening an org buffer.
    (defun org-agenda-show-mine (&optional arg)
      (interactive "P")
      (org-agenda arg "A"))

    ;; bind custom agenda to SPC-A
    (spacemacs/set-leader-keys "A" 'org-agenda-show-mine)

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
      ;;(split-window-below-and-focus) ;; Split the right side into two and move focus
      ;;(mu4e) ;; start mail in lower right part
      ;;(winum-select-window-2) ;; Move focus back to agenda
      )

    (defun well-done ()
      "Setup windows for cooldown"
      (interactive)
      (persp-switch (concat my-org-file-path "/"))
      (spacemacs/window-split-single-column t)
      (find-file (concat my-org-file-path "/gtd-daily-cooldown.org"))
      (split-window-right-and-focus) ;; Split and move to the right
      (org-agenda-show-mine) ;; load agenda in upper right window
      (split-window-below-and-focus)
      (org-todo-list "STARTED")
      ;; (split-window-below-and-focus)
      ;; (org-todo-list "NEXT")
      ;; (winum-select-window-1)
      (split-window-below-and-focus)
      (org-ql-view-recent-items :num-days 1)
      (winum-select-window-1)
      )


    (evil-leader/set-key "aM" 'good-morning)
    (evil-leader/set-key "aW" 'well-done)
    (evil-leader/set-key "aoq" 'org-ql-view-sidebar)
    (evil-leader/set-key "ao/" 'org-ql-find-in-agenda)

    (setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
    (setq org-list-allow-alphabetical t)
    ) ;; org-mode eval after load end
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

  ;; use 2 spaces in web mode for everything (instead of 4)
  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)
  (setq-default typescript-indent-level 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-indent-style 2)
  (setq-default typescript-indent-level 2)
  ;; NOTE that indent when using beautify (SPC m =) is determined by the external
  ;; .jsbeautifyrc file, see https://github.com/yasuyk/web-beautify

  ;; never indent with tabs
  (setq-default indent-tabs-mode nil)

  ;; how can the default config not set this?!
  (global-set-key (kbd "C-i") 'evil-jump-forward)

  ;; DocView renders PDFs as PNGs to display them. The default resolution (DPI)
  ;; for this makes the PDFs look rasterized on hiDPI displays.
  (setq-default doc-view-resolution 300)

  ;; Reload document when it changes on disk
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1)


  (setq rubocop-check-command "rubocop --format emacs")

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
    ;;(require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text)
    (add-hook 'mu4e-view-mode-hook
              (lambda()
                ;; try to emulate some of the eww key-bindings
                (local-set-key (kbd "<tab>") 'shr-next-link)
                (local-set-key (kbd "<backtab>") 'shr-previous-link)))

    )
  ;; to avoid "The TLS connection was non-properly terminated"
  ;; see https://github.com/syl20bnr/spacemacs/issues/6638
  ;;(setq dotspacemacs-elpa-https nil)


  (defun my-php-mode-setup ()
    "My PHP-mode hook."
    ;;(require 'flycheck-phpstan)
    (flycheck-mode t)
    (setq flycheck-checker-error-threshold 5000)
    (setq phpstan-working-dir ".")
    ;;(flycheck-add-next-checker 'phpstan 'php-phpcs)
    (flycheck-add-next-checker 'php-phpcs)
    )

  (add-hook 'php-mode-hook 'my-php-mode-setup)

  (add-to-list 'projectile-project-root-files "go.mod")
  (add-to-list 'projectile-project-root-files "devbox.json")
  (projectile-register-project-type 'go '("go.mod")
                                    :project-file "go.mod"
                                    :test-suffix "_test")

  (setq create-lockfiles nil) ;; webpack can't handle lockfiles
  (setq counsel-dash-common-docsets '("Javascript" "HTML" "Go" "PHP"))

  (setq javascript-fmt-tool 'prettier)
  (setq typescript-fmt-tool 'prettier)

  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    )
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;  '(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;  '(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;  '(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;  '(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;  '(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;  '(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;  '(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;  '(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  ;;'(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch) :height 1.2)))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 240))))
   '(fixed-pitch ((t ( :family "Victor Mono" :height 160)))))

  (custom-set-variables
   '(phpcbf-standard "PSR2")
   )
  (setq flycheck-phpcs-standard "PSR2")

  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'php-mode-hook #'lsp-go-install-save-hooks)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "s*" 'org-toggle-heading)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "sP" 'outline-up-heading)
  (setq helm-org-format-outline-path t)
  ;; not working, needs to be added to "helm actions" somehow
  ;;(with-eval-after-load 'helm-files
  ;;  (define-key helm-find-files-map (kbd "S-<return>") 'helm-find-files-other-window))


  (setq lsp-go-gopls-server-path
        (pcase system-name
          ("daltigoth" "/home/sven/go/bin/gopls")
          ("istar.localdomain" "/opt/homebrew/bin/gopls")))
  (setq lsp-gopls-server-path lsp-go-gopls-server-path)

  ;; also install coreutils through brew: "brew install coreutils"
  (when (equal system-type 'darwin)
    (setq insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))

  (with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))

  (add-hook 'prog-mode-hook 'copilot-mode)

  (defun file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash
     (lambda (key _value)
       (file-notify-rm-watch key))
     file-notify-descriptors))


  (add-to-list 'auto-mode-alist '("Tiltfile" . python-mode))

  (require 'conventional-commit)
  (add-hook 'git-commit-mode-hook 'conventional-commit-setup)
  (add-hook 'company-mode-hook
            (lambda()
              (global-set-key (kbd "S-TAB") 'company-complete)))

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
   '(custom-safe-themes
     '("eecff0e045e5a54e5a517a042a7491eecfb8d49e79c5813e2110ad3458df52ce" default))
   '(evil-want-Y-yank-to-eol t)
   '(exec-path
     '("/usr/local/sbin/" "/usr/local/bin/" "/usr/bin/" "/opt/android-sdk/platform-tools/" "/opt/android-sdk/tools/" "/usr/lib/jvm/default/bin/" "/usr/bin/site_perl/" "/usr/bin/vendor_perl/" "/usr/bin/core_perl/" "/home/sven/.rbenv/shims/" "/opt/homebrew/bin"))
   '(flycheck-disabled-checkers '(ruby ruby-rubylint javascript-jshint))
   '(flycheck-go-golint-executable "/Users/sven/go/bin/golint")
   '(flycheck-phpcs-standard "PSR2")
   '(flycheck-phpmd-rulesets '("codesize" "design" "unusedcode"))
   '(flycheck-phpstan-executable "/home/sven/.config/composer/vendor/bin/phpstan")
   '(haskell-tags-on-save t)
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
   '(jiralib-url "https://commercetools.atlassian.net" t)
   '(jiralib-user "63456be7754fb6b373b94dcc")
   '(js2-missing-semi-one-line-override t)
   '(js2-strict-missing-semi-warning nil)
   '(lsp-intelephense-php-version "8.1.0")
   '(mu4e-view-show-addresses t)
   '(mu4e-view-show-images t)
   '(native-comp-deferred-compilation-deny-list '(".*projectile.*"))
   '(org-ql-views
     '(("Review: Refile" :buffers-files
        ("/Users/sven/org/anniversary.org" "/Users/sven/org/bugs.org" "/Users/sven/org/cli-presentation.org" "/Users/sven/org/getdigital.org" "/Users/sven/org/gfxpro.org" "/Users/sven/org/gtd-daily-cooldown.org" "/Users/sven/org/gtd-evening-review.org" "/Users/sven/org/gtd-weekly-reviews.org" "/Users/sven/org/gxp-frontastic-cli.org" "/Users/sven/org/gxp-frontastic-meetings.org" "/Users/sven/org/gxp-frontastic.org" "/Users/sven/org/gxp-lasersoft-doc.org" "/Users/sven/org/gxp-lasersoft.org" "/Users/sven/org/isavision.org" "/Users/sven/org/knw-dev.org" "/Users/sven/org/knw-docker.org" "/Users/sven/org/knw-emacs.org" "/Users/sven/org/knw-git.org" "/Users/sven/org/knw-go.org" "/Users/sven/org/knw-hardware.org" "/Users/sven/org/knw-haskell.org" "/Users/sven/org/knw-java.org" "/Users/sven/org/knw-linux.org" "/Users/sven/org/knw-macos.org" "/Users/sven/org/knw-misc.org" "/Users/sven/org/knw-php.org" "/Users/sven/org/knw-productivity.org" "/Users/sven/org/knw-ruby.org" "/Users/sven/org/knw-scala.org" "/Users/sven/org/knw-webdev.org" "/Users/sven/org/knw-windows.org" "/Users/sven/org/les-aprpat.org" "/Users/sven/org/mallorca-selection-verarbeitungsverzeichnis.org" "/Users/sven/org/meta.org" "/Users/sven/org/newsfeeds.org" "/Users/sven/org/notes.org" "/Users/sven/org/pav-dev-orga.org" "/Users/sven/org/pav-lambda.org" "/Users/sven/org/pav-plan.org" "/Users/sven/org/pav-psi.org" "/Users/sven/org/pav-rho.org" "/Users/sven/org/pav-sea-gmbh.org" "/Users/sven/org/pav-sysops.org" "/Users/sven/org/pav-theory.org" "/Users/sven/org/plan.org" "/Users/sven/org/postgres-tuning.org" "/Users/sven/org/privat.org" "/Users/sven/org/prs-linting.org" "/Users/sven/org/prv-garten.org" "/Users/sven/org/prv-haus.org" "/Users/sven/org/prv-haushalt.org" "/Users/sven/org/prv-kindergarten.org" "/Users/sven/org/prv-mama.org" "/Users/sven/org/prv-steuern.org" "/Users/sven/org/prv-sysops.org" "/Users/sven/org/refile.org" "/Users/sven/org/social.org" "/Users/sven/org/swc-docu.org" "/Users/sven/org/swc-finalapp.org" "/Users/sven/org/swc-gui.org" "/Users/sven/org/swc-misc.org" "/Users/sven/org/swc-orga.org" "/Users/sven/org/swc-server.org" "/Users/sven/org/swc-sysops.org" "/Users/sven/org/swc-team.org" "/Users/sven/org/swc-verzeichnis-verarbeitungstaetigkeiten.org" "/Users/sven/org/swc-webapp.org" "/Users/sven/org/swc-website.org" "/Users/sven/org/swc-workshop.org" "/Users/sven/org/tmp-daily-cooldown.org" "/Users/sven/org/tmp-evening-review.org" "/Users/sven/org/tmp-frontastic-weekly-report.org" "/Users/sven/org/tmp-weekly-review.org" "/Users/sven/org/uni-bachelor-tim.org" "/Users/sven/org/uni-digisys.org" "/Users/sven/org/uni-doktorarbeit.org" "/Users/sven/org/uni-imps.org" "/Users/sven/org/uni-infprog.org" "/Users/sven/org/uni-misc.org" "/Users/sven/org/uni-plan.org")
        :query
        (tags "REFILE")
        :sort
        (date)
        :narrow nil :super-groups nil :title "Review: Refile")
       ("Prioritize Tasks ct" :buffers-files
        ("/Users/sven/org/anniversary.org" "/Users/sven/org/bugs.org" "/Users/sven/org/cli-presentation.org" "/Users/sven/org/getdigital.org" "/Users/sven/org/gfxpro.org" "/Users/sven/org/gtd-daily-cooldown.org" "/Users/sven/org/gtd-evening-review.org" "/Users/sven/org/gtd-weekly-reviews.org" "/Users/sven/org/gxp-frontastic-cli.org" "/Users/sven/org/gxp-frontastic-meetings.org" "/Users/sven/org/gxp-frontastic.org" "/Users/sven/org/gxp-lasersoft-doc.org" "/Users/sven/org/gxp-lasersoft.org" "/Users/sven/org/isavision.org" "/Users/sven/org/knw-dev.org" "/Users/sven/org/knw-docker.org" "/Users/sven/org/knw-emacs.org" "/Users/sven/org/knw-git.org" "/Users/sven/org/knw-go.org" "/Users/sven/org/knw-hardware.org" "/Users/sven/org/knw-haskell.org" "/Users/sven/org/knw-java.org" "/Users/sven/org/knw-linux.org" "/Users/sven/org/knw-macos.org" "/Users/sven/org/knw-misc.org" "/Users/sven/org/knw-php.org" "/Users/sven/org/knw-productivity.org" "/Users/sven/org/knw-ruby.org" "/Users/sven/org/knw-scala.org" "/Users/sven/org/knw-webdev.org" "/Users/sven/org/knw-windows.org" "/Users/sven/org/les-aprpat.org" "/Users/sven/org/mallorca-selection-verarbeitungsverzeichnis.org" "/Users/sven/org/meta.org" "/Users/sven/org/newsfeeds.org" "/Users/sven/org/notes.org" "/Users/sven/org/pav-dev-orga.org" "/Users/sven/org/pav-lambda.org" "/Users/sven/org/pav-plan.org" "/Users/sven/org/pav-psi.org" "/Users/sven/org/pav-rho.org" "/Users/sven/org/pav-sea-gmbh.org" "/Users/sven/org/pav-sysops.org" "/Users/sven/org/pav-theory.org" "/Users/sven/org/plan.org" "/Users/sven/org/postgres-tuning.org" "/Users/sven/org/privat.org" "/Users/sven/org/prs-linting.org" "/Users/sven/org/prv-garten.org" "/Users/sven/org/prv-haus.org" "/Users/sven/org/prv-haushalt.org" "/Users/sven/org/prv-kindergarten.org" "/Users/sven/org/prv-mama.org" "/Users/sven/org/prv-steuern.org" "/Users/sven/org/prv-sysops.org" "/Users/sven/org/refile.org" "/Users/sven/org/social.org" "/Users/sven/org/swc-docu.org" "/Users/sven/org/swc-finalapp.org" "/Users/sven/org/swc-gui.org" "/Users/sven/org/swc-misc.org" "/Users/sven/org/swc-orga.org" "/Users/sven/org/swc-server.org" "/Users/sven/org/swc-sysops.org" "/Users/sven/org/swc-team.org" "/Users/sven/org/swc-verzeichnis-verarbeitungstaetigkeiten.org" "/Users/sven/org/swc-webapp.org" "/Users/sven/org/swc-website.org" "/Users/sven/org/swc-workshop.org" "/Users/sven/org/tmp-daily-cooldown.org" "/Users/sven/org/tmp-evening-review.org" "/Users/sven/org/tmp-frontastic-weekly-report.org" "/Users/sven/org/tmp-weekly-review.org" "/Users/sven/org/uni-bachelor-tim.org" "/Users/sven/org/uni-digisys.org" "/Users/sven/org/uni-doktorarbeit.org" "/Users/sven/org/uni-imps.org" "/Users/sven/org/uni-infprog.org" "/Users/sven/org/uni-misc.org" "/Users/sven/org/uni-plan.org")
        :query
        (tags "prioritize")
        :sort nil :narrow nil :super-groups nil :title "Prioritize Tasks ct")
       ("Overview: Agenda-like" :buffers-files org-agenda-files :query
        (and
         (not
          (done))
         (or
          (habit)
          (deadline auto)
          (scheduled :to today)
          (ts-active :on today)))
        :sort
        (todo priority date)
        :super-groups org-super-agenda-groups :title "Agenda-like")
       ("Overview: NEXT tasks" :buffers-files org-agenda-files :query
        (todo "NEXT")
        :sort
        (date priority)
        :super-groups org-super-agenda-groups :title "Overview: NEXT tasks")
       ("Calendar: Today" :buffers-files org-agenda-files :query
        (ts-active :on today)
        :title "Today" :super-groups org-super-agenda-groups :sort
        (priority))
       ("Calendar: This week" .
        #[0 "\301 \302\303\304\305\304\306\304\307\310\301 \311\1!\10>\204\34\0\312\313\314\3D\"\210\211\315H\204\232\0\211\315\316\317\320\311\6\6!\10>\2048\0\312\313\314\6\10D\"\210\5\321H\204\223\0\5\321\311\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\203\215\0\4\203\215\0\3\203\215\0\2\203\215\0\1\203\215\0\211\203\215\0\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\202\221\0\330 \266\206I\210\5\321H\"!I\210\211\315H\262\1[\6\12#&\7\302\303\332\305\333\306\333\307\310\327\301 \311\1!\10>\204\300\0\312\313\314\3D\"\210\211\315H\204>\1\211\315\316\317\320\311\6\6!\10>\204\334\0\312\313\314\6\10D\"\210\5\321H\2047\1\5\321\311\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\2031\1\4\2031\1\3\2031\1\2\2031\1\1\2031\1\211\2031\1\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\2025\1\330 \266\206I\210\5\321H\"!I\210\211\315H\262\1Z\6\13#&\7\334\335 \336\337\5\340\6\6\257\5\341\342\343\344\345\346&\10\207"
            [cl-struct-ts-tags ts-now ts-apply :hour 0 :minute :second ts-adjust day type-of signal wrong-type-argument ts 7 string-to-number format-time-string "%w" 17 3 2 1 4 5 6 float-time encode-time 23 59 org-ql-search org-agenda-files ts-active :from :to :title "This week" :super-groups org-super-agenda-groups :sort
                               (priority)]
            34 "Show items with an active timestamp during this calendar week." nil])
       ("Calendar: Next week" .
        #[0 "\301\302\303\304 #\305\306\307\310\307\311\307\301\302\304 \312\1!\10>\204 \0\313\314\315\3D\"\210\211\303H\204\236\0\211\303\316\317\320\312\6\6!\10>\204<\0\313\314\315\6\10D\"\210\5\321H\204\227\0\5\321\312\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\203\221\0\4\203\221\0\3\203\221\0\2\203\221\0\1\203\221\0\211\203\221\0\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\202\225\0\330 \266\206I\210\5\321H\"!I\210\211\303H\262\1[\6\12#&\7\305\306\332\310\333\311\333\301\302\327\304 \312\1!\10>\204\304\0\313\314\315\3D\"\210\211\303H\204B\1\211\303\316\317\320\312\6\6!\10>\204\340\0\313\314\315\6\10D\"\210\5\321H\204;\1\5\321\312\6\10!\10>\210\6\7\322H\6\10\323H\6\11\324H\6\12\325H\6\13\326H\6\14\327H\5\2035\1\4\2035\1\3\2035\1\2\2035\1\1\2035\1\211\2035\1\330\331\6\7\6\7\6\7\6\7\6\7\6\7&\6!\266\206\2029\1\330 \266\206I\210\5\321H\"!I\210\211\303H\262\1Z\6\13#&\7\334\335 \336\337\5\340\6\6\257\5\341\342\343\344\345\346&\10\207"
            [cl-struct-ts-tags ts-adjust day 7 ts-now ts-apply :hour 0 :minute :second type-of signal wrong-type-argument ts string-to-number format-time-string "%w" 17 3 2 1 4 5 6 float-time encode-time 23 59 org-ql-search org-agenda-files ts-active :from :to :title "Next week" :super-groups org-super-agenda-groups :sort
                               (priority)]
            34 "Show items with an active timestamp during the next calendar week." nil])
       ("Review: Recently timestamped" . org-ql-view-recent-items)
       (#("Review: Dangling tasks" 0 22
          (help-echo "Tasks whose ancestor is done"))
        :buffers-files org-agenda-files :query
        (and
         (todo)
         (ancestors
          (done)))
        :title
        #("Review: Dangling tasks" 0 22
          (help-echo "Tasks whose ancestor is done"))
        :sort
        (todo priority date)
        :super-groups
        ((:auto-parent t)))
       (#("Review: Stale tasks" 0 19
          (help-echo "Tasks without a timestamp in the past 2 weeks"))
        :buffers-files org-agenda-files :query
        (and
         (todo)
         (not
          (ts :from -14)))
        :title
        #("Review: Stale tasks" 0 19
          (help-echo "Tasks without a timestamp in the past 2 weeks"))
        :sort
        (todo priority date)
        :super-groups
        ((:auto-parent t)))
       (#("Review: Stuck projects" 0 22
          (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
        :buffers-files org-agenda-files :query
        (and
         (todo)
         (descendants
          (todo))
         (not
          (descendants
           (todo "NEXT"))))
        :title
        #("Review: Stuck projects" 0 22
          (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
        :sort
        (date priority)
        :super-groups org-super-agenda-groups)))
   '(package-selected-packages
     '(magit-todos zpresent org-parser yasnippet-snippets yapfify yaml-mode yafolding xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree queue typo typescript-mode toc-org tide tagedit string-inflection sql-indent spotify spaceline smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restclient-helm restart-emacs rbenv ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails rake inflections plantuml-mode pip-requirements phpunit php-extras persp-mode pcre2el paradox spinner ox-gfm origami orgit org-sidebar org-randomnote org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets open-junk-file ob-restclient ob-http ob-elixir nginx-mode neotree multi-term move-text mmm-mode mixed-pitch minitest markdown-toc markdown-mode magit-gitflow magit-popup magit magit-section macrostep lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative key-chord json-mode json-snatcher js2-refactor multiple-cursors js2-mode js-doc jinja2-mode insert-shebang indent-guide hydra lv hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-spotify-plus multi helm-pydoc helm-projectile projectile helm-org-ql org-ql helm-org peg ov org-super-agenda ts helm-mode-manager helm-make helm-gitignore request git-modes helm-flx helm-descbinds helm-dash dash-docs helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter git-commit with-editor transient compat gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck-gometalinter flycheck-credo flycheck flx-ido flx fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu evil goto-chg eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies link-hint powerline popwin elfeed dumb-jump drupal-mode php-mode diminish diff-hl define-word dash-at-point cython-mode csv-mode copilot editorconfig company-web web-completion-data company-statistics company-shell company-restclient restclient know-your-http-well company-quickhelp pos-tip company-go go-mode company-box frame-local company-ansible company-anaconda column-enforce-mode coffee-mode clean-aindent-mode chruby bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol ht auto-dictionary auto-compile ansible-doc ansible anaconda-mode pythonic f alchemist s pkg-info company elixir-mode epl aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup solarized-theme dash))
   '(paradox-github-token t)
   '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
   '(php-mode-enable-project-coding-style t)
   '(phpcbf-standard "PSR2")
   '(rbenv-modeline-function 'rbenv--modeline-plain)
   '(select-enable-primary t)
   '(send-mail-function 'smtpmail-send-it)
   '(warning-suppress-types '((comp)))
   '(writeroom-width 144))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(fixed-pitch ((t (:family "Victor Mono" :height 160))))
   '(variable-pitch ((t (:family "ETBembo" :height 240)))))
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zpresent org-parser yasnippet-snippets yapfify yaml-mode yafolding xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree queue typo typescript-mode toc-org tide tagedit string-inflection sql-indent spotify spaceline smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restclient-helm restart-emacs rbenv ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails rake inflections plantuml-mode pip-requirements phpunit php-extras persp-mode pcre2el paradox spinner ox-gfm origami orgit org-sidebar org-randomnote org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-bullets open-junk-file ob-restclient ob-http ob-elixir nginx-mode neotree multi-term move-text mmm-mode mixed-pitch minitest markdown-toc markdown-mode magit-gitflow magit-popup magit magit-section macrostep lorem-ipsum livid-mode skewer-mode live-py-mode linum-relative key-chord json-mode json-snatcher js2-refactor multiple-cursors js2-mode js-doc jinja2-mode insert-shebang indent-guide hydra lv hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-spotify-plus multi helm-pydoc helm-projectile projectile helm-org-ql org-ql helm-org peg ov org-super-agenda ts helm-mode-manager helm-make helm-gitignore request git-modes helm-flx helm-descbinds helm-dash dash-docs helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter git-commit with-editor transient compat gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck-gometalinter flycheck-credo flycheck flx-ido flx fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu evil goto-chg eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav elfeed-web simple-httpd elfeed-org elfeed-goodies link-hint powerline popwin elfeed dumb-jump drupal-mode php-mode diminish diff-hl define-word dash-at-point cython-mode csv-mode copilot editorconfig company-web web-completion-data company-statistics company-shell company-restclient restclient know-your-http-well company-quickhelp pos-tip company-go go-mode company-box frame-local company-ansible company-anaconda column-enforce-mode coffee-mode clean-aindent-mode chruby bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol ht auto-dictionary auto-compile ansible-doc ansible anaconda-mode pythonic f alchemist s pkg-info company elixir-mode epl aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup solarized-theme dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
