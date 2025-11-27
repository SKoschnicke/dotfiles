;; Auto-tangle on save
(defun org-babel-auto-tangle ()
  "Automatically tangle org file when saved."
  (when (eq major-mode 'org-mode)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'after-save-hook 'org-babel-auto-tangle)

;; Function to open Jira ticket from org section
(defun my/org-open-jira-ticket-at-point ()
  "Find Jira ticket reference in current org section and open it in browser.
     Looks for:
     - Tag in the format FP_1234
     - Link in the format https://commercetools.atlassian.net/browse/FP-1234"
  (interactive)
  (let* ((jira-base-url "https://commercetools.atlassian.net/browse/")
         (ticket-id nil))

    ;; First check for tags on the current heading
    (save-excursion
      (org-back-to-heading t)
      (let ((tags (org-get-tags)))
        (when tags
          (dolist (tag tags)
            (when (string-match "\\([A-Z]+\\)_\\([0-9]+\\)" tag)
              (setq ticket-id (concat (match-string 1 tag) "-" (match-string 2 tag))))))))

    ;; If no tag found, look for URL in heading text and content
    (when (not ticket-id)
      (save-excursion
        (org-back-to-heading t)
        (let ((end-of-subtree (save-excursion (org-end-of-subtree) (point)))
              (url-regexp "https://commercetools\\.atlassian\\.net/browse/\\([A-Z]+-[0-9]+\\)"))
          (when (re-search-forward url-regexp end-of-subtree t)
            (setq ticket-id (match-string 1))))))

    ;; If ticket-id found, open in browser
    (if ticket-id
        (progn
          (browse-url (concat jira-base-url ticket-id))
          (message "Opening Jira ticket %s" ticket-id))
      (message "No Jira ticket reference found in this section"))))

;; Bind the function to a key in org-mode
(spacemacs/set-leader-keys-for-major-mode 'org-mode "j" 'my/org-open-jira-ticket-at-point)

(evil-leader/set-key "aoS" 'org-slack-export-to-clipboard-as-slack)

;; Slack deep link conversion for org-mode
;; This code converts Slack web URLs to slack:// deep links
;; It then uses the "open" utility of macOS to directly send the deep link to
;; the Slack app. This is way faster than using the browser.

(defcustom slack-team-id nil
  "Your Slack team ID (e.g., T12345678).
You can find this in your Slack workspace settings or by checking
the URL when you're logged into Slack web."
  :type 'string
  :group 'org-link)

(defun slack-extract-team-id-from-url (url)
  "Extract team ID from Slack URL if available, otherwise use slack-team-id."
  (or slack-team-id
      ;; Try to extract from URL - this is a fallback that may not always work
      ;; You should set slack-team-id instead
      (when (string-match "https://\\([^.]+\\)\\.slack\\.com" url)
        (concat "T" (match-string 1 url)))))

(defun slack-convert-url-to-deep-link (url)
  "Convert a Slack web URL to a slack:// deep link."
  (when (string-match "https://\\([^.]+\\)\\.slack\\.com/archives/\\([^/]+\\)\\(?:/p\\([0-9]+\\)\\)?" url)
    (let* ((workspace (match-string 1 url))
           (channel-id (match-string 2 url))
           (message-ts (match-string 3 url))
           (team-id (slack-extract-team-id-from-url url)))
      (if team-id
          (if message-ts
              ;; Include message timestamp to highlight specific message
              (format "slack://channel?team=%s&id=%s&message=%s"
                      team-id channel-id message-ts)
            ;; Just open channel if no message timestamp
            (format "slack://channel?team=%s&id=%s" team-id channel-id))
        (error "Slack team ID not configured. Please set slack-team-id variable")))))

(defun slack-open-deep-link (url)
  "Open Slack URL using deep link via native macOS open command."
  (let ((deep-link (slack-convert-url-to-deep-link url)))
    (if deep-link
        (progn
          (message "Opening Slack deep link: %s" deep-link)
          (if (eq system-type 'darwin) ; macOS
              ;; Use open -a Slack to explicitly open with Slack app
              (start-process "slack-open" nil "open" "-a" "Slack" deep-link)
            ;; Fallback for other systems
            (browse-url deep-link)))
      (error "Failed to convert Slack URL to deep link"))))

(defun org-slack-link-open (path &optional arg)
  "Open Slack links using deep links instead of browser."
  (let ((url (if (string-match "^https?://" path)
                 path
               (concat "https://" path))))
    (slack-open-deep-link url)))

;; Register the custom link type
(with-eval-after-load 'org
  (org-link-set-parameters "slack"
                           :follow #'org-slack-link-open))

;; ;; Override the default https handler for slack.com URLs
;; (defun org-slack-link-advice (orig-fun url &rest args)
;;   "Advice to intercept Slack URLs and convert them to deep links."
;;   (if (string-match "https://[^.]+\\.slack\\.com/" url)
;;       (slack-open-deep-link url)
;;     (apply orig-fun url args)))

;; ;; Add advice to org-link-open to intercept Slack URLs
;; (advice-add 'browse-url :around #'org-slack-link-advice)

;; Add a custom protocol handler for existing https links
(defun org-link-slack-handler (url &rest args)
  "Handle Slack URLs by converting them to deep links."
  (when (string-match "https://[^.]+\\.slack\\.com/" url)
    (slack-open-deep-link url)
    t)) ; Return t to indicate we handled it

;; Register the handler
(add-to-list 'browse-url-handlers '("slack\\.com" . org-link-slack-handler))

;; Configuration helper function
(defun slack-setup-team-id ()
  "Interactive function to set up your Slack team ID."
  (interactive)
  (let ((team-id (read-string "Enter your Slack team ID (e.g., T12345678): ")))
    (setq slack-team-id team-id)
    (customize-save-variable 'slack-team-id team-id)
    (message "Slack team ID set to: %s" team-id)))

(defun slack-remove-deep-link-handling ()
  "Remove all Slack deep link handling and restore default behavior."
  (interactive)
  ;; Remove the advice from browse-url
  (advice-remove 'browse-url #'org-slack-link-advice)

  ;; Remove the custom URL handler
  (setq browse-url-handlers
        (seq-remove (lambda (handler)
                      (string-match-p "slack\\.com" (car handler)))
                    browse-url-handlers))

  ;; Remove the custom slack link type (this will reset it to default)
  (org-link-set-parameters "slack" :follow nil)

  ;; Optionally clear the team ID
  (setq slack-team-id nil)

  (message "Slack deep link handling removed. URLs will now open in browser."))

(defun my/org-git-commit-day ()
  "Commit all changes and push with the current day name as commit message."
  (interactive)
  (let* ((day-names '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
         (current-day (nth (string-to-number (format-time-string "%w")) day-names))
         (default-directory (or (magit-toplevel) default-directory)))

    (if (not (magit-toplevel))
        (message "Not in a git repository")
      (progn
        (message "Committing changes with message: %s" current-day)
        (magit-stage-modified t) ;; Stage all modified files
        (magit-commit-create (list "-m" current-day))
        (when (magit-get-current-branch)
          (magit-push-current-to-pushremote nil))
        (message "Successfully committed and pushed with message: %s" current-day)))))

;; Bind the function to a key in org-mode
(spacemacs/set-leader-keys-for-major-mode 'org-mode "g c" 'my/org-git-commit-day)

(defun my/org-checkout-todo-branch ()
  "Check out the git branch specified in the BRANCH property of the current org-mode TODO.
Uses magit for the checkout operation in the ~/development/frontastic directory.
If no BRANCH property is found at point, recursively checks parent headings."
  (interactive)
  (let ((branch (org-entry-get (point) "BRANCH" t))  ; t means inherit from parents
        (project-dir "~/development/frontastic"))
    (if branch
        (progn
          (message "Checking out branch: %s in %s" branch project-dir)
          (let ((default-directory project-dir))
            (magit-checkout branch)))
      (message "No BRANCH property found in this heading or its parents"))))

(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "b" 'my/org-checkout-todo-branch))

(defun magit-insert-flycheck-errors ()
  "Insert flycheck errors for modified files in magit status buffer."
  (when-let ((modified-files (magit-modified-files)))
    (let ((all-errors '()))
      ;; Collect errors from modified files
      (dolist (file modified-files)
        (let* ((full-path (expand-file-name file (magit-toplevel)))
               (buffer (find-buffer-visiting full-path)))
          (when (and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (when (and (bound-and-true-p flycheck-mode)
                         flycheck-current-errors)
                (dolist (error flycheck-current-errors)
                  (push (cons file error) all-errors)))))))

      ;; Insert section if we have errors
      (when all-errors
        (magit-insert-section (flycheck-errors)
          (magit-insert-heading "Flycheck errors in modified files:")
          (dolist (file-error all-errors)
            (let* ((file (car file-error))
                   (error (cdr file-error))
                   (line (flycheck-error-line error))
                   (col (flycheck-error-column error))
                   (level (flycheck-error-level error))
                   (msg (flycheck-error-message error))
                   (face (pcase level
                           ('error 'flycheck-error)
                           ('warning 'flycheck-warning)
                           (_ 'flycheck-info))))
              (magit-insert-section (flycheck-error error)
                (insert (format "  %s:%d:%d "
                                (propertize file 'face 'magit-filename)
                                line (or col 1)))
                (insert (propertize (format "%s" level) 'face face))
                (insert (format ": %s\n" msg)))))
          (insert "\n"))))))

(with-eval-after-load 'magit
  ;; Add the section to magit status
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-flycheck-errors
                          'magit-insert-unpushed-to-upstream
                          t))

(defcustom my/standup-hidden-tags '("REFILE" "gxp" "frontastic")
  "List of tags to hide in standup messages."
  :type '(repeat string)
  :group 'org-standup)

(defcustom my/standup-template
  "Yesterday I completed:\n%s\nI also worked on:\n%s\nToday I will:\n%s"
  "Template for standup messages. First %s is for completed tasks, second for clocked tasks, third for planned tasks."
  :type 'string
  :group 'org-standup)

(defcustom my/standup-monday-template
  "Last week I completed:\n%s\nI also worked on:\n%s\nToday I will:\n%s"
  "Template for Monday standup messages. First %s is for completed tasks, second for clocked tasks, third for planned tasks."
  :type 'string
  :group 'org-standup)

(defun my/filter-tags (tags)
  "Remove hidden tags from TAGS list."
  (cl-remove-if (lambda (tag)
                  (member tag my/standup-hidden-tags))
                tags))

(defun my/get-parent-context ()
  "Get parent heading context if not at level 1.
Returns nil if at level 1 or no parent found."
  (save-excursion
    (when (> (org-current-level) 1)
      (org-up-heading-safe)
      (when (> (org-current-level) 1)  ; Skip level 1 parents
        (org-get-heading t t t t)))))

(defun my/get-jira-link (tag)
  "Convert a Jira tag (like FP_1234) into a Jira link with title.
Returns nil if tag doesn't match Jira pattern."
  (when (string-match "^\\([A-Z]+\\)_\\([0-9]+\\)$" tag)
    (format "[Jira %s-%s](https://commercetools.atlassian.net/browse/%s-%s)"
            (match-string 1 tag)
            (match-string 2 tag)
            (match-string 1 tag)
            (match-string 2 tag))))

(defun my/format-task (task)
  "Format a single TASK for display."
  (let* ((category (nth 0 task))
         (heading (nth 1 task))
         (tags (my/filter-tags (nth 2 task)))
         (priority (nth 3 task))
         (effort (nth 4 task))
         (parent (nth 5 task))
         (priority-str (if priority (format "[%s] " priority) ""))
         (effort-str (if effort (format " (%s)" effort) ""))
         (jira-links (delq nil (mapcar #'my/get-jira-link tags)))
         (display-heading (if parent
                              (format "%s - %s" parent heading)
                            heading))
         (jira-links-str (when jira-links
                           (format " %s"
                                   (string-join jira-links " ")))))
    (format "• %s%s%s%s\n"
            priority-str
            display-heading
            effort-str
            (or jira-links-str ""))))

(defun my/get-previous-workday (today)
  "Get the previous workday's ts object from TODAY.
If today is Monday, returns last Friday. Otherwise returns yesterday."
  (let* ((day-of-week (ts-dow today))
         (days-to-subtract (if (= day-of-week 1) 3 1))) ; If Monday (1), subtract 3 days
    (ts-adjust 'day (- days-to-subtract) today)))

(defun my/get-previous-workweek-range (today)
  "Get the date range for the previous work week (Mon-Fri) if TODAY is Monday.
Otherwise, returns the previous day as a single-day range.
Returns cons cell (start-date . end-date) as ts objects."
  (let* ((day-of-week (ts-dow today)))
    (if (= day-of-week 1) ; If Monday, get last week's work days (Mon-Fri)
        (let* ((days-to-friday (- 3)) ; 3 days back from Monday to get to Friday
               (days-to-monday (- 7)) ; 7 days back from Monday to get to last Monday
               (last-friday (ts-adjust 'day days-to-friday today))
               (last-monday (ts-adjust 'day days-to-monday today)))
          (cons
           (ts-apply :hour 0 :minute 0 :second 0 last-monday)
           (ts-apply :hour 23 :minute 59 :second 59 last-friday)))
      ; For other days, just return previous day as a range
      (let* ((yesterday (ts-adjust 'day -1 today)))
        (cons
         (ts-apply :hour 0 :minute 0 :second 0 yesterday)
         (ts-apply :hour 23 :minute 59 :second 59 yesterday))))))

(defun my/get-date-range (date)
  "Get start and end of DATE as ts objects."
  (let ((start (ts-apply :hour 0 :minute 0 :second 0 date))
        (end (ts-apply :hour 23 :minute 59 :second 59 date)))
    (cons start end)))

(defun my/task-filter-tags ()
  "Return the list of tags that should exclude tasks from standup messages."
  '("no_announce" "prv"))

(defun my/get-scheduled-time (pom)
  "Get the scheduled time for point-or-marker POM.
Returns a cons cell (HAS-TIME . TIMESTAMP) where HAS-TIME is t if the
timestamp includes a time, and TIMESTAMP is the full time value for sorting."
  (let* ((scheduled-time (org-entry-get pom "SCHEDULED"))
         (ts (when scheduled-time
               (org-timestamp-from-string scheduled-time))))
    (if ts
        (cons (org-timestamp-has-time-p ts)
              (apply #'encode-time (org-parse-time-string scheduled-time)))
      (cons nil nil))))

(defun my/generate-standup-message ()
  "Generate a Slack standup message based on today's scheduled tasks, yesterday's completed tasks, and clocked tasks."
  (interactive)
  (let* ((today (ts-now))
         (day-of-week (ts-dow today))
         (is-monday (= day-of-week 1))
         (template (if is-monday my/standup-monday-template my/standup-template))
         (prev-workday-range (my/get-previous-workweek-range today))
         (exclude-tags (my/task-filter-tags))
         ;; Get today's planned tasks
         (planned-tasks (org-ql-query
                          :select '(list (org-get-category)
                                         (org-get-heading t t t t)
                                         (org-get-tags)
                                         (org-element-property :priority (org-element-at-point))
                                         (org-entry-get nil "EFFORT")
                                         (my/get-parent-context)
                                         (my/get-scheduled-time (point)))
                          :from (org-agenda-files)
                          :where `(and (scheduled :on today)
                                       (not (tags ,@exclude-tags)))))
         ;; Sort planned tasks by scheduled time
         (sorted-planned-tasks
          (sort planned-tasks
                (lambda (a b)
                  (let ((time-a (nth 6 a))
                        (time-b (nth 6 b)))
                    (cond
                     ;; Both have times, compare timestamps
                     ((and (car time-a) (car time-b))
                      (time-less-p (cdr time-a) (cdr time-b)))
                     ;; Only a has time, a comes first
                     ((car time-a) t)
                     ;; Only b has time, b comes first
                     ((car time-b) nil)
                     ;; Neither has time, use priority
                     (t (let ((pri-a (nth 3 a))
                              (pri-b (nth 3 b)))
                          (if (and pri-a pri-b)
                              (string< pri-a pri-b)
                            (if pri-a t nil)))))))))
         ;; Get completed tasks from previous workday
         (completed-tasks (org-ql-query
                            :select '(list (org-get-category)
                                           (org-get-heading t t t t)
                                           (org-get-tags)
                                           (org-element-property :priority (org-element-at-point))
                                           (org-entry-get nil "EFFORT")
                                           (my/get-parent-context))
                            :from (org-agenda-files)
                            :where `(and (done)
                                         (closed :from ,(car prev-workday-range) :to ,(cdr prev-workday-range))
                                         (not (tags ,@exclude-tags)))
                            :order-by '(priority)))
         ;; Get clocked tasks from previous workday
         (clocked-tasks (org-ql-query
                          :select '(list (org-get-category)
                                         (org-get-heading t t t t)
                                         (org-get-tags)
                                         (org-element-property :priority (org-element-at-point))
                                         (org-entry-get nil "EFFORT")
                                         (my/get-parent-context))
                          :from (org-agenda-files)
                          :where `(and (clocked :from ,(car prev-workday-range) :to ,(cdr prev-workday-range))
                                       (not (tags ,@exclude-tags)))
                          :order-by '(priority)))
         (message-text
          (with-temp-buffer
            (insert (format
                     template
                     (if completed-tasks
                         (mapconcat #'my/format-task completed-tasks "")
                       (if is-monday
                           "\n• _No tasks completed last week_\n"
                         "\n• _No tasks completed yesterday_\n"))
                     (if clocked-tasks
                         (mapconcat #'my/format-task clocked-tasks "")
                       "\n• _No tasks clocked_\n")
                     (if sorted-planned-tasks
                         (mapconcat (lambda (task)
                                      (my/format-task (butlast task))) sorted-planned-tasks "")
                       "\n• _No tasks scheduled_\n")))
            (buffer-string))))
    (kill-new message-text)
    (message "Standup message copied to clipboard!")
    (with-current-buffer (get-buffer-create "*Standup Preview*")
      (erase-buffer)
      (insert message-text)
      (switch-to-buffer-other-window (current-buffer)))))

(defun my/insert-standup-message ()
  "Insert the standup message at point."
  (interactive)
  (let ((message-text (with-current-buffer "*Standup Preview*"
                        (buffer-string))))
    (insert message-text)))

(defun my/generate-standup-message-as-day (day-number)
  "Generate a standup message as if today were the specified day.
DAY-NUMBER is the day of week number (0=Sunday, 1=Monday, ..., 6=Saturday).
This is useful for testing the Monday behavior on other days."
  (interactive "nEnter day number (0=Sun, 1=Mon, ..., 6=Sat): ")
  (let* ((today (ts-now))
         (current-dow (ts-dow today))
         (day-diff (- day-number current-dow))
         (simulated-day (ts-adjust 'day day-diff today))
         (ts-now-orig (symbol-function 'ts-now)))
    ;; Temporarily override ts-now to return our simulated day
    (cl-letf (((symbol-function 'ts-now) (lambda () simulated-day)))
      (message "Generating standup message as if today were %s"
               (nth day-number '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")))
      (my/generate-standup-message))))

(defun my/org-count-todays-tasks ()
  "Count tasks completed and created today.
Returns a string with the statistics."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (done-count 0)
         (created-count 0))

    ;; Search for all tasks marked DONE today
    (org-map-entries
     (lambda ()
       (let ((closed (org-entry-get (point) "CLOSED")))
         (when (and closed (string-match today closed))
           (setq done-count (1+ done-count)))))
     "TODO=\"DONE\"" 'agenda)

    ;; Search for all tasks created today
    (org-map-entries
     (lambda ()
       (save-excursion
         (let ((end (save-excursion (outline-next-heading) (point)))
               (timestamp-regex (concat "\\[" today)))
           (forward-line 1)  ;; Move past the heading
           (when (and (< (point) end)
                      (re-search-forward timestamp-regex end t))
             (setq created-count (1+ created-count))))))
     "TODO={.+}" 'agenda)  ;; Match all TODO states

    ;; Format and return the results
    (format "- Tasks completed today: %d
- New tasks created today: %d"
            done-count created-count)))

(defun my/add-to-refile (text)
  "Add TEXT to the refile.org file."
  (save-window-excursion
    (find-file (concat my-org-file-path "/refile.org"))
    (goto-char (point-max))
    (insert "\n")
    (insert text)
    (save-buffer)))

(defun my/add-executable-to-exec-path (executable)
  "Find EXECUTABLE using whereis and add its directory to exec-path."
  (let* ((whereis-output (shell-command-to-string (concat "whereis " executable)))
         (exec-file-path (when (string-match (concat "/" "[^ ]+" "/" executable) whereis-output)
                           (match-string 0 whereis-output))))
    (when exec-file-path
      (let ((exec-dir (file-name-directory exec-file-path)))
        (add-to-list 'exec-path exec-dir)
        (message "Added %s to exec-path" exec-dir)))))

(defun my/add-essential-executables-to-exec-path ()
  "Add essential executables (git, node, sh, ispell, languagetool, ripgrep) to exec-path."
  (interactive)
  (dolist (executable '("git" "node" "sh" "ispell" "languagetool" "rg"))
    (my/add-executable-to-exec-path executable)))

;; Run when Emacs starts
(eval-after-load 'org
  '(my/add-essential-executables-to-exec-path))

(defun my-enable-lsp-for-visible-php-buffers (frame)
  "Enable LSP mode for visible PHP buffers that don't have it active."
  (dolist (window (window-list frame))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'php-mode)
                 (not (bound-and-true-p lsp-mode)))
        (lsp-deferred)))))

(add-hook 'window-buffer-change-functions #'my-enable-lsp-for-visible-php-buffers)

;; Toggle for inserting dates after created headers
(defvar my/org-auto-insert-created-date t
  "When non-nil, automatically insert creation date into new org headings.")

(defun my/toggle-org-auto-insert-created-date ()
  "Toggle automatic insertion of creation dates in org headings."
  (interactive)
  (setq my/org-auto-insert-created-date (not my/org-auto-insert-created-date))
  (message "The Chronicles of Creation %s"
           (if my/org-auto-insert-created-date "pulse with power" "lie dormant")))

;; Function to insert date after created headers
(defun my/insert-created-date (&rest ignore)
  "Insert creation date into org heading if `my/org-auto-insert-created-date' is non-nil."
  (when my/org-auto-insert-created-date
    (insert "\n")
    (org-insert-time-stamp (current-time) 't 't)
    (org-back-to-heading) ;; in org-capture, this folds the entry; when inserting a heading, this moves point back to the heading line
    (move-end-of-line))) ;; when inserting a heading, this moves point to the end of the line

(advice-add 'org-insert-heading :after #'my/insert-created-date)

(defun org-edna-action/eval-babel! (last-entry block-name)
  "Execute the named Babel source block specified by BLOCK-NAME.
LAST-ENTRY is the marker for the current heading."
  (save-excursion
    (with-current-buffer (marker-buffer last-entry)
      (goto-char last-entry)
      (org-babel-goto-named-src-block block-name)
      (org-babel-execute-src-block))))

(with-eval-after-load 'gptel
  (setq gptel-directives
        (append gptel-directives
                '((performance-review-creation .
                                               "You are helping me write professional peer feedback for end-of-year performance reviews. I will provide bullet points about a colleague, and you should convert them into a well-structured, professional feedback text.

Requirements:
- Write in a professional but warm tone
- Use specific examples when provided
- The feedback is either for \"Strenghts and Positive Behaviours\" or \"Areas for Development\". You can tell from the notes given.
- Keep it concise but substantive (aim for 200-300 words total)
- Use first person perspective (\"I have observed...\", \"In my experience working with...\")
- Make it constructive and actionable
- Avoid generic corporate language - be authentic and specific")
                  (performance-review-review .
                                             "Revise the given performance review text. The revised text should maintain the original meaning and key points, while improving the grammar, spelling, and alignment with Radical Candor principles.")
                  (emacs-configurator .
                                      "You are an expert emacs user. I'm asking questions about customizing my emacs instance by modifying configuration (emacs-lisp code) or understanding how parts of emacs work. I'm a professional developer, I know how to program and have a good understanding of Linux, but I'm new to emacs lisp")))))

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "search_org_items"
   :function (lambda (query &optional scope)
               (require 'org-ql)
               (let ((search-scope (or scope "agenda"))
                     (buffers-or-files nil)
                     (refile-files nil)
                     (all-files nil)
                     (results '())
                     (start-time (current-time))
                     (frontastic-query nil)
                     (parsed-query nil))

                 ;; Parse the query string into a sexp
                 (condition-case err
                     (setq parsed-query (read query))
                   (error
                    (message "[org-ql search] ERROR: Invalid Lisp syntax in query: %s" (error-message-string err))
                    (error "Query must be valid Lisp syntax, e.g., '(todo)' or '(and (todo) (tags \"project\"))'")))

                 ;; Create frontastic-filtered query
                 (setq frontastic-query `(and (tags "frontastic") ,parsed-query))

                 ;; Log the original and modified query parameters
                 (message "[org-ql search] Original query: %s | Scope: %s" query search-scope)
                 (message "[org-ql search] Frontastic query: %S" frontastic-query)
                 (message "[org-ql search] Refile query: %S" parsed-query)

                 (cond
                  ((string= search-scope "current")
                   ;; Search current buffer only
                   (if (derived-mode-p 'org-mode)
                       (progn
                         (setq buffers-or-files (list (current-buffer)))
                         (message "[org-ql search] Searching current buffer: %s" (buffer-name)))
                     (error "Current buffer is not an org-mode buffer")))
                  ((string= search-scope "agenda")
                   ;; Search agenda files + refile.org
                   (setq all-files (org-agenda-files))
                   (setq refile-files (seq-filter (lambda (file)
                                                    (string-match-p "refile\\.org$" file))
                                                  all-files))
                   (setq buffers-or-files all-files)
                   (message "[org-ql search] Searching %d agenda files (including %d refile.org files): %s"
                            (length buffers-or-files)
                            (length refile-files)
                            (mapconcat 'file-name-nondirectory buffers-or-files ", ")))
                  (t
                   ;; Search all org files (default to agenda files)
                   (setq all-files (org-agenda-files))
                   (setq refile-files (seq-filter (lambda (file)
                                                    (string-match-p "refile\\.org$" file))
                                                  all-files))
                   (setq buffers-or-files all-files)
                   (message "[org-ql search] Searching %d files (default scope, including %d refile.org files): %s"
                            (length buffers-or-files)
                            (length refile-files)
                            (mapconcat 'file-name-nondirectory buffers-or-files ", "))))

                 ;; First search for frontastic-tagged items in all files
                 (condition-case err
                     (progn
                       (message "[org-ql search] Executing org-ql query for frontastic items...")
                       (setq results
                             (org-ql-select buffers-or-files
                               frontastic-query
                               :action '(list :heading (org-get-heading t t t t)
                                              :file (buffer-file-name)
                                              :tags (org-get-tags)
                                              :todo-keyword (org-get-todo-state)
                                              :priority (org-get-priority (org-get-heading))
                                              :outline-path (org-get-outline-path t))))
                       (message "[org-ql search] Found %d frontastic-tagged results" (length results)))
                   (error
                    (message "[org-ql search] ERROR in frontastic search: %s" (error-message-string err))))

                 ;; Then search for items in refile.org files (without frontastic tag requirement)
                 (when refile-files
                   (condition-case err
                       (progn
                         (message "[org-ql search] Executing org-ql query for refile.org items...")
                         (let ((refile-results
                                (org-ql-select refile-files
                                  parsed-query
                                  :action '(list :heading (org-get-heading t t t t)
                                                 :file (buffer-file-name)
                                                 :tags (org-get-tags)
                                                 :todo-keyword (org-get-todo-state)
                                                 :priority (org-get-priority (org-get-heading))
                                                 :outline-path (org-get-outline-path t)))))
                           (setq results (append results refile-results))
                           (message "[org-ql search] Found %d additional refile.org results" (length refile-results))))
                     (error
                      (message "[org-ql search] ERROR in refile.org search: %s" (error-message-string err)))))

                 (message "[org-ql search] Total found %d results in %.2f seconds"
                          (length results)
                          (float-time (time-subtract (current-time) start-time)))

                 (if results
                     (progn
                       (message "[org-ql search] Returning formatted results")
                       (mapconcat
                        (lambda (item)
                          (let ((id (plist-get item :id))
                                (heading (plist-get item :heading))
                                (todo-keyword (plist-get item :todo-keyword))
                                (file (plist-get item :file))
                                (outline-path (plist-get item :outline-path))
                                (tags (plist-get item :tags)))
                            (format "- %s%s%s%s\n  File: %s\n  Path: %s\n  Tags: %s\n"
                                    (or todo-keyword "")
                                    (if todo-keyword " " "")
                                    heading
                                    (if id (format " [ID: %s]" id) "")
                                    (file-name-nondirectory (or file ""))
                                    (mapconcat 'identity outline-path " > ")
                                    (mapconcat 'identity (or tags '()) ", "))))
                        results "\n"))
                   (progn
                     (message "[org-ql search] No results found")
                     "No matching org items found."))))
   :description "Search for org mode items using org-ql Lisp syntax. Results are automatically limited to items with the 'frontastic' tag OR items in the refile.org file. Use proper Lisp syntax for queries."
   :args (list '(:name "query"
                       :type string
                       :description "org-ql query in Lisp syntax (e.g., '(todo)', '(tags \"project\")', '(and (todo) (priority \"A\"))', '(or (scheduled :from today) (deadline :to +7))', '(heading-regexp \"meeting\")'). Note different timestamp predicates: 'ts' (any timestamp), 'ts-active' (only active), 'ts-inactive' (only inactive), 'scheduled' (match scheduled timestamp), 'deadline' (match deadline timestamp) and 'planning' (i.e. its deadline, scheduled, or closed timestamp). Do NOT mix syntaxes - use only Lisp forms.")
               '(:name "scope"
                       :type string
                       :description "Search scope: 'current' (current buffer), 'agenda' (agenda files - default), or 'all' (same as agenda)"))
   :category "org-mode"))

(defcustom my/org-link-preview-lines 15
  "Number of lines to show in source file preview."
  :type 'integer
  :group 'org-link)

(defcustom my/org-link-preview-context-lines 5
  "Number of context lines to show before and after target line."
  :type 'integer
  :group 'org-link)

(defvar-local my/org-link-preview-mode nil
  "Whether automatic link preview mode is enabled in this buffer.")

(defvar-local my/org-link-preview-overlay nil
  "Overlay used for displaying the current link preview.")

(defvar-local my/org-link-preview-last-position nil
  "Last position where preview was shown.")

(defun my/org-link-find-line-by-text (file search-text)
  "Find line number in FILE that contains SEARCH-TEXT.
Returns line number or nil if not found."
  (when (and file (file-exists-p file) search-text)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (search-forward search-text nil t)
        (line-number-at-pos)))))

(defun my/org-link-get-file-and-line ()
  "Get file path and line number from link at point.
Returns (FILE . LINE) or nil if not on a file link.
Handles both numeric line references (::42) and text searches (::some text)."
  (when (org-in-regexp org-link-bracket-re 1)
    (let* ((link (org-element-lineage (org-element-context) '(link) t))
           (type (org-element-property :type link))
           (path (org-element-property :path link))
           (search-option (org-element-property :search-option link)))
      (when (and link (member type '("file" "attachment")))
        (let* ((file (expand-file-name path))
               (line (when search-option
                       (if (string-match "^\\([0-9]+\\)$" search-option)
                           ;; Numeric line reference
                           (string-to-number (match-string 1 search-option))
                         ;; Text search - find the line number
                         (my/org-link-find-line-by-text file search-option)))))
          (cons file line))))))

(defun my/org-link-preview-remove ()
  "Remove the current link preview overlay."
  (when my/org-link-preview-overlay
    (delete-overlay my/org-link-preview-overlay)
    (setq my/org-link-preview-overlay nil)
    (setq my/org-link-preview-last-position nil)))

(defun my/org-link-get-language-from-extension (file)
  "Get org-babel language identifier from FILE extension.
Uses auto-mode-alist to find the major mode, then derives language name."
  (let* ((mode (assoc-default file auto-mode-alist 'string-match))
         (mode-name (and mode (symbol-name mode))))
    (if mode-name
        ;; Remove '-mode' suffix and handle special cases
        (let ((lang (replace-regexp-in-string "-mode$" "" mode-name)))
          (cond
           ;; Check org-src-lang-modes for reverse mappings
           ((assoc lang org-src-lang-modes) lang)
           ;; Handle common special cases
           ((string= lang "js") "javascript")
           ((string= lang "emacs-lisp") "emacs-lisp")
           (t lang)))
      ;; Fallback to file extension if no mode found
      (or (file-name-extension file) "text"))))

(defun my/org-link-apply-syntax-highlighting (content file)
  "Apply syntax highlighting to CONTENT based on FILE's major mode.
Returns the highlighted content as a string."
  (with-temp-buffer
    (insert content)
    (let ((mode (assoc-default file auto-mode-alist 'string-match)))
      (when (and mode (fboundp mode))
        (ignore-errors
          (funcall mode)
          (font-lock-ensure))))
    (buffer-string)))

(defun my/org-link-format-preview-content (content start-line target-line)
  "Format CONTENT as preview with optional TARGET-LINE highlighting.
START-LINE is the first line number in the content.
Returns formatted string with line markers and highlighting."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((current-line start-line)
          (result ""))
      (while (not (eobp))
        (let* ((line-text (buffer-substring (point) (line-end-position)))
               (is-target (and target-line (= current-line target-line))))
          (setq result
                (concat result
                        (if is-target
                            (propertize (concat "👉 " line-text "\n") 'face 'hl-line)
                          (concat "   " line-text "\n"))))
          (setq current-line (1+ current-line))
          (forward-line 1)))
      result)))

(defun my/org-link-get-file-preview (file &optional target-line)
  "Get preview text for FILE, optionally centered around TARGET-LINE.
Returns propertized string formatted as an org source block."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((language (my/org-link-get-language-from-extension file))
             (start-line (if target-line
                             (max 1 (- target-line my/org-link-preview-context-lines))
                           1))
             (end-line (if target-line
                           (+ target-line my/org-link-preview-context-lines)
                         my/org-link-preview-lines)))
        ;; Extract and highlight the content
        (goto-char (point-min))
        (forward-line (1- start-line))
        (let* ((content-start (point))
               (content-end (progn (forward-line (- end-line start-line -1)) (point)))
               (content (buffer-substring content-start content-end))
               (highlighted-content (my/org-link-apply-syntax-highlighting content file))
               (formatted-content (my/org-link-format-preview-content
                                   highlighted-content start-line target-line))
               (line-info (if target-line
                              (format " :file %s :line %d"
                                      (file-name-nondirectory file) target-line)
                            (format " :file %s :lines %d-%d"
                                    (file-name-nondirectory file) start-line end-line))))
          ;; Return as org source block
          (concat (propertize (format "#+begin_src %s%s\n" language line-info)
                              'face 'org-block-begin-line)
                  formatted-content
                  (propertize "#+end_src" 'face 'org-block-end-line)))))))

(defun my/org-link-show-preview-at-point ()
  "Show preview for file link at point if mode is enabled."
  (when (and my/org-link-preview-mode
             (derived-mode-p 'org-mode))
    (let ((file-and-line (my/org-link-get-file-and-line))
          (current-pos (point)))
      ;; Check if we're on a different link or no link
      (if (or (not file-and-line)
              (not (equal current-pos my/org-link-preview-last-position)))
          (progn
            ;; Remove old preview if we moved away or to different link
            (my/org-link-preview-remove)
            ;; Create new preview if on a link
            (when file-and-line
              (let* ((file (car file-and-line))
                     (line (cdr file-and-line))
                     (preview-text (my/org-link-get-file-preview file line)))
                (when preview-text
                  (save-excursion
                    (end-of-line)
                    (let ((ov (make-overlay (point) (point))))
                      (overlay-put ov 'after-string (concat "\n" preview-text "\n"))
                      (overlay-put ov 'my-org-link-preview t)
                      (setq my/org-link-preview-overlay ov)
                      (setq my/org-link-preview-last-position current-pos)))))))))))

(defun my/org-toggle-link-preview-mode ()
  "Toggle automatic preview mode for file links.
When enabled, shows preview automatically when cursor is on a file link."
  (interactive)
  (setq my/org-link-preview-mode (not my/org-link-preview-mode))
  (if my/org-link-preview-mode
      (progn
        (add-hook 'post-command-hook #'my/org-link-show-preview-at-point nil t)
        (message "Link preview mode enabled"))
    (progn
      (remove-hook 'post-command-hook #'my/org-link-show-preview-at-point t)
      (my/org-link-preview-remove)
      (message "Link preview mode disabled"))))

;; Clean up previews when killing buffer
(add-hook 'kill-buffer-hook 'my/org-link-preview-remove)

;; Bind to key in org-mode
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "lp" 'my/org-toggle-link-preview-mode))

(provide 'my-org-extensions)
