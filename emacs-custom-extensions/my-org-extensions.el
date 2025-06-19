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
  "Add essential executables (git, node, sh, ispell, aider) to exec-path."
  (interactive)
  (dolist (executable '("git" "node" "sh" "ispell" "aider"))
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

(defun org-edna-action/eval-babel! (last-entry block-name)
  "Execute the named Babel source block specified by BLOCK-NAME.
LAST-ENTRY is the marker for the current heading."
  (save-excursion
    (with-current-buffer (marker-buffer last-entry)
      (goto-char last-entry)
      (org-babel-goto-named-src-block block-name)
      (org-babel-execute-src-block))))

(provide 'my-org-extensions)
