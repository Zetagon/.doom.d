;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; (use-package! weechat)

;; Place your private configuration here
(after! org-ql
  (defun my/today ()
    (interactive)
    (org-ql-search (append '("~/org/orgzly/InboxComputer.org" "~/org/orgzly/Inbox.org") (org-agenda-files))
      '(scheduled :on today)))

  (defun my//create-schedule (tags)
    (org-ql-search (org-agenda-files)
      `(and (todo)
            (not (org-entry-blocked-p))
            (not ,tags )
            (not (and
                  (property "BLOCKED" "")
                  (descendants (todo)))))
      :super-groups
      '((:name "Scheduled"
               :scheduled future)
        (:name "Deadlines"
               :deadline t
               :log t)
        (:auto-category t))))

  (defun my/create-schedule-for-today ()
    (interactive)
    (my//create-schedule '(tags "weeklyreview" "monthlyreview" "yearlyreview")))

  (defun my/create-schedule-for-week ()
    (interactive)
    (my//create-schedule '(tags "monthlyreview" "yearlyreview")))
  (defun my/create-schedule-for-month ()
    (interactive)
    (my//create-schedule '(tags "yearlyreview")))

  (defun my/anytime-todos ()
    (interactive)
    (org-ql-search (org-agenda-files)
      '(and (todo "TODO" "NEXT" "WAITING")
            (tags "anytime"))
      :super-groups
      '((:auto-category)))))
