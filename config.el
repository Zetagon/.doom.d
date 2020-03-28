;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; (use-package! weechat)

;; Place your private configuration here
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :config
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-directory "~/org/references/notes")
  (map! (:leader
          :prefix "n"
          :desc "Org-Roam-Insert" "i" #'org-roam-insert
          :desc "Org-Roam-Find" "f" #'org-roam-find-file
          :desc "Org-Roam-Buffer" "r" #'org-roam)
        (:map org-roam-backlinks-mode-map
          :n "<return>" #'nil
          :n "RET" #'nil
          :n [return] #'nil
          :n [return] #'org-roam-open-at-point
          :n "<return>" #'org-roam-open-at-point
          :n "RET" #'org-roam-open-at-point))
  (add-hook 'org-roam-backlinks-mode-hook
            #'visual-line-mode)
  (org-roam-mode +1))

;; TODO: define variable for storing where the command was called form
;; and use that to insert link to
(after! (swiper org-roam)
  (defun my/counsel-ag-org-roam-action (candidate)
    "FIXME"
    (interactive)
    (with-ivy-window
      (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" candidate)
        (let ((file-name (match-string-no-properties 1 candidate))
              )
          (insert (format " %s "
                          (org-roam--format-link
                           file-name
                           (org-roam--format-link-title
                            (with-current-buffer (find-file-noselect file-name)
                              (org-roam--extract-titles))))))))))

  (defun my/counsel-rg-view-action (x)
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x))
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (cl-parse-integer (match-string-no-properties 2 x))))
      (find-file-other-window file-name)
      (with-current-buffer
          (find-file-noselect file-name)
        (goto-char (point-min))
        (forward-line (1- line-number)))))

  (defun my/search-notes ()
    (interactive)
    (ivy-set-action #'my/counsel-ag-org-roam-action)
    (counsel-rg nil org-roam-directory))
  (map! :map counsel-ag-map
        "C-l" (λ! (ivy-set-action #'my/counsel-ag-org-roam-action)
               (ivy-call))
        "RET" (λ! (ivy-set-action #'counsel-git-grep-action)
                  (ivy-done))
        "TAB" (λ! (ivy-set-action #'my/counsel-rg-view-action)
                  (ivy-call)))

  (ivy-set-actions #'my/search-notes
                   '(("l" my/counsel-ag-org-roam-action "Link to zettelkasten")
                     ("p" my/counsel-rg-view-action
                      "View in other window"))))

(when (featurep! :config default)
  (map! :leader
        "n/" #'my/search-notes))

(my/counsel-ag-org-roam-action
 "orgzly/2020-03-18-1357-12-nen-and-personality.org:15:[[tmp:2020-03-18-1347-02-hunter-x-hunter-nen.org][2020-03-18-1347-02-hunter-x-hunter-nen]]") [[file:orgzly/2020-03-18-1357-12-nen-and-personality.org][orgzly/2020-03-18-1357-12-nen-and-personality.org]]
(use-package! org-noter
  :defer t
  :config
  (map!
   (:map pdf-view-mode-map
     :n "i" #'nil)
   :map org-noter-doc-mode-map
   :n "i" #'org-noter-insert-note
   :n "M-n" #'org-noter-sync-next-note
   :n "M-p" #'org-noter-sync-prev-note))

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
