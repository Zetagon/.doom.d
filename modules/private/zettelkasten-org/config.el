;;; private/zettelkasten-org/config.el -*- lexical-binding: t; -*-

(defvar zettelkasten-directory  "~/org/references/notes/"
  "FIXME")

(defvar zettelkasten-referenced-section "Referenced in")

(defvar zettelkasten-scrapbook-description-prefix "sb:"
  "The prefix to description in backlinks to scrapbook")

(defvar zettelkasten-visit-stack '()
  "A stack of zettels to be visited either because you have sidetracked or becaues you have a hole to fill in.")

(defvar zettelkasten-helm-action-list
  (helm-make-actions
   "Insert link here"
   #'zettelkasten--create-link-action
   "Create a link with word at point or region as description"
   #'zettelkasten--create-link-with-word-action))

(defvar zettelkasten-link-from-word-action-list
  (helm-make-actions
   "Create a link with word or region as description"
   #'zettelkasten--create-link-with-word-action
   "Insert link here"
   #'zettelkasten--create-link-action))

(defvar zettelkasten-current-helm-action-list
  zettelkasten-helm-action-list)

(defun zettelkasten--create-link-action (original-buffer)
  (zettelkasten--create-link original-buffer))

(defun zettelkasten--create-link-with-word-action (original-buffer)
  (lambda (file)
    (let ((word (zettelkasten--delete-word-or-region)))
      (funcall (zettelkasten--create-link original-buffer word)
               file))))

(defun zettelkasten-helm ()
  (interactive)
  (let ((original-buffer (current-buffer)))
    (helm
     :sources
     (helm-build-sync-source "Zettelkasten"
       :candidates
       (lambda ()  (directory-files zettelkasten-directory))
       :filtered-candidate-transformer
       #'zettelkasten--helm-filter-transformer
       :action (-map (lambda (pair)
                       (pcase pair
                         (`(,desc . ,fn)
                          `(,desc . ,(funcall fn original-buffer)))) )
                     zettelkasten-current-helm-action-list)))))

(defun zettelkasten-create-link-from-word-at-point ()
  (interactive)
  (let ((zettelkasten-current-helm-action-list zettelkasten-link-from-word-action-list))
    (zettelkasten-helm)))


(defun zettelkasten--helm-filter-transformer (cand-list source)
         (if cand-list
             cand-list
           ;; This is the case when there are no candidates left
           (list (concat "[?] " helm-pattern))))

(defun zettelkasten-begin-sidetrack (name)
  (interactive "sZettel Name: ")
  (add-to-list 'zettelkasten-visit-stack (current-buffer))
  (let ((new-zettel (zettelkasten-generate-file-name name)))
    (zettelkasen-insert-link
     new-zettel
     (current-buffer))
    (find-file new-zettel)))

(defun zettelkasten--create-link (original-buffer &optional description)
  (lambda (file)
    (if (s-starts-with? "[?] " file) ;; The create a new file case
        (progn
          (let ((new-file (zettelkasten--prompt-file-name (cadr (s-split-up-to " " file 1 t)))))
            (add-to-list 'zettelkasten-visit-stack new-file)
            (zettelkasen-insert-link
             new-file
             original-buffer
             description)))
      (zettelkasen-insert-link file original-buffer description))))

(defun zettelkasten--prompt-file-name (&optional default-name)
  (zettelkasten-generate-file-name (read-string "Create new zettel: " default-name nil default-name)))

(defun zettelkasten-generate-file-name (name)
  (concat (zettelkasten-generate-id) "-"
          (zettelkasten--normalize-file-name name)
          ".org"))

(defun zettelkasten--normalize-file-name (name)
  "Normalize the non-id part of a file name."
  (s-replace " " "-" (s-trim name)))

(defun zettelkasten-pop-new-file-stack ()
  (interactive)
  (find-file (pop zettelkasten-visit-stack)))

(defun zettelkasten--delete-word-or-region ()
  "Delete word or region and return the deleted text."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      text)))

(defcustom zettelkasten-id-format "%Y-%m-%d-%H%M-%S"
  "Format used when generating zettelkasten IDs.

Be warned: the regexp to find IDs is set separately.
If you change this value, set `zettelkasten-id-regex' so that
the IDs can be found.

Check the documentation of the `format-time-string'
function to see which placeholders can be used."
  :type 'string
  :group 'zettelkasten)
(defun zettelkasten-generate-id ()
  "Generate an ID in the format of `zettelkasten-id-format'."
  (format-time-string zettelkasten-id-format))


(defun zettelkasen-insert-link (file original-buffer &optional description)
  "FIXME"
  (let ((backlink (concat "\n[["
                          (if (org-before-first-heading-p)
                              (file-relative-name (my/buffer-file-name original-buffer))
                            (concat "id:" (org-id-get-create)))
                          "]["
                          (concat
                           (when (string-equal
                                  (file-name-base (my/buffer-file-name original-buffer))
                                  "scrapbook")
                             zettelkasten-scrapbook-description-prefix)
                           (my/if-nil-default (org-get-heading nil nil nil nil)
                                              (file-name-base (my/buffer-file-name original-buffer))))
                          "]]\n"))
        link)
    (with-current-buffer original-buffer
      (with-current-buffer (find-file-noselect file)
        (setq link (concat "[[./" (file-relative-name (my/buffer-file-name)) "]["
                           ;; if the user has specified a description, use that
                           (or description (file-name-base (my/buffer-file-name)))
                           "]]"))
        (goto-char (point-min))
        (unless
            (re-search-forward (concat "^\\*+[ \t]+" (regexp-quote zettelkasten-referenced-section) "[ \t]*$")
                               nil
                               'move-to-end)
          (insert (concat "* " zettelkasten-referenced-section "\n")))
        (outline-next-heading)
        (forward-line -1)
        (goto-char (line-end-position))
        (when (eobp) (insert "\n"))
        (insert backlink))
      (insert link))))


(defun my/buffer-file-name (&optional buffer)
  (buffer-file-name (buffer-base-buffer buffer)))

(defun my/if-nil-default (x default)
  (if x
      x
    default))
