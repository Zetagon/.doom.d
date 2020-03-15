;;; private/zettelkasten-org/config.el -*- lexical-binding: t; -*-

;; ---------------------------------------------------
;; -------------------- Variables --------------------
;; ---------------------------------------------------
(after! helm
  (defcustom zettelkasten-id-format "%Y-%m-%d-%H%M-%S"
    "Format used when generating zettelkasten IDs.

Be warned: the regexp to find IDs is set separately.
If you change this value, set `zettelkasten-id-regex' so that
the IDs can be found.

Check the documentation of the `format-time-string'
function to see which placeholders can be used."
    :type 'string
    :group 'zettelkasten)

  (defvar zettelkasten-directory  "~/org/references/notes/"
    "The directory where the zettelkasten resides")

  (defvar zettelkasten-referenced-section "Referenced in"
    "The org heading to put backlinks in.")

  (defvar zettelkasten-scrapbook-description-prefix "sb:"
    "The prefix to description in backlinks to scrapbook")

  (defvar zettelkasten-visit-stack '()
    "A stack of zettels to be visited either because you have sidetracked or becaues you have a hole to fill in.")

  (defvar zettelkasten-helm-action-list
    (helm-make-actions
     "Insert link here"
     #'zettelkasten--create-link-action
     "Create a link with word at point or region as description"
     #'zettelkasten--create-link-with-word-action)
    "Default action list for `zettelkasten-helm`")

  (defvar zettelkasten-link-from-word-action-list
    (helm-make-actions
     "Create a link with word or region as description"
     #'zettelkasten--create-link-with-word-action
     "Insert link here"
     #'zettelkasten--create-link-action)
    "Action list for `zettelkasten-helm` with `zettelkasten--create-link-with-word-action` first.")

  (defvar zettelkasten-current-helm-action-list
    zettelkasten-helm-action-list)

  (defvar zettelkasten-prefil-input
    ""
    "String to pre-fill the helm prompt with.")

  (defun zettelkasten--create-link-action (original-buffer)
    (zettelkasten--insert-link-action original-buffer))

  (defun zettelkasten--create-link-with-word-action (original-buffer)
    "A helm action that uses the word at point or region as description when creating a link."
    (lambda (file)
      (let ((word (zettelkasten--delete-word-or-region)))
        (funcall (zettelkasten--insert-link-action original-buffer word)
                 file))))

  ;; ---------------------------------------------------
  ;; -------------- Interactive Functions --------------
  ;; ---------------------------------------------------

  (defun zettelkasten-helm ()
    "The main entry point for the package. Choose a file and an action to execute on that file.

The action list can be customized by setting
`zettelkasten-current-helm-action-list`. The type for the functions in the action list is:
Buffer -> File -> IO ()
Buffer is the buffer is the buffer from which `zettelkasten-helm` was called.
File is the file chosen by the user using helm.

Note: The function type is curried, meaning that the function should return another function."
    (interactive)
    (let ((original-buffer (current-buffer)))
      (helm
       :input zettelkasten-prefil-input
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
                       zettelkasten-current-helm-action-list))))
    (setq zettelkasten-prefil-input ""))

  (defun zettelkasten-create-link-from-word-at-point ()
    "Like `zettelkasten-helm` but uses `zettelkasten-link-from-word-action-list` as action list."
    (interactive)
    (let* ((zettelkasten-current-helm-action-list zettelkasten-link-from-word-action-list)
           (bound (zettelkasten--get-bounds-of-thing-at-point))
           (zettelkasten-prefil-input (if bound
                                          (progn (buffer-substring-no-properties
                                                  (car bound)
                                                  (cdr bound)))
                                        "")))
      (zettelkasten-helm)))

  (defun zettelkasten-begin-sidetrack (name)
    "Insert a link to a new zettel and go to that newly created zettel."
    (interactive "sZettel Name: ")
    (add-to-list 'zettelkasten-visit-stack (current-buffer))
    (let ((new-zettel (zettelkasten-generate-file-name name)))
      (zettelkasen--insert-link
       new-zettel
       (current-buffer))
      (find-file new-zettel)))

  (defun zettelkasten-pop-visit-stack ()
    "Pop and goto the file at the top of the visit stack"
    (interactive)
    (when zettelkasten-visit-stack
      (find-file (pop zettelkasten-visit-stack))))

  (defun zettelkasten-new-zettel (name)
    (interactive "sZettel Name: ")
    (find-file (concat zettelkasten-directory (zettelkasten-generate-file-name name))))

  ;; ---------------------------------------------------
  ;; -------------- Helper Functions --------------
  ;; ---------------------------------------------------

  (defun zettelkasten--helm-filter-transformer (cand-list source)
    "A transformer for helm. Use the text the user has typed in if there are no other candidates left.

Note: To use this function no candidate is allowed to start with \"[?] \""
    (if cand-list
        cand-list
      ;; This is the case when there are no candidates left
      (list (concat "[?] " helm-pattern))))

  (defun zettelkasten--insert-link-action (original-buffer &optional description)
    "An action for `zettelkasten-helm`. Insert a link to chosen
file in ORIGINAL-BUFFER and insert a backlink in chosen file."
    (lambda (file)
      (if (s-starts-with? "[?] " file) ;; The create a new file case
          (progn
            (let ((new-file (zettelkasten--prompt-file-name (cadr (s-split-up-to " " file 1 t)))))
              (add-to-list 'zettelkasten-visit-stack new-file)
              (zettelkasen--insert-link
               new-file
               original-buffer
               description)))
        (zettelkasen--insert-link file original-buffer description))))

  (defun zettelkasten--prompt-file-name (&optional default-name)
    "Prompt the user for a name for a new zettel. Return a correctly formatted filename."
    (zettelkasten-generate-file-name (read-string "Create new zettel: " default-name nil default-name)))

  (defun zettelkasten-generate-file-name (name)
    "Create a proper file name with id and file extension from NAME"
    (concat (zettelkasten-generate-id) "-"
            (zettelkasten--normalize-file-name name)
            ".org"))

  (defun zettelkasten--normalize-file-name (name)
    "Normalize the non-id part of a file name. Dont call this on a file name with an ID"
    (s-replace " " "-" (s-trim name)))

  (defun zettelkasten--get-bounds-of-thing-at-point ()
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (bounds-of-thing-at-point 'symbol)))

  (defun zettelkasten--delete-word-or-region ()
    "Delete word or region and return the deleted text."
    (let* ((bounds (zettelkasten--get-bounds-of-thing-at-point))
           (text   (if bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "")))
      (when bounds
        (delete-region (car bounds) (cdr bounds))
        text)))



  (defun zettelkasten-generate-id ()
    "Generate an ID in the format of `zettelkasten-id-format'."
    (format-time-string zettelkasten-id-format))


  (defun zettelkasen--insert-link (file original-buffer &optional description)
    "Insert a link to FILE in ORIGINAL-BUFFER and a backlink to
ORIGINAL-BUFFER in FILE.

 If DESCRIPTION is provided use that as description in the link(but not backlink)"
    (let ((backlink (concat "\n[["
                            (if (org-before-first-heading-p)
                                (concat "./" (file-relative-name (my/buffer-file-name original-buffer)))
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
    "Helper function to get the file name of BUFFER. This ignores indirect buffers."
    (buffer-file-name (buffer-base-buffer buffer)))

  (defun my/if-nil-default (x default)
    (if x
        x
      default))

  (defun zettelkasten--get-title ()
    (replace-regexp-in-string "-" " "
                              (replace-regexp-in-string
                               "[0-9]*-[0-9]*-[0-9]*-[0-9]*-[0-9]*" ""
                               (capitalize (file-name-base buffer-file-name)))))

)
