;;; private/zettelkasten-org/config.el -*- lexical-binding: t; -*-

(defvar zettelkasten-directory  "~/org/references/notes/"
  "FIXME")

(defvar zettelkasten-referenced-section "Referenced in")

(defun zettelkasten-insert-link ()
  (interactive)
  (let ((original-buffer (current-buffer)))
    (helm
     :sources
     (helm-build-sync-source "Zettelkasten"
       :candidates
       (lambda ()  (directory-files zettelkasten-directory))
       :filtered-candidate-transformer
       (lambda (cand-list source)
         (if cand-list
             cand-list
           (list (concat "[?] " helm-pattern))))
       :action (helm-make-actions "Create Link"
                                  (lambda (file)
                                    (if (s-starts-with? "[?] " file)
                                        (progn
                                          (zettelkasten-create-link
                                           (zettelkasten-generate-file-name (cadr (s-split-up-to " " file 1 t)))
                                           original-buffer))
                                      (zettelkasten-create-link file original-buffer))))))))

(defun zettelkasten-generate-file-name (name)
  (concat (zettelkasten-generate-id) "-" (read-string "Create new zettel: " name nil name) ".org"))

(defcustom zettelkasten-id-format "%Y-%m-%d-%H%M"
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


(defun zettelkasten-create-link (file original-buffer)
  "FIXME"
  (let ((backlink (concat "\n[[" (if (org-before-first-heading-p)
                                     (buffer-file-name original-buffer)
                                 (concat "id:" (org-id-get-create)))
                          "]["
                          (my/if-nil-default (org-get-heading nil nil nil nil)
                                             (file-name-base (buffer-file-name original-buffer)))
                          "]]\n"))
        link)
    (with-current-buffer original-buffer
      (with-current-buffer (find-file-noselect file)
        (setq link (concat "[[" (buffer-file-name) "]["
                           (file-name-base (buffer-file-name)) "]]"))
        (goto-char (point-min))
        (unless
            (re-search-forward (concat "^\\*+[ \t]+" (regexp-quote zettelkasten-referenced-section) "[ \t]*$")
                               nil
                               'move-to-end)
          (insert (concat "* " zettelkasten-referenced-section "\n")))
        (outline-next-heading)
        (previous-line 1)
        (goto-char (line-end-position))
        (when (eobp) (insert "\n"))
        (insert backlink))
      (insert link))))
(defun my/if-nil-default (x default)
  (if x
      x
    default))
