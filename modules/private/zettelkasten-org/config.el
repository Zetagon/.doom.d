;;; private/zettelkasten-org/config.el -*- lexical-binding: t; -*-

(defvar zettelkasten-directory  "~/org/references/notes/"
  "FIXME")


(defun zettelkasten-insert-link ()
  (interactive)
  (let ((original-buffer (current-buffer)))
    (helm
     :sources (helm-build-sync-source "Zettelkasten"
                :candidates
                (lambda () (directory-files zettelkasten-directory))
                :action (helm-make-actions "Create Link"
                                           (lambda (file)
                                             (zettelkasten-create-link file original-buffer)))))))

(defun zettelkasten-create-link (file original-buffer)
  "FIXME"
  (with-current-buffer original-buffer
    (let ((file-name (buffer-file-name original-buffer))
          (original-buffer-marker (point-marker))
          (id (org-id-get-create)))
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (if (org-before-first-heading-p)
            (org-insert-link nil original-buffer-marker)
          (insert
           (concat
            "[[id:"
            id
            "]]")))))))
