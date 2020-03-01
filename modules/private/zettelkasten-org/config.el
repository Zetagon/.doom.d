;;; private/zettelkasten-org/config.el -*- lexical-binding: t; -*-

(defvar zettelkasten-directory  "~/org/references/notes/"
  "FIXME")


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
                                    (message (file-name-base file))
                                    (zettelkasten-create-link file original-buffer)))))))

(defun zettelkasten-create-link (file original-buffer)
  "FIXME"
  (let ((backlink (concat "[[" (if (org-before-first-heading-p)
                                   (buffer-file-name original-buffer)
                                 (concat "id:" (org-id-get-create)))
                          "]["
                          (org-get-heading nil nil nil nil)
                          "]]"))
        link)
    (with-current-buffer original-buffer
      (with-current-buffer (find-file-noselect file)
        (setq link (concat "[[" (buffer-file-name) "]["
                           (file-name-base (buffer-file-name)) "]]"))
        (goto-char (point-max))
        (insert backlink))
      (insert link))))
