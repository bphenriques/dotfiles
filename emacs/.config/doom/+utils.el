;;; ~/Documents/repos/workspace/emacs/.config/doom/+utils.el -*- lexical-binding: t; -*-

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun custom/list-directories (directory)
  (let* ((files (remove-if
                 (lambda (f) (or (equal f ".") (equal f "..")))
                 (directory-files directory)))
         (files-paths (seq-map
                       (lambda (f) (concat (file-name-as-directory directory) f))
                       files))
         (directory-paths (remove-if-not #'file-directory-p files-paths))
         (directories (seq-map #'file-name-nondirectory directory-paths)))
    directories))
