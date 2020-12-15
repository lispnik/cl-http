(defun with-all-files-in-directory (verzeichnis aktion)
  (dolist (datei (directory (concatenate 'string (namestring verzeichnis) "*")
                            :files t
                            :directories nil))
    (funcall aktion datei))
  (dolist (v1 (directory (concatenate 'string (namestring verzeichnis) "*")
                         :files nil
                         :directories t))
    (with-all-files-in-directory v1 aktion)))

#|
To remove the binaries

(with-all-files-in-directory 
  (choose-directory-dialog)
  #'(lambda(file)
      (when (eql :fasl (mac-file-type file))
        (when (file-locked-p file)
          (unlock-file file))
        (format t "~s deleted~%" file)
        (delete-file file))))


|#
