(load "config.lisp")

(defun install (dest &rest files)
  (ensure-directories-exist dest)
  (dolist (filespec files)
    (let ((new-name (merge-pathnames filespec dest)))
      (copy-file filespec new-name))))

(defun copy-file (filespec new-name)
  (with-open-file (s filespec)
    (with-open-file (o new-name :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (let ((buffer (make-array 4096)))
        (loop as pos = (read-sequence buffer s)
              do (write-sequence buffer o :end pos)
              while (plusp pos))))))

(install "config:cgi-bin;zimlet" "zimlet.lisp")

(install "config:htdocs;*.*" "index.html" "login.html" 
         "add-calendar.html" "error.html" "style.css"
         "prefs.html" "list-shares.html")

(quit)

