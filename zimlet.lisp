#!/bin/env ecl-shell

(setq *load-print* nil)
(setq *load-verbose* nil)

(load "/etc/zimlet.conf")

(require "xml")
(require "xml-template")
(require "zimbra")
(require "cgi")
(require "db2")

(defvar *prefs-template*
  (translate-logical-pathname "config:htdocs;prefs.html"))

(defvar *add-calendar-template* 
  (translate-logical-pathname "config:htdocs;add-calendar.html"))

(defvar *error-template*
  (translate-logical-pathname "config:htdocs;error.html"))

(defvar *login-template*
  (translate-logical-pathname "config:htdocs;login.html"))

(defvar *list-shares-template*
  (translate-logical-pathname "config:htdocs;list-shares.html"))

(defvar *db*
  "/var/local/db/tweaks/user.db")

(defun /prefs (&rest query)
  (restricted)
  (format t "Content-Type: text/html~2%")
  (with-db-open (db *db*)
    (with-db-lock (db)
      (let ((prefs (db-get (zimbra-get-account-name) db)))
        (xml-template-print *prefs-template* prefs)))))


(defun save-prefs (uid prefs)
  (with-db-open (db *db*)
    (with-db-lock (db)
      (db-put uid prefs db))))


(defun /prefs-save (&rest query &key agenda automount &allow-other-keys)
  (restricted)
  (let ((uid (zimbra-get-account-name)))
    (save-prefs uid (list :uid uid 
                          :automount (equal automount "t")
                          :agenda (equal agenda "t")))
    (format t "Location: https://~A~A/prefs~2%" 
            (cgi-server-name) (cgi-script-name))))


(defun /auth (&key user pass redir args &allow-other-keys)
  (when (zimbra-auth user pass)
    (format t "Set-Cookie: ~A=~A~%" *zimbra-auth-cookie* *zimbra-auth-token*)
    (format t "Location: https://~A~A~A?~A~%~%"
            (cgi-server-name) (cgi-script-name) redir args)
    t))

(defun /login (&rest query &key user &allow-other-keys)
  (unless (and user (apply #'/auth query))
    (format t "Content-Type: text/html~%~%")
    (xml-template-print *login-template* query)))


(defun /add-calendar (&rest query &key &allow-other-keys)
  (restricted)
  (unless (apply #'/add-calendar-confirm query)
    (format t "Content-Type: text/html~%~%")
    (xml-template-print *add-calendar-template* query)))

    
(defun /add-calendar-confirm (&rest query &key name url action &allow-other-keys)
  (restricted)
  (when (equal action "OK")
    (zimbra-create-folder-request name :view "appointment" :url url :f "#b")
    (format t "Location: https://~A/service/preauth?isredirect=1&authtoken=~A&redirectURL=/zimbra/?app=calendar~%~%"
            *zimbra-host* *zimbra-auth-token*)))

(defun /list-shares (&rest query)
  (restricted)
  (format t "Content-Type: text/html~2%")
  (xml-template-print *list-shares-template*
    (list :shares
       (mapcar #'(lambda (share)
                   (list :owner-name (zimbra-share-owner-email share)
                         :owner-id (zimbra-share-owner-id share)
                         :folder-path (concatenate 'string (zimbra-share-owner-name share)
                                                   (zimbra-sanitize-path (zimbra-share-folder-path share)))
                         :folder-id (zimbra-share-folder-id share)
                         :view (zimbra-share-view share)))
               (remove-if #'zimbra-share-mid (zimbra-shares))))))


(defun /subscribe (&rest query &key name owner-id folder-id flags view &allow-other-keys)
  (restricted)
  (zimbra-mount name owner-id folder-id :view view)
  (format t "Location: https://~A~A/list-shares~2%"
          (cgi-server-name) (cgi-script-name)))

(defun redirect-to-login-page ()
  (format t "Location: https://~A~A/login?redir=~A&args=~A~%~%" 
          (cgi-server-name)
          (cgi-script-name)
          (cgi-url-encode (cgi-path-info))
          (cgi-url-encode (cgi-query-string))))

(defun authenticatedp ()
  (setf *zimbra-auth-token* (cgi-get-cookie *zimbra-auth-cookie*)))

(define-condition authorization-required (condition) ())

(defun restricted ()
  (unless (authenticatedp)
    (signal 'authorization-required)))

(defun cgi-dispatch ()
  (let ((fname (intern (string-upcase (cgi-path-info)))))
    (apply (symbol-function fname) (cgi-get-query-plist))))

(defun send-error (e)
  (format t "Content-Type: text/html~%~%")
  (xml-template-print *error-template* (list :error (format nil "~A" e))))


(handler-case (cgi-dispatch)
  (authorization-required () (redirect-to-login-page))
  (error (e) (send-error e)))

