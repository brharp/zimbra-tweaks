;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Zimbra Daily Agenda
;;; Copyright (c) 2010 M. Brent Harp

(eval-when (:compile-toplevel) (load "config"))

(require "zimbra")

(defvar *disclaimer*
  "You received this email from the Daily Agenda Zimlet for Gryph Mail and Calendar.")

(defvar *agenda-conf*
  #.(translate-logical-pathname "config:etc;agenda.conf"))

(when (probe-file *agenda-conf*)
  (load *agenda-conf*))

(defun calendar-p (folder)
  (equal "appointment" (zimbra-folder-view folder)))

(defun in-calendar-query ()
  (let* ((folders (zimbra-folders))
         (calendar (remove-if-not #'calendar-p folders))
         (ids (mapcar #'zimbra-folder-id calendar)))
    (format nil "(inid:\"~A\"~{ or inid:\"~A\"~})" (first ids) (rest ids))))

(defun zimbra-agenda-request (start end)
  (zimbra-search-request (in-calendar-query)
      :types "appointment" :sort-by "dateAsc"
      :cal-expand-inst-start (format nil "~A" start)
      :cal-expand-inst-end   (format nil "~A" end)))

(defun appt-start-time (appt)
  (parse-integer (zimbra-inst-s (zimbra-appt-inst appt))))

(defun zimbra-agenda (start end)
  (let ((result (zimbra-agenda-request start end)))
    (let ((appts (xml-get-elements-by-tag-name result "appt")))
      (sort (mapcar #'zimbra-parse-appt appts) #'<
            :key #'appt-start-time))))

(defstruct zimbra-appt
  uid d rev all-day status is-org ms class loc l comp-num
  fb id alarm x-uid dur s md name fba inv-id transp ptst 
  or inst)

(defun zimbra-parse-appt (doc)
  (let ((appt (make-zimbra-appt
               :uid     (xml-get-attribute doc "uid")
               :all-day (xml-get-attribute doc "allDay")
               :status  (xml-get-attribute doc "status")
               :is-org  (xml-get-attribute doc "isOrg")
               :class   (xml-get-attribute doc "class")
               :loc     (xml-get-attribute doc "loc")
               :fb      (xml-get-attribute doc "fb")
               :dur     (xml-get-attribute doc "dur")
               :name    (xml-get-attribute doc "name")
               :ptst    (xml-get-attribute doc "ptst"))))
    (let ((inst (first (xml-get-elements-by-tag-name doc "inst"))))
      (setf (zimbra-appt-inst appt) (zimbra-parse-inst inst)))
    appt))
            
(defstruct zimbra-inst s rid-z tzo)

(defun zimbra-parse-inst (doc)
  (make-zimbra-inst :s (xml-get-attribute doc "s")
                    :rid-z (xml-get-attribute doc "ridZ")
                    :tzo (xml-get-attribute doc "tzo")))

(defun zimbra-time-from-universal-time (&optional (time (get-universal-time)))
  (* 1000 (- time (encode-universal-time 0 0 0 1 1 1970))))

(defun zimbra-to-universal-time (time)
  (+ (floor (/ time 1000)) (encode-universal-time 0 0 0 1 1 1970 0)))

(defun midnight ()
  (multiple-value-bind
   (sec min hour date month year)
   (get-decoded-time)
   (encode-universal-time 0 0 0 date month year)))

(defun zimbra-print-date-to-string (time)
  (multiple-value-bind
   (sec min hour date month year day daylight-p zone)
   (decode-universal-time (zimbra-to-universal-time time))
   (format nil "~2D:~2,'0D~A"
           (if (> hour 12) (- hour 12) hour) min
           (if (> hour 12) "p" "a"))))

(defun print-appointment (appt)
  (let* ((name  (zimbra-appt-name appt))
         (start (parse-integer (zimbra-inst-s (zimbra-appt-inst appt))))
         (end   (+ start (parse-integer (zimbra-appt-dur appt))))
         (loc   (zimbra-appt-loc appt)))
    (if (not (equal "1" (zimbra-appt-all-day appt)))
      (format t "~A - ~A " (zimbra-print-date-to-string start)
              (zimbra-print-date-to-string end))
      (format t "  -  "))
    (princ name)
    (when (and loc (plusp (length loc)))
      (format t " (~A)" loc))
    (format t "~%~%")))

(defun date (&optional (format "+%c"))
  (read-line (popen (list "date" format) "r")))

(defun print-agenda ()
  (let* ((start (zimbra-time-from-universal-time (midnight)))
         (end   (1- (+ start (* 1000 60 60 24))))
         (appts (zimbra-agenda start end)))
    (format t "*** ~A ***~%~%" (date "+%a, %b %d, %Y"))
    (if (null appts)
        (format t "No appointments~%")
      (dolist (appt appts)
        (print-appointment appt)))
    (format t "~%~%~%~A~%" *disclaimer*)))

(defun print-appointment-html (appt)
  (let* ((name    (zimbra-appt-name appt))
         (start   (parse-integer (zimbra-inst-s (zimbra-appt-inst appt))))
         (end     (+ start (parse-integer (zimbra-appt-dur appt))))
         (loc     (zimbra-appt-loc appt))
         (all-day (equal "1" (zimbra-appt-all-day appt))))
    (format t "<tr><td align=\"right\" valign=\"top\">")
    (when (not all-day)
      (format t "~A" (zimbra-print-date-to-string start)))
    (format t "</td><td align=\"left\" valign=\"top\"> - ")
    (when (not all-day)
      (format t "~A " (zimbra-print-date-to-string end)))
    (format t "~A" name)
    (when (and loc (plusp (length loc)))
      (format t " (~A)" loc))
    (format t "</td></tr>~%")))

(defun print-agenda-html ()
  (let* ((start (zimbra-time-from-universal-time (midnight)))
         (end   (1- (+ start (* 1000 60 60 24))))
         (appts (zimbra-agenda start end)))
    (format t "<table border=\"0\">~%")
    (format t "<tr><th colspan=\"2\" align=\"left\">~A</td></tr>~%"
            (date "+%a %b %d, %Y"))
    (if (null appts)
        (format t "<tr><td colspan=\"2\"><i>No appointments</i></td></tr>~%")
      (dolist (appt appts)
        (print-appointment-html appt)))
    (format t "</table>~%")
    (format t "<p style=\"font-size: small\">--<br/>~A</p>~%" *disclaimer*)))

(defun mktemp ()
  (read-line (popen (list "mktemp") "r")))

(defun zimbra-mail-agenda (account)
  (with-open-file (temp (mktemp) :direction :output)
    (let ((*standard-output* temp))
      (format t "Subject: Daily Agenda for ~A~%To: ~A~%~%" account account)
      (print-agenda)
      (format t ".~%")
      (finish-output temp))
    (ext:system (format nil "sendmail ~A < ~A" account (truename temp)))
    (close temp)
    (delete-file temp)))

(defun zimbra-mail-agenda-html (account cn)
  (with-open-file (temp (mktemp) :direction :output)
    (let ((*standard-output* temp))
      (format t "Subject: Daily Agenda for ~A~%" (or cn account))
      (format t "From: ~A~%" account)
      (format t "To: ~A~%" account)
      (format t "MIME-Version: 1.0~%")
      (format t "Content-Type: multipart/alternative; 
	boundary=\"----=_Part_823717_620177132.1322847972306\"~%~%")
      (format t "------=_Part_823717_620177132.1322847972306
Content-Type: text/plain; charset=utf-8~%~%")
      (print-agenda)
      (format t "------=_Part_823717_620177132.1322847972306
Content-Type: text/html; charset=utf-8~%~%")
      (print-agenda-html)
      (format t "------=_Part_823717_620177132.1322847972306--~%")
      (format t ".~%")
      (finish-output temp))
    (ext:system (format nil "sendmail ~A < ~A" account (truename temp)))
    (close temp)
    (delete-file temp)))

(zimbra-admin-auth)

(dolist (account (zimbra-admin-zimlet-enabled "ca_uoguelph_ccs_appointmentsummary"))
  (let ((name (zimbra-account-name account))
        (cn   (zimbra-account-cn   account)))
    (zimbra-admin-delegate-auth name)
    (zimbra-mail-agenda-html name cn)))

(quit)
