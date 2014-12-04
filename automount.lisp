;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Zimbra Share Automounter
;;; Copyright (c) 2010 M. Brent Harp

(eval-when (:compile-toplevel) (load "config"))

(require "zimbra")

(defvar *automount-conf*
  #.(translate-logical-pathname "config:etc;automount.conf")
  "Automount configuration file pathname.")

(defvar *automount-allow*
  #.(translate-logical-pathname "config:etc;automount.allow")
  "Automount enabled accounts file pathname.")

;; If a local config file exists and is readable,
;; load it now.
(when (probe-file *automount-conf*)
  (load *automount-conf*))

(zimbra-admin-auth)

(with-open-file 
 (*standard-input* *automount-allow*)
 (do ((account (read-line nil nil) (read-line nil nil)))
     ((null account))
     (zimbra-admin-delegate-auth account)
     (zimbra-mount-shares)))

(quit)

