;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Copyright (c) 2010 M. Brent Harp

(setf (logical-pathname-translations "config")
      '(("etc;*.*.*"     "/etc/*.*")
        ("share;*.*.*"   "/usr/local/share/tweaks/*.*")
        ("**;*.*.*"      "/usr/local/**/*.*")))
