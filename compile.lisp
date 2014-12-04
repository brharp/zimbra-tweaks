
(load "config.lisp")

(defun my-compile-file (file)
  (apply #'compile-file (cons file *compiler-options*)))

(compile-file "zimlet.lisp" :system-p t)
(c:build-program "zimlet" :lisp-files '("zimlet.o") :epilogue-code "")

(compile-file "agenda.lisp" :system-p t)
(c:build-program "agenda" :lisp-files '("agenda.o") :epilogue-code "")

(quit)

