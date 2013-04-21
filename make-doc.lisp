(require :sb-posix)
(require :sb-introspect)

(flet ((probe-load (path &optional (default (user-homedir-pathname)))
         (let ((path (merge-pathnames path default)))
           (when (probe-file path) (load path))))
       (funcallstr (string &rest args)
         (apply (read-from-string string) args)))
  (or (probe-load #p"quicklisp/setup.lisp")
      (probe-load #p".quicklisp/setup.lisp")
      (let ((url "http://beta.quicklisp.org/quicklisp.lisp")
            (init (nth-value 1 (funcallstr "sb-posix:mkstemp"
                                           "/tmp/quicklisp-XXXXXX"))))
        (unwind-protect
             (progn
               (sb-ext:run-program "wget" (list "-O" init "--" url)
                                   :search t :output t)
               (when (probe-load init)
                 (funcallstr "quicklisp-quickstart:install")))
          (delete-file init)))))

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (loop :for symbol :in (sort (loop :for symbol
                                    :being :each :external-symbol :in package
                                    :collect symbol)
                              #'string< :key #'symbol-name)
        :for name := (symbol-name symbol)
        :do (loop :for type :in '(function variable type)
                  :for doc := (documentation symbol type)
                  :if doc :do
                  (format stream "~A" prefix)
                  (case type
                    (function
                     (format stream "~A: `(~(~A~)"
                             (if (macro-function symbol) "Macro" "Function")
                             name)
                     (let ((ll (sb-introspect:function-lambda-list symbol)))
                       (when ll (format stream " ~{~(~A~)~^ ~}" ll)))
                     (format stream ")`"))
                    (variable (format stream "Variable: `~(~A~)`" name))
                    (type (if (ignore-errors (find-class symbol))
                              (format stream "Class: `~(~A~)`" name)
                              (format stream "Type: `~(~A~)`" name))))
                  (format stream "~%~%~A~%~%~%" doc))))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "enchant"))

(print-doc (nth 1 sb-ext:*posix-argv*))
