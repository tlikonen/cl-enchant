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

(defun symbol-doc-type (symbol)
  (let (docs)
    (flet ((doc (symbol type key)
             (push (list symbol key (documentation symbol type)) docs)))
      (cond ((ignore-errors (macro-function symbol))
             (doc symbol 'function :macro))
            ((ignore-errors (symbol-function symbol))
             (doc symbol 'function :function)))
      (when (ignore-errors (symbol-value symbol))
        (doc symbol 'variable :variable))
      (cond ((subtypep symbol 'condition)
             (doc symbol 'type :condition))
            ((ignore-errors (find-class symbol))
             (doc symbol 'type :class))))
    docs))

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (loop :with symbols := (sort (loop :for symbol
                                     :being :each :external-symbol :in package
                                     :collect symbol)
                               #'string-lessp :key #'symbol-name)

        :for (symbol type doc) :in (mapcan #'symbol-doc-type symbols)
        :for name := (string-downcase (symbol-name symbol))
        :if doc
        :do
        (format stream "~A" prefix)
        (case type
          (:function
           (format stream "Function: `(~A~A)`" name
                   (let ((ll (sb-introspect:function-lambda-list symbol)))
                     (if ll (format nil " ~{~(~A~)~^ ~}" ll) ""))))
          (:macro
           (format stream "Macro: `(~A~A)`" name
                   (let ((ll (sb-introspect:function-lambda-list symbol)))
                     (if ll (format nil " ~{~(~A~)~^ ~}" ll) ""))))
          (:variable (format stream "Variable: `~A`" name))
          (:condition (format stream "Condition: `~A`" name))
          (:class (format stream "Class: `~A`" name)))
        (format stream "~%~%~A~%~%~%" doc)))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "enchant"))

(print-doc (nth 1 sb-ext:*posix-argv*))
