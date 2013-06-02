#-sbcl
(error "This is only for SBCL.")

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

(defparameter *head*
  "~
CL Enchant
==========

**Common Lisp interface for Enchant spell-checker library**


Introduction
------------

_CL Enchant_ is a Common Lisp interface for the [Enchant][]
spell-checker library. Enchant is a generic spell-checker library which
uses other spell-checkers transparently as back-end. Enchant supports
the following checkers:

  - Aspell/Pspell
  - Ispell
  - MySpell/Hunspell
  - Uspell (Yiddish, Hebrew and Eastern European languages)
  - Hspell (Hebrew)
  - Zemberek (Turkish)
  - Voikko (Finnish)
  - AppleSpell (Mac OSX)

_CL Enchant_ aim's to provide all or most of the Enchant's features. It
uses [The Common Foreign Function Interface][CFFI] (CFFI) for accessing
the Enchant C library. It should work on any Common Lisp implementation
which supports CFFI.

[Enchant]: http://www.abisource.com/projects/enchant/
[CFFI]:    http://common-lisp.net/project/cffi/


Examples
--------

### Function: `(dict-check dict word)`

Check the spelling for _word_ using dictionary _dict_.

    ENCHANT> (with-dict (lang \"en_GB\")
               (dict-check lang \"working\")) ; correct
    \"working\"

    ENCHANT> (with-dict (lang \"en_GB\")
               (dict-check lang \"wrking\"))  ; incorrect
    NIL


### Function: `(dict-suggest dict word)`

Get spelling suggestions for _word_ using dictionary _dict_.

    ENCHANT> (with-dict (lang \"en_US\")
               (dict-suggest lang \"wrking\"))
    (\"wring\" \"working\" \"irking\" \"waking\" \"wrying\" \"parking\" \"marking\" \"winking\"
     \"wicking\" \"Zworykin\" \"dragging\")


Interface (API)
---------------

")

(defparameter *tail*
  "~
Missing features
----------------

  - `enchant_dict_describe()`
  - `enchant_broker_request_pwl_dict()`
  - `enchant_broker_describe()`
  - `enchant_broker_list_dicts()`
  - `enchant_broker_set_ordering()`


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
")

(defun print-doc (package &key (stream *standard-output*) (prefix "### "))
  (format stream *head*)
  (loop :with *package* := (find-package package)
        :with *print-right-margin* := 72
        :with *print-case* := :downcase
        :with symbols := (sort (loop :for symbol
                                     :being :each :external-symbol :in package
                                     :collect symbol)
                               #'string-lessp :key #'symbol-name)

        :for (symbol type doc) :in (mapcan #'symbol-doc-type symbols)
        :if doc :do
        (format stream "~A" prefix)
        (case type
          (:function
           (format stream "Function: `~A`" symbol)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~S" ll))))
          (:macro
           (format stream "Macro: `~A`" symbol)
           (let ((ll (sb-introspect:function-lambda-list symbol)))
             (when ll
               (format stream "~%~%The lambda list:~%~%     ~S" ll))))
          (:variable (format stream "Variable: `~S`" symbol))
          (:condition (format stream "Condition: `~S`" symbol))
          (:class (format stream "Class: `~S`" symbol)))
        (format stream "~%~%~A~%~%~%" doc))
  (format stream *tail*))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "enchant"))

(handler-case (print-doc "ENCHANT")
  (error (c)
    (format *error-output* "~A~%" c)))
