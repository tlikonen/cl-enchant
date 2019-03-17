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
Enchant
=======

**Common Lisp programming interface for Enchant spell-checker library**


Introduction
------------

Enchant is a Common Lisp interface for the [Enchant][] spell-checker
library. The Enchant library is a generic spell-checker library which
uses other spell-checkers transparently as back-end. The library
supports the following checkers:

  - Aspell/Pspell
  - Ispell
  - MySpell/Hunspell
  - Uspell (Yiddish, Hebrew and Eastern European languages)
  - Hspell (Hebrew)
  - Zemberek (Turkish)
  - Voikko (Finnish)
  - AppleSpell (Mac OSX)

This Common Lisp Enchant package uses [The Common Foreign Function
Interface][CFFI] (CFFI) for accessing the Enchant C library. This
package should work on any Common Lisp implementation which supports
CFFI.

[Enchant]: http://www.abisource.com/projects/enchant/
[CFFI]:    http://common-lisp.net/project/cffi/


Installation
------------

[Quicklisp][QL] is the easiest way to install and load Enchant. Two
[ASDF][] systems are provided: The system `enchant` loads the main
Enchant library. There is also `enchant-autoload` system which depends
on the main library and also tries to load the Enchant C library (using
CFFI's facilities).

[QL]:   http://www.quicklisp.org/
[ASDF]: http://common-lisp.net/project/asdf/


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
    (\"wring\" \"working\" \"irking\" \"waking\" \"wrying\" \"parking\"
     \"marking\" \"winking\" \"wicking\" \"Zworykin\" \"dragging\")


License and Source Code
-----------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

License: [Creative Commons CC0][CC0] (public domain dedication)

The source code repository:
<https://github.com/tlikonen/cl-enchant>

[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/legalcode


The Programming Interface
-------------------------

Enchant package uses similar names to the original Enchant C library.
Common Lisp's naming conventions are respected, though. For example, the
original C language function `enchant_dict_check()` has been named
`enchant:dict-check`. The C function `enchant_broker_dict_exists()` has
been named `enchant:broker-dict-exists-p` and thus respecting the Common
Lisp suffix convention for predicate functions.

There are also macros for convenience: `with-broker`, `with-dict` and
`with-pwl-dict`. They hide some low-level resource initialization and
freeing operations.


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
        (format stream "~%~%~A~%~%~%" doc)))


(pushnew (make-pathname :directory (pathname-directory *load-pathname*))
         asdf:*central-registry*)

(with-output-to-string (*standard-output*)
  (ql:quickload "enchant"))

(handler-case (print-doc "ENCHANT")
  (error (c)
    (format *error-output* "~A~%" c)))
