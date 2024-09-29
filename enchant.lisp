;;;; Enchant -- Programming interface for Enchant spell-checker library
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Creative Commons CC0 (public domain dedication)
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

(defpackage #:enchant
  (:use #:cl)
  (:export #:get-version #:enchant-error #:error-string #:activep

           #:broker #:broker-init #:not-active-broker #:broker-get-error
           #:broker-free #:with-broker #:broker-describe #:broker-list-dicts
           #:broker-set-ordering

           #:dict #:not-active-dict #:dict-not-found #:dict-get-error
           #:broker-request-dict #:broker-request-pwl-dict
           #:broker-free-dict #:dict-check #:broker-dict-exists-p #:with-dict
           #:with-pwl-dict #:dict-suggest

           #:dict-add #:dict-add-to-session #:dict-is-added-p
           #:dict-remove #:dict-remove-from-session #:dict-is-removed-p
           #:dict-store-replacement #:dict-describe))

(in-package #:enchant)

(eval-when (:load-toplevel :execute)
  (with-simple-restart (skip-load-enchant "Skip loading Enchant C library.")
    (cffi:load-foreign-library '(:or
                                 (:default "libenchant-2")
                                 (:default "libenchant")))))

;;; General

(defun error-if-not-proper-string (object)
  (assert (and (stringp object)
               (plusp (length object)))
          nil "The argument must be a non-empty string."))

(define-condition enchant-error (error)
  ((error-string :initarg :string :reader error-string))
  (:report (lambda (condition stream)
             (format stream "~A" (error-string condition)))))

(defclass foreign-object ()
  ((address :initarg :address :accessor address)))

(defun proper-pointer-p (object)
  (and (cffi:pointerp object)
       (not (cffi:null-pointer-p object))))

(defgeneric activep (object)
  (:documentation "Test if _object_ is active. Return a generalized
boolean. This can be used with `broker` and `dict` objects."))

(defmethod activep ((object foreign-object))
  (proper-pointer-p (address object)))

(defmethod print-object ((object foreign-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~A)" (if (activep object) "ACTIVE" "INACTIVE"))))

(defgeneric free-foreign-resource (object &key &allow-other-keys))

(defmethod free-foreign-resource :around ((object foreign-object) &key)
  (when (activep object)
    (call-next-method)
    (setf (address object) nil)
    object))

(defun get-version ()
  "Return the Enchant library version."
  (cffi:foreign-funcall "enchant_get_version" :string))

(defvar *callback-data*)

(cffi:defcallback broker-describe-fn :void ((name :string)
                                            (desc :string)
                                            (file :string))
  (push (list name desc file) *callback-data*))

(cffi:defcallback dict-describe-fn :void ((lang :string)
                                          (name :string)
                                          (desc :string)
                                          (file :string))
  (push (list lang name desc file) *callback-data*))

;;; Brokers

(defclass broker (foreign-object)
  nil
  (:documentation
   "Class for holding pointers to foreign (non-Lisp) broker resources.
Instances are created with `broker-init` function."))

(define-condition not-active-broker (enchant-error) nil)

(defun broker-init ()
  "Initialize a new broker. Return a `broker` object which can be used
to request dictionaries etc. See function `broker-request-dict`.

A `broker` object is active when it has been successfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free`. After being freed it becomes inactive and thus
unusable. Generic function `activep` can be used to test if a `broker`
object is active or not.

See macros `with-broker`, `with-dict` and `with-pwl-dict` which
automatically initialize and free broker and dictionary resources."

  (let ((broker (cffi:foreign-funcall "enchant_broker_init" :pointer)))
    (when (proper-pointer-p broker)
      (make-instance 'broker :address broker))))

(defmethod free-foreign-resource ((broker broker) &key)
  (cffi:foreign-funcall "enchant_broker_free"
                        :pointer (address broker)
                        :void))

(defun broker-free (broker)
  "Free the foreign (non-Lisp) `broker` resources. The argument is a
`broker` object returned by `broker-init`. The `broker` object becomes
inactive and can't be used anymore."
  (free-foreign-resource broker))

(defun error-if-not-active-broker (object)
  (unless (and (typep object 'broker)
               (activep object))
    (error 'not-active-broker :string "Not an active BROKER object.")))

(defun broker-dict-exists-p (broker language)
  "Check if dictionary for _language_ exists. _Broker_ must be a valid
`broker` object returned by `broker-init`. _Language_ is a language code
and optional country code as a string (e.g., `\"en\"`, `\"en_GB\"`). If
the _language_ exists return the _language_ string. Otherwise return
`nil`.

If _broker_ is not an active `broker` object, signal `not-active-broker`
error condition."

  (error-if-not-active-broker broker)
  (error-if-not-proper-string language)
  (case (cffi:foreign-funcall "enchant_broker_dict_exists"
                              :pointer (address broker)
                              :string language
                              :int)
    (0 nil)
    (1 language)))

(defmacro with-broker (variable &body body)
  "Initialize a new `broker` (using `broker-init`) and lexically bind
_variable_ to the `broker` object. Execute all _body_ forms and return
the values of the last _body_ form. Finally, free the `broker` resources
with function `broker-free`."

  (let ((broker (gensym "BROKER")))
    `(let* ((,broker (broker-init))
            (,variable ,broker))
       (declare (ignorable ,variable))
       (unwind-protect (progn ,@body)
         (broker-free ,broker)))))

(defun broker-describe (broker)
  "Get information about Enchant dictionary providers. Return a list of
lists of three strings: provider name, provider description, provider
library filename.

If _broker_ is not an active `broker` object, signal `not-active-broker`
error condition."

  (error-if-not-active-broker broker)
  (let (*callback-data*)
    (cffi:foreign-funcall "enchant_broker_describe"
                          :pointer (address broker)
                          :pointer (cffi:callback broker-describe-fn)
                          :pointer (cffi:null-pointer)
                          :void)
    (nreverse *callback-data*)))

(defun broker-list-dicts (broker)
  "List all dictionaries that are available. Return a list of lists with
four strings: language and optional country code (e.g., `\"en\"` or
`\"en_GB\"`), provider name, provider description and provider library
filename.

If _broker_ is not an active `broker` object, signal `not-active-broker`
error condition."

  (error-if-not-active-broker broker)
  (let (*callback-data*)
    (cffi:foreign-funcall "enchant_broker_list_dicts"
                          :pointer (address broker)
                          :pointer (cffi:callback dict-describe-fn)
                          :pointer (cffi:null-pointer)
                          :void)
    (nreverse *callback-data*)))

(defun broker-get-error (broker)
  "Return an error message string (or `nil`) describing the last error
in the UTF-8 encoding. This can be called after `broker` operations."
  (error-if-not-active-broker broker)
  (cffi:foreign-funcall "enchant_broker_get_error"
                        :pointer (address broker)
                        :string))

(defun broker-set-ordering (broker language ordering)
  "Declares a preference of providers to use for _language_.
The _language_ argument is a language code and optional country
code (e.g., `\"en\"` or `\"en_GB\"`). Pseudo language `\"*\"` can be
used to declare a default ordering. It is used by any language that does
not explicitly declare an ordering. The _ordering_ argument is a list of
provider name strings (e.g., `(\"myspell\" \"aspell\" \"ispell\")`).

If _broker_ is not an active `broker` object, signal `not-active-broker`
error condition."
  (error-if-not-active-broker broker)
  (error-if-not-proper-string language)
  (assert (and (consp ordering)
               (every #'stringp ordering))
          nil "The ORDERING argument must be a list of strings.")
  (cffi:foreign-funcall "enchant_broker_set_ordering"
                        :pointer (address broker)
                        :string language
                        :string (format nil "~{~A~^,~}" ordering)
                        :void))

;;; Dicts

(defclass dict (foreign-object)
  nil
  (:documentation
   "Class for holding pointers to foreign (non-Lisp) dictionary
resources. Instances are created with `broker-request-dict` function."))

(define-condition not-active-dict (enchant-error) nil)
(define-condition dict-not-found (enchant-error) nil)

(defun error-if-not-active-dict (object)
  (unless (and (typep object 'dict) (activep object))
    (error 'not-active-dict :string "Not an active DICT object.")))

(defun dict-get-error (dict)
  "Return an error message string (or `nil`) describing the last error
in the UTF-8 encoding. This can be called after `dict` operations."
  (error-if-not-active-dict dict)
  (cffi:foreign-funcall "enchant_dict_get_error"
                        :pointer (address dict)
                        :string))

(defun broker-request-dict (broker language)
  "Request a new dictionary for _language_. Return a `dict` object which
can be used with spell-checker operations etc.

The _broker_ argument must be an active `broker` object created with
`broker-init`. _Language_ is a language code and optional country code
as a string (e.g., `\"en\"` or `\"en_GB\"`).

A `dict` object is active when it has been successfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free-dict`. After being freed it becomes inactive and
thus unusable. Generic function `activep` can be used to test if `dict`
object is active or not.

If no suitable dictionary could be found `dict-not-found` error
condition is signaled.

See also `with-dict` macro which automatically creates a `dict`
environment and frees it in the end."

  (error-if-not-active-broker broker)
  (error-if-not-proper-string language)
  (let ((ptr (cffi:foreign-funcall "enchant_broker_request_dict"
                                   :pointer (address broker)
                                   :string language
                                   :pointer)))
    (if (proper-pointer-p ptr)
        (make-instance 'dict :address ptr)
        (error 'dict-not-found :string (format nil "Dictionary \"~A\" not found."
                                               language)))))


(defun broker-request-pwl-dict (broker pwl)
  "Request a new dictionary for personal word list file _pwl_ (a filename string).
Return a `dict` object which can be used with spell-checker operations.

The _broker_ argument must be an active `broker` object created with
`broker-init`. Personal word list file _pwl_ is a text file with one
entry (a word) per line. If the file does not exist it is created. New
words can be added to the personal word list file with function
`dict-add`.

A `dict` object is active when it has been successfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free-dict`. After being freed it becomes inactive and
thus unusable. Generic function `activep` can be used to test if `dict`
object is active or not.

See also `with-pwl-dict` macro which automatically creates a `dict`
environment and frees it in the end."

  (error-if-not-active-broker broker)
  (error-if-not-proper-string pwl)
  (let ((ptr (cffi:foreign-funcall "enchant_broker_request_pwl_dict"
                                   :pointer (address broker)
                                   :string pwl
                                   :pointer)))
    (when (proper-pointer-p ptr)
      (make-instance 'dict :address ptr))))


(defmethod free-foreign-resource ((dict dict) &key broker)
  (cffi:foreign-funcall "enchant_broker_free_dict"
                        :pointer (address broker)
                        :pointer (address dict)
                        :void))

(defun broker-free-dict (broker dict)
  "Free the foreign (non-Lisp) `dict` resources. The first argument is a
`broker` object returned by `broker-init` and the second a `dict` object
returned by `broker-request-dict`. The `dict` object becomes inactive
and can't be used anymore."
  (error-if-not-active-broker broker)
  (free-foreign-resource dict :broker broker))

(defun dict-check (dict word)
  "Check the spelling of _word_ (string) using dictionary _dict_.
Return the _word_ if the spelling is correct, `nil` otherwise.

_Dict_ must be an active `dict` object returned by
`broker-request-dict`, if not, signal a `not-active-dict` condition."

  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (let ((value (cffi:foreign-funcall "enchant_dict_check"
                                     :pointer (address dict)
                                     :string word
                                     :int -1 ;use strlen()
                                     :int)))
    (cond ((zerop value) word)
          ((plusp value) nil)
          ((minusp value)
           (error 'enchant-error
                  :string (format nil "Error code: ~A" value))))))

(defun dict-suggest (dict word)
  "Request spelling suggestions for _word_ (string) using dictionary _dict_.
Return a list of suggestions (strings) or `nil` if there aren't any.

_Dict_ must be an active `dict` object returned by
`broker-request-dict`, if not, signal `not-active-dict` condition."

  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (cffi:with-foreign-object (len :int)
    (let ((suggestions (cffi:foreign-funcall "enchant_dict_suggest"
                                             :pointer (address dict)
                                             :string word
                                             :int -1
                                             :pointer len
                                             :pointer)))
      (when (proper-pointer-p suggestions)
        (unwind-protect (loop :for i :upfrom 0 :below (cffi:mem-aref len :int)
                              :collect (cffi:mem-aref suggestions :string i))
          (cffi:foreign-funcall "enchant_dict_free_string_list"
                                :pointer (address dict)
                                :pointer suggestions
                                :void))))))

(defmacro with-dict ((variable language &optional broker) &body body)
  "Request a new `dict` object for _language_. Lexically bind _variable_
to the new `dict` object and execute all _body_ forms. Return the values
of the last _body_ form. Finally, free the `dict` resources with
function `broker-free-dict`.

If the optional _broker_ argument is given reuse that broker object when
requesting `dict`. If the _broker_ argument is not given create
implicitly a new `broker` object with `broker-init` and free it in the
end with `broker-free`. Note that the decision about the _broker_
argument is done at the macro-expansion time. If there is
anything (except the symbol `nil`) in the place of the _broker_ argument
that will be used as the broker.

Examples:

    ENCHANT> (with-dict (lang \"en_GB\")
               (dict-check lang \"working\"))
    \"working\"

    ENCHANT> (with-dict (lang \"en_GB\")
               (dict-suggest lang \"wrking\"))
    (\"wring\" \"working\" \"irking\" \"waking\" \"wrecking\" \"winking\"
     \"wrinkling\" \"marking\" \"Wrekin\" \"raking\")"

  (let* ((brokersym (gensym "BROKER"))
         (dictsym (gensym "DICT"))
         (code `(let* ((,dictsym (broker-request-dict ,brokersym ,language))
                       (,variable ,dictsym))
                  (declare (ignorable ,variable))
                  (unwind-protect (progn ,@body)
                    (broker-free-dict ,brokersym ,dictsym)))))
    (if broker
        `(let ((,brokersym ,broker)) ,code)
        `(with-broker ,brokersym ,code))))

(defmacro with-pwl-dict ((variable pwl &optional broker) &body body)
  "Request a new `dict` object for personal word list file _pwl_.
Lexically bind _variable_ to the new `dict` object and execute all
_body_ forms. Return the values of the last _body_ form. Finally, free
the `dict` resources with function `broker-free-dict`.

For more information on personal word list files see the documentation
of function `broker-request-pwl-dict`.

If the optional _broker_ argument is given reuse that broker object when
requesting `dict`. If the _broker_ argument is not given create
implicitly a new `broker` object with `broker-init` and free it in the
end with `broker-free`. Note that the decision about the _broker_
argument is done at the macro-expansion time. If there is
anything (except the symbol `nil`) in the place of the _broker_ argument
that will be used as the broker."

  (let* ((brokersym (gensym "BROKER"))
         (dictsym (gensym "DICT"))
         (code `(let* ((,dictsym (broker-request-pwl-dict ,brokersym ,pwl))
                       (,variable ,dictsym))
                  (declare (ignorable ,variable))
                  (unwind-protect (progn ,@body)
                    (broker-free-dict ,brokersym ,dictsym)))))
    (if broker
        `(let ((,brokersym ,broker)) ,code)
        `(with-broker ,brokersym ,code))))


(defun dict-add (dict word)
  "Add _word_ to user's personal dictionary _dict_. If the _word_ exists
in user's exclude dictionary also remove it from there."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (cffi:foreign-funcall "enchant_dict_add"
                        :pointer (address dict)
                        :string word
                        :int -1
                        :void))

(defun dict-add-to-session (dict word)
  "Add _word_ to the current spell-checking session _dict_. _Word_ is
then recognized as a correct word in the current session."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (cffi:foreign-funcall "enchant_dict_add_to_session"
                        :pointer (address dict)
                        :string word
                        :int -1
                        :void))

(defun dict-is-added-p (dict word)
  "Check if _word_ has been added to user's personal dictionary or to
the current spell-checking session _dict_. If true, return _word_.
Otherwise return `nil`.

Functions for adding words are `dict-add` and `dict-add-to-session`."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (case (cffi:foreign-funcall "enchant_dict_is_added"
                              :pointer (address dict)
                              :string word
                              :int -1
                              :int)
    (1 word)
    (0 nil)))

(defun dict-remove (dict word)
  "Add _word_ to user's exclude dictionary for _dict_. If the _word_
exists in user's personal dictionary also remove it from there."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (cffi:foreign-funcall "enchant_dict_remove"
                        :pointer (address dict)
                        :string word
                        :int -1
                        :void))

(defun dict-remove-from-session (dict word)
  "Remove _word_ from the current spell-checking session _dict_.
The _word_ is not recognized anymore in the current session."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (cffi:foreign-funcall "enchant_dict_remove_from_session"
                        :pointer (address dict)
                        :string word
                        :int -1
                        :void))

(defun dict-is-removed-p (dict word)
  "Check if _word_ has been removed from user's personal dictionary or
from the current spell-checking session _dict_. If true, return _word_.
Otherwise return `nil`."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (case (cffi:foreign-funcall "enchant_dict_is_removed"
                              :pointer (address dict)
                              :string word
                              :int -1
                              :int)
    (1 word)
    (0 nil)))

(defun dict-store-replacement (dict word correction)
  "Add a correction statement from misspelled _word_ to _correction_
using dictionary _dict_. _Correction_ might show up in the suggestion
list."
  (error-if-not-active-dict dict)
  (error-if-not-proper-string word)
  (error-if-not-proper-string correction)
  (cffi:foreign-funcall "enchant_dict_store_replacement"
                        :pointer (address dict)
                        :string word :int -1
                        :string correction :int -1
                        :void))

(defun dict-describe (dict)
  "Describe dictionary _dict_. Return a list of four strings: language
code and optional country code (e.g., `\"en\"` or `\"en_GB\"`), provider
name, provider description and provider library filename.

_Dict_ must be an active `dict` object returned by
`broker-request-dict`, if not, signal a `not-active-dict` condition."

  (error-if-not-active-dict dict)
  (let (*callback-data*)
    (cffi:foreign-funcall "enchant_dict_describe"
                          :pointer (address dict)
                          :pointer (cffi:callback dict-describe-fn)
                          :pointer (cffi:null-pointer)
                          :void)
    (first *callback-data*)))
