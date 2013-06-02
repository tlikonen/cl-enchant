;;;; Enchant -- Enchant library bindings for Common Lisp
;;
;; Author: Teemu Likonen <tlikonen@iki.fi>
;;
;; License: Public domain
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:enchant
  (:use #:cl)
  (:export #:get-version #:enchant-error #:activep

           #:broker #:broker-init #:not-active-broker
           #:broker-free #:with-broker

           #:dict #:not-active-dict #:dict-not-found
           #:broker-request-dict #:broker-free-dict #:dict-check
           #:broker-dict-exists-p #:with-dict #:dict-suggest))

(in-package #:enchant)

(eval-when (:load-toplevel :execute)
  (with-simple-restart (skip-enchant "Skip loading the Enchant library.")
    (cffi:load-foreign-library '(:default "libenchant"))))

;;; General

(defun error-if-not-proper-string (object)
  (assert (and (stringp object)
               (plusp (length object)))
          nil "The argument must be a non-empty string."))

(define-condition enchant-error (error)
  ((error-string :initarg :string))
  (:report (lambda (condition stream)
             (format stream "~A" (slot-value condition 'error-string)))))

(defclass foreign-object ()
  ((address :initarg :address :accessor address)))

(defun proper-pointer-p (object)
  (and (cffi:pointerp object)
       (not (cffi:null-pointer-p object))))

(defgeneric activep (object)
  (:documentation "Test if _object_ is active. Return a generalized
boolean."))

(defmethod activep ((object foreign-object))
  (proper-pointer-p (address object)))

(defmethod print-object ((object foreign-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~A)" (if (activep object) "ACTIVE" "INACTIVE"))))

(defgeneric free-foreign-resource (object))

(defmethod free-foreign-resource :around ((object foreign-object))
  (when (activep object)
    (call-next-method)
    (setf (address object) nil)
    object))

(defun get-version ()
  "Return the Enchant library version."
  (cffi:foreign-funcall "enchant_get_version" :string))

;;; Brokers

(defclass broker (foreign-object)
  nil
  (:documentation
   "Class for holding pointers to foreign (non-Lisp) broker resources.
Instances are created with `broker-init` function."))

(define-condition not-active-broker (enchant-error) nil)

(defun broker-init ()
  "Initialize a new broker. Return a `broker` object which can be used
to request dictionares etc. See function `broker-request-dict`.

A `broker` object is \"active\" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free`. After being freed it becomes \"inactive\" and
thus unusable. Generic function `activep` can be used to test if a
`broker` object is active or not.

See macros `with-broker` and `with-dict` which automatically initialize
and free broker and dictionary resources."

  (let ((broker (cffi:foreign-funcall "enchant_broker_init" :pointer)))
    (when (proper-pointer-p broker)
      (make-instance 'broker :address broker))))

(defmethod free-foreign-resource ((object broker))
  (when (activep object)
    (cffi:foreign-funcall "enchant_broker_free" :pointer (address object)
                          :void)))

(defun broker-free (broker)
  "Free the foreign (non-Lisp) `broker` resources. The argument is a
`broker` object returned by `broker-init`. The `broker` object becomes
\"inactive\" and can't be used anymore."
    (free-foreign-resource broker))

(defun error-if-not-active-broker (object)
  (unless (and (typep object 'broker)
               (activep object))
    (error 'not-active-broker :string "Not an active BROKER object.")))

(defun broker-dict-exists-p (broker language)
  "Check if _language_ exists. _Broker_ must be a valid `broker` object
returned by `broker-init`. _Language_ is a language code and optional
country code as a string (e.g., \"fi\", \"en_GB\").

If the _language_ exists return the _language_ string. Otherwise return
`nil`.

If _broker_ is not an active `broker` object signal `not-active-broker`
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
  "Initialize a new `broker` (using `broker-init`) and bind _variable_
to the `broker` object. Execute all _body_ forms and return the values
of the last _body_ form. Finally, free the `broker` resources with
function `broker-free`."

  (let ((broker (gensym "BROKER")))
    `(let* ((,broker (broker-init))
            (,variable ,broker))
       (declare (ignorable ,variable))
       (unwind-protect (progn ,@body)
         (broker-free ,broker)))))

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
  "Return error string (or nil) describing the last exception."
  (error-if-not-active-dict dict)
  (cffi:foreign-funcall "enchant_dict_get_error"
                        :pointer (address dict)
                        :string))

(defun broker-request-dict (broker language)
  "Request a new dictionary for _language_. Return a `dict` object which
can be used with spell-checker operations.

The _broker_ argument must be an active `broker` object created with
`broker-init`. _Language_ is a language code and optional country code
as a string (e.g., \"fi\", \"en_GB\").

A `dict` object is \"active\" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free-dict`. After being freed it becomes \"inactive\"
and thus unusable. Generic function `activep` can be used to test if
`dict` object is active or not.

If no suitable dictionary could be found `dict-not-found` error
condition is signalled.

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

(defmethod free-foreign-resource ((object dict))
  nil)

(defun broker-free-dict (broker dict)
  "Free the foreign (non-Lisp) `dict` resources. The first argument is a
`broker` object returned by `broker-init` and the second a `dict` object
returned by `broker-request-dict`. The `dict` object becomes
\"inactive\" and can't be used anymore."

  (when (and (typep broker 'broker)
             (activep broker)
             (typep dict 'dict)
             (activep dict))
    (cffi:foreign-funcall "enchant_broker_free_dict"
                          :pointer (address broker)
                          :pointer (address dict)
                          :void)
    (free-foreign-resource dict)))

(defun dict-check (dict word)
  "Check the spelling of _word_ (string) using dictionary _dict_.
Return _word_ if the spelling is correct, `nil` otherwise.

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
          ((minusp value) (error 'enchant-error
                                 :string (format nil "Error: ~A"
                                                 (dict-get-error dict)))))))

(defun dict-suggest (dict word)
  "Request spelling suggestions for _word_ (string) using dictionary _dict_.
Return a list of suggestions (strings) or nil if there aren't any.

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
  "Request a new dictionary object for _language_. Bind _variable_ to
the new `dict` object and execute all _body_ forms. Return the values of
the last _body_ form. Finally, free the `dict` resources with function
`broker-free-dict`.

If the optional _broker_ argument is given reuse that broker object when
requesting DICT. If the _broker_ argument is not given create implicitly
a new `broker` object with `broker-init` and free it in the end with
`broker-free`. Note that the decision about the _broker_ argument is
done at the macro-expansion time. If there is anything (except the
symbol `nil`) in the place of the _broker_ argument that will be used as
the broker.

Examples:

    ENCHANT> (with-dict (lang \"fi\")
               (dict-check lang \"toimii\"))
    \"toimii\"

    ENCHANT> (with-broker b
               (with-dict (lang \"fi\" b)
                 (dict-suggest lang \"tomii\")))
    (\"omii\" \"Tomi\" \"toimi\" \"toimii\" \"Tomisi\")"

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
