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
  (with-simple-restart (skip-enchant "Skip loading Enchant library.")
    (cffi:load-foreign-library '(:default "libenchant"))))

;;; General

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
  (:documentation "Test if OBJECT is active. Return generalized boolean."))

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
  "Return Enchant library version."
  (cffi:foreign-funcall "enchant_get_version" :string))

;;; Brokers

(defclass broker (foreign-object)
  nil
  (:documentation
   "Class for holding pointers to foreign (non-Lisp) broker resources.
Instances are created with BROKER-INIT function."))

(define-condition not-active-broker (enchant-error) nil)

(defun broker-init ()
  "Initialize a new broker. Return a BROKER object which can be used to
request dictionares etc. See function BROKER-REQUEST-DICT.

BROKER object is \"active\" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function BROKER-FREE. After being freed it becomes \"inactive\" and thus
unusable. Generic function ACTIVEP can be used to test if BROKER object
is active or not.

See also macros WITH-BROKER and WITH-DICT which automatically initialize
and free broker and dictionary resources."

  (let ((broker (cffi:foreign-funcall "enchant_broker_init" :pointer)))
    (when (proper-pointer-p broker)
      (make-instance 'broker :address broker))))

(defmethod free-foreign-resource ((object broker))
  (when (activep object)
    (cffi:foreign-funcall "enchant_broker_free" :pointer (address object)
                          :void)))

(defun broker-free (broker)
  "Free the foreign (non-Lisp) BROKER resources. The argument is a
BROKER object returned by BROKER-INIT. The BROKER object becomes
\"inactive\" and can't be used anymore."
    (free-foreign-resource broker))

(defun error-if-not-active-broker (object)
  (unless (and (typep object 'broker)
               (activep object))
    (error 'not-active-broker :string "Not an active BROKER object.")))

(defun broker-dict-exists-p (broker language)
  "Check if LANGUAGE exists. BROKER must be a valid BROKER object
returned by BROKER-INIT. LANGUAGE is a language code and optional
country code as a string (e.g., \"fi\", \"en_GB\").

If the LANGUAGE exists return the LANGUAGE string. Otherwise return NIL.

If BROKER is not an active BROKER object signal NOT-ACTIVE-BROKER error
condition."

  (error-if-not-active-broker broker)
  (assert (stringp language))
  (let ((value (cffi:foreign-funcall "enchant_broker_dict_exists"
                                     :pointer (address broker)
                                     :string language
                                     :int)))
    (case value
      (0 nil)
      (1 language))))

(defmacro with-broker (variable &body body)
  "Initialize a new broker (using BROKER-INIT) and bind VARIABLE to the
BROKER object. Execute all BODY forms and return the values of the last
BODY form. Finally, free the BROKER resources with function
BROKER-FREE."

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
resources. Instances are created with BROKER-REQUEST-DICT function."))

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
  "Request new dictionary for LANGUAGE. Return DICT object which can be
used with spell-checker operations.

The BROKER argument must be an active BROKER object created with
BROKER-INIT. LANGUAGE is a language code and optional country code as a
string (e.g., \"fi\", \"en_GB\").

DICT object is \"active\" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function BROKER-FREE-DICT. After being freed it becomes \"inactive\" and
thus unusable. Generic function ACTIVEP can be used to test if DICT
object is active or not.

If no suitable dictionary could be found DICT-NOT-FOUND error condition
is signalled.

See also WITH-DICT macro which automatically creates a DICT
environment and frees it in the end."

  (error-if-not-active-broker broker)
  (assert (stringp language))
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
  "Free the foreign (non-Lisp) DICT resources. The first argument is a
BROKER object returned by BROKER-INIT and the second a DICT object
returned by BROKER-REQUEST-DICT. The DICT object becomes \"inactive\"
and can't be used anymore."

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
  "Check the spelling of WORD (string) using dictionary DICT.
Return WORD if the spelling is correct, NIL otherwise.

DICT must be an active DICT object returned by BROKER-REQUEST-DICT. If
not, signal NOT-ACTIVE-DICT condition"

  (error-if-not-active-dict dict)
  (assert (stringp word))
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
  "Request spelling suggestions for WORD (string) using dictionary DICT.
Return a list of suggestions (strings) or nil if there aren't any.

DICT must be an active DICT object returned by BROKER-REQUEST-DICT. If
not, signal NOT-ACTIVE-DICT condition."

  (error-if-not-active-dict dict)
  (assert (stringp word))
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
  "Request a new dictionary object for LANGUAGE. Bind VARIABLE to the
new DICT object and execute all BODY forms. Return the values of the
last BODY form. Finally, free the DICT resources with function
BROKER-FREE-DICT.

If the optional BROKER argument is given reuse that broker object when
requesting DICT. If the BROKER argument is not given create implicitly a
new BROKER object with BROKER-INIT and free it in the end with
BROKER-FREE.

Note that the decision about the broker argument is done at the
macro-expansion time. If there is anything (except the symbol NIL) in
the place of the BROKER argument that will be used as the broker.

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
