;;;; Enchant
;;
;; Enchant library bindings for Common Lisp
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
  (:export #:get-version #:enchant-error
           
           #:broker #:brokerp #:broker-init #:not-active-broker
           #:broker-free #:with-broker

           #:dict #:dictp #:not-active-dict #:dict-not-found
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
  (:documentation "Return a boolean whether OBJECT is active."))

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
  (cffi:foreign-funcall "enchant_get_version" :string))

;;; Brokers

(defclass broker (foreign-object) nil)

(defun brokerp (object)
  (typep object 'broker))

(define-condition not-active-broker (enchant-error) nil)

(defun broker-init ()
  (let ((broker (cffi:foreign-funcall "enchant_broker_init" :pointer)))
    (when (proper-pointer-p broker)
      (make-instance 'broker :address broker))))

(defmethod free-foreign-resource ((object broker))
  (when (activep object)
    (cffi:foreign-funcall "enchant_broker_free" :pointer (address object)
                          :void)))

(defun broker-free (broker)
  (free-foreign-resource broker))

(defun error-if-not-active-broker (object)
  (unless (and (brokerp object) (activep object))
    (error 'not-active-broker :string "Not an active BROKER object.")))

(defun broker-dict-exists-p (broker language)
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
  (let ((broker (gensym "BROKER")))
    `(let* ((,broker (broker-init))
            (,variable ,broker))
       (declare (ignorable ,variable))
       (unwind-protect (progn ,@body)
         (broker-free ,broker)))))

;;; Dicts

(defclass dict (foreign-object) nil)

(defun dictp (object)
  (typep object 'dict))

(define-condition not-active-dict (enchant-error) nil)
(define-condition dict-not-found (enchant-error) nil)

(defun error-if-not-active-dict (object)
  (unless (and (dictp object) (activep object))
    (error 'not-active-dict :string "Not an active DICT object.")))

(defun dict-get-error (dict)
  (error-if-not-active-dict dict)
  (cffi:foreign-funcall "enchant_dict_get_error"
                        :pointer (address dict)
                        :string))

(defun broker-request-dict (broker language)
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
  (when (and (brokerp broker) (activep broker) 
             (dictp dict) (activep dict))
    (cffi:foreign-funcall "enchant_broker_free_dict"
                          :pointer (address broker)
                          :pointer (address dict)
                          :void)
    (free-foreign-resource dict)))

(defun dict-check (dict word)
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
  (let ((brokersym (gensym "BROKER"))
        (dictsym (gensym "DICT")))
    `(let ((,brokersym ,broker))
       (if ,brokersym
           (let* ((,dictsym (broker-request-dict ,brokersym ,language))
                  (,variable ,dictsym))
             (declare (ignorable ,variable))
             (unwind-protect (progn ,@body)
               (broker-free-dict ,brokersym ,dictsym)))
           (with-broker ,brokersym
             (let* ((,dictsym (broker-request-dict ,brokersym ,language))
                    (,variable ,dictsym))
               (declare (ignorable ,variable))
               (unwind-protect (progn ,@body)
                 (broker-free-dict ,brokersym ,dictsym))))))))
