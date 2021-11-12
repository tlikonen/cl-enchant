(cl:eval-when (:load-toplevel :execute)
  (cffi:load-foreign-library '(:or (:default "libenchant-2")
                                   (:default "libenchant"))))
