(defsystem :enchant
  :description "Bindings for Enchant spell-checker library"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Public Domain"
  :depends-on (:cffi)
  :components ((:file "enchant")))
