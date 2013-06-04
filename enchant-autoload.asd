(defsystem :enchant-autoload
  :description "Bindings for Enchant spell-checker library (autoload Enchant)"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Public Domain"
  :depends-on (:enchant)
  :components ((:file "load-enchant")))
