(defsystem "enchant-autoload"
  :description "Programming interface for Enchant spell-checker library (autoload Enchant)"
  :author "Teemu Likonen <tlikonen@iki.fi>"
  :licence "Creative Commons CC0 (public domain dedication)"
  :depends-on ("enchant")
  :components ((:file "load-enchant")))
