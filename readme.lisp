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

  - Hunspell
  - GNU Aspell
  - Hspell
  - Voikko
  - Apple Spell
  - Zemberek

This Common Lisp Enchant package uses [The Common Foreign Function
Interface][CFFI] (CFFI) for accessing the Enchant C library. This
package should work on any Common Lisp implementation which supports
CFFI.

[Enchant]: https://rrthomas.github.io/enchant/
[CFFI]:    https://cffi.common-lisp.dev/


Installation
------------

[Quicklisp][QL] is the easiest way to install and load Enchant. Two
[ASDF][] systems are provided: The system `enchant` loads the main
Enchant library. There is also `enchant-autoload` system which depends
on the main library and also tries to load the Enchant C library (using
CFFI's facilities).

[QL]:   http://www.quicklisp.org/
[ASDF]: https://asdf.common-lisp.dev/


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

Author: Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [6965F03973F0D4CA22B9410F0F2CAE0E07608462][PGP]

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

(load "~/quicklisp/setup.lisp")

(asdf:initialize-source-registry
 (list :source-registry
       :ignore-inherited-configuration
       (list :directory *default-pathname-defaults*)))

(ql:quickload "enchant" :silent t)
(load "print-doc.lisp")
(format t *head*)
(print-doc:print-doc "ENCHANT")
