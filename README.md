CL-Enchant
==========

**Common Lisp interface for Enchant spell-checker library** (incomplete)


Introduction
------------

_CL-Enchant_ is a Common Lisp interface for the [Enchant][]
spell-checker library. Enchant is a generic spell-checker library which
uses other spell-checkers transparently as back-end. Enchant supports
the following checkers:

  - Aspell/Pspell
  - Ispell
  - MySpell/Hunspell
  - Uspell (Yiddish, Hebrew and Eastern European languages)
  - Hspell (Hebrew)
  - Zemberek (Turkish)
  - Voikko (Finnish)
  - AppleSpell (Mac OSX)

_CL-Enchant_ uses [The Common Foreign Function Interface][CFFI] (CFFI)
for accessing the Enchant C library. It should work on any Common Lisp
implementation which supports CFFI.

[Enchant]: http://www.abisource.com/projects/enchant/
[CFFI]:    http://common-lisp.net/project/cffi/


Interface (API)
---------------

_CL-Enchant_ aim's to provide all or most of the Enchant's features.
Currently the project has just started and only the basic functionality
has been implemented. Some examples below.


### Function: `(get-version)`

    ENCHANT> (get-version)
    "1.6.0"


### Macro: `(with-dict (variable language) &body ...)`

    ENCHANT> (with-dict (english "en_GB")
               (dict-check english "working"))
    T

    ENCHANT> (with-dict (english "en_GB")
               (dict-check english "wrking"))
    NIL


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
