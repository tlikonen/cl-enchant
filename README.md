CL Enchant
==========

**Common Lisp interface for Enchant spell-checker library**

(incomplete)


Introduction
------------

_CL Enchant_ is a Common Lisp interface for the [Enchant][]
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

_CL Enchant_ aim's to provide all or most of the Enchant's features. It
uses [The Common Foreign Function Interface][CFFI] (CFFI) for accessing
the Enchant C library. It should work on any Common Lisp implementation
which supports CFFI.

[Enchant]: http://www.abisource.com/projects/enchant/
[CFFI]:    http://common-lisp.net/project/cffi/


Examples
--------

### Function: `(dict-check dict word)`

Check the spelling for _word_ using dictionary _dict_.

    ENCHANT> (with-dict (lang "en_GB")
               (dict-check lang "working")) ; correct
    "working"

    ENCHANT> (with-dict (lang "en_GB")
               (dict-check lang "wrking"))  ; incorrect
    NIL


### Function: `(dict-suggest dict word)`

Get spelling suggestions for _word_ using dictionary _dict_.

    ENCHANT> (with-dict (lang "en_US")
               (dict-suggest lang "wrking"))
    ("wring" "working" "irking" "waking" "wrying" "parking" "marking" "winking"
     "wicking" "Zworykin" "dragging")


Interface (API)
---------------

The public programming interface is documented in the
[api-doc](api-doc.md) file.


Author and license
------------------

Author:  Teemu Likonen <<tlikonen@iki.fi>>

License: Public domain

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
