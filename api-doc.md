The API documentation for CL-Enchant
====================================


### Function: `(activep object)`

Test if OBJECT is active. Return generalized boolean.


### Class: `broker`

Class for holding pointers to foreign (non-Lisp) broker resources.
Instances are created with BROKER-INIT function.


### Function: `(broker-dict-exists-p broker language)`

Check if LANGUAGE exists. BROKER must be a valid BROKER object
returned by BROKER-INIT. LANGUAGE is a language code and optional
country code as a string (e.g., "fi", "en_GB").

If the LANGUAGE exists return the LANGUAGE string. Otherwise return NIL.

If BROKER is not an active BROKER object signal NOT-ACTIVE-BROKER error
condition.


### Function: `(broker-free broker)`

Free the foreign (non-Lisp) BROKER resources. The argument is a
BROKER object returned by BROKER-INIT. The BROKER object becomes
"inactive" and can't be used anymore.


### Function: `(broker-free-dict broker dict)`

Free the foreign (non-Lisp) DICT resources. The first argument is a
BROKER object returned by BROKER-INIT and the second a DICT object
returned by BROKER-REQUEST-DICT. The DICT object becomes "inactive"
and can't be used anymore.


### Function: `(broker-init)`

Initialize a new broker. Return a BROKER object which can be used to
request dictionares etc. See function BROKER-REQUEST-DICT.

BROKER object is "active" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function BROKER-FREE. After being freed it becomes "inactive" and thus
unusable. Generic function ACTIVEP can be used to test if BROKER object
is active or not.

See also macros WITH-BROKER and WITH-DICT which automatically initialize
and free broker and dictionary resources.


### Function: `(broker-request-dict broker language)`

Request new dictionary for LANGUAGE. Return DICT object which can be
used with spell-checker operations.

The BROKER argument must be an active BROKER object created with
BROKER-INIT. LANGUAGE is a language code and optional country code as a
string (e.g., "fi", "en_GB").

DICT object is "active" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function BROKER-FREE-DICT. After being freed it becomes "inactive" and
thus unusable. Generic function ACTIVEP can be used to test if DICT
object is active or not.

If no suitable dictionary could be found DICT-NOT-FOUND error condition
is signalled.

See also WITH-DICT macro which automatically creates a DICT
environment and frees it in the end.


### Class: `dict`

Class for holding pointers to foreign (non-Lisp) dictionary
resources. Instances are be created with BROKER-REQUEST-DICT
function.


### Function: `(dict-check dict word)`

Check the spelling of WORD (string) using dictionary DICT.
Return WORD if the spelling is correct, NIL otherwise.

DICT must be an active DICT object returned by BROKER-REQUEST-DICT. If
not, signal NOT-ACTIVE-DICT condition


### Function: `(dict-suggest dict word)`

Request spelling suggestions for WORD (string) using dictionary DICT.
Return a list of suggestions (strings) or nil if there aren't any.

DICT must be an active DICT object returned by BROKER-REQUEST-DICT. If
not, signal NOT-ACTIVE-DICT condition.


### Function: `(get-version)`

Return Enchant library version.


### Macro: `(with-broker variable &body body)`

Initialize a new broker (using BROKER-INIT) and bind VARIABLE to the
BROKER object. Execute all BODY forms and return the values of the last
BODY form. Finally, free the BROKER resources with function
BROKER-FREE.


### Macro: `(with-dict (variable language &optional broker) &body body)`

Request a new dictionary object for LANGUAGE. Bind VARIABLE to the
new DICT object and execute all BODY forms. Return the values of the
last BODY form. Finally, free the DICT resources with function
BROKER-FREE-DICT.

If the optional BROKER argument is given reuse that broker object when
requesting DICT. If the BROKER argument is not given (NIL) create
implicitly a new BROKER object with BROKER-INIT and free it in the end
with BROKER-FREE.

Examples:

    ENCHANT> (with-dict (lang "fi")
               (dict-check lang "toimii"))
    "toimii"

    ENCHANT> (with-broker b
               (with-dict (lang "fi" b)
                 (dict-suggest lang "tomii")))
    ("omii" "Tomi" "toimi" "toimii" "Tomisi")


