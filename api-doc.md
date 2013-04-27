The API documentation for CL Enchant
====================================


### Function: `(activep object)`

Test if _object_ is active. Return a generalized
boolean.


### Class: `broker`

Class for holding pointers to foreign (non-Lisp) broker resources.
Instances are created with `broker-init` function.


### Function: `(broker-dict-exists-p broker language)`

Check if _language_ exists. _Broker_ must be a valid `broker` object
returned by `broker-init`. _Language_ is a language code and optional
country code as a string (e.g., "fi", "en_GB").

If the _language_ exists return the _language_ string. Otherwise return
`nil`.

If _broker_ is not an active `broker` object signal `not-active-broker`
error condition.


### Function: `(broker-free broker)`

Free the foreign (non-Lisp) `broker` resources. The argument is a
`broker` object returned by `broker-init`. The `broker` object becomes
"inactive" and can't be used anymore.


### Function: `(broker-free-dict broker dict)`

Free the foreign (non-Lisp) `dict` resources. The first argument is a
`broker` object returned by `broker-init` and the second a `dict` object
returned by `broker-request-dict`. The `dict` object becomes
"inactive" and can't be used anymore.


### Function: `(broker-init)`

Initialize a new broker. Return a `broker` object which can be used
to request dictionares etc. See function `broker-request-dict`.

A `broker` object is "active" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free`. After being freed it becomes "inactive" and
thus unusable. Generic function `activep` can be used to test if a
`broker` object is active or not.

See macros `with-broker` and `with-dict` which automatically initialize
and free broker and dictionary resources.


### Function: `(broker-request-dict broker language)`

Request a new dictionary for _language_. Return a `dict` object which
can be used with spell-checker operations.

The _broker_ argument must be an active `broker` object created with
`broker-init`. _Language_ is a language code and optional country code
as a string (e.g., "fi", "en_GB").

A `dict` object is "active" when it has been succesfully created. It
allocates foreign (non-Lisp) resources and must be freed after use with
function `broker-free-dict`. After being freed it becomes "inactive"
and thus unusable. Generic function `activep` can be used to test if
`dict` object is active or not.

If no suitable dictionary could be found `dict-not-found` error
condition is signalled.

See also `with-dict` macro which automatically creates a `dict`
environment and frees it in the end.


### Class: `dict`

Class for holding pointers to foreign (non-Lisp) dictionary
resources. Instances are created with `broker-request-dict` function.


### Function: `(dict-check dict word)`

Check the spelling of _word_ (string) using dictionary _dict_.
Return _word_ if the spelling is correct, `nil` otherwise.

_Dict_ must be an active `dict` object returned by
`broker-request-dict`, if not, signal a `not-active-dict` condition.


### Function: `(dict-suggest dict word)`

Request spelling suggestions for _word_ (string) using dictionary _dict_.
Return a list of suggestions (strings) or nil if there aren't any.

_Dict_ must be an active `dict` object returned by
`broker-request-dict`, if not, signal `not-active-dict` condition.


### Function: `(get-version)`

Return the Enchant library version.


### Macro: `(with-broker variable &body body)`

Initialize a new `broker` (using `broker-init`) and bind _variable_
to the `broker` object. Execute all _body_ forms and return the values
of the last _body_ form. Finally, free the `broker` resources with
function `broker-free`.


### Macro: `(with-dict (variable language &optional broker) &body body)`

Request a new dictionary object for _language_. Bind _variable_ to
the new `dict` object and execute all _body_ forms. Return the values of
the last _body_ form. Finally, free the `dict` resources with function
`broker-free-dict`.

If the optional _broker_ argument is given reuse that broker object when
requesting DICT. If the _broker_ argument is not given create implicitly
a new `broker` object with `broker-init` and free it in the end with
`broker-free`. Note that the decision about the _broker_ argument is
done at the macro-expansion time. If there is anything (except the
symbol `nil`) in the place of the _broker_ argument that will be used as
the broker.

Examples:

    ENCHANT> (with-dict (lang "fi")
               (dict-check lang "toimii"))
    "toimii"

    ENCHANT> (with-broker b
               (with-dict (lang "fi" b)
                 (dict-suggest lang "tomii")))
    ("omii" "Tomi" "toimi" "toimii" "Tomisi")


