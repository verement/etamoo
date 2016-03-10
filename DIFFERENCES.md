
Differences between EtaMOO and LambdaMOO
========================================

Besides the most notable differences described in the README, other minor
differences include:

  * EtaMOO fixes a long-standing bug in LambdaMOO that prevents commands using
    the "off of" preposition from being parsed correctly. To accomplish this,
    the "off/off of" preposition has been changed to "off of/off".

  * Assignment expressions behave somewhat differently in EtaMOO than they do
    in LambdaMOO.

    Assuming `x = {1, 2}` and `y = "foo"`:

| Expression        | LambdaMOO        | EtaMOO           |
| ----------------- | ---------------- | ---------------- |
| `x[1] = x[2] = 3` | `x` => `{3, 2}`  | `x` => `{3, 3}`  |
| `x[1] = (x = 0)`  | `x` => `{0, 2}`  | (error) `E_TYPE` |
| `y[$][1] = "b"`   | (error) `E_TYPE` | `y` => `"fob"`   |

  * EtaMOO provides a visual indication of the point at which MOO code
    compilation failed as part of the list of strings returned by
    `set_verb_code()` and `eval()`.

  * To mirror and complement the native support for string-key association
    list indexing, EtaMOO also extends the `listset()` and `listdelete()`
    functions to accept string-key indices for manipulating well-formed
    association lists.

  * Versions of LambdaMOO up to 1.8.3 only restrict to wizards the *reading*
    of built-in properties protected by `$server_options.protect_`*`prop`*.
    EtaMOO, as well as more recent versions of LambdaMOO, also restrict
    *writing* to such protected properties.

  * EtaMOO doesn't currently check the validity of built-in function names
    when compiling verb code; instead, calling an unknown function raises an
    error at runtime. (This is subject to change.)

  * In some cases the semantics of a language construct or built-in function
    differ slightly from that of LambdaMOO. Because MOO tasks run inside of an
    atomic transaction in EtaMOO, it is sometimes necessary to commit the
    transaction prematurely in order to perform some I/O or schedule another
    task. In these cases, the effect is the same as if `suspend(0)` had been
    called. These cases include:

      * `fork`
      * `listen()`
      * `open_network_connection()`
      * `memory_usage()`

  * In both EtaMOO and LambdaMOO, the `crypt()` built-in is a thin wrapper
    around the host system's `crypt()` library function. LambdaMOO doesn't
    check the return value from this function to see if it failed; it ends up
    returning an empty string in this case. EtaMOO raises `E_INVARG` instead.
    Note that `crypt()` can fail if an unsupported salt parameter is used.

  * The `value_hash()`, `string_hash()`, and `binary_hash()` built-in
    functions in EtaMOO accept two optional arguments in addition to the value
    or string to be hashed. The second argument is a string which selects the
    particular hash algorithm to use, and defaults to `"MD5"`. The following
    algorithms are supported:

        MD2           SHA-256        SHA3-256         Skein-512-256
        MD4           SHA-384        SHA3-384         Skein-512-384
        MD5           SHA-512        SHA3-512         Skein-512-512
        RIPEMD-160    SHA-512/224    Skein-256-224    Tiger
        SHA-1         SHA-512/256    Skein-256-256    Whirlpool
        SHA-224       SHA3-224       Skein-512-224

    The third argument, if provided and true, causes the digest value to be
    returned as a binary string instead of a string of hexadecimal digits.

  * In LambdaMOO, the strings returned from the `value_hash()`,
    `string_hash()`, `binary_hash()`, and `encode_binary()` built-in functions
    use uppercase hexadecimal digits. In EtaMOO, these strings use lowercase
    digits.

  * EtaMOO expects only printable ASCII characters to be present within MOO
    binary strings; in particular, ASCII HT (horizontal tab) is forbidden, and
    should be encoded instead as `"~09"`.

  * In LambdaMOO, the `buffered_output_length()` built-in returns the number
    of *bytes* currently buffered for output to a connection. In EtaMOO, this
    built-in currently returns the number of *items* buffered, where an item
    essentially represents all the data from a single call to `notify()`.
    (This is subject to change.)

  * EtaMOO accepts an optional argument to the `db_disk_size()` built-in that,
    if provided and true, causes the function to return an association list
    with various statistics from the persistence layer.

  * The result of the `disassemble()` built-in is very different in EtaMOO
    than in LambdaMOO, and currently shows the internal abstract syntax tree
    associated with a verb. (This is subject to change.)

  * The effective range of object values in EtaMOO is system-dependent, and
    not necessarily the same as the range of integer values.

  * Due to the way regular expression matching is implemented in EtaMOO, the
    `match()` built-in function is generally going to be more efficient than
    `rmatch()` and may also be able to handle a greater range of patterns
    before encountering resource limitations.

  * The numbers returned by the `value_bytes()` and `object_bytes()` built-in
    functions, as well as the last number in each list returned by
    `queued_tasks()`, are really vague estimates and probably not very
    accurate or meaningful due to the nature of the Haskell run time
    environment.

  * The Emergency Wizard Mode provided by EtaMOO works differently than it
    does in LambdaMOO. Firstly, in EtaMOO it is not possible to discard the
    changes made in this mode; be sure to operate on a database copy if there
    is a possibility of making unwanted changes. Secondly, the built-in MOO
    code/expression evaluator in EtaMOO maintains a single environment for
    variables throughout the mode, so for example it is possible to reference
    the values assigned to variables in previous evaluations. Thirdly, EtaMOO
    incorporates a full MOO command parser, so it is possible to execute
    arbitrary MOO commands from the database in addition to the mode's special
    built-in commands. Fourthly, EtaMOO provides line editing, history, and
    command completion (based on database contents) for convenience.

WAIF Notes
----------

EtaMOO includes an optional WAIF datatype implementation based on
[Ben Jackson's WAIF patches][] for the LambdaMOO server. While the
implementation closely follows Ben Jackson's description, there are some
differences compared with LambdaMOO:

  [Ben Jackson's WAIF patches]: http://ben.com/MOO/waif.html

  * The `toliteral()` format of WAIF values in EtaMOO includes a unique
    address to distinguish individual WAIFs. Consequently, `toliteral(w1) ==
    toliteral(w2)` iff `w1 == w2` for any WAIFs `w1` and `w2`, which also
    ensures a high likelihood that `value_hash(w1) != value_hash(w2)` when `w1
    != w2`. Note that WAIF addresses are informational only and should not be
    relied upon for any purpose; they are not preserved across database
    export/import, and they are not universally unique. (The behavior of
    `toliteral()` is subject to change.)

  * The ordering comparison operators (`<`, `<=`, `>=`, `>`) cannot be used
    with WAIFs in EtaMOO; such use will raise `E_TYPE`. (While these operators
    did not raise an error in LambdaMOO, they did elicit a log message and the
    results were basically nonsensical.)

  * EtaMOO allows WAIFs to be created with an invalid class object
    (e.g. `#-1`). Such WAIFs behave the same as WAIFs whose class object is
    later recycled; attempts to dereference properties or call methods on
    these WAIFs will raise `E_INVIND`.

  * The effect of renaming properties on class objects referenced by existing
    WAIFs is unspecified. Currently this may cause the corresponding property
    value on all existing WAIFs to become hidden, or it may reveal a
    previously hidden value. Similar effects can also result from re-adding
    previously removed properties. (This is subject to change.)

  * Tracebacks in EtaMOO include a description of `this` when the value is a
    WAIF. (This is subject to change.)

  * EtaMOO defines the variable `WAIF` within MOO verbs to have the same value
    as `typeof(new_waif())`.
