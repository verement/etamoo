
Important!
==========

**This is experimental and (currently) incomplete software. It is not yet
usable, although with further development it is hoped that it soon will be.**

_At present, all the code will do is (optionally) load a database file and
present a REPL interface for running commands and evaluating MOO code from the
console as if you had connected as a wizard. While most MOO code will run
correctly, there are still some built-in functions that are not yet
implemented. Also, notably, there is almost no network functionality, and no
way to save any changes made to the database. Finally, there has been no
effort to optimize any performance bottlenecks, of which loading a database is
at least one._

_Once the code is in at least a minimally useful state, this message will be
updated or removed._

About
=====

EtaMOO is a new implementation of the LambdaMOO server written in Haskell.

[LambdaMOO][] is a network-accessible, multi-user, programmable, interactive
system well-suited to the construction of text-based adventure games,
conferencing systems, and other collaborative software.

  [LambdaMOO]: http://www.ipomoea.org/moo/

EtaMOO differs from LambdaMOO in a few significant ways:

  * EtaMOO is multi-threaded. MOO tasks run concurrently, producing network
    output and changes to the database in isolated transactions that are
    committed only when not in conflict with any other transaction. (In cases
    of conflict, transactions are automatically retried.) Separate threads are
    also used for network connection management, so for example name lookups
    do not block the entire server.

  * In some cases, MOO values may be computed in a "lazy" manner, meaning the
    evaluation could be skipped if MOO code never fully inspects the result.
    This could be beneficial when large list structures are returned from
    built-in functions, for example.

  * It is anticipated that EtaMOO will easily support 64-bit MOO integers
    and/or Unicode MOO strings via independent build options.

  * EtaMOO supports IPv6.

The implementation of EtaMOO closely follows the specifications of the
[LambdaMOO Programmer's Manual][Programmer's Manual], and should therefore be
compatible with most LambdaMOO databases as of about version 1.8.3 of the
LambdaMOO server code.

  [Programmer's Manual]: http://www.ipomoea.org/moo/#progman

Installing
----------

EtaMOO is built with [Cabal][], the Haskell package manager. In the simplest
case, simply running:

    cabal install EtaMOO

should automatically download, build, and install the `etamoo` executable
after doing the same for all of its Haskell dependencies.

  [Cabal]: http://www.haskell.org/cabal/

Cabal itself is part of the [Haskell Platform][] which is available for many
distributions and platforms.

  [Haskell Platform]: http://www.haskell.org/platform/

EtaMOO has non-Haskell dependencies on two external libraries: _libpcre_ (with
UTF-8 support enabled) for regular expression matching, and, possibly,
_libcrypt_ (often part of the standard libraries) for the MOO `crypt()`
built-in function. You may need to ensure you have these available before
installing EtaMOO.

Hacking
-------

[Documentation][] is available for the various types, data structures, and
functions used internally by EtaMOO.

  [Documentation]: http://verement.github.io/etamoo/doc/

