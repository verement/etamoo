
Important!
==========

**This is experimental software. While it is now mostly functional, it is not
  yet fully complete.**

_Until such time as the EtaMOO database format is well tested and considered
stable, please make and keep LambdaMOO-format backup copies of your EtaMOO
databases._

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

  * EtaMOO uses [LMDB][] as a persistent backing store for the MOO database.
    Changes are committed on an ongoing basis for instantaneous crash
    recovery; checkpoints perform a quick synchronization for guaranteed
    durability, but are otherwise unnecessary. The persistence layer includes
    an automatic value cache with implicit structure sharing, so the entire
    database need not be in memory at once, and duplicate values are stored
    only once. EtaMOO provides mechanisms for importing and exporting
    LambdaMOO-format databases to and from the EtaMOO-native format.

  * EtaMOO is Unicode-aware, and will eventually include support for Unicode
    MOO strings via compile-time build option.

  * EtaMOO supports 64-bit MOO integers via compile-time build option.

  * EtaMOO supports the lightweight object [WAIF datatype][] via compile-time
    build option.

  * EtaMOO natively supports string-key association lists with efficient
    lookup and update operations; the list index syntax has been extended to
    allow _`alist`_`[`_`key`_`]` and _`alist`_`[`_`key`_`] = `_`value`_ for
    string *`key`*s whenever *`alist`* is a well-formed association list.

  * EtaMOO supports several additional hashing algorithms besides MD5,
    including SHA-1, SHA-2, SHA-3, and RIPEMD-160, via optional argument to
    `string_hash()`, `binary_hash()`, and `value_hash()`. Hash digests may
    also optionally be returned as binary strings.

  * EtaMOO internally handles binary strings in an efficient manner, and only
    translates to and from the special MOO *binary string* syntax upon demand.
    For example, passing a binary string read from the network directly to
    `decode_binary()` does not suffer a round trip through the *binary string*
    representation.

  * EtaMOO supports fractional second delays in `suspend()` and `fork`.

  * EtaMOO supports IPv6.

  [LMDB]: http://symas.com/mdb/
  [WAIF datatype]: http://ben.com/MOO/waif.html

The implementation of EtaMOO otherwise closely follows the specifications of
the [LambdaMOO Programmer's Manual][], and should be compatible with most
LambdaMOO databases as of about version 1.8.3 of the LambdaMOO server code.

  [LambdaMOO Programmer's Manual]: http://www.ipomoea.org/moo/#progman

Installing
----------

EtaMOO is built with [Cabal][], the Haskell package manager. In the simplest
case, running:

    cabal install EtaMOO

should automatically download, build, and install the `etamoo` executable
after doing the same for all of its Haskell dependencies.

Cabal itself is part of the [Haskell Platform][] which is available for many
distributions and platforms.

  [Cabal]: http://www.haskell.org/cabal/
  [Haskell Platform]: http://www.haskell.org/platform/

There are a few options you can give to `cabal install` to customize your
build:

| Option                | Feature                                       |
| --------------------- | --------------------------------------------- |
| `-j`                  | Build in parallel using multiple processors   |
| `-f llvm`             | Use GHC's LLVM backend to compile the code    |
| `-f 64bit`            | Enable 64-bit MOO integers                    |
| `-f waif`             | Enable the lightweight object WAIF datatype   |

EtaMOO has non-Haskell dependencies on three external libraries: _liblmdb_ for
database persistence, _libpcre_ (with UTF-8 support enabled) for regular
expression matching, and, possibly, _libcrypt_ (often part of the standard
libraries) for the MOO `crypt()` built-in function. You should ensure you have
these available before installing EtaMOO (e.g. on Debian-derived systems,
`sudo apt-get install liblmdb-dev libpcre3-dev`).

Running
-------

`etamoo` is nearly a drop-in replacement for the LambdaMOO `moo` executable;
the main difference is that `etamoo` takes a single database path, rather than
both input and output paths. You can run `etamoo --help` for a command-line
synopsis.

EtaMOO uses a native binary database format that allows quick loading and
checkpointing, and instantaneous crash recovery. You can create a native
database from a LambdaMOO-format database by using `etamoo --import`. You can
also go the other way and convert an EtaMOO database back to a
LambdaMOO-format database with `etamoo --export`.

If you don't already have a database, you can find LambdaMOO-format cores for
various MOOs online -- for example there is the venerable [LambdaCore][], or
an alternative [JHCore][]. You can also request a character on [Waterpoint][]
and then perform a live [JHCore extraction][]. (Note that Waterpoint's core
extraction process requires running an actual LambdaMOO server executable on
the precore database to obtain the final core database; EtaMOO cannot yet do
this itself.)

  [LambdaCore]: http://ftp.lambda.moo.mud.org/pub/MOO/
  [JHCore]: http://jhcore.sourceforge.net
  [Waterpoint]: http://waterpoint.moo.mud.org/
  [JHCore extraction]: http://waterpoint.moo.mud.org:8080/core-extraction/

By default, EtaMOO will make use of all available CPUs for maximum
parallelism. If you'd rather limit the number of processors EtaMOO uses, you
can use the command-line option `+RTS -N`_`n`_` -RTS` where _`n`_ is the
number of processors to use.

If you want to enable statistics from the `memory_usage()` built-in function,
you will need to add `+RTS -T -RTS` to the command line options.

Limitations
-----------

The following LambdaMOO features are currently unsupported:

  * The `.program` intrinsic command
  * The `verb_cache_stats()` and `log_cache_stats()` built-in functions
  * Importing, exporting, or checkpointing of queued tasks in the database
    file
  * Task time limits (ticks are counted, but seconds are not)
  * The `NP_SINGLE` and `NP_LOCAL` network protocols (i.e. stdin/stdout,
    UNIX-domain sockets, and/or named pipes; only TCP/IP is supported)
  * Customizing `OUT_OF_BAND_PREFIX` and `OUT_OF_BAND_QUOTE_PREFIX` (these are
    currently fixed as `#$#` and `#$"`, respectively)
  * The `IGNORE_PROP_PROTECTED` compilation option
  * `$server_options.name_lookup_timeout`

See also the `DIFFERENCES.md` file for other differences between EtaMOO and
LambdaMOO.

Hacking
-------

[Documentation][] is available for the various types, data structures, and
functions used internally by EtaMOO.

  [Documentation]: http://verement.github.io/etamoo/doc/

