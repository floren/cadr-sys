System 78 is now released to users.  It has been a long time since an
official release, so this message is rather long.
From now on, system release messages will be divided into two parts, to
make things more pleasant for those of you who don't like reading long
messages.  First, we will list changes that might potentially cause
problems or confuse you when you start using the new system.  Second, we
will list new features or important bug fixes that might help you out.  If
you're a Lisp Machine user, you should read the first section, but you can
safely ignore the second section if you'd rather not be bothered.

  Table of contents:
  Things you should take note of:
 [1] System 78 requires new microcode.
 [2] If the QFASL and source of a file are in different directories, you
     must recompile the file.
 [3] Function specs have changed; FDEFINE, COMPILE, TRACE, et. al. are affected.
 [4] The :CLOSE message to a chaos stream has changed - probably you are not affected.
 [5] The system menu has changed - it is a more powerful superset.
 [6] You may get spurious warnings about redefining functions - recompile.
  Things you may safely ignore:
 [A] Streams have been changed; there is a system of flavors that you can use if you
     want to, though you don't have to.
 [B] OPEN has a new argument syntax (the old one still works).
 [C] WITH-OPEN-STREAM exists.
 [D] There are new high-level functions for using the Chaosnet.
 [E] New :HANDLER function spec.
 [F] SI:PRINT-READABLY controls unreadable forms.
 [G] TV sync prom is used when machine is booted.
 [H] Source files are remembered for all definitions.
 [I] Files with compile-flavor-methods ought to be recompiled if you care a lot
     about efficiency.
 [J] New features in MAKE-SYSTEM.
 [K] Some changes in low-level disk control.
 [L] The scavenger working set size can be controlled.
 [M] The editor tries to give you more information in its display.
 [N] Preparing now for coming new error system.
 [O] The Greek letter convention for the Control/Meta keys is obsolete.

  Things that you should take note of:

[1] System 78 requires microcode 836.  It will fail to cold-boot with the
previous microcode (793).  The system 74 will fail to work with
the new microcode also.  (Ucode versions < 810, 825, 827 and 828 go with system 74,
811, 826, 832 and up are compatible with systems 76, 77, and 78).

[2] In order to interact properly with M-. in the editor, all files whose
QFASL's live in a different directory than the source will need to be
recompiled in system 78 (or in system 75.14 or later).  The present scheme of
guessing the source file pathname based on the truename of the source has
proved too bug-prone, and the correct information is now recorded by the
compiler.

[3] Function specs have been completely reimplemented.  As a side-effect of
this the function spec (FOO BAR) as an abbreviation for (:PROPERTY FOO BAR)
is now accepted only by defining forms such as DEFUN, DEFMACRO, and
DEFSELECT; the full form must be used with the function-manipulating
functions such as FDEFINE, COMPILE, TRACE, etc.  Formerly the short form
worked partially with these functions.  If you get an error message that
something is not a valid function spec, this is probably the reason.

[4] The :CLOSE message to a chaos stream now behaves like the :EOF message
if not given an abort-p argument.  The connection is also freed, so this
need not be done manually.  Note that the CHAOS:STREAM function still
gives bidirectional streams, but in many cases you may only want the
unidirectional flavor (see below for more details).

[5] The system menu has been made more useful; the "Other" command have been
integrated into the menu, as have some useful screen editor commands and
some commands that are like what you can do with the System key.  If you
just pop one up and look at the mouse documentation for each item, it
should be clear what all of the commands mean.  Lacking any better place
to document it, I'll mention here that the function
TV:ADD-TO-SYSTEM-MENU-PROGRAMS-COLUMN should be called by user programs
that want to add themselves to the system menu.

[6] In some obscure cases, you may get undeserved warnings about
redefining system functions when you load old QFASL files into System
77.  For example, if you had a defstruct with the same name as a
system function, and you last compiled the file in certain system
releases, this may happen.  Just recompile the file in System 77 and
the new QFASL file won't produce the warning.

  Things that you may safely ignore:

[A] A major change in this system is a thorough reimplementation of streams.
All old streams will continue to work; however, a system of flavors has
been set up to make it easier for you to write new streams.  All buffered
streams in the system (e.g. files, network, mag tape) are now built out of
these flavors, eliminating much duplicated code and the consequent
inconsistencies and inefficiencies.  This new basis for streams is not yet
documented, but the source code may be found in the file SYS:IO;STREAM
LISP.  The :FINISH message has been changed incompatibly; it now implies a
:FORCE-OUTPUT (which is what it ought to have done all along).  There is a
new message to streams, :STRING-IN, which reads efficiently into an array;
it and :STRING-OUT are the right primitives from which to build most forms
of binary record I/O.  As mentioned above, the :CLOSE message to a Chaosnet
stream is now consistent with all other streams.  A new message to
buffered streams, :READ-INPUT-BUFFER, behaves exactly like
:GET-INPUT-BUFFER but returns the values ARRAY, INDEX, and LIMIT.  This
should be used in all new code instead of :GET-INPUT-BUFFER, which will
be faded out in 1990.
    There has been some inconsistancy in the meaning of the optional
argument to the :ADVANCE-INPUT-BUFFER message to a stream.  In system
78, everything has been changed to assume it is a buffer index.  This
agrees with the documentation.

[B] The OPEN function now accepts keyword arguments in the standard fashion,
after a required first argument which is the pathname of the file to be
opened.  The keywords will be documented shortly.  For backwards
compatibility, if OPEN is given only two arguments the second argument
is an option or list of options from the old set of Maclisp-compatible
OPEN options.  Naturally WITH-OPEN-FILE takes keywords the new way, just
as OPEN does.

[C] There is a new special form, WITH-OPEN-STREAM, which is completely
analogous to WITH-OPEN-FILE except that you specify a form whose value
is the stream, rather than arguments to OPEN.  This is used with
non-file streams.

[D] The preferred way of interacting with the chaosnet at the highest level
is now via two new functions, CHAOS:OPEN-STREAM and CHAOS:MAKE-STREAM.
CHAOS:OPEN-STREAM takes a host, contact name, and set of keyword options
and returns an open stream to that host.  CHAOS:MAKE-STREAM takes an
open connection and options and returns a stream.  This is useful when
the connection has been gotten via a CHAOS:LISTEN, for example.
Both functions accept the following keywords:
:DIRECTION - :INPUT, :OUTPUT, or :BIDIRECTIONAL.  Default is :BIDIRECTIONAL.
:CHARACTERS - boolean.  Default is T.
  If non-NIL, ascii rather than binary data is to be sent.

CHAOS:OPEN-STREAM also accepts these keywords relating to the connection itself:
:ERROR - boolean.  Default is T.  A string is returned on an error for :ERROR NIL.
:TIMEOUT - number of 60ths of a second.  Default is 10 seconds.
:WINDOW-SIZE - number.  Default is like CHAOS:CONNECT formerly.

[E] A new function spec (:HANDLER <flavor> <message>) refers to the function
which is evoked when an object of flavor <flavor> is sent the message
<message> (i.e., the element of the FLAVOR-SELECT-METHOD).  This is
distinct from the :METHOD function spec, which refers to the function
defined in flavor <flavor>, and possibly inherited by many other
flavors.

[F] A new variable SI:PRINT-READABLY enables you to specify that what you
are printing is intended to be read with READ again, and should not
contain any unreadable forms.  When SI:PRINT-READABLY is bound to
non-NIL and an attempt is made to print an unreadable object, an error
will be signalled on SI:PRINT-NOT-READABLE.  Anyone who defines their
own :PRINT-SELF messages should be aware of a new macro
SI:PRINTING-RANDOM-OBJECT, which must be used to preserve the
functionality of SI:PRINT-READABLY.  It is documented in the source.

[G] When booted, the system will now always use the TV sync prom if there
is one, unless set-tv-speed was done before the disk-save.  If you do
a set-tv-speed and save out the band, please put something in the label
comment so someone transfering the band to another machine will be aware
of what has been done ("60Hz" is the usual thing to reduce hum on VMI monitors).
This should decrease the incidence of seasickness.

[H] The system now remembers source files for all functions, not just those
named by symbols, and does not get confused by multiple definitions of
different types with the same name (for instance, a function, a variable, a
flavor, a defstruct, and a resource with the same name).  Thus the editor
M-. command is more likely to work now.  The whole function spec and source
file system has been reimplemented, and numerous bugs have been fixed.
flavor-method-symbols (e.g. tv:basic-momentary-menu-after-deexpose-method)
no longer exist; the function spec (e.g. (:method tv:basic-momentary-menu
:after :deexpose) is now used for all purposes formerly served by the
symbol).

[I] Any files containing COMPILE-FLAVOR-METHODS should be recompiled in system
77 (or in system 75.22 or later).  Old qfasl files will still work; however,
the system will regard all combined-methods in them as obsolete and will
generate new ones at load time; thus the COMPILE-FLAVOR-METHODS will be
inutile.

[J] MAKE-SYSTEM will now offer to compile and load a new version of the file
containing the DEFSYSTEM for the system if it has changed.  In fact, a new
function SI:SET-SYSTEM-SOURCE-FILE allows you to specify what file a system
is defined in before that system is loaded at all.  It will then be loaded
the first time MAKE-SYSTEM is done on it.  This might be useful in your
init file, for example.  The argument to this function is the name of the
system, i.e. the symbol that you would give to MAKE-SYSTEM.  Note: since
the system system has strong opinions about file types, this only happens
when the file with the DEFSYSTEM form has a file type of LISP, i.e. an FN2
of > on ITS.  Thus, if you have a file FOO PKG, and want to benefit from
this feature, you should rename it to FOOPKG >.
    The :RECOMPILE keyword to MAKE-SYSTEM is the same now as the :COMPILE and
:RELOAD keywords together.  This should be easier to remember.

[K] The A-MEMORY variable SYS:%DISK-READ-COMPARE-ENABLES has been renamed to
SYS:%DISK-SWITCHES.  Bit 2 in this variable now controls the multiple
page swapout feature.  When a page is swapped out and this bit is on,
the system will look to see if the next higher page in virtual memory is
present in physical memory and also needs to be written on disk.  If so,
it will be appended to the transfer.  Currently the maximum size of
transfer which can be built in this way is set to 20 pages.  The pages in
question are not actually removed from core, but just changed to
READ-WRITE-FIRST status.  If they are later selected for swapout
before they are modified again, a disk write will be saved.  A new
meter, %COUNT-DISK-PAGE-WRITE-APPENDS, records the number of pages
added to transfers in this way. 

Bit 3 in SYS:%DISK-SWITCHES now controls the multiple page swapin
feature which exists in microcode version 826 and later.  When a swapin
is called for, the system decides how many pages it would like
to transfer with one disk op.  In system 78, this computation depends the area
of the page being brought in.  SYS:AREA-SWAP-RECOMMENDATIONS is a new
area number indexed area which holds the recommended swap size minus one
per area.  (Ie 0 says just swap the needed page, 1 try to append one page
to that, etc).  The user should not store in SYS:AREA-SWAP-RECOMMENDATIONS
directly, but should use (SI:SET-SWAP-RECOMMENDATIONS-OF-AREA <AREA-NUMBER> <N>)
Currently, 17 is the maximum value of N which should be used.
There is not much experience yet as to what are good values to put in here.  
(set-all-swap-recommendations 2) is a good way to turn on the feature
and start seeing what happens.  Exactly what the best initial setting
is will be determined thru experience.   Generally speaking, the more main
memory the machine has, the higher the swap sizes should be.
  There frequently seems to be a rather dramatic improvement in context
switch times (such as entering the editor for the first time, etc.)
Another new meter, %COUNT-DISK-PAGE-READ-APPENDS records
the number of pages added to transfers in this way.

[L] The size of the scavenger working set, formerly fixed at the low value
of 12 pages, may now be set at any time by (SI:SET-SCAVENGER-WS
<value>).  The way that this works is that main memory is divided into
two pools; one pool is only used by the user program, while both the
user and the scavenger contend for the other pool.  The <value> is the
number of pages of main memory (256 words each) in the shared pool.
Setting this value higher than 12 seems to improve the performance of
the GC significantly.  Values of 400 seem to work well for 256K
machines, larger values should probably be used for machines with more
memory.  Please report your experiences setting this parameter, since it
is unclear at this time what the default value should be.
  As part of this change, the meaning of the SI:%SCAVENGER-WS-ENABLE
variable has been changed.  It is now the dividing point in physical
memory between the two pools.  (As a semi-compatibility feature,
however, SI:%SCAVENGER-WS-ENABLE of NIL will still turn off the
scavenger working set feature.)
  The former low value of the scavenger working set parameter may have
accounted for unacceptable garbage collector paging performance in some
applications.

[M] When the editor is reading a command argument from the mini-buffer
and some sort of command or filename completion is present, the blinker
will have a small "c" character inside its box.  Likewise, when a search
string is being read that may contain special search characters (such as
<and> and <or>), a small "s" character appears.  This feature is
experimental.  Your comments on it and especially suggestions for
improving it are welcome.
    The version of the file last read or written is now displayed in
parentheses in the mode line.

[N] A new object oriented error handling and condition system will be
installed in one of the next few major system releases.  The major
change for simple programs is that functions like CHAOS:CONNECT and OPEN
which take a no-error argument and now return a string of the error will
instead return an error instance.  Since we imagine that there are a lot
of these simple programs, and to help ease you into this change, there
is a new global function ERRORP in system 78.  It is the same as
STRINGP.  When the new system is installed, it will be different, but
will still correctly recognize the error return case of the above
mentioned functions.

[O] The old convention of using alpha to mean Control, beta to mean
Meta, and so on, has been declared obsolete.  The use of this convention
in some system help messages has been replaced with the new convention,
namely C-, M-, and so on.  The FORMAT ~C directive has been changed to
use the new convention.  The Greek letters will continue to be accepted
by #/ for compatibility only.  If you see the system trying to type Greek
at you using this convention, tell us and we wil put a stop to it.
