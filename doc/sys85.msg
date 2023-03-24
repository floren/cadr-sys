-*- Text -*-
MIT System 85 comes with microcode 122.

  Table of contents of additions to system 85 as compared to 83:
  Things you should take note of:
   [1] A new rubout handler installed by default.
   [2] There is a new compressed format for bands.
   [3] A new LOGIN syntax is being introduced.

  Things you may safely ignore:
   [A] During a QC-FILE the compile and run time flavor environments are segregated.
   [B] Flavor instantiation has been sped up, and the number of conses reduced.
   [C] Many new ZWEI features.
   [D] TV:SET-DEFAULT-FONT improved, TV:SET-STANDARD-FONT introduced.
   [E] DRIBBLE-ALL

[1]  CWH's rubout handler is loaded and turned on by default.
   (TV:RH-OFF) turns it off, and (TV:RH-ON) turns it back on.
   Its main features are that it is fast and unobtrusive as compared
   to ZTOP mode.  The commands are a subset of ZWEI.  Just typing HELP
   at a lisp listener will give a brief blurb.
   Additional: the rubout handler now shares ZWEI's kill ring.
   If you start typing something at a rubout handler, you can type
   Clear-Input, go to the editor, and yank it back.

[2]  There is a new compressed band format, which makes saved disk bands
   significantly smaller.  Garbage collecting no longer increases the size
   of disk partition needed to save a core image, on the contrary, the band
   size required will be considerably reduced after a garbage collection.
   One note of caution though.  To run a restored band, the paging partition
   must be large enough to accomodate the highest used virtual page number
   within the band, which is no longer related to the size of the band.
   The function (SI:DESCRIBE-PARTITION <partition>) prints interesting
   information about a disk partition, including the highest used virtual
   page number and the desired microcode version in the case of a "LODn"
   partition.

[3]  A new LOGIN syntax is being introduced. The format is:
   (LOG1 user-name keyword1 argument1 keyword2 argument2...)
   where keywordN is either a keyword to be used by the user's init
   file, or one of the two special keywords :HOST and :INIT.  :HOST
   hostname specifies the associated machine for the session, and :INIT
   arg will load the users init file if arg is true. In addition to this
   special processing, the entire keyword list is available during the
   reading of the init file in the variable SI:USER-INIT-OPTIONS, which
   is NIL at all other times.  The HOST and INIT keywords are not
   required, and LOG1 will default in the same way that LOGIN does.

[A]  The compile time flavor environment is now segregated from the run time
   flavor environment during a QC-FILE, similarly to the way MACROs are treated.
   Previously, QC-FILEing a file which contained a DEFFLAVOR actually
   installed the new definition in the running environment.  If this definition
   was incompatible with the currently active definition, existing instances
   of those flavors would be "detached" and other weird effects could occur.
   Now, the DEFFLAVOR seen by QC-FILE merely makes an entry on FILE-LOCAL-DECLARATIONS
   and does not install the new definition until the QFASL file is loaded.

[B]  Flavor instantiation has been sped up, and the number of conses
   done concerned with INIT-PLISTs has been considerably reduced in many cases.

[C]  New ZWEI features:
  * Numeric arguments and prefix characters prompt, after a short wait,
    as they do in EMACS.

  * Files with no -*-Package: ...;-*- at the top are now edited in USER
    by default.  This default, and any package you specify with M-X Set Package,
    apply only to the particular editor buffer, not to other uses of the file.

  * The M-X Compile File command now reads a file name, not a buffer name.
    The new command M-X Load File reads a filename and loads it.
    The new command M-X Compile and Load File compiles the file if necessary,
    then loads it in any case.
    The new commands M-X Copy Text File and Copy Binary File copy files
    assuming the specified kind of data.

  * C-X O now takes a numeric argument, which specifies which window to go to,
    counting from the window at the top of the screen as 1.

  * If you visit two files with the same name (but different versions, types,
    directory names, etc), you get a warning, just to keep you alert.

  * Readin of files with Fonts: declarations is much faster.

  * Very many bugs have been fixed; see ZWEI;BUGS STATUS.

[D]  TV:SET-DEFAULT-FONT now reaches most of the things it ought to reach.
    It does not affect labels and menus, however, because those do not use
    the default font.  You can set the fonts used for them by doing
    (TV:SET-STANDARD-FONT ':LABEL font) or similar for ':MENU.
    Note that various pop-up menus, not normally active, are not found
    by SET-STANDARD-FONT, so their fonts will not change.  This may be fixed some day.

[E]  A new function DRIBBLE-ALL is like DRIBBLE-START but it
    puts all I/O into the dribble file, including breaks, errors
    and queries.  Some associated bugs have been fixed.


MIT System 83 comes with microcode 104.
[Microcode numbers have been wrapped around to avoid confusion]

    System 83 has sometimes been refered to as an MIT/LMI system.  This name comes
    about simply because Symbolics has chosen to split off from what was previously
    the MIT/LMI/Symbolics system.  The arrangements between MIT and LMI continue
    unchanged.  System 83 belongs to MIT, not to LMI.

  Table of contents:
  Things you should take note of:
   [0] Operational notes
   [1] File connections go away if unused for a period of time.
   [2] Flavors have been sped up considerably.
   [3] EQ hash tables have been improved.
   [4] &KEY and duplicate keywords.
   [5] Converse no longer appears by default.

  Things you may safely ignore:
   [A] New functions GETDECL, PUTDECL, DEFDECL.
   [B] DO* a sequential version of DO exists.
   [C] WITH-LOCK.
   [D] TYPECASE.
   [E] :NAMED-STRUCTURE is now allowed as a second argument for TYPEP.
   [F] PRESS:PRINT-FILE, PRESS:SPOOL-FILE and PRESS:PRINT-FROM-STREAM
   [G] Generic pathnames have been reimplemented.
   [H] Interlocking of garbage collector operations has been improved.
   [I] MAPHASH-EQUAL-RETURN
   [J] INTERSECTION and UNION.
   [K] Error level appears in error handler prompt.  EH:PDL-GROW-RATIO.
   [L] FQUERY:  :ANY as choice, and :STREAM option.
   [M] new ZWEI command SOURCE COMPARE CHANGES.
   [N] FILLARRAY creates array if first arg is NIL.

  Plus numerous random bugs are fixed, including many recently reported.

  Things that you should take note of:

[0] To be sure of running QFASL files compiled by system 83 in system 79,
    you must run system 79 with microcode 862, and have patch 79.43 loaded.

[1] File connections go away if unused for a period which is set by
FS:HOST-UNIT-LIFETIME in units of 60ths of a second.
The default is 20 minutes.

[2] Flavors have been sped up considerably, by using hash tables
for method lookup and not binding the instance variables as specials.
This new implementation of flavors is much more compatible that
the brand S implementation, though just as fast.  In particular,
DECLARE-FLAVOR-INSTANCE-VARIABLES still works, BOUNDP still works,
(FUNCALL #'(:METHOD FOO :BAR) ':BAR ...) still works (subject to the
same condition as before: SELF's flavor must incorporate FOO),
the interpreter still works, the error handler works as before.
But there are still a few things that will cease to work:
 [a] Using SET or SYMEVAL on an instance variable's name
     will no longer get the instance variable's value.
     You must use SET-IN-INSTANCE or SYMEVAL-IN-INSTANCE.
 [b] If you use LET, PROG etc. to rebind an instance variable
     within a method, with the idea of faking out subroutines
     the subroutines will no longer be affected.  The local variable
     you get this way will no longer have any relationship to the
     instance variable.  You might be able to win by using
     LET-GLOBALLY instead; or you can use (BIND (LOCF variable) value),
     but that only works compiled.  It is probably best to
     rewrite the code some other cleaner way.
 [c] If you call EVAL and wish to have the instances of SELF bound
     for the expression to be evaluated, put the macro
     (WITH-SELF-VARIABLES-BOUND ...) around the call to EVAL.
You can make some or all instance variables special as before by specifying
:SPECIAL-INSTANCE-VARIABLES in the DEFFLAVOR.  Then those constructs
will all continue to work; but the some or all the improvement in
speed will be lost.
 [d] Some weird wrappers that enclose the other methods in an
     internal LAMBDA and pass it to a functional need special hair
     to continue to work.  See the DEFWRAPPER for :EXPOSE in LMWIN;SHEET.
     We expect that no users have such wrappers; if you have any,
     tell BUG-LISPM.
System 83 with patches will run methods compiled in system 79,
but the speed advantage is lost for flavors with such methods loaded.
When loading a file compiled with 79 into 83, in certain rare cases it
may be necessary to do some manual operations.  Namely, if the file
contains DEFUN-METHOD or DECLARE-FLAVOR-INSTANCE-VARIABLES type functions
for a flavor for which it defines no methods, a 
(SI:MAKE-FLAVOR-ALL-SPECIAL <flavor-name>) will have to be done manually.

All flavor instances now accept the :BREAK operation.  This gets you
a break loop with all the instance variables bound.

Both System 79 and System 83 have DEFUN-METHOD and INSTANCE-VARIABLE-BOUNDP
for compatibility with the Symbolics system.

Redefining wrappers now automatically causes appropriate combined
methods to be recompiled.  You do not need to do anything special
when you redefine a wrapper to have it take effect.

[3] EQ hash tables have been improved.
EQ-HASH tables now contain a lock which is seized during critical
sections, so the user can reference the same hash table from more than
one process (beware of deadly embraces in the case of MAPHASH though).
You can now have hash tables which store several values per key.
In such hash tables, you get or set all the values at once.
GETHASH now returns three values.  The first two are as before:
the first value (or NIL), and T if the key was found.
The third is a list, shared with the hash table entry; its elements
are the key followed by all the associated values.
You can read the associated values out of the list, or you can RPLAC
the list and change the values.
The args to PUTHASH are somewhat ugly due to compatibility considerations.  
It is now (PUTHASH <key> <first value> <hash-table> &rest <remaining values>...).
MAPHASH passes the user supplied function as args the key followed by all the
associated values.  Hash tables with more than one value per key are created
using the :NUMBER-OF-VALUES keyword argument to MAKE-HASH-TABLE.

EQUAL hash tables do not have these new features.
The intention is that soon the same functions will be used for
both EQUAL and EQ hash tables.  This will automatically propagate the new
features to EQUAL hash tables.

[4] If a function using &KEY was supplied a keyword argument list containing
the same keyword more than once, it used to be that the last
instance of that keyword (and its associated argument) were the ones
that mattered.  Now, the first is used and the rest are ignored.
Hopefully few people will be affected by this, but it is something
to be aware of in case of weird bugs.

[5] Converse no longer appears on the screen by default.
Instead, you get a "simple reply window", in which you can
optionally type a reply or enter Converse.
Actually, you have several options, controlled by the variable
ZWEI:*CONVERSE-RECEIVE-MODE*.  If it is :AUTO, Converse appears.
If it is :NOTIFY or :NOTIFY-WITH-MESSAGE, you just get a notification
(with or without the first line of the message).  Type Terminal 0 S
then to enter Converse to send a reply.  If it is :SIMPLE or :POP-UP,
you get the simple reply window.

  Things that you may safely ignore:

[A] New functions GETDECL, PUTDECL, DEFDECL which are similar to 
GET, PUTPROP, and DEFPROP in normal execution or at load time.
**EXCEPT** the order of arguments in PUTDECL and DEFDECL is
symbol, property-name, and value, unlike PUTPROP but like the
new Common Lisp property functions (I think).
When compiling a file, PUTDECL and DEFDECL put entries on LOCAL-DECLARATIONS
and GETDECL checks LOCAL-DECLARATIONS first, properties second.
Use GETDECL instead of GET in macros and functions called by macros;
then you can use DEFDECL in a file which uses the macro
to provide information for the macro at compile time.
SETF uses GETDECL, so if you have a file in which you would like to
be able to SETF a function which the system does not know how to SETF,
you can include in that file a DEFDECL to specify a SETF property for it.

[B] DO* a sequential version of DO exists.  It's clear what it has to be by
analogy with LET* and PROG*, so it might as well exist.  Both the
binding and the stepping are done sequentially.

[C] (WITH-LOCK (<locator> . OPTIONS) &body body) exists.
OPTIONS is a keyword argument list.  Currently existing options are:
  :NORECURSIVE, if T, will cause an error if a process
recursively attempts to lock the lock when it already has it.   If :NORECURSIVE
is NIL, the default, a count of recursive locks will be kept and the lock released only
when freed by the highest level.
  :NOERROR, if T, inhibits the usual error check for a process which doesnt
have the lock attempting to unlock it.  :NOERROR is NIL by default.

[D] A TYPECASE macro is now available.  Example:
(TYPECASE OBJECT
  (:SYMBOL (GET-PNAME OBJECT))
  (:LIST (CAR OBJECT))
  ((:FIX :FLOAT) (+ OBJECT 1))
  (TV:WINDOW (FUNCALL OBJECT ':EXPOSE))
  (T (FERROR NIL "You lose")))

Anything that is a suitable second argument for TYPEP can be used
to start a clause in TYPECASE.  A list of such type names is also ok.
T or OTHERWISE starts a clause to handle whatever has not been handled yet.

[E] :NAMED-STRUCTURE is now allowed as a second argument for TYPEP.

[F] PRESS:PRINT-FILE, PRESS:SPOOL-FILE and PRESS:PRINT-FROM-STREAM
now allow two new options: :FONT-LIST and :HEADER-FONT.
:HEADER-FONT specifies the font to be used for page headings if
such are being printed.  :FONT-LIST takes a list of font names
and specifies printing of a multiple-font file in those fonts.
The character sequence ^Fn is used in the file to switch to font n,
where n is a digit.  If you put multiple fonts in a file using ZWEI
and store it, this is the format it is stored in.

These functions now use &KEY, so you can see from their arglists
what options they allow.

[G] Generic pathnames have been reimplemented.  See discussion near the beginning
of LMIO;PATHNM for details.  There should be fewer screws than before.

[H] Interlocking of garbage collector operations has been improved.

[I] There is a new function called MAPHASH-EQUAL-RETURN which takes the arguments
FUNCTION and HASH-TABLE and optional arguments of 'LIST or 'NCONC.  It returns
either a list or the NCONC of values returned by FUNCTION.  The default is 'LIST.

[J] UNION and INTERSECTION now copy their first arguments.
This eliminates a bug whereby they clobbered those arguments.
When you wish to avoid the cost of copying them, you can
use the new functions NUNION and NINTERSECTION.

[K] If you get an error in an expression you evaluate in the
error handler, the prompt in the recursive error's error handler
includes a level number, as in "Error level 2->".
When you get a stack overflow error and continue,
the stack size is increased by the ratio EH:PDL-GROW-RATIO,
which is initially 1.5s0.

[L] FQUERY now allows :ANY as a choice.  You can type anything at all.
if you type something recognized by one of the specific choices,
FQUERY acts as before.  If you type anything not recognized, the character
or string you typed is returned.
In addition, there is a new option :STREAM.  The associated value
should be either an I/O stream or a symbol or list which will evaluate
to an I/O stream.  The stream is used instead of QUERY-IO, and QUERY-IO
is not rebound.

[M]  New ZWEI command SOURCE COMPARE CHANGES (for now accessible only via
cntrl-meta-x).   Source Compares a buffer versus its associated file.
If there are no differences, the modified bit of the buffer is cleared.

[N]  If the first argument to FILLARRAY, (normally the array to be filled),
is NIL, FILLARRAY creates an ART-Q array the length of the fill data and
proceeds to fill it.
