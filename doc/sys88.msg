Some significant features have been patched into system 87 since it was announced.
(That announcement did not go to INFO-LISPM since that mailing list had not
been moved to OZ yet; you can find it on OZ:SRC:<L.DOC>SYS87.MSG)

0) The system functions are documented!

Hundreds of documentation strings have been added to the functions in
the system.  Just about every function you might ever think of calling
now has a documentation string, which you can view using the editor
commands C-Shift-D and M-Shift-D.

C-Shift-D prints just the first line of documentation for the function
you are inside a call to; this is like C-Shift-A.

M-Shift-D prints the full documentation for a function, reading its name
(defaulting to the same one C-Shift-D would give you).

ZWEI, ZMAIL, the demos, and the CADR package have yet to be gone over.

1) Complex numbers are supported.

A complex number can have any kind of number as its real part,
and any kind as its imaginary part.

(REALP x) is T if x is real.
(COMPLEXP x) is T if x is not real.

(COMPLEX r i) creates a complex number.

(IMAGPART c) returns the imaginary part of the number c,
 which is zero if c is real.

(REALPART c) returns the real part of the number c,
 which is c if c is real.

(CONJUGATE c) returns the conjugate of the number c,
 which is c if c is real.

(SIGNUM c) returns a number with magnitude 1 and the same phase as c.
 This is 1 for positive reals, -1 for negative reals.
 0 is an exception; (SIGNUM 0) is 0.

(PHASE c) returns the phase of the complex number c.
 It is 0 for positive reals.

(CIS x) returns exp(i*x) where x is real.

Three new array types are defined, ART-COMPLEX, ART-COMPLEX-FLOAT and
ART-COMPLEX-FPS-FLOAT.  Using them reduces garbage collection if you
have an array containing only numbers, some of which are complex.
ART-COMPLEX allows the real and imaginary parts to be anything, while
ART-COMPLEX-FLOAT converts them to flonums when you store into the
array.

ART-COMPLEX-FPS-FLOAT is like ART-COMPLEX-FLOAT, but uses the FPS format
for storing each flonum.  Real and imaginary parts alternate in memory.

3) Rational numbers improved:

The support for rational numbers is now in closer to its final
Common Lisp form.

(RATIONALP x) is T if the number X is rational.
Note that integers are considered rationals.

(INTEGERP x) is T if x is an integer.  This is the same as FIXP.

(NUMERATOR x) returns the numerator of the rational numebr x,
which is x if x is an integer.

(DENOMINATOR x) returns the denominator of the rational number x,
which is 1 if x is an integer.

You get a rational number by calling RATIONAL or RATIONALIZE.
These convert floating point numbers to rationals.
(An integer is already a rational).
RATIONAL treats the flonum as an exact mathematical value,
whereas RATIONALIZE returns the simplest fraction which would
convert to the specified flonum.

RATIONALIZE takes a second argument which is the number of bits of
precision desired, or the ratio of the allowed error to the number itself.
It will produce a fraction that accounts for the specified precision
of the argument.  This is useful for arguments that are rational!
It will return another fraction with smaller numerator and denominator
that is close to the argument.

The new function %DIV, with integer arguments, will produce a rational value.
Eventually // will behave like %DIV.

2) New Common Lisp division functions:

(FLOOR x) returns the largest integer not greater than x.
(CEILING x) returns the smallest integer not less than x.
(TRUNCATE x) is (FLOOR x) if x is positive, (CEILING x) if x is negative.
(ROUND x) returns the closest integer to x.
 If x is midway between two integers, the even one is returned.

All four functions return a second value which is the remainder:
 the argument minus the value.

When given two arguments, these functions divide the first by the second
and convert the quotient to an integer.  The second value is 
the original dividend, minus the quotient times the divisor.

TRUNCATE is the same as the current meaning of //, when operating on fixnums.

3) Division of fixnums will change its meaning:

At some time in the future, ordinary division, the function //, will
behave differently on integer arguments: it will return a rational number
which is the mathematically correct ratio between the arguments.
This is part of the adoption of Common Lisp (and therefore, the same
Change will happen at some time in Symbolics systems as well).

If you have code which depends on dividing integers and getting an
integer as the value, you should convert your code now to use TRUNC
or one of the other integer division functions.

Meanwhile, the eventual meaning of // is available now under the name %DIV.

The change in // is probably a few months in the future, but it is not too
soon to prepare for the change.  When // changes, %DIV and TRUNC will not change.

4) (LISTP NIL) will return T, some day.

This change is another part of adopting Common Lisp.

The new function CONSP is currently the same as LISTP, but when
LISTP is changed, CONSP will not be changed.  You should begin converting
programs now to use CONSP instead of LISTP when appropriate.

5) Arrays will be stored in row-major order.

At some time in the future, the order in which array elements are stored
internally will change.  This is also part of adopting Common Lisp.

Most programs that use arrays will not be affected by the change.
However, if you use indirect arrays which are multidimensional, or which
indirect to multidimensional arrays, you will notice the effects.

In addition, the order of pixels in the hardware screen buffer will not
be changed.  So all array references to such arrays -- and to arrays
that are BITBLT'ed into them -- must have their indices in the other order.

To smooth the conversion, two new functions AR-2-REVERSE and AS-2-REVERSE
have been created.  These are currently the same as AR-2 and AS-2.
But when the order of array elements is changed, these two new functions
will begin to interchange the two subscripts.  Therefore, if AR-2-REVERSE
is used to access a screen array or bit map, it will work both before
and after the change is made.

In addition, you can use MAKE-PIXEL-ARRAY to create a bit map.  Its
arguments are like MAKE-ARRAY's except that instead of one list of
dimensions you give a width and a height, separately, in that order.
MAKE-PIXEL-ARRAY will use the width as either the first dimension or the
second, whichever is correct.

PIXEL-ARRAY-WIDTH returns the width of a bit array, and
PIXEL-ARRAY-HEIGHT returns its height.

6) DIRED is now a File System Editor

DIRED now has several new and changed commands, giving it the full power
of the Symbolics File System Editor.

First of all, click right now gets a menu of operations on the file
the mouse was pointing to.  All the usual operations are available this way.

One new command is S, for Subdirectory.  If you are pointing to a line
which describes a subdirectory, this command inserts a listing of that
subdirectory into the DIRED buffer underneath the subdirectory itself.
Thus, you can operate on multiple levels of directory at once.
Using S again removes the subdirectory's files from the display.
Indentation is used to show you which files are in subdirectories.

The X command in DIRED is no longer the same as Meta-X.
Instead, it is like Q but leaves you in DIRED.
That is, it queries about the files to be processed and processes them
but does not exit.

Meanwhile, processing the files with X or Q now eliminates all F, P and A
(Find File, Print or Apply Function) markings found on files, so you get a
clean slate to request a new set of operations.  D markings are left on
the deleted files, since they are now deleted; but if you expunge, or
if the file system does not support undeletion, the deleted files are
removed from the Dired buffer entirely.

Sorting is now done using M-X commands.
The sorting commands are M-X Sort Increasing Creation Date,
M-X Sort Increasing Reference Date, M-X Sort Increasing Size,
and M-X Sort Increasing File Name.

There are also four other commands with Decreasing instead of Increasing.

Please request any file operations that you do not see in Help in DIRED.

7) Unlimited Undo capability in the editor

The editor now saves all the changes you make to each buffer,
and you can undo them as far back as you like by doing M-X Undo
multiple times.  To undo the undoing, use M-X Redo.  These commands
also show you a summary of the part of the buffer they are going to change
and what it will be changed to.

Since these commands may be useful more often, they are now on
the keys C-Shift-U and C-Shift-R. 

If you set the variable ZWEI:*UNDO-SAVE-SMALL-CHANGES* to non-NIL,
absolutely all editing operations will be saved.  Even individual insertions
and deletions will be saved, but they will be batched together.
This will eventually become the default after it has been tested
for a while.

The undo memory for a buffer is cleared out when you read or write a file
with that buffer.

8) Generic file operations.

The functions DELETEF, UNDELETEF and RENAMEF now accept filenames with
wildcards in them, and operate on multiple files.  They also optionally
query about the files.  They return a list of the files operated on.
One unfortunate byproduct of this change is that they are slower in the
ordinary case of a single file with no querying.  If this is important,
you can instead send a :DELETE, :UNDELETE or :RENAME message to a
pathname object.  This lower-level interface does not provide the new
features but is still fast.

The new function FS:COPY-FILE copies a file.  It takes two filenames
(strings or pathnames).  The first argument may be a string instead.
There are two additional arguments: ERROR-P (default T), and COPY-MODE
which says whether to copy as a text file or a binary file.
Possible values are :CHARACTERS, :BINARY, :ASK (always query the user),
:NEVER-ASK (just guess), or NIL (try to guess, but query the user
if the guess is not reasonably certain).  FS:COPY-FILE also works on pathnames
with wildcards.

The new pathname operation :WILD-P returns T if the pathname it is sent
to contains any sort of wildcards.

The new pathname operation :WILDCARD-MAP is a mapping operation that loops
over all the files that a pathname with wildcards refers to.  It does so
by doing a :DIRECTORY-LIST first.  It also works on pathnames that don't
have wildcards.  Supply this operation with these arguments:
  OPERATION PLISTP DIR-LIST-OPTIONS &REST ARGS...
OPERATION is a function to be called on each file.
 It will receive a pathname or a property list as its first argument,
 and ARGS as remaining arguments.
PLISTP says whether OPERATION should be given a file property list.
 (This takes extra time when a pathname with no wildcards is used).
 If it is NIL, OPERATION's first arg is just a pathname.
 If it is T, OPERATION's first arg is a property list whose car
 is a pathname and whose cdr is alternating properties and values.
 This is just an element of the directory-list which is obtained.
DIR-LIST-OPTIONS is passed to DIRECTORY-LIST.  You can use this
 to get deleted files mentioned in the list, for example.

The new pathname operation :UNDELETEABLE-P returns T if sent to a pathname
for a file system which supports undeletion.

The new pathname operation :SOURCE-PATHNAME returns the pathname of the
source file corresponding to the pathname you sent the message to.  If you
evaluate, or compile and load, a file whose type is not LISP, then this
will get the correct type for the file you loaded or compiled.

Unix files whose names happen to contain ".LISP" or anything else which
is the same as a standard Lisp machine file type no longer cause problems.
In fact, all the long-standing problems with Unix pathnames should now be gone.

MERGE-PATHNAME-DEFAULTS, with a pathname given as the defaults, now uses
that pathname's components as the defaults.  (This may be what you thought
it already did -- it is what I thought it did).

9) QSEND, REPLY, and MAIL.

The function QSEND now accepts multiple recipient names.
The names go in one string, separated by commas, as in
(QSEND "RMS@OZ, FOO@OZ" "Message")

The function REPLY sends a message to the person who last sent you one.
It normally takes one argument, the message.

The function MAIL, when given two arguments (recipient and message),
no longer enters the editor.  It sends the string as you gave it.

These features were added by HDT.

10) Network O in SUPDUP and TELNET toggles overprinting.

Normally, SUPDUP and TELNET overprint if the remote system sends
two characters to be output on the same spot without erasing.
If you are talking to a system that assumes your terminal cannot
overprint, this may look ugly.  Network O causes SUPDUP or TELNET
to erase character positions when they are about to be written in,
so that your screen will look right.

)

(TV:SHEET-CLEAR-STRING sheet string &optional start end)

clears enough space after SHEET's cursor to hold STRING (or the specified portion).

TV:SET-TV-SPEED now takes a second optional argument which specifies
how many scan lines at the bottom of the screen should be unused.
This is useful if it is hard to see some of the bottom of the screen
because the monitor is incorrectly adjusted.

)
Two new transformations are defined for DEFSYSTEM, for use with programs
that run in Maclisp as well as on the Lisp machine.

They are :COMPAT-COMPILE, :COMPAT-COMPILE-LOAD, and :COMPAT-COMPILE-LOAD-INIT.

They are the same as the transformations :COMPILE, :COMPILE-LOAD and
:COMPILE-LOAD-INIT except that source files are called "LSP" instead
of "LISP".

Meanwhile, patches record the original source file of the contents of the
patch, so that if the patch defines any new functions, the editor will
find their definitions in their actual source files rather than in the patch file.

)

Compile Buffer Changed Sections, etc., now can always tell when new
functions have been inserted in the buffer.  When it says that it is
compiling function FOO, you can trust that this means precisely function
FOO and no other.

It is still the case that inserting a function causes the following
function to be marked as changed.  So Compile Buffer Changed Sections
will tell you it is recompiling the following function as well as
compiling the inserted function.  This is not easy to fix.

)

M-X Kill or Save Buffers allows you to kill and save the same file.

) New Convenient Repetition Construct, #`

3#`* is a construct for repeating an expression with some
subexpressions varying.  It is an abbreviation for writing several
similar expressions, or for the use of 3mapc*.  Each subexpression
that is to be varied is written as a comma followed by a list of the
things to substitute.  The expression is expanded at read time into
a 3progn* containing the individual versions.

   #`(send stream ',(:clear-input :clear-output))

expands into

   (progn (send stream ':clear-input) (send stream ':clear-output))

Multiple repetitions can be done in parallel by using commas
in several subexpressions:

   #`(renamef ,("foo" "bar") ,("ofoo" "obar"))

expands into

   (progn (renamef "foo" "ofoo")
          (renamef "bar" "obar"))

If you want to do multiple independent repetitions, you must use nested
3#`* constructs.  Individual commas inside the inner 3#`* apply to
that 3#`*; they vary at maximum speed.  To specify a subexpression
that varies in the outer 3#`*, use two commas.

   #`#`(print (* ,(5 7) ,,(11. 13.)))

expands into

   (progn (progn (print (* 5 11.)) (print (* 7 11.)))
          (progn (print (* 5 13.)) (print (* 7 13.)))

*) When you are notified that a deexposed window is waiting to type out,
you can type Terminal Resume to permit it to typeout while remaining deexposed.

*) In the local file system, subdirectories are now supported.
Subdirectories appear in a pathname separated by periods, before the semicolon:
"LM: RMS.SUBDIR.LEVEL2; FOO.BAR".

Also, Twenex-style pathname syntax is now allowed for the local file
system as an alternative to the old syntax.  Simply use whichever syntax
you prefer.

If the flag FS:*LMFS-USE-TWENEX-SYNTAX* is set to non-NIL, pathnames
for local files will print out in Tops-20 syntax, rather than the
normal ITS-like syntax.  It is always possible to type them in using
either syntax.

Note that changing the flag's value will not change the printed
representation of an existing pathname object, since it is cached.
Setting this flag in your login init file will eliminate most such
confusion.

*) File Already Exists error handled better.

If you compile a file on a Twenex that has already been compiled, Twenex
will give a "File already exists" error because a QFASL file of the same
version was created by the previous compilation.

The Lisp machine will now offer to delete the old QFASL file for you.
You can type Y, N or P.  If you type N, you get the usual "use what
filename instead" offer.  P is the same as Y for this question and all
such questions in the future.  It sets the following variable to T.

*AUTO-DELETE-EXISTING-FILE*
 If non-nil, an attempt to write a file with a specific version number
that already exists will try to delete the old file immediately.
Initially nil.

Note: if someone is reading the file, Twenex will not allow it to be
deleted or renamed.  There is absolutely no way on Twenex to implement
what is supposed to be the default mode of opening an output file: if a
file with the specified name and version already exists, to
replace it when the new file is closed.  Doesn't Twenex suck?

) :UNBOUND in DEFVAR.

You can now define a variable with DEFVAR and give it a documentation
string without specifying an initial value.  Just use :UNBOUND as the
initial value.  DEFVAR will make no attempt to evaluate :UNBOUND; it
will simply not try to set the variable.

Now there is no excuse whatever for writing a DEFVAR without a
documentation string.

) Accessing Editor Buffers as Files

Three new host names, ED:, ED-BUFFER:, and ED-FILE: are available for opening
editor buffers as if they were files.  ED: uses the completing reader to recognise
abbreviated buffer names.  ED-BUFFER: requires the exact name.  ED-FILE: takes a
file name and looks for a buffer by that name, if necessary creating a new buffer
visiting the file.  --Devon

*) Editing multi-font files

* is now understood in multi-font files being read into ZWEI.

When multi-font files are saved, * is used when appropriate to say that
the font has changed back to one that was in use before.

As a result, if you use a text justifier such as R or BOLIO in which 
specifies a font change, you can now have your files displayed in your
favorite "equivalent" Lisp machine fonts when you edit it.

*) ZMAIL terminology changes

There has been a change in terminology in ZMAIL.  What used to be called
a "mail file" is now called a "buffer".  "Temporary mail files" are now
"temporary buffers", or "subset buffers" if they are made with a filter
(yielding the set of all messages in a universe that satisfy the filter).
"Disk mail files" are now "mail file buffers".  The term "mail file" is
still used, but now always refers to an actual file rather than an
entity within ZMAIL.

*) New hardcopy functions:

HARDCOPY-FILE
HARDCOPY-STREAM
HARDCOPY-STATUS
HARDCOPY-BIT-ARRAY.

Site options :DEFAULT-PRINTER and :DEFAULT-BIT-ARRAY-PRINTER.

*) Overriding Site Options on a Per-Machine Basis

The file SYS: SITE; LMLOCS LISP, which describes the location of each
Lisp machine at your site, can now contain a list of site options which
should be overridden for that particular machine.

The entry for a machine can now have six elements.  The sixth element is
a list of site option specifications, each of the form

   (site-option-name expression)

This is just like what would go in the SITE file for that option.

For example, it might be useful to specify the :DEFAULT-PRINTER option
separately for individual machines, so each machine can print on a
printer near it.

The function GET-SITE-OPTION checks the per-machine specifications
before the site definition, so no other code should need to be changed.

A new initialization list, SI:SITE-OPTION-INITIALIZATION-LIST, is run at
an appropriate time when new site files are loaded and also after
booting.  It is useful for reinitializing anything that depends on a
site option.

*) Recompiling Site Files

There is now an easy way to recompile the site files, including the host
table.  Just do (MAKE-SYSTEM 'SITE 'COMPILE).

*) EQL

The function EQL is the same as EQ except on numbers, where it does =.

*) Directory Maintenance

(FS:BALANCE-DIRECTORIES dirspec1 dirspec2 &rest options)

Generates directory lists using the dirspec arguments, then analyzes
the lists to determine which files appear in one directory and not the
other; any files that aren't up to date in either directory are
copied.  The result is that each directory contains up to date copies
of every file in either directory.

A number of options are available for controlling the actions of the
program.  Note that dirspec may be anything that is acceptable to
FS:DIRECTORY-LIST, so it is possible to balance selected parts of a
directory.
