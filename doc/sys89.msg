-*-Fonts:TR12,cptfont,TR12I,CPTFONT,TR12B; Mode:Text-*-

System 89 comes with microcode 201.  It was first released with microcode 183,
but certain new features work only with 201.

Microcode 183 will work with system 87, and system 87 bands should be updated
to use 183, as it fixes some bugs.

There are no major incompatibilities between systems 87 and 89.

Programming Changes:

[0] The System Functions Are Documented!

Hundreds of documentation strings have been added to the functions in
the system.  Just about every function you might ever think of calling
now has a documentation string, which you can view using the editor
commands 1C-Shift-D* and 1M-Shift-D*.

1C-Shift-D* prints just the first line of documentation for the function
you are inside a call to; this is like 1C-Shift-A*.

1M-Shift-D* prints the full documentation for a function, reading its name
(defaulting to the same one 1C-Shift-D* would give you).

3C-Shift-D* is available in the rubout handler as well as the editor; there it prints
the complete documentation.

ZWEI, ZMAIL, the demos, and the CADR package have yet to be gone over.

Major Incompatibilities in the Not Quite Immediate Future

		[1] Division of Fixnums Will Change Its Meaning:
		[2] New Common Lisp Division Functions:
		[3] 1(LISTP NIL)* will return 1T*, some day.
		[4] Arrays Will Be Stored in Row-Major Order.

Common Lisp will require some incompatible changes to the Lisp machine
system.  None of these changes is happening yet.  But some new functions are
being added now to smooth the way.

[1] Division of Fixnums Will Change Its Meaning:

At some time in the future, ordinary division, the function 1//*, will
behave differently on integer arguments: it will return a rational number
which is the mathematically correct ratio between the arguments.
This is part of the adoption of Common Lisp (and therefore, the same
change will happen at some time in Symbolics systems as well).

If you have code which depends on dividing integers and getting an
integer as the value, you should convert your code now to use 1TRUNCATE*
or one of the other integer division functions.

Meanwhile, the eventual meaning of 1//* is available now under the name 1%DIV*.

The change in 1//* is probably several months in the future, but it is not too
soon to prepare for the change.  When 1//* changes, 1%DIV* and 1TRUNCATE* will not change.

[2] New Common Lisp Division Functions:

1(FLOOR* 2x1)** returns the largest integer not greater than 2x*.
1(CEILING* 2x1)** returns the smallest integer not less than 2x*.
1(TRUNCATE* 2x1)** is 1(FLOOR* 2x1)** if 2x* is positive, 1(CEILING* 2x1)** if 2x* is negative.
1(ROUND* 2x1)** returns the closest integer to 2x*.
 If 2x* is midway between two integers, the even one is returned.

All four functions return a second value which is the remainder:
 the argument minus the value.

When given two arguments, these functions divide the first by the second
and convert the quotient to an integer.  The second value is 
the original dividend, minus the quotient times the divisor.

1TRUNCATE* is the same as the current meaning of 1//*, when operating on fixnums.

[3] 1(LISTP NIL)* will return 1T*, some day.

This change is another part of adopting Common Lisp.

The new function 1CONSP* is currently the same as 1LISTP*, but when
1LISTP* is changed, 1CONSP* will not be changed.  You should begin converting
programs now to use 1CONSP* instead of 1LISTP* when appropriate.

[4] Arrays Will Be Stored in Row-Major Order.

At some time in the future, the order in which array elements are stored
internally will change.  This is also part of adopting Common Lisp.

Most programs that use arrays will not be affected by the change.  However, if
you use indirect arrays which are multidimensional, or which indirect to
multidimensional arrays, you will notice the effects.  Programs which move
sequentially through large arrays will continue to work, but the best subscript
order for optimal paging behavior will be the opposite of what it is now.

In addition, the order of pixels in the hardware screen buffer will not
be changed.  So all array references to such arrays -- and to arrays
that are 1BITBLT*'ed into them -- must have their indices in the other order.

To smooth the conversion, two new functions 1AR-2-REVERSE* and 1AS-2-REVERSE*
have been created.  These are accessing functions for two-dimensional arrays,
and are currently the same as 1AREF* and 1ASET* on them.  But when the order of
array elements is changed, these two new functions will begin to interchange the
two subscripts.

The significant fact is that if you replace 1AREF* and 1ASET* with 1AR-2-REVERSE* and
1AS-2-REVERSE* where you access screen arrays, your code will continue to work
properly now, and will also keep working properly when the system is changed.
You will not even have to recompile then.

To create a bit map array, use 1MAKE-PIXEL-ARRAY*.

1(MAKE-PIXEL-ARRAY 2width height *&REST 2options)**

This is currently the same as 1(MAKE-ARRAY (LIST 2width height*) ...)*
but when the order of array dimensions changes, it will do
1(MAKE-ARRAY (LIST 2height* 2width*) ...)* instead.  In other words,
1MAKE-PIXEL-ARRAY* will use the width as either the first dimension or the second,
whichever is correct.

1PIXEL-ARRAY-WIDTH* returns the width of a bit array, and 1PIXEL-ARRAY-HEIGHT*
returns its height.  Again, the width is whichever dimension varies faster
through memory.

Programming Changes:

		   [5] Complex Numbers Are Supported.
		   [6] Rational numbers improved:
		   [7] Wildcard File Operations.
		   [8] New Operations on Pathname Objects
		   [9] New Erasing Operation on Windows.
		   [10] 1DEFSYSTEM* Change.
		   [11] 1:UNBOUND* in 1DEFVAR*.
		   [12] 1CONTENTS* Gets the Contents of a Locative.
		   [13] 1EQL* is 1=* on numbers, 1EQ* on other objects.
	           [14] Using 1DECLARE*s Locally.
	           [15] New Function 1COMPILE-LAMBDA*.
	           [16] New Initialization Lists.
		   [17] You Can Tell If a Macro Definition Has Changed.
                   [18] Making Flavor Instances Print Readably.

[5] Complex Numbers Are Supported.

A complex number can have any kind of number as its real part,
and any kind as its imaginary part.

1(REALP* 2x1)** is 1T* if 2x* is real.
1(COMPLEXP* 2x1)** is 1T* if 2x* is not real.

1(COMPLEX 2r i*)* creates a complex number with real part 2r* and imaginary part 2i*.

1(IMAGPART2 c*)* returns the imaginary part of the number 1c*,
 which is zero if 1c* is real.

1(REALPART* 2c1)** returns the real part of the number 2c*,
 which is 2c* if 2c* is real.

1(CONJUGATE* 2c1)** returns the conjugate of the number 2c*,
 which is 2c* if 2c* is real.

1(SIGNUM* 2c1)** returns a number with magnitude 11* and the same phase as 2c*.
 This is 11* for positive reals, 1-1* for negative reals.
 10* is an exception; 1(SIGNUM 0)* is 10*.

1(PHASE* 2c1)** returns the phase of the complex number 1c*.
 It is 10* for positive reals.

1(CIS* 2x1)** returns exp(i*2x*) where 2x* is real.

Three new array types are defined: 1ART-COMPLEX*, 1ART-COMPLEX-FLOAT* and
1ART-COMPLEX-FPS-FLOAT*.  Using them reduces garbage collection if you
have an array containing only numbers, some of which are complex.
1ART-COMPLEX* allows the real and imaginary parts to be anything, while
1ART-COMPLEX-FLOAT* converts them to flonums when you store into the
array.

1ART-COMPLEX-FPS-FLOAT* is like 1ART-COMPLEX-FLOAT*, but uses the FPS format
for storing each flonum.  Real and imaginary parts alternate in memory.

[6] Rational numbers improved:

The support for rational numbers is now in closer to its final
Common Lisp form.

You can type in a rational number as 2numerator* 1\* 2denominator*.
Don't actually put in spaces around the 1\*; they appear in the previous line for
visual clarity.  Rational numbers print out that way, too.

1(RATIONALP* 2x1)** is 1T* if the number 2x* is rational.
Note that integers are considered rationals.

1(INTEGERP* 2x1)** is 1T* if 2x* is an integer.  This is the same as 1FIXP*.

1(NUMERATOR* 2x1)** returns the numerator of the rational number 2x*,
 which is 2x* if 2x* is an integer.

1(DENOMINATOR* 2x1)** returns the denominator of the rational number 2x*,
 which is 11* if 2x* is an integer.

You get a rational number by calling 1RATIONAL* or1 RATIONALIZE*.
These convert floating point numbers to rationals.
(An integer is already a rational).

1RATIONAL* treats the flonum as an exact mathematical value,
whereas 1RATIONALIZE* returns the simplest fraction which would
convert to the specified flonum.

1RATIONALIZE* takes a second argument which is the number of bits of
precision desired, or the ratio of the allowed error to the number itself.
It will produce a fraction that accounts for the specified precision
of the argument.  This is useful for arguments that are rational!
It will return another fraction with smaller numerator and denominator
that is close to the argument.

The new function 1%DIV*, with integer arguments, will produce a rational value.
Eventually 1//* will behave like 1%DIV*.

[7] Wildcard File Operations.

The functions 1DELETEF*, 1UNDELETEF* and 1RENAMEF* now accept filenames with
wildcards in them, and operate on multiple files.  They also optionally
query about the files.  They return a list of the files operated on.
One unfortunate byproduct of this change is that they are slower in the
ordinary case of a single file with no querying.  If this is important,
you can instead send a 1:DELETE*, 1:UNDELETE* or 1:RENAME* message to a
pathname object.  This lower-level interface does not provide the new
features but is still fast.

The new function 1FS:COPY-FILE* copies a file.  It takes two filenames
(strings or pathnames).  The first argument may be a string instead.
There are two additional arguments: 1ERROR-P* (default 1T*), and 1COPY-MODE*
which says whether to copy as a text file or a binary file.
Possible values are 1:CHARACTERS*, 1:BINARY*, 1:ASK* (always query the user),
1:NEVER-ASK* (just guess), or 1NIL* (try to guess, but query the user
if the guess is not reasonably certain).  1FS:COPY-FILE* also works on pathnames
with wildcards. - Hanson.

[8] New Operations on Pathname Objects

The new pathname operation 1:WILD-P* returns 1T* if the pathname it is sent
to contains any sort of wildcards.

The new pathname operation 1:WILDCARD-MAP* is a mapping operation that loops
over all the files that a pathname with wildcards refers to.  It does so
by doing a 1:DIRECTORY-LIST* first.  It also works on pathnames that don't
have wildcards.  Supply this operation with these arguments:
  2operation* 2plistp* 2dir-list-options* 1&REST* 2args*...
2operation* is a function to be called on each file.
 It will receive a pathname or a property list as its first argument,
 and 2args* as remaining arguments.
2plistp* says whether 2operation* should be given a file property list.
 (This takes extra time when a pathname with no wildcards is used).
 If it is 1NIL*, 2operation*'s first arg is just a pathname.
 If it is 1T*, 2operation*'s first arg is a property list whose car
 is a pathname and whose cdr is alternating properties and values.
 This is just an element of the directory-list which is obtained.
2dir-list-options* is passed to 1FS:DIRECTORY-LIST*.  You can use this
 to get deleted files mentioned in the list, for example.

The new pathname operation 1:UNDELETABLE-P* returns 1T* if sent to a pathname
for a file system which supports undeletion.

The new pathname operation 1:SOURCE-PATHNAME* returns the pathname of the
source file corresponding to the pathname you sent the message to.  If you
evaluate, or compile and load, a file whose type is not 1LISP*, then this
will get the correct type for the file you loaded or compiled.

Unix files whose names happen to contain ".1LISP*" or anything else which
is the same as a standard Lisp machine file type no longer cause problems.
In fact, all the long-standing problems with Unix pathnames should now be gone.

1FS:MERGE-PATHNAME-DEFAULTS*, with a pathname given as the defaults, now uses
that pathname's components as the defaults.  (This may be what you thought
it already did -- it is what I thought it did).

[9] New Erasing Operation on Windows.

1(TV:SHEET-CLEAR-STRING* 2sheet* 2string* 1&OPTIONAL* 2start* 2end*)

clears enough space after 2sheet*'s cursor to hold 2string* (or the specified portion).

[10] 1DEFSYSTEM* Change.

Two new transformations are defined for 1DEFSYSTEM*, for use with programs
that run in Maclisp as well as on the Lisp machine.

They are 1:COMPAT-COMPILE*, 1:COMPAT-COMPILE-LOAD*, and 1:COMPAT-COMPILE-LOAD-INIT*.

They are the same as the transformations 1:COMPILE*, 1:COMPILE-LOAD* and
1:COMPILE-LOAD-INIT* except that source files are called 1LSP* instead
of 1LISP*.

[11] 1:UNBOUND* in 1DEFVAR*.

You can now define a variable with 1DEFVAR* and give it a documentation
string without specifying an initial value.  Just use 1:UNBOUND* as the
initial value.  1DEFVAR* will make no attempt to evaluate 1:UNBOUND*; it
will simply not try to set the variable.

Now there is no excuse whatever for writing a 1DEFVAR* without a
documentation string.

[12] 1CONTENTS* Gets the Contents of a Locative.

Given a locative, you can now use the function 1CONTENTS* to get the contents of
the cell to which the locative points.

1CONTENTS* is actually the same as 1CDR*, but use of 1CDR* has been called a violation
of abstraction.

1(SETF (CONTENTS 2locative*) 2value*)* works also.

[13] New Function 1EQL

EQL* is 1=* on numbers, 1EQ* on other objects.

1(EQL 1 T) => NIL
(EQL 1 1.0) => T
(EQL 'FOO 'FOO) => T*

[14] Using 3DECLARE*s Locally.

3DECLARE*s can now appear at the front of the body of a 3PROGN*, 3LET*, 3LET**, 3PROG*,
3PROG** or internal 3LAMBDA*, and apply to the entire construct, including the local
variables (if any) bound by that construct.  Example:

3  (let (x y)
    (declare (special x))
    ...  ;X* is special herein
3    )*

It now works to define a macro that expands3,* sometimes or always, into a
1DECLARE3,** and use that macro at the front of a function body to provide a local
declaration.

For example,

1(DEFMACRO DS (&REST VARS) `(DECLARE (SPECIAL . ,VARS)))

(DEFUN FOO (A B)
  (DS A B)
  (BAR))*

will make 1A* and 1B* special in 1FOO*.

[15] New Function 1COMPILE-LAMBDA*.

1(COMPILE-LAMBDA 2lambda-expression* &OPTIONAL 2name* 2processing-mode*)*

compiles 2lambda-expression* into a compiled function object which remembers
2name* as its name, and returns it.  No function spec is defined; it is up to the
caller to store the compiled function object somewhere.

If 2processing-mode* is 1COMPILER:MICRO-COMPILE*, the function is microcompiled
instead.

[16] New Initialization Lists.

1SI:FULL-GC-INITIALIZATION-LIST* is run at the end of the function 1SI:FULL-GC*.
It is specified by the 1ADD-INITIALIZATION* keyword 1FULL-GC* (in any package).

1SI:SITE-OPTION-INITIALIZATION-LIST* is run whenever new site tables are
loaded, and also on cold booting.  It is specified by the 1ADD-INITIALIZATION*
keyword 1SITE-OPTION* (in any package).  Initializations put on this list are by
default also executed immediately.

[17] You Can Tell If a Macro Definition Has Changed.

The compiler now records the 1SXHASH* of every macro expanded in each function
you compile.  Compiled macros know the 1SXHASH*es of their original expr
definitions so this can be done for them as well.

The records go under 1:MACROS-EXPANDED* in the function's debugging info alist.
The value associated with this keyword is now a list of either macro names or
lists of the form 1(2macroname** 2macro-sxhash1)**.  (Just a macro name is recorded for a
macro which was compiled in earlier systems and does not know the original
1SXHASH*).

You can get the appropriate 1SXHASH* for the current definition of a macro with 
1(COMPILER:EXPR-SXHASH 2macroname*).*  By comparing the two, you can tell
whether you must recompile the other function because of a change in the
macro.

If a compiled function is fasloaded and can be seen to have been compiled with
a version of a macro which does not match the current definition, a warning is
printed, giving the name of the losing function and the name of the macro.

[18] Making Flavor Instances Print Readably.

3SI:PRINT-READABLY-MIXIN* is a convenient way of making a flavor's instances
print in a way that can be read back.

In order for it to work, you must define the 3:RECONSTRUCTION-INIT-PLIST*
operation.  It should return a list of init-options which could be passed to
3MAKE-INSTANCE* to create the desired new instance at read-in time.
3SI:PRINT-READABLY-MIXIN* cannot define this for you, because only you know
which instance variables ought to be part of the printed representation.

Example:

3(defflavor foo (a car-of-a) (si:print-readably-mixin)
  (:inittable-instance-variables a))

(defmethod (foo :init) (ignore)
  (setq car-of-a (car a)))

(defmethod (foo :reconstruction-init-plist) ()
  `(:a ,a))*				;Note that 3car-of-a* is not included.

3(make-instance 'foo ':a '(69.))* 3=>* 3#mypackage:foo a (71)*

As an additional feature, it becomes possible to fasdump objects containing
such instances.

[] 3READLINE-TRIM* 2stream* 2eof-option

3READLINE-TRIM** is line 3READLINE* except that any leading or trailing spaces and
tabs are discarded from the line before the value is returned.

3  (READLINE-TRIM 2args*...)*

is almost the same as

3  (STRING-TRIM '(#\SP #\TAB) (READLINE 2args*...))*

the difference being that3 READLINE-TRIM* will return the 2eof-option* on end of file
without trying to trim it.

[] 3(PROMPT-AND-READ* 2keyword3 *format-string3 *format-arguments3...)**

The function 3PROMPT-AND-READ* serves to read and return an object after
prompting.  You specify the way to read the object with a keyword, and the way
to prompt with a string and arguments that are passd to 3FORMAT*.

The 2keyword* is one of these: 3:READ, :NUMBER, :EVAL-READ,* 3:EVAL-READ-OR-END,
:STRING, :STRING-OR-NIL, :PATHNAME.

:READ - *the object is read by calling 3READ*.
3:NUMBER* - the object is read by calling 3READ* in base ten.  If it is not a number,
  the user gets an error message and must rub it out and try over.
3:EVAL-READ* - the object is read by 3(EVAL (READ))*.
3:EVAL-READ-OR-END* - the object is read by 3(EVAL (READ))*.  However, the user
 can type the 3End* character instead of an object.  In this case, 3PROMPT-AND-READ*
 returns 3NIL* as the first value, and 3#\End* as the second.
3:STRING* - the object is read by 3READLINE*, so the value is a string.
3:STRING-OR-NIL* - the object is read by 3READLINE-TRIM*, except that if
 the resulting string is empty, 3NIL* is returned instead.
3:PATHNAME* - the object is read by 3(FS:MERGE-PATHNAME-DEFAULTS (READLINE))*.
3(:PATHNAME :DEFAULTS* 2defaults-list3)** - like just 3:PATHNAME*, except that
 2defaults-list* will be passed to 3FS:MERGE-PATHNAME-DEFAULTS* for use as the
 defaults.  2defaults-list* must be literally present in the list passed to
 3PROMPT-AND-READ*.

3PROMPT-AND-READ* always uses 3QUERY-IO* to do the i/o.  It uses the rubout handler
if the stream supports one, and tells the rubout handler how to reprint the
prompt when that is appropriate.

[] 3:FILL-POINTER* Keyword Argument to3 MAKE-ARRAY*.

A new convenient way to make an array with a fill pointer is to use the
3:FILL-POINTER* argument to 3MAKE-ARRAY*.  The presence of this argument requests
a leader of length one, and the value of the argument is used to initialize the
leader slot.

  3(MAKE-ARRAY 5 ':TYPE ART-STRING ':INITIAL-VALUE #\A ':FILL-POINTER 2)
    => "AA"*

[] 3WITH-STACK-LIST* and 3WITH-STACK-LIST**.

These two new special forms allow you to create and use a list whose memory is
allocated inside the stack.  The list will automatically be reclaimed when the 
3WITH-STACK-LIST* is exited, with no permanent consumption of space; but you
had better have no pointers to the list at that time, for they will become garbled.

A use of 3WITH-STACK-LIST* looks like this:

  3(with-stack-list (2variable* 2list-elements*...)*
  3  2body*...)*

During the execution of 2body*, 2variable* will be bound to the temporary list.
2list-elements* are used as the elements when the list is constructed.
The stack list can be 3RPLACA*'d but it cannot be 3RPLACD*'d.

3WITH-STACK-LIST** is called like 3WITH-STACK-LIST*.  The only difference is that it
constructs the list to have a specified final cdr rather than 3NIL*--just as 3LIST**
does.

[] The Applyhook.

The applyhook is a new companion to the old 3EVALHOOK* feature.  While the
evalhook is called when the interpreter calls 3EVAL*, the applyhook is called when
3EVAL* would normally call 3APPLY*.  (Never mind that 3EVAL* does not actually call
3APPLY* per se; it does something equivalent).

To use the applyhook, call 3EVALHOOK* with a third argument, which is the
applyhook function.  As usual, 3EVALHOOK* will evaluate its first argument; but if
that evaluation would apply a function, the applyhook is called instead.
The second argument to 3EVALHOOK*, if non-3NIL*, is used as the evalhook function,
which is called if the evaluation would call 3EVAL* recursively.

The applyhook function receives two arguments: the function that 3EVAL* was
about to apply, and the list of arguments the function was going to get.  These
are the same sorts of arguments that 3APPLY* expects.  An applyhook function
which simply passes its arguments to 3APPLY* is the same as no applyhook at all.

When either the applyhook function or the evalhook function is called,
both the applyhook and the evalhook are bound off.

[] New Flavor Feature: 3:ABSTRACT-FLAVOR

:ABSTRACT-FLAVOR* is a new 3DEFFLAVOR* option.  Its purpose is to suppress the
warning that you would normally get from 3COMPILE-FLAVOR-METHODS* if there are
any required flavors, methods or instance variables that are missing.  This allows
you to do 3COMPILE-FLAVOR-METHODS* on a flavor which is not actually suited for
instantiation.  (This is useful if the combined methods of this flavor could be
inherited without change by several other flavors which can be instantiated).

Since 3:ABSTRACT-FLAVOR* is intended for flavors that are not suitable for
instantiation, it also marks the flavor so that you are not allowed to try to
instantiate it.  This is not logically necessary, but otherwise, once the
3COMPILE-FLAVOR-METHODS* had been done with its warning suppressed, you would
be able to instantiate the flavor with never any warning about the missing
required flavors, methods or instance variables.

[]3:CASE* Method Combination

3:CASE* is a new type of method combination for flavor methods.

Recall that each type of method combination is a way of taking the various
methods for a given flavor and operation and deciding what order to call them
in, and how.  3:DAEMON* method combination, which looks for 3:BEFORE* and 3:AFTER*
methods, is just the default method combination type, not the only one.

3:CASE* method combination provides an automatic subdispatch on the first
argument to the operation.  Each method says which first argument it wants to
handle.  When a message is sent, only one method is called, according to the
first argument in the message.

For example, 

3  (defflavor foo (a b) ()
    (:method-combination :case :base-flavor-last :do-something))

  (defflavor bar (c) (foo))

  (defmethod (foo :case :do-something :print-a) ()
    (print a))

  (defmethod (foo :case :do-something :clear-b) ()
    (setq b nil))

  (defmethod (bar :case :do-something :set-c) (new-c)
    (*setq3 c* 3new-c))*

defines a 3:DO-SOMETHING* operation which, for flavor 3FOO*, understands only
3:PRINT-A* as its first argument, but for flavor 3BAR* understands either 3:PRINT-A* or
3:SET-C*.  3:PRINT-A* and 3:SET-C* are called 2suboperations*.  Each flavor can define
any number of 3:CASE* methods for the same operation if they are for different
suboperations.

You would use this operation in this manner:

3  (send my-bar ':do-something ':set-c 'new-value-for-c)*

The internals of 3:CASE* method combination would automatically dispatch to the
3(BAR :CASE :DO-SOMETHING :SET-C)* method to handle this.  Note that you do not
specify any argument variable in the 3DEFMETHOD* to correspond to the 3:SET-C*.

In addition to the 3:CASE* methods, you can also have a primary method.  It gets
called if none of the 3:CASE* methods applies; that is, when the suboperation
does not match any of the3:CASE* methods.  It is a sort of default handler.  It 2does*
receive the suboperation explicitly as an argument.

You can also have one or more 3:OR* methods.  These are called just before the
primary method, after determining that no 3:CASE* method applies.  If an 3:OR*
method returns non-3NIL*, that is the end: its first value is the value of the
operation.  If all the 3:OR* methods return 3NIL*, the primary method is used.

The suboperations 3:WHICH-OPERATIONS, :SEND-IF-HANDLES,
:OPERATION-HANDLED-P, :GET-HANDLER-FOR* and 3:CASE-DOCUMENTATION* are
handled specially and automatically.  The first four have meanings analogous to
the operations of the same name.  Note that they consider a suboperation to be
handled only if there is a 3:CASE* method for it.  3:CASE-DOCUMENTATION* takes one
argument, a suboperation, and returns the documentation string of the 3:CASE*
method for that suboperation, otherwise 3NIL*.

[] 3(STRING-CAPITALIZE-WORDS 2string* &optional (2copy-flag* t))*

This function performs two transformations on 2string*: it changes all dashes into
spaces, and capitalizes each word.  The initial becomes upper case, and the rest
of each word lower case.

The original string is unchanged, and the value is a new string, unless 2copy-flag*
is 3nil*.

[] Restrictions Lifted on 3UNWIND-PROTECT* and Peculiar Multiple-Value Constructs

3UNWIND-PROTECT* now interacts correctly with 3GO* and 3RETURN* in all cases.  For
example,

3  (prog ()
    (unwind-protect
      (print (if a b (go tag)) stream)
      (close stream))
   tag
    ...)*

will now close the stream even if the 3GO* takes place.

3RETURN* of multiple values to internal destinations now works also.  For example,

3  (multiple-value (a b)
    (prog ()
      (print (if c d (return base t)))
      (return ibase nil)))*

will set 3A* and 3B* properly, and no longer crash the machine.

These two improvements are often useful together, as in

3  (multiple-value (a b)
    (prog ()
      (unwind-protect
        (.... (return x y) ...)
        (close stream))))*

[] 3:NEW-SUGGESTED-NAME* and 3:NEW-SUGGESTED-DIRECTORY* Pathname Operations.

The 3:NEW-SUGGESTED-DIRECTORY* operation is nearly the same as the
3:NEW-DIRECTORY* operation, but it has a slightly different purpose.  The new
directory name is not precisely what is desired, but just a suggestion, based on
some sort of default.  For example, it might be a user name.  Similarly,
3:NEW-SUGGESTED-NAME* supplies a suggestion for the name, rather than a precise
desired name.  It might be the name of an editor buffer.

The difference between the actions of the new operations and the actions of
3:NEW-NAME* and 3:NEW-DIRECTORY* is that the new operations may make
file-system-dependent changes in their arguments.  For example, on systems
such as Twenex, where filenames can contain lowercase characters but normally
do not, the 3-SUGGESTED-* operations convert the name to upper case.

[] (3DUMP-FORMS-TO-FILE* 2filename* 2forms-list* 3&optional* 2attribute-list*)

3DUMP-FORMS-TO-FILE* writes a QFASL file whose effect, when loaded, is to
execute the forms in 2forms-list*.  For example,

3  (dump-forms-to-file "foo" '((setq x 1) (setq y 2)))
  (load "foo")
  x => 1
  y => 2

2attribute-list** is a file attribute list to give to the QFASL file.  It is a list of
alternating keywords and values, and corresponds to the 3-*-* line of a source file.
The most useful keyword in this context is 3:PACKAGE*, whose value in the
attribute list specifies the package to be used both in dumping the forms and in
loading the file.  If no 3:PACKAGE* keyword is present, the file will be loaded in
whatever package is current at the time.
   

User Interface Changes:

	    [A] New Convenient Repetition Construct, 1#`*.
	    [B] DIRED Is Now a File System Editor.
	    [C] Unlimited Undo Capability in the Editor.
	    [D] Editor Buffer Sectionization.
	    [E] 1Kill or Save Buffers*.
	    [F] Accessing Editor Buffers as Files.
	    [G] Editing Multi-Font Files.
	    [H] 1QSEND*, 1REPLY*, and 1MAIL*.
	    [I] 1Network O* in SUPDUP and TELNET toggles overprinting.
	    [J] If You Can't See the Bottom of the Screen...
	    [K] Patches Record Original Source File.
	    [L] 1Terminal* 1Resume* command.
	    [M] Directory Maintenance.
	    [N] Local File System Improvements.
	    [O] File Already Exists error handled better.
	    [P] ZMAIL Terminology Changes.
	    [Q] New Hardcopy Functions.
	    [R] Overriding Site Options on a Per-Machine Basis.
	    [S] Recompiling Site Files.
	    [T] Turning Off Read-Onliness of Editor Buffers.
	    [U] Advice and Other Encapsulations May Be Compiled.
            [V] 3M-X Rename File* Offers to Rename Editor Buffers
            [W] Period Is Now a Word Delimiter.
            [Z] 3ZWEI:*MAJOR-MODE-TRANSLATIONS**

[A] New Convenient Repetition Construct, 1#`*.

1#`* is a construct for repeating an expression with some
subexpressions varying.  It is an abbreviation for writing several
similar expressions, or for the use of 1MAPC*.  Each subexpression
that is to be varied is written as a comma followed by a list of the
things to substitute.  The expression is expanded at read time into
a 1PROGN* containing the individual versions.

1   #`(send stream ',(:clear-input :clear-output))*

expands into

1   (progn (send stream ':clear-input) (send stream ':clear-output))*

Multiple repetitions can be done in parallel by using commas
in several subexpressions:

1   #`(renamef ,("foo" "bar") ,("ofoo" "obar"))*

expands into

1   (progn (renamef "foo" "ofoo")
          (renamef "bar" "obar"))*

If you want to do multiple independent repetitions, you must use nested
1#`* constructs.  Individual commas inside the inner 1#`* apply to
that 1#`*; they vary at maximum speed.  To specify a subexpression
that varies in the outer 1#`*, use two commas.

1   #`#`(print (* ,(5 7) ,,(11. 13.)))*

expands into

1   (progn (progn (print (* 5 11.)) (print (* 7 11.)))
          (progn (print (* 5 13.)) (print (* 7 13.)))*

[B] DIRED Is Now a File System Editor.

DIRED now has several new and changed commands, giving it the full power
of the Symbolics File System Editor.

First of all, the 1C* command now copies the file on the line you are pointing at.
The name to copy to is read with a minibuffer.  The command for comparing
the file against its most recent version is now called 1=*.

Click right on the mouse now gets a menu of operations on the file
the mouse was pointing to.  All the usual operations are available this way.

One new command is 1S*, for Subdirectory.  If you are pointing to a line
which describes a subdirectory, this command inserts a listing of that
subdirectory into the DIRED buffer underneath the subdirectory itself.
Thus, you can operate on multiple levels of directory at once.
Using 1S* again removes the subdirectory's files from the display.
Indentation is used to show you which files are in subdirectories.

The 1X* command in DIRED is no longer the same as Meta-X.  Instead, it is like 1Q*
but does not exit.  That is, it processes all the deletions, printing and
function applications you have specified, but leaves you in DIRED.

Meanwhile, processing the files with 1X* or 1Q* now eliminates all 1F*, 1P* and 1A*
(Find File, Print or Apply Function) markings found on files, so you get a
clean slate to request a new set of operations.  1D* markings are left on
the deleted files, since they are now deleted; but if you expunge, or
if the file system does not support undeletion, the deleted files are
removed from the Dired buffer entirely.

Sorting is now done using 1M-X* commands.
The sorting commands are 1M-X Sort Increasing Creation Date*,
1M-X Sort Increasing Reference Date*, 1M-X Sort Increasing Size*,
and 1M-X Sort Increasing File Name*.

There are also four other commands with 1Decreasing* instead of 1Increasing*.

If you would like any file operation that you do not see in the Help printout of
DIRED, please request it.

Meanwhile, the commands 1M-X Delete File*, 1M-X Undelete File*,
1M-X Rename File* and 1M-X Copy File* now accept filenames with wildcards
and operate on multiple files.

[C] Unlimited Undo Capability in the Editor.

The editor now saves all the changes you make to each buffer,
and you can undo them as far back as you like by doing 1M-X Undo*
multiple times.  To undo the undoing, use 1M-X Redo*.  These commands
also show you a summary of the part of the buffer they are going to change
and what it will be changed to, and then they ask for confirmation.

Since these commands may be useful more often, they are now on
the keys 1C-Shift-U* and 1C-Shift-R*.   When you use those keys, there is no
querying for confirmation.  You simply see the result.

If the variable 1ZWEI:*UNDO-SAVE-SMALL-CHANGES* *is non-1NIL*,
absolutely all editing operations are saved.  Even individual insertions
and deletions are saved, but batched together.
This mode is now the default as of 89.26.

The undo memory for a buffer is cleared out when you read or write a file
with that buffer.

[D] Editor Buffer Sectionization.

1M-X Compile Buffer Changed Sections*, etc., now can always tell when new
functions have been inserted in the buffer.  When it says that it is
compiling function FOO, you can trust that this means precisely function
FOO and no other.

It is still the case that inserting a function causes the following
function to be marked as changed.  So 1Compile Buffer Changed Sections*
will tell you it is recompiling the following function as well as
compiling the inserted function.  This is not easy to fix.

[E] 1Kill or Save Buffers*.

1M-X Kill or Save Buffers* allows you to kill and save the same file.

[F] Accessing Editor Buffers as Files.

Three new host names, 1ED:*, 1ED-BUFFER:*, and 1ED-FILE:* are available for opening
editor buffers as if they were files.  1ED:* uses the completing reader to recognise
abbreviated buffer names.  1ED-BUFFER:* requires the exact name.  1ED-FILE:* takes a
file name and looks for a buffer by that name, if necessary creating a new buffer
visiting the file.  --Devon

[G] Editing Multi-Font Files.

1** is now understood in multi-font files being read into ZWEI.

When multi-font files are saved,  is used when appropriate to say that
the font has changed back to one that was in use before.

As a result, if you use a text justifier such as R or BOLIO in which 
specifies a font change, you can now have your files displayed in your
favorite "equivalent" Lisp machine fonts when you edit it.

[H] 1QSEND**, 1REPLY*, and 1MAIL*.

The function 1QSEND* now accepts multiple recipient names.
The names go in one string, separated by commas, as in
1(QSEND "RMS@OZ, FOO@OZ" "Message")*.
The value is a list of the recipients to which the message was successfully sent.

You can also send a message to multiple people in Converse, just by putting
multiple names in the To: line of the message.

The function 1REPLY* sends a message to the person who last sent you one.
It normally takes one argument, the message.

In either 3QSEND* or 3REPLY*, if you do not specify the message as an argument, it is
read from 3STANDARD-INPUT*.  While you are typing the message, you can type the
command 3Control-Meta-E* to switch to Converse.  The text you had entered, and
the recipient, will be provided there for you.  In 3REPLY*, you can also type
3Control-Meta-Y*, which will insert the contents of the message you are replying
to.

The function 1MAIL*, when given two arguments (recipient and message),
no longer enters the editor.  It sends the string as you gave it.

These features were added by HDT.

[I] 1Network O* in SUPDUP and TELNET Toggles Overprinting.

Normally, SUPDUP and TELNET overprint if the remote system sends
two characters to be output on the same spot without erasing.
If you are talking to a system that assumes your terminal cannot
overprint, this may look ugly.  1Network O* causes SUPDUP or TELNET
to erase character positions when they are about to be written in,
so that your screen will look right.

[J] If You Can't See the Bottom of the Screen...

1TV:SET-TV-SPEED* now takes a second optional argument which specifies
how many scan lines at the bottom of the screen should be unused.
This is useful if it is hard to see some of the bottom of the screen
because it is hidden behind a cover.

If you do not have any preference for the first argument, use 160.5*, which is the
default.

[K] Patches Record Original Source File.

Patches record the original source file of the contents of the patch, so that if the
patch defines any new functions, the editor will find their definitions in their
actual source files rather than in the patch file.

[L] The 1Terminal* 1Resume* Command.

When you are notified that a deexposed window is waiting to type out,
you can type 1Terminal* 1Resume* to permit it to typeout while remaining deexposed.
It used to be necessary to select the window in order to do this.

[M] Directory Maintenance.

1(FS:BALANCE-DIRECTORIES* 2dirspec1* 2dirspec2* 1&REST* 2options1)**

Generates directory lists using the 2dirspec* arguments, then analyzes
the lists to determine which files appear in one directory and not the
other; any files that aren't up to date in either directory are
copied.  The result is that each directory contains up to date copies
of every file in either directory.

A number of options are available for controlling the actions of the
program.  Note that 2dirspec* may be anything that is acceptable to
1FS:DIRECTORY-LIST*, so it is possible to balance selected parts of a
directory.  - Hanson.

[N] Local File System Improvements.

Subdirectories are now allowed.
Subdirectories appear in a pathname separated by periods, before the semicolon:
             1LM: RMS.SUBDIR.LEVEL2; FOO.BAR#>*

Also, Twenex-style pathname syntax is now allowed for the local file
system as an alternative to the old syntax.  Simply use whichever syntax
you prefer.

             1LM: <RMS.SUBDIR.LEVEL2>FOO.BAR.0*

If the flag 1FS:*LMFS-USE-TWENEX-SYNTAX** is set to non-1NIL*, pathnames
for local files will print out in Twenex syntax, rather than the
normal ITS-like syntax.  It is always possible to type them in using
either syntax.

Note that changing the flag's value will not change the printed
representation of an existing pathname object, since it is cached.
Setting this flag in your login init file will eliminate most such
confusion.

[O] "File Already Exists" Error Handled Better.

If you compile a file on a Twenex that has already been compiled, Twenex
will give a "File already exists" error because a QFASL file of the same
version was created by the previous compilation.

The Lisp machine will now offer to delete the old QFASL file for you.
You can type Y, N or P.  If you type N, you get the usual "use what
filename instead" offer.  P is the same as Y for this question and all
such questions in the future.  It sets the following variable to 1T*:

1FS:*AUTO-DELETE-EXISTING-FILE**
 If non-1NIL*, an attempt to write a file with a specific version number
that already exists will try to delete the old file immediately.
Initially 1NIL*.

Note: if someone is reading the file, Twenex will not allow it to be
deleted or renamed.  There is absolutely no way on Twenex to implement
what is supposed to be the default mode of opening an output file: if a
file with the specified name and version already exists, to
replace it when the new file is closed.  Doesn't Twenex suck?

[P] ZMAIL Terminology Changes.

There has been a change in terminology in ZMAIL.  What used to be called
a "mail file" is now called a "buffer".  "Temporary mail files" are now
"temporary buffers", or "subset buffers" if they are made with a filter
(yielding the set of all messages in a universe that satisfy the filter).
"Disk mail files" are now "mail file buffers".  The term "mail file" is
still used, but now always refers to an actual file rather than an
entity within ZMAIL.

[Q] New Hardcopy Functions.

There are some new printer-independent functions for making hardcopy output.
Each type of printer is named with a keyword; you specify a printer with either
a printer-type keyword by itself, or with a list containing a printer-type keyword
and arguments.  The purpose of the arguments is to specify a particular printer
of that type, if there can be more than one.  Some printer-types want arguments
and some do not.

For example, 1:DOVER* is a printer type that does not take arguments -- there is no
provision for more than one Dover.  So the printer you would specify is just the
keyword :1DOVER*.

Another type of "printer" is 1:PRESS-FILE*, which means that the "printing" is done
by writing a press format file on a file server.  This type of printer does require
an argument, with which you specify the name to give the press file.  So,
1(:PRESS-FILE "<RMS>FOO.PRESS")* is an example of a possible 2printer* of type
1:PRESS-FILE*.

1(HARDCOPY-FILE* 2filename* 1&REST* 2options1)**

prints the file 2filename* according to 2options* (a list of alternating keywords and values).
Options include 1:PRINTER*, whose value specifies the printer to use, and
1:FORMAT*, whose value is either 1:TEXT*, 1:PRESS*, 1:XGP* or 1:SUDS-PLOT*, which
specifies how to interpret the contents of the file (this defaults from the
filetype).  Individual printers can implement other formats, and may fail to
implement all of these.

Each type of printer may implement other options.  See the on-line
documentation of 1HARDCOPY-FILE* for more information.

To find out whether your output has been printed, use

1(HARDCOPY-STATUS* 1&OPTIONAL* 2printer* 2stream1)**

which prints the status of the specified or default printer, if that makes sense.
It will generally describe both the printer itself and the spooler queue if there is
one.  It is not implemented for all printers (for example, it is not meaningful for
printers of type 1:PRESS-FILE*).

To print out the contents of an array, such as the screen, as black and white
dots, use the function

1(HARDCOPY-BIT-ARRAY* 2array* 2left* 2top* 2right* 2bottom* 1&REST* 2options*)

The first argument is the array to print, and the next four specify the
subrectangle to use.  All four measure from the top left corner, which is
element 10*, 10* of the array.  2options* can include 1:PRINTER*.

The default printer to use for 1HARDCOPY-FILE* and 1HARDCOPY-STATUS* is the value
of 1SI:*DEFAULT-PRINTER**.  For 1HARDCOPY-BIT-ARRAY*, the default is the value of
1SI:*DEFAULT-BIT-ARRAY-PRINTER**.  The separate default is because not all
printers can print a full screen bit array.

These two variables are initialized from the site options 1:DEFAULT-PRINTER* and
1:DEFAULT-BIT-ARRAY-PRINTER*.  They can also be specified on a per-machine
basis in the 1LMLOCS* file, as follows.

[R] Overriding Site Options on a Per-Machine Basis.

The file 1SYS: SITE; LMLOCS LISP*, which describes the location of each
Lisp machine at your site, can now contain a list of site options which
should be overridden for that particular machine.

The entry for a machine can now have six elements.  The sixth element is
a list of site option specifications, each of the form

   1(2site-option-name** 2expression1)**

This is just like what would go in the SITE file for that option.

For example, it might be useful to specify the 1:DEFAULT-PRINTER* option
separately for individual machines, so each machine can print on a
printer near it.

The function 1GET-SITE-OPTION* checks the per-machine specifications
before the site definition, so no other code should need to be changed.

A new initialization list, 1SI:SITE-OPTION-INITIALIZATION-LIST*, is run at
an appropriate time when new site files are loaded and also after
booting.  It is useful for reinitializing anything that depends on a
site option.  The 1ADD-INITIALIZATION* keyword 1SITE-OPTION* specifies this list.

[S] Recompiling Site Files.

There is now an easy way to recompile the site files, including the host
table.  Just do 1(MAKE-SYSTEM 'SITE 'COMPILE)*.

[T] Turning Off Read-Onliness of Editor Buffers.

The new command 1C-X C-Q* toggles the read-only flag of the selected
editor buffer.

[U] Advice and Other Encapsulations May Be Compiled.

Recall that 1ADVISE*, 1TRACE* and 1BREAKON* work by putting what are called
2encapsulations* on a function, and these are actually separate functions (lambda
expressions) which refer to the original function.  You can now cause these
functions to be compiled.  As a result, it becomes practical to use advice
in places that are called frequently.

1(COMPILE-ENCAPSULATIONS* 2function-spec* 1&REST* 2encapsulation-types1)**

compiles the specified types of encapsulations of 2function-spec*.  Standard types
of encapsulations include 1ADVISE*, 1TRACE* and 1BREAKON*, and also 1SI:RENAME-WITHIN*
(a kind of encapsulation that is put on the function 1FOO* if you advise, trace or
breakon 1(:WITHIN FOO BAR)* for any function 1BAR* called by 1FOO*).

If you don't specify any types of encapsulations, all encapsulations that
2function-spec* has are compiled.

If you redefine one of the encapsulations -- for example, if you add additional
advice -- then that encapsulation is no longer compiled.  However, redefining
the function itself does not cause encapsulations to be uncompiled.

If the variable 1COMPILE-ENCAPSULATIONS-FLAG* is non-1NIL*, all encapsulations that
you create or alter are compiled automatically.

[V] 3M-X Rename File* Offers to Rename Editor Buffers

If you rename with 3M-X Rename File* a file that is being edited, it offers to
rename the editor buffer as well.

[W] Period Is Now a Word Delimiter.

Except in MIDAS mode, the character Period is now a word delimiter in the editor.

[Z] 3ZWEI:*MAJOR-MODE-TRANSLATIONS**

If you frequently edit files whose attribute lists specify modes which do not exist
in ZWEI, you can "translate" those modes to other modes which do exist.
Push an element of the form (2file-mode* . 2translated-mode*) onto the variable
3ZWEI:*MAJOR-MODE-TRANSLATIONS**, and files which specify mode 2file-mode* will
be edited in mode 2translated-mode*.  Both modes should be keywords, and should
not contain the word 3MODE*.

Initially, this variable has one element: 3(:SCRIBE . :TEXT)*

[] 3COMPILER:COMPILER-VERBOSE*

If this variable is non-3NIL* the compiler prints a message at the beginning of
each function it compiles, stating the function's name.
