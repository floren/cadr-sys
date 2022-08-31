-*-Fonts:TR12,cptfont,TR12I,CPTFONT,TR12B; Mode:Text-*-

System 91 comes with microcode 204.  It does not run with earlier microcode versions,
and system 89 does not run with microcode 204.

System 91 will not run code compiled by Systems 93 and later unless microcode
226 or later is used with it.  System 91 will run properly with this microcode,
but most existing copies of 91 are not set to want it by default.

There are some major incompatibilities related to the presence of a new,
flavor-based error system, documented in the file 1SYS: MAN; ERRORS TEXT.*
The major incompatibilities alone are documented here.
       
       1[A]* 1ERROR-RESTART* Works Differently.
       1[B]* Condition Handler Functions Called Differently.
       1[C]* Interfacing with the 1ABORT* key.
       1[D]* 1ERROR-HANDLER-IO* renamed to 1DEBUG-IO.*
       1[E]* Time-Parsing Functions Now Signal Errors.
       1[F]* 1OPEN* and Other Functions Return Condition Instances.
       1[G]* 1OPEN* and Other Functions No Longer Ask for New Pathnames.
       1[H]* 1WITH-OPEN-FILE-CASE* and 1WITH-OPEN-STREAM-CASE* Defined.
       1[I]* 1PROBEF* Changed.

1[A]* 1ERROR-RESTART* Works Differently.

The meaning and usage of the 1ERROR-RESTART* special form have changed;
all uses will have to be modified.  The full details are in 1SYS: MAN; ERRORS TEXT*.

1[B]* Condition Handler Functions Called Differently.

 The calling conventions of condition handler functions (used in
1CONDITION-BIND*) have changed, so all uses of 1CONDITION-BIND* will have to be
modified.

1[C]* Interfacing with the 1ABORT* key.

Places for the 1ABORT* key to go back to are no longer created using the
catch tag 1SYS:COMMAND-LEVEL*.  In fact, a catch for this tag now has no effect.
The new way to define a place to abort to is to make a resume handler
for the 1SYS:ABORT* condition using 1CATCH-ERROR-RESTART*.  Example:

1    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to ~A loop." PROGNAME)*
	2...body...1)

[D]** 1ERROR-HANDLER-IO* renamed to 1DEBUG-IO.*

The stream used for interactive I/O by the debugger is now called 1DEBUG-IO.*
Furthermore, its default value is a synonym stream for 1TERMINAL-IO*,
rather than 1NIL*.

1[E]* Time-Parsing Functions Now Signal Errors.

The time-parsing functions 1TIME:PARSE-UNIVERSAL-TIME* and 1TIME:PARSE*,
and the chaosnet connection functions 1CHAOS:CONNECT* and 1CHAOS:SIMPLE*,
now signal errors when there is a problem with parsing the argument,
rather than returning a string describing the problem.  1CHAOS:FINGER* and
1CHAOS:WHOIS* also have this change.

1[F]* 1OPEN* and Other Functions Return Condition Instances.

1OPEN* and the other functions for operating on files still return an object
describing the error if the caller specifies 1:ERROR NIL*, but this object is now a
flavor instance rather than a string.  1ERRORP* returns 1T* for both error instances
and strings, in system 91.  In system 92, however, it will be changed to return
1NIL* for strings.  The two-step change is to give users time to convert.

1[G]* 1OPEN* and Other Functions No Longer Ask for New Pathnames.

3OPEN* and other file accessing functions now by default simply signal an error
for problems such as "file not found", rather than asking for a new pathname.  If
you would like to ask the user for a new pathname, you can specify the value
1:REPROMPT* for the 1ERROR-P* or 1:ERROR* argument; or you can use the special forms
1FILE-RETRY-NEW-PATHNAME* or 1WITH-OPEN-FILE-RETRY* which give the caller more
information about what is going on.

1FILE-RETRY-NEW-PATHNAME (2pathname-variable condition-names...*) 2body...**
  Executes 2body*, returning its values if it returns normally.
  If a condition with one of the condition names 2condition-names* is signaled
  during the execution of 2body*, then a condition handler set up by this
  macro reads a new pathname from 1QUERY-IO*, setq's 2pathname-variable* to it,
  and starts 2body* over again.  2pathname-variable* is not bound by the
  1FILE-RETRY-NEW-PATHNAME* form; it is used free.  Normally, on entry, its
  value will be the pathname you originally intend to use, and on exit you
  can expect it to contain the pathname actually opened, deleted or whatever.

1FILE-RETRY-NEW-PATHNAME-IF 2cond-form* (2pathname-variable condition-names...*) 2body...**
  Like 1FILE-RETRY-NEW-PATHNAME*, but provides the condition handler only
  if the value of 2cond-form* is non-1NIL*.

1WITH-OPEN-FILE-RETRY (2stream-var *(2pathname-var condition-names...*) 2options...*) 2body...**
  This is like 1WITH-OPEN-FILE* except that the opening is done using the value of
  2pathname-var* as the pathname, inside a 1FILE-RETRY-NEW-PATHNAME*.

1[H]* 1WITH-OPEN-FILE-CASE* and 1WITH-OPEN-STREAM-CASE* Defined.

These two new special forms are combinations of 1WITH-OPEN-FILE* or 3WITH-OPEN-STREAM*
with 1CONDITION-CASE* (see 1SYS: MAN; ERRORS TEXT*).  They allow you to open a stream,
use it, and close it, while dispatching on whether there was an error in opening,
and on what kind of error it was.

1WITH-OPEN-FILE-CASE (2stream-var pathname options...*) 2clauses...**

This opens the file and binds 2stream-var* just like 1WITH-OPEN-FILE*.  The
difference is that instead of a simple body made of forms to be executed, there
is a sequence of 2clauses* like the clauses in a 3SELECTQ*.  A clause starts with a
condition name or a list of condition names, or else with the keyword 1:NO-ERROR*.
This is followed in the clause by forms to be executed.

If opening the file succeeds (does not signal an error), the 1:NO-ERROR* clause is
executed.  Otherwise, the first clause whose condition names match the error is
executed.  Then 2stream-var* holds the condition instance that was signaled.  If no
clause matches, the error is not handled here; it may be handled elsewhere or it
may enter the debugger.

Example:

1   (WITH-OPEN-FILE-CASE (STREAM FILENAME ':CHARACTERS ':DEFAULT)
     (:NO-ERROR (SELECTQ (SEND STREAM ':CHARACTERS) ...))
     (FS:FILE-NOT-FOUND (FORMAT T "~&(New File)"))
     (ERROR (BARF "Error: ~A" STREAM)))  ;STREAM* here is the condition instance.

1WITH-OPEN-STREAM-CASE* is similar, but takes an explicit form which says
how to create the stream.

1[I]* 1PROBEF* Changed.

1PROBEF* now returns 1NIL* only for "file not found".
If any other error happens, 1PROBEF* does not handle it.  You can handle it
yourself with your own condition handler, established around the call to 1PROBEF.*

Programming Changes:
1    
    [A]* 1CATCH-CONTINUATION*.
1    [B]* 3READLINE-TRIM* 2stream* 2eof-option
1    [C]** 3(PROMPT-AND-READ* 2keyword3 *format-string3 *format-arguments3...)
1    [D]*** 3:FILL-POINTER* Keyword Argument to3 MAKE-ARRAY*.
1    [E]* 3WITH-STACK-LIST* and 3WITH-STACK-LIST**.
1    [F]* The Applyhook.
1    [E]* New Flavor Feature: 3:ABSTRACT-FLAVOR
1    [G]**3:CASE* Method Combination
1    [H]* 3(STRING-CAPITALIZE-WORDS 2string* &optional (2copy-flag* t))
1    [I]** Restrictions Lifted on 3UNWIND-PROTECT* and Peculiar Multiple-Value Constructs
1    [J]* 3:NEW-SUGGESTED-NAME* and 3:NEW-SUGGESTED-DIRECTORY* Pathname Operations.
1    [K]* Readmacros 1#M*, 1#Q*, 1#+*, 1#-* Work Better.
1    [L]* 1:ITEM* Operation on 1TV:TEXT-SCROLL-WINDOW* Changed.
1    [M]* Reading and Printing of Hex Numbers Improved.
1    [N]* 1CURRENT-STACK-GROUP* and 1CURRENT-STACK-GROUP-RESUMER*.
1    [O]* New Stack Analysis Functions.
1    [P]* Lambda List Keyword 1&KEY*.
1    [Q]* 2units* Argument to Window Operations 1:INSERT-LINE*, etc.
1    [R]* New 1DEFSTRUCT* Options.
1    [S]* New 1FORMAT* Directives1 ~$*, 1~\DATE\*, 1~\TIME\*, 1~\DATIME\*, 1~\TIME-INTERVAL\*.
1    [T]* Asynchronous Characters.
1    [U]* New Window Graphics Operations in 1TV:GRAPHICS-MIXIN*.
1    [V]* 1DONT-OPTIMIZE* Works Inside 1SETF*.
1    [W]* 1#?NIL* Allowed in 1SELECT-MATCH*.

1[A]* 1CATCH-CONTINUATION*.

The new special form 1CATCH-CONTINUATION* makes it easy to write a catch,
pass back all multiple values from its body, and still find out whether the
catch was exited normally or thrown to.  The syntax is

1CATCH-CONTINUATION* 2tagexp* 2throw-cont* 2non-throw-cont* 2body...

body* is executed with a catch for a tag which is the value of 2tagexp*.

If 2body* returns normally, what happens is determined by the value of
2non-throw-cont*, a function used as a continuation.  If it is 1NIL*, the values of 2body*
are simply returned.  Otherwise, they are passed as arguments to 2non-throw-cont*,
and 2its* values are returned.  Usually 2non-throw-cont* is 1NIL*.

If 2body* throws to this catch, the four standard values of 1*CATCH* are collected,
and what happens to them is determined by the value of 2throw-cont*, another
continuation.  If it is 1NIL*, those values are returned from the 1CATCH-CONTINUATION.*
Otherwise, they are passed to the continuation function, and its values are returned.

If the continuation function is written as 1#'(LAMBDA ...)*, then only as many
values are collected as the continuation function wants, and no consing is needed.
The continuation function is also open-coded.  This is the most usual form of use.

1CATCH-CONTINUATION-IF* is also provided.  Its first argument is a conditional form.
The catch is only in effect if the conditional's value is non-1NIL*.  The remaining
arguments are the same as those of 1CATCH-CONTINUATION*.

1[B]* 3READLINE-TRIM* 2stream* 2eof-option

3READLINE-TRIM** is line 3READLINE* except that any leading or trailing spaces and
tabs are discarded from the line before the value is returned.

3  (READLINE-TRIM 2args*...)*

is almost the same as

3  (STRING-TRIM '(#\SP #\TAB) (READLINE 2args*...))*

the difference being that3 READLINE-TRIM* will return the 2eof-option* on end of file
without trying to trim it.

1[C]* 3(PROMPT-AND-READ* 2keyword3 *format-string3 *format-arguments3...)**

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

1[D]* 3:FILL-POINTER* Keyword Argument to3 MAKE-ARRAY*.

A new convenient way to make an array with a fill pointer is to use the
3:FILL-POINTER* argument to 3MAKE-ARRAY*.  The presence of this argument requests
a leader of length one, and the value of the argument is used to initialize the
leader slot.

  3(MAKE-ARRAY 5 ':TYPE ART-STRING ':INITIAL-VALUE #\A ':FILL-POINTER 2)
    => "AA"

1[E]** 3WITH-STACK-LIST* and 3WITH-STACK-LIST**.

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

1[F]* The Applyhook.

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

1[E]* New Flavor Feature: 3:ABSTRACT-FLAVOR

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

1[G]*3:CASE* Method Combination

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

1[H]* 3(STRING-CAPITALIZE-WORDS 2string* &optional (2copy-flag* t))*

This function performs two transformations on 2string*: it changes all dashes into
spaces, and capitalizes each word.  The initial becomes upper case, and the rest
of each word lower case.

The original string is unchanged, and the value is a new string, unless 2copy-flag*
is 3nil*.

1[I]* Restrictions Lifted on 3UNWIND-PROTECT* and Peculiar Multiple-Value Constructs

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
        (close stream))))

1[J]** 3:NEW-SUGGESTED-NAME* and 3:NEW-SUGGESTED-DIRECTORY* Pathname Operations.

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

1[K]* Readmacros 1#M*, 1#Q*, 1#+*, 1#-* Work Better.

These read-time conditional constructs now do not intern the symbols in the
following s-expression, if the conditional fails.  As a result, there will be no
error if the s-expression contains symbols with package prefixes for packages
that do not, and should not, exist.

1[L]* 1:ITEM* Operation on 1TV:TEXT-SCROLL-WINDOW* Changed.

The 1:ITEM* operation on these windows is now compatible with the 1:ITEM*
operation provided by typeout windows.  As a result, objects which print
themselves in a mouse-sensitive fashion using the 1:ITEM* operation when it is
available will now work properly in the inspector.

The old calling conventions for the 1:ITEM* operation are now available as
the 1:ITEM1* operation.

1[M]* Reading and Printing of Hex Numbers Improved.

You can now read in Hexadecimal numbers successfully using either
1#X* or 1#16R* followd by the number.  However, in order to cause the
characters 1A* through 1F* to be recognized as digits, you must begin
the number with a sign (1+* or 1-*).

To print hexadecimal numbers, it suffices to set 1BASE* to 16 in any fashion.

1[N]* 1CURRENT-STACK-GROUP* and 1CURRENT-STACK-GROUP-RESUMER*.

These are new, global names for 1SI:%CURRENT-STACK-GROUP* and
1SI:%CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP.

[O]* New Stack Analysis Functions.

Several functions in the 1EH* package used for finding stack frames and extacting
arguments, locals, etc. from them are now documented in the file 1SYS: MAN; FD-SG TEXT.

[P]* Lambda List Keyword 1&KEY*.

This feature still exists as announced a year ago.  Refer to 1SYS: MAN; FD-EVA
TEXT*, until the new manual is out.

1[Q]* 2units* Argument to Window Operations 1:INSERT-LINE*, etc.

The operations 1:INSERT-LINE*, 1:DELETE-LINE*, 1:INSERT-CHAR* and 1:DELETE-CHAR*
now take two arguments.  The second argument says what units are used
to interpret the first argument.  If the second argument is 1:PIXEL*, then
the first argument is taken as the number of pixels of space to insert.
If the second argument is 1:CHARACTER*, which is the default, the first argument
is the number of characters or lines.

1[R]* New 1DEFSTRUCT* Options.

There are three new options for 1DEFSTRUCT*: 1:PRINT*, 1:PREDICATE* and 1:COPIER.*

1) 1:PRINT*

This allows the user to control the printed representation of his structure in
an implementation independent way:
1    
    (DEFSTRUCT (FOO :NAMED
                (:PRINT "#<Foo ~S ~S>" (FOO-A FOO) (FOO-B FOO)))*
	1FOO-A*
	1FOO-B)*

The arguments to the 1:PRINT* option are arguments to the 1FORMAT* function (except
for the stream of course!).  They are evaluated in an environment where the
name symbol of the structure (1FOO* in this case) is bound to the instance of the
structure to be printed.

This works by creating a simple named structure handler.  You should not use
the 1:PRINT* option if you are defining a named structure handler explicitly.

2) 1:PREDICATE*

The 1:PREDICATE* option causes 1DEFSTRUCT* to generate a predicate to recognize
instances of the structure.  Naturally it only works for "named" defstruct types1.*
The argument to the 1:PREDICATE* option is the name to give the predicate.  If it
is present without an argument, then the name is formed by concatenating "1-P*"
to the end of the name symbol of the structure.  If the option is not present,
then no predicate is generated.  Example:

1    (DEFSTRUCT (FOO :NAMED :PREDICATE)*
	1FOO-A*
	1FOO-B)*

defines a single argument function, 1FOO-P*, that is true only of instances of
this structure.

3) 1:COPIER*

This option causes 1DEFSTRUCT* to generate a single argument function that will
copy instances of this structure.  Its argument is the name of the copying
function.  If the option is present without an argument, then the name is
formed by concatenating "1COPY-*" with the name of the structure.  Example:

1    (DEFSTRUCT (FOO (:TYPE :LIST) :COPIER)*
	1FOO-A*
	1FOO-B)*

Generates a function approximately like:

1    (DEFUN COPY-FOO (X)*
	1(LIST (CAR X) (CADR X)))

[S]* New 1FORMAT* Directives1 ~$*, 1~\DATE\*, 1~\TIME\*, 1~\DATIME\*, 1~\TIME-INTERVAL\*.

1~$

3~2rdig*,2ldig*,2field*,2padchar*** 3$* prints 2arg*, a flonum, with exactly
2rdig* digits after the decimal point.  The default for 2rdig* is 2,
which is convenient for printing amounts of money.

At least 2ldig* digits will be printed preceding the decimal point;
leading zeros will be printed if there would be fewer than 2ldig*.
The default for 2ldig* is 1.  The number is right justified in a field 2field*
columns long, padded out with 2padchar*.  The colon modifier means
that the sign character is to be at the beginning of the field, before
the padding, rather than just to the left of the number.  The atsign modifier
says that the sign character should always be output.

If 2arg* is not a number, or is unreasonably large, it will be printed
in 3~2field*,,,2padchar*@A* format; i.e. it will be 3PRINC*'ed
right-justified in the specified field width.

1~\DATE\*

This expects an argument which is a universal time, and prints it as a date and
time using 3TIME:PRINT-UNIVERSAL-DATE*.  Example:
1    (FORMAT T "It is now ~\DATE\" (TIME:GET-UNIVERSAL-TIME))*
prints
1    It is now Saturday the fourth of December, 1982; 4:00:32 am

~\TIME\*

This expects an argument which is a universal time and prints it in a brief
format using 3TIME:PRINT-UNIVERSAL-TIME*.  Example:
1    (FORMAT T "It is now ~\TIME\" (TIME:GET-UNIVERSAL-TIME))*
prints
1    It is now 12/04/82 04:01:38

~\DATIME\*

This prints the current time and date.  It does not use an argument.
It is equivalent to using the 3~\TIME\* directive with
3(TIME:GET-UNIVERSAL-TIME)* as argument.

1~\TIME-INTERVAL~\*
This prints a time interval measured in seconds using the function
3TIME:PRINT-INTERVAL-OR-NEVER*.  Example:

1    (FORMAT T "It took ~\TIME-INTERVAL\." 3601.)*
prints
1    It took 1 hour 1 second.

[T]* Asynchronous Characters.

The characters 1Control-ABORT* and 1Control-Meta-BREAK* are examples of
"asynchronous characters", characters processed specially as soon as they are
typed, even if the program is not reading input.  Programs can now define
their own asynchronous characters on a per-window basis.  This facility is
provided by the flavor 1TV:STREAM-MIXIN.*

The basic way to specify the asynchronous characters for a window is with the
init option 1:ASYNCHRONOUS-CHARACTERS* in 1TV:MAKE-WINDOW.*  The value you
specify should be an alist of characters and their handlers.  Each element of the
alist contains a character, a function to call, and some additional arguments to
pass to the function.  When the function is called, its arguments will be the
character typed (the same function can handle more than one character),
the selected window, and any specified additional args.

If the init option 1:ASYNCHRONOUS-CHARACTERS* is not specified, the default comes
from the value of 1TV:KBD-STANDARD-ASYNCHRONOUS-CHARACTERS*, the initial value
of which is
    1'((#\C-ABORT TV:KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER*
	1(:NAME "Abort" :PRIORITY 50.)*
	1TV:KBD-INTERCEPT-ABORT)*
      1(#\C-M-ABORT TV:KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER*
       1(:NAME "Abort All" :PRIORITY 50.)*
       1TV:KBD-INTERCEPT-ABORT-ALL)*
      1(#\C-BREAK TV:KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER*
       1(:NAME "Break" :PRIORITY 40.)*
       1TV:KBD-INTERCEPT-BREAK)*
      1(#\C-M-BREAK TV:KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER*
       1(:NAME "Error Break" :PRIORITY 40.)*
       1TV:KBD-INTERCEPT-ERROR-BREAK))*

An asynchronous character is handled by a process called the 2keyboard process*.  It
handles the character by calling a function associated with it.  The function runs
in the keyboard process rather than your program's process, and must obey some
strict conventions.  It must not do any I/O, or wait for anything; it should not
run for very long; it should not get an error.  It is usually easiest to create
another process and do the real work there, using 1PROCESS-RUN-FUNCTION*.  This
is usually done with the function1 TV:KBD-ASYNCHRONOUS-INTERCEPT-CHARACTER*.
It is used with two additional arguments: the process name and options for
1PROCESS-RUN-FUNCTION*, and the function to call in the new process.

You can examine and change a window's asynchronous characters with these
window operations:

1:ASYNCHRONOUS-CHARACTER-P 2character**

  Returns non-1NIL* if the window defines 2character* to be an asynchronous
  character.

1:HANDLE-ASYNCHRONOUS-CHARACTER 2character**

  Run the handler associated with the asynchronous character 2character*.
  This runs the handler function in your current process.  But since most
  handler functions just do 1PROCESS-RUN-FUNCTION*, it probably doesn't matter.

1:ADD-ASYNCHRONOUS-CHARACTER 2character handler-function *&REST 2additional-args**

  Add 2character* to this window's list of asynchronous characters, defined to
  call 2handler-function*.  2additional-args* are passed to the handler function
  when it is called.  The arguments to this operation, all of them, simply become
  the alist element.

1:REMOVE-ASYNCHRONOUS-CHARACTER 2character**

  Removes 2character* from this window's list of asynchronous characters.

1[U]* New Window Graphics Operations in 1TV:GRAPHICS-MIXIN*.

1:DRAW-CIRCULAR-ARC* 2center-x center-y radius start-theta end-theta 1&OPTIONAL* alu*

  This operation draws a specified part of a circle centered at the point
  2center-x*, 2center-y* and of radius 2radius*.  The part of the circle to be drawn
  is specified by 2start-theta* and 2end-theta*.  Both angles should be in the
  interval 1[0,2)*; an angle of zero goes to the right, and small positive
  angles point slightly upward.  The arc starts at 2start-theta* and goes through
  increasing angles, passing through zero if necessary, to stop at 2end-theta.

  alu* controls how the points are drawn (or erased).

1:DRAW-CLOSED-CURVE* 2px py 1&OPTIONAL** 2end alu*

  This is the same as the 1:DRAW-CURVE* operation except that the last point is
  connected back to the first.  2px* and 2py* are vectors of coordinates, and 2end*
  is an optional index in those arrays to stop drawing at.

1:DRAW-DASHED-LINE 2x0 y0 x1 y1** 3&OPTIONAL* 2alu
1    (*dash-spacing1 20.) *space-literally-p1 (*offset1 0)
    (*dash-length1 (FLOOR *dash-spacing1 2)))**

  This operation draws a line divided into dashes.  The first five arguments are
  the same as those of the 1:DRAW-LINE* operation.

  The argument 2dash-spacing* specifies the period of repetition of the dashes;
  it is the length of a dash plus the length of a space between dashes.
  2dash-length* is the length of the actual dash; it defaults to half the spacing.

  If 2space-literally-p* is 1NIL*, the spacing between dashes is adjusted so that
  the dashes fit evenly into the length of line to be drawn.  If it is non-1NIL*,
  the spacing is used exactly as specified, even though that might put the
  end point in the middle of a space between dashes.

  A nonzero 2offset* is used if you want a space between the starting point and the
  beginning of the first dash.  The value is the amount of space desired, in
  pixels.  The same space will be provided at the end point, if 2space-literally-p*
  is 1NIL*.

1[V]* 1DONT-OPTIMIZE* Works Inside 1SETF*.

1DONT*-1OPTIMIZE*, which is used around calls to 1DEFSUBST*s to avoid expanding them
at compile time, can now be used inside a 1SETF*.  For example,

    1(SETF (ZWEI:BUFFER-PACKAGE BUFFER) (PKG-FIND-PACKAGE "USER"))*

sets the package of 1BUFFER*, but needs to be recompiled if the definition of
1BUFFER-PACKAGE* is changed.

    1(SETF (DONT-OPTIMIZE (ZWEI:BUFFER-PACKAGE BUFFER))*
	1   (PKG-FIND-PACKAGE "USER"))*

does the same job but never needs to be recompiled.

1[W]* 1#?NIL* Allowed in 1SELECT-MATCH*.

1#?NIL* may now be used in a 1SELECT-MATCH* pattern to match anything and bind
no variable.
1
User* Interface Changes:

    1[A]* 1LOGIN* Calling Conventions Change.
    1[B]* Debugger Command 1Control-M* for Sending Bug Reports.
    1[C] *New Calling Conventions for 1APROPOS*.
    1[D]* 1ZWEI:*DEFAULT-MAJOR-MODE** Values Change.
    1[E]* 1ZWEI:EDITING-TYPE *Property.
    1[F]* Reading Files into ZWEI May Look Slower, But It Is Not.
    1[G]* 1M-X Find File No Sectionize*.
    1[H]* New Editor Variable 1ZWEI:*ONE-WINDOW-DEFAULT* *Controls 1C-X 1.*
    1[I]* 1C-M-.* is 1Edit Zmacs Command*.
    1[J]* Balance Directories Editor.
    1[K]* 3COMPILER:COMPILER-VERBOSE*.
    1[L]* Compiler Peephole Optimizer1.*
    1[M]* 1DUMP-FORMS-TO-FILE* Writes A QFASL File.
    1[N]* Completion in 1M-X Bug*.  How to Add New Bug Report Lists. 
    1[O]* Negative Arguments Allowed in the Rubout Handler.
    1[P]* Wait States for Chaosnet Connecting Show the Host Name.
    1[Q]* New Function 1LISTF* Lists a Directory.
    1[R]* 1DISASSEMBLE* Now Describes Any Complex Function Entry Initializations
    1[S]* Better Truncation of Pathnames in the Who Line
    1[T]* 1(DECLARE (VALUES ...))*
    1[U]* 1GC-IMMEDIATELY*.
    1[V]* PEEK Improvements.
    1[W]* New function 1(teach-zmacs)*.
    1[X] M-X Teach Zmacs

[A]* 1LOGIN* Calling Conventions Change.

The sense of the third argument to 1LOGIN* is now reversed.
1T* now means 2do not* load the init file, and 1NIL* (the default) means do load it.
This is so that the 1T* in 1(LOGIN "FOO" "OZ" T)* means the same thing as the 1T*
in 1(LOGIN "FOO" T)*.

1[B]* Debugger Command 1Control-M* for Sending Bug Reports.

In the debugger, you can conveniently send a bug report containing the error
message and a backtrace by typing 1Control-M*.  This puts you into an editor
window by calling the function 1BUG*, and preinitializes it with the error message
and stack information.  You edit in your description of the circumstances
and type 1End*.

The new 1:MAIL* command in CC performs a similar function for reporting
bugs that crash the machine.  It sends a report starting with the same text that
1:WHY* prints.

1[C] *New Calling Conventions for 1APROPOS*.

1APROPOS* used to take arguments 2substring* 2package superiors inferiors dont-print.*

Now all but the first argument are keyword arguments.  The keywords are
  1:PACKAGE* -- specify the package to look through.  Default is 1GLOBAL*.
  1:SUPERIORS* -- if 1NIL*, the superiors of that package are not searched.
     The default is 1T*.
  1:INFERIORS* -- if 1NIL*, the subpackages of that package are not searched.
     The default is 1T*.
  1:DONT-PRINT* -- if non-1NIL*, the symbols are not printed when they are found.
     Then the only thing 1APROPOS* does is return a list of them.
  1:PREDICATE* -- if non-1NIL*, a predicate to filter symbols with.
     A symbol is ignored unless the predicate returns non-1NIL*.
     For example, use 1FBOUNDP* to find only defined functions.

1SUB-APROPOS* has been changed in the same fashion.
It takes two positional arguments, the substring and a list of symbols,
and then allows 1:PREDICATE* and 1:DONT-PRINT* as keyword arguments.

1[D]* 1ZWEI:*DEFAULT-MAJOR-MODE** Values Change.

The meaningful values of this variable used to be symbols such as
1ZWEI:LISP-MODE*.  They are now keywords such as 1:LISP*.  The values you can
use here are keywords which are the name of a mode, without the "1-MODE*", just
like what you would use in 1ZWEI:*MAJOR-MODE-TRANSLATIONS** or
1FS:*FILE-TYPE-MODE-ALIST*.

[E]* 1ZWEI:EDITING-TYPE *Property.

A ZWEI mode symbol such as 1ZWEI:LISP-MODE* may have a 1ZWEI:EDITING-TYPE*
property which says how to analyze the syntax of the contents of the file.  The
two defined values for this property are 1:LISP* and 1:TEXT*.  1:LISP* means to
assume Lisp syntax, and 1:TEXT* means to recognize some kinds of text-justifier
input syntax.

For example, when files are scanned for definitions for the sake of the 1Meta-.*
command, 1:LISP* means look for lines starting with "1(DEF"*, etc., whereas 1:TEXT*
means to look for lines starting with "1.def*", which might identify the
documentation of some object, in a text justifier input file.

1[F]* Reading Files into ZWEI May Look Slower, But It Is Not.

Files used to be divided up into sections as they were read into the editor.
Now the whole file is read in, and then divided up into sections.
This is done to fix a class of bugs in sectionization.

The result is that the file is read in quickly, and then there is a long pause at
end of file.  Tests show that the total amount of time taken is about the same as
it used to be.

1[G]* 1M-X Find File No Sectionize*.

This editor command is like 1C-X C-F* except that it does not search the file
for definitions and record them for 1Meta-Period*.  It is useful if you do not
want 1Meta-Period* to see the definitions in that file (for example, if it is
an old version, and you want 1Meta-Period* to show you the latest version always).

If you do 1M-X List Sections*, 1M-X List Buffer Changed Sections*, or any other
command which specifically operates on the sectionization of the buffer, it will
be sectionized then.  In system 92 this will be changed not to sectionize the
buffer ever1 *unless you give an explicit 1M-X Sectionize Buffer* command.

1[H]* New Editor Variable 1ZWEI:*ONE-WINDOW-DEFAULT* *Controls 1C-X 1.*

Which window is selected after a 1C-X 1* command is determined by the value of
1ZWEI:*ONE-WINDOW-DEFAULT**.  The possibilities are:

  1:TOP* -- leave the uppermost window selected.
  1:BOTTOM* -- leave the lowest window selected.
  1:CURRENT* -- leave the selected window selected.
  1:OTHER* -- select the uppermost window other than the window selected now.

1:CURRENT* is the default.

In any case, a numeric argument tells 1C-X 1* to leave the selected window selected.
If there is no numeric argument, 1C-X 1* will never pick a window whose buffer
is in 1Warnings* mode; it will used the other window.

1[I]* 1C-M-.* is 1Edit Zmacs Command*.

This command asks you to type another editor command, and visits the source
for that command.  The command you visit can include prefix characters such as
1Control-X*, and it can also be an extended command starting with 1Meta-X* or
1Control-Meta-X*.

1[J]* Balance Directories Editor.

In addition to the function 1FS:BALANCE-DIRECTORIES*, which was
implemented in System 89, there is now a ZWEI interface 1M-X BDIRED.*
This command will prompt for two pathnames.  The pathnames default
1to* name and type wild, version newest.  Given the pathnames, two
directory lists are made, and these lists are then compared.  The
resulting directory lists are displayed in normal 1DIRED* format,
one above the other, with "T" next to each file that needs to be
transferred to the other directory.  The algorithm used to determine
which files are to be transferred is identical to that used by
1FS:BALANCE-DIRECTORIES*.

Once the buffer is displayed, you can move around in it in much the
same way as in 1DIRED*.  By means of the T and U commands it is
possible to edit which files will actually get transferred.  The
1End* key terminates the editing process and causes the transfers
to take place.

1[K]* 3COMPILER:COMPILER-VERBOSE*.

If this variable is non-3NIL* the compiler prints a message at the beginning of
each function it compiles, stating the function's name.

1[L]* Compiler Peephole Optimizer1.*

There is now an optimizer that works on the code generated by the compiler.
This should be of no concern to you unless you suspect you are running into a
bug in it.  If so, you can set the variable 1COMPILER:PEEP-ENABLE* to 1NIL* and recompile.

The peephole optimizer is extremely fast, examining few instructions more than once,
so there is no other reason to turn it off.  The code is available to everyone.

1[M]* 1DUMP-FORMS-TO-FILE* Writes A QFASL File.

The function 1DUMP-FORMS-TO-FILE* can be used to construct a QFASL file
that when loaded performs arbitrary actions you specify.  The calling sequence is

1DUMP-FORMS-TO-FILE 2filename forms-list** 3&optional* 2attribute-list*

This writes a QFASL file named 2filename* which contains, in effect, the
forms in 2forms-list*.  That is to say, when the file is loaded, its
effect will the same as evaluating those forms.  Example:

1    (dump-forms-to-file "foo" '((setq x 1) (setq y 2)))
    (load "foo")
    x => 1
    y => 2

2attribute-list** specifies the file attribute list to store in the QFASL file.  It is a
list of alternating keywords and values, and corresponds to the 1-*-* line of a
source file.  The most useful keyword in this context is 3:PACKAGE*, whose
value in the attribute list specifies the package to be used both in dumping the
forms and in loading the file.  If no 3:PACKAGE* keyword is present, the file will
be loaded in whatever package is current at the time.

1[N]* Completion in 1M-X Bug*.  How to Add New Bug Report Lists. 

The editor command 1M-X Bug* now offers completion.  The primary benefit is
that you can type 1Control-?* and get a list of the standard bug reporting lists.

There are two ways you can add another entry to the list of standard bug report
addresses.  The function 1ZWEI:ADD-BUG-REPORT* takes two arguments, the
name to send mail to and a line of documentation (both strings).
Alternatively, you can define a bug report list when you define a system.
Use the 1DEFSYSTEM* keyword 1:BUG-REPORTS* in a list like this
1    (:BUG-REPORTS "MYSYSTEM" "Report a bug in MYSYSTEM.")*
in your 1DEFSYSTEM*.

1[O]* Negative Arguments Allowed in the Rubout Handler.

Yes, you can now do 1Meta-Minus Meta-C.

[P]* Wait States for Chaosnet Connecting Show the Host Name.

1[Q]* New Function 1LISTF* Lists a Directory.

1[R]* 1DISASSEMBLE* Now Describes Any Complex Function Entry Initializations

When you 1DISASSEMBLE* a function which provides complicated default values for
optional arguments or 1&AUX* variables as part of function entry, it used to be hard
to tell what was going on, since the compiled code per se does not explain it.

Now 1DISASSEMBLE* prints a description of such variable binding activities at the
beginning of the output.

1[S]* Better Truncation of Pathnames in the Who Line

You may notice that, when you are doing file i/o, the who line tries to be intelligent
about what part of the filename to truncate from, if it will not all fit.

1[T]* 1(DECLARE (VALUES ...))

(DECLARE (VALUES 2variables...*))* is now equivalent to 1(DECLARE (RETURN-LIST 2variables...*))

[U]* 1GC-IMMEDIATELY*.

This function performs a complete garbage collection, in the current process.
It is the same as the guts of 1SI:FULL-GC*; but it does not reset temporary areas
or run the 1FULL-GC* initializations.  Use 1SI:FULL-GC* before saving a band, and
1GC-IMMEDIATELY* if you are going to continue running the machine.

1[V]* PEEK Improvements.

PEEK no longer does weird things when the 1ABORT* key is typed.

The 1Space* key is now a command that tells PEEK to update its display immediately.

While looking at the display of chaosnet connections, you can mouse on the
words "1Connection to 2contactname**" to get a menu of operations on the
connection, including closing the connection.  You can also mouse on the host name
to get some operations that pertain to the host.

1[W] (teach-zmacs)*

This new function makes a private copy, if needed, of the ZMacs tutorial file 
1SYS: ZWEI: TEACH-ZMACS* 1TEXT*, then enters ZMacs with that file specified.
The tutorial file is based on the one used by Teach-EMACS, with appropriate
changes for the Lisp Machine.

1[X] M-X Teach Zmacs*

This does much the same thing as the function, but from within an editor.
`
A Comparison of the MIT and Symbolics Error Systems

[A] Major Differences

The primary difference between the MIT new error system and the Symbolics
one is that theirs requires defining a new flavor whenever a condition is to
be signaled in a slightly different way.  This required the creation of a very
large number of new flavors, greatly increasing the size of the saved world.
It is also a hassle for the user who wishes to define ways of signaling errors.

In the MIT system it is not necessary to define a new flavor just to represent a
hierarchy of classifications of conditions.  Additional condition names can be
specified in a signal name, instead.

In the Symbolics system, to remember an additional piece of information in a
condition instance, one must define a new flavor and give it an instance variable.
This works in the MIT system as well, but it is easier to use 1DEFSIGNAL* and
the "implicit instance variable" feature.

The cost of this is that in addition to the concept of condition flavor names, one
must know the concepts of condition names and signal names.

Another significant difference is that in the MIT system it is possible to signal
the same condition instance more than once.  The Symbolics documentation says
this is forbidden; I do not know what would happen if you tried.  This is useful
in several ways.  For one thing, it makes the 3ABORT* key much faster.  For
another, the caller of 1OPEN* could call with 1:ERROR NIL* and then possibly resignal
the condition instance that 1OPEN* returns.  A 1CONDITION-CASE* handler can do this
too.  (I don't know yet whether this is useful).  Finally, a condition handler can
recursively signal the condition it is handling; this provides for a clean solution
to the problem of generalizing default handlers, which the Symbolics
documentation throws up its hands at.  (I did not have this in mind when I
decided to make conditions resignalable; my intuition told me it would be good
for something).

Another difference is that the Symbolics system makes restart handlers and
proceeding two ideosyncratically different things.  In the MIT system, restart
handlers are just proceed types that are provided nonlocally rather than by the
immediate signaler.  The 1:USER-PROCEED-TYPES* operation does the same job as
the Symbolics "special commands" feature, but in a simpler and more general
way.

Also, in the MIT system I have tried to preserve the usefulness of the old
error-signaling functions 1FERROR* and 1CERROR*.  Rather than being semi-obsolete,
they are perfectly good ways of signaling errors, using their traditional calling
conventions.  The amount of change required to old programs is thus much less.

[B] Compatibility

Nearly all the condition-2handling* facilities of the MIT system are upward
compatibile with those of the Symbolics system.  The standard system condition
names are also for the most part the same, and the same extra operations and
proceed types are provided.  So no special care is needed to move between
systems for simple programs that only handle conditions.

The main exception is that you must use 1CONDITION-TYPEP* in place of 1TYPEP*,
if you want to look at a condition instance and ask whether it has a certain
condition name.  Also, a few operations work only inside condition handlers,
including 1:PROCEED-TYPES*, because they refer to global variables bound by
1SIGNAL-CONDITION* rather than to instance variables (the condition
instance does not contain information about the particular signaler, since
that would not be reentrant).

For 2signaling* conditions, the MIT system is upward compatible in its general
facilities, with a few exceptions.  Flavors which would work in the Symbolics
system will also work, the same way, in the MIT system.  However, the standard
system conditions, which in the Symbolics system are all flavor names, are
mostly just condition names in the MIT system.  As a result, your flavor
definition may refer to flavors which do not exist.  If this happens, your best bet
is just to 2define the flavor yourself*.  Write a definition which "could have been
used to define it" in the Symbolics system, and use it in the MIT system.
The system will not be confused by the existence of a flavor it does not know
about, with the same name as a condition it is using, and handlers will not care
whether the condition name came from a flavor component or from a 3DEFSIGNAL*.

The MIT system expects signalers to specify exactly which proceed types they
will handle.  The function 3SIGNAL* will still default this as in the Symbolics
system, but the flavor definitions of the MIT system may define more proceed
types than you expect.  Mentioning them explicitly will not hurt.  (I think the
feature of defaulting them is a bad idea).

Many Symbolics ways of doing things exist, undocumented, in the MIT system,
for compatibility only (because more general or better ways of doing those
things are recommended).  These include special commands, the handling of the
1:PROCEED* operation, and 3EH:1INVOKE-RESTART-HANDLERS**.  1DBG:* is accepted as a
synonym of EH:.