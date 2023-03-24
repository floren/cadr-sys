-*-Fonts:TR12,HL12B,TR12I,CPTFONT,TR12B; Mode:Text-*-

System 86 comes with microcode 136.
Microcode 136 will work with system 85.
Microcodes 117, 123 and 124 will work with system 86, modulo minor bugs
that existed for months or years, if you do not use tail recursion.

There are no major incompatibilities in source code between system 85 and
system 86.  System 85 compiled code will also all run in system 86.
86 is very well debugged, and there is little reason for anyone not
to move from 85 to 86 right away.

Major Changes::
   [1] 1DEFSUBST0 functions now run compiled.
   [2] 1:SELF-FLAVOR 0declarations.
   [3] The Mode Line functions and editor commands have been renamed.
   [4] You can now make flavor instances print out so they can read back.
   [5] Tail Recursion!

New Functions:
   [6] 1MAKE-ARRAY0 accepts a new argument keyword 1:INITIAL-VALUE0.
   [7] The function 1(STORE-CONDITIONAL0 2location oldvalue newvalue1)0 now exists.
   [10] 1VIEWF, UNDELETEF, FUNDEFINE 0and 1PRINT-HERALD0 are now in 1GLOBAL.
0   [11] The function 1UNDEFUN0 is improved.
   [12] New functions 1LOCATION-BOUNDP 0and1 LOCATION-MAKUNBOUND.
0   [13] The functions 1SEND0 and1 LEXPR-SEND0 are now defined.
   [14] The special form 1WITH-HELP-STREAM0 creates a stream for a help message.
   [15] 1FS:DIRECTORY-LIST0 and 1FS:DIRECTORY-LIST-STREAM0 are improved.
   [16] Window input operations: 1:ANY-TYI 0and 1:WAIT-FOR-INPUT-WITH-TIMEOUT.
0   [17] 1ZWEI:INTERVAL-WITH-FONTS-IO0 now supports 1:SET-POINTER0 (to 0 only),
   [20] New functions for closures.
   [21] New special forms: 1VARIABLE-BOUNDP, VARIABLE-MAKUNBOUND,0 1VARIABLE-LOCATION0.
   [22] The new function 1TIME-INCREMENT0 adds a number to a time (such as 1(TIME)
0   [23] The new function 1FUNCTION-NAME0, returns the name of any sort of function object.
   [24] The new function 1FS:FLAVOR-ALLOWED-INIT-KEYWORDS0.
   [25] New functions to read and print intervals of time.
   [26] 1SI:NULL-STREAM0.
   [27] How to add new keywords for 1ADD-INITIALIZATION.
0   [28] New macro 1WITH-OPEN-STRING0.
   [29] 1WITH-INPUT-FROM-STRING 0and 1WITH-OUTPUT-TO-STRING0 can now be nested.

Utilities and Operation:
   [@] Compiler warnings are now recorded in a data base of list structure.
   [A] New font handling features in 1PRESS:PRINT-FROM-STREAM0.
   [B] You can specify a filename instead of a buffer name in ZWEI.
   [C] You can easily insert defaults while typing a filename in ZWEI.
   [D] 4M-X Save All Files0 now asks all its questions immediately.
   [E] 4C-M-L0 now operates on all the buffers that you see in a 4List Buffers0.
   [F] ZWEI can display a file while still reading it.
   [G] New ZWEI commands for producing hardcopy:
   [H] Inspector improvements.
   [I] Passwords for file jobs are remembered for you.
   [J] Better DWIM in 1BREAKON, TRACE0 and 1ADVISE0.
   [K] Reloading files from which some methods have been deleted.
   [L] Changes in Terminal key commands.
   [M] Reading multi-font files into ZWEI should be much faster.
   [N] New ZWEI feature for visiting lists of things (callers, etc.)
   [O] There are three new commands for manipulating patches:
   [P] 1DRIBBLE-ALL0 is a new alternate way of saving your terminal i/o.
   [Q] Conversation-motion commands in CONVERSE.
   [R] 1CHAOS:NOTIFY0 and1 CHAOS:NOTIFY-ALL-LMS
0   [S] DIRED features.
   [T] It is no longer necessary to do 1TV:SET-TV-SPEED0.
   [U] Each lisp listener window now has its own binding of 1PACKAGE0.
   [V] 1TV:SET-DEFAULT-FONT0 now works "right".
   [W] New feature to allow shift keys to replace double-clicking.
   [X] New feature in Choose-Variable-Values.
   [Y] Hosts not in the host table can be named, as 1CHAOS|nnnn0.
   [Z] 4Control-Shift-2letter 0and 4Meta-Shift-2letter 0in ZWEI:
   [AA] Help Apropos in ZWEI is now much faster.

The worst incompatibility comes from this feature:

[1] 1DEFSUBST0 functions now run compiled.

You can now compile a 1DEFSUBST0 just like any other function.  The resulting
compiled subst executes with the speed of compiled code when applied, but
the expr definition is still present as well to enable the compiler to open code
explicit calls to the 1DEFSUBST0.

These compiled 1DEFSUBST0s run properly in old systems, but only system 86
knows how to expand them when you compile calls to them.  If you do your
compilation in 86, the resulting binary files should work in earlier systems, but
if you compile a file of definitions in 86, load the result into 85, and compile
other files there which use the definitions, they will fail to be open-coded.  This
can be patched in 85 if people need it.  But it may be easier for you to simply
stop using 85.

Undeclared free variables in 1DEFSUBST0s do not get warnings, because they occur
frequently (default pointers in 1DEFSTRUCTs0, etc.).  Such a 1DEFSUBST0 is reasonable
as long as you intend it only to be expanded; if you call it at run time, an error
will occur then since the variable will be unbound.

Changes in programming:

[2] 1:SELF-FLAVOR 0declarations.

In place of 1DECLARE-FLAVOR-INSTANCE-VARIABLES0 around a function, you
can now use 1(DECLARE (:SELF-FLAVOR0 2flavorname1))0 as the first thing in the body
of the function.  This is cleaner because it allows the 1DEFUN0 itself to be the
top-level list in your file.  While this form of code can only be compiled in
system 86, the compiled code is identical to that produced for
1DECLARE-FLAVOR-INSTANCE-VARIABLES0, and runs at full speed in system 85, and
at lower speed in system 79.

The purpose of this change is to reduce the need to put anything "around" the
1DEFUN0s in source files.

[3] The Mode Line functions and editor commands have been renamed
(to Attribute List).

The list of properties obtained from the "-*-" line at the front of a text file is
now called the file's attribute list rather than its property list.  The functions for
accessing this are now

1  FS:FILE-EXTRACT-ATTRIBUTE-LIST (2stream1)0 return the attribute list
	read from that stream, which should initially be at beginning of file.
        The stream will have its pointer returned to the beginning after the
        operation.  The list is returned as an alternating list of properties
	(interned in 1USER0) and values. 1 :MODE, :PACKAGE, :BASE0 are typical. 
1  FS:FILE-READ-ATTRIBUTE-LIST (2generic-pathname stream1)
0	read the attribute list from the stream, and put each attribute
	as a property on the generic pathname. 
1  FS:FILE-ATTRIBUTE-LIST (2pathname1)
0	return the attribute list of the specified file (text or qfasl). 
1  FS:FILE-ATTRIBUTE-BINDINGS (2generic-pathname1)
0	returns two values describing the bindings to be made before
	reading things from that file.  The two values are a list of
	variables and a list of values.  They can be passed to 1PROGV
0	to do the binding.  Variables bound will typically include
	1PACKAGE, BASE0 and1 IBASE0.

The ZWEI commands 4Update Mode Line0 and 4Reparse Mode Line0 now have new
names 4Update Attributes List0 and4 Reparse Attributes List0.  The former
changes the file's -*- line to contain the current mode, package, fonts, and base.
The latter examines the attribute line and sets the current mode, package, fonts,
and base.  A related new ZWEI command is 4Dired Print File Attributes
0(invoked by 4,0 in DIRED) which prints out the attribute list of the file indicated
by the cursor.

[4] You can now make flavor instances print out so they can read back.

Pathnames use this mechanism; you will see that they print out a little
differently.  You can make use of this feature for any other sort of abstract
object (flavor or class instance, or named structure).  Your object must print
itself out as 1#32typename additional-data30.  (For those not reading this on a Lisp
machine, 30 and 30 are horseshoes).  You can bring this about by defining the
object's1 :PRINT-SELF0 operation to print that way, using whatever additional-data
you need.  2typename0 is a keyword that 1READ0 uses to figure out how to read in
the rest of the printed representation and construct the object.  It is read in in
package 1USER0 (but it can contain a package prefix).  The resulting symbol
should either have a 1SI:READ-INSTANCE0 property or be the name of a flavor
which handles the1 :READ-INSTANCE0 operation.

In the first case, the property is applied as a function to the typename symbol
itself and the input stream.  In the second, the handler for that operation is
applied to the operation name (as always), the typename symbol, and the input
stream (three arguments, but the first is implicit and not mentioned in the
1DEFMETHOD0).  1SELF0 will be 1NIL0 and instance variables should not be used.  In
either case, the handler function should read the remaining data from the stream,
and construct and return the datum it describes.  It should return with the "30"
character waiting to be read from the input stream (1:UNTYI0 it if necessary).  1READ
0will get an error after it is returned to if a "30" character doesn't follow.

The typename can be any symbol with an appropriate property or flavor, not
necessarily related to the type of object that is created; but for clarity, it is good
if it is the same as the 1TYPEP0 of the object printed.  Since the type symbol is
passed to the handler, one flavor's handler can be inherited by many other
flavors and can examine the type symbol read in to decide what flavor to
construct.

[5] Tail Recursion!

The Lisp machine is now tail-recursive if the variable 1TAIL-RECURSION-FLAG0 is
not 1NIL.0  Specifically, any time one function finishes by calling another function,
if no special variables or catches are present in the calling function, the caller's
stack frame will be thrown away.  This means that certain programs will now
execute in bounded stack space which used to require unbounded stack space.
It has an intrinsic drawback: less information is left on the stack to be examined
in the error handler.  It is for this reason that the new mode is optional.

Tail recursion does not take place when
   -- the calling function has bound special variables
   -- the call is within a 1*CATCH0 in the calling function.
   -- the calling function has a &REST argument 
	which lives on the stack.
   -- the calling function has asked for the location of any
	argument or local variable.


New Functions:

[6] 1MAKE-ARRAY0 accepts a new argument keyword 1:INITIAL-VALUE0.

The value supplied with the 1:INITIAL-VALUE0 keyword is stored in every element
of the array.  This uses the new function 1(ARRAY-INITIALIZE0 2array value start end1)
0which is by far the fastest way to propagate one value through an array.  Another
new function is 1ARRAY-RANK0, which is a new name for 1ARRAY-/#-DIMS.

0[7] The function 1(STORE-CONDITIONAL0 2location oldvalue newvalue1)0 now exists.

It stores the new value only if the contents match the old value.  This is a basic
building block for atomic testing-and-modifying operations for interlocking.
**NOTE** if given a list as first argument, it tests and stores into the 1CDR0 of the
list, not its 1CAR0.  This is in accord with the general principle that you should
reference locatives with 1CDR0 rather than 1CAR0: then 1CDR0 of the list pointer gets
you the cdr of the cons cell, and 1CDR0 of the locative pointer gets you the car of
the cons.  Thus, by passing a function either the list pointer or the locative, you
can cause the function, which uses 1CDR0, to refer to either the car or the cdr of
the cell you have in hand.  1(LOCF (CAR ...))0 and 1(LOCF (CDR ...))0 are defined so
as to work with this convention.

[10] 1VIEWF, UNDELETEF, FUNDEFINE 0and 1PRINT-HERALD0 are now in 1GLOBAL.

VIEWF0 prints the contents of a file.  1UNDELETEF0 undeletes a file.  1FUNDEFINE
0undefines any function spec.  1PRINT-HERALD0 prints the things that are printed at
the top of the screen when you boot (system version numbers, machine host
name, etc).

[11] The function 1UNDEFUN0, which reverts a function spec to its previous
definition, now offers to 1FUNDEFINE0 the function spec if it has no previous
definition.

[12] New functions 1LOCATION-BOUNDP 0and1 LOCATION-MAKUNBOUND.

0The function 1LOCATION-BOUNDP0, given a locative, tells you whether the
loction is bound, as opposed to "unbound" (contains 1DTP-NULL0).
1(LOCATION-BOUNDP (VALUE-CELL-LOCATION 2exp1))0 is equivalent to 1(BOUNDP0 2exp1)0.
Similarly, there is 1LOCATION-MAKUNBOUND0, which makes the location be
"unbound".

[13] The functions 1SEND0 and1 LEXPR-SEND0 are now defined.

These are now the preferred names for message-passing to objects.  At present,
they are simply macros which expand into 1FUNCALL0 and 1LEXPR-FUNCALL
0respectively.  It is possible that at some future time additional functionality
might be added to message passing which will require the use of the new
functions.

[14] The special form 1WITH-HELP-STREAM0 creates a stream for use in printing a
long help message.  It takes the form
  1(WITH-HELP-STREAM (2stream1 . 2options1) &BODY 2body1)
0Options are 1:LABEL0 2label0, 1:WIDTH0 2width-var0, 1:HEIGHT0 2height-var0, and 1:SUPERIOR
2superior-window0.  The label should be something appropriate for the message.
The 1WIDTH0 and 1HEIGHT0 options are useful if the program wants to format the
help message based on the size of the window.  The body gets turned into a
1LAMBDA0-expression with the stream and the width and height variables bound
appropriately; note that this means that variables from the surrounding program
that are to be accessed by the body must be special.

[15] 1FS:DIRECTORY-LIST0 and 1FS:DIRECTORY-LIST-STREAM0 are improved.

The directory list element for pathname 1NIL0, which describes the directory as a
whole, now contains a property named 1:PATHNAME0 whose value is the pathname
which the directory list was made from.  If you specify a directory that does not
exist, and type a name to use instead from the terminal, this will be the name
that was actually used.  ZWEI uses this, so that it correctly understands what
directory was ultimately specified in DIRED, etc.

Note how this has no connection with any "new error system".  Having hacked the
error handler recently, I can appreciate some of the benefits to be had from a
new error system, and I plan to implement one as soon as Symbolics has finished
(so it can be compatible in use).  But I also believe it has been used as an
excuse in many contexts where it is not really relevant.

[16] Window input operations: 1:ANY-TYI 0and 1:WAIT-FOR-INPUT-WITH-TIMEOUT.

0The operation 1:ANY-TYI0 is now defined on all windows (all which have 1:TYI0,
that is -- it is in 1TV:STREAM-MIXIN0).  The only thing that 1TV:ANY-TYI-MIXIN0 does
now is cause 1:TYI0 to discard everything but fixnums.  This will eventually
become the default behavior and 1TV:ANY-TYI-MIXIN0 will become a no-op.

The operation 1:WAIT-FOR-INPUT-WITH-TIMEOUT0 is also now defined in
1TV:STREAM-MIXIN0.  This operation takes an argument, an amount of time in
60'ths of a second, and returns in that amount of time or when input is available,
whichever comes first.

[17] 1ZWEI:INTERVAL-WITH-FONTS-IO0 now supports 1:SET-POINTER0 (to 0 only),
1:PATHNAME0, and 1:FRESH-LINE0 messages.  This means these fake files act more like
real ones than before; this might be useful to someone.  (1ZWEI:INTERVAL-IO
0streams have supported these for some time; however, a related bug has been
fixed.) 

[20] New functions for closures.

1CLOSURE-VARIABLES0 returns a list of the
variables bound in a closure; 1EQUAL0 to the first argument used when the closure
was made.

1COPY-CLOSURE0 returns a new closure with the same function, variables andvalues
as the one you provide.  The new closure and the old one do not share bindings.

(1BOUNDP-IN-CLOSURE0 2closure symbol-or-locative1)0 returns 1T0 if that symbol or
locative would be 1BOUNDP0 (or 1LOCATIVE-BOUNDP0) if you were inside the closure.
1MAKUNBOUND-IN-CLOSURE0, with the same arguments, makes it be not bound.
These join the existing functions 1SYMEVAL-IN-CLOSURE0 and 1SET-IN-CLOSURE0.
All four can read or write the current binding of the variable if the closure does
not bind it.

[21] New special forms: 1VARIABLE-BOUNDP, VARIABLE-MAKUNBOUND,0 1VARIABLE-LOCATION0.

1(VARIABLE-LOCATION &QUOTE0 2variable-name1)0 returns a locative pointing to where
the value of the variable is stored.  This used to be written as
1(VALUE-CELL-LOCATION0 1'2variable-name1)0, but it is really incorrect for
1VALUE-CELL-LOCATION0 to know about local variables.  It should always refer to
the symbol's value cell, like 1SYMEVAL0 and 1SET0.  For now, it still does know about
local variables, but the compiler warns you if you use it that way.

Similarly, use 1(VARIABLE-BOUNDP0 2variable-name1)0 to test whether a variable is
bound.  For ordinary local variables, this is not useful, since the answer is always
1T0; but for flavor instance variables it is useful.  1(BOUNDP0 1'2variable-name1)
0continues to work, but elicits a warning from the compiler.
1VARIABLE-MAKUNBOUND0 also exists.

Compiled code using these new special forms will run properly in earlier
systems even though the special forms do not exist in those systems.

[22] The new function 1TIME-INCREMENT0 adds a number to a time (such as 1(TIME)
0returns) to get a new time.  The number is in units of 1/60 second.  This
function joins 1TIME-DIFFERENCE0 and1 TIME-LESSP0.

[23] The new function 1FUNCTION-NAME0 returns the name of any sort of function.

If the argument is a function object for which a name can be determined,
1FUNCTION-NAME 0returns that name; otherwise, it returns the original object.
This is useful for printing messages about a function.  The second value
returned is 1T0 if a name was actually found.  A second optional argument, if
non-nil, says that if the first argument is an instance of a flavor, the flavor name
should be returned as the function name.  This is not always desirable, because
the flavor name doesn't identify the particular instance.

[24] 1FS:FLAVOR-ALLOWED-INIT-KEYWORDS0.

The new function 1FS:FLAVOR-ALLOWED-INIT-KEYWORDS0 returns a list of all the
init keywords which a specified flavor understands.  The argument is a flavor
name (a symbol).

[25] New functions to read and print intervals of time.

There are new functions for reading and printing time intervals.  They convert
between strings of the form "3 minutes 23 seconds" and fixnums representing
numbers of seconds.  Here are the details:

1TIME:PRINT-INTERVAL-OR-NEVER0 2interval1 &OPTIONAL (2stream standard-output1)
0    2interval0 should be a non-negative fixnum, or 1NIL0.  This function
    prints 2interval0's representation as a time interval onto 2stream0. 
    If 2interval0 is 1NIL0, it prints "Never".

1TIME:PARSE-INTERVAL-OR-NEVER0 2string1 &OPTIONAL0 2start end
0    2string0 is the character string representation of an interval of time. 
    2start0 and 2end0 specify a substring of 2string0 to be parsed; they
    default to the beginning and end of 2string0, respectively.  The
    function returns a fixnum if 2string0 represented an interval, or 1NIL0 if
    2string0 represented "never".  If 2string0 is anything else, an error is
    signalled.  Here are some examples of acceptable strings:

        "4 seconds" "4 secs" "4 s"
        "5 mins 23 secs" "5 m 23 s" "23 SECONDS 5 M"
        "3 yrs 1 week 1 hr 2 mins 1 sec"
        "never" "not ever" "no" ""

    Note that several abbreviations are understood, the components may be in
    any order, and case (upper versus lower) is ignored.  Also, "months" are
    not recognized, since various months have different lengths and there is
    no way to know which month is being spoken of.  This function will
    always accept anything that was produced by 1TIME:PRINT-INTERVAL-OR-NEVER0;
    furthermore, it will return exactly the same fixnum (or 1NIL0) that was printed.

1TIME:READ-INTERVAL-OR-NEVER &OPTIONAL (2stream standard-input1)
0    This function reads a line of input from 2stream0 (using 1READLINE0) and
    then calls 1TIME:PARSE-INTERVAL-OR-NEVER0 on the resulting string.

[26] 1SI:NULL-STREAM0.

1SI:NULL-STREAM0 (the symbol itself, not its value) can be used as an I/O
stream which discards all output and returns immediate EOF on input.

[27] How to add new keywords for 1ADD-INITIALIZATION.

0If you want to add new keywords that can be understood by 1ADD-INITIALIZATION
0and the other initialization functions, you can do so by pushing a new element
onto the following variable:

1SI:INITIALIZATION-KEYWORDS0 [Variable]
    Each element on this list defines the name of one initialization list. 
    Each element is a list of two or three elements.  The first is the
    keyword symbol that names the initialization list.  The second is a
    special variable, whose value is the initialization list itself.  The
    third, if present, is a symbol defining the default "time" at which
    initializations added to this list should be evaluated; it should be
    1SI:NORMAL, SI:NOW, SI:FIRST0, or1 SI:REDO0.  This third element just acts
    as a default; if the list of keywords passed to 1ADD-INITIALIZATION
0    contains one of the keywords 1NORMAL, NOW, FIRST0, or1 REDO0, it will
    override this default.  If the third element is not present, it is as if
    the third element were 1SI:NORMAL0.

Note that the "keywords" used in 1ADD-INITIALIZATION0 need not be
keyword-package symbols (you are allowed to use 1FIRST0 as well as 1:FIRST0),
because 1STRING-EQUAL0 is used to recognize the symbols.

In addition to the system initialization lists mentioned in Revision 4 of the Lisp
Machine Manual (on p. 490), there is a new initialization lists called 1SITE0.  This
list is executed by the function 1SI:UPDATE-SITE-CONFIGURATION-INFO0 just after it
loads the site table.

Of the pre-defined system initialization lists, the default "time" for almost all of
them is 1NORMAL0, with the following exceptions: the 1SYSTEM0 and 1ONCE
0initialization lists default to 1FIRST0, and the 1SITE0 initialization list defaults to 1NOW0.

[28] New macro 1WITH-OPEN-STRING.

(WITH-OPEN-STRING (2stream string . options) 1&BODY 2body1)
0creates a stream associated with the specified string.  This takes a keyword
argument list instead of the positional arguments of 1WITH-INPUT-FROM-STRING
0and 1WITH-OUTPUT-TO-STRING0, and permits the creation of bidirectional string
streams.  (The other macros expand into this now.)

[29] 1WITH-INPUT-FROM-STRING 0and 1WITH-OUTPUT-TO-STRING 0can be nested.

String input and output has been reimplemented based on select-methods and
closures.  (The stream given to the user is now a closure.)  This means that
these forms can be nested as desired, and the streams can be passed downward
(the closures are not killed in any way by unwind-protects).  The new
implementation also supports 1:READ-POINTER0 and 1:SET-POINTER 0operations.
(Read the comments in AI: LISPM2; STRING > for more information.)

Changes in utilities and operating the machine:

[@] Compiler warnings are now recorded in a data base of list structure.

Each time a function is compiled, an entry for that function is made in the
compiler warnings data base or updated.  Warnings from the latest compilation go
in the data base and warnings from previous compilations that did not happen
this time are thrown away.

The command 4Edit Warnings0 asks you, for each file that has warnings, whether
you want to edit that file's warnings.  Then, it fills the buffer 4*Warnings* 0with
an up-to-date list of warnings for those files and starts you stepping through
them.  You use the two commands 4C-Shift-W0 and4 M-Shift-W0 to go to the next
or previous warning.  The command 4Edit File Warnings0 edits the warnings for
one file only; you specify the file.  4Edit System Warnings0 edits the warnings for
all the source files of a system; you specify the system.

If you visit the buffer 4*Warnings*0, you can go to any warning and type 4C-/ 0to
visit the code for that warning.

You can also do 4Insert Warnings0 to insert a complete current list of warnings in
the buffer at point.  The mark is placed after the end, though the region is not
turned on.  4Insert File Warnings0 inserts the warnings for one file only.

Meanwhile, errors in reading or macro expansion during compilation now
produce warnings.  They do not go to the error handler.  This is so your
compilation will finish and give you the rest of the errors in the file.  If you
need to go to the error handler, as before, set the variable
1COMPILER:WARN-ON-ERRORS0 to1 NIL0 and recompile.

You can write part of the compiler warnings data base to a file for later
reloading.  Use 1SI:PRINT-WARNINGS (2list-of-files stream1)0.  The1 :BATCH0 option of
1MAKE-SYSTEM0 now writes the data base in this fashion to a file whose name you
are asked for.  Reload the warnings written in the file, if necessary, with
1READFILE0 or 4M-X Load File0.

The warnings data base is not only for compilation.  To learn how to interface to
it, see the file LISPM;QNEW.

[A] New font handling features in 1PRESS:PRINT-FROM-STREAM
0(and therefore 1PRESS:PRINT-FILE, PRESS:SPOOL-FILE0, 4M-X Print File0, 4M-X Print Buffer, 
M-X0 4Print All Buffers, Hyper-Meta-P, 0the 4P0 command in DIRED, etc.):

    If the file being printed has a 1FONTS0 attribute, and you have not
    specified a 1FONT0 or 1FONT-LIST0 explicitly, 1PRINT-FROM-STREAM0 will attempt
    to do the right thing by reading your 1FONTS0 list.  What it does is:
    
    1.  If your file only specifies one font, it is treated as 1(:FONT0 2font1)0. 
	This causes the whole file to be printed in an appropriate font. 
    2.  If your file specifies more than one font, is is treated as
	1(:FONT-LIST0 1(2list-of-fonts1)).  0This causes epsilon (code 006)
        characters in the file (such as ZWEI outputs for a fontified file)
        to be treated specially, so everything is printed in the correct font. 
    
    The system associates Lisp Machine fonts with Dover fonts by looking
    them up in the A-list 1PRESS:DOVER-FONT-EQUIVALENCES0.  It currently knows
    about most of the popular Lisp Machine text fonts.  Feel free to extend
    it to cover additional fonts.

[B] You can specify a filename instead of a buffer name in ZWEI.

When ZWEI asks you for a buffer name, you can now type 4C-Shift-F
0and then a filename (with defaulting and completion available as usual). 
The buffer containing that file is used.  The file is read in if necessary.

[C] You can easily insert defaults while typing a filename in ZWEI.

Whenever ZWEI asks you for a file name, you can now type 4C-Shift-Y
0to yank the default value (as mentioned in the prompt) into the minibuffer,
or 4M-Shift-Y0 to yank the last filename you typed previously (as you had
typed it).  Previously, ZWEI used to push these onto the kill ring
temporarily.  This interfered with moving text between the mini buffer
and top level on the kill ring.

[D] 4M-X Save All Files0 now asks all its questions immediately,
and then does whatever saving you requested.

[E] 4C-M-L0 now operates on all the buffers that you see in a 4List Buffers0,
with the exception of DIREDs and MAILs that you have exited normally
(4Q0 in DIRED, 4End0 in MAIL).  And 4List Buffers0 (4C-X C-B0) shows you
the same order of buffers that 4C-M-L0 rotates.

[F] ZWEI can display a file while still reading it.

If you set the variable 1ZWEI:*FIND-FILE-EARLY-SELECT*0 to non-1NIL0,
then when you visit a new file in ZWEI, the beginning of the file will
be displayed as soon as enough of the file is read in.  Editing commands
will not be executed, though, until the file is all in.  Also, if the
file is being read in as part of some more complicated command (such as
Meta-.) then this display will not take place, since the beginning of
the file may be the wrong part to display.

[G] New ZWEI commands for producing hardcopy:

4M-X Print Buffer0	4      0Sends a buffer to the Dover.  Prompts in4 0minibuffer. 
4M-X Print All Buffers   0Asks if you want each of your buffers printed,
			      then prints the ones you asked for. 
4Hyper-Meta-P0	4      (Quick Print Buffer) 0Prints the current buffer.

The file LMLIB;DOVERB is now obsolete.

[H] Inspector improvements.

The inspector now has keyboard commands to do
the things that the menu does.  In particular, you can modify a field
by selecting it with the mouse while holding down the Hyper key.  You
can return by typing End; the value of 1*0 is returned from the function
1INSPECT0 if you had reached the inspector that way.  Type Help to find
out more about keyboard commands.

While in the inspector, the variables 1*0, 1**0 and 1***0 have as their
values the three objects being displayed in the inspection panes.  1*
0is the bottom one and 1***0 is the top one.

Lists are now displayed in a more appropriate format, and very large
or deep lists are truncated.  The ellipses "3...0" and "1**0" replace what
is not displayed.  Click on them to see what they abbreviate.

[I] Passwords for file jobs are remembered for you.

The passwords you specify when you connect to file servers are now
remembered on 1FS:USER-HOST-PASSWORD-ALIST0 for the duration of your session
unless you set 1FS:RECORD-PASSWORDS-FLAG0 to1 NIL0.  In any case, any
passwords which are on the alist (perhaps from your init file) will be
used rather than asking you for one.  If you are super paranoid,
you can set the alist to 1NIL0 in your init file as well as setting
the flag to 1NIL0.

[J] Better DWIM for functions in the wrong package in 1BREAKON, TRACE0 and1 ADVISE0.

If 1BREAKON, TRACE0 or1 ADVISE0 says the function is not defined and
offers you some other function to use instead, it now works to say
yes.

[K] Reloading files from which some methods have been deleted.

If you load a file and some methods that were previously defined in
that file are no longer there, you are asked whether to undefine those
methods.  Undefining the methods may be vital to cause other methods
to be inherited where they should be.

[L] Changes in Terminal key commands.

Terminal M now sets the selected window's **more** processing
enable rather than the global one.  Terminal >= (Top-M) switches the
mouse screen.

[M] Reading multi-font files into ZWEI should be much faster.

[N] New ZWEI feature for visiting lists of things (callers, etc.)

ZWEI commands such as 4List Callers, List Changed Functions0, etc. 
which give you a list of functions to look at now put the list in a
buffer called 4*Possibilities*0.  The command 4C-Shift-P0 goes to the next
possibility in the list; or you can visit the buffer yourself and type
4C-/ 0to visit the possibility you are positioned at.  You can also move
the pointer in that buffer and go back to stepping through with 4C-Shift-P0. 
As a result, once you have waited for a 4List Callers0 to be done, the
list remains available to use whenever you wish.

4Meta-Period0 also uses this mechanism when there are multiple
definitions.  4Meta-Period0 with argument is what goes to the next
possibility in this case, and the definitions are stored in the buffer
4*Definitions*0.

[O] There are three new commands for manipulating patches:
4M-X Start Patch, Resume Patch0 and4 Cancel Patch0. 

4Start Patch0 begins a patch, like 4Add Patch0, but does not put anything in it. 
This is nearly equivalent to doing 4Add Patch0 with an empty region. 
4Resume Patch0 resumes editing a patch which you started but did not finish
(on another machine, or before cold booting).  You specify both the
name of the system to patch, and the patch version to resume working on. 
If you had saved the patch buffer before booting, you start off with the
latest existing version of the patch file source. 
4Cancel Patch0, when you are in the middle of making a patch, removes
that patch from the data base; it cancels having begun to make a patch. 
This is the same as Symbolics's 4Abort Patch0, but that name interferes with
the completion of 4Add Patch0, so I believe this name is superior.

[P] 1DRIBBLE-ALL0 is a new alternate way of saving your terminal i/o.

The new function 1DRIBBLE-ALL0 opens a dribble file in which all I/O
in the current process, including break loops, queries and errors,
will be recorded.  Close it with 1DRIBBLE-END0 like an ordinary dribble
file.

[Q] Conversation-motion commands in CONVERSE.

In CONVERSE, the commands to move up and down by conversations are
now 4M-{ 0and 4M-}0.  The 4M-[0 and 4M-]0 keys are left as the usual paragraph
commands.  The comversation movers are improved, in that they move
forward or back until reaching the spot at the end of a To: line.

[R] 1CHAOS:NOTIFY0 and1 CHAOS:NOTIFY-ALL-LMS

0Symbolics generously contributed its functions 1(CHAOS:NOTIFY0 host1 &OPTIONAL
0message1)0 and 1(CHAOS:NOTIFY-ALL-LMS &OPTIONAL0 message1)0 The first sends a
notification to the specified host, with the specified message (or one read from
the terminal if you omit it).  The second sends the same notification to all lisp
machines.  Note that the function 1CHAOS:SHOUT0 is still supported; however, its
useis discouraged, since shouting at System 79 users will throw them into
Converse!  I assume that, by putting these functions in our files, they agree to
their unlimited redistribution.

[S] DIRED features.

When you exit DIRED, you can now type 4E0 as an alternative to 4Y0, 4N0 or 4Q0. 
4E0 means go ahead and delete the files (like 4Q0), then expunge the directory. 
The new command "1,0" in DIRED prints out the attribute list of the file you
are pointing at.

[T] It is no longer necessary to do 1TV:SET-TV-SPEED0.

60hz is now the default.  This speed works fine on CPT monitors as
well as VMI's (which was true in earlier system versions as well. 
CPTs are happy at any speed, during their short lifetimes).

[U] Each lisp listener window now has its own binding of 1PACKAGE0.

The window remembers its top-level binding of 1PACKAGE0 in an instance
variable, and provides operations 1:PACKAGE0 and1 :SET-PACKAGE0.  You can
change the package either by doing 1(PKG-GOTO ...) 0in the listener's
process or by sending a 1:SET-PACKAGE0 message.  The process and window
exchange information just before each time the process does a
top-level 1READ0.  The global binding of 1PACKAGE0, which is still used
for random background processes, is also the initial package for newly
created lisp listeners.  You can set it, as before, by doing
1(PROCESS-RUN-FUNCTION0 "Foo"1 'PKG-GOTO0 2package-or-refname1)

0[V] 1TV:SET-DEFAULT-FONT0 now works "right".

I believe that all existing windows are notified and adjusted properly
for the new font.  Note that it makes a difference whether a window is
using teh default font or using a specific font which happens to be
the default at the moment; 1TV:SET-DEFAULT-FONT0 will affect the former
but not the latter.  That is, dependency information is preserved.

Note however that menus and labels do not use the default font. 
They use the standard fonts for menus and labels.  You can set those
standard fonts by doing 1(TV:SET-STANDARD-FONT ':MENU0 2font-descriptor1)
0or likewise for 1':LABEL. TV:SET-DEFAULT-FONT0 simply does
1(TV:SET-STANDARD-FONT ':DEFAULT ...). 0These standard font names are
looked up on the font alist of the screen in use.

[W] New feature to allow shift keys to replace double-clicking.

For those who don't like double-clicking of mouse buttons, there is a
new feature.  If you click a mouse button while holding down any of the
keys labelled SHIFT, CONTROL, or HYPER, your click will be interpreted
as a double-click.  Furthermore, if you set 1TV:MOUSE-DOUBLE-CLICK-TIME
0to 1NIL0, then double-clicking in the usual way will be disabled, and the
system will no longer wait, after you click, to see whether you are
going to click again.  This can improve mouse-click response time.

This feature is under control of the following variable.  You can set
this variable to 1NIL0 if you want to turn off the new behavior (you might
want to 1LOGIN-SETQ0 it in your INIT file).

1TV:*MOUSE-INCREMENTING-KEYSTATES*0 [Variable]
    The value is a list of the names of those shifting keys which, if held
    down, will make a single-click of the mouse into a double-click.  Its
    default value is 1(:SHIFT :CONTROL :HYPER)0.

Holding down two of these keys produces a triple click.  This was
inspired by the name of the variable; perhaps it works in Brand S
also.

[X] New feature in Choose-Variable-Values.

The Choose-Variable-Values facility has been extended so that the
variable specification can be a locative or a cons cell instead of a
special variable.  This lets arbitrary cells in the machine be examined
and modified by Choose-Variable-Values.  (If a cons cell is passed, its
CAR is the memory location used, and the long form of specification
must be used to avoid syntactic ambiguity.)

[Y] Hosts not in the host table can be named, as 1CHAOS|nnnn0.

It is now possible to name hosts that are not in the host table.  The address
1CHAOS|nnnn0, where "nnnn" is an octal numeral (of any length), refers to the host
with that number on the Chaosnet.  For example, 1CHAOS|4010 and 1CHAOS|17003
0are valid addresses.  Normally, all hosts at your site are in the host table and
have mnemonic names, so you shouldn't need to use this syntax.  However, it
comes in useful if your host table is incorrect or out-of-date, in which case you
may need to communicate with a host that's not in your host table.
4 
0[Z] 4Control-Shift-2letter 0and 4Meta-Shift-2letter 0in ZWEI:

This isn't new, but since I've never seen it documented anywhere, folks might
like to know that in ZWEI 4Control-Shift-2letter 0= 4Hyper-Control-2letter0, and
4Meta-Shift-2letter 0= 4Hyper-Meta-2letter0.  If you 2type1 Control-Shift- 0it is internally
translated into 1Hyper-Control-0, and the latter is what you must use when you
call 1SET-COMTAB 0to redefine a character.1 

0[AA] Help Apropos in ZWEI is now much faster.2  0Also, if a command is not
otherwise assigned, it now tells you that it can be invoked via 4C-M-X0, instead of
simply saying that is is not on any keys.
