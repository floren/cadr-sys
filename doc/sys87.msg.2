-*-Fonts:TR12,cptfont,TR12I,CPTFONT,TR12B; Mode:Text-*-

System 87 comes with microcode 164.  It does not run with previous microcodes.
Microcode 164 will work with system 86, however.

There are no major incompatibilities between systems 86 and 87.

OZ is now the default host for bug reports, and is also the associated
machine for all the machines which used to have AI.

Programming changes:

   [1] A new system for read-time and run-time conditionalization.
   [2] Array reference functions.
   [3] Changes in hash tables.
   [4] Changes in areas and explicit freeing of storage:
   [5] 2:1MINI-BUFFER-OR-READLINE0 is now allowed as a 2type0 for 1FQUERY0.
   [6] 1YES-OR-NO-P0 and1 Y-OR-N-P0 now print 1(Y or N)0 or 1(Yes or No)0.
   [7] The function for reading a file attribute list from a stream is now
       called 1FS:READ-ATTRIBUTE-LIST0.
   [8] New function 1FS:CREATE-DIRECTORY0 to create a directory on a file
       server.
   [9] New function 1FS:REMOTE-CONNECT0 performs the Twenex CONNECT or
       ACCESS operations, as far as access control is concerned.
   [10]1 ZWEI:FIND-FILE0 now returns the buffer containing that file.
   [11] A new type of method, the 1:AROUND0 method, has been defined.
   [12] 1DOLIST0 and1 DOTIMES0 can return values.
   [13] New conditional form 1SELECT-MATCH0.
   [14] 1**MORE**0 processing and typeout windows.
   [15] 1WHEN0 and 1UNLESS0.
   [16] Changes in 1SXHASH0.
   [17] Transcendental functions of small flonums
   [18] 1LET-GLOBALLY0 works on initially-unbound variables.

[1] A new system for read-time and run-time conditionalization for
different systems and sites has been contributed by Symbolics.

Read time conditionalization of an expression is done by writing
1#+2feature expression 0or 1#-2feature expression0.  The former causes
2expression0 to be read in only if 2feature0 is present; the latter, only if
2feature0 is absent.  This much is the same as before.  What is changed
is which features you can test in this way.

The features you can test for now include the types of systems (1MIT 0and
1SYMBOLICS0) and the types of network software (1CHAOS0 and 1ETHER0).

You can no longer use a read-time conditional to test for a particular
site.  Instead, you should use a run-time conditional on the value of
the variable 1SITE-NAME0, which will be a symbol in the keyword package:
1:MIT0 if you are running at1 0MIT.

The motivation for this is that you will probably have to recompile your
files anyway to move to a different type of system, but ordinarily the
same compiled files will run at any site.  So read-time conditionals,
which are checked only when a file is compiled, not when it is run,
bring no extra problems if they are used for different system types,
but should not be used for distinguishing sites.

[2] Array reference functions:

Array references are now faster.  Also,1 ALOC0 and1 AP-10, etc., will no
longer examine the contents of the slot you are getting a locative to.
This prevents spurious errors.

There are new array functions 1AR-1-FORCE0, 1AS-1-FORCE0 and1 AP-1-FORCE
0that read, set or get a locative to an array element using a single
subscript regardless of the rank of the array.  This is equivalent
to making a one-dimensional indirect array pointing to the original array
and indexing through that; but much faster and without consing.

1ARRAY-RANK0 is a new name for1 ARRAY-/#-DIMS

0[3] Changes in hash tables:

EQUAL and EQ hash tables are now nearly the same kind of object,
and are accessed using the same functions.

You still use 1MAKE-EQUAL-HASH-TABLE0 to make an EQUAL hash table and
1MAKE-HASH-TABLE0 to make an EQ hash table, but from then on you use the
same functions (1GETHASH0,1 PUTHASH0, etc) for both of them.  The old function
names for EQUAL hash tables, 1GETHASH-EQUAL0 and so on, still exist as synonyms
for the other names.

A new function, 1MAPHASH-RETURN0, maps a function over the items in a hash table
like1 MAPHASH0, and returns a list of all the values returned by the function.
So 1MAPHASH-RETURN0 is to1 MAPHASH0 as1 MAPCAR0 is to1 MAPC0.

The arguments to 1MAPHASH-RETURN0 are a function and a hash table.
Each time the function is called, its arguments are one hash key
and the associated value or values stored in the hash table.

[4] Changes in areas and explicit freeing of storage:

1RESET-TEMPORARY-AREA0 is only allowed on areas that have
been marked as "temporary".  You mark an area as temporary by calling
1MAKE-AREA-TEMPORARY0 on the area number, or by specifying 1':GC ':TEMPORARY
0in1 MAKE-AREA0.  If you reset a non-temporary area, you get an error.
If you continue, with 1Resume0, the area is marked temporary for you.

1RETURN-ARRAY0 has been renamed to1 RETURN-STORAGE0.  It will now return
the storage of any type of object.

It is now possible to grow an area to be more than half of the address space.
In more detail, an area for which no maximum size was specified (and
which therefore has the largest positive fixnum as its maximum size)
is now allowed to grow without limit.

[5] 1:MINI-BUFFER-OR-READLINE0 is now allowed as a2 type0 for 1FQUERY0.
It behaves just like the type1 :READLINE0, except that if used inside
the editor, it uses a minibuffer to read in the line.

[6] 1YES-OR-NO-P0 and1 Y-OR-N-P0 now by default list the choices you can type:
either 1Y or N0, or1 Yes or No0.  This is a change in the first argument which
those functions pass to 1FQUERY0.  Some old calls to 1Y-OR-N-P0 or1 YES-OR-NO-P
0may include "(Y or N)" or something similar as part of the prompt string.
They should be changed.

[7] The function for reading a file attribute list from a stream is now
called1 FS:READ-ATTRIBUTE-LIST0, for compatibility with brand S.
This was called 1FS:FILE-READ-PROPERTY-LIST0 in system 79, and was
1FS:FILE-READ-ATTRIBUTE-LIST0 in system 86.  Both old names will continue to work.

[8] The function1 FS:CREATE-DIRECTORY0 takes a pathname specifying a directory,
and an optional keyword arg1 :ERROR0 (default1 T0), and creates a directory
on a file server.  This is implemented in the Twenex file server.

[9] The function 1FS:REMOTE-CONNECT0 takes a pathname specifying a directory,
and two optional keyword arguments 1:ERROR0 (default1 T0) and1 :ACCESS0 (default1 NIL0),
and tells the file server (on the host specified in the pathname)
to do a Connect or an Access to the device/directory specified in the
pathname.  You will be asked for a password if one is needed.
Passwords are remembered once specified, unless you have turned off
that feature for login passwords.  In addition, your connected or accessed
directory is remembered even if the file server connection is deleted
for lack of use.

Note that this operation only affects access control.  The file server's
connected directory is never used by system 86 or 87 to deteremine
what file you will get.  If you wish to "connect" to specify a default,
you should instead use the "working directory" feature, which does the same
job but operates entirely within the Lisp machine and works uniformly for
all types of file servers.  See item [a], below.

[10]1 ZWEI:FIND-FILE0 now returns the buffer containing that file (after
reading the file in if necessary).  This is useful if you give1 NIL0 as a
second argument, meaning don't select the buffer.

[11] A new type of method, the 1:AROUND0 method, has been defined.  An
1:AROUND0 method specifies code to be executed around the execution of the
other methods.  In this respect it is like a wrapper.  However, a
wrapper is a kind of macro; it computes code to be executed later.  The
1:AROUND0 method is an ordinary function, executed at run time, like most
other methods.

1:AROUND0 methods are executed base flavor innermost, interspersed with any
wrappers.  If one flavor provides both a wrapper and a 1:AROUND0 method, the
1:AROUND0 method goes inside.

An1 :AROUND0 method receives three special arguments, which come before the
arguments actually passed in the message.  These are customarily called
2continuation0,2 data10 and2 data20.

2continuation0 is a function, like a combined method, which calls the
remaining methods.2  data10 is the mapping table for continuation to use.
2data20 is just the message being processed -- a list of all the arguments
sent to the flavor instance, starting with the operation name.

The1 :AROUND0 method does 1(AROUND-METHOD-CONTINUE 2continuation0 2data1 data21)
0to call the continuation and pass it the same arguments that were supplied.
When the continuation returns to the 1:AROUND0 method, it can do additional work.
The1 :AROUND0 method can propagate the values returned by the continuation, or
it can return its own values.

In a sense,2 data20 is redundant, in that you could construct it yourself
from the individual arguments to the1 :AROUND0 method.  The list is passed
separately as a convenience.  Sometimes it is useful to pass the
continuation slightly modified arguments.  This is done by writing
1(FUNCALL-WITH-MAPPING-TABLE2 continuation data10 '2operation any-other-args0...)
where you must use the individual arguments of the1 :AROUND0 method to make
up the remaining args.  1LEXPR-FUNCALL-WITH-MAPPING-TABLE0 also exists.

Here is an 1:AROUND0 method example

3(defmethod (shift-lines-left-mixin :draw-line) (continuation mapping-table ignore
0							        3from-x from-y to-x to-y
0								3&rest pass-along)
  (lexpr-funcall-with-mapping-table continuation mapping-table
0						3(1+ from-x) from-y
0						3(1+ to-x) to-y
0						3pass-along))

0This mixin, combined into a window flavor, would cause lines drawn with
the 1:DRAW-LINE0 operation to come out one pixel to the left of where they
normally would.

Definitions of 1DEFWHOPPER0, 1CONTINUE-WHOPPER0 and 3LEXPR-1CONTINUE-WHOPPER
0in terms of3 1:AROUND0 methods are provided for compatibility with brand S.

[12]1 DOLIST0 and1 DOTIMES0 can now return values on normal termination.

In1 (DOTIMES (2counter maximum value-expression1)0 2body1)0, when the value of
2counter0 reaches that of 2maximum0, 2value-expression0 is evaluated and its
value(s) returned.  Previously, 1DOTIMES0 always returned 1NIL0.

The same feature is available in 1DOLIST0.
1(DOLIST (2variable list value-expression1)2 body1)0 returns the value(s) of
2value-expression0 when2 list0 is exhausted.

[13] A new conditional form 1SELECT-MATCH0 is installed.
This was invented by AGRE, who called it 1SELECTP0.
It resembles1 SELECT0, except that each clause can specify a pattern
to match the select-value against.

The syntax is 
1   (SELECT-MATCH0 2object
0     1(2pattern condition sexp0 ... 2sexp1)
0     1(2pattern condition sexp0 ... 2sexp1)
0     ...
     1(2pattern condition sexp0 ... 2sexp1)
0     1(OTHERWISE 2sexp0 ...2 sexp1))

0The value of 2object0 is matched against the 2pattern0's one at a time until a
match succeeds and the accompanying 2condition0 evaluates to something
non-null, at which point the 2sexp0's associated with the 2pattern0 and
2condition0 are evaluated one at a time and the last value is returned.  The
2pattern0's can be arbitrary s-expressions, with variables signified by
1#?2variable0.  When the pattern is matched against the 2object0's value, the
variables are to bound to their matched values.  Different occurences of the
same variable in a given pattern must match to the same thing, so that

3    (SELECT-MATCH '(A B C) 
      ((#?X B #?X) T 'LOSE)
      ((#?X B #?Y) T 'WIN)
      (OTHERWISE 'LOSE-BIG))

0returns 1WIN0.  The variables mentioned in the 2pattern0's need not be bound by
the user; they are bound by the expression resulting from the expansion of the
macro.  (The variable 1#?IGNORE0 matches everything and doesn't become bound.
Using 1#?IGNORE0 for a variable you don't intend to make any use of avoids
"bound but not used" warnings from the compiler.)

[14]  1**MORE** 0processing and typeout windows.

A typeout window now shares a single 1**MORE**0 processing enable flag with its
superior window.  This is so that the user need not be aware of the distinction
between the typeout window and its superior, when he uses the 1Terminal M
0command.  For example, typing 1Terminal M0 when an editor window is selected
complements 1**MORE**0 processing for typeout in that window, and the user need
not be aware that typeout actually happens in a different window called a
typeout window.

The implementor is not so lucky.  Many types of windows that are used for
display rather than for sequential output have been written to turn off 1**MORE**
0processing by setting the init-option 1:MORE-P0 to 1nil0.   This now has the effect
of turning off 1**MORE**0 processing in the typeout window as well.  Furthermore,
if the user turns that back on with 1Terminal M0, he turns on 1**MORE**0 processing
in the main window as well, which may interfere with its display updating.

The correct solution is to eliminate 1**MORE**0 processing in the superior window
regardless of the state of that window's 1**MORE**0-enable flag.  This can be done
in two ways:

This method causes3 **MORE**0's, when they happen in the main window, to do nothing.

3(defmethod (my-display-window-with-typeout-window :more-exception) ()
  (setf (tv:sheet-more-flag) 0))

0The other method is to mix the new mixin 1TV:INTRINSIC-NO-MORE-MIXIN 0into
your flavor of window.  This redefines the 1:MORE-P 0and1 :SET-MORE-P 0operations
to operate on a separate flag which exists only to remember 1Terminal0 1M0 and
transmit it to inferior windows; at the same time, it initializes the "real"
1**MORE**-0enable flag to 1nil0.

[15] 1WHEN0 and 1UNLESS0.

1WHEN0 and 1UNLESS0 are two simple new control structure
macros.  The first argument is a condition expression, and the
remainder is a body to be executed if the condition is true (for
1WHEN0) or if it is 1NIL0 (for 1UNLESS0).

Using one of these macros is equivalent to writing an 1AND0 or an
1OR0 with a 1PROGN0 as its second argument.

[16] Changes in 1SXHASH0.

When 1SXHASH0 is given a named structure or a flavor instance, it
offers the object a chance to supply its own hash code by handling the
1:SXHASH0 operation.  The value returned should be the hash code,
a nonnegative fixnum.

If 1SXHASH0 is given a non=1NIL0 second argument, this says
that the address of an object may be used as its hash code if no other
means of computing a hash code is available.  The kinds of objects for
which this is done are incomprehensible to 1EQUAL0, so it is still
true that the 1SXHASH0es of two objects that are equal are the
same.  However, if you use this option, you must be prepared to deal
with hash codes changing due to garbage collection.

If the object is an array and the second argument is absent or
1NIL0, the length of the array is now used as its hash code.
It's better than always using zero.

[17] Transcendental functions of small flonums

The functions 1SQRT0, 1SIN0, 1SIND0, 3COS0, 3COSD0, 3ATAN0 and 3ATAN20 will now
return small flonums when given small flonums as arguments.

[18] 1LET-GLOBALLY0 works on initially-unbound variables.

You can now change a variable temporarily with 1LET-GLOBALLY0 even if it does
not already have a value.  The variable becomes unbound again on exit from
the body of the 1LET-GLOBALLY0.


Changes in utilities and operation:
   
   [@] Unlimited levels of Undo in the editor.
   [A] 1QC-FILE0 uses indentation to pinpoint parenthesis mismatches.
   [B] Undefined function references go in the compiler warnings data base.
   [C] Compiled functions record the macros expanded during compilation.
   [D] 1UNCOMPILE0 works reliably.
   [E] 1Terminal F0 no longer clears out the idle time.
   [F] The error handler: storage allocation, warnings, and background errors.
   [G] ZWEI now contains a "buffer editor", invoked with 1M-X Buffer Edit0.
   [H] 1M-X Dissociated Press0 is now implemented.  Try it!  It's fun.
   [I] 1Finish Patch0 now allows multi-line descriptions of patches.
   [J] ZWEI variables 1*INDENT-WITH-TABS*0 and 1*ALPHABETIC-CASE-AFFECTS-SEARCH*0.
   [K] New ZWEI commands 1Tabify0 and1 Untabify0.
   [L] 1C-Shift-Y0 in minibuffers; echo area output/prompting in minibuffers.
   [M] ZWEI commands1 Create Directory0,1 Remote Connect0 and1 Remote Access
0   [N] 1Help L0 works in all kinds of editor windows.
   [O] DIRED now has an 1R0 command, for renaming a file.
   [P] The Lisp Edit window and the Ztop mode in ZMACS now work much better.
   [Q] The inspector displays arbitrary size arrays.  3Control-V0 and 3Meta-V0.
   [R] Peek now has a menu of modes.
   [S] You must now use 1Control-Abort0 to get out of 1HOSTAT0.
   [T] A 1DEFSYSTEM0 can specify the filetype for binary files.
   [U] ZMAIL: Profile editor, argument prompting, and 1:GMSGS-HOST0 option.
   [V] How to use the garbage collector.
   [W] Spacewar.
   [X] Multi-font files in ZWEI.
   [Y] Reverse-video region marking.

[@] In the editor, you can now undo more than just the last change you have made.

Each time you do1 M-X Undo0 (or1 C-Shift-U0), it undoes one change.
You can redo the changes undone with 1M-X Redo0 or1 C-Shift-R0.

If you set the variable 1ZWEI:*UNDO-SAVE-SMALL-CHANGES*0 non-1NIL0, 2all0 changes
you make are recorded for undoing.  Each batch of consecutive changes is
recorded as one unit of undoing.  This may become the default in a little while,
after people have had a chance to exercise it.

The undo records are stored separately for each buffer, and are cleared out when
you read or write a file, or when you do 1M-X Discard Undo Information0.  I do
not know yet whether the existence of a large amount of undo information can
possibly slow down editing operations enough to notice.

[A]1 QC-FILE0 now uses the indentation of the source file to detect
mismatched parentheses.  A mismatch in one function will now ruin
only the compilation of that one function, not the entire file.

The compiler assumes that you follow the usual indentation convention
that an open parenthesis in column zero indicates the start of a top
level list.  (This is the same convention that the editor assumes).
If the compiler finds an open parenthesis in column zero in the middle
of a list, it invents enough close parentheses to close off the list
that is in progress.  A compiler warning is produced instead of an
error.  After that list has been processed, the open parenthesis is read
again.  The compilation of list that was forcefully closed off is
probably useless, but the compilation of the rest of the file is
usually correct.  You can read the file into the editor and fix and
recompile just the function that was unbalanced.

A similar thing happens on end of file in the middle of a list, so that
you get to see any warnings for the function that was unbalanced.

Certain special forms including as1 EVAL-WHEN, PROGN0,1 LOCAL-DECLARE0,
1DECLARE-FLAVOR-INSTANCE-VARIABLES0, and1 COMMENT0 are customarily used
around lists that start in column zero.  These symbols have a non-1nil
SI:MAY-SURROUND-DEFUN0 property that makes it ok for a list to start
in column zero inside them.  You can add such properties to other symbols
if you want.  You can also turn off the indentation checking feature
by setting 1COMPILER:QC-FILE-CHECK-INDENTATION0 to1 NIL0.  However, please
report to me any case in which this feature causes problems.  I would
like to improve it to the point where nobody could ever mind it.

The compiler implements this feature by calling 1READ-CHECK-INDENTATION
0instead of1 READ0.  That function does all the work.  It signals the
1:MISSING-CLOSEPAREN0 condition if it finds a problem, with an argument
that is 1T0 if the problem was end of file, or1 NIL0 if it was an open
paren in column zero.

[B] Meanwhile, references to undefined functions in functions being compiled
now produce ordinary compiler warnings which go in the data base of such.
Rebinding an instance variable also produces a warning, telling you
that the inner binding is just a local variable.

[C] In newly compiled files, the macros used in each function are
recorded in its debugging info.  So 1WHO-CALLS0 of a macro works.

[D] 1(UNCOMPILE2 function-spec1)0 now reliably replaces a compiled function
definition with the equivalent interpreted definition, or tells you that
it cannot do so.  It can always do so if the function was compiled in
core.

[E] Commands using the 1Terminal0 key, such as 1Terminal 1 F0, no longer
clear out the keyboard idle time that appears in the who line and in
FINGER.

[F] The error handler: storage allocation, warnings, and background errors.

The error handler now uses its own area for storage allocation, and
takes care to make sure that the area is kept supplied with free space.
As a result, you are much less likely to have any problem getting into
the error handler on an error due to the exhaustion of the address space
or some important area.

If the error handler is entered while the default cons area is a
temporary area, it will print a message to warn you to be careful.

If an error happens in a process which has no window or other suitable stream to
type out on, the window system tries to assign it a "background window" and
print a notification to tell you it is there.  If the window system is in a clean
state at the moment, this can be done, and you can then type 1Terminal 0 S 0to
select the background window.

However, if the window system cannot notify you, because windows are locked,
The window system can still inform you of the error.  The who-line displays a
string containing flashing asterisks that tells you there are errors in the
background.

At this time you can either try using 1Terminal Control-Clear-Input 0to unlock
the locks and allow the notification to be printed normally, or you can elect to
handle the error using the cold-load stream by typing 1Terminal Call0.  This
command normally enters a break-loop that uses the cold-load stream, but if
there are any background errors, it asks you about handling one of them instead.

[G] ZWEI now contains a "buffer editor", invoked with 1M-X Buffer Edit
0or1 M-X Edit Buffers0.  This is analogous to DIRED, but for buffers
instead of files.  It displays a list of all ZMACS buffers, which you
move through using ordinary editing commands.  You can specify to
save, kill, write, revert, unmodify and select buffers by typing
single character commands that refer to the buffer you are pointing at.
Type 3Help0 while in this mode to get the details.

[H] 1M-X Dissociated Press0 is now implemented.  Try it!  It's fun.

[I] 1Finish Patch0 now allows you to specify multiple lines of description
of what the patch is for.  You end the description with1 End0 now, rather
than1 Return0.

[J] ZWEI has new variables:
1 *INDENT-WITH-TABS*0, 1T0 by default, says that indentation commands should
 insert tabs.  1NIL0 says they should use only spaces.

1 *ALPHABETIC-CASE-AFFECTS-SEARCH*0, 1NIL0 by default, says if1 T0 that searches
 should only find text that matches the case of the search string.
 This affects1 Query Replace0 as well.

[K] New ZWEI commands:
1Untabify 0replaces all tabs after point (or in the region) with spaces.
1Tabify   0replaces spaces (in runs of three or more) with tabs as much
            as possible.
1List Flavor Components
0	  lists the names of all components of a specified flavor.
	  You can edit them with1 C-Shift-P0 or1 C-.0, or click on one
	  with the mouse.
1List Flavor Dependents
0	  lists the names of all flavors that incorporate a specified flavor.
	  You can edit them with1 C-Shift-P0 or1 C-.0, or click on one
	  with the mouse.
1List Flavor Methods
0	  lists all the methods of the specified flavor.
	  You can edit them with1 C-Shift-P0 or1 C-.0, or click on one
	  with the mouse.  The list does 2not0 include inherited methods.
1Edit Flavor Components0,1 Edit Flavor Dependents0, 1Edit Flavor Methods
0	  These are similar, but immediately visit the first one's definition
	  without bothering to a display a list of all the names.
1Edit System Warnings
0	  1Edit Warnings0 for all source files in a specified System.

[L] Minibuffer improvements:

In most minibuffers, the command 1C-Shift-Y0 will yank either the default
value or the old value of some parameter you are modifying.

For example, in the second argument to1 Query Replace0, it yanks the
string being replaced.  In 1Set Fonts0, it yanks the old list of fonts.
In1 Set Variable0, it yanks the old value of the variable.

When reading buffer names, filenames or function names, 1C-Shift-Y0 yanks
the same value that would be used if you just type Return.

Many of these default strings used to be made available by temporarily
pushing them on the kill ring, so you could yank them with 1C-Y0.
However, this interfered with using the kill ring to move text in and
out of minibuffers as one would naively expect to be able to do.  Now
minibuffers do nothing strange to the kill ring and it works as one
would expect.

One additional feature: echo area output while you are in the minibuffer
now stays on the screen long enough to be read.  After a pause, it
disappears and the minibuffer reappears.

[M] The ZWEI commands 1M-X Create Directory0,1 M-X Remote Connect0 and
1M-X Remote Access0 provide convenient interfaces to the new functions
1FS:CREATE-DIRECTORY0 and1 FS:REMOTE-CONNECT0.  The first creates a
directory on a file server.  The other two commands perform the
equivalent of the Twenex CONNECT and ACCESS commands, in the remote file
server, but affect access control only.  They do not affect filename
defaulting.  To get the same effect as the Twenex CONNECT command for
filename defaulting, use the "working directory" feature documented in
item [a], below.

[N] All windows now record type-in, and support the1 :PLAYBACK0 operation
which returns an array containing the last 60 characters of input.
1Help L0, which uses this operation, now works in all kinds of editor windows,
including ZMAIL, Converse and DIRED.

[O] DIRED now has an3 R0 command, to rename the file on the current line.
You type the new filenames using the mini buffer.
The filename in the line in the dired buffer is changed to reflect what
you have done.  If you rename the file to a different directory,
you are offered the choice of deleting the line from the dired buffer.

[P] The Lisp Edit window and the Ztop mode in ZMACS now work much better.
Their former gross problems have been fixed.

[Q] The inspector can display the contents of all arrays regardless of size
or number of dimensions.  The commands1 Control-V0 and1 Meta-V0 scroll the
bottom inspect pane.

[R] Peek now has a menu of modes.  Click on a mode in the menu to select
that mode.  The current mode is highlighted.  The keyboard commands still work.

[S] You must now use 1Control-Abort0 to get out of1 HOSTAT0.

[T] When you define a system with 1DEFSYSTEM0, you can now specify the filetype
to use for compiled files in that system.  The default is still "QFASL".

To specify some other string, such as "MYFASL", include
1a (:DEFAULT-BINARY-FILE-TYPE "MYFASL")0 in the1 DEFSYSTEM0.
Then 1MAKE-SYSTEM0,1 LOAD 0and1 QC-FILE 0all1 0know to use1 MYFASL0 by default for all
the files in your system.

[U] ZMAIL improvements:

If you go into ZMAIL's profile editor and actually edit the text of your
ZMAIL init file, when you type1 End0 to exit, the ZMAIL options are all
set based on the editing you did.

ZMAIL now allows a BABYL file to have a1 :GMSGS-HOST0 option which specifies
which host to do GMSGS on, for that BABYL file.  Thus, if you keep your
BABYL file on OZ, which has no GMSGS server, you can have GMSGS done on AI.

ZMAIL's command loop now prompts like ZWEI and EMACS for multi-line commands.

There is now an O command which moves the current message to a file
whose name is read using the mini buffer.

Errors in background saves no longer permanently hang the background process.

[V] How to use the garbage collector:

You can turn on automatic use of the garbage collector by doing
1(SI:GC-ON)0.  1(SI:GC-OFF)0 turns it off.

Normally, automatic garbage collection happens in incremental mode; that
is, scavenging happens in parallel with computation.  Each consing
operation scavenges or copies four words per word consed.  In addition,
scavenging goes on whenever the machine appears idle.

If you are running a noninteractive crunching program, the incremental
nature of garbage collection may be of no value.  Then you can make
garbage collection more efficient by making it a batch process.  To do
this, set1 SI:GC-RECLAIM-IMMEDIATELY0 to non1-nil0.  This flag causes
automatic garbage collections to be done as batch operations: the
garbage collection process scavenges and reclaims oldspace immediately
right after a flip happens, and uses all of the machine's
physical memory.  (Incremental scavenging is restricted to a fixed
amount of physical memory to reduce its interference with your other
activities).  This variable is only relevant if you have turned on
automatic garbage collection with 1(SI:GC-ON)0.

In addition to turning on automatic garbage collection, you can also
manually request one complete collection with the function 1SI:FULL-GC0.
The usual reason for doing this is to make a band smaller before saving
it.1  SI:FULL-GC0 also resets all temporary areas.

You can cause all the normally static (not garbage collected) areas to
be garbage collected, just this once, by passing a non-1nil0 argument to
1SI:FULL-GC0.  This can further reduce the size of a band, but is usually
not needed except right after a new system band has been built.  After
these areas have been cleaned up once, they usually don't accumulate
much additional garbage.

A more selective way of causing static areas to be garbage collected
once is to call1 SI:CLEAN-UP-STATIC-AREA0.  The argument is the area
number of a static area; that particular area will be garbage collected
the next time 1SI:FULL-GC0 is called.  This way you can control which
areas to add to the garbage collection.

In system 87, any request to perform garbage collection will warn and
query you if there is not enough free space to be sure of success.  The
criteria for success have been newly adjusted and made to depend properly
on whether you are doing incremental gc or batch.

The function 1SI:GC-STATUS0 prints information related to garbage collection.
When scavenging is in progress, it tells you how the task is progressing.
It prints both a maximum and a minimum, since there is no way of predicting
how much oldspace data will have to be copied and scavenged.
While scavenging is not in progress and oldspace does not exist,
it prints information about how soon a new flip will be required.

When you garbage collect to reduce the size of a band, it is best to do
1two0 garbage collections.  If you process any static areas, you should
process the same ones both times.  This is to keep the data compacted
toward the bottom of the virtual address space, which makes it possible
to run the saved band in small PAGE partitions.  Here is why it works:

The saved system band occupies a nearly contiguous range of virtual
memory, at the bottom of the address space.  Doing one garbage
collection copies all the useful data.  Since the lowest addresses were
already in use before, the copy has to occupy higher addresses.  The low
addresses become a hole.  The highest used address is now much higher
than before.

The second garbage collection copies everything again.  Assuming that
the first garbage collection actually freed some data, the copy will fit
into the hole left by the first garbage collection.  The high addresses
used after the first collection are now free, and the highest used
address is back down to its original value.

If you do only one garbage collection and then load in more data,
another garbage collection later will not have a neat hole to copy
into.  Fragmentation may develop.  As long as two garbage collections
are done in a row, there should be no problem.  In any case, problems
will only occur with PAGE partitions smaller than 36000 blocks.

[W] Spacewar.

Spacewar, the original video game, is now available.
To run it, load SYS:IO1;SWAR and call 3SPACEWAR:SPACEWAR0.

[X] Multi-font files in ZWEI:
You can now specify the interline spacing with the attribute 3VSP:0 2number3;
0in the file's attribute list.  The variable 3*VSP*0 (initially 2) is used for files that
do not specify a vsp.

The command 3M-X0 3Set0 3Vsp0, which now uses a numeric argument to determine
the new vsp, sets the current file's vsp, temporarily overriding the attribute list.
If you then do 3M-X Update0 3Attribute0s3 List0, the attribute list will be updated to
contain what you specified by hand.

Writing multi-font files is much faster now.
(Reading them was sped up in system 86.)

[Y] Reverse-video region display

Reverse-video region display, which you get by setting
3ZWEI:*REGION-MARKING-MODE*0 to 3:REVERSE0-3VIDEO0, is now improved.
There is no longer a band of white between the lines of the region.

The new variable 3ZWEI:*REGION-RIGHT-MARGIN-MODE*, 0if set to 3T0, causes the
reverse video area (or underlining, if you are using that) on a line to extend all
the way to the right margin if the region does not end on that line.
3
0Pathname improvements:

   [a] Improvements to defaulting.  Working directories.
   [b] Working Directories.
   [c] Improvements for Unix pathnames.
   [d] Logical pathnames: case conversion, and 30 for 1:UNSPECIFIC0.
   [e] Quoting in Twenex pathnames.

[a] Improvements to defaulting.

The default type if you specify a name only and no type is now 1"LISP"0.
It used to be 1:UNSPECIFIC0.  I think this will be more useful.
(Actually, the value of the variable 1FS:*NAME-SPECIFIED-DEFAULT-TYPE*
0is what is used.)

As before, setting 1FS:*ALWAYS-MERGE-TYPE-AND-VERSION*0 to1 T0 causes the
type of the default pathname to be used instead of a uniform default.
In this case, the value of the new variable is irrelevant.

[b] Working Directories.

The Lisp machine now remembers a "working directory" for each host.
It is actually a device name and optionally a directory name.  Whenever
you specify device 1DSK0 in a pathname, 1MERGE-PATHNAME-DEFAULTS0 replaces
it with the working device for that host; in addition, if the pathname
has1 NIL0 for a 2directory0, the working directory will be used rather
than the current default.

For example, if the working directory for host OZ is PS:<RMS>, then
specifying "OZ:DSK:FOO" is equivalent to specifying "OZ:PS:<RMS>FOO",
and specifying "OZ:DSK:<BAR>FOO" is equivalent to specifying
"OZ:PS:<BAR>FOO".

The function 1(FS:SET-HOST-WORKING-DIRECTORY 2host pathname1)
0sets the working directory for2 host0 to that specified3 0in2 pathname0.

The working directory is a local, inside-the-lisp-machine equivalent of
the effect of the Twenex CONNECT operation on pathname defaulting; but
it works on all kinds of file systems for which you can specify a device
name -- all but Unix and Multics.  Something can be done for them too,
if people like this feature.

The ZWEI command1 M-X Set Working Directory0 provides a convenient
interface to this command.

[c] Improvements for Unix pathnames.

Case is now preserved in Unix pathnames!  It used to be that you could
not input a Unix pathname differing only in case from a previously
entered Unix pathname, because the system would canonicalize it into the
previous one.  This problem is no longer with us.

[d] The translations of a logical host can now specify a physical device as
well as a physical directory for each logical directory.  Simply include
a device name in the pathname string that specifies what to translate to.
In addition, they can specify a default physical device for use with
directory names not mentioned specifically in translations.  This is
specified by providing a translation for logical directory name 1NIL0.

Logical pathnames are now converted to upper case, except for characters
quoted with 30.  This seems to be the best thing to do, even though some
file servers consider case significant, because the purpose of logical
pathnames is transportability between file servers, and to try to make
any use of case distinctions in pathnames would defeat that purpose.
If anyone is using logical pathnames with a Unix system and dislikes
having his pathnames in upper case there, he should tell me; I have an
idea for how to do this in a clean fashion, but I don't want to
implement it unless there is actual need.

The character 30 (up-horseshoe) can now be used in logical pathnames to
signify a 2name0,2 type0 or2 version0 of1 :UNSPECIFIC0.  This is differerent
from the character 30, which signifies a2 name0,2 type0 or2 version0 of 1NIL
0(that is, to be replaced by the default).  Using 30 for the2 type0 and
2version0, you can now type in the typical generic pathname.  To include a
30 character in a name, quote it with 30.

[e] Quoting wildcards in Twenex and Tenex pathnames are now

* and % when not quoted are wildcard characters that match any string or any
single character, respectively.  This is not new.

What is new is that quoting * and % works and allows you to refer to a file
whose name contains those characters.

Quoting is, as before, done with the character 30 (ASCII ^V), which must itself be
quoted with a slash when included in a string.
