-*-Fonts:TR12,cptfont,TR12I,HL12B,TR12B; Mode:Text-*-

System 93 is released with microcode 226.  It should not be used
with earlier microcode versions.

4Compiled Code Will Become Incompatible.*

As of March 22, code compiled in system 93 will no longer run in system 91.

At this time, the compiler will be changed to take advantage of new
macroinstructions for accessing arrays and instances with small constant indices.
This should make access to slots of 1DEFSTRUCT* arrays and outside accessible
instance variables faster.

The new compiled code will work in system 91 if microcode 226 is used.
I believe system 91 will work with microcode 226 as long as unibus interrupts
are not used, but due to that exception microcode 226 should not be generally
installed with system 91.

4Other Incompatibilities* (further discussed below).

Compiled ZMAIL init files containing filter definitions must be recompiled.

Frames no longer have the instance variable 1TV:SELECTED-PANE.*
It is replaced by 1TV:SELECTION-SUBSTITUTE*.

The 1:TYI* operation on windows now discards blips ("characters" which are lists).
This is to say, the effect of 1TV:ANY-TYI-MIXIN* is now the default.

The 1:REDEFINE-MARGINS* window operation no longer takes an argument.

Hash tables are now flavor instances.

Editor buffers and windows have been reorganized internally,
and several editor commands have been renamed.

1Abort* in incremental search moves back to the starting point of the search.

4Window System Changes*

	[1A*] Selection Substitutes.
	[1B*] 1:TYI* Returns Only Fixnums.
	[1C*] Any Window May Be a Pane.
	[1D*] More Windows Have Borders.
	[1E*] 1:DESELECT* Operation on Windows Generalized.
	[1F*] Global Asynchronous Characters.
	[1G*] Shifted Control and Meta Characters.
	[1H*] 1:REDEFINE-MARGINS* Operation Changed.
	[1I*] Defining Terminal and System Keys.
	[1J*] Momentary Menus and Mouse Warping.
	[1K*] 1NIL* in Menu Item Lists.

[A]4 Selection Substitutes.*

Every window now has the ability to designate a "selection substitute".
If a window has a substitute, the 1:SELECT* operation will actually select the
substitute (or its substitute, etc.)  Switching between panes within a frame
is now done by altering the selection substitute of the frame.  As a result,
the frame knows at all times which window to select if the user asks to
select the frame.

These operations on windows are provided for working with selection
substitutes:

1:SELECTION-SUBSTITUTE* -- returns the window's selection substitute,
           which is 1NIL* if the window does not currently have one.

1:SET-SELECTION-SUBSTITUTE* 2substitute* -- sets the window's selection
           substitute to 2substitute* (another window).  If this window or its
           old substitute was selected, 2substitute* will be selected instead.
           2substitute* should be 1NIL* to get rid of any substitute; then if
           the window's old substitute was selected, the window itself
           will be selected instead.

1:ULTIMATE-SELECTION-SUBSTITUTE* -- returns the window's substitute's
           substitute...  If the window has no substitute, the value is the window
           itself.

1:SELF-OR-SUBSTITUTE-SELECTED-P* -- 1T* if this window, or its substitute,
           or its substitute's substitute, etc., is selected.

When a program which uses a frame wishes to change the selected pane,
it should send 1:SET-SELECTION-SUBSTITUTE *to the frame, with the desired
pane as argument.  Note that the pane to select need not be an immediate
inferior of the frame.  This operation produces the correct results regardless
of whether the frame is currently selected at all.

The old operation 1:SELECT-PANE*, which used to exist on frames,
is now the same as 1:SET-SELECTION-SUBSTITUTE*.
The instance variable 1TV:SELECTED-PANE* that frames used to have
is gone, subsumed by 1TV:SELECTION-SUBSTITUTE* which all windows have.

To switch the selected pane temporarily, use the macro

1(TV:WITH-SELECTION-SUBSTITUTE (2window* 2for-window*) 2body*...)*
           executes 2body* with 2window* as the substitute for 2for-window*.
           On exit, it sets 2for-window* back to whatever it used to be,
           and deexposes or deactivates 2window* if appropriate.

This macro should replace most uses of 1TV:WINDOW-CALL*.

4Non-Inferior Substitutes.*

Some programs wish to "replace" one window with another temporarily.  For
example, the functions 1SUPDUP* and 1TELNET* can behave this way, giving the
appearance of temporarily changing the Lisp listener or other window they are
called in into a 1SUPDUP* or 1TELNET* window.  They do this by creating a suitable
1SUPDUP* or 1TELNET* window and making it the substitute for the original window.
In this case, the substitute window will have the same edges and the same
superior as the original window.  It is not an inferior of the original window.
It is not required that the "replacement" window be the same size as the
original, either.

It is important that the 1:ALIAS-FOR-SELECTED-WINDOWS* operation be the
inverse of 1:ULTIMATE-SELECTION-SUBSTITUTE*: if 2w1* is the substitute for 2w2*,
then 2w1*'s 1:ALIAS-FOR-SELECTED-WINDOWS* must be the same as 2w2*'s.  With
frames, you can ensure this by mixing 1TV:ALIAS-FOR-INFERIORS-MIXIN* or 
1TV:FRAME-INFERIORS-NOT-EXTERNALLY-SELECTABLE-MIXIN* into the frame flavor.
This makes the frame be the 1:ALIAS-FOR-SELECTED-WINDOWS* of all of its
inferiors to all levels.  In the case of "replacing" a window with another that
is not its inferior, you must still make the 1:ALIAS-FOR-SELECTED-WINDOWS* of
the replacement window be that of the original window, by defining a
new method for that operation and keeping a pointer to the original window.

4Typeout Windows and Selection Substitutes.*

Typeout windows now use the selection substitute mechanism to select
themselves when output on.  The typeout window makes itself the substitute of a
suitable window above it in the hierarchy, not necessarily the immediate
superior of the typeout window.  This is so that it will work to type on the
typeout window when its immediate superior is not selected (this happens in the
editor if you type 1Meta-X Help*).  It also records what that window 2used* to have as
a substitute.

When the typeout window is deactivated, if it is still the substitute for that
window, it attempts sets that window's substitute back to what it used to be.  It
does this by sending that window a 1:REMOVE-SELECTION-SUBSTITUTE* message.

1:REMOVE-SELECTION-SUBSTITUTE 2window-to-remove** 2suggested-substitute*
           makes sure that 2window-to-remove* is not this window's substitute,
           suggesting 2suggested-substitute* as a substitute instead.
           The standard implementation of this operation simply sets the
           substitute to 2suggested-substitute* if the substitute was
           2window-to-remove*.  This operation is used and documented so
           that particular windows can define their own ways of calculating
           the new value for the substitute, perhaps ignoring 2suggested-substitute*.

The flavor 1TV:KLUDGE-INFERIOR-MIXIN* is gone; the functionality it used to
provide is now intergal to 1TV:BASIC-TYPEOUT-WINDOW.

3FQUERY4 and Substitutes.***

The 1:SELECT *option in 1FQUERY* was created to make sure that the editor's
type-in window (the "echo area") would have a blinking cursor when a
query happens in the editor.  The type-in window now arranges this automatically
by making itself the substitute of the editor frame whenever input is done on it.
Simply selecting it and deselecting it, as 1FQUERY* would have done, would not
be correct any more.  So 1FQUERY*'s 1:SELECT* option no longer does anything;
it is accepted but ignored.

It is possible that some users are relying on 1FQUERY*'s 1:SELECT* option in other
circumstances.  If necessary, I can develop a way to make it work properly.
However, it is likely that a better solution to the problem is to make the
windows concerned know how to select themselves, as the editor's type-in
window does.  To see how, refer to the definition of 
1(:METHOD ZWEI:ECHO-AREA-QUERY-WINDOW :WRAPPER :ANY-TYI)*.

[B] 3:TYI4 Returns Only Fixnums.**

The 1:TYI* operation on windows now returns only fixnums.  Lists ("blips") in the
input stream are just discarded, and 1:TYI* does not return until it finds a fixnum.

To do input and see blips, use the 1:ANY-TYI* operation, which all windows support.

This behavior used to be the result of 1TV:ANY-TYI-MIXIN*.  As the behavior is
now the default, the mixin is a no-op, but it remains in existence for
compatibility.

4A note on the use of blips.*

Blips are lists put into an input stream with the 1:FORCE-KBD-INPUT* operation.

It is always possible for a blip to be lost.  For example, the process reading
the input stream could be in the error handler, which ignores all blips.
So if you use blips, you must make sure that losing a blip is not a problem.

There are two known correct uses of blips, mouse commands and "wakeup"
signals.

Blips for mouse commands comes from things like command menus and typeout
windows, and direct a process to handle the command.  It is no problem that
these blips will be lost if the process is in the error handler; the user will
understand that it cannot obey its usual mouse commands at that time.

Blips have sometimes been used to convey asynchronous requests to a process;
such as, the completion of background loading or saving a file in ZMAIL.
This is an error, because the loss of the blip causes serious problems.
A correct technique is to put the information itself on a separate queue
and then supply a "wakeup" blip which says only "wake up and check the queue".
The process should check its queue each time around its main command loop.
The wakeup blip is treated as a no-op, but it makes the process go around its
loop and incidentally poll the queue.  If the wakeup blip is lost, eventually
the program will return from wherever it was, and check the queue anyway.

[C] 4Any Window May Be a Pane.

1TV:PANE-MIXIN** now has no effect.  Any window can serve as a pane in a frame.

[D] 4More Windows Have Borders.*

Typeout windows--such as those used for output printed by editor commands--
now have borders.  The border is a line drawn across the bottom of the screen area
actually occupied by output.  This feature can be disabled by setting
1TV:*ENABLE-TYPEOUT-WINDOW-BORDERS** to 1NIL*.

Editor frames now have borders that go around the whole frame, including the
mode line and mini buffer area.

Lisp Listeners and SUPDUP windows used to have no borders or labels if they
occupied the whole screen.  This is no longer true; now they always display
borders and labels.  1TV:FULL-SCREEN-HACK-MIXIN*, which used to implement the
special case check, no longer exists.

[E] 3:DESELECT* 4Operation on Windows Generalized.*

The argument to the 1:DESELECT* operation on a window now has these possible values:

1    :FIRST* -- put this window at the front of the ring of selected windows, after
1             *selecting the window that was there.  This is like 1Terminal S.
    :LAST -- *put this window at the end of the ring, and select the window
1             *at the front of the ring.
1    :DONT-SAVE -- *do not put this window into the ring,
1             *and do not automatically select another.
1    :BEGINNING --  *put this window at the beginning,
1             *but do not automatically select another.
1    :END --  *put this window at the end, but do not automatically select another.

1NIL* is equivalent to 1:BEGINNING*.

[F] 4Global Asynchronous Characters.*

The 1Terminal* and 1System* keys are standardly 2global asynchronous characters*; that is,
they are asynchronous characters that are handled regardless of the selected window.
The set of active global asynchronous characters is now controlled by the value of
the variable 1TV:KBD-GLOBAL-ASYNCHRONOUS-CHARACTERS*.  It is an alist whose elements
look like 1(2char* 2function*)*.  2function* is a function of no arguments that will be
called in the keyboard process.  It must not do i/o or wait for any reason; if that
is part of the desired functionality, use 1PROCESS-RUN-FUNCTION* to have it done in
another process.

[G]4 Shifted Control and Meta Characters.*

The 1Shift* key on letters with 1Control*, 1Meta* or 1Super* used to be a synonym
for 1Hyper*, making 1C-Shift-A* another way of typing 1Hyper-Control-A*.

Now the 1Shift* key with 1Control* (etc.) letters is a way of forcing lower case.
So 1Control-Shift-A* is really "1Control-Lowercase-A*", which there used to
be no way to type (recall that letters with meta bits are normally uppercase).
In programs, this character can be input as 1#\CONTROL-SHIFT-A*, or as
1#\CONTROL-/a* (again, 1#\CONTROL-A* is a controlified uppercase A).

Another way of thinking of this is that the 1Shift* key has the opposite of its
normal meaning when 1Control*, 1Meta*, 1Super* or 1Hyper* is present; the character
is uppercase unless 1Shift* is pressed.  (Also, 1Shift*-1lock* has no effect).

In the editor, the new shifted control letters are normally defined as synonyms
for the Hyper characters they used to be identical to.

A program can turn off the interaction between the 1Control*, 1Meta*, 1Super* and
1Hyper* keys and the 1Shift* key by putting a non-1NIL* value for the property
1:DONT-UPCASE-CONTROL-CHARACTERS* on the property list of a window's input
buffer.  Then the characters are lowercase unless 1Shift* is pressed.  This is used
by SUPDUP so that it can transmit the case as specified.

[H] 3:REDEFINE-MARGINS4 Operation Changed.**

The 1:REDEFINE-MARGINS* operation on windows, which tells a window
to recompute the size of its margins, now no longer takes an argument.

This operation now directs a window to recompute how much margin space is
required for its box, label, scroll bar or whatever it has, based on whatever
is currently specified for how to display the box, label, etc.  For example,
the 1:SET-LABEL* operation does a 1:REDEFINE-MARGINS* after it changes the
label-describing data structure.

[I] 4Defining 3Terminal* and 3System* Keys.*

Some new functions have been added to make it user to customize what
typing the teminal or system keys will do.  The function 1TV:ADD-ESCAPE-KEY*
defines a new 1Terminal* key, and 1TV:ADD-SYSTEM-KEY* defines a new 1System* key.
Here is how to call them:

1TV:ADD-ESCAPE-KEY* 2character* 2function* 2documentation* &rest 2options*

    Defines 1Terminal* followed by 2character* to call 2function*.
    2documentation* is printed to describe the meaning of 2character*
    if the user does 1Terminal* 1Help*.  This string is no longer
    restricted to a single line.

    2options* are keywords (not accompanied by values).
    The meaningful keywords are 1:KEYBOARD-PROCESS*, which means
    that 2function* should be called in the keyboard process rather than
    in a separate process, and 1:TYPEAHEAD,* which says to copy all type-ahead
    into the input buffer of the selected windows.  1:TYPEAHEAD* should be
    used with commands that can switch windows.

1TV:ADD-SYSTEM-KEY* 2character* 2window-or-flavor* 2documentation* &optional 1(2create* T)*

    Defines 1System* followed by 2character* to select a window according
    to 2window-or-flavor*.  This argument may be
      a window, which will be selected if 1System* 2character* is typed.
      a window flavor name.  1System* 2character* will select a window
        which is 1TYPEP* that flavor, or rotate through all such windows.
        This flavor may be a mixin.
      a list.  1System* 2character* will evaluate it.  The value should be
        a window or a window flavor name.
    If there is no such window, or if 1Control* is held down with 2character*,
    a window is created if 2create* is non-1NIL*.  2create* may be
      1T*, meaning that a window of flavor 2window-or-flavor* should be
        created.  In this case, that flavor must be instantiable.
      a window flavor name.  A window of that flavor will be created.
      a list.  It will be evaluated, and should create and select a window.

There are also functions 1TV:REMOVE-ESCAPE-KEY* and 1TV:REMOVE-SYSTEM-KEY*
which undefine characters.  They take one argument, the character to undefine.

[J] 4Momentary Menus and Mouse Warping.*

When a momentary menu disappears after you have chosen an item, the mouse
is warped on the screen back to where it was before the menu appeared.  This
can be convenient if the mouse was pointing at some feature on the screen that
you wish to click on frequently.

[K] 3NIL4 in Menu Item Lists.

1NIL*** in a menu item list is now ignored.  It does not display at all.
(The old behavior of 1NIL* was fairly useless).
Thus, 1(TV:MENU-CHOOSE (LIST (IF X 'Y) 'Z (IF A 'B)))*
will always offer the alternative 1Z*, and may offer 1Y*, 1B* or neither or both of them.

4Programming Changes.*

	[1A*] Property List Functions Work on Instances.
	[1B*] Hash Tables Are Now Flavor Instances.
	[1C*]4 *Compiler Optimizes 1(FUNCALL SELF)* into 1(FUNCALL-SELF)* When Appropriate.
	[1D*] New Construct 1LET-GLOBALLY-IF*.
	[1E*] New Construct 1CATCH-ERROR-RESTART-EXPLICIT-IF*.
	[1F*] New Variable 1CURRENT-STACK-GROUP*.
	[1G*] 1SPACES* Argument to 1STRING-CAPITALIZE-WORDS*.
	[1H*] New Function 1STRING-SUBST-CHAR*.
	[1I*] 1STRING-SEARCH* Can Search for a Substring of a String.
	[1J*] Rank-0 Arrays Are Allowed.
	[1K*] Returning No Values is Allowed
	[1L*] New Function 1ARRAY-DIMENSION*.
	[1M*] Reading Hex Numbers.
	[1N*] Reading Complex Numbers.
	[1O*] Output of Lozenged Strings.
	[1P*] New Function 1CLOSURE-BINDINGS*.
	[1Q*] New Mode for 1TIME:MONTH-STRING* and 1TIME:DAY-OF-THE-WEEK-STRING*.
	[1R*] 1PROCESS-LOCK* Timeouts.
	[1S*]1 INSTANCEP*.
	[1T*] 1:NULL* as Argument to 1TYPEP*.
	[1U*] New Error Condition Name 1SYS:INVALID-FUNCTION-SPEC*.

[A] 4Property List Functions Work on* 4Instances.*

The property list functions 1GET*, 1GETL*, 1PUTPROP*, 1REMPROP*, 1PLIST* and 1SETPLIST*
now allow an instance as the first argument.  In this case, they work by sending a
message to the instance.  If the instance's flavor incorporates
1SI:PROPERTY-LIST-MIXIN*, you will access the instance's property list.
Familiar objects this applies to include editor buffers, pathnames and conditions.

[B] 4Hash Tables Are Now Flavor Instances.*

Hash tables are now instances of the two flavors 1SI:EQ-HASH-TABLE* and
1SI:EQUAL-HASH-TABLE*.  All the old hash table functions such as 1GETHASH*
and 1MAKE-HASH-TABLE* still work; only the data type of the object is different.
You can also create hash tables with 1MAKE-INSTANCE*.  All the keyword arguments
that 1MAKE-HASH-TABLE* accepts are now init keywords for those flavors.

There is one new keyword: 1:REHASH-BEFORE-COLD*.  If supplied with a non-1NIL*
value, the hash table will be rehashed automatically (if it needs it) when
1DISK-SAVE* is done.  This will avoid a delay when the hash table is used
for the first time when the saved band is booted.

The operations supported by hash tables are:

1:SIZE* -- returns the number of entries the table has.  Note that the table
           will be rehashed well before all the entries are full.
1:FILLED-ENTRIES* -- returns the number of entries now in use.
1:GET-HASH* 2key* -- returns the value associated with 2key*.
           The second value returned is 1T* if 2key* was found at all;
           this way you can distinguish an explicit 1NIL* from a key not found.
           The third value is a list, shared with the hash table data structure,
           that holds the 2key* and all the values associated with it.
1:PUT-HASH* 2key* &rest 2values* -- sets the values associated with 2key* to 2values*.
           A hash table records a fixed number of values for each key;
           the number of values is specified when the hash table is created.
           Any values you do not specify with args to 1:PUT-HASH* are set to
           1NIL* by the 1:PUT-HASH*.
1:REM-HASH* 2key* -- deletes the entry for 2key
1:CLEAR-HASH** -- deletes all entries, leaving the table empty.
1:SWAP-HASH* 2key* &rest 2values* -- like 1:PUT-HASH* but returns the old value.
1:MODIFY-HASH* 2key* 2function* &rest 2additional-args*
           passes the value for 2key* to 2function* and updates the value
           to be whatever 2function* returns.  Thus, the hash association for
           2key* is both examined and updated according to 2function*.
           The arguments passed to 2function* are 2key*, the value for it,
           a flag (1T* if 2key* is actually found in the hash table), and the
           2additional-args* that you specify.
           Only the first value of those associated with  2key* is available to
           2function* and only that value is modified.
1:MAP-HASH* 2function* &rest 2extra-args* -- calls 2function* once for each entry in use
           in the hash table.
           The arguments passed to 2function* are the key for that entry,
           the values associated with it (a fixed number for each hash table, remember)
           and the 2extra-args*.  Thus, the number of arguments
           depends on how many values the hash table records.
1:MAP-HASH-RETURN* 2function* &optional 2return-function*
           Calls 2function* once for each in-use entry and accumulates the values
           returned by 2function*.  2return-function* is used to combine the values.
           The default is 1LIST*.  1NCONC* is also useful, and is optimized.

3LOOP* 4Iteration Path for Hash Tables*

There is a new 1LOOP* iteration path for doing effectively the same thing as 1MAPHASH*.
It is called 1HASH-ELEMENTS*.  It is used as follows:

1    (LOOP FOR P BEING THE HASH-ELEMENTS
                 OF FS:*PATHNAME-HASH-TABLE* WITH-KEY K
          DOING (FORMAT T "~&Key ~S, pathname ~A." K P))*

In this example, 1P* gets successively each pathname object used so far,
and 1K* gets the list of components used to extract it from the hash table.

The 1WITH-KEY* 2keyvar* can be omitted if you wish to examine only
the values, not the keys.

[C]4 Compiler Optimizes3 (FUNCALL SELF)* into 3(FUNCALL-SELF)* When Appropriate.*

The compiler optimizes 1(FUNCALL SELF ...) *into 1(FUNCALL-SELF ...)* whenever
it is seen in a method or a function with a 1:SELF-FLAVOR* declaration.  Since the
1SEND* function actually macroexpands into 1FUNCALL* in the MIT system, it will also
be optimized.  You can now write 1(SEND SELF ...)* for source compatibility with
Brand S, and the MIT compiler will do the right thing.

[D] 4New Construct 3LET-GLOBALLY-IF*.

1(LET-GLOBALLY-IF 2cond-form* ((2variable* 2value-form*)...) 2body*...)**

This conditional version of 1LET-GLOBALLY* first evaluates 2cond-form*.  If its value
is non-1NIL*, each 2variable* is set to the corresponding 2value-form*'s value on entry,
and set back to its original value on exit from the 2body*.  If the value of
2cond-form* is 1NIL*, the variables are not set at either time.

[E] 4New Construct 3CATCH-ERROR-RESTART-EXPLICIT-IF**.

(1CATCH-ERROR-RESTART-EXPLICIT-IF
    * 2cond-form1 (*condition-names1 *proceed-type1 *format-string1 *format-args1...)
  *body1...)**

This macro does about the same thing as 1CATCH-ERROR-RESTART-IF* except that it
uses the specified proceed type instead of an anonymous list to identify the
resume handler that it creates.  In addition to making it possible for condition
handlers to resume by specifying that proceed type, this also makes the resume
handler a candidate for the 1Resume* key in the debugger.

[F] 4New Variable* 3CURRENT-STACK-GROUP1.**

The currently running stack group can now be found as the value of
the variable 1CURRENT-STACK-GROUP*.  1SI:%CURRENT-STACK-GROUP* still works.

[G] 3SPACES* 4Argument to* 3STRING-CAPITALIZE-WORDS.

1STRING-CAPITALIZE-WORDS** now takes a third, optional argument 1SPACES*.
If this argument is 1NIL*, hyphens are not converted to spaces.

[H] 4New Function 3STRING-SUBST-CHAR.

1STRING-SUBST-CHAR*** 2new-char* 2old-char* 2string*

  replaces every occurrence of 2old-char* in 2string* with 2new-char*.
  The value is a copy; 2string* itself is not changed.

[I] 3STRING-SEARCH4 Can Search for a Substring of a String.

1STRING-SEARCH*** and 1STRING-REVERSE-SEARCH* can now be given extra arguments to
specify a substring of the search key.  This is equivalent to making a substring
of the key first and then passing it to 1STRING-SEARCH* or 1STRING-REVERSE-SEARCH*,
but it is faster.  The complete arguments are now:

1STRING-SEARCH* 2key* 2string* &optional (2start* 10*) 2end* (2key-start* 10*) 2key-end

key-start* and 2key-end* are the arguments that specify the portion of 2key* to be
used; they default to the whole thing.  2start* and 2end* specify as before the portion
of 2string* to be searched.

[J] 4Rank-0 Arrays Are Allowed.*

It is now valid to use 1NIL* as the list of dimensions when creating an array.
The array you get has rank zero; it has a single element, which is accessed
using 1AREF* with no subscripts.

[K] 4Returning No Values is Allowed*

You can now write 1(VALUES)* to return no values from a function.
If the function was called within 1MULTIPLE-VALUE-LIST*, the value returned
from that will be 1NIL*.

[L] 4New Function3 ARRAY-DIMENSION.

1ARRAY-DIMENSION*** 2array* 2dimension*

  returns the length of the 2dimension*'th dimension of 2array*.
  The values of 2dimension* range from zero through one less than
  the rank of 2array*.  This function makes ARRAY-DIMENSION-N obsolete.

[M] 4Reading Hex Numbers.*

It is no longer necessary to put a sign in front of a number in bases greater than ten
when it contains letters serving as digits beyond 9.  Those letters which are used
in the given radix (A through F in hex) will automatically be recognized as digits.
They also lose their normal syntactic meaning.  For example 110E3* in hex is a fixnum,
not a flonum as usual.  Such conflicts are inherent in the use letters as digits,
so nothing can be done about them.

This change would be undesirable for anyone simply setting 1IBASE* to 16. permanently,
but for reading individual numbers it is an improvement.

[N] 4Reading Complex Numbers.*

Complex numbers can now be typed in, using the syntax 2m1+*n1i** or 2m1-*n1i**, where
2m* and 2n* can be any numbers.  This is the same syntax used to print them.

[O] 4Output of Lozenged Strings.*

A lozenged string is the term for the way special characters echo and print out:
a string inside an outline.  This kind of output is now available under program control.

The window operation 1:DISPLAY-LOZENGED-STRING* displays an arbitrary string
inside a lozenge.  Its only argument is a string.

1FORMAT:OCHAR *can also display a lozenged string if given 1:LOZENGED* as its second
argument.  The output is the same as with a second argument of 1:EDITOR*, inside
a lozenge provided the stream supports the 1:DISPLAY-LOZENGED-STRING* operation
unless the character is a graphic character.  For example,

1        (FORMAT:OCHAR #\RETURN ':LOZENGED)*

will print "1RETURN*" inside a lozenge if 1STANDARD-OUTPUT* is a window.

[P] 4New Function 3CLOSURE-BINDINGS*.

1CLOSURE-BINDINGS** 2closure*

  This returns the list of binding-instances of 2closure*, a data structure containing
  the external value cells of the closure.  You can pass the data structure to
  1SYS:%USING-BINDING-INSTANCES* to enter the environment of the inside of the
  closure.

[Q] 4New Mode for3 TIME:MONTH-STRING* and3 TIME:DAY-OF-THE-WEEK-STRING.**

Both2 1TIME:MONTH-STRING** and1 TIME:DAY-OF-THE-WEEK-STRING* now accept
1':ITALIAN* as a mode argument.  The appropriate name, such as "Lunedi" for Monday 
(day 0) or "Febbraio" for February (month 2), is returned.

[R] 3PROCESS-LOCK* 4Timeouts*.

You can now specify a timeout to the function 1PROCESS-LOCK*.  If the lock
you are trying to lock does not become available before the timeout elapses,
an error possessing condition name 1SYS:LOCK-TIMEOUT* is signaled.

The timeout is in units of 60'ths of a second, and is specified as the third
argument to 1PROCESS-LOCK*.

1[*S1] 3INSTANCEP4.***

The function1 INSTANCEP* returns 1T* iff its argument is an instance.

[T] 3:NULL4 as Argument to *TYPEP4.

1:NULL*** is now a "type" recognized by 1TYPEP*.  The only object which fits this type
is 1NIL*.  (Note that 1(TYPEP NIL) *=> 1:SYMBOL*, not 1:NULL*; but 1(TYPEP NIL ':NULL) => T*.)

[U] 4New Error Condition Name 3SYS:INVALID-FUNCTION-SPEC*.

1SYS:INVALID-FUNCTION-SPEC** is signaled on any operation on an invalid function
spec, including 1FDEFINE* and 1FDEFINITION*.

4Editor Changes.*

	[1A*] Multiple ZMACS Processes.
	[1B*] Incompatible Internal Changes.
	[1C*] Attribute Lists in the Editor.
	[1D*] "Register" Commands Renamed.
	[1E*] Other Editor Commands Renamed.
	[1F*] 1M-X Macro Expand Expression All*.
	[1G*] 1M-X List/Edit Changed Sections*.
	[1H*] 1M-X Count Words*.
	[1I*] New Major Mode, 1{TeX} Mode*.
	[1J*] 1M-X List/Edit Flavor Direct Dependents*.
	[1K*] 1M-X Find System Files*.
	[1L*] Editing More Than One Patch at One Time.
	[1M*] Editor Parenthesis Balancing.
	[1N*] Source Compare Output is Saved in a Buffer.
	[1O*] Editor Command 1Meta-$* for Spelling Correction.
	[1P*] New Editor Options.
	[1Q*] 1Meta-X Compile File* and 1Meta-X Load File* Improved.
	[1R*] Changed Commands in 1M-X Buffer Edit*.
	[1S*] Aborting Incremental Search.
	[1T*] Mode Line.
	[1U*] Sectionization Done While Reading Once Again.
	[1V*] DIRED No Longer Slow.
	[1W*]4 *New DIRED command 1Control-Shift-E*.

[A] 4Multiple ZMACS Processes.*

Each ZMACS frame now has its own process.  As a result, being in the
debugger, a breakpoint, a minibuffer, or a command in one ZMACS frame
does not prevent you from editing in another one.

The function 1ED*, the debugger command 1Control-E*, and the 1Edit* selection in
the window error handler now look for a ZMACS frame whose process is
listening at the editor top level, and use that to visit the specifed function or
file.  As a result, you can use 1Control-E* in an error in a ZMACS process without
losing the state of that process.

All ZMACS frames still share one collection of buffers.  It is conceivable for two
ZMACS frames modifying the same buffer at once to garbage the buffer;
however, this can only happen if they are both in the middle of a command
simultaneously, which will not happen in normal use since normally you do not
leave an editor window while a command is actually executing.  Leaving a
ZMACS frame while it is reading input (whether top level, a command argument,
or inside a breakpoint) or while it is compiling, cannot cause a problem.

[B] 4Incompatible Internal Changes.*

Several rearrangements have been made in the way the editor works.
Buffers and windows are now flavor instances.

Luckily, simple extensions do not need to be changed, since the macros
that they have used are still defined; but the macros are changed now,
so such extensions must be recompiled and then will no longer run in system 91.
More sophisticated extensions may have more difficulties.

The changes include:

  -- Buffers and intervals of all sorts are now flavor instances.
  -- There is only one object representing each editor window;
     it is a flavor instance which is a real window and also records
     ZWEI's own information.  The value of 1*WINDOW** is such an object.
  -- There is no longer an "editor" object.  Most of the operations
     that used to be defined on "editor" objects are now defined on
     editor windows instead.  However, the special instance variables
     that the editor object used to have are not instance variables of
     the editor window.  Instead, the window has an instance variable
     named 1EDITOR-CLOSURE* which is a closure of the function 1FUNCALL*
     over those variables.
  -- Attributes of a buffer are now stored on the property list
     of the buffer.  They can be examined with the 1:GET* operation.
     For setting them, it is best to use the 1:SET-ATTRIBUTE* operation
     rather than 1:PUTPROP*.  The arguments to 1:SET-ATTRIBUTE* are
     the attribute keyword, the value, and a flag saying whether to
     change the buffer's 1-*-* line as well (values are 1T*, 1NIL* or
     1:ASK*).
  -- The I/O stream conventions have been changed.  Instead of
     using 1ZWEI:*TYPEOUT-WINDOW** and 1ZWEI:*TYPEIN-WINDOW**,
     you should use 1STANDARD-OUTPUT* and 1QUERY-IO*.  Also, it is no longer
     necessary to use the functions 1ZWEI:TYPEIN-LINE* or
1    ZWEI:TYPEIN-LINE-MORE*.  You can simply call 1FORMAT* instead.
     However, the old variables and function still work.
  -- It no longer works to modify elements of 1ZWEI:*LIST-SYNTAX-TABLE**
     in your init file, since each editor window has its own copy of this.
     Instead, you should define a minor mode along the lines of
     1ZWEI:COM-ANY-BRACKETS-MODE*, and turn that mode on when desired.

[C] 4Attribute Lists in the Editor.*

Recall that the attribute list of a file is a property list which is specified, in
a text file, by the characters after 1-*-* in the first nonblank line.

When a file is visited in the editor, the editor decodes the attribute list specified
by the text of the file and records the attributes in the property list of the
editor buffer object.  Each buffer thus has its own recorded attributes.

For each standardly known attribute there is an editor command to set it, and the
command offers you the choice of setting the attribute temporarily, which means
that just the property list of the buffer object is changed, or permanently, which
is done by modifying the text of the 1-*-* line.

These commands set attributes of wide significance:

1    Set Package*	sets the Package attribute, which specifies the package
			in which to evaluate or compile from this buffer.
			This package is also current in any breakpoint or error
			within the editor.
			After asking whether to modify the 1-*-* line,
			this command asks a second question, whether to
			resectionize the file.  If your file contains a line
			"1(DEFUN FOO *", it was recorded as defining 1FOO* in the old
			package; if you resectionize, the editor will now record
			the file as containing a definition of 1FOO* in the new package.

1    Set Base*	sets the Base attribute, which specifies the value of 1IBASE*
			with which to evaluate or compile from this buffer.
			This value is also current in any breakpoint or error
			within the editor.

1    Set Patch-File* sets the Patch-File attribute.  If non-1NIL*, this attribute
			identifies the file as a patch file, which affects the
			actions of certain functions if they are called while loading
			the file.  For example, a 1DEFVAR* in a patch file will always
			reinitialize the variable, whereas normally it sets the
			variable only if it is unbound.

These attributes affect only the editor:

1    Set Backspace*	sets the Backspace attribute, which controls whether
			1Backspace* characters in the text will overprint when
			displayed by the editor.  The values are 1NIL* or
			non-1NIL*, with 1NIL* (do not overprint) the default.

1    Set Fonts*	sets the Fonts attribute, which controls which fonts
			are used for display in the editor.

1    Set Lowercase *sets the Lowercase attribute.  If non-1NIL*, this attribute
			tells the editor not to use 1Electric Shift Lock Mode*
			on this file, even for users who have requested that that
			mode be on by default.  This is done for files of code
			which is mostly in lower case.

1    Set Nofill*	sets the Nofill attribute.  If non-1NIL*, this attribute tells
			the editor not to use 1Auto Fill Mode* on this file, even
			for users who have specified that mode as the default.
			This should be done only for files in which the position
			of line breaks is crucial to the proper functioning, and
			not just as a matter of taste.

1    Set Tab-Width*	sets the Tab-Width attribute, which controls how far
			apart come the tab stops used for display of Tab characters
			in the file.  The value of the attribute is a number, which
			is taken as a multiple of the width of a Space in font zero.

1    Set Vsp*		sets the VSP attribute, which is the number of extra scan
			lines of vertical space between text lines in the editor display.
			The default is 2, for 2 scan lines.

Another way to change an attribute permanently is to edit the 1-*-* line
and then use the command 1Reparse Attribute List.*  This resets all
attributes to the values specified in the 1-*-* line, or to their default values
if not mentioned there.  The inverse of this command is 1Update Attribute
List,* which rewrites the 1-*-* line according to the current "temporary"
attribute values specified with the 1Set* commands.

[D] 4"Register" Commands Renamed.*

The editor commands that operate on 2registers* (formerly called "Q-registers"
following EMACS) have been renamed, and two have been newly installed on keys.
These commands can be used to save and restore text or buffer positions.
Most commands ask you to type a single letter which is the register name.
Each register can remember one piece of saved text, one saved position, or both.

1C-X X  (Put Register)
  *saves the contents of the region in the specified register.
1C-X G  (Open Get Register)
  *inserts the contents of the specified register.
1C-X S  (Save Position)
  *records the current value of Point in the specified register.
1C-X J  *(1Jump* 1to* 1Saved* 1Position*)
1  *jumps to the value of Point saved in the specified register.
1M-X View Register
  *shows the text and/or position saved in the specified register.
1M-X List Registers
  *shows all existing registers with the text and/or positions saved in them.

[E] 4Other Editor Commands Renamed.*

Some editor commands for evaluating or compiling parts of the buffer have been
renamed.  The renamed commands are those apply to the region, if any, or to
the Defun containing Point.

1    Evaluate Defun* -> 1Evaluate Region.
    Evaluate Defun Verbose* -> 1Evaluate Region Verbose.
    Evaluate Defun Hack* -> 1Evaluate Region Hack.
    Compile Defun* -> 1Compile Region.
    Compile Defun Verbose* -> 1Compile Region Verbose.
    Compile Defun Hack* -> 1Compile Region Hack.
    Microcompile Defun* -> 1Microcompile Region.*

There already were commands named 1Compile Region* and 1Evaluate Region*,
but the renamed commands are upward compatible with them anyway.

4Some other renamings:

1    Macro Expand Sexp** -> 1Macro Expand Expression
    Variable Document* -> 1Describe Variable
    List Matching Functions* -> 1List Matching Symbols*

[F] 3M-X Macro Expand Expression All4.

1M-X Macro Expand Expression All*** expands the expression following Point at all
levels, not just the top level.  This does not always work properly, as it may
expand sublists which are not actually expressions, if they resemble macro calls.

[G] 3M-X List/Edit Changed Sections.*

The editor commands 1List Changed Sections*, 1Edit Changed Sections*,
1List Buffer Changed Sections* and 1Edit Buffer Changed Sections*
now take a numeric argument which specifies "changed since when".

If the argument is one, or omitted, you get all sections changed since
the file was read in.

If it is two, you get all sections changed since the file was read in or saved.

If it is three, you get all sections changed since they were last recompiled.

The commands 1Evaluate Changed Sections* and 1Compile Changed Sections*
always use the last of these criteria.

[H] 3M-X Count Words.*

This command counts the number of words in the region, or, in the buffer.

[I] 4New Major Mode*,3 {TeX} Mode.*

The command 1M-X {TeX} Mode* enters {TeX} mode, for editing TeX source files.

[J] 3M-X List/Edit Flavor Direct Dependents.*

Two new editor commands are 1List Flavor Direct Dependents* and
1Edit Flavor Direct Dependents*.  They read the name of a flavor
and then either list or visit sequentially all the flavors that directly depend
on the specified one.

[K] 3M-X Find System Files.

1M-X Find System Files** reads into ZMACS all the component files of a specified
system.  It sets the package for each editor buffer thus created to the package
used for the file in that system, which may have been specified in the
1DEFSYSTEM* to override the file's 1-*-* line.

[L] 4Editing More Than One Patch at One Time.*

The patch commands now allow you to switch between patches.
If you give the command 1M-X Start Patch* or 1M-X Resume Patch* while
editing a patch, you will be asked for confirmation; if you give it,
you will start or resume another patch as you specify.  The previous
patch will remain in its editor buffer, and you can go back to editing it
with another 1M-X Resume Patch*.

[M] 4Editor Parenthesis Balancing.*

The S-expression motion commands of the editor, such as 1Control-Meta-F*, now
refuse to move from the inside of a string to the inside of another string.  If
there is no appropriate textual unit contained entirely within the original string,
the low level primitive 1ZWEI:FORWARD-SEXP* returns 1NIL*, and the editor commands
beep.

As a result, when the cursor is after a close parenthesis inside a string, the editor
will not flash an irrelevant open parenthesis.

In modes where more than one kind of bracket character is set up to be counted
by the expression and list commands, an incorrect pairing such as 1[ ... )* causes
a warning message if you put the cursor after it.

[N] 3Source Compare4 Output is Saved in a Buffer.**

When you invoke 1Source Compare* in any fashion from the editor, including 1DIRED*,
the output is saved in an editor buffer whose name starts with 1*Source Compare*
and finishes with the names of the files compared.

[O] 4Editor Command3 Meta-$* for Spelling Correction.*

The editor command 1Meta-$* checks the spelling of the word at Point,
using a chaosnet server on a Twenex system (at MIT, the server runs on OZ).
It offers you a choice of similar words to replace the word with, if the word
itself is not in the dictionary.  You can also choose to Query Replace the
same replacement through the buffer.  

[P] 4New Editor Options.*

If 1ZWEI:*FIND-FILE-NOT-FOUND-IS-AN-ERROR** is non-1NIL*, visiting a nonexistent
file is an error.  Normally it creates an empty buffer which, if saved, will create
the file.

1ZWEI:*DISCARD-UNDO-INFO-ON-SAVING** controls whether you can undo all the
way to when you visited the file, or only until the last save.  Normally it is 1T*,
meaning undo only to the last save.  The relocatable buffer pointers used
to record undo information can slow down editing in some cases, which is
why discarding them can be desirable.

1ZWEI:*CHECK-UNBALANCED-PARENTHESES-WHEN-SAVING**, if non-1NIL*, enables a
feature which balances the parentheses in a file as it is being written out by the
editor (only if the major mode indicates the contents have Lisp syntax).  If a
mismatch is found, a message is printed, but saving continues.  You can use the
command 1Meta-X Find Unbalanced Parentheses* to find the possible point of
mismatch.

[Q] 3Meta-X Compile File* 4and* 3Meta-X Load File* 4Improved.*

If the file being compiled or loaded is visited in some ZMACS buffer, the
package set in that buffer with 1M-X Set Package* will be used for loading
or compilation.

[R] 4Changed Commands in 3M-X Buffer Edit.**

The command to specify that a certain buffer should be selected on exit
is now "1.*" rather than "1**", and the buffer to be selected is displayed with a "1.*".
"1**" is used to indicate modified buffers.

[S] 4Aborting Incremental Search.*

Typing 1Abort* in an incremental search now moves the cursor back to the
starting point of the search.  Please let me know whether you like this.

[T] 4Mode Line.*

The mode line in ZMACS now contains 1* if there is more text above the screen,
1* if there is more below, or 1* if there is more text both above and below.
The arrows appear just before where the 1** appears when the buffer is modified.

[U] 4Sectionization Done While Reading Once Again.*

In system 91, ZMACS was changed to sectionize files after reading them rather
than while reading them.  Unfortunately, this was slower due to reduced overlap
if the file server could not keep up during the read-in phase.

In system 93, sectionization is once again done in parallel with read-in.  The
ancient bugs that the previous change was made to fix (failing to find certain
definitions) have been fixed in other ways.

[V] 3DIRED* 4No Longer Slow.*

Entry into 1DIRED* was slowed down accidentally by the installation of flavorized
editor buffer streams into system 91 in January.  It is fast again now.

[W]4 New DIRED command Control-Shift-E*.

The DIRED command 1Control-Shift-E *(Dired Edit File Two Windows) does a
Find File of the current file in another window.  It enters two-window mode if
you are presently in one-window mode.  The DIRED remains on the screen.

4System Operating Changes.*
	
	[1A*] Changes in Pathnames.
	[1B*] Unknown Directories in Logical Pathnames.
	[1C*] Garbage Collector.
	[1D*] New Init-file Feature, 1LOGIN-FORMS*.
	[1E*]4 *Trivial Errors Do Not Enter the Debugger.
	[1F*] Debugger Command 1Control-R* Will Return Any Number of Values.
	[1G*] Debugger Command 1Meta-R* to Reinvoke a Frame with Changed Arguments.
	[1H*] Screen Restored after Using the Cold-Load Stream.
	[1I*] Function 1EH* Improved.
	[1J*] 1:MAIL* Command in 1CC*.
	[1K*] 1SYSTEM* and 1GLOBAL* Packages Protected.
	[1L*] Better Handling of 1READ* Errors.
	[1M*] New 1Terminal* Commands: 1G*, 1V* and 1Control-A*.
	[1N*] Site Option Changes.
	[1O*] PEEK Improvements
	[1P*] Layouts Option in System Menu Changed.
	[1Q*] Serial Stream Buffer Capacity Can Be Specified.
	[1R*] Chaosnet Change.
	[1S*] 1QSEND* Features.
	[1T*] Operations on Processes.
	[1U*] New Function Names and Packages.
	[1V*] ZMAIL Changes.

[A] 4Changes in Pathnames.*

There have been many important changes in pathnames, regarding case
conversion and canonical types.  These are described in section 22.2 of the new
manual.

[B] 4Unknown Directories in Logical Pathnames.*

If a logical pathname is used with a directory that is not recognized on the
particular logical host, an error happens when the pathname is translated.  At
this time, you can specify a translation to use for the unknown logical directory,
either just for this time or for the rest of the session.

[C] 4Garbage Collector.*

Two new variables affect garbage collection.

1SI:GC-RECLAIM-IMMEDIATELY-IF-NECESSARY*, if non-1NIL*, says that batch garbage
collection should be done if, on flipping, there does not appear to be enough
free space to finish an incremental garbage collection.  Note that if the
variable 1SI:GC-RECLAIM-IMMEDIATELY* is non-1NIL*, garbage collection is 2always* done
in batch.

1SI:GC-FLIP-MINIMUM-RATIO* is used, when non-1NIL*, to control warnings about
having too little space to garbage collect.  Its value is a number, calibrated as the
fraction of data that is expected to be non-garbage, just like 1SI:GC-FLIP-RATIO*.
The difference between the two is that 1SI:GC-FLIP-RATIO* controls when garbage
collection is 2recommended*, whereas 1SI:GC-FLIP-MINIMUM-RATIO* controls when the
system considers the last possible time to do so.  If 1SI:GC-FLIP-MINIMUM-RATIO* is
1NIL*, 1SI:GC-FLIP-RATIO* serves both purposes.

[D] 4New Init-file Feature, 3LOGIN-FORMS*.

1LOGIN-FORMS** replaces 1LOGIN-SETQ*, 1LOGIN-FDEFINE*, and others.  It is a general
feature for saying, "Do this now, and undo it if I call 1LOGOUT*."

Simply use  1LOGIN-FORMS* surrounding the forms that you wish to have undone
at logout time, as in

1    (LOGIN-FORMS
      (SETQ FS:*DEFAULTS-ARE-PER-HOST* T))

LOGIN-FORMS* is not an AI program; it must be told how to undo each function
(such as 1SETQ*, 1DEFF*, etc.) that will be used immediately inside it.
This is done by giving the function name a 1:UNDO-FUNCTION* property which
is a function that takes a form as an argument and returns a form to undo
the original form.  For 1SETQ*, this is done as follows:

1    (DEFUN (SETQ :UNDO-FUNCTION) (FORM &AUX RESULTS)*
	1(DO L (CDR FORM) (CDDR L) (NULL L)*
	1    (COND ((BOUNDP (CAR L))*
		1   (PUSH `(SETQ ,(CAR L) ',(SYMEVAL (CAR L))) RESULTS))*
		1  (T (PUSH `(MAKUNBOUND ',(CAR L)) RESULTS))))*
	1`(PROGN . ,RESULTS))*

Properties are standardly provided for 1SETQ*, 1DEFF*, 1DEFUN*, 1DEFSUBST*.

[E]4 Trivial Errors Do Not Enter the Debugger.*

Trivial errors in expressions that you type in read-eval-print loops no longer
enter the debugger automatically.  Instead, they print the error message and then
ask whether to enter the debugger.  Answering "No" is equivalent to typing 1Abort*.

The advantage is that it is much faster now if you do not actually want the
debugger.  For a trivial error (defined as an error involving a function or variable
that actually appears in the expression you typed in),  one frequently wishes
to edit and resubmit the input, making the debugger unnecessary.

One reason you might wish to enter the debugger is if you may have typed a
variable or function name in the wrong package.  The check for symbols with
the same name in other packages is not made unless the debugger is entered.
(Checking for this is slow).  Answer Yes if you would like the check to be made.

To use this feature in your own read-eval-print loops, call
1SI:EVAL-ABORT-TRIVIAL-ERRORS* instead of 1EVAL*.

[F] 4Debugger Command3 Control-R* Will Return Any Number of Values.*

If you use the command 1Control-R* to return from a function whose caller is
asking for multiple values, you can now specify as many values as the caller is
ready to accept.  Type the 1End* key instead of a form, when you wish to stop
supplying values.

[G] 4Debugger Command 3Meta-R* to Reinvoke a Frame with Changed Arguments.*

The debugger command 1Meta-R* is like 1C-M-R* but asks for each argument.
You can type 1Space* to use the same argument as before, or type an
expression for a new value of the argument.  You can also terminate
with fewer arguments, or specify additional arguments.

[H] 4Screen Restored after Using the Cold-Load Stream.*

If the debugger needs to use the cold-load stream, it offers to restore the old
screen contents after you exit.  The same thing happens most of the times
when the system uses the cold-load stream automatically.  One exception
is if you set 1DEBUG-IO* to the cold load stream; this is because, when stepping
through calls and returns in that mode, it is desirable to keep the entire
debugger session on the screen.

[I] 4Function3 EH1 **Improved.*

The function 1EH*, which invokes the debugger on a specified process or stack
group, has new features.

If the argument is a process which had an error but is trying to use a deexposed
window, 1EH* allows the debugger to run in that process using the window that was
available to the function 1EH*.  So you can proceed, abort, throw or return in the
erring process.

If the argument is a process that is not waiting to run the debugger, a keyboard
break is caused in that process, much like what happens if you type 1C-M-Break*,
and then the debugger runs as in the previous case.

If the argument is a stack group, the debugger is called in 1EH*'s stack group,
in examine-only mode.

If you do not give an argument, 1EH* offers you a choice of processes waiting to
have errors handled.

[J] 3:MAIL 4Command in *CC.

1CC**, the program for debugging one machine from another machine, now has a
command 1:MAIL* for automatically stuffing a description of the reasons for a crash
into a bug report.  The description is basically the same as the output from the
1:WHY* command.  You can edit in your own comments before finally sending the
report.

[K] 3SYSTEM4 and* GLOBAL4 Packages Protected.

1READ*** will no longer create new symbols in packages which have subpackages,
such as 1SYSTEM* and 1GLOBAL*.  This is because creation there of a symbol which is
not supposed to exist can impact proper functioning of the subpackages.
1INTERN* will still create symbols there; 1READ* makes a special check.

[L] 4Better Handling of3 READ* Errors*.

The rubout handler now handles only errors which possess the condition name
1SYS:READ-ERROR*.  This includes all the errors you are likely to get from common
mistakes in syntax, including nonexistent packages, commas not inside
backquotes, etc.

The compiler now proceeds past all 1READ* errors, allowing the rest of the
expression to be read in properly.  A comma not inside a backquote, for
example, will not interfere with proper reading of the rest of the function.
As before, the error results only in a compiler warning.

[M] 4New 3Terminal* Commands: 3G*, 3V* and 3Control-A*.

1Terminal G** prints the garbage collector status.

1Terminal V* prints your new mail.  With an argument of 1, your old mail file is used instead;
with an argument of 0 you are prompted for whose mail you want to read.

1Terminal* 1Control-A* arrests nearly all processes.  The only processes it does not
arrest are the keyboard process, the mouse process, the screen manager process
and whichever process is having its state described in the who line.  This goes
with the existing 1Terminal A* command, which arrests 2only* the process described
in the who line.

In particular, the chaosnet processes are stopped, so after a minute or so goes by
other hosts will break any connections with you, if you do not unarrest these
processes within the time limit.

The purpose of 1Terminal Control-A* is to stop a background process that is
running away and not letting you do anything else.  It leaves just the bare
minimum of processes functioning for you to be able to examine things or
resume execution.  Normally you would continue by unarresting processes
selectively, possibly watching to see which process is the troublemaker.

To unarrest processes selectively, use 1Terminal Minus A*, which unarrests only
the process described in the who line.  Use 1Terminal 3 W* to rotate the who line
through all processes, so you can find the ones you wish to unarrest.  If you find
you have unarrested the misbehaving process, do 1Terminal A* to arrest it again.

After correcting the problem, do 1Terminal Minus Control-A* to unarrest all
processes.

(More precisely, 1Terminal Minus Control-A* removes only the arrest reasons put on
by 1Terminal* commands.  Processes arrested for other reasons remain so.
1Terminal Minus A* removes 2any* arrest reasons, and thus is guaranteed to make
the single affected process no longer be arrested.)

[N] 4Site Option Changes.*

The new, easy-to-remember way to load updated site files is

1        (MAKE-SYSTEM 'SITE)*

If the latest site files are already loaded, it does not do anything.

To recompile and reload site files as necessary, do

1        (MAKE-SYSTEM 'SITE 'COMPILE)*

Don't load site files by hand!

The new site option 1:DEFAULT-ASSOCIATED-MACHINE* specifies a default for the
associated machine of a Lisp machine which is not found in the 1LMLOCS* file.
The value is a host name string.  The new site option 1:USUAL-LM-NAME-PREFIX*
specifies a string which is the typical beginning of host names of Lisp machines
at your site.  At MIT, it is 1"CADR-"*.

The new site option 1:SPELL-SERVER-HOSTS* specifies a list of host names of hosts
that offer 1SPELL* servers for the editor's 1Meta-$* command.

The site option 1:MACHINES-WITH-LOCAL-FILE-SYSTEMS* is no longer used.
Whenever the 1LOCAL-FILE* system is loaded, your machine's host will be usable
in pathnames.  Lisp machines that run the file server should be mentioned in
the 1:CHAOS-FILE-HOSTS* site option to make their host names usable on other
machines.

Lisp machines that provide servers for the LMFILE file system should now be
mentioned in the new site option 1:LMFILE-SERVER-HOSTS*; the entry in this list is
just the hostname to use to refer to LMFILE there.  This should be one of the
nicknames in the host table of the machine itself The site option
1:CHAOS-LMFILE-SERVER-HOSTS* is no longer used.

The site option 1:INCLUDED-NON-CHAOS-HOSTS* tells the host table generator about
non-CHAOSnet hosts which should be included in SI:HOST-ALIST.  The name of
each host should be its official name that appears first in its line in the host
names source file (1SYS:CHAOS;HOSTS TEXT).*

[O] 4PEEK Improvements*

PEEK now does a better job of listening to keyboard input in preference to
doing output.

The Window Hierarchy mode is now fast enough for practical use.

When PEEK offers a menu to operate on a process, window, etc., the name of the
process or window appears in the menu, so you do not have to worry that the
mouse may have slipped and chosen the wrong one.

[P] 3Layouts4 Option in System Menu Changed.**

The 1Layouts* option in the system menu now records partially visible windows
when you save a layout, and brings them back to partial visibility when you
restore the layout.

[Q] 4Serial Stream Buffer Capacity Can Be Specified.*

You can specify the size of the input and output buffers when you create
a serial stream with the keyword arguments 1:INPUT-BUFFER-SIZE* and 1:OUTPUT-BUFFER-SIZE*
to the function 1SI:MAKE-SERIAL-STREAM.*  The argument values are numbers of
characters, which will be rounded up to approximately a multiple of 128.

[R] 4Chaosnet Change.*

The function 1CHAOS:GET-NEXT-PKT* takes an additional argument, 2check-conn-state*.
If this is non-1NIL*, the state of the conn is checked and an error is signaled
if the state is invalid.  The default value of the argument is 1T* if the
2no-hang-p* argument is 1NIL*, producing the same behavior as before.
All the arguments are:  2conn* &optional 2no-hang-p* 2whostate* 2check-conn-state*

[S] 3QSEND* 4Features.*

The function 1QSEND* (and therefore 1REPLY*) allows you two special commands
while you are typing in the message to be sent.  1C-M-Y* yanks in the
text of the last 1QSEND*-type message you received.  1C-M-E* switches to
Converse, inserting the text you have typed so far into the Converse
buffer so you can continue editing it there.

[T] 4Operations on Processes.*

Some operations on processes were omitted from the manual:

1:IDLE-TIME* -- returns the amount of time in seconds since this process
		  last ran, or 1NIL* if it has never run.

1:TOTAL-RUN-TIME* -- returns the amount of time this process has run,
		  in 60'ths of a second.  This includes cpu time and disk wait time.

1:DISK-WAIT-TIME* -- returns the amount of time this process has spent
		  waiting for disk I/O, in 60'ths of a second.

1:CPU-TIME* -- returns the amount of time this process has spent actually
		  executing instructions, in 60'ths of a second.

1:PERCENT-UTILIZATION* -- returns the fraction of the machine's time this
		  process has been using recently, as a percentage
	          (a number between 0 and 100.0).  The value is a weighted
		  average giving more weight to more recent instants.

1:RESET-METERS* -- zero out all the meters above.

[U] 4New Function Names and Packages.*

The symbols1 DESCRIBE-SYSTEM* and 1RECORD-SOURCE-FILE-NAME* have been
moved into the 1GLOBAL* package.

1QREPLY* is an alternative name for the 1REPLY* function, which sends a 1QSEND*
message back to the person who last sent such a message to you.

[V] 4ZMAIL Changes.

All compiled ZMAIL init files containing filter definitions must be recompiled.*
This is due to the incompatible changes in editor data structures.  The filter
definitions themselves are still good, but macros they use now expand differently.
The easiest way to do this is to call 1QC-FILE* with your init file source file's
name as first argument, and 1"ZMAIL.INIT"* or whatever is the actual init file's
name as the second argument.  4Note: the recompiled files will not run in
system 91.*

The default value of the option 1ZWEI:*MAIL-WINDOW-CONFIGURATION** is now
1:NORMAL*.  This value, a newly introduced alternative, means to use the same
window configuration as normally used for reading mail.  Sending mail this
way is much faster than before.

There are some new keyboard commands:
  1Control-F* -- searches for the next message in this buffer
		  that contains a specified string.
  1L* -- changes keywords of this message according to the
        keyword-filter associations of the filters the message fits.
        Like mouse-middle on 1Keywords*.
  1F* -- forwards this message.

There are new commands for editing "conversations", groups of messages
that refer to each other.  1M-X Select Conversation By References*
selects a temporary buffer holding all messages related by reference,
directly or indirectly, to the current message.
1M-X Delete Conversation By References* also exists; this deletes
all such messages (including at least the current message).  If you want
to see which messages those are, select the conversation first and then
do 1Map Over* of 1Delete* to delete the messages in it.  These commands,
and the existing reference commands, now use a hash table for speed.

When you enter the window configuration which has separate header
and text windows for composing mail, the header window becomes as
big as necessary (within limits) to show all the header lines that exist.

Case is preserved in names of filters and universes defined from now on.

ZMAIL can now handle mail files on Unix stored in either BABYL format
or the standard Unix mail file format.

The background process will not do anything if it sees that the
ZMAIL foreground process is not idle.

You can put a copy of a message you send into one of your
own mail files by putting an 1FCC:* field in the message.  This should
contain one or more pathnames separated by commas.  The pathnames
should be names of mail files which ZMAIL has or can read in.
Copies of the mail are stuck directly into those mail files within ZMAIL.

You can direct a message solely to one of your own mail files by means
of an 1FTo:* header field.  Like the 1FCC:* field, this field contains one
or more filenames separated by commas.  Unlike 1FCC:*, an 1FTo:* field
satisfies ZMAIL's demand for some sort of "To" recipient, so you do
not need any 1To:* field.  This replaces the "local mail" facility
(which is still provided, however).

1BCC:* and 1BFCC:* fields are now supported in messages sent with ZMAIL.
1BCC:* sends copies to mailboxes and 1BFCC:* puts them into mail files
within ZMAIL itself.  The 1BCC:* and 1BFCC:* recipients are not listed
in the copies sent to the other recipients.
