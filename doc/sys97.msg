-*-Fonts:TR12,cptfont,TR12I,HL12B,TR12B; Mode:Text-*-

4Notes on System 97.*

System 97 comes with microcode 257.  It will not run with system 94 microcodes,
nor will microcode 257 run with systems before 97.  97 was initially released with
microcode 255, but 257 fixes an important bug and should be installed in all
system 97 bands.

All files must be recompiled in system 97 in order to work in system 97.
Files compiled in system 97 can usually be used in older system versions.
Problems are sometimes caused by the symbols which are global in 97
but not global in 94; see the end of the following page.

To debug a system 97 from another machine running system 94, you must
have patch 94.41 loaded.  97 can also debug 94.

4Noteworthy Features.*
  Selective Undo in ZWEI.
  Incremental band saving.
  New package system.

4Incompatibilities Now.
1     
     A**] 4Package System Replaced.
1     B**] 4Case-Sensitive and -Insensitive String Comparison Functions.
1     C**] 3'COMPILE4 No Longer Needed in *PROGN4.
1     D***] 3EVAL-WHEN4 Rationalized.
1     E***] 3TV:SHEET-FORCE-ACCESS4 Does Not Prepare the Sheet.
1     F***] 4Change to 3SI:FULL-GC-INITIALIZATION-LIST*.
1     G**] 4Flushed Processes.
1     H**] 3PROGV4 With More Variables Than Values.
1     I***] 4New Global Symbols -- Backwards Compatibility Problems.

1A**] 4Package System Replaced.*

There is now a new package system, essentially that of Common Lisp plus a few
compatibility features.  Its highlights are:

 Symbols in a package are now marked either internal or external.
 Only the external symbols are inherited by other packages.

 Packages are no longer arranged in a hierarchy; inheritance is no
 longer required to be transitive.  Now you can specify exactly which
 other packages' external symbols are to be inherited by a new package.

 1KEYWORD* and 1USER* are now two distinct packages.  No symbol is shared
 between 1KEYWORD* and 1GLOBAL*, so that 1COMPILE* and 1:COMPILE* are two
 distinct symbols, and so are 1NIL* and 1:NIL*.  Your code must now be
 careful to use the correct symbol (keyword or global) in all such cases,
 where it used to make no difference.

For full details, see the file 1SYS: DOC; CLPACK TEXT*.  It should be noted
that similar changes will probably be in the next Symbolics release.

1B*] 4Case-Sensitive and -Insensitive String Comparison Functions.*

The function 1EQUAL* now considers the strings 1"A"* and 1"a"* to be distinct.
Use 1EQUALP* if you wish to ignore case in the comparison.
This is a Common Lisp change.

1CHAR-EQUAL* and 1STRING-EQUAL* always ignore case.
To consider case in comparing characters or strings this way,
use 1=* for characters and the new function 1STRING=* for strings.

1SAMEPNAMEP* now considers case significant.

The functions of the 1STRING-SEARCH* series now take an extra optional
argument which says whether to consider case.

The old flag 1ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON* is now used
only by the 1%STRING-SEARCH* and 1%STRING-EQUAL* microcode primitives.
These primitives now consider font significant as well as case when the
flag is non-1NIL*.

1C*] 3'COMPILE4 No Longer Needed in *PROGN4.**

Any 1PROGN* encountered at top level by the compiler is now handled
by treating each element as if it had been found at top level.
Macros that used to expand into 1(PROGN 'COMPILE 2forms*...)*
can now expand into just 1(PROGN 2forms*...)*

This is a Common Lisp change.

1D*] 3EVAL-WHEN4 Rationalized.**

The treatment of 1(EVAL-WHEN (LOAD) 2forms*...)* by the compiler is now
identical to the treatment of forms encountered with no 1EVAL-WHEN*.  They are
put into the file to be evaluated on loading, or compiled if they are 1DEFUNs*,
and any macros defined are made available for expansion during the compilation.

As a consequence, you can no-op an 1EVAL-WHEN* by supplying 1(LOAD EVAL)*
as its first argument.  It is then equivalent in all cases to no 1EVAL-WHEN* at all.

Nested 1EVAL-WHEN*s now effectively intersect their list of times to evaluate.
As a result,
1   (EVAL-WHEN (COMPILE LOAD EVAL)
     2compile-time-forms*...
     (EVAL-WHEN (LOAD EVAL)
       2forms*...))*
treats the 2forms* in the ordinary manner, overriding the special treatment
given to the 2compile-time-forms*.
1   (EVAL-WHEN (COMPILE) (EVAL-WHEN (LOAD) 2ignored-forms*...))*
does not do anything with the 2ignored-forms*.

Please let me know if this causes any problem.

1E*] 3TV:SHEET-FORCE-ACCESS4 Does Not Prepare the Sheet.**

The macro 1TV:SHEET-FORCE-ACCESS* (documented in the window system manual)
used to put a 1TV:PREPARE-SHEET* into its expansion unless an optional argument
was supplied to inhibit doing so.

It turned out that most uses of the macro had no need to prepare the sheet but
were neglecting to supply the optional argument.  Since combining the
two facilities is unmodular, the prepare-sheet has simply been flushed from
1TV:SHEET-FORCE-ACCESS*.  If you really want to do one, simply write a
1TV:PREPARE-SHEET* explicitly in the body of the 1TV:SHEET-FORCE-ACCESS*.

The old optional 2dont-prepare-flag* argument is still accepted but has no effect now.

1F*] 4Change to 3SI:FULL-GC-INITIALIZATION-LIST*.*

The 1SI:FULL-GC-INITIALIZATION-LIST* initializations are now run
before the garbage collection in 1SI:FULL-GC*, rather than after.
A new initialization list, 1SI:AFTER-FULL-GC-INITIALIZATION-LIST*,
is run after.  The old list which now runs before GC can be requested
with the keyword 1:FULL-GC* in 1ADD-INITIALIZATION*, and the
new list which runs after can be requested with 1:AFTER-FULL-GC*.

This change is for greater compatibility with Symbolics systems.

1G*] 4Flushed Processes.*

A flushed process now has the symbol 1SI:FLUSHED-PROCESS*
as its wait function.  This function is equivalent to 1FALSE* in that
it always returns 1NIL*, but it is distinguishable from 1FALSE*.
Thus, flushed processes can reliably be distinguished from
those that have done 1PROCESS-WAIT-FOREVER*.

1H*] 3PROGV4 With More Variables Than Values.**

The function 1PROGV* accepts a list of variables and a list of values.
In the past, if the list of variables was longer, 1NIL* was used in place of the
missing values.  Now, in this case, the extra variables which have no corresponding
values will be made "unbound".  This is a Common Lisp change.

1I*] 4New Global Symbols -- Backwards Compatibility Problems.*

The many new global symbols present in system 97 for Common Lisp
do not require changes in programs unless you are already using
the same names for other things, but they sometimes cause
trouble for compiling a file in system 97 and loading it in system 94.

For example, in system 97, the symbol 1PATHNAME* is global.  If your
code refers to 1FS:PATHNAME* (the base flavor of pathnames), when
it is read into system 97 during compilation this will get the global
symbol.  The QFASL file thus produced will not say "package FS" for
that symbol, because the distinction no longer exists.  If the QFASL
file is then loaded into system 94, it will refer to a symbol 1PATHNAME*
in whatever package you are loading into.  This will not be the same symbol
as 1FS:PATHNAME*, so the program will not work.

Most problems of this sort can be fixed by 1INTERN*ing the intended symbol
in the package you are loading into.  In the above example, doing
1    (INTERN 'FS:PATHNAME '2mypackagename*)*
before loading the file would make it work.  You would probably do this
in the file containing the 1PACKAGE-DECLARE*.  It will have no effect
when loading into system 97.

4Incompatibilities Coming In Future Systems.
1     
     A**] 4Decimal Radix Will Become the Default.
1     B**] 4Row-Major Order of Array Indices.
1     C**] 4The Flag Bit Will Cease to Exist.
1     D**] 3COMPILER:RETURN-FROM-T4 May Go Away.
1     E***] 4Bit Fields in Character Codes May Be Rearranged.

1A**] 4Decimal Radix Will Become the Default.*

In some future system, base 10. will become the default.  This is a Common Lisp
change.  However, it will still be possible to specify the radix for each file
individually.  To avoid any difficulties, place 1Base:* 18;* in the attribute list (the
1-*-* line) of any file which is supposed to be in octal.

1B*] 4Row-Major Order of Array Indices.*

Arrays are currently stored in column-major order, but for Common Lisp it is
necessary to store them in row-major order.  This change is likely to happen
in the next system released after 97.  For more details on how to ride this change
conveniently, see section 8.8 of the Lisp Machine manual.

1C*] 4The Flag Bit Will Cease to Exist.*

In a future version of the system, there will not be a flag bit.
This is to enable pointers to be 25 bits long.
If you use the flag bit, you should start converting now to stop using it.

1D*] 3COMPILER:RETURN-FROM-T4 May Go Away.**

If you have macros that produce expansions that use 1COMPILER:RETURN-FROM-T*,
you should start changing them to expand into 1BLOCK*s instead.  1BLOCK* is a new
Common Lisp control construct, described in detail below.  You can use a
gensym 1BLOCK* name to avoid any possibility of interference with the code in
which the macro is used.

1E*] 4Bit Fields in Character Codes May Be Rearranged.*

This will probably not require you to change your code, if you refer
to characters only through standard byte pointers such as 1%%KBD-CONTROL* and
standard types of character naming such as 1#\Control-F*, but it will require
recompilation of all files that contain codes for control characters.

4Compatible Programming Changes:
1     
     A**] 4Many New Global Symbols.
1     B**] 4New Macro 3PUSHNEW*.
1     C**] 4Common Lisp Control Constructs 3BLOCK* and 3TAGBODY*.
1     D**] 3LEXPR-FUNCALL4 And *APPLY4 Now Synonymous.
1     E***] 3:ALLOW-OTHER-KEYS4 Special Keyword for *&KEY4 Arguments.
1     F***] 4Character Objects Exist.
1     G**] 3READING-FROM-FILE*, 3READING-FROM-FILE-CASE4 Special Forms.
1     H]*** 4New Function 3FS:EXTRACT-ATTRIBUTE-BINDINGS*.
1     I**] 3GET4 Accepts Third Argument.
1     J***]4 Common Lisp Numeric Functions.
1     K**] 3CHAOS:SEND* 4Is No More.
1     L**] 4New Function 3DEFF-MACRO*.
1     M**] 4Named Structure Operation 3:FASLOAD-FIXUP*.
1     N**] 3FUNCALL4ing a Named Structure.
1     O***] 4Note on 3DEFSELECT* and Named Structures.
1     P**] 3TV:MAKE-WINDOW4 Now Identical to *MAKE-INSTANCE4.
1     Q***] 4Window Manual Error: 3TV:MOUSE-WAKEUP* and 3TV:MOUSE-RECONSIDER*.
1     R**] 4Delaying Flavor Recompilation.
1     S**] 3:INSTANCE-AREA-FUNCTION4 Flavor Keyword.
1     T***] 3%PAGE-STATUS4 Change.
1     U***] 4Testing Values for Pointerhood.
1     V**] 4Infix Expressions.
1     W**] 3EH:REQUIRE-PDL-ROOM4.
1     X***] 4Rubout Handler Option 3:DONT-SAVE*.
1     Y**] 4New Host Operation 3:NETWORK-ADDRESSES*.
1     Z**] 4Error in Manual: 3SI:FILE-INPUT-STREAM-MIXIN*.
1     AA**] 4New Error Condition 3SYS:NO-SERVER-UP*.
1     BB**] 4New Function 3CHAOS:UP-HOSTS*.
1     CC**] 3GET4 and *GETHASH4 with Three Arguments.
1     DD***] 4New Macro 3TYPECASE**.

1A*] 4Many New Global Symbols.*

The file 1SYS: DOC;* 1CLISP-GLOBAL-SORTED TEXT* contains a list of many
symbols inserted in the 1GLOBAL* package because they are standard
Common Lisp constructs.  Some of them have been defined already
in system 97; more will be defined as patches, and others in the next system.

1B*] 4New Macro 3PUSHNEW*.

1    (PUSHNEW 2elt* 2list*)**

is equivalent to

1    (OR (MEMQ 2elt* 2list*)
        (PUSH 2elt* 2list*)

C*] 4Common Lisp Control Constructs 3BLOCK* and 3TAGBODY*.

1BLOCK** takes a block name and a body:

1    (BLOCK 2name* 2body*...)*

and executes the 2body*, while allowing a 1RETURN-FROM* 2name* to be used within
it to exit the 1BLOCK*.  If the 2body* completes normally, the values of the last
body form are the values of the 1BLOCK*.

A 1BLOCK* whose 2name* is 1NIL* can be exited with plain 1RETURN*, as well as with
1RETURN-FROM* 1NIL*.

1BLOCK* can be thought of as the essence of what named 1PROG*s do, isolated and
without the other features of 1PROG* (variable binding and 1GO* tags).

Every function defined with 1DEFUN* whose name is a symbol contains an automatically
generated 1BLOCK* whose name is the same as the function's name, surrounding the
entire body of the function.

1TAGBODY*, on the other hand, is the essence of 1GO* tags.  A 1TAGBODY* form contains
statements and tags, just as a 1PROG*'s body does.  A symbol in the 1TAGBODY* form is
a tag, while a list is a statement to be evaluated.  The value returned by a
1TAGBODY* is always 1NIL*.  1TAGBODY* does not have anything to do with 1RETURN*.

1PROG* is now equivalent to a macro

1    (MACRO PROG (FORM)*
	1(LET* ((NAME (AND (SYMBOLP (CADR FORM)) (CADR FORM)))*
		1 (VARS (IF NAME (CADDR FORM) (CADR FORM)))*
		1 (BODY (IF NAME (CDDDR FORM) (CDDR FORM))))*
	1  (IF NAME*
		1`(BLOCK ,NAME*
		1   (BLOCK NIL*
		1     (LET ,VARS*
			1 (TAGBODY . ,BODY))))*
	1    `(BLOCK NIL*
		1 (LET ,VARS*
		1   (TAGBODY . ,BODY))))))*

if we ignore the added complication of 1PROG*s named 1T* and 1RETURN-FROM-T*.

1D*] 3LEXPR-FUNCALL4 And *APPLY4 Now Synonymous.

1APPLY*** now accepts any number of arguments and behaves like 1LEXPR-FUNCALL*.
1LEXPR-FUNCALL* with two arguments now works the way 1APPLY* used to, passing
an explicit rest-argument rather than spreading it.  This eliminates the old
reasons why 1LEXPR-FUNCALL* was not the best thing to use in certain cases,
and paves the way for 1APPLY* to translate into it.

1E*] 3:ALLOW-OTHER-KEYS4 Special Keyword for *&KEY4 Arguments.**

When a function whose argument list includes 1&KEY* is called, if the keyword arguments
passed include the keyword 1:ALLOW-OTHER-KEYS* with a non-1NIL* value, then any
unrecognized keywords present in the argument list will not be an error.
You do not have to do anything in the function definition to make 1:ALLOW-OTHER-KEYS*
work.

For example,

1    (DEFUN FOO (&KEY A B) (LIST A B))
    (FOO ':A 3 ':B 4 ':C 5 ':ALLOW-OTHER-KEYS T)
       *; The 1':C* and its value are ignored
1       *; rather than causing an error.

It does not matter where among the keywords the 1:ALLOW-OTHER-KEYS* is found.

1F*] 4Character Objects Exist.*

A new data type, 1DTP-CHARACTER*, has been created for character objects.  These
print out as things like 1#/A* for the character A.  This printed representation
can be read back in.  Note that this is not the same as the Common Lisp printed
representation for characters; that cannot be used in Zetalisp because it is
already defined and means something else.  Zetalisp and Common Lisp syntax
differ in a number of other ways as well.  When Common Lisp syntax is
provided, in another system version this fall, Zetalisp syntax will remain available
and will be the same as it is now.

Characters can be used in arithmetic like fixnums, and compared
like fixnums.  1(= 101 #/A)* is 1T*, but 1(EQUAL 101 #/A)* is 1NIL*.
1EQUALP* when comparing two characters ignores case, so that
1(EQUALP #/A #/a)* is 1T*.

Characters can also be used to index arrays, just like fixnums,
and can be printed out on windows and to files.

Characters evaluate to themselves.

1G*] 3READING-FROM-FILE*, 3READING-FROM-FILE-CASE4 Special Forms.**

These two special forms are a straightforward aid in writing code that reads Lisp
forms from a file, while obeying the attribute list of the file.  (The attribute list
file's pathname object or generic pathname object 2is not updated* with this 
special form.)

The following form prints out the result of evaluating each form in the file:

1  (FS:READING-FROM-FILE (FORM FILE)
     (FORMAT T "Values from ~S are: " FORM)
     (FORMAT:PRINT-LIST T "~S" (MULTIPLE-VALUE-LIST (EVAL FORM))))

FS:READING-FROM-FILE-CASE* is a cross between 1FS:READING-FROM-FILE* and
1WITH-OPEN-FILE-CASE*, except there's an additional argument 2error* for use
in the clauses.

1   (FS:READING-FROM-FILE-CASE (FORM FILE ERROR)
       ((FS:FILE-NOT-FOUND
          (FORMAT T "~&Options file ~A not found, using default values." FILE))*
 1       (FS:FILE-ERROR*
	   1  (FORMAT T "~&Error: ~A" (SEND ERROR ':REPORT-STRING)))
        (:NO-ERROR* 1(PROCESS-OPTION FORM))))*

There must be a 1:NO-ERROR* clause.

1H]* 4New Function 3FS:EXTRACT-ATTRIBUTE-BINDINGS*.

1FS:EXTRACT-ATTRIBUTE-BINDINGS2 stream***
   returns two values: a list of variables, and a corresponding list of values to bind them to,
   to set up an environment to read data from 2stream* in accordance with 2stream*'s attribute list.

1I*] 3GET4 Accepts Third Argument.

1    (GET 2symbol* 2property* 2default-value*)***

returns 2default-value* if the requested property is not present.

1J*]4 Common Lisp Numeric Functions.*

Nearly all the Common Lisp numeric functions, with the exception of transcendental
functions with complex arguments or values, are now implemented.

Two remaining exceptions:
   1//* still truncates fixnums, and you must still use
   1%DIV* to divide and make a rational from them.

   1REM* still is and will remain a function for nondestructive removal
   of elements from a list, rather than a numeric remainder function.

1K*] 3CHAOS:SEND* 4Is No More.*

The symbol 1CHAOS:SEND* no longer exists;1 SEND *used to be shadowed in1 *the
1CHAOS* package, but it isn't anymore.  1CHAOS:SEND* did not have a definition,
value, or properties for quite a while.

1L*] 4New Function 3DEFF-MACRO*.

1DEFF-MACRO** &quote 2function-spec* &eval 2definition*
  defines 2function-spec* as 2definition*, just like 1DEFF*.
  The difference comes in compiling a file, where the compiler assumes
  that 1DEFF-MACRO* is defining a macro and makes the definition available
  for expansion during this compilation.  1DEFF*, on the other hand, is just
  passed through to be evaluated when the file is loaded.
  To use 1DEFF-MACRO* properly, 2definition* must be a list starting with
  1MACRO* or a suitable subst function (a list starting with 1SUBST* or a
  compiled function which records an interpreted definition which is
  a list starting with 1SUBST*).

1M*] 4Named Structure Operation 3:FASLOAD-FIXUP*.*

The named structure operation 1:FASLOAD-FIXUP* is invoked by 1FASLOAD* whenever
a named structure is created according to data in a QFASL file.  This operation
can do whatever is necessary to make the structure properly valid, in case just
reloading it with all its components is not right.  For most kinds of structures,
this operation need not do anything; it is enough if it does not get an error.

1N*] 3FUNCALL4ing a Named Structure.**

You can now funcall a named structure to invoke a generic operation on it,
just as you would a flavor instance.  In fact, you can have code which operates
on named structures and flavor instances indiscriminately, if you make sure
that the named structures you are using support whichever operations you
plan to use.

For example,
1    (FUNCALL PACKAGE ':DESCRIBE)*
invokes the 1:DESCRIBE* operation on the current package, just as
1    (NAMED-STRUCTURE-INVOKE ':DESCRIBE PACKAGE)*
would do.

This has not been made ultra-fast, but that can be done in a future
microcode release once all the machines have the full control memory plugged in.

1O*] 4Note on 3DEFSELECT* and Named Structures.

1DEFSELECT**, by default, defines the function to signal an error if it is called
with a first argument not defined in the 1DEFSELECT* (except for 1:WHICH-OPERATIONS*,
which is defined implicitly by 1DEFSELECT*).

If you use 1DEFSELECT* to define the handler function for a named structure type,
and you use this default behavior, you will get errors at times when the system
invokes operations that you may not know or care about, such as 1:SXHASH* or
1:FASLOAD-FIXUP*.

To avoid this problem, specify 1IGNORE* as the default handler in the 1DEFSELECT*.
1IGNORE* accepts any arguments and returns 1NIL*.

1P*] 3TV:MAKE-WINDOW4 Now Identical to *MAKE-INSTANCE4.**

Windows can now be created with 1MAKE-INSTANCE* just like any other flavor instances.
The function 1TV:MAKE-WINDOW* will be supported indefinitely since it is so widely used.

1Q*] 4Window Manual Error: 3TV:MOUSE-WAKEUP* and 3TV:MOUSE-RECONSIDER*.*

The window manual says that you should call the function 1TV:MOUSE-WAKEUP* to
report a change in screen configuration.  This is not exactly true.

The function 1TV:MOUSE-WAKEUP* causes the mouse process to look again at the
position of the mouse.  It is called by the function 1TV:MOUSE-WARP*, so that the
mouse will be tracked to its specified new position.  It is also the thing to use if
you redisplay a menu-like window with a new set of menu items, for example,
so that the mouse process will notice whether the mouse position is now inside a
different menu item.

However, actual changes in the window configuration may make it necessary to
force recomputation of which window owns the mouse.  This is done by setting
the variable 1TV:MOUSE-RECONSIDER* non-1NIL*.  Calling 1TV:MOUSE-WAKEUP* may not
be enough, since the current mouse position may still be inside the old screen
area of a no-longer-eligible window.

1R*] 4Delaying Flavor Recompilation.*

Normally the system recompiles combined methods automatically when
you make a change that requires this.  If you plan to make more than one change,
you might wish to recompile only once.  To do this, set the variable
1SI:*DONT-RECOMPILE-FLAVORS** non-1NIL* before you make the change1s.*
Then set it back to 1NIL*, and use 1RECOMPILE-FLAVOR* to perform
the appropriate recompilations.

1S*] 3:INSTANCE-AREA-FUNCTION4 Flavor Keyword.**

You can now control which area flavor instances are consed in, on a per-flavor basis,
by giving a flavor an instance-area function.  This is a function which will be
called whenever the flavor is instantiated, and expected to return the area to
cons in (or 1NIL*, if it has no opinion).  The function is passed one argument,
the init-plist, so if you want to have an init option for the caller to specify the
area, the instance-area function can use 1GET* to get the value the caller specified.

The instance-area function is specified with

     1(:INSTANCE-AREA-FUNCTION 2function*)*

in the flavor definition, and it is inherited by flavors which use this one
as a component.

1T*] 3%PAGE-STATUS4 Change.**

The subprimitive 1%PAGE-STATUS* now returns the entire first word
of the page hash table entry for a page, if the page is swapped in;
or 1NIL* for a swapped-out page, or for certain low-numbered areas
(which are all wired, so their pages' actual statuses never vary).
The argument is an address in the page you are interested in--data type is irrelevant.
The 1%%PHT1-* symbols in 1SYS: SYS; QCOM LISP* are byte pointers
you can use for decoding the value.

1U*] 4Testing Values for Pointerhood.

1%POINTERP** 2object*
  returns non-1NIL* if 2object* points to storage.  For example,
  1(%POINTERP "FOO")* is 1T*, but 1(%POINTERP 5)* is 1NIL*.

1%P-POINTERP* 2location*
  returns non-1NIL* if the contents of the word at 2location* points to storage.
  This is similar to 1(%POINTERP (CONTECTS 2location*))*, but the latter may get
  an error if 2location* contains a forwarding pointer, a header type, or an
  unbound marker.  In such cases, 1%P-POINTERP* will correctly tell you whether
  the header or forward points to storage.

1%P-POINTERP-OFFSET* 2location* 2offset*
  similar to 1%P-POINTERP* but operates on the word 2offset* words beyond 2location*.

1%P-CONTENTS-SAFE-P* 2location*
  returns non-1NIL* if the contents of word 2location* are a valid Lisp object, at least
  as far as data type is concerned.  It is 1NIL* if the word contains a header type,
  a forwarding pointer, or an unbound marker.  If the value of this function is
  non-1NIL*, you will not get an error from 1(CONTENTS 2location*).

%P-CONTENTS-SAFE-P-OFFSET* 2location* 2offset*
  similar to 1%P-CONTENTS-SAFE-P* but operates on the word 2offset* words beyond 2location*.

1%P-SAFE-CONTENTS-OFFSET* 2location* 2offset*
  returns the contents of the word 2offset* words beyond 2location* as accurately as
  possible without getting an error.

  If the data there are a valid Lisp object, it is returned exactly.
  If the data are not a valid Lisp object but do point to storage,
  the value returned is a locative which points to the same place in storage.
  If the data are not a valid LIsp object and do not point to storage,
  the value returned is a fixnum with the same pointer field.

1%POINTER-TYPE-P* 2data-type*
  returns non-1NIL* if the specified data type is one which points to storage.
  For example, 1(%POINTER-TYPE-P DTP-FIX)* returns 1NIL*.

1V*] 4Infix Expressions.*

You can now include infix expressions in your Lisp code.
For example,
1   #X:Y+CAR(A1[I,J])*
is equivalent to
1   (SETQ X (+ Y (CAR (AREF A1 I J))))

#* begins an infix expression, and 1* ends it.

The atomic terms of infix expressions include

      symbols: use 1\* to quote special characters.

      numbers: any valid Lisp real or imaginary number is accepted.
        Complex numbers can be constructed by addition or subtraction.

      strings: the same as in ordinary Lisp syntax.

      raw Lisp data: 1!* followed by any Lisp expression, as in
          1# FOO . !(CAR BAR)  * 1=> * 1(LIST* FOO (CAR BAR))*

Combining operations:

Highest precedence
	2a1 [ *i1 ]**	1(AREF 2a* 2i*)*
	2a1 [ *i1, *j1 ]**	1(AREF 2a* 2i* 2j*)*		and so on
  examples
	1X[I,J+3]   =>   (AREF X (+ J 3))
      (GET-MY-ARRAY(FOO))[I]   =>   (AREF (GET-MY-ARRAY FOO) I)*

	2f1 ( *a1 )**	1(2f* 2a*)*
	2f1 ( *a1, *b1 )**	1(2f* 2a* 2b*)*		and so on
  examples
	1CAR(X)   =>   (CAR X)*

	1( 2exp* )*	2exp*			parentheses control order of evaluation.
  examples
	1(X+1)*Y   =>   (* (+ X 1) Y)*

	1( 2e1*, 2e2* )*	1(PROGN 2e1* 2e2*)*	and so on
  examples
	1(X:5, X*X)   =>   (PROGN (SETQ X 5) (* X X))*

	1[ 2elt* ]*	1(LIST 2elt*)*
	1[ 2e1*, 2e2* ]*	1(LIST 2e1* 2e2*)*		and so on
  examples
	1[!'X,Y,Z]   =>   (LIST 'X Y Z)*

Precedence1 180* on left, 120* on right
	2a1 : *b*	1      (SETF 2a* 2b*)*
  examples
	1X: 1 + Y: Z+5   =>   (SETQ X (+ 1 (SETQ Y (+ Z 5))))*

Precedence1 140*
	2a1 ^ *b*	1      (EXPT 2a* 2b*)*		right associative
  examples
	1X ^ N ^ 2   =>   (EXPT X (EXPT N 2))*

Precedence1 120*
	2a1 * *b*	1      (* 2a* 2b*)*
	2a1 * *b1 * *c*	1(* 2a* 2b* 2c*)*		and so on
	2a1 / *b*	1      (// 2a* 2b*)*
	2a1 / *b1 / *c*	1(// 2a* 2b* 2c*)*		and so on

Precedence1 100*
	1- 2a**		1(- 2a*)*
	2a1 + *b*	1      (+ 2a* 2b*)*
	2a1 + *b1 + *c*	1(+ 2a* 2b* 2c*)*		and so on
	2a1 - *b*	1      (- 2a* 2b*)*
	2a1 - *b1 - *c*	1(- 2a* 2b* 2c*)*		and so on

Precedence1 95*
	2a1 . *b*	1      (LIST* 2a* 2b*)*
	2a1 . *b1 . *c*	1(LIST* 2a* 2b* 2c*)*	and so on
	2a1 @ *b*	1      (APPEND 2a* 2b*)*
	2a1 @ *b1 @ *c*	1(APPEND 2a* 2b* 2c*)*	and so on

Precedence1 80*
	2a1  *b*	1      (MEMQ 2a* 2b*)*
	2a1 = *b*	1      (= 2a* 2b*)*
	2a1 = *b1 = *c*	1(= 2a* 2b* 2c*)*		and so on
1        <, >, , ,  *are like1 =.*

Precedence1 70*
	1NOT 2a**	1      (NOT 2a*)*

Precedence1 60*
	2a1 AND *b*		1(AND 2a* 2b*)*
	2a1 AND *b1 AND *c*	1(AND 2a* 2b* 2c*)*	and so on

Precedence1 50*
	2a1 OR *b*			1(OR 2a* 2b*)*
	2a1 OR *b1 OR *c*		1(OR 2a* 2b* 2c*)*	and so on

Precedence 145* for 2c*, 125* for 2a* and 2b*.
	1IF 2c* THEN 2a**		1(IF 2c* 2a*)*
	1IF 2c* THEN 2a* ELSE 2b** 	1(IF 2c* 2a* 2b*)*

It is easy to define new operators.  See 1SYS: IO1; INFIX LISP*.

1W*] 3EH:REQUIRE-PDL-ROOM4.

1EH:REQUIRE-PDL-ROOM*** 2regpdl-space* 2specpdl-space*
  makes the current stack group larger if necessary, to make sure that
  there are at least 2regpdl-space* free words in the regular pdl, and at
  least 2specpdl-space* free words in the special pdl, not counting what
  is currently in use.

1X*] 4Rubout Handler Option 3:DONT-SAVE*.*

If you specify a non-1NIL* value for the 1:DONT-SAVE* option when
you invoke the rubout handler, then the input string read will not
be saved on the input ring for the 1Control-C* command.

This feature is used by 1YES-OR-NO-P* so that the Yes or No
does not get saved.

1Y*] 4New Host Operation 3:NETWORK-ADDRESSES*.*

The operation 1:NETWORK-ADDRESSES*, on a host object, returns an alternating
list of network names and lists of addresses, such as
1    (:CHAOS (3104) :ARPA (106357002))*
You can therefore find out all networks a host is known to be on, etc.

1Z*] 4Error in Manual: 3SI:FILE-INPUT-STREAM-MIXIN*.*

The flavor documented in the manual as 1SI:FILE-INPUT-STREAM-MIXIN*
is actually called 1SI:INPUT-FILE-STREAM-MIXIN*.  Similarly, what is described
as 1SI:FILE-OUTPUT-STREAM-MIXIN* is actually called 1SI:OUTPUT-FILE-STREAM-MIXIN*.

1AA*] 4New Error Condition 3SYS:NO-SERVER-UP*.*

The error condition 1SYS:NO-SERVER-UP* is signaled by certain requests for
a service from any available network host, when no suitable host is currently
available.

1BB*] 4New Function 3CHAOS:UP-HOSTS*.

1CHAOS:UP-HOSTS** 2host-list* &optional 2number-of-hosts* (2timeout* 250.)
  returns a list of all the hosts in 2host-list* which are currently accessible
  over the chaos network.  2host-list* is a list of host names and/or host objects.
  The value is always a list of host objects, possibly 1NIL* for none of them.

  If 2number-of-hosts* is non-1NIL*, it should be a positive integer; when that
  many hosts have responded, 1CHAOS:UP-HOSTS* returns right away without
  bothering to listen for replies from the rest.

  2timeout* is how long it takes, without response, to decide that a host is down.

1CC*] 3GET4 and *GETHASH4 with Three Arguments.

1GET*** and 1GETHASH* now take an optional third argument, which is a default value
to be returned as the value if no property or hash table entry is found.

1DD*] 4New Macro 3TYPECASE**.

There is now a 1TYPECASE* macro, compatible with Common Lisp.

4Miscellaneous User Interface Changes
1     
     A**] 4Incremental Bands.
1     B**] 4Evaluation in the Debugger.
1     C**] 4Beep Types.
1     D**] 3Terminal T* 4Change.
1     E]** 4New Function3 SI:VIEW-UNFINISHED-PATCHES
1     F***] 4VMS Default Device.
1     G**] 4Improved File Error Handling.
1     H**] 4Some Functions' Keyword Args Missing in 3ARGLIST*.
1     I**] 4Evaluating/Compiling Multi-Font Files.

1A**] 4Incremental Bands.*

You can now save disk space when you save a band containing your favorite
programs by making an 2incremental band*; that is, a band which contains the
differences between the Lisp world you want to save and another band (the one
you originally loaded).

To do this, provide a non-1NIL* third argument to 1DISK-SAVE*, as in
1    (DISK-SAVE "LOD4" NIL T)*
It takes a couple of extra minutes to find what pages need to be saved.
You can restore the incremental band with 1DISK-RESTORE* or boot it like
any other band.

Booting or restoring an incremental band works by first booting the original band
and then copying in the differences that the incremental band records.  This
takes only a little longer than booting the original complete load band.

The original band to which an incremental band refers must be a complete load.
When you update a standard system band (loading patches, for instance) you
should always make a complete load, so that the previous system band is not
needed for the new one to function.

The incremental band contains the partition name of the original band;
1DESCRIBE-PARTITION* prints it out for you.  That original band must still exist,
with the same contents, in order for the incremental band to work properly.  The
incremental band contains error check data used to verify this.  The error
checking is done by the microcode when the incremental band is booted, but it
is also done by 1SET-CURRENT-BAND*, so that you will not be permitted to make an
incremental band current if it is not going to work.

1B*] 4Evaluation in the Debugger.*

When you evaluate an expression in the debugger, it is evaluated in the
binding environment of the frame that is current in the debugger.

Initially, the debugger starts out with its current frame being the one in which
the error happened.  Therefore, your expressions are evaluated in the environment
of the error.  However, you now have the option of evaluating them in other
environments instead.

The debugger command 1Meta-S* is no longer necessary in most cases, since simply
evaluating the special variable will get the same result.  But it is still useful with
a few variables such as 1STANDARD-INPUT* and 1EH:CONDITION-HANDLERS* which are
rebound by the debugger for your protection when you evaluate anything.

1C*] 4Beep Types.*

The system now supplies a non-1NIL* beep-type to the function 1BEEP* on certain
occasions.  These are the types defined so far:

1ZWEI:CONVERSE-PROBLEM*
  Used for the beep that is done when Converse is unable to send a message.
1ZWEI:CONVERSE-MESSAGE-RECEIVED*
  Used for the beeps done when a Converse message is received.
1ZWEI:NO-COMPLETION*
  Used when you ask for completion in the editor and the string does not complete.
1TV:NOTIFY*
  Used for the beep done when you get a notification that cannot 
  be printed on the selected window.

Those of you who redefine 1BEEP* can use the beep type (the first argument)
to produce different sounds for different occasions.  More standard beep types
will be defined in the future, if users suggest occasions that deserve beep types.

1D*] 3Terminal T* 4Change.

1Terminal T** now controls just the deexposed Typeout action of the selected
window.  A new command 1Terminal I* controls the deexposed type-In action.
(Sadly, 1Terminal O* is already in use).

1Terminal 0 T* -- just wait for exposure on output when deexposed.
1Terminal 1 T* -- notify user on attempt to do output when deexposed
1Terminal 2 T* -- permit output when deexposed.

1Terminal 0 I* -- just wait for exposure on input when deexposed.
1Terminal 1 I* -- notify user on attempt to do input when deexposed
1Terminal 2 I* -- There is no 1Terminal 2 I*.  It doesn't make sense.

1E]* 4New Function3 SI:VIEW-UNFINISHED-PATCHES

1SI:VIEW-UNFINISHED-PATCHES*** &optional2 1(*system* 1"System")* 1(2stream *STANDARD-OUTPUT)*
prints out the unfinished patches of the 2system* onto2 stream*.

1F*] 4VMS Default Device.*

The "primary device" for VMS hosts is now 1USRD$* rather than 1SYS$SYSDSK.

G*] 4Improved File Error Handling.*

When there is an error accessing a file and the system asks for a new pathname,
you now have the option of entering the debugger instead.  Simply type 1End*.

1H*] 4Some Functions' Keyword Args Missing in 3ARGLIST*.*

Calling 1ARGLIST* on certain functions will omit the list of their keyword args.
This is due to a compile bug when they were compiled.  If you think you have
come across such a function, report it and it will be corrected with a patch.

1I*] 4Evaluating/Compiling Multi-Font Files.*

It now works to evaluate or compile files that contain multiple fonts
as specified with the 1Fonts* attribute in the 1-*-* line.  The old kludge
that some users used for doing this should no longer be used.

To make this work in all cases, user-defined readmacro characters should do all
input using the function 1SI:XR-XRTYI* (see its on-line documentation).  You may
wish to specify arguments of 2stream* 1NIL T*.

Note that if a readmacro detects a syntax error and wants to report this by
signaling an Lisp error, it should always make 1SYS:READ-ERROR* one of the
condition names and provide the proceed-type 1:NO-ACTION*, which should
be handled by skipping over the invalid data and returning 2something* (1NIL*
is a reasonable thing to return).

4Editor and ZMail Changes*
     
     1A*] 4Selective Undo.*
     1B*] 3Control-Shift-D4 Command Changed.**
     1C*] 4New Dired Command on* 1<4 Key.**
     1D*] 4ZMail Can Generate Message-ID Fields.*
     1E*] 4New ZMail Command 3M-X Undigestify Message*.*
     1F*] 4VMS Mail Files Understood.

1A**] 4Selective Undo.*

You can now undo an editing change that is not the most recent change you made.
If you give the Undo command 1C-Shift-U* while there is a region, it undoes
the most recent batch of changes that falls within the region.  The region does
not go away, so you can repeat the command to undo successive changes
within the same region.  For example, you can undo your changes to a
specific Lisp function by using 1C-M-H* to create a region around it
and then using C-Shift-U.

1B*] 3Control-Shift-D4 Command Changed.**

The command 1Control-Shift-D* now prints the full documentation of the
function which point is inside a call to.  1Control-Shift-D* is thus analogous to
1Control-Shift-A*.  1Meta-Shift-D* is still available if you wish to specify the
function to be documented.

1C*] 4New Dired Command on* 1<4 Key.**

A new command to edit the superior directory of the current buffer's directory
can be found on the 1<* key in DIRED.

1D*] 4ZMail Can Generate Message-ID Fields.*

If you want, ZMail can put a Message-ID field in your outgoing messages.  Go into
the Profile editor to get this behavior, because the default is not to generate
Message-ID fields.

1E*] 4New ZMail Command 3M-X Undigestify Message*.*

This command takes the current message and splits it into its submitted messages
so that you can act on them individually.  You can set aspects of what the command
does by using the Profile editor:

   (1) Should the original message be deleted ? (Default: 2Yes*)
   (2) Should everything but the header and ``table of contents'' be clipped out of
        the original message ?  (Default: 2No*)
   (3) Should the name of the digest be append to the subject field of all the new 
       messages so that you can tell from which digest they came ?  (Default: Yes)

1F*] 4VMS Mail Files Understood.*

ZMail can now read and write VMS format mail files, as used at MIT.
