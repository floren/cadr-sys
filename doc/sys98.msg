;-*- Mode:Text; Fonts:(TR12 CPTFONT TR12I HL12B TR12B TR12BI) -*-

System 98 is now released.  It uses microcode version 306.  It will not
run with earlier microcode versions, and microcode 306 may not be used with
system 97.


System 98 was originally used with microcode 305, but 306 fixes some important bugs.
So all system 98 bands should be updated to use microcode 306.

Files compiled in system 97 4will* run in system 98
  unless they contain character constants for
  control, meta, super or hyper characters (such as 1#\C-A*)
  or for mouse characters (such as 1#\MOUSE-3-1*).
  This means that init files which define editor commands
  usually must be recompiled.
Files compiled in system 98 4will not* run in system 97.

You can debug system 98 with CC from system 97 provided that CADR patch 1.1
is loaded.  You cannot debug system 98 from system 94.  You can debug all
earlier systems from system 98.

You should also read the files 1SYS: DOC; COMMON LISP*,
1SYS: DOC; SYS98 DEFSTRUCT *and 1SYS: MAN; GENERIC TEXT*,
which describe other compatible improvements present in system 98.
The current status of the package system is described in
1SYS: DOC; SYS98 PACKAGES*.

Contents of the rest of this file:

    4Incompatible Changes*
    4Incompatible Changes Coming in Future Systems.*
    4Compatible Lisp Programming Changes*
    4Window System Changes*
    4User Interface Changes*
    4Editor Changes

Common Lisp Is Supported.*

Most of the differences between Common Lisp and the traditional Lisp machine
dialect of Lisp are compatible extensions.  These extensions are available in
all programs.  They are described in 1SYS: DOC; COMMON LISP*,
1SYS: DOC; SYS98 DEFSTRUCT* and 1SYS: MAN; GENERIC TEXT*.  Many of these
extensions are very useful, so all users should read these files even if they have
no plans to write portable Common Lisp code.

There are some incompatibilities between Common Lisp and the traditional Lisp
machine system, however.  For the sake of existing programs, in most cases the
system still works the traditional way.  If you wish to write a Common Lisp
program and therefore want the Common Lisp definitions of the incompatible
constructs, you should place

				1Common Lisp: T; Base: 10;*

in the 1-*-* line of your file.  This gets you the Common Lisp reader syntax
and the incompatible Common Lisp definitions of certain system functions.
These Common Lisp incompatible versions of traditional system functions
actually live in the 1CLI* package, and are documented that way in this file
and the others.

If you want to use Common Lisp for interactive input, set the variable
1*DEFAULT-COMMON-LISP** non-1NIL*.  This variable is bound at the top level
of each process, so setting it applies only to the current process.
Lisp listeners check the variable before each form so it works
to set the variable while operating in the listener.

A few things in Common Lisp are not yet supported:
 transcendental functions of complex numbers
 "alternative" definitions as macros of nonstandard special forms
 1INLINE* and 1NOTINLINE* declarations

A few incompatible changes could not be made conditional in this way,
and apply to all programs.  The serious ones have been announced previously.
They are all described on the following page.

4Incompatible Changes*

] 4Arrays Stored in Row-Major Order.*

Arrays used to be stored in column-major order.  This does not affect user programs
except when they do one of these three things:

  Access screen arrays of windows using 1AREF*.  Since the TV hardware
  has not been changed, the horizontal dimension is still the one that
  varies fastest in memory; which means it is now the second dimension
  rather than the first.

  Use multidimensional displaced arrays or arrays displaced to
  multidimensional arrays.

  Deal with large multidimensional arrays and want to optimize paging behavior

  Store multidimensional arrays in QFASL files.
  A QFASL file records the elements of an array in the order they appear
  in storage.  Therefore, if an array is dumped in an earlier system
  and loaded into system 98, it will appear to be transposed.

The functions 1AR-2-REVERSE*, 1MAKE-PIXEL-ARRAY* and others are provided to make
it easier for you to change your code so that it works in both system 98 and older
system versions.  See section 8.8 of the Lisp machine manual.

] 4All 3&KEY* Arguments Are Now Optional.*

It is no longer ever an error to omit a keyword argument defined with 1&KEY*.
1&OPTIONAL* now has no effect on the treatment of 1&KEY* arguments.
This is for Common Lisp and for compatibility with Symbolics systems.

] 4Colons on Keywords Required in 3USER* Package.*

The 1USER* package is now just like all other packages in requiring that colons
be used in front of keyword symbols.  For example, you can no longer write
just 1TYI* instead of 1:TYI* if your program is in 1USER*.

] 4Double Colon for Internal Symbols; 3#:* to Override Local Nicknames.*

The brain-damaged designers of Common Lisp have now changed their minds
about how to refer to an internal symbol of another package.  They now
specify 1::* for that, rather than 1#:*.

The Lisp machine does not actually require you to use this construct, and
you are free to access internal symbols with plain colon in a package prefix.
However, the need to give 1::* this new meaning means that it cannot have
its old meaning, which is to suppress local package nicknames.

Therefore, the meaning of suppressing local package nicknames is given
to 1#:*.

As of system 98.8, the situation is:

  1FOO:BAR*		refer to external symbol of package 1FOO*
			(but actually you can use it for any symbol in 1FOO*).
  1FOO::BAR*		refer to an internal symbol of package 1FOO*,
			in strict Common Lisp.
  1FOO#:BAR*		refer to external (really, any) symbol in the package
			whose global name or nickname is 1FOO*.
  1#:BAR*		make an uninterned symbol named 1BAR*.

Another change is that the printer now does not try to indicate internal symbols
in any way if you are using the standard readtable.  If you are using the
Common Lisp readtable, such symbols will now be indicated with 1::*.

] 4Keywords Evaluate to Themselves.*

All symbols in the 1KEYWORD* package--that is to say, symbols that you
write with a colon, such as 1:STRING-OUT*--are now automatically set
up to evaluate to themselves.  Thus, you can now write
1    (SEND STREAM :TYI)*
instead of
1    (SEND STREAM ':TYI)*

This Common Lisp change ought not to invalidate any reasonable programs.

] 4Pointer Fields Now 25 Bits; Flag Bit Gone.*

Each typed data word in Lisp machine memory used to have one bit called
the "flag bit" which was not considered part of the contents of the word.
This is no longer so.  There is no longer a flag bit; instead, the pointer field
of the word is one bit larger, making it 25 bits in all.

This extra bit extends the range of integers that can be represented without
allocation of storage, and also extends the precision of small-floats by one bit.

On the Lambda processor, the maximum size of virtual memory is doubled.
This is the primary reason for the change.  Unfortunately, the CADR mapping
hardware is not able to use the extra bit as an address bit, so the maximum
virtual memory size on a CADR is unchanged.

The functions 1%24-BIT-PLUS*, 1%24-BIT-DIFFERENCE* and 1%24-BIT-TIMES* still
produce only 24 bits of result.  If you wish to have a result the full size
of the pointer field, however wide that is, you should use the functions
1%POINTER-DIFFERENCE* and 1%POINTER-TIMES* (the last is new), and
1%MAKE-POINTER-OFFSET* with a first argument of 1DTP-FIX* to do addition.

The functions 1%FLOAT-DOUBLE*, 1%DIVIDE-DOUBLE*, 1%REMAINDER-DOUBLE* and
1%MULTIPLY-FRACTIONS* use the full width of the pointer field.

The values returned by 1SXHASH* have not changed!  They are always positive
fixnums less than 21***23.

] 3*THROW4 Passes Multiple Values to **CATCH4.

1*CATCH*** used to return exactly four arguments, of which the first one was
a single value given to 1*THROW*, and the other three had complicated meanings.

Now, 1*CATCH* returns any number of values: either the values thrown, or
the values of the last form inside the 1*CATCH*, if no 1*THROW* was done.

To throw more than one value, make the second subform of a 1*THROW*
something which returns multiple values.  Thus,
1    (*CATCH 'FOO (*THROW 'FOO (VALUES 'A 'B)))*
returns the two values 1A* and 1B*.

] 3CATCH-ALL4 Returns Additional Values.

1CATCH-ALL*** now returns all the values of the body or all the values thrown,
plus three more: the tag, action and count a la 1*UNWIND-STACK*.

Yes, it is peculiar for a function to return 2n* values followed by three specific ones,
but it has to work that way.

If you want to receive all these values, you should use 1CACTH-ALL* within a
1MULTIPLE-VALUE-LIST* and then use 1(BUTLAST 2list* 3)* to get the values
thrown or returned and 1(NLEFT 3 2list*)* to get the three specific values.

] 4Character Bits Moved.*

The Control, Meta, Super, Hyper and Mouse bits now occupy a new position in
character codes.  This is so that they will not overlap the field used by the
character's font number.

You can continue to use the byte pointers 1%%KBD-CONTROL* to examine and
set the bits; these byte pointers have different values now but your code
will work anyway.  No change to the source is needed.

However, you must recompile any file that contains a character constant which
has one of the control bits set.

] 4Local 3SPECIAL* Declarations to Change in Meaning.*

One change that will take place for the sake of Common Lisp
is that a 1SPECIAL* declaration within a function will have to be
present in the construct (1LET*, 1PROG*, etc.) which binds a variable
in order to make the binding be special.  Thus, for example,
1    (DEFUN FOO (A)
      (DECLARE (SPECIAL B))
      (LET (B)
        ...))*
will no longer make 1B* special.  Instead, you must write
1    (DEFUN FOO (A)
      (LET (B)
        (DECLARE (SPECIAL B))
        ...))*
where the local declaration appears just inside the construct that binds
the variable in question.

A further unfortunate consequence of this is that 1LOCAL-DECLARE* cannot
be used any more to make a binding special, as in
1    (DEFUN FOO (A)
      (LOCAL-DECLARE ((SPECIAL B))
        (LET (B)
          ...)))*
because this too would fail to put the declaration just inside the 1LET*.

To facilitate the changeover, this change has not actually been made.
Local 1SPECIAL* declarations will still affect code just as they used to.
However, any code which depends on this will get a warning reminding
you to fix the code.  The actual change will occur in a future system version.

Note that 1LOCAL-DECLARE*s of 1SPECIAL* around an entire function,
affecting arguments of the function, will continue to work.
Also, if you are just examining or setting the variable, as in
1    (LOCAL-DECLARE ((SPECIAL A))
      ... (+ A 5) ...)*
and not rebinding it, then your code will not be affected.

] 3EVALHOOK4/*APPLYHOOK4 Incompatible Change.**

Evalhook and applyhook functions are now passed two additional arguments,
which describe the interpreter environment that the evaluation or application
was going to take place in.  See the section on evaluation in 1SYS: DOC; COMMON LISP*
for more information.

] 4Time Functions Return Exact Year.*

The functions 1DECODE-UNIVERSAL-TIME*, 1TIME:GET-TIME* and 1GET-DECODED-TIME* now
return the correct year number (a number greater than 1900.) rather than the
year number modulo 100.

] 3SELECT-MATCH4 Changed, and Much Faster.  *LIST-MATCH-P4.**

The syntax of 1SELECT-MATCH* has been changed so as to avoid use of the construct 1#?*.
This is to avoid defining the construct 1#?* and therefore leave it free for users to define.

In addition, new instructions are used to make the matching extremely fast,
faster than any other way you could test for a list's having certain elements
and extract the others.

As before, 1SELECT-MATCH* takes an expression for an object to be tested
followed by any number of clauses to try.  Each clause contains a pattern,
a conditional form, and more forms that make the body of the clause.
The first clause whose pattern matches the object and for which the conditional
form produces a non-1NIL* value is the chosen clause, and its body is executed.
The last clause can be an 1OTHERWISE* clause.

The change is that the pattern is now an expression made with the 1`* character,
with commas indicating a variable in the pattern.  For example, in
1    (SELECT-MATCH FOO
      (`(A ,B ,B) (SYMBOLP B) (PRINT B))
      (OTHERWISE (PRINT FOO)))*
the first clause matches if 1FOO* is a list of three elements, the first being the
symbol 1A*, and the second and third being the same symbol.  The second
clause matches anything that slips through the first.

Note that variables used in the patterns (such as 1B* in the example above) are
bound locally by the 1SELECT-MATCH*.

You can get the effect of a single 1SELECT-MATCH* pattern with 1LIST-MATCH-P*:

1LIST-MATCH-P* 2list* &quote 2pattern*
  returns 1T* if the value of 2list* matches 2pattern*.
  Any match variables appearing in 2pattern* will be set in the course of the
  matching, and some of the variables may be set even if the match fails.

] 3BREAK4 Arguments Changed.**

The function 1BREAK* is being changed to accept a format string and format
arguments.  It used to take an unevaluated first argument, normally a symbol,
and simply print it.

To make the changeover easier, 1BREAK* evaluates its first argument by hand,
unless it is a symbol--then its pname is used as the format string.
However, the compiler prints a warning if you use 1BREAK* in the old way.

] 3Y-OR-N-P4 And *YES-OR-NO-P4 Arguments Changed.**

These functions now take just a format string and format arguments.
They no longer accept the stream to use as an argument; they always
use the value of 1QUERY-IO*.

If you used to pass two arguments, you must now bind 1QUERY-IO* around
the call instead.

] 3:EVAL-WHEN4 Option in *DEFSTRUCT4 Gone.**

You probably did not know about this option unless you found out by being screwed.
1DEFSTRUCT* no longer generates any sort of 1EVAL-WHEN*.  If you want the expansion of
a 1DEFSTRUCT* to be inside an 1EVAL-WHEN*, simply write an 1EVAL-WHEN* around the
1DEFSTRUCT*.

] 4Macro Expander Functions Take Two Arguments.*

A macro's expander function used to be passed only one argument, the
macro call to be expanded.  Now it is passed a second argument as well.
It is an "environment" object, and it is used to record the local macro
definitions currently in effect.

Since many old macros are still compiled to accept only one argument,
1MACROEXPAND-1* is smart and will pass only one argument in such a case.
So there is no need to alter or recompile your macro definitions now.

However, if you have anything else which calls macro expander functions
directly, it must be changed to do what 1MACROEXPAND-1* does.  The easiest
way is to write
1    (CALL 2expander-function* NIL 2form* :OPTIONAL 2environment*)*

If you define a macro using 1MACRO* (instead of 1DEFMACRO*), you should
change the arglist yourself to accept a second optional argument, even if
it is just 1IGNORE*.

] 3SETF4 and *LOCF4 Definitions Done Differently.**

You no longer use 1SETF* and 1LOCF* properties to define how to do 1SETF* or
1LOCF* on some kind of form.  Instead, you use the macro 1DEFSETF* to define
how to 1SETF* it, and you do 1(DEFMACRO (2function* SI:LOCF-METHOD) ...)*
to define how to do 1LOCF* on it.  See the section in 1SYS: DOC; COMMON LISP*
that talks about 1SETF*.

One exception: 1(DEFPROP FOO SI:UNSETFABLE SETF)* still works, by special
dispensation.  Likewise for 1SI:UNLOCFABLE*.

] 3COPY-FILE4 Takes Keyword Arguments.

1COPY-FILE*** 2filename* 2new-name* &key (2error* 1T*) (2copy-creation-date* 1T*) (2copy-author* 1T*) 2report-stream*
                 (2create-directories* 1':QUERY*) (2characters* 1':DEFAULT*) (2byte-size* 1':DEFAULT*)
  copies the file named 2filename* to the name 2new-name*.
  2characters* and 2byte-size* specify what mode of I/O to use to transfer the data.
  2characters* can be 1T* to specify character input and output, 1NIL* for binary,
   1:ASK* meaning ask the user which one, 1:MAYBE-ASK* meaning ask if it is not
   possible to tell with certainty which method is best, or 1:DEFAULT* meaning
   to guess as well as possible automatically.
  If binary transfer is done, 2byte-size* is the byte size to use.  1:DEFAULT* means
  to ask the file system for the byte size that the old file is stored in,
  just as it does in 1OPEN*

  2copy-author* and 2copy-creation-date* say whether to set those properties of the
  new file to be the same as those of the old file.  If a property is not copied,
  it is set to your login name or the current date and time.

  2report-stream*, if non-1NIL*, is a stream on which a message should be printed
  describing the file copied, where it is copied to, and which mode was used.

  2create-directories* says what to do if the output filename specifies a directory that
  does not exist.  It can be 1T* meaning create the directory, 1NIL* meaning treat it as
  an error, or 1:QUERY* meaning ask the user which one to do.

  2error*, if 1NIL*, means that if an error happens then this function should just return
  an error indication.

  There are four values:
    The defaulted pathname to be opened for output in copying this file.
    The truename of the file copied
    The "outcome":
      The truename of the new file, If the file was successfully copied.
      A condition object, if there was an error and 2error* was 1NIL*.
      1NIL* if the user was asked whether to copy this file and said no.
    The mode: a Common Lisp type descriptor such as 1STRING-CHAR*
    or 1(UNSIGNED-BYTE 8)*, saying how the file was copied.

  If 2filename* contains wildcards, multiple files are copied.  The new name for each
  file is obtained by merging 2new-name* (parsed into a pathname) with that file's
  truename as a default.  The mode of copy is determined for each file individually,
  and each copy is reported on the 2report-stream* if there is one.  If 2error* is 1NIL*,
  an error in copying one file does not prevent the others from being copied.

  When wildcards are used, each of the four values is a list with one element
  for each file that matched.

] 3MAKE-PATHNAME4 Change.**

The meaning of the 2defaults* argument to 1MAKE-PATHNAME* is changed.
Now all pathname components that are not specified or specified as 1NIL*
are defaulted from the 2defaults*, if you give 2defaults*.  If you do not give
2defaults*, then the host alone defaults, from 1*DEFAULT-PATHNAME-DEFAULTS**,
as it used to.

] 3SI:PRINT-OBJECT4 and *SI:PRINT-LIST4 Arguments Changed.**

These functions no longer accept the 2slashify-p* argument.
Instead, they look at the current value of 1*PRINT-ESCAPE**.
Their argument lists are now:

1SI:PRINT-OBJECT* 2object* 2prindepth* 2stream* &optional 2which-operations
1SI:PRINT-LIST** 2list* 2prindepth* 2stream* &optional 2which-operations*

] 3PROMPT-AND-READ4 Changed.**

Two 1PROMPT-AND-READ* options, namely 1:EVAL-FORM* and
1:EVAL-FORM-OR-END*, have been changed incompatibly in the
conventions they use to return values.  See the section on
1PROMPT-AND-READ*, below.

4Incompatible Changes Coming In Future Systems.* 

] 4Decimal Radix Will Become the Default.* 

In some future system, base 10. will become the default.  This is a Common Lisp
change.  However, it will still be possible to specify the radix for each file
individually.  To avoid any difficulties, place 1Base:* 18;* in the attribute list (the
1-*-* line) of any file which is supposed to be in octal.

1] 3FORMAT4 control argument will change.***

The control argument to 1FORMAT* will change in some future system to be compatible
with Common Lisp. There will no longer be separate functions 1FORMAT* and 1CLI:FORMAT*, as there
are in System 98.
This change will affect the following 1FORMAT* directives:

1~X*	1~2width,padchar,commachar*X* 
	1      *Prints its argument in hexadecimal. (Analogous to 1~O*, 1~B* and 1~D*)
	This command used to be used to insert spaces into the output.

	Use 1~2number-of-spaces*@T* to achieve the same result as the old 1~2number-of-spaces*X* directive.

1~F*	~2width,decimal-places,scale,overflowchar,padchar1F** 
	1      *Prints a floating-point number in nonexponential notation.
	        Multiplies by 10^2scale* before printing if 2scale* is specified.
	        Prints in 2width* positions, with 2decimal-places* digits after the decimal point.
	        Pads on left with 2padchar* if necessary.  If then number doesn't fit in 2width* positions,
	        and 2overflowchar* is specified, this command just fills the 2width* positions with
	        that character.
	This directive used to just take one optional prefix control arg, which specified how many
	        mantissa digits to print. This is the same as 2+2decimal-places* for the new 1FORMAT*.

	Use 1~,2n+2*F* to achieve the same result as the old 1~2n*F* directive.

1~E*	1~2width**,2decimal-places*,2exponent-places*,2scale*,2overflowchar,padchar,exptchar1E** 
	1      *Prints a floating-point number in exponential notation.
	        Prints in 2width* positions, with 2exponent-places* digits of exponent.
	        If 2scale* (default is 1) is positive, prints 2scale* digits before point, 2decimal-places*-2scale*+1 after.
	        If 2scale* is zero, prints 2decimal-places* digits after the point, and a zero before if there's room.
	        If 2scale* is negative, prints 2decimal-places* digits after the point, of which the first -2scale*
	        are zeros.
	        If 2exptchar* is specified, it is used to delimit the exponent2 *(instead of "e" or whatever.)
	        If2 overflowchar* is specified, then if the number doesn't fit in the specified2 width*,
	        or if the exponent doesn't fit in 2exponent-places* positions, the field is filled with 2overflowchar*
	        instead.
	This directive used to just take one optional prefix control arg, which specified how many
	        mantissa digits to print. This is the same as 2+2decimal-places* for the new 1FORMAT*.

	Use 1~,2n+2*E* to achieve the same result as the old 1~2n*E* directive.

1~G*	1~2width**,2decimal-places*,2exponent-places*,2scale*,2overflowchar,padchar,exptchar1G** 
	1      *Like 1~E*, but if the number fits without an exponent, it is printed without one.
	This command used to be used to go to a particular argument.

	Use 1~2argument-number*@** to achieve the same result as the old 1~2argument-number*G* directive.

Note: in System 98 you must use 1CLI:FORMAT* to use the new Common Lisp style format directives.
1GLOBAL:FORMAT* will continue to use the old directives.
In System 99 these two functions will become identical, so the best thing is to update your code to
use 1CLI:FORMAT* now so that it will work in both System 98 and in future systems.

4Compatible Lisp Programming Changes* 

1] 4All Macros Are Displacing In Interpreted Code.** 

All macros are displacing when encountered by 1EVAL*.
1DEFMACRO-DISPLACE*, and so on, are now synonyms for 1DEFMACRO*, and so on.

] 3:ALLOW-OTHER-KEYS4 As A Keyword Argument.** 

1:ALLOW-OTHER-KEYS* has a special meaning as a keyword when passed
to a function that takes 1&KEY* arguments.  If followed by a non-1NIL* value,
it prevents an error if any keyword is not recognized.  Thus, given the function

	1(DEFUN FOO (&KEY A B) (LIST A B))*

you would get an error if you do 1(FOO :A 5 :C T)* because 1:C* is not recognized.
But if you do 1(FOO :A 5 :C T :ALLOW-OTHER-KEYS T)*, you get no error.
The 1:C* and its argument are just ignored.

] 4 Package System Changes.* 

1PKG-EXTERNAL-SYMBOLS* 2package*
  returns a list of all the external symbols of 2package*.  2package* can be
  a package or a package name (as a symbol or string).

1MAKE-PACKAGE* allows additional keyword arguments:
     2invisible* 2import* 2shadowing-import* 2import-from* 2relative-names* 2relative-names-for-me*

  2invisible*, if non-1NIL*, means that this package should not be put on the
  list 1*ALL-PACKAGES**.  As a result, 1FIND-PACKAGE* will not find this package,
  not by its name and not by its nicknames.  You can make normal use of the
  package in all other respects (passing it to 1INTERN*, passing it to 1USE-PACKAGE*
  to make other packages inherit from it or it from others, and so on).

  2import*, if non-1NIL*, is a symbol or a list of symbols to be imported into this
  package.  You could accomplish as much by calling 1IMPORT* after you have
  created the package.

  2shadowing-import*, if non-1NIL*, is a symbol or a list of symbols to be imported
  into this package with shadowing.  You could accomplish as much by calling
  1SHADOWING-IMPORT* after you have created the package.

  2import-from*, if non-1NIL*, is a list containing a package (or package name)
  followed by names of symbols to import from that package.  Specifying
  2import-from* as 1(CHAOS "OPEN" "CLOSE")* is nearly the same as specifying
  2import* as 1(CHAOS:OPEN CHAOS:CLOSE)*, the difference being that with
  2import-from* the symbols 1OPEN* and 1CLOSE* are not looked up in the 1CHAOS*
  package until it is time to import them.

  2relative-names* is an alist specifying the local nicknames to have in this
  package for other packages.  Each element looks like 1(2localname* . 2package*)*,
  where 2package* is a package or a name for one, and 2localname* is the
  desired local nickname.

  2relative-names-for-me* is an alist specifying local nicknames by which this
  package can be referred to from other packages.  Each element looks like
  1(2package* 2localname*)*, where 2package* is a package name and 2localname* is
  the name to refer to this package by from 2package*.  You will note that
  the elements of this list are not dotted while those of 2relative-names* are.
  Blame everyone's favorite obnoxious Lisp machine company for this.

1DEFPACKAGE* &quote 2name* 2keywords-and-values*...
  is a new way to create a package.  All the arguments are simply passed to
  1MAKE-PACKAGE*.  The differences between this function (actually, macro) and
  1MAKE-PACKAGE* are:

    1DEFPACKAGE* does not evaluate any arguments.

    Re-evaluating a 1DEFPACKAGE* for an existing package is allowed, and modifies
    the existing package in accordance with changes in the definition.

    The editor notices 1DEFPACKAGE* and records it as the "definition" of the package.

Package attributes in a file's 1-*-* line can now have this format
    1Package: (2name** 2keyword* 2value* 2keyword* 2value1...)**;
which means that the package to be used is 2name* and, if that package does
not exist, it should be created by passing 2name* and the keywords and values
to 1MAKE-PACKAGE*.

1PKG-ADD-RELATIVE-NAME* 2in-pkg* 2name* 2for-pkg*
  defines 2name* as a local nickname in 2in-pkg* for 2for-pkg*.  2in-pkg* and 2for-pkg*
  may be packages, symbols or strings.

1PKG-DELETE-RELATIVE-NAME* 2in-pkg* 2name*
  eliminates 2name* as a local nickname in 2in-pkg*.

1SYS:PACKAGE-NOT-FOUND*							Error Condition
  is signaled whenever you do 1PKG-FIND-PACKAGE* with second argument
  1:ERROR*, 1NIL* or omitted, and the package you were looking for does not exist.

  The condition instance supports the operations 1:NAME* and 1:RELATIVE-TO*;
  these return whatever was passed as the first and third arguments to
  1PKG-FIND-PACKAGE* (the package name, and the package whose local nicknames
  should be searched).

  The proceed types 1:RETRY*, 1:NO-ACTION*, 1:NEW-NAME* and 1:CREATE-PACKAGE*
  may be available.
  1:RETRY* says to search again for the specified name
  in case it has become defined; if it is still undefined, the error occurs again.
  1:CREATE-PACKAGE* says to search again for the specified name, and create
  a package with that name if none exists yet.
  1:NEW-NAME* is accompanied by a name (a string) as an argument.  That name
  is used instead, ignoring any local nicknames.  If that name too is not found,
  another error occurs.
  1:NO-ACTION* (available on errors from within 1READ*) says to continue with the
  entire 1READ* as well as is possible without having a valid package.

] 4All Objects Except Symbols and Lists Are Constants.*

All arrays, instances, fefs, characters, closures, etc. now evaluate to themselves.
Evaluating such objects used to be an error; this new behavior therefore
cannot hurt anything.

The only kinds of objects which currently can evaluate to anything but themselves
are symbols and lists.  However, it is not guaranteed that no other kind of object
will ever be defined to evaluate to other than itself.

] 4Nonlocal 3GO* and 3RETURN*.*

You can now 1GO* or 1RETURN* from an internal 1LAMBDA* expression to the containing
function.  Example:

1    (PROG ()
      (MAPC #'(LAMBDA (X) (IF (NUMBERP X) (RETURN T)))
            INPUTS))*

returns 1T* if any element of 1INPUTS* is a number.  So does

1    (PROG ()
      (MAPC #'(LAMBDA (X) (IF (NUMBERP X) (GO RET-T)))
            INPUTS)
      (RETURN NIL)
     RET-T
      (RETURN T))*

] 3DEFSUBST4 Preserves Order of Evaluation.**

It used to be the case that if a 1DEFSUBST*'s body used an argument more than once,
or used its arguments out of order, the forms supplied as arguments would be
evaluated multiple times or in the wrong order.

This has been fixed.  The arguments passed to a 1DEFSUBST* function will be
evaluated exactly once, in the order they are written.

For example, after 1(DEFSUBST FOO (A B) (CONS B A))*, the reference
1(FOO X (SETQ X Y))* used to turn into 1(CONS (SETQ X Y) X)*, which is incorrect
since it uses the new value of 1X* twice.  To be correct, the old value of 1X*
should be used for the second argument to 1CONS*.

Now, the expansion will be something effectively like
    1(LET ((TEMP X)) (CONS (SETQ X Y) TEMP))*

] 3MACROEXPAND-ALL4.**

The function 1MACROEXPAND-ALL* is called like 1MACROEXPAND*.  It expands
macro definitions not only at the top level of a form but also in its subexpressions.
It is never confused by a macro name appearing at the start of a list
which is not a subexpression.

1MACROEXPAND-ALL* 2form* &optional 2environment*
  expands macro definitions at all levels in 2form* and returns the result.
  2environment* is used for finding local 1MACROLET* macro definitions;
  it is like the second argument to 1MACROEXPAND* (see previous page).

  Only one value is returned.

] 3TV:BITBLT-CLIPPED4.

1TV:BITBLT-CLIPPED*** is just like 1TV:BITBLT*, except that if you specify
transfers that include points outside the bounds of either the source or
destination array, only the part of the transfer that is within the bounds
of both arrays will take place.

The height and width you specify are required to be positive.

] 3%BLT-TYPED4.  Proper Use of *%P-STORE-TAG-AND-POINTER4, *%P-DPB(-OFFSET)4.

1%BLT-TYPED*** is called just like 1%BLT* and does about the same thing: copy any
number of consecutive memory words from one place in memory to another.
The difference is that 1%BLT* is only properly used on data which contains no
pointers to storage, while 1%BLT-TYPED* is only properly used on boxed data.

Both 1%BLT* and 1%BLT-TYPED* can be used validly on data which is formatted with
data types (boxed) but whose contents never point to storage.  This includes
words whose contents are always fixnums or small flonums, and also words which
contain array headers, array leader headers, or FEF headers.  Whether or not the
machine is told to examine the data types of such data makes no difference
since, on examining them, it would decide that nothing needed to be done.

For unboxed data (data formatted so as not to contain valid data type fields), such
as the inside of a numeric array or the instruction words of a FEF, only 1%BLT*
may be used.  If 1%BLT-TYPED* were used, it would examine the data type fields of
the data words, and probably halt due to an invalid data type code.

For boxed data which may contain pointers, only 1%BLT-TYPED* may be used.  If
1%BLT* were used, it would appear to work, but problems could appear mysteriously
later because nothing would notice the presence of the pointer there.  For
example, the pointer might point to a bignum in the number consing area, and
moving it in this way would fail to copy it into a nontemporary area.  Then the
pointer would become invalidated the next time the number consing area was
emptied out.  There could also be problems with lexical closures and with
garbage collection.

1%P-STORE-TAG-AND-POINTER* should be used only for storing into boxed words,
for the same reason as 1%BLT-TYPED*: the microcode could halt if the data stored
is not valid boxed data.

1%P-DBP* and 1%P-DPB-OFFSET* should be used only when the word being modified
does not contain a pointer.  It may be an unboxed word, or it may be a boxed word
containing a fixnum, small-flonum or array header.  The same goes for
1%P-DEPOSIT-FIELD* and 1%P-DEPOSIT-FIELD-OFFSET*.

] 3CAR-SAFE4, Etc.

1CAR-SAFE*** is like 1CAR* when operating on a list.  If the argument is not a list,
1CAR-SAFE* returns 1NIL*.  1CAR-SAFE* never gets an error.

The functions 1CDR-SAFE*, 1CDDR-SAFE*, 1NTH-SAFE* and 1NTHCDR-SAFE* also exist
and are analogous.

] 3SETQ-GLOBALLY4, etc.**

There are now functions to use to examine or set the global binding of
a variable, as opposed to the binding currently in effect.  The global binding
is the one that is in effect in all processes or stack groups which have not
rebound the variable.

They work by forking off another process and examining or setting the
variable in that process.  The bindings of your own process are not visible
in the other process, and that process establishes no bindings of its own,
so references to the symbol there access the global binding.

1SYMEVAL-GLOBALLY* 2symbol*
  returns the global binding of 2symbol*.

1SETQ-GLOBALLY* 2unevaluated-symbol* 2value* 2unevaluated-symbol* 2value*...
  sets the global binding of each 2symbol* to the corresponding 2value*.

1SET-GLOBALLY* 2symbol* 2value*
  sets the global binding of 2symbol* to 2value*.  2symbol* is an evaluated argument.

1MAKUNBOUND-GLOBALLY* 2symbol*
  makes the global binding of 2symbol* be void.

1BOUNDP-GLOBALLY* 2symbol*
  returns 1T* if the global binding of 2symbol* is not void.

The primary use of these functions are so that init files can set variables
which are bound by the 1LOAD* function, such as 1PACKAGE* or 1BASE*.
If your init file does 1(SETQ PACKAGE (FIND-PACKAGE 'FOO))*, this will
be nullified as soon as 1LOAD* exits and its binding of 1PACKAGE* goes away.
If you do 1(SETQ-GLOBALLY PACKAGE (FIND-PACKAGE 'FOO))*, the current
binding established by 1LOAD* is actually not changed, but when the 1LOAD*
exits and the global binding is in effect again, 1FOO* will be the current package.

] 3:STRING-LINE-IN4 Stream Operation.

1:STRING-LINE-IN*** is a new standard input stream operation, supported by all
the input streams provided by the system.  It fills a user-supplied buffer with
text from the stream until either the buffer is full, end of file is reached,
or a 1Return* is found in the input.  If input stops due to a 1Return*, the 1Return*
itself is not put in the buffer.

Thus, this operation is nearly the same as 1:STRING-IN*, except that 1:STRING-IN*
always keeps going until the buffer is full or until end of file.

1:STRING-LINE-IN* returns three values:
  the index in the buffer at which filling stopped
      (if the buffer has a fill pointer, it is set to this value as well.)
  1T* if end of file was reached.
  1T* if the line is not complete; that is, input did not encounter a
      1Return* character.  In this case there may be more text in the file
      belonging to the same line.

] 3LOCATION-MAKUNBOUND4 Takes Two Arguments.

1LOCATION-MAKUNBOUND*** now takes a second, optional argument.
This argument supplies a pointer value to use in the void marker
that is stored.

A void location actually contains a pointer with data type 1DTP-NULL*.
This pointer is supposed to point to the object whose value or function definition
is void.  In the case of a symbol's value cell or function cell, the object
would be the symbol itself.

1LOCATION-MAKUNBOUND* makes the location point to whatever object you supply
as the second value.

] 3:REQUIRED-INIT-KEYWORDS4.**

You can now write 1(:REQUIRED-INIT-KEYWORDS 2init-keywords*...)* in a 1DEFFLAVOR*.
Then, whenever the flavor (or any flavor that depends on it) is instantiated,
it will be an error if any of those init keywords fails to be specified.  For example,
after
1  * 1(DEFFLAVOR FOO (A) :INITTABLE-INSTANCE-VARIABLES (:REQUIRED-INIT-KEYWORDS :A))*
it is an error to do 1(MAKE-INSTANCE 'FOO)* since the 1:A* keyword is not provided.

] 4Method Combination Improvements.*

When using method combination types such as 1:LIST*, 1:PROGN*, 1:APPEND* and
1:PASS-ON*, which formerly allowed any number of untyped methods and nothing
else, you can now use the method combination type keyword as a method type.
For example, when using 1:OR* combination for operation 1:DOIT*, you can now
define a method 1(MYFLAVOR :OR :DOIT)* as well as 1(MYFLAVOR :DOIT)*.  The
method is combined the same way whichever name you use.  However, when the
operation is invoked, all the typed methods are called first, followed by all the
untyped methods.

There is no longer a limit of three values passed back from the primary method
when 1:AFTER* methods are in use.  However many values the primary method
chooses to return, they will be passed back to the ultimate caller.

] 3UNDEFFLAVOR4.

1UNDEFFLAVOR*** 2flavor-name*
  removes the definition of 2flavor-name*.  Any flavors that depend on it are no longer
  valid to instantiate.

] 4Instantiation Flavor Functions.*

Now you can set up a flavor 1FOO* so that, when you try to instantiate it, it calls a
function to decide what flavor it should really instantiate (not necessarily 1FOO*).
You do this by giving 1FOO* an 2instantiation flavor function*.  Write
    1(:INSTANTIATION-FLAVOR-FUNCTION 2function-name*)*
in the 1DEFFLAVOR* for 1FOO* to do this.

When 1(MAKE-INSTANCE 'FOO 2keyword-args*)* is done, the function specified is
called with two arguments: the flavor name specified (1FOO* in this case)
and the init plist (the list of keyword args).  It should return the name of
the flavor that should actually be instantiated.

Note that the instantiation flavor function applies only to the flavor it is
specified for.  It is not inherited by dependent flavors.

] 4Run-Time-Alternative Flavors.*

A run-time-alternative flavor is a way to define a collection of similar flavors,
all built on the same base flavor but having various mixins as well, and choose
which one to instantiate based on init options.  (This is implemented using the
1:INSTANTIATION-FLAVOR-FUNCTION* feature.)

A simple example would be

1    (DEFFLAVOR FOO () (BASIC-FOO)
      (:RUN-TIME-ALTERNATIVES
         (:BIG BIG-FOO-MIXIN))
      (:INIT-KEYWORDS :BIG))*

Then, 1(MAKE-INSTANCE 'FOO ':BIG T)* will get you an instance of a flavor
whose components are 1BIG-FOO-MIXIN* as well as 1FOO*.  But 1(MAKE-INSTANCE 'FOO)*
or 1(MAKE-INSTANCE 'FOO ':BIG NIL)* will get you an instance of 1FOO* itself.
The clause 1(:BIG BIG-FOO-MIXIN)* in the 1:RUN-TIME-ALTERNATIVES* says to
incorporate 1BIG-FOO-MIXIN* if 1:BIG*'s value is 1T*, but not if it is 1NIL*.

You can have several clauses in the 1:RUN-TIME-ALTERNATIVES*.  Each one
is processed independently.  Thus, you could have keywords 1:BIG* and 1:WIDE*
independently control two mixins and get four possibilities.

You can test for values of a keyword other than just 1T* or 1NIL*.  The clause
1    (:SIZE (:BIG BIG-FOO-MIXIN) (:SMALL SMALL-FOO-MIXIN)
           (NIL NIL))*
allows the value for the keyword 1:SIZE* to be 1:BIG*, 1:SMALL* or 1NIL* (or omitted).
If it is 1NIL* or omitted, no mixin is used (that's what the second 1NIL* means).
If it is 1:BIG* or 1:SMALL*, an appropriate mixin is used.  This kind of clause
is distinguished from the simpler kind by having a list as its second element.
The values you check for can be anything, but 1EQ* is used to compare them.

You can also have the value of one keyword control the interpretation of others
by inserting clauses within clauses.  After the place where you put the mixin name
or 1NIL* for no mixin, you can put other clauses which specify keywords and
their interpretation.  These other clauses are acted on only if the containing
alternative is chosen.  For example, the clause
1    (:ETHERIAL (T ETHERIAL-MIXIN)
               (NIL NIL
                    (:SIZE (:BIG BIG-FOO-MIXIN) (:SMALL SMALL-FOO-MIXIN)
                           (NIL NIL))))*
says to consider the 1:SIZE* keyword only if 1:ETHERIAL* is 1NIL*.

1:RUN-TIME-ALTERNATIVES*. can also be called 1:MIXTURE*, for compatibility
with brand S.

] 4Additional Arguments to 3FS:PARSE-PATHNAME*.

1FS:PARSE-PATHNAME** 2thing* &optional1 2with-respect-to** 2defaults* (2start1 0**) 2end* 2junk-allowed*
  Parses 2thing* into a pathname and returns it.
  2thing* can be a pathname, a string or symbol, or a Maclisp-style namelist.
  If it is a pathname, it is returned unchanged, and the other arguments
  do not matter.

  2with-respect-to* can be 1NIL* or a host or host-name;
   if it is not 1NIL*, the pathname is parsed for that host
   and it is an error if the pathname specifies a different host.
  If 2with-respect-to* is 1NIL*, then 2defaults* is used to get the host
   if none is specified.  2defaults* may be a host object in this case.
  2start* and 2end* are indices specifying a substring of 2thing* to be parsed.
   They default to 0 for 2start* and 1NIL* (meaning end of 2thing*) for 2end*.
  If 2junk-allowed* is non-1NIL*, parsing stops without error if
   the syntax is invalid, and this function returns 1NIL*.
   The second value is the index in 2thing* at which parsing stopped,
   which is the index of the invalid character if there was invalid syntax.
  If 2junk-allowed* is 1NIL*, invalid syntax signals an error.

] 3:DEVICE-WILD-P4, etc., Pathname Operations.**

The operation 1:DEVICE-WILD-P* operation on a pathname object is defined to
return non-1NIL* if the pathname's device component contains a wildcard.

1:DIRECTORY-WILD-P*, 1:NAME-WILD-P*, 1:TYPE-WILD-P* and 1:VERSION-WILD-P*
are similar, for their respective pathname components.

] 3STRING-LENGTH4 Now Accepts Same Args as *STRING4.**

Anything which 1STRING* could coerce into a string,
1STRING-LENGTH* can give you the length of.

] 3MAKE-PLANE4 Allows Additional Keyword Arguments.**

The arguments 2initial-dimensions* and 2initial-origins* are now accepted.
You can use them to specify which part of the infinite plane the initially
allocated storage should be for.  Each of these arguments, if specified,
should be a list whose length is the rank of the plane; one element for
each dimension.  Each element of 2initial-dimensions* is the width to
allocate initially in that dimension.  Each element of 2initial-origins*
is the smallest index, in that dimension, for which the initially allocated
storage should be used.

] 3MAP-RESOURCE4 And *DEALLOCATE-WHOLE-RESOURCE4.

1MAP-RESOURCE*** 2function* 2resource-name* &rest 2extra-args*
  operates with 2function* on each object created in resource 2resource-name*.
  Each time 2function* is called, it receives three fixed args, plus the 2extra-args*.
  The three fixed args are
    an object of the resource;
    1T* if the object is currently allocated ("in use");
    the resource data structure itself.

1DEALLOCATE-WHOLE-RESOURCE* 2resource-name*
  deallocates each object in resource 2resource-name*.  This is equivalent to doing
  1DEALLOCATE-RESOURCE* on each one individually.  This function is often useful
  in warm-boot initializations.

] 3~14 Format Directive.***

Format output within a 1~* ... 1~* construct is printed with each line indented
to match the indentation that was current when the 1~* was reached.

] 3DEFSELECT-INCREMENTAL4.**

With 1DEFSELECT-INCREMENTAL* you can define a 1DEFSELECT* that starts out empty
and has methods added to it incrementally with individual 1DEFUN*s.

You do 1(DEFSELECT-INCREMENTAL 2fspec* 2default-handler*)* to define 2fspec*
as a select-method function which has no methods except the standard
ones (1:WHICH-OPERATIONS*, 1:OPERATION-HANDLED-P*, and 1:SEND-IF-HANDLES*).

Then, to define the indivudual methods, use 1DEFUN* on function specs of the form
1(:SELECT-METHOD 2fspec* 2operation*)*.  Note that the argument list of the 1DEFUN* must
explicitly provide for the fact that the operation will be the first argument;
this is different from what you do in an ordinary 1DEFSELECT*.

Example:

1    (DEFSELECT-INCREMENTAL FOO IGNORE)   ;*The function 1IGNORE* is the default handler
1    (DEFUN (:SELECT-METHOD FOO :LOSE) (IGNORE A)
      (1+ A))*

defines 1FOO* just like

1    (DEFSELECT (FOO IGNORE)
      (:LOSE (A) (1+ A)))*

The difference is that reevaluating the 1DEFSELECT* gets rid of any methods
that used to exist but have been deleted from the 1DEFSELECT* itself.
Reevaluating the 1DEFSELECT-INCREMENTAL* has no such effect, and reevaluating
an individual 1DEFUN* redefines only that method.

] 3READLINE4, *READLINE-TRIM4, *READLINE-OR-NIL4.

1READLINE*** and 1READLINE-TRIM* have been extended to return a second
value.  This value is 1T* if end-of-file was encountered.

Note that end-of-file can still be an error if encountered at the beginning of the
line, and this is still controlled by the 2eof-option* argument.  But if the function
does return, the second argument always says whether there was an end-of-file

The new function 1READLINE-OR-NIL* is like 1READLINE-TRIM* except that it returns
1NIL* rather than 1""* if the input line is empty or all blank.

] 3READ-DELIMITED-STRING4.

1READ-DELIMITED-STRING*** &optional 2delimiter* 2stream* 2eof* 2rubout-handler-options* 2buffer-size*
  reads input from 2stream* until a delimiter character is reached, then returns
  all the input before but not including the delimiter as a string.
  2delimiter* is either a character or a list of characters which all serve as delimiters.
  It defaults to the character 1End*.  2stream* defaults to the value of 1STANDARD-INPUT*.

  If 2eof* is non-1NIL*, then end of file on attempting to read the first character
  is an error.  Otherwise it just causes an empty string to be returned.
  End of file once at least one character has been read is never an error
  but it does cause the function to return all the input so far.

  Input is done using rubout handling and echoing if 2stream* supports the
  1:RUBOUT-HANDLER* operation.  In this case, 2rubout-handler-options* are passed
  as the options argument to that operation.

  2buffer-size* specifies the size of string buffer to allocate initially.

  Three values are returned:
   the string of input read;
   a flag which is 1T* if input ended due to end of file;
   the delimiter character which terminated input (or 1NIL* if end of file was reached).

] 3PROMPT-AND-READ4 Improvements.**

There are several new options you can give to 1PROMPT-AND-READ*, and some
existing options now take arguments.  Remember that the first argument to
1PROMPT-AND-READ* is an option, which is either a keyword or a list
of a keyword followed by arguments (alternating keywords and values).
The rest of the arguments are a string and additional args passed to 1FORMAT* to
print the prompt.

These options are changed incompatibly:

1:EVAL-FORM-OR-END*
  is changed so that, if the user types just 1End*, it returns 1:END* as the second value.
  It used to return 1#/END* as the second value in that case.  The first value will still be 1NIL*.

1(:EVAL-FORM-OR-END :DEFAULT 2object*)
(:EVAL-FORM :DEFAULT 2object*)*
  if the user types Space meaning use the default, the second value will now be 1:DEFAULT*
  rather than 1#/SPACE*.  The first value will still be 2object*, the default.

Here are the options that now take additional arguments:

1(:PATHNAME :DEFAULTS 2default-list* :VERSION 2default-version*)*
  2default-list* is passed as the second argument to1 FS:MERGE-PATHNAME-DEFAULTS*,
  and 2default-version* is passed as the fourth argument.

1(:NUMBER :INPUT-RADIX 2radix** 1:OR-NIL 2nil-ok-flag*)*
  reads a string terminated by 1Return* or 1End*, and parses it into a number
  using radix 2radix* if the number is a rational.  The number is returned.
  If 2nil-ok-flag* is non-1NIL*, then it is also permitted to type just
  1Return* or 1End*, and then 1NIL* is returned.

Here are the new options:

1:CHARACTER*
  means to read a single character and return a fixnum representing it.

1(:DATE :NEVER-P 2never-ok* :PAST-P 2past-required*)*
  means to read a string terminated by 1Return* or 1End* and parse it as a date/time.
  The universal time number representing that date/time is returned.
  If 2past-required* is non-1NIL*, the date must be before the present time,
  or else you get an error and must rub out and use a different date.
  If 2never-ok* is non-1NIL*, then you may also type "never"; 1NIL* is returned.

1:EXPRESSION*
  is the same as 1:READ*: read a Lisp object using 1READ* and return it.

1:EXPRESSION-OR-END*
  means to read a Lisp object using 1READ*, but alternately allow just 1End*
  to be typed and return the two values 1NIL* and 1:END*.

1:PATHNAME-OR-NIL*
  means to read a file name and return a pathname object,
  but if the user types just 1End* then return 1NIL* instead.
  The pathname is defaulted with 1FS:MERGE-PATHNAME-DEFAULTS*.
1(:PATHNAME-OR-NIL :DEFAULTS 2default-list* :VERSION 2default-version*)*
  2default-list* is passed as the second argument to1 FS:MERGE-PATHNAME-DEFAULTS*,
  and 2default-version* is passed as the fourth argument.

] 3:NO-ERROR4 Clauses in *CONDITION-CALL4.**

The last clause in a 1CONDITION-CALL* or 1CONDITION-CALL-IF* may now be
a 1:NO-ERROR* clause.  This looks and works about the same as a 1:NO-ERROR* clause in
a 1CONDITION-CASE*: it is executed if the body returns without error.
The values returned by the body are stored in the variables which are
the elements of the list which is the first argument of the 1CONDITION-CALL*,
and the values of the last form in the clause are returned by the
1CONDITION-CALL* form itself.

] 3SI:SET-SYNTAX-FROM-DESCRIPTION4.**

There are new syntax descriptions that you can pass to this function:

    1SI:ESCAPE*		a quote-one-character character.
				In the standard readtable / is such a character.

    1SI:MULTIPLE-ESCAPE*    a quote-several-characters character.
				In the standard readtable | is such a character.

    1SI:CHARACTER-CODE-ESCAPE*  is followed by a character's octal code.
				In the standard readtable  is such a character.

    1SI:DIGITSCALE*		a character for shifting an integer by digits.
				In the standard readtable ^ is such a character.

    1SI:BITSCALE*		a character for shifting an integer by bits.
				In the standard readtable _ is such a character.

    1SI:NON-TERMINATING-MACRO*  a macro character that is not recognized
				if a token is already in progress.
				In the standard readtable # is such a character.
				(It is also a 2dispatching* macro, but that is another matter.)
				The correct way to make a character be a macro
				is with 1SET-MACRO-CHARACTER*, not with this description.

1SI:SLASH* and 1SI:CIRCLECROSS* are still implemented
but it is preferable to use 1SI:ESCAPE* or 1SI:CHARACTER-CODE-ESCAPE*.
1SI:VERTICALBAR* is no longer defined; use 1SI:MULTIPLE-ESCAPE*.
Unfortunately it is no longer possible to define 1SI:DOUBLEQUOTE*
as Doublequote is now just a macro character.

] 4Microcoded Functions Interruptible.*

Many microcoded functions, including 1LAST*, 1MEMQ*, 1ASSQ* and 1GET*, are now
interruptible.  This means in particular that if you pass a circular list to any of
them you can now abort successfully.

] 3NTH-VALUE4.**

The function 1NTH-VALUE* makes it convenient to select one of the values
returned by a function.  For example, 1(NTH-VALUE 1 (FOO))* returns
the second of 1FOO*'s values.  1NTH-VALUE* operates without consing in compiled
code if the first argument's value is known at compile time.

1NTH-VALUE* 2value-number* 2expression*					Special form
  evaluates 2expression*, then returns one of its values as specified by
  2value-number* (with 0 selecting the first value).

] 3PATCH-LOADED-P4.

1PATCH-LOADED-P*** 2major-version* 2minor-version* &optional (2system-name* 1"SYSTEM"*)
  returns 1T* if the changes in patch number 2major-version1.*minor-version* of
  system 2system-name* are loaded.  If 2major-version* is the major version of
  that system which is currently loaded, then the changes in that patch
  are loaded if the current minor version is greater than or equal to
  2minor-version*.  If the currently loaded major version is greater than
  2major-version* then it is assumed that the newer system version contains
  all the improvements patched into earlier versions, so the value is 1T*.

] 4Process Queues.*

A process queue is a kind of lock which can record several processes which
are waiting for the lock and grant them the lock in the order that they requested it.
The queue has a fixed size.  If the number of processes waiting remains less than that
size then they will all get the lock in the order of requests.  If too many processes
are waiting than the order of requesting is not remembered for the extra ones.

1SI:MAKE-PROCESS-QUEUE* 2name* 2size*
  makes a process queue object named 2name*, able to record 2size* processes.
  2size* includes the process that owns the lock.

1SI:PROCESS-ENQUEUE* 2process-queue* &optional 2lock-value* 2who-state*
  attempts to lock 2process-queue* on behalf of 2lock-value*.  If 2lock-value* is 1NIL*
  then the locking is done on behalf of 1CURRENT-PROCESS*.

  If the queue is locked, then 2lock-value* or the current process is put on the
  queue.  Then this function waits for that lock value to reach the front of
  the queue.  When it does so, the lock has been granted, and this function returns.

  2who-state* appears in the who line during the wait.  It defaults to 1"Lock"*.

1SI:PROCESS-DEQEUEUE* 2process-queue* &optional 2lock-value*
  unlocks 2process-queue*.  2lock-value* (which defaults to the current process)
  must be the value which now owns the lock on the queue, or an error occurs.
  The next process or other object on the queue is granted the lock and its
  call to 1SI:PROCESS-ENQUEUE* will therefore return.

1SI:RESET-PROCESS-QUEUE* 2process-queue*
  unlocks the queue and clears out the list of things waiting to lock it.

1SI:PROCESS-QUEUE-LOCKER* 2process-queue*
  returns the object in whose name the queue is currently locked,
  or 1NIL* if it is not now locked.

] 3DEFINE-SYMBOL-MACRO4.

1DEFINE-SYMBOL-MACRO***, a misfeature recently introduced by Symbolics,
has5 not* been implemented in the MIT system.

The effect of 1(DEFINE-SYMBOL-MACRO FOO (PRINT 'HUH))* would be
that evaluating the symbol 1FOO* would execute 1(PRINT 'HUH)*.
Yes, 1(EVAL 'FOO)* would print 1HUH*!  Can you believe it?

I think 1DEFINE-SYMBOL-MACRO* is disgusting, but if users find it is useful
or needed for compatibility I will implement it.  Tell me if you want this.

] 3WITH-OPEN-FILE-SEARCH4.

1WITH-OPEN-FILE-SEARCH*** is a new macro for opening a file and trying various
pathnames until one of them succeeds.  The pathnames tried differ only in
their type components.

1WITH-OPEN-FILE-SEARCH* (2streamvar* (2operation* 2defaults* 2auto-retry*) 2types-and-pathname* . 2options*) &body 2body*
  tries opening various files until one succeeds; then binds 2streamvar* to the stream
  and executes 2body*, closing the stream on exit.

  2types-and-pathname* should evaluate to two values, the first being a list of types to try
  and the second being a pathname, called the base pathname.  Each pathname to try
  is made by merging the base pathname with the defaults 2defaults* and one of the types.
  2options* should evaluate alternatingly to keywords and values, which are passed to 1OPEN*.

  If all the names to be tried fail, a 1FS:MULTIPLE-FILE-NOT-FOUND* error is signaled.
  2operation* is provided just so that the 1:OPERATION* operation on the error object
  can return it.  It is usually the user-level function for which the
  1WITH-OPEN-FILE-SEARCH* is being done.

  If 2auto-retry* is non-1NIL*, an error causes the user to be prompted for a new base
  pathname.  The entire set of types specified is tried anew with this pathname.

] 4Creating Links.

1FS:CREATE-LINK** 2link-name* 2link-to* &key (2error* 1T*)
  creates a link named 2link-name* which points to a file named 2link-to*.
  An error happens if the host specified in 2link-name* does not support links,
  or for any of the usual problems that can happen in creating a file.

] 4Rubout Handler Interface.*

There are some new options for use in controlling the rubout handler;
some other options are changed.  The new options are
 1:NO-INPUT-SAVE, :ACTIVATION*, 1:COMMAND*, 1:PREEMPTABLE* and 1:EDITING-COMMAND*.
The changed options are
 1:DO-NOT-ECHO*, 1:PASS-THROUGH* and 1:PROMPT*.

Recall that the options are the first argument to the 1:RUBOUT-HANDLER* stream
operation; the remaining arguments being the parsing function and arguments to
call it with.  The options argument is an alist; each element should look like one
of these patterns:

    1(:NO-INPUT-SAVE T)* 
        Don't save this batch of input in the input history when it is done.
	1YES-OR-NO-P* specifies this option.

    1(:FULL-RUBOUT 2value*)* 
	Cause immediate return from the 1:RUBOUT-HANDLER* operation
	if the buffer ever becomes empty due to deletion of text.
	Two values are returned: 1NIL* and 2value*.

	The debugger uses this option so it can erase "Eval:" from the screen
	if you rub out all the characters of the form to be evaluated.

    1(:INITIAL-INPUT 2string*)* 
        Start out with 2string* in the buffer.

    1(:INITIAL-INPUT-POINTER 2n*)* 
        Start out with editing pointer 2n* chars from the beginning of the buffer.
        This is used with 1:INITIAL-INPUT*.

    1(:ACTIVATION 2fn* 2args**...1)* 
	Activate if certain characters are typed in.
	An activation character causes the buffered input to be read immediately,
	and moves the editing pointer to the end of the buffer.

        2fn* is used to test characters for being activators.
	It is called with an input character (never a blip) as the first arg
	and 2args* as additional args.  If 2fn* returns non-1NIL*, the character is an activation.

	The activation character does not go in the buffer itself.
	However, after the parsing function has read the entire contents
	of the buffer, it reads a blip 1(:ACTIVATION 2char* 2numeric-arg*)*
	where 2char* is the character that activated and 2numeric-arg*
	is the numeric arg that was pending for the next rubout handler command.

    1(:DO-NOT-ECHO 2chars**...1)* 
	Poor man's activation characters.  Like 1:ACTIVATION* except:
	the characters that should activate are listed explicitly, and
	the character itself is returned, rather than a blip, after all the buffered input.

    1(:COMMAND 2fn* 2args**...1)* 
	Make certain characters preemptive commands.
	A preemptive command returns instantly to the caller,
	of the 1:RUBOUT-HANDLER* operation, regardless of the input in the buffer.
	It returns two values: a list 1(:COMMAND 2char* 2numeric-arg*)* and the keyword 1:COMMAND*.
	Any buffered input remains in the buffer for the next time input is done.
	In the mean time, the preemptive command character can be processed
	by the command loop.

	In testing for whether a character should be a preemptive command,
	this works just like 1:ACTIVATION*.

    1(:PREEMPTABLE 2value*)* 
 	Make all blips act as preemptive commands.
	If this option is specified, the rubout handler returns immediately
	when it reads a blip, leaving buffered input for next time.
	Two values are returned: the blip that was read, and 2value*.

    1(:EDITING-COMMAND (2char* 2doc*)*...1)* 
	Define editing commands to be executed by the parsing function itself.
	Each 2char* is such a command, and 2doc* says what it does.
	2doc* is printed out by the rubout handler's help command.
	If any of these characters is read by the rubout handler,
	it is returned immediately to the parsing function
	regardless of where the input pointer is in the buffer.
	The parsing function should not regard the character as part of the input.

	There are two reasonable things that the parsing function can do:
	  print some output
	  1:FORCE-KBD-INPUT*
        If output is printed, the 1:REFRESH-RUBOUT-HANDLER* operation should
	be invoked afterward.  This causes the rubout handler to redisplay
	so that the input being edited appears after the output that was done.
	If input is forced, it will be interpreted as rubout handler commands.

	There is no way to act directly on the buffered input because different
	implementations of the rubout handler store it in different ways.

    1(:PASS-THROUGH 2chars**...1)* 
	Causes the rubout handler to treat the specified characters as ordinary
	self-inserting characters. This enables characters such as 1Help* to be input
	without any need to quote them. This can make the any rubout handler
	editing command which was assigned to that key inaccessible. This option
	only works with characters with no control bits set.

    1(:PROMPT 2fn-or-string*)* 
	Direct prompting for the input being read.
	If 2fn-or-string* is a string, it is printed; otherwise it is called with two args:
	the stream, and a character which is an editing command that says
	why the prompt is being printed.

    1(:REPROMPT 2fn-or-string*)* 
	Same as 1:PROMPT* except used only if the input is reprinted
	for some reason after editing has begun.  The 1:REPROMPT* option
	is not used on initial entry.  If both 1:PROMPT* and 1:REPROMPT*
        are specified, 1:PROMPT* is used on initial entry and 1:REPROMPT* thereafter.

   1 (:NONRECURSIVE T)*
	Means to ignore previously-specified rubout-handler options and only use the
	options specified to this invocation of the rubout handler, either using the
	1:RUBOUT-HANDLER* operation or 1WITH-INPUT-EDITING*. Use this option to override
	the default behaviour, which is to look at all rubout-handler options including
	those specified at an earlier time.

A new convenient way to invoke the rubout handler on a stream
if the stream supports it is to use 1WITH-INPUT-EDITING*.

1WITH-INPUT-EDITING* (2stream* 2options*) 2body*...			Macro
  invokes the rubout handler on 2stream*, if it is supported,
  and then executes 2body*.  2body* is executed in any case,
  within the rubout handler if possible.
  2body*'s values are returned by 1WITH-INPUT-EDITING*.
  However, if a preemptive command is read, 1WITH-INPUT-EDITING*
  returns immediately with the values being as specified above
  under 1:COMMAND* or 1:PREEMPTABLE*.

  2options* are used as the rubout handler options.

All rubout handlers now check for the condition name 1SYS:PARSE-ERROR* when they
decide whether to handle an error.  They used to check for 1SYS:READ-ERROR*.
All the errors signaled by the system which have the condition name 1SYS:READ-ERROR*
now have 1SYS:PARSE-ERROR* as well, so no change in behavior should be apparent.
However, you can signal an error which has 1SYS:PARSE-ERROR* but not 1SYS:READ-ERROR*
if you wish (say, if the error happens in some function other than 1READ*).

1SYS:PARSE-ERROR* is also the condition name which the compiler looks for
in its efforts to continue from errors that happen while reading text to be compiled.

The function 1SYS:PARSE-FERROR* is a convenient way to signal such an error,
if you do not want any additional condition names besides 1SYS:PARSE-ERROR* and the
ones it implies.  The arguments are a format string and args for it.
If 1SYS:PARSE-FERROR* is called while reading text to be compiled, it will
return 1NIL* automatically.

] 4Some Chaosnet Functions Renamed.*

Some functions in the 1CHAOS* package have had their names changed.
This is so we can avoid having two advertised system functions with
the same name in different packages.  The old names still work.

	Old Name			New Name

	1CHAOS:FINISH*		1CHAOS:FINISH-CONN*
	1CHAOS:CLOSE*			1CHAOS:CLOSE-CONN*
	1CHAOS:FINISHED-P*		1CHAOS:CONN-FINISHED-P*

] 4Chaosnet Listening Streams.*

Now you can listen for a chaosnet connection and open a stream at the same time.
To do this, call 1CHAOS:OPEN-STREAM* with 1NIL* as the 2host* argument.
You must still pass a non-1NIL* 2contact-name* argument.  The function will
return a stream to you as soon as someone attempts to connect to that contact name.

At this time, you must accept or reject the connection by invoking the
stream operation 1:ACCEPT* or 1:REJECT*.  1:REJECT* takes one argument, a string
to send back as the reason for rejection.  Before you decide which to do,
you can use the 1:FOREIGN-HOST* operation to find out where the connection came from.

4Window System Changes*

] 3TV:LIST-MOUSE-BUTTONS-MIXIN4's Functionality Is Now The Default.**

If a window has an input buffer and does not define a handler for mouse clicks,
they are handled by putting 1:MOUSE-CLICK* blips into the input buffer.
It used to be necessary to mix in 1TV:LIST-MOUSE-BUTTONS-MIXIN* to get this behavior.
Now that flavor is a no-op.

Refer to section 10.1 of the window system manual for more information.

] 3:PREEMPTABLE-READ4 Operation Available in *TV:STREAM-MIXIN4.**

Now all windows that handle 1:RUBOUT-HANDLER* also handle the
1:PREEMPTABLE-READ* operation.  It used to be necessary to mix in
1TV:PREEMPTABLE-READ-ANY-TYI-MIXIN* to have this operation available.
That flavor is now a no-op.

Refer to page 55 of the window system manual for information on using
this operation.

You can also do preemptable input using the 1:RUBOUT-HANDLER* operation
with the 1:PREEMPTABLE* option.  This is a new feature documented in this file.

] 3:MENU4 Menu Item Type Improved.**

The value of a 1:MENU* menu item can now be any form which evaluates to
a suitable menu.  A menu itself is a special case of such a form, now that
menus and other unusual objects evaluate to themselves.

] 3:FUNCALL-WITH-SELF4 Menu Item Type.

1:FUNCALL-WITH-SELF*** is a new type of menu item.  The value associated with it
is a function of one argument.  If the menu item is executed, the function will
be called, with the menu (which is the value 1SELF*, in the menu's 1:EXECUTE*
method) as its argument.  The value that the function returns is the value
of executing the menu item.

] 3TV:MOUSE-WAIT4 Takes Who-state as Argument.

1TV:MOUSE-WAIT*** takes an additional optional argument which, if specified, is
displayed as the run state in the who line while the function waits for mouse input.

] 3TV:CHAR-MOUSE-P4 and *TV:MAKE-MOUSE-CHAR4.**

You should no longer use the byte pointer 1%%KBD-MOUSE* in making
mouse characters or testing whether a character is a mouse character.
It still works at the moment, but may stop working in the future.
To avoid problems, convert code as soon as you have switched over to 98.

To test, use 1TV:CHAR-MOUSE-P*.  To construct, use 1TV:MAKE-MOUSE-CHAR*.

1TV:CHAR-MOUSE-P* 2char*
  1T* if 2char* is a mouse character.

1TV:MAKE-MOUSE-CHAR* 2button* 2n-clicks*
  returns the mouse character for clicking on button 2button*, 2n-clicks* times.
  Both 2button* and 2n-clicks* range from 0 to 2; 2n-clicks* is actually one less
  than the number of clicks.  The left button is button 0; the right one is 2.

You continue to use 1%%KBD-MOUSE-BUTTON* and 1%%KBD-MOUSE-N-CLICKS* as byte pointers
to extract from a mouse character the button clicked and how many times.

] 3TV:MARGIN-SPACE-MIXIN4.**

The mixin 1TV:MARGIN-SPACE-MIXIN* defines a margin item which is blank.
You can leave blank space next to any of the window's edges.  The blank
space can go between two margin items at that edge, or between the inside
of the window and the margin items.  For example, it could be used to
separate the scroll bar from the inside of the window, or separate the
scroll bar from the border, depending on where in the ordering you mix
the mixin in.

The mixin defines an init keyword called 1:SPACE* whose value specifies
how much blank space to leave at each edge.  The values you can use are:
1  T      *leave one pixel of space at all four edges.
1  NIL    *leave no blank space.  This turns off the effect of the mixin.
1  2n*      *leave 2n* pixels of space at each edge.
1  (2left* 2top* 2right* 2bottom*)
         *leave 2top* pixels of space at the top edge, 2left* pixels at the left edge, etc.

Two operations are also defined by the mixin: 1:SPACE* and 1:SET-SPACE.
 :SET-SPACE* takes an argument just like the 1:SPACE* init keyword and alters
1 *the amount of space the mixin is generating.  1:SPACE* as an operation returns
1 *a list of four values 1(2left* 2top* 2right* 2bottom*)* describing how much space is
1 *currently being taken up by the mixin.

] 3TV:ADD-SYSTEM-KEY4 Saves Previous Definition.**

If a system key is already defined and you use 1TV:ADD-SYSTEM-KEY* to redefine it,
the previous definition is restored when you do 1TV:REMOVE-SYSTEM-KEY* to
remove the new definition.
1
4User Interface Changes**

] 3*VALUES*4 Records All Values Produced in a Listen Loop.**

Lisp listeners, 1BREAK* and the debugger now record all the values of each
evaluated form in the variable 1*VALUES**.  Each process has its own 1*VALUES**.
The value of 1*VALUES** is a list, and each element is a list of the values of one
evaluated form.  The most recent forms' values come first.

If a form is aborted for any reason, 1NIL* is pushed on 1*VALUES** for it.

1(CAAR *VALUES*)* is therefore equivalent to the value of the variable 1**
if and only if the last form was not aborted.

] 4Variable Ratio Mouse Motion.*

The ratio of mouse motion on the table to mouse cursor motion on the screen
now depends on the speed of motion.  If you move the mouse slowly, the cursor
moves only a little as the mouse moves.  As you move the mouse faster, the same
amount of mouse motion moves the cursor a long distance.

To control this feature, use this function

1TV:MOUSE-SPEED-HACK* &rest 2specs*
  2specs* consists of an odd number of elements: alternating scale factors and speeds,
  followed by one more scale factor.  Each scale factor applies up to the speed
  that follows it.  The last scale factor applies to all higher speeds.
  The standard settings are made with 2specs* of 1(.6 120 1 200 1.5 400 2.2 700 3.3)*
  so you can see that a speed of 120 is fairly slow, while 700 is moderately fast.
  A scale factor of 1 corresponds to the mouse motion ratio previously in use.
  So, 1(TV:MOUSE-SPEED-HACK 1)* would restore the old fixed-ratio behavior.

] 3UNADVISE.*

The function 1UNADVISE* has been generalized in that all arguments now act
independently to restrict which pieces of advice should be removed.
Thus, if all three arguments are 1NIL*, all advice is removed.
If the first argument is non-1NIL*, it is a function spec, and only advice
on that function spec is removed.
If the second argument is non-1NIL*, it is an advice class (1:BEFORE*, 1:AFTER* or 1:AROUND*),
and only advice of that class is removed.
If the third argument is non-1NIL*, it is a position (if it is a number) or a name
(if it is a symbol), and only advice with that position or that number is removed.

1UNADVISE-WITHIN* has been improved in a similar fashion.
1ADVISE* has been changed so that 1(ADVISE)* returns a list of all functions which have
advice, in the same way the 1(TRACE)* returns a list of all traced functions.

] 4Choose Variable Values Windows.*

Clicking the right mouse button on a variable's value now puts you in
the rubout handler with the old value of the variable there for you to edit.
You can use parts of the text of the old value to make up the text of the
new value.

Clicking left still puts you in the rubout handler with a blank slate;
then you must type the new value from scratch.

The keyword 1:PAST-DATE* is now allowed for describing how to print and
parse a variable value.  It is the same as 1:DATE* for printing.  For parsing,
it is almost the same as 1:DATE*, but if the day is specified only by a day
of the week then the most recent past day of that name will be used, rather
than the next coming one.

] 4Output of Character Names.

1FORMAT** 1~:C* and 1FORMAT:OCHAR* now never output a character name for graphic
characters other than 1Space* and 1Altmode*.  All other graphic characters are output
as themselves, whether or not they have names, since they appear on the
keyboard as themselves.

] 3:STEPCOND4 Argument to *TRACE4.**

The 1:STEPCOND* argument to 1TRACE* generalizes the 1:STEP* argument.  It allows
you to specify that 1STEP* should be invoked on the execution of the traced
function only if a certain condition is met.  The value you provide for the
1:STEPCOND* argument should be a form to be evaluated when the traced function
is called; if the form evaluates non-1NIL*, the function will be stepped.

] 3MONITOR-VARIABLE4 No Longer Exists.**

One consequence of the fact that boxed data words no longer have a flag bit
is that 1MONITOR-VARIABLE* is no longer possible to implement.  This function
has been removed.  Sorry.

] 3C-M-H4 Command in Debugger.**

The debugger command 1Control-Meta-H* prints a description of the condition
handlers established by the stack frame you are looking at.

] 3EH:*DEBUG-IO-OVERRIDE*4.**

If 1EH:*DEBUG-IO-OVERRIDE** is non-1NIL*, the debugger will now use it for its input
and output, rather than using the value of 1DEBUG-IO*.

] 4Compiled Code Read-Only Now.*

The areas in which compiled code lives are now read-only.
This is to catch bugs such as 1NCONC*ing onto a constant list.

The pnames of interned symbols are also read-only now.

] 4Compilation Allowed in More Than One Process At Once.*

It is no longer the case that compilation uses fixed data structures
which exist in only one copy.  You will no longer get the message
"Compiler in process FOO waiting for resources."

] 3MAKE-SYSTEM4 Improvements.**

If 1MAKE-SYSTEM* is done on a system which is not known, the file
1SYS: SITE; 2system* SYSTEM* is now loaded without any query if the file exists.

When 1MAKE-SYSTEM* asks you about a list of files to be compiled or loaded,
you now have the option of saying you would like to be asked again about
each individual file.  Do this by typing "S" instead of "Y" or "N".

After you type "S", you will be asked about each file in the bunch just
as you would have been earlier if you had specified 1:SELECTIVE* as
an argument to 1MAKE-SYSTEM*.  Finally you will be asked once again
to approve of the entire bunch of files, before processing actually begins.

] 4Fasdumping Functions Always Record Package to Load in.*

The functions 1DUMP-FORMS-TO-FILE*, 1COMPILER:FASD-SYMBOL-VALUE*,
1COMPILER:FASD-FONT* and 1COMPILER:FASD-FILE-SYMBOLS-PROPERTIES*
now always record in the QFASL file created the name of the package
in which the file was written.  This makes sure that the symbols used
when the file is loaded will be the same as when it was dumped.

In 1DUMP-FORMS-TO-FILE*, you can specify the package to use by including
a 1:PACKAGE* attribute in the 1ATTRIBUTE-LIST* argument.  For example, if
that argument is the list 1(:PACKAGE :SI)* then the file is dumped and loaded
in the 1SI* package.  If you do not specify a package, the file is dumped and
loaded in 1USER*.  With the other three functions, the file is always dumped
and loaded in 1USER*.

] 3APROPOS4 and *SUB-APROPOS4 Accept Keywords *:BOUNDP4 and *:FBOUNDP4.**

In 1APROPOS*, specifying a non-1NIL* value for the keyword argument 2boundp*
restricts the search to symbols which have values.  A non-1NIL* 2fboundp*
restricts it to symbols with function definitions.  1SUB-APROPOS* accepts
the same new arguments.

] 3FS:LOAD-PATHNAME-DEFAULTS4 Now a Synonym for **DEFAULT-PATHNAME-DEFAULTS*.*

Common Lisp wants 1LOAD* to use the default defaults.  It seems that if
1LOAD* should do so then everything else which used the 1LOAD* defaults
should do likewise.  So the two variables have been forwarded together.

4Editor Changes*

] 4Yank Command Improvements.*

What used to be called the kill ring is now called the 2kill history* because it is
no longer a ring buffer.  It now records all the kills you have ever done, in
strict chronological order.

1Meta-Y* still brings older kills into the region, and any particular sequence of
1Meta-Y* commands works just as it used to.  But the history is not permanently
rotated; as soon as a new kill is done, it snaps back to chronological order.
We say that 1Meta-Y* rotates the history's 2yank pointer* around the history list.
1Control-Y* with no argument yanks what the yank pointer points at.

So far, the yank pointer corresponds entirely to what used to be the front of
the kill ring, but here are the differences.
   - Killing anything moves the yank pointer up to the front of the list.
   - Numeric arguments 1Control-Y* count from the most recent kill,
     not from the yank pointer.

You can think of this as meaning that either killing or 1Control-Y* with an argument
"un-rotates" any rotation you have done, before it does its work.

1Control-Y* with an argument of zero prints a list of the first 20 elements of
the kill history.  Click on one of them to yank it.  Click on the message saying
that there are more elements, if you want to see the rest of them.

There are several other histories as well as the kill history.
They all work just like the kill history, except that you use some other
command instead of 1Control-Y* to yank from them.  1Meta-Y* is used for
rotating the yank pointer no matter which history you are yanking from;
it simply works on whatever history your last yank used.

For example, the previous inputs in the rubout handler are now stored
in a history.  The command 1Control-C* yanks from it, pretty much as before,
except that it now takes arguments exactly like 1Control-Y*.
1Control-Meta-Y* is a new alias for 1Control-C*; it has the advantage
of not being a debugger command, so you can use it in the debugger
with no extra complications.

All the pathnames you have typed in minibuffers now go in a history.
The command 1Meta-Shift-Y*, which used to yank the last pathname input,
has been generalized to yanks from this history.  It takes args just like
1Control-Y*, now.  Use 1Meta-Y* immediately after a 1Meta-Shift-Y* to rotate
the yank pointer to other pathnames in the history.

All buffer names given as arguments in the minibuffer also have a history.
(Actually, each Zmacs window has its own history of these.)
1Meta-Shift-Y* is the command for this history as well.

All function specs and other definition names you give as arguments in the
minibuffer also have their own history, which is accessible through 1Meta-Shift-Y*.

There is no ambiguity in 1Meta-Shift-Y*.  
When the minibuffer wants a pathname, 1Meta-Shift-Y* uses the pathname ring.
When the minibuffer wants a buffer name, 1Meta-Shift-Y* uses the buffer name ring.
When the minibuffer wants a definition name, 1Meta-Shift-Y* uses that ring.
Other rings of minibuffer arguments of particular kinds may be created in
the future; 1Meta-Shift-Y* will be the way to access all of them.

Note that the command 1Control-X Altmode*, which repeats previous minibuffer
commands, takes arguments just like 1Control-Y*, and also stores its data in a
history.  However, this command does not really work by yanking text.
There has been no change in the way 1Control-Meta-Y* is used to go back to
previous minibuffer arguments or to a previous command.

Summary:

    1Control-Y*		kill history		everywhere
    1Control-Meta-Y*		input history		rubout handler
             1Control-C* is the same as 1Control-Meta-Y*
    1Control-Meta-Y*		input history	1      *Editor Top Level and Ztop
    1Meta-Shift-Y*		arg history		minibuffer

    1Meta-Y*			rotate yank pointer of any history

] 4Lisp (Edit) Windows, and Ztop.*

The Lisp (Edit) window or editor top level, and Ztop mode, now provide
infinitely long input histories just like the one that the usual rubout handler
provides.  Formerly each batch of input read in a Lisp (Edit) window or in
Ztop mode was pushed on the kill history.  Now it goes on the window's
or Ztop buffer's input history instead.  Use 1Control-Meta-Y* to yank
the most recent element of the input history, just as you would in the
rubout handler, and then use 1Meta-Y* to rotate to earlier inputs if you wish.

] 4Yanking and Fonts.*

When text is moved between buffers and files in which fonts are specified,
the precise font of each character is now preserved.  If you yank a character
which was in font 1MEDFNT* in the buffer where it used to be, it will be in
1MEDFNT* after it is yanked.  This may necessitate adding fonts to the font list
of the buffer you are editing; if so, you will be asked whether to modify
the attribute list (the 1-*-* line) in the text as well.

This new feature applies to the commands 1C-Y*, 1Insert File*, 1Insert Buffer*,
and 1C-X G*.  But it only applies when fonts have been specified in the buffer
you are editing (with 1Set Fonts* or with a Fonts: attribute).  Otherwise, all
the yanked text gets put into the default font along with everything else
in the buffer.  Also, if fonts were not specified in the file or buffer that
the text came from, it simply goes into font zero of the current buffer.

] 4Sectionization Improvements.*

Now each form in a buffer gets its own section.  This has several beneficial results.

1M-X Compile Buffer Changed Sections* will no longer recompile
any random forms that are adjacent to functions you have edited.
In fact, this command recompiles only sections containing 1DEF*... forms.

Evaluating a random form in the buffer will no longer mark any definition
as "already recompiled".  Even evaluating a form which is part of a definition
will no longer mark the entire definition as "already recompiled".

1C-Shift-C* can now print the name of the function being compiled
very quickly, based on the sectionization.

The section nodes for non-definition forms have names which are strings
containing the file or buffer name, the function which the form invokes, and a
numeric suffix to make the name unique: for example, 1QFCTNS-DEFPROP-182*.
You will see these section names mentioned in the output of 1Meta-X List
Sections* and other commands for listing or visiting sets of sections.

] 4Per-Buffer Local Variables.*

Now you can make any special variable's value local in a specific editor
or, in the case of Zmacs, in a specific buffer.  For editor user option
variables this can be done with a 1Meta-X* command.

1ZWEI:MAKE-LOCAL-VARIABLE* 2variable* &optional 2value* 2xvcell*
  makes 2variable* local in the current editor, or in the current buffer
  if this is an editor which can select various buffers (that is, Zmacs).
  If 2value* is specified (whether 1NIL* or not) then 2variable* is set to
  2value* after it is made local; otherwise it keeps its global value.

  The argument 2xvcell* is used in Zmacs buffer switching.  If non-1NIL*,
  it should be a closure value cell, which is used as the value cell for
  the local binding.  2value* is ignored when 2xvcell* is given.

1ZWEI:KILL-LOCAL-VARIABLE* 2variable*
  makes 2variable* no longer be local in the current editor or buffer.
  It reverts to its global value.

The easy way to make a Zwei user option variable such as 1*COMMENT-COLUMN**
local is with the command 1Meta-X Make Local Variable*.  It reads a variable's
pretty name (such as 1Comment Column*) with completion and makes that variable
local.  The complementary command 1Meta-X Kill Local Variable* also exists.

1Meta-X List Local Variables* prints the names and values of all the local
variables in the current editor or current buffer.

] 4`Shifted' Mouse Clicks.* 

If is now possible to use `shifted' mouse clicks to give Zmacs commands.
(Where `shifted' means modified by one or more of the 1Control-*,1 Meta-*,
1Super-* or1 Hyper-* keys) Thus it is now possible to give the 1M-X Set Key* a
`shifted' mouse click (like 1Control-Mouse-Left-1) *as the key, and to set
`shifted' mouse keys in init files using 1ZWEI:SET-COMTAB*.

] 4Deleted Files and 3DIRED*.*

DIRED now displays files that are really deleted on disk with a lower case "d"
in the first column.  Files whose deletion has been requested but not done
are displayed with a capital "D".  If you request undeletion of an actually deleted
file, the file is displayed with a capital "U", but such operations as printing,
editing, or applying a function to the file are not allowed since the file is
really still deleted.

] 4Pathname Defaulting in 3DIRED* and 3BDIRED*.*

When the current buffer is a DIRED or BDIRED buffer and you issue a
command that reads a filename, the default filename is now the file
whose line you are pointing at.

] 4Matching Close Parentheses Displayed for Open Parentheses.*

When point is before an open parenthesis, the matching close parenthesis now
blinks.  If point is both before an open parenthesis and after a close parenthesis,
the matching open of the preceding close parenthesis is the one that blinks.

] 3C-Shift-J 4is now *Change Font Region4.**

The command 1C-Shift-J* is now 1Change Font Region*, just like 1C-X C-J*.

] 3M-Shift-J4 is now *Change One Font Region.*

This command operates on all characters in the region which have
a particular font, changing them to another font.  It asks for the
font to look for first, and then the font to change to.  For example,
you could specify to change all font A characters into font C.

] 3Meta-X View File4 and Fonts.**

The 1View File* command now displays the file with its correct fonts
if the file specifies fonts in its attribute list.

] 3Meta-X Tags Search List Sections4.**

This command searches all the files in the currently selected tag table
for a string that you specify.  It does not itself move point or select
a different buffer.  Instead it records which sections the string is found in
and then prints a list of the sections' names.  You can then begin visiting
the sections one by one with 1C-Shift-P*.  

] 3Control-X 4 J* 4Jumps to Saved Point in Other WIndow.*

If you save a location in a register with 1C-X S* 2register*, you can jump to it again
with 1C-X J* 2register*.  Now you can also select the other window (in or entering
two-window mode) and jump to the saved location in that window, by using
1Control-X 4 J* 2register*.

] 4Changed Default for 3Meta-X Source Compare*.*

The default version is now 1:NEWEST* for both the first and the second input file.

] 3Meta-X Start Private Patch4.**

A private patch is one which is not installed in the system.  It is not associated
with any specific patchable system, and it does not get a patch version number.
It is simply a file of redefinitions which you can load explicitly if you like.
1LOAD-PATCHES* does not know about private patches.

1Meta-X Start Private Patch* starts editing a private patch.  You are asked
to specify the pathname of the patch file.  Once you have done
this, you can put text into the patch with 1Meta-X Add Patch*, just as you
can for installed patches.  You finish with 1Meta-X Finish Patch*, as usual.
This saves and compiles the patch file.

You can use 1Meta-X Start Private Patch* to resume editing a private patch
you created previously.  This works whether or not you had finished the patch
earlier.

] 3Meta-X Add Patch Changed Sections4.**

The command 1Meta-X Add Patch Changed Sections* finds all sections you have
changed the text of, in all buffers, and asks you for each one whether to do
1Meta-X Add Patch* of its text.  But sections which have been 1Add Patch*ed already
since their last modification are excluded.  If you answer the question P,
then all the rest of the changed sections in the same buffer are patched without
further question.

1Meta-X Add Patch Buffer Changed Sections* is similar but considers only the
current buffer's sections.

] 4Buffer Selection History Now Per Window.*

Each Zmacs window now keeps its own history of all buffers. The 1C-M-L*
command, and defaulting when reading a buffer name argument, both use the
selected window's history.  (This is the same history that you can yank from
using the 1Meta-Shift-Y* command when giving a buffer name argument.)
The history's `most recent' elements are buffers which have been selected in this
window, most recent first. The least recent elements are other Zmacs buffers which
have not yet been selected by this window. The histories of different Zmacs windows
all contain the same elements, but they may be in different orders.
1C-X C-B* now displays the per-window history.

] 4Command 3C-X C-;* to Make the Region into a Comment.*

This command puts a comment starter (value of 1ZWEI:*COMMENT-BEGIN**) in front
of each line starting in the region, except for blank lines.
With numeric argument, it removes precisely the value of 1ZWEI:*COMMENT-BEGIN**
(a single semicolon in Lisp mode) from each line in the region that starts with one.

You can use 1C-X C-;* to comment out the lines of the region before recompiling
a function.  Later, use 1C-U C-X C-;* to remove the commenting thus made.
Only a single semicolon is removed, so any lines which were comments before
commenting out the region remain comments after un-commenting the region.

] 4More Rubout Handler Commands.*

The rubout handler now has a mark, and supports the commands 1Control-Space*,
1Control->*, 1Control-<*, 1Control-W* and 1Meta-W*.  They work about the same
as the editor commands of the same names.

The rubout handler also now supports 1Meta-T*.

1Meta-Status* is now a way to print the rest of the input history beyond
the part that 1Status* shows you.  A numeric argument specifies how many
elements to skip mentioning at the front of the input history.
1Control-Meta-Status* does a similar thing for the kill history, to complement
the 1Control-Status* command.

] 3M-X Copy File, Rename File, Delete File, Undelete File4.**

These four commands have been changed to do prompting and querying in a new way.

If the pathname specified has no wildcards, no prompting or querying is done.
The operation is just performed.

If there is a wildcard, then a list of the files that match it is printed all at once.
You are then asked to confirm with Y or N.  If you say Y, then all the files
are operated on forthwith.
