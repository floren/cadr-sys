;-*- Mode:Text; Fonts:(TR12 CPTFONT TR12I HL12B TR12B TR12BI); Readtable:T; Base:10 -*-

System 99 is nigh!! Beware the coming of system 99!
Who knows what microcode version it will use, but
it will be  320. (For the CADR)  (For the Lambda, it's XXX)

Files compiled in System 98 will probably run in System 99.
Files compiled in System 99 will probably not run in System 98. Stay tuned.

CADR Users:

Using some as-yet-unwritten magic, it will be possible to debug System 99 from
System 98 and perhaps also System 97, but not any earlier systems.
System 99 can debug all earlier systems. Yow!

Contents of the rest of this file:

4   Using Common Lisp in the MIT Lisp Machine System.* 
4   Incompatible Changes* 
4   Compatible Lisp Programming Changes* 
4   Window System Changes* 
4   User Interface Changes* 
4   Editor Changes* 
4   Network Changes
   Site option Changes*


4Using Common Lisp in the MIT Lisp Machine System.* 

The variable 1*DEFAULT-COMMON-LISP** has now gone --- readtables now contain all
the data which relied upon this variable. To use make a lisp listener use Common
Lisp reader syntax and functions by default, do
1 (SETQ *READTABLE* SI:COMMON-LISP-READTABLE)* 
  or equivalently,  1(COMMON-LISP T)* 
To use traditional Zetalisp syntax and functions, do
1 (SETQ *READTABLE* SI:STANDARD-READTABLE)* or 1(COMMON-LISP NIL)* 

To make a file read in using common-lisp syntax, and use common-lisp functions,
put "1Readtable: Common-Lisp*" or "1Readtable: CL*" in the attribute list
(the "1-*-*" line) of the file.
To make a file always read using traditional syntax and functions, use
"1Readtable: Traditional*" or just "1Readtable: T*".

The new editor command 1m-x Set Readtable* will do this for you.
(See the Editor Changes section.)

Note that Zmacs now understands the different quoting characters ("1/*" and "1\*" respectively)
used by the different readtables, and is parses lisp code based on which readtable is
current.

4Incompatible Changes* 

1] 4Decimal radix is now the default.** 

The default values for reading and printing integers is now base 10.
The value of 1*nopoint* is now 1T* by default. All files (except init files) written
out by Zmacs in the last few systems have automatically had a base attribute
added to the "1-*-*" line, so this should not break many existing files.
If you want your init file to be in base 8, add 1Base:8* to the 1-*-* line.

To get back the old behaviour, do 1(setq *print-base* 8. *read-base* 8.* 1*nopoint nil)*

] 4Lexical Evaluator is now the default.* 

hacks declarations, leaps tall buildings in a single bound.
semantics are now exactly the same as those of compiler.
say something about lexical closures??
1(set 'a 3)* 
1(defun foo ()* 
   1(declare (special a))* 
   1(print a))* 
1(let ((a 1))* 
   1(foo))* 
will print "13*" just as it would if compiled.

Say much much more!!!

(let ((foo 1)) #'(lambda () foo) =>

]1 4Changes to 3FORMAT* control argument.** 

The control argument to 1FORMAT* has changed for compatibility with Common Lisp.
There are no longer separate functions 1FORMAT* and 1CLI:FORMAT*, as there were in
System 98.  This change will affects the following 1FORMAT* directives:

1~X*	1~2width,padchar,commachar*X* 
	1      *Prints its argument in hexadecimal. (Analogous to 1~O*, 1~B* and 1~D*)
	This command used to be used to insert spaces into the output.

	Use 1~2number-of-spaces*@T* to achieve the same result as the old 1~2number-of-spaces*X* directive.

1~F*	1~2width,decimal-places,scale,overflowchar,padchar*F* 
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

1  *In System 99, 1CLI:FORMAT* has the same function definition as 1GLOBAL:FORMAT*, so code written
for1 *System 98 which use1d CLI:FORMAT* will work correctly in System 99.
1  *Only code which used 1GLOBAL:FORMAT* with any of the above changed1 FORMAT* directives will need
to be altered to run in System 99.

] 4The3 FONTS* package no longer3 USE*s3 GLOBAL.** 

This means that any fonts created in earlier systems will have to be redumped
in order to work with system 99. This has been done for all the system's fonts
appearing  in the 1SYS: FONTS;* directory.
There are two ways to do update the fonts to run in system 99. The first is to
write out (using 1FED*) a 1KST* format file of the font, load that into a system 99
world and then write out a 1QFASL* font file.
The other technique is to do the following (in either system 98 or system 99)
   1(use-package "GLOBAL" "FONTS")* 
   1(load 2file-containing-font*)* 
   1(unuse-package "GLOBAL" "FONTS")* 
   1(compiler:fasd-symbol-value 2file-to-contain-font *'fonts:2name-of-font*)* 
Fonts from system 99 will load into system 98, but to load a system 98 font into
a system 99 world, you will have to perform one of the above conversions.

] 4Ratios read and print according to current value of 3*READ-BASE** and 3*PRINT-BASE*** 

Ratios used to be always read and printed using decimal notation when using
traditional (non-common-lisp) syntax.
Thus, 1#5r-10\12* (or 1#5r -10/12* in common-lisp syntax) now represents
``minus five sevenths.''

] 3SELECTQ4 now uses *EQL4 as its test function.** 

1SELECTQ* formerly performed all its comparisons using 1EQ*.
Since everything that is 1EQ* is also 1EQL*, and the only things which are 1EQL* but not 1EQ* 
are flonums, bignums and ratios (which should never have been used as tests for
1SELECTQ* in the past for this very reason) there should be no effect on any existing code.
1SELECTQ* and the Common Lisp macro 1CASE* are thus now identical.


] 3CATCH4 and *THROW4 are now synonymous with **CATCH4 and **THROW4 respectively.** 

1CATCH* and 1THROW* used to be defined in a way which was compatible with Maclisp.
1(CATCH *form1 TAG)* used to be what 1(CATCH 'TAG *form1)* is now, and
1(THROW *form1 TAG)* used to be what 1(THROW 'TAG *form1)* is now.
Since Maclisp itself has been issuing warnings for years saying to use 1*CATCH*, this
should cause no problems.
The implementation restrictions and general weirdness associated with the values from
1CATCH* (aka 1*CATCH*) in older system versions have been fixed; 1CATCH* now returns all the
values from the last form executed (if no 1THROW* occurs) or else the values supplied by
the second argument to 1THROW*.

4] Arguments to 3APROPOS* and 3WHERE-IS* extended* 

The arguments to apropos are now
1SUBSTRING &OPTIONAL (PACKAGE *ALL-PACKAGES*)* 
1          &KEY (INHERITORS NIL) (INHERITED T)* 1DONT-PRINT* 
1               PREDICATE BOUNDP FBOUNDP*
The 1PACKAGE* argument is now the always the second argument
(it used to be a keyword argument)
The value of this argument may be 1NIL*, meaning to search all packages,
a single package or package name, or a list of packages and/or package names.

1WHERE-IS* now accepts a package or a package name or a list of packages
and/or package names as it second argument

] DEFSTRUCT Macro Constructors Frob 'foo???

What does this mean ?
I'll tell you when I fix it.

] Change In Device Delimiting In Filenames

Because of changes in the file access system internals, a device in a
file specification with no explicit host must be preceded by a colon.
(This is because the filename parser must check to see if the device name
could also be a host name.)  For example, :SS:<SYS.UNSUPPORTED>YOW.MAC
is needed to specify a file on the SS device of a TOPS-20 host (which
is presumably a part of the defaults).  If the colon were not there, the
system may try to contact to host table in order to find out if
``SS'' were really a host.

Note that that change is important only if you most often use
Twenex, VMS, or ITS file servers.

BOB!!!!!! This MUST get an error if there is no such host!
It can't just randomly default to the current host! FIX THIS ASAP!

] channge in meanning of seconnd arg (with-respect-to) to fs:parse-pathnname

4Compatible Lisp Programming Changes*

] New frobs 1SI:FIND-READTABLE-NAMED SI:*ALL-READTABLES* SI:RDTBL-NAMES* 

Talk a little about named readtablesat all ?

] 4New way of initializing process of 3TV:PROCESS-MIXIN** 

Normally, if the 1PROCESS* argument to 1TV:PROCESS-MIXIN* is  a symbol,
it is used as the top level function and 1MAKE-PROCESS* is called with
no keyword arguments.
But, as an exception, if1 PROCESS* is1 T*, the top level function is
to send the window a 1:PROCESS-TOP-LEVEL* message with no arguments.
So, for example, one could write:

1(defflavor crock-window ()* 
1                        (tv:process-mixin tv:window)* 
1  (:default-init-plist :process t)* 
1  (:documentation "A window which displays a crock."))* 

1(defmethod (crock-window :process-top-level) ()* 
1  (draw-crock self)* 
1  (do-forever* 
1    (update-hands)* 
1    (process-sleep 60.)))* 

] 4Compiler Optimization changed.* 

Many compiler optimizers have been reimplemented, and should often produce
better code. The most visible change is that any form is only optimized once,
no matter where it appears. (In earlier systems, a form could sometimes be optimized
twice, which could produce duplicate compiler warnings) In addition, the order in which
optimizations are carried out has changed. All the arguments to a function are optimized
before the call to the function on those arguments, unless the "function" is a macro
or special form, in which case it is expected to take responsibility for doing its own
optimizations.

4] New 3:SUBMIT* option for opening files* 

A new 1:submit* option available in 1OPEN* and 1WITH-OPEN-FILE* and friends, when 1T*
(default 1NIL*) will submit the file for batch processing.  Thus, the 1:direction*
must be 1:output* as well.  The 1:submit* option is currently effective on VMS and
Twenex Chaosnet FILE servers.

An example:

1(DEFUN RETRIEVE-TWENEX-FILE (F) 
  "Submit a batch job that will retrieve the file F" 
  (SETQ F (FS:PARSE-PATHNAME F))
  (WITH-OPEN-FILE (S (SEND (SEND (SEND (SEND F :HOST) :SAMPLE-PATHNAME) *
				1 :HOMEDIR)*
			1   :NEW-PATHNAME :NAME "_RETRIEVE" *
			1   :TYPE "CTL" :VERSION :NEWEST)*
		1     :DIRECTION :OUTPUT*
		1     :TEMPORARY T*
		1     :SUBMIT T)
    (FORMAT S "@RETRIEVE ~A~%" (SEND F :STRING-FOR-HOST))))

4] New 3:PROPERTIES* operation on** 4file streams* 

Sending a 1:properties* message to a file stream returns two values: a property
list, like the kind which is a element of the list returns by 1FS:DIRECTORY-LIST*,
and a list of settable properties.  There is the usual optional ``error-p'' argument,
as well.  This operation uses a new 1PROPERTIES* command in the Chaosnet file
protocol, so it may not work with servers running old software.

4User Interface Changes* 

] 4New function* 3COMMON-LISP*  

When this function is called in a lisp-listener, it changes whether Common Lisp
or Tradtional Zetalisp syntax and incompatible functions are to be used for reading
and printing lisp objects. It works by 1SETQ*ing 1*READTABLE**.

It takes one argument, which should be either 1T* or 1NIL*.

See the section 5Using Common Lisp in the MIT Lisp Machine System*.

] 4Standard hardcopy options can be defaulted on a per-printer-type basis* 

1(SET-PRINTER-DEFAULT-OPTION* printer-type option value1)* allows the user to set a
default option for a printer type, which the hardcopy functions look at.
A common use at MIT may be 1(SET-PRINTER-DEFAULT-OPTION :DOVER :SPOOL* 1T)*,
which will cause Dover output to be spooled unless the 1:SPOOL* option to a
hardcopy function is supplied.  Currently defaultable options are 1:FONT*,
1:FONT-LIST*, 1:HEADING-FONT*, 1:PAGE-HEADINGS*, 1:VSP*, 1:COPIES*, and 1:SPOOL*.

] 4Usual mail file* 4directory option for ZMail* 

You can set this option in the Profile editor in ZMail.  It simply informs ZMail to
use a short name for a mail file in a menu, if that file is found in the
directory.  (The full name of the file is displayed if it has not been read
into a buffer yet.)

]4 New beep types* 4for the type argument to* 3TV:BEEP* 

	2Name*			      2Condition* 
	1FQUERY*		      1FQUERY* asks for something and needs to beep.
	1SUPDUP:TERMINAL-BELL*   Bell character received

4] 3DRIBBLE-START*, 3DRIBBLE-END** 4gone*
Use 1(DRIBBLE 2filename*)* or 1(DRIBBLE-ALL 2filename*)* to start wallpapering output
to a file, and 1(DRIBBLE)* with no arguments to terminate output and close the
file.

4] New special form: 3MULTIPLE-CERROR** 
do I really want ot document this??

] string-append frobs array type

] :set method on flavors


4] 3SELECT-MATCH* and 3LIST-MATCH-P* now accept logical combinations of patterns* 
Hack and, or, not appearing at top-level. Not ambiguous, since not quoted.

1(defun hack-add-sub (x)* 
1  (select-match x* 
1    ((or `(+ ,y 0) `(- ,y 0) `(+ y)) t* 
1     y)* 
1    ((not (or `(+ . ,ignore) `(- . ,ignore))) t* 
1     (ferror nil "You lose"))* 
1    (t x)))* 

4] Complex and rational arithmetic is now common-lisp compatible.* 

canonicalization
rampant floating and complex contagion
complex rational canonicalization
real and imag parts of complexnum of same type

There are still some many cases of transcendental functions of complex to be written
Also atan of one arg.

SYS:NEGATIVE-SQRT gone

new condition SYS:ILLEGAL-EXPT

log takes optional second arg
sqrt returns complexnum if necesary

4] Short-floats have one more bit of mantissa.* 

Short-floats now have 17 bits of mantissa, 7 bits of exponent magnitude, and
1 bit of exponent sign.  Short-floats in QFASL files dumped in 99 will load
properly in System 98 if the mantissa is within the old range; if not, an
error will be signalled.

4] New types 3NON-COMPLEX-NUMBER* and 3REAL** 

1(TYPEP 2x *(NON-COMPLEX-NUMBER 2low high*))* returns 1T* if 2x* is a non-complex
number (ie a floating-point number, a ratio or an integer) between 2low* and2 high*,
the limits as usual being inclusive normally, or exclusive if they constist of a
list of one element.
Note that complex-numbers with imaginary part = 0 are never of 1NON-COMPLEX-NUMBER*,
since they are always of type 1COMPLEX*. To account for this additional case, there
is another new type, 1REAL*, which is defined such that 
1(TYPEP 2x *(REAL 2low high*))* returns 1T* if 2x* is a either a non-complex
number between 2low* and2 high*, or a complex number with a zero imaginary part
and a real part lying between 2low* and 2high*.

4] Changes to forms specially treated at top-level in the compiler.* 

Following is a partial list of symbols, which, when appearing as the car of a list in a top-level
form, will cause that form to be treated specially by the compiler.
Only those whose meanings have changed, or require clarificatrion, are listed here.

1PROGN*		Treat all following forms as if they also were at top level.
		Note that in Maclisp and in systems 97 and earlier, it was
		necessary for the cadr of the list to be the symbol 1'COMPILE* 
		for this to happen. This curious behaviour has been eliminated.
1PROCLAIM*	The arguments are evaluated, and relevant proclaimations
		(such as 1SPECIAL*, 1NOTINLINE*) are used in the remainder of the compilation.
		This is as if the form were contained within a 1(eval-when (eval compile load) ...)* 
1EXPORT IMPORT IN-PACKAGE MAKE-PACKAGE SHADOW* 
1SHADOWING-IMPORT UNEXPORT UNUSE-PACKAGE USE-PACKAGE* 
		These perform their relevant actions as if the form contained within a
		1(eval-when (eval compile load) ...)* 
1REQUIRE*	Ditto.

To cause a form not to be treated specially at top-level by the compiler, enclose it in
an 1EVAL-WHEN*. Eg:
1(EVAL-WHEN (LOAD)*		;don't want this package to be consed up when we're just compiling!
1  (MAKE-PACKAGE "LOSSAGE" :USE NIL :SIZE 69))* 

4] New macro: 3TV:WITH-WHO-LINE-RUN-STATE** 

Using this macro, it is possible to bind the run-state field of wholine (the one which
usually either "1Keyboard*" or "1Run*") to some user-informative value in a simple manner.
Before, it was necessary to change an instance variable of the current process, which
the scheduler would keep bashing to "1Run*".

Example usage:
1(DEFUN TOP-LEVEL-FROBNICATE (WIDGET)* 
1  (TV:WITH-WHO-LINE-RUN-STATE ("Preprocessing")* 
1    (SETQ WIDGET (PREFROBNICATE WIDGET)))* 
1  (TV:WITH-WHO-LINE-RUN-STATE ("Pass 1")* 
1    (FROB-1 WIDGET))* 
1  (TV:WITH-WHO-LINE-RUN-STATE ("Pass 2")* 
1    (FROB-2 WIDGET)))* 

4] New variable 3TIME:*DEFAULT-DATE-PRINT-MODE*** 

What kind of time printing does it effect ?
 --- see the manual, ``Bob'' Section 34.2, `Printing dates and times,' or sys:io1;time
 time:print-time, time:print-universal-time, time:print-brief-universal-time,
 time:print-date, time:print-universal-date
Description of possible values
Only one "/" in zl
 :DD//MM//YY						;27/10{/66}
 :DD//MM//YYYY						;27/10{/1966}
 :MM//DD//YY						;10/27{/66}
 :MM//DD//YYYY						;10/27{/1966}
 :DD-MM-YY							;27-10{-66}
 :DD-MM-YYYY						;27-10{-1966}
 :DD-MMM-YY						;27-Oct{-66}
 :DD-MMM-YYYY						;27-Oct{-1966}
Note that use "\" in cl
 :DD/ MMM/ YY						;27 Oct{ 66}
 :DD/ MMM/ YYYY						;27 Oct{ 1966}
 :DDMMMYY							;27Oct{66}
 :DDMMMYYYY						;27Oct{1966}
 :YYMMDD							;661027
 :YYYYMMDD						;19661027
 :YYMMMDD							;{66}Oct27
 :YYYYMMMDD						;{1966}Oct27
 :YY-MMM-DD						;{66-}Oct-27
 :YYYY-MMM-DD						;{1966-}Oct-27
 :YY-MM-DD							;{66-}10-27
 :YYYY-MM-DD						;{1966-}10-27
these last four, and all the "yyyy" ones are new since the manual

Default value
 :mm//dd//yy YECH! BLECH! BARF!! I really think that this should be :dd-mmm-yy
 I cannot see anything that this could break. ``Bob'' would you ask LMI people what
 they think of that?

] time parser accepts iso format dates (99.10)

understands yyyy-mmm-dd, yyyy-mm-dd


4] Changes to logical pathnames* 

This needs much flaming!

Logical pathnames now have structured directories.

Pathname translations are now really translations, not just an alist.
Example of use.

FS:MAKE-LOGICAL-PATHNAME-HOST
FS:SET-LOGICAL-PATHNAME-HOST

SYS:SITE;FOO translations

4] New function: 3FS:MERGE-PATHNAME-COMPONENTS** 

This function extends the functionality of both the commonlisp function 1MERGE-PATHNAMES* 
and the old Lisp Machine function 1FS:MERGE-PATHNAME-DEFAULTS*. 

Its argument list is:
1(PATHNAME &OPTIONAL DEFAULTS* 
1          &KEY DEFAULT-NAME ALWAYS-MERGE-NAME* 
1               DEFAULT-TYPE ALWAYS-MERGE-TYPE* 
1               DEFAULT-VERSION ALWAYS-MERGE-VERSION)* 

1MERGE-PATHNAME-COMPONENTS* defaults components that are of 1PATHNAME* which are 1NIL*,
and returns the defaulted pathname.
1DEFAULTS* is a pathname or a defaults-list to get defaults from.
If non-1NIL*, 1DEFAULT-NAME,* 1DEFAULT-TYPE* and 1DEFAULT-VERSION* respectively are used as the
defaults for the name, type and version components if those components are not supplied
by 1PATHNAME*.  Otherwise, those components are defaulted from 1DEFAULTS* in the usual manner.
1ALWAYS-MERGE-NAME*, 1ALWAYS-MERGE-TYPE* and 1ALWAYS-MERGE-VERSION* respectively mean that
the version and type components should always be merged in (from either 1DEFAULT-2xxx**
or from 1DEFAULTS*) even if the relevant component is already specified by 1PATHNAME*.

1(MERGE-PATHNAMES PATHNAME DEFAULTS DEFAULT-VERSION)* is thus equivalent to:
1(MERGE-PATHNAMES-COMPONENTS PATHNAME DEFAULTS :DEFAULT-VERSION DEFAULT-VERSION
                                              :ALWAYS-MERGE-VERSION T)*
since `CommonLisp says' that the 1DEFAULT-VERSION* argument to 1MERGE-PATHNAMES* is merged
into the resulting even if 1PATHNAME* already had a version component.

1FS:MERGE-PATHNAME-COMPONENTS* differs from 1FS:MERGE-PATHNAME-DEFAULTS* in that it performs
2only* the merging operation of filling 1NIL* components of one pathname with (possibly 1NIL*)
components from the defaults, whereas 1FS:MERGE-PATHNAME-DEFAULTS* will 2never* return a pathname
with a 1NIL* name or type component (and may also do a whole bunch of heuristic dwinoid things,
which is not always appropriate)

  1FS:MERGE-PATHNAME-DEFAULTS* is thus a function useful for defaulting a pathname that the user
has just entered for some purpose, such as to be read.
  1FS:MERGE-PATHNAMES-COMPONMENTS* will perform a single merging (and 2may* return a pathname
which is not accceptable for performing file operations upon --- such as a pathname with a name
of 1NIL*.) It is useful for programs which need to manipulate filenames in an exact manner
(such as the file server) and do not want any user-oriented heuristics happening ``behind its back.''
It ignores such variables as 1*ALWAYS-MERGE-TYPE-AND-VERSION** and1 *NAME-SPECIFIED-DEFAULT-TYPE**,
which 1FS:MERGE-PATHNAME-DEFAULTS* uses.
  1MERGE-PATHNAMES* is a lobotomized version of 1FS:MERGE-PATHNAME-COMPONENTS* which Common-Lisp
implementations understand.

A typical use of 1FS:MERGE-PATHNAME-COMPONENTS* is
1(SETQ QFASL-FILE (FS:MERGE-PATHNAME-COMPONENTS QFASL-FILE LISP-FILE* 
1                                              * 1:ALWAYS-DEFAULT-VERSION T* 
1                                              * 1:DEFAULT-TYPE :QFASL* 
1                                              * 1:ALWAYS-DEFAULT-TYPE T))* 
which will produce a file whose version is the same as that of 1LISP-FILE* and whose type is
always 1QFASL*, and whose other components are the (perhaps 1NIL*) results of merging
the components of 1LISP-FILE* with 1QFASL-FILE*.

Some examples:
1(setq pn1 (make-pathname :host twenex-host :name "FOO" :version 259))* 
1  => #FS::TOPS20-PATHNAME "TWENEX:FOO..259"* 
1(setq pn2 (make-pathname :host twenex-host :device "DP" :type :TEXT))* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:.TEXT."* 

1(fs:merge-pathname-components pn1 pn2)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.TEXT.259"* 
1(fs:merge-pathname-components pn1 pn2 :default-version 5)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.TEXT.259"* 
1(fs:merge-pathname-components pn1 pn2 :default-version 5 :always-merge-version t)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.TEXT.5"* 
1(fs:merge-pathname-components pn1 pn2 :default-version 5 :default-type :lisp
                              :always-merge-version t)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.LISP.5"* 

1(fs:merge-pathname-components pn2 pn1)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.TEXT.259"* 
1(fs:merge-pathname-components pn2 pn1 :always-merge-type t)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO..259"*		;merges in null type!
1(fs:merge-pathname-components pn2 pn1 :default-type :lisp)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.TEXT.259"* 
1(fs:merge-pathname-components pn2 pn1 :default-type :lisp :always-merge-type t)* 
1  => #FS::TOPS20-PATHNAME "TWENEX:DP:FOO.LISP.259"* 


] mouse blips (as from :any-tyi) format changed.
Used to be (:mouse-button # # #/mouse-l-1 ...)
Now (:mouse-button #/mouse-l-1 window x-position y-position)

This is different from what is documented in windowman.
Changed for brand s

4] 3TERMINAL C-CLEAR-INPUT* is now 3TERMINAL C-M-CLEAR-INPUT** 

This keyboard sequence is used to try to unhang some window-system
problems. It has been changed so that 1c-clear-input* is typeable (by having
it quoted with "1terminal*," which causes it to lose its special meaning of "flush
keyboard typeahead" and be simply passed on to the program which is reading
from the keyboard.)

4Editor Changes* 

] Editor Aids for Common Lisp

m-X Set Readtable
How is the readtable name interpreted ?  What are the names for Zetalisp ?
For Common Lisp ?  What happens if you just type <Return> ?

m-X Set Common-Lisp [???]
Does this set the base, too ?

Besides binding the readtable for the editor buffer and the break loop,
the readtable attribute also sets the quoting character (one of the two 
slash characters) as appropriate.

] Lisp Case Changing Commands Renamed

The extended (m-X) commands for changing the alphabetic case of Lisp code
have been renamed:

  Old name                     New name
  Lisp Lowercase Region        Lowercase Lisp Code In Region
  Lisp Uppercase Region        Uppercase Lisp Code In Region

As a result, typing ``m-X lisp'' now completes to ``Lisp Mode.''

] Changes in Two History Yank Commands

When editing in a minibuffer, c-m-Y yanks in the history, making it consisten with
the action of c-m-y in the rest of the system, and  m-sh-Y inserts the previous command
given in the minibuffer.

] ZWEI:SEARCH renamed ZWEI:ZWEI-SEARCH to avoid name conflict with GLOBAL:SEARCH
Note that this is only an internal frob used by people writing their own zmacs commands,
and is not visible to m-x losers

[Does the old name still work ?]
Yes, but the compiler barfs. The name should be changed asap, but will continue to work
for another system version or so, I guess

4Network Changes* 

]4 3SI:PARSE-HOST* Uses the 3HOSTAB* Servers By Default* 

1SI:PARSE-HOST*'s second argument, 1UNKNOWN-OK*, now defaults to 1T*.  That means that if it
can't find the host on 1SI:HOST-ALIST*, it tries contacting a host table server to see if 2it* 
knows about the host.  If the server contact does not, an error is signalled (or 1NIL* is
returned) as usual.  The change was made to minimise the penalty for not loading the
latest site files.  (Up-to-date site information can be a problem at large installations.)

The list of hosts that may be contacted on the Chaosnet for this service are listed in the
site option 1:CHAOS-HOST-TABLE-SERVER-HOSTS*.

]4 New Chaos Routing Inspector Functions.* 

These two functions make use of the 3DUMP-ROUTING-TABLE* protocol,
documented in the new edition of the Lisp Machine Manual.  They are primarily
for inspecting the operation of the network and the localisation of
bridging and routing problems.

(1CHAOS:SHOW-ROUTING-TABLE* host &optional (stream 1*STANDARD-OUTPUT**)) will
print out the routing table of 2host* onto2 stream*.

(1CHAOS:SHOW-ROUTING-PATH* &key (from 1SI:LOCAL-HOST*) to (stream 1*STANDARD-OUTPUT**))
will show how packets will flow from 2from* to 2to*, using the routing information
supplied by 2from* and any intervening bridges to figure out the path.

For example, 1(chaos:show-routing-path :from "charon" :to "nu-1")* may produce
the following output:

1MIT-CHARON will bounce the packet off MIT-SIPB-11 at cost 81.* 
1MIT-SIPB-11 will bounce the packet off MIT-INFINITE at cost 63.* 
1MIT-INFINITE will bounce the packet off MIT-BYPASS at cost 51.* 
1MIT-BYPASS will bounce the packet off MIT-OZ-11 at cost 37.* 
1MIT-OZ-11 will bounce the packet off XI (XX-Network-11) at cost 23.* 
1Direct path from XI (XX-Network-11) to host MIT-NU-1 on subnet 32 at interface 1.* 

]4 Chaosnet source files have been reorganised* 

The old files 1SYS: IO; CHSNCP *and1 CHSAUX *have been moved to
the1 NETWORK;CHAOS;* 1 *directory.  (See the 2Site option Changes* section for advice
before restoring the1 *new sources.)  These files have been augmented by a new
file, 1CHUSE*, which now contains the user connection-level functions previously
found in 1CHSAUX*.  Also moved from the 1IO1* directory are 1CHATST* (low-level
testing functions) and 1EFTP*, an interface to the 3Mb (old) Ethernet Easy File
Transfer Protocol that requires a protocol-translating gateway.

4Site option Changes* 

]4 New source directories need to be created* 

A few new directoires will be used to hold Lisp Machine source files that relate
to network software.  Please create these directories on the 1SYS* host, and update
the appropriate 2sys-host-translations* variable(s) in 1SYS: SITE; SITE LISP :* 

	2Logical directory*	2Suggested translation*
	1NETWORK*		1L.NETWORK* 
	1NETWORK; CHAOS*	1L.NETWORK.CHAOS* 
	1NETWORK; IP*		1L.NETWORK.IP* 
	1IO*;1 FILE*	1      L.IO.FILE*

Say how to use sys:site; system translations to frob this for random hosts

Of course, the names of the directories will be different if you don't store
sources under a 1L* hierarchy, but the names should follow this form.

]4 New site option* 3:STANDALONE* 

If the Lisp Machine is just by itself, the option should be supplied with value T.
This will cause the Lisp Machine to not to try to use the Chaosnet for getting
the time, for one thing.  On the Lambda, the time will obtained from the SDU's
clock.  On the CADR, the time will be obtained from the user.

]4 New site option* 3:LOCAL-INTERNET-HOST-TABLE* 

Defaults to 1SYS: SITE; IN-HOSTS TEXT* (if not supplied or with the value
T), but can be changed to share with other subsystems or machine types.

]4 New site option* 3:VERIFY-LM-DUMPS* 

If this site option is 1T*, Lisp Machine file system dump tapes will be verified.

] New Way of specifying file computers

Because of changes to the internals of the file access system, the method
for specifying file computer at a site has changes.  The old list
in the site option 1:CHAOS-FILE-SERVER-HOSTS,* but the new option
1:FILE-SERVER-HOSTS* is preferred.  Note that:

 * A pathname host does not have to be on the list :FILE-SERVER-HOSTS
   to actually be recognized as a pathname host.  (This was not true in
   earlier systems.)  As long as the host is known (either in the host table
   or known about by a host table server), an error will not occur.
   Thus, it is not necessary to edit 1SYS: SITE; SITE LISP* when a new
   file server is attached to the site; the site option merely names what
   file servers are known initially.

 * For now, the old and new names of the site option are equivalent, 
   (with one exception -- see below) but
   the new name is preferred in antcipation of when TCP/FTP file access is
   implemented.

For LMFS support, a file computer in the pathname host list was specified
as a list of the host and the file system type.  This has been changed
to be more convenient -- instead, there is new site option
:SPECIAL-FILE-HOSTS which is a list of

	1(2file-system-type* . 2hosts*)* 

where file-system-type is a keyword.  The only supported one at the
moment is 1:LMFS*, as before.

It is now possible is specify the default device of a host by using the
site option 1:HOST-DEFAULT-DEVICE-ALIST*, an alist of host names and device
names (with the colon).  This currently only works for Twenex hosts.
An example of the use of the option:

	  1(:HOST-DEFAULT-DEVICE-ALIST '(("OZ" . "OZ")))* 

Here, we are overriding the default name 1PS*. 

If, for example, a Twenex host is configured for a non-1PS* primary structure
name, this option should be used, to eliminate some strange interactions
that can happen when the truenames of files are compared against supplied
names.

