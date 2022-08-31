
-*-Fonts:TR12,cptfont,TR12I,HL12B,TR12B; Mode:Text-*-

4Common Lisp on the MIT Lisp Machine.

Data Types*

Common Lisp defines four standard names for floating point formats.  In order of
increasing precision, these are 1SHORT-FLOAT*, 1SINGLE-FLOAT*, 1DOUBLE-FLOAT,* and
1LONG-FLOAT*.  However, they are not required to be all distinct.  The Lisp machine
actually provides two distinct floating point formats, just as it used to.
1SHORT-FLOAT* is a name for the smaller one, the 2small-flonum*, and the other three
names, 1SINGLE-FLOAT* and so on, are names for the larger one.

In a complex number, Common Lisp specifies that the real and imaginary part
must either both be rational or both be floating point numbers of the same type.
This is now so.

In addition, a complex number whose components are rational and whose
imaginary part is zero is automatically converted to a real number whenever it is
formed.  So complex rational numbers have only one representation; 12+0i is the
same as 12.  But the same is not true for floating imaginary parts: 12.0+0.0i is
different from 12.0.

4Names for Data Types*

Common Lisp includes a very general way of naming data types: 2type specifiers*.
A type specifier describes a class of possible Lisp objects; the function 1TYPEP*
asks whether a given object matches a given type specifier.

Some type specifiers are symbols: for example, 1NUMBER*, 1CONS*, 1SYMBOL*, 1INTEGER*,
1CHARACTER*, 1COMPILED-FUNCTION*, 1ARRAY*, 1VECTOR*.  Their meanings are mostly
obvious, but a table follows below.

Lists can also be type specifiers.  They are either combinations or restrictions of
other type specifiers.  The car of the list is the key to understanding what it
means.  An example of a combination is 1(OR ARRAY SYMBOL)*, which will match
any array and will match any symbol.  An example of a restriction type is
1(INTEGER 0 6)*, which specifies an integer between 0 and 6 (inclusive).

Any given object matches many different type specifiers; for example, the
number 11* matches the type specifiers 1NUMBER*, 1INTEGER*, 1REAL*, 1RATIONAL*,
1(INTEGER -5 2)*, 1(SIGNED-BYTE 3)*, 1(MOD 2)*, 1(MOD 3)*, 1(MOD 4)*, 1(MOD 5)*, and
infinitely many others.  The function 1TYPE-OF* returns 2a* type specifier that
a given object matches, chosen to be one which clearly and usefully classifies
the object, but you should not make assumptions about which one it would be
for any particular object.

1TYPEP* 2object* 2type-specifier*
  1T* if 2object* matches 2type-specifier*.

1TYPE-OF* 2object*
  returns some type specifier which 2object* matches.
  This replaces the old usage of 1TYPEP* with one arg, though that
  usage is still supported.

1SUBTYPEP* 2type1* 2type2*
  returns 1T* if 2type1* is a subtype of 2type2*; that is, if any object of 2type1*
  is certainly also of 2type2*.  For example, 1(SUBTYPEP 'CONS 'LIST)* is 1T*,
  but 1(SUBTYPEP* 1'LIST 'NUMBER)* is 1NIL*, because not all lists are numbers
  (in fact, no lists are number).  1(SUBTYPEP 'NUMBER 'RATIONAL)* is also1 NIL*.

  In some cases the system cannot tell whether 2type1* is a subtype of 2type2*.
  In the most general case, this is not even decideable, due to the existence
  of 1SATISFIES* type specifiers.  1SUBTYPEP*'s value is 1NIL* if the system cannot tell.
  Thus, 1NIL* could mean that 2type1* is certainly not a subtype of 2type2*, or it
  could mean that there is no way to tell whether it is a subtype.  1SUBTYPEP*
  returns a second value to distinguish these two situations: the second value
  is 1T* if 1SUBTYPEP*'s first value is definitive, 1NIL* if the system does not know
  the answer.

1TYPECASE* 2object* 2clauses*...
  tests 2object* against various 2type-spec*s and dispatches according to the result.
  Each 2clause* looks like 1(2type-spec* 2forms**...1)*.  The clauses are tested one by one
  by matching 2object* against the clause's 2type-spec*.  If it matches, then the
  2forms* of the clause are executed and the last form's values are returned
  from the 1TYPECASE*.  If no clause matches, the value is 1NIL*.

  A clause can also have 1T* or 1OTHERWISE* instead of a 2type-spec*.  Then the
  clause always matches if the previous clauses have not.

1COERCE* 2object* 2type-spec*
  converts 2object* to an "equivalent" object which matches 2type-spec*.
  Common Lisp specifies exactly which types can be converted to
  which other types.  In general, anything which would lose information,
  such as turning a float into an integer, is not allowed as a coercion.
  Here is a complete list of types you can coerce to.

  1COMPLEX*
  1(COMPLEX 2type*)*
	Real numbers can be coerced to complex.  If the real is a
	rational, however, then the result will actually be the same
	as the original number, and not complex at all!
	But if you coerce it to type 1(COMPLEX SINGLE-FLOAT)*, say,
	then the result really will be complex.

  1SHORT-FLOAT*
  1SINGLE-FLOAT*
	Rational numbers can be coerced to floating point numbers,
	and any kind of floating point number can be coerced to
	any other floating point format.

  1FLOAT*
	Rational numbers are converted to 1SINGLE-FLOAT*s;
	floats of all kinds are left alone.

  1CHARACTER*
	Strings of length one can be coerced to characters.
	Symbols whose pnames have length one can also be.
	Integers can be coerced to characters.

  1LIST*
  1VECTOR* or 1ARRAY* or any restricted array type.
	Any list or vector can be coerced to type 1LIST* or to any
	type of array.
	If you specify a type of array with restricted element type,
	you may actually get an array which can hold other kinds
	of things as well.  For example, the Lisp machine does not
	provide anything of type 1(ARRAY SYMBOL)*, so if you specify that,
	you will get an 1ART-Q* array (since at least that can hold symbols).
	Also, if the elements you have in the original sequence do
	not fit in the new array, you just get an error.

  1T*
	Any object can be coerced to type 1T*, without change
	to the object.

Here is a table of type specifiers.  Valid kinds of lists are described
according to the symbol that would appear as the car of the list.

      1Basic Data Types

CONS*			non-1NIL* lists.
1SYMBOL*		symbols.
1ARRAY*			all arrays, including strings.
1NUMBER*		numbers of all kinds.
1INSTANCE*		all instances of any flavor.
1STRUCTURE*		named structures.
1LOCATIVE*		locatives.
1CLOSURE*		closures.
1ENTITY*		entities.
1STACK-GROUP*		stack groups.
1COMPILED-FUNCTION*	macrocode functions such as the compiler makes.
1MICROCODE-FUNCTION* build-in functions implemented by the microcode.
1SELECT*		select-method functions (defined by 1DEFSELECT*).
1CHARACTER*		character objects.

      1Other Useful Simple Types

T*			all Lisp objects belongs to this type.
1NIL*			nothing belongs to this type.
1STRING-CHAR*		characters that can go in strings.
1STANDARD-CHAR*	characters defined by Common Lisp.
			These are the 95 Ascii printing characters (including 1Space*)
			and 1Return*.
1LIST*			lists, including 1NIL*.
1SEQUENCE*		lists and vectors.  There are many new functions
			which accept either a list or a vector as a way of
			describing a sequence of elements.
1NULL*			1NIL* is the only object that belongs to type 1NULL*.
1KEYWORD*		keywords (symbols whose home package is 1KEYWORD*).
1ATOM*			anything but 1CONS*es.
1COMMON*		objects of all types defined by Common Lisp.
			This is all Lisp objects except closures, entities,
			stack groups, locatives, instances, select-methods,
			and compiled and microcode functions.
			(but a few kinds of instances, such as pathnames,
			are 1COMMON*, because Common Lisp does define how
			to manipulate pathnames, and it is considered irrelevant
			that the Lisp machine happens to implement pathnames
			using instances).
1STREAM*		anything that looks like it might be a valid I/O stream
			It is impossible to tell for certain whether an object
			is a stream, since any function with proper behavior
			may be used as a stream.  Therefore, use of this
			type specifier is discouraged.

      1More Obscure Types

PACKAGE*		packages, such as 1FIND-PACKAGE* might return.
1READTABLE*		structures such as can be the value of 1*READTABLE**.
1RANDOM-STATE*	random-states.  See 1RANDOM*, below.
1PATHNAME*		pathnames (instances of the flavor 1PATHNAME*).
1HASH-TABLE*		hash-tables (instances of the flavor 1HASH-TABLE*).
2flavor-name*		instances of that flavor, or of any flavor that contains it.
2defstruct-name*	structures of that type, or of any type that includes it.

      1Simple Number Types

NUMBER*		any kind of number.
1INTEGER*		fixnums and bignums.
1RATIO*			explicit rational numbers, such as 11\2*.
1RATIONAL*		integers and ratios.
1FIXNUM*		small integers, whose 1%DATA-TYPE* is 1DTP-FIX* and which occupy no storage.
1BIT*			very small integers--only 0 and 1 belong to this type.
1BIGNUM*		larger integers, which occupy storage.
1FLOAT*			any floating point number regardless of format.
1SHORT-FLOAT* 		small-flonums.
1SINGLE-FLOAT*	regular-size flonums.
1DOUBLE-FLOAT*, 1LONG-FLOAT*   synonymous with 1SINGLE-FLOAT*, on the Lisp machine.
1REAL*			any number that is not explicitly stored as complex.
1COMPLEX*		a number explicitly stored as complex.
			It is possible for such a number to have zero as an imaginary part
			but only if it is a floating point zero.

      1Restriction Types for Numbers

(COMPLEX 2type-spec*)*
  a complex number whose components match 2type-spec*.
  Thus, 1(COMPLEX RATIONAL)* is the type of complex numbers with
  rational components.  1(COMPLEX T)* is equivalent to 1COMPLEX*.

1(INTEGER 2low* 2high*)*
  an integer between 2low* and 2high*.
  2low* can be:
     an integer, which is then an inclusive lower limit
     a list of one integer element.  That integer is then an exclusive lower limit.
     1**, which means that there is no lower limit.
  2high* has the same sorts of possibilities.
  If 2high* is omitted, it defaults to 1**.  If both 2low* and 2high* are omitted,
  you have 1(INTEGER)*, which is the same as plain 1INTEGER*.
  1(INTEGER 0 *)* therefore specifies a nonnegative integer.  So does 1(INTEGER 0)*.
  1(INTEGER -4 3)* specifies an integer betwee -4 and 3, inclusive.
  1FIXNUM* is equivalent to 1(INTEGER 2l* 2h*)*, for appropriate values of 2l* and 2h*.
  1BIT* is equivalent to 1(INTEGER 0 1)*.

1(RATIONAL 2low* 2high*)
(FLOAT 2low* 2high*)
(SHORT-FLOAT 2low* 2high*)
(SINGLE-FLOAT 2low* 2high*)
(DOUBLE-FLOAT 2low* 2high*)
(LONG-FLOAT 2low* 2high*)*
  These specify restrictive bounds for the types 1RATIONAL*, 1FLOAT* and so on.
  The bounds work on these types just the way they do on 1INTEGER*.

1(MOD* 2high*)
  a nonnegative integer less than 2high*.  2high* should be an integer.
  1(MOD)*, 1(MOD *)* and plain 1MOD* are allowed, but the same as 1(INTEGER 0)*.

1(SIGNED-BYTE 2size*)*
  an integer which fits into a byte of 2size* bits, of which one bit is the sign bit.
  1(SIGNED-BYTE 4)* is equivalent to 1(INTEGER -8 7)*.
  1(SIGNED-BYTE *)* and plain 1SIGNED-BYTE* are the same as 1INTEGER*.

1(UNSIGNED-BYTE 2size*)*
  a nonnegative integer which fits into a byte of 2size* bits, with no sign bit.
  1(UNSIGNED-BYTE 3)* is equivalent to 1(INTEGER 0 7)*.
  1(UNSIGNED-BYTE *)* and plain 1UNSIGNED-BYTE* are the same as 1(INTEGER 0)*.

      1Simple Types for Arrays

ARRAY*			all arrays.
1SIMPLE-ARRAY*	arrays which are not displaced and have no fill pointers.
1VECTOR*		arrays of rank one.
1BIT-VECTOR*		1ART-1B* arrays of rank one.
1STRING*		1ART-STRING* and 1ART-FAT-STRING* arrays of rank one.
1SIMPLE-BIT-VECTOR*	  1(AND BIT-VECTOR SIMPLE-ARRAY)
SIMPLE-STRING*	1(AND STRING SIMPLE-ARRAY)
SIMPLE-VECTOR*	simple-arrays of rank one, whose elements are unrestricted
			(1ART-Q*, 1ART-Q-LIST*, etc).
			This is 2not* the same as 1(AND VECTOR SIMPLE-ARRAY)*!

      1Restriction Types for Arrays

(ARRAY 2element-type* 2dimensions*)*
  an array belongs to this type if
  1) it is capable of having as an element anything that matches 2element-type*
     and 4nothing else*.  If 2element-type* is 1(SIGNED-BYTE 4)*, the array must
     be an 1ART-4B* array; an 1ART-8B* or 1ART-Q* array whose elements happen to be
     less than 16. does not belong to 1(ARRAY (SIGNED-BYTE 4))*.

     If 2element-type* is 1T*, it specifies arrays which are capable of having
     any Lisp object as a component (such as 1ART-Q* arrays).

     If 2element-type* is 1**, the array type is not restricted.

  2) its dimensions match 2dimensions*.  2dimensions* can be an integer or a list.

     If it is an integer, it specifies the rank of the array.
     Then any array of that rank matches.

     If 2dimensions* is a list, its length specifies the rank,
     and each element of 2dimensions* restricts one dimension.
     If the element is an integer, that dimension's length must equal it.
     If the element is 1**, that dimension's length is not restricted.

     If 2dimensions* is 1** or omitted, the array's dimensions are not restricted.

1(SIMPLE-ARRAY 2element-type* 2dimensions*)*
  the restrictions work as in 1(ARRAY 2element-type* 2dimensions*)*, but in addition
  the array must be a simple array.

1(VECTOR 2element-type* 2size*)*
  2element-type* works as above.  The array must be a vector.
  2size* must be an integer or 1**;
  if it is an integer, the vector's length must equal 2size*.

1(BIT-VECTOR 2size*)
(SIMPLE-VECTOR 2size*)
(SIMPLE-BIT-VECTOR 2size*)
(STRING 2size*)
(SIMPLE-STRING 2size*)*
  The type of elements is implicitly specified; therefore, there is no point in
  having an 2element-type*.  2size* works as in 1VECTOR*.
1  *
      1Combination Type Specifiers

(SATISFIES 2predicate*)*
  an object belongs to this type if the function 2predicate* returns non-1NIL*
  with the object as its argument.  Thus, 1(SATISFIES NUMBERP)* is equivalent
  as a type specifier to 1NUMBER* (though the system could not tell that this is so).
  2predicate* must be a symbol, not a 1LAMBDA*-expression.

1(AND 2type-specs*...)*
  includes an object if it matches all the 2type-specs*.
  Thus, 1(AND INTEGER (SATISFIES ODDP))* is the type of odd integers.

1(OR 2type-specs*...)*
  includes all objects that match any of 2type-specs*.
  Thus, 1(OR NUMBER ARRAY)* includes all numbers and all arrays.

1(NOT 2type-spec*)*
  includes all objects that do not match 2type-spec*.

      1Defining New Type Specifiers

DEFTYPE* 2type-name* 2lambda-list* 2body*...
  defines 2type-name* as a type specifier by providing code to expand it into
  another type specifier -- a sort of type specifier macro.

  When a list starting with 2type-name* is encountered as a type specifier,
  the 2lambda-list* is matched against the cdr of the type specifier
  just as the lambda-list of a 1DEFMACRO* is matched against the cdr of a form.
  Then the 2body* is executed and should return a new type specifier
  to be used instead of the original one.

  If there are optional arguments in 2lambda-list* for which no default value
  is specified, they get 1** as a default value.

  If 2type-name* by itself is encountered as a type specifier, it is treated
  as if it were 1(2type-name*)*; that is to say, 2lambda-list* is matched against
  no arguments.

      1Type Predicates

INTEGERP* 2object*
  1T* if 2object* is an integer.  The Common Lisp name for 1FIXP*.

1CLI:LISTP* 2object*
  1T* if 2object* is a list, including 1NIL*.  Regular 1LISTP* is unchanged,
  and still returns 1NIL* if the arg is 1NIL*.

1CHARACTERP* 2object*
  1T* if 2object* is a character object.

1VECTORP* 2object*
  1T* if 2object* is an array of rank 1.

1BIT-VECTOR-P* 2object*
  1T* if 2object* is an 1ART-1B* array of rank 1.

1SIMPLE-VECTOR-P* 2object*
  1T* if 2object* is an array of rank 1, with no fill pointer and not displaced,
  which can have any Lisp object as an element.

1SIMPLE-BIT-VECTOR-P* 2object*
  1T* if 2object* is an 1ART-1B* array of rank 1, with no fill pointer and not displaced.

1SIMPLE-STRING-P* 2object*
  1T* if 2object* is a string with no fill pointer and not displaced.

1PACKAGEP* 2object*
  1T* if 2object* is a package.

1COMPILED-FUNCTION-P* 2object*
  1T* if 2object* is a compiled function.

1COMMONP* 2object*
  1T* if 2object* is of a type that Common Lisp defines operations on.
  See the definition of the type specifier 1COMMON*, above.

1SPECIAL-FORM-P* 2symbol*
  Non-1NIL* if 2symbol* is defined as a function that takes some unevaluated args.

1CHARACTERP*, 1PATHNAMEP*, 1HASH-TABLE-P*, 1RANDOM-STATE-P*, 1READTABLEP*,
and 1STREAMP* are also defined.

4Evaluation

1CLI:EVAL** 2form* &optional 2nohook*
  evaluates 2form*, Common Lisp style, in an empty lexical environment
  but using the current bindings of special variables.
  If 2nohook* is non-1NIL*, the current 1*EVALHOOK** if any is not invoked.

1EVAL* 2form* &optional 2nohook*
  evaluates 2form* in the traditional manner (all variables special).
  If 2nohook* is non-1NIL*, the current 1*EVALHOOK** if any is not invoked.

1SI:INTERPRETER-ENVIRONMENT*					Variable
1SI:INTERPRETER-FUNCTION-ENVIRONMENT*				Variable
  These two variables hold the current lexical environment, and
  also serve as a flag to indicate if traditional nonlexical evaluation
  is being done.  They are both 1NIL* for Common Lisp evaluation
  in the global environment.  The first is 1NIL* and the second is 1T*
  for traditional nonlexical evaluation.

  To evaluate a form in a specified environment, use 1EVALHOOK*
  and specify 1NIL* for the evalhook and the applyhook.

1*EVALHOOK**								Variable
  the Common Lisp name for the variable 1EVALHOOK*.
  If this variable is non-1NIL*, any kind of 1EVAL* will call it as a function
  instead of doing the usual work of 1EVAL*.

  The arguments passed to the hook function, if it is non-1NIL*, are:
   1) the form that was to be evaluated
   2) an object representing the environment of this application,
    suitable for passing as the 2environment* argument to 1*EVAL*,
    1EVALHOOK* or 1APPLYHOOK*.
  which is everything the hook function needs to continue with the
  evaluation by calling 1EVALHOOK*.

1*APPLYHOOK**								Variable
  the Common Lisp name for the variable 1APPLYHOOK*.
  If this variable is non-1NIL*, any kind of 1EVAL* will call it as a function
  instead of applying a function found in the car of a form to its arguments.

  The arguments passed to the 1APPLYHOOK* function, if it is non-1NIL*, are:
   1) the function that was to be applied
   2) the list of arguments that it was to be applied to
   3) an object representing the environment of this application,
    suitable for passing as the 2environment* argument to 1*EVAL*,
    1EVALHOOK* or 1APPLYHOOK*.
  which is everything the hook function needs to continue with the
  application by calling 1APPLYHOOK*.

1*EVALHOOK** and 1*APPLYHOOK** are bound to 1NIL* before either kind of hook
function is called.

1EVALHOOK* 2form* 2evalhook* 2applyhook* &optional 2environment*
  evaluates 2form* in the specified environment, with 2evalhook* and 2applyhook* in
  effect for all recursive evaluations of subforms of 2form*.  However, the
  2evalhook* is 2not* called for the evaluation of 2form* itself.

  2environment* is a list whose car is used for 1SI:INTERPRETER-ENVIRONMENT*
  and whose cadr is used for 1SI:INTERPRETER-FUNCTION-ENVIRONMENT*.

  If 2environment* is 1NIL*, it specifies Common Lisp evaluation in an
  empty lexical environment.  Traditional (all variables special)
  evaluation results from using 1(NIL . T)* for 2environment*.

  Aside from this, the only reasonable way to get a value to pass for
  2environment* is to use the argument passed to an evalhook or applyhook
  function, use the 1&ENVIRONMENT* argument of a macro, or do
  1(LIST SI:INTERPRETER-ENVIRONMENT SI:INTERPRETER-FUNCTION-ENVIRONMENT)*
  or the equivalent using 1WITH-STACK-LIST*.  Whichever way you obtain
  an environment object, you must take care not to use it after the
  context in which it was made is exited, because both 1SI:INTERPRETER-ENVIRONMENT*
  and 1SI:INTERPRETER-FUNCTION-ENVIRONMENT* normally contain stack lists
  which will become invalid then.

  2environment* has no effect on the evaluation of a variable which is regarded
  as special.  This is always done using 1SYMEVAL*.  However, 2environment*
  contains the record of the local 1SPECIAL* declarations currently in effect,
  so it does enter in the decision of 2whether* a variable is special.

1APPLYHOOK* 2function* 2list-of-args* 2evalhook* 2applyhook* &optional 2environment*
  applies 2function* to 2list-of-args* in the specified environment, with 2evalhook*
  and 2applyhook* in effect for all recursive evaluations of subforms of 2function*'s body.
  However, 2applyhook* is 2not* called for this application of 2function* itself.
  See the description of 1EVALHOOK*, above, for more information on 2environment*.

1LAMBDA-PARAMETERS-LIMIT*						Constant
  has as its value the number of parameters that a lambda-list may have.
  At least, if a lambda-list has fewer than this many parameters, that
  is guaranteed not to be too many.  More parameters may or may not work.

1CALL-ARGUMENTS-LIMIT*						Constant
  has as its value the number of arguments that can be dealt with
  in a function call.  At least, fewer than this many will never be more
  than the system can deal with.  More arguments may or may not work.

  Note that if you pass a list of arguments with 1APPLY* to a function
  that takes a rest arguments, there is no limit on the number of elements
  this list may have (except the size of memory).

4Declarations*

When you are running Common Lisp code, the interpreter pays attention to
1SPECIAL* declarations just as the compiler does.  Variables that are not declared
special may be used only in accordance with lexical scoping.  The interpreter is
now totally compatible with the compiler in this regard.

This change has not been made for all programs because I hear that there are
users who do not compile their programs and assume that all variables are
special.  I strongly urge all users to start compiling their programs.  At some
time in the future this new interpreter will become the default.

1PROCLAIM* &rest 2declarations*
  This is a Common Lisp replacement for a 1DECLARE* at top level.
  In Common Lisp, 1DECLARE* is used only for local declarations.
  1PROCLAIM* differs from traditional top-level 1DECLARE* in evaluating
  its arguments; thus, you would say 1(PROCLAIM '(SPECIAL X))*
  instead of 1(DECLARE (SPECIAL X))*.

  Note that top-level 1SPECIAL* declarations such as that example are no
  longer a recommended way of doing things (use 1DEFVAR*, 1DEFCONSTANT*
  or 1DEFPARAMETER*), and the Lisp machine does not pay attention to
  type declarations, so 1PROCLAIM* is pretty much useless on the Lisp
  machine.

1THE* &quote 2type-specifier* &eval 2value*
  is effectively the same as 2value*.  It declares that 2value* is an object of
  the specified type.  Since the Lisp machine does all type checking
  at run time, this has no effect.  It has not been set up to do an error
  check at run time because its whole purpose is to allow open compilation
  on machines lacking fast microcode type checking.

1DEFPARAMETER* 2name* 2initial-value* [2documentation*]		Macro
  is the Common Lisp name for 1DEFCONST*.

1DEFCONSTANT* 2name* 2value* [2documentation*]				Macro
  is similar to 1DEFPARAMETER* but more powerful; it defines a compile-time
  constant.  The compiler is allowed to substitute the value of 2name*
  into functions that refer to 2name*, making the assumption that 2name*'s
  value will never change.  You will usually get a warning if you try
  to 1SETQ* or bind such a symbol.

4Control Constructs

1CLI:CATCH** is a synonym for 1*CATCH*.  1CATCH* is still the Maclisp function,
for compatibility with old programs.  Is this still necessary?
1CLI:THROW* is a synonym for 1*THROW*.  1THROW* is still the Maclisp function.

1APPLY* is now generalized to be identical to 1LEXPR-FUNCALL*.
The latter is therefore somewhat obselete.

1FLET* 2local-functions* 2body*...						Special Form
  executes 2body* with 2local-functions* available as function definitions.

  Each element of 2local-functions* should look like
    1(2name* 2lambda-list* 2function-body*...)*
  just like the cdr of a 1DEFUN* form; 2name* is defined as a local function.
  Within the 2body*, using 2name* as a function name (using 2name* as the
  car of a form, or using 1(FUNCTION 2name*)*) will access the local definition.

  Local functions are like local variables; they are visible only in the
  lexical scope of the 1FLET*.

  Each local function is closed in the environment outside the 1FLET*.
  As a result, the local functions cannot see each other.

1LABELS* 2local-functions* 2body*...					Special Form
  is like 1FLET* except that the local functions can call each other.
  They are closed in the environment 2inside* the 1MACROLET*, so all
  the local function names are accessible inside the bodies of the
  local functions.

1MACROLET* 2local-macros* 2body*...					Special Form
  is like 1FLET* except that the locally function definitions are actually macros.
  2local-macros* looks like 2local-functions*, but each element is understood
  as the cdr of a 1DEFMACRO* rather than the cdr of a 1DEFUN*.  If a local macro
  name appears as the car of a form lexically within the 2body*, it will expand
  according to the local definition in the 1MACROLET*.

1CASE*
  is a synonym for 1SELECTQ*.

1TYPECASE* 2object* 2clauses*						Macro
  picks a clause to execute by testing the type of 2object* (with 1TYPEP*).
  Each clause begins with a type specifier (which is not evaluated).
  In execution, the clauses are tested one by one by testing the value
  of 2object* against that type specifier.  As soon as a type specifier matches,
  that clause's body is executed and its values are the values of the 1TYPECASE*.

  A clause is allowed to start with 1OTHERWISE* instead of a type specifier.
  Then it matches any object.  A clause starting with 1T* also does that, since
  1T* is a type specifier which all objects fit.

1LOOP* 2forms*...								Macro
  in Common Lisp is equivalent to 1(DO-FOREVER 2forms*...)*.
  This presents a problem because it is incompatible with the 1LOOP*
  that has been installed traditionally.  The two ways of using 1LOOP* are
  distinguished by looking at the second element of the list; in the
  traditional 1LOOP* macro, it must be a symbol.  So if it is a symbol
  the 1LOOP* is assumed to be the traditional kind.  Otherwise, it is
  treated as a Common Lisp 1LOOP*.
					  
1MAPL*
  is Common Lisp's name for 1MAP*.  1MAP* in Common Lisp means something else,
  but 1MAP* in non-Common-Lisp programs has not been changed.

4Multiple Values

1MULTIPLE-VALUES-LIMIT**						Constant
  smallest number of values that might possibly fail to work.
  Returning a number of values less than this many cannot possibly
  run into trouble with an implementation limit on number of values returned.

1MULTIPLE-VALUE-SETQ*
  is Common Lisp's name for 1MULTIPLE-VALUE*.  1MULTIPLE-VALUE* still works.

1MULTIPLE-VALUE-CALL* 2function* 2argforms*...				Special Form
  evaluates the 2argforms*, saving all of their values, and then calls
  2function* with all those values as arguments.  This differs from
    1(FUNCALL 2function* 2argforms*)*
  because that would get only one argument for 2function* from each
  2argform*, whereas 1MULTIPLE-VALUE-CALL* will get as many args from each
  2argform* as the 2argform* cares to return.  This works by consing.

1MULTIPLE-VALUE-PROG1* 2form* 2forms*...				Special Form
  evaluates 2form*, saves its values, evaluates the 2forms*, discards their values,
  then returns whatever values 2form* produced.
  This does not cons.

4Macros*

A macro's expander function now receives two arguments.  The first one
is the macro call being expanded, as before.  The second one, which is
new, is the 2environment* argument passed to 1MACROEXPAND-1*.  An
environment records many things, but the one which is relevant to
expanding macros is the set of local macro definitions (made with
1MACROLET*) currently in effect.

For compatibility, a macro expander function is allowed to demand
only one argument.  Then the environment is not passed.  Macro
definitions compiled before system 98.6 will in fact accept only
one argument.

1MACROEXPAND-1* 2form* &optional 2environment*
  Expands 2form* if it is a macro call (or a call to a 1DEFSUBST* function).
  2environment* is used only to supply the set of local 1MACROLET* macro
  definitions in effect for this form.

1MACROEXPAND* 2form* &optional 2environment*
  Expands 2form* if it is a macro call, then expands the result, and so on,
  until a form which is not a macro call is obtained; that form is returned.

1DEFMACRO* now allows additional lambda list keywords 1&WHOLE* and 1&ENVIRONMENT*.

1&WHOLE* is used, followed by a variable name, at the front of the argument list.
That variable is bound to the entire macro call being expanded.
Additional arguments to be bound as usual to parts of the macro call
may follow the 1&WHOLE* argument.

1&ENVIRONMENT* is used, followed by a variable name, anywhere in the argument
list.  That variable is bound to the environment object passed as the second
argument to the expander function.

1DEFMACRO* allows the macro name to be any function spec.  Normally only
symbols are used, since the interpreter and compiler provide no standard
way to look anywhere else for a macro definition.  However, it can
sometimes be useful to 1DEFMACRO* a 1:PROPERTY* function spec, if some part
of the system is going to look on a property for a macro definition.  For
example, this is how you define how to do 1LOCF* on some kind of form
(see below).

1*MACROEXPAND-HOOK**						Variable
  holds a function used by 1MACROEXPAND* to apply a macro's expander function
  to its argument, the macro call.  The default value of this variable is 1FUNCALL*.
  However, when the interpreter invokes macroexpansion, it will instead
  be another function that clobbers the original call so it looks like the
  expansion; this is called displacing the macro call and is used to speed
  up execution.

1MACRO-FUNCTION* 2function-spec*
  If 2function-spec* is defined as a macro, then this returns its expander-function:
  the function which should be called, with a macro call as its sole argument,
  to produce the macro expansion.  Otherwise, 1MACRO-FUNCTION* returns 1NIL*.

  In fact, a definition as a macro looks like 1(MACRO . 2expander-function*)*.

  You can define 2function-spec* as a macro with expander function 2expander*
  by doing 1(SETF (MACRO-FUNCTION 2function-spec*) 2expander*)*.  This is equivalent
  to 1(FSET 2function-spec* (CONS 'MACRO 2expander*))*.

3SETF4 And Related Things**

It used to be the case that 1SETF* could accidentally evaluate something twice.
For example, 1(SETF (LDB %%FOO (BAR X)) 5)* would evaluate 1(BAR X)* twice.
Macros based on 1SETF*, such as 1INCF* and 1PUSH*, were even more likely to do so;
1(PUSH X (CAR (FOO)))* would evaluate 1(FOO)* twice.  Now this 2never* happens.

1SETF* now accepts any number of places and values, just as 1SETQ* does.

1PSETF* 2place* 2value* 2place* 2value*...
  stores each 2value* into the corresponding 2place*, with the changes taking
  effect in parallel.  The subforms of the 2place*s, and the 2value*s, are evaluated
  in order; thus, in 
1    (PSETF (AREF A (TYI)) (TYI) (AREF B (TYI)) (AREF A (TYI)))*
  the first input character indexes 1A*, the second is stored, the third indexes 1B*,
  and the fourth indexes 1A*.  The parallel nature of 1PSETF* implies that,
  should the first and fourth characters be equal, the old value of that
  element of 1A* is what is stored into the array 1B*, rather than the new
  value which comes from the second character read.

1SHIFTF* 2place*... 
  sets the first 2place* from the second, the second from the third, and so on.
  The last 2place* is not set, so it doesn't really need to be a 1SETF*'able place;
  it can be any form.
  The value of the 1SHIFTF* form is the old value of the first 2place*.

1ROTATEF* 2place*...
  sets the first 2place* from the second, the second from the third, and so on,
  and sets the last 2place* from the old value of the first 2place*.
  Thus, the values of the 2place*s are permuted among the 2place*s
  in a cyclic fashion.


To define how to 1SETF* a function, you now use 1DEFSETF*.  There are two ways
to do this, the trivial way and the complicated way.

1DEFSETF* 2function* 2setting-function*
  says that the way to store into 1(2function* 2args*...)* is to do
  1(2setting-function* 2args*... 2new-value*)*.  For example, the 1DEFSETF* for 1CAR* 
  looks like 1(DEFSETF CAR SYS:SETCAR)*, so that 1(SETF (CAR X) Y)*
  expands into 1(SETCAR X Y)*.  (1SETCAR* is like 1RPLACA* except that
  1SETCAR* returns its second argument).

1DEFSETF* 2function* 1(2function-args**...1)* (2value-arg*) 2body*...
  says how to store into 1(2function* 2args*...)* by providing something like
  a macro defininition to expand into code to do the storing.
  2body* computes the code; the last form in 2body* returns a suitable expression.
  2function-args* should be a lambda list, which can have optional and rest args.
  2body* can substitute the values of the variables in this lambda list, to refer
  to the arguments in the form being 1SETF*'d.  Likewise, it can substitute
  in 2value-arg* to refer to the value to be stored.

  In fact, the 2function-args* and 2value-arg* will not actually be the subforms
  of the form being 1SETF*'d and the value to be stored; they will be gensyms.
  After 2body* returns, the corresponding expressions may be substituted for
  the gensyms, or the gensyms may remain as local variables with a suitable
  1LET* provided to bind them.  This is how 1SETF* ensures a correct order of
  evaluation.

1DEFINE-SETF-METHOD* 2function* 1(2function-args**...1)* (2value-arg*) 2body*...
  defines how to do 1SETF* on places starting with 2function*, with more power
  and generality than 1DEFSETF* provides, but more complicated to use.

  The 2function-args* will be the actual subforms of the place to be 1SETF*'d,
  and the full power of 1DEFMACRO* arglists can be used to match against it.
  2value-arg* will be the actual form used as the second argument to 1SETF*.

  2body* is executed, and it must return five values which describe how to
  do 1SETF* on this place.  It must identify all the subforms of the place
  which need to be evaluated (generally the 2function-args* arglist is arranged
  to make each arg get one subform) and a temporary variable should be
  made for each one by calling 1GENSYM*.  Another temporary variable should
  be made to correspond to the value to be stored, again by calling 1GENSYM*.
  Then the five values to be returned are:
   A list of the temporary variables for the subforms of the place.
   A list of the subforms that they correspond to.
   A list of the temporary variables for the values to be stored.
     Currently there can only be one value to be stored, so there is
     only one variable in this list, always.
   A form to do the storing.  In this form, only the temporary variables
     should appear, none of the parts of the original 1SETF*.
   A form to get the value of the place (in case this is 1PUSH* or 1INCF*
     rather than 1SETF*, and will need to examine the old value).
     This too should contain only the temporary variables.
  This information is everything that the macro (1SETF* or something
  more complicated) needs to know to decide what to do.
  Example:
1    (DEFINE-SETF-METHOD CAR (FUNCTION-SPEC)*
	1(LET ((TEMPVARS (LIST (GENSYM)))*
		1(TEMPARGS (LIST (LIST-FORM)))*
		1(STOREVAR (GENSYM)))*
	1  (VALUES TEMPVARS TEMPARGS (LIST STOREVAR)*
		1    `(SI:SETCAR ,(FIRST TEMPVARS) ,STOREVAR)*
		1    `(CAR ,(FIRST TEMPVARS))))))*
  is a definition of how to 1SETF* 1(CAR 2list*)* which is equivalent to the simple
  1(DEFSETF CAR SI:SETCAR)* which is actually used.

Here it is appropriate to say that the way to define how to do 1LOCF* on a function
has been changed.  1LOCF* properties are not used.  Instead, you define a
1SI:LOCF-METHOD* property, which should be either
   a symbol.  Then that symbol should be the function to use to compute the
      locative.  For example, 1(DEFPROP AREF ALOC SI:LOCF-METHOD).*
   a macro definition, 1(MACRO . 2expander-function*).*  The macro definition
      is expanded, with the form to be 1LOCF*'d as its argument, and should
      return a form to compute the locative.  For example,
      1(DEFMACRO (:PROPERTY AREF SI:LOCF-METHOD) (ARRAY &REST INDICES)
      `(ALOC ,ARRAY . ,INDICES))*
      would be equivalent to the simpler 1DEFPROP* shown above.

1GET-SETF-METHOD* 2form*
  invokes the 1SETF* method for 2form* (which must be a list) and returns the
  five values produced by the body of the 1DEFINE-SETF-METHOD* for the symbol
  which is the car of 2form*.  The meanings of these five values are given
  immediately above.  If the way to 1SETF* that symbol was defined with 1DEFSETF*,
  you still get five values, which you can interpret in the same ways; thus,
  1DEFSETF* is effectively an abbreviation for a suitable 1DEFINE-SETF-METHOD*.

  There are two ways to use 1GET-SETF-METHOD*:
    in a 1DEFINE-SETF-METHOD* for something like 1LDB*, which is 1SETF*'d by
     setting one of its arguments.  You would append your new tempvars and
     tempargs to the ones you got from 1GET-SETF-METHOD* to get the combined
     lists which you return.  The forms returned by the 1GET-SETF-METHOD* you
     would stick into the forms you return.
    in a macro which, like 1SETF* or 1INCF* or 1PUSH*, wants to store into a place.

  An example of a 1DEFINE-SETF-METHOD* that uses 1GET-SETF-METHOD* is that for 1LDB*:

1    (DEFINE-SETF-METHOD LDB (BYTESPEC INT)
      (MULTIPLE-VALUE-BIND (TEMPS VALS STORES STORE-FORM ACCESS-FORM)
          (GET-SETF-METHOD INT)
        (LET ((BTEMP (GENSYM))
              (STORE (GENSYM))
              (ITEMP (FIRST STORES)))
          (VALUES (CONS BTEMP TEMPS)
                  (CONS BYTESPEC VALS)
                  (LIST STORE)
                  `(PROGN 
                     ,(SUBLIS (LIST (CONS ITEMP `(DPB ,STORE ,BTEMP ,ACCESS-FORM)))
                              STORE-FORM)
                     ,STORE)
                  `(LDB ,BTEMP ,ACCESS-FORM)))))*

  An example of a macro which uses 1GET-SETF-METHOD* is 1PUSHNEW*.
  (The real 1PUSHNEW* is a little hairier to handle the 2test*, 2test-not* and 2key* arguments).

1    (DEFMACRO PUSHNEW (VALUE PLACE)
      "Add ITEM to the front of the list PLACE, if it's not already MEMQ there."
      (MULTIPLE-VALUE-BIND (TEMPVARS TEMPARGS STOREVARS STOREFORM REFFORM)
          (GET-SETF-METHOD PLACE)
        (SI:SUBLIS-EVAL-ONCE (CONS `(-VAL- . ,VALUE) (PAIRLIS TEMPVARS TEMPARGS))
                             `(IF (MEMQ -VAL- ,REFFORM)*
					1    ,REFFORM
                                ,(SUBLIS (LIST (CONS (CAR STOREVARS)
                                                     `(CONS -VAL- ,REFFORM)))
                                         STOREFORM))
                             T T)))

SI:SUBLIS-EVAL-ONCE* 2alist* 2form* &optional 2reuse-tempvars* 2sequential-flag*
  replaces temporary variables in 2form* with corresponding values,
  but generates local variables when necessary to make sure that the
  corresponding values are evaluated exactly once and in same order that they
  appear in 2alist*.  (This complication is skipped when the values are constant).
  The result is a form equivalent to
  1`(LET ,(MAPCAR #'(LAMBDA (ELT) (LIST (CAR ELT) (CDR ELT))) 2alist*)
    ,2form*)*
  but containing, usually, fewer temporary variables and faster to execute.

  If 2reuse-tempvars* is non-1NIL*, the temporary variables which appear as the cars
  of the elements of 2alist* are allowed to appear in the resulting form.
  Otherwise, none of them appears in the resulting form, and if any local
  variables turn out to be needed, they are made afresh with 1GENSYM*.
  2reuse-tempvars* should be used only when it is guaranteed that none of the
  temporary variables in 2alist* is referred to by any of the values to be substituted;
  as, when the temporary variables have been freshly made with 1GENSYM*.

  If 2sequential-flag* is non-1NIL*, then the value substituted for a temporary variable
  is allowed to refer to the temporary variables preceding it in 2alist*.  1SETF*
  and similar macros should all use this option.

1DEFINE-MODIFY-MACRO* 2macro-name* (2lambda-list*...) 2combiner-function* [2doc-string*]	Macro
  is a quick way to define 1SETF*'ing macros which resemble 1INCF*.
  For example, here is how 1INCF* is defined:
    1(DEFINE-MODIFY-MACRO INCF (&OPTIONAL (DELTA 1)) +*
      1 "Increment PLACE's value by DELTA.")*
  The 2lambda-list* describes any arguments the macro accepts, but 2not*
  first argument, which is always the place to be examined and modified.
  The old value of this place, and any additional arguments such as 1DELTA*,
  are combined using the 2combiner-function* (in this case, 1+*) to get the
  new value which is stored back in the place.

4Modules*

In Common Lisp, a 2module* is a name given to a group of files of code.
However, nothing in the Lisp system records what the "contents" of any
particular module may be.  Instead, one of the files which defines the module
will contain a 1PROVIDE* form which says, when that file is loaded, "module FOO
is now present."  Other files may say, using 1REQUIRE*, "I want to use module FOO."
Normally the 1REQUIRE* form also specifies the files to load if FOO has not been
1PROVIDE*d already.  If it does not, the module name FOO is used as a system name
in 1MAKE-SYSTEM* in order to load the module.

1PROVIDE* 2module-name*
  adds 2module-name* to the list 1*MODULES** of modules already loaded.
  2module-name* should be a string; case is significant.

1REQUIRE* 2module-name* &rest 2pathnames*
  if module 2module-name* is not already loaded (on 1*MODULES**), the files 2pathnames* are
  loaded in order to make the module available.
  2module-name* should be a string; case is significant.
  If 2pathnames* is 1NIL*, then 1(MAKE-SYSTEM 2module-name* :NOCONFIRM)* is done.
  Note, however, that case is 2not* significant in the argument to 1MAKE-SYSTEM*,
  and will not be made so!

1*MODULES**								Variable
  a list of module names 1PROVIDE*d so far.

4Numbers

1=** &rest 2numbers*
  1T* if all the arguments are numerically equal.  They need not be of the same type;
  1 and 1.0 are considered equal.  Character objects are also allowed, and in effect
  coerced to integers for comparison.

1* &rest 2numbers
1//=** &rest 2numbers*
  these two synonymous functions return 1T* if no two arguments are numerically equal.
  1* is an existing name, extended to more than two arguments; the name 1//=* is new.

1LCM* 2integer* &rest 2integers*
  returns the least common multiple of the specified integers.

1PI*									Constant
  is the value of 1*.

      1Division Functions

CLI://* 2number* &rest 2numbers*
  with one argument, takes the reciprocal of 2number*.  With more than one argument,
  divides 2number* by each of 2numbers*, one by one.  If an integer is divided by an
  integer, the result is a rational number, and is exactly correct.  This is how
  1CLI://* differs from ordinary 1//*, which would behave like 1TRUNCATE* in that case.

  1//* may be converted in the future to divide integers exactly, and be the same
  as 1CLI://* is now.

  Note that in Common Lisp syntax you would write just 1/* rather than 1//*.

1MOD* 2number* 2divisor*
  returns the root of 2number* modulo 2divisor*.  This is a number between 0
  and 2divisor*, or possibly 0, whose difference from 2number* is a multiple of 2divisor*.
  It is also the second value of 1(FLOOR 2number* 2divisor*)*.  Examples:
    1(MOD 2 5)  =>  2*
    1(MOD -2 5)  =>  3
   (MOD -2 -5)  =>  -2
   (MOD 2 -5)  =>  -3

CLI:REM* 2number* 2divisor*
  is a synonym for 1\*.  It is the second value of 1(TRUCNATE 2number* 2divisor*)*;
  a kind of remainder whose sign is the same as that of 2number*.
  Only the absolute value of 2divisor* matters.

  The traditional 1REM* function is, of course, a function for removing elements
  from a list with copying.  That is why there is a separate 1CLI:REM* function
  for Common Lisp.

      1Floating Point Functions

FLOAT* 2number* &optional 2flonum*
  converts 2number* to a floating point number and returns it.

  If 2flonum* is specified, it specifies the float format to use--
  namely, the same format that 2flonum* is--and if 2number* is
  a float of a different format then it is converted.

  If 2flonum* is omitted specified, then 2number* is converted to
  a single-float, but if 2number* is already a floating point number
  it is returned unchanged.

1DECODE-FLOAT* 2flonum*
  returns three values which express, in a different fashion, the value of 2flonum*.
  The first value is a positive flonum of the same format having the same mantissa,
  but with an exponent chosen to make it between 1/2 and 1, less than 1.
  The second value is the exponent of 2flonum*: the power of 2 by which the first
  value needs to be scaled in order to get 2flonum* back.
  The third value expresses the sign of 2flonum*.  It is a flonum of the same format
  as 2flonum*, whose value is either 1 or -1.
  Example:
  1(DECODE-FLOAT 38.2)  =>  0.596875   6   1.0

INTEGER-DECODE-FLOAT* 2flonum*
  like 1DECODE-FLOAT* except that the first value is scaled so as to make it an integer,
  and the second value is modified by addition of a constant so that it goes with
  the first argument.
  1(INTEGER-DECODE-FLOAT 38.2)  =>  #O* 111431463146   -25.   1.0

SCALE-FLOAT* 2flonum* 2integer*
  multiples 2flonum* by 2 raised to the 2integer* power.  2flonum* can actually be
  an integer also; it is converted to a flonum and then scaled.
  1(SCALE-FLOAT 0.596875 6)  =>  38.2*
  (1SCALE-FLOAT #O11431463146 -25.)  =>  38.2

FLOAT-SIGN* 2float1* &optional 2float2*
  returns a flonum whose sign matches that of 2float1* and whose magnitude
  and format are those of 2float2*.  If 2float2* is omitted, 1.0 is used as the magnitude
  and 2float1*'s format is used.
  1(FLOAT-SIGN -1.0s0 35.3)  =>  -35.3*
  1(FLOAT-SIGN -1.0s0 35.3s0)  =>  -35.3s0

FLOAT-RADIX* 2flonum*
  returns the radix used for the exponent of in the format used for 2flonum*.
  On the Lisp machine, floating point exponents are always powers of 2,
  so 1FLOAT-RADIX* ignores its argument and always returns 2.

1FLOAT-DIGITS* 2flonum*
  returns the number of bits of mantissa in the floating point format which
  2flonum* is an example of.  It is 17. for small flonums and 321.* for regular size ones.

1FLOAT-PRECISION* 2flonum*
  returns the number of significant figures present in in the mantissa of 2flonum*.
  This is always the same as 1(FLOAT-DIGITS 2flonum*)* for normalized numbers,
  and on the Lisp machine all flonums are normalized, so the two functions are the same.

      1Bit-Hacking Functions

LOGIOR*, 1LOGAND*, 1LOGXOR* changed.
  These functions now allow zero arguments, and return an identity for the operation.
  For 1LOGIOR* or 1LOGXOR*, the identity is zero.  For 1LOGAND*, it is -1.

1LOGEQV* &rest 2integers*
  Combines the 2integers* together bitwise using the equivalence operation, which, for two
  arguments, is defined to result in 1 if the two argument bits are equal.
  This operation is asociative.  With no args, the value is -1, which is an identity
  for the equivalence operation.

1LOGNAND* 2integer1* 2integer2*
  returns the bitwise-NAND of the two arguments.  A bit of the result is 1
  if at least one of the corresponding argument bits is 0.
  Exactly two arguments are required because this operation is not associative.

1LOGNOR* 2integer1* 2integer2*
  returns the bitwise-NOR of the two arguments.  A bit of the result is 1
  if both of the corresponding argument bits are 0.
  Exactly two arguments are required because this operation is not associative.

1LOGORC1* 2integer1* 2integer2*
  returns the bitwise-OR of 2integer2* with the complement of 2integer1*.

1LOGORC2* 2integer1* 2integer2*
  returns the bitwise-OR of 2integer1* with the complement of 2integer2*.

1LOGANDC1* 2integer1* 2integer2*
  returns the bitwise-AND of 2integer2* with the complement of 2integer1*.

1LOGANDC2* 2integer1* 2integer2*
  returns the bitwise-AND of 2integer1* with the complement of 2integer2*.

1BOOLE-CLR*								Constant
  the 1BOOLE* opcode for the trivial operation that always returns zero.

1BOOLE-SET*								Constant
  the 1BOOLE* opcode for the trivial operation that always returns one.

1BOOLE-1*								Constant
  the 1BOOLE* opcode for the trivial operation that always returns the first argument.

1BOOLE-2*								Constant
  the 1BOOLE* opcode for the trivial operation that always returns the second argument.

1BOOLE-IOR*								Constant
1BOOLE-AND*								Constant
1BOOLE-XOR*								Constant
1BOOLE-EQV*								Constant
1BOOLE-NAND*								Constant
1BOOLE-NOR*								Constant
1BOOLE-ORC1*								Constant
1BOOLE-ORC2*								Constant
1BOOLE-ANDC1*								Constant
1BOOLE-ANDC2*								Constant
  the 1BOOLE* opcodes that correspond to the functions 1LOGIOR*, 1LOGAND*, etc.

1LOGTEST* 2integer1* 2integer2*
  1T* if 1(LOGAND 2integer1* 2integer2*)* is nonzero.
  This is a Common Lisp synonym for 1BIT-TEST*.

1LOGBITP* 2index* 2integer*
  1T* if the bit 2index* up from the least significant in 2integer* is a 1.
  This is equivalent to 1(LDB-TEST (BYTE 2index* 1) 2integer*)*.

1LOGCOUNT* 2integer*
  The number of 1 bits in 2integer*, if it is positive.
  The number of 0 bits in 2integer*, if it is negative.
  (There are infinitely many 1 bits in a negative integer.)
       1(LOGCOUNT #O15)  =>  3*
       1(LOGCOUNT #O-15)  =>  2

INTEGER-LENGTH 2integer**
  The minimum number of bits (aside from sign) needed to represent
  2integer* in two's complement.

      1Byte Functions

BYTE* 2size* 2position*
  returns a byte-spec which specifies the byte of 2size* bits, positioned
  to exclude the 2position* least significant bits.  This byte-spec can be
  passed as the first argument to 1LDB*, 1DPB*, 1%LOGLDB*, 1LOGDPB*,
  1MASK-FIELD*, 1%P-LDB*, 1%P-LDB-OFFSET*, and so on.

1BYTE-POSITION* 2byte-spec
1BYTE-SIZE** 2byte-spec*
  return, respectively, the size and the position of 2byte-spec*.
  It is always true that 1(BYTE (BYTE-SIZE 2byte-spec*) (BYTE-POSITION 2byte-spec*))*
  equals 2byte-spec*.

      1Random Numbers*

A 2random*-2state* is a structure whose contents specify the future actions
of the random number generator.  Each time you call the function 1RANDOM*,
it uses (and updates) one random-state.  Random-states print as
  1#S(RANDOM-STATE ...2more data*...)*
so that they can be read back in.

1RANDOM* &optional 2number* 2state*
  returns a randomly generated number.  If 2number* is specified,
  the random number is nonnegative and less than 2number*, and of the
  same type as 2number* (floating if 2number* is floating, etc.).

  According to Common Lisp, 2number* must always be specified.
  But you are still allowed to omit it, for the sake of compatibility.
  If 2number* is omitted, the result is a randomly chosen fixnum.

  2state* is a random-state object.  1RANDOM* uses that object to choose the
  number to return, and updates the object so a different number would be
  chosen next.

1RANDOM-STATE-P* 2object*
  1T* if 2object* is a random-state.

1*RANDOM-STATE**							Variable
  This random-state is used by default when 1RANDOM* is called (if you do
  not specify the 2state* argument).

1MAKE-RANDOM-STATE* &optional 2random-state*
  creates and returns a new random-state object.
  If 2random-state* is 1NIL*, the new random-state is a copy of 1*RANDOM-STATE**.
  If 2random-state* is a random-state, the new one is a copy of that one.
  If 2random-state* is 1T*, the new random-state is initialized truly randomly
  (based on the value of 1(TIME)*).

      1Machine Precision Information*

Common Lisp defines some constants whose values give information
in a standard way about the ranges of numbers representable in the
individual Lisp implementation.

1MOST-NEGATIVE-FIXNUM*						Constant
  Any integer smaller than this must be a bignum.

1MOST-POSITIVE-FIXNUM*						Constant
  Any integer larger than this must be a bignum.

1MOST-POSITIVE-SHORT-FLOAT*					Constant
  No short float can be greater than this number.

1LEAST-POSITIVE-SHORT-FLOAT*					Constant
  No positive short float can be closer to zero than this number.

1LEAST-NEGATIVE-SHORT-FLOAT*					Constant
  No negative short float can be closer to zero than this number.

1MOST-NEGATIVE-SHORT-FLOAT*					Constant
  No short float can be less than this (negative) number.

1MOST-POSITIVE-SINGLE-FLOAT*					Constant
1LEAST-POSITIVE-SINGLE-FLOAT*					Constant
1LEAST-NEGATIVE-SINGLE-FLOAT*					Constant
1MOST-NEGATIVE-SINGLE-FLOAT*					Constant
  Similar to the above, but for single-floats (ordinary flonums) rather than
  for short-floats (small-flonums).

1MOST-POSITIVE-DOUBLE-FLOAT*					Constant
1LEAST-POSITIVE-DOUBLE-FLOAT*					Constant
1LEAST-NEGATIVE-DOUBLE-FLOAT*					Constant
1MOST-NEGATIVE-DOUBLE-FLOAT*					Constant
1MOST-POSITIVE-LONG-FLOAT*					Constant
1LEAST-POSITIVE-LONG-FLOAT*					Constant
1LEAST-NEGATIVE-LONG-FLOAT*					Constant
1MOST-NEGATIVE-LONG-FLOAT*					Constant
  These are defined by Common Lisp to be similar to the above,
  but for double-floats and long-floats.  On the Lisp machine,
  there are no distinct double and long floating formats; they are
  synonyms for single-floats.  So these constants exist but their values
  are the same as those of 1MOST-POSITIVE-SINGLE-FLOAT* and so on.

1SHORT-FLOAT-EPSILON*						Constant
  Smallest positive short float which can be added to 11.0s0* and make a difference.

1SINGLE-FLOAT-EPSILON*						Constant
1DOUBLE-FLOAT-EPSILON*						Constant
1LONG-FLOAT-EPSILON*						Constant
  Smallest positive float which can be added to 11.0* and make a difference.
  The three names are synonyms on the Lisp machine,
  for reasons explained above.

1SHORT-FLOAT-NEGATIVE-EPSILON*					Constant
  Smallest positive short float which can be subtracted from 11.0s0*
  and make a difference.

1SINGLE-FLOAT-NEGATIVE-EPSILON*					Constant
1DOUBLE-FLOAT-NEGATIVE-EPSILON*					Constant
1LONG-FLOAT-NEGATIVE-EPSILON*					Constant
  Smallest positive float which can be subtracted from 11.0* and make a difference.

4Symbol and Plist Functions

1SYMBOL-PLIST** 2symbol*
  is a new name for 1PLIST*; it returns the contents of the property list of 2symbol*.
  1(SETF (SYMBOL-PLIST 2symbol*) 2newvalue*) can *be used to set the property list.

1SYMBOL-NAME* 2symbol*
  returns the pname string of 2symbol*.  This is a new name for 1GET-PNAME*.

1SYMBOL-VALUE* 2symbol*
  returns the value of 2symbol*.  This is a new name for 1SYMEVAL*.
  1(SETF (SYMBOL-VALUE 2symbol*) 2newvalue*)* is used to alter a symbol's value.
  There is no equivalent of the function 1SET* in Common Lisp; this 1SETF*
  construct is the only way to do it.

1SYMBOL-FUNCTION* 2symbol*
  returns the function definition of 2symbol*.  This is a new name for 1FSYMEVAL*.
  1(SETF (SYMBOL-FUNCTION 2symbol*) 2newvalue*)* is used to alter a symbol's function definition.
  There is no equivalent of the function 1FSET* in Common Lisp; this 1SETF*
  construct is the only way to do it.

1COPY-SYMBOL* 2symbol* &optional 2copy-props*
  makes a new uninterned symbol whose name is the same as that of 2symbol*.
  If 2copy-props* is non-1NIL*, the value, function definition and property list of
  2symbol* are copied as well.  This is a new name for 1COPYSYMBOL*.

1GENTEMP* &optional (2prefix* 1"T"*) (2a-package* 1PACKAGE*)
  creates and returns a new symbol whose name starts with 2prefix*,
  interned in 2a-package*, and distinct from any symbol already present there.
  This is done by trying names one by one until a name not already in use
  is found.

1GETF* 2place* 2property* &optional 2default*				Macro
  Equivalent to 1(GET (LOCF 2place*) 2property* 2default*)*, except that 1GETF* is
  allowed in Common Lisp, which does not have 1LOCF* or locatives of any kind.

1REMF* 2place* 2property*							Macro
  Equivalent to 1(REMPROP (LOCF 2place*) 2property* 2default*)*, except that 1REMF* is
  allowed in Common Lisp.

1GET-PROPERTIES* 2place* 2list-of-properties*				Macro
  The Common Lisp replacement for 1GETL*.  It is like
  1(GETL (LOCF 2place*) 2list-of-properties*)* except that it returns slightly
  different values.  Specifically, it searches the property list for a property
  name which is 1MEMQ* in 2list-of-properties*, then returns three values:
     the property name found
     the value of that property
     the cell (in the property list) whose car is the property name found.
  If nothing is found, all three values are 1NIL*.

1GENSYM*
  now allows the prefix you specify to be any string.  It used to have to
  be a single character.


4Character Functions and Related Constants*

Character objects were introduced in system version 97.
In the traditional syntax, the character object A appears as 1#/A*.
In Common Lisp syntax, it looks like 1#\A*.

Common Lisp programs typically work with actual character objects but
programs traditionally use integers to represent characters.  The new Common
Lisp functions for operating with characters have been implemented to accept
integers as well, so that they can be used equally well from traditional programs.

1CHARACTERP* 2object*
  1T* if 2object* is a character object.

      1Components of Character Objects*

Common Lisp says that each character object has a character code,
a font, and a bunch of bits.  Each of these things is an integer from
a fixed range.

However, you cannot necessarily take any valid code, any valid font, and any
valid bits and make a character out of them.  And if you can make a character
out of them, it cannot necessarily be stored in a string.

1CHAR-CODE* 2char*
  Returns the code of 2char*.  This is what used to be done with 1(LDB %%CH-CHAR 2char*)*.

1CHAR-FONT* 2char*
  Returns the font of 2char*, a number less that 1CHAR-FONT-LIMIT*.

1CHAR-BITS* 2char*
  Returns the bits of 2char*, a number less than 1CHAR-BITS-LIMIT*.

1CHAR-CODE-LIMIT*							Constant
  a constant whose value is a bound on the maximum code of any character.
  In the Lisp machine, currently, it is 400 (octal).

1CHAR-BITS-LIMIT*							Constant
  a constant whose value is a bound on the maximum bits value of any character.
  In the Lisp machine, currently, it is 40 (octal).

1CHAR-FONT-LIMIT*							Constant
  a constant whose value is a bound on the maximum font value of any character.
  In the Lisp machine, currently, it is 400 (octal).

The "bits" of a character are just the familiar Control, Meta, Super and Hyper bits,
plus one more (the Mouse bit).

1CHAR-CONTROL-BIT*							Constant
1CHAR-META-BIT*							Constant
1CHAR-SUPER-BIT*							Constant
1CHAR-HYPER-BIT*							Constant
  constants with value 1, 2, 4 and 8.
  These give the meanings of the bits within the bits-field of a character object.
  Thus, 1(BIT-TEST CHAR-META-BIT (CHAR-BITS 2char*))* would be non-1NIL*
  if 2char* is a meta-character.

1CHAR-BIT* 2char* 2name*
  1T* if 2char* has the bit named by 2name*.  2name* is a symbol, one of
  1:CONTROL*, 1:META*, 1:SUPER*, or 1:HYPER*.  Thus,
  1(CHAR-BIT #\META-X ':META)* is 1T.

SET-CHAR-BIT* 2char* 2name* 2newvalue*
  Returns a character like 2char* except that the bit specified by 2name*
  is present or absent according to 2newvalue* (which is 1T* or 1NIL*).  Thus,
  1(SET-CHAR-BIT #\X ':META T)* returns 1#\META-X*.

      1Classifying Characters

STRING-CHAR-P* 2char*
  1T* if 2char* is a character that can be stored in a string.  On the Lisp machine,
  this is if the bits and font of 2char* are zero.

1STANDARD-CHAR-P* 2char*
  1T* if 2char* is a 2standard* character, according to Common Lisp.  This is a character
  which belongs to the standard Common Lisp character set consisting of the
  95 ASCII printing characters (including 1Space*) and the 1Return* character.
  This 1(STANDARD-CHAR-P #\END)* is 1NIL*.

1GRAPHIC-CHAR-P* 2char*
  1T* if 2char* is a graphic character; one which has a printed shape.
  A, -, 1Space* and 1* are all graphic characters; 1Return*, 1End* and 1Abort* are not.
  A character whose bits are nonzero is never graphic.

  Ordinary output to windows prints graphic characters using the current font.
  Nongraphic characters are printed using lozenges unless they have a special
  formatting meaning (as 1Return* does).

  Common Lisp says that programs may assume that graphic characters of font 0
  may be assumed to be all of equal width.  Since the Lisp machine allows you
  to use any font at any time, this clearly cannot always be true.  Too bad.

1ALPHA-CHAR-P* 2char*
  1T* if 2char* is a letter, with zero bits.

1UPPER-CASE-P* 2char*
  1T* if 2char* is an upper case letter, with zero bits.

1LOWER-CASE-P* 2char*
  1T* if 2char* is an lower case letter, with zero bits.

1BOTH-CASE-P* 2char*
  1T* if 2char* is a character which has distinct upper and lower case forms--
  that is to say, a letter--with zero bits.

1DIGIT-CHAR-P* 2char* &optional (2radix* 110.*)
  If 2char* is a digit available in the specified radix, returns the "weight" of that digit.
  Otherwise, it returns 1NIL*.  If the bits of 2char* are nonzero, the value is 1NIL*.
  Thus, 1(DIGIT-CHAR-P #\8 8)* is 1NIL* but 1(DIGIT-CHAR-P #\8 *9) is 18*.
  Radices greater than ten use letters as additional digits, so
  1(DIGIT-CHAR-P #\F 16.)* is 115.*.

1ALPHANUMERICP* 2char*
  1T* if 2char* is a letter or a digit 0 through 9, with zero bits.

1CHAR=* 2char1* &rest 2chars
1CHAR//=** 2char1* &rest 2chars
1CHAR>** 2char1* &rest 2chars
1CHAR<** 2char1* &rest 2chars
1CHAR>=** 2char1* &rest 2chars
1CHAR<=** 2char1* &rest 2chars*
  These are the Common Lisp functions for comparing characters and 
  including the case, font and bits in the comparison.  On the Lisp machine
  they are synonyms for the numeric comparison functions 1=*, 1>*, etc.
  Note that in Common Lisp syntax you would write 1CHAR/=*, not 1CHAR//=*.

1CHAR-EQUAL* 2char1* &rest 2chars
1CHAR-NOT-EQUAL** 2char1* &rest 2chars
1CHAR-LESSP** 2char1* &rest 2chars
1CHAR-GREATERP** 2char1* &rest 2chars
1CHAR-NOT-LESSP** 2char1* &rest 2chars
1CHAR-NOT-GREATERP** 2char1* &rest 2chars*
  These are the Common Lisp functions for comparing characters, ignoring
  differences in case, font and bits.

      1Making Characters

CHARACTER* 2object*
  Coerces 2object* into a character and returns the character as a fixnum
  for traditional programs.

1CLI:CHARACTER* 2object*
  Coerces 2object* into a character and returns the character as a character object
  for Common Lisp programs.

1CODE-CHAR* 2code* &optional (2bits* 10*) (2font* 10*)
  Returns a character object made from 2code*, 2bits* and 2font*, IF that is possible.
  Not all combinations of valid 2code*, 2bits* and 2font* can go together.
  If the specified arguments do not go together, the value is 1NIL*.

1MAKE-CHAR* 2char* &optional (2bits* 10*) (2font* 10*)
  Like 1CODE-CHAR* except that the first argument
  is a character whose code is used, not an integer.
  In the Lisp machine, this and 1CODE-CHAR* are identical and either one will accept
  a character or a number.

1DIGIT-CHAR* 2weight* &optional (2radix* 110.*) (2font* 10*)
  Returns a character which is digit with the specified weight, and with
  font as specified.  However, if there is no suitable character which has
  weight 2weight* in the specified 2radix*, the value is 1NIL*.  If the "digit" is
  a letter (if 2weight* is > 9), it is upper case.

1CHAR-INT* 2char*
  Returns the integer whose pointer field matches 2char*.

1INT-CHAR* 2integer*
  Returns the character object whose pointer field matches 2integer*.

1CHAR-UPCASE* 2char
1CHAR-DOWNCASE** 2char*
  When given a character object, these functions now return a character object.
  When given an integer, they still return an integer.

      1Character Names

CHAR-NAME* 2char*
  Returns the standard name (or one of the standard names) of 2char*,
  or 1NIL* if there is none.  The name is returned as a string.
  1(CHAR-NAME #\SPACE)* is the string 1"SPACE"*.
  If 2char* has nonzero bits, the value is 1NIL*.   Names such as
  1Control-X* are not constructed by this function.

1NAME-CHAR* 2symbol*
  Returns the character for which 2symbol* is a name, as a character object,
  or returns 1NIL* if 2symbol* is not recorded as a character name.
  Compound names such as Control-X are not recognized.
  Strings are allowed as well as symbols.

4Hash Tables*

The base flavor for hash tables is now called 1HASH-TABLE* rather than 1EQ-HASH-TABLE*.
1EQL* hash tables now exist standardly as well as 1EQ* and 1EQUAL* hash tables.
There are these new functions:

1HASH-TABLE-P* 2object*
  1T* if 2object* is a hash table.  1(TYPEP 2object* 'HASH-TABLE)*.

1HASH-TABLE-COUNT* 2hash-table*
  The number of filled entries in 2hash-table*.

The function 1MAKE-HASH-TABLE* takes new arguments:
 the keyword argument 2rehash-threshold* may now be an integer,
  in which case it is the exact number of filled entries at which a rehash should be done.
  If so, it will be increased in proportion to the hash table size when teh rehash happens.
  The threshold can still be a flonum between zero and one, interpreted as a fraction
  of the total size.
 the new keyword argument 2test* can be used to specify the type of hashing.
  It must be 1EQ*, 1EQL* or 1EQUAL*.

4Lists

1TREE-EQUAL** 2x* 2y* &key 2test* 2test-not*
  compares two trees recursively to all levels.  Atoms must match under the
  function 2test* (which defaults to 1EQL*).  Conses must match recursively in
  both the car and the cdr.

  If 2test-not* is specified instead of 2test*, two atoms match if 2test-not* returns 1NIL*.

1ENDP* 2list*
  returns 1T* if 2list* is 1NIL*, 1NIL* if 2list* is a cons cell.  Gets an error if 2list* is not a list.
  This is the way Common Lisp recommends for terminating a loop which cdr's
  down a list.  However, Lisp machine system functions generally prefer to test
  for the end of the list with 1ATOM*; it is regarded as a feature that these functions
  do something useful for dotted lists.

1REST* 2list*
  is a synonym for 1CDR*.

1LIST-LENGTH* 2list*
  returns the length of 2list*, or 1NIL* if 2list* is circular.
  The function 1LENGTH* would loop forever if given a circular list.

1COPY-LIST*, 1COPY-ALIST*, 1COPY-TREE*
  Common Lisp names for 1COPYLIST*, 1COPYALIST*, 1COPYTREE*.

1REVAPPEND* 2list* 2tail*
  like 1(NCONC (REVERSE 2list*) 2tail*)*, but a little faster.

1BUTLAST* 2list* &optional (2n* 11*)
  returns a list like 2list* but missing the last 2n* elements.

1NBUTLAST* 2list* &optional (2n* 11*)
  modifies 2list* to remove the last 2n* elements, by changing a cdr pointer,
  and then returns 2list*.

1CLI:SUBST* 2new* 2old* 2tree* &key 2test* 2test-not* 2key*
  replaces with 2new* every atom or subtree in 2tree* which matches 2old*,
  returning a new tree.  List structure is copied as necessary
  to avoid clobbering parts of 2tree*.  This differs from the traditional
  1SUBST* function, which always copies the entire tree.

  2test* or 2test-not* is used to do the matching.  If 2test* is specified, a
  match happens when 2test* returns non-1NIL*; otherwise, if 2test-not*
  is specified, a match happens when it returns 1NIL*.  If neither is
  specified, then 1EQL* is used for 2test*.

  The first argument to the 2test* or 2test-not* function is always 2old*.
  The second argument is normally a leaf or subtree of 2tree*.
  However, if 2key* is non-1NIL*, then it is called with the subtree as argument,
  and the result of this is passed to the 2test* or 2test-not* function.

  Because 1(SUBST NIL NIL 2tree*)* is a widely used ideom for copying a tree,
  even though it is obsolete, it will be impractical to install
  new function as the standard 1SUBST* for a long time.

1NSUBST* 2new* 2old* 2tree* &key 2test* 2test-not* 2key*
  like 1CLI:SUBST* but modifies 2tree* itself and returns it.
  No new list structure is created.

1SUBST-IF* 2new* 2predicate* 2tree* &key 2key*
  replaces with 2new* every atom or subtree in 2tree* which satisfies 2predicate*.
  List structure is copied as necessary so that the original 2tree* is not modified.
  2key*, if non-1NIL*, is a function applied to each element to get the
  object to match against.  If 2key* is 1NIL*, the element itself is used.

1SUBST-IF-NOT* 2new* 2predicate* 2tree* &key 2key*
  similar but replaces tree nodes which do not satisfy 2predicate*.

1NSUBST-IF*, 1NSUBST-IF-NOT*
  just like 1SUBST-IF* and 1SUBST-IF-NOT* except that they modify 2tree* itself
  and return it.

1SUBLIS* 2alist* 2tree* &key 2test* 2test-not* 2key*
  performs multiple parallel replacements on 2tree*, returning a new tree.
  2tree* itself is not modified because list structure is copied as necessary.
  Each element of 2alist* specifies one replacement; the car is what to
  look for, and the cdr is what to replace it with.

  2test* or 2test-not* is used to do the matching.  If 2test* is specified, a
  match happens when 2test* returns non-1NIL*; otherwise, if 2test-not*
  is specified, a match happens when it returns 1NIL*.  If neither is
  specified, then 1EQL* is used for 2test*.

  The first argument to 2test* or 2test-not* is the car of an element of 2alist*.
  The second argument is normally a leaf or subtree of 2tree*.
  However, if 2key* is non-1NIL*, then it is called with the subtree as argument,
  and the result of this is passed to the 2test* or 2test-not* function.

1NSUBLIS* 2alist* 2tree* &key 2test* 2test-not* 2key*
  is like 1SUBLIS* but modifies 2tree* and returns it.

1CLI:MEMBER* 2item* 2list* &key 2test* 2test-not* 2key*
  searches the elements of 2list* for one which matches 2item*, then returns
  the tail of 2list* whose car is that element.  If no match is found, 1NIL* is returned.

  2test*, 2test-not* and 2key* are used in matching the elements, just as described
  under 1CLI:SUBST*.  If neither 2test* nor 2test-not* is specified, the default is
  to compare with 1EQL*.  For this reason, 1CLI:MEMBER* is thoroughly incompatible 
  with traditional 1MEMBER*, which uses 1EQUAL* for the comparison.
  This is a real shame.  I tried my best to fight against it.

1MEMBER-IF* 2predicate* 2list* &key 2key*
  searches the elements of 2list* for one which satisfies 2predicate*.
  If one is found, the value is the tail of 2list* whose car is that element.
  Otherwise the value is 1NIL*.

  If 2key* is non-1NIL*, then predicate is applied to 1(FUNCALL 2key* 2element*)*
  rather than to the element itself.

1MEMBER-IF-NOT* 2predicate* 2list* &key 2key*
  searches for an element which does not satisfy 2predicate*.
  Otherwise like 1MEMBER-IF*.

1ADJOIN* 2item* 2list* &key 2test* 2test-not* 2key*
  returns a list like 2list* but with 2item* as an additional element
  if no existing element matches 2item*.  It is done like this:
1    (IF (MEMBER 2item* 2list* 2other-args*...)
        2list**
	1(CONS 2item* 2list*))

PUSHNEW* 2item* 2list-place* &key 2test* 2test-not* 2key*
  1PUSH*es 2item* onto 2list-place* unless 2item* matches an existing element
  of the value stored in that place.  Much like
1    (SETF 2list-place* (ADJOIN 2item* 2list-place* 2keyword-args*...))*
  except for order of evaluation.

1CLI:UNION* 2list1* 2list2* &key 2test* 2test-not* 2key*
  returns a list which has all the elements of 2list1* and all the elements of 2list2*.
  If 2list1* and 2list2* have elements in common, these elements need appear
  only once in the resulting list.  Elements are compared for this purpose
  using the 2test* function or the 2test-not* function, or using 1EQL* if neither
  arguemnt was specified.

  If 2key* is non-1NIL*, then 2key* is applied to each of the elements to be compared
  to get a key which is then passed to 2test* or 2test-not*.  Thus, you can say
  that elements are duplicates if their cars are 1EQL* by using 1CAR* as 2key*.

  If there are duplicate elements within 2list1* itself, or within 2list2*, then there
  may be duplicate elements in the result.  Elements of each list are matched
  against elements of the other, but not against other elements of the same list.

1CLI:NUNION* 2list1* 2list2* &key 2test* 2test-not* 2key*
  like 1UNION* but modifies 2list1*, 2list2* or both to get the cells to make the
  list that is returned.

1CLI:INTERSECTION* 2list1* 2list2* &key 2test* 2test-not* 2key*
  returns a list which has all the elements of 2list1* which match some
  element of 2list2*.  2test*, 2test-not* and 2key* are used in comparing elements
  just as they are used in 1UNION*.  If 2list1* contains duplicate elements, the
  duplicates can both appear in the result, as elements of 2list1* are not
  compared against other elements of 2list1*.

1CLI:NINTERSECTION* 2list1* 2list2* &key 2test* 2test-not* 2key*
  like 1INTERSECTION* but destructively modifies 2list1* to produce the value.

1SET-DIFFERENCE* 2list1* 2list2* &key 2test* 2test-not* 2key*
  returns a list which has all the elements of 2list1* which 2do not* match any
  element of 2list2*.  2test*, 2test-not* and 2key* are used in comparing elements
  just as they are used in 1UNION*.  If 2list1* contains duplicate elements, the
  duplicates can both appear in the result, as elements of 2list1* are not
  compared against other elements of 2list1*.

1NSET-DIFFERENCE* 2list1* 2list2* &key 2test* 2test-not* 2key*
  like 1SET-DIFFERENCE* but destructively modifies 2list1* to produce the value.

1SET-EXCLUSIVE-OR* 2list1* 2list2* &key 2test* 2test-not* 2key*
  returns a list which has all the elements of 2list1* which 2do not* match any
  element of 2list2*, and also all the elements of 2list2* which do not match
  any element of 2list1*.  2test*, 2test-not* and 2key* are used in comparing elements
  just as they are used in 1UNION*.  If either list contains duplicate elements, the
  duplicates can both appear in the result, as comparisons are done only
  between an element of 2list1* and an element of 2list2*.

1NSET-EXCLUSIVE-OR* 2list1* 2list2* &key 2test* 2test-not* 2key*
  like 1SET-EXCLUSIVE-OR* but may destructively modify both 2list1* and 2list2*
  to produce the value.

1SUBSETP* 2list1* 2list2* &key 2test* 2test-not* 2key*
  1T* if every element of 2list1* matches some element of 2list2*.
  2test*, 2test-not* and 2key* are used in comparing elements.

1PAIRLIS* 2cars* 2cdrs* &optional 2tail*
  returns 1(NCONC (MAPCAR 'CONS 2cars* 2cdrs*) 2tail*)*.

1ACONS* 2acar* 2acdr* 2tail*
  returns 1(CONS (CONS 2acar* 2acdr*) 2tail*)*.

1CLI:ASSOC* 2item* 2alist* &key 2test* 2test-not*
  returns the first element of 2alist* whose car matches 2item*,
  or 1NIL* if there is no such element.  Elements which are 1NIL*
  are ignored; they do not result in comparing 2item* with 1NIL*.

  2test* and 2test-not* are used in comparing elements.

  This differs from the traditional function 1ASSOC* in that
  by default it uses 1EQL* rather than 1EQUAL* for the comparison.

1CLI:RASSOC* 2item* 2alist* &key 2test* 2test-not*
  like 1CLI:ASSOC* but compares against the cdr of each element rather
  than the car.

1ASSOC-IF* 2predicate* 2alist*
  returns the first element of 2alist* whose car satisfies 2predicate*,
  or 1NIL* if there is no such element.  Elements which are 1NIL*
  are ignored; they do not result in applying 2predicate* to 1NIL*.

1ASSOC-IF-NOT* 2predicate* 2alist*
  returns the first element of 2alist* whose car 2does not* satisfy 2predicate*,
  or 1NIL* if there is no such element.  Elements which are 1NIL*
  are ignored; they do not result in applying 2predicate* to 1NIL*.

1RASSOC-IF* 2predicate* 2alist
1RASSOC-IF-NOT** 2predicate* 2alist*
  like 1RASSOC-IF* and 1RASSOC-IF-NOT* but test the cdr of each element
  rather than the car.

1MAKE-LIST*
  this function now takes a keyword argument 2initial-element* which specifies
  the value to store in each word of the newly made list.  The old name for
  this argument was 2initial-value*.  Both names are accepted.

4Arrays*

Traditionally the elements of a string are fixnums which represent characters.
According to Common Lisp, the elements of a string are character objects.
Therefore, a different version of 1AREF* has been provided for Common Lisp
programs which returns a character object when the first argument is a string.
It is called 1CLI:AREF*.  It behaves just like 1AREF* on arrays other than strings.

An array which allowes arbitrary elements is called a 2general* array.  An array
whose elements are restricted to a certain type is a 2specialized* array.  The only
specialized arrays in the Lisp machine system are strings, whose elements are
characters, and numeric arrays, whose elements are restricted to be numbers (of
particular types).

An array of rank one is called a 2vector*.  There are many new functions,
called the 2generic sequence functions*, which work equally well on vectors and on lists.

1VECTORP* 2object*
  1T* if 2object* is a vector.

1BIT-VECTOR-P* 2object*
  1T* if 2object* is a bit vector, an array of type 1ART-1B* and rank one.

1SIMPLE-VECTOR-P* 2object*
  1T* if 2object* is a simple general vector; a rank-1 array which is not displaced and
  has no fill pointer, and whose elements may be any Lisp object.

1SIMPLE-BIT-VECTOR-P* 2object*
  1T* if 2object* is a simple bit vector; a rank-1 1ART-1B* array which is not displaced and
  has no fill pointer.

1SIMPLE-STRING-P* 2object*
  1T* if 2object* is a simple string; a rank-1 1ART-STRING* or 1ART-FAT-STRING* array
  which is not displaced and has no fill pointer.

1MAKE-ARRAY* takes three new keyword arguments:
 2initial-element* specifies a value to initialize each array element to.
  It is equivalent to the 2initial-value* argument, which is still accepted.

 2element-type* is a new way to specify the array type.
  Its value is a Common Lisp type specifier.
  The array type used is the most specialized which can allow as an element
  anything which fits the type specifier.  For example,
  if 2element-type* is 1(MOD 4)*, you will get an 1ART-2B* array.
  Specifying 2element-type* is an alternative to specifying 2type*.

 2initial-contents* specifies the entire contents for the new array,
  as a list of lists of lists...  Array element 1 3 4 of a three-dimensional
  array would be 1(NTH 4 (NTH 3 (NTH 1 2initial-contents*))).

 2adjustable-p** is another argument which is allowed for compatibility
  with other Common Lisp implementations.  A non-1NIL* 2adjustable-p*n
  says that the array should be made so that its size can be changed later.
  On the Lisp machine any array's size can be changed.

1ARRAY-RANK-LIMIT*							Constant
  a constant giving the limit on the rank of an array.  It is 8,
  indicating that 7 is the highest possible rank.

1ARRAY-DIMENSION-LIMIT*						Constant
  any one dimension of an array must be smaller than this constant.

1ARRAY-TOTAL-SIZE-LIMIT*						Constant
  the total number of elements of any array must be smaller than this constant.

1VECTOR* &rest 2elements*
  creates and returns a general vector whose elements are as specified.

1ARRAY-ELEMENT-TYPE* 2array*
  returns a type specifier which describes what elements could be stored in 2array*.
  Thus, if 2array* is a string, the value is 1STRING-CHAR*.  If 2array* is an 1ART-1B* array,
  the value is 1BIT*.  If 2array* is an 1ART-Q* array, the value is 1T* (the type which
  all objects belong to).

1ARRAY-TOTAL-SIZE* 2array*
  the total number of elements in 2array*.  The same as 1ARRAY-LENGTH*.

1ARRAY-ROW-MAJOR-INDEX* 2array* &rest 2indices*
  calculates the cumulative index in 2array* of the element at indices 2indices*.
  1(AR-1-FORCE 2array* (ARRAY-ROW-MAJOR-INDEX 2array* 2indices*...))* is
  equivalent to 1(AREF 2array* 2indices*...)*.

1SVREF* 2vector* 2index*
  A special accessing function defined by Common Lisp to work only on
  simple general vectors.  Some other Lisp systems may be able to open code
  1SVREF* so that it is faster than 1AREF*, but on the Lisp machine 1SVREF* is a
  synonym for 1AREF*.

1BIT* 2bit-vector* 2index
1SBIT** 2bit-vector* 2index
1CHAR** 2bit-vector* 2index
1SCHAR** 2bit-vector* 2index*
  Special accessing functions defined to work only on bit vectors, only on
  simple bit vectors, only on strings, and only on simple strings, respectively.
  On the Lisp machine they are all synonyms for 1AREF*.

1BIT-AND* 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-IOR** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-XOR** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-EQV** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-NAND** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-NOR** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-ANDC1** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-ANDC2** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-ORC1** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array
1BIT-ORC2** 2bit-array-1* 2bit-array-2* &optional 2result-bit-array*
  perform boolean operations element by element on bit arrays.
  The arguments must match in their size and shape, and all
  of their elements must be integers.  Corresponding elements of
  2bit-array-1* and 2bit-array-2* are taken and passed to one of 1LOGAND*, 1LOGIOR*, ...
  to get an element of the result array.

  If the third argument is non-1NIL*,
  the result bits are stored into it, modifying it destructively.
  Otherwise a new 1ART-1B* array is created and used for the result.
  In either case, the value returned is the array where the results are stored.

  Common Lisp defines these operations only when all arguments
  are specialized arrays that hold only bits (1ART-1B* arrays, in the Lisp machine).

1BIT-NOT* 2bit-array* &optional 2result-bit-array*
  performs 1LOGNOT* on each element of 2bit-array* to get a result bit.
  If 2result-bit-array* is non-1NIL*, the result bits are stored in it;
  it must match 2bit-array* in size and shape.  Otherwise, a new 1ART-1B* array
  is created and used to hold the result.

  Each bit of the result is 0 if the argument was 1.

1ARRAY-HAS-FILL-POINTER-P* 2array*
  1T* if 2array* has a fill pointer.  It must have a leader and leader element 0
  must be an integer.

1VECTOR-PUSH* 2new-element* 2vector
1VECTOR-PUSH-EXTEND** 2new-element* 2vector* &optional 2amount*
  exactly like 1ARRAY-PUSH* and 1ARRAY-PUSH-EXTEND* except that
  the first two arguments are interchanged.

1VECTOR-POP* 2vector*
  a synonym for 1ARRAY-POP*.

1ADJUST-ARRAY* 2array* 2new-dimensions* &key 2element-type* 2initial-element* 2initial-contents* 2fill-pointer* 2displaced-to* 2displaced-index-offset*
  modifies various aspects of an array.  2array* is modified in place if that is possible;
  otherwise, a new array is created and 2array* is forwarded to it.  In either case,
  2array* is returned.  The arguments have the same names as arguments
  to 1MAKE-ARRAY*, and signify approximately the same thing.  However:

  2element-type* is just an error check.  1ADJUST-ARRAY* cannot change the array type.
  If the array type of 2array* is not what 2element-type* would request, you get an error.

  If 2displaced-to* is specified, the new array is displaced as specified.  If 2array*
  itself was displaced, it is modified in place provided that either
    2array* has an index offset and 2displaced-index-offset* is non-1NIL*, or
    2array* has no index offset and 2displaced-index-offset* is 1NIL*.

  Otherwise, if 2initial-contents* was specified, it is used to set all the contents
  of the array.  The old contents of 2array* are irrelevant.

  Otherwise, each element of 2array* is copied forward into the new array
  to the slot with the same indices, if there is one.  Any new slots whose indices
  were out of range in 2array* are initialized to 2initial-element*, or to 1NIL* or 0
  if 2initial-element* was not specified.

  2fill-pointer*, if specified, is used to set the fill pointer of the array.

4String Functions

1CLI:STRING=** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING//=** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING *string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING<** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING>** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING>=** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING<=** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2*
  compares all or portions of 2string1* and 2string2*, not ignoring case or font.
  For 1STRING=*, the value is 1T* when the strings match completely.
  For the other functions, when the condition is met, the value is
  the index in 2string1* of the point of first difference.

  There is a distinct Common Lisp version of 1STRING=* because a function
  1STRING=* already exists with the same purpose but a different calling sequence.

  Note that in Common Lisp syntax you would write 1STRING/=*, not 1STRING//=*.


1CLI:STRING-EQUAL* 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING-NOT-EQUAL** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING-LESSP** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING-GREATERP** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING-NOT-GREATERP** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2
1STRING-NOT-LESSP** 2string1* 2string2* &optional (2start1* 10*) end1 (2start2* 10*) 2end2*
  compares all or portions of 2string1* and 2string2*, ignoring case and font.
  For 1STRING-EQUAL*, the value is 1T* when the strings match completely.
  For the other functions, when the condition is met, the value is
  the index in 2string1* of the point of first difference.

  There is a distinct Common Lisp version of 1STRING-EQUAL* because a function
  1STRING-EQUAL* already exists with the same purpose but a different calling sequence.

1MAKE-STRING* 2size* &key (2initial-element* 10*)
  creates and returns a string of length 2size*, with each element initialized to
  2initial-element*.

1STRING-UPCASE* 2string* &key (2start* 10*) 2end*
  makes and returns a copy of 2string* in which all, or the specified portion, has been
  converted to upper case.

  The value may be 2string* itself if no characters in 2string* require conversion.

1STRING-DOWNCASE* 2string* &key (2start* 10*) 2end*
  makes and returns a copy of 2string* in which all, or the specified portion, has been
  converted to lower case.

  The value may be 2string* itself if no characters in 2string* require conversion.

1STRING-CAP*I1TALIZE* 2string* &key (2start* 10*) 2end*
  makes and returns a copy of 2string* in which all, or the specified portion, has been
  processed by capitalizing each word.  For this function, a word is any maximal
  sequence of letters or digits.  It is capitalized by putting the first character
  (if it is a letter) in upper case and any letters in the rest of the word in lower case.

  The value may be 2string* itself if no characters in 2string* require conversion.

1NSTRING-UPCASE* 2string* &key (2start* 10*) 2end
1NSTRING-DOWNCASE** 2string* &key (2start* 10*) 2end
1NSTRING-CAPITALIZE** 2string* &key (2start* 10*) 2end*
  like the previous functions except that they modify 2string* itself and return it.

4I/O Streams*

Common Lisp speaks of objects called "I/O streams", but as far as Common Lisp
is concerned these are simply whatever you can legitimately pass to functions
such as 1READ* and 1READ-CHAR* (the Common Lisp replacement for 1TYI*) and
1PRINT* and 1WRITE-CHAR* (which replaces 1TYO*).  Common Lisp has no concept
of streams as message-handling objects, or of users defining their own types
of streams.  It has a few standard functions that produce streams.

Of course, on the Lisp machine, the streams still are message-handling objects.
You can still define streams just as before, and they will work properly with
the Common Lisp I/O functions (which all work by invoking standard stream operations).

1*STANDARD-INPUT**							Variable
  an alias for 1STANDARD-INPUT

*STANDARD-OUTPUT**, 1*TERMINAL-IO**, 1*QUERY-IO**,		Variables
 1*DEBUG-IO**, 1*TRACE-OUTPUT**, 1*ERROR-OUTPUT**
  more synonyms.

1MAKE-SYNONYM-STREAM* 2symbol*
  returns a stream which does its work by invoking the value of 2symbol* as a stream.
  This is a new name for 1MAKE-SYN-STREAM*, and therefore works on locatives too.

1MAKE-CONCATENATED-STREAM* &rest 2streams*
  returns an input stream which will read its input from the first of 2streams* until
  that reaches its eof, then read input from the second of 2streams*, and so on until
  the last of 2streams* has reached eof.

1MAKE-TWO-WAY-STREAM* 2input-stream* 2output-stream*
  returns a bidirectional stream which passes input operations to 2input-stream* and passes
  output operations to 2output-stream*.  This works by attempting to recognize all standard
  input operations; anything not recognized is passed to 2output-stream*.

1MAKE-ECHO-STREAM* 2input-stream* 2output-stream*
  like 1MAKE-TWO-WAY-STREAM* except that each input character read via 2input-stream*
  is output to 2output-stream* before it is returned to the caller.

1MAKE-STRING-INPUT-STREAM* 2string* &optional (2start* 10*) 2end*
  returns a stream which can be used to read the contents of 2string* (or the specified
  portion of it) as input.  Eof will occur on reading past position 2end* or the end of 2string*.

1MAKE-STRING-OUTPUT-STREAM* 2string*
  returns an output stream which will accumulate all output in a string.

1GET-OUTPUT-STREAM-STRING* 2string-output-stream*
  returns the string of output accumulated so far by a stream which was made
  by calling 1MAKE-STRING-OUTPUT-STREAM*.  The accumulation is reset, so the
  output obtained this time will not be obtained again if 1GET-OUTPUT-STREAM-STRING*
  is called again later on the same stream.

1STREAMP* 2object*
  According to Common Lisp, 1T* if 2object* is a stream.  In the Lisp machine, a stream
  is any object which can be called as a function with certain calling conventions.
  It is theoretically impossible to test for this.  However, 1STREAMP* does return 1T*
  for any of the usual types of streams, and 1NIL* for any Common Lisp datum which
  is not a stream.

1INPUT-STREAM-P* 2stream*
  1T* if 2stream* handles input operations (at least, if it handles 1:TYI*).

1OUTPUT-STREAM-P* 2stream*
  1T* if 2stream* handles output operations (at least, if it handles 1:TYO*).

1STREAM-ELEMENT-TYPE* 2stream*
  returns a type specifier which describes the the 2typical* object input
  from or output to 2stream*.  The value is always a subtype of 1INTEGER* or a
  subtype of 1CHARACTER*.  If it is a subtype of 1CHARACTER*, a Common Lisp
  program should use 1READ-CHAR* or 1WRITE-CHAR*; otherwise it should use
  1READ-BYTE* or 1WRITE-BYTE*.

  However, the value returned is not intended to be rigidly accurate.
  Typical data transferred will fit the type, but there may be unusual cases
  in which other data are transferred that do not fit it; also, not all objects
  of the type may be possible as input or even make sense as output.
  The element type may be 1CHARACTER*, 1FIXNUM* or 1UNSIGNED-BYTE* if
  that is as much as the system knows about the stream, even if in fact
  only some characters or only bytes of a certain size really mean anything.

1CLI:CLOSE* 2stream* &key 2abort*
  Like traditional 1CLOSE* but the calling sequence is different.
  If 2abort* is non-1NIL*, the file (if it is being written) is not kept around.

      1Functions Changed

WITH-INPUT-FROM-STRING* (2var* 2string* &key 2index* 2start* 2end*) 2body1...**
  The calling sequence is changed.  2start* and 2end* specify, optionally,
  a portion of 2string* to be read.  2index*, if specified, is a variable in which
  the current index of reading in 2string* will be stored when the
  1WITH-INPUT-FROM-STRING* is exited.
  The old calling sequence was (2var* 2string* 2index* 2end*) 2body1...**
  This sequence is still recognized, for the sake of compatibility.

4The Reader; Input Functions*

      1Syntax Extensions*

These extensions to the reader syntax are upward compatible and apply to
Common Lisp and traditional syntax alike.

1...|...|...*
           Vertical bars can now be used 2within* a symbol, to quote some portion of it.
           For example, 1AB| ... "quoted stuff" :; |CDE* is one symbol.
           1AB|cd|EF|gh|* is also one symbol.  It used to be the case that if
           vertical bars were used in a symbol they had to go around the whole thing.

1#(...)*   signifies a vector.  It can contain any number of elements, of any type.
           Thus, 1#(A 5 "Foo")* reads as a vector containing a symbol, an integer and a string.
	   The vector created will always be of type 1ART-Q*.

1#*2bbb*...* signifies a bit vector; 2bbb*... are the bits (characters "1" or "0").
           A vector of type 1ART-1B* is created and filled with the specified bits,
	   starting with element 0.  The length is however many bits you specify.
	   Alternatively, specify the length with a decimal number between the
	   1#* and the 1**.  The last "1" or "0" specified is duplicated to fill the
	   additional 1bits.

#O*  causes numbers in the following object to be read in octal.
1#X*  causes numbers in the following object to be read in hex.
1#B*  causes numbers in the following object to be read in binary.

1#2n*A(...2contents*...)*
	1  *signifies an array of rank 2n*, containing 2contents*.  The entire list
	   which the 2contents* appear in is passed as the 2initial-contents* arg
	   to 1MAKE-ARRAY* to produce the array.  The array dimensions are
	   determined from the contents as well.  The rank is specified
	   explicitly so that the reader can distinguish whether a list
	   in the 2contents* is a list of array elements or a single array element.
	   The array type is always 1ART-Q*.

1#S(2type** 2slot* 2value* 2slot* 2value* 2slot* 2value* 1...)*
	   signifies a structure of type 2type*.  Any structure type defined with
	   1DEFSTRUCT* can be used as 2type* provided it has a standard constructor
	   macro taking slot values as keyword arguments.  The slot names
	   and values appearing in the read syntax are passed to the constructor
	   so that they initialize the structure.

1#C(2real* 2imag*)*
	   is a new input syntax for complex numbers, equivalent to
	   2real1+*imag1i**.  Unfortunately, the superior traditional Lisp machine
	   syntax is not allowed in Common Lisp files.  It is still allowed in
	   non-Common-Lisp files, of course.

1#(...)* within a backquote expression means "construct a vector"
         just as 1(...)* within a backquote expression means "construct a list".
         Thus, 1`(A #(B ,C))* expands into something like 1(LIST 'A (VECTOR 'B C))*.

Syntax for circular structure:

1#2n*#*  refers to the object with label 2n*.  2n* is a numeral made of decimal digits.
1#2n*=*  says that the object that follows is the antecedent of label 2n*.
Thus, 1#1=(A . #1#)* is a way of notating a circular list such as would be
produced by 1(CIRCULAR-LIST 'A)*.  The cdr of this list is ths list itself.

Syntax for floating point numbers:

Common Lisp defined four sizes of floating point number, but in a given
implementation they need not all be distinct.  The four sizes are called
1SHORT-FLOAT*, 1LONG-FLOAT*, 1SINGLE-FLOAT* and 1DOUBLE-FLOAT*.  They are specified
in read syntax by the use of 1S*, 1L*, 1F* or 1D* to start the exponent (thus, 11.5L6*
indicates a 1LONG-FLOAT*).  On the Lisp machine, there are really only two
sizes: 1SHORT-FLOAT* (also known as small flonums) and 1SINGLE-FLOAT*
(also known as regular flonums).  If you ask to make a double or long
float, you get a single-float.

The exponent can also be delimited with 1E*; this does not specify
the size of float, just as a number with no exponent (such as 11.5*) does not.
Then the variable 1*READ-DEFAULT-FLOAT-FORMAT** determines the size of
flonum used.  On output, flonums of the default size are printed
with no exponent (if that's convenient) or with 1E*, and flonums of
the other size are printed with a letter that indicates the size (1S* or 1F*).

      1Common Lisp vs Traditional Read Syntax*

These are the incompatible changes in reader syntax that apply only
to programs marked as Common Lisp.  There is no incompatible change
to the syntax of existing programs.

1/*  is a symbol constituent, just like 1A* and 1=*.
1\*  is the single-character-quote character.

1#\* produces a character object rather than a fixnum representing
    a character code.
1#/* is not defined as reader syntax.

Rational numbers are written with 1/* rather than 1\*, as 1in 1/2*,
and they are read using the current radix rather than always decimal.
Thus, the 1#B*, 1#O*, etc. prefixes affect them.

Complex numbers must be written with the new 1#C* syntax, as the
traditional 11+3i* syntax is not allowed in Common Lisp for some
brain-damaged reason.

      1Readtables, etc.

*READTABLE**								Variable
  an alias for the variable 1READTABLE*.  The value is the current readtable.

1COPY-READTABLE* &optional 2from-readtable* 2to-readtable*
  Copies the contents of 2from-readtable* into 2to-readtable*.  2to-readtable* is returned.
  If 2to-readtable* is 1NIL*, a new readtable is constructed,
  made a copy of 2from-readtable*, and returned.

  If 2from-readtable* is omitted, it defaults to something.
  The Common Lisp manual contradicts itself on what this something should be.

1READTABLEP* 2object*
  1T* if 2object* is a readtable.

1SET-MACRO-CHARACTER* 2char* 2function* &optional 2non-terminating-p* 2in-readtable*
  sets the syntax of character 2char* in readtable 2in-readtable* to be that of a
  macro character which is handled by 2function*.  When that character is read
  by 1READ*, 2function* is called.  The arguments given to 2function* are
  the input stream 1READ* is reading from, and the character just read (ie. 2char*).
  2function* should return zero or more values, which are the objects that
  the macro construct "reads as".  Zero values causes the macro construct
  to be ignored (the semicolon macro character would do this), and one value
  causes the macro construct to read as a single object (most macro characters
  do this).  More than one value is allowed only within a list.

  If 2non-terminating-p* is non-1NIL*, then 2char* will be recognized as a macro character
  only at the start of a token.  If found in the middle of a symbol, it will be alphabetic.
  1#* is a non-terminating macro character.

1GET-MACRO-CHARACTER* 2char* 2in-readtable*
  returns two values that describe the macro character status of 2char* in 2in-readtable*.
  If 2char* is not a macro character, both values are 1NIL*.  Otherwise, the first value
  is the 2function* and the second value is the 2non-terminating-p* for this character.
  Those two values, passed to 1SET-MACRO-CHARACTER*, would be sufficient to
  exactly the syntax that 2char* currently has.

1MAKE-DISPATCH-MACRO-CHARACTER* 2char* &optional 2non-terminating-p* 2in-readtable*
  makes 2char* be a dispatch macro character in 2in-readtable*.  This means
  that when 2char* is seen the reader will read one more character to decide
  what to do.  1#* is an example of a dispatch macro character.
  2non-terminating-p* means the same thing as in 1SET-MACRO-CHARACTER*.

1SET-DISPATCH-MACRO-CHARACTER* 2char* 2subchar* 2function* &optional 2in-readtable*
  sets the syntax of the two-character sequence 2char* 2subchar*, assuming that
  2char* is a dispatch macro character like 1#*.  When this two-character sequence
  is seen by 1READ*, it will call 2function* with three arguments:
  the input stream, the subchar that caused 2function* to be invoked,
  and the infix argument (in 1#5R*, this is the number 5) or 1NIL* if there
  is no infix argument this time.

  If 2subchar* is lower case, it is converted to upper case.  Case is never
  significant for the character that follows a dispatch macro character.
  The decimal digits may not be defined as subchars since they are always
  used for infix numeric arguments a la 1#5R*.

1GET-DISPATCH-MACRO-CHARACTER* 2char* 2subchar* &optional 2in-readtable*
  returns the 2function* for 2subchar* following dispatch macro character 2char*
  in readtable 2in-readtable*.  The value is 1NIL* if 2subchar* is not defined.

1SET-SYNTAX-FROM-CHAR* 2to-char* 2from-char* &optional 2to-readtable* 2from-readtable*
  copies the syntax of 2from-char* in 2from-readtable* to character 2to-char* in 2to-readtable*.
  2from-readtable* defaults to the current readtable and 2to-readtable* defaults
  to standard Common Lisp syntax.

  Common Lisp has a peculiar idea of what it means to copy the syntax of a character.
  The only aspect of syntax that the readtable supposedly specifies is the choice among
     token constituent (digits, letters, random things like @, !, $, and also colon!)
     whitespace
     escape character (/ traditionally, \ in Common Lisp)
     multiple escape character (vertical bar)
     macro character (which includes characters 1()",.'`;* )
     nonterminating macro character (1#*)
  the difference among macro characters are determined entirely by
  the functions that they invoke.  The differences among token constituents
  (including the difference between A and colon) are 2fixed*!  You can make A
  be a macro character, or whitespace, or a quote character, but if you make it a
  token constituent then it always behaves the way it normally does.
  Likewise, if you make open-paren into a token constituent, there is only
  one kind of token constituent it can be (it forces the token to be a symbol, like $ or @ or %).

  So, if 2from-char* is some kind of token constituent, this function makes 2to-char*
  into a token constituent of the kind that 2to-char* is supposed to be--not the kind
  of token constituent that 2from-char* is.

  This is precisely how 1SET-SYNTAX-FROM-CHAR* differs from the traditional
  function 1COPY-SYNTAX*.  1COPY-SYNTAX* would make 2to-char* have exactly 
  the same syntactic properties that 2from-char* has now.

      1Input Functions*

In all of these functions, the argument 2stream* defaults to 1STANDARD-INPUT*.

1*READ-BASE**								Variable
  an alias for 1IBASE*.

1CLI:READ* &optional 2stream* (2eof-errorp* 1T*) 2eof-value* 2recursive-p*
  This is like 1READ* but with slightly different arguments.
  End of file is an error if 2eof-errorp* is non-1NIL*.  Otherwise,
  end of file not in the middle of an object causes 1CLI:READ* to return 2eof-value*.
  End of file in the middle of an object is always an error.

  2recursive-p* should be non-1NIL* for calls to 1READ* from macro characters.
  This affects the processing of 1#2n*#* and 1#2n*=* labels, and of trailing whitespace.

1READ-PRESERVING-WHITESPACE* &optional 2stream* (2eof-errorp* 1T*) 2eof-value* 2recursive-p*
  Like 1CLI:READ* but binds 1READ-PRESERVE-DELIMITERS* to 1T*.
  This is the Common Lisp way of requesting the 1READ-PRESERVE-DELIMITERS* feature.

1READ-DELIMITED-LIST* 2char* &optional 2stream* 2recursive-p*
  reads s-expressions from 2stream* until the character 2char* is seen at top level,
  then returns a list of the objects read.  For example, if 2char* is ], and the text
  to be read from 2stream* is  1A (B C)] ...*  then the objects 1A* and 1(B C)* will
  be read, the 1]* will be seen as a terminator and discarded, and the value will
  be 1(A (B C))*.  2recursive-p* is as for 1CLI:READ*, affecting only 1#2n*=* labels.
  End of file within this function is always an error since it is always "within
  an object".

1*READ-DEFAULT-FLOAT-FORMAT**					Variable
  Its value is the type for read to produce by default for flonums whose precise
  type is not specified by the syntax.  The value should be either
  1GLOBAL:SMALL-FLOAT* or 1GLOBAL:SINGLE-FLOAT*, these being the only distinct
  floating formats that the Lisp machine has.

1READ-LINE* &optional 2stream*
  a synonym for 1READLINE*.

1READ-CHAR* &optional 2stream* (2eof-errorp* 1T*) 2eof-value*
  reads a character from 2stream* and returns it as a character object.
  End of file is an error if 1eof-errorp* is non-1NIL*; otherwise,
  it causes 1READ-CHAR* to return 2eof-value*.  Uses the 1:TYI* stream operation.

1READ-CHAR-NO-HANG* &optional 2stream* (2eof-errorp* 1T*) 2eof-value*
  similar but returns 1NIL* immediately when no input is available on an interactive stream.
  Uses the 1:TYI-NO-HANG* stream operation.

1UNREAD-CHAR* 2char* &optional 2stream*
  Untyi's 2char* on 2stream*.  2char* may be an integer or a character object.
  Uses the 1:UNTYI* stream operation.

1PEEK-CHAR* 2peek-type* &optional 2stream* (2eof-errorp* 1T*) 2eof-value*
  If 2peek-type* is 1NIL*, this is like 1READ-CHAR* except leaves the character
  to be read again by the next input operation.

  If 2peek-type* is 1T*, skips whitespace characters and peeks at the first nonwhitespace character.
  That character is the value, and is also left to be reread.

  If 2peek-type* is a character, reads input until that character is seen.
  That character is unread and also returned.

1LISTEN* &optional 2stream*
  1T* if input is available on 2stream*.  Uses the 1:LISTEN* operation.

1CLEAR-INPUT* &optional 2stream*
  discards any input now available on 2stream*, if it is an interactive stream.
  Uses the 1:CLEAR-INPUT* stream operation.

1CLI:READ-FROM-STRING* 2string* &optional (2eof-errorp* 1T*) 2eof-value* &key (2start* 10*) 2end* 2preserve-whitespace*
  reads input from the contents of 2string*, or the portion of it specified by 2start* and 2end*.
  The value of this function is the result of calling 1READ*.  2eof-errorp* and 2eof-value* are
  passed to 1READ*.  If 2preserve-whitespace* is non-1NIL*, 1READ-PRESERVING-WHITESPACE* is used.
  This function differs from 1READ-FROM-STRING* in having some additional arguments.

1PARSE-INTEGER* 2string* &key (2start* 10*) 2end* (2radix* 110.*) 2junk-allowed*
  parses the contents of 2string* (or the portion from 2start* to 2end*) as a numeral
  for an integer, and returns the integer it describes, using the specified radix.
  Radices larger than ten are allowed, and they use letters as digits beyond 9.
  Leading whitespace is always allowed and ignored.  A leading sign is also allowed
  and considered part of the number.

  When 2junk-allowed* is 1NIL*, the entire specified portion of 2string* must consist
  of an integer and leading and trailing whitespace.  Otherwise, an error happens.

  If 2junk-allowed* is non-1NIL*, parsing just stops when a non-digit is encountered.
  The number parsed so far is returned as the first value, and the index in 2string*
  at which parsing stopped is returned as the second value.  This number equals
  2end* (or the length of 2string*) if there is nothing but a number.  If non-digits are
  found without finding a number first, the first value is 1NIL*.

1READ-BYTE* 2stream* &optional (2eof-errorp* 1T*) 2eof-value*
  like 1TYI* except for handling the eof arguments just like the other functions above.

4The Printer; Output Functions*

There are now many special variables which you can bind to control
various options of printing.

1*PRINT-ESCAPE**							Variable
  if non-1NIL*, quoting characters (slashes or backslashes) are printed where
  necessary to make the output readable by Lisp.  Most of the output functions
  bind this variable to 1T* or to 1NIL*, so you rarely use the variable itself.
  1PRINC* binds the variable to 1NIL* and all the other output functions bind it to 1T*.

1*PRINT-BASE**							Variable
  The radix to use for printing integers.  This is a synonym for 1BASE*.
  Its default value is supposed to be ten, but it is actually eight at present.
  If the Common Lisp readtable is in use, this radix is used for printing ratios
  as well; the standard readtable currently always prints ratios in decimal.

1*PRINT-RADIX**							Variable
  if non-1NIL*, integers and ratios are output with something to indicate the radix
  that was used to print them.  The radix can be indicated with a prefix such
  as 1#X* or 1#3R*, or (for an integer) with a trailing decimal point.
  This is a replacement, though not a synonym, for 1*NOPOINT*.
  If 1*NOPOINT* is 1NIL* then the value of this variable is irrelevant.
  The normal means for selection of Common Lisp syntax also make
  1*NOPOINT* be 1T* and thus make 1*PRINT-RADIX** take effect.

1*PRINT-CIRCLE**							Variable
  if non-1NIL*, the printer recognizes circular structure and prints it using
  1#2n*=* labels so that it has a finite printed representation (which can be read back in).
  The default is 1NIL*, since it makes printing slower.

1*PRINT-PRETTY**							Variable
  if non-1NIL*, the printer actually calls 1GRIND-TOP-LEVEL* so that it prints extra
  whitespace for the sake of formatting.  The default is 1NIL*.  Currently the
  grinder does not know how to detect circular structure, so 1*PRINT-CIRCLE**
  is ignored in this case.

1*PRINT-GENSYM**							Variable
  if non-1NIL*, uninterned symbols are printed with the prefix 1#:* to mark them as such
  (but only when 1*PRINT-ESCAPE** is non-1NIL*).
  If 1NIL*, no prefix is used for uninterned symbols.  The default is 1T*.

1*PRINT-ARRAY**							Variable
  if non-1NIL*, arrays are printed in the new 1#(...)*, 1#** or 1#2n*A(...)* syntax
  so that you can see their contents.  The default is 1NIL*.

1*PRINT-CASE**							Variable
  controls the case used for printing upper-case letters in the names of symbols.
  Its value should be 1:UPCASE*, 1:DOWNCASE* or 1:CAPITALIZE*.  These mean, respectively,
  to print those letters as upper case, to print them as lower case, or to capitalize
  each word (see 1STRING-CAPITALIZE*).  Any lower case letters in the symbol name
  will be printed as lower case and quoted suitably; this flag does not affect them.
  Note that the case used for printing the upper case letters has no effect on
  reading the symbols back in, since they are case-converted by 1READ*.

1*PRINT-LEVEL**, 1*PRINT-LENGTH**					Variable
  aliases for 1PRINLEVEL* and 1PRINLENGTH*.

      1Print Functions*

The argument 2stream* always defaults to 1STANDARD-OUTPUT*.

1PRIN1
PRINT* 2object* &optional 2stream
1PRINC** 2object* &optional 2stream
1PRIN1-THEN-SPACE** 2object* &optional 2stream*
  These functions are unchanged, and mentioned just to remind you of them.

1PPRINT* 2object* &optional 2stream*
  like 1PRINT* except that it binds 1*PRINT-PRETTY** to 1T* so that the grinder is used.
  Also, it returns zero values.

1WRITE* 2object* &key 2stream* 2escape* 2radix* 2base* 2circle* 2pretty* 2level* 2length* 2case* 2gensym* 2array*
  prints 2object* on 2stream*, having bound all the printing flags according to the
  other keyword arguments.  Thus, the keyword argument 2array* specifies how
  to bind 1*PRINT-ARRAY**; if 2array* is omitted, the ambient value of 1*PRINT-ARRAY**
  is used.  This function is redundant given that the flag variables
  themselves are advertised for users to bind.  The value is 2object*.

1WRITE-TO-STRING* 2object* &key 2escape* 2radix* 2base* 2circle* 2pretty* 2level* 2length* 2case* 2gensym* 2array*
  like 1WRITE* but puts the output in a string and returns the string.

1PRIN1-TO-STRING* 2object
1PRINC-TO-STRING** 2object*
  like 1PRIN1* and 1PRINC* bit put the output in a string and return the string.

      1Output Functions

WRITE-CHAR* 2char* &optional 2stream*
  outputs 2char* to 2stream* (using 1:TYO*).  2char* may be an integer or a character object;
  in the latter case, it is converted to an integer before the 1:TYO*.

1WRITE-STRING* 2string* &optional 2stream* &key (2start* 10*) 2end*
  outputs 2string* (or the specified portion) to 2stream*.

1WRITE-LINE* 2string* &optional 2stream* &key (2start* 10*) 2end*
  outputs 2string* (or the specified portion) to 2stream*, followed by a Return character.

1TERPRI* &optional 2stream*
  outputs a Return character to 2stream*.

1FRESH-LINE* &optional 2stream*
  outputs a Return character to 2stream* unless either
   nothing has been output to 2stream* yet, or
   the last thing output was a Return character, or
   2stream* cannot tell what previous output there has been.
  This uses the 1:FRESH-LINE* stream operation.
  The value is 1T* if a Return was output (because all 1:FRESH-LINE* methods
  have been fixed to return this value).

1FINISH-OUTPUT* &optional 2stream
1CLEAR-OUTPUT** &optional 2stream
1FORCE-OUTPUT** &optional 2stream*
  invoke the 1:FINISH*, 1:CLEAR-OUTPUT* and 1:FORCE-OUTPUT* stream operations.

1WRITE-BYTE* 2number* &optional 2stream*
  outputs 2number* to 2stream* using 1:TYO*.

      1FORMAT Changes*

There are some new format operations, and a few that are changed in
Common Lisp.  There is a distinct symbol and function 1CLI:FORMAT* which is
used in Common Lisp programs, and this is how the 1FORMAT* package knows
which interpretation to use for the format operations that are incompatible.

Here are the format operations that are new, and that work the same in
all programs:

~B	like ~O and ~D, but prints in binary.
~X	like ~O and ~D, but prints in hex.
~@T	spaces horizontally a specified amount.
	1~2rel**,2period1@T** first outputs 2rel* spaces, and then zero or more spaces
	to get to a column which is a multiple of 2period*.
	If the stream being used cannot tell its cursor position, however,
	then 2period* is ignored.  Then this always outputs 2rel* spaces.
~@*	equivalent to the old (and current in non-Common-Lisp) meaning of ~G.
~?	indirect format string.  This gobbles two of the arguments
	given to 1FORMAT*, and uses the first as a 1FORMAT* string and the
	second as a list of arguments for that string.  Example:
	  1(FORMAT "~? ~D" "~O ~O" '(4 20.) 9)*   prints1  4 24 9*
~(...~)	case converted output.  The text within the construct is processed
	by 1FORMAT* as usual, but all letters output are converted to lower case.
	~:(...) converts case so that each word is capitalized.
	~@(...) capitalizes the first word, and converts all the rest of
	  the output to lower case.
	~:@(...) converts all letters to upper case.

Numeric parameters in the format string can now have minus signs.

Here are the format operations that are changed incompatibly for
Common Lisp programs only.

~E, and ~F now take many hairy arguments that control formatting.
~G is now a new kind of floating point output mode; use ~@* to
  get the traditional meaning of ~G.  For more information on these
  output modes, see the Common Lisp manual.

4Pathnames*

Common Lisp does not involve much change in the nature of pathnames,
but there are a few new functions.

1PATHNAMEP* 2object*
  1T* if 2object* is a pathname.

1PATHNAME* 2object*
  converts 2object* to a pathname and returns that, if possible.
  If 2object* is a string or symbol, it is parsed.
  If 2object* is a plausible stream, it is asked for its pathname
  with the 1:PATHNAME* operation.
  If 2object* is a pathname, it is simply returned.
  Anything else is not allowed.

1TRUENAME* 2object*
  returns the truename of the file specified somehow by 2object*.
  If 2object* is a plausible stream, it is asked for the truename with
  the 1:TRUENAME* operation.  Otherwise, 2object* is converted to a pathname
  and that pathname is opened to get its file's truename.

1PARSE-NAMESTRING* 2thing* &optional 2with-respect-to* (2defaults* 1*DEFAULT-PATHNAME-DEFAULTS**)
       &key (2start* 10*) 2end* 2junk-allowed*
  is equivalent to 1FS:PARSE-PATHNAME* except in that it takes some keyword arguments
  where the other function takes all positional arguments.

  1PARSE-NAMESTRING* parses 2thing* into a pathname and returns it.
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

1*DEFAULT-PATHNAME-DEFAULTS**					Variable
  an alias for 1DEFAULT-PATHNAME-DEFAULTS*.
  WARNING!!  According to the Common Lisp manual, this variable
  is supposed to have a pathname as its value.  On the Lisp machine,
  the value is and has always been an alist of hosts versus pathnames.
  This is how the 1*DEFAULTS-ARE-PER-HOST** feature is implemented.
  A separate variable 1CLI:*DEFAULTS-ARE-PER-HOST** would not work:
  which one would be used for actual defaulting?

  4If you set this variable to a pathname as a way of setting the*
  4defaults, you will lose big!*

  I do not know any way to protect you from this, except to flush
  the 1*DEFAULTS-ARE-PER-HOST** feature.  I have not done so because I
  think that if the Common Lisp designers disdainfully force inconvenience
  on the Lisp machine users then the hardship should be borne by
  users of Common Lisp rather than users of traditional Lisp machine
  features which antedate Common Lisp.

1MERGE-PATHNAMES* 2pathname* &optional (2defaults* 1*DEFAULT-PATHNAME-DEFAULTS**) (2default-version* 1':NEWEST*)
  merges defaults from 2defaults* into 2pathname* to get a new pathname, which is returned.
  2pathname* can be a string (or symbol); then it is parsed and the result is defaulted.
  2default-version* is used as the version when 2pathname* has a name but no version.

1NAMESTRING* 2pathname*
  returns a string containing the printed form of 2pathname*, as you would type it in.
  This uses the 1:STRING-FOR-PRINTING* operation.

1FILE-NAMESTRING* 2pathname*
  returns a string showing just the name, type and version of 2pathname*.
  This uses the 1:STRING-FOR-DIRED* operation.

1DIRECTORY-NAMESTRING* 2pathname*
  returns a string showing just the device and directory of 2pathname*.
  This uses the 1:STRING-FOR-DIRECTORY* operation.

1ENOUGH-NAMESTRING* 2pathname* &optional 2defaults*
  returns a string showing just the components of 2pathname* which would
  not be obtained by defaulting from 2defaults*.  This is the shortest string
  that would suffice to specify 2pathname*, given those defaults.
  It is done by using the 1:STRING-FOR-PRINTING* operation on a modified pathname.

1USER-HOMEDIR-PATHNAME* &optional (2host* 1FS:USER-LOGIN-MACHINE*) 2reset-p* 2user* 2force-p*
  a synonym for 1FS:USER-HOMEDIR*.

1TRANSLATED-PATHNAME* 2pathname*
  returns the translation of 2pathname*, the same as 2pathname* unless it is a logical
  pathname.  This uses the 1:TRANSLATED-PATHNAME* operation.
  This function is no longer part of the Common Lisp specification
  as of November 1983, but now that it exists I will leave it alone.

1BACK-TRANSLATED-PATHNAME* 2logical-pathname* 2actual-pathname*
  returns a logical pathname whose host is that of 2logical-pathname* and which would
  translate into 2actual-pathname*.  This uses the 1:BACK-TRANSLATED-PATHNAME* operation.
  This function is no longer part of the Common Lisp specification
  as of November 1983, but now that it exists I will leave it alone.

The following functions are now in GLOBAL rather than just in the FS package:

1INIT-FILE-PATHNAME* 2program-name* &optional (2host* 1FS:USER-LOGIN-MACHINE*)
1PATHNAME-HOST* 2pathname
1PATHNAME-DEVICE** 2pathname
1PATHNAME-DIRECTORY** 2pathname
1PATHNAME-NAME** 2pathname
1PATHNAME-TYPE** 2pathname
1PATHNAME-VERSION** 2pathname
1PATHNAME-PLIST** 2pathname
1MAKE-PATHNAME &key *host1 *device1 *directory1 *name1 *type1 *version1 *defaults1 *raw-device1 *raw-directory1 *raw-name1 *raw-type1 *defaults1 *canonical-type1 *original-type*
   (only 2host* 2device* 2directory* 2name* 2type* 2version* and 2defaults* are standard Common Lisp.)
1ADD-LOGICAL-PATHNAME-HOST* 2logical-host* 2actual-host* 2default-device* 2translations

4File Access Functions**

An argument named 2file* can be a pathname, a string or symbol which can be
parsed into a pathname, or a stream on which a file is open.

1OPEN* and 1WITH-OPEN-FILE* have new keyword arguments which are the
Common Lisp replacements for the keyword arguments used so far.
These are 2if-exists*, 2if-does-not-exist*, and 2element-type*.  In addition, the
2direction* argument allows new values which are synonyms for the others.

  for output opens, 2if-exists* specifies what to do if a file with the specified
  name already exists.  There are several values you can use:
    1:NEW-VERSION*  means create a new version.  This makes sense only
       when the pathname has 1:NEWEST* as its version, and it is the default
       in that case.
    1:SUPERSEDE*  means make a new file which, when closed, replaces the old one.
    1:OVERWRITE*  means write over the data of the existing file, starting
       at the beginning, and set the file's length to the length of the
       newly written data.
    1:TRUNCATE*  is like 1:OVERWRITE* except that it does not free the
       disk storage allocated to the file.  This might be faster.
    1:APPEND*  means add new data onto the existing file at the end.
    1:RENAME*  means rename the existing file and then create a new one.
    1:RENAME-AND-DELETE*  means rename the existing file, create a new one,
       and delete the old file when the new one is closed.
    1:ERROR*  means signal an error (1FS:FILE-ALREADY-EXISTS*).
       This is the default when the pathname's version is not 1:NEWEST*.
    1NIL*  means return 1NIL* from 1OPEN* in this case.

  2if-does-not-exist* specifies what to do when the file you ask for does not
  exist.  There are three values you can use:
    1:CREATE*  create a file.  This is the default for output opens,
       except when you use 1:APPEND*, 1:OVERWRITE* or 1:TRUNCATE* as
       the 2if-exists* argument.  I think that exception is a stupid mistake,
       but this is what Common Lisp says.
    1:ERROR*  signal an error.  This is the default for input opens,
       and also for output opens when you use 1:APPEND*, 1:OVERWRITE*
       or 1:TRUNCATE* as the 2if-exists*x argument.
    1NIL*  return 1NIL* from 1OPEN*.  This is the default for 1:PROBE* opens.

  2element-type* specifies what kind of objects the stream will want to
  read or write.  This combines the effect of the 2characters* and 2byte-size*
  arguments.  The value is a type specifier; it must be one of the following:
    1STRING-CHAR*  means read or write characters as usual.  The default.
    1CHARACTER*  means read or write characters, dealing with characters
       that are more than 8 bits.  You can succeed in writing out any
       sequence of character objects and reading it back, but the file
       does not look anything like a text file.
    1(UNSIGNED-BYTE 2n*)*  means read or write 2n*-bit bytes.
       Like 2characters* = 1NIL*, 2byte-size* = 2n*.
    1(SIGNED-BYTE 2n*)*  means read or write 2n*-bit bytes, sign extended.
       Each byte read from the file is sign-extended so that its top bit
       serves as a sign bit.
    1UNSIGNED-BYTE* or 1SIGNED-BYTE*  is similar but specifies 1:DEFAULT*
       as the byte size.  The file system will use the byte size of the file
       you have opened.
    1(MOD 2n*)*  same 1as UNSIGNED*-1BYTE* for a big enough byte size to hold
       all numbers less than 2n*.  1BIT* is also accepted, and means 1(MOD 2)*.
    1:DEFAULT*  is allowed, even though it is not a type specifier.
       It is the same as using 1:DEFAULT* as the value of 2characters*.

  2direction* now allows the values 1:PROBE* and 1:IO*.  1:PROBE* is the same
  as 1NIL*, to do a probe open.  1:IO* means you want a bidirectional stream
  (both input and output), but this is not now supported by any file system.

  Two other 2direction* values allowed are 1:PROBE-DIRECTORY* and 1:PROBE-LINK*.
  1:PROBE-DIRECTORY* is used to see whether a directory exists.
  If the opened pathname's directory is found, then the 1OPEN* completes
  (returning a non-I/O stream) as if the specified file existed whether
  it really exists or not.
  1:PROBE-LINK* is used to find out the truename of a link.  If the file
  specified exists as a link, then the 1OPEN* completes returning a non-I/O stream
  which describes the link itself rather than the file linked to.  If the
  file exists and is not a link, the 1OPEN* also completes for it as with any probe.

1DELETE-FILE* 2file* &key 2error* 2query*
  deletes the specified file.  Like 1DELETEF*, but takes its args keyword style.

1UNDELETE-FILE* 2file* &key 2error* 2query*
  undeletes the specified file.  This function is not Common Lisp, but it might as well exist.

1RENAME-FILE* 2file* 2new-name* &key 2error* 2query*
  renames 2file* to have the name 2new-name*.   Like 1RENAMEF* but takes keyword args,

1COPY-FILE* 2file* 2new-name* &key 2error* ...lots more args...
  not Common Lisp, but analogous to the others.  See 1SYS98 MSG* for all the args it takes.

1PROBE-FILE* 2pathname*
  if a file named 2pathname* exists, returns the file's truename;
  otherwise, returns 1NIL*.  (Actually, failure to open the file
  with any error condition except 1FS:FILE-NOT-FOUND* is not caught.)

1FILE-WRITE-DATE* 2file*
  returns the creation date/time of 2file*, as a universal time.

1FILE-AUTHOR* 2file*
  returns the name of the author of 2file* (the user who wrote it), as a string.

1FILE-LENGTH* 2file-stream*
  returns the length of the file open on 2file-stream*, in terms of the units
  in which I/O is being done on that stream.  (A stream is needed, rather than
  just a pathname, in order to specify the units.)

1FILE-POSITION* 2file-stream* &optional 2new-position*
  with one argument, returns the current position in the file of 2file-stream*,
  using the 1:READ-POINTER* stream operation.  It may return 1NIL* meaning that
  the position cannot be determined.  In fact, it will always return 1NIL*
  for a stream open in character mode and not at the beginning of the file.

  with two arguments, sets the position using the 1:SET-POINTER* stream operation,
  if possible, and returns 1T* if the setting was possible and 1NIL* if not.
  You can specify 1:START* to position to the beginning of the file, or
  1:END* to position to the end.

1DIRECTORY* 2pathname*
  returns a list of pathnames (truenames) of the files in the directory specified
  by 2pathname*.

1*LOAD-VERBOSE**							Variable
  the default value for the 2verbose* argument to 1LOAD*.

1LOAD* 2pathname* &key 2verbose* 2print* (2if-does-not-exist* 1T*) 2set-default-pathname* 2package*
  loads the specified file.  The old calling sequence with positional arguments
  is still accepted.  The new calling sequence differs mainly by having keyword
  instead of positional arguments; the argument 2print* and the feature it controls is new.
  The 2package* argument is not standard Common Lisp, but everybody uses it.
  The 2set-default-package* argument was in an earlier version of the Common Lisp
  standard but has been removed from the latest standard.

  If 2verbose* is non-1NIL* (it defaults to the value of 1*LOAD-VERBOSE**), then a message
  may be printed saying which file is being loaded and into which package.

  If 2set-default-pathname* is non-1NIL*, the pathname defaults are
  set to the name of the file loaded.  The default for 2set-default-pathname* is 1T*.


  If 2print* is non-1NIL*, the value of each expression evaluated from the file is printed
  on 1STANDARD-OUTPUT*.

4Error Signaling and Checking*

Note that Common Lisp does not define any way of handing errors, so there has
been no change in the facilities for doing so.

1CLI:ERROR* 2format-string* &rest 2args*
  signals an uncorrectable error whose error message is printed by passing
  2format-string* and 2args* to 1CLI:FORMAT*.  (1CLI:FORMAT* is used because only
  someone trying to write transportable Common Lisp code would want to
  use this calling sequence.)

1CERROR* 2continue-format-string* 2error-format-string* &rest 2args*
  when the first argument to 1CERROR* is a string, it means you are using
  this Common Lisp calling sequence.  Then the error prints its error message
  by passing 2error-format-string* and 2args* to 1CLI:FORMAT*, and it has one proceed type,
  which documents itself by passing 2continue-format-string* and 2args* to 1CLI:FORMAT*.

  The old calling sequence for 1CERROR* is still accepted, and even preferred.

1WARN* 2format-string* &rest 2args*
  prints a warning on 1ERROR-OUTPUT* by passing the args to 1FORMAT*,
  starting on a fresh line, and then returns.

  If 1*BREAK-ON-WARNINGS** is non-1NIL*, however, 1WARN* signals an error
  sort of like 1CERROR*.  You can proceed from the error, in which case
  1WARN* simply returns.

1*BREAK-ON-WARNINGS**						Variable
  If non-1NIL*, 1WARN* signals an error rather than just printing a message.

1BREAK* 2format-string* &rest 2args*
  1BREAK* passes 2format-string* and 2args* to 1FORMAT* to print a message,
  then enters a read-eval-print loop reading from 1TERMINAL-IO*.
  When you type the 1Resume* character, 1BREAK* returns 1NIL*.

  This is an incompatible change in the function 1BREAK*.  It used to accept
  an unevaluated string or symbol as its first argument and print it literally.
  To make old code continue to work, 1BREAK* actually does not evaluate
  its first argument, currently.  However, if you call 1BREAK* with a symbol
  as the first argument, the compiler will print a warning urging you to
  change the symbol to a string.

1CHECK-TYPE* 2place* 2typespec* [2string*]					Macro
  signals an error if the value of 2place* does not fit the type 2typespec*.
  This is tested by 1(TYPEP 2place* '2typespec*)*.  Note how 2typespec* is not
  evaluated; its value is used at compile time when you are compiling.
  2string* is used in the error message to say what the object was supposed to be;
  it should start with an indefinite article, as in 1"a number"*.  Usually you
  do not need to specify 2string* as a default will be computed from the 2typespec*. 
  The error message will also include both 2place* itself and 2place*'s value
  on this occasion (the value which failed the type test).

  The error is of condition 1SYS:WRONG-TYPE-ARGUMENT* and you can proceed,
  specifying a new value which is stored into 2place* with a 1SETF*.  The new value
  is then tested, and so on until a value passes the test.  Then 1CHECK-TYPE*
  returns.

1ASSERT* 2test-form* [(2places*...) [2string* 2args*...]]				Macro
  signals an error if 2test-form* evaluates to 1NIL*.  The rest of the 1ASSERT* is
  relevant only if the error happens.

  First of all, the 2places* are forms that can be 1SETF*, and which are used
  (presumably) in 2test-form*.  The reason that the 2places* are specified again
  in the 1ASSERT* is so that the expanded code can arrange for the user to
  be able to specify a new value to be stored into any one of them when
  he proceeds from the error.  When the error is signaled, one proceed-type
  is provided for each 2place* that can be set.

  If the user does proceed with a new value in that fashion, the 2test-form* is
  evaluated again, and the error repeats until the 2test-form* comes out non-1NIL*.

  The 2string* and 2args* are used to print the error message.  If they are
  omitted, a message "Failed assertion" is printed.

2  *The 2args* are evaluated only when an error is signaled, and are evaluated again
  each time an error is signaled.  1SETF*'ing the 2places* may also involve evaluation
  which will happen each time the user proceeds and sets one.

  Example:
   1(ASSERT (NEQ (CAR A) (CAR B)) ((CAR A) (CAR B))
          "A and B are EQ: ~S and ~S" (CAR A) (CAR B))*
  The 2places* here are 1(CAR A)* and 1(CAR B)*.  The 2args* happen to be the same
  two forms, by not-exactly-coincidence; the current values of the 2places* will
  often be useful in the error message.

1ETYPECASE* 2keyform* 2clauses*...
  like 1TYPECASE* except that an uncorrectable error is signaled if every clause fails.
  A 2clause* looks like 1(2typespec* 2forms*...)*.  The clause succeeds if 2keyform*'s value
  matches 2typespec* with 1TYPEP*; then the 2forms* are evaluated and the values of the
  last form are returned by the 1ETYPECASE*.  The first successful clause is the one
  that is used.

  1TYPECASE* allows 1OTHERWISE* clauses also, but 1ETYPECASE* does not allow them.

1CTYPECASE* 2keyform* 2clauses*...
  like 1ETYPECASE* except that 2keyform* must be 1SETF*'able and the error signaled
  is a correctable one.  The user can proceed with a new value; this value is
  stored into 2keyform* with 1SETF* and the clauses are tested again.

1ECASE* 2keyform* 2clauses*...
  like 1CASE* (or, 1SELECTQ*) except that an uncorrectable error is signaled if
  every clause fails.  Each clause starts with a value or a list of values,
  followed by forms.  The clauses are tested by matching the value of 2keyform*
  against the value or values specified in the clause.  If there is a match,
  the clause succeeds; its forms are evaluated and the values of the last form
  are returned from the 1ECASE*.  If all the clauses have been tested and failed,
  the error is signaled.

1CCASE* 2keyform* 2clauses*...
  like 1ECASE* except that 2keyform* must be 1SETF*'able and the error signaled
  is a correctable one.  The user can proceed with a new value; this value is
  stored into 2keyform* with 1SETF* and the clauses are tested again.

4The Compiler

1COMPILE** 2function-spec* &optional 2definition*
  If 2function-spec* is non-1NIL*, its definition is compiled.  If 2definition* is non-1NIL*,
  it should be a 1LAMBDA*-expression; it is compiled and the result becomes
  the definition of 2function-spec*.  The value is 2function-spec*.

  If 2function-spec* is 1NIL*, 2definition* is compiled and the result
  is returned without storing it anywhere.

1COMPILE-FILE* 2input-file* &key 2output-file* 2set-default-pathname*
  compiles the file specified by 2input-file*, a pathname.

  If 2output-file* is specified, it is a pathname used for the compiled file.
  Otherwise, the ouptut file name is computed from the input file name.

  2set-default-pathname*, if non-1NIL*, means that the defaults
  should be set to the input file's name.  If 2set-default-pathname* defaults to 1T*.
  The 2set-default-package* argument was in an earlier version of the Common Lisp
  standard but has been removed from the latest standard.

4Miscellaneous

1DOCUMENTATION** 2name* 2doc-type*
  returns the documentation of 2name* in the role 2doc-type*.
  If 2doc-type* is 1FUNCTION*, then 2name* may be any function-spec, and the
  documentation string of its function definition is returned.
  Otherwise, 2name* must be a symbol, and 2doc-type* may be anything.
  However, only these values of 2doc-type* are standardly used:
      1VARIABLE*	documentation of 2name* as a special variable.
	  Put on by doc strings in 1DEFVAR*, 1DEFCONST*, 1DEFCONSTANT*, 1DEFPARAMETER*.
      1TYPE*		documentation of 2name* as a type for 1TYPEP*.
	  Put on by doc strings in 1DEFTYPE* forms.
      1STRUCTURE*	documentation of 2name* as a 1DEFSTRUCT* type.
	  Put on by doc strings in 1DEFSTRUCT*s.
      1SETF*		documentation on what it means to 1SETF* a form
			that starts with 2name*.
	  Put on by a doc string in a 1DEFSETF* of 2name*.
      1DEFFLAVOR*      documentation of the flavor named 2name*.
	  Put on by the 1:DOCUMENTATION* option in 1DEFFLAVOR*.
	  It would be more consistent to use 1FLAVOR* as the 2doc-type*
	  for this, but it is desirable not to have to put 1FLAVOR* in GLOBAL.
	  Being forced to put 1TYPE* and 1VARIABLE* there is bad enough!

Documentation strings for any 2doc-type* can also be added to symbols
by means of 1(SETF (DOCUMENTATION 2name* 2doc-type*) 2string*)*.

1TIME* 2form*
  evaluates 2form* and prints the length of time that the evaluation took.
  The values of 2form* are returned.

  1TIME* with no argument still returns a fixnum time value counting
  in 60'ths of a second.

1DRIBBLE* &optional 2pathname*
  This function with an argument replaces 1DRIBBLE-START*.
  With no argument, it replaces 1DRIBBLE-END*.

1LISP-IMPLEMENTATION-TYPE*
  returns a string saying what kind of Lisp implementation you are using.
  On the Lisp machine it is always 1"Zetalisp"*.

1LISP-IMPLEMENTATION-VERSION*
  returns a string saying the version numbers of the Lisp implementation.
  It looks something like 1"System 98.3, CADR 3.0, ZMAIL 52.2"*.

1MACHINE-TYPE*
  returns a string describing the kind of hardware in use.
  It is 1"CADR"* or 1"LAMBDA"*.

1MACHINE-VERSION*
  returns a string describing the kind of hardware and microcode version.
  It starts with the 1MACHINE-TYPE*.  Example: 1"CADR Microcode 286"*.

1MACHINE-INSTANCE*
  returns a string giving the name of this machine.  Do not be confused;
  the value is a string, not an instance.  Example:  1"CADR-18"*.

1SOFTWARE-TYPE*
  returns a string describing the type of operating system software that Lisp
  is working with.  On the Lisp machine, it is always 1"Zetalisp"*, since the
  Lisp machine Lisp software 2is* the operating system.

1SOFTWARE-VERSION*
  returns a string describing the version numbers of the operating system software
  in use.  This is the same as 1LISP-IMPLEMENTATION-VERSION* on the Lisp machine
  since the same software is being described.

1SHORT-SITE-NAME*
  returns a string giving briefly the name of the site you are at.
  A site is an institution which has a group of Lisp machines.
  The string you get is the value of the 1:SHORT-SITE-NAME* site option
  as given in 1SYS: SITE; SITE LISP*.  Example: 1"MIT AI Lab".

LONG-SITE-NAME*
  returns a string giving a verbose name for the site you are at.
  This string is specified by the site option 1:LONG-SITE-NAME*.  Example:
  1"Massachusetts Institute of Technology, Artificial Intelligence Laboratory"

*FEATURES**								Variable
  the value is a list of symbols describing the features available in the
  Lisp system you are using.  The 1#+* and 1#-* read-time conditionals check
  for the presence or absence of keywords on this list.  Comparison against
  elements of 1*FEATURES** is done using 1STRING-EQUAL* so that package
  is irrelevant.

4Time Functions

1GET-INTERNAL-RUN-TIME
GET-INTERNAL-REAL-TIME**
  Both of these functions are equivalent to 1TIME* with no argument.
  They return the current time in 60'ths of a second, counting from
  an arbitrary instant.  This time value wraps around every so often and
  therefore values should be compared using the already existing functions
  1TIME-DIFFERENCE* and 1TIME-INCREMENT*.

1INTERNAL-TIME-UNITS-PER-SECOND*				Variable
  The number of time units in a second, for the values returned by
  1GET-INTERNAL-RUN-TIME*.  The value is 60.  The value may be different
  in other Common Lisp implementations.

1SLEEP* 2seconds*
  equivalent to 1(PROCESS-SLEEP (* 60. 2seconds*))

GET-UNIVERSAL-TIME*, 1DECODE-UNIVERSAL-TIME*, 1ENCODE-UNIVERSAL-TIME*
  These functions, which already existed in the 1TIME* package, are
  now in 1GLOBAL*.

  1DECODE-UNIVERSAL-TIME* returns one additional value,
  the time zone your machine is in.  Also, the year it returns is now
  a number > 1900 rather than a number < 100.

1GET-DECODED-TIME*
  equivalent to 1(DECODE-UNIVERSAL-TIME (GET-UNIVERSAL-TIME))*,
  however it can return 1NIL* if the system does not know the time.

One additional change is that a year number less than 100 is now extended
to a year within 50 years of the present, so that 27 stands for 2027 rather
than 1927.
