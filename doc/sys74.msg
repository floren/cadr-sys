-*- Text -*-
System 74 is now released to users.  This system comes with microcode 793.
Except for new features it should work with the previous microcode (786)
for the most part.

Systems earlier than 74.21 will not work with lisp machines as file
computers.

Non-special variables now obey lexical scoping in compiled code, if
you use LEXICAL-CLOSURE instead of FUNCTION around your internal
LAMBDA-expressions.  For example,
	(defun foo (a) (lexical-closure (lambda () a)))
returns a function which returns a.
Lexical closures do no consing if used in a downward direction.
They are automatically consed as soon as you try to pass them upward.
At some time in the future, FUNCTION may be changed to do the same
thing.  Note that this would be upward compatible with the previous
behavior of non-special variables.  Also, the interpreter may be
changed to be compatible with the compiler.  This would be an
incompatible change to the interpreter, but would not affect any code
which currently runs properly compiled.

With &KEY, you can now specify the keyword and variable name for a
keyword argument independently.  To do this, use two levels of list
structure for the variable: starting with the list which contains the
name and possibly the initial value, and then replace the name with
another list, containing the keyword followed by the name.
For example: (defun foo (&key ((:x x-variable))
			 &optional ((si:y y-variable) default)) ...)
It is a good idea to change functions to use &KEY, because then
ARGLIST will give complete information on the keywords they accept.

The first argument to BREAK is now optional;  (BREAK)  <=>  (BREAK NIL).
This means that there are two easy ways to write a breakpoint into your
program: (BREAK) gets a read-eval-print loop, and (ERR) gets the error
handler.  (These are the programmatic equivalents of the Break and Meta-Break
keys on the keyboard.)

The C-M-N and C-M-P commands in the error handler will now show you
the incomplete frames for functions whose arguments are still being
accumulated.  What you will see will include some number of
already-computed arguments, plus some number of temporaries on the
stack for microcoded primitives.  The error handler doesn't know how
to sort which are which, yet, but at least you can see them.

A new function ZWEI:AUTO-FILL-IF-APPROPRIATE, analogous to
ZWEI:ELECTRIC-SHIFT-LOCK-IF-APPROPRIATE.  LOGIN-SETQ ZWEI:TEXT-MODE-HOOK
to this in your init file and text without -*-Nofill:T-*- will be in
auto fill mode.

Support has been added for rational numbers.  It will change relatively
soon, so programs should not be made to depend on it yet.

Hosts now have standard representations as instances.  In particular,
the HOST instance variable of a pathname is one.  This change should not
affect any user code.  The variable CHAOS:HOST-ALIST is now gone, since
the host table can handle multiple networks.  SI:PARSE-HOST is the way
to convert a string into a host object, from which you can get the
standard name or nicknames.  (SI:GET-HOST-FROM-ADDRESS num ':CHAOS) is
the way to get a host object from a number.

Files that have not been recompiled in a very long time will need to be
recompiled for this system.  The symptom is that loading them tries to
send a message to an array.

There is a new process-warm-boot-action, si:process-warm-boot-delayed-restart,
and it is now the default instead of si:process-warm-boot-restart.  The
effect is that random processes do not get restarted any more before the
system has finished initializing itself.  Processes needed to initialize the
system, such as the Keyboard and Chaosnet processes, do not use this
warm-boot action.

Some small bugs associated with floating-point numbers have been fixed;
some numerical programs may get different results.  The principal source
of roundoff error in the reader (inaccurate powers of ten) has been corrected;
many more numbers will be read as the best flonum approximation, although
large and small numbers will still have an error of 1 least significant bit
in some cases.  (There are no longer any cases with an error of more than 1
LSB).  A fencepost error in floating-point subtraction has been corrected;
formerly if the subtrahend was exactly equal to 1.0 times the weight of the
last significant bit in the minuend, the answer would be the minuend, which
is incorrect.  (The fact that pdp-10's have the identical bug does not make
it less of a bug.)  The bug in integer division reported a few weeks ago
is also fixed.
