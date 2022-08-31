-*- Text -*-
Relevant features to appear in system 201.
System 201 comes with microcode 854.

[Note that this should be practically the same as LMDOC;SYSTEM >, in
which RMS has documented his EH changes...]

  Table of contents:
  Things you should take note of:
 [Fill in table of contents later.]

  Things you may safely ignore:
 [Fill in table of contents later.]

 [Number/letter the paragraphs later]

  Things that you should take note of:

The editor command List Multiple Callers has been renamed to Multiple List
Callers for consistency (there is a whole family of these; everything with
Multiple in its name now has Multiple as the first word.)

process-run-function now does what process-run-temporary-function used to
do.  This means that the writeup in the manual is now obsolete.
process-run-temporary-function is still around, but is obsolete and will go
away eventually.  



  Things that you may safely ignore:

There is a new function SI:LOAD-AND-SAVE-PATCHES, which is the right
thing to use if you want to load patches and save out the band.  It
loads any patches, and if some were made, it prints the disk label and
queries you to find out which partition should be used for the dump.
This function may (and should) be called before you log in, so as to
avoid putting the contents of your login file into the world load.  It
is smart about many of the common errors people make when saving bands,
and therefore should avoid many of those problems.

A related change is that LOAD-PATCHES now returns T if any patches were
made, and NIL otherwise.

A new user option FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is available
for those who prefer TENEX style pathname defaulting.  The default
algorithm for merging does NOT take the type and version from the
default pathname if a name is specified.  For example, on TWENEX, typing
"FOO" with the default "BAR.LISP" will give "FOO", NOT "FOO.LISP".
There have been some complaints from people who dislike this convention,
so with FS:*ALWAYS-MERGE-TYPE-AND-VERSION* set to T, they will get the
desired behavior.  Most people will not want to use this.

A bug in %P-STORE-CONTENTS, causing it to follow invisible pointers,
has been fixed.  This is mentioned here since it is a low-level change
that might conceivably break something.  (In fact two functions in
the system depended on the buggy behavior and have been fixed; other
functions in the system did not work because of the buggy behavior.)

Only one process at a time may run the compiler.  If a second process tries
to enter the compiler while it is busy, a notification will be printed and
the second process will wait with who-line state "Compiler".  If the
first process is not ever going to get out of the compiler (perhaps
it is stopped with an error), you must abort it in order for the second
process to proceed.

The function FILL-POINTER, given an array, returns the fill pointer of
the array.  This is actually a SUBST, and so you can use SETF with it.

New function ERRORP.  For now equivalent to STRINGP, for the benefit
of the :NOERROR keyword to OPEN and corresponding arguments to DELETEF
and related functions.  In the new error system, these functions will
return error objects rather than strings, and ERRORP will be changed
to test whether something is an error object.  Start converting your code.

The error handler now does its i/o on the stream which is the value
of ERROR-OUTPUT, or what it is indirected to if it is a syn stream.  It
used to use TERMINAL-IO.  The default value of ERROR-OUTPUT is syn to
TERMINAL-IO, hence this change only makes a difference if you change
ERROR-OUTPUT.  The error handler bugs where QUERY-IO was bound to the
wrong thing are also fixed.

This may be changed again, by introducing a new stream ERROR-IO which
the error handler uses, and making ERROR-OUTPUT be separately bindable.
That has not been done yet, and may be a bad idea.

New lambda-macro feature and associated globals DEFLAMBDA-MACRO,
DEFLAMBDA-MACRO-DISPLACE, and LAMBDA-MACRO....

New global DEFFUNCTION....

The first argument to process-run-function (and the two similar functions)
may now be a list of alternating keywords and values rather than a string.
The keywords are:

	:name - a string, default "Anonymous"
	:restart-after-reset - boolean, default nil => :reset kills process
	:restart-after-boot - boolean, default nil => warm or cold boot kills
	:warm-boot-action - same as for make-process, except :flush does
			    what :warm-boot-action nil does, and nil (default)
			    means obey the :restart-after-boot keyword
	:priority - a number, default 0 which is standard priority
	:quantum - a number of 60ths of a second, default 60.

process-run-restartable-function defaults :restart-after-boot and
:restart-after-reset to t rather than nil.

defresource has been changed to provide some new features and to have
better paging performance.  The :initializer is a form or function
that sees OBJECT and its parameters as arguments, when OBJECT is about
to be allocated (whether it is being reused or was just created); it
may initialize the object.  The :free-list-size option allows you to
set the size of the array of objects.  [Soon to be added?  Threaded
free list?]  [Yeah, but not in this system.]

There is a new function spec (:SELECT-METHOD fcn-spec message).
If fcn-spec contains a select-method, then this refers to the function
to which the select method dispatches upon receiving <message>.  It is
an error if fcn-spec doesn't contain a select method, or if the select
method doesn't support message.  This is now used by DEFSELECT rather
than making up new symbols for the functions.

Numerous system functions are in different files now, for various reasons.
This should not affect anyone.

[If the new flavor implementation is included in this system, document
its incompatibilities here.]

[Document CONVERSE]