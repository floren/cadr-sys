Date: Friday, 12 February 1982, 14:30-EST
From: David L. Andre <DLA at SCRC-TENEX>
Subject: System 79 release.
To: INFO-LISPM at MIT-AI
Cc: DLA at SCRC-TENEX

System 79.8 is now released to users.  It comes with ZMail 40,
LMFILE-Remote 5, Remote-File 2, and microcode 849.  Copies of system 79
bands are kept on the FileComputer (LM27), and you may get a band from
there at any time, regardless of who is logged in there.

A number of patches before 79.8 take a long time to load.  It will
probably be easier for you to get a band from the FileComputer than to
load those patches, if you already have a system 79 band.

  Table of contents:

  Things you should take note of:
 [1] New Converse subsystem for interactive messages.
 [2] Name changes to some editor commands.
 [3] PROCESS-RUN-FUNCTION and related functions have been changed slightly.
 [4] New function SI:LOAD-AND-SAVE-PATCHES.
 [5] File computers.

  Things you may safely ignore:
 [A] New user option FS:*ALWAYS-MERGE-TYPE-AND-VERSION*
 [B] A bug in %P-STORE-CONTENTS has been fixed.
 [C] Only one process at a time may run in the compiler.
 [D] New subst FILL-POINTER.
 [E] New function ERRORP.
 [F] Error handler now uses ERROR-OUTPUT for its IO.
 [G] PROCESS-RUN-FUNCTION changes.
 [H] New features in resources.
 [I] New :SELECT-METHOD function spec.
 [J] Some functions have moved to new files.
 [K] New hosts are supported as file computers.
 [L] Terminal B buries the selected window.
 [M] New PEEK S mode for information on servers.
 [N] MONITOR-VARIABLE and UNMONITOR-VARIABLE now actually work.
 [O] New functions for use in the interactive error handler (debugger).
 [P] Lambda macros are now supported.

  Things that you should take note of:

[1] This system includes the new Converse subsystem, for interactive
messages between users.  (qsend) or System-C will select the Converse
window, which should be self-documenting (type the Help key at that
window for documentation on the window, the Lisp functions, and the
user-option variables).  The qsend function with arguments works the
same as before, except that if you don't specify a host it will search
all Lisp machines on the local Chaosnet for the person to send to; in
other words, all Lisp machines whose names the qsend function would
accept.

[2] The editor command List Multiple Callers has been renamed to
Multiple List Callers for consistency (there is a whole family of these;
everything with Multiple in its name now has Multiple as the first
word.)

The editor command List Functions has been renamed to List Definitions.

[3] PROCESS-RUN-FUNCTION now does what PROCESS-RUN-TEMPORARY-FUNCTION used to
do.  This means that the writeup in the manual is now obsolete.
PROCESS-RUN-TEMPORARY-FUNCTION is still around, but is obsolete and will go
away eventually.   More about this in the second section.

[4] There is a new function SI:LOAD-AND-SAVE-PATCHES, which is the right
thing to use if you want to load patches and save out the band.  It
loads any patches, and if some were made, it prints the disk label and
queries you to find out which partition should be used for the dump.
This function may (and should, usually) be called without logging in, so
as to avoid putting the contents of your login file into the world load.
It is smart about many of the common errors people make when saving
bands, and therefore should avoid many of those problems.

A related change is that LOAD-PATCHES now returns T if any patches were
made, and NIL otherwise.

[5] Access to the file computer is now loaded by default.  This is the
reason for the LMFILE-Remote and Remote-File systems in the herald.
Interested parties are referred to LMFS;FSDOC for information on the LM
file system, and to LMFILE;DOC for information on the LMFILE file
system.

  Things that you may safely ignore:

[A] A new user option FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is available
for those who prefer TENEX style pathname defaulting.  The default
algorithm for merging does NOT take the type and version from the
default pathname if a name is specified.  For example, on TWENEX, typing
"FOO" with the default "BAR.LISP" will give "FOO", NOT "FOO.LISP".
There have been some complaints from people who dislike this convention,
so with FS:*ALWAYS-MERGE-TYPE-AND-VERSION* set to T, they will get the
desired behavior.  Most people will not want to use this.

[B] A bug in %P-STORE-CONTENTS, causing it to follow invisible pointers,
has been fixed.  This is mentioned here since it is a low-level change
that might conceivably break something.  (In fact two functions in
the system depended on the buggy behavior and have been fixed; other
functions in the system did not work because of the buggy behavior.)

[C] Only one process at a time may run the compiler.  If a second
process tries to enter the compiler while it is busy, a notification
will be printed and the second process will wait with who-line state
"Compiler".  If the first process is not ever going to get out of the
compiler (perhaps it is stopped with an error), you must abort it in
order for the second process to proceed.  This was announced in system
78, but now it really works.

[D] The function FILL-POINTER, given an array, returns the fill pointer
of the array.  This is actually a SUBST, and so you can use SETF with it. 

[E] New function ERRORP.  For now equivalent to STRINGP, for the benefit
of the :NOERROR keyword to OPEN and corresponding arguments to DELETEF
and related functions.  In the new error system, these functions will
return error objects rather than strings, and ERRORP will be changed to
test whether something is an error object.  Start converting your code.

[F] The error handler now does its i/o on the stream which is the value
of ERROR-OUTPUT, or what it is indirected to if it is a syn stream.  It
used to use TERMINAL-IO.  The default value of ERROR-OUTPUT is syn to
TERMINAL-IO, hence this change only makes a difference if you change
ERROR-OUTPUT.  The error handler bugs where QUERY-IO was bound to the
wrong thing are also fixed.

This may be changed again, by introducing a new stream ERROR-IO which
the error handler uses, and making ERROR-OUTPUT be separately bindable.
That has not been done yet, and may be a bad idea.

[G] The first argument to PROCESS-RUN-FUNCTION (and the two similar
functions) may now be a list of alternating keywords and values rather
than a string.  The keywords are:

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

Note that the names of processes, like the names of everything else, are
strings.  It used to work, at least to some extent, to use any Lisp object
as the name of a process, and some people have been using symbols.  This
no longer works, as it happens.

A related change is that P in PEEK now shows more information about
processes.

[H] DEFRESOURCE has been changed to provide some new features and to
have better paging performance.  The :INITIALIZER is a form or function
that sees OBJECT and its parameters as arguments, when OBJECT is about
to be allocated (whether it is being reused or was just created); it may
initialize the object.  The :FREE-LIST-SIZE option allows you to set the
size of the array of objects.

[I] There is a new function spec (:SELECT-METHOD fcn-spec message).  If
fcn-spec contains a select-method, then this refers to the function to
which the select method dispatches upon receiving <message>.  It is an
error if fcn-spec doesn't contain a select method, or if the select
method doesn't support message.  This is now used by DEFSELECT rather
than making up new symbols for the functions.

This also means that if a DEFSELECT is compiled in system 79, it will
not load into an older system.

[J] Numerous system functions are in different files now, for various
reasons. This should not affect anyone.

[K] VMS and Unix file systems are now supported as file servers.

NOTE WELL: There are known problems with Unix pathnames with regard to
alphabetic case and file-types.  These reflect a low-level pathname
problem, which cannot be fixed until the next system release.  Be
forewarned. 

[L]  Terminal B buries the selected window.

[M]  In PEEK, S now displays information on the active servers.

[N] MONITOR-VARIABLE and UNMONITOR-VARIABLE actually work now (they have
been in the manual for some time).

[O] The error handler now provides functions EH-ARG, EH-LOC, EH-VAL and
EH-FUN which you can use for looking at the args, locals, values being
returned and function of the current frame.  Using SETF and LOCF, you
can also modify these things or get their locations.

EH-ARG and EH-LOC accept either the number of the arg or local (origin
0) or its name (packages do not matter), and return the value of the
arg or local.  EH-FUN takes no arg and returns the function this frame
is calling.

EH-VAL examines the values this frame is going to return, or, for a
trap on exit, the values it has already returned.  It allows only
a number to say which value, since values do not have names.
EH-VAL with no argument returns a list of all the values this frame is
returning.  EH-VAL with no argument cannot be SETF'd or LOCF'd,
however.

[P] Lambda macros are now supported.  Associated functions are
DEFFUNCTION, DEFLAMBDA-MACRO, DEFLAMBDA-MACRO-DISPLACE, and
LAMBDA-MACRO.  Interested parties are referred to LMDOC;LMACRO for more
information.
