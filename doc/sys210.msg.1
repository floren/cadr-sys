.c -*- Text -*-

RELEASE NOTES FOR SYSTEM 210

INTRODUCTION

These notes accompany the Symbolics software release that includes
System version 210.  They cover all important changes since the System
204 release notes.

This document is in the following files:

EE:LIB:<LISPM.DOCUMENTATION>SYSTEM-210.RELEASE
AI: LMDOC; SYS210 MSG
SCRC:<DOC>MIT-RELEASE-210.TEXT

There have been no particularly major changes in system 210.  There are
the usual slew of minor bug fixes, some slight performance improvements,
and some minor new features.

This set of release notes is very long.  To help make it more manageable,
these release notes are divided into four sections:

 o  Changes to take note of.

      Everybody should read, or at least skim, this section.  It tells
      about incompatible changes that might break programs, or that might
      confuse you.

 o  Changes that only affect advanced users.

      The changes in this section might break programs written by advanced
      users, but are very unlikely to affect normal or casual users of the
      system.  You only have to worry about things in this section if you
      work with advanced subsystems and use advanced features.

 o  Upward-compatible changes.

      This section documents new features.  You can skip this entirely if
      you don't feel like reading a lot of stuff.  However, there are
      several interesting new features that you might like to know about,
      and we encourage you to take advantage of them.

 o  Notes.

      This section isn't about changes at all.  It answers some commonly
      asked questions and provides various news.  Of course, you can skip
      this entirely if you like.


SUMMARY OF CHANGES.

Changes to take note of:
[1] System 210 comes with microcode 896.  They must be run together.
[2] You must load patches into older systems to allow them to CC System 210.
[3] LOAD-PATCHES loads the site files if they have changed.
[4] SET-TV-SPEED should not be needed any more.
[5] Mode Line commands have been renamed (to Attribute List).
[6] UNION and INTERSECTION changed; new functions NUNION and NINTERSECTION.
[7] The "class" feature has been removed.  <- is gone.
[8] Compiler warnings work differently; several editor commands changed.
[9] HOSTAT must be aborted with Control-Abort now, like everything else.
[10] The Meta-X Set Package in Zmacs has been changed.
[11] The "Require Subjects on Outgoing Messages" Zmail option has changed.
[12] Converse (the interactive message system) has been changed.

Changes that only affect advanced users.
[1] The "compile driver" has been changed incompatibly.
[2] SI:RESET-TEMPORARY-AREA requires you to declare the area to be temporary.
[3] Some useless or obsolete symbols have been removed from GLOBAL.
[4] The TV:SHEET- functions are now obsolete.
[5] CHAOS:SHOUT removed.

Upward-compatible changes.
[1] The command interface to Peek has changed slightly.
[2] The names of process wait states have been changed.
[3] New functions CHAOS:NOTIFY-ALL-LMS and CHAOS:NOTIFY.
[4] Package declarations can be placed in the attribute list of a file.
[5] New feature to make DISK-RESTORE faster.
[6] New utility: the "band compressor".
[7] New Zmacs commands: Meta-X Start Patch and Meta-X Abort Patch.
[8] New functions: SEND and LEXPR-SEND.
[9] New, experimental rubout handler available for you to try.
[10] New function: TIME-INCREMENT.
[11] New function: UPTIME.
[12] New global function: PRINT-HERALD.
[13] New function: STORE-CONDITIONAL.
[14] New function: FUNDEFINE.
[15] New function: UNDELETEF.
[16] New function: SI:FLAVOR-ALLOWED-INIT-KEYWORDS.
[17] New function: SI:CLEAR-BAND.
[18] New global function: VIEWF.
[19] New functions to handle intervals of time.
[20] New feature to allow shift keys to replace double-clicking.
[21] New feature in Choose-Variable-Values.
[22] Hosts not in the host table can be named, as CHAOS|nnnn.
[23] New flavor: TV:CENTERED-LABEL-MIXIN.
[24] New option to MAKE-ARRAY: :INITIAL-VALUE.
[25] New functions: SI:DESCRIBE-PARTITIONS and SI:DESCRIBE-PARTITION.
[26] New special forms: VARIABLE-LOCATION, VARIABLE-BOUNDP, VARIABLE-MAKUNBOUND.
[27] New function: SI:FULL-GC.  New initialization list: FULL-GC.
[28] New functions to deal with closures.
[29] New debugger command: Meta-I.  New debugger functions: EH-FRAME and EH-SG.
[30] The function FQUERY has some new features.
[31] New facility for handling messages to flavor objects: "whoppers"
[32] New special forms: DO* and DO*-NAMED.
[33] Two new features have been added to the Dired mode of Zmacs.
[34] New stream function: SI:NULL-STREAM.
[35] VAX/VMS is now a supported file system.
[36] How to add new keywords for ADD-INITIALIZATION.

Notes.
[1] Clarification of how MAKE-SYMBOL works.
[2] How to get panes out of the Select menu.
[3] The MAR facility doesn't work.
[4] How to choose process priority levels.
[5] How to interpret directory listings.
[6] Status report on problems with Unix file systems.
[7] Problems with Zmail background process errors have been fixed.
[8] How to establish Chaosnet servers.


CHANGES TO TAKE NOTE OF

[1] System 210 comes with microcode 896.  They must be run together.

[2] You must load patches into older systems to allow them to CC System 210.

Using CC (the cross-machine debugger) will not work if the slave is
running System 210 and the master is running an older system that does
not have the latest patches loaded.  In System 79, you need patch 79.48;
in System 85, you need patch 85.44; in System 204, you need patch
204.74.

[3] LOAD-PATCHES loads the site files if they have changed.

When you do (LOAD-PATCHES), if any of the site-specific information
for your site has changed, this new information will be loaded as well.
For example, if your host table has been updated, (LOAD-PATCHES) will
load the new host table.  If you call LOAD-PATCHES to load the patches
for a specific system (rather than for all systems), LOAD-PATCHES won't
update the site information; if you want to load patches for all systems
but not update the site information, you can use the new :NOSITE option
to LOAD-PATCHES.

[4] SET-TV-SPEED should not be needed any more.

Because of out-of-date hardware in several MIT machines, it has been
necessary to do (SI:SET-TV-SPEED 60.) to get acceptable picture quality
on the black-and-white display on some machines.  With System 210, this
should no longer be necessary.  Please report any problems in this regard.
(Apparently on CADR13 it is still sometimes necessary to do
(SI:SETUP-CPT SI:CPT-SYNC-60HZ NIL T) under some circumstances; apparently
this machine has very old hardware.)

[5] Mode Line commands have been renamed (to Attribute List).

The name of the "-*-" line, which appears at the beginning of files,
has changed.  It used to be called the "file property list" (see section
21.9.2 of the Lisp Machine Manual, page 369 of Revision 4).  It is now
called the "attribute list", in order to avoid confusing it with the
"property lists" that the Lisp Machine File System provides for files.

The term "attribute list" applies not only to the -*- line in character
files, but also to an analogous data structure in compiled (QFASL)
files.  For example, in both cases the attribute list tells LOAD what
package to load the file into.

The most important result of this change is that two Zmacs commands
have new names:

   Meta-X Update Mode Line    is now called  Meta-X Update Attribute List
   Meta-X Reparse Mode Line   is now called  Meta-X Reparse Attribute List.

We hope that this incompatible change won't bother anyone too much.

For naming consistency, the following two function have been renamed:

   FS:FILE-READ-PROPERTY-LIST is now called  FS:READ-ATTRIBUTE-LIST
   FS:FILE-PROPERTY-BINDINGS  is now called  FS:FILE-ATTRIBUTE-BINDINGS

The old function names will continue to work indefinitely, but the
compiler will warn you that they are obsolete.

[6] UNION and INTERSECTION changed; new functions NUNION and NINTERSECTION.

The functions UNION and INTERSECTION have been changed so that they
do not clobber their arguments; they are now non-destructive.  New
functions NUNION and NINTERSECTION now exist; these are destructive
versions and may re-use cons cells from any list passed to them.

[7] The "class" feature has been removed.  <- is gone.

The old "class" feature, which was never documented and is considered
to have been made obsolete by flavors, has finally been removed.  Many
functions and special forms associated with classes have been removed,
and editor commands associated with message-passing now only act on
flavors.  Unless you have old software that actually uses classes, this
change should not affect your programs.  The one important
incompatibility for some users is the removal of the function "<-".
Such uses should be replaced by the new function SEND (mentioned
elsewhere in this document).  The following symbols have been
removed from the GLOBAL package:

  %METHOD-CLASS <- <-AS <<-- ARRAY-CLASS  CLASS CLASS-CLASS CLASS-METHOD-SYMBOL
  CLASS-SYMBOL CLASS-SYMBOLP CONS-CLASS DEFCLASS DEFMETHOD-INSTANCE
  DEFSTRUCTCLASS ENDF FIXNUM-CLASS FLONUM-CLASS MAP-CLASS-HIERARCHY
  NUMBER-CLASS OBJECT-CLASS PAIR REMMETHOD SUBCLASS-OF-CLASSP
  SUBCLASS-OF-CLASS-SYMBOL-P SUBINSTANCE-OF-CLASSP SUBINSTANCE-OF-CLASS-SYMBOL-P
  SYMBOL-CLASS TAIL 

If you feel you still need classes to run old software, please send mail
to BUG-LISPM-SCRC@SCRC.  If there is enough demand, a compatibility
package will be written using flavors (it's not hard).  If you just need
for <- to work and don't want to change it to SEND, you can define <-
as a macro that expands into a SEND.

[8] Compiler warnings work differently; several editor commands changed.

Compiler warnings work differently.  In old systems, compiler
warnings were all directed to a special Zmacs buffer named "Compiler
Warnings", which was always present.  In System 210, warnings are kept
in an internal database, and several functions and editor commands are
provided that allow you to inspect and manipulate this database in
various ways.  (Not all of the following is new to System 210, but it
has never all been documented in one place before.)

The database of compiler warnings is organized by pathname: warnings
that were generated during the compilation of a particular file are all
kept together, and this body of warnings is identified by the generic
pathname of the file being compiled.  Any warnings that were generated
while compiling some function not in any file (for example, by using the
COMPILE function on some interpreted code) are stored under the
"pathname" NIL.  For each pathname, the database has entries each of
which associates the name of a function (or of a flavor) with the
warnings generated during its compilation.  The database starts out
empty when you cold-boot.  Whenever you compile a file, buffer, or
function, the warnings generated during its compilation are entered into
the database.  If you recompile a function, the old warnings are
removed, and any new warnings are inserted.  So, if you get some
warnings and then you fix the mistakes and recompile everything, the
database will be empty again.

When warnings are generated, they are both printed out for you to see
and stored in the database.  If the value of the special variable
SUPPRESS-COMPILER-WARNINGS is not NIL, the printing of the warnings
will not happen; however, the warnings will still go into the database
for later examination.

The database has a printed representation.  PRINT-COMPILER-WARNINGS
produces this printed representation from the database, and
COMPILER:LOAD-COMPILE-WARNINGS updates the database from a saved
printed representation.  Here are the details:

PRINT-COMPILER-WARNINGS &OPTIONAL files (stream STANDARD-OUTPUT)
    Prints out the compiler warnings database.  If "files" is NIL (this is
    the default), print the entire database.  Otherwise, "files" should be a
    list of generic pathnames, and only the warnings for the specified files
    are printed.  (NIL may be a member of the list, too, in which case
    warnings for functions not associated with any file are printed as well.)
    The output is sent to "stream"; you could use this to send the results
    to a file.

COMPILER:LOAD-COMPILER-WARNINGS file &OPTIONAL (flush-old-warnings T))
    "file" should be the pathname of a file containing the printed
    representation of the compiler warnings regarding the compilation
    of one or more files.  These warnings are loaded into the database.
    If "flush-old-warnings" is T (this is the default), any existing
    warnings in the database for the files in question will be completely
    replaced by the warnings in "file".  If "flush-old-warnings" is NIL,
    the warnings in "file" will be added to those already in the database.

The printed representation of a set of compiler warnings is sometimes
stored in a file.  You can create such a file using
PRINT-COMPILER-WARNINGS, but the usual way they're created is by
MAKE-SYSTEM, when it is given the :BATCH option.  The default type for
such files is "CWARNS"; that is, a file that contains compiler warnings
is usually called "SOMETHING.CWARNS".  (On VAX/VMS file systems, the
"CWARNS" type is translated to "CWN".  On Unix file systems, it is
translated to "cw".)

There are also several Zmacs commands that deal with the database.

Meta-X Compiler Warnings
    Print all of the compiler warnings database into a buffer called
    "Compiler Warnings", creating the buffer if it doesn't exist already,
    and switch to that buffer.  This lets you to peruse the compiler
    warnings, scrolling around and doing text searches through them.

Meta-X Edit Compiler Warnings
    This command lets you edit all of the functions whose compilation
    generated a warnings message, so that you can swiftly go to all the
    places in which various errors need to be fixed.  Zmacs first asks you
    about each file mentioned in the database, asking you whether you are
    interested in editing the warnings for that file or not.  This lets you
    pick the subset of files that you're interested in working on.  It then
    splits the Zmacs frame into two windows; the upper window displays a
    warning message, and the lower one displays the source code whose
    compilation caused the warning.  After you have finished editing each
    function, the Control-. command gets you to the next warning: the top
    window scrolls to show the next warning and the bottom window displays
    the function associated with this warning.  Successive Control-.
    commands will take you through all of the warning messages for all of
    the files that you said you were interested in.  When you're done, the
    last Control-. puts the frame back into its one-window configuration.

Meta-X Edit File Warnings
    This is like Edit Compiler Warnings, except that instead of prompting
    you with the name of each file in the database, it asks you for a file
    name.  You should give the file name of the program whose warnings you
    wish to edit; you can give either the source file or the compiled file
    (it doesn't matter).  Only warnings for this file will be edited.  If
    the database does not have any entries for the file you specify, this
    command will prompt you for the name of a file that contains the warnings,
    just in case you know that the warnings are stored in a file somewhere.

Meta-X Load Compiler Warnings
    This is just like the COMPILER:LOAD-COMPILER-WARNINGS function.  It
    prompts you for the name of a file containing the printed representation
    of some compiler warnings, and it loads them into the database.  This
    command always passes T as the the "flush-old-warnings" argument; that
    is, it replaces the old warnings rather than merging with them.  The
    default file type is "CWARNS" and the default version is :NEWEST (the
    latest version).

The symbols COMPILER-WARNINGS-BUFFER and CONCATENATE-COMPILER-WARNINGS-P
are now obsolete and have been removed from GLOBAL.

[9] HOSTAT must be aborted with Control-Abort now, like everything else.

During the operation of the HOSTAT function (or the Terminal H
command), it used to be true that the typing of any character would
cause the HOSTAT to stop.  In System 210, HOSTAT was made like everything
else: if you want to stop it in the middle, you use Control-Abort.  HOSTAT
has a new prompt message to remind you of this.

[10] The Meta-X Set Package in Zmacs has been changed.

The Zmacs command Meta-X Set Package has been changed.  Before System
210, Meta-X Set Package changed the current package and changed the
package associated with the file, but did not affect the attribute list
(-*- line) of the buffer.  As a result, you might do a Meta-X Set
Package to a new package, but then when you subsequently read the file
into the editor, it would still be associated with the old package
because the attribute list had not been updated.  Many people found this
confusing.

As of System 210, Meta-X Set Package always changes the current package,
and then it asks you whether you want the file's associated package to
be changed as well.  If you say "no", nothing else is changed.  If you
say "yes", the package associated with the file is updated, and the
attribute list of the file is updated.

For those users who feel unnecessarily burdened by this question and are
not worried about the possibility of the confusion mentioned above, a
new variable is provided to control the new behavior.  You can set this
variable in your INIT file if you want.

ZWEI:*SET-PACKAGE-UPDATE-ATTRIBUTE-LIST*      [Variable]
    This variable controls the behavior of the Meta-X Set Package command.
    If its value is :ASK (this is the default), Meta-X Set Package will
    ask you whether to update the package of the file as well.  If its value
    is NIL, Meta-X Set Package will never update the package of the file.
    If its value is T, Meta-X Set Package will always update the package of
    the file.

[11] The "Require Subjects on Outgoing Messages" Zmail option has changed.

The "Require Subjects on Outgoing Messages" option to Zmail works
somewhat differently now.  This option appears as one of the lines in
the big Choose Variable Values window in Zmail's Profile mode.  It has
four possible values:

 o  No: Zmail will not do anything special.  This is the default.

 o  Yes: Zmail will insist that you attach a "Subject" line to some or any
    of the messages that you send.  First of all, it encourages you to give
    a Subject, by putting an empty "Subject:" line in the "Headers" window.
    (If the message is a reply, the Subject line will be derived from the
    subject of the original message, as usual.) Then, if you try to send the
    message without providing a subject, Zmail will prompt you for a subject
    in the mini-buffer.

 o  On bug reports:  This is the same as "Yes" except that it only applies
    to messages that are bug reports (that is, messages that you send by clicking
    Middle on Mail, or clicking right on Mail and then clicking on Bug).
    Mail other than bug reports will not be affected.

 o  Initial but not required:  Zmail reminds you to put in a subject line
    to your message, by make sure there is a "Subject:" line in the "Headers
    window, but it does not complain if you try to send the message without
    actually supplying a subject.

[12] Converse (the interactive message system) has been changed.

In System 210, when a message arrives, a little window is popped up and
the message is displayed in it, just as System 78 used to do.  You can
control what happens when a message arrives by setting the following
variable in your INIT file:

ZWEI:*CONVERSE-MODE*             [Variable]
    The value of ZWEI:*CONVERSE-MODE* controls what happens when an
    interactive message arrives over the network at your machine.  It should
    have one of the following values:

    :POP-UP  (This is the default.)
      A small window pops up at the top of the screen, displaying the message.
      You are asked to type "R" (for Reply), "N" (for Nothing), or "C" (for
      Converse).  If you type "R", you can type in a reply to the message
      inside the little window.  When you type End, this reply will be sent
      back to whomever sent the original message to you, and the little window
      will disappear.  If you type "N", the little window will just disappear
      immediately.  If you type "C", the Converse window will be selected, as
      if ZWEI:*CONVERSE-MODE* were :AUTO (see below).

    :AUTO
      The Converse window is selected.  This is the big window that shows you
      all of your conversations, letting you see everything that has happened,
      and letting you edit your replies with the full power of the Zwei
      editor.  With this window selected, you can reply to the message that
      was sent, send new messages, participate in other conversations, and so
      on.  You can exit with the End or Abort commands (End sends a message
      and exits, whereas Abort just exits), or you can select a new window by
      any of the usual means (such as the Terminal or System keys).

    :NOTIFY
      A notification is printed, telling you that a message arrived, and who
      sent the message.  If you want to see the message, you can enter
      Converse with System C, or use the PRINT-SENDS function.  If you want to
      reply to the sender, you can enter Converse or use the QSEND function
      manually.

    :NOTIFY-WITH-MESSAGE
      A notification is printed, including the entire contents of the message
      as well as telling you who sent the message.  If you want to reply to
      the sender, you can enter Converse or use the QSEND function manually.

The following two variables also control Converse's behavior, and you can
set them in your INIT file.

ZWEI:*CONVERSE-APPEND-P*         [Variable]
    If the value is NIL (which is the default), a new message will be added to
    the beginning (prepended) of its conversation. If the value is not NIL, a
    new message will be added to the end (appended) of its conversation.
    PRINT-SENDS is not affected by this variable; it always prints all messages
    in reverse chronological order.

ZWEI:*CONVERSE-BEEP-COUNT*       [Variable]
    The value is the number of times to beep when a message arrives.  The default
    value is 2.  This beeping is only done if the Converse window is exposed, or if
    the value of ZWEI:*CONVERSE-MODE* is :POP-UP or :AUTO.  (Otherwise,
    notification will be used to tell you about the message, and the usual beeping
    that accompanies notification will be heard.)

If you are using the new rubout handler (mentioned earlier), it will be used
while you type in a message to QSEND and while you type in a reply in :POP-UP
mode.  So if you use the default mode (:POP-UP) and turn on the new rubout
handler, you will get some editing power while replying, although not as much
as with full Converse since the latter uses Zwei.



CHANGES THAT ONLY AFFECT ADVANCED USERS

[1] The "compile driver" has been changed incompatibly.

The "compile driver" has been changed incompatibly in several ways.
The software affected is mostly internal and should not affect most
users.  However, if you have an advanced tool that interfaces to the
compiler, or uses the functions in Zmacs that do compilation or
evaluation of sections of buffers (in particular the function
ZWEI:EVAL-PRINT), that tool may need to be changed.  None of the
software that was changed is currently documented, so for details of the
change, see the source code.

[2] SI:RESET-TEMPORARY-AREA requires you to declare the area to be temporary.

Some programs use the dangerous SI:RESET-TEMPORARY-AREA feature to
deallocate all Lisp objects stored in a given area.  As before, use of
this technique is not recommended, since if any outstanding references
to objects in the area do exist, gross system failure can result.
If you are not now using temporary areas, you can ignore the following
change.

However, those programs that do use the feature must now declare any
areas that are to be mistreated this way.  When you create a temporary
area with MAKE-AREA, you must give the :GC keyword and supply the value
:TEMPORARY.  (This marks the area as being :STATIC, as well; all
temporary areas are considered static by the garbage collector.)
SYS:RESET-TEMPORARY-AREA now signals an error if its argument has not
been declared temporary.  

[3] Some useless or obsolete symbols have been removed from GLOBAL.

Several useless or obsolete symbols have been removed from the GLOBAL
package, in addition to other symbols mentioned earlier.  The following
symbols were removed:

  AREA-NUMBER ART-Q-LIST-ARRAY BEGF GET-FROM-ALTERNATING-LIST LOCATIVE-POINTER
  PKG-LOAD-MAP PROCESS-ERROR-STOP-PROCESS PROCESS-PLIST PUT-ON-ALTERNATING-LIST
  TRAP-ENABLE UNWIND-PROTECT-TAG UNWIND-PROTECT-VALUE

[4] The TV:SHEET- functions are now obsolete.

Various internal reorganizations were made within the window system.
The undocumented TV:SHEET- functions are now obsolete; most still exist,
but the compiler will warn you that they are obsolete if it sees any
calls to them.  If you were using them, use message-passing instead.

[5] CHAOS:SHOUT removed.

The function CHAOS:SHOUT, mentioned in the System 78 release notes,
has been removed.  It was useful for sending announcements to all of
the users at a site, but by using the interactive message system, it
elicited a reply from each recipient, which was inappropriate.  It is
replaced by the new function CHAOS:NOTIFY-ALL-LMS, which is mentioned
below.


UPWARD-COMPATIBLE CHANGES

[1] The command interface to Peek has changed slightly.

To switch Peek from one mode to another, you used to type a single
character.  This still works.  However, Peek now also has a menu at the
top, with one item for each of Peek's modes.  The item for the
currently-selected mode is highlighted (in reverse video).  If you click
on one of the items with the mouse, Peek will switch to that mode.  The
Help message is now a Peek mode, instead of being printed specially, and
Peek starts out in the Help mode.

Also, the "Z" command in Peek has been changed to use units of seconds,
instead of units of 60ths of a second.

[2] The names of process wait states have been changed.

The names of process wait states are the things you see in the who-line
(at the bottom of the screen) such as "TYI", "NETO", and so on; you also
see them in Peek when you look at the display of processes.  They are
intended to explain the state of a process: whether it is running,
stopped, or waiting for something.  If the process is waiting, this name
tells you what it is waiting for.  They've been changed to try be more
consistent in their spelling and capitalization; for example, "NETO" has
been changed to "Net Out".  Since these names are only for informational
purposes (programs don't look at them), this change should not cause
programs any problems.

[3] New functions CHAOS:NOTIFY-ALL-LMS and CHAOS:NOTIFY.

CHAOS:NOTIFY-ALL-LMS &OPTIONAL message
    "message" should be a string; if it is not provided, the function prompts for
    for a message.  The message is sent to all Lisp Machines at your site
    (based on the value of the list SI:MACHINE-LOCATION-ALIST).  Each
    recipient will receive the message as a notification, rather than as an
    interactive message.

CHAOS:NOTIFY host &OPTIONAL message
    "host" should be a host (the host name, as a string, or a host object).
    "message" is a string; if it is not provided, the function prompts for
    for a message.  The message is sent to the specified host.  The
    recipient will receive the message as a notification, rather than as an
    interactive message.

[4] Package declarations can be placed in the attribute list of a file.

It is now possible to put the package-declaration of a package into
the attribute list (the -*- line) of a file.  In previous releases, it
was necessary to construct a "package declaration" for a package; the
declaration for FOO would reside in its own little file, and then each
file to be loaded into package FOO would say "Package: FOO" in its
attribute list.  As of system 210, the contents of the package declaration
may now reside in the attribute lists of the files themselves.  To do so,
the Package attribute should have the syntax of a Lisp list.  Example:

    -*- Mode: Lisp; Package: (FOO GLOBAL 1000); Base: 8 -*-

The first element of the list is the name of the package; the second is
the name of the superior package (this is almost always GLOBAL); the
third is the initial estimate of the number of symbols in the package
(using 1000 is safe; it will grow by itself if necessary, so this number
is not critical).  Options such as SHADOW and EXTERN may be included in
the list as subsequent items; the syntax of this list is analogous to
that of the PACKAGE-DECLARE special form except that the "file alist",
which is obsolete anyway, is omitted.

If several files get loaded into the same package, they should all have
exactly the same Package attribute.  If the declaration of the
package makes much use of special options such as SHADOW and EXTERN, it
may be more convenient to do things the old way, putting the package
declaration into its own file, to prevent accidents in which changes
to the package declaration aren't faithfully duplicated in all of the
source files.  Furthermore, if you already have a little file to contain
a system declaration, then that's a perfectly good place to put the package
declaration as well; whether you want to use the new syntax is a matter
of personal preference.  The new syntax is primarily useful when you
have a program small enough that it fits in one file, so that there
isn't any system declaration.

This new feature is NOT backward-compatible.  Files using this new
syntax cannot be loaded into systems that predate System 210.
Furthermore, when a file using the new syntax is compiled, the compiled
(QFASL) file's attribute list will also use the new syntax, and so such
compiled files cannot be loaded into systems that predate System 210
either.  (MIT System 85 is compatible with System 210 in this respect,
but previous MIT systems are not.)

[5] New feature to make DISK-RESTORE faster.

There is a new, experimental feature that makes DISK-RESTORE faster.
It has a drawback: paging performance may be degraded for a while as you
start to use the machine after it has been DISK-RESTOREd in this way.
The effects of the tradeoff are unclear, and so the feature is being
offered experimentally so that we can find out whether people like it.

When you use DISK-RESTORE in the normal way, the entire contents of the
LODn partition on the disk are copied into the PAGE partition, in large
blocks.  It is this copying that takes most of the time in DISK-RESTORE.
DISK-RESTORE now takes a second argument, called FULL-RESTORE-P, whose
value is T by default; if you give NIL explicitly as the second
argument, the band will be restored in the new way.  Instead of copying
the LODn partition, the virtual memory system gets set up in such a way
that the first time a page is referenced, the page will be read in from
the saved LODn partition directly; when the page is paged out, it will
be put into the PAGE partition as usual, and any new pages will be
created in the PAGE partition.  The reason that this can hurt
performance is that page faults satisfied from the LODn partition cause
longer disk seeks, which take more time.  As a result, the first time
you try to use any particular body of software such as the editor or the
compiler, the paging activity needed to page in that body will take
longer.

If you use DISK-RESTORE, try giving it a second argument of NIL, and see
whether you think that the overall change in performance is an
improvement or a drawback.  If you have any comments, please let us
know.

[6] New utility: the "band compressor".

There is a new utility program called the "band compressor".  It lets
you remove all garbage from a world load and then "compress" the
remaining objects, thus creating a physically smaller band.  Having
smaller bands is mainly useful to machines that have small disk drives
(such as T-80s).  The documentation is in a separate file, stored at
all of the following places:

  AI: LMDOC; COMPRE >
  EE:LIB:<LISPM.DOCUMENTATION>COMPRESS-DOC.TEXT
  POINTER:>LMSYM>Compress-Doc.Text

[7] New Zmacs commands: Meta-X Start Patch and Meta-X Abort Patch.

There is a new Zmacs command useful if you change your mind in the
middle of creating a patch to a patchable system.  If you have given the
m-X Add Patch command (once or many times) but have not yet done m-X
Finish Patch to install the patch, and you decide that you don't really
want to make the patch after all, use the new m-X Abort Patch command.
This will deallocate the minor version number that m-X Add Patch
allocated, and it will tell the editor that you are no longer "in the
middle of making a patch" (so that the next time you do m-X Add Patch,
Zmacs will start a new patch instead of appending to the one in
progress).

There is another new command called Meta-X Start Patch which starts a
new patch but doesn't move any Lisp forms into the patch file.  Usually,
you don't have to use this command, since Meta-X Add Patch starts up a
new patch if there isn't one in progress.  However, sometimes you want
to create a patch file that doesn't contain any Lisp code from any other
file; the patch file might contain only an ad-hoc form to be evaluated,
rather than any redefinitions.  If you just want to create a fresh patch
file and type some forms into it, then you can use Meta-X Start Patch,
select the patch buffer, and start editing it.  You can do Meta-X Add
Patch later for the same patch, if you want to.

[8] New functions: SEND and LEXPR-SEND.

There is a new function called SEND, which is the new official way to
send messages to objects.  SEND should be used in the same way that
FUNCALL has been used up to now: the first argument to SEND is a Lisp
object, the second is a message name (a symbol), and the rest are
arguments.  The object is sent the given message with the given
arguments.

Currently, SEND just does exactly the same thing as FUNCALL.  However,
in a future release, it will be possible to send messages to objects of
any data type, and SEND will be changed upward-compatibly to make this
work.  Also, it is generally agreed that SEND is a better name than
FUNCALL for message sending.

There is also a function called LEXPR-SEND, which is to SEND as
LEXPR-FUNCALL is to FUNCALL.  The first two arguments to LEXPR-SEND are
the object and the message name, and the rest are arguments to the
handler except that the last argument to LEXPR-SEND is actually a list
of arguments to be passed to the handler.  So, for example,

    (SEND SOME-WINDOW ':SET-EDGES 10 10 40 40)

does the same thing as

    (SETQ NEW-EDGES '(10 10 40 40))
    (LEXPR-SEND SOME-WINDOW ':SET-EDGES NEW-EDGES)

(By the way, there isn't any SEND-SELF or LEXPR-SEND-SELF, since because
the new implementation of flavors, there would be no particular
performance benefit to be gained.   To send a message to SELF, just
pass SELF as the first argument to SEND.)

[9] New, experimental rubout handler available for you to try.

There is a new, experimental rubout handler.  The rubout handler is
the software module that lets you use the Rubout key when you are being
prompted for a Lisp form or a line of text; you most frequently
encounter the rubout handler in Lisp Listeners.  The existing rubout
handler is very simple, having only a few commands (such as Rubout and
Clear Input).

To try out the new rubout handler, do (TV:RH-ON).  It is
upward-compatible with the old rubout handler, but has a lot of new
commands.  Most of its commands are analogous to commands in the Zwei
editor; for example, Control-F and Control-B move the cursor forward and
backward by one character.  Type "Help" for a complete list of commands.

The rubout handler has some new commands that are not present in the
editor.  The most interesting commands are c-C, m-C, and Status.  These
commands deal with a "ring" buffer of pieces of text, similar to but
distinct from the "kill" buffer.  This new ring is called the "input"
buffer, and it contains a history of the most recent things that you
have typed.  c-C and m-C work analogously to c-Y and m-Y, but they use
this "input" buffer instead of the "kill" buffer.  Status (a key near
the upper-right corner of the keyboard) displays the entries of the
"input" buffer.  These commands if you want to retype something that you
typed before, possibly with some modifications.

The rubout handler does not actually use internal functions of Zwei to
do its editing.  As a result, it is not completely compatible with the
analogous editor commands.  The most striking difference is that the
rubout handler's c-m-F, c-m-B, c-m-K, and c-m-Rubout commands work
simply on areas of text delimited by pairs of parentheses; they do
not understand any of the subtle aspects of Lisp syntax, and they do
not recognize Lisp symbols or numbers as objects.  This problem will
be fixed in a future release; it's one of the reasons that the new
rubout handler is experimental.

You may want to put a (TV:RH-ON) in your INIT file, so that it
will be turned on whenever you log in.  If you think something is wrong
with it and you need to turn it off and go back to the old rubout
processor, you can do (TV:RH-OFF).  (If the rubout processor seems so
broken that you can't even type Lisp forms, type Terminal Call to get
into the "cold load stream", type (TV:RH-OFF), then type Abort to get
out of the cold load stream, and type Terminal Clear-Screen to fix up
the display.  This should not be necessary -- the new rubout handler has
been tested extensively -- but bugs have been known to show up in tested
software before.)

[10] New function: TIME-INCREMENT.

There is a new function called TIME-INCREMENT, which joins TIME-LESSP
and TIME-DIFFERENCE.  These three functions do a special kind of
wrap-around arithmetic used to manipulate the values returned by the
TIME function.  TIME-LESSP and TIME-DIFFERENCE are explained in the Lisp
Machine Manual (q.v.).  TIME-INCREMENT takes a time value and a number
of sixtieths of a second, and adds them together, wrapping around the
result as necessary to produce a new time value.

[11] New function: UPTIME.

There is a new function called UPTIME, which is a variant of HOSTAT.
UPTIME queries all hosts on the Chaosnet, asking them for their
"uptime"; that is, each host responds saying for how long it has been up
and running.  UPTIME prints out the results.  If UPTIME reports that a
host is "not responding", this either means that the host is not
responding to the network, or that it does not support the UPTIME
protocol.  Note that Lisp Machine running systems earlier than 210 do
not support the UPTIME protocol; don't be disappointed if nothing
answers.  This protocol is very simple and will eventually be supported
by all operating systems that support the Chaosnet.

(The Chaosnet protocol itself works just like the TIME protocol, except
that the number returned is the uptime of the server expressed in 60ths
of a second, instead of being the time of day expressed in seconds, and
the contact name is "UPTIME" rather than "TIME".)

[12] New global function: PRINT-HERALD.

The function PRINT-HERALD has been made global (it used to be in the
SYSTEM-INTERNALS package).  This function prints out the "herald"
message, which is what the machine prints when it is cold-booted
to tell you what version of the system is running.  It optionally
takes one argument, which is the stream on which to print the herald;
it defaults to STANDARD-OUTPUT.  (The function PRINT-LOADED-BAND,
which printed out the message in very old system releases (pre-78),
is no longer global.)

[13] New function: STORE-CONDITIONAL.

STORE-CONDITIONAL is a new primitive for the construction of
inter-process communication tools.  STORE-CONDITIONAL takes three
arguments: a locative (which addresses some cell), an old value (any
Lisp object), and a new value (any Lisp object).  It checks to see
whether the cell contains the old value, and, if so, it stores the new
value into the cell.  The test and the set are done as a single atomic
operation.  STORE-CONDITIONAL returns T if the test succeeded and NIL if
the test failed.  You can use STORE-CONDITIONAL to do arbitrary atomic
operations to variables that are shared between processes.  For example,
to atomically add 3 into a variable X:

    (DO ()
        ((STORE-CONDITIONAL (LOCF X) X (+ X 3))))

The first argument is a locative so that you can atomically affect any
cell in memory; for example, you could atomically add 3 to an element of
an array or structure.  The first argument may also be a cons, in which
case the cell that is the CAR of the cons will be used.

(For a long time there has been a function called %STORE-CONDITIONAL
that worked almost the same way; however, it was a special subprimitive
that did not check its data type.  The new function, without the "%" in
its name, is a normal Lisp function that checks the type of its first
argument appropriately.)

[14] New function: FUNDEFINE.

There is a new function called FUNDEFINE.  It takes one argument,
which should be a function spec.  It removes the definition of that
function spec, if any.  (For example, if the argument is a symbol,
it does the same thing as FMAKUNBOUND.)

[15] New function: UNDELETEF.

There is a new function called UNDELETEF.  It works just like
DELETEF except that it "undeletes" the file; this is only meaningful
for file systems that support undeletion, such as TOPS-20 and the
Lisp Machine file system.

[16] New function: SI:FLAVOR-ALLOWED-INIT-KEYWORDS.

There is a new function called SI:FLAVOR-ALLOWED-INIT-KEYWORDS.  It
takes one argument, a flavor name (which should be a symbol).  It return
a list of all of the symbols that are valid init-options for the flavor,
sorted alphabetically.  This function is primarily useful to people to
call in order to get information, rather than programs.  You can use
this to help remember the name of an init option, or to help you write
documentation about a particular flavor.

[17] New function: SI:CLEAR-BAND.

There is a new function SI:CLEAR-BAND.  It takes one argument, which
specifies a partition on the disk in the manner of the other "band"
functions such as SI:RECEIVE-BAND (a string or symbol being the name of
the partion, or a number "n" to mean "LODn").  It writes zeroes over the
entire band on the disk.  It optionally takes two more arguments,
"subset-start" and "subset-n-blocks"; if these are given, only the
specified contiguous subset of the band is cleared.  The partition's
comment field in the disk label is cleared as well.  Like all functions
that clobber disk partitions, it asks for confirmation first.  This
function is occasionally useful for getting rid of the remnants of an
old file system in a FILE partition, or something; in general it is not
needed.

[18] New global function: VIEWF.

The function VIEWF has been made global (it used to be in the
FILE-SYSTEM package).  VIEWF takes one argument, a filename, and prints
that file.  The optional second argument is the stream to which to print
the file; it defaults to STANDARD-OUTPUT.  (The optional third argument
is passed as the LEADER argument to STREAM-COPY-UNTIL-EOF; this should
not be of interest to most users.)  The name VIEWF is analogous with
DELETEF, RENAMEF, and so on.

[19] New functions to handle intervals of time.

There are new functions for reading and printing time intervals.
They convert between strings of the form "3 minutes 23 seconds" and
fixnums representing numbers of seconds.  Here are the details:

TIME:PRINT-INTERVAL-OR-NEVER interval &OPTIONAL (stream standard-output)
    "interval" should be a non-negative fixnum, or NIL.  This function
    prints "interval"'s representation as a time interval onto "stream".
    If "interval" is NIL, it prints "Never".

TIME:PARSE-INTERVAL-OR-NEVER string &OPTIONAL start end
    "string" is the character string representation of an interval of time.
    "start" and "end" specify a substring of "string" to be parsed; they
    default to the beginning and end of "string", respectively.  The
    function returns a fixnum if "string" represented an interval, or NIL if
    "string" represented "never".  If "string" is anything else, an error is
    signalled.  Here are some examples of acceptable strings:

        "4 seconds"   "4 secs"   "4 s"
        "5 mins 23 secs"  "5 m 23 s"  "23 SECONDS 5 M"
        "3 yrs 1 week 1 hr 2 mins 1 sec"
        "never" "not ever" "no" ""

    Note that several abbreviations are understood, the components may be in
    any order, and case (upper versus lower) is ignored.  Also, "months" are
    not recognized, since various months have different lengths and there is
    no way to know which month is being spoken of.  This function will
    always accept anything that was produced by TIME:PRINT-INTERVAL-OR-NEVER;
    furthermore, it will return exactly the same fixnum (or NIL) that was printed.

TIME:READ-INTERVAL-OR-NEVER &OPTIONAL (stream standard-input)
    This function reads a line of input from "stream" (using READLINE) and
    then calls TIME:PARSE-INTERVAL-OR-NEVER on the resulting string.

[20] New feature to allow shift keys to replace double-clicking.

For those who don't like double-clicking of mouse buttons, there is a
new feature.  If you click a mouse button while holding down any of the
keys labelled SHIFT, CONTROL, or HYPER, your click will be interpreted
as a double-click.  Furthermore, if you set TV:MOUSE-DOUBLE-CLICK-TIME
to NIL, then double-clicking in the usual way will be disabled, and the
system will no longer wait, after you click, to see whether you are
going to click again.  This can improve mouse-click response time.

This feature is under control of the following variable.  You can set
this variable to NIL if you want to turn off the new behavior (you might
want to LOGIN-SETQ it in your INIT file).

TV:*MOUSE-INCREMENTING-KEYSTATES*           [Variable]
    The value is a list of the names of those shifting keys which, if held
    down, will make a single-click of the mouse into a double-click.  Its
    default value is (:SHIFT :CONTROL :HYPER).

[21] New feature in Choose-Variable-Values.

The Choose-Variable-Values facility has been extended so that the
variable specification can be a locative or a cons cell instead of a
special variable.  This lets arbitrary cells in the machine be examined
and modified by Choose-Variable-Values.  (If a cons cell is passed, its
CAR is the memory location used, and the long form of specification
must be used to avoid syntactic ambiguity.)

[22] Hosts not in the host table can be named, as CHAOS|nnnn.

It is now possible to name hosts that are not in the host table.  The
address CHAOS|nnnn, where "nnnn" is an octal numeral (of any length),
refers to the host with that number on the Chaosnet.  For example,
CHAOS|401 and CHAOS|17003 are valid addresses.  Normally, all hosts at
your site are in the host table and have mnemonic names, so you
shouldn't need to use this syntax.  However, it comes in useful if your
host table is incorrect or out-of-date, in which case you may need
to communicate with a host that's not in your host table.

[23] New flavor: TV:CENTERED-LABEL-MIXIN.

There is a new flavor in the window system called
TV:CENTERED-LABEL-MIXIN.  If you mix this with any of the flavors that
create labels, the text of the label will be centered within the edge of
the window (instead of being aligned against the left edge).  Also,
labels can now have more than one line of text (by containing Return
characters).

[24] New option to MAKE-ARRAY: :INITIAL-VALUE.

There is a new option to MAKE-ARRAY, called :INITIAL-VALUE.  The
value of this option will be made the initial value of every element of
the array.  Example:

    (MAKE-ARRAY 5 ':TYPE 'ART-STRING ':INITIAL-VALUE #/A) => "AAAAA"

[25] New functions: SI:DESCRIBE-PARTITIONS and SI:DESCRIBE-PARTITION.

There are two new functions called SI:DESCRIBE-PARTITIONS and
SI:DESCRIBE-PARTITION.  These functions print out descriptions
of the contents of the partitions on the disk.  

SI:DESCRIBE-PARTITIONS &OPTIONAL (unit 0)
    Print out descriptions of all of the partitions on the given "unit"
    to STANDARD-OUTPUT.

SI:DESCRIBE-PARTITION partition &OPTIONAL (unit 0)
    Print out the description of the specified "partition" on the given "unit"
    to STANDARD-OUTPUT.  "partition" should be the name of a partition (as a
    string or symbol, or as a number "n" to indicate "LODn".

The description of any partition starts with the line that would be
printed by PRINT-DISK-LABEL, giving the partition's name, address, size,
and comment.  For partitions containing world-loads, the description
includes four other data:

 o  Valid Size: The size of the world-load itself.  (The partition is generally larger
    than the world load, and the rest of the partition is unused.)
    
 o  System Version: The major version number of System in the world load.
    (For example, for the world load being distributed now, this would be 210.)
    
 o  Desired Microcode: The version of the microcode with which this world load
    expects to be booted.
    
 o  GC Generation Count: The number of times that a either a garbage
    collector "flip" has happened, or a band compression has happened.  This
    number is generally 0 for uncompressed bands and 2 for compressed bands.

For partitions containing microloads, the description tells how many
entries the microload contains for various internal processor memories.
The most interesting piece of information that it tells you about
microloads is the version number of the microload; this is normally in
the disk label comment as well, but the comments can easily be changed
to any text string, whereas the description is based on examination of
the actual contents of the partition.

Future system releases may print out more information about these and
other kinds of partitions.

[26] New special forms: VARIABLE-LOCATION, VARIABLE-BOUNDP, VARIABLE-MAKUNBOUND.

VALUE-CELL-LOCATION on local variables is now considered obsolete.
It used to be that the only way to generate a locative pointer to the
memory cell associated with a local variable called A was with the form
(VALUE-CELL-LOCATION 'A).  This is actually quite inelegant, since
VALUE-CELL-LOCATION is really a function that has to do with symbols
(special variables) in particular rather than variables in general (see
section 3.1 of the Lisp Machine Manual (page 14 of Revision 4) for a
complete explanation of the distinction.)  This form will continue to
work, but the compiler will issue a warning, telling you that it is
obsolete.  There is a new special form to replace it:

VARIABLE-LOCATION variable      [Special Form]
    "variable" should be a variable (it is not evaluated).  It may be any
    kind of variable: a local variable, a special variable, or an instance
    variable.  VARIABLE-LOCATION returns a locative pointer to the memory
    cell that holds the value of the variable.

If A is a local or instance variable, and you use the obsolete
(VALUE-CELL-LOCATION 'A) form, the compiler will issue a warning and
convert it into the proper VARIABLE-LOCATION form.  So if you have
programs that use this form, they will continue to work.  Similarly,
the compiler will issue warnings for obsolete uses BOUNDP and MAKUNBOUND,
and generate code that will work.

(VALUE-CELL-LOCATION 'A) is still a perfectly good form when A is a
special variable.  It behaves slightly differently from the form
(VARIABLE-LOCATION A), in the case that A is a variable "closed over" by
some closure (see Chapter 11 of the Lisp Machine Manual).
VALUE-CELL-LOCATION will return a locative pointer to the "internal"
value cell of the symbol (the one that holds the invisible pointer,
which is the real value cell of the symbol), whereas VARIABLE-LOCATION
will return a locative pionter to the "external" value cell of the
symbol (the one pointed to by the invisible pointer, which holds the
actual value of the variable).

You can also use LOCF on variables (this has always been true).  (LOCF A)
now expands into (VARIABLE-LOCATION A).

There are two more new special forms:

VARIABLE-BOUNDP variable            [Special Form]
    "variable" should be a variable (it is not evaluated).  It may be any
    kind of variable: a local variable, a special variable, or an instance
    variable.  VARIABLE-BOUNDP returns T if the variable is bound, and
    NIL if the variable is not bound.  Note: local variables are always
    bound; if "variable" is local, the compiler will issue a warning and
    replace this form with T.

VARIABLE-MAKUNBOUND variable        [Special Form]
    "variable" should be a variable (it is not evaluated).  It may be any
    kind of variable: a local variable, a special variable, or an instance
    variable.  VARIABLE-MAKUNBOUND makes the variable be unbound, and returns
    "variable".  Note: since local variables are always bound, they cannot
    be made unbound; if "variable" is local, the compiler will issue a warning.

If A is a special variable, (BOUNDP 'A) is the same as (VARIABLE-BOUNDP A),
and (MAKUNBOUND 'A) is the same as (VARIABLE-MAKUNBOUND A).

Note: along with these changes, LET-GLOBALLY has been fixed to deal with
unbound variables correctly.  If LET-GLOBALLY is entered and one of the
variables is unbound, then it will be made unbound again when the
LET-GLOBALLY is exited, whether it is exited normally of abnormally.

[27] New function: SI:FULL-GC.  New initialization list: FULL-GC.

There is a new function called SI:FULL-GC, and a new initialization
list accessed through the FULL-GC keyword to ADD-INITIALIZATION.

SI:FULL-GC &OPTIONAL gc-static-areas
    This is the simplest available functional interface to the garbage
    collector.  SI:FULL-GC does an immediate, complete, non-incremental
    garbage collection.  When it is over, no further garbage collection will
    happen.  If "gc-static-areas" is not NIL, a set of system areas that are
    normally "static" (not subject to garbage reclamation) will be treated
    as if they were dynamic for the duration of this garbage collection.
    Before it does any collection, SI:FULL-GC runs the forms on the FULL-GC
    initialization list.  The running of the initialization list, and the
    entire garbage collection are done without multiprocessing (inside a
    WITHOUT-INTERRUPTS form), and so the machine essentially "freezes" and
    does nothing but garbage collection for the duration.  This operation
    takes about 20 minutes; Using "gc-static-areas" does not appear to take
    significantly longer.

There is a new system initialization list called the FULL-GC initialization
list.  You can add forms to it by passing the FULL-GC keyword in the list of
keywords that is the third argument of ADD-INITIALIZATION.  The FULL-GC
initialization list is run just before a full garbage collection is performed.
All forms are executed without multiprocessing, and so the evaluation of these
forms must not require any use of multiprocessing: they should not go to sleep
or do input/output operations that might wait for something.  Typical forms on
this initialization list reset the temporary area of subsystems, and do their
best to make sure that what is really garbage (and out to be collected) has no
more pointers to it.

[28] New functions to deal with closures.

COPY-CLOSURE closure
    "closure" should be a closure (or an entity).  COPY-CLOSURE creates and
    returns a new closure by copying "closure".  COPY-CLOSURE generates new
    external value cells for each variable in the closure, and initializes their
    contents from the external value cells of "closure".

CLOSURE-VARIABLES closure
    "closure" should be a closure (or an entity).  CLOSURE-VARIABLES creates
    and returns list of all of the variables in the closure.  That is, it
    returns a copy of the list that was passed as the first argument to
    CLOSURE when "closure" was created.

BOUNDP-IN-CLOSURE closure symbol
    This returns T if "symbol" is bound in the environment of "closure"; that
    is, it does what BOUNDP would do if you restored the value cells known
    about by "closure".  If "symbol" is not closed over by "closure", this
    is just like BOUNDP.

MAKUNBOUND-IN-CLOSURE closure symbol
    This makes "symbol" be unbound in the environment of "closure"; that is,
    it does what MAKUNBOUND would do if you restored the value cells known
    about by "closure".  If "symbol" is not closed over by "closure", this
    is just like MAKUNBOUND.

A note about all of the xxx-IN-CLOSURE functions (SET-, SYMEVAL-,
BOUNDP-, and MAKUNBOUND-): if the variable is not directly closed over,
the variable's value cell from the global environment will be used.
That is, if closure A closes over closure B, xxx-IN-CLOSURE of A will
not notice any variables closed over by B. 
    
A note about CLOSURE-ALIST: if any of the variables in the closure is
unbound, this function will signal an error.  This is essentially
inherent in the way CLOSURE-ALIST is defined to work; it has always had
this behavior, and always will.  Use of this function is not
particularly recommended.  Also, this function has been changed to
return the variables in forward order, rather than in reverse order as
it used to.

[29] New debugger command: Meta-I.  New debugger functions: EH-FRAME and EH-SG.

There is a new debugger command, Meta-I, to help you examine the
values of instance variables in the stack group being debugged.  The "I"
stands for "Instance".  The command prompts you for the name of an
instance variable, and it prints out the value of that instance
variable, inside the instance that is the value of SELF in the
environment of the current frame.

Also, there are two variables to go along with the EH-ARG, EH-LOC,
EH-FUN, and EH-VAL functions that were introduced in System 79.  These
variables can only be used when you are inside the debugger.  The value
of EH-SG is the stack group that is being debugged, and EH-FRAME is the
location of the current frame (expressed as a fixnum offset, into the
control stack of the stack group being debugged, of the base word of the
current frame).

[30] The function FQUERY has some new features.

There is a new value for the :TYPE keyword, called
:MINI-BUFFER-OR-READLINE.  It is like the :READLINE value, except that
if FQUERY is called from inside Zwei or Zmail, the line of text will be
read from the mini-buffer instead of from the QUERY-IO stream.  The idea
of this feature is to let you write things that will work equally well
inside Zwei or on their own; if you use this value, you make it easier
for your code to be integrated into a Zwei extension.

In the list that is the value of the :CHOICES option, there is a new
"choice" available: the symbol :ANY.  If the last element in the list of
choices is the symbol :ANY (instead of being a list, like all other
"choices"), then if the user gives some response that is not one of the
other choices, instead of complaining and re-prompting the user, FQUERY
will return what the user typed (a single character, or a string,
depending on the :TYPE option).

There is a new keyword called :STREAM, whose value is the stream to use
for both input and output.  The default value is the value of the global
variable QUERY-IO.

[31] New facility for handling messages to flavor objects: "whoppers"

There is a new facility in Flavors, related to "wrappers", called "whoppers".
A wrapper is a kind of macro that can be used to handle a message to an object
of some flavor; wrappers are defined with DEFWRAPPER and are explained in The
Lisp Machine Manual.  System 210 introduces "whoppers"; they can do most of
the things that wrappers can do, but have several advantages.

Wrappers and whoppers are used in certain cases in which :BEFORE and :AFTER
daemons are not powerful enough.  :BEFORE and :AFTER daemons let you put some
code before or after the executing of a method; wrappers and whoppers let you
put some code around the execution of the method.  For example, you might want
to bind a special variable to some value around the execution of a method.
You might also want to establish a condition handler or set up a *CATCH.
Another thing that a wrapper or whopper can do is decide whether or not the
execution of the method should happen.  There are a few examples of how to do
these things with wrappers in The Lisp Machine Manual.

The main difference between wrappers and whoppers is that a wrapper is like a
macro, whereas a whopper is like a function.  If you modify a wrapper, all of
the combined methods that use that wrapper will have to be recompiled; the
system does this automatically, but it still takes time.  But if you modify a
whopper, only the whopper itself has to be recompiled; the combined methods
need not be changed.  Another disadvantage of wrappers is that a wrapper's
body is expanded in all of the combined methods in which it is involved, and
if that body is very large and complex, all of that code will be duplicated in
many different compiled code objects instead of being shared.  Using whoppers
is also somewhat easier than using wrappers.

Whoppers are defined with the following special form:

DEFWHOPPER (flavor-name message-name) arglist body	[Special Form]
    This defines a whopper for the specified message to the specified flavor.
    "Arglist" is the list of arguments, which should just be the same as the
    argument list for any method handling the specified message.

When a message is sent to an object of some flavor, and there is a whopper
defined for that message, the whopper runs before any of the methods (primary
or daemon).  The arguments are passed, and the body of the whopper is
executed.  If the whopper doesn't do anything special, the methods themselves
will never be run; the result of the whopper will be returned as the result of
sending the message.  However, most whoppers usually run the methods for the
message.  To make this happen, the body of the whopper calls one of the
following two functions:

CONTINUE-WHOPPER &REST arguments
    This function must only be called from inside the body of a whopper.  It
    calls the methods for the message that was intercepted by the whopper.
    "Arguments" is the list of arguments passed to those methods.  Normally
    the whopper will just pass down the same arguments that it was given.
    However, some whoppers may want to change the values of the arguments and
    pass new values, and this is perfectly legal.

LEXPR-CONTINUE-WHOPPER &REST arguments
    This is like CONTINUE-WHOPPER, but the last element of "arguments" is
    really a list of arguments to be passed.  It is mainly useful when the
    arguments to the intercepted message include a &REST argument.

Here are some examples.  Here is a whopper that will bind the value of the
special variable BASE to 3 around the execution of the :PRINT-INTEGER message
to flavor FOO (this message takes one argument):

    (defwhopper (foo :print-integer) (n)
      (let ((base 3))
	(continue-whopper n)))

Here is a whopper that will set up a *CATCH around the execution of the
:COMPUTE-HEIGHT message to flavor GIANT, no matter what arguments this message
uses:

    (defwhopper (giant :compute-height) (&rest args)
      (*catch 'too-high
	 (lexpr-continue-whopper args)))
    
Like daemon methods, whoppers work in outward-in order; when you add a
DEFWHOPPER to a flavor built on other flavors, the new whopper is placed
outside any whoppers of the component flavors.  However, ALL whoppers happen
before ANY daemons happen.  Thus, if a component defines a whopper, methods
added by new flavors will be considered part of the continuation of that
whopper, and will only be called when the whopper calls its continuation.

Whoppers and wrappers are considered equal for purposes of combination.  If
two flavors are combined, one having a wrapper and the other having a whopper
for some method, then the wrapper or whopper of the flavor that is further out
will be on the outside.  If, for some reason, the very same flavor has both a
wrapper and a whopper for the same message, the wrapper will go outside the
whopper.

[32] New special forms: DO* and DO*-NAMED.

There is a new special form, called DO*.  It is just like DO except
that the variable clauses are evaluated sequentially, rather than "in
parallel".  When a DO starts, all of the initialization forms are
evaluated before any of the variables are set to the results; when a DO*
starts, the first initialization form is evaluated, then the first
variable is set to the result, then the second initialization for is
evaluated, and so on.  The stepping forms work analogously.  There is
also a new special form called DO*-NAMED, which is a version of DO-NAMED
that has the same sequential binding as DO*.

[33] Two new features have been added to the Dired mode of Zmacs.

There is a new command called "Describe Attribute List".  It is assigned
to the comma key (","), and it is available on the menu that pops up
when you click Right while in Dired.  This command prints out the
contents of the attribute list of the current file (the one that the
cursor is at).  It works both for character files and for compiled
files.

When you type "Q" to finish up Dired, and you are shown the files that
will be deleted, and you are asked for confirmation, there is a new
valid answer: "E".  "E" means the same thing is "Y", namely, that the
files should indeed be deleted, but if you say "E" instead of "Y", the
directory will be expunged after the files are deleted.  This is only
meaningful for file systems in which there is "undeletion", namely
TOPS-20, Tenex, and the Lisp Machine File System.  The new command is
especially useful if you were using Dired in order to free up disk
space, since the disk space is not really deallocated until the
directory is expunged.

[34] New stream function: SI:NULL-STREAM.

The function SI:NULL-STREAM can be used as a dummy stream object.
As an input stream, it will immediately report "end of file"; as an
output stream, it will absorb and discard arbitrary amounts of output.
Note: SI:NULL-STREAM is not a variable; it is defined as a function.
Use its definition (or the symbol itself) as a stream, not its value.

[35] VAX/VMS is now a supported file system.

Since file extensions in the VAX/VMS file system are restricted to three
characters, the representation of standard pathname "types", such as
LISP and TEXT, are stored in abbreviated forms in VAX/VMS pathnames.  As
was mentioned in the System 78 release notes, such abbreviations are
also used in Unix files, because of the 14-character limitation of
filenames in Unix.  Here is a complete list of the standard pathname
types, and their abbreviations on both VAX/VMS and Unix.

   System Type  VAX/VMS abbreviation   Unix abbreviation

     LISP              LSP               l
     TEXT              TXT               tx
     MIDAS             MID               md
     QFASL             QFS               qf
     PRESS             PRS               pr
     (PDIR)            PDR               pd
     QWABL             QWB               qw
     PATCH-DIRECTORY   PDR               pd
     BABYL             BAB               bb
     XMAIL             XML               xm
     MAIL              MAI               ma
     INIT              INI               in
     UNFASL            UNF               uf
     OUTPUT            OUT               ot
     ULOAD             ULD               ul
     CWARNS            CWN               cw

If you create a pathname for a VAX/VMS host whose name is "FOO" and
whose type is "QFASL", the printed representation of the pathname will
be "FOO.QFS", and this is what the file will be named in the VAX/VMS
file system.  If you parse a pathname that looks like "FOO.QFS", the
pathname object will have name "FOO" and type "QFASL"; that is, this
translation is performed in both directions.  Fortunately, since VAX/VMS
pathnames cannot have extensions longer than three characters, and all
system types are longer than three characters long, there is no
possibility for the problem of ambiguity that Unix has.  (This problem
was mentioned in the System 78 release notes; see the note about it
later in this document.)

[36] How to add new keywords for ADD-INITIALIZATION.

If you want to add new keywords that can be understood by
ADD-INITIALIZATION and the other initialization functions, you can do
so by pushing a new element onto the following variable:

SI:INITIALIZATION-KEYWORDS             [Variable]
    Each element on this list defines the name of one initialization list.
    Each element is a list of two or three elements.  The first is the
    keyword symbol that names the initialization list.  The second is a
    special variable, whose value is the initialization list itself.  The
    third, if present, is a symbol defining the default "time" at which
    initializations added to this list should be evaluated; it should be
    SI:NORMAL, SI:NOW, SI:FIRST, or SI:REDO.  This third element just acts
    as a default; if the list of keywords passed to ADD-INITIALIZATION
    contains one of the keywords NORMAL, NOW, FIRST, or REDO, it will
    override this default.  If the third element is not present, it is as if
    the third element were SI:NORMAL.

Note that the "keywords" used in ADD-INITIALIZATION need not be
keyword-package symbols (you are allowed to use FIRST as well as
:FIRST), because STRING-EQUAL is used to recognize the symbols.

In addition to the system initialization lists mentioned in Revision 4
of the Lisp Machine Manual (on p. 490), there are two new initialization
lists called SITE and FULL-GC.  FULL-GC was explained above, and SITE
is explained in the Software Installation Guide.

Of the pre-defined system initialization lists, the default "time" for
almost all of them is NORMAL, with the following exceptions: the SYSTEM
and ONCE initialization lists default to FIRST, and the SITE
initialization list defaults to NOW.


NOTES

[1] Clarification of how MAKE-SYMBOL works.

A poorly-written example in the Lisp Machine Manual has confused some
people about the function MAKE-SYMBOL's treatment of upper and lower
case.  Here is a clearer explanation of what MAKE-SYMBOL does.
MAKE-SYMBOL creates a symbol whose print-name is exactly the string that
it is given.  If you give MAKE-SYMBOL an all-uppercase string such as
"FOO", it will make a symbol with that print name, which will print as
FOO.  If you give it a lowercase or mixed-case string, such as "Foo", it
will make a symbol with that print name, which will print as |Foo|.  The
reason that the vertical bars appear is that the Lisp reader converts
symbols to all-uppercase when it reads them, and so the Lisp printer
"slashifies" the lowercase characters using vertical bars so that the
reader, if it ever encounters these characters, will not convert them to
all-uppercase.  Here are the examples again:

   (MAKE-SYMBOL "FOO") => FOO
   (MAKE-SYMBOL "Foo") => |Foo|

[2] How to get panes out of the Select menu.

The following answers an often-asked question about the window
system.  Some users have created frames only to find that every pane of
the frame appears separately in the "Select" menu, and have asked how to
get rid of them.  The way to do this is to mix
TV:DONT-SELECT-WITH-MOUSE-MIXIN with the flavors of all of your panes.
Since you already have to mix in TV:PANE-MIXIN for all of your panes
anyway, the flavor TV:PANE-NO-MOUSE-SELECT-MIXIN is a mixture of both of
these, so that you can just mix it in with all of the flavors of your
panes and take care of both problems with one flavor.

[3] The MAR facility doesn't work.

It has been reported that the MAR facility (see The Lisp Machine
Manual) currently does not work.  It seems to sometimes damage the
system sufficiently that a warm-boot is needed to recover.  We will
attempt to fix this problem as soon as possible.

[4] How to choose process priority levels.

The Lisp Machine Manual explains that processes have priorities, and
tells you how to examine and modify the priority of a process, but it
doesn't give any guidelines about what values to use.  First of all, by
default, processes run with a priority of 0.  This is the default, and
most processes just use this default.  If the priority number is higher,
the process will receive higher priority.  You should avoid using
priority values higher than 20, since some critical system processes use
priorities of 25 and 30, and setting up competing processes could lead
to degraded performance or system failure.  You can also use negative
values to get processes to run in the "background".  Values of -5 or -10
for background processes and 5 or 10 for urgent processes are
reasonable.  Only the relative values of these numbers are important;
that is, only the ordering of the processes by these numbers makes any
difference.  (You can even use floating point numbers to squeeze in more
intermediate levels, though there should never actually be any need to do
so!)

[5] How to interpret directory listings.

The system displays directories of file systems to you in several
contexts: Meta-X View Directory and Meta-X Dired in Zmacs, and in the
File System Editor.  Directories are always displayed in a certain
format, regardless of the context and regardless of what kind of file
system (Lisp Machine, TOPS-20, Unix, etc.) the directory came from.
Since this format is designed to express a great deal of information in
a single line, it is rather abbreviated, and some of the ways it
expresses things are not clear without an explanation.  Here is a
complete description of the format used whenever the system lists a
directory.

The basic format usually looks something like this:

      pal.lisp.65      7  25548(8)         03/12/82 12:42:41 (05/13/82)   dlw

In this example, "pal.lisp.65" is the name of the file ("pal" is the
name, "lisp" is the type, and "65" is the version).  7 is the length of
the file in "blocks".  25548 is the length of the file in "bytes", and 8
is the byte size of the file.  03/12/82 12:42:41 is when the file was
created, and 5/13/82 is when the file was last referenced.  "dlw" is the
author.

Many other things can appear in such a line; some of these things will
only be seen on certain types of file systems.  If the first character
in the line is a "D", the file has been deleted (this only makes sense
on file systems that support "undeletion", such as the Lisp Machine and
TOPS-20 file systems).  After the "D", if any, and before the name of
the file, is the name of the "physical volume" that the file is stored
on (on ITS, this is the disk pack number).

On a line that really describes a link rather than a file, then the
length numbers are all omitted, and instead there is a little arrow
("=>") followed by the name of the target of the link.  On a line that
really describes a sub-directory rather than a file, the
length-in-blocks number is shown (if provided by the file system), but
the length-in-bytes is replaced by the string "DIRECTORY".

Next, before the dates, the line may contain any of several punctuation
characters indicating things about the file.  Keep in mind that each of
these flags only is understood by some of the file systems, and not
others.  Here is a list of the various characters, and what flags they
indicate:

    Character    Flag
    =========    ====
       !          Not backed up.
       @          Don't delete.
       $          Don't reap.

For lines indicating sub-directories, the reference date may be replaced
by a date preceded by "X="; this is the date at which this directory
was last expunged.  The dates are followed by the file's author, and
the author is followed by the file's reader (the last user to read
the file).

Remember that only certain file systems support certain features; most
file systems don't keep track of the file's reader and don't have any
such thing as a "don't supersede" flag.  Therefore, any of the above
fields may be omitted on certain file systems.  However, the same
general format is followed for all file systems, and so you can
interpret the meaning of a line of a directory listing even for a file
system with which you are not familiar.

[6] Status report on problems with Unix file systems.

In the System 78 release notes, we explained a problem with the
support of the Unix file system.  The problem is caused by the need for
abbreviations for system file types.  Since files of type "LISP" are
represented in the Unix file system as files whose names end in ".l", it
is necessary to parse files named "foo.l" as having the name "FOO" and
the type "LISP", and so when the system encounters a file that is
actually named "foo.lisp", there is nothing that it can be represented
as.  As a result, the system is incapable of handling files in the Unix
file system whose names end with ".lisp" or ".text" or any of the other
system types.  Another severe problem with Unix support is that the
system's internal representation of file names does not distinguish
uppercase characters from lowercase characters, whereas Unix does.  The
symptom is that if you refer to a file with the wrong case, the system
will be incapable of referring to it with the correct case until it is
cold-booted.

We regret to announce that we still have not fixed these problems.  To
fix them, and a class of related problems, requires a major
reorganization of system modularity, and we were not able to accomplish
this in time for the release of System 210.  We realize that these
problems are very important, and we intend to fix them as soon as
possible.  We hope to have this problem solved by the next release, and
apologize for the inconvenience that they are causing.

[7] Problems with Zmail background process errors have been fixed.

In every release, a large number of bugs are fixed, and normally we
do not attempt to report every such fix, since if we did so the release
notes would be much longer than they are now.  However, we are happy to
report that one problem that caused lots of problems and complaints has
finally been fixed.  It used to be that if the Zmail Background process
got errors while doing I/O to its file computer (particularly if the
file computer were to crash while you were reading your mail), the
process would continually get errors, over and over.  This has been
fixed in System 210; the Zmail Background process now knows how to deal
with these errors.

[8] How to establish Chaosnet servers.

The Chaosnet document's explanation of how to establish servers for
new Chaosnet protocols is rather brief; here is a longer description.
Suppose you created a new protocol with contact name ALPHA and wanted to
set up a server for the ALPHA protocol.  You'd put the following
top-level form into your program:

   (ADD-INITIALIZATION
     "ALPHA"
     '(PROCESS-RUN-FUNCTION "ALPHA Server" 'ALPHA-SERVER)
     NIL
     'CHAOS:SERVER-ALIST)

To remove the server, you would evaluate the following form:

  (DELETE-INITIALIZATION "ALPHA" NIL 'CHAOS:SERVER-ALIST)

In other words, an initialization list, stored in the variable
CHAOS:SERVER-ALIST, associates contact names with forms to be run when
some network host tries to connect to that contact name.  Your form
should always create a new process to handle the connection; in this
example, the name of the process will be "ALPHA Server", and the first
thing that the process will do is call the function ALPHA-SERVER.  This
function should start off by calling CHAOS:LISTEN to get the
"connection" object; then it can call CHAOS:ACCEPT, CHAOS:REJECT,
CHAOS:ANSWER, or CHAOS:FORWARD as is appropriate.