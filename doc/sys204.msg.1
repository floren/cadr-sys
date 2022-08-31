.c -*- Text -*-

These notes accompany the Symbolics software release that includes
System version 204.  They cover all important changes since the System
79 release notes.

The reason that the version numbers jumped is so that you can tell
whether a release is from Symbolics; Symbolics releases have major
System version numbers greater than 200.

By the way, release notes from previous releases can be found in the
files AI: LMDOC; SYS79 MSG, AI: LMDOC; SYS78 MSG, and so on for each
system that got released.  (Note: some numbers are missing because the
corresponding system was never released.)  This note is in the files
AI: LMDOC; SYS204 MSG, and EE:LIB:<LISPM.DOCUMENTATION>SYSTEM-204.RELEASE.

As usual, the changes are divided into two sections.  The first section
explains those changes that might potentially cause problems or confuse
users when they start using the new system.  The second section lists
new features or minor changes.  It is important for you to read about
the changes in the first section, in order to understand problems that
might otherwise arise when you start using the new software release;
however, you may safely ignore the second section, or read it at your
leisure.

>> Summary of Changes

  Changes to take note of:
  [1]  System 204 must be run with microcode 865.
  [2]  Flavors have been incompatibly reimplemented.
  [3]  The editor's file copying commands have changed.

  Changes that may be safely ignored:
  [A]  Many functions have been moved into different files.
  [B]  Terminal 0 H finds the status of selected hosts.
  [C]  Peek now shows more information about processes.
  [D]  New special form TYPECASE.
  [E]  SET-SYSTEM-STATUS is globalized.
  [F]  Compiler warnings are handled differently.
  [G]  Idle file server jobs get logged out after 30 minutes.
  [H]  A new function CHAOS:HOST-UP.
  [I]  A new editor command M-X Print Buffer.
  [J]  Certain subsystems are not supported or not loaded anymore.

>> Changes to take note of:

[1] System 204 comes with microcode 865.  They must be run together.

[2] Flavors have been completely reimplemented in this system, in order
to provide improved performance.  Unfortunately, the change is
incompatible in several ways.  Any program that uses flavors
must be recompiled.  In addition, if it makes use of some of the
more sophisticated features for flavors, it may need to be modified.
See the file AI: LMDOC; SYS204 FLAVOR, or the file
EE:LIB:<LISPM.DOCUMENTATION>SYS204-FLAVOR.TEXT, for details.

[3] Copy Text File and Copy Binary File are both gone from Zmacs.  In
their place in Copy File, which tries to figure out which format to use
and uses it.  If it is wrong, you can force it by giving numeric
arguments, the exact value of which are in the self-documentation.

>> Changes that may be safely ignored:

[A] Numerous system functions are in different files now, for various
reasons.  This should not affect anyone; you can still use Meta-. to
find the sources of system functions.

[B] The Terminal 0 H command lets you ask for the status of specific
hosts.  Like Terminal H, it is a user-end of the Chaosnet STATUS
protocol, and asks for the status of various hosts on the network.
Terminal H queries every host in the host table, whereas Terminal 0 H
asks you which hosts should be queried, and only queries those hosts.

[C] The "P" mode in Peek now shows more information about processes,
including the priorities, quanta, percentage of the processor being
used, and idle time, for each process.

[D] There is a new special form called TYPECASE, used for selecting
various forms to be evaluated based on the type of an object.  It
is somewhat like SELECT.  A TYPECASE form looks like:

    (typecase <form>
       (<types> <consequent> <consequent> ...)
       (<types> <consequent> <consequent> ...)
       ...
       )

<form> is evaluated, producing an object.  TYPECASE examines each clause
in sequence.  <types> in each clause is either a single type (if it is a
symbol) or a list of types (otherwise).  If the object is of that type,
or of one of those types, then the consequents are evaluated and the
result of the last one is returned.  Otherwise, TYPECASE moves on to the
next clause.  As a special case, <types> may be OTHERWISE; in this case,
the clause will always be executed, and so this should only be used in
the last clause.  For an object to be of a given type means that if
TYPEP is applied to the object and the type, it returns T.  That is, a
"type" is something meaninful as a second argument to TYPEP.

[E] The function SET-SYSTEM-STATUS is now on the global package.  You call
this function manually when you want to change the status of a patchable
system.  The arguments are

       (SET-SYSTEM-STATUS <system> <new-status> &optional <major-version>)

<system> should be a string, naming the system.  <new-status> should be
one of the symbols :EXPERIMENTAL, :RELEASED, :OBSOLETE, or :BROKEN.
<major-version> should be a fixnum that identifies the major version
of the system whose status you want to set; it defaults to the major
version that is currently loaded.  Note that SET-SYSTEM-STATUS is
something that you call manually; you should NOT put it into patch files.

[F] In order to improve performance and modularity, compiler warnings
and error messages are now handled differently.  In previous releases,
the compiler placed warnings in a special Zmacs buffer called "Compiler
Warnings".  This slowed down the compiler, and created an undesirable
modularity dependence between the compiler and the editor.  The compiler
now puts its messages into a database that can be examined by programs,
such as the editor.  Here are the changes to take note of.

  1.  The Zmacs buffer called "Compiler Warnings" is no longer
  automatically created.
  
  2.  A simple way to look at compiler warnings messages is with a new
  function called (PRINT-COMPILER-WARNINGS &optional <files> <stream>).
  <files>, if given, should be a list of the generic pathnames of the
  files whose compiler warnings you wish printed.  The messages will be
  printed to <stream>.
  
  3.  If you have macros that issue their own compiler warnings, it no
  longer works to simply type out to the ERROR-OUTPUT stream.  You now
  have to use the function COMPILER:WARN to get your warning into the new
  database.  If you really want to use this function, see the source for
  documentation.  Most users should not need this.
  
  4.  A new Zmacs command, M-X Compiler Warnings, creates a buffer called
  "Compiler Warnings" (if it does not already exist), does
  PRINT-COMPILER-WARNINGS into it (if there are any new ones), and selects
  it.
  
  5.  The Zmacs command M-X Edit Compiler Warnings continues to work.
  
  6.  Some variables associated with the old compiler warnings scheme
  which had been global have been flushed.  This really shouldn't affect
  anybody.

[G] Idle file server jobs now get logged out automatically after 30 minutes.

Here's what that means: when you use some host on the Chaosnet, such as
one of the PDP-10s or -20s, a file server job gets created on that host.
This job sits around waiting for commands from your Lisp Machine, which
it processes.  However, the job uses up some resources on the host even
when it is idle.  In order to avoid wasting resources, the Lisp Machine
will automatically kill the file server job after it has been idle for a
while.  If you access files on a host, and then don't use that host
at all for a long time, the server gets killed.

If you try to use the host again after the server has been killed, the
system will automatically create a new server.  So you can almost ignore
the fact that the server is killed.  The main way in which it affects
you is that if the server is question requires a password, you will
get asked for the password again and have to type it in.  Of the servers
commonly used at MIT right now, the TOPS-20 and VAX systems require
passwords, and the ITS systems and the file computer do not.

Some people have suggested that the system be changed to remember such
passwords and reissue them automatically.  Since Lisp Machines
themselves are completely without security, this would create a very
easy way for people to have their passwords compromised.  Some Lisp
Machine users are not aware of this problem, or may not fully understand
the issues, especially new users or people who rarely use the machine
and don't keep up with INFO-LISPM announcements.  We feel that under the
circumstances it would be extremely irresponsible to release a system
that would, unknowingly to the user, make his password publicly
accessible.

By the way, data connections are also destroyed automatically.  This is
completely invisible to the user; it just saves resources.  The first
one goes after 10 idle minutes, and additional ones go after 1 idle minute.

[H] A new function (CHAOS:HOST-UP <host> &optional <timeout>) asks a
host whether it is up (alive, functional, responding) or not.  If it is
up, this function returns T, if not, it returns two values: NIL, and the
error that occured (usually "Host not responding.").  <host> may be a
host object or the name of a host; <timeout> is in 60ths of a second
and defaults to 3 seconds.  If the host does not respond after this much
time, it is assumed to be down.

Note that if this function returns NIL, it is possible that the host is
really up but is not connected to the Chaosnet for some reason.  What
this function really tests is whether the Lisp Machine is capable of
communicating with the host over the Chaosnet.

[I]  A new editor command, M-X Print Buffer, allows you to print the
contents of a buffer.  It is analgous to M-X Print File.

[J] The microassembler is not loaded into the default system.  The main
reason for this is that it has always been necessary to do a
(make-system 'ua) before running it anyway, in order to get the latest
QCOM and so on.  Another reason is that it is rarely used and increases
the side of bands, which is inconvenient for those who have T-80 disk
drives (particularly the EE department machines).

Symbolics does not intend to provide support for the microcompiler nor
for the "FC" and "FS" file systems.  Dave Andre will continue to
support the "FS" system himself.  It is possible in system 204, as
before, to load the Remote-File, Local-File, and UA systems simply by
doing make-system.
