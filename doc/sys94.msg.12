-*-Fonts:TR12,cptfont,TR12I,HL12B,TR12B; Mode:Text-*-

System 94 comes with microcode 239.  An earlier microcode version contained bugs.
All System 94 bands should be updated to use microcode 239.

There are no major changes that affect user programs.  The most important
change is a rearrangement of the macroinstruction destination codes which will
allow several new macroinstructions to be created.  As a result of this change,
QFASL files made by 1Meta-X Fasl Update* in system 94 cannot be loaded into
system 93, nor can any QFASL file made by fasdumping data that includes
compiled functions.

QFASL files produced by 1QC-FILE* in system 94 2can* be loaded into system 93,
but a few things will work only if all system 93 patches are loaded.

4Programming Changes:*

    1A*] 3ERRORP4 of a String Is Now *NIL4.**
    1B*] 3WITH-TIMEOUT4.**
    1C*] 4Inhibiting 3SETF* and 3LOCF*.*
    1D*] 4"A or An" String Functions.*
    1E*] 3FS:BALANCE-DIRECTORIES4 Improved.**
    1F*] 4Pathname Creation Changes.*
    1G*] 4Dangerous Conditions Not Caught.*
    1H*] 4End-of-File Errors Handled Differently.*
    1I*]4 Flavor Aliases.*
    1J*] 3:INVERSE-AROUND4 Methods.**
    1K*]4 3TV:DESELECT-AND-MAYBE-BURY-WINDOW** 4Takes New Argument.*
    1L*] 3TAB-WIDTH4 Argument to *:COMPUTE-MOTION 4and *:STRING-LENGTH4 Operations.


1A***] 3ERRORP4 of a String Is Now *NIL4.**

It used to be 1T* for compatibility with the times before the new error system.

1B*] 3WITH-TIMEOUT4.**

The new special form 1WITH-TIMEOUT* allows you to execute any piece of code
with a timeout.  Do

1(WITH-TIMEOUT (2timeout* 2timeout-forms*...)
  2body*...)

2body** is executed with a timeout in effect for 2timeout* 60'ths of a second.  If
2body* finishes before then, values of the last form in 2body* are returned.
Otherwise, at the end of the timeout, the 2timeout-forms* are evaluated and the
values of the last one of them are returned.  For example,

1(WITH-TIMEOUT ((* 60. 60.) (FORMAT QUERY-IO " ... Yes.") T)
  (Y-OR-N-P "Really do it? (Yes automatically after one minute) "))

C*] 4Inhibiting 3SETF* and 3LOCF*.*

If you have a macro or subst function that should never be used in a 1SETF*,
you can prevent 1SETF* from working on it by giving the function name
a 1SETF* property of 1SI:UNSETFABLE*, as in

1   (DEFUN FOO (A) (AREF A 3))
   (DEFDECL FOO SETF SI:UNSETFABLE)*

Normally the system would automatically handle 1(SETF (FOO X) Y)*
by expanding the definition of 1FOO* and using 1AREF*'s 1SETF* definition.
The 1DEFDECL* shown prevents that.

1SI:UNLOCFABLE* is used similarly to forbid 1LOCF*.

1D*] 4"A or An" String Functions.

1(STRING-SELECT-A-OR-AN 2word*)** returns 1"a"* or 1"an"* according to the string
2word*; whichever one appears to be correct to use before 2word* in English.

1(STRING-APPEND-A-OR-AN* 2word1)** returns the result of prepending 1"a"* or 1"an"* to
2word* with a space in between.

1E*] 3FS:BALANCE-DIRECTORIES4 Improved.

1FS:BALANCE-DIRECTORIES*** now takes a new keyword argument 1:DIRECTION*
which specifies to transfer files only in one direction between the directories.
The value may be 1:1->2* meaning transfer only from the first directory to the
second, or 1:2->1* meaning transfer only from the second to the first.  The
default, 1NIL*, means transfer either way.

1F*] 4Pathname Creation Changes.*

The 1:NEW-PATHNAME* operation, and all other operations for creating a new
pathname, now replace any invalid component specified with 2something* valid.

The 1:NEW-DEFAULT-PATHNAME* operation is now identical to 1:NEW-PATHNAME*.
The 1:VALID-NAME*, etc, operations no longer exist.  To tell whether the value of
1FOO* is a valid name (say) component for a pathname on host 1BAR*, you can do

1(EQUAL FOO (SEND (SEND BAR-PATHNAME ':NEW-NAME FOO) ':NAME))

G*] 4Dangerous Conditions Not Caught.

1ERRSET**, 1CATCH-ERRORS* and 1IGNORE-ERRORS* no longer catch "dangerous"
conditions (those for which 1:DANGEROUS-CONDITION-P* returns non-1NIL*).  These
include errors such as running out of virtual memory.  You can still catch
them with a 1CONDITION-CASE* if you are willing to suffer the dire consequences.

1H*] 4End-of-File Errors Handled Differently.*

Errors for end of file from operations such as 1:TYI* now signal a condition with
1SYS:END-OF-FILE* as a condition name, and a standard error message which
includes the stream.  The argument to these operations, which used to be used
as the error messages if it was non-1NIL*, is now just a boolean flag saying
whether to get an error on end of file.

1I*]4 Flavor Aliases.*

You can now define one flavor name to be an alias for another.
Do 1(DEFFLAVOR FOO () (BAR) :ALIAS-FLAVOR) *to make 1FOO* an alias for 1BAR*.
Attempting to create an instance of 1FOO* will actually make an instance of 1BAR*.

The main use of this is for changing a flavor's name gracefully.  It has been
used to eliminate various flavors such as 1TV:COMMAND-MENU-PANE* which are now
identical to other existing flavors without requiring user programs to be
changed.

1J*] 3:INVERSE-AROUND4 Methods.

1:INVERSE-AROUND*** methods are a new feature of flavors.  They work like 1:AROUND*
methods but they are nested in the inverse order: those on earlier flavor
components are done inside those on later flavor components.  Thus, you can use
them to bind a special variable to a particular value, and then be able to
override a base flavor's binding with a new 1:INVERSE-AROUND* on a mixin or
combination flavor.

1K*]4 3TV:DESELECT-AND-MAYBE-BURY-WINDOW** 4Takes New Argument.

1TV:DESELECT-AND-MAYBE-BURY-WINDOW** now takes two arguments, a window and
an optional 2deselect-mode*.  The latter is passed as an argument to the 1:DESELECT*
operation on the window.  It can specify where to put the window in the list of
previously selected windows used by 1Terminal* and 1System* commands, including
1Terminal S*.

1L*] 3TAB-WIDTH4 Argument to *:COMPUTE-MOTION 4and *:STRING-LENGTH4 Operations.**

These two operations on windows now take the spacing to assume between tab stops
as an additional argument.  The default is to use the spacing that the window
actually uses on output.

4Miscellaneous User Interface Changes:

    1A**] 4New Way of Presenting Notifications.
    1B**] 4No More "Illegal Simultaneous Access".
    1C**] 4Password Guessing Improved.
    1D**]4 Converse: 3End* Now Stays in Converse, and 3Control-End* exits.
    1E**] 3SI:EDIT-DISK-LABEL4 Improved.
    1F***]4 3QC-FILE* and Microcompilation.
    1G**] 4Dumping Microassemblies.
    1H**] 4Shifted Mouse Characters.
    1I**] 3TV:FULL-SCREEN-HACK-MIXIN4 Is Back.
    1J***] 3Network Break4 in SUPDUP.
    1K***] 4Host Objects Print Readably.
    1L**]4 System Menu 3Layouts* Command Records Partially Visible Windows.
    1M**] 3DEBUG-IO4 May Be *NIL4.
    1N]*** 4Debugger Command 3Meta-R*.
    1O**] 3BREAKON4 With No Argument.


1A***] 4New Way of Presenting Notifications.*

When a notification is to be printed (such as, for an incoming 1QSEND*, a window
or background process wanting to do input or output, or garbage collection
activity), what happens depends on the selected window.  Certain flavors of
window will, when selected, allow the notification to be printed on them.  These
include Lisp listeners and the typeout windows of editors.  Other windows, such
as SUPDUP, PEEK, and editors except for short notifications, cannot allow the
notification to be simply printed.

In the past, the latter class of windows handled a notification by presenting a
small pop-up window and printing the notification there.  But this is slow.  In
the hope of making notifications less of a nuisance, a new behavior is installed
for these windows.  The keyboard will beep, and the who-line mouse
documentation line will announces that notifications are waiting.
Blinking asterisks will appear at both ends of the line to attract your attention.
The notifications will keep on waiting until you switch to a window that can print
them directly -- such as a lisp listener -- or until you type  1Terminal N*.

The new command  1Terminal N*  is defined to quickly show you your new notifications.
1Terminal 1 N* will display all the notifications you have received
(like 1(PRINT-NOTIFICATIONS)*) in addition to any pending notifications.

If you would rather not see the notifications now and would like your mouse
documentation line to go back to its usual function, type  1Terminal 2 N*.
You can still see the notifications when you are ready to see them by typing
1Terminal N*  at that time.

If the notification is going to urge you to select some window,
the notification will go away if you select the window without printing the notification.
You can do this with 1Terminal 0 S* as you used to.

The variable 1TV:WAIT-FOR-NOTIFICATIONS-FLAG* controls whether the old or new behavior
will occur.  Set this variable to 1NIL* to get the old behavior back.

Please express your opinions on this change.

1B*] 4No More "Illegal Simultaneous Access".*

The tendency of the Twenex file server to complain "illegal simultaneous
access" if you abort out of opening a file has been circumvented.  It ought not
to happen any more.

1C*] 4Password Guessing Improved.*

Once a password is recorded for one host, if you connect to a file server on
another host the system will guess that your password on the second host is the
same as on the first.

As before, the first time you connect to a host your user-id (or the part of it
after the last period) will be used as the guess for the password.

1D*]4 Converse: 3End* Now Stays in Converse, and 3Control-End* exits.*

The meanings of 1End* and 1Control-End* in Converse have been interchanged.
1End* now sends the message and stays in Converse, while 1Control-End* sends
it and exits.

The idea is that if you want to exit and you type 1End* and it fails to exit,
you can exit painlessly by typing 1Abort*, whereas the old way if you typed
1End* by habit and you wanted to stay in Converse it was a hassle to switch
back there.

1E*] 3SI:EDIT-DISK-LABEL4 Improved.**

This function now prompts for commands so you can tell whether
you are inside it or not.

The new 1Control-D* command describes a partition.

If you try to exit without saving your changes, you will be asked
whether to save them.

1F*]4 3QC-FILE* and Microcompilation.*

The way to say in a source file that function 1FOO* should be microcompiled is
now

1    (DEFDECL FOO COMPILER:MICROCOMPILE T)

G*] 4Dumping Microassemblies.*

Once you have used 1UA:ADD-ASSEMBLY* to assemble an incremental microcode
module, you can now save it in a file with 1UA:DUMP-MODULE.*  Normally you
need only give it one argument, the output file name.  The result is a QFASL
file which saves the result of the microassembly.  Just loading the file is
equivalent to calling 1UA:ADD-ASSEMBLY* again, but is much faster.

Remember that the microcode module is not available for use unless you call
1UA:LOAD-MODULE*.  This is true whether the module was created by
1UA:ADD-ASSEMBLY* or by loading a file made by 1UA:DUMP-MODULE.

H*] 4Shifted Mouse Characters.*

"Mouse" characters inserted in input buffers by 1TV:KBD-MOUSE-BUTTONS-MIXIN*
and appearing in the lists inserted by 1TV:LIST-MOUSE-BUTTONS-MIXIN* now contain
information on the state of the four main shift keys on the keyboard.  The
1%%KBD-HYPER*, 1%%KBD-SUPER*, 1%%KBD-META* and 1%%KBD-CONTROL* fields of the
character code are now 1 if either one of the corresponding shift keys is pressed
down.  The new function 1TV:MERGE-SHIFT-KEYS* is used to create the character
code with correct values for those bits.

This is completely independent of the 1TV:*MOUSE-INCREMENTING-KEYSTATES**
feature whereby some of the same shift keys (or other keys) can act like
an extra click.

1I*] 3TV:FULL-SCREEN-HACK-MIXIN4 Is Back.**

In earlier systems, some commonly-used windows did not have borders when they
filled the whole screen.  This was changed in system 93 because most people
prefer the borders.  But some people prefer no borders.  Now you can turn
them on and off at run time.

The function 1TV:FLUSH-FULL-SCREEN-BORDERS* turns borders on or off.
It takes one argument, 1NIL* to turn borders 3on* and 1T* to turn them 3off* (flush them).
The function applies to existing windows as well as those yet to be created,
but only windows that fill the whole screen.

To be affected, a window must have 1TV:FULL-SCREEN-HACK-MIXIN* in its flavor.
Various system flavors, such as Lisp listeners, peek frames, inspectors, and SUPDUP,
use the mixin.

1J*] 3Network Break4 in SUPDUP.**

You can now use 1Network Break *instead of 1Network B* to get a break loop
inside SUPDUP.

1K*] 4Host Objects Print Readably.*

They look like 1#FS:TOPS20-CHAOS-HOST "MIT-OZ"*, etc.

1L*]4 System Menu 3Layouts* Command Records Partially Visible Windows.*

The 1Layouts* command in the system menu now records the ordering of
all the active windows when you save a layout, and puts them back into
that order when you restore the layout.  As a result, this command is
now useful for recording a setup of overlapping windows which you
wish to switch between.

1M*] 3DEBUG-IO4 May Be *NIL4.**

It is now legitimate for 1DEBUG-IO* to be 1NIL*.  This tells the debugger to use the
value of 1TERMINAL-IO*.  If you give 1DEBUG-IO* a nonstandard value and then wish
to change it back to normal, setting it to 1NIL* is an easier way.

1N]* 4Debugger Command 3Meta-R*.*

The debugger has a new command, 1Meta-R*.  It reinvokes the current frame's
function, like 1C-M-R*, but allows you to replace the arguments or change the
number of arguments.

1Meta-R* asks you what to do for each argument, one by one.  You can use the
previous value of that argument by typing a 1Space*, or type a new value for the
argument.  To terminate the argument list (once all the required arguments
have been supplied), type 1End* rather than an argument.

1O*] 3BREAKON4 With No Argument.

1(BREAKON)*** returns a list of all functions broken on.

4Editor and ZMail Changes:

    1A**] 3{TEX} Mode4 is now *TEX Mode4.
    1B***] 3Control-X Control-F4 Allows Wildcards.
    1C***] 4New Editor Case-Conversion Commands.
    1D**] 3ZWEI:*DEFAULT-BASE*4 Initialized from *BASE4.
    1E***] 4Blinkers in Incremental Search.
    1F**]4 DIRED Improvements* 4and Related Things.
    1G**] 4ZMail Header Editing Commands in 3Control-X M*.
    1H**] 4Preloading ZMAIL.
    1I**] 4ZMAIL Commands for Window Configuration While Sending Mail.
    1J**]4 Mail Templates.
    1K**] 4ZMail Can Reformat Headers.
    1L**] 4Moving Messages by Individual Filters.


1A**] 3{TEX} Mode4 is now *TEX Mode4.**

The editor command 1Meta-X {TEX} Mode* has been renamed to
1Meta-X* 1TEX MODE*, for Emacs compatibility.  Apologies to those of you who try to
enter Text mode by typing m-x tex !

1B*] 3Control-X Control-F4 Allows Wildcards.**

You can now use a pathname with wildcards as the argument to the ZMACS
command 1Control-X Control-F*.  This causes each file matching what you specify
to be loaded into an editor buffer.

1C*] 4New Editor Case-Conversion Commands.

1Meta-X Lisp Uppercase Region** and 1Meta-X Lisp Lowercase Region* convert
the Lisp code in the region to the specified case, while not touching comments,
strings and slashified characters.  This is just the thing for taking a file
written in primarily upper case and converting it to primarily lower case,
or vice versa.

1D*] 3ZWEI:*DEFAULT-BASE*4 Initialized from *BASE4.**

The first tie a ZMACS window is exposed in each session, the option
1ZWEI:*DEFAULT-BASE** is initialized from the current value of 1BASE*.
So if you set 1BASE* in your init file, 1ZWEI:*DEFAULT-BASE** will be set also.
If you set 1ZWEI:*DEFAULT-BASE** to something non-1NIL* after you log in,
however, that value will not be overridden.

1E*] 4Blinkers in Incremental Search.*

During an incremental search, the blinkers blink both in the typein window
where the search string is displayed and in the editor window where the buffer
is displayed.

1F*]4 DIRED Improvements* 4and Related Things.*

DIRED now names its buffers after the directory being edited.  Once you
observe the pattern of naming from the mode line, you will be able to reselect
a particular DIRED buffer without having to do 1C-X B* to figure out its number.

1L* is now a DIRED command which says to load the file you are pointing at;
the function 1LOAD* is 1used.

<* is now a DIRED command which says to edit the superior directory of
the directory you are editing.  This creates a new DIRED buffer for that directory.

1C-Shift-E* is now a DIRED command which visits the file you are pointing at,
in the other window.  This is like 1C-X 4 F* but operating on a filename from
the DIRED buffer rather than reading one.

When you use the 1A* command in 1DIRED* to mark files to have a function applied
to them, you can now specify a lambda-expression as the function to be applied.
Previously, a symbol or function spec had to be used.

1Meta-X Undelete File* undeletes a deleted file.  It works like 1Meta-X Delete
File.

Meta-X Reap File* and 1Meta-X Clean Directory* now offer to expunge at the
end if you have deleted anything.

1Meta-X Reap File*, 1Meta-X Clean Directory* and the 1H* command in DIRED
now only delete versions that are connected to the most recent version
by consecutive versions.

For example, if there are versions 8, 14, 22, 23, 24 and 25, the versions deleted
or marked for deletion would be 22 and 23.  8 and 14 would be exempted because
of the gap between them and the most recent sequence of versions.

1G*] 4ZMail Header Editing Commands in 3Control-X M*.*

The ZMail commands for editing headers of outgoing mail are now available
in ZMACS in the 1Control-X M* command for sending mail.

1H*] 4Preloading ZMAIL.*

"Preloading" ZMAIL means requesting that various files be loaded into ZMAIL
while you work on other things.  You would be able to enter ZMAIL later, after
preloading was finished, and access all the files with no delay.  (If you enter
ZMAIL sooner and try to access one of them, you must wait while it is read, as
usual.)

To do this, simply call 1ZWEI:PRELOAD-ZMAIL* with a list of pathnames or strings
as arguments.  This might be done in your LISPM init file, for example.  The
more hairy calling sequence used by Symbolics systems is also supported, but
the hair is not needed.

1I*] 4ZMAIL Commands for Window Configuration While Sending Mail.*

The ZMAIL commands you use to switch window configurations while
composing outgoing mail have been changed.

It used to be that 1C-X 2* went into "two window mode", which had three
windows.  The other commands were similarly misnamed.

Now 1C-X 3* enters "three window mode", which shows the same three windows
(selected incoming message, headers, and text).  C-1X 2* enters "two window
mode", which shows two windows (headers and text).  1C-X 1* enters "one
window" mode, which shows the headers and text together in one window
(plus the summary window, if that is normally displayed).

The ZMAIL profile options for window configurations for the Reply
command have new values: 1:LIKE-MAIL* means to use the same configuration
that the Mail comand would use, and 1:SHOW-ORIGINAL* means to display the
original message as well as the headers and text of the reply (three windows).
The default is now 1:LIKE-MAIL*.  The old values of these keywords are still
understood, if they are present in existing ZMAIL init files.

1J*]4 Mail Templates.*

ZMAIL templates are a means of specifying header fields automatically in
messages you are sending.  Each template you define has a name and
its definition specifies various header fields to initialize.  You can invoke
a template explicitly, and you can also have various default templates
that are invoked automatically in certain circumstances.

A template is defined with 1ZWEI:DEFINE-MAIL-TEMPLATE*, as follows:

1(ZWEI:DEFINE-MAIL-TEMPLATE 2name* 2documentation*
  2body...*)

2body** is Lisp code which uses convenient functions described below
to modify header fields as you desire.  2documentation* is a string whose
first line is brief documentation (a complete sentence) and whose entire
text is the full documentation.  The purpose of the documentation is to say
what this template is to be used for.  2name* is the template's name, a symbol.

An editor command is automatically constructed from the template.
Its name is created by appending "1COM-*" and 2name*.  The resulting command
is made available through 1Meta-X* when you are composing mail in ZMACS
or ZMAIL, and when you are editing a received message in ZMAIL.
Thus, a template named 1MORE-INFO* would produce a command 1COM-MORE-INFO*
which would be accessed using 1Meta-X More Info*.

These are the functions recommended for use in the body of a template:

1ZWEI:ADD-FIELD 2type* 2contents**

   Adds a 2type* field to the header, putting in contents 2contents*.  Existing
2type* fields are left alone.  2type* should be a keyword identifying a header field,
such as 1:CC* or 1:REPLY-TO*.  2contents* should be a string.

1ZWEI:DEFAULT-FIELD 2type* 2contents**

   Similar to 1ADD-FIELD* but does nothing if a field of type 2type* already exists
with nonempty contents.

1ZWEI:DELETE-FIELD 2type**

   Deletes any field(s) of type 2type*.

1ZWEI:FIND-FIELD 2type**

   Returns a buffer pointer to the first 2type* field, or 1NIL* if there are none.
This is for sophisticated alteration of header fields.

1ZWEI:ADD-TEXT-START 2string**

   Adds 2string* to the text of the message, at the beginning of the text.

1ZWEI:ADD-TEXT-END 2string**

   Adds 2string* to the text of the message, after any existing text.

The arguments to these functions can be calculated in an arbitrary fashion.
Actually, templates are not limited to calling these functions.  They can run
arbitrary Lisp code.  In particular, they can use any of the primitives that
editor commands use.  1ZWEI:*INTERVAL** will be bound to an interval
containing the message being operated on.  In addition, if you are replying
to messages or forwarding messages in ZMAIL, the variable 1ZWEI:*MSGS** will
be bound to a list of those messages (otherwise, it is 1NIL*).  You can therefore
easily examine those messages, such as by using 1ZWEI:MSG-FITS-FILTER-P* on
them to see if they match one of your ZMAIL filters, and make your header
alterations conditional on the answer.

In addition to invoking templates expicitly with 1Meta-X*, you can have
templates that are run automatically when you begin to send a message.  The
ZMAIL options 1ZWEI:*DEFAULT-FORWARDING-TEMPLATE**,
1ZWEI:*DEFAULT-BUG-TEMPLATE**, 1ZWEI:*DEFAULT-REPLY-TEMPLATE**, and
1ZWEI:*DEFAULT-MAIL-TEMPLATE** may have as their values the names of
templates (as specified in the template definitions) to be run automatically
when you start to compose, respectively, a forwarding of other messages, a
bug report, a reply, or anything else.  The variables may also be 1NIL* to
specify that no template is to be invoked; this is the default.  Sending with
ZMACS uses, instead, the variables 1ZWEI:*DEFAULT-ZMACS-MAIL-TEMPLATE** and
1ZWEI:*DEFAULT-ZMACS-BUG-TEMPLATE**.  They too should have as values either
template names or 1NIL*.

Here is an example of a template:

1(ZWEI:DEFINE-MAIL-TEMPLATE MORE-INFO *
	1"Compose request for more info on a bug."
  (ZWEI:ADD-FIELD ':CC "BUG-LISPM")
  (ZWEI:DEFAULT-FIELD ':SUBJECT "Your bug report.")
  (ZWEI:ADD-TEXT-START "I need more information in order to track
down the bug that you reported."))

K*] 4ZMail Can Reformat Headers.*

You can now tell ZMail to reformat headers of messages in Babyl files for
display.  For example, you can easily cause certain boring header fields not to be
shown.

To do this, create a mail template as described above to do the editing you want.
Set the ZMail option 1*DEFAULT-REFORMATTING-TEMPLATE** to the name of that
template.  The template will be called only once on any given message; from
then on, the Babyl file remembers both the original header and the reformatted
one.

For example, this template would cause Return-path fields not to appear:

1(ZWEI:DEFINE-MAIL-TEMPLATE DISCARD-BORING-FIELDS
      "Delete fields I don't like to see displayed."
  (ZWEI:DELETE-FIELD ':RETURN-PATH))

L*] 4Moving Messages by Individual Filters.*

In ZMail, 1Map Over* of 1Move to File* now allows you to move each message to a
different mail file, determined from the message itself using the filter--mail-file
associations.  Each message is individually matched against the filters to find the
mail file to move it to; if a message does not match any of the filters that are
associated with mail files, that message is not moved.

To do this, click middle on 1Move to File* after 1Map Over*, or choose
1By Individual Filters* from the menu from clicking right on 1Move to File*.

4Changes in Patches:

    1A**] 4Explicit Release of Patches.
    1B**] 3LOAD-PATCHES4 Reloads Site Files When** 4Needed.
    1C**] 3MAKE-SYSTEM4 of a Patchable System Loads Patches.
    1D***] 3SI:LOAD-AND-SAVE-PATCHES4 Queries At The Beginning.


1A***] 4Explicit Release of Patches.*

It is now possible to finish a patch so that it can be loaded experimentally without
releasing the patch for users in general to load.

To do this, use the command 1Meta-X Finish Patch Unreleased* instead of the
usual 1Meta-X Finish Patch*.

This will finish the patch, compiling it, but mark it in the patch directory as
unreleased.  By default, 1LOAD-PATCHES* requires confirmation before loading an
unreleased patch, and warns the user that doing so is risky.  1LOAD-PATCHES* in
nonselective mode will not load them at all.  You can load the patch, see that
it works reasonably, fix it if necessary by editing and recompiling the patch
file source, and finally release or cancel the patch.  You release the patch with
1Meta-X Release Patch* or cancel it with 1Meta-X Cancel Patch*.

Loading an unreleased patch warns the user not to try to save the band, and
sets the status of the patchable system to 1:INCONSISTENT*.  This tells 1DISK-SAVE*
to require extra confirmation after a stern warning.  If you do save such a
band, the disk label will say it is "Bad".  These precautions are because
unreleased patches are still subject to change, and a band dumped with them
might possibly contain invalid patches that differ from the patches ultimately
released.

If 1:UNRELEASED* is passed as an argument to 1LOAD-PATCHES*, it will load
unreleased patches with no special confirmation.  1DISK-SAVE* will still warn,
however.

1B*] 3LOAD-PATCHES4 Reloads Site Files When** 4Needed.

1LOAD-PATCHES** now checks whether the site files have changed since they were
last loaded, and loads them if that is appropriate.  This is done by default if you
do not specify which systems to load patches for.  Additionally, you can say to
do this with the argument 1:SITE* and say not to do this with the argument
1:NOSITE*.

1C*] 3MAKE-SYSTEM4 of a Patchable System Loads Patches.**

If you load a patchable system with 1MAKE-SYSTEM*, it automatically loads
patches.  It queries for loading patches if it was querying for loading the
system's files.

1D*] 3SI:LOAD-AND-SAVE-PATCHES4 Queries At The Beginning.

1SI:LOAD-AND-SAVE-PATCHES*** now will ask all the questions it needs to ask before
loading the patches.  Its first argument is now the partition to save into; only
the following arguments are passed to 1LOAD-PATCHES*.  Once it has begin loading
patches, no questions will be asked unless something goes wrong (such as, the
specified partition is not big enough).

This should make it convenient to use this function to update a band when you
are ready to go home, and leave it running when you go.
