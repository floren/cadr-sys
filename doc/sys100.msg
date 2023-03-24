;-*- Mode:Text; Fonts:(TR12 CPTFONT TR12I HL12B TR12B TR12BI); Readtable:T; Base:10 -*-

System 100 is now released.  It uses microcode version 323.  It will
not run with earlier microcode versions.

System 100 is the restored content of TID/671, a backup of OZ from the
MIT ToTS (Tapes of Tech Square) project, that contained a full backup
of the Lisp Machine operating system -- possibly the last back up and
version of the Lisp Machine for the CADR.

You should also read the file 1SYS: DOC; SYS99 MSG*, which
describe the improvements that where made in System 99.

There are no major incompatibilities between Systems 99.32 and 100,
they should for the most part be identical.  Files compiled in System
99 4will* run in System 100.  All systems have had their version
incremented by one to avoid confusion.

The Lisp Machine Manual, Sixth Edition (June 1984) pairs with System
100.

Contents of the rest of this file:

4   License Change*
4   Summary of Changes Against TID/671* 
4   Incompatible Changes* 
4   Known Bugs* 
4   User Interface Changes* 
4   Network Changes* 


4License Change*

      3Restrain social decay -- help get programmers sharing again.*

Everything is now licensed under the terms of the GNU Affero General
Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.

4Summary of Changes Against TID/671* 

The majority of changes between System 100.0 and System 99.32
(TID/671) are typos -- missing parenthesis, misspellings, and other
minor lossage.  Some changes though are critical, and quite broken.

Bringing System 100.0 (which should be equal to System 99.32) was
troublesome since one needs a similar enough system to get things into
working order.

This was only possible when we got System 98 working well enough that
it could bootstrap it self, which in turn allowed for something close
enough to System 99.0 to do the same.  The difference between System
99.0 and System 99.32 (TID/671) is almost 7 years of development with
many incompatible changes, out right broken changes, and typos.

4Incompatible Changes* 

] 4Kludges relating to Unix file names has been removed*

This also means that the canonical file type on Unix is the same as on
the Lisp Machine (i.e., not truncated).

4Known Bugs* 

] 4Blinker not working properly*

The blinker that shows where the point is does not work, this will be
addressed soon as a System patch.

4User Interface Changes* 

] 4CADRLP prompts for version number when assembling microcode*

If CADRLP cannot figure out what the next microcode version number
will be it will prompt the user for one.  This occurs on file systems
that do not support version numbers (e.g. Unix).

4Network Changes* 

] 4OZ is not really MIT-OZ*

MIT-OZ is identifies now a Unix machine (instead of TOPS-20), and has
a new Chaosnet address.  MINI hard codes the MINI server to OZ on the
same subnet.  If doing QLD against something else set
MINI-DESTINATION-ADDRESS and MINI-ROUTING-ADDRESS accordingly before
calling SI:QLD.

The host table, SITE; HOSTS TEXT has been trimmed to only list MIT-OZ
and MIT-LISPM-1.