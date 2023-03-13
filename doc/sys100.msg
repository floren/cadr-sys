;-*- Mode:Text; Fonts:(TR12 CPTFONT TR12I HL12B TR12B TR12BI); Readtable:T; Base:10 -*-

System 100 is now released.  It uses microcode version 323.  It will
not run with earlier microcode versions.

There are no major incompatibilities between Systems 99 and 100, they
should for the most part be identical.  Files compiled in System 99
4will* run in System 100.

System 100 is the content of TID/671 -- possibly the last back up of
the Lisp Machine for the CADR, restored to a working state.

You should also read the file 1SYS: DOC; SYS99 MSG*, which
describe improvements that where made in System 99.

Contents of the rest of this file:

4   License Change* 
4   Incompatible Changes* 
4   Window System Changes* 
4   User Interface Changes* 
4   Editor Changes* 
4   Network Changes* 


4License Change*

      3Restrain social decay -- help get programmers sharing again.*

Everything is now redistributed and/or modifed under the terms of the
GNU Affero General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

4Incompatible Changes* 

] 4Disk kludges*

>> Hard coded disk size?

] 4Kludges relating to Unix file names has been removed*

This also means that the canonical file type on Unix is the same as on
the Lisp Machine (i.e., not truncated).

4Window System Changes* 

4User Interface Changes* 

] 4CADRLP prompts for version number when assembling microcode*

If CADRLP cannot figure out what the next microcode version number
will be it will prompt the user for one.  This occurs on file systems
that do not support version numbers (e.g. Unix).

4Editor Changes* 

4Network Changes* 

] 4MIT-OZ is not really OZ*

>> MINI hard codes to OZ.
>> OZ changed to pretend to be Unix.

4Site option Changes* 
