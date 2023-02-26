#!/bin/sh

rm -v patch/cwarns-*.lisp*
rm -v site/hsttbl.lisp

find . -name '\#FILE*' -exec rm -v {} \;
find . -iname '*.qfasl' -exec rm -v {} \;
find . -iname '*.qfasl~' -exec rm -v {} \;


# Restore fonts, and UCINIT QFASL.

fossil revert demo/tvbgar.qfasl
fossil revert demo/wormch.qfasl
fossil revert fonts/

fossil revert sys/ucinit.qfasl
