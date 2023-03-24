;-*- Mode:Text; Fonts:(TR12 CPTFONT TR12I HL12B TR12B TR12BI); Readtable:T; Base:10 -*-

This file will document network changes to be implemented in a future system.
Most network description information will be available via a server, or perhaps a
file.


]4 New meaning for site option* 3:CHAOS* 

Before, this site option was merely 1T* or 1NIL*.  Now, a non-1NIL* value of
this option should be a string, the name of the Chaosnet.  This is to
formalize the fact that there is more than one Chaosnet in the world,
and to facilitate addressing a Chaosnet other than the local one, which
which probably be implemented through a special gateway server.  A
site's Chaosnet name is usually the site name.

]4 New site option* 3:INTERNET* 

This site option will be needed when IP is implemented for the Lisp Machine.
The value is a list of Internet subnetwork specifications.  Each specification
associates an Internet subnet with some kind of transport, usually
another type of network capable of encapsulating packets.  The spec is
of the form:

 (2internet-address transport-type . transport-parameters*) 

The Internet address actually describes a subnet of the Internet, and
will have at least one trailing zero field.  Currently defined transport
types are:

	1:CHAOS* 2chaosnet-name* 2subnet* A Chaosnet which forms
	part of the Internet by encapsulating Internet packets
	in Chaosnet UNC packets.  The subnet field is decribed below.

	1:ETHERNET* 1T* Ethernet II hardware which is an IP-active cable.
	In this case, Internet packets are in Ethernet frames with the
	standard Internet type code for Ethernet.  The T is a
	placeholder until some way of differentating IP/Ethernet
	networks is needed, if ever.

The purpose of such an option is to allow the Lisp Machine to determine,
assuming it has an NCP for Internet, how to use its interfaces and local
networks to talk IP.  For example, a Lambda running at MIT could use
either of the above transports.  It can use 1:CHAOS* because of the Internet
routing set up at MIT (even though the Chaos network at the Lambda
itself is using Ethernet as a packet medium  !), and it can use the
Ethernet interface to talk IP also, since Ethernet provides for this.
If the Lambda was just given its Internet addresses, it would have no
way of knowing for sure what interface or network it was supposed to
use.  (The site option is most useful if a machine has more than one interface to
the Internet.)

For example, MIT's Chaosnet serves as TCP medium:

	1("18.20.0.0" :CHAOS "MIT")* 

The first two components of the Internet address must be non-zero.  The
remaining fields yield the Chaosnet subnet number and host number.  If the
third field is non-zero, then this is used; the subnet number is thrown away.
This is an instance of the local Chaosnet functioning as a Class C network.
Unless your site has a particularly large number (> 5) of existing Chaosnet
subnets, it is better to ask (from the NIC) for a class C network number for
each subnet.  This will include most sites; only the MIT Chaosnet really
requires a Class B network.  Note that the Chaosnet internal (subnet)
routing is transparent from an Internet point of view, even when several
Class C networks (carrying Chaosnet packets) are in use.  At MIT, Internet
118.20.26.64* (decimal numbers) maps to Chaosnet subnet 32, host 100
(octal numbers).

Things are more complicated in the Class C case.  It is desirable for the
third component of the Internet address for the subnet to be the same
as the Chaos subnet number, but this cannot always be the case.  In such
a case (which will be fairly common), the subnet number must be obtained
in a way other than looking at the third component.  The subnet parameter
mentioned above provides the correct Chaos subnet.  Here are two
translations:

	1("192.13.4.0" :CHAOS "WIDGET")* 
	1("192.13.5.0" :CHAOS "WIDGET" 6)* 

The WIDGET Chaosnet has two subnets numbered 4 and 6, but they could not
get an Internet Class C subnet number that matched 1*.*.6.0*.
So, 1192.13.4.5* maps to Chaos subnet 4, host 5, while Internet
1"192.13.5.5* maps to subnet 6, host 5.

A ``TCP on a LAN'' using Ethernet would probably look something like
this:

	1("128.31.6.0" :ETHERNET T)* 

In the Ethernet scheme, address translation to Ethernet hardware address
is accomplished by the address resolution protocol.  The option here is
merely for informative purposes.
 
#|

]4 New site option* 3:NET-REGIONS* 

Describes an ``area'' of the network.

	1:CHAOS* 2subnets*   One argument of 1T* includes all subnets.  Currently only
	        refers to the local Chaosnet.
	1:INTERNET* 2subnets*  Matching is done.
	1:REGION* 2region-names*  (Obviously, circularity is discouraged !)

A list of the form 1(:EXCEPT .* 2subnets1)** is also allowed.

The new predicate 1(NET:IN-NETWORK-REGION-P* network network-address
region-name1)* uses this.

|#
