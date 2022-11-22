#!/bin/sh

hcuot()
{
    d=$1
    fn=$2
    if [ -e "$fn" ]; then
	touch -d "$d" -- "$fn"
    fi
}

# tid/2100147

hcuot "Oct 25 23:03:03 1983" 'cc/cadld.lisp.6'			   # Author: RMS
hcuot "May  1 05:26:47 1984" 'cc/cadld.lisp.7'			   # Author: MLY
hcuot "Jul  2 16:02:39 1982" 'cc/cadreg.lisp.1'			   # Author: RPK
hcuot "Apr  7 08:02:03 1984" 'cc/cadreg.lisp.2'			   # Author: MLY
hcuot "Jul  6 19:40:03 1984" 'cc/cc.lisp.48'			   # Author: RPK
hcuot "Sep  8 21:39:15 1984" 'cc/cc.lisp.49'			   # Author: RMS
hcuot "Apr  9 04:17:09 1983" 'cc/chploc.lisp.4'			   # Author: RMS
hcuot "Dec 22 00:46:28 1982" 'cc/dcfu.uload.1'			   # Author: PGS
hcuot "Oct 27 18:54:38 1983" 'cc/dcheck.lisp.6'			   # Author: RMS
hcuot "Oct 25 22:54:36 1983" 'cc/diags.lisp.157'		   # Author: RMS
hcuot "Oct  9 03:57:49 1983" 'cc/dmon.lisp.56'			   # Author: RMS
hcuot "Apr  7 08:03:28 1984" 'cc/lcadmc.lisp.30'		   # Author: MLY
hcuot "Aug 18 12:35:04 1983" 'cc/lcadrd.lisp.94'		   # Author: SORAYAMA
hcuot "Sep  7 20:19:50 1984" 'cc/patch-3.directory.22'		   # Author: JGB
hcuot "Aug 30 19:36:34 1984" 'cc/qf.lisp.125'			   # Author: RMS
hcuot "Oct 18 15:04:38 1983" 'cc/salvag.lisp.37'		   # Author: RMS
hcuot "Sep 10 16:11:17 1984" 'cold/coldld.lisp.83'		   # Author: RMS
hcuot "Aug 26 21:36:53 1984" 'cold/coldpk.lisp.20'		   # Author: MLY
hcuot "Jul 11 22:06:39 1984" 'cold/coldut.lisp.97'		   # Author: MLY
hcuot "Jul  2 05:39:28 1984" 'cold/defmic.lisp.194'		   # Author: MLY
hcuot "Jul 26 02:20:42 1984" 'cold/docmic.lisp.37'		   # Author: MLY
hcuot "Sep 26 04:23:45 1984" 'cold/docmic.lisp.38'		   # Author: MLY
hcuot "Feb  1 18:30:00 1983" 'cold/mini.lisp.88'		   # Author: MLY
hcuot "Sep  2 02:23:17 1984" 'cold/qcom.lisp.579'		   # Author: RMS
hcuot "Dec  6 03:29:00 1984" 'cold/qcom.lisp.581'		   # Author: MLY
hcuot "Aug 27 17:39:24 1984" 'cold/qdefs.lisp.386'		   # Author: MLY
hcuot "Sep 12 11:24:45 1984" 'cold/qdefs.lisp.387'		   # Author: RMS
hcuot "Apr  7 08:09:02 1984" 'demo/doctor.lisp.9'		   # Author: MLY
hcuot "Jun 19 20:51:57 1983" 'demo/hcedit.lisp.27'		   # Author: RMS
hcuot "Jul 21 09:52:55 1982" 'demo/liss.lisp.4'			   # Author: RMS
hcuot "Nov 18 09:00:51 1983" 'demo/worm.lisp.8'			   # Author: RMS
hcuot "Feb 20 18:08:57 1984" 'distribution/dist.lisp.7'		   # Author: MLY
hcuot "Oct 24 12:34:36 1984" 'doc/sys99.msg.37'			   # Author: RPK
hcuot "Nov  9 05:28:17 1984" 'doc/sys99.msg.38'			   # Author: MLY
hcuot "Nov  9 04:44:53 1984" 'eh/eh.lisp.338'			   # Author: MLY
hcuot "Nov  9 05:09:25 1984" 'eh/ehc.lisp.234'			   # Author: MLY
hcuot "Jun  9 07:25:10 1984" 'eh/ehf.lisp.207'			   # Author: MLY
hcuot "Oct 14 05:01:23 1984" 'eh/ehf.lisp.226'			   # Author: MLY
hcuot "Jan  3 02:50:32 1984" 'file/copy.lisp.128'		   # Author: RPK
hcuot "Jan  3 03:30:45 1984" 'file/fs.lisp.76'			   # Author: RPK
hcuot "Nov 21 15:54:37 1984" 'file/fs-48.directory.15'		   # Author: LMFILE
hcuot "Feb  8 09:17:10 1984" 'file/fs-48-4.lisp.1'		   # Author: MLY
hcuot "Jul 16 11:16:25 1984" 'file/fs-49.directory.2'		   # Author: RPK
hcuot "May 26 10:20:04 1984" 'file/fsdefs.lisp.173'		   # Author: MLY
hcuot "Feb  8 07:20:18 1984" 'file/fsguts.lisp.369'		   # Author: MLY
hcuot "Jun 10 08:14:33 1984" 'file/fsname.lisp.104'		   # Author: MLY
hcuot "Jul 16 12:47:56 1984" 'file/fsstr.lisp.105'		   # Author: RPK
hcuot "Apr  7 08:14:13 1984" 'file/hogs.lisp.4'			   # Author: MLY
hcuot "Sep 11 23:35:02 1984" 'file/lmpars.lisp.112'		   # Author: RMS
hcuot "Jan  2 23:38:57 1984" 'file/mtaux.lisp.76'		   # Author: RPK
hcuot "Jun 20 00:21:53 1983" 'file/mtdefs.lisp.28'		   # Author: RMS
hcuot "Jan  1 23:28:11 1984" 'file/mtstr.lisp.84'		   # Author: RPK
hcuot "Dec 17 22:58:53 1983" 'file/server.directory.5'		   # Author: RMS
hcuot "May 26 06:40:07 1984" 'file/server-8.directory.13'	   # Author: LISPM
hcuot "Apr  7 08:15:26 1984" 'file/zmail.lisp.4'		   # Author: MLY
hcuot "Dec 18 16:58:54 1983" 'file2/anydir.lisp.200'		   # Author: RMS
hcuot "Dec 18 04:21:17 1983" 'file2/area.lisp.21'		   # Author: RMS
hcuot "Jan 19 05:25:43 1983" 'file2/complt.lisp.16'		   # Author: RMS
hcuot "May 12 18:06:24 1984" 'file2/defs.lisp.186'		   # Author: MLY
hcuot "Jan 18 11:47:44 1984" 'file2/diread.lisp.60'		   # Author: MLY
hcuot "Dec 18 16:52:09 1983" 'file2/dump.lisp.28'		   # Author: RMS
hcuot "Dec 18 16:59:11 1983" 'file2/files.lisp.121'		   # Author: RMS
hcuot "Dec 18 04:21:27 1983" 'file2/free.lisp.47'		   # Author: RMS
hcuot "Jan 20 01:10:20 1983" 'file2/gc.lisp.18'			   # Author: RMS
hcuot "Dec 18 16:59:16 1983" 'file2/io.lisp.93'			   # Author: RMS
hcuot "Jan 19 23:29:26 1983" 'file2/link.lisp.45'		   # Author: RMS
hcuot "Dec 18 04:09:21 1983" 'file2/lmfile.directory.3'		   # Author: RMS
hcuot "Jul  7 01:32:09 1983" 'file2/maiser.lisp.9'		   # Author: LISPM
hcuot "Nov  8 01:24:01 1984" 'file2/maiser.lisp.12'		   # Author: LISPM
hcuot "Dec 18 16:59:05 1983" 'file2/node.lisp.161'		   # Author: RMS
hcuot "Dec 18 16:59:29 1983" 'file2/pack.lisp.81'		   # Author: RMS
hcuot "Aug 13 14:14:51 1984" 'file2/pathnm.lisp.162'		   # Author: RPK
hcuot "Jan 19 23:29:32 1983" 'file2/pdp10.lisp.19'		   # Author: RMS
hcuot "Dec 18 16:52:11 1983" 'file2/salvag.lisp.21'		   # Author: RMS
hcuot "Aug  3 02:37:58 1984" 'file2/server.lisp.45'		   # Author: RMS
hcuot "Nov  8 05:07:49 1984" 'file2/server.lisp.46'		   # Author: LISPM
hcuot "Dec 18 16:51:53 1983" 'file2/spcdir.lisp.91'		   # Author: RMS
hcuot "Jan 18 11:49:14 1984" 'file2/stream.lisp.209'		   # Author: MLY
hcuot "Apr  8 04:43:36 1983" 'file2/system.lisp.31'		   # Author: LISPM
hcuot "Aug 31 11:21:00 1984" 'io/crdtbl.lisp.34'		   # Author: RMS
hcuot "Nov 21 12:02:41 1984" 'io/disk.lisp.291'			   # Author: LMFILE
hcuot "Mar 13 01:13:51 1984" 'io/dledit.lisp.51'		   # Author: RMS
hcuot "May  5 22:06:55 1984" 'io/dribbl.lisp.35'		   # Author: MLY
hcuot "Aug  2 12:33:09 1984" 'io/format.lisp.234'		   # Author: MLY
hcuot "Oct 28 15:22:16 1984" 'io/format.lisp.237'		   # Author: RMS
hcuot "Oct 29 18:51:56 1983" 'io/fread.lisp.29'			   # Author: RMS
hcuot "Nov 29 16:17:27 1983" 'io/grind.lisp.143'		   # Author: RMS
hcuot "Jun 13 18:36:43 1984" 'io/qio.lisp.210'			   # Author: MERMAN.JAN
hcuot "Jun 29 04:43:34 1982" 'io/rcomp.lisp.9'			   # Author: RMS
hcuot "May 14 03:31:39 1984" 'io/rddefs.lisp.51'		   # Author: RMS
hcuot "Jun 29 10:45:33 1984" 'io/rdtbl.lisp.167'		   # Author: MLY
hcuot "Apr 29 20:27:56 1984" 'io/rtc.lisp.40'			   # Author: MLY
hcuot "Jan  1 05:19:35 1984" 'io/stream.lisp.108'		   # Author: RMS
hcuot "Apr 10 09:02:57 1984" 'io/unibus.lisp.25'		   # Author: MLY
hcuot "Oct  9 04:11:17 1984" 'io/access.lisp.11'		   # was 'io/file/access.lisp.11' # Author: MLY
hcuot "Sep 11 23:09:45 1984" 'io/open.lisp.175'			   # was 'io/file/open.lisp.175' # Author: RMS
hcuot "Nov 20 23:41:36 1984" 'io/pathnm.lisp.534'		   # was 'io/file/pathnm.lisp.534' # Author: MLY
hcuot "Nov 29 00:58:22 1984" 'io/pathnm.lisp.535'		   # was 'io/file/pathnm.lisp.535' # Author: MLY
hcuot "Sep  5 12:53:30 1984" 'io/pathst.lisp.173'		   # was 'io/file/pathst.lisp.173' Author: RMS
hcuot "Oct  2 22:12:52 1982" 'io1/cdrive.lisp.102'		   # Author: RMS
hcuot "Oct 12 15:34:02 1983" 'io1/chatst.lisp.65'		   # Author: ALR
hcuot "Jun 24 12:02:29 1984" 'io1/conver.lisp.146'		   # Author: MLY
hcuot "Sep 16 05:08:40 1982" 'io1/draw.lisp.22'			   # Author: RMS
hcuot "Nov 28 17:43:10 1983" 'io1/eftp.lisp.37'			   # Author: PHILIP
hcuot "Dec 11 07:01:42 1983" 'io1/fntcnv.lisp.79'		   # Author: DANIEL.G.MLY
hcuot "Jul  3 16:30:24 1984" 'io1/fquery.lisp.43'		   # Author: MLY
hcuot "Feb  4 06:35:07 1984" 'io1/infix.lisp.6'			   # Author: RMS
hcuot "Feb 17 14:38:14 1984" 'io1/meter.lisp.37'		   # Author: MLY
hcuot "Mar  9 15:49:43 1984" 'io1/output.lisp.35'		   # Author: MLY
hcuot "Jul 22 04:54:42 1983" 'io1/relld.lisp.9'			   # Author: RMS
hcuot "Jun  5 01:00:45 1984" 'io1/rfontw.lisp.80'		   # Author: MLY
hcuot "May 25 15:17:24 1984" 'io1/srccom.lisp.36'		   # Author: MLY
hcuot "Jul  3 11:46:39 1984" 'io1/time.lisp.105'		   # Author: MERMAN.JAN
hcuot "Feb 16 10:07:45 1984" 'io1/timpar.lisp.72'		   # Author: RPK
hcuot "Jan 22 07:30:47 1983" 'man/areas.text.42'		   # Author: RMS
hcuot "Feb 25 06:14:29 1983" 'man/chaos.text.11'		   # Author: RMS
hcuot "May 15 05:12:57 1984" 'man/code.text.35'			   # Author: RMS
hcuot "May 21 00:56:32 1984" 'man/compil.text.103'		   # Author: RMS
hcuot "Jun  8 02:33:45 1984" 'man/cumulative.vars.23'		   # Author: RMS
hcuot "Mar 30 17:42:46 1984" 'man/db-aid.text.11'		   # Author: MLY
hcuot "May 15 04:06:19 1984" 'man/debug.text.17'		   # Author: RMS
hcuot "Jun  2 19:58:32 1984" 'man/defstr.text.116'		   # Author: RMS
hcuot "May 15 18:10:23 1984" 'man/errors.text.98'		   # Author: RMS
hcuot "May 12 06:05:00 1984" 'man/fd-arr.text.22'		   # Author: RMS
hcuot "May 19 15:21:33 1984" 'man/fd-con.text.27'		   # Author: RMS
hcuot "May 20 19:21:21 1984" 'man/fd-dtp.text.18'		   # Author: RMS
hcuot "May 12 04:30:00 1984" 'man/fd-eva.text.36'		   # Author: RMS
hcuot "May 11 00:04:25 1984" 'man/fd-fio.text.19'		   # Author: RMS
hcuot "May 16 15:48:07 1984" 'man/fd-flo.text.21'		   # Author: RMS
hcuot "May  3 02:56:02 1984" 'man/fd-fun.text.20'		   # Author: RMS
hcuot "May 16 15:07:20 1984" 'man/fd-hac.text.36'		   # Author: RMS
hcuot "Mar 14 03:59:33 1984" 'man/fd-loc.text.5'		   # Author: RMS
hcuot "Jun  4 03:29:31 1984" 'man/fd-num.text.36'		   # Author: RMS
hcuot "Feb  3 03:15:49 1984" 'man/fd-op.text.3'			   # Author: RMS
hcuot "May 20 04:25:10 1984" 'man/fd-sg.text.15'		   # Author: RMS
hcuot "May 16 15:08:13 1984" 'man/fd-str.text.21'		   # Author: RMS
hcuot "Mar 14 04:16:25 1984" 'man/fd-sub.text.12'		   # Author: RMS
hcuot "May 12 02:37:41 1984" 'man/fd-sym.text.10'		   # Author: RMS
hcuot "May 14 07:42:32 1984" 'man/files.text.14'		   # Author: RMS
hcuot "May  1 00:11:54 1984" 'man/flavor.text.123'		   # Author: RMS
hcuot "May 12 06:24:17 1984" 'man/generic.text.12'		   # Author: RMS
hcuot "Jun  8 00:18:44 1984" 'man/index.temp.1'			   # Author: RMS
hcuot "Mar 22 01:21:38 1984" 'man/init.text.14'			   # Author: RMS
hcuot "Jun  1 04:32:32 1984" 'man/intro.text.17'		   # Author: RMS
hcuot "May  1 16:45:38 1984" 'man/ios.text.241'			   # Author: MLY
hcuot "Aug  1 17:27:18 1982" 'man/looptm.lispm.1'		   # Author: MOON
hcuot "Mar 20 01:37:36 1984" 'man/looptm.text.315'		   # Author: RMS
hcuot "May  1 16:46:06 1984" 'man/macros.text.97'		   # Author: MLY
hcuot "Mar 23 03:50:23 1984" 'man/manual.lisp.30'		   # Author: RMS
hcuot "Jun  8 01:37:59 1984" 'man/manual.vars.25'		   # Author: RMS
hcuot "Mar 10 18:21:36 1984" 'man/manual2.bolio.1'		   # Author: RMS
hcuot "Mar 23 03:52:08 1984" 'man/manual2.text.7'		   # Author: RMS
hcuot "May 19 00:12:09 1984" 'man/packd.text.104'		   # Author: RMS
hcuot "May 19 14:30:22 1984" 'man/patch.text.53'		   # Author: RMS
hcuot "Jun  1 04:34:19 1984" 'man/pathnm.text.98'		   # Author: RMS
hcuot "May 15 05:15:46 1984" 'man/query.text.19'		   # Author: RMS
hcuot "May 14 07:42:08 1984" 'man/rdprt.text.20'		   # Author: RMS
hcuot "May 12 02:31:29 1984" 'man/resour.text.24'		   # Author: RMS
hcuot "Apr  3 19:19:33 1984" 'man/stream.text.33'		   # Author: RMS
hcuot "Mar 30 17:46:11 1984" 'man/time.text.36'			   # Author: MLY
hcuot "Jun  1 04:42:15 1984" 'man/title.text.10'		   # Author: RMS
hcuot "Nov 20 23:42:13 1984" 'network/host.lisp.120'		   # Author: MLY
hcuot "Sep  1 09:51:24 1984" 'network/package.lisp.6'		   # Author: RMS
hcuot "May 29 23:54:46 1984" 'network/service.lisp.2'		   # Author: RPK
hcuot "Jun  4 14:56:40 1984" 'io/chsaux.lisp.350'		   # was 'network/chaos/chsaux.lisp.350' # Author: MLY
hcuot "Aug  8 09:12:21 1984" 'io/chsaux.lisp.359'		   # was 'network/chaos/chsaux.lisp.359' # Author: RPK
hcuot "Nov  9 14:28:15 1984" 'io/chsaux.lisp.362'		   # was 'network/chaos/chsaux.lisp.362' # Author: MLY
hcuot "Nov 18 14:15:11 1984" 'io/chsaux.lisp.365'		   # was 'network/chaos/chsaux.lisp.365' # Author: MLY
hcuot "Jan 22 13:58:00 1984" 'io/chsncp.lisp.241'		   # was 'network/chaos/chsncp.lisp.241' # Author: MLY
hcuot "Nov 10 03:50:04 1984" 'io/chsncp.lisp.265'		   # was 'network/chaos/chsncp.lisp.265' # Author: LISPM
hcuot "Sep  4 13:44:32 1984" 'io/chuse.lisp.11'			   # was 'network/chaos/chuse.lisp.11' # Author: RMS
hcuot "Oct 20 15:09:37 1984" 'io/chuse.lisp.12'			   # was 'network/chaos/chuse.lisp.12' # Author: MLY
hcuot "Jul  1 15:14:32 1984" 'window/peekch.lisp.30'		   # was 'network/chaos/peekch.lisp.30' # Author: MLY
hcuot "Jan 18 09:59:01 1984" 'io/qfile.lisp.336'		   # was 'network/chaos/qfile.lisp.336' # Author: MLY
hcuot "Sep 10 15:58:49 1984" 'io/qfile.lisp.353'		   # was 'network/chaos/qfile.lisp.353' # Author: RMS
hcuot "Nov 20 21:46:45 1984" 'io/qfile.lisp.357'		   # was 'network/chaos/qfile.lisp.357' # Author: MLY
hcuot "Jul 21 01:57:31 1984" 'network/ip/hostsnic.lisp.3'	   # Author: RPK
hcuot "Nov 16 02:01:27 1984" 'patch/band.win.lisp.1'		   # Author: STRAZ
hcuot "Nov 28 03:40:52 1984" 'patch/system-98.patch-directory.303' # Author: MLY
hcuot "Nov 28 09:53:15 1984" 'patch/system-98-81.lisp.1'	   # Author: MLY
hcuot "Nov 28 09:53:39 1984" 'patch/system-98-81.qfasl.1'	   # Author: MLY
hcuot "Sep 12 11:29:05 1984" 'patch/system-99-1.lisp.2'		   # Author: RMS
hcuot "Nov  9 14:03:39 1984" 'patch/system-99-10.lisp.30'	   # Author: MLY
hcuot "Nov 14 05:59:40 1984" 'patch/system-99-11.lisp.13'	   # Author: SAZ
hcuot "Dec  2 02:01:22 1984" 'patch/system-99-13.qfasl.16'	   # Author: MLY
hcuot "Dec 12 05:38:52 1984" 'patch/system-99-14.lisp.23'	   # Author: MLY
hcuot "Dec 12 09:44:02 1984" 'patch/system-99-14.qfasl.23'	   # Author: ELISHA
hcuot "Dec 14 08:16:24 1984" 'patch/system-99-15.lisp.6'	   # Author: MLY
hcuot "Dec 14 08:16:46 1984" 'patch/system-99-15.qfasl.6'	   # Author: MLY
hcuot "Dec  9 23:17:57 1984" 'patch/system-99-16.qfasl.2'	   # Author: MLY
hcuot "Sep 12 11:50:00 1984" 'patch/system-99-2.lisp.1'		   # Author: RMS
hcuot "Sep 13 16:51:26 1984" 'patch/system-99-3.lisp.4'		   # Author: RMS
hcuot "Sep 26 10:02:44 1984" 'patch/system-99-5.lisp.9'		   # Author: MLY
hcuot "Sep 26 17:57:10 1984" 'patch/system-99-6.lisp.2'		   # Author: RMS
hcuot "Sep  9 17:45:44 1984" 'patch/zmail.patch-directory.2'	   # Author: RMS
hcuot "Feb  5 00:14:08 1984" 'sys/cadrlp.lisp.148'		   # Author: MLY
hcuot "Sep  8 21:39:01 1984" 'sys/cadrlp.lisp.149'		   # Author: RMS
hcuot "Jul 31 12:51:23 1982" 'sys/cdmp.lisp.47'			   # Author: DDM
hcuot "Sep 10 00:15:02 1984" 'sys/clpack.lisp.151'		   # Author: RMS
hcuot "Sep  8 22:13:21 1984" 'sys/eval.lisp.78'			   # Author: RMS
hcuot "Oct  8 15:58:59 1984" 'sys/ltop.lisp.496'		   # Author: MLY
hcuot "Sep  8 17:30:46 1984" 'sys/qcdefs.lisp.149'		   # Author: RMS
hcuot "Oct 19 14:57:43 1983" 'sys/qcfasd.lisp.229'		   # Author: RMS
hcuot "Aug 14 21:18:22 1984" 'sys/qcfile.lisp.321'		   # Author: RPK
hcuot "Jun 13 01:50:37 1984" 'sys/qclap.lisp.236'		   # Author: MLY
hcuot "Apr 26 00:24:10 1984" 'sys/qcluke.lisp.23'		   # Author: MLY
hcuot "Oct 24 02:49:36 1984" 'sys/qcopt.lisp.136'		   # Author: ELISHA
hcuot "Oct 28 15:42:02 1984" 'sys/qcp1.lisp.569'		   # Author: RMS
hcuot "Nov  2 03:40:55 1984" 'sys/qcp1.lisp.572'		   # Author: MLY
hcuot "Aug 28 00:18:42 1984" 'sys/qcp2.lisp.252'		   # Author: RMS
hcuot "Feb 17 04:46:04 1984" 'sys/qcpeep.lisp.34'		   # Author: MLY
hcuot "Aug 19 14:43:32 1983" 'sys/qfasl.lisp.432'		   # Author: RMS
hcuot "Aug 14 21:17:24 1984" 'sys/qfasl.lisp.461'		   # Author: RPK
hcuot "Aug 31 11:55:02 1984" 'sys/qfctns.lisp.769'		   # Author: RMS
hcuot "Oct 17 07:24:56 1984" 'sys/qfctns.lisp.770'		   # Author: MLY
hcuot "Sep 26 06:38:57 1984" 'sys/qmisc.lisp.655'		   # Author: MLY
hcuot "Oct 28 22:38:09 1984" 'sys/qmisc.lisp.658'		   # Author: MLY
hcuot "Sep  4 17:43:16 1984" 'sys/qrand.lisp.408'		   # Author: RMS
hcuot "Oct 26 01:10:35 1983" 'sys/qwmcr.lisp.20'		   # Author: RMS
hcuot "Sep 25 01:28:40 1984" 'sys/types.lisp.70'		   # Author: MLY
hcuot "May 25 18:21:06 1984" 'sys2/advise.lisp.35'		   # Author: MLY
hcuot "Nov 23 03:48:18 1983" 'sys2/class.lisp.88'		   # Author: RMS
hcuot "May 15 09:48:48 1984" 'sys2/clmac.lisp.3'		   # Author: MLY
hcuot "Jun 26 14:57:47 1984" 'sys2/defsel.lisp.69'		   # Author: MLY
hcuot "Nov 17 06:07:18 1983" 'sys2/disass.lisp.90'		   # Author: RMS
hcuot "Jul 29 19:28:00 1984" 'sys2/encaps.lisp.27'		   # Author: MLY
hcuot "Oct 14 06:12:49 1984" 'sys2/gc.lisp.170'			   # Author: MLY
hcuot "Dec  6 03:07:27 1984" 'sys2/gc.lisp.173'			   # Author: MLY
hcuot "Jun  4 02:44:01 1984" 'sys2/lmmac.lisp.356'		   # Author: RMS
hcuot "Aug  2 13:51:26 1984" 'sys2/login.lisp.85'		   # Author: MLY
hcuot "Nov 29 19:23:16 1983" 'sys2/loop.lisp.795'		   # Author: RMS
hcuot "Jun 20 12:08:26 1984" 'sys2/loop.lisp.798'		   # Author: MERMAN.JAN
hcuot "Oct 23 18:14:32 1984" 'sys2/loop.lisp.799'		   # Author: ELISHA
hcuot "Jun 16 14:18:39 1984" 'sys2/maksys.lisp.176'		   # Author: MLY
hcuot "Mar 14 13:02:00 1984" 'sys2/matrix.lisp.23'		   # Author: MLY
hcuot "Jun 17 00:43:45 1984" 'sys2/meth.lisp.61'		   # Author: MLY
hcuot "Jun 26 14:55:29 1984" 'sys2/numdef.lisp.10'		   # Author: MLY
hcuot "Oct  6 06:43:26 1984" 'sys2/numer.lisp.61'		   # Author: MLY
hcuot "Jul 26 07:25:55 1984" 'sys2/patch.lisp.158'		   # Author: MLY
hcuot "Apr 21 21:59:26 1984" 'sys2/qtrace.lisp.149'		   # Author: RMS
hcuot "Jun  3 21:02:01 1984" 'sys2/rat.lisp.38'			   # Author: MLY
hcuot "Aug  6 06:26:38 1983" 'sys2/resour.lisp.17'		   # Author: RMS
hcuot "Jun 24 00:09:43 1984" 'sys2/resour.lisp.28'		   # Author: MLY
hcuot "Feb  8 08:52:48 1984" 'sys2/selev.lisp.21'		   # Author: MLY
hcuot "Jun 17 00:31:52 1984" 'sys2/setf.lisp.91'		   # Author: MLY
hcuot "Jan 12 08:29:47 1984" 'sys2/sgdefs.lisp.54'		   # Author: DANIEL.G.MLY
hcuot "Jun 17 00:30:55 1984" 'sys2/step.lisp.67'		   # Author: MLY
hcuot "Jul 31 17:41:47 1984" 'sys2/string.lisp.141'		   # Author: MLY
hcuot "May 18 00:09:04 1984" 'sys2/struct.lisp.311'		   # Author: RMS
hcuot "Aug  3 02:04:28 1983" 'sys2/unfasl.lisp.16'		   # Author: RMS
hcuot "Oct 22 09:37:42 1983" 'sys2/usymld.lisp.183'		   # Author: DANIEL.G.MLY
hcuot "Feb 16 07:56:28 1984" 'tape/fdump.lisp.24'		   # Author: LMFILE
hcuot "Feb 16 07:56:36 1984" 'tape/fdump-def.lisp.8'		   # Author: LMFILE
hcuot "Feb 16 07:56:44 1984" 'tape/fdump-r.lisp.4'		   # Author: LMFILE
hcuot "Jan  2 22:42:55 1984" 'tape/tm.lisp.23'			   # Author: RMS
hcuot "Jan  2 22:43:02 1984" 'tape/tmdefs.lisp.6'		   # Author: RMS
hcuot "May 11 23:29:49 1984" 'tape/new/mtdefs.lisp.3'		   # Author: RPK
hcuot "Jan 11 00:38:45 1984" 'tape/new/mtrqb.lisp.2'		   # Author: RPK
hcuot "May 11 23:29:50 1984" 'tape/new/mtstr.lisp.4'		   # Author: RPK
hcuot "Jan 19 10:27:21 1984" 'tape/new/tmunit.lisp.2'		   # Author: RPK
hcuot "Jan 11 00:38:10 1984" 'tape/new/weunit.lisp.2'		   # Author: RPK
hcuot "Mar  3 02:22:09 1984" 'ubin/ucadr.locs.309'		   # Author: RMS
hcuot "Jun 16 19:45:52 1984" 'ubin/ucadr.locs.314'		   # Author: MLY
hcuot "May  6 19:19:58 1983" 'ubin/ucadr.mcr.239'		   # Author: RMS.G.DULCEY
hcuot "Jun 16 19:42:17 1984" 'ubin/ucadr.mcr.314'		   # Author: MLY
hcuot "May  6 19:20:31 1983" 'ubin/ucadr.sym.239'		   # Author: RMS.G.DULCEY
hcuot "Aug 26 08:17:12 1983" 'ubin/ucadr.sym.257'		   # Author: RMS
hcuot "Jun 16 19:42:47 1984" 'ubin/ucadr.sym.314'		   # Author: MLY
hcuot "May  6 19:22:07 1983" 'ubin/ucadr.tbl.239'		   # Author: RMS.G.DULCEY
hcuot "Jun 16 19:45:55 1984" 'ubin/ucadr.tbl.314'		   # Author: MLY
hcuot "Jun 27 01:40:14 1984" 'ucadr/uc-arith.lisp.33'		   # Author: RMS
hcuot "Nov 29 15:22:31 1983" 'ucadr/uc-array.lisp.59'		   # Author: RMS
hcuot "Jul 29 05:02:16 1983" 'ucadr/uc-cadr.lisp.7'		   # Author: RMS
hcuot "Jun  8 05:15:36 1983" 'ucadr/uc-cold-disk.lisp.9'	   # Author: RMS
hcuot "Apr  3 05:37:59 1983" 'ucadr/uc-hacks.lisp.3'		   # Author: RMS
hcuot "Oct 29 19:16:52 1983" 'ucadr/uc-interrupt.lisp.7'	   # Author: RMS
hcuot "Nov 30 21:01:55 1984" 'ucadr/uc-interrupt.lisp.8'	   # Author: PAO
hcuot "Jul 23 05:00:06 1983" 'ucadr/uc-logical.lisp.7'		   # Author: NGL
hcuot "Oct 11 15:53:14 1982" 'ucadr/uc-mc.lisp.1'		   # Author: RG
hcuot "Apr  5 02:49:33 1983" 'ucadr/uc-meter.lisp.3'		   # Author: RMS
hcuot "Jun  5 04:47:27 1983" 'ucadr/uc-page-fault.lisp.7'	   # Author: RMS
hcuot "Dec 28 01:16:45 1983" 'ucadr/uc-parameters.lisp.228'	   # Author: RMS
hcuot "Apr  5 02:49:39 1983" 'ucadr/uc-stack-groups.lisp.4'	   # Author: RMS
hcuot "Jul 29 04:34:22 1983" 'ucadr/uc-storage-allocation.lisp.15' # Author: RMS
hcuot "Jul 28 14:32:30 1984" 'ucadr/uc-string.lisp.25'		   # Author: RMS
hcuot "Dec 28 23:09:16 1983" 'ucadr/uc-transporter.lisp.22'	   # Author: RMS
hcuot "Apr  4 02:20:18 1983" 'ucadr/uc-tv.lisp.3'		   # Author: RMS
hcuot "Nov 15 23:33:30 1983" 'wind/choice.text.94'		   # Author: RMS
hcuot "Nov 22 01:10:00 1984" 'window/basstr.lisp.372'		   # Author: RMS
hcuot "Jun  6 05:46:15 1984" 'window/baswin.lisp.561'		   # Author: MLY
hcuot "Apr 22 04:43:14 1984" 'window/choice.lisp.111'		   # Author: MLY
hcuot "Jun 16 22:26:37 1984" 'window/cold.lisp.128'		   # Author: MLY
hcuot "Dec 10 23:41:19 1983" 'window/color.lisp.66'		   # Author: RMS
hcuot "Jun 29 03:12:56 1982" 'window/cometh.lisp.23'		   # Author: RMS
hcuot "Feb  6 08:55:35 1984" 'window/fed.lisp.199'		   # Author: MLY
hcuot "Jun 16 09:41:36 1983" 'window/frame.lisp.164'		   # Author: RMS
hcuot "Oct  8 15:57:28 1984" 'window/inspct.lisp.155'		   # Author: MLY
hcuot "Sep  7 12:30:18 1984" 'window/menu.lisp.104'		   # Author: RMS
hcuot "May 16 13:35:36 1984" 'window/mouse.lisp.247'		   # Author: MLY
hcuot "Aug  4 21:49:54 1984" 'window/peek.lisp.149'		   # Author: MLY
hcuot "Jun 29 03:46:44 1982" 'window/peekfs.lisp.9'		   # Author: RMS
hcuot "Apr 18 02:15:27 1984" 'window/rh.lisp.160'		   # Author: MLY
hcuot "Nov 21 02:04:19 1983" 'window/scred.lisp.106'		   # Author: RMS
hcuot "Jul 25 04:19:44 1983" 'window/scrman.lisp.165'		   # Author: RMS
hcuot "Oct  6 12:48:59 1983" 'window/scroll.lisp.175'		   # Author: RMS
hcuot "Feb  3 18:57:09 1984" 'window/sheet.lisp.554'		   # Author: MLY
hcuot "Sep  7 12:33:12 1984" 'window/shwarm.lisp.328'		   # Author: RMS
hcuot "Aug  8 09:04:05 1984" 'window/stream.lisp.144'		   # Author: MLY
hcuot "Jun 12 14:14:53 1984" 'window/supdup.lisp.272'		   # Author: RMS
hcuot "Mar 17 22:49:43 1984" 'window/sysmen.lisp.176'		   # Author: MLY
hcuot "Sep  5 22:26:39 1984" 'window/telnet-code.lisp.5'	   # Author: GANDER
hcuot "Oct 10 23:37:48 1984" 'window/tscrol.lisp.73'		   # Author: MLY
hcuot "Aug 28 19:21:19 1984" 'window/tvdefs.lisp.284'		   # Author: RMS
hcuot "Aug 20 08:24:51 1983" 'window/typwin.lisp.105'		   # Author: RMS
hcuot "Jul  7 00:08:43 1984" 'window/wholin.lisp.88'		   # Author: MLY
hcuot "Apr 10 20:28:22 1984" 'zmail/button.lisp.23'		   # Author: MLY
hcuot "Sep  9 17:58:08 1984" 'zmail/comnds.lisp.581'		   # Author: RMS
hcuot "Sep 26 06:37:47 1984" 'zmail/comnds.lisp.582'		   # Author: MLY
hcuot "Sep  9 18:51:46 1984" 'zmail/filter.lisp.355'		   # Author: RMS
hcuot "Dec  3 08:55:46 1983" 'zmail/lex733.lisp.13'		   # Author: RMS
hcuot "Jul 13 01:23:16 1984" 'zmail/mfhost.lisp.58'		   # Author: RPK
hcuot "Jul 13 01:22:59 1984" 'zmail/mfiles.lisp.323'		   # Author: RPK
hcuot "Apr  7 09:02:56 1984" 'zmail/mult.lisp.24'		   # Author: MLY
hcuot "Jul 13 01:22:00 1984" 'zmail/profil.lisp.121'		   # Author: RPK
hcuot "Apr  7 09:03:55 1984" 'zmail/refer.lisp.6'		   # Author: MLY
hcuot "Apr 30 08:38:24 1984" 'zmail/rfc733.lisp.56'		   # Author: MLY
hcuot "Sep  9 17:57:54 1984" 'zmail/top.lisp.554'		   # Author: RMS
hcuot "Jul 13 01:22:49 1984" 'zmail/window.lisp.342'		   # Author: RPK
hcuot "Jul 28 14:07:07 1984" 'zwei/coma.lisp.102'		   # Author: MLY
hcuot "Sep 26 06:38:20 1984" 'zwei/coma.lisp.103'		   # Author: MLY
hcuot "Sep  8 22:13:16 1984" 'zwei/comc.lisp.204'		   # Author: RMS
hcuot "Apr  5 10:57:40 1984" 'zwei/come.lisp.133'		   # Author: MLY
hcuot "Jun  4 21:52:51 1984" 'zwei/comg.lisp.39'		   # Author: MLY
hcuot "Mar 31 08:25:07 1984" 'zwei/comtab.lisp.307'		   # Author: MLY
hcuot "Sep  5 17:14:03 1984" 'zwei/comtab.lisp.317'		   # Author: RMS
hcuot "Jun 20 20:53:18 1984" 'zwei/doc.lisp.74'			   # Author: MLY
hcuot "Aug 10 04:28:26 1983" 'zwei/fasupd.lisp.29'		   # Author: RMS
hcuot "Feb  2 04:15:30 1984" 'zwei/font.lisp.86'		   # Author: RMS
hcuot "Jul 28 15:24:34 1984" 'zwei/for.lisp.61'			   # Author: MLY
hcuot "Oct 25 18:06:09 1983" 'zwei/indent.lisp.103'		   # Author: RMS
hcuot "Apr  7 09:06:12 1984" 'zwei/insert.lisp.33'		   # Author: MLY
hcuot "Jun  6 01:17:18 1984" 'zwei/kbdmac.lisp.47'		   # Author: MLY
hcuot "May 13 18:52:44 1984" 'zwei/mouse.lisp.96'		   # Author: MLY
hcuot "Dec 23 00:50:27 1983" 'zwei/nprim.lisp.33'		   # Author: RMS
hcuot "Sep 12 23:07:41 1984" 'zwei/pated.lisp.26'		   # Author: RMS
hcuot "Dec 16 07:12:34 1982" 'zwei/pl1mod.lisp.13'		   # Author: RMS
hcuot "Aug  4 20:39:09 1984" 'zwei/primit.lisp.174'		   # Author: MLY
hcuot "Oct 13 06:50:53 1984" 'zwei/sectio.lisp.267'		   # Author: MLY
hcuot "Jul  4 12:36:58 1984" 'zwei/stream.lisp.167'		   # Author: MLY
hcuot "Jul  8 14:17:30 1984" 'zwei/zmnew.lisp.33'		   # Author: MLY
hcuot "Jun 19 19:31:52 1983" 'zwei/zymurg.lisp.41'		   # Author: RMS

# tid/9003304

hcuot "Dec 16 14:32:14 1983" 'sys/qcdefs.lisp.128'
hcuot "Sep  8 17:30:46 1984" 'sys/qcdefs.lisp.149'
hcuot "Jan 30 06:11:13 1985" 'sys/qcdefs.lisp.153'
hcuot "Sep  9 14:00:57 1984" 'sys/qcdefs.qfasl.149'
hcuot "Oct 19 14:57:43 1983" 'sys/qcfasd.lisp.229'
hcuot "Sep 10 17:05:15 1984" 'sys/qcfasd.lisp.248'
hcuot "Sep 10 17:05:43 1984" 'sys/qcfasd.qfasl.248'
hcuot "Mar 27 20:58:18 1984" 'sys/qcfile.lisp.307'
hcuot "Aug 14 21:18:22 1984" 'sys/qcfile.lisp.321'
hcuot "Jan 30 10:48:52 1985" 'sys/qcfile.lisp.324'
hcuot "Sep  6 20:12:10 1984" 'sys/qcfile.qfasl.322'
hcuot "Jun 13 01:50:37 1984" 'sys/qclap.lisp.236'
hcuot "Sep  8 17:30:16 1984" 'sys/qclap.lisp.244'
hcuot "Sep  9 14:16:55 1984" 'sys/qclap.qfasl.244'
hcuot "Apr 26 00:24:10 1984" 'sys/qcluke.lisp.23'
hcuot "Aug 30 07:51:41 1984" 'sys/qcluke.lisp.26'
hcuot "Aug 30 14:38:31 1984" 'sys/qcluke.qfasl.26'
hcuot "Nov 29 17:18:23 1983" 'sys/qcopt.lisp.95'
hcuot "Aug  1 23:15:22 1984" 'sys/qcopt.lisp.126'
hcuot "Oct 24 02:49:36 1984" 'sys/qcopt.lisp.136'
hcuot "Nov  6 07:41:16 1984" 'sys/qcopt.lisp.137'
hcuot "Sep  9 14:13:24 1984" 'sys/qcopt.qfasl.133'
hcuot "Jul  4 21:23:04 1984" 'sys/qcp1.lisp.547'
hcuot "Aug 28 00:30:17 1984" 'sys/qcp1.lisp.550'
hcuot "Oct 28 15:42:02 1984" 'sys/qcp1.lisp.569'
hcuot "Nov  2 03:40:55 1984" 'sys/qcp1.lisp.572'
hcuot "Dec 11 13:38:25 1984" 'sys/qcp1.lisp.573'
hcuot "Sep  9 14:03:10 1984" 'sys/qcp1.qfasl.562'
hcuot "Jun 17 08:30:49 1984" 'sys/qcp2.lisp.246'
hcuot "Aug 28 00:18:42 1984" 'sys/qcp2.lisp.252'
hcuot "Oct 28 15:41:46 1984" 'sys/qcp2.lisp.261'
hcuot "Sep  9 14:09:07 1984" 'sys/qcp2.qfasl.259'
hcuot "Aug 17 11:51:51 1983" 'sys/qcpeep.lisp.31'
hcuot "Feb 17 04:46:04 1984" 'sys/qcpeep.lisp.34'
hcuot "Aug  2 21:31:47 1984" 'sys/qcpeep.lisp.36'
hcuot "Aug  2 21:31:56 1984" 'sys/qcpeep.qfasl.36'
hcuot "Jan 11 15:27:40 1984" 'sys/qev.lisp.289'
hcuot "Aug 19 14:43:32 1983" 'sys/qfasl.lisp.432'
hcuot "Jun 26 14:57:53 1984" 'sys/qfasl.lisp.459'
hcuot "Aug 14 21:17:24 1984" 'sys/qfasl.lisp.461'
hcuot "Oct 28 22:42:43 1984" 'sys/qfasl.lisp.462'
hcuot "Feb 26 04:27:46 1985" 'sys/qfasl.lisp.463'
hcuot "Aug 14 23:35:35 1984" 'sys/qfasl.qfasl.461'
hcuot "Jun 12 01:51:12 1984" 'sys/qfctns.lisp.753'
hcuot "Aug 31 11:55:02 1984" 'sys/qfctns.lisp.769'
hcuot "Oct 17 07:24:56 1984" 'sys/qfctns.lisp.770'
hcuot "Nov 16 19:51:17 1984" 'sys/qfctns.lisp.774'
hcuot "Aug 31 11:59:24 1984" 'sys/qfctns.qfasl.769'
hcuot "Nov 28 11:40:06 1983" 'sys/qmisc.lisp.590'
hcuot "Sep 26 06:38:57 1984" 'sys/qmisc.lisp.655'
hcuot "Oct 28 22:38:09 1984" 'sys/qmisc.lisp.658'
hcuot "Dec 14 02:04:10 1984" 'sys/qmisc.lisp.659'
hcuot "Aug 31 16:20:48 1984" 'sys/qmisc.qfasl.652'
hcuot "Jun 21 12:36:00 1983" 'sys/qnew.lisp.19'
hcuot "Apr  3 08:55:26 1984" 'sys/qnew.lisp.20'
hcuot "Aug 14 23:54:18 1984" 'sys/qnew.qfasl.20'
hcuot "Jun 17 00:31:08 1984" 'sys/qrand.lisp.395'
hcuot "Sep  4 17:43:16 1984" 'sys/qrand.lisp.408'
hcuot "Dec  1 14:28:06 1984" 'sys/qrand.lisp.411'
hcuot "Jul  9 14:51:08 1985" 'sys/qrand.lisp.412'
hcuot "Sep  4 17:45:01 1984" 'sys/qrand.qfasl.408'
hcuot "Oct 26 01:10:35 1983" 'sys/qwmcr.lisp.20'
hcuot "Dec 11 08:56:51 1984" 'sys/qwmcr.lisp.22'
hcuot "Dec 11 08:56:58 1984" 'sys/qwmcr.qfasl.22'
hcuot "Jun 18 02:57:37 1983" 'sys/recom.lisp.33'
hcuot "Aug 27 02:28:30 1982" 'sys/sgfctn.lisp.57'
hcuot "Aug 15 00:13:05 1984" 'sys/sgfctn.qfasl.57'
hcuot "Oct 10 02:20:38 1983" 'sys/sort.lisp.59'
hcuot "Aug 15 00:13:45 1984" 'sys/sort.qfasl.59'
hcuot "Sep 25 01:05:36 1984" 'sys/sysdcl.lisp.186'
hcuot "Feb  2 19:20:43 1985" 'sys/sysdcl.lisp.187'
hcuot "Feb  8 22:19:38 1985" 'sys/sysdcl.lisp.188'
hcuot "Feb 12 03:46:00 1985" 'sys/sysdcl.lisp.189'
hcuot "Feb 12 04:22:41 1985" 'sys/sysdcl.lisp.190'
hcuot "Feb 14 23:34:21 1985" 'sys/sysdcl.lisp.191'
hcuot "Feb 28 09:52:49 1985" 'sys/sysdcl.lisp.192'
hcuot "May  2 21:12:41 1985" 'sys/sysdcl.lisp.193'
hcuot "May  3 17:42:45 1985" 'sys/sysdcl.qfasl.193'
hcuot "Jun 12 02:06:02 1984" 'sys/types.lisp.48'
hcuot "Sep 25 01:28:40 1984" 'sys/types.lisp.70'
hcuot "Jan 28 20:23:45 1985" 'sys/types.lisp.72'
hcuot "Sep  8 21:39:45 1984" 'sys/types.qfasl.69'
hcuot "Jun 29 02:49:58 1982" 'sys/ucinit.qfasl.1'
hcuot "May 25 18:21:06 1984" 'sys2/advise.lisp.35'
hcuot "Feb 18 02:47:48 1985" 'sys2/advise.lisp.38'
hcuot "Aug 14 21:43:06 1984" 'sys2/advise.qfasl.37'
hcuot "Oct  6 12:50:44 1983" 'sys2/analyze.lisp.16'
hcuot "Sep 26 03:58:58 1984" 'sys2/analyze.lisp.18'
hcuot "Feb 24 05:42:25 1985" 'sys2/analyze.lisp.19'
hcuot "Sep 11 01:03:49 1984" 'sys2/analyze.qfasl.17'
hcuot "Aug 21 18:06:13 1983" 'sys2/band.lisp.43'
hcuot "Jul 27 02:09:35 1984" 'sys2/band.lisp.44'
hcuot "Nov 24 02:31:51 1984" 'sys2/band.qfasl.46'
hcuot "Jul 29 21:02:02 1984" 'sys2/character.lisp.16'
hcuot "Sep 25 01:22:45 1984" 'sys2/character.lisp.21'
hcuot "Feb  4 01:18:04 1985" 'sys2/character.lisp.22'
hcuot "Sep  7 16:04:06 1984" 'sys2/character.qfasl.20'
hcuot "Nov 23 03:48:18 1983" 'sys2/class.lisp.88'
hcuot "Jun 14 23:56:23 1984" 'sys2/class.lisp.99'
hcuot "Sep  4 15:31:45 1984" 'sys2/class.qfasl.99'
hcuot "May 15 09:48:48 1984" 'sys2/clmac.lisp.3'
hcuot "Aug 24 04:12:36 1984" 'sys2/clmac.lisp.4'
hcuot "Aug 28 21:55:00 1984" 'sys2/clmac.qfasl.4'
hcuot "Apr  7 08:50:06 1984" 'sys2/cmany.lisp.46'
hcuot "Apr  7 08:50:53 1984" 'sys2/condit.lisp.2'
hcuot "Feb  8 07:06:51 1984" 'sys2/defmac.lisp.74'
hcuot "May 16 16:43:32 1984" 'sys2/defmac.lisp.75'
hcuot "Feb 13 10:25:14 1985" 'sys2/defmac.lisp.80'
hcuot "Aug 29 15:55:29 1984" 'sys2/defmac.qfasl.78'
hcuot "Jun 26 14:57:47 1984" 'sys2/defsel.lisp.69'
hcuot "Aug 28 20:45:38 1984" 'sys2/defsel.lisp.70'
hcuot "Aug 29 01:26:03 1984" 'sys2/defsel.qfasl.70'
hcuot "Feb 13 08:15:24 1985" 'sys2/describe.lisp.2'
hcuot "Feb 15 01:49:03 1985" 'sys2/describe.lisp.3'
hcuot "Nov 17 06:07:18 1983" 'sys2/disass.lisp.90'
hcuot "Dec 14 07:05:57 1984" 'sys2/disass.lisp.94'
hcuot "Aug  1 16:38:35 1984" 'sys2/disass.qfasl.92'
hcuot "Oct  9 03:05:36 1983" 'sys2/encaps.lisp.19'
hcuot "Jul 29 19:28:00 1984" 'sys2/encaps.lisp.27'
hcuot "Nov 28 09:50:50 1984" 'sys2/encaps.lisp.28'
hcuot "Aug 14 22:10:54 1984" 'sys2/encaps.qfasl.27'
hcuot "Jul 30 12:54:40 1984" 'sys2/flavor.lisp.272'
hcuot "Sep 10 17:44:11 1984" 'sys2/flavor.lisp.280'
hcuot "Feb 11 00:03:31 1985" 'sys2/flavor.lisp.283'
hcuot "Sep 10 23:12:37 1984" 'sys2/flavor.qfasl.280'
hcuot "Apr 28 17:39:18 1984" 'sys2/gc.lisp.165'
hcuot "Oct 14 06:12:49 1984" 'sys2/gc.lisp.170'
hcuot "Dec  6 03:07:27 1984" 'sys2/gc.lisp.173'
hcuot "Dec 14 03:01:18 1984" 'sys2/gc.lisp.174'
hcuot "Aug 14 22:34:41 1984" 'sys2/gc.qfasl.169'
hcuot "Jun  4 19:39:39 1984" 'sys2/hash.lisp.83'
hcuot "Oct 13 06:53:10 1984" 'sys2/hash.lisp.88'
hcuot "Mar  2 08:03:29 1985" 'sys2/hash.lisp.89'
hcuot "Aug 14 22:51:05 1984" 'sys2/hash.qfasl.87'
hcuot "Apr 16 14:35:12 1984" 'sys2/hashfl.lisp.24'
hcuot "Jun 13 19:02:46 1984" 'sys2/hashfl.lisp.28'
hcuot "Oct 23 22:53:53 1984" 'sys2/hashfl.lisp.31'
hcuot "Mar  6 00:23:07 1985" 'sys2/hashfl.lisp.33'
hcuot "Aug 14 22:52:39 1984" 'sys2/hashfl.qfasl.29'
hcuot "Apr  7 08:51:47 1984" 'sys2/let.lisp.8'
hcuot "Jun  4 02:44:01 1984" 'sys2/lmmac.lisp.356'
hcuot "Aug 31 14:13:38 1984" 'sys2/lmmac.lisp.372'
hcuot "Nov 29 11:06:17 1984" 'sys2/lmmac.lisp.382'
hcuot "Feb 18 03:04:28 1985" 'sys2/lmmac.lisp.387'
hcuot "Feb 28 22:22:28 1985" 'sys2/lmmac.lisp.388'
hcuot "May  2 21:13:45 1985" 'sys2/lmmac.lisp.389'
hcuot "Aug 31 15:54:02 1984" 'sys2/lmmac.qfasl.372'
hcuot "May  9 06:09:16 1984" 'sys2/login.lisp.83'
hcuot "Aug  2 13:51:26 1984" 'sys2/login.lisp.85'
hcuot "Sep  3 20:04:30 1984" 'sys2/login.lisp.87'
hcuot "Sep  3 21:06:25 1984" 'sys2/login.qfasl.87'
hcuot "Nov 29 19:23:16 1983" 'sys2/loop.lisp.795'
hcuot "Jun 20 12:08:26 1984" 'sys2/loop.lisp.798'
hcuot "Oct 23 18:14:32 1984" 'sys2/loop.lisp.799'
hcuot "Dec  9 00:37:37 1984" 'sys2/loop.lisp.829'
hcuot "Oct 24 03:03:31 1984" 'sys2/loop.qfasl.799'
hcuot "May  2 21:54:59 1985" 'sys2/macarr.lisp.2'
hcuot "May  2 21:55:13 1985" 'sys2/macarr.qfasl.2'
hcuot "Jan 25 01:41:37 1984" 'sys2/maksys.lisp.174'
hcuot "Jun 16 14:18:39 1984" 'sys2/maksys.lisp.176'
hcuot "Sep 13 16:53:32 1984" 'sys2/maksys.lisp.180'
hcuot "Sep  4 16:16:36 1984" 'sys2/maksys.qfasl.178'
hcuot "Mar 14 13:02:00 1984" 'sys2/matrix.lisp.23'
hcuot "Apr  9 11:08:41 1984" 'sys2/matrix.lisp.26'
hcuot "Aug 29 21:20:25 1984" 'sys2/matrix.qfasl.26'
hcuot "Jun 17 00:43:45 1984" 'sys2/meth.lisp.61'
hcuot "Sep  4 15:35:26 1984" 'sys2/meth.lisp.63'
hcuot "Sep  4 15:35:37 1984" 'sys2/meth.qfasl.63'
hcuot "Jun 26 14:55:29 1984" 'sys2/numdef.lisp.10'
hcuot "Oct  6 06:43:45 1984" 'sys2/numdef.lisp.12'
hcuot "Sep 10 16:30:29 1984" 'sys2/numdef.qfasl.11'
hcuot "Mar 13 16:55:49 1984" 'sys2/numer.lisp.43'
hcuot "Jul  2 15:42:29 1984" 'sys2/numer.lisp.58'
hcuot "Oct  6 06:43:26 1984" 'sys2/numer.lisp.61'
hcuot "Dec 14 02:58:42 1984" 'sys2/numer.lisp.62'
hcuot "Sep 10 16:32:20 1984" 'sys2/numer.qfasl.60'
hcuot "Mar 21 22:17:42 1984" 'sys2/patch.lisp.150'
hcuot "Jul 26 07:25:55 1984" 'sys2/patch.lisp.158'
hcuot "Oct 20 15:21:29 1984" 'sys2/patch.lisp.165'
hcuot "Aug 14 23:22:21 1984" 'sys2/patch.qfasl.158'
hcuot "Aug 29 18:33:36 1984" 'sys2/plane.lisp.32'
hcuot "Aug 29 18:45:45 1984" 'sys2/plane.qfasl.32'
hcuot "Feb 17 05:36:34 1984" 'sys2/proces.lisp.153'
hcuot "Nov  9 15:05:07 1984" 'sys2/proces.lisp.158'
hcuot "Feb 13 09:50:42 1985" 'sys2/proces.lisp.159'
hcuot "Aug 14 23:28:32 1984" 'sys2/proces.qfasl.157'
hcuot "May 12 20:30:56 1984" 'sys2/prodef.lisp.40'
hcuot "Aug 31 13:58:32 1984" 'sys2/prodef.lisp.48'
hcuot "Feb 13 09:59:15 1985" 'sys2/prodef.lisp.49'
hcuot "Aug 31 13:59:54 1984" 'sys2/prodef.qfasl.48'
hcuot "Apr 21 21:59:26 1984" 'sys2/qtrace.lisp.149'
hcuot "Dec  5 19:41:34 1984" 'sys2/qtrace.lisp.152'
hcuot "Sep  4 15:43:34 1984" 'sys2/qtrace.qfasl.151'
hcuot "Jun  3 21:02:01 1984" 'sys2/rat.lisp.38'
hcuot "Sep  4 16:08:45 1984" 'sys2/rat.lisp.46'
hcuot "Sep 10 16:47:13 1984" 'sys2/rat.qfasl.46'
hcuot "Aug  6 06:26:38 1983" 'sys2/resour.lisp.17'
hcuot "Jun 24 00:09:43 1984" 'sys2/resour.lisp.28'
hcuot "Nov 10 03:26:41 1984" 'sys2/resour.lisp.31'
hcuot "Aug 15 00:08:50 1984" 'sys2/resour.qfasl.28'
hcuot "Feb  8 08:52:48 1984" 'sys2/selev.lisp.21'
hcuot "Feb 13 08:19:42 1985" 'sys2/selev.lisp.24'
hcuot "Aug 28 21:54:21 1984" 'sys2/selev.qfasl.23'
hcuot "May 20 02:10:47 1984" 'sys2/setf.lisp.86'
hcuot "Jun 17 00:31:52 1984" 'sys2/setf.lisp.91'
hcuot "Oct 29 03:56:33 1984" 'sys2/setf.lisp.97'
hcuot "Aug 31 12:48:05 1984" 'sys2/setf.qfasl.95'
hcuot "Jan 12 08:29:47 1984" 'sys2/sgdefs.lisp.54'
hcuot "Dec  2 03:03:49 1984" 'sys2/sgdefs.lisp.55'
hcuot "Feb  7 00:26:28 1985" 'sys2/sgdefs.lisp.57'
hcuot "Aug 14 21:40:43 1984" 'sys2/sgdefs.qfasl.54'
hcuot "Jun 17 00:30:55 1984" 'sys2/step.lisp.67'
hcuot "Sep 26 02:01:17 1984" 'sys2/step.lisp.72'
hcuot "Aug 15 00:15:30 1984" 'sys2/step.qfasl.70'
hcuot "Apr  6 03:08:15 1984" 'sys2/string.lisp.130'
hcuot "Jul 31 17:41:47 1984" 'sys2/string.lisp.141'
hcuot "Sep 25 01:20:50 1984" 'sys2/string.lisp.147'
hcuot "Sep 10 01:15:39 1984" 'sys2/string.qfasl.146'
hcuot "Nov 28 23:45:39 1983" 'sys2/struct.lisp.292'
hcuot "May 18 00:09:04 1984" 'sys2/struct.lisp.311'
hcuot "Jul 31 17:42:25 1984" 'sys2/struct.lisp.322'
hcuot "Aug 14 16:20:04 1984" 'sys2/struct.qfasl.322'
hcuot "Aug  3 02:04:28 1983" 'sys2/unfasl.lisp.16'
hcuot "Oct  9 08:52:31 1984" 'sys2/unfasl.lisp.19'
hcuot "Sep 11 01:52:22 1984" 'sys2/unfasl.qfasl.18'
hcuot "Oct 22 09:37:42 1983" 'sys2/usymld.lisp.183'
hcuot "Sep  8 21:39:09 1984" 'sys2/usymld.lisp.187'
hcuot "Sep  8 21:49:15 1984" 'sys2/usymld.qfasl.187'
hcuot "Jan  3 02:50:32 1984" 'tape/copy.lisp.128'
hcuot "Feb 16 07:56:19 1984" 'tape/copy.lisp.133'
hcuot "Jan  3 03:50:47 1984" 'tape/copy.qfasl.128'
hcuot "Jan 19 10:25:49 1984" 'tape/ddoc.text.4'
hcuot "May 11 23:49:07 1984" 'tape/ddoc.text.8'
hcuot "Jan 19 10:25:57 1984" 'tape/fdump.lisp.18'
hcuot "Feb 16 07:56:28 1984" 'tape/fdump.lisp.24'
hcuot "May 11 23:29:43 1984" 'tape/fdump.lisp.27'
hcuot "Jan  9 22:56:50 1984" 'tape/fdump-def.lisp.5'
hcuot "Feb 16 07:56:36 1984" 'tape/fdump-def.lisp.8'
hcuot "May 11 23:52:14 1984" 'tape/fdump-def.lisp.12'
hcuot "Jan  2 03:37:10 1984" 'tape/fdump-def.qfasl.1'
hcuot "Jan  9 21:54:15 1984" 'tape/fdump-file-cdate-i.lisp.1'
hcuot "Jan  9 22:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'
hcuot "Jan 19 10:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'
hcuot "Feb 16 07:56:44 1984" 'tape/fdump-r.lisp.4'
hcuot "May 11 23:29:45 1984" 'tape/fdump-r.lisp.5'
hcuot "Jan  3 04:36:11 1984" 'tape/magtape.directory.11'
hcuot "Oct 26 15:41:54 1983" 'tape/magtape-14.directory.14'
hcuot "Mar  8 00:56:47 1983" 'tape/magtape-14-1.qfasl.1'
hcuot "Apr 25 03:51:48 1983" 'tape/magtape-14-3.qfasl.1'
hcuot "May 18 22:11:34 1983" 'tape/magtape-14-4.qfasl.3'
hcuot "Oct 26 15:41:16 1983" 'tape/magtape-14-5.qfasl.1'
hcuot "Feb 16 08:24:04 1984" 'tape/magtape-22.directory.13'
hcuot "Jan  7 16:40:45 1984" 'tape/magtape-22-1.lisp.1'
hcuot "Jan  7 16:40:56 1984" 'tape/magtape-22-1.qfasl.1'
hcuot "Jan  7 17:28:27 1984" 'tape/magtape-22-2.lisp.1'
hcuot "Jan  7 17:28:40 1984" 'tape/magtape-22-2.qfasl.1'
hcuot "Jan  7 18:41:18 1984" 'tape/magtape-22-3.lisp.1'
hcuot "Jan  7 18:41:44 1984" 'tape/magtape-22-3.qfasl.1'
hcuot "Jan 13 07:06:26 1984" 'tape/magtape-22-4.lisp.1'
hcuot "Jan 13 07:06:35 1984" 'tape/magtape-22-4.qfasl.1'
hcuot "Jan 19 11:40:22 1984" 'tape/magtape-22-5.lisp.1'
hcuot "Jan 19 11:40:32 1984" 'tape/magtape-22-5.qfasl.1'
hcuot "Feb 16 08:23:22 1984" 'tape/magtape-22-6.lisp.1'
hcuot "Feb 16 08:23:28 1984" 'tape/magtape-22-6.qfasl.1'
hcuot "Jan 13 06:25:26 1984" 'tape/mtaux.lisp.79'
hcuot "Jan 19 11:04:02 1984" 'tape/mtaux.lisp.80'
hcuot "Jan  3 03:52:48 1984" 'tape/mtaux.qfasl.77'
hcuot "Jun 20 00:21:53 1983" 'tape/mtdefs.lisp.28'
hcuot "Dec 16 09:34:10 1983" 'tape/mtdefs.lisp.30'
hcuot "Jan  3 03:46:18 1984" 'tape/mtdefs.qfasl.30'
hcuot "Jan  7 17:43:06 1984" 'tape/mtstr.lisp.86'
hcuot "Jan 10 23:40:52 1984" 'tape/mtstr.lisp.87'
hcuot "Jan  3 03:47:58 1984" 'tape/mtstr.qfasl.85'
hcuot "Jan  3 02:50:55 1984" 'tape/odump.lisp.1'
hcuot "Jan  3 04:33:05 1984" 'tape/odump.qfasl.1'
hcuot "May 11 23:29:46 1984" 'tape/package.lisp.1'
hcuot "Jan  3 01:59:49 1984" 'tape/pdp10.lisp.1'
hcuot "May 12 02:31:18 1984" 'tape/rmunit.lisp.3'
hcuot "May 11 23:29:46 1984" 'tape/system.lisp.3'
hcuot "Jan  2 22:42:55 1984" 'tape/tm.lisp.23'
hcuot "May 11 23:29:47 1984" 'tape/tm.lisp.25'
hcuot "Jan  2 22:43:02 1984" 'tape/tmdefs.lisp.6'
hcuot "May 11 23:29:48 1984" 'tape/tmdefs.lisp.7'
hcuot "May 12 01:27:24 1984" 'tape/unit.lisp.7'
hcuot "Jan  3 02:01:02 1984" 'tape/vms.lisp.1'
hcuot "Jan  3 02:01:02 1984" 'tape/vms.lisp.2'
hcuot "May 11 23:29:49 1984" 'tape/new/mtdefs.lisp.3'
hcuot "May 12 01:28:11 1984" 'tape/new/mtdefs.lisp.4'
hcuot "May 12 01:45:03 1984" 'tape/new/mtdefs.qfasl.4'
hcuot "Jan 11 00:38:45 1984" 'tape/new/mtrqb.lisp.2'
hcuot "May 11 23:29:49 1984" 'tape/new/mtrqb.lisp.3'
hcuot "May 11 23:29:50 1984" 'tape/new/mtstr.lisp.4'
hcuot "May 12 02:31:35 1984" 'tape/new/mtstr.lisp.5'
hcuot "Jan 19 10:27:21 1984" 'tape/new/tmunit.lisp.2'
hcuot "May 11 23:29:50 1984" 'tape/new/tmunit.lisp.5'
hcuot "Jan 11 00:38:10 1984" 'tape/new/weunit.lisp.2'
hcuot "May 11 23:29:51 1984" 'tape/new/weunit.lisp.3'
hcuot "Nov 20 17:29:49 1982" 'ubin/dcfu.uload.4'
hcuot "Aug  4 01:23:05 1982" 'ubin/memd.uload.1'
hcuot "Dec 11 06:08:01 1984" 'ubin/ucadr.loc.321'
hcuot "May  6 19:22:05 1983" 'ubin/ucadr.locs.239'
hcuot "Aug 26 08:18:51 1983" 'ubin/ucadr.locs.257'
hcuot "Mar  3 02:22:09 1984" 'ubin/ucadr.locs.309'
hcuot "Jun 16 19:45:52 1984" 'ubin/ucadr.locs.314'
hcuot "Sep 11 15:24:11 1984" 'ubin/ucadr.locs.320'
hcuot "May  6 19:19:58 1983" 'ubin/ucadr.mcr.239'
hcuot "Aug 26 08:16:41 1983" 'ubin/ucadr.mcr.257'
hcuot "Mar  3 02:20:07 1984" 'ubin/ucadr.mcr.309'
hcuot "Jun 16 19:42:17 1984" 'ubin/ucadr.mcr.314'
hcuot "Sep 11 15:21:19 1984" 'ubin/ucadr.mcr.320'
hcuot "Dec 11 06:04:48 1984" 'ubin/ucadr.mcr.321'
hcuot "May  6 19:20:31 1983" 'ubin/ucadr.sym.239'
hcuot "Aug 26 08:17:12 1983" 'ubin/ucadr.sym.257'
hcuot "Mar  3 02:20:33 1984" 'ubin/ucadr.sym.309'
hcuot "Jun 16 19:42:47 1984" 'ubin/ucadr.sym.314'
hcuot "Sep 11 15:22:04 1984" 'ubin/ucadr.sym.320'
hcuot "Dec 11 06:05:39 1984" 'ubin/ucadr.sym.321'
hcuot "May  6 19:22:07 1983" 'ubin/ucadr.tbl.239'
hcuot "Aug 26 08:18:53 1983" 'ubin/ucadr.tbl.257'
hcuot "Mar  3 02:22:10 1984" 'ubin/ucadr.tbl.309'
hcuot "Jun 16 19:45:55 1984" 'ubin/ucadr.tbl.314'
hcuot "Sep 11 15:24:18 1984" 'ubin/ucadr.tbl.320'
hcuot "Dec 11 06:08:04 1984" 'ubin/ucadr.tbl.321'
hcuot "Apr  9 04:19:01 1983" 'ucadr/cadldb.lisp.20'
hcuot "Jul 26 04:31:51 1983" 'ucadr/cadldb.qfasl.20'
hcuot "Jun 29 04:56:11 1982" 'ucadr/cadtlk.mid.9'
hcuot "Jun 29 04:56:32 1982" 'ucadr/chaos.test.1'
hcuot "Jun 29 04:56:46 1982" 'ucadr/dcfu.text.23'
hcuot "Dec 22 00:46:28 1982" 'ucadr/dcfu.uload.3'
hcuot "Jun 29 04:59:34 1982" 'ucadr/memd.lisp.26'
hcuot "Jun 29 04:59:39 1982" 'ucadr/mmtest.lisp.15'
hcuot "Jun 29 04:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1'
hcuot "Nov 17 00:26:42 1982" 'ucadr/packed.lisp.119'
hcuot "Oct 14 18:41:23 1983" 'ucadr/packed.lisp.124'
hcuot "Jun 29 05:00:13 1982" 'ucadr/praid.lisp.21'
hcuot "Jun 29 05:00:18 1982" 'ucadr/promh.text.9'
hcuot "Nov 18 08:23:35 1983" 'ucadr/uc-arith.lisp.25'
hcuot "Apr 30 17:43:08 1984" 'ucadr/uc-arith.lisp.28'
hcuot "Jun 27 01:40:14 1984" 'ucadr/uc-arith.lisp.33'
hcuot "Oct  6 05:49:21 1984" 'ucadr/uc-arith.lisp.34'
hcuot "Jul 23 04:01:24 1983" 'ucadr/uc-array.lisp.28'
hcuot "Nov 29 15:22:31 1983" 'ucadr/uc-array.lisp.59'
hcuot "Jun 16 19:36:02 1984" 'ucadr/uc-array.lisp.63'
hcuot "Mar 31 16:16:21 1983" 'ucadr/uc-array-cache.lisp.1'
hcuot "Jul 29 05:02:16 1983" 'ucadr/uc-cadr.lisp.7'
hcuot "Jun  1 21:53:36 1984" 'ucadr/uc-cadr.lisp.8'
hcuot "Jan 27 02:37:59 1984" 'ucadr/uc-call-return.lisp.97'
hcuot "Jul 26 22:48:47 1984" 'ucadr/uc-call-return.lisp.103'
hcuot "Sep 11 15:15:57 1984" 'ucadr/uc-call-return.lisp.108'
hcuot "Feb 11 23:46:46 1985" 'ucadr/uc-call-return.lisp.109'
hcuot "Oct 11 02:19:04 1982" 'ucadr/uc-chaos.lisp.1'
hcuot "Oct 14 20:27:48 1982" 'ucadr/uc-cold-disk.lisp.2'
hcuot "Jun  8 05:15:36 1983" 'ucadr/uc-cold-disk.lisp.9'
hcuot "Nov 14 04:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'
hcuot "Oct 11 02:18:14 1982" 'ucadr/uc-disk.lisp.1'
hcuot "Nov 14 02:21:19 1983" 'ucadr/uc-disk.lisp.2'
hcuot "Sep 13 03:27:33 1983" 'ucadr/uc-fctns.lisp.40'
hcuot "May 10 23:41:36 1984" 'ucadr/uc-fctns.lisp.75'
hcuot "Jun 16 15:24:03 1984" 'ucadr/uc-fctns.lisp.78'
hcuot "Aug  3 04:30:30 1984" 'ucadr/uc-fctns.lisp.84'
hcuot "Jun 12 20:12:27 1985" 'ucadr/uc-fctns.lisp.85'
hcuot "Apr  3 05:37:59 1983" 'ucadr/uc-hacks.lisp.3'
hcuot "Oct 17 11:11:57 1983" 'ucadr/uc-hacks.lisp.5'
hcuot "Feb  4 03:53:01 1983" 'ucadr/uc-interrupt.lisp.4'
hcuot "Oct 29 19:16:52 1983" 'ucadr/uc-interrupt.lisp.7'
hcuot "Nov 30 21:01:55 1984" 'ucadr/uc-interrupt.lisp.8'
hcuot "Dec 10 03:51:17 1984" 'ucadr/uc-interrupt.lisp.9'
hcuot "Jul 23 05:00:06 1983" 'ucadr/uc-logical.lisp.7'
hcuot "Mar  2 22:56:48 1984" 'ucadr/uc-logical.lisp.8'
hcuot "Apr  4 00:09:21 1983" 'ucadr/uc-macrocode.lisp.9'
hcuot "Nov 13 20:49:50 1983" 'ucadr/uc-macrocode.lisp.28'
hcuot "Jul  2 05:39:55 1984" 'ucadr/uc-macrocode.lisp.29'
hcuot "Oct 11 15:53:14 1982" 'ucadr/uc-mc.lisp.1'
hcuot "Nov 13 20:47:25 1983" 'ucadr/uc-mc.lisp.2'
hcuot "Apr  5 02:49:33 1983" 'ucadr/uc-meter.lisp.3'
hcuot "Jul 23 06:00:11 1983" 'ucadr/uc-meter.lisp.4'
hcuot "Aug  1 03:39:57 1983" 'ucadr/uc-meter.lisp.5'
hcuot "Jun  5 04:47:27 1983" 'ucadr/uc-page-fault.lisp.7'
hcuot "Oct 17 11:11:44 1983" 'ucadr/uc-page-fault.lisp.10'
hcuot "Nov 21 03:24:14 1983" 'ucadr/uc-page-fault.lisp.13'
hcuot "Oct 29 17:36:55 1983" 'ucadr/uc-parameters.lisp.222'
hcuot "Dec 28 01:16:45 1983" 'ucadr/uc-parameters.lisp.228'
hcuot "Dec 10 02:16:18 1984" 'ucadr/uc-parameters.lisp.230'
hcuot "Oct 11 02:18:51 1982" 'ucadr/uc-pup.lisp.1'
hcuot "Nov 16 04:34:29 1983" 'ucadr/uc-stack-closure.lisp.3'
hcuot "Jan 22 21:33:37 1984" 'ucadr/uc-stack-closure.lisp.6'
hcuot "Jul 26 22:49:03 1984" 'ucadr/uc-stack-closure.lisp.10'
hcuot "Aug 31 13:43:35 1984" 'ucadr/uc-stack-closure.lisp.11'
hcuot "Feb 11 23:47:05 1985" 'ucadr/uc-stack-closure.lisp.12'
hcuot "Apr  5 02:49:39 1983" 'ucadr/uc-stack-groups.lisp.4'
hcuot "Jul 23 05:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'
hcuot "Jul 29 04:34:22 1983" 'ucadr/uc-storage-allocation.lisp.15'
hcuot "May 18 22:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'
hcuot "Jul 20 04:00:15 1983" 'ucadr/uc-string.lisp.13'
hcuot "Jul 28 14:32:30 1984" 'ucadr/uc-string.lisp.25'
hcuot "Sep  6 14:03:25 1984" 'ucadr/uc-string.lisp.26'
hcuot "Apr  3 05:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'
hcuot "Aug 15 08:07:26 1983" 'ucadr/uc-transporter.lisp.9'
hcuot "Dec 28 23:09:16 1983" 'ucadr/uc-transporter.lisp.22'
hcuot "May  1 03:20:21 1984" 'ucadr/uc-transporter.lisp.23'
hcuot "Apr  4 02:20:18 1983" 'ucadr/uc-tv.lisp.3'
hcuot "Jul 23 06:18:17 1983" 'ucadr/uc-tv.lisp.4'
hcuot "Apr  4 20:29:11 1984" 'ucadr/uc-tv.lisp.5'
hcuot "Jun 29 05:00:58 1982" 'ucadr/ucadlr.text.746'
hcuot "Nov 13 19:30:39 1983" 'ucadr/ucode.lisp.19'
hcuot "Aug 24 23:24:02 1982" 'wind/baswin.text.7'
hcuot "Aug  8 20:24:49 1983" 'wind/blink.text.21'
hcuot "Nov 15 23:33:30 1983" 'wind/choice.text.94'
hcuot "Jan 22 03:23:05 1984" 'wind/choice.text.95'
hcuot "Jul 22 22:52:44 1983" 'wind/edges.text.14'
hcuot "Aug 24 23:25:39 1982" 'wind/emack.fasl.1'
hcuot "Aug 24 23:25:27 1982" 'wind/emack.lisp.36'
hcuot "Apr  7 08:53:58 1984" 'wind/emack.lisp.37'
hcuot "Jul  5 22:46:40 1983" 'wind/fonts.text.17'
hcuot "Aug  8 20:21:25 1983" 'wind/frames.text.14'
hcuot "Aug  7 23:44:03 1983" 'wind/grafix.text.24'
hcuot "Aug  8 01:11:30 1983" 'wind/input.text.24'
hcuot "Sep 30 03:33:21 1983" 'wind/input.text.26'
hcuot "Aug 24 23:26:36 1982" 'wind/lstfla.lisp.5'
hcuot "Apr  7 08:55:43 1984" 'wind/lstfla.lisp.6'
hcuot "Aug  8 00:15:43 1983" 'wind/margin.text.20'
hcuot "Aug 23 09:42:25 1983" 'wind/misc.text.24'
hcuot "Aug  8 05:24:58 1983" 'wind/mouse.text.33'
hcuot "Aug 24 23:27:06 1982" 'wind/operat.bolio.1'
hcuot "Aug 24 23:26:46 1982" 'wind/operat.text.45'
hcuot "Aug 24 23:27:12 1982" 'wind/outlin.text.2'
hcuot "Aug  8 22:51:45 1983" 'wind/output.text.27'
hcuot "Oct 28 21:54:29 1983" 'wind/output.text.28'
hcuot "Aug  8 01:11:36 1983" 'wind/select.text.21'
hcuot "Nov 18 19:30:08 1983" 'wind/select.text.22'
hcuot "Aug  8 04:42:49 1983" 'wind/tscrol.text.37'
hcuot "Jul  5 23:25:11 1983" 'wind/typout.text.17'
hcuot "Aug  8 21:11:35 1983" 'wind/windo1.text.51'
hcuot "Feb  4 05:57:38 1984" 'wind/windo1.text.52'
hcuot "Jul  3 00:44:16 1983" 'wind/windoc.bolio.14'
hcuot "Jun 21 00:34:00 1983" 'wind/windoc.dict.1'
hcuot "Aug  8 21:12:27 1983" 'wind/windoc.log.12'
hcuot "Aug  8 21:10:00 1983" 'wind/windoc.text.15'
hcuot "Aug  8 21:23:20 1983" 'wind/windoc.vars.33'
hcuot "Aug 24 23:30:20 1982" 'wind/window.gloss.1'
hcuot "Aug 24 23:30:32 1982" 'wind/window.manual.1'
hcuot "Aug 24 23:30:42 1982" 'wind/window.methds.1'
hcuot "Aug 24 23:30:47 1982" 'wind/winman.text.1'
hcuot "Apr  7 13:52:51 1984" 'window/basstr.lisp.361'
hcuot "Nov 22 01:10:00 1984" 'window/basstr.lisp.372'
hcuot "Dec  7 05:20:04 1984" 'window/basstr.lisp.373'
hcuot "Sep  7 13:42:22 1984" 'window/basstr.qfasl.372'
hcuot "Apr 15 00:28:51 1984" 'window/baswin.lisp.559'
hcuot "Jun  6 05:46:15 1984" 'window/baswin.lisp.561'
hcuot "Sep  6 15:24:14 1984" 'window/baswin.lisp.562'
hcuot "Sep  7 13:37:06 1984" 'window/baswin.qfasl.562'
hcuot "Mar  2 22:18:07 1984" 'window/choice.lisp.110'
hcuot "Apr 22 04:43:14 1984" 'window/choice.lisp.111'
hcuot "Aug  4 20:39:30 1984" 'window/choice.lisp.116'
hcuot "Sep  7 17:32:54 1984" 'window/choice.qfasl.116'
hcuot "Aug  3 04:36:59 1983" 'window/cold.lisp.105'
hcuot "Nov 10 01:11:35 1983" 'window/cold.lisp.112'
hcuot "Jun 16 22:26:37 1984" 'window/cold.lisp.128'
hcuot "Aug 28 18:50:25 1984" 'window/cold.lisp.129'
hcuot "Aug 28 20:53:22 1984" 'window/cold.qfasl.129'
hcuot "Dec 10 23:41:19 1983" 'window/color.lisp.66'
hcuot "Oct 14 15:56:06 1984" 'window/color.lisp.69'
hcuot "Aug 29 18:54:42 1984" 'window/color.qfasl.67'
hcuot "Jun 29 03:12:56 1982" 'window/cometh.lisp.23'
hcuot "Aug  4 06:29:09 1983" 'window/cometh.lisp.26'
hcuot "Aug  3 16:17:13 1984" 'window/cometh.qfasl.26'
hcuot "Feb  2 19:32:16 1985" 'window/csrpos.lisp.10'
hcuot "Aug  3 16:53:14 1984" 'window/csrpos.qfasl.9'
hcuot "Nov 23 10:24:15 1983" 'window/fed.lisp.194'
hcuot "Feb  6 08:55:35 1984" 'window/fed.lisp.199'
hcuot "Dec 13 18:47:42 1984" 'window/fed.lisp.200'
hcuot "Sep  7 18:51:09 1984" 'window/fed.qfasl.199'
hcuot "Jun 16 09:41:36 1983" 'window/frame.lisp.164'
hcuot "Apr 10 20:28:15 1984" 'window/frame.lisp.165'
hcuot "Sep  7 18:59:41 1984" 'window/frame.qfasl.165'
hcuot "Jun  3 21:03:15 1984" 'window/graphics.lisp.1'
hcuot "Aug  3 16:06:31 1984" 'window/graphics.qfasl.1'
hcuot "Jun  6 04:01:37 1984" 'window/inspct.lisp.153'
hcuot "Oct  8 15:57:28 1984" 'window/inspct.lisp.155'
hcuot "Nov 28 03:32:08 1984" 'window/inspct.lisp.158'
hcuot "Jan 30 10:30:23 1985" 'window/inspct.lisp.159'
hcuot "Sep  7 17:26:22 1984" 'window/inspct.qfasl.154'
hcuot "Oct 30 01:32:13 1983" 'window/menu.lisp.98'
hcuot "Feb  5 04:43:00 1984" 'window/menu.lisp.103'
hcuot "Sep  7 12:30:18 1984" 'window/menu.lisp.104'
hcuot "Oct 20 14:40:52 1984" 'window/menu.lisp.105'
hcuot "May  3 22:40:02 1985" 'window/menu.press.1'
hcuot "Sep  7 12:43:14 1984" 'window/menu.qfasl.104'
hcuot "May  1 17:20:08 1984" 'window/mouse.lisp.246'
hcuot "May 16 13:35:36 1984" 'window/mouse.lisp.247'
hcuot "Oct 10 23:37:53 1984" 'window/mouse.lisp.248'
hcuot "Aug  3 06:23:18 1984" 'window/mouse.qfasl.247'
hcuot "Jul 16 09:26:31 1984" 'window/peek.lisp.147'
hcuot "Aug  4 21:49:54 1984" 'window/peek.lisp.149'
hcuot "Sep  7 12:30:52 1984" 'window/peek.lisp.153'
hcuot "Sep  7 12:50:42 1984" 'window/peek.qfasl.153'
hcuot "May 24 20:14:17 1984" 'window/peekch.lisp.27'
hcuot "Jun 29 03:46:44 1982" 'window/peekfs.lisp.9'
hcuot "Sep  7 12:30:42 1984" 'window/peekfs.lisp.10'
hcuot "Sep  7 13:08:30 1984" 'window/peekfs.qfasl.10'
hcuot "Apr  7 08:56:29 1984" 'window/quest.lisp.43'
hcuot "Nov 29 23:28:54 1983" 'window/rh.lisp.146'
hcuot "Apr 18 02:15:27 1984" 'window/rh.lisp.160'
hcuot "Sep 11 15:05:34 1984" 'window/rh.lisp.162'
hcuot "Sep 11 15:34:52 1984" 'window/rh.qfasl.162'
hcuot "Nov 21 02:04:19 1983" 'window/scred.lisp.106'
hcuot "May  3 13:34:28 1984" 'window/scred.lisp.107'
hcuot "Oct  9 06:48:09 1984" 'window/scred.lisp.112'
hcuot "Aug  3 16:27:31 1984" 'window/scred.qfasl.111'
hcuot "Jul 25 04:19:44 1983" 'window/scrman.lisp.165'
hcuot "Dec 14 00:43:19 1984" 'window/scrman.lisp.166'
hcuot "Aug  3 05:54:11 1984" 'window/scrman.qfasl.165'
hcuot "Oct  6 12:48:59 1983" 'window/scroll.lisp.175'
hcuot "Aug  4 20:39:39 1984" 'window/scroll.lisp.176'
hcuot "Aug  4 20:45:11 1984" 'window/scroll.qfasl.176'
hcuot "Feb  3 18:57:09 1984" 'window/sheet.lisp.554'
hcuot "Dec  7 00:53:21 1984" 'window/sheet.lisp.558'
hcuot "Aug  3 05:56:23 1984" 'window/sheet.qfasl.557'
hcuot "May 20 16:13:35 1984" 'window/shwarm.lisp.321'
hcuot "Sep  7 12:33:12 1984" 'window/shwarm.lisp.328'
hcuot "Nov  1 05:35:45 1984" 'window/shwarm.lisp.332'
hcuot "Mar 11 22:08:36 1985" 'window/shwarm.lisp.334'
hcuot "Sep  7 13:31:11 1984" 'window/shwarm.qfasl.328'
hcuot "Nov 21 04:54:11 1983" 'window/stream.lisp.116'
hcuot "May 27 09:38:37 1984" 'window/stream.lisp.136'
hcuot "Aug  8 09:04:05 1984" 'window/stream.lisp.144'
hcuot "Sep  8 13:06:44 1984" 'window/stream.lisp.145'
hcuot "Sep  8 23:51:21 1984" 'window/stream.qfasl.145'
hcuot "Jun 12 14:14:53 1984" 'window/supdup.lisp.272'
hcuot "Jul  4 21:33:56 1984" 'window/supdup.lisp.276'
hcuot "Aug  3 17:14:36 1984" 'window/supdup.qfasl.276'
hcuot "Nov 28 23:56:40 1983" 'window/sysmen.lisp.173'
hcuot "Mar 17 22:49:43 1984" 'window/sysmen.lisp.176'
hcuot "Oct 11 03:55:04 1984" 'window/sysmen.lisp.178'
hcuot "Aug  3 16:22:16 1984" 'window/sysmen.qfasl.177'
hcuot "Jun 29 03:51:17 1982" 'window/task.list.1'
hcuot "Sep  5 22:26:39 1984" 'window/telnet-code.lisp.5'
hcuot "Sep  5 23:00:41 1984" 'window/telnet-code.lisp.6'
hcuot "Sep  1 00:28:56 1984" 'window/telnet-front-hack.lisp.1'
hcuot "Dec 25 21:16:03 1983" 'window/tscrol.lisp.69'
hcuot "Jun  6 05:47:06 1984" 'window/tscrol.lisp.70'
hcuot "Oct 10 23:37:48 1984" 'window/tscrol.lisp.73'
hcuot "Nov  9 14:17:36 1984" 'window/tscrol.lisp.74'
hcuot "Apr  7 12:54:59 1984" 'window/tvdefs.lisp.278'
hcuot "Aug 28 19:21:19 1984" 'window/tvdefs.lisp.284'
hcuot "Jan 28 06:05:54 1985" 'window/tvdefs.lisp.286'
hcuot "Aug 29 03:10:22 1984" 'window/tvdefs.qfasl.284'
hcuot "Aug 20 08:24:51 1983" 'window/typwin.lisp.105'
hcuot "May  1 17:22:28 1984" 'window/typwin.lisp.118'
hcuot "Sep  7 17:40:15 1984" 'window/typwin.qfasl.118'
hcuot "Jan 18 14:56:57 1984" 'window/wholin.lisp.85'
hcuot "Jul  7 00:08:43 1984" 'window/wholin.lisp.88'
hcuot "Dec 11 02:01:17 1984" 'window/wholin.lisp.92'
hcuot "Sep  4 15:02:13 1984" 'window/wholin.qfasl.90'
hcuot "Dec  9 17:26:17 1983" 'window/winddoc.lisp.2'
hcuot "Aug  1 14:49:56 1985" 'zmail/bug.zmail.1'
hcuot "Apr 12 18:45:40 1985" 'zmail/bug.zmail1.1'
hcuot "Apr 10 20:28:22 1984" 'zmail/button.lisp.23'
hcuot "Jul 13 01:17:38 1984" 'zmail/button.lisp.24'
hcuot "Sep  9 14:58:54 1984" 'zmail/button.qfasl.24'
hcuot "Apr  7 08:57:16 1984" 'zmail/cometh.lisp.51'
hcuot "Sep  9 15:11:39 1984" 'zmail/cometh.qfasl.51'
hcuot "Apr 21 17:16:43 1984" 'zmail/comnds.lisp.579'
hcuot "Jul 13 01:22:36 1984" 'zmail/comnds.lisp.580'
hcuot "Sep  9 17:58:08 1984" 'zmail/comnds.lisp.581'
hcuot "Sep 26 06:37:47 1984" 'zmail/comnds.lisp.582'
hcuot "Oct 14 05:23:33 1984" 'zmail/comnds.lisp.583'
hcuot "Sep 10 01:59:56 1984" 'zmail/comnds.qfasl.581'
hcuot "Aug 17 14:36:10 1983" 'zmail/defs.lisp.268'
hcuot "Apr 18 02:42:50 1984" 'zmail/defs.lisp.270'
hcuot "Jul 13 01:20:38 1984" 'zmail/defs.lisp.272'
hcuot "Sep  9 12:35:08 1984" 'zmail/defs.lisp.273'
hcuot "Mar 16 20:11:04 1985" 'zmail/defs.lisp.274'
hcuot "Sep  9 13:19:35 1984" 'zmail/defs.qfasl.273'
hcuot "Feb 23 07:39:32 1984" 'zmail/filter.lisp.350'
hcuot "Apr 10 23:36:04 1984" 'zmail/filter.lisp.352'
hcuot "Jul 13 01:22:09 1984" 'zmail/filter.lisp.353'
hcuot "Sep  9 18:51:46 1984" 'zmail/filter.lisp.355'
hcuot "Sep 25 01:29:51 1984" 'zmail/filter.lisp.356'
hcuot "Sep 10 02:16:57 1984" 'zmail/filter.qfasl.355'
hcuot "Jun 29 05:22:50 1982" 'zmail/info.mail.1'
hcuot "Dec  3 08:55:46 1983" 'zmail/lex733.lisp.13'
hcuot "Apr 30 09:49:02 1984" 'zmail/lex733.lisp.14'
hcuot "Sep  9 23:59:29 1984" 'zmail/lex733.qfasl.1'
hcuot "Apr  7 08:57:50 1984" 'zmail/lm.lisp.4'
hcuot "Apr  7 08:58:15 1984" 'zmail/lmcsrv.lisp.5'
hcuot "Jul 13 01:23:12 1984" 'zmail/lmfile.lisp.5'
hcuot "Sep  9 14:30:09 1984" 'zmail/lmfile.qfasl.5'
hcuot "May 16 05:06:22 1984" 'zmail/mail.lisp.308'
hcuot "Jul 13 01:22:22 1984" 'zmail/mail.lisp.310'
hcuot "Sep  9 17:58:19 1984" 'zmail/mail.lisp.311'
hcuot "Mar 16 20:10:55 1985" 'zmail/mail.lisp.312'
hcuot "Sep 10 02:07:59 1984" 'zmail/mail.qfasl.311'
hcuot "Apr  7 09:02:19 1984" 'zmail/mfhost.lisp.57'
hcuot "Jul 13 01:23:16 1984" 'zmail/mfhost.lisp.58'
hcuot "Nov 30 00:35:32 1984" 'zmail/mfhost.lisp.59'
hcuot "Sep  9 14:25:31 1984" 'zmail/mfhost.qfasl.58'
hcuot "Apr 10 23:35:11 1984" 'zmail/mfiles.lisp.322'
hcuot "Jul 13 01:22:59 1984" 'zmail/mfiles.lisp.323'
hcuot "Sep  9 17:58:00 1984" 'zmail/mfiles.lisp.324'
hcuot "Sep 10 01:49:43 1984" 'zmail/mfiles.qfasl.324'
hcuot "Apr  7 09:02:56 1984" 'zmail/mult.lisp.24'
hcuot "Jul 13 01:19:28 1984" 'zmail/mult.lisp.25'
hcuot "Sep  9 14:57:38 1984" 'zmail/mult.qfasl.25'
hcuot "Dec 10 17:37:49 1983" 'zmail/parse.lisp.52'
hcuot "Nov 15 05:02:07 1983" 'zmail/patch.directory.13'
hcuot "Aug 22 18:25:50 1983" 'zmail/patch-51-1.lisp.1'
hcuot "Sep  7 15:56:01 1983" 'zmail/patch-51-2.lisp.1'
hcuot "Sep 21 17:30:38 1983" 'zmail/patch-51-3.lisp.6'
hcuot "Sep 21 17:26:54 1983" 'zmail/patch-51-4.lisp.2'
hcuot "Sep 23 02:11:23 1983" 'zmail/patch-51-5.lisp.2'
hcuot "Sep 26 00:52:32 1983" 'zmail/patch-51-6.lisp.1'
hcuot "Oct 14 02:56:33 1983" 'zmail/patch-51-7.lisp.1'
hcuot "Oct 22 03:30:39 1983" 'zmail/patch-51-8.lisp.1'
hcuot "Oct 28 02:02:36 1983" 'zmail/patch-51-9.lisp.1'
hcuot "Oct 14 05:56:00 1984" 'zmail/patch-53.directory.50'
hcuot "Oct 14 05:59:23 1984" 'zmail/patch-53.directory.51'
hcuot "Mar 18 13:49:43 1985" 'zmail/patch-53.directory.52'
hcuot "Mar 18 13:53:51 1985" 'zmail/patch-53.directory.53'
hcuot "Mar 24 04:37:58 1985" 'zmail/patch-53.directory.54'
hcuot "Mar 24 04:39:39 1985" 'zmail/patch-53.directory.55'
hcuot "Dec  7 06:43:52 1983" 'zmail/patch-53-1.qfasl.2'
hcuot "Jan 30 00:21:26 1984" 'zmail/patch-53-10.lisp.1'
hcuot "Jan 30 00:21:32 1984" 'zmail/patch-53-10.qfasl.1'
hcuot "Feb 16 01:57:45 1984" 'zmail/patch-53-11.lisp.2'
hcuot "Feb 16 01:57:48 1984" 'zmail/patch-53-11.qfasl.2'
hcuot "Feb 23 07:40:40 1984" 'zmail/patch-53-12.lisp.2'
hcuot "Feb 23 07:40:45 1984" 'zmail/patch-53-12.qfasl.2'
hcuot "Mar  4 02:41:33 1984" 'zmail/patch-53-13.lisp.1'
hcuot "Mar  4 02:41:37 1984" 'zmail/patch-53-13.qfasl.1'
hcuot "Mar 24 11:24:31 1984" 'zmail/patch-53-14.lisp.2'
hcuot "Mar 24 11:24:35 1984" 'zmail/patch-53-14.qfasl.2'
hcuot "Apr 11 00:05:23 1984" 'zmail/patch-53-15.lisp.3'
hcuot "Apr 11 00:05:32 1984" 'zmail/patch-53-15.qfasl.3'
hcuot "Apr 18 02:41:32 1984" 'zmail/patch-53-16.lisp.1'
hcuot "Apr 18 02:41:38 1984" 'zmail/patch-53-16.qfasl.1'
hcuot "Apr 21 17:46:53 1984" 'zmail/patch-53-17.lisp.2'
hcuot "Apr 21 17:47:01 1984" 'zmail/patch-53-17.qfasl.2'
hcuot "Jun 28 22:21:13 1984" 'zmail/patch-53-18.lisp.1'
hcuot "Jun 29 02:53:32 1984" 'zmail/patch-53-18.qfasl.1'
hcuot "Oct 14 05:57:28 1984" 'zmail/patch-53-19.lisp.1'
hcuot "Oct 14 05:57:55 1984" 'zmail/patch-53-19.qfasl.1'
hcuot "Dec  5 23:18:26 1983" 'zmail/patch-53-2.lisp.1'
hcuot "Dec  5 23:18:36 1983" 'zmail/patch-53-2.qfasl.1'
hcuot "Mar 18 13:53:14 1985" 'zmail/patch-53-20.lisp.1'
hcuot "Mar 18 13:53:32 1985" 'zmail/patch-53-20.qfasl.1'
hcuot "Mar 24 04:39:04 1985" 'zmail/patch-53-21.lisp.1'
hcuot "Mar 24 04:39:07 1985" 'zmail/patch-53-21.qfasl.1'
hcuot "Dec 13 00:15:17 1983" 'zmail/patch-53-3.lisp.2'
hcuot "Dec 13 00:15:23 1983" 'zmail/patch-53-3.qfasl.2'
hcuot "Dec 14 02:54:56 1983" 'zmail/patch-53-5.lisp.1'
hcuot "Dec 14 02:55:02 1983" 'zmail/patch-53-5.qfasl.1'
hcuot "Jan  3 12:55:45 1984" 'zmail/patch-53-6.lisp.2'
hcuot "Jan  3 12:55:54 1984" 'zmail/patch-53-6.qfasl.2'
hcuot "Dec 31 19:08:53 1983" 'zmail/patch-53-7.lisp.3'
hcuot "Dec 31 19:09:00 1983" 'zmail/patch-53-7.qfasl.3'
hcuot "Jan  1 09:59:26 1984" 'zmail/patch-53-8.lisp.3'
hcuot "Jan  1 09:59:30 1984" 'zmail/patch-53-8.qfasl.3'
hcuot "Jan  1 10:00:18 1984" 'zmail/patch-53-9.lisp.2'
hcuot "Jan  1 10:00:22 1984" 'zmail/patch-53-9.qfasl.2'
hcuot "Jun 29 05:28:11 1982" 'zmail/poop.text.35'
hcuot "Apr 16 14:36:09 1984" 'zmail/profil.lisp.119'
hcuot "Jul 13 01:22:00 1984" 'zmail/profil.lisp.121'
hcuot "Sep 11 00:21:26 1984" 'zmail/profil.lisp.124'
hcuot "Sep 11 00:21:59 1984" 'zmail/profil.qfasl.124'
hcuot "Apr  7 09:03:55 1984" 'zmail/refer.lisp.6'
hcuot "Jul 13 01:22:56 1984" 'zmail/refer.lisp.7'
hcuot "Sep  9 14:29:01 1984" 'zmail/refer.qfasl.7'
hcuot "Apr 30 08:38:24 1984" 'zmail/rfc733.lisp.56'
hcuot "Jul 13 01:16:29 1984" 'zmail/rfc733.lisp.57'
hcuot "Sep  9 15:03:17 1984" 'zmail/rfc733.qfasl.57'
hcuot "Dec 30 21:02:48 1983" 'zmail/top.lisp.551'
hcuot "Jul 13 00:57:38 1984" 'zmail/top.lisp.553'
hcuot "Sep  9 17:57:54 1984" 'zmail/top.lisp.554'
hcuot "Sep 26 06:37:36 1984" 'zmail/top.lisp.555'
hcuot "Sep 10 01:45:42 1984" 'zmail/top.qfasl.554'
hcuot "Apr 10 23:35:53 1984" 'zmail/window.lisp.340'
hcuot "Jul 13 01:22:49 1984" 'zmail/window.lisp.342'
hcuot "Sep  9 17:58:28 1984" 'zmail/window.lisp.343'
hcuot "Sep 10 02:13:55 1984" 'zmail/window.qfasl.343'
hcuot "Dec 14 00:00:40 1984" 'zmail/manual/manual.text.1'
hcuot "Jun  8 03:14:17 1983" 'zmail/manual/top.txt.1'
hcuot "Jun 29 05:04:18 1982" 'zwei/.comnd.text.1'
hcuot "Jun 29 05:04:27 1982" 'zwei/atsign.xfile.1'
hcuot "Jun  9 07:24:16 1984" 'zwei/bdired.lisp.41'
hcuot "Aug  4 22:08:03 1984" 'zwei/bdired.qfasl.41'
hcuot "Jan 27 13:35:00 1983" 'zwei/bug.bugs7.1'
hcuot "Jul 30 16:32:37 1985" 'zwei/bug.zwei.1'
hcuot "Oct  8 05:11:11 1983" 'zwei/bug-zwei.text.1'
hcuot "Jun 29 05:04:29 1982" 'zwei/bugs.bugs.1'
hcuot "Jun 29 05:05:20 1982" 'zwei/bugs.bugs6.1'
hcuot "Jun 29 05:05:52 1982" 'zwei/bugs.status.1'
hcuot "Mar 15 03:23:53 1984" 'zwei/coma.lisp.101'
hcuot "Jul 28 14:07:07 1984" 'zwei/coma.lisp.102'
hcuot "Sep 26 06:38:20 1984" 'zwei/coma.lisp.103'
hcuot "Nov 15 04:03:18 1984" 'zwei/coma.lisp.104'
hcuot "Mar  4 15:10:47 1985" 'zwei/coma.lisp.105'
hcuot "Aug  3 18:18:12 1984" 'zwei/coma.qfasl.102'
hcuot "Apr  5 11:02:13 1984" 'zwei/comb.lisp.92'
hcuot "Oct 11 03:58:19 1984" 'zwei/comb.lisp.95'
hcuot "Aug  3 18:20:56 1984" 'zwei/comb.qfasl.94'
hcuot "Jun  9 07:24:12 1984" 'zwei/comc.lisp.201'
hcuot "Sep  8 22:13:16 1984" 'zwei/comc.lisp.204'
hcuot "Nov 13 22:48:57 1984" 'zwei/comc.lisp.205'
hcuot "Sep  8 23:47:20 1984" 'zwei/comc.qfasl.204'
hcuot "Jan  1 20:23:32 1984" 'zwei/comd.lisp.158'
hcuot "Aug  4 19:13:21 1984" 'zwei/comd.lisp.165'
hcuot "Oct 30 06:53:47 1984" 'zwei/comd.lisp.169'
hcuot "Sep  7 16:43:25 1984" 'zwei/comd.qfasl.167'
hcuot "Dec 12 23:42:14 1983" 'zwei/come.lisp.132'
hcuot "Apr  5 10:57:40 1984" 'zwei/come.lisp.133'
hcuot "Nov 13 22:47:10 1984" 'zwei/come.lisp.134'
hcuot "Aug  3 18:28:48 1984" 'zwei/come.qfasl.133'
hcuot "Mar 24 11:00:22 1984" 'zwei/comf.lisp.95'
hcuot "Aug  4 18:20:06 1984" 'zwei/comf.lisp.98'
hcuot "Dec 14 09:48:33 1984" 'zwei/comf.lisp.101'
hcuot "Apr 13 04:34:35 1985" 'zwei/comf.lisp.102'
hcuot "Sep  8 23:48:56 1984" 'zwei/comf.qfasl.99'
hcuot "Jun  4 21:52:51 1984" 'zwei/comg.lisp.39'
hcuot "Aug 14 21:17:48 1984" 'zwei/comg.lisp.40'
hcuot "Aug 29 03:32:06 1984" 'zwei/comg.qfasl.40'
hcuot "Apr 16 14:22:48 1984" 'zwei/comh.lisp.8'
hcuot "Aug  4 18:04:40 1984" 'zwei/comh.lisp.13'
hcuot "Aug  4 18:04:47 1984" 'zwei/comh.qfasl.13'
hcuot "Mar 31 06:33:54 1984" 'zwei/coms.lisp.81'
hcuot "Apr 21 21:59:03 1984" 'zwei/coms.lisp.82'
hcuot "May 16 05:06:10 1984" 'zwei/coms.lisp.83'
hcuot "Jul  8 12:15:48 1984" 'zwei/coms.lisp.85'
hcuot "Aug  4 21:58:57 1984" 'zwei/coms.qfasl.85'
hcuot "Mar 31 08:25:07 1984" 'zwei/comtab.lisp.307'
hcuot "Jun  6 01:23:51 1984" 'zwei/comtab.lisp.310'
hcuot "Sep  5 17:14:03 1984" 'zwei/comtab.lisp.317'
hcuot "Jan 31 09:07:30 1985" 'zwei/comtab.lisp.322'
hcuot "Sep  7 16:39:57 1984" 'zwei/comtab.qfasl.317'
hcuot "Jan 19 15:30:52 1984" 'zwei/defs.lisp.144'
hcuot "Jun 12 11:53:09 1984" 'zwei/defs.lisp.150'
hcuot "Sep 11 14:44:53 1984" 'zwei/defs.lisp.155'
hcuot "Jan 30 04:52:20 1985" 'zwei/defs.lisp.156'
hcuot "Sep 11 15:19:07 1984" 'zwei/defs.qfasl.155'
hcuot "Feb  9 13:45:22 1984" 'zwei/dired.lisp.299'
hcuot "Jul 26 02:30:44 1984" 'zwei/dired.lisp.303'
hcuot "Dec 14 09:49:04 1984" 'zwei/dired.lisp.307'
hcuot "Aug 29 03:33:34 1984" 'zwei/dired.qfasl.304'
hcuot "Dec 27 02:59:04 1983" 'zwei/displa.lisp.149'
hcuot "Feb  7 07:56:37 1984" 'zwei/displa.lisp.151'
hcuot "Aug  4 20:39:21 1984" 'zwei/displa.lisp.155'
hcuot "Sep  6 12:54:03 1984" 'zwei/displa.lisp.157'
hcuot "Mar 11 22:11:40 1985" 'zwei/displa.lisp.158'
hcuot "Sep  7 16:46:25 1984" 'zwei/displa.qfasl.157'
hcuot "Feb 19 09:26:09 1984" 'zwei/doc.lisp.72'
hcuot "Jun 20 20:53:18 1984" 'zwei/doc.lisp.74'
hcuot "Jan 29 06:00:22 1985" 'zwei/doc.lisp.76'
hcuot "Aug  4 22:09:10 1984" 'zwei/doc.qfasl.74'
hcuot "Jun 29 05:10:53 1982" 'zwei/emacs.comdif.1'
hcuot "Aug 10 04:28:26 1983" 'zwei/fasupd.lisp.29'
hcuot "Apr  7 09:05:03 1984" 'zwei/fasupd.lisp.31'
hcuot "Aug  4 22:10:49 1984" 'zwei/fasupd.qfasl.31'
hcuot "Jan  3 18:13:46 1984" 'zwei/files.lisp.192'
hcuot "Jul  2 05:40:05 1984" 'zwei/files.lisp.195'
hcuot "Mar  1 02:31:14 1985" 'zwei/files.lisp.197'
hcuot "May 17 01:22:10 1985" 'zwei/files.lisp.198'
hcuot "Aug  4 22:11:25 1984" 'zwei/files.qfasl.195'
hcuot "Feb  2 04:15:30 1984" 'zwei/font.lisp.86'
hcuot "May 21 18:58:47 1984" 'zwei/font.lisp.88'
hcuot "Aug  3 18:11:45 1984" 'zwei/font.qfasl.88'
hcuot "Jul 28 15:24:34 1984" 'zwei/for.lisp.61'
hcuot "Aug  4 20:39:16 1984" 'zwei/for.lisp.62'
hcuot "Aug  4 21:53:34 1984" 'zwei/for.qfasl.62'
hcuot "Mar 27 04:35:18 1985" 'zwei/grind.definition.1'
hcuot "Jan  3 18:50:12 1984" 'zwei/history.lisp.15'
hcuot "Sep 11 15:05:42 1984" 'zwei/history.lisp.16'
hcuot "Jan 29 04:47:23 1985" 'zwei/history.lisp.18'
hcuot "Sep 11 15:33:45 1984" 'zwei/history.qfasl.16'
hcuot "Dec 22 04:01:04 1983" 'zwei/host.lisp.20'
hcuot "Aug  4 22:15:39 1984" 'zwei/host.qfasl.20'
hcuot "Oct 25 18:06:09 1983" 'zwei/indent.lisp.103'
hcuot "Oct 11 03:56:06 1984" 'zwei/indent.lisp.105'
hcuot "Feb 15 01:54:51 1985" 'zwei/indent.lisp.107'
hcuot "Aug  3 17:57:17 1984" 'zwei/indent.qfasl.104'
hcuot "Jan 16 15:21:27 1984" 'zwei/info.zwei.1'
hcuot "Jul 20 05:20:46 1983" 'zwei/insert.lisp.32'
hcuot "Apr  7 09:06:12 1984" 'zwei/insert.lisp.33'
hcuot "Nov  4 23:31:43 1984" 'zwei/insert.lisp.35'
hcuot "Aug  3 17:59:24 1984" 'zwei/insert.qfasl.33'
hcuot "Jul  8 12:10:53 1984" 'zwei/ispell.lisp.41'
hcuot "Aug  4 22:16:46 1984" 'zwei/ispell.qfasl.41'
hcuot "Dec 28 15:06:08 1983" 'zwei/kbdmac.lisp.46'
hcuot "Jun  6 01:17:18 1984" 'zwei/kbdmac.lisp.47'
hcuot "Sep  5 18:19:24 1984" 'zwei/kbdmac.lisp.48'
hcuot "Sep  7 17:06:51 1984" 'zwei/kbdmac.qfasl.48'
hcuot "Dec 24 02:43:28 1983" 'zwei/lparse.lisp.31'
hcuot "Aug  4 22:17:46 1984" 'zwei/lparse.qfasl.31'
hcuot "Mar 15 03:24:11 1984" 'zwei/macros.lisp.137'
hcuot "Jun 18 11:18:16 1984" 'zwei/macros.lisp.141'
hcuot "Sep 26 06:38:08 1984" 'zwei/macros.lisp.148'
hcuot "Mar 24 09:33:02 1985" 'zwei/macros.lisp.150'
hcuot "Sep  7 16:33:54 1984" 'zwei/macros.qfasl.147'
hcuot "Jan 29 17:12:53 1984" 'zwei/meth.lisp.45'
hcuot "Mar 18 03:39:19 1984" 'zwei/meth.lisp.47'
hcuot "Jun  6 01:17:09 1984" 'zwei/meth.lisp.48'
hcuot "Jan 30 06:03:17 1985" 'zwei/meth.lisp.49'
hcuot "Aug  3 18:00:57 1984" 'zwei/meth.qfasl.48'
hcuot "Jan  1 20:23:39 1984" 'zwei/modes.lisp.127'
hcuot "May 25 14:16:25 1984" 'zwei/modes.lisp.130'
hcuot "Sep  6 18:25:37 1984" 'zwei/modes.lisp.138'
hcuot "Jan 31 05:32:35 1985" 'zwei/modes.lisp.139'
hcuot "Sep  7 16:36:22 1984" 'zwei/modes.qfasl.138'
hcuot "Feb  3 18:57:01 1984" 'zwei/mouse.lisp.95'
hcuot "May 13 18:52:44 1984" 'zwei/mouse.lisp.96'
hcuot "Oct 13 05:56:35 1984" 'zwei/mouse.lisp.97'
hcuot "Mar  2 08:56:03 1985" 'zwei/mouse.lisp.98'
hcuot "Aug  4 22:22:39 1984" 'zwei/mouse.qfasl.96'
hcuot "Dec 23 00:50:27 1983" 'zwei/nprim.lisp.33'
hcuot "Jul  3 17:23:20 1984" 'zwei/nprim.lisp.34'
hcuot "Aug  3 18:08:03 1984" 'zwei/nprim.qfasl.34'
hcuot "Jan 28 00:16:26 1984" 'zwei/pated.lisp.22'
hcuot "Sep 12 23:07:41 1984" 'zwei/pated.lisp.26'
hcuot "Jan 30 03:35:55 1985" 'zwei/pated.lisp.31'
hcuot "Feb 14 23:39:26 1985" 'zwei/pated.lisp.33'
hcuot "Aug  4 22:24:35 1984" 'zwei/pated.qfasl.25'
hcuot "Dec 16 07:12:34 1982" 'zwei/pl1mod.lisp.13'
hcuot "Apr  7 09:06:39 1984" 'zwei/pl1mod.lisp.14'
hcuot "Aug  4 22:25:49 1984" 'zwei/pl1mod.qfasl.14'
hcuot "Feb 17 03:39:18 1984" 'zwei/poss.lisp.85'
hcuot "Oct 13 06:51:43 1984" 'zwei/poss.lisp.89'
hcuot "Dec  1 14:23:59 1984" 'zwei/poss.lisp.90'
hcuot "Aug  4 22:31:53 1984" 'zwei/poss.qfasl.87'
hcuot "Feb 19 09:24:49 1984" 'zwei/primit.lisp.171'
hcuot "Aug  4 20:39:09 1984" 'zwei/primit.lisp.174'
hcuot "Sep 26 06:38:14 1984" 'zwei/primit.lisp.175'
hcuot "Aug  4 21:55:55 1984" 'zwei/primit.qfasl.174'
hcuot "Jan  3 03:26:46 1984" 'zwei/screen.lisp.455'
hcuot "Jun 29 14:36:00 1984" 'zwei/screen.lisp.461'
hcuot "Oct 10 23:38:06 1984" 'zwei/screen.lisp.467'
hcuot "Mar 11 22:13:17 1985" 'zwei/screen.lisp.468'
hcuot "Sep  7 16:54:21 1984" 'zwei/screen.qfasl.466'
hcuot "Mar 13 01:14:13 1984" 'zwei/search.lisp.82'
hcuot "Jul 29 20:57:19 1984" 'zwei/search.lisp.86'
hcuot "Sep  7 16:30:55 1984" 'zwei/search.qfasl.86'
hcuot "Feb  2 04:15:54 1984" 'zwei/sectio.lisp.261'
hcuot "May 16 05:06:36 1984" 'zwei/sectio.lisp.263'
hcuot "Oct 13 06:50:53 1984" 'zwei/sectio.lisp.267'
hcuot "Dec  4 23:28:17 1984" 'zwei/sectio.lisp.268'
hcuot "Feb 14 04:07:01 1985" 'zwei/sectio.lisp.272'
hcuot "Feb 15 02:02:40 1985" 'zwei/sectio.lisp.273'
hcuot "Aug  4 22:40:42 1984" 'zwei/sectio.qfasl.266'
hcuot "Sep 17 18:28:13 1983" 'zwei/stream.lisp.157'
hcuot "Jul  4 12:36:58 1984" 'zwei/stream.lisp.167'
hcuot "Sep  6 18:35:06 1984" 'zwei/stream.lisp.168'
hcuot "Sep  7 16:50:12 1984" 'zwei/stream.qfasl.168'
hcuot "Jan 19 21:45:33 1983" 'zwei/teach-zmacs.text.2'
hcuot "Mar 17 22:53:21 1984" 'zwei/zmacs.lisp.507'
hcuot "Jul  4 18:51:22 1984" 'zwei/zmacs.lisp.516'
hcuot "Dec  4 18:39:36 1984" 'zwei/zmacs.lisp.519'
hcuot "Jan 30 06:00:33 1985" 'zwei/zmacs.lisp.520'
hcuot "Feb 26 21:21:17 1985" 'zwei/zmacs.lisp.521'
hcuot "Apr 13 04:52:55 1985" 'zwei/zmacs.lisp.522'
hcuot "Sep  7 17:01:03 1984" 'zwei/zmacs.qfasl.518'
hcuot "Mar 17 22:52:47 1984" 'zwei/zmnew.lisp.27'
hcuot "Jul  8 14:17:30 1984" 'zwei/zmnew.lisp.33'
hcuot "Jan 31 18:19:56 1985" 'zwei/zmnew.lisp.36'
hcuot "Sep 10 01:42:59 1984" 'zwei/zmnew.qfasl.35'
hcuot "Jun 19 19:31:52 1983" 'zwei/zymurg.lisp.41'
hcuot "Apr  7 09:07:07 1984" 'zwei/zymurg.lisp.42'
hcuot "Sep  7 11:25:02 1984" 'zwei/zymurg.qfasl.42'

# tid/9003839

hcuot "Sep  3 16:58:12 1984" 'cc/cadld.lisp.8'
hcuot "Sep  8 23:13:51 1984" 'cc/cadld.qfasl.8'
hcuot "Sep 12 00:20:06 1984" 'cc/cadreg.lisp.4'
hcuot "Jul 23 22:17:33 1983" 'cc/cc.help.4'
hcuot "Sep 11 18:22:33 1984" 'cc/cc.lisp.50'
hcuot "Sep 12 00:20:40 1984" 'cc/cc.qfasl.50'
hcuot "Oct  9 03:57:54 1983" 'cc/ccdisk.lisp.106'
hcuot "Sep  8 23:19:18 1984" 'cc/ccdisk.qfasl.106'
hcuot "Dec 27 00:00:38 1983" 'cc/ccgsyl.lisp.6'
hcuot "Sep  8 22:58:38 1984" 'cc/ccgsyl.qfasl.6'
hcuot "Aug 16 21:47:14 1983" 'cc/ccwhy.lisp.12'
hcuot "Sep  8 23:17:29 1984" 'cc/ccwhy.qfasl.12'
hcuot "Apr  7 08:02:25 1984" 'cc/chploc.lisp.5'
hcuot "Sep  8 23:23:30 1984" 'cc/chploc.qfasl.5'
hcuot "Mar  9 14:11:02 1984" 'cc/dcfu.uload.2'
hcuot "Sep  8 23:28:27 1984" 'cc/dcheck.lisp.7'
hcuot "Jul  2 15:56:55 1982" 'cc/dcheck.loop.1'
hcuot "Sep  8 23:36:25 1984" 'cc/dcheck.qfasl.7'
hcuot "Sep  8 23:03:26 1984" 'cc/diags.lisp.159'
hcuot "Sep  8 23:04:08 1984" 'cc/diags.qfasl.159'
hcuot "Jan  3 14:00:50 1985" 'cc/dmon.lisp.57'
hcuot "Sep  8 23:10:55 1984" 'cc/dmon.qfasl.56'
hcuot "Dec  7 16:33:10 1984" 'cc/junk..1'
hcuot "Sep  8 21:31:10 1984" 'cc/lcadmc.lisp.31'
hcuot "Sep  8 22:49:35 1984" 'cc/lcadmc.qfasl.31'
hcuot "Apr  7 08:04:15 1984" 'cc/lcadrd.lisp.95'
hcuot "Sep  8 22:58:54 1984" 'cc/lcadrd.qfasl.95'
hcuot "Jun 19 19:44:48 1983" 'cc/ldbg.lisp.45'
hcuot "Sep  8 23:12:40 1984" 'cc/ldbg.qfasl.45'
hcuot "Nov 12 02:26:07 1983" 'cc/lqfmac.lisp.17'
hcuot "Sep  8 22:48:57 1984" 'cc/lqfmac.qfasl.17'
hcuot "Oct 25 23:06:09 1983" 'cc/patch.directory.3'
hcuot "Sep  7 20:21:16 1984" 'cc/patch-3.directory.23'
hcuot "Dec  1 08:12:53 1983" 'cc/patch-3-1.qfasl.1'
hcuot "Sep  7 20:20:42 1984" 'cc/patch-3-10.lisp.1'
hcuot "Sep  7 20:20:45 1984" 'cc/patch-3-10.qfasl.1'
hcuot "Dec 17 18:50:34 1983" 'cc/patch-3-2.qfasl.2'
hcuot "Dec 26 23:59:31 1983" 'cc/patch-3-3.qfasl.1'
hcuot "Dec 27 13:59:00 1983" 'cc/patch-3-4.qfasl.1'
hcuot "Jan 23 01:16:31 1984" 'cc/patch-3-5.qfasl.2'
hcuot "Jan 27 03:11:57 1984" 'cc/patch-3-6.qfasl.1'
hcuot "Jun 11 17:06:48 1984" 'cc/patch-3-7.qfasl.1'
hcuot "Jul  6 19:29:34 1984" 'cc/patch-3-8.qfasl.1'
hcuot "Sep  6 14:28:13 1984" 'cc/patch-3-9.lisp.1'
hcuot "Sep  6 14:28:20 1984" 'cc/patch-3-9.qfasl.1'
hcuot "Sep  8 23:29:20 1984" 'cc/qf.lisp.126'
hcuot "Sep  8 23:33:23 1984" 'cc/qf.qfasl.126'
hcuot "Jul  6 19:39:01 1984" 'cc/salvag.lisp.38'
hcuot "Sep  8 23:23:45 1984" 'cc/salvag.qfasl.38'
hcuot "Apr  7 08:05:00 1984" 'cc/zero.lisp.15'
hcuot "Sep  8 23:13:29 1984" 'cc/zero.qfasl.15'
hcuot "May 27 17:12:18 1986" 'chaos/hosts.text.392'
hcuot "Oct  9 08:51:43 1984" 'cold/coldld.lisp.84'
hcuot "Sep 11 01:47:22 1984" 'cold/coldld.qfasl.83'
hcuot "Sep 11 01:43:22 1984" 'cold/coldpk.lisp.25'
hcuot "Sep 11 01:43:37 1984" 'cold/coldpk.qfasl.25'
hcuot "Aug 30 00:45:32 1984" 'cold/coldut.lisp.100'
hcuot "Aug 30 03:00:56 1984" 'cold/coldut.qfasl.100'
hcuot "Sep  5 14:29:29 1984" 'cold/defmic.lisp.200'
hcuot "Feb 12 19:46:43 1985" 'cold/docmic.lisp.41'
hcuot "Mar 11 18:16:37 1985" 'cold/export.lisp.31'
hcuot "Feb 28 10:10:43 1985" 'cold/global.lisp.644'
hcuot "Aug 14 22:46:01 1984" 'cold/global.qfasl.634'
hcuot "Feb 28 09:51:49 1985" 'cold/lisp.lisp.2'
hcuot "Feb 14 23:33:28 1985" 'cold/mini.lisp.90'
hcuot "Aug 14 23:19:46 1984" 'cold/mini.qfasl.88'
hcuot "Nov 10 21:18:52 1984" 'cold/minisr.exe.1'
hcuot "Nov 10 21:18:01 1984" 'cold/minisr.mid.44'
hcuot "Feb 12 03:35:01 1985" 'cold/qcom.lisp.583'
hcuot "Dec  6 03:29:11 1984" 'cold/qdefs.lisp.388'
hcuot "Feb 17 03:27:51 1985" 'cold/system.lisp.106'
hcuot "Aug 15 00:23:06 1984" 'cold/system.qfasl.102'
hcuot "Apr  7 08:06:03 1984" 'demo/abacus.lisp.20'
hcuot "Sep  7 19:13:00 1984" 'demo/abacus.qfasl.20'
hcuot "Dec 12 23:41:22 1983" 'demo/alarm.lisp.50'
hcuot "Jul  4 15:19:15 1985" 'demo/alarm.qfasl.50'
hcuot "Aug 16 04:56:31 1983" 'demo/beeps.lisp.8'
hcuot "Oct 26 18:25:37 1983" 'demo/beeps.qfasl.8'
hcuot "Apr  7 08:06:23 1984" 'demo/cafe.lisp.8'
hcuot "Jun  6 07:00:32 1984" 'demo/cafe.qfasl.8'
hcuot "Nov 12 18:18:36 1983" 'demo/colorhack.lisp.7'
hcuot "Nov 13 01:19:50 1983" 'demo/colorhack.qfasl.7'
hcuot "Jun 19 20:52:15 1983" 'demo/colxor.lisp.52'
hcuot "Oct 26 18:29:52 1983" 'demo/colxor.qfasl.52'
hcuot "Jul 21 11:00:24 1982" 'demo/craze.lisp.2'
hcuot "Aug 14 04:34:04 1983" 'demo/craze.qfasl.2'
hcuot "Dec  8 22:29:51 1983" 'demo/crock.lisp.6'
hcuot "Jul  6 03:06:15 1985" 'demo/crock.qfasl.6'
hcuot "Apr  7 09:38:29 1984" 'demo/ctest.lisp.1'
hcuot "Mar 31 11:14:03 1984" 'demo/dc.lisp.4'
hcuot "Sep  7 19:10:53 1984" 'demo/dc.qfasl.4'
hcuot "Apr  7 08:07:09 1984" 'demo/deutsc.lisp.34'
hcuot "Jun  6 07:04:44 1984" 'demo/deutsc.qfasl.34'
hcuot "Apr  7 08:07:35 1984" 'demo/dlwhak.lisp.37'
hcuot "Jun  6 07:05:34 1984" 'demo/dlwhak.qfasl.37'
hcuot "Apr  7 08:08:37 1984" 'demo/docscr.lisp.6'
hcuot "Jun  6 07:07:54 1984" 'demo/docscr.qfasl.6'
hcuot "Sep  5 13:58:44 1984" 'demo/doctor.lisp.10'
hcuot "Sep  7 17:25:44 1984" 'demo/doctor.qfasl.10'
hcuot "Jun 19 20:52:55 1983" 'demo/geb.lisp.27'
hcuot "Oct 26 18:38:48 1983" 'demo/geb.qfasl.27'
hcuot "Jun 19 20:53:09 1983" 'demo/hakdef.lisp.14'
hcuot "Sep  7 19:14:00 1984" 'demo/hakdef.qfasl.14'
hcuot "Apr  7 08:09:41 1984" 'demo/hcedit.lisp.28'
hcuot "Jun  6 07:08:50 1984" 'demo/hcedit.qfasl.28'
hcuot "Apr  7 08:11:41 1984" 'demo/liss.lisp.5'
hcuot "Nov 10 06:46:06 1983" 'demo/munch.lisp.14'
hcuot "Sep  7 19:09:22 1984" 'demo/munch.qfasl.14'
hcuot "Apr  7 08:24:04 1984" 'demo/npaint.lisp.1'
hcuot "Aug 16 04:56:24 1983" 'demo/ohacks.lisp.35'
hcuot "Oct 26 18:44:57 1983" 'demo/ohacks.qfasl.35'
hcuot "Apr  7 08:12:09 1984" 'demo/organ.lisp.18'
hcuot "Jun  6 07:09:56 1984" 'demo/organ.qfasl.18'
hcuot "Jun 19 20:52:00 1983" 'demo/pfom.lisp.31'
hcuot "Aug 14 04:44:10 1983" 'demo/pfom.qfasl.31'
hcuot "Oct 24 15:42:24 1983" 'demo/qix.lisp.3'
hcuot "Oct 26 18:47:45 1983" 'demo/qix.qfasl.3'
hcuot "Jul 26 04:08:11 1983" 'demo/rotate.lisp.5'
hcuot "Oct 26 18:48:20 1983" 'demo/rotate.qfasl.5'
hcuot "Aug 20 15:03:08 1983" 'demo/rotcir.lisp.5'
hcuot "Oct 26 18:49:12 1983" 'demo/rotcir.qfasl.5'
hcuot "Dec 27 08:24:52 1983" 'demo/treedv.lisp.4'
hcuot "Dec 27 08:25:03 1983" 'demo/treedv.qfasl.4'
hcuot "Jul 20 08:04:52 1982" 'demo/tvbgar.qfasl.1'
hcuot "Apr  7 08:39:50 1984" 'demo/versat.lisp.1'
hcuot "Apr  7 08:41:20 1984" 'demo/votrax.lisp.1'
hcuot "Oct 21 21:49:11 1983" 'demo/what.lisp.19'
hcuot "Oct 26 18:54:43 1983" 'demo/what.qfasl.19'
hcuot "Apr  7 08:42:46 1984" 'demo/words.lisp.1'
hcuot "Sep  6 17:42:51 1984" 'demo/worm.lisp.9'
hcuot "Sep 10 01:44:09 1984" 'demo/worm.qfasl.9'
hcuot "Dec 12 23:41:19 1983" 'demo/worm-trails.lisp.15'
hcuot "Sep  7 19:08:09 1984" 'demo/worm-trails.qfasl.15'
hcuot "Jul 20 08:05:17 1982" 'demo/wormch.ast.1'
hcuot "Jul 20 08:05:20 1982" 'demo/wormch.qfasl.1'
hcuot "Jun 15 04:28:10 1984" 'distribution/dist.lisp.8'
hcuot "Feb 20 18:09:00 1984" 'distribution/dist.qfasl.7'
hcuot "Feb 16 07:57:28 1984" 'distribution/lmi-filter.lisp.2'
hcuot "Feb  7 23:09:02 1985" 'eh/eh.lisp.340'
hcuot "Sep  8 23:53:06 1984" 'eh/eh.qfasl.336'
hcuot "Feb 17 04:09:48 1985" 'eh/ehc.lisp.236'
hcuot "Sep  7 16:04:40 1984" 'eh/ehc.qfasl.233'
hcuot "Feb  7 07:02:56 1985" 'eh/ehf.lisp.228'
hcuot "Dec  8 12:28:16 1986" 'eh/ehf.lisp.229'
hcuot "Sep 11 15:22:50 1984" 'eh/ehf.qfasl.225'
hcuot "Nov  9 05:13:07 1984" 'eh/ehsys.lisp.1'
hcuot "May 16 06:21:59 1984" 'eh/ehw.lisp.109'
hcuot "Sep  7 19:04:14 1984" 'eh/ehw.qfasl.109'
hcuot "Feb 13 07:01:19 1985" 'eh/errmac.lisp.2'
hcuot "Aug 27 19:12:14 1985" 'eh/she.lisp.1'
hcuot "Dec  5 00:43:25 1984" 'file/bugs.mail.1'
hcuot "Jul 22 09:24:10 1982" 'file/clear.lisp.1'
hcuot "Sep 14 02:15:08 1984" 'file/copy.lisp.131'
hcuot "Jan  3 03:07:03 1984" 'file/copy.qfasl.128'
hcuot "May 14 16:28:15 1986" 'file/fs.directory.14'
hcuot "Jul 22 09:28:50 1982" 'file/fs.improv.1'
hcuot "Sep 11 00:45:16 1984" 'file/fs.lisp.77'
hcuot "Sep 11 00:48:35 1984" 'file/fs.qfasl.77'
hcuot "Nov 21 16:05:22 1984" 'file/fs-48.directory.16'
hcuot "Jan  4 18:02:49 1984" 'file/fs-48-1.lisp.1'
hcuot "Jan  4 18:03:05 1984" 'file/fs-48-1.qfasl.1'
hcuot "Jan 18 11:50:40 1984" 'file/fs-48-2.lisp.4'
hcuot "Jan 27 01:15:20 1984" 'file/fs-48-3.lisp.2'
hcuot "Jan 27 01:15:27 1984" 'file/fs-48-3.qfasl.2'
hcuot "May 16 04:04:05 1984" 'file/fs-48-4.lisp.2'
hcuot "May 16 04:04:09 1984" 'file/fs-48-4.qfasl.2'
hcuot "Jun 10 08:19:39 1984" 'file/fs-48-5.lisp.1'
hcuot "Jun 10 08:19:43 1984" 'file/fs-48-5.qfasl.1'
hcuot "Nov 21 16:04:24 1984" 'file/fs-48-6.lisp.1'
hcuot "Nov 21 16:04:35 1984" 'file/fs-48-6.qfasl.1'
hcuot "Jul 16 11:33:45 1984" 'file/fs-49.directory.3'
hcuot "Jul 16 11:33:11 1984" 'file/fs-49-1.lisp.1'
hcuot "Jul 16 11:33:18 1984" 'file/fs-49-1.qfasl.1'
hcuot "Sep 11 19:49:42 1984" 'file/fs-50.directory.1'
hcuot "Sep 14 20:45:55 1984" 'file/fs-51.directory.1'
hcuot "Apr 11 20:33:51 1985" 'file/fs-52.directory.1'
hcuot "May 14 16:28:18 1986" 'file/fs-53.directory.1'
hcuot "Apr 13 05:21:42 1985" 'file/fsacc.lisp.6'
hcuot "May 14 16:26:59 1986" 'file/fsacc.qfasl.6'
hcuot "Sep 11 23:09:25 1984" 'file/fsdefs.lisp.177'
hcuot "May 14 16:10:20 1986" 'file/fsdefs.qfasl.177'
hcuot "Nov  6 09:13:45 1982" 'file/fsdoc.text.6'
hcuot "Sep 11 23:09:30 1984" 'file/fsguts.lisp.371'
hcuot "May 14 16:17:16 1986" 'file/fsguts.qfasl.371'
hcuot "Sep 11 00:21:11 1984" 'file/fsname.lisp.106'
hcuot "Jul  5 05:03:05 1984" 'file/fsname.qfasl.104'
hcuot "Sep 11 23:09:42 1984" 'file/fsstr.lisp.107'
hcuot "May 14 16:14:57 1986" 'file/fsstr.qfasl.107'
hcuot "Sep 13 23:24:39 1984" 'file/hogs.lisp.6'
hcuot "Sep 13 22:59:04 1984" 'file/hogs.qfasl.4'
hcuot "Dec 13 20:31:32 1984" 'file/lmpars.lisp.113'
hcuot "Sep 10 14:39:55 1984" 'file/lmpars.qfasl.110'
hcuot "Apr  7 08:14:57 1984" 'file/login.lisp.26'
hcuot "Jul 22 09:35:33 1982" 'file/login.qfasl.25'
hcuot "Jan  3 00:30:18 1984" 'file/magtape.directory.9'
hcuot "Oct 26 15:41:54 1983" 'file/magtape-14.directory.14'
hcuot "Mar  8 00:56:29 1983" 'file/magtape-14-1.lisp.1'
hcuot "Mar  8 00:56:47 1983" 'file/magtape-14-1.qfasl.1'
hcuot "Mar 29 01:21:02 1983" 'file/magtape-14-2.lisp.1'
hcuot "Apr 25 03:51:40 1983" 'file/magtape-14-3.lisp.1'
hcuot "Apr 25 03:51:48 1983" 'file/magtape-14-3.qfasl.1'
hcuot "May 18 22:11:18 1983" 'file/magtape-14-4.lisp.3'
hcuot "May 18 22:11:35 1983" 'file/magtape-14-4.qfasl.3'
hcuot "Oct 26 15:41:05 1983" 'file/magtape-14-5.lisp.1'
hcuot "Oct 26 15:41:17 1983" 'file/magtape-14-5.qfasl.1'
hcuot "Jan  3 02:48:40 1984" 'file/mtaux.lisp.77'
hcuot "Dec 16 09:34:10 1983" 'file/mtdefs.lisp.30'
hcuot "Jan  3 02:49:38 1984" 'file/mtstr.lisp.85'
hcuot "Jan  3 02:50:55 1984" 'file/odump.lisp.1'
hcuot "May 14 18:29:35 1986" 'file/server.directory.7'
hcuot "Mar 27 17:45:30 1985" 'file/server.lisp.154'
hcuot "May 14 18:22:55 1986" 'file/server.qfasl.154'
hcuot "May 14 18:29:39 1986" 'file/server-10.directory.1'
hcuot "May 26 06:42:23 1984" 'file/server-8.directory.14'
hcuot "Jan  4 00:49:04 1984" 'file/server-8-1.lisp.1'
hcuot "Jan  4 00:49:14 1984" 'file/server-8-1.qfasl.1'
hcuot "Jan  4 18:06:06 1984" 'file/server-8-2.lisp.1'
hcuot "Jan  4 18:06:20 1984" 'file/server-8-2.qfasl.1'
hcuot "May 26 06:36:29 1984" 'file/server-8-3.lisp.4'
hcuot "May 26 06:36:34 1984" 'file/server-8-3.qfasl.4'
hcuot "Feb 16 09:28:03 1984" 'file/server-8-4.lisp.1'
hcuot "Feb 16 09:28:08 1984" 'file/server-8-4.qfasl.1'
hcuot "May 26 06:39:28 1984" 'file/server-8-5.lisp.2'
hcuot "May 26 06:39:34 1984" 'file/server-8-5.qfasl.2'
hcuot "Sep 14 20:35:57 1984" 'file/server-9.directory.1'
hcuot "Jul 13 01:23:33 1984" 'file/zmail.lisp.5'
hcuot "Sep  9 14:30:21 1984" 'file/zmail.qfasl.5'
hcuot "Jan  3 19:55:28 1984" 'file2/anydir.lisp.201'
hcuot "Aug  3 01:41:49 1984" 'file2/anydir.qfasl.201'
hcuot "Jan 18 11:35:23 1984" 'file2/area.lisp.22'
hcuot "Aug  3 01:33:44 1984" 'file2/area.qfasl.22'
hcuot "Jul 22 09:59:30 1982" 'file2/bfsplm.text.4'
hcuot "Dec 25 03:02:12 1985" 'file2/bug-lmfile-mail.txt.1'
hcuot "Jul 22 09:59:57 1982" 'file2/chgnod.lisp.5'
hcuot "Aug  3 01:40:05 1984" 'file2/chgnod.qfasl.5'
hcuot "Jan 18 11:46:24 1984" 'file2/complt.lisp.17'
hcuot "Aug  3 01:50:25 1984" 'file2/complt.qfasl.17'
hcuot "Aug  3 01:19:33 1984" 'file2/defs.lisp.190'
hcuot "Nov 21 13:17:17 1984" 'file2/defs.qfasl.190'
hcuot "Jan 29 01:16:03 1984" 'file2/diread.lisp.61'
hcuot "Aug  3 01:48:32 1984" 'file2/diread.qfasl.61'
hcuot "Jan 18 11:50:00 1984" 'file2/dump.lisp.29'
hcuot "Aug  3 01:56:23 1984" 'file2/dump.qfasl.29'
hcuot "Jan 18 11:34:11 1984" 'file2/files.lisp.122'
hcuot "Aug  3 01:30:22 1984" 'file2/files.qfasl.122'
hcuot "Dec 18 04:50:47 1983" 'file2/free.lisp.48'
hcuot "Aug  3 01:26:33 1984" 'file2/free.qfasl.48'
hcuot "Feb  9 12:19:25 1986" 'file2/fs-fc-mail.txt.1'
hcuot "Jan 18 11:30:16 1984" 'file2/gc.lisp.19'
hcuot "Aug  3 01:32:53 1984" 'file2/gc.qfasl.19'
hcuot "Jan 18 11:29:20 1984" 'file2/io.lisp.94'
hcuot "Aug  3 01:28:01 1984" 'file2/io.qfasl.94'
hcuot "Jan 31 21:07:28 1984" 'file2/link.lisp.47'
hcuot "Aug  3 01:37:26 1984" 'file2/link.qfasl.47'
hcuot "Aug  3 02:02:34 1984" 'file2/lmfile.directory.4'
hcuot "Sep 30 02:33:31 1983" 'file2/lmfile-2.directory.10'
hcuot "Jun 16 17:34:55 1983" 'file2/lmfile-2-1.lisp.1'
hcuot "Jul  6 16:59:00 1983" 'file2/lmfile-2-2.lisp.1'
hcuot "Jul  7 01:33:09 1983" 'file2/lmfile-2-3.lisp.2'
hcuot "Sep 30 02:32:51 1983" 'file2/lmfile-2-4.lisp.1'
hcuot "Jan 29 02:54:05 1984" 'file2/lmfile-3.directory.7'
hcuot "Jan  3 19:56:37 1984" 'file2/lmfile-3-1.lisp.1'
hcuot "Jan  3 19:56:48 1984" 'file2/lmfile-3-1.qfasl.1'
hcuot "Jan 29 02:53:39 1984" 'file2/lmfile-3-3.lisp.1'
hcuot "Jan 29 02:53:44 1984" 'file2/lmfile-3-3.qfasl.1'
hcuot "Aug  3 02:02:37 1984" 'file2/lmfile-4.directory.1'
hcuot "Jul 22 10:07:13 1982" 'file2/maint.text.9'
hcuot "Dec  2 09:30:00 1984" 'file2/maiser.lisp.13'
hcuot "Aug  3 02:01:34 1984" 'file2/maiser.qfasl.9'
hcuot "Jan 18 11:37:55 1984" 'file2/node.lisp.162'
hcuot "Aug  3 01:34:26 1984" 'file2/node.qfasl.162'
hcuot "Nov 21 12:30:20 1984" 'file2/pack.lisp.83'
hcuot "Nov 21 13:37:41 1984" 'file2/pack.qfasl.83'
hcuot "Nov 20 23:43:10 1984" 'file2/pathnm.lisp.163'
hcuot "Sep 11 00:27:31 1984" 'file2/pathnm.qfasl.162'
hcuot "Dec 18 16:59:00 1983" 'file2/pdp10.lisp.20'
hcuot "Aug  3 01:38:34 1984" 'file2/pdp10.qfasl.20'
hcuot "Nov 23 03:57:38 1982" 'file2/remote.directory.10'
hcuot "Dec 12 01:15:02 1982" 'file2/remote.lisp.30'
hcuot "Dec 29 23:30:12 1982" 'file2/remote-23.directory.4'
hcuot "Jan 29 02:53:17 1984" 'file2/repair.lisp.1'
hcuot "Dec 18 04:20:56 1983" 'file2/rmdefs.lisp.10'
hcuot "Aug  3 03:02:57 1984" 'file2/rmdefs.qfasl.10'
hcuot "Jan 18 11:42:38 1984" 'file2/salvag.lisp.22'
hcuot "Aug  3 01:40:37 1984" 'file2/salvag.qfasl.22'
hcuot "Nov 21 15:59:39 1984" 'file2/server.lisp.52'
hcuot "Nov 21 13:41:01 1984" 'file2/server.qfasl.50'
hcuot "Jan 18 11:45:18 1984" 'file2/spcdir.lisp.92'
hcuot "Aug  3 01:45:33 1984" 'file2/spcdir.qfasl.92'
hcuot "Aug  3 02:54:13 1984" 'file2/stream.lisp.210'
hcuot "Aug  3 02:57:08 1984" 'file2/stream.qfasl.210'
hcuot "Apr  7 08:16:07 1984" 'file2/system.lisp.32'
hcuot "May 22 21:23:41 1984" 'file2/system.qfasl.32'
hcuot "Jul 22 10:18:48 1982" 'file2/view.text.9'
hcuot "Jan 18 11:22:34 1984" 'file2/xrmdefs.lisp.11'
hcuot "Jan 18 11:05:07 1984" 'file2/xserver.lisp.44'
hcuot "Jun 17 23:38:36 1984" 'fonts/13fgb.qfasl.6'
hcuot "Jun 17 23:38:32 1984" 'fonts/16fg.qfasl.5'
hcuot "Jun 17 23:38:21 1984" 'fonts/18fg.qfasl.5'
hcuot "Jun 17 23:38:12 1984" 'fonts/20vr.qfasl.5'
hcuot "Jun 17 23:38:08 1984" 'fonts/25fr3.qfasl.5'
hcuot "Jun 17 23:38:03 1984" 'fonts/31vr.qfasl.5'
hcuot "Jun 17 23:37:56 1984" 'fonts/40vr.qfasl.5'
hcuot "Jun 17 23:37:52 1984" 'fonts/40vshd.qfasl.5'
hcuot "Jun 17 23:37:43 1984" 'fonts/43vxms.qfasl.10'
hcuot "Jun 17 23:37:40 1984" 'fonts/5x5.qfasl.10'
hcuot "Jun 17 23:37:37 1984" 'fonts/abacus.qfasl.5'
hcuot "Jun 17 23:37:34 1984" 'fonts/apl14.qfasl.4'
hcuot "Jun 17 23:37:30 1984" 'fonts/arr10.qfasl.5'
hcuot "Sep 10 14:20:10 1984" 'fonts/bigfnt.qfasl.10'
hcuot "Jul 20 08:17:55 1982" 'fonts/bigold.qfasl.1'
hcuot "Jun 17 23:37:22 1984" 'fonts/bigvg.qfasl.4'
hcuot "Jun 17 23:37:20 1984" 'fonts/color-5x5.qfasl.4'
hcuot "Jun 17 23:37:16 1984" 'fonts/color-cptfont.qfasl.3'
hcuot "Jun 17 23:37:12 1984" 'fonts/color-medfnt.qfasl.4'
hcuot "Jun 17 23:37:07 1984" 'fonts/color-mouse.qfasl.4'
hcuot "Jun 17 23:37:04 1984" 'fonts/courier.qfasl.5'
hcuot "Jul 20 08:18:26 1982" 'fonts/cptfon.qfasl.3'
hcuot "Jun 17 23:37:01 1984" 'fonts/cptfont.qfasl.19'
hcuot "Jun 17 23:36:58 1984" 'fonts/cptfontb.qfasl.7'
hcuot "Jun 17 23:36:54 1984" 'fonts/cyr.qfasl.5'
hcuot "Jun 17 23:36:50 1984" 'fonts/cyr12.qfasl.5'
hcuot "Jun 17 23:36:46 1984" 'fonts/ent.qfasl.5'
hcuot "Dec 10 17:35:35 1983" 'fonts/equivalence.lisp.1'
hcuot "Jun 17 23:36:42 1984" 'fonts/gach10.qfasl.3'
hcuot "Jun 17 23:36:35 1984" 'fonts/gach10b.qfasl.3'
hcuot "Jun 17 23:36:27 1984" 'fonts/gach12.qfasl.3'
hcuot "Jul 20 08:18:41 1982" 'fonts/gfr.archiv.1'
hcuot "Jun 17 23:36:20 1984" 'fonts/hippo10.qfasl.4'
hcuot "Jun 17 23:36:14 1984" 'fonts/hippo18.qfasl.3'
hcuot "Jun 17 23:36:11 1984" 'fonts/hl10.qfasl.9'
hcuot "Jun 17 23:36:07 1984" 'fonts/hl10b.qfasl.9'
hcuot "Jun 17 23:36:03 1984" 'fonts/hl12.qfasl.10'
hcuot "Jun 17 23:35:59 1984" 'fonts/hl12b.qfasl.15'
hcuot "Jun 17 23:35:56 1984" 'fonts/hl12b1.qfasl.3'
hcuot "Jun 17 23:35:52 1984" 'fonts/hl12bi.qfasl.10'
hcuot "Jun 17 23:35:49 1984" 'fonts/hl12i.qfasl.11'
hcuot "Jun 17 23:35:45 1984" 'fonts/hl18.qfasl.6'
hcuot "Jun 17 23:35:42 1984" 'fonts/hl6.qfasl.9'
hcuot "Jun 17 23:35:39 1984" 'fonts/hl7.qfasl.9'
hcuot "Jun 17 23:35:34 1984" 'fonts/icons.qfasl.3'
hcuot "Jun 17 23:35:30 1984" 'fonts/invisible.qfasl.3'
hcuot "Jun 17 23:35:27 1984" 'fonts/medfnb.qfasl.8'
hcuot "Jun 17 23:35:17 1984" 'fonts/medfnt.qfasl.9'
hcuot "Jun 17 23:35:13 1984" 'fonts/mets.qfasl.9'
hcuot "Jun 17 23:35:07 1984" 'fonts/metsi.qfasl.9'
hcuot "Jun 17 23:35:00 1984" 'fonts/mit.qfasl.5'
hcuot "Jun 17 23:34:56 1984" 'fonts/mouse.qfasl.9'
hcuot "Jun 17 23:34:53 1984" 'fonts/narrow.qfasl.5'
hcuot "Jun 17 23:34:50 1984" 'fonts/panes.qfasl.3'
hcuot "Nov 15 06:23:32 1983" 'fonts/prt12b.qfasl.2'
hcuot "Jun 17 23:34:45 1984" 'fonts/s30chs.qfasl.5'
hcuot "Jun 17 23:34:41 1984" 'fonts/s35ger.qfasl.3'
hcuot "Jun 17 23:34:35 1984" 'fonts/sail12.qfasl.6'
hcuot "Jun 17 23:34:32 1984" 'fonts/search.qfasl.9'
hcuot "Jun 17 23:34:28 1984" 'fonts/ship.qfasl.6'
hcuot "Oct  9 06:53:23 1984" 'fonts/storybook.qfasl.1'
hcuot "Oct  9 06:54:06 1984" 'fonts/storybookbold.qfasl.1'
hcuot "Jun 17 23:34:24 1984" 'fonts/tally.qfasl.5'
hcuot "Jul 20 08:20:57 1982" 'fonts/times.9rom.1'
hcuot "Jun 17 23:34:20 1984" 'fonts/tiny.qfasl.5'
hcuot "Jun 17 23:34:15 1984" 'fonts/tog.qfasl.5'
hcuot "Jun 17 23:34:12 1984" 'fonts/tr10.qfasl.9'
hcuot "Jun 17 23:34:09 1984" 'fonts/tr10b.qfasl.8'
hcuot "Jun 17 23:34:04 1984" 'fonts/tr10bi.qfasl.7'
hcuot "Jun 17 23:34:00 1984" 'fonts/tr10i.qfasl.7'
hcuot "Jun 17 23:33:57 1984" 'fonts/tr10ic.qfasl.4'
hcuot "Jun 17 23:33:53 1984" 'fonts/tr12.qfasl.11'
hcuot "Jun 17 23:33:49 1984" 'fonts/tr12b.qfasl.17'
hcuot "Jun 17 23:33:45 1984" 'fonts/tr12b1.qfasl.8'
hcuot "Jun 17 23:33:42 1984" 'fonts/tr12bi.qfasl.9'
hcuot "Jun 17 23:33:37 1984" 'fonts/tr12i.qfasl.13'
hcuot "Jun 17 23:33:30 1984" 'fonts/tr18.qfasl.7'
hcuot "Jun 17 23:33:16 1984" 'fonts/tr18b.qfasl.3'
hcuot "Jun 17 23:33:12 1984" 'fonts/tr8.qfasl.8'
hcuot "Jun 17 23:33:07 1984" 'fonts/tr8b.qfasl.8'
hcuot "Jun 17 23:33:04 1984" 'fonts/tr8i.qfasl.6'
hcuot "Jun 17 23:33:00 1984" 'fonts/tvbug.qfasl.5'
hcuot "Jun 17 23:32:57 1984" 'fonts/tvfont.qfasl.7'
hcuot "Jun 17 23:32:50 1984" 'fonts/worm.qfasl.4'
hcuot "Sep 15 02:28:36 1984" 'io/crdtbl.lisp.35'
hcuot "Aug 31 11:23:39 1984" 'io/crdtbl.qfasl.1'
hcuot "Nov 27 18:27:38 1984" 'io/disk.lisp.292'
hcuot "Nov 21 13:22:39 1984" 'io/disk.qfasl.291'
hcuot "May 20 07:21:35 1984" 'io/dledit.lisp.52'
hcuot "Nov 21 13:34:29 1984" 'io/dledit.qfasl.52'
hcuot "Apr  5 01:37:55 1985" 'io/dribbl.lisp.37'
hcuot "Aug 14 22:10:20 1984" 'io/dribbl.qfasl.36'
hcuot "Aug 30 15:58:54 1984" 'io/find-plausible-partitions.lisp.1'
hcuot "Feb  7 23:25:40 1985" 'io/format.lisp.241'
hcuot "Sep  7 16:25:02 1984" 'io/format.qfasl.234'
hcuot "Jan 22 09:20:39 1983" 'io/format-macro.lisp.2'
hcuot "Aug  3 00:29:26 1984" 'io/format-macro.qfasl.2'
hcuot "Dec 10 06:33:13 1984" 'io/fread.lisp.30'
hcuot "Oct  4 13:25:58 1985" 'io/fread.qfasl.30'
hcuot "Aug 10 02:29:02 1984" 'io/grind.lisp.146'
hcuot "Aug 29 17:06:51 1984" 'io/grind.qfasl.145'
hcuot "Feb 26 20:58:01 1985" 'io/print.lisp.183'
hcuot "Sep 10 01:18:21 1984" 'io/print.qfasl.178'
hcuot "Dec  9 03:44:53 1984" 'io/qio.lisp.217'
hcuot "Aug 30 23:38:38 1984" 'io/qio.qfasl.214'
hcuot "Jun 20 02:21:38 1986" 'io/random-walk.lisp.1'
hcuot "Apr  7 08:18:03 1984" 'io/rcomp.lisp.10'
hcuot "Nov  1 05:32:34 1984" 'io/rddefs.lisp.62'
hcuot "Sep  7 16:28:41 1984" 'io/rddefs.qfasl.61'
hcuot "Sep 15 02:33:22 1984" 'io/rdtbl.lisp.169'
hcuot "Sep 15 02:32:16 1984" 'io/rdtbl.qfasl.167'
hcuot "Nov 20 13:22:01 1984" 'io/read.lisp.437'
hcuot "Aug 15 00:03:40 1984" 'io/read.qfasl.432'
hcuot "Dec  6 04:14:19 1984" 'io/rtc.lisp.47'
hcuot "Sep  9 23:53:33 1984" 'io/rtc.qfasl.46'
hcuot "Jan 26 23:19:23 1984" 'io/simple-ether.lisp.1'
hcuot "Mar  1 23:04:00 1985" 'io/stream.lisp.111'
hcuot "Sep  3 23:40:02 1984" 'io/stream.qfasl.108'
hcuot "Oct 30 00:27:53 1983" 'io/strmdoc.lisp.2'
hcuot "Nov 30 21:13:54 1984" 'io/unibus.lisp.26'
hcuot "Jun 20 00:04:54 1986" 'io/unibus.lisp.27'
hcuot "Aug 15 00:24:34 1984" 'io/unibus.qfasl.25'
hcuot "Sep 11 01:23:13 1984" 'io/access.qfasl.8'   # was 'io/file/access.qfasl.8'
hcuot "Oct 24 23:23:11 1983" 'io/baldir.lisp.114'  # was 'io/file/baldir.lisp.114'
hcuot "Aug 15 02:28:24 1984" 'io/baldir.qfasl.114' # was 'io/file/baldir.qfasl.114'
hcuot "Feb 14 23:03:22 1985" 'io/logical.lisp.1'   # was 'io/file/logical.lisp.1'
hcuot "Feb 12 23:15:41 1985" 'io/open.lisp.180'	   # was 'io/file/open.lisp.180'
hcuot "Sep 11 00:36:49 1984" 'io/open.qfasl.174'   # was 'io/file/open.qfasl.174'
hcuot "May 17 01:21:01 1985" 'io/pathnm.lisp.538'  # was 'io/file/pathnm.lisp.538'
hcuot "Sep  7 17:08:16 1984" 'io/pathnm.qfasl.528' # was 'io/file/pathnm.qfasl.528'
hcuot "Feb 24 02:18:10 1985" 'io/pathst.lisp.181'  # was 'io/file/pathst.lisp.181'
hcuot "Sep  7 17:13:26 1984" 'io/pathst.qfasl.173' # was 'io/file/pathst.qfasl.173'
hcuot "Jun 29 04:46:28 1982" 'io1/10leaf.points.1'
hcuot "Mar 17 02:10:51 1983" 'io1/as8748.lisp.40'
hcuot "Sep 16 05:09:14 1982" 'io1/as8751.lisp.29'
hcuot "Apr  7 08:18:57 1984" 'io1/cdrive.lisp.103'
hcuot "Apr  7 08:19:19 1984" 'io1/chatst.lisp.66'
hcuot "Sep 11 15:11:30 1984" 'io1/conver.lisp.147'
hcuot "Sep 11 15:37:37 1984" 'io1/conver.qfasl.147'
hcuot "Jun 29 04:49:20 1982" 'io1/door.bin.1'
hcuot "Jun 29 04:49:15 1982" 'io1/door.text.2'
hcuot "Jan 27 19:44:08 1984" 'io1/dplt.lisp.109'
hcuot "Apr  7 08:21:38 1984" 'io1/draw.lisp.23'
hcuot "Feb 12 19:04:09 1984" 'io1/eftp.bin-4.1'
hcuot "Nov 28 17:44:32 1983" 'io1/eftp.bin-5.1'
hcuot "Mar 31 00:20:29 1985" 'io1/eftp.bin-6.1'
hcuot "Dec 18 14:24:31 1983" 'io1/eftp.lisp.38'
hcuot "Jun  5 00:27:46 1984" 'io1/fntcnv.lisp.83'
hcuot "Aug 29 18:16:54 1984" 'io1/fntcnv.qfasl.83'
hcuot "Apr  7 08:22:34 1984" 'io1/fntdef.lisp.20'
hcuot "Feb  6 23:12:08 1985" 'io1/fquery.lisp.46'
hcuot "Aug  3 00:30:55 1984" 'io1/fquery.qfasl.45'
hcuot "Jun 29 04:50:23 1982" 'io1/hacks.lisp.190'
hcuot "Jun 27 06:05:25 1984" 'io1/hardcopy.lisp.1'
hcuot "Aug 14 23:53:21 1984" 'io1/hardcopy.qfasl.1'
hcuot "Aug 24 10:56:29 1983" 'io1/inc.lisp.8'
hcuot "Aug 14 22:58:05 1984" 'io1/inc.qfasl.8'
hcuot "Jun 13 16:49:50 1984" 'io1/infix.lisp.11'
hcuot "Aug 14 22:59:17 1984" 'io1/infix.qfasl.11'
hcuot "Jun 24 11:23:23 1984" 'io1/meter.lisp.42'
hcuot "Aug 29 21:21:38 1984" 'io1/meter.qfasl.42'
hcuot "Jun 29 04:50:59 1982" 'io1/mouse.text.11'
hcuot "Aug  3 01:01:51 1984" 'io1/output.lisp.38'
hcuot "Sep  7 16:29:43 1984" 'io1/output.qfasl.38'
hcuot "Nov 28 17:41:27 1983" 'io1/press.bin-5.3'
hcuot "Oct 13 00:30:20 1984" 'io1/press.lisp.147'
hcuot "Aug 29 21:16:40 1984" 'io1/press.qfasl.146'
hcuot "Apr  7 08:25:35 1984" 'io1/promp.lisp.13'
hcuot "Aug  3 06:06:58 1984" 'io1/reldmp.lisp.12'
hcuot "Aug  3 16:01:31 1984" 'io1/reldmp.qfasl.12'
hcuot "Apr  7 08:29:25 1984" 'io1/relld.lisp.10'
hcuot "Sep  7 11:09:42 1984" 'io1/rfontw.bin-5.1'
hcuot "Mar 31 00:19:47 1985" 'io1/rfontw.bin-6.1'
hcuot "Sep  6 16:22:47 1984" 'io1/rfontw.lisp.82'
hcuot "Sep  6 16:25:03 1984" 'io1/rfontw.qfasl.82'
hcuot "Nov 28 16:09:46 1983" 'io1/rfontx.lisp.75'
hcuot "Sep  6 16:16:41 1984" 'io1/rfontx.qfasl.75'
hcuot "May 12 00:21:02 1984" 'io1/serial.lisp.32'
hcuot "Aug 15 00:11:14 1984" 'io1/serial.qfasl.32'
hcuot "Jul  2 10:55:54 1984" 'io1/srccom.lisp.37'
hcuot "Aug 29 21:24:08 1984" 'io1/srccom.qfasl.37'
hcuot "Apr 23 04:39:47 1984" 'io1/swar.lisp.12'
hcuot "May 31 06:30:55 1985" 'io1/swar.qfasl.12'
hcuot "Nov  6 06:48:06 1984" 'io1/time.lisp.110'
hcuot "Aug  3 05:46:51 1984" 'io1/time.qfasl.105'
hcuot "Oct 20 14:28:27 1984" 'io1/timpar.lisp.75'
hcuot "Aug  3 05:48:41 1984" 'io1/timpar.qfasl.74'
hcuot "Jun 29 04:55:20 1982" 'io1/ukbd.lisp.24'
hcuot "Jun 29 04:55:35 1982" 'io1/wlr.doc.1'
hcuot "Jul 22 05:15:54 1983" 'io1/xgp.lisp.33'
hcuot "Sep 10 14:20:48 1984" 'io1/xgp.qfasl.33'
hcuot "Aug  1 17:16:45 1982" 'man/.bug.lmman.1'
hcuot "Aug  1 17:17:23 1982" 'man/.dlw.wordab.1'
hcuot "Aug  1 17:17:27 1982" 'man/.forma.text.33'
hcuot "Aug  1 17:17:39 1982" 'man/.machn.compar.1'
hcuot "Aug  1 17:17:42 1982" 'man/30flsp.kst.1'
hcuot "Aug  1 17:17:52 1982" 'man/37vrbl.kst.1'
hcuot "May 19 19:12:20 1984" 'man/areas.text.46'
hcuot "Oct 28 21:30:47 1985" 'man/bug-mail.txt.1'
hcuot "Jul 27 00:52:53 1984" 'man/chaos.text.27'
hcuot "Jun  1 02:44:46 1984" 'man/code.text.36'
hcuot "Jun  4 01:31:51 1984" 'man/compil.text.106'
hcuot "Jun  8 02:41:24 1984" 'man/cumulative.vars.24'
hcuot "Jun  1 01:23:25 1984" 'man/db-aid.text.14'
hcuot "May 21 14:48:55 1984" 'man/debug.text.21'
hcuot "Jun  8 01:14:16 1984" 'man/defstr.text.117'
hcuot "Jun  1 00:32:40 1984" 'man/errors.text.102'
hcuot "May 21 00:53:47 1984" 'man/fd-arr.text.26'
hcuot "May 19 19:12:55 1984" 'man/fd-clo.text.12'
hcuot "May 20 19:20:57 1984" 'man/fd-con.text.28'
hcuot "Jun  4 23:25:24 1984" 'man/fd-dtp.text.19'
hcuot "Jun  1 05:17:42 1984" 'man/fd-eva.text.46'
hcuot "May 21 12:53:05 1984" 'man/fd-fio.text.24'
hcuot "May 19 15:22:03 1984" 'man/fd-flo.text.24'
hcuot "May 31 22:27:05 1984" 'man/fd-fun.text.26'
hcuot "Jun  1 03:36:57 1984" 'man/fd-hac.text.47'
hcuot "May 20 17:56:54 1984" 'man/fd-loc.text.9'
hcuot "Jun  8 15:51:15 1984" 'man/fd-num.text.37'
hcuot "May 19 15:22:43 1984" 'man/fd-op.text.5'
hcuot "Jul 27 00:51:28 1984" 'man/fd-sg.text.16'
hcuot "Jun  1 04:34:17 1984" 'man/fd-str.text.27'
hcuot "May 31 22:32:03 1984" 'man/fd-sub.text.19'
hcuot "May 20 17:56:59 1984" 'man/fd-sym.text.14'
hcuot "Jun  1 02:45:51 1984" 'man/files.text.24'
hcuot "Aug  1 17:25:37 1982" 'man/flavor.bolio.1'
hcuot "Jun  1 05:34:13 1984" 'man/flavor.text.134'
hcuot "Aug  1 17:25:53 1982" 'man/font3.kst.1'
hcuot "May 19 14:30:31 1984" 'man/generic.text.14'
hcuot "Jun  8 02:08:53 1984" 'man/index.temp.2'
hcuot "May 19 14:56:30 1984" 'man/init.text.17'
hcuot "Jun  4 01:16:55 1984" 'man/intro.text.18'
hcuot "May 21 12:53:12 1984" 'man/ios.text.247'
hcuot "Apr 26 06:50:32 1983" 'man/looptm.lispm.2'
hcuot "May 31 22:33:03 1984" 'man/looptm.text.320'
hcuot "May 21 01:37:20 1984" 'man/macros.text.104'
hcuot "May 18 23:54:05 1984" 'man/maksys.text.38'
hcuot "Mar 10 18:43:51 1984" 'man/manual.bolio.25'
hcuot "Jun  1 04:42:32 1984" 'man/manual.fasl.33'
hcuot "Jun  1 04:30:15 1984" 'man/manual.lisp.33'
hcuot "Jun  1 00:40:13 1984" 'man/manual.text.44'
hcuot "Jun  8 02:31:51 1984" 'man/manual.vars.26'
hcuot "Mar 10 18:43:46 1984" 'man/manual2.bolio.2'
hcuot "Jun  8 02:09:29 1984" 'man/manual2.log.6'
hcuot "May 21 14:38:36 1984" 'man/manual2.text.8'
hcuot "Jun  1 00:56:56 1984" 'man/manual2a.10.1'
hcuot "Mar 10 18:43:46 1984" 'man/manual3.bolio.1'
hcuot "Mar 11 23:27:45 1984" 'man/manual3.text.1'
hcuot "Jun  1 00:38:17 1984" 'man/manual3a.text.1'
hcuot "Aug  1 17:28:55 1982" 'man/msg.text.8'
hcuot "Jun  1 00:28:39 1984" 'man/packd.text.106'
hcuot "Jun  1 00:29:53 1984" 'man/patch.text.54'
hcuot "Jun  4 23:31:29 1984" 'man/pathnm.text.99'
hcuot "May 19 19:15:40 1984" 'man/proces.text.55'
hcuot "May 20 00:21:25 1984" 'man/query.text.22'
hcuot "May 31 23:25:06 1984" 'man/rdprt.text.29'
hcuot "May 20 19:20:51 1984" 'man/resour.text.28'
hcuot "May 20 00:21:07 1984" 'man/stream.text.37'
hcuot "Oct  7 02:05:57 1982" 'man/testman.bolio.5'
hcuot "Oct  6 21:51:24 1982" 'man/testman.text.2'
hcuot "May 19 14:30:40 1984" 'man/time.text.40'
hcuot "Jun  7 22:11:55 1984" 'man/title.text.11'
hcuot "Jul 10 20:29:09 1984" 'network/addr-res.lisp.8'
hcuot "Jan  1 20:34:32 1984" 'network/ether-mini.lisp.10'
hcuot "Nov 28 05:35:58 1984" 'network/host.lisp.121'
hcuot "Sep 10 16:45:38 1984" 'network/host.qfasl.116'
hcuot "Sep  8 21:54:51 1984" 'network/package.lisp.7'
hcuot "Sep  8 23:59:30 1984" 'network/package.qfasl.7'
hcuot "Jul 15 00:00:45 1984" 'network/regions.lisp.1'
hcuot "May 30 01:15:13 1984" 'network/server.lisp.1'
hcuot "May 30 01:14:54 1984" 'network/service.lisp.3'
hcuot "Jul 13 13:29:53 1984" 'network/simple-ether.lisp.51'
hcuot "Jul 15 00:01:32 1984" 'network/smtp.lisp.1'
hcuot "Jul  4 12:10:43 1984" 'network/symbols.lisp.1'
hcuot "May 29 23:58:43 1984" 'network/symbols.qfasl.1'
hcuot "Dec 14 01:14:16 1984" 'io1/chatst.lisp.67'     # was 'network/chaos/chatst.lisp.67'
hcuot "Jun  6 00:11:05 1984" 'io1/chatst.qfasl.66'    # was 'network/chaos/chatst.qfasl.66'
hcuot "Dec  6 07:20:08 1984" 'io/chsaux.lisp.366'     # was 'network/chaos/chsaux.lisp.366'
hcuot "Aug 15 01:26:28 1984" 'io/chsaux.qfasl.359'    # was 'network/chaos/chsaux.qfasl.359'
hcuot "Mar 11 19:23:31 1985" 'io/chsncp.lisp.270'     # was 'network/chaos/chsncp.lisp.270'
hcuot "Sep 11 15:12:45 1984" 'io/chsncp.qfasl.265'    # was 'network/chaos/chsncp.qfasl.265'
hcuot "Nov 26 14:11:26 1984" 'io/chuse.lisp.14'	      # was 'network/chaos/chuse.lisp.14'
hcuot "Sep  4 15:12:29 1984" 'io/chuse.qfasl.11'      # was 'network/chaos/chuse.qfasl.11'
hcuot "Jun  4 14:53:34 1984" 'io1/eftp.lisp.39'	      # was 'network/chaos/eftp.lisp.39'
hcuot "Jun  6 00:12:57 1984" 'io1/eftp.qfasl.39'      # was 'network/chaos/eftp.qfasl.39'
hcuot "Sep 10 15:59:11 1984" 'window/peekch.lisp.31'  # was 'network/chaos/peekch.lisp.31'
hcuot "Sep 10 16:44:15 1984" 'window/peekch.qfasl.31' # was 'network/chaos/peekch.qfasl.31'
hcuot "Apr 12 22:45:29 1985" 'io/qfile.lisp.360'      # was 'network/chaos/qfile.lisp.360'
hcuot "Sep 10 16:37:50 1984" 'io/qfile.qfasl.353'     # was 'network/chaos/qfile.qfasl.353'
hcuot "Jul 17 09:27:46 1984" 'network/ip/address.lisp.3'
hcuot "Jul 17 09:27:51 1984" 'network/ip/address.qfasl.3'
hcuot "Nov 10 17:02:36 1984" 'network/ip/hostsnic.lisp.4'
hcuot "Nov 16 02:06:05 1984" 'patch/band.win.lisp.2'
hcuot "Nov 16 02:06:21 1984" 'patch/band.win.qfasl.2'
hcuot "Sep 11 17:19:54 1984" 'patch/cadr.patch-directory.1'
hcuot "Oct 22 14:47:31 1985" 'patch/cadr-4.patch-directory.12'
hcuot "Jan 28 01:01:11 1985" 'patch/cadr-4-1.lisp.8'
hcuot "Jan 28 01:01:19 1985" 'patch/cadr-4-1.qfasl.8'
hcuot "Jan  3 14:09:24 1985" 'patch/cadr-4-2.lisp.1'
hcuot "Jan  3 14:10:46 1985" 'patch/cadr-4-2.qfasl.1'
hcuot "Oct 22 14:45:59 1985" 'patch/cadr-4-3.lisp.1'
hcuot "Oct 22 14:46:16 1985" 'patch/cadr-4-3.qfasl.1'
hcuot "Nov 16 02:09:19 1984" 'patch/lm27fix.lisp.1'
hcuot "Nov 16 02:09:37 1984" 'patch/lm27fix.qfasl.1'
hcuot "Jun  6 18:30:00 1984" 'patch/system.patch-directory.25'
hcuot "Feb  3 21:12:33 1984" 'patch/system-94.patch-directory.129'
hcuot "Aug 21 15:52:31 1983" 'patch/system-94-41.qfasl.2'
hcuot "Nov  8 03:03:14 1983" 'patch/system-94-42.qfasl.1'
hcuot "Nov 12 23:20:58 1983" 'patch/system-94-43.qfasl.2'
hcuot "Nov 29 20:11:39 1983" 'patch/system-97.patch-directory.76'
hcuot "Nov  9 16:51:17 1983" 'patch/system-97-25.qfasl.1'
hcuot "Nov 11 14:27:52 1983" 'patch/system-97-26.qfasl.1'
hcuot "Nov 29 17:17:29 1983" 'patch/system-97-27.qfasl.1'
hcuot "Nov 29 20:11:04 1983" 'patch/system-97-28.qfasl.1'
hcuot "Nov 28 10:02:04 1984" 'patch/system-98.patch-directory.304'
hcuot "Nov 23 18:42:57 1983" 'patch/system-98-1.lisp.5'
hcuot "Dec 23 02:14:14 1983" 'patch/system-98-10.lisp.15'
hcuot "Dec 26 03:56:51 1983" 'patch/system-98-11.lisp.19'
hcuot "Dec 27 01:15:02 1983" 'patch/system-98-12.lisp.16'
hcuot "Dec 24 00:37:44 1983" 'patch/system-98-13.lisp.4'
hcuot "Dec 27 02:58:42 1983" 'patch/system-98-14.lisp.15'
hcuot "Dec 28 04:09:13 1983" 'patch/system-98-15.lisp.7'
hcuot "Dec 29 04:08:59 1983" 'patch/system-98-16.lisp.6'
hcuot "Dec 31 23:30:38 1983" 'patch/system-98-17.lisp.18'
hcuot "Jan  1 09:49:39 1984" 'patch/system-98-18.lisp.10'
hcuot "Jan  2 23:54:10 1984" 'patch/system-98-19.lisp.20'
hcuot "Nov 29 23:36:10 1983" 'patch/system-98-2.lisp.12'
hcuot "Jan  2 01:53:30 1984" 'patch/system-98-20.lisp.4'
hcuot "Jan  3 00:16:42 1984" 'patch/system-98-21.lisp.2'
hcuot "Jan  3 03:49:01 1984" 'patch/system-98-22.lisp.6'
hcuot "Jan  4 04:19:59 1984" 'patch/system-98-23.lisp.10'
hcuot "Jan  3 04:57:14 1984" 'patch/system-98-24.lisp.3'
hcuot "Jan  5 17:39:41 1984" 'patch/system-98-25.lisp.8'
hcuot "Jan  7 06:40:39 1984" 'patch/system-98-26.lisp.6'
hcuot "Jan 12 10:11:12 1984" 'patch/system-98-27.lisp.7'
hcuot "Jan  9 20:43:59 1984" 'patch/system-98-28.lisp.3'
hcuot "Jan 14 21:33:30 1984" 'patch/system-98-29.lisp.12'
hcuot "Dec  6 08:55:14 1983" 'patch/system-98-3.lisp.16'
hcuot "Jan 29 22:20:17 1984" 'patch/system-98-30.lisp.22'
hcuot "Feb  1 04:45:59 1984" 'patch/system-98-31.lisp.18'
hcuot "Jan 27 03:08:05 1984" 'patch/system-98-32.lisp.8'
hcuot "Feb  8 11:54:19 1984" 'patch/system-98-33.lisp.26'
hcuot "Feb  1 07:00:29 1984" 'patch/system-98-34.lisp.1'
hcuot "Feb 15 17:07:07 1984" 'patch/system-98-35.lisp.9'
hcuot "Feb  3 03:30:52 1984" 'patch/system-98-36.lisp.1'
hcuot "Feb 22 17:51:54 1984" 'patch/system-98-37.lisp.10'
hcuot "Mar 11 19:18:12 1984" 'patch/system-98-38.lisp.4'
hcuot "Mar 24 14:37:28 1984" 'patch/system-98-39.lisp.20'
hcuot "Dec  5 08:56:31 1983" 'patch/system-98-4.lisp.7'
hcuot "Apr  3 05:57:33 1984" 'patch/system-98-40.lisp.43'
hcuot "Apr  6 16:33:48 1984" 'patch/system-98-41.lisp.10'
hcuot "Mar 21 15:54:25 1984" 'patch/system-98-42.lisp.2'
hcuot "Mar 15 03:21:58 1984" 'patch/system-98-43.lisp.1'
hcuot "Apr 17 14:47:49 1984" 'patch/system-98-44.lisp.22'
hcuot "Apr 21 16:27:40 1984" 'patch/system-98-45.lisp.5'
hcuot "Apr  6 02:35:55 1984" 'patch/system-98-46.lisp.1'
hcuot "May  8 04:23:06 1984" 'patch/system-98-47.lisp.37'
hcuot "Apr 18 01:16:58 1984" 'patch/system-98-48.lisp.1'
hcuot "May 29 12:44:39 1984" 'patch/system-98-49.lisp.8'
hcuot "Dec  8 22:28:54 1983" 'patch/system-98-5.lisp.11'
hcuot "Jun  5 18:30:09 1984" 'patch/system-98-50.lisp.39'
hcuot "Jun  5 18:34:15 1984" 'patch/system-98-50.qfasl.39'
hcuot "May  1 05:40:21 1984" 'patch/system-98-51.lisp.1'
hcuot "May  1 05:40:31 1984" 'patch/system-98-51.qfasl.1'
hcuot "May 10 03:26:54 1984" 'patch/system-98-52.lisp.1'
hcuot "May 10 03:27:04 1984" 'patch/system-98-52.qfasl.1'
hcuot "May 12 02:33:07 1984" 'patch/system-98-53.lisp.2'
hcuot "May 12 02:33:12 1984" 'patch/system-98-53.qfasl.2'
hcuot "May 22 22:29:09 1984" 'patch/system-98-54.lisp.4'
hcuot "May 22 22:29:30 1984" 'patch/system-98-54.qfasl.4'
hcuot "May 28 01:59:01 1984" 'patch/system-98-55.lisp.3'
hcuot "May 28 01:59:51 1984" 'patch/system-98-55.qfasl.3'
hcuot "May 21 20:26:36 1984" 'patch/system-98-56.lisp.2'
hcuot "May 21 20:26:50 1984" 'patch/system-98-56.qfasl.2'
hcuot "Jun  4 13:45:26 1984" 'patch/system-98-57.lisp.23'
hcuot "Jun  4 14:08:38 1984" 'patch/system-98-57.qfasl.23'
hcuot "May 24 19:26:35 1984" 'patch/system-98-58.lisp.1'
hcuot "May 24 19:26:44 1984" 'patch/system-98-58.qfasl.1'
hcuot "Jun  5 15:11:43 1984" 'patch/system-98-59.lisp.3'
hcuot "Jun  5 15:11:53 1984" 'patch/system-98-59.qfasl.3'
hcuot "Dec 13 16:06:04 1983" 'patch/system-98-6.lisp.17'
hcuot "Jun 13 05:08:07 1984" 'patch/system-98-60.lisp.5'
hcuot "Jun 13 05:08:13 1984" 'patch/system-98-60.qfasl.5'
hcuot "Jun  9 04:10:30 1984" 'patch/system-98-61.lisp.2'
hcuot "Jun  9 04:10:39 1984" 'patch/system-98-61.qfasl.2'
hcuot "Jun 17 04:05:51 1984" 'patch/system-98-62.lisp.12'
hcuot "Jun 19 18:27:54 1984" 'patch/system-98-62.qfasl.12'
hcuot "Jul  2 15:43:53 1984" 'patch/system-98-63.lisp.18'
hcuot "Jul  2 15:44:01 1984" 'patch/system-98-63.qfasl.18'
hcuot "Jun 15 04:19:50 1984" 'patch/system-98-64.lisp.1'
hcuot "Jun 29 03:07:30 1984" 'patch/system-98-64.qfasl.1'
hcuot "Jul  2 15:55:49 1984" 'patch/system-98-65.lisp.10'
hcuot "Jul  2 15:56:02 1984" 'patch/system-98-65.qfasl.10'
hcuot "Jul  9 15:08:09 1984" 'patch/system-98-66.lisp.9'
hcuot "Jul  9 15:08:19 1984" 'patch/system-98-66.qfasl.9'
hcuot "Jun 29 03:22:38 1984" 'patch/system-98-67.lisp.1'
hcuot "Jun 29 03:22:46 1984" 'patch/system-98-67.qfasl.1'
hcuot "Jul 18 12:57:10 1984" 'patch/system-98-68.lisp.5'
hcuot "Jul 18 12:57:21 1984" 'patch/system-98-68.qfasl.5'
hcuot "Aug 14 14:39:14 1984" 'patch/system-98-69.lisp.1'
hcuot "Aug 14 14:39:24 1984" 'patch/system-98-69.qfasl.1'
hcuot "Dec 15 21:40:10 1983" 'patch/system-98-7.lisp.7'
hcuot "Aug 29 09:25:48 1984" 'patch/system-98-70.lisp.1'
hcuot "Aug 29 09:25:55 1984" 'patch/system-98-70.qfasl.1'
hcuot "Oct 12 20:02:56 1984" 'patch/system-98-71.lisp.3'
hcuot "Oct 12 20:04:03 1984" 'patch/system-98-71.qfasl.3'
hcuot "Oct 14 16:03:15 1984" 'patch/system-98-72.lisp.2'
hcuot "Oct 14 16:03:36 1984" 'patch/system-98-72.qfasl.2'
hcuot "Oct 11 03:17:25 1984" 'patch/system-98-73.lisp.2'
hcuot "Oct 11 09:50:01 1984" 'patch/system-98-73.qfasl.2'
hcuot "Oct 12 20:03:51 1984" 'patch/system-98-74.lisp.1'
hcuot "Oct 12 20:04:03 1984" 'patch/system-98-74.qfasl.1'
hcuot "Oct 14 01:56:53 1984" 'patch/system-98-75.lisp.1'
hcuot "Oct 14 01:57:21 1984" 'patch/system-98-75.qfasl.1'
hcuot "Oct 14 15:58:47 1984" 'patch/system-98-76.lisp.1'
hcuot "Oct 14 15:59:14 1984" 'patch/system-98-76.qfasl.1'
hcuot "Oct 20 13:08:35 1984" 'patch/system-98-77.lisp.1'
hcuot "Nov 12 07:24:48 1984" 'patch/system-98-77.qfasl.1'
hcuot "Nov 12 06:57:12 1984" 'patch/system-98-78.lisp.5'
hcuot "Nov 12 06:57:43 1984" 'patch/system-98-78.qfasl.5'
hcuot "Nov 20 10:35:13 1984" 'patch/system-98-79.lisp.8'
hcuot "Nov 20 18:34:09 1984" 'patch/system-98-79.ncp.3'
hcuot "Nov 20 10:35:23 1984" 'patch/system-98-79.qfasl.8'
hcuot "Nov 20 17:51:27 1984" 'patch/system-98-79-chsncp.lisp.3'
hcuot "Dec 17 19:24:01 1983" 'patch/system-98-8.lisp.12'
hcuot "Nov 26 15:21:29 1984" 'patch/system-98-80.lisp.1'
hcuot "Nov 26 15:21:56 1984" 'patch/system-98-80.qfasl.1'
hcuot "Nov 28 09:58:45 1984" 'patch/system-98-81.lisp.2'
hcuot "Nov 28 09:59:12 1984" 'patch/system-98-81.qfasl.2'
hcuot "Dec 22 11:18:46 1983" 'patch/system-98-9.lisp.9'
hcuot "Oct 14 10:17:53 1984" 'patch/system-98-9.qfasl.9'
hcuot "Apr 28 13:38:10 1987" 'patch/system-99.patch-directory.131'
hcuot "Apr 28 13:41:53 1987" 'patch/system-99.patch-directory.132'
hcuot "Sep 12 12:29:34 1984" 'patch/system-99-1.lisp.3'
hcuot "Sep 12 12:30:01 1984" 'patch/system-99-1.qfasl.3'
hcuot "Nov  9 15:17:06 1984" 'patch/system-99-10.lisp.31'
hcuot "Nov  9 15:32:38 1984" 'patch/system-99-10.qfasl.31'
hcuot "Dec  4 13:45:55 1984" 'patch/system-99-13.lisp.17'
hcuot "Dec  4 13:46:23 1984" 'patch/system-99-13.qfasl.17'
hcuot "Dec 14 07:48:14 1984" 'patch/system-99-14.lisp.25'
hcuot "Dec 14 07:48:35 1984" 'patch/system-99-14.qfasl.25'
hcuot "Dec 14 09:09:41 1984" 'patch/system-99-15.lisp.7'
hcuot "Dec 14 09:59:42 1984" 'patch/system-99-15.qfasl.7'
hcuot "Jan 28 00:53:36 1985" 'patch/system-99-16.lisp.3'
hcuot "Jan 28 00:53:43 1985" 'patch/system-99-16.qfasl.3'
hcuot "Feb 11 20:32:11 1985" 'patch/system-99-17.lisp.11'
hcuot "Feb 11 20:32:27 1985" 'patch/system-99-17.qfasl.11'
hcuot "Feb 11 20:41:59 1985" 'patch/system-99-18.lisp.36'
hcuot "Feb 11 20:42:16 1985" 'patch/system-99-18.qfasl.36'
hcuot "Feb 17 03:34:09 1985" 'patch/system-99-19.lisp.26'
hcuot "Feb 17 04:27:41 1985" 'patch/system-99-19.qfasl.26'
hcuot "Sep 12 12:28:00 1984" 'patch/system-99-2.lisp.2'
hcuot "Sep 12 12:28:11 1984" 'patch/system-99-2.qfasl.2'
hcuot "Feb 15 02:05:58 1985" 'patch/system-99-20.lisp.6'
hcuot "Feb 15 02:06:09 1985" 'patch/system-99-20.qfasl.6'
hcuot "Feb 18 10:45:52 1985" 'patch/system-99-21.lisp.17'
hcuot "Feb 27 13:42:42 1985" 'patch/system-99-21.qfasl.17'
hcuot "Feb 28 10:07:50 1985" 'patch/system-99-22.lisp.14'
hcuot "Feb 28 10:08:05 1985" 'patch/system-99-22.qfasl.14'
hcuot "May 17 01:06:11 1985" 'patch/system-99-23.lisp.13'
hcuot "May 17 01:06:19 1985" 'patch/system-99-23.qfasl.13'
hcuot "May 17 00:59:53 1985" 'patch/system-99-24.lisp.7'
hcuot "May 17 01:25:44 1985" 'patch/system-99-24.qfasl.7'
hcuot "May  2 20:50:42 1985" 'patch/system-99-25.lisp.7'
hcuot "May  2 20:51:00 1985" 'patch/system-99-25.qfasl.7'
hcuot "May  2 21:50:29 1985" 'patch/system-99-26.lisp.5'
hcuot "May  2 21:50:39 1985" 'patch/system-99-26.qfasl.5'
hcuot "May 17 01:23:15 1985" 'patch/system-99-27.lisp.4'
hcuot "May 17 01:23:21 1985" 'patch/system-99-27.qfasl.4'
hcuot "May 17 01:05:54 1985" 'patch/system-99-28.lisp.1'
hcuot "Dec  8 16:53:28 1986" 'patch/system-99-28.qfasl.1'
hcuot "Sep 10 02:08:44 1985" 'patch/system-99-29.lisp.8'
hcuot "Jul 18 15:48:17 1986" 'patch/system-99-29.lisp.9'
hcuot "Jul 18 15:48:32 1986" 'patch/system-99-29.qfasl.9'
hcuot "Sep 14 20:40:56 1984" 'patch/system-99-3.lisp.5'
hcuot "Sep 14 20:41:07 1984" 'patch/system-99-3.qfasl.5'
hcuot "Jul 18 15:22:29 1986" 'patch/system-99-30.lisp.1'
hcuot "Jul 18 16:02:59 1986" 'patch/system-99-30.lisp.2'
hcuot "Jul 18 16:03:06 1986" 'patch/system-99-30.qfasl.2'
hcuot "Dec  8 12:26:00 1986" 'patch/system-99-31.lisp.1'
hcuot "Dec  8 12:26:07 1986" 'patch/system-99-31.qfasl.1'
hcuot "Apr 28 13:41:16 1987" 'patch/system-99-32.lisp.1'
hcuot "Apr 28 13:41:21 1987" 'patch/system-99-32.qfasl.1'
hcuot "Sep 26 09:23:43 1984" 'patch/system-99-4.lisp.6'
hcuot "Sep 26 09:23:50 1984" 'patch/system-99-4.qfasl.6'
hcuot "Sep 26 13:13:08 1984" 'patch/system-99-5.lisp.10'
hcuot "Sep 26 13:13:19 1984" 'patch/system-99-5.qfasl.10'
hcuot "Sep 29 07:04:03 1984" 'patch/system-99-6.lisp.3'
hcuot "Sep 29 07:04:07 1984" 'patch/system-99-6.qfasl.3'
hcuot "Oct 16 02:25:45 1984" 'patch/system-99-7.lisp.10'
hcuot "Oct 16 02:26:00 1984" 'patch/system-99-7.qfasl.10'
hcuot "Oct 16 08:48:46 1984" 'patch/system-99-8.lisp.9'
hcuot "Oct 16 08:49:03 1984" 'patch/system-99-8.qfasl.9'
hcuot "Oct 23 18:36:34 1984" 'patch/system-99-9.lisp.16'
hcuot "Oct 23 18:37:01 1984" 'patch/system-99-9.qfasl.16'
hcuot "Sep  9 17:46:32 1984" 'patch/zmail.patch-directory.3'
hcuot "Mar 16 20:13:01 1985" 'patch/zmail-54.patch-directory.10'
hcuot "Sep 26 01:22:03 1984" 'patch/zmail-54-1.lisp.2'
hcuot "Sep 26 01:22:09 1984" 'patch/zmail-54-1.qfasl.2'
hcuot "Oct 14 05:12:26 1984" 'patch/zmail-54-2.lisp.1'
hcuot "Oct 14 05:12:30 1984" 'patch/zmail-54-2.qfasl.1'
hcuot "Nov 30 00:36:29 1984" 'patch/zmail-54-3.lisp.1'
hcuot "Nov 30 00:36:35 1984" 'patch/zmail-54-3.qfasl.1'
hcuot "Mar 16 20:13:18 1985" 'patch/zmail-54-4.lisp.3'
hcuot "Mar 16 20:13:25 1985" 'patch/zmail-54-4.qfasl.3'
hcuot "Nov 12 22:40:20 1984" 'site/-read-.-me-.1'
hcuot "Jan 10 13:02:06 1985" 'site/hsttbl.lisp.124'
hcuot "May  3 17:47:46 1985" 'site/hsttbl.qfasl.124'
hcuot "Sep 20 10:57:24 1985" 'site/lmlocs.lisp.162'
hcuot "Jan 24 18:56:53 1985" 'site/lmlocs.qbin.1'
hcuot "May  3 17:47:12 1985" 'site/lmlocs.qfasl.161'
hcuot "Jan 10 12:44:03 1985" 'site/site.lisp.1'
hcuot "May  3 17:46:36 1985" 'site/site.qfasl.1'
hcuot "Jun 29 02:32:33 1982" 'sys/-read-.-this-.1'
hcuot "Dec 11 06:29:53 1984" 'sys/cadrlp.lisp.152'
hcuot "Oct  4 13:13:47 1985" 'sys/cadrlp.qfasl.152'
hcuot "Apr  7 08:44:05 1984" 'sys/cadsym.lisp.25'
hcuot "Jun 16 15:47:26 1984" 'sys/cdmp.lisp.52'
hcuot "Oct  4 13:24:42 1985" 'sys/cdmp.qfasl.52'
hcuot "Feb 28 09:05:47 1985" 'sys/clpack.lisp.153'
hcuot "Sep 10 01:09:12 1984" 'sys/clpack.qfasl.151'
hcuot "Apr  7 08:45:18 1984" 'sys/compat.lisp.32'
hcuot "Jul  1 12:34:34 1985" 'sys/eval.lisp.97'
hcuot "Sep  8 23:39:13 1984" 'sys/eval.qfasl.78'
hcuot "Feb 12 04:14:08 1985" 'sys/fspec.lisp.1'
hcuot "Jan 30 11:13:11 1985" 'sys/genric.lisp.33'
hcuot "Jan 30 11:13:36 1985" 'sys/genric.qfasl.33'
hcuot "Feb 13 09:44:30 1985" 'sys/ltop.lisp.498'
hcuot "Sep 11 01:20:26 1984" 'sys/ltop.qfasl.494'
hcuot "Oct 23 22:41:44 1982" 'sys/ma.lisp.305'
hcuot "Aug  1 16:39:34 1984" 'sys/ma.qfasl.305'
hcuot "Jun 29 02:36:30 1982" 'sys/madefs.lisp.7'
hcuot "Jul 29 21:18:23 1984" 'sys/madefs.qfasl.7'
hcuot "Oct 12 19:33:14 1983" 'sys/maopt.lisp.4'
hcuot "Aug  1 16:44:12 1984" 'sys/maopt.qfasl.4'
hcuot "Nov 16 03:21:33 1983" 'sys/mc.lisp.354'
hcuot "Aug  1 16:45:44 1984" 'sys/mc.qfasl.354'
hcuot "Jan  3 18:08:41 1983" 'sys/mlap.lisp.51'
hcuot "Aug  1 16:48:15 1984" 'sys/mlap.qfasl.51'
hcuot "Jun 25 17:40:50 1983" 'sys/pack4.lisp.286'
hcuot "Jan 30 06:11:13 1985" 'sys/qcdefs.lisp.153'
hcuot "Sep  9 14:00:57 1984" 'sys/qcdefs.qfasl.149'
hcuot "Sep 10 17:05:15 1984" 'sys/qcfasd.lisp.248'
hcuot "Sep 10 17:05:43 1984" 'sys/qcfasd.qfasl.248'
hcuot "Jan 30 10:48:52 1985" 'sys/qcfile.lisp.324'
hcuot "Sep  6 20:12:10 1984" 'sys/qcfile.qfasl.322'
hcuot "Sep  8 17:30:16 1984" 'sys/qclap.lisp.244'
hcuot "Sep  9 14:16:55 1984" 'sys/qclap.qfasl.244'
hcuot "Aug 30 07:51:41 1984" 'sys/qcluke.lisp.26'
hcuot "Aug 30 14:38:31 1984" 'sys/qcluke.qfasl.26'
hcuot "Nov  6 07:41:16 1984" 'sys/qcopt.lisp.137'
hcuot "Sep  9 14:13:24 1984" 'sys/qcopt.qfasl.133'
hcuot "Dec 11 13:38:25 1984" 'sys/qcp1.lisp.573'
hcuot "Sep  9 14:03:10 1984" 'sys/qcp1.qfasl.562'
hcuot "Oct 28 15:41:46 1984" 'sys/qcp2.lisp.261'
hcuot "Sep  9 14:09:07 1984" 'sys/qcp2.qfasl.259'
hcuot "Aug  2 21:31:47 1984" 'sys/qcpeep.lisp.36'
hcuot "Aug  2 21:31:56 1984" 'sys/qcpeep.qfasl.36'
hcuot "Jan 11 15:27:40 1984" 'sys/qev.lisp.289'
hcuot "Feb 26 04:27:46 1985" 'sys/qfasl.lisp.463'
hcuot "Aug 14 23:35:35 1984" 'sys/qfasl.qfasl.461'
hcuot "Nov 16 19:51:17 1984" 'sys/qfctns.lisp.774'
hcuot "Aug 31 11:59:24 1984" 'sys/qfctns.qfasl.769'
hcuot "Dec 14 02:04:10 1984" 'sys/qmisc.lisp.659'
hcuot "Aug 31 16:20:48 1984" 'sys/qmisc.qfasl.652'
hcuot "Apr  3 08:55:26 1984" 'sys/qnew.lisp.20'
hcuot "Aug 14 23:54:18 1984" 'sys/qnew.qfasl.20'
hcuot "Jul  9 14:51:08 1985" 'sys/qrand.lisp.412'
hcuot "Sep  4 17:45:01 1984" 'sys/qrand.qfasl.408'
hcuot "Dec 11 08:56:51 1984" 'sys/qwmcr.lisp.22'
hcuot "Dec 11 08:56:58 1984" 'sys/qwmcr.qfasl.22'
hcuot "Jun 18 02:57:37 1983" 'sys/recom.lisp.33'
hcuot "Aug 27 02:28:30 1982" 'sys/sgfctn.lisp.57'
hcuot "Aug 15 00:13:05 1984" 'sys/sgfctn.qfasl.57'
hcuot "Oct 10 02:20:38 1983" 'sys/sort.lisp.59'
hcuot "Aug 15 00:13:45 1984" 'sys/sort.qfasl.59'
hcuot "May  2 21:12:41 1985" 'sys/sysdcl.lisp.193'
hcuot "Oct  4 12:41:35 1985" 'sys/sysdcl.qfasl.193'
hcuot "Jan 28 20:23:45 1985" 'sys/types.lisp.72'
hcuot "Sep  8 21:39:45 1984" 'sys/types.qfasl.69'
hcuot "Jun 29 02:49:58 1982" 'sys/ucinit.qfasl.1'
hcuot "Feb 18 02:47:48 1985" 'sys2/advise.lisp.38'
hcuot "Aug 14 21:43:06 1984" 'sys2/advise.qfasl.37'
hcuot "Feb 24 05:42:25 1985" 'sys2/analyze.lisp.19'
hcuot "Sep 11 01:03:49 1984" 'sys2/analyze.qfasl.17'
hcuot "Jul 27 02:09:35 1984" 'sys2/band.lisp.44'
hcuot "Nov 24 02:31:51 1984" 'sys2/band.qfasl.46'
hcuot "Feb  4 01:18:04 1985" 'sys2/character.lisp.22'
hcuot "Sep  7 16:04:06 1984" 'sys2/character.qfasl.20'
hcuot "Jun 14 23:56:23 1984" 'sys2/class.lisp.99'
hcuot "Sep  4 15:31:45 1984" 'sys2/class.qfasl.99'
hcuot "Aug 24 04:12:36 1984" 'sys2/clmac.lisp.4'
hcuot "Aug 28 21:55:00 1984" 'sys2/clmac.qfasl.4'
hcuot "Apr  7 08:50:06 1984" 'sys2/cmany.lisp.46'
hcuot "Apr  7 08:50:53 1984" 'sys2/condit.lisp.2'
hcuot "Feb 13 10:25:14 1985" 'sys2/defmac.lisp.80'
hcuot "Aug 29 15:55:29 1984" 'sys2/defmac.qfasl.78'
hcuot "Aug 28 20:45:38 1984" 'sys2/defsel.lisp.70'
hcuot "Aug 29 01:26:03 1984" 'sys2/defsel.qfasl.70'
hcuot "Feb 15 01:49:03 1985" 'sys2/describe.lisp.3'
hcuot "Dec 14 07:05:57 1984" 'sys2/disass.lisp.94'
hcuot "Aug  1 16:38:35 1984" 'sys2/disass.qfasl.92'
hcuot "Nov 28 09:50:50 1984" 'sys2/encaps.lisp.28'
hcuot "Aug 14 22:10:54 1984" 'sys2/encaps.qfasl.27'
hcuot "Feb 11 00:03:31 1985" 'sys2/flavor.lisp.283'
hcuot "Sep 10 23:12:37 1984" 'sys2/flavor.qfasl.280'
hcuot "Dec 14 03:01:18 1984" 'sys2/gc.lisp.174'
hcuot "Aug 14 22:34:41 1984" 'sys2/gc.qfasl.169'
hcuot "Mar  2 08:03:29 1985" 'sys2/hash.lisp.89'
hcuot "Aug 14 22:51:05 1984" 'sys2/hash.qfasl.87'
hcuot "Mar  6 00:23:07 1985" 'sys2/hashfl.lisp.33'
hcuot "Aug 14 22:52:39 1984" 'sys2/hashfl.qfasl.29'
hcuot "Apr  7 08:51:47 1984" 'sys2/let.lisp.8'
hcuot "May  2 21:13:45 1985" 'sys2/lmmac.lisp.389'
hcuot "Aug 31 15:54:02 1984" 'sys2/lmmac.qfasl.372'
hcuot "Sep  3 20:04:30 1984" 'sys2/login.lisp.87'
hcuot "Sep  3 21:06:25 1984" 'sys2/login.qfasl.87'
hcuot "Dec  9 00:37:37 1984" 'sys2/loop.lisp.829'
hcuot "Oct 24 03:03:31 1984" 'sys2/loop.qfasl.799'
hcuot "May  2 21:54:59 1985" 'sys2/macarr.lisp.2'
hcuot "May  2 21:55:13 1985" 'sys2/macarr.qfasl.2'
hcuot "Sep 13 16:53:32 1984" 'sys2/maksys.lisp.180'
hcuot "Sep  4 16:16:36 1984" 'sys2/maksys.qfasl.178'
hcuot "Apr  9 11:08:41 1984" 'sys2/matrix.lisp.26'
hcuot "Aug 29 21:20:25 1984" 'sys2/matrix.qfasl.26'
hcuot "Sep  4 15:35:26 1984" 'sys2/meth.lisp.63'
hcuot "Sep  4 15:35:37 1984" 'sys2/meth.qfasl.63'
hcuot "Oct  6 06:43:45 1984" 'sys2/numdef.lisp.12'
hcuot "Sep 10 16:30:29 1984" 'sys2/numdef.qfasl.11'
hcuot "Dec 14 02:58:42 1984" 'sys2/numer.lisp.62'
hcuot "Sep 10 16:32:20 1984" 'sys2/numer.qfasl.60'
hcuot "May  1 19:02:09 1986" 'sys2/patch.lisp.166'
hcuot "Dec  8 13:41:50 1986" 'sys2/patch.lisp.167'
hcuot "Aug 14 23:22:21 1984" 'sys2/patch.qfasl.158'
hcuot "Aug 29 18:33:36 1984" 'sys2/plane.lisp.32'
hcuot "Aug 29 18:45:45 1984" 'sys2/plane.qfasl.32'
hcuot "Feb 13 09:50:42 1985" 'sys2/proces.lisp.159'
hcuot "Aug 14 23:28:32 1984" 'sys2/proces.qfasl.157'
hcuot "Feb 13 09:59:15 1985" 'sys2/prodef.lisp.49'
hcuot "Aug 31 13:59:54 1984" 'sys2/prodef.qfasl.48'
hcuot "Dec  5 19:41:34 1984" 'sys2/qtrace.lisp.152'
hcuot "Sep  4 15:43:34 1984" 'sys2/qtrace.qfasl.151'
hcuot "Sep  4 16:08:45 1984" 'sys2/rat.lisp.46'
hcuot "Sep 10 16:47:13 1984" 'sys2/rat.qfasl.46'
hcuot "Nov 10 03:26:41 1984" 'sys2/resour.lisp.31'
hcuot "Aug 15 00:08:50 1984" 'sys2/resour.qfasl.28'
hcuot "Feb 13 08:19:42 1985" 'sys2/selev.lisp.24'
hcuot "Aug 28 21:54:21 1984" 'sys2/selev.qfasl.23'
hcuot "Oct 29 03:56:33 1984" 'sys2/setf.lisp.97'
hcuot "Aug 31 12:48:05 1984" 'sys2/setf.qfasl.95'
hcuot "Feb  7 00:26:28 1985" 'sys2/sgdefs.lisp.57'
hcuot "Aug 14 21:40:43 1984" 'sys2/sgdefs.qfasl.54'
hcuot "Sep 26 02:01:17 1984" 'sys2/step.lisp.72'
hcuot "Aug 15 00:15:30 1984" 'sys2/step.qfasl.70'
hcuot "Sep 25 01:20:50 1984" 'sys2/string.lisp.147'
hcuot "Sep 10 01:15:39 1984" 'sys2/string.qfasl.146'
hcuot "Jul 31 17:42:25 1984" 'sys2/struct.lisp.322'
hcuot "Aug 14 16:20:04 1984" 'sys2/struct.qfasl.322'
hcuot "Oct  9 08:52:31 1984" 'sys2/unfasl.lisp.19'
hcuot "Sep 11 01:52:22 1984" 'sys2/unfasl.qfasl.18'
hcuot "Oct 22 15:07:22 1985" 'sys2/usymld.lisp.188'
hcuot "Oct 22 15:07:43 1985" 'sys2/usymld.qfasl.188'
hcuot "Feb 16 07:56:19 1984" 'tape/copy.lisp.133'
hcuot "Jan  3 03:50:47 1984" 'tape/copy.qfasl.128'
hcuot "May 11 23:49:07 1984" 'tape/ddoc.text.8'
hcuot "May 11 23:29:43 1984" 'tape/fdump.lisp.27'
hcuot "May 11 23:52:14 1984" 'tape/fdump-def.lisp.12'
hcuot "Jan  2 03:37:10 1984" 'tape/fdump-def.qfasl.1'
hcuot "Jan  9 22:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'
hcuot "Jan 19 10:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'
hcuot "May 11 23:29:45 1984" 'tape/fdump-r.lisp.5'
hcuot "Jan  3 04:36:11 1984" 'tape/magtape.directory.11'
hcuot "Oct 26 15:41:54 1983" 'tape/magtape-14.directory.14'
hcuot "Mar  8 00:56:47 1983" 'tape/magtape-14-1.qfasl.1'
hcuot "Apr 25 03:51:48 1983" 'tape/magtape-14-3.qfasl.1'
hcuot "May 18 22:11:34 1983" 'tape/magtape-14-4.qfasl.3'
hcuot "Oct 26 15:41:16 1983" 'tape/magtape-14-5.qfasl.1'
hcuot "Feb 16 08:24:04 1984" 'tape/magtape-22.directory.13'
hcuot "Jan  7 16:40:45 1984" 'tape/magtape-22-1.lisp.1'
hcuot "Jan  7 16:40:56 1984" 'tape/magtape-22-1.qfasl.1'
hcuot "Jan  7 17:28:27 1984" 'tape/magtape-22-2.lisp.1'
hcuot "Jan  7 17:28:40 1984" 'tape/magtape-22-2.qfasl.1'
hcuot "Jan  7 18:41:18 1984" 'tape/magtape-22-3.lisp.1'
hcuot "Jan  7 18:41:44 1984" 'tape/magtape-22-3.qfasl.1'
hcuot "Jan 13 07:06:26 1984" 'tape/magtape-22-4.lisp.1'
hcuot "Jan 13 07:06:35 1984" 'tape/magtape-22-4.qfasl.1'
hcuot "Jan 19 11:40:22 1984" 'tape/magtape-22-5.lisp.1'
hcuot "Jan 19 11:40:32 1984" 'tape/magtape-22-5.qfasl.1'
hcuot "Feb 16 08:23:22 1984" 'tape/magtape-22-6.lisp.1'
hcuot "Feb 16 08:23:28 1984" 'tape/magtape-22-6.qfasl.1'
hcuot "Jan 19 11:04:02 1984" 'tape/mtaux.lisp.80'
hcuot "Jan  3 03:52:48 1984" 'tape/mtaux.qfasl.77'
hcuot "Dec 16 09:34:10 1983" 'tape/mtdefs.lisp.30'
hcuot "Jan  3 03:46:18 1984" 'tape/mtdefs.qfasl.30'
hcuot "Jan 10 23:40:52 1984" 'tape/mtstr.lisp.87'
hcuot "Jan  3 03:47:58 1984" 'tape/mtstr.qfasl.85'
hcuot "Jan  3 02:50:55 1984" 'tape/odump.lisp.1'
hcuot "Jan  3 04:33:05 1984" 'tape/odump.qfasl.1'
hcuot "May 11 23:29:46 1984" 'tape/package.lisp.1'
hcuot "Jan  3 01:59:49 1984" 'tape/pdp10.lisp.1'
hcuot "May 12 02:31:18 1984" 'tape/rmunit.lisp.3'
hcuot "May 11 23:29:46 1984" 'tape/system.lisp.3'
hcuot "May 11 23:29:47 1984" 'tape/tm.lisp.25'
hcuot "May 11 23:29:48 1984" 'tape/tmdefs.lisp.7'
hcuot "May 12 01:27:24 1984" 'tape/unit.lisp.7'
hcuot "Jan  3 02:01:02 1984" 'tape/vms.lisp.1'
hcuot "May 12 01:28:11 1984" 'tape/new/mtdefs.lisp.4'
hcuot "May 12 01:45:03 1984" 'tape/new/mtdefs.qfasl.4'
hcuot "May 11 23:29:49 1984" 'tape/new/mtrqb.lisp.3'
hcuot "May 12 02:31:35 1984" 'tape/new/mtstr.lisp.5'
hcuot "May 11 23:29:50 1984" 'tape/new/tmunit.lisp.5'
hcuot "May 11 23:29:51 1984" 'tape/new/weunit.lisp.3'
hcuot "Nov 20 17:29:49 1982" 'ubin/dcfu.uload.4'
hcuot "Aug  4 01:23:05 1982" 'ubin/memd.uload.1'
hcuot "Feb  6 23:35:36 1986" 'ubin/ucadr.loc.322'
hcuot "Sep 11 15:24:11 1984" 'ubin/ucadr.locs.320'
hcuot "Feb  6 23:33:22 1986" 'ubin/ucadr.mcr.322'
hcuot "Sep 11 15:22:04 1984" 'ubin/ucadr.sym.320'
hcuot "Feb  6 23:33:48 1986" 'ubin/ucadr.sym.322'
hcuot "Feb  6 23:35:38 1986" 'ubin/ucadr.tbl.322'
hcuot "Apr  9 04:19:01 1983" 'ucadr/cadldb.lisp.20'
hcuot "Jul 26 04:31:51 1983" 'ucadr/cadldb.qfasl.20'
hcuot "Jun 29 04:56:11 1982" 'ucadr/cadtlk.mid.9'
hcuot "Jun 29 04:56:32 1982" 'ucadr/chaos.test.1'
hcuot "Jun 29 04:56:46 1982" 'ucadr/dcfu.text.23'
hcuot "Dec 22 00:46:28 1982" 'ucadr/dcfu.uload.3'
hcuot "Jun 29 04:59:34 1982" 'ucadr/memd.lisp.26'
hcuot "Jun 29 04:59:39 1982" 'ucadr/mmtest.lisp.15'
hcuot "Jun 29 04:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1'
hcuot "Oct 14 18:41:23 1983" 'ucadr/packed.lisp.124'
hcuot "Jun 29 05:00:13 1982" 'ucadr/praid.lisp.21'
hcuot "Jun 29 05:00:18 1982" 'ucadr/promh.text.9'
hcuot "Oct  6 05:49:21 1984" 'ucadr/uc-arith.lisp.34'
hcuot "Jun 16 19:36:02 1984" 'ucadr/uc-array.lisp.63'
hcuot "Mar 31 16:16:21 1983" 'ucadr/uc-array-cache.lisp.1'
hcuot "Jun  1 21:53:36 1984" 'ucadr/uc-cadr.lisp.8'
hcuot "Feb 11 23:46:46 1985" 'ucadr/uc-call-return.lisp.109'
hcuot "Oct 11 02:19:04 1982" 'ucadr/uc-chaos.lisp.1'
hcuot "Nov 14 04:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'
hcuot "Nov 14 02:21:19 1983" 'ucadr/uc-disk.lisp.2'
hcuot "Jun 12 20:12:27 1985" 'ucadr/uc-fctns.lisp.85'
hcuot "Oct 17 11:11:57 1983" 'ucadr/uc-hacks.lisp.5'
hcuot "Dec 10 03:51:17 1984" 'ucadr/uc-interrupt.lisp.9'
hcuot "Mar  2 22:56:48 1984" 'ucadr/uc-logical.lisp.8'
hcuot "Jul  2 05:39:55 1984" 'ucadr/uc-macrocode.lisp.29'
hcuot "Nov 13 20:47:25 1983" 'ucadr/uc-mc.lisp.2'
hcuot "Aug  1 03:39:57 1983" 'ucadr/uc-meter.lisp.5'
hcuot "Nov 21 03:24:14 1983" 'ucadr/uc-page-fault.lisp.13'
hcuot "Dec 10 02:16:18 1984" 'ucadr/uc-parameters.lisp.230'
hcuot "Oct 11 02:18:51 1982" 'ucadr/uc-pup.lisp.1'
hcuot "Feb 11 23:47:05 1985" 'ucadr/uc-stack-closure.lisp.12'
hcuot "Jul 23 05:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'
hcuot "May 18 22:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'
hcuot "Sep  6 14:03:25 1984" 'ucadr/uc-string.lisp.26'
hcuot "Apr  3 05:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'
hcuot "May  1 03:20:21 1984" 'ucadr/uc-transporter.lisp.23'
hcuot "Apr  4 20:29:11 1984" 'ucadr/uc-tv.lisp.5'
hcuot "Jun 29 05:00:58 1982" 'ucadr/ucadlr.text.746'
hcuot "Nov 13 19:30:39 1983" 'ucadr/ucode.lisp.19'
hcuot "Aug 24 23:24:02 1982" 'wind/baswin.text.7'
hcuot "Aug  8 20:24:49 1983" 'wind/blink.text.21'
hcuot "Jan 22 03:23:05 1984" 'wind/choice.text.95'
hcuot "Jul 22 22:52:44 1983" 'wind/edges.text.14'
hcuot "Aug 24 23:25:39 1982" 'wind/emack.fasl.1'
hcuot "Apr  7 08:53:58 1984" 'wind/emack.lisp.37'
hcuot "Jul  5 22:46:40 1983" 'wind/fonts.text.17'
hcuot "Aug  8 20:21:25 1983" 'wind/frames.text.14'
hcuot "Aug  7 23:44:03 1983" 'wind/grafix.text.24'
hcuot "Sep 30 03:33:21 1983" 'wind/input.text.26'
hcuot "Apr  7 08:55:43 1984" 'wind/lstfla.lisp.6'
hcuot "Aug  8 00:15:43 1983" 'wind/margin.text.20'
hcuot "Aug 23 09:42:25 1983" 'wind/misc.text.24'
hcuot "Aug  8 05:24:58 1983" 'wind/mouse.text.33'
hcuot "Aug 24 23:27:06 1982" 'wind/operat.bolio.1'
hcuot "Aug 24 23:26:46 1982" 'wind/operat.text.45'
hcuot "Aug 24 23:27:12 1982" 'wind/outlin.text.2'
hcuot "Oct 28 21:54:29 1983" 'wind/output.text.28'
hcuot "Nov 18 19:30:08 1983" 'wind/select.text.22'
hcuot "Aug  8 04:42:49 1983" 'wind/tscrol.text.37'
hcuot "Jul  5 23:25:11 1983" 'wind/typout.text.17'
hcuot "Feb  4 05:57:38 1984" 'wind/windo1.text.52'
hcuot "Jul  3 00:44:16 1983" 'wind/windoc.bolio.14'
hcuot "Jun 21 00:34:00 1983" 'wind/windoc.dict.1'
hcuot "Aug  8 21:12:27 1983" 'wind/windoc.log.12'
hcuot "Aug  8 21:10:00 1983" 'wind/windoc.text.15'
hcuot "Aug  8 21:23:20 1983" 'wind/windoc.vars.33'
hcuot "Aug 24 23:30:20 1982" 'wind/window.gloss.1'
hcuot "Aug 24 23:30:32 1982" 'wind/window.manual.1'
hcuot "Aug 24 23:30:42 1982" 'wind/window.methds.1'
hcuot "Aug 24 23:30:47 1982" 'wind/winman.text.1'
hcuot "Dec  7 05:20:04 1984" 'window/basstr.lisp.373'
hcuot "Sep  7 13:42:22 1984" 'window/basstr.qfasl.372'
hcuot "Sep  6 15:24:14 1984" 'window/baswin.lisp.562'
hcuot "Jul 18 15:24:35 1986" 'window/baswin.lisp.563'
hcuot "Sep  7 13:37:06 1984" 'window/baswin.qfasl.562'
hcuot "Aug  4 20:39:30 1984" 'window/choice.lisp.116'
hcuot "Sep  7 17:32:54 1984" 'window/choice.qfasl.116'
hcuot "Aug 28 18:50:25 1984" 'window/cold.lisp.129'
hcuot "Aug 28 20:53:22 1984" 'window/cold.qfasl.129'
hcuot "Oct 14 15:56:06 1984" 'window/color.lisp.69'
hcuot "Aug 29 18:54:42 1984" 'window/color.qfasl.67'
hcuot "Aug  4 06:29:09 1983" 'window/cometh.lisp.26'
hcuot "Aug  3 16:17:13 1984" 'window/cometh.qfasl.26'
hcuot "Feb  2 19:32:16 1985" 'window/csrpos.lisp.10'
hcuot "Aug  3 16:53:14 1984" 'window/csrpos.qfasl.9'
hcuot "Dec 13 18:47:42 1984" 'window/fed.lisp.200'
hcuot "Sep  7 18:51:09 1984" 'window/fed.qfasl.199'
hcuot "Apr 10 20:28:15 1984" 'window/frame.lisp.165'
hcuot "Sep  7 18:59:41 1984" 'window/frame.qfasl.165'
hcuot "Jun  3 21:03:15 1984" 'window/graphics.lisp.1'
hcuot "Aug  3 16:06:31 1984" 'window/graphics.qfasl.1'
hcuot "Jan 30 10:30:23 1985" 'window/inspct.lisp.159'
hcuot "Sep  7 17:26:22 1984" 'window/inspct.qfasl.154'
hcuot "Oct 20 14:40:52 1984" 'window/menu.lisp.105'
hcuot "Sep  7 12:43:14 1984" 'window/menu.qfasl.104'
hcuot "Oct 10 23:37:53 1984" 'window/mouse.lisp.248'
hcuot "Aug  3 06:23:18 1984" 'window/mouse.qfasl.247'
hcuot "Sep  7 12:30:52 1984" 'window/peek.lisp.153'
hcuot "Sep  7 12:50:42 1984" 'window/peek.qfasl.153'
hcuot "May 24 20:14:17 1984" 'window/peekch.lisp.27'
hcuot "Sep  7 12:30:42 1984" 'window/peekfs.lisp.10'
hcuot "Sep  7 13:08:30 1984" 'window/peekfs.qfasl.10'
hcuot "Apr  7 08:56:29 1984" 'window/quest.lisp.43'
hcuot "Sep 11 15:05:34 1984" 'window/rh.lisp.162'
hcuot "Sep 11 15:34:52 1984" 'window/rh.qfasl.162'
hcuot "Oct  9 06:48:09 1984" 'window/scred.lisp.112'
hcuot "Aug  3 16:27:31 1984" 'window/scred.qfasl.111'
hcuot "Dec 14 00:43:19 1984" 'window/scrman.lisp.166'
hcuot "Aug  3 05:54:11 1984" 'window/scrman.qfasl.165'
hcuot "Aug  4 20:39:39 1984" 'window/scroll.lisp.176'
hcuot "Aug  4 20:45:11 1984" 'window/scroll.qfasl.176'
hcuot "Dec  7 00:53:21 1984" 'window/sheet.lisp.558'
hcuot "Aug  3 05:56:23 1984" 'window/sheet.qfasl.557'
hcuot "Mar 11 22:08:36 1985" 'window/shwarm.lisp.334'
hcuot "Sep  7 13:31:11 1984" 'window/shwarm.qfasl.328'
hcuot "Sep  8 13:06:44 1984" 'window/stream.lisp.145'
hcuot "Sep  8 23:51:21 1984" 'window/stream.qfasl.145'
hcuot "Jul  4 21:33:56 1984" 'window/supdup.lisp.276'
hcuot "Apr 28 13:42:05 1987" 'window/supdup.lisp.277'
hcuot "Aug  3 17:14:36 1984" 'window/supdup.qfasl.276'
hcuot "Oct 11 03:55:04 1984" 'window/sysmen.lisp.178'
hcuot "Aug  3 16:22:16 1984" 'window/sysmen.qfasl.177'
hcuot "Jun 29 03:51:17 1982" 'window/task.list.1'
hcuot "Sep  5 23:00:41 1984" 'window/telnet-code.lisp.6'
hcuot "Sep  1 00:28:56 1984" 'window/telnet-front-hack.lisp.1'
hcuot "Nov  5 12:22:00 1985" 'window/tscrol.lisp.75'
hcuot "Jul 29 20:54:26 1984" 'window/tscrol.qfasl.72'
hcuot "Jan 28 06:05:54 1985" 'window/tvdefs.lisp.286'
hcuot "Aug 29 03:10:22 1984" 'window/tvdefs.qfasl.284'
hcuot "May  1 17:22:28 1984" 'window/typwin.lisp.118'
hcuot "Sep  7 17:40:15 1984" 'window/typwin.qfasl.118'
hcuot "Dec 11 02:01:17 1984" 'window/wholin.lisp.92'
hcuot "Sep  4 15:02:13 1984" 'window/wholin.qfasl.90'
hcuot "Dec  9 17:26:17 1983" 'window/winddoc.lisp.2'
hcuot "Mar 30 03:11:15 1987" 'zmail/bug.idx.1'
hcuot "Apr 12 18:45:40 1985" 'zmail/bug.zmail1.1'
hcuot "Jul 13 01:17:38 1984" 'zmail/button.lisp.24'
hcuot "Sep  9 14:58:54 1984" 'zmail/button.qfasl.24'
hcuot "Apr  7 08:57:16 1984" 'zmail/cometh.lisp.51'
hcuot "Sep  9 15:11:39 1984" 'zmail/cometh.qfasl.51'
hcuot "Oct 14 05:23:33 1984" 'zmail/comnds.lisp.583'
hcuot "Sep 10 01:59:56 1984" 'zmail/comnds.qfasl.581'
hcuot "Mar 16 20:11:04 1985" 'zmail/defs.lisp.274'
hcuot "Sep  9 13:19:35 1984" 'zmail/defs.qfasl.273'
hcuot "Sep 25 01:29:51 1984" 'zmail/filter.lisp.356'
hcuot "Sep 10 02:16:57 1984" 'zmail/filter.qfasl.355'
hcuot "Jun 29 05:22:50 1982" 'zmail/info.mail.1'
hcuot "Apr 30 09:49:02 1984" 'zmail/lex733.lisp.14'
hcuot "Sep  9 23:59:29 1984" 'zmail/lex733.qfasl.1'
hcuot "Apr  7 08:57:50 1984" 'zmail/lm.lisp.4'
hcuot "Apr  7 08:58:15 1984" 'zmail/lmcsrv.lisp.5'
hcuot "Jul 13 01:23:12 1984" 'zmail/lmfile.lisp.5'
hcuot "Sep  9 14:30:09 1984" 'zmail/lmfile.qfasl.5'
hcuot "Mar 16 20:10:55 1985" 'zmail/mail.lisp.312'
hcuot "Sep 10 02:07:59 1984" 'zmail/mail.qfasl.311'
hcuot "Nov 30 00:35:32 1984" 'zmail/mfhost.lisp.59'
hcuot "Sep  9 14:25:31 1984" 'zmail/mfhost.qfasl.58'
hcuot "Sep  9 17:58:00 1984" 'zmail/mfiles.lisp.324'
hcuot "Sep 10 01:49:43 1984" 'zmail/mfiles.qfasl.324'
hcuot "Jul 13 01:19:28 1984" 'zmail/mult.lisp.25'
hcuot "Sep  9 14:57:38 1984" 'zmail/mult.qfasl.25'
hcuot "Dec 10 17:37:49 1983" 'zmail/parse.lisp.52'
hcuot "Nov 15 05:02:07 1983" 'zmail/patch.directory.13'
hcuot "Mar 24 04:39:39 1985" 'zmail/patch-53.directory.55'
hcuot "Dec  7 06:43:52 1983" 'zmail/patch-53-1.qfasl.2'
hcuot "Jan 30 00:21:26 1984" 'zmail/patch-53-10.lisp.1'
hcuot "Jan 30 00:21:32 1984" 'zmail/patch-53-10.qfasl.1'
hcuot "Feb 16 01:57:45 1984" 'zmail/patch-53-11.lisp.2'
hcuot "Feb 16 01:57:48 1984" 'zmail/patch-53-11.qfasl.2'
hcuot "Feb 23 07:40:40 1984" 'zmail/patch-53-12.lisp.2'
hcuot "Feb 23 07:40:45 1984" 'zmail/patch-53-12.qfasl.2'
hcuot "Mar  4 02:41:33 1984" 'zmail/patch-53-13.lisp.1'
hcuot "Mar  4 02:41:37 1984" 'zmail/patch-53-13.qfasl.1'
hcuot "Mar 24 11:24:31 1984" 'zmail/patch-53-14.lisp.2'
hcuot "Mar 24 11:24:35 1984" 'zmail/patch-53-14.qfasl.2'
hcuot "Apr 11 00:05:23 1984" 'zmail/patch-53-15.lisp.3'
hcuot "Apr 11 00:05:32 1984" 'zmail/patch-53-15.qfasl.3'
hcuot "Apr 18 02:41:32 1984" 'zmail/patch-53-16.lisp.1'
hcuot "Apr 18 02:41:38 1984" 'zmail/patch-53-16.qfasl.1'
hcuot "Apr 21 17:46:53 1984" 'zmail/patch-53-17.lisp.2'
hcuot "Apr 21 17:47:01 1984" 'zmail/patch-53-17.qfasl.2'
hcuot "Jun 28 22:21:13 1984" 'zmail/patch-53-18.lisp.1'
hcuot "Jun 29 02:53:32 1984" 'zmail/patch-53-18.qfasl.1'
hcuot "Oct 14 05:57:28 1984" 'zmail/patch-53-19.lisp.1'
hcuot "Oct 14 05:57:55 1984" 'zmail/patch-53-19.qfasl.1'
hcuot "Dec  5 23:18:26 1983" 'zmail/patch-53-2.lisp.1'
hcuot "Dec  5 23:18:36 1983" 'zmail/patch-53-2.qfasl.1'
hcuot "Mar 18 13:53:14 1985" 'zmail/patch-53-20.lisp.1'
hcuot "Mar 18 13:53:32 1985" 'zmail/patch-53-20.qfasl.1'
hcuot "Mar 24 04:39:04 1985" 'zmail/patch-53-21.lisp.1'
hcuot "Mar 24 04:39:07 1985" 'zmail/patch-53-21.qfasl.1'
hcuot "Dec 13 00:15:17 1983" 'zmail/patch-53-3.lisp.2'
hcuot "Dec 13 00:15:23 1983" 'zmail/patch-53-3.qfasl.2'
hcuot "Dec 14 02:54:56 1983" 'zmail/patch-53-5.lisp.1'
hcuot "Dec 14 02:55:02 1983" 'zmail/patch-53-5.qfasl.1'
hcuot "Jan  3 12:55:45 1984" 'zmail/patch-53-6.lisp.2'
hcuot "Jan  3 12:55:54 1984" 'zmail/patch-53-6.qfasl.2'
hcuot "Dec 31 19:08:53 1983" 'zmail/patch-53-7.lisp.3'
hcuot "Dec 31 19:09:00 1983" 'zmail/patch-53-7.qfasl.3'
hcuot "Jan  1 09:59:26 1984" 'zmail/patch-53-8.lisp.3'
hcuot "Jan  1 09:59:30 1984" 'zmail/patch-53-8.qfasl.3'
hcuot "Jan  1 10:00:18 1984" 'zmail/patch-53-9.lisp.2'
hcuot "Jan  1 10:00:22 1984" 'zmail/patch-53-9.qfasl.2'
hcuot "Jun 29 05:28:11 1982" 'zmail/poop.text.35'
hcuot "Sep 11 00:21:26 1984" 'zmail/profil.lisp.124'
hcuot "Sep 11 00:21:59 1984" 'zmail/profil.qfasl.124'
hcuot "Jul 13 01:22:56 1984" 'zmail/refer.lisp.7'
hcuot "Sep  9 14:29:01 1984" 'zmail/refer.qfasl.7'
hcuot "Jul 13 01:16:29 1984" 'zmail/rfc733.lisp.57'
hcuot "Sep  9 15:03:17 1984" 'zmail/rfc733.qfasl.57'
hcuot "Sep 26 06:37:36 1984" 'zmail/top.lisp.555'
hcuot "Sep 10 01:45:42 1984" 'zmail/top.qfasl.554'
hcuot "Sep  9 17:58:28 1984" 'zmail/window.lisp.343'
hcuot "Sep 10 02:13:55 1984" 'zmail/window.qfasl.343'
hcuot "Dec 14 00:00:40 1984" 'zmail/manual/manual.text.1'
hcuot "Jun  8 03:14:17 1983" 'zmail/manual/top.txt.1'
hcuot "Jun 29 05:04:18 1982" 'zwei/.comnd.text.1'
hcuot "Jun 29 05:04:27 1982" 'zwei/atsign.xfile.1'
hcuot "Aug  4 22:08:03 1984" 'zwei/bdired.qfasl.41'
hcuot "Jan 27 13:35:00 1983" 'zwei/bug.bugs7.1'
hcuot "Nov 16 03:01:12 1986" 'zwei/bug.idx.1'
hcuot "Oct  8 05:11:11 1983" 'zwei/bug-zwei.text.1'
hcuot "Jun 29 05:04:29 1982" 'zwei/bugs.bugs.1'
hcuot "Jun 29 05:05:20 1982" 'zwei/bugs.bugs6.1'
hcuot "Jun 29 05:05:52 1982" 'zwei/bugs.status.1'
hcuot "Aug  3 18:18:12 1984" 'zwei/coma.qfasl.102'
hcuot "Aug  3 18:20:56 1984" 'zwei/comb.qfasl.94'
hcuot "Sep  8 23:47:20 1984" 'zwei/comc.qfasl.204'
hcuot "Sep  7 16:43:25 1984" 'zwei/comd.qfasl.167'
hcuot "Aug  3 18:28:48 1984" 'zwei/come.qfasl.133'
hcuot "Sep  8 23:48:56 1984" 'zwei/comf.qfasl.99'
hcuot "Aug 29 03:32:06 1984" 'zwei/comg.qfasl.40'
hcuot "Aug  4 18:04:47 1984" 'zwei/comh.qfasl.13'
hcuot "Aug  4 21:58:57 1984" 'zwei/coms.qfasl.85'
hcuot "Sep  7 16:39:57 1984" 'zwei/comtab.qfasl.317'
hcuot "Sep 11 15:19:07 1984" 'zwei/defs.qfasl.155'
hcuot "Sep  9 23:16:32 1985" 'zwei/dired.lisp.309'
hcuot "Aug 29 03:33:34 1984" 'zwei/dired.qfasl.304'
hcuot "Sep 10 01:58:18 1985" 'zwei/displa.lisp.159'
hcuot "Sep  7 16:46:25 1984" 'zwei/displa.qfasl.157'
hcuot "Sep 10 02:08:05 1985" 'zwei/doc.lisp.77'
hcuot "Aug  4 22:09:10 1984" 'zwei/doc.qfasl.74'
hcuot "Jun 29 05:10:53 1982" 'zwei/emacs.comdif.1'
hcuot "Apr  7 09:05:03 1984" 'zwei/fasupd.lisp.31'
hcuot "Aug  4 22:10:49 1984" 'zwei/fasupd.qfasl.31'
hcuot "May 17 01:22:10 1985" 'zwei/files.lisp.198'
hcuot "Aug  4 22:11:25 1984" 'zwei/files.qfasl.195'
hcuot "May 21 18:58:47 1984" 'zwei/font.lisp.88'
hcuot "Aug  3 18:11:45 1984" 'zwei/font.qfasl.88'
hcuot "Aug  4 20:39:16 1984" 'zwei/for.lisp.62'
hcuot "Aug  4 21:53:34 1984" 'zwei/for.qfasl.62'
hcuot "Mar 27 04:35:18 1985" 'zwei/grind.definition.1'
hcuot "Jan 29 04:47:23 1985" 'zwei/history.lisp.18'
hcuot "Sep 11 15:33:45 1984" 'zwei/history.qfasl.16'
hcuot "Dec 22 04:01:04 1983" 'zwei/host.lisp.20'
hcuot "Aug  4 22:15:39 1984" 'zwei/host.qfasl.20'
hcuot "Feb 15 01:54:51 1985" 'zwei/indent.lisp.107'
hcuot "Aug  3 17:57:17 1984" 'zwei/indent.qfasl.104'
hcuot "Jan 16 15:21:27 1984" 'zwei/info.zwei.1'
hcuot "Nov  4 23:31:43 1984" 'zwei/insert.lisp.35'
hcuot "Aug  3 17:59:24 1984" 'zwei/insert.qfasl.33'
hcuot "Jul  8 12:10:53 1984" 'zwei/ispell.lisp.41'
hcuot "Aug  4 22:16:46 1984" 'zwei/ispell.qfasl.41'
hcuot "Sep  5 18:19:24 1984" 'zwei/kbdmac.lisp.48'
hcuot "Sep  7 17:06:51 1984" 'zwei/kbdmac.qfasl.48'
hcuot "Dec 24 02:43:28 1983" 'zwei/lparse.lisp.31'
hcuot "Aug  4 22:17:46 1984" 'zwei/lparse.qfasl.31'
hcuot "Mar 24 09:33:02 1985" 'zwei/macros.lisp.150'
hcuot "Sep  7 16:33:54 1984" 'zwei/macros.qfasl.147'
hcuot "Jan 30 06:03:17 1985" 'zwei/meth.lisp.49'
hcuot "Aug  3 18:00:57 1984" 'zwei/meth.qfasl.48'
hcuot "Sep  7 16:36:22 1984" 'zwei/modes.qfasl.138'
hcuot "Mar  2 08:56:03 1985" 'zwei/mouse.lisp.98'
hcuot "Aug  4 22:22:39 1984" 'zwei/mouse.qfasl.96'
hcuot "Jul  3 17:23:20 1984" 'zwei/nprim.lisp.34'
hcuot "Aug  3 18:08:03 1984" 'zwei/nprim.qfasl.34'
hcuot "Feb 14 23:39:26 1985" 'zwei/pated.lisp.33'
hcuot "Aug  4 22:24:35 1984" 'zwei/pated.qfasl.25'
hcuot "Apr  7 09:06:39 1984" 'zwei/pl1mod.lisp.14'
hcuot "Aug  4 22:25:49 1984" 'zwei/pl1mod.qfasl.14'
hcuot "Dec  1 14:23:59 1984" 'zwei/poss.lisp.90'
hcuot "Aug  4 22:31:53 1984" 'zwei/poss.qfasl.87'
hcuot "Sep 26 06:38:14 1984" 'zwei/primit.lisp.175'
hcuot "Aug  4 21:55:55 1984" 'zwei/primit.qfasl.174'
hcuot "Mar 11 22:13:17 1985" 'zwei/screen.lisp.468'
hcuot "Sep  7 16:54:21 1984" 'zwei/screen.qfasl.466'
hcuot "Jul 29 20:57:19 1984" 'zwei/search.lisp.86'
hcuot "Sep  7 16:30:55 1984" 'zwei/search.qfasl.86'
hcuot "Feb 15 02:02:40 1985" 'zwei/sectio.lisp.273'
hcuot "Aug  4 22:40:42 1984" 'zwei/sectio.qfasl.266'
hcuot "Sep  6 18:35:06 1984" 'zwei/stream.lisp.168'
hcuot "Sep  7 16:50:12 1984" 'zwei/stream.qfasl.168'
hcuot "Jan 19 21:45:33 1983" 'zwei/teach-zmacs.text.2'
hcuot "Apr 13 04:52:55 1985" 'zwei/zmacs.lisp.522'
hcuot "Sep  7 17:01:03 1984" 'zwei/zmacs.qfasl.518'
hcuot "Jan 31 18:19:56 1985" 'zwei/zmnew.lisp.36'
hcuot "Sep 10 01:42:59 1984" 'zwei/zmnew.qfasl.35'
hcuot "Apr  7 09:07:07 1984" 'zwei/zymurg.lisp.42'
hcuot "Sep  7 11:25:02 1984" 'zwei/zymurg.qfasl.42'

# tid/3299

hcuot "Nov 14 12:08:03 1984" 'patch/system-99-11.lisp.14'	     # Author: SAZ
hcuot "Nov 14 12:08:43 1984" 'patch/system-99-11.qfasl.14'	     # Author: SAZ
hcuot "Nov 29 13:04:07 1984" 'patch/system-99-12.lisp.43'	     # Author: MLY
hcuot "Dec  2 01:02:57 1984" 'patch/system-99-12.lisp.44'	     # Author: MLY
hcuot "Dec  2 01:18:35 1984" 'patch/system-99-12.qfasl.44'	     # Author: MLY
hcuot "Dec  2 08:01:06 1984" 'patch/system-99-13.lisp.16'	     # Author: MLY
hcuot "Dec  4 19:45:55 1984" 'patch/system-99-13.lisp.17'	     # Author: MLY
hcuot "Dec  2 08:01:22 1984" 'patch/system-99-13.qfasl.16'	     # Author: MLY
hcuot "Dec  4 19:46:23 1984" 'patch/system-99-13.qfasl.17'	     # Author: MLY
hcuot "Dec 12 11:38:52 1984" 'patch/system-99-14.lisp.23'	     # Author: MLY
hcuot "Dec 14 13:48:14 1984" 'patch/system-99-14.lisp.25'	     # Author: MLY
hcuot "Dec 12 15:44:02 1984" 'patch/system-99-14.qfasl.23'	     # Author: ELISHA
hcuot "Dec 14 13:48:35 1984" 'patch/system-99-14.qfasl.25'	     # Author: MLY
hcuot "Dec 14 14:16:24 1984" 'patch/system-99-15.lisp.6'	     # Author: MLY
hcuot "Dec 14 15:09:41 1984" 'patch/system-99-15.lisp.7'	     # Author: MLY
hcuot "Dec 14 14:16:46 1984" 'patch/system-99-15.qfasl.6'	     # Author: MLY
hcuot "Dec 14 15:59:42 1984" 'patch/system-99-15.qfasl.7'	     # Author: MLY
hcuot "Dec  9 22:24:38 1984" 'patch/system-99-16.lisp.1'	     # Author: PAO
hcuot "Dec 10 05:17:50 1984" 'patch/system-99-16.lisp.2'	     # Author: MLY
hcuot "Dec 10 05:17:57 1984" 'patch/system-99-16.qfasl.2'	     # Author: MLY
hcuot "Dec 14 13:08:43 1984" 'patch/system-99-17.lisp.6'	     # Author: MLY
hcuot "Dec 14 15:50:03 1984" 'patch/system-99-17.lisp.7'	     # Author: MLY
hcuot "Dec 16 07:24:51 1984" 'patch/system-99-17.qfasl.7'	     # Author: MLY
hcuot "Sep 12 17:50:00 1984" 'patch/system-99-2.lisp.1'		     # Author: RMS
hcuot "Sep 12 18:28:00 1984" 'patch/system-99-2.lisp.2'		     # Author: RMS
hcuot "Sep 12 18:28:11 1984" 'patch/system-99-2.qfasl.2'	     # Author: RMS
hcuot "Sep 13 22:51:26 1984" 'patch/system-99-3.lisp.4'		     # Author: RMS
hcuot "Sep 15 02:40:56 1984" 'patch/system-99-3.lisp.5'		     # Author: RG
hcuot "Sep 15 02:41:07 1984" 'patch/system-99-3.qfasl.5'	     # Author: RG
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.6'		     # Author: MLY
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.7'		     # Author: KHS
hcuot "Sep 26 15:23:50 1984" 'patch/system-99-4.qfasl.6'	     # Author: MLY
hcuot "Sep 26 16:02:44 1984" 'patch/system-99-5.lisp.9'		     # Author: MLY
hcuot "Sep 26 19:13:08 1984" 'patch/system-99-5.lisp.10'	     # Author: MLY
hcuot "Sep 26 19:13:19 1984" 'patch/system-99-5.qfasl.10'	     # Author: MLY
hcuot "Sep 26 23:57:10 1984" 'patch/system-99-6.lisp.2'		     # Author: RMS
hcuot "Sep 29 13:04:03 1984" 'patch/system-99-6.lisp.3'		     # Author: LISPM
hcuot "Sep 29 13:04:07 1984" 'patch/system-99-6.qfasl.3'	     # Author: LISPM
hcuot "Oct 16 07:25:45 1984" 'patch/system-99-7.lisp.10'	     # Author: TIM
hcuot "Oct 16 07:26:00 1984" 'patch/system-99-7.qfasl.10'	     # Author: TIM
hcuot "Oct 16 13:48:46 1984" 'patch/system-99-8.lisp.9'		     # Author: MLY
hcuot "Oct 16 13:49:03 1984" 'patch/system-99-8.qfasl.9'	     # Author: MLY
hcuot "Oct 23 23:36:34 1984" 'patch/system-99-9.lisp.16'	     # Author: ELISHA
hcuot "Oct 23 23:37:01 1984" 'patch/system-99-9.qfasl.16'	     # Author: ELISHA
hcuot "Sep  9 23:45:44 1984" 'patch/zmail.patch-directory.2'	     # Author: RMS
hcuot "Sep  9 23:46:32 1984" 'patch/zmail.patch-directory.3'	     # Author: RMS
hcuot "Nov 30 06:35:59 1984" 'patch/zmail-54.patch-directory.7'	     # Author: RMS
hcuot "Nov 30 06:36:51 1984" 'patch/zmail-54.patch-directory.8'	     # Author: RMS
hcuot "Sep 26 07:22:03 1984" 'patch/zmail-54-1.lisp.2'		     # Author: MLY
hcuot "Sep 26 07:22:09 1984" 'patch/zmail-54-1.qfasl.2'		     # Author: MLY
hcuot "Oct 14 10:12:26 1984" 'patch/zmail-54-2.lisp.1'		     # Author: LISPM
hcuot "Oct 14 10:12:30 1984" 'patch/zmail-54-2.qfasl.1'		     # Author: LISPM
hcuot "Nov 30 06:36:29 1984" 'patch/zmail-54-3.lisp.1'		     # Author: RMS
hcuot "Nov 30 06:36:35 1984" 'patch/zmail-54-3.qfasl.1'		     # Author: RMS
hcuot "Nov 13 04:40:20 1984" 'site/-read-.-me-.1'		     # Author: MLY
hcuot "Jan 10 19:02:06 1985" 'site/hsttbl.lisp.124'		     # Author: ANONYMOUS
hcuot "Jan 10 18:40:04 1985" 'site/lmlocs.lisp.159'		     # Author: ANONYMOUS
hcuot "Jan 10 18:44:03 1985" 'site/site.lisp.1'			     # Author: ANONYMOUS
hcuot "Jun 29 08:32:33 1982" 'sys/-read-.-this-.1'		     # Author: RMS
hcuot "Nov 12 09:18:17 1983" 'sys/cadrlp.lisp.147'		     # Author: RMS
hcuot "Feb  5 06:14:08 1984" 'sys/cadrlp.lisp.148'		     # Author: MLY
hcuot "Sep  9 03:39:01 1984" 'sys/cadrlp.lisp.149'		     # Author: RMS
hcuot "Dec 11 12:29:53 1984" 'sys/cadrlp.lisp.152'		     # Author: MLY
hcuot "Dec 11 14:29:30 1984" 'sys/cadrlp.qfasl.152'		     # Author: MLY
hcuot "Jun 29 08:33:09 1982" 'sys/cadsym.lisp.24'		     # Author: RMS
hcuot "Apr  7 15:44:05 1984" 'sys/cadsym.lisp.25'		     # Author: MLY
hcuot "Jul 31 18:51:23 1982" 'sys/cdmp.lisp.47'			     # Author: DDM
hcuot "Jun 16 21:47:26 1984" 'sys/cdmp.lisp.52'			     # Author: MLY
hcuot "Sep  8 23:46:19 1984" 'sys/cdmp.qfasl.52'		     # Author: RMS
hcuot "Aug  1 02:43:23 1984" 'sys/clpack.lisp.142'		     # Author: MLY
hcuot "Sep 10 06:15:02 1984" 'sys/clpack.lisp.151'		     # Author: RMS
hcuot "Sep 12 17:49:30 1984" 'sys/clpack.lisp.152'		     # Author: RMS
hcuot "Sep 10 07:09:12 1984" 'sys/clpack.qfasl.151'		     # Author: RMS
hcuot "Aug 15 18:29:02 1983" 'sys/compat.lisp.30'		     # Author: RMS
hcuot "Apr  7 15:45:18 1984" 'sys/compat.lisp.32'		     # Author: MLY
hcuot "Jul 30 18:15:20 1984" 'sys/eval.lisp.69'			     # Author: MLY
hcuot "Sep  9 04:13:21 1984" 'sys/eval.lisp.78'			     # Author: RMS
hcuot "Oct 17 13:03:39 1984" 'sys/eval.lisp.83'			     # Author: MLY
hcuot "Nov 28 11:02:29 1984" 'sys/eval.lisp.88'			     # Author: MLY
hcuot "Dec 14 12:13:36 1984" 'sys/eval.lisp.90'			     # Author: MLY
hcuot "Sep  9 05:39:13 1984" 'sys/eval.qfasl.78'		     # Author: RMS
hcuot "Jul 12 04:46:05 1984" 'sys/genric.lisp.24'		     # Author: MLY
hcuot "Aug 29 05:16:10 1984" 'sys/genric.lisp.27'		     # Author: RMS
hcuot "Oct 14 06:44:02 1984" 'sys/genric.lisp.28'		     # Author: TIM
hcuot "Dec 14 13:08:04 1984" 'sys/genric.lisp.29'		     # Author: MLY
hcuot "Aug 29 07:59:43 1984" 'sys/genric.qfasl.27'		     # Author: RMS
hcuot "Oct 27 23:53:09 1983" 'sys/ltop.lisp.436'		     # Author: RMS
hcuot "Jun 29 12:33:08 1984" 'sys/ltop.lisp.487'		     # Author: MLY
hcuot "Oct  8 20:58:59 1984" 'sys/ltop.lisp.496'		     # Author: MLY
hcuot "Dec 14 05:21:28 1984" 'sys/ltop.lisp.497'		     # Author: MLY
hcuot "Sep 11 07:20:26 1984" 'sys/ltop.qfasl.494'		     # Author: RMS
hcuot "Oct 24 03:41:44 1982" 'sys/ma.lisp.305'			     # Author: RMS
hcuot "Aug  1 22:39:34 1984" 'sys/ma.qfasl.305'			     # Author: MLY
hcuot "Jun 29 08:36:30 1982" 'sys/madefs.lisp.7'		     # Author: RMS
hcuot "Jul 30 03:18:23 1984" 'sys/madefs.qfasl.7'		     # Author: MLY
hcuot "Oct 13 00:33:14 1983" 'sys/maopt.lisp.4'			     # Author: ALR
hcuot "Aug  1 22:44:12 1984" 'sys/maopt.qfasl.4'		     # Author: MLY
hcuot "Oct 25 04:08:13 1983" 'sys/mc.lisp.353'			     # Author: RMS
hcuot "Nov 16 09:21:33 1983" 'sys/mc.lisp.354'			     # Author: RMS
hcuot "Aug  1 22:45:44 1984" 'sys/mc.qfasl.354'			     # Author: MLY
hcuot "Jan  4 00:08:41 1983" 'sys/mlap.lisp.51'			     # Author: RMS
hcuot "Aug  1 22:48:15 1984" 'sys/mlap.qfasl.51'		     # Author: MLY
hcuot "Jun 25 23:40:50 1983" 'sys/pack4.lisp.286'		     # Author: RMS
hcuot "Dec 16 20:32:14 1983" 'sys/qcdefs.lisp.128'		     # Author: RMS
hcuot "Sep  8 23:30:46 1984" 'sys/qcdefs.lisp.149'		     # Author: RMS
hcuot "Oct 17 13:21:56 1984" 'sys/qcdefs.lisp.152'		     # Author: MLY
hcuot "Sep  9 20:00:57 1984" 'sys/qcdefs.qfasl.149'		     # Author: RMS
hcuot "Oct 19 19:57:43 1983" 'sys/qcfasd.lisp.229'		     # Author: RMS
hcuot "Sep 10 23:05:15 1984" 'sys/qcfasd.lisp.248'		     # Author: RMS
hcuot "Sep 10 23:05:43 1984" 'sys/qcfasd.qfasl.248'		     # Author: RMS
hcuot "Mar 28 03:58:18 1984" 'sys/qcfile.lisp.307'		     # Author: MLY
hcuot "Aug 15 03:18:22 1984" 'sys/qcfile.lisp.321'		     # Author: RPK
hcuot "Oct 29 04:42:33 1984" 'sys/qcfile.lisp.323'		     # Author: MLY
hcuot "Sep  7 02:12:10 1984" 'sys/qcfile.qfasl.322'		     # Author: RMS
hcuot "Jun 13 07:50:37 1984" 'sys/qclap.lisp.236'		     # Author: MLY
hcuot "Sep  8 23:30:16 1984" 'sys/qclap.lisp.244'		     # Author: RMS
hcuot "Sep  9 20:16:55 1984" 'sys/qclap.qfasl.244'		     # Author: RMS
hcuot "Apr 26 07:24:10 1984" 'sys/qcluke.lisp.23'		     # Author: MLY
hcuot "Aug 30 13:51:41 1984" 'sys/qcluke.lisp.26'		     # Author: MLY
hcuot "Aug 30 20:38:31 1984" 'sys/qcluke.qfasl.26'		     # Author: MLY
hcuot "Nov 29 23:18:23 1983" 'sys/qcopt.lisp.95'		     # Author: RMS
hcuot "Aug  2 05:15:22 1984" 'sys/qcopt.lisp.126'		     # Author: MLY
hcuot "Oct 24 07:49:36 1984" 'sys/qcopt.lisp.136'		     # Author: ELISHA
hcuot "Nov  6 13:41:16 1984" 'sys/qcopt.lisp.137'		     # Author: MLY
hcuot "Sep  9 20:13:24 1984" 'sys/qcopt.qfasl.133'		     # Author: RMS
hcuot "Jul  5 03:23:04 1984" 'sys/qcp1.lisp.547'		     # Author: MLY
hcuot "Aug 28 06:30:17 1984" 'sys/qcp1.lisp.550'		     # Author: RMS
hcuot "Oct 28 21:42:02 1984" 'sys/qcp1.lisp.569'		     # Author: RMS
hcuot "Nov  2 09:40:55 1984" 'sys/qcp1.lisp.572'		     # Author: MLY
hcuot "Dec 11 19:38:25 1984" 'sys/qcp1.lisp.573'		     # Author: MLY
hcuot "Sep  9 20:03:10 1984" 'sys/qcp1.qfasl.562'		     # Author: RMS
hcuot "Jun 17 14:30:49 1984" 'sys/qcp2.lisp.246'		     # Author: RG
hcuot "Aug 28 06:18:42 1984" 'sys/qcp2.lisp.252'		     # Author: RMS
hcuot "Oct 28 21:41:46 1984" 'sys/qcp2.lisp.261'		     # Author: RMS
hcuot "Sep  9 20:09:07 1984" 'sys/qcp2.qfasl.259'		     # Author: RMS
hcuot "Aug 17 17:51:51 1983" 'sys/qcpeep.lisp.31'		     # Author: RMS
hcuot "Feb 17 10:46:04 1984" 'sys/qcpeep.lisp.34'		     # Author: MLY
hcuot "Aug  3 03:31:47 1984" 'sys/qcpeep.lisp.36'		     # Author: MLY
hcuot "Aug  3 03:31:56 1984" 'sys/qcpeep.qfasl.36'		     # Author: MLY
hcuot "Jan 11 21:27:40 1984" 'sys/qev.lisp.289'			     # Author: MLY
hcuot "Aug 19 20:43:32 1983" 'sys/qfasl.lisp.432'		     # Author: RMS
hcuot "Jun 26 20:57:53 1984" 'sys/qfasl.lisp.459'		     # Author: MLY
hcuot "Aug 15 03:17:24 1984" 'sys/qfasl.lisp.461'		     # Author: RPK
hcuot "Oct 29 04:42:43 1984" 'sys/qfasl.lisp.462'		     # Author: MLY
hcuot "Aug 15 05:35:35 1984" 'sys/qfasl.qfasl.461'		     # Author: RPK
hcuot "Jun 12 07:51:12 1984" 'sys/qfctns.lisp.753'		     # Author: MLY
hcuot "Aug 31 17:55:02 1984" 'sys/qfctns.lisp.769'		     # Author: RMS
hcuot "Oct 17 12:24:56 1984" 'sys/qfctns.lisp.770'		     # Author: MLY
hcuot "Nov 17 01:51:17 1984" 'sys/qfctns.lisp.774'		     # Author: MLY
hcuot "Aug 31 17:59:24 1984" 'sys/qfctns.qfasl.769'		     # Author: RMS
hcuot "Nov 28 17:40:06 1983" 'sys/qmisc.lisp.590'		     # Author: RMS
hcuot "Sep 26 12:38:57 1984" 'sys/qmisc.lisp.655'		     # Author: MLY
hcuot "Oct 29 04:38:09 1984" 'sys/qmisc.lisp.658'		     # Author: MLY
hcuot "Dec 14 08:04:10 1984" 'sys/qmisc.lisp.659'		     # Author: MLY
hcuot "Aug 31 22:20:48 1984" 'sys/qmisc.qfasl.652'		     # Author: RMS
hcuot "Jun 21 18:36:00 1983" 'sys/qnew.lisp.19'			     # Author: RMS
hcuot "Apr  3 15:55:26 1984" 'sys/qnew.lisp.20'			     # Author: MLY
hcuot "Aug 15 05:54:18 1984" 'sys/qnew.qfasl.20'		     # Author: MLY
hcuot "Jun 17 06:31:08 1984" 'sys/qrand.lisp.395'		     # Author: MLY
hcuot "Sep  4 23:43:16 1984" 'sys/qrand.lisp.408'		     # Author: RMS
hcuot "Dec  1 20:28:06 1984" 'sys/qrand.lisp.411'		     # Author: MLY
hcuot "Sep  4 23:45:01 1984" 'sys/qrand.qfasl.408'		     # Author: RMS
hcuot "Oct 26 06:10:35 1983" 'sys/qwmcr.lisp.20'		     # Author: RMS
hcuot "Dec 11 14:56:51 1984" 'sys/qwmcr.lisp.22'		     # Author: MLY
hcuot "Dec 11 14:56:58 1984" 'sys/qwmcr.qfasl.22'		     # Author: MLY
hcuot "Jun 18 08:57:37 1983" 'sys/recom.lisp.33'		     # Author: RMS
hcuot "Aug 27 08:28:30 1982" 'sys/sgfctn.lisp.57'		     # Author: RMS
hcuot "Aug 15 06:13:05 1984" 'sys/sgfctn.qfasl.57'		     # Author: MLY
hcuot "Oct 10 07:20:38 1983" 'sys/sort.lisp.59'			     # Author: RMS
hcuot "Aug 15 06:13:45 1984" 'sys/sort.qfasl.59'		     # Author: MLY
hcuot "Sep 25 07:05:36 1984" 'sys/sysdcl.lisp.186'		     # Author: MLY
hcuot "Oct 18 19:56:06 1984" 'sys/sysdcl.qfasl.186'		     # Author: RAMESH
hcuot "Jun 12 08:06:02 1984" 'sys/types.lisp.48'		     # Author: NGL
hcuot "Sep 25 07:28:40 1984" 'sys/types.lisp.70'		     # Author: MLY
hcuot "Dec  8 09:02:13 1984" 'sys/types.lisp.71'		     # Author: MLY
hcuot "Sep  9 03:39:45 1984" 'sys/types.qfasl.69'		     # Author: RMS
hcuot "Jun 29 08:49:58 1982" 'sys/ucinit.qfasl.1'		     # Author: RMS
hcuot "May 26 00:21:06 1984" 'sys2/advise.lisp.35'		     # Author: MLY
hcuot "May 29 18:44:47 1984" 'sys2/advise.lisp.37'		     # Author: RMS
hcuot "Aug 15 03:43:06 1984" 'sys2/advise.qfasl.37'		     # Author: MLY
hcuot "Oct  6 17:50:44 1983" 'sys2/analyze.lisp.16'		     # Author: RMS
hcuot "Sep 26 09:58:58 1984" 'sys2/analyze.lisp.18'		     # Author: MLY
hcuot "Sep 11 07:03:49 1984" 'sys2/analyze.qfasl.17'		     # Author: RMS
hcuot "Aug 22 00:06:13 1983" 'sys2/band.lisp.43'		     # Author: RMS
hcuot "Jul 27 08:09:35 1984" 'sys2/band.lisp.44'		     # Author: MLY
hcuot "Nov 24 08:31:51 1984" 'sys2/band.qfasl.46'		     # Author: LMFILE
hcuot "Jul 30 03:02:02 1984" 'sys2/character.lisp.16'		     # Author: MLY
hcuot "Sep 25 07:22:45 1984" 'sys2/character.lisp.21'		     # Author: MLY
hcuot "Sep  7 22:04:06 1984" 'sys2/character.qfasl.20'		     # Author: RMS
hcuot "Nov 23 09:48:18 1983" 'sys2/class.lisp.88'		     # Author: RMS
hcuot "Jun 15 05:56:23 1984" 'sys2/class.lisp.99'		     # Author: RMS
hcuot "Sep  4 21:31:45 1984" 'sys2/class.qfasl.99'		     # Author: MLY
hcuot "May 15 15:48:48 1984" 'sys2/clmac.lisp.3'		     # Author: MLY
hcuot "Aug 24 10:12:36 1984" 'sys2/clmac.lisp.4'		     # Author: MLY
hcuot "Aug 29 03:55:00 1984" 'sys2/clmac.qfasl.4'		     # Author: RMS
hcuot "Apr  7 15:50:06 1984" 'sys2/cmany.lisp.46'		     # Author: MLY
hcuot "Apr  7 15:50:53 1984" 'sys2/condit.lisp.2'		     # Author: MLY
hcuot "Feb  8 13:06:51 1984" 'sys2/defmac.lisp.74'		     # Author: MLY
hcuot "May 16 22:43:32 1984" 'sys2/defmac.lisp.75'		     # Author: MLY
hcuot "Oct 29 04:37:08 1984" 'sys2/defmac.lisp.79'		     # Author: ELISHA
hcuot "Aug 29 21:55:29 1984" 'sys2/defmac.qfasl.78'		     # Author: RMS
hcuot "Jun 26 20:57:47 1984" 'sys2/defsel.lisp.69'		     # Author: MLY
hcuot "Aug 29 02:45:38 1984" 'sys2/defsel.lisp.70'		     # Author: RMS
hcuot "Aug 29 07:26:03 1984" 'sys2/defsel.qfasl.70'		     # Author: RMS
hcuot "Nov 17 12:07:18 1983" 'sys2/disass.lisp.90'		     # Author: RMS
hcuot "Dec 14 13:05:57 1984" 'sys2/disass.lisp.94'		     # Author: MLY
hcuot "Aug  1 22:38:35 1984" 'sys2/disass.qfasl.92'		     # Author: MLY
hcuot "Mar 22 07:07:02 1984" 'sys2/eh.lisp.320'			     # Author: MLY
hcuot "Jun 17 04:54:46 1984" 'sys2/eh.lisp.331'			     # Author: MLY
hcuot "Sep 12 18:24:24 1984" 'sys2/eh.lisp.337'			     # Author: RMS
hcuot "Nov  9 10:44:53 1984" 'sys2/eh.lisp.338'			     # Author: MLY
hcuot "Dec  2 03:04:05 1984" 'sys2/eh.lisp.339'			     # Author: MLY
hcuot "Sep  9 05:53:06 1984" 'sys2/eh.qfasl.336'		     # Author: RMS
hcuot "May 15 14:03:23 1984" 'sys2/ehc.lisp.223'		     # Author: MLY
hcuot "Jun  6 06:08:49 1984" 'sys2/ehc.lisp.226'		     # Author: MLY
hcuot "Nov  9 11:09:25 1984" 'sys2/ehc.lisp.234'		     # Author: MLY
hcuot "Dec  8 07:07:26 1984" 'sys2/ehc.lisp.235'		     # Author: MLY
hcuot "Sep  7 22:04:40 1984" 'sys2/ehc.qfasl.233'		     # Author: RMS
hcuot "Jun  9 13:25:10 1984" 'sys2/ehf.lisp.207'		     # Author: MLY
hcuot "Sep  5 23:49:56 1984" 'sys2/ehf.lisp.224'		     # Author: RMS
hcuot "Oct 14 10:01:23 1984" 'sys2/ehf.lisp.226'		     # Author: LISPM
hcuot "Nov  9 10:52:43 1984" 'sys2/ehf.lisp.227'		     # Author: MLY
hcuot "Sep 11 21:22:50 1984" 'sys2/ehf.qfasl.225'		     # Author: LISPM
hcuot "Nov  9 11:13:07 1984" 'sys2/ehsys.lisp.1'		     # Author: MLY
hcuot "Mar 22 11:38:18 1984" 'sys2/ehw.lisp.108'		     # Author: MLY
hcuot "May 16 12:21:59 1984" 'sys2/ehw.lisp.109'		     # Author: MLY
hcuot "Sep  8 01:04:14 1984" 'sys2/ehw.qfasl.109'		     # Author: RMS
hcuot "Oct  9 08:05:36 1983" 'sys2/encaps.lisp.19'		     # Author: RMS
hcuot "Jul 30 01:28:00 1984" 'sys2/encaps.lisp.27'		     # Author: MLY
hcuot "Nov 28 15:50:50 1984" 'sys2/encaps.lisp.28'		     # Author: MLY
hcuot "Aug 15 04:10:54 1984" 'sys2/encaps.qfasl.27'		     # Author: MLY
hcuot "Jul 30 18:54:40 1984" 'sys2/flavor.lisp.272'		     # Author: MLY
hcuot "Sep 10 23:44:11 1984" 'sys2/flavor.lisp.280'		     # Author: RMS
hcuot "Oct 13 10:45:00 1984" 'sys2/flavor.lisp.282'		     # Author: MLY
hcuot "Sep 11 05:12:37 1984" 'sys2/flavor.qfasl.280'		     # Author: RMS
hcuot "Apr 29 00:39:18 1984" 'sys2/gc.lisp.165'			     # Author: RMS
hcuot "Oct 14 11:12:49 1984" 'sys2/gc.lisp.170'			     # Author: MLY
hcuot "Dec  6 09:07:27 1984" 'sys2/gc.lisp.173'			     # Author: MLY
hcuot "Dec 14 09:01:18 1984" 'sys2/gc.lisp.174'			     # Author: MLY
hcuot "Aug 15 04:34:41 1984" 'sys2/gc.qfasl.169'		     # Author: MLY
hcuot "Jun  5 01:39:39 1984" 'sys2/hash.lisp.83'		     # Author: MLY
hcuot "Oct 13 11:53:10 1984" 'sys2/hash.lisp.88'		     # Author: MLY
hcuot "Aug 15 04:51:05 1984" 'sys2/hash.qfasl.87'		     # Author: MLY
hcuot "Apr 16 21:35:12 1984" 'sys2/hashfl.lisp.24'		     # Author: MLY
hcuot "Jun 14 01:02:46 1984" 'sys2/hashfl.lisp.28'		     # Author: MERMAN.JAN
hcuot "Oct 24 03:53:53 1984" 'sys2/hashfl.lisp.31'		     # Author: ELISHA
hcuot "Aug 15 04:52:39 1984" 'sys2/hashfl.qfasl.29'		     # Author: MLY
hcuot "Apr  7 15:51:47 1984" 'sys2/let.lisp.8'			     # Author: MLY
hcuot "Jun  4 08:44:01 1984" 'sys2/lmmac.lisp.356'		     # Author: RMS
hcuot "Aug 31 20:13:38 1984" 'sys2/lmmac.lisp.372'		     # Author: RMS
hcuot "Oct 30 01:55:36 1984" 'sys2/lmmac.lisp.378'		     # Author: RMS
hcuot "Nov 29 17:06:17 1984" 'sys2/lmmac.lisp.382'		     # Author: MLY
hcuot "Aug 31 21:54:02 1984" 'sys2/lmmac.qfasl.372'		     # Author: RMS
hcuot "May  9 12:09:16 1984" 'sys2/login.lisp.83'		     # Author: MLY
hcuot "Aug  2 19:51:26 1984" 'sys2/login.lisp.85'		     # Author: MLY
hcuot "Sep  4 02:04:30 1984" 'sys2/login.lisp.87'		     # Author: RMS
hcuot "Sep  4 03:06:25 1984" 'sys2/login.qfasl.87'		     # Author: RMS
hcuot "Nov 30 01:23:16 1983" 'sys2/loop.lisp.795'		     # Author: RMS
hcuot "Jun 20 18:08:26 1984" 'sys2/loop.lisp.798'		     # Author: MERMAN.JAN
hcuot "Oct 23 23:14:32 1984" 'sys2/loop.lisp.799'		     # Author: ELISHA
hcuot "Dec  9 06:37:37 1984" 'sys2/loop.lisp.829'		     # Author: MLY
hcuot "Oct 24 08:03:31 1984" 'sys2/loop.qfasl.799'		     # Author: ELISHA
hcuot "Jan 25 07:41:37 1984" 'sys2/maksys.lisp.174'		     # Author: RMS
hcuot "Jun 16 20:18:39 1984" 'sys2/maksys.lisp.176'		     # Author: MLY
hcuot "Sep 13 22:53:32 1984" 'sys2/maksys.lisp.180'		     # Author: RMS
hcuot "Sep  4 22:16:36 1984" 'sys2/maksys.qfasl.178'		     # Author: RMS
hcuot "Mar 14 19:02:00 1984" 'sys2/matrix.lisp.23'		     # Author: MLY
hcuot "Apr  9 18:08:41 1984" 'sys2/matrix.lisp.26'		     # Author: KAB
hcuot "Aug 30 03:20:25 1984" 'sys2/matrix.qfasl.26'		     # Author: RMS
hcuot "Jun 17 06:43:45 1984" 'sys2/meth.lisp.61'		     # Author: MLY
hcuot "Sep  4 21:35:26 1984" 'sys2/meth.lisp.63'		     # Author: RMS
hcuot "Sep  4 21:35:37 1984" 'sys2/meth.qfasl.63'		     # Author: RMS
hcuot "Jun 26 20:55:29 1984" 'sys2/numdef.lisp.10'		     # Author: MLY
hcuot "Oct  6 11:43:45 1984" 'sys2/numdef.lisp.12'		     # Author: MLY
hcuot "Sep 10 22:30:29 1984" 'sys2/numdef.qfasl.11'		     # Author: RMS
hcuot "Mar 13 22:55:49 1984" 'sys2/numer.lisp.43'		     # Author: RMS
hcuot "Jul  2 21:42:29 1984" 'sys2/numer.lisp.58'		     # Author: MERMAN.JAN
hcuot "Oct  6 11:43:26 1984" 'sys2/numer.lisp.61'		     # Author: MLY
hcuot "Dec 14 08:58:42 1984" 'sys2/numer.lisp.62'		     # Author: MLY
hcuot "Sep 10 22:32:20 1984" 'sys2/numer.qfasl.60'		     # Author: RMS
hcuot "Mar 22 04:17:42 1984" 'sys2/patch.lisp.150'		     # Author: MLY
hcuot "Jul 26 13:25:55 1984" 'sys2/patch.lisp.158'		     # Author: MLY
hcuot "Oct 20 20:21:29 1984" 'sys2/patch.lisp.165'		     # Author: MLY
hcuot "Aug 15 05:22:21 1984" 'sys2/patch.qfasl.158'		     # Author: MLY
hcuot "Dec  6 01:27:36 1984" 'sys2/photo.log.1'			     # Author: MLY
hcuot "Aug 30 00:33:36 1984" 'sys2/plane.lisp.32'		     # Author: RMS
hcuot "Aug 30 00:45:45 1984" 'sys2/plane.qfasl.32'		     # Author: RMS
hcuot "Feb 17 11:36:34 1984" 'sys2/proces.lisp.153'		     # Author: MLY
hcuot "Nov  9 21:05:07 1984" 'sys2/proces.lisp.158'		     # Author: MLY
hcuot "Aug 15 05:28:32 1984" 'sys2/proces.qfasl.157'		     # Author: MLY
hcuot "May 13 02:30:56 1984" 'sys2/prodef.lisp.40'		     # Author: MLY
hcuot "Aug 31 19:58:32 1984" 'sys2/prodef.lisp.48'		     # Author: MLY
hcuot "Aug 31 19:59:54 1984" 'sys2/prodef.qfasl.48'		     # Author: RMS
hcuot "Apr 22 04:59:26 1984" 'sys2/qtrace.lisp.149'		     # Author: RMS
hcuot "Dec  6 01:41:34 1984" 'sys2/qtrace.lisp.152'		     # Author: MLY
hcuot "Sep  4 21:43:34 1984" 'sys2/qtrace.qfasl.151'		     # Author: RMS
hcuot "Jun  4 03:02:01 1984" 'sys2/rat.lisp.38'			     # Author: MLY
hcuot "Sep  4 22:08:45 1984" 'sys2/rat.lisp.46'			     # Author: RMS
hcuot "Sep 10 22:47:13 1984" 'sys2/rat.qfasl.46'		     # Author: RMS
hcuot "Aug  6 12:26:38 1983" 'sys2/resour.lisp.17'		     # Author: RMS
hcuot "Jun 24 06:09:43 1984" 'sys2/resour.lisp.28'		     # Author: MLY
hcuot "Nov 10 09:26:41 1984" 'sys2/resour.lisp.31'		     # Author: MLY
hcuot "Aug 15 06:08:50 1984" 'sys2/resour.qfasl.28'		     # Author: MLY
hcuot "Feb  8 14:52:48 1984" 'sys2/selev.lisp.21'		     # Author: MLY
hcuot "Aug 27 10:43:29 1984" 'sys2/selev.lisp.23'		     # Author: MLY
hcuot "Aug 29 03:54:21 1984" 'sys2/selev.qfasl.23'		     # Author: RMS
hcuot "May 20 08:10:47 1984" 'sys2/setf.lisp.86'		     # Author: RMS
hcuot "Jun 17 06:31:52 1984" 'sys2/setf.lisp.91'		     # Author: MLY
hcuot "Oct 29 09:56:33 1984" 'sys2/setf.lisp.97'		     # Author: ELISHA
hcuot "Aug 31 18:48:05 1984" 'sys2/setf.qfasl.95'		     # Author: RMS
hcuot "Jan 12 14:29:47 1984" 'sys2/sgdefs.lisp.54'		     # Author: DANIEL.G.MLY
hcuot "Dec  2 09:03:49 1984" 'sys2/sgdefs.lisp.55'		     # Author: MLY
hcuot "Aug 15 03:40:43 1984" 'sys2/sgdefs.qfasl.54'		     # Author: MLY
hcuot "Jun 17 06:30:55 1984" 'sys2/step.lisp.67'		     # Author: MLY
hcuot "Sep 26 08:01:17 1984" 'sys2/step.lisp.72'		     # Author: MLY
hcuot "Aug 15 06:15:30 1984" 'sys2/step.qfasl.70'		     # Author: RPK
hcuot "Apr  6 10:08:15 1984" 'sys2/string.lisp.130'		     # Author: RMS
hcuot "Jul 31 23:41:47 1984" 'sys2/string.lisp.141'		     # Author: MLY
hcuot "Sep 25 07:20:50 1984" 'sys2/string.lisp.147'		     # Author: MLY
hcuot "Sep 10 07:15:39 1984" 'sys2/string.qfasl.146'		     # Author: RMS
hcuot "Nov 29 05:45:39 1983" 'sys2/struct.lisp.292'		     # Author: RMS
hcuot "May 18 06:09:04 1984" 'sys2/struct.lisp.311'		     # Author: RMS
hcuot "Jul 31 23:42:25 1984" 'sys2/struct.lisp.322'		     # Author: MLY
hcuot "Aug 14 22:20:04 1984" 'sys2/struct.qfasl.322'		     # Author: MLY
hcuot "Aug  3 08:04:28 1983" 'sys2/unfasl.lisp.16'		     # Author: RMS
hcuot "Oct  9 13:52:31 1984" 'sys2/unfasl.lisp.19'		     # Author: MLY
hcuot "Sep 11 07:52:22 1984" 'sys2/unfasl.qfasl.18'		     # Author: RMS
hcuot "Oct 22 14:37:42 1983" 'sys2/usymld.lisp.183'		     # Author: DANIEL.G.MLY
hcuot "Sep  9 03:39:09 1984" 'sys2/usymld.lisp.187'		     # Author: RMS
hcuot "Sep  9 03:49:15 1984" 'sys2/usymld.qfasl.187'		     # Author: RMS
hcuot "Jan  3 08:50:32 1984" 'tape/copy.lisp.128'		     # Author: RPK
hcuot "Feb 16 13:56:19 1984" 'tape/copy.lisp.133'		     # Author: LMFILE
hcuot "Jan  3 09:50:47 1984" 'tape/copy.qfasl.128'		     # Author: RPK
hcuot "Jan 19 16:25:49 1984" 'tape/ddoc.text.4'			     # Author: RPK
hcuot "May 12 05:49:07 1984" 'tape/ddoc.text.8'			     # Author: RPK
hcuot "Jan 19 16:25:57 1984" 'tape/fdump.lisp.18'		     # Author: RPK
hcuot "Feb 16 13:56:28 1984" 'tape/fdump.lisp.24'		     # Author: LMFILE
hcuot "May 12 05:29:43 1984" 'tape/fdump.lisp.27'		     # Author: RPK
hcuot "Jan 10 04:56:50 1984" 'tape/fdump-def.lisp.5'		     # Author: RPK
hcuot "Feb 16 13:56:36 1984" 'tape/fdump-def.lisp.8'		     # Author: LMFILE
hcuot "May 12 05:52:14 1984" 'tape/fdump-def.lisp.12'		     # Author: RPK
hcuot "Jan  2 09:37:10 1984" 'tape/fdump-def.qfasl.1'		     # Author: RPK
hcuot "Jan 10 03:54:15 1984" 'tape/fdump-file-cdate-i.lisp.1'	     # Author: RPK
hcuot "Jan 10 04:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'	     # Author: RPK
hcuot "Jan 19 16:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'	     # Author: RPK
hcuot "Feb 16 13:56:44 1984" 'tape/fdump-r.lisp.4'		     # Author: LMFILE
hcuot "May 12 05:29:45 1984" 'tape/fdump-r.lisp.5'		     # Author: RPK
hcuot "Jan  3 10:36:11 1984" 'tape/magtape.directory.11'	     # Author: RPK
hcuot "Oct 26 20:41:54 1983" 'tape/magtape-14.directory.14'	     # Author: RPK
hcuot "Mar  8 06:56:47 1983" 'tape/magtape-14-1.qfasl.1'	     # Author: RPK
hcuot "Apr 25 09:51:48 1983" 'tape/magtape-14-3.qfasl.1'	     # Author: RPK
hcuot "May 19 04:11:34 1983" 'tape/magtape-14-4.qfasl.3'	     # Author: RPK
hcuot "Oct 26 20:41:16 1983" 'tape/magtape-14-5.qfasl.1'	     # Author: RPK
hcuot "Feb 16 14:24:04 1984" 'tape/magtape-22.directory.13'	     # Author: LMFILE
hcuot "Jan  7 22:40:45 1984" 'tape/magtape-22-1.lisp.1'		     # Author: RMS
hcuot "Jan  7 22:40:56 1984" 'tape/magtape-22-1.qfasl.1'	     # Author: RMS
hcuot "Jan  7 23:28:27 1984" 'tape/magtape-22-2.lisp.1'		     # Author: RMS
hcuot "Jan  7 23:28:40 1984" 'tape/magtape-22-2.qfasl.1'	     # Author: RMS
hcuot "Jan  8 00:41:18 1984" 'tape/magtape-22-3.lisp.1'		     # Author: RMS
hcuot "Jan  8 00:41:44 1984" 'tape/magtape-22-3.qfasl.1'	     # Author: RMS
hcuot "Jan 13 13:06:26 1984" 'tape/magtape-22-4.lisp.1'		     # Author: GJC
hcuot "Jan 13 13:06:35 1984" 'tape/magtape-22-4.qfasl.1'	     # Author: GJC
hcuot "Jan 19 17:40:22 1984" 'tape/magtape-22-5.lisp.1'		     # Author: RPK
hcuot "Jan 19 17:40:32 1984" 'tape/magtape-22-5.qfasl.1'	     # Author: RPK
hcuot "Feb 16 14:23:22 1984" 'tape/magtape-22-6.lisp.1'		     # Author: LMFILE
hcuot "Feb 16 14:23:28 1984" 'tape/magtape-22-6.qfasl.1'	     # Author: LMFILE
hcuot "Jan 13 12:25:26 1984" 'tape/mtaux.lisp.79'		     # Author: GJC
hcuot "Jan 19 17:04:02 1984" 'tape/mtaux.lisp.80'		     # Author: RPK
hcuot "Jan  3 09:52:48 1984" 'tape/mtaux.qfasl.77'		     # Author: RPK
hcuot "Jun 20 06:21:53 1983" 'tape/mtdefs.lisp.28'		     # Author: RPK
hcuot "Dec 16 15:34:10 1983" 'tape/mtdefs.lisp.30'		     # Author: RPK
hcuot "Jan  3 09:46:18 1984" 'tape/mtdefs.qfasl.30'		     # Author: RPK
hcuot "Jan  7 23:43:06 1984" 'tape/mtstr.lisp.86'		     # Author: RMS
hcuot "Jan 11 05:40:52 1984" 'tape/mtstr.lisp.87'		     # Author: RPK
hcuot "Jan  3 09:47:58 1984" 'tape/mtstr.qfasl.85'		     # Author: RPK
hcuot "Jan  3 08:50:55 1984" 'tape/odump.lisp.1'		     # Author: RPK
hcuot "Jan  3 10:33:05 1984" 'tape/odump.qfasl.1'		     # Author: RPK
hcuot "May 12 05:29:46 1984" 'tape/package.lisp.1'		     # Author: RPK
hcuot "Jan  3 07:59:49 1984" 'tape/pdp10.lisp.1'		     # Author: RPK
hcuot "May 12 08:31:18 1984" 'tape/rmunit.lisp.3'		     # Author: RPK
hcuot "May 12 05:29:46 1984" 'tape/system.lisp.3'		     # Author: RPK
hcuot "Jan  3 04:42:55 1984" 'tape/tm.lisp.23'			     # Author: RMS
hcuot "May 12 05:29:47 1984" 'tape/tm.lisp.25'			     # Author: RPK
hcuot "Jan  3 04:43:02 1984" 'tape/tmdefs.lisp.6'		     # Author: RMS
hcuot "May 12 05:29:48 1984" 'tape/tmdefs.lisp.7'		     # Author: RPK
hcuot "May 12 07:27:24 1984" 'tape/unit.lisp.7'			     # Author: MLY
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.1'			     # Author: RPK
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.2'			     # Author: MLY
hcuot "May 12 05:29:49 1984" 'tape/new/mtdefs.lisp.3'		     # Author: RPK
hcuot "May 12 07:28:11 1984" 'tape/new/mtdefs.lisp.4'		     # Author: MLY
hcuot "May 12 07:45:03 1984" 'tape/new/mtdefs.qfasl.4'		     # Author: RPK
hcuot "Jan 11 06:38:45 1984" 'tape/new/mtrqb.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:49 1984" 'tape/new/mtrqb.lisp.3'		     # Author: RPK
hcuot "May 12 05:29:50 1984" 'tape/new/mtstr.lisp.4'		     # Author: RPK
hcuot "May 12 08:31:35 1984" 'tape/new/mtstr.lisp.5'		     # Author: RPK
hcuot "Jan 19 16:27:21 1984" 'tape/new/tmunit.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:50 1984" 'tape/new/tmunit.lisp.5'		     # Author: RPK
hcuot "Jan 11 06:38:10 1984" 'tape/new/weunit.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:51 1984" 'tape/new/weunit.lisp.3'		     # Author: RPK
hcuot "Nov 20 23:29:49 1982" 'ubin/dcfu.uload.4'		     # Author: AKR
hcuot "Aug  4 07:23:05 1982" 'ubin/memd.uload.1'		     # Author: RMS
hcuot "Dec 11 12:08:01 1984" 'ubin/ucadr.loc.321'		     # Author: MLY
hcuot "May  7 01:22:05 1983" 'ubin/ucadr.locs.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:18:51 1983" 'ubin/ucadr.locs.257'		     # Author: RMS
hcuot "Mar  3 08:22:09 1984" 'ubin/ucadr.locs.309'		     # Author: RMS
hcuot "Jun 17 01:45:52 1984" 'ubin/ucadr.locs.314'		     # Author: MLY
hcuot "Sep 11 21:24:11 1984" 'ubin/ucadr.locs.320'		     # Author: RMS
hcuot "May  7 01:19:58 1983" 'ubin/ucadr.mcr.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:16:41 1983" 'ubin/ucadr.mcr.257'		     # Author: RMS
hcuot "Mar  3 08:20:07 1984" 'ubin/ucadr.mcr.309'		     # Author: RMS
hcuot "Jun 17 01:42:17 1984" 'ubin/ucadr.mcr.314'		     # Author: MLY
hcuot "Sep 11 21:21:19 1984" 'ubin/ucadr.mcr.320'		     # Author: RMS
hcuot "Dec 11 12:04:48 1984" 'ubin/ucadr.mcr.321'		     # Author: MLY
hcuot "May  7 01:20:31 1983" 'ubin/ucadr.sym.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:17:12 1983" 'ubin/ucadr.sym.257'		     # Author: RMS
hcuot "Mar  3 08:20:33 1984" 'ubin/ucadr.sym.309'		     # Author: RMS
hcuot "Jun 17 01:42:47 1984" 'ubin/ucadr.sym.314'		     # Author: MLY
hcuot "Sep 11 21:22:04 1984" 'ubin/ucadr.sym.320'		     # Author: RMS
hcuot "Dec 11 12:05:39 1984" 'ubin/ucadr.sym.321'		     # Author: MLY
hcuot "May  7 01:22:07 1983" 'ubin/ucadr.tbl.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:18:53 1983" 'ubin/ucadr.tbl.257'		     # Author: RMS
hcuot "Mar  3 08:22:10 1984" 'ubin/ucadr.tbl.309'		     # Author: RMS
hcuot "Jun 17 01:45:55 1984" 'ubin/ucadr.tbl.314'		     # Author: MLY
hcuot "Sep 11 21:24:18 1984" 'ubin/ucadr.tbl.320'		     # Author: RMS
hcuot "Dec 11 12:08:04 1984" 'ubin/ucadr.tbl.321'		     # Author: MLY
hcuot "Apr  9 11:19:01 1983" 'ucadr/cadldb.lisp.20'		     # Author: RMS
hcuot "Jul 26 10:31:51 1983" 'ucadr/cadldb.qfasl.20'		     # Author: RMS
hcuot "Jun 29 10:56:11 1982" 'ucadr/cadtlk.mid.9'		     # Author: RMS
hcuot "Jun 29 10:56:32 1982" 'ucadr/chaos.test.1'		     # Author: RMS
hcuot "Jun 29 10:56:46 1982" 'ucadr/dcfu.text.23'		     # Author: RMS
hcuot "Dec 22 06:46:28 1982" 'ucadr/dcfu.uload.3'		     # Author: AKR
hcuot "Jun 29 10:59:34 1982" 'ucadr/memd.lisp.26'		     # Author: RMS
hcuot "Jun 29 10:59:39 1982" 'ucadr/mmtest.lisp.15'		     # Author: RMS
hcuot "Jun 29 10:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1' # Author: RMS
hcuot "Nov 17 06:26:42 1982" 'ucadr/packed.lisp.119'		     # Author: RMS
hcuot "Oct 14 23:41:23 1983" 'ucadr/packed.lisp.124'		     # Author: ALR
hcuot "Jun 29 11:00:13 1982" 'ucadr/praid.lisp.21'		     # Author: RMS
hcuot "Jun 29 11:00:18 1982" 'ucadr/promh.text.9'		     # Author: RMS
hcuot "Nov 18 14:23:35 1983" 'ucadr/uc-arith.lisp.25'		     # Author: RMS
hcuot "Apr 30 23:43:08 1984" 'ucadr/uc-arith.lisp.28'		     # Author: RMS
hcuot "Jun 27 07:40:14 1984" 'ucadr/uc-arith.lisp.33'		     # Author: RMS
hcuot "Oct  6 10:49:21 1984" 'ucadr/uc-arith.lisp.34'		     # Author: RMS
hcuot "Jul 23 10:01:24 1983" 'ucadr/uc-array.lisp.28'		     # Author: NGL
hcuot "Nov 29 21:22:31 1983" 'ucadr/uc-array.lisp.59'		     # Author: RMS
hcuot "Jun 17 01:36:02 1984" 'ucadr/uc-array.lisp.63'		     # Author: MLY
hcuot "Mar 31 23:16:21 1983" 'ucadr/uc-array-cache.lisp.1'	     # Author: RMS
hcuot "Jul 29 11:02:16 1983" 'ucadr/uc-cadr.lisp.7'		     # Author: RMS
hcuot "Jun  2 03:53:36 1984" 'ucadr/uc-cadr.lisp.8'		     # Author: RMS
hcuot "Jan 27 08:37:59 1984" 'ucadr/uc-call-return.lisp.97'	     # Author: RMS
hcuot "Jul 27 04:48:47 1984" 'ucadr/uc-call-return.lisp.103'	     # Author: MLY
hcuot "Sep 11 21:15:57 1984" 'ucadr/uc-call-return.lisp.108'	     # Author: RMS
hcuot "Oct 11 07:19:04 1982" 'ucadr/uc-chaos.lisp.1'		     # Author: RG
hcuot "Oct 15 01:27:48 1982" 'ucadr/uc-cold-disk.lisp.2'	     # Author: RG
hcuot "Jun  8 11:15:36 1983" 'ucadr/uc-cold-disk.lisp.9'	     # Author: RMS
hcuot "Nov 14 10:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'	     # Author: RMS
hcuot "Oct 11 07:18:14 1982" 'ucadr/uc-disk.lisp.1'		     # Author: RG
hcuot "Nov 14 08:21:19 1983" 'ucadr/uc-disk.lisp.2'		     # Author: RMS
hcuot "Sep 13 09:27:33 1983" 'ucadr/uc-fctns.lisp.40'		     # Author: RMS
hcuot "May 11 05:41:36 1984" 'ucadr/uc-fctns.lisp.75'		     # Author: RMS
hcuot "Jun 16 21:24:03 1984" 'ucadr/uc-fctns.lisp.78'		     # Author: MLY
hcuot "Aug  3 10:30:30 1984" 'ucadr/uc-fctns.lisp.84'		     # Author: RMS
hcuot "Apr  3 12:37:59 1983" 'ucadr/uc-hacks.lisp.3'		     # Author: RMS
hcuot "Oct 17 16:11:57 1983" 'ucadr/uc-hacks.lisp.5'		     # Author: RMS
hcuot "Feb  4 09:53:01 1983" 'ucadr/uc-interrupt.lisp.4'	     # Author: RMS
hcuot "Oct 30 00:16:52 1983" 'ucadr/uc-interrupt.lisp.7'	     # Author: RMS
hcuot "Dec  1 03:01:55 1984" 'ucadr/uc-interrupt.lisp.8'	     # Author: PAO
hcuot "Dec 10 09:51:17 1984" 'ucadr/uc-interrupt.lisp.9'	     # Author: MLY
hcuot "Jul 23 11:00:06 1983" 'ucadr/uc-logical.lisp.7'		     # Author: NGL
hcuot "Mar  3 04:56:48 1984" 'ucadr/uc-logical.lisp.8'		     # Author: RMS
hcuot "Apr  4 07:09:21 1983" 'ucadr/uc-macrocode.lisp.9'	     # Author: RMS
hcuot "Nov 14 02:49:50 1983" 'ucadr/uc-macrocode.lisp.28'	     # Author: RMS
hcuot "Jul  2 11:39:55 1984" 'ucadr/uc-macrocode.lisp.29'	     # Author: MLY
hcuot "Oct 11 20:53:14 1982" 'ucadr/uc-mc.lisp.1'		     # Author: RG
hcuot "Nov 14 02:47:25 1983" 'ucadr/uc-mc.lisp.2'		     # Author: RMS
hcuot "Apr  5 09:49:33 1983" 'ucadr/uc-meter.lisp.3'		     # Author: RMS
hcuot "Jul 23 12:00:11 1983" 'ucadr/uc-meter.lisp.4'		     # Author: NGL
hcuot "Aug  1 09:39:57 1983" 'ucadr/uc-meter.lisp.5'		     # Author: RMS
hcuot "Jun  5 10:47:27 1983" 'ucadr/uc-page-fault.lisp.7'	     # Author: RMS
hcuot "Oct 17 16:11:44 1983" 'ucadr/uc-page-fault.lisp.10'	     # Author: RMS
hcuot "Nov 21 09:24:14 1983" 'ucadr/uc-page-fault.lisp.13'	     # Author: RMS
hcuot "Oct 29 22:36:55 1983" 'ucadr/uc-parameters.lisp.222'	     # Author: RMS
hcuot "Dec 28 07:16:45 1983" 'ucadr/uc-parameters.lisp.228'	     # Author: RMS
hcuot "Dec 10 08:16:18 1984" 'ucadr/uc-parameters.lisp.230'	     # Author: RMS
hcuot "Oct 11 07:18:51 1982" 'ucadr/uc-pup.lisp.1'		     # Author: RG
hcuot "Nov 16 10:34:29 1983" 'ucadr/uc-stack-closure.lisp.3'	     # Author: RMS
hcuot "Jan 23 03:33:37 1984" 'ucadr/uc-stack-closure.lisp.6'	     # Author: RMS
hcuot "Jul 27 04:49:03 1984" 'ucadr/uc-stack-closure.lisp.10'	     # Author: MLY
hcuot "Aug 31 19:43:35 1984" 'ucadr/uc-stack-closure.lisp.11'	     # Author: RMS
hcuot "Apr  5 09:49:39 1983" 'ucadr/uc-stack-groups.lisp.4'	     # Author: RMS
hcuot "Jul 23 11:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'	     # Author: NGL
hcuot "Jul 29 10:34:22 1983" 'ucadr/uc-storage-allocation.lisp.15'   # Author: RMS
hcuot "May 19 04:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'   # Author: RMS
hcuot "Jul 20 10:00:15 1983" 'ucadr/uc-string.lisp.13'		     # Author: RMS
hcuot "Jul 28 20:32:30 1984" 'ucadr/uc-string.lisp.25'		     # Author: RMS
hcuot "Sep  6 20:03:25 1984" 'ucadr/uc-string.lisp.26'		     # Author: RMS
hcuot "Apr  3 12:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'	     # Author: RMS
hcuot "Aug 15 14:07:26 1983" 'ucadr/uc-transporter.lisp.9'	     # Author: RMS
hcuot "Dec 29 05:09:16 1983" 'ucadr/uc-transporter.lisp.22'	     # Author: RMS
hcuot "May  1 09:20:21 1984" 'ucadr/uc-transporter.lisp.23'	     # Author: RMS
hcuot "Apr  4 09:20:18 1983" 'ucadr/uc-tv.lisp.3'		     # Author: RMS
hcuot "Jul 23 12:18:17 1983" 'ucadr/uc-tv.lisp.4'		     # Author: NGL
hcuot "Apr  5 03:29:11 1984" 'ucadr/uc-tv.lisp.5'		     # Author: RMS
hcuot "Jun 29 11:00:58 1982" 'ucadr/ucadlr.text.746'		     # Author: RMS
hcuot "Nov 14 01:30:39 1983" 'ucadr/ucode.lisp.19'		     # Author: RMS
hcuot "Aug 25 05:24:02 1982" 'wind/baswin.text.7'		     # Author: RMS
hcuot "Aug  9 02:24:49 1983" 'wind/blink.text.21'		     # Author: RMS
hcuot "Nov 16 05:33:30 1983" 'wind/choice.text.94'		     # Author: RMS
hcuot "Jan 22 09:23:05 1984" 'wind/choice.text.95'		     # Author: RMS
hcuot "Jul 23 04:52:44 1983" 'wind/edges.text.14'		     # Author: RMS
hcuot "Aug 25 05:25:39 1982" 'wind/emack.fasl.1'		     # Author: RMS
hcuot "Aug 25 05:25:27 1982" 'wind/emack.lisp.36'		     # Author: RMS
hcuot "Apr  7 15:53:58 1984" 'wind/emack.lisp.37'		     # Author: MLY
hcuot "Jul  6 04:46:40 1983" 'wind/fonts.text.17'		     # Author: RMS
hcuot "Aug  9 02:21:25 1983" 'wind/frames.text.14'		     # Author: RMS
hcuot "Aug  8 05:44:03 1983" 'wind/grafix.text.24'		     # Author: RMS
hcuot "Aug  8 07:11:30 1983" 'wind/input.text.24'		     # Author: RMS
hcuot "Sep 30 08:33:21 1983" 'wind/input.text.26'		     # Author: RMS
hcuot "Aug 25 05:26:36 1982" 'wind/lstfla.lisp.5'		     # Author: RMS
hcuot "Apr  7 15:55:43 1984" 'wind/lstfla.lisp.6'		     # Author: MLY
hcuot "Aug  8 06:15:43 1983" 'wind/margin.text.20'		     # Author: RMS
hcuot "Aug 23 15:42:25 1983" 'wind/misc.text.24'		     # Author: RMS
hcuot "Aug  8 11:24:58 1983" 'wind/mouse.text.33'		     # Author: RMS
hcuot "Aug 25 05:27:06 1982" 'wind/operat.bolio.1'		     # Author: RMS
hcuot "Aug 25 05:26:46 1982" 'wind/operat.text.45'		     # Author: RMS
hcuot "Aug 25 05:27:12 1982" 'wind/outlin.text.2'		     # Author: RMS
hcuot "Aug  9 04:51:45 1983" 'wind/output.text.27'		     # Author: RMS
hcuot "Oct 29 02:54:29 1983" 'wind/output.text.28'		     # Author: RMS
hcuot "Aug  8 07:11:36 1983" 'wind/select.text.21'		     # Author: RMS
hcuot "Nov 19 01:30:08 1983" 'wind/select.text.22'		     # Author: RMS
hcuot "Aug  8 10:42:49 1983" 'wind/tscrol.text.37'		     # Author: RMS
hcuot "Jul  6 05:25:11 1983" 'wind/typout.text.17'		     # Author: RMS
hcuot "Aug  9 03:11:35 1983" 'wind/windo1.text.51'		     # Author: RMS
hcuot "Feb  4 11:57:38 1984" 'wind/windo1.text.52'		     # Author: RMS
hcuot "Jul  3 06:44:16 1983" 'wind/windoc.bolio.14'		     # Author: GSB
hcuot "Jun 21 06:34:00 1983" 'wind/windoc.dict.1'		     # Author: RMS
hcuot "Aug  9 03:12:27 1983" 'wind/windoc.log.12'		     # Author: RMS
hcuot "Aug  9 03:10:00 1983" 'wind/windoc.text.15'		     # Author: RMS
hcuot "Aug  9 03:23:20 1983" 'wind/windoc.vars.33'		     # Author: RMS
hcuot "Aug 25 05:30:20 1982" 'wind/window.gloss.1'		     # Author: RMS
hcuot "Aug 25 05:30:32 1982" 'wind/window.manual.1'		     # Author: RMS
hcuot "Aug 25 05:30:42 1982" 'wind/window.methds.1'		     # Author: RMS
hcuot "Aug 25 05:30:47 1982" 'wind/winman.text.1'		     # Author: RMS
hcuot "Apr  7 20:52:51 1984" 'window/basstr.lisp.361'		     # Author: MLY
hcuot "Nov 22 07:10:00 1984" 'window/basstr.lisp.372'		     # Author: RMS
hcuot "Dec  7 11:20:04 1984" 'window/basstr.lisp.373'		     # Author: MLY
hcuot "Sep  7 19:42:22 1984" 'window/basstr.qfasl.372'		     # Author: RMS
hcuot "Apr 15 07:28:51 1984" 'window/baswin.lisp.559'		     # Author: MLY
hcuot "Jun  6 11:46:15 1984" 'window/baswin.lisp.561'		     # Author: MLY
hcuot "Sep  6 21:24:14 1984" 'window/baswin.lisp.562'		     # Author: RMS
hcuot "Sep  7 19:37:06 1984" 'window/baswin.qfasl.562'		     # Author: RMS
hcuot "Mar  3 04:18:07 1984" 'window/choice.lisp.110'		     # Author: RMS
hcuot "Apr 22 11:43:14 1984" 'window/choice.lisp.111'		     # Author: MLY
hcuot "Aug  5 02:39:30 1984" 'window/choice.lisp.116'		     # Author: MLY
hcuot "Sep  7 23:32:54 1984" 'window/choice.qfasl.116'		     # Author: MLY
hcuot "Aug  3 10:36:59 1983" 'window/cold.lisp.105'		     # Author: RMS
hcuot "Nov 10 07:11:35 1983" 'window/cold.lisp.112'		     # Author: RMS
hcuot "Jun 17 04:26:37 1984" 'window/cold.lisp.128'		     # Author: MLY
hcuot "Aug 29 00:50:25 1984" 'window/cold.lisp.129'		     # Author: RMS
hcuot "Aug 29 02:53:22 1984" 'window/cold.qfasl.129'		     # Author: RMS
hcuot "Dec 11 05:41:19 1983" 'window/color.lisp.66'		     # Author: RMS
hcuot "Oct 14 20:56:06 1984" 'window/color.lisp.69'		     # Author: MLY
hcuot "Aug 30 00:54:42 1984" 'window/color.qfasl.67'		     # Author: RMS
hcuot "Jun 29 09:12:56 1982" 'window/cometh.lisp.23'		     # Author: RMS
hcuot "Aug  4 12:29:09 1983" 'window/cometh.lisp.26'		     # Author: RMS
hcuot "Aug  3 22:17:13 1984" 'window/cometh.qfasl.26'		     # Author: MLY
hcuot "Jun 29 09:13:09 1982" 'window/csrpos.lisp.9'		     # Author: RMS
hcuot "Aug  3 22:53:14 1984" 'window/csrpos.qfasl.9'		     # Author: MLY
hcuot "Nov 23 16:24:15 1983" 'window/fed.lisp.194'		     # Author: DANIEL.G.MLY
hcuot "Feb  6 14:55:35 1984" 'window/fed.lisp.199'		     # Author: MLY
hcuot "Dec 14 00:47:42 1984" 'window/fed.lisp.200'		     # Author: MLY
hcuot "Sep  8 00:51:09 1984" 'window/fed.qfasl.199'		     # Author: RMS
hcuot "Jun 16 15:41:36 1983" 'window/frame.lisp.164'		     # Author: RMS
hcuot "Apr 11 03:28:15 1984" 'window/frame.lisp.165'		     # Author: MLY
hcuot "Sep  8 00:59:41 1984" 'window/frame.qfasl.165'		     # Author: MLY
hcuot "Jun  4 03:03:15 1984" 'window/graphics.lisp.1'		     # Author: MLY
hcuot "Aug  3 22:06:31 1984" 'window/graphics.qfasl.1'		     # Author: MLY
hcuot "Jun  6 10:01:37 1984" 'window/inspct.lisp.153'		     # Author: MLY
hcuot "Oct  8 20:57:28 1984" 'window/inspct.lisp.155'		     # Author: MLY
hcuot "Nov 28 09:32:08 1984" 'window/inspct.lisp.158'		     # Author: MLY
hcuot "Sep  7 23:26:22 1984" 'window/inspct.qfasl.154'		     # Author: MLY
hcuot "Oct 30 06:32:13 1983" 'window/menu.lisp.98'		     # Author: RMS
hcuot "Feb  5 10:43:00 1984" 'window/menu.lisp.103'		     # Author: MLY
hcuot "Sep  7 18:30:18 1984" 'window/menu.lisp.104'		     # Author: RMS
hcuot "Oct 20 19:40:52 1984" 'window/menu.lisp.105'		     # Author: MLY
hcuot "Sep  7 18:43:14 1984" 'window/menu.qfasl.104'		     # Author: RMS
hcuot "May  1 23:20:08 1984" 'window/mouse.lisp.246'		     # Author: MLY
hcuot "May 16 19:35:36 1984" 'window/mouse.lisp.247'		     # Author: MLY
hcuot "Oct 11 04:37:53 1984" 'window/mouse.lisp.248'		     # Author: MLY
hcuot "Aug  3 12:23:18 1984" 'window/mouse.qfasl.247'		     # Author: MLY
hcuot "Jul 16 15:26:31 1984" 'window/peek.lisp.147'		     # Author: RPK
hcuot "Aug  5 03:49:54 1984" 'window/peek.lisp.149'		     # Author: MLY
hcuot "Sep  7 18:30:52 1984" 'window/peek.lisp.153'		     # Author: RMS
hcuot "Sep  7 18:50:42 1984" 'window/peek.qfasl.153'		     # Author: RMS
hcuot "May 25 02:14:17 1984" 'window/peekch.lisp.27'		     # Author: RPK
hcuot "Jun 29 09:46:44 1982" 'window/peekfs.lisp.9'		     # Author: RMS
hcuot "Sep  7 18:30:42 1984" 'window/peekfs.lisp.10'		     # Author: RMS
hcuot "Sep  7 19:08:30 1984" 'window/peekfs.qfasl.10'		     # Author: RMS
hcuot "Apr  7 15:56:29 1984" 'window/quest.lisp.43'		     # Author: MLY
hcuot "Nov 30 05:28:54 1983" 'window/rh.lisp.146'		     # Author: RMS
hcuot "Apr 18 09:15:27 1984" 'window/rh.lisp.160'		     # Author: MLY
hcuot "Sep 11 21:05:34 1984" 'window/rh.lisp.162'		     # Author: CENT
hcuot "Nov 16 15:35:58 1984" 'window/rh.lisp.163'		     # Author: MLY
hcuot "Jan 12 02:41:29 1985" 'window/rh.lisp.164'		     # Author: ANONYMOUS
hcuot "Sep 11 21:34:52 1984" 'window/rh.qfasl.162'		     # Author: LISPM
hcuot "Nov 21 08:04:19 1983" 'window/scred.lisp.106'		     # Author: RMS
hcuot "May  3 19:34:28 1984" 'window/scred.lisp.107'		     # Author: RPK
hcuot "Oct  9 11:48:09 1984" 'window/scred.lisp.112'		     # Author: MLY
hcuot "Aug  3 22:27:31 1984" 'window/scred.qfasl.111'		     # Author: MLY
hcuot "Jul 25 10:19:44 1983" 'window/scrman.lisp.165'		     # Author: RMS
hcuot "Dec 14 06:43:19 1984" 'window/scrman.lisp.166'		     # Author: MLY
hcuot "Aug  3 11:54:11 1984" 'window/scrman.qfasl.165'		     # Author: MLY
hcuot "Oct  6 17:48:59 1983" 'window/scroll.lisp.175'		     # Author: RMS
hcuot "Aug  5 02:39:39 1984" 'window/scroll.lisp.176'		     # Author: MLY
hcuot "Aug  5 02:45:11 1984" 'window/scroll.qfasl.176'		     # Author: MLY
hcuot "Feb  4 00:57:09 1984" 'window/sheet.lisp.554'		     # Author: MLY
hcuot "Dec  7 06:53:21 1984" 'window/sheet.lisp.558'		     # Author: MLY
hcuot "Aug  3 11:56:23 1984" 'window/sheet.qfasl.557'		     # Author: MLY
hcuot "May 20 22:13:35 1984" 'window/shwarm.lisp.321'		     # Author: RMS
hcuot "Jul 24 08:14:29 1984" 'window/shwarm.lisp.324'		     # Author: RMS
hcuot "Sep  7 18:33:12 1984" 'window/shwarm.lisp.328'		     # Author: RMS
hcuot "Nov  1 11:35:45 1984" 'window/shwarm.lisp.332'		     # Author: MLY
hcuot "Sep  7 19:31:11 1984" 'window/shwarm.qfasl.328'		     # Author: RMS
hcuot "Nov 21 10:54:11 1983" 'window/stream.lisp.116'		     # Author: RMS
hcuot "May 27 15:38:37 1984" 'window/stream.lisp.136'		     # Author: MLY
hcuot "Aug  8 15:04:05 1984" 'window/stream.lisp.144'		     # Author: MLY
hcuot "Sep  8 19:06:44 1984" 'window/stream.lisp.145'		     # Author: RMS
hcuot "Sep  9 05:51:21 1984" 'window/stream.qfasl.145'		     # Author: RMS
hcuot "Jun 12 20:14:53 1984" 'window/supdup.lisp.272'		     # Author: RMS
hcuot "Jul  5 03:33:56 1984" 'window/supdup.lisp.276'		     # Author: MLY
hcuot "Aug  3 23:14:36 1984" 'window/supdup.qfasl.276'		     # Author: MLY
hcuot "Nov 29 05:56:40 1983" 'window/sysmen.lisp.173'		     # Author: RMS
hcuot "Mar 18 04:49:43 1984" 'window/sysmen.lisp.176'		     # Author: MLY
hcuot "Oct 11 08:55:04 1984" 'window/sysmen.lisp.178'		     # Author: MLY
hcuot "Aug  3 22:22:16 1984" 'window/sysmen.qfasl.177'		     # Author: MLY
hcuot "Jun 29 09:51:17 1982" 'window/task.list.1'		     # Author: RMS
hcuot "Sep  6 04:26:39 1984" 'window/telnet-code.lisp.5'	     # Author: GANDER
hcuot "Sep  6 05:00:41 1984" 'window/telnet-code.lisp.6'	     # Author: GANDER
hcuot "Sep  1 06:28:56 1984" 'window/telnet-front-hack.lisp.1'	     # Author: GANDER
hcuot "Dec 26 03:16:03 1983" 'window/tscrol.lisp.69'		     # Author: RMS
hcuot "Jun  6 11:47:06 1984" 'window/tscrol.lisp.70'		     # Author: MLY
hcuot "Oct 11 04:37:48 1984" 'window/tscrol.lisp.73'		     # Author: MLY
hcuot "Nov  9 20:17:36 1984" 'window/tscrol.lisp.74'		     # Author: MLY
hcuot "Jul 30 02:54:26 1984" 'window/tscrol.qfasl.72'		     # Author: MLY
hcuot "Apr  7 19:54:59 1984" 'window/tvdefs.lisp.278'		     # Author: MLY
hcuot "Jul 25 03:06:56 1984" 'window/tvdefs.lisp.283'		     # Author: MLY
hcuot "Aug 29 01:21:19 1984" 'window/tvdefs.lisp.284'		     # Author: RMS
hcuot "Nov 16 07:32:33 1984" 'window/tvdefs.lisp.285'		     # Author: MLY
hcuot "Aug 29 09:10:22 1984" 'window/tvdefs.qfasl.284'		     # Author: RMS
hcuot "Aug 20 14:24:51 1983" 'window/typwin.lisp.105'		     # Author: RMS
hcuot "May  1 23:22:28 1984" 'window/typwin.lisp.118'		     # Author: MLY
hcuot "Sep  7 23:40:15 1984" 'window/typwin.qfasl.118'		     # Author: MLY
hcuot "Jan 18 20:56:57 1984" 'window/wholin.lisp.85'		     # Author: RPK
hcuot "Jul  7 06:08:43 1984" 'window/wholin.lisp.88'		     # Author: MLY
hcuot "Dec 11 08:01:17 1984" 'window/wholin.lisp.92'		     # Author: MLY
hcuot "Sep  4 21:02:13 1984" 'window/wholin.qfasl.90'		     # Author: RMS
hcuot "Dec  9 23:26:17 1983" 'window/winddoc.lisp.2'		     # Author: DANIEL.G.MLY
hcuot "Jan 14 23:14:54 1985" 'zmail/bug.zmail.1'		     # Author: RMS
hcuot "Apr 11 03:28:22 1984" 'zmail/button.lisp.23'		     # Author: MLY
hcuot "Jul 13 07:17:38 1984" 'zmail/button.lisp.24'		     # Author: RPK
hcuot "Sep  9 20:58:54 1984" 'zmail/button.qfasl.24'		     # Author: RMS
hcuot "Apr  7 15:57:16 1984" 'zmail/cometh.lisp.51'		     # Author: MLY
hcuot "Sep  9 21:11:39 1984" 'zmail/cometh.qfasl.51'		     # Author: RMS
hcuot "Apr 22 00:16:43 1984" 'zmail/comnds.lisp.579'		     # Author: RMS
hcuot "Jul 13 07:22:36 1984" 'zmail/comnds.lisp.580'		     # Author: RPK
hcuot "Sep  9 23:58:08 1984" 'zmail/comnds.lisp.581'		     # Author: RMS
hcuot "Sep 26 12:37:47 1984" 'zmail/comnds.lisp.582'		     # Author: MLY
hcuot "Oct 14 10:23:33 1984" 'zmail/comnds.lisp.583'		     # Author: LISPM
hcuot "Sep 10 07:59:56 1984" 'zmail/comnds.qfasl.581'		     # Author: RMS
hcuot "Aug 17 20:36:10 1983" 'zmail/defs.lisp.268'		     # Author: RMS
hcuot "Apr 18 09:42:50 1984" 'zmail/defs.lisp.270'		     # Author: RPK
hcuot "Jul 13 07:20:38 1984" 'zmail/defs.lisp.272'		     # Author: RPK
hcuot "Sep  9 18:35:08 1984" 'zmail/defs.lisp.273'		     # Author: RMS
hcuot "Sep  9 19:19:35 1984" 'zmail/defs.qfasl.273'		     # Author: RMS
hcuot "Feb 23 13:39:32 1984" 'zmail/filter.lisp.350'		     # Author: MLY
hcuot "Apr 11 06:36:04 1984" 'zmail/filter.lisp.352'		     # Author: MLY
hcuot "Jul 13 07:22:09 1984" 'zmail/filter.lisp.353'		     # Author: RPK
hcuot "Sep 10 00:51:46 1984" 'zmail/filter.lisp.355'		     # Author: RMS
hcuot "Sep 25 07:29:51 1984" 'zmail/filter.lisp.356'		     # Author: MLY
hcuot "Sep 10 08:16:57 1984" 'zmail/filter.qfasl.355'		     # Author: RMS
hcuot "Jun 29 11:22:50 1982" 'zmail/info.mail.1'		     # Author: RMS
hcuot "Dec  3 14:55:46 1983" 'zmail/lex733.lisp.13'		     # Author: RMS
hcuot "Apr 30 15:49:02 1984" 'zmail/lex733.lisp.14'		     # Author: MLY
hcuot "Sep 10 05:59:29 1984" 'zmail/lex733.qfasl.1'		     # Author: RMS
hcuot "Apr  7 15:57:50 1984" 'zmail/lm.lisp.4'			     # Author: MLY
hcuot "Apr  7 15:58:15 1984" 'zmail/lmcsrv.lisp.5'		     # Author: MLY
hcuot "Jul 13 07:23:12 1984" 'zmail/lmfile.lisp.5'		     # Author: RPK
hcuot "Sep  9 20:30:09 1984" 'zmail/lmfile.qfasl.5'		     # Author: RMS
hcuot "May 16 11:06:22 1984" 'zmail/mail.lisp.308'		     # Author: RMS
hcuot "Jul 13 07:22:22 1984" 'zmail/mail.lisp.310'		     # Author: RPK
hcuot "Sep  9 23:58:19 1984" 'zmail/mail.lisp.311'		     # Author: RMS
hcuot "Sep 10 08:07:59 1984" 'zmail/mail.qfasl.311'		     # Author: RMS
hcuot "Apr  7 16:02:19 1984" 'zmail/mfhost.lisp.57'		     # Author: MLY
hcuot "Jul 13 07:23:16 1984" 'zmail/mfhost.lisp.58'		     # Author: RPK
hcuot "Nov 30 06:35:32 1984" 'zmail/mfhost.lisp.59'		     # Author: RMS
hcuot "Sep  9 20:25:31 1984" 'zmail/mfhost.qfasl.58'		     # Author: RMS
hcuot "Apr 11 06:35:11 1984" 'zmail/mfiles.lisp.322'		     # Author: MLY
hcuot "Jul 13 07:22:59 1984" 'zmail/mfiles.lisp.323'		     # Author: RPK
hcuot "Sep  9 23:58:00 1984" 'zmail/mfiles.lisp.324'		     # Author: RMS
hcuot "Sep 10 07:49:43 1984" 'zmail/mfiles.qfasl.324'		     # Author: RMS
hcuot "Apr  7 16:02:56 1984" 'zmail/mult.lisp.24'		     # Author: MLY
hcuot "Jul 13 07:19:28 1984" 'zmail/mult.lisp.25'		     # Author: RPK
hcuot "Sep  9 20:57:38 1984" 'zmail/mult.qfasl.25'		     # Author: RMS
hcuot "Dec 10 23:37:49 1983" 'zmail/parse.lisp.52'		     # Author: DANIEL.G.MLY
hcuot "Nov 15 11:02:07 1983" 'zmail/patch.directory.13'		     # Author: RMS
hcuot "Aug 23 00:25:50 1983" 'zmail/patch-51-1.lisp.1'		     # Author: RMS
hcuot "Sep  7 21:56:01 1983" 'zmail/patch-51-2.lisp.1'		     # Author: RMS
hcuot "Sep 21 23:30:38 1983" 'zmail/patch-51-3.lisp.6'		     # Author: EB.TFC
hcuot "Sep 21 23:26:54 1983" 'zmail/patch-51-4.lisp.2'		     # Author: EB.TFC
hcuot "Sep 23 08:11:23 1983" 'zmail/patch-51-5.lisp.2'		     # Author: RMS
hcuot "Sep 26 05:52:32 1983" 'zmail/patch-51-6.lisp.1'		     # Author: RMS
hcuot "Oct 14 07:56:33 1983" 'zmail/patch-51-7.lisp.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 14 07:56:39 1983" 'zmail/patch-51-7.qfasl.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 22 08:30:39 1983" 'zmail/patch-51-8.lisp.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 22 08:30:48 1983" 'zmail/patch-51-8.qfasl.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 28 07:02:36 1983" 'zmail/patch-51-9.lisp.1'		     # Author: RMS
hcuot "Oct 28 07:02:49 1983" 'zmail/patch-51-9.qfasl.1'		     # Author: RMS
hcuot "Oct 14 10:56:00 1984" 'zmail/patch-53.directory.50'	     # Author: MLY
hcuot "Oct 14 10:59:23 1984" 'zmail/patch-53.directory.51'	     # Author: MLY
hcuot "Dec  7 12:43:52 1983" 'zmail/patch-53-1.qfasl.2'		     # Author: RMS
hcuot "Jan 30 06:21:26 1984" 'zmail/patch-53-10.lisp.1'		     # Author: RPK
hcuot "Jan 30 06:21:32 1984" 'zmail/patch-53-10.qfasl.1'	     # Author: RPK
hcuot "Feb 16 07:57:45 1984" 'zmail/patch-53-11.lisp.2'		     # Author: MLY
hcuot "Feb 16 07:57:48 1984" 'zmail/patch-53-11.qfasl.2'	     # Author: MLY
hcuot "Feb 23 13:40:40 1984" 'zmail/patch-53-12.lisp.2'		     # Author: MLY
hcuot "Feb 23 13:40:45 1984" 'zmail/patch-53-12.qfasl.2'	     # Author: MLY
hcuot "Mar  4 08:41:33 1984" 'zmail/patch-53-13.lisp.1'		     # Author: MLY
hcuot "Mar  4 08:41:37 1984" 'zmail/patch-53-13.qfasl.1'	     # Author: MLY
hcuot "Mar 24 17:24:31 1984" 'zmail/patch-53-14.lisp.2'		     # Author: RMS
hcuot "Mar 24 17:24:35 1984" 'zmail/patch-53-14.qfasl.2'	     # Author: RMS
hcuot "Apr 11 07:05:23 1984" 'zmail/patch-53-15.lisp.3'		     # Author: MLY
hcuot "Apr 11 07:05:32 1984" 'zmail/patch-53-15.qfasl.3'	     # Author: MLY
hcuot "Apr 18 09:41:32 1984" 'zmail/patch-53-16.lisp.1'		     # Author: RPK
hcuot "Apr 18 09:41:38 1984" 'zmail/patch-53-16.qfasl.1'	     # Author: RPK
hcuot "Apr 22 00:46:53 1984" 'zmail/patch-53-17.lisp.2'		     # Author: RMS
hcuot "Apr 22 00:47:01 1984" 'zmail/patch-53-17.qfasl.2'	     # Author: RMS
hcuot "Jun 29 04:21:13 1984" 'zmail/patch-53-18.lisp.1'		     # Author: RPK
hcuot "Jun 29 08:53:32 1984" 'zmail/patch-53-18.qfasl.1'	     # Author: RPK
hcuot "Oct 14 10:57:28 1984" 'zmail/patch-53-19.lisp.1'		     # Author: MLY
hcuot "Oct 14 10:57:55 1984" 'zmail/patch-53-19.qfasl.1'	     # Author: MLY
hcuot "Dec  6 05:18:26 1983" 'zmail/patch-53-2.lisp.1'		     # Author: RMS
hcuot "Dec  6 05:18:36 1983" 'zmail/patch-53-2.qfasl.1'		     # Author: RMS
hcuot "Dec 13 06:15:17 1983" 'zmail/patch-53-3.lisp.2'		     # Author: RMS
hcuot "Dec 13 06:15:23 1983" 'zmail/patch-53-3.qfasl.2'		     # Author: RMS
hcuot "Dec 14 08:54:56 1983" 'zmail/patch-53-5.lisp.1'		     # Author: RMS
hcuot "Dec 14 08:55:02 1983" 'zmail/patch-53-5.qfasl.1'		     # Author: RMS
hcuot "Jan  3 18:55:45 1984" 'zmail/patch-53-6.lisp.2'		     # Author: PGS
hcuot "Jan  3 18:55:54 1984" 'zmail/patch-53-6.qfasl.2'		     # Author: PGS
hcuot "Jan  1 01:08:53 1984" 'zmail/patch-53-7.lisp.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 01:09:00 1984" 'zmail/patch-53-7.qfasl.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 15:59:26 1984" 'zmail/patch-53-8.lisp.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 15:59:30 1984" 'zmail/patch-53-8.qfasl.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 16:00:18 1984" 'zmail/patch-53-9.lisp.2'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 16:00:22 1984" 'zmail/patch-53-9.qfasl.2'		     # Author: DANIEL.G.MLY
hcuot "Jun 29 11:28:11 1982" 'zmail/poop.text.35'		     # Author: RMS
hcuot "Apr 16 21:36:09 1984" 'zmail/profil.lisp.119'		     # Author: MLY
hcuot "Jul 13 07:22:00 1984" 'zmail/profil.lisp.121'		     # Author: RPK
hcuot "Sep 11 06:21:26 1984" 'zmail/profil.lisp.124'		     # Author: RMS
hcuot "Sep 11 06:21:59 1984" 'zmail/profil.qfasl.124'		     # Author: RMS
hcuot "Apr  7 16:03:55 1984" 'zmail/refer.lisp.6'		     # Author: MLY
hcuot "Jul 13 07:22:56 1984" 'zmail/refer.lisp.7'		     # Author: RPK
hcuot "Sep  9 20:29:01 1984" 'zmail/refer.qfasl.7'		     # Author: RMS
hcuot "Apr 30 14:38:24 1984" 'zmail/rfc733.lisp.56'		     # Author: MLY
hcuot "Jul 13 07:16:29 1984" 'zmail/rfc733.lisp.57'		     # Author: RPK
hcuot "Sep  9 21:03:17 1984" 'zmail/rfc733.qfasl.57'		     # Author: RMS
hcuot "Dec 31 03:02:48 1983" 'zmail/top.lisp.551'		     # Author: DANIEL.G.MLY
hcuot "Jul 13 06:57:38 1984" 'zmail/top.lisp.553'		     # Author: RPK
hcuot "Sep  9 23:57:54 1984" 'zmail/top.lisp.554'		     # Author: RMS
hcuot "Sep 26 12:37:36 1984" 'zmail/top.lisp.555'		     # Author: MLY
hcuot "Sep 10 07:45:42 1984" 'zmail/top.qfasl.554'		     # Author: RMS
hcuot "Apr 11 06:35:53 1984" 'zmail/window.lisp.340'		     # Author: MLY
hcuot "Jul 13 07:22:49 1984" 'zmail/window.lisp.342'		     # Author: RPK
hcuot "Sep  9 23:58:28 1984" 'zmail/window.lisp.343'		     # Author: RMS
hcuot "Sep 10 08:13:55 1984" 'zmail/window.qfasl.343'		     # Author: RMS
hcuot "Dec 14 06:00:40 1984" 'zmail/manual/manual.text.1'	     # Author: MLY
hcuot "Jun  8 09:14:17 1983" 'zmail/manual/top.txt.1'		     # Author: RPK
hcuot "Jun 29 11:04:18 1982" 'zwei/.comnd.text.1'		     # Author: RMS
hcuot "Jun 29 11:04:27 1982" 'zwei/atsign.xfile.1'		     # Author: RMS
hcuot "Jun  9 13:24:16 1984" 'zwei/bdired.lisp.41'		     # Author: MLY
hcuot "Aug  5 04:08:03 1984" 'zwei/bdired.qfasl.41'		     # Author: MLY
hcuot "Jan 27 19:35:00 1983" 'zwei/bug.bugs7.1'			     # Author: RMS
hcuot "Jan 14 02:27:23 1985" 'zwei/bug.zwei.1'			     # Author: OPERATOR
hcuot "Oct  8 10:11:11 1983" 'zwei/bug-zwei.text.1'		     # Author: TIM
hcuot "Jun 29 11:04:29 1982" 'zwei/bugs.bugs.1'			     # Author: RMS
hcuot "Jun 29 11:05:20 1982" 'zwei/bugs.bugs6.1'		     # Author: RMS
hcuot "Jun 29 11:05:52 1982" 'zwei/bugs.status.1'		     # Author: RMS
hcuot "Mar 15 09:23:53 1984" 'zwei/coma.lisp.101'		     # Author: TIM
hcuot "Jul 28 20:07:07 1984" 'zwei/coma.lisp.102'		     # Author: MLY
hcuot "Sep 26 12:38:20 1984" 'zwei/coma.lisp.103'		     # Author: MLY
hcuot "Nov 15 10:03:18 1984" 'zwei/coma.lisp.104'		     # Author: MLY
hcuot "Aug  4 00:18:12 1984" 'zwei/coma.qfasl.102'		     # Author: MLY
hcuot "Apr  5 18:02:13 1984" 'zwei/comb.lisp.92'		     # Author: MLY
hcuot "Oct 11 08:58:19 1984" 'zwei/comb.lisp.95'		     # Author: MLY
hcuot "Aug  4 00:20:56 1984" 'zwei/comb.qfasl.94'		     # Author: MLY
hcuot "Jun  9 13:24:12 1984" 'zwei/comc.lisp.201'		     # Author: MLY
hcuot "Sep  9 04:13:16 1984" 'zwei/comc.lisp.204'		     # Author: RMS
hcuot "Nov 14 04:48:57 1984" 'zwei/comc.lisp.205'		     # Author: MLY
hcuot "Sep  9 05:47:20 1984" 'zwei/comc.qfasl.204'		     # Author: RMS
hcuot "Jan  2 02:23:32 1984" 'zwei/comd.lisp.158'		     # Author: RMS
hcuot "Aug  5 01:13:21 1984" 'zwei/comd.lisp.165'		     # Author: MLY
hcuot "Oct 30 12:53:47 1984" 'zwei/comd.lisp.169'		     # Author: MLY
hcuot "Sep  7 22:43:25 1984" 'zwei/comd.qfasl.167'		     # Author: RMS
hcuot "Dec 13 05:42:14 1983" 'zwei/come.lisp.132'		     # Author: RMS
hcuot "Apr  5 17:57:40 1984" 'zwei/come.lisp.133'		     # Author: MLY
hcuot "Nov 14 04:47:10 1984" 'zwei/come.lisp.134'		     # Author: MLY
hcuot "Aug  4 00:28:48 1984" 'zwei/come.qfasl.133'		     # Author: MLY
hcuot "Mar 24 17:00:22 1984" 'zwei/comf.lisp.95'		     # Author: RMS
hcuot "Aug  5 00:20:06 1984" 'zwei/comf.lisp.98'		     # Author: MLY
hcuot "Dec 14 15:48:33 1984" 'zwei/comf.lisp.101'		     # Author: MLY
hcuot "Sep  9 05:48:56 1984" 'zwei/comf.qfasl.99'		     # Author: RMS
hcuot "Jun  5 03:52:51 1984" 'zwei/comg.lisp.39'		     # Author: MLY
hcuot "Aug 15 03:17:48 1984" 'zwei/comg.lisp.40'		     # Author: RPK
hcuot "Aug 29 09:32:06 1984" 'zwei/comg.qfasl.40'		     # Author: RMS
hcuot "Apr 16 21:22:48 1984" 'zwei/comh.lisp.8'			     # Author: CENT
hcuot "Aug  5 00:04:40 1984" 'zwei/comh.lisp.13'		     # Author: MLY
hcuot "Aug  5 00:04:47 1984" 'zwei/comh.qfasl.13'		     # Author: MLY
hcuot "Mar 31 13:33:54 1984" 'zwei/coms.lisp.81'		     # Author: MLY
hcuot "Apr 22 04:59:03 1984" 'zwei/coms.lisp.82'		     # Author: RMS
hcuot "May 16 11:06:10 1984" 'zwei/coms.lisp.83'		     # Author: RMS
hcuot "Jul  8 18:15:48 1984" 'zwei/coms.lisp.85'		     # Author: MLY
hcuot "Aug  5 03:58:57 1984" 'zwei/coms.qfasl.85'		     # Author: MLY
hcuot "Mar 31 15:25:07 1984" 'zwei/comtab.lisp.307'		     # Author: MLY
hcuot "Jun  6 07:23:51 1984" 'zwei/comtab.lisp.310'		     # Author: MLY
hcuot "Sep  5 23:14:03 1984" 'zwei/comtab.lisp.317'		     # Author: RMS
hcuot "Nov 28 14:34:00 1984" 'zwei/comtab.lisp.321'		     # Author: MLY
hcuot "Sep  7 22:39:57 1984" 'zwei/comtab.qfasl.317'		     # Author: RMS
hcuot "Jan 19 21:30:52 1984" 'zwei/defs.lisp.144'		     # Author: MLY
hcuot "Jun 12 17:53:09 1984" 'zwei/defs.lisp.150'		     # Author: RMS
hcuot "Sep 11 20:44:53 1984" 'zwei/defs.lisp.155'		     # Author: LISPM
hcuot "Sep 11 21:19:07 1984" 'zwei/defs.qfasl.155'		     # Author: LISPM
hcuot "Feb  9 19:45:22 1984" 'zwei/dired.lisp.299'		     # Author: RMS
hcuot "Jul 26 08:30:44 1984" 'zwei/dired.lisp.303'		     # Author: MLY
hcuot "Dec 14 15:49:04 1984" 'zwei/dired.lisp.307'		     # Author: MLY
hcuot "Aug 29 09:33:34 1984" 'zwei/dired.qfasl.304'		     # Author: RMS
hcuot "Dec 27 08:59:04 1983" 'zwei/displa.lisp.149'		     # Author: RMS
hcuot "Feb  7 13:56:37 1984" 'zwei/displa.lisp.151'		     # Author: MLY
hcuot "Aug  5 02:39:21 1984" 'zwei/displa.lisp.155'		     # Author: MLY
hcuot "Sep  6 18:54:03 1984" 'zwei/displa.lisp.157'		     # Author: RMS
hcuot "Sep  7 22:46:25 1984" 'zwei/displa.qfasl.157'		     # Author: RMS
hcuot "Feb 19 15:26:09 1984" 'zwei/doc.lisp.72'			     # Author: MLY
hcuot "Jun 21 02:53:18 1984" 'zwei/doc.lisp.74'			     # Author: MLY
hcuot "Nov 28 10:42:09 1984" 'zwei/doc.lisp.75'			     # Author: MLY
hcuot "Aug  5 04:09:10 1984" 'zwei/doc.qfasl.74'		     # Author: MLY
hcuot "Jun 29 11:10:53 1982" 'zwei/emacs.comdif.1'		     # Author: RMS
hcuot "Aug 10 10:28:26 1983" 'zwei/fasupd.lisp.29'		     # Author: RMS
hcuot "Apr  7 16:05:03 1984" 'zwei/fasupd.lisp.31'		     # Author: MLY
hcuot "Aug  5 04:10:49 1984" 'zwei/fasupd.qfasl.31'		     # Author: MLY
hcuot "Jan  4 00:13:46 1984" 'zwei/files.lisp.192'		     # Author: RMS
hcuot "Jul  2 11:40:05 1984" 'zwei/files.lisp.195'		     # Author: MLY
hcuot "Oct 14 09:27:47 1984" 'zwei/files.lisp.196'		     # Author: LISPM
hcuot "Aug  5 04:11:25 1984" 'zwei/files.qfasl.195'		     # Author: MLY
hcuot "Feb  2 10:15:30 1984" 'zwei/font.lisp.86'		     # Author: RMS
hcuot "May 22 00:58:47 1984" 'zwei/font.lisp.88'		     # Author: MLY
hcuot "Aug  4 00:11:45 1984" 'zwei/font.qfasl.88'		     # Author: MLY
hcuot "Jul 28 21:24:34 1984" 'zwei/for.lisp.61'			     # Author: MLY
hcuot "Aug  5 02:39:16 1984" 'zwei/for.lisp.62'			     # Author: MLY
hcuot "Aug  5 03:53:34 1984" 'zwei/for.qfasl.62'		     # Author: MLY
hcuot "Jan  4 00:50:12 1984" 'zwei/history.lisp.15'		     # Author: RMS
hcuot "Sep 11 21:05:42 1984" 'zwei/history.lisp.16'		     # Author: LISPM
hcuot "Sep 11 21:33:45 1984" 'zwei/history.qfasl.16'		     # Author: LISPM
hcuot "Dec 22 10:01:04 1983" 'zwei/host.lisp.20'		     # Author: DANIEL.G.MLY
hcuot "Aug  5 04:15:39 1984" 'zwei/host.qfasl.20'		     # Author: MLY
hcuot "Oct 25 23:06:09 1983" 'zwei/indent.lisp.103'		     # Author: RMS
hcuot "Oct 11 08:56:06 1984" 'zwei/indent.lisp.105'		     # Author: MLY
hcuot "Aug  3 23:57:17 1984" 'zwei/indent.qfasl.104'		     # Author: MLY
hcuot "Jan 16 21:21:27 1984" 'zwei/info.zwei.1'			     # Author: RMS
hcuot "Jul 20 11:20:46 1983" 'zwei/insert.lisp.32'		     # Author: RMS
hcuot "Apr  7 16:06:12 1984" 'zwei/insert.lisp.33'		     # Author: MLY
hcuot "Nov  5 05:31:43 1984" 'zwei/insert.lisp.35'		     # Author: MLY
hcuot "Aug  3 23:59:24 1984" 'zwei/insert.qfasl.33'		     # Author: MLY
hcuot "Jul  8 18:10:53 1984" 'zwei/ispell.lisp.41'		     # Author: MLY
hcuot "Aug  5 04:16:46 1984" 'zwei/ispell.qfasl.41'		     # Author: MLY
hcuot "Dec 28 21:06:08 1983" 'zwei/kbdmac.lisp.46'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 07:17:18 1984" 'zwei/kbdmac.lisp.47'		     # Author: MLY
hcuot "Sep  6 00:19:24 1984" 'zwei/kbdmac.lisp.48'		     # Author: RMS
hcuot "Sep  7 23:06:51 1984" 'zwei/kbdmac.qfasl.48'		     # Author: RMS
hcuot "Dec 24 08:43:28 1983" 'zwei/lparse.lisp.31'		     # Author: RMS
hcuot "Aug  5 04:17:46 1984" 'zwei/lparse.qfasl.31'		     # Author: MLY
hcuot "Mar 15 09:24:11 1984" 'zwei/macros.lisp.137'		     # Author: TIM
hcuot "Jun 18 17:18:16 1984" 'zwei/macros.lisp.141'		     # Author: MLY
hcuot "Sep 26 12:38:08 1984" 'zwei/macros.lisp.148'		     # Author: MLY
hcuot "Sep  7 22:33:54 1984" 'zwei/macros.qfasl.147'		     # Author: RMS
hcuot "Jan 29 23:12:53 1984" 'zwei/meth.lisp.45'		     # Author: MLY
hcuot "Mar 18 09:39:19 1984" 'zwei/meth.lisp.47'		     # Author: MLY
hcuot "Jun  6 07:17:09 1984" 'zwei/meth.lisp.48'		     # Author: MLY
hcuot "Aug  4 00:00:57 1984" 'zwei/meth.qfasl.48'		     # Author: MLY
hcuot "Jan  2 02:23:39 1984" 'zwei/modes.lisp.127'		     # Author: RMS
hcuot "May 25 20:16:25 1984" 'zwei/modes.lisp.130'		     # Author: MLY
hcuot "Sep  7 00:25:37 1984" 'zwei/modes.lisp.138'		     # Author: RMS
hcuot "Sep  7 22:36:22 1984" 'zwei/modes.qfasl.138'		     # Author: RMS
hcuot "Feb  4 00:57:01 1984" 'zwei/mouse.lisp.95'		     # Author: MLY
hcuot "May 14 00:52:44 1984" 'zwei/mouse.lisp.96'		     # Author: MLY
hcuot "Oct 13 10:56:35 1984" 'zwei/mouse.lisp.97'		     # Author: MLY
hcuot "Aug  5 04:22:39 1984" 'zwei/mouse.qfasl.96'		     # Author: MLY
hcuot "Dec 23 06:50:27 1983" 'zwei/nprim.lisp.33'		     # Author: RMS
hcuot "Jul  3 23:23:20 1984" 'zwei/nprim.lisp.34'		     # Author: MLY
hcuot "Aug  4 00:08:03 1984" 'zwei/nprim.qfasl.34'		     # Author: MLY
hcuot "Jan 28 06:16:26 1984" 'zwei/pated.lisp.22'		     # Author: RMS
hcuot "Sep 13 05:07:41 1984" 'zwei/pated.lisp.26'		     # Author: RMS
hcuot "Nov  5 05:24:58 1984" 'zwei/pated.lisp.29'		     # Author: MLY
hcuot "Dec  7 06:56:33 1984" 'zwei/pated.lisp.30'		     # Author: MLY
hcuot "Aug  5 04:24:35 1984" 'zwei/pated.qfasl.25'		     # Author: MLY
hcuot "Dec 16 13:12:34 1982" 'zwei/pl1mod.lisp.13'		     # Author: RMS
hcuot "Apr  7 16:06:39 1984" 'zwei/pl1mod.lisp.14'		     # Author: MLY
hcuot "Aug  5 04:25:49 1984" 'zwei/pl1mod.qfasl.14'		     # Author: MLY
hcuot "Feb 17 09:39:18 1984" 'zwei/poss.lisp.85'		     # Author: MLY
hcuot "Oct 13 11:51:43 1984" 'zwei/poss.lisp.89'		     # Author: MLY
hcuot "Dec  1 20:23:59 1984" 'zwei/poss.lisp.90'		     # Author: MLY

# tid/3309

hcuot "Mar 22 06:37:20 1983" '-read-.-this-.2'
hcuot "Oct 26 04:03:03 1983" 'cc/cadld.lisp.6'
hcuot "May  1 11:26:47 1984" 'cc/cadld.lisp.7'
hcuot "Sep  3 22:58:12 1984" 'cc/cadld.lisp.8'
hcuot "Sep  9 05:13:51 1984" 'cc/cadld.qfasl.8'
hcuot "Jul  2 22:02:39 1982" 'cc/cadreg.lisp.1'
hcuot "Apr  7 15:02:03 1984" 'cc/cadreg.lisp.2'
hcuot "Sep 12 06:20:06 1984" 'cc/cadreg.lisp.4'
hcuot "Jul 24 04:17:33 1983" 'cc/cc.help.4'
hcuot "Jul  7 01:40:03 1984" 'cc/cc.lisp.48'
hcuot "Sep  9 03:39:15 1984" 'cc/cc.lisp.49'
hcuot "Sep 12 00:22:33 1984" 'cc/cc.lisp.50'
hcuot "Sep 12 06:20:40 1984" 'cc/cc.qfasl.50'
hcuot "Oct  9 08:57:54 1983" 'cc/ccdisk.lisp.106'
hcuot "Sep  9 05:19:18 1984" 'cc/ccdisk.qfasl.106'
hcuot "Dec 27 06:00:38 1983" 'cc/ccgsyl.lisp.6'
hcuot "Sep  9 04:58:38 1984" 'cc/ccgsyl.qfasl.6'
hcuot "Aug 17 03:47:14 1983" 'cc/ccwhy.lisp.12'
hcuot "Sep  9 05:17:29 1984" 'cc/ccwhy.qfasl.12'
hcuot "Apr  9 11:17:09 1983" 'cc/chploc.lisp.4'
hcuot "Apr  7 15:02:25 1984" 'cc/chploc.lisp.5'
hcuot "Sep  9 05:23:30 1984" 'cc/chploc.qfasl.5'
hcuot "Dec 22 06:46:28 1982" 'cc/dcfu.uload.1'
hcuot "Mar  9 20:11:02 1984" 'cc/dcfu.uload.2'
hcuot "Oct 27 23:54:38 1983" 'cc/dcheck.lisp.6'
hcuot "Sep  9 05:28:27 1984" 'cc/dcheck.lisp.7'
hcuot "Jul  2 21:56:55 1982" 'cc/dcheck.loop.1'
hcuot "Sep  9 05:36:25 1984" 'cc/dcheck.qfasl.7'
hcuot "Oct 26 03:54:36 1983" 'cc/diags.lisp.157'
hcuot "Sep  9 05:03:26 1984" 'cc/diags.lisp.159'
hcuot "Sep  9 05:04:08 1984" 'cc/diags.qfasl.159'
hcuot "Oct  9 08:57:49 1983" 'cc/dmon.lisp.56'
hcuot "Jan  3 20:00:50 1985" 'cc/dmon.lisp.57'
hcuot "Sep  9 05:10:55 1984" 'cc/dmon.qfasl.56'
hcuot "Dec  7 22:33:10 1984" 'cc/junk..1'
hcuot "Oct  9 08:57:45 1983" 'cc/lcadmc.lisp.29'
hcuot "Apr  7 15:03:28 1984" 'cc/lcadmc.lisp.30'
hcuot "Sep  9 03:31:10 1984" 'cc/lcadmc.lisp.31'
hcuot "Sep  9 04:49:35 1984" 'cc/lcadmc.qfasl.31'
hcuot "Aug 18 18:35:04 1983" 'cc/lcadrd.lisp.94'
hcuot "Apr  7 15:04:15 1984" 'cc/lcadrd.lisp.95'
hcuot "Sep  9 04:58:54 1984" 'cc/lcadrd.qfasl.95'
hcuot "Jun 20 01:44:48 1983" 'cc/ldbg.lisp.45'
hcuot "Sep  9 05:12:40 1984" 'cc/ldbg.qfasl.45'
hcuot "Nov 12 08:26:07 1983" 'cc/lqfmac.lisp.17'
hcuot "Sep  9 04:48:57 1984" 'cc/lqfmac.qfasl.17'
hcuot "Oct 26 04:06:09 1983" 'cc/patch.directory.3'
hcuot "Sep  8 02:19:50 1984" 'cc/patch-3.directory.22'
hcuot "Sep  8 02:21:16 1984" 'cc/patch-3.directory.23'
hcuot "Dec  1 14:12:53 1983" 'cc/patch-3-1.qfasl.1'
hcuot "Sep  8 02:20:42 1984" 'cc/patch-3-10.lisp.1'
hcuot "Sep  8 02:20:45 1984" 'cc/patch-3-10.qfasl.1'
hcuot "Dec 18 00:50:34 1983" 'cc/patch-3-2.qfasl.2'
hcuot "Dec 27 05:59:31 1983" 'cc/patch-3-3.qfasl.1'
hcuot "Dec 27 19:59:00 1983" 'cc/patch-3-4.qfasl.1'
hcuot "Jan 23 07:16:31 1984" 'cc/patch-3-5.qfasl.2'
hcuot "Jan 27 09:11:57 1984" 'cc/patch-3-6.qfasl.1'
hcuot "Jun 11 23:06:48 1984" 'cc/patch-3-7.qfasl.1'
hcuot "Jul  7 01:29:34 1984" 'cc/patch-3-8.qfasl.1'
hcuot "Sep  6 20:28:13 1984" 'cc/patch-3-9.lisp.1'
hcuot "Sep  6 20:28:20 1984" 'cc/patch-3-9.qfasl.1'
hcuot "Jan 27 08:10:57 1984" 'cc/qf.lisp.124'
hcuot "Aug 31 01:36:34 1984" 'cc/qf.lisp.125'
hcuot "Sep  9 05:29:20 1984" 'cc/qf.lisp.126'
hcuot "Sep  9 05:33:23 1984" 'cc/qf.qfasl.126'
hcuot "Oct 18 20:04:38 1983" 'cc/salvag.lisp.37'
hcuot "Jul  7 01:39:01 1984" 'cc/salvag.lisp.38'
hcuot "Sep  9 05:23:45 1984" 'cc/salvag.qfasl.38'
hcuot "Apr  7 15:05:00 1984" 'cc/zero.lisp.15'
hcuot "Sep  9 05:13:29 1984" 'cc/zero.qfasl.15'
hcuot "Sep  4 16:10:52 1984" 'chaos/hosts.text.270'
hcuot "Dec  5 18:44:19 1984" 'chaos/hosts.text.330'
hcuot "Dec  6 00:34:15 1984" 'chaos/hosts.text.331'
hcuot "Oct 16 10:29:07 1983" 'cold/coldld.lisp.77'
hcuot "Sep 10 22:11:17 1984" 'cold/coldld.lisp.83'
hcuot "Oct  9 13:51:43 1984" 'cold/coldld.lisp.84'
hcuot "Sep 11 07:47:22 1984" 'cold/coldld.qfasl.83'
hcuot "Oct 11 07:05:45 1983" 'cold/coldpk.lisp.17'
hcuot "Jul  8 21:08:12 1984" 'cold/coldpk.lisp.19'
hcuot "Aug 27 03:36:53 1984" 'cold/coldpk.lisp.20'
hcuot "Sep 11 07:43:22 1984" 'cold/coldpk.lisp.25'
hcuot "Sep 11 07:43:37 1984" 'cold/coldpk.qfasl.25'
hcuot "Nov  9 08:42:59 1983" 'cold/coldut.lisp.91'
hcuot "Jun 14 09:04:24 1984" 'cold/coldut.lisp.95'
hcuot "Jul 12 04:06:39 1984" 'cold/coldut.lisp.97'
hcuot "Aug 30 06:45:32 1984" 'cold/coldut.lisp.100'
hcuot "Aug 30 09:00:56 1984" 'cold/coldut.qfasl.100'
hcuot "Jul  8 18:41:04 1984" 'cold/common-lisp.lisp.1'
hcuot "Sep 14 21:21:53 1983" 'cold/defmic.lisp.154'
hcuot "Jul  2 11:39:28 1984" 'cold/defmic.lisp.194'
hcuot "Sep  5 20:29:29 1984" 'cold/defmic.lisp.200'
hcuot "Apr 17 23:49:49 1984" 'cold/docmic.lisp.32'
hcuot "Jul 26 08:20:42 1984" 'cold/docmic.lisp.37'
hcuot "Sep 26 10:23:45 1984" 'cold/docmic.lisp.38'
hcuot "Nov 17 02:06:08 1984" 'cold/docmic.lisp.40'
hcuot "Oct  9 13:12:02 1984" 'cold/export.lisp.21'
hcuot "Nov 16 07:27:38 1984" 'cold/export.lisp.22'
hcuot "Nov 28 12:34:36 1984" 'cold/export.lisp.23'
hcuot "Nov 17 02:00:26 1984" 'cold/global.lisp.637'
hcuot "Dec 14 13:58:14 1984" 'cold/global.lisp.639'
hcuot "Aug 15 04:46:01 1984" 'cold/global.qfasl.634'
hcuot "Feb  2 00:30:00 1983" 'cold/mini.lisp.88'
hcuot "Oct  7 07:54:17 1984" 'cold/mini.lisp.89'
hcuot "Aug 15 05:19:46 1984" 'cold/mini.qfasl.88'
hcuot "Nov 11 03:18:52 1984" 'cold/minisr.exe.1'
hcuot "Nov 11 03:18:01 1984" 'cold/minisr.mid.44'
hcuot "Dec 19 03:17:21 1983" 'cold/qcom.lisp.569'
hcuot "Jun  2 04:25:25 1984" 'cold/qcom.lisp.572'
hcuot "Sep  2 08:23:17 1984" 'cold/qcom.lisp.579'
hcuot "Dec  6 09:29:00 1984" 'cold/qcom.lisp.581'
hcuot "Dec  9 22:21:56 1984" 'cold/qcom.lisp.582'
hcuot "Nov  1 05:54:53 1983" 'cold/qdefs.lisp.369'
hcuot "Jun  2 04:24:47 1984" 'cold/qdefs.lisp.377'
hcuot "Aug 27 23:39:24 1984" 'cold/qdefs.lisp.386'
hcuot "Sep 12 17:24:45 1984" 'cold/qdefs.lisp.387'
hcuot "Dec  6 09:29:11 1984" 'cold/qdefs.lisp.388'
hcuot "Nov 10 11:06:56 1983" 'cold/system.lisp.82'
hcuot "Jun  7 04:10:40 1984" 'cold/system.lisp.94'
hcuot "Sep 25 07:31:53 1984" 'cold/system.lisp.103'
hcuot "Oct 24 03:48:58 1984" 'cold/system.lisp.104'
hcuot "Aug 15 06:23:06 1984" 'cold/system.qfasl.102'
hcuot "Apr  7 15:06:03 1984" 'demo/abacus.lisp.20'
hcuot "Sep  8 01:13:00 1984" 'demo/abacus.qfasl.20'
hcuot "Dec 13 05:41:22 1983" 'demo/alarm.lisp.50'
hcuot "Jun  6 12:58:32 1984" 'demo/alarm.qfasl.50'
hcuot "Aug 16 10:56:31 1983" 'demo/beeps.lisp.8'
hcuot "Oct 26 23:25:37 1983" 'demo/beeps.qfasl.8'
hcuot "Apr  7 15:06:23 1984" 'demo/cafe.lisp.8'
hcuot "Jun  6 13:00:32 1984" 'demo/cafe.qfasl.8'
hcuot "Nov 13 00:18:36 1983" 'demo/colorhack.lisp.7'
hcuot "Nov 13 07:19:50 1983" 'demo/colorhack.qfasl.7'
hcuot "Jun 20 02:52:15 1983" 'demo/colxor.lisp.52'
hcuot "Oct 26 23:29:52 1983" 'demo/colxor.qfasl.52'
hcuot "Jul 21 17:00:24 1982" 'demo/craze.lisp.2'
hcuot "Aug 14 10:34:04 1983" 'demo/craze.qfasl.2'
hcuot "Dec  9 04:29:51 1983" 'demo/crock.lisp.6'
hcuot "Sep  8 01:11:57 1984" 'demo/crock.qfasl.6'
hcuot "Apr  7 16:38:29 1984" 'demo/ctest.lisp.1'
hcuot "Mar 31 18:14:03 1984" 'demo/dc.lisp.4'
hcuot "Sep  8 01:10:53 1984" 'demo/dc.qfasl.4'
hcuot "Apr  7 15:07:09 1984" 'demo/deutsc.lisp.34'
hcuot "Jun  6 13:04:44 1984" 'demo/deutsc.qfasl.34'
hcuot "Apr  7 15:07:35 1984" 'demo/dlwhak.lisp.37'
hcuot "Jun  6 13:05:34 1984" 'demo/dlwhak.qfasl.37'
hcuot "Apr  7 15:08:37 1984" 'demo/docscr.lisp.6'
hcuot "Jun  6 13:07:54 1984" 'demo/docscr.qfasl.6'
hcuot "Apr  7 15:09:02 1984" 'demo/doctor.lisp.9'
hcuot "Sep  5 19:58:44 1984" 'demo/doctor.lisp.10'
hcuot "Sep  7 23:25:44 1984" 'demo/doctor.qfasl.10'
hcuot "Jun 20 02:52:55 1983" 'demo/geb.lisp.27'
hcuot "Oct 26 23:38:48 1983" 'demo/geb.qfasl.27'
hcuot "Jun 20 02:53:09 1983" 'demo/hakdef.lisp.14'
hcuot "Sep  8 01:14:00 1984" 'demo/hakdef.qfasl.14'
hcuot "Jun 20 02:51:57 1983" 'demo/hcedit.lisp.27'
hcuot "Apr  7 15:09:41 1984" 'demo/hcedit.lisp.28'
hcuot "Jun  6 13:08:50 1984" 'demo/hcedit.qfasl.28'
hcuot "Jul 21 15:52:55 1982" 'demo/liss.lisp.4'
hcuot "Apr  7 15:11:41 1984" 'demo/liss.lisp.5'
hcuot "Nov 10 12:46:06 1983" 'demo/munch.lisp.14'
hcuot "Sep  8 01:09:22 1984" 'demo/munch.qfasl.14'
hcuot "Apr  7 15:24:04 1984" 'demo/npaint.lisp.1'
hcuot "Aug 16 10:56:24 1983" 'demo/ohacks.lisp.35'
hcuot "Oct 26 23:44:57 1983" 'demo/ohacks.qfasl.35'
hcuot "Apr  7 15:12:09 1984" 'demo/organ.lisp.18'
hcuot "Jun  6 13:09:56 1984" 'demo/organ.qfasl.18'
hcuot "Jun 20 02:52:00 1983" 'demo/pfom.lisp.31'
hcuot "Aug 14 10:44:10 1983" 'demo/pfom.qfasl.31'
hcuot "Oct 24 20:42:24 1983" 'demo/qix.lisp.3'
hcuot "Oct 26 23:47:45 1983" 'demo/qix.qfasl.3'
hcuot "Jul 26 10:08:11 1983" 'demo/rotate.lisp.5'
hcuot "Oct 26 23:48:20 1983" 'demo/rotate.qfasl.5'
hcuot "Aug 20 21:03:08 1983" 'demo/rotcir.lisp.5'
hcuot "Oct 26 23:49:12 1983" 'demo/rotcir.qfasl.5'
hcuot "Dec 27 14:24:52 1983" 'demo/treedv.lisp.4'
hcuot "Dec 27 14:25:03 1983" 'demo/treedv.qfasl.4'
hcuot "Jul 20 14:04:52 1982" 'demo/tvbgar.qfasl.1'
hcuot "Apr  7 15:39:50 1984" 'demo/versat.lisp.1'
hcuot "Apr  7 15:41:20 1984" 'demo/votrax.lisp.1'
hcuot "Oct 22 02:49:11 1983" 'demo/what.lisp.19'
hcuot "Oct 26 23:54:43 1983" 'demo/what.qfasl.19'
hcuot "Apr  7 15:42:46 1984" 'demo/words.lisp.1'
hcuot "Nov 18 15:00:51 1983" 'demo/worm.lisp.8'
hcuot "Sep  6 23:42:51 1984" 'demo/worm.lisp.9'
hcuot "Sep 10 07:44:09 1984" 'demo/worm.qfasl.9'
hcuot "Dec 13 05:41:19 1983" 'demo/worm-trails.lisp.15'
hcuot "Sep  8 01:08:09 1984" 'demo/worm-trails.qfasl.15'
hcuot "Jul 20 14:05:17 1982" 'demo/wormch.ast.1'
hcuot "Jul 20 14:05:20 1982" 'demo/wormch.qfasl.1'
hcuot "Feb 21 00:08:57 1984" 'distribution/dist.lisp.7'
hcuot "Jun 15 10:28:10 1984" 'distribution/dist.lisp.8'
hcuot "Feb 21 00:09:00 1984" 'distribution/dist.qfasl.7'
hcuot "Feb 16 13:57:28 1984" 'distribution/lmi-filter.lisp.2'
hcuot "Aug  1 21:21:36 1982" 'doc/.compl.prelud.1'
hcuot "Aug  1 21:21:44 1982" 'doc/array.intent.1'
hcuot "Aug  1 21:21:50 1982" 'doc/bmcode.text.4'
hcuot "Jan 19 01:13:05 1985" 'doc/bug.lispm.1'
hcuot "Aug  1 21:24:26 1982" 'doc/bug.lispm10.1'
hcuot "Aug  1 21:25:10 1982" 'doc/bug.lispm11.1'
hcuot "Aug  1 21:26:02 1982" 'doc/bug.lispm12.1'
hcuot "Aug  1 21:22:04 1982" 'doc/bug.lispm13.1'
hcuot "Nov 30 00:47:02 1982" 'doc/bug.lispm14.1'
hcuot "Jan 11 22:41:27 1983" 'doc/bug.lispm15.1'
hcuot "Feb 24 02:11:04 1983" 'doc/bug.lispm16.1'
hcuot "Apr 18 07:53:19 1983" 'doc/bug.lispm17.1'
hcuot "Jun  1 20:37:41 1983" 'doc/bug.lispm18.1'
hcuot "May 23 13:26:53 1984" 'doc/bug.lispm18.2'
hcuot "Aug  4 04:36:47 1983" 'doc/bug.lispm19.1'
hcuot "Sep 23 01:01:25 1984" 'doc/bug.lispm19.2'
hcuot "Oct  5 19:38:55 1983" 'doc/bug.lispm20.1'
hcuot "Nov 12 23:29:45 1983" 'doc/bug.lispm21.1'
hcuot "Jan  7 20:27:51 1984" 'doc/bug.lispm22.1'
hcuot "May 16 00:33:17 1984" 'doc/bug.lispm23.1'
hcuot "Nov 16 10:43:03 1984" 'doc/bug.lispm24.1'
hcuot "Nov 16 11:05:09 1984" 'doc/bug.lispm25.1'
hcuot "Nov 16 11:20:32 1984" 'doc/bug.lispm26.1'
hcuot "Aug  1 21:23:42 1982" 'doc/bug.lispm9.1'
hcuot "Aug  1 21:26:50 1982" 'doc/bug.not-info-lispm!.1'
hcuot "Aug  1 21:26:56 1982" 'doc/cadr.text.164'
hcuot "Oct  5 22:51:06 1984" 'doc/cadr-mail.idx.1'
hcuot "Jan 18 23:41:54 1985" 'doc/cadr-mail.txt.1'
hcuot "Aug  1 21:27:24 1982" 'doc/cells.text.4'
hcuot "Aug  1 21:27:33 1982" 'doc/char.text.18'
hcuot "Aug  1 21:27:39 1982" 'doc/chead.text.4'
hcuot "Nov  1 18:57:53 1982" 'doc/chfile.text.3'
hcuot "Aug  1 21:27:57 1982" 'doc/chod1.drw.1'
hcuot "Aug  1 21:28:07 1982" 'doc/chodam.drw.1'
hcuot "Aug  1 21:28:16 1982" 'doc/chodi.drw.1'
hcuot "Aug  1 21:28:26 1982" 'doc/chodtm.drw.1'
hcuot "Jun 16 01:54:24 1984" 'doc/clisp-mail.txt.1'
hcuot "Aug  1 21:28:32 1982" 'doc/closur.text.12'
hcuot "Aug  1 21:28:39 1982" 'doc/cold.tags.1'
hcuot "Jan  2 07:51:49 1984" 'doc/common.lisp.9'
hcuot "Aug  1 21:28:51 1982" 'doc/cons.text.93'
hcuot "Dec 26 05:02:18 1984" 'doc/converse.bugs.1'
hcuot "Aug  1 21:29:11 1982" 'doc/csoft.text.20'
hcuot "Aug  1 21:29:32 1982" 'doc/cursor.answer.1'
hcuot "Aug  1 21:29:39 1982" 'doc/dfs.text.12'
hcuot "Aug  1 21:29:45 1982" 'doc/disk.text.22'
hcuot "Dec  9 19:32:38 1984" 'doc/doc-changes-mail.txt.1'
hcuot "Aug  1 21:29:53 1982" 'doc/doctor.text.5'
hcuot "Aug  1 21:30:04 1982" 'doc/eddoc.text.6'
hcuot "Aug  1 21:30:11 1982" 'doc/edfnd.text.11'
hcuot "Aug  1 21:30:16 1982" 'doc/eined.text.5'
hcuot "Aug  1 21:30:23 1982" 'doc/error.lights.1'
hcuot "Aug  1 21:30:31 1982" 'doc/fasld.text.1'
hcuot "Aug  1 21:30:35 1982" 'doc/fcfs.text.10'
hcuot "Aug  1 21:42:29 1982" 'doc/fig.fil.2'
hcuot "Aug  1 21:42:35 1982" 'doc/fig1.drw.1'
hcuot "Aug  1 21:42:44 1982" 'doc/fig2.drw.1'
hcuot "Aug  1 21:42:54 1982" 'doc/fig3.drw.1'
hcuot "Aug  1 21:43:02 1982" 'doc/fig5.drw.1'
hcuot "Aug  1 21:43:11 1982" 'doc/format.text.77'
hcuot "Aug  1 21:43:25 1982" 'doc/gctim.text.5'
hcuot "Aug  1 21:43:32 1982" 'doc/goto.text.1'
hcuot "Aug  1 21:43:38 1982" 'doc/if.answer.1'
hcuot "Jan 18 03:52:06 1985" 'doc/info.lispm.1'
hcuot "Aug  1 21:44:12 1982" 'doc/info.lispm1.1'
hcuot "Aug  1 21:43:42 1982" 'doc/info.lispm2.1'
hcuot "Jan  7 18:46:34 1983" 'doc/instal.newsys.2'
hcuot "Aug  1 21:45:03 1982" 'doc/io.text.3'
hcuot "Aug  1 21:45:09 1982" 'doc/iob.text.9'
hcuot "Aug  1 21:45:16 1982" 'doc/kbds.text.8'
hcuot "Aug  1 21:45:58 1982" 'doc/lmacro.text.1'
hcuot "Aug  1 21:46:05 1982" 'doc/lmcomp.text.3'
hcuot "Aug  1 22:00:01 1982" 'doc/lmfns.text.8'
hcuot "Aug  1 22:00:23 1982" 'doc/lmnuc.text.156'
hcuot "Aug  1 22:00:41 1982" 'doc/lmtape.text.1'
hcuot "Aug  1 23:10:27 1982" 'doc/macro.text.28'
hcuot "Aug  1 23:10:33 1982" 'doc/mcdoc.text.12'
hcuot "Aug  1 23:10:38 1982" 'doc/mcrdoc.text.3'
hcuot "Aug  1 23:10:44 1982" 'doc/menu.text.1'
hcuot "Aug  1 23:10:50 1982" 'doc/mess.text.5'
hcuot "Aug  1 23:10:58 1982" 'doc/meter.text.7'
hcuot "Aug  1 23:11:06 1982" 'doc/mouse.text.3'
hcuot "Aug  1 23:11:13 1982" 'doc/name.text.5'
hcuot "Aug  1 23:11:17 1982" 'doc/nboot.text.19'
hcuot "Oct 24 21:45:35 1982" 'doc/nes.text.2'
hcuot "Nov 26 08:19:34 1984" 'doc/netwrk.msg.1'
hcuot "Aug  1 23:12:05 1982" 'doc/packd.text.5'
hcuot "Aug  1 23:12:15 1982" 'doc/paging.text.41'
hcuot "Aug  1 23:12:27 1982" 'doc/paper.text.105'
hcuot "Aug  1 23:12:48 1982" 'doc/proces.text.6'
hcuot "Aug  1 23:12:55 1982" 'doc/progr.text.29'
hcuot "Aug  1 23:13:06 1982" 'doc/qev.text.1'
hcuot "Aug  1 23:13:09 1982" 'doc/rename.text.21'
hcuot "Aug  1 23:13:13 1982" 'doc/sgmods.text.15'
hcuot "Aug  1 23:13:17 1982" 'doc/ss201.msg.1'
hcuot "Aug  1 23:13:20 1982" 'doc/stackg.text.4'
hcuot "Aug  1 23:13:24 1982" 'doc/storag.text.39'
hcuot "Aug  1 23:13:33 1982" 'doc/sys204.flavor.1'
hcuot "Aug  1 23:13:38 1982" 'doc/sys204.msg.1'
hcuot "Aug  1 23:13:44 1982" 'doc/sys210.msg.1'
hcuot "Sep 19 06:01:03 1983" 'doc/sys286.msg.5'
hcuot "Aug  1 23:14:08 1982" 'doc/sys74.msg.1'
hcuot "Aug  1 23:14:12 1982" 'doc/sys78.msg.1'
hcuot "Aug  1 23:14:16 1982" 'doc/sys79.msg.1'
hcuot "Aug  1 23:14:21 1982" 'doc/sys85.msg.1'
hcuot "Aug  1 23:14:27 1982" 'doc/sys86.msg.1'
hcuot "Aug 30 09:31:33 1982" 'doc/sys87.msg.2'
hcuot "Oct 12 09:32:12 1982" 'doc/sys88.msg.8'
hcuot "Oct 31 19:32:24 1982" 'doc/sys89.msg.9'
hcuot "Feb 22 01:00:09 1983" 'doc/sys91.msg.10'
hcuot "Mar 17 05:11:14 1983" 'doc/sys93.msg.13'
hcuot "Jun  2 12:26:24 1983" 'doc/sys94.msg.12'
hcuot "Sep 19 09:34:26 1983" 'doc/sys97.msg.7'
hcuot "Dec 24 06:19:52 1983" 'doc/sys98.defstruct.6'
hcuot "Apr 29 00:34:35 1984" 'doc/sys98.msg.34'
hcuot "Dec 15 03:24:42 1983" 'doc/sys98.packages.2'
hcuot "Sep  8 00:59:54 1984" 'doc/sys99.msg.31'
hcuot "Sep 25 07:31:38 1984" 'doc/sys99.msg.34'
hcuot "Oct  2 18:54:38 1984" 'doc/sys99.msg.35'
hcuot "Oct 24 17:34:36 1984" 'doc/sys99.msg.37'
hcuot "Nov  9 11:28:17 1984" 'doc/sys99.msg.38'
hcuot "Nov 28 10:26:08 1984" 'doc/sys99.msg.41'
hcuot "Aug  1 23:14:41 1982" 'doc/tvdoc.text.34'
hcuot "Aug  1 23:15:02 1982" 'doc/unaddr.text.55'
hcuot "Aug  1 23:15:16 1982" 'doc/zwei.answer.1'
hcuot "Aug 19 15:56:59 1983" 'doc/zweidoc.txt.1'
hcuot "Sep 23 01:21:02 1984" 'doc/^current-bug-lispm.txt^.1'
hcuot "Dec  5 06:43:25 1984" 'file/bugs.mail.1'
hcuot "Jul 22 15:24:10 1982" 'file/clear.lisp.1'
hcuot "Jan  3 08:50:32 1984" 'file/copy.lisp.128'
hcuot "Aug  5 12:22:50 1984" 'file/copy.lisp.129'
hcuot "Sep 14 08:15:08 1984" 'file/copy.lisp.131'
hcuot "Jan  3 09:07:03 1984" 'file/copy.qfasl.128'
hcuot "Sep 12 01:49:40 1984" 'file/fs.directory.11'
hcuot "Sep 15 02:45:53 1984" 'file/fs.directory.12'
hcuot "Jul 22 15:28:50 1982" 'file/fs.improv.1'
hcuot "Mar  3 21:24:11 1983" 'file/fs.lisp.73'
hcuot "Jan  3 09:30:45 1984" 'file/fs.lisp.76'
hcuot "Sep 11 06:45:16 1984" 'file/fs.lisp.77'
hcuot "Sep 11 06:48:35 1984" 'file/fs.qfasl.77'
hcuot "Nov 21 21:54:37 1984" 'file/fs-48.directory.15'
hcuot "Nov 21 22:05:22 1984" 'file/fs-48.directory.16'
hcuot "Jan  5 00:02:49 1984" 'file/fs-48-1.lisp.1'
hcuot "Jan  5 00:03:05 1984" 'file/fs-48-1.qfasl.1'
hcuot "Jan 18 17:50:40 1984" 'file/fs-48-2.lisp.4'
hcuot "Jan 27 07:15:20 1984" 'file/fs-48-3.lisp.2'
hcuot "Jan 27 07:15:27 1984" 'file/fs-48-3.qfasl.2'
hcuot "Feb  8 15:17:10 1984" 'file/fs-48-4.lisp.1'
hcuot "May 16 10:04:05 1984" 'file/fs-48-4.lisp.2'
hcuot "May 16 10:04:09 1984" 'file/fs-48-4.qfasl.2'
hcuot "Jun 10 14:19:39 1984" 'file/fs-48-5.lisp.1'
hcuot "Jun 10 14:19:43 1984" 'file/fs-48-5.qfasl.1'
hcuot "Nov 21 22:04:24 1984" 'file/fs-48-6.lisp.1'
hcuot "Nov 21 22:04:35 1984" 'file/fs-48-6.qfasl.1'
hcuot "Jul 16 17:16:25 1984" 'file/fs-49.directory.2'
hcuot "Jul 16 17:33:45 1984" 'file/fs-49.directory.3'
hcuot "Jul 16 17:33:11 1984" 'file/fs-49-1.lisp.1'
hcuot "Jul 16 17:33:18 1984" 'file/fs-49-1.qfasl.1'
hcuot "Sep 12 01:49:42 1984" 'file/fs-50.directory.1'
hcuot "Sep 15 02:45:55 1984" 'file/fs-51.directory.1'
hcuot "Sep 12 02:41:05 1984" 'file/fsacc.lisp.3'
hcuot "Sep 12 05:09:56 1984" 'file/fsacc.lisp.4'
hcuot "Sep 12 05:34:49 1984" 'file/fsacc.lisp.5'
hcuot "Sep 12 06:08:40 1984" 'file/fsacc.qfasl.5'
hcuot "Jan 18 16:02:58 1984" 'file/fsdefs.lisp.171'
hcuot "May 26 16:20:04 1984" 'file/fsdefs.lisp.173'
hcuot "Sep 12 05:09:25 1984" 'file/fsdefs.lisp.177'
hcuot "Sep 12 05:54:26 1984" 'file/fsdefs.qfasl.177'
hcuot "Nov  6 15:13:45 1982" 'file/fsdoc.text.6'
hcuot "Dec 18 04:41:12 1983" 'file/fsguts.lisp.366'
hcuot "Feb  8 13:20:18 1984" 'file/fsguts.lisp.369'
hcuot "Sep 12 05:09:30 1984" 'file/fsguts.lisp.371'
hcuot "Nov 21 21:00:57 1984" 'file/fsguts.lisp.373'
hcuot "Sep 12 06:00:31 1984" 'file/fsguts.qfasl.371'
hcuot "Jan  3 09:24:35 1984" 'file/fsname.lisp.103'
hcuot "Jun 10 14:14:33 1984" 'file/fsname.lisp.104'
hcuot "Jul 16 18:47:52 1984" 'file/fsname.lisp.105'
hcuot "Sep 11 06:21:11 1984" 'file/fsname.lisp.106'
hcuot "Jul  5 11:03:05 1984" 'file/fsname.qfasl.104'
hcuot "Dec 18 03:29:18 1983" 'file/fsstr.lisp.104'
hcuot "Jul 16 18:47:56 1984" 'file/fsstr.lisp.105'
hcuot "Sep 12 05:09:42 1984" 'file/fsstr.lisp.107'
hcuot "Sep 12 05:58:38 1984" 'file/fsstr.qfasl.107'
hcuot "Apr  7 15:14:13 1984" 'file/hogs.lisp.4'
hcuot "Sep 14 05:24:39 1984" 'file/hogs.lisp.6'
hcuot "Sep 14 04:59:04 1984" 'file/hogs.qfasl.4'
hcuot "Jul 20 04:50:13 1983" 'file/lmpars.lisp.102'
hcuot "Jun 11 04:08:07 1984" 'file/lmpars.lisp.105'
hcuot "Aug 13 20:15:21 1984" 'file/lmpars.lisp.106'
hcuot "Sep 12 05:35:02 1984" 'file/lmpars.lisp.112'
hcuot "Dec 14 02:31:32 1984" 'file/lmpars.lisp.113'
hcuot "Sep 10 20:39:55 1984" 'file/lmpars.qfasl.110'
hcuot "Apr  7 15:14:57 1984" 'file/login.lisp.26'
hcuot "Jul 22 15:35:33 1982" 'file/login.qfasl.25'
hcuot "Jan  3 06:30:18 1984" 'file/magtape.directory.9'
hcuot "Oct 26 20:41:54 1983" 'file/magtape-14.directory.14'
hcuot "Mar  8 06:56:29 1983" 'file/magtape-14-1.lisp.1'
hcuot "Mar  8 06:56:47 1983" 'file/magtape-14-1.qfasl.1'
hcuot "Mar 29 08:21:02 1983" 'file/magtape-14-2.lisp.1'
hcuot "Apr 25 09:51:40 1983" 'file/magtape-14-3.lisp.1'
hcuot "Apr 25 09:51:48 1983" 'file/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:18 1983" 'file/magtape-14-4.lisp.3'
hcuot "May 19 04:11:35 1983" 'file/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:05 1983" 'file/magtape-14-5.lisp.1'
hcuot "Oct 26 20:41:17 1983" 'file/magtape-14-5.qfasl.1'
hcuot "Jan  3 05:38:57 1984" 'file/mtaux.lisp.76'
hcuot "Jan  3 08:48:40 1984" 'file/mtaux.lisp.77'
hcuot "Jun 20 06:21:53 1983" 'file/mtdefs.lisp.28'
hcuot "Dec 16 15:34:10 1983" 'file/mtdefs.lisp.30'
hcuot "Jan  2 05:28:11 1984" 'file/mtstr.lisp.84'
hcuot "Jan  3 08:49:38 1984" 'file/mtstr.lisp.85'
hcuot "Jan  3 08:50:55 1984" 'file/odump.lisp.1'
hcuot "Dec 18 04:58:53 1983" 'file/server.directory.5'
hcuot "Sep 15 02:35:51 1984" 'file/server.directory.6'
hcuot "Jan 18 16:30:07 1984" 'file/server.lisp.147'
hcuot "Feb 19 13:02:22 1984" 'file/server.lisp.149'
hcuot "May 26 12:36:18 1984" 'file/server.lisp.151'
hcuot "Sep 15 02:44:54 1984" 'file/server.lisp.152'
hcuot "Sep 15 02:46:21 1984" 'file/server.qfasl.152'
hcuot "May 26 12:40:07 1984" 'file/server-8.directory.13'
hcuot "May 26 12:42:23 1984" 'file/server-8.directory.14'
hcuot "Jan  4 06:49:04 1984" 'file/server-8-1.lisp.1'
hcuot "Jan  4 06:49:14 1984" 'file/server-8-1.qfasl.1'
hcuot "Jan  5 00:06:06 1984" 'file/server-8-2.lisp.1'
hcuot "Jan  5 00:06:20 1984" 'file/server-8-2.qfasl.1'
hcuot "May 26 12:36:29 1984" 'file/server-8-3.lisp.4'
hcuot "May 26 12:36:34 1984" 'file/server-8-3.qfasl.4'
hcuot "Feb 16 15:28:03 1984" 'file/server-8-4.lisp.1'
hcuot "Feb 16 15:28:08 1984" 'file/server-8-4.qfasl.1'
hcuot "May 26 12:39:28 1984" 'file/server-8-5.lisp.2'
hcuot "May 26 12:39:34 1984" 'file/server-8-5.qfasl.2'
hcuot "Sep 15 02:35:57 1984" 'file/server-9.directory.1'
hcuot "Apr  7 15:15:26 1984" 'file/zmail.lisp.4'
hcuot "Jul 13 07:23:33 1984" 'file/zmail.lisp.5'
hcuot "Sep  9 20:30:21 1984" 'file/zmail.qfasl.5'
hcuot "Dec 18 22:58:54 1983" 'file2/anydir.lisp.200'
hcuot "Jan  4 01:55:28 1984" 'file2/anydir.lisp.201'
hcuot "Aug  3 07:41:49 1984" 'file2/anydir.qfasl.201'
hcuot "Dec 18 10:21:17 1983" 'file2/area.lisp.21'
hcuot "Jan 18 17:35:23 1984" 'file2/area.lisp.22'
hcuot "Aug  3 07:33:44 1984" 'file2/area.qfasl.22'
hcuot "Jul 22 15:59:30 1982" 'file2/bfsplm.text.4'
hcuot "Dec 25 06:00:34 1984" 'file2/bug-lmfile-mail.txt.1'
hcuot "Jul 22 15:59:57 1982" 'file2/chgnod.lisp.5'
hcuot "Aug  3 07:40:05 1984" 'file2/chgnod.qfasl.5'
hcuot "Jan 19 11:25:43 1983" 'file2/complt.lisp.16'
hcuot "Jan 18 17:46:24 1984" 'file2/complt.lisp.17'
hcuot "Aug  3 07:50:25 1984" 'file2/complt.qfasl.17'
hcuot "Dec 18 22:52:20 1983" 'file2/defs.lisp.184'
hcuot "May 13 00:06:24 1984" 'file2/defs.lisp.186'
hcuot "Aug  3 07:19:33 1984" 'file2/defs.lisp.190'
hcuot "Nov 21 19:17:17 1984" 'file2/defs.qfasl.190'
hcuot "Jan 18 17:47:44 1984" 'file2/diread.lisp.60'
hcuot "Jan 29 07:16:03 1984" 'file2/diread.lisp.61'
hcuot "Aug  3 07:48:32 1984" 'file2/diread.qfasl.61'
hcuot "Jul 22 16:02:31 1982" 'file2/doc.text.12'
hcuot "Dec 18 22:52:09 1983" 'file2/dump.lisp.28'
hcuot "Jan 18 17:50:00 1984" 'file2/dump.lisp.29'
hcuot "Aug  3 07:56:23 1984" 'file2/dump.qfasl.29'
hcuot "Dec 18 22:59:11 1983" 'file2/files.lisp.121'
hcuot "Jan 18 17:34:11 1984" 'file2/files.lisp.122'
hcuot "Aug  3 07:30:22 1984" 'file2/files.qfasl.122'
hcuot "Dec 18 10:21:27 1983" 'file2/free.lisp.47'
hcuot "Dec 18 10:50:47 1983" 'file2/free.lisp.48'
hcuot "Aug  3 07:26:33 1984" 'file2/free.qfasl.48'
hcuot "Nov 16 09:43:46 1984" 'file2/fs-fc-mail.txt.1'
hcuot "Jan 20 07:10:20 1983" 'file2/gc.lisp.18'
hcuot "Jan 18 17:30:16 1984" 'file2/gc.lisp.19'
hcuot "Aug  3 07:32:53 1984" 'file2/gc.qfasl.19'
hcuot "Dec 18 22:59:16 1983" 'file2/io.lisp.93'
hcuot "Jan 18 17:29:20 1984" 'file2/io.lisp.94'
hcuot "Aug  3 07:28:01 1984" 'file2/io.qfasl.94'
hcuot "Jan 20 05:29:26 1983" 'file2/link.lisp.45'
hcuot "Feb  1 03:07:28 1984" 'file2/link.lisp.47'
hcuot "Aug  3 07:37:26 1984" 'file2/link.qfasl.47'
hcuot "Dec 18 10:09:21 1983" 'file2/lmfile.directory.3'
hcuot "Aug  3 08:02:34 1984" 'file2/lmfile.directory.4'
hcuot "Sep 30 07:33:31 1983" 'file2/lmfile-2.directory.10'
hcuot "Jun 16 23:34:55 1983" 'file2/lmfile-2-1.lisp.1'
hcuot "Jul  6 22:59:00 1983" 'file2/lmfile-2-2.lisp.1'
hcuot "Jul  7 07:33:09 1983" 'file2/lmfile-2-3.lisp.2'
hcuot "Sep 30 07:32:51 1983" 'file2/lmfile-2-4.lisp.1'
hcuot "Jan 29 08:54:05 1984" 'file2/lmfile-3.directory.7'
hcuot "Jan  4 01:56:37 1984" 'file2/lmfile-3-1.lisp.1'
hcuot "Jan  4 01:56:48 1984" 'file2/lmfile-3-1.qfasl.1'
hcuot "Jan 29 08:53:39 1984" 'file2/lmfile-3-3.lisp.1'
hcuot "Jan 29 08:53:44 1984" 'file2/lmfile-3-3.qfasl.1'
hcuot "Aug  3 08:02:37 1984" 'file2/lmfile-4.directory.1'
hcuot "Jul 22 16:07:13 1982" 'file2/maint.text.9'
hcuot "Jul  7 07:32:09 1983" 'file2/maiser.lisp.9'
hcuot "Nov  8 07:24:01 1984" 'file2/maiser.lisp.12'
hcuot "Dec  2 15:30:00 1984" 'file2/maiser.lisp.13'
hcuot "Aug  3 08:01:34 1984" 'file2/maiser.qfasl.9'
hcuot "Dec 18 22:59:05 1983" 'file2/node.lisp.161'
hcuot "Jan 18 17:37:55 1984" 'file2/node.lisp.162'
hcuot "Aug  3 07:34:26 1984" 'file2/node.qfasl.162'
hcuot "Dec 18 22:59:29 1983" 'file2/pack.lisp.81'
hcuot "Nov 21 18:30:20 1984" 'file2/pack.lisp.83'
hcuot "Nov 21 19:37:41 1984" 'file2/pack.qfasl.83'
hcuot "Oct 30 06:32:35 1983" 'file2/pathnm.lisp.159'
hcuot "Jan 18 17:27:37 1984" 'file2/pathnm.lisp.161'
hcuot "Aug 13 20:14:51 1984" 'file2/pathnm.lisp.162'
hcuot "Nov 21 05:43:10 1984" 'file2/pathnm.lisp.163'
hcuot "Sep 11 06:27:31 1984" 'file2/pathnm.qfasl.162'
hcuot "Jan 20 05:29:32 1983" 'file2/pdp10.lisp.19'
hcuot "Dec 18 22:59:00 1983" 'file2/pdp10.lisp.20'
hcuot "Aug  3 07:38:34 1984" 'file2/pdp10.qfasl.20'
hcuot "Nov 23 09:57:38 1982" 'file2/remote.directory.10'
hcuot "Dec 12 07:15:02 1982" 'file2/remote.lisp.30'
hcuot "Dec 30 05:30:12 1982" 'file2/remote-23.directory.4'
hcuot "Jan 29 08:53:17 1984" 'file2/repair.lisp.1'
hcuot "Dec 18 10:20:56 1983" 'file2/rmdefs.lisp.10'
hcuot "Aug  3 09:02:57 1984" 'file2/rmdefs.qfasl.10'
hcuot "Dec 18 22:52:11 1983" 'file2/salvag.lisp.21'
hcuot "Jan 18 17:42:38 1984" 'file2/salvag.lisp.22'
hcuot "Aug  3 07:40:37 1984" 'file2/salvag.qfasl.22'
hcuot "Aug  3 08:37:58 1984" 'file2/server.lisp.45'
hcuot "Nov  8 11:07:49 1984" 'file2/server.lisp.46'
hcuot "Nov 21 21:59:39 1984" 'file2/server.lisp.52'
hcuot "Nov 21 19:41:01 1984" 'file2/server.qfasl.50'
hcuot "Dec 18 22:51:53 1983" 'file2/spcdir.lisp.91'
hcuot "Jan 18 17:45:18 1984" 'file2/spcdir.lisp.92'
hcuot "Aug  3 07:45:33 1984" 'file2/spcdir.qfasl.92'
hcuot "Jan  4 01:55:10 1984" 'file2/stream.lisp.208'
hcuot "Jan 18 17:49:14 1984" 'file2/stream.lisp.209'
hcuot "Aug  3 08:54:13 1984" 'file2/stream.lisp.210'
hcuot "Aug  3 08:57:08 1984" 'file2/stream.qfasl.210'
hcuot "Apr  8 11:43:36 1983" 'file2/system.lisp.31'
hcuot "Apr  7 15:16:07 1984" 'file2/system.lisp.32'
hcuot "May 23 03:23:41 1984" 'file2/system.qfasl.32'
hcuot "Jul 22 16:18:48 1982" 'file2/view.text.9'
hcuot "Jan 18 17:22:34 1984" 'file2/xrmdefs.lisp.11'
hcuot "Jan 18 17:05:07 1984" 'file2/xserver.lisp.44'
hcuot "Jun 18 05:38:36 1984" 'fonts/13fgb.qfasl.6'
hcuot "Jun 18 05:38:32 1984" 'fonts/16fg.qfasl.5'
hcuot "Jun 18 05:38:21 1984" 'fonts/18fg.qfasl.5'
hcuot "Jun 18 05:38:12 1984" 'fonts/20vr.qfasl.5'
hcuot "Jun 18 05:38:08 1984" 'fonts/25fr3.qfasl.5'
hcuot "Jun 18 05:38:03 1984" 'fonts/31vr.qfasl.5'
hcuot "Jun 18 05:37:56 1984" 'fonts/40vr.qfasl.5'
hcuot "Jun 18 05:37:52 1984" 'fonts/40vshd.qfasl.5'
hcuot "Jun 18 05:37:43 1984" 'fonts/43vxms.qfasl.10'
hcuot "Jun 18 05:37:40 1984" 'fonts/5x5.qfasl.10'
hcuot "Jun 18 05:37:37 1984" 'fonts/abacus.qfasl.5'
hcuot "Jun 18 05:37:34 1984" 'fonts/apl14.qfasl.4'
hcuot "Jun 18 05:37:30 1984" 'fonts/arr10.qfasl.5'
hcuot "Sep 10 20:20:10 1984" 'fonts/bigfnt.qfasl.10'
hcuot "Jul 20 14:17:55 1982" 'fonts/bigold.qfasl.1'
hcuot "Jun 18 05:37:22 1984" 'fonts/bigvg.qfasl.4'
hcuot "Jun 18 05:37:20 1984" 'fonts/color-5x5.qfasl.4'
hcuot "Jun 18 05:37:16 1984" 'fonts/color-cptfont.qfasl.3'
hcuot "Jun 18 05:37:12 1984" 'fonts/color-medfnt.qfasl.4'
hcuot "Jun 18 05:37:07 1984" 'fonts/color-mouse.qfasl.4'
hcuot "Jun 18 05:37:04 1984" 'fonts/courier.qfasl.5'
hcuot "Jul 20 14:18:26 1982" 'fonts/cptfon.qfasl.3'
hcuot "Jun 18 05:37:01 1984" 'fonts/cptfont.qfasl.19'
hcuot "Jun 18 05:36:58 1984" 'fonts/cptfontb.qfasl.7'
hcuot "Jun 18 05:36:54 1984" 'fonts/cyr.qfasl.5'
hcuot "Jun 18 05:36:50 1984" 'fonts/cyr12.qfasl.5'
hcuot "Jun 18 05:36:46 1984" 'fonts/ent.qfasl.5'
hcuot "Dec 10 23:35:35 1983" 'fonts/equivalence.lisp.1'
hcuot "Jun 18 05:36:42 1984" 'fonts/gach10.qfasl.3'
hcuot "Jun 18 05:36:35 1984" 'fonts/gach10b.qfasl.3'
hcuot "Jun 18 05:36:27 1984" 'fonts/gach12.qfasl.3'
hcuot "Jul 20 14:18:41 1982" 'fonts/gfr.archiv.1'
hcuot "Jun 18 05:36:20 1984" 'fonts/hippo10.qfasl.4'
hcuot "Jun 18 05:36:14 1984" 'fonts/hippo18.qfasl.3'
hcuot "Jun 18 05:36:11 1984" 'fonts/hl10.qfasl.9'
hcuot "Jun 18 05:36:07 1984" 'fonts/hl10b.qfasl.9'
hcuot "Jun 18 05:36:03 1984" 'fonts/hl12.qfasl.10'
hcuot "Jun 18 05:35:59 1984" 'fonts/hl12b.qfasl.15'
hcuot "Jun 18 05:35:56 1984" 'fonts/hl12b1.qfasl.3'
hcuot "Jun 18 05:35:52 1984" 'fonts/hl12bi.qfasl.10'
hcuot "Jun 18 05:35:49 1984" 'fonts/hl12i.qfasl.11'
hcuot "Jun 18 05:35:45 1984" 'fonts/hl18.qfasl.6'
hcuot "Jun 18 05:35:42 1984" 'fonts/hl6.qfasl.9'
hcuot "Jun 18 05:35:39 1984" 'fonts/hl7.qfasl.9'
hcuot "Jun 18 05:35:34 1984" 'fonts/icons.qfasl.3'
hcuot "Jun 18 05:35:30 1984" 'fonts/invisible.qfasl.3'
hcuot "Jun 18 05:35:27 1984" 'fonts/medfnb.qfasl.8'
hcuot "Jun 18 05:35:17 1984" 'fonts/medfnt.qfasl.9'
hcuot "Jun 18 05:35:13 1984" 'fonts/mets.qfasl.9'
hcuot "Jun 18 05:35:07 1984" 'fonts/metsi.qfasl.9'
hcuot "Jun 18 05:35:00 1984" 'fonts/mit.qfasl.5'
hcuot "Jun 18 05:34:56 1984" 'fonts/mouse.qfasl.9'
hcuot "Jun 18 05:34:53 1984" 'fonts/narrow.qfasl.5'
hcuot "Jun 18 05:34:50 1984" 'fonts/panes.qfasl.3'
hcuot "Nov 15 12:23:32 1983" 'fonts/prt12b.qfasl.2'
hcuot "Jun 18 05:34:45 1984" 'fonts/s30chs.qfasl.5'
hcuot "Jun 18 05:34:41 1984" 'fonts/s35ger.qfasl.3'
hcuot "Jun 18 05:34:35 1984" 'fonts/sail12.qfasl.6'
hcuot "Jun 18 05:34:32 1984" 'fonts/search.qfasl.9'
hcuot "Jun 18 05:34:28 1984" 'fonts/ship.qfasl.6'
hcuot "Oct  9 11:53:23 1984" 'fonts/storybook.qfasl.1'
hcuot "Oct  9 11:54:06 1984" 'fonts/storybookbold.qfasl.1'
hcuot "Jun 18 05:34:24 1984" 'fonts/tally.qfasl.5'
hcuot "Jul 20 14:20:57 1982" 'fonts/times.9rom.1'
hcuot "Jun 18 05:34:20 1984" 'fonts/tiny.qfasl.5'
hcuot "Jun 18 05:34:15 1984" 'fonts/tog.qfasl.5'
hcuot "Jun 18 05:34:12 1984" 'fonts/tr10.qfasl.9'
hcuot "Jun 18 05:34:09 1984" 'fonts/tr10b.qfasl.8'
hcuot "Jun 18 05:34:04 1984" 'fonts/tr10bi.qfasl.7'
hcuot "Jun 18 05:34:00 1984" 'fonts/tr10i.qfasl.7'
hcuot "Jun 18 05:33:57 1984" 'fonts/tr10ic.qfasl.4'
hcuot "Jun 18 05:33:53 1984" 'fonts/tr12.qfasl.11'
hcuot "Jun 18 05:33:49 1984" 'fonts/tr12b.qfasl.17'
hcuot "Jun 18 05:33:45 1984" 'fonts/tr12b1.qfasl.8'
hcuot "Jun 18 05:33:42 1984" 'fonts/tr12bi.qfasl.9'
hcuot "Jun 18 05:33:37 1984" 'fonts/tr12i.qfasl.13'
hcuot "Jun 18 05:33:30 1984" 'fonts/tr18.qfasl.7'
hcuot "Jun 18 05:33:16 1984" 'fonts/tr18b.qfasl.3'
hcuot "Jun 18 05:33:12 1984" 'fonts/tr8.qfasl.8'
hcuot "Jun 18 05:33:07 1984" 'fonts/tr8b.qfasl.8'
hcuot "Jun 18 05:33:04 1984" 'fonts/tr8i.qfasl.6'
hcuot "Jun 18 05:33:00 1984" 'fonts/tvbug.qfasl.5'
hcuot "Jun 18 05:32:57 1984" 'fonts/tvfont.qfasl.7'
hcuot "Jun 18 05:32:50 1984" 'fonts/worm.qfasl.4'
hcuot "May  1 13:39:06 1984" 'io/crdtbl.lisp.25'
hcuot "Aug 31 17:21:00 1984" 'io/crdtbl.lisp.34'
hcuot "Sep 15 08:28:36 1984" 'io/crdtbl.lisp.35'
hcuot "Aug 31 17:23:39 1984" 'io/crdtbl.qfasl.1'
hcuot "Feb 17 21:06:25 1984" 'io/disk.lisp.279'
hcuot "May 16 11:26:03 1984" 'io/disk.lisp.280'
hcuot "Jul 26 14:58:27 1984" 'io/disk.lisp.286'
hcuot "Nov 21 18:02:41 1984" 'io/disk.lisp.291'
hcuot "Nov 28 00:27:38 1984" 'io/disk.lisp.292'
hcuot "Nov 21 19:22:39 1984" 'io/disk.qfasl.291'
hcuot "Mar 13 07:13:51 1984" 'io/dledit.lisp.51'
hcuot "May 20 13:21:35 1984" 'io/dledit.lisp.52'
hcuot "Nov 21 19:34:29 1984" 'io/dledit.qfasl.52'
hcuot "May  6 04:06:55 1984" 'io/dribbl.lisp.35'
hcuot "Jun  4 08:50:29 1984" 'io/dribbl.lisp.36'
hcuot "Aug 15 04:10:20 1984" 'io/dribbl.qfasl.36'
hcuot "Aug 30 21:58:54 1984" 'io/find-plausible-partitions.lisp.1'
hcuot "Dec 20 19:39:53 1983" 'io/format.lisp.208'
hcuot "Aug  2 18:33:09 1984" 'io/format.lisp.234'
hcuot "Oct 28 21:22:16 1984" 'io/format.lisp.237'
hcuot "Nov 14 10:56:50 1984" 'io/format.lisp.240'
hcuot "Sep  7 22:25:02 1984" 'io/format.qfasl.234'
hcuot "Jan 22 15:20:39 1983" 'io/format-macro.lisp.2'
hcuot "Aug  3 06:29:26 1984" 'io/format-macro.qfasl.2'
hcuot "Oct 29 23:51:56 1983" 'io/fread.lisp.29'
hcuot "Dec 10 12:33:13 1984" 'io/fread.lisp.30'
hcuot "Dec 11 06:04:57 1984" 'io/fread.qfasl.30'
hcuot "Oct  8 02:43:00 1983" 'io/grind.lisp.140'
hcuot "Nov 29 22:17:27 1983" 'io/grind.lisp.143'
hcuot "Aug 10 08:29:02 1984" 'io/grind.lisp.146'
hcuot "Aug 29 23:06:51 1984" 'io/grind.qfasl.145'
hcuot "Apr 29 00:22:48 1984" 'io/print.lisp.171'
hcuot "Jul 28 19:18:25 1984" 'io/print.lisp.177'
hcuot "Oct 30 01:44:25 1984" 'io/print.lisp.182'
hcuot "Sep 10 07:18:21 1984" 'io/print.qfasl.178'
hcuot "Apr 21 21:05:42 1984" 'io/qio.lisp.202'
hcuot "Jun 14 00:36:43 1984" 'io/qio.lisp.210'
hcuot "Dec  9 09:44:53 1984" 'io/qio.lisp.217'
hcuot "Aug 31 05:38:38 1984" 'io/qio.qfasl.214'
hcuot "Jun 29 10:43:34 1982" 'io/rcomp.lisp.9'
hcuot "Apr  7 15:18:03 1984" 'io/rcomp.lisp.10'
hcuot "Mar 18 19:49:19 1984" 'io/rddefs.lisp.42'
hcuot "May 14 09:31:39 1984" 'io/rddefs.lisp.51'
hcuot "Nov  1 11:32:34 1984" 'io/rddefs.lisp.62'
hcuot "Sep  7 22:28:41 1984" 'io/rddefs.qfasl.61'
hcuot "Dec 26 09:06:27 1983" 'io/rdtbl.lisp.156'
hcuot "May 10 11:30:04 1984" 'io/rdtbl.lisp.162'
hcuot "Jun 29 16:45:33 1984" 'io/rdtbl.lisp.167'
hcuot "Sep 15 08:33:22 1984" 'io/rdtbl.lisp.169'
hcuot "Sep 15 08:32:16 1984" 'io/rdtbl.qfasl.167'
hcuot "Dec 27 08:58:53 1983" 'io/read.lisp.403'
hcuot "Jun 20 17:32:13 1984" 'io/read.lisp.428'
hcuot "Oct 10 12:03:07 1984" 'io/read.lisp.435'
hcuot "Nov 20 19:22:01 1984" 'io/read.lisp.437'
hcuot "Aug 15 06:03:40 1984" 'io/read.qfasl.432'
hcuot "Dec 15 21:52:45 1983" 'io/rtc.lisp.34'
hcuot "Apr 30 02:27:56 1984" 'io/rtc.lisp.40'
hcuot "Dec  6 10:14:19 1984" 'io/rtc.lisp.47'
hcuot "Sep 10 05:53:33 1984" 'io/rtc.qfasl.46'
hcuot "Jan 27 05:19:23 1984" 'io/simple-ether.lisp.1'
hcuot "Oct 12 21:07:01 1983" 'io/stream.lisp.103'
hcuot "Jan  1 11:19:35 1984" 'io/stream.lisp.108'
hcuot "Nov 10 10:24:31 1984" 'io/stream.lisp.109'
hcuot "Sep  4 05:40:02 1984" 'io/stream.qfasl.108'
hcuot "Oct 30 05:27:53 1983" 'io/strmdoc.lisp.2'
hcuot "Oct 12 21:10:51 1983" 'io/unibus.lisp.24'
hcuot "Apr 10 16:02:57 1984" 'io/unibus.lisp.25'
hcuot "Dec  1 03:13:54 1984" 'io/unibus.lisp.26'
hcuot "Aug 15 06:24:34 1984" 'io/unibus.qfasl.25'
hcuot "Sep 12 05:34:52 1984" 'io/access.lisp.9'	   # was 'io/file/access.lisp.9'
hcuot "Oct  9 09:11:17 1984" 'io/access.lisp.11'   # was 'io/file/access.lisp.11'
hcuot "Nov 29 06:55:23 1984" 'io/access.lisp.13'   # was 'io/file/access.lisp.13'
hcuot "Sep 11 07:23:13 1984" 'io/access.qfasl.8'   # was 'io/file/access.qfasl.8'
hcuot "Oct 25 04:23:11 1983" 'io/baldir.lisp.114'  # was 'io/file/baldir.lisp.114'
hcuot "Aug 15 08:28:24 1984" 'io/baldir.qfasl.114' # was 'io/file/baldir.qfasl.114'
hcuot "Dec 20 22:06:18 1983" 'io/open.lisp.136'	   # was 'io/file/open.lisp.136'
hcuot "Apr 28 22:53:58 1984" 'io/open.lisp.158'	   # was 'io/file/open.lisp.158'
hcuot "Aug  5 05:03:33 1984" 'io/open.lisp.172'	   # was 'io/file/open.lisp.172'
hcuot "Sep 12 05:09:45 1984" 'io/open.lisp.175'	   # was 'io/file/open.lisp.175'
hcuot "Nov 28 12:37:30 1984" 'io/open.lisp.179'	   # was 'io/file/open.lisp.179'
hcuot "Sep 11 06:36:49 1984" 'io/open.qfasl.174'   # was 'io/file/open.qfasl.174'
hcuot "Sep 13 22:45:55 1984" 'io/pathnm.lisp.531'  # was 'io/file/pathnm.lisp.531'
hcuot "Nov 21 05:41:36 1984" 'io/pathnm.lisp.534'  # was 'io/file/pathnm.lisp.534'
hcuot "Nov 29 06:58:22 1984" 'io/pathnm.lisp.535'  # was 'io/file/pathnm.lisp.535'
hcuot "Dec 14 13:43:08 1984" 'io/pathnm.lisp.536'  # was 'io/file/pathnm.lisp.536'
hcuot "Sep  7 23:08:16 1984" 'io/pathnm.qfasl.528' # was 'io/file/pathnm.qfasl.528'
hcuot "Dec 24 03:29:18 1983" 'io/pathst.lisp.135'  # was 'io/file/pathst.lisp.135'
hcuot "Jun  9 13:23:35 1984" 'io/pathst.lisp.151'  # was 'io/file/pathst.lisp.151'
hcuot "Aug 13 20:55:06 1984" 'io/pathst.lisp.170'  # was 'io/file/pathst.lisp.170'
hcuot "Sep  5 18:53:30 1984" 'io/pathst.lisp.173'  # was 'io/file/pathst.lisp.173'
hcuot "Nov 21 05:42:30 1984" 'io/pathst.lisp.179'  # was 'io/file/pathst.lisp.179'
hcuot "Sep  7 23:13:26 1984" 'io/pathst.qfasl.173' # was 'io/file/pathst.qfasl.173'
hcuot "Jun 29 10:46:28 1982" 'io1/10leaf.points.1'
hcuot "Mar 17 08:10:51 1983" 'io1/as8748.lisp.40'
hcuot "Sep 16 11:09:14 1982" 'io1/as8751.lisp.29'
hcuot "Oct  3 03:12:52 1982" 'io1/cdrive.lisp.102'
hcuot "Apr  7 15:18:57 1984" 'io1/cdrive.lisp.103'
hcuot "Oct 12 20:34:02 1983" 'io1/chatst.lisp.65'
hcuot "Apr  7 15:19:19 1984" 'io1/chatst.lisp.66'
hcuot "Jan  4 09:20:29 1984" 'io1/conver.lisp.145'
hcuot "Jun 24 18:02:29 1984" 'io1/conver.lisp.146'
hcuot "Sep 11 21:11:30 1984" 'io1/conver.lisp.147'
hcuot "Sep 11 21:37:37 1984" 'io1/conver.qfasl.147'
hcuot "Jun 29 10:49:20 1982" 'io1/door.bin.1'
hcuot "Jun 29 10:49:15 1982" 'io1/door.text.2'
hcuot "Jun 18 21:30:10 1983" 'io1/dplt.lisp.108'
hcuot "Jan 28 01:44:08 1984" 'io1/dplt.lisp.109'
hcuot "Sep 16 11:08:40 1982" 'io1/draw.lisp.22'
hcuot "Apr  7 15:21:38 1984" 'io1/draw.lisp.23'
hcuot "Feb 13 01:04:09 1984" 'io1/eftp.bin-4.1'
hcuot "Nov 28 23:44:32 1983" 'io1/eftp.bin-5.1'
hcuot "Nov 28 23:43:10 1983" 'io1/eftp.lisp.37'
hcuot "Dec 18 20:24:31 1983" 'io1/eftp.lisp.38'
hcuot "Dec 11 13:01:42 1983" 'io1/fntcnv.lisp.79'
hcuot "Jun  5 06:27:46 1984" 'io1/fntcnv.lisp.83'
hcuot "Aug 30 00:16:54 1984" 'io1/fntcnv.qfasl.83'
hcuot "Apr  7 15:22:34 1984" 'io1/fntdef.lisp.20'
hcuot "May 10 09:55:58 1984" 'io1/fquery.lisp.38'
hcuot "Jul  3 22:30:24 1984" 'io1/fquery.lisp.43'
hcuot "Jul 26 08:34:19 1984" 'io1/fquery.lisp.45'
hcuot "Aug  3 06:30:55 1984" 'io1/fquery.qfasl.45'
hcuot "Jun 29 10:50:23 1982" 'io1/hacks.lisp.190'
hcuot "Jun 27 12:05:25 1984" 'io1/hardcopy.lisp.1'
hcuot "Aug 15 05:53:21 1984" 'io1/hardcopy.qfasl.1'
hcuot "Aug 24 16:56:29 1983" 'io1/inc.lisp.8'
hcuot "Aug 15 04:58:05 1984" 'io1/inc.qfasl.8'
hcuot "Feb  4 12:35:07 1984" 'io1/infix.lisp.6'
hcuot "Jun 13 22:49:50 1984" 'io1/infix.lisp.11'
hcuot "Aug 15 04:59:17 1984" 'io1/infix.qfasl.11'
hcuot "Feb 17 20:38:14 1984" 'io1/meter.lisp.37'
hcuot "Jun 24 17:23:23 1984" 'io1/meter.lisp.42'
hcuot "Aug 30 03:21:38 1984" 'io1/meter.qfasl.42'
hcuot "Jun 29 10:50:59 1982" 'io1/mouse.text.11'
hcuot "Mar  9 21:49:43 1984" 'io1/output.lisp.35'
hcuot "Aug  3 07:01:51 1984" 'io1/output.lisp.38'
hcuot "Sep  7 22:29:43 1984" 'io1/output.qfasl.38'
hcuot "Nov 28 23:41:27 1983" 'io1/press.bin-5.3'
hcuot "Nov 28 23:40:52 1983" 'io1/press.lisp.141'
hcuot "May 16 21:52:13 1984" 'io1/press.lisp.144'
hcuot "Jun  5 09:08:53 1984" 'io1/press.lisp.146'
hcuot "Oct 13 05:30:20 1984" 'io1/press.lisp.147'
hcuot "Aug 30 03:16:40 1984" 'io1/press.qfasl.146'
hcuot "Apr  7 15:25:35 1984" 'io1/promp.lisp.13'
hcuot "Dec 22 05:03:38 1983" 'io1/reldmp.lisp.11'
hcuot "Aug  3 12:06:58 1984" 'io1/reldmp.lisp.12'
hcuot "Aug  3 22:01:31 1984" 'io1/reldmp.qfasl.12'
hcuot "Jul 22 10:54:42 1983" 'io1/relld.lisp.9'
hcuot "Apr  7 15:29:25 1984" 'io1/relld.lisp.10'
hcuot "Sep  7 17:09:42 1984" 'io1/rfontw.bin-5.1'
hcuot "Nov 28 22:09:47 1983" 'io1/rfontw.lisp.75'
hcuot "Apr  7 15:39:04 1984" 'io1/rfontw.lisp.78'
hcuot "Jun  5 07:00:45 1984" 'io1/rfontw.lisp.80'
hcuot "Sep  6 22:22:47 1984" 'io1/rfontw.lisp.82'
hcuot "Sep  6 22:25:03 1984" 'io1/rfontw.qfasl.82'
hcuot "Nov 28 22:09:46 1983" 'io1/rfontx.lisp.75'
hcuot "Sep  6 22:16:41 1984" 'io1/rfontx.qfasl.75'
hcuot "Oct 12 20:49:03 1983" 'io1/serial.lisp.30'
hcuot "May 12 06:21:02 1984" 'io1/serial.lisp.32'
hcuot "Aug 15 06:11:14 1984" 'io1/serial.qfasl.32'
hcuot "Feb 29 11:44:41 1984" 'io1/srccom.lisp.33'
hcuot "May 25 21:17:24 1984" 'io1/srccom.lisp.36'
hcuot "Jul  2 16:55:54 1984" 'io1/srccom.lisp.37'
hcuot "Aug 30 03:24:08 1984" 'io1/srccom.qfasl.37'
hcuot "Apr 23 11:39:47 1984" 'io1/swar.lisp.12'
hcuot "Jan 29 23:10:00 1984" 'io1/time.lisp.84'
hcuot "Mar  9 16:15:22 1984" 'io1/time.lisp.95'
hcuot "Jul  3 17:46:39 1984" 'io1/time.lisp.105'
hcuot "Nov  6 12:48:06 1984" 'io1/time.lisp.110'
hcuot "Aug  3 11:46:51 1984" 'io1/time.qfasl.105'
hcuot "Dec 15 08:43:32 1983" 'io1/timpar.lisp.68'
hcuot "Feb 16 16:07:45 1984" 'io1/timpar.lisp.72'
hcuot "Oct 20 19:28:27 1984" 'io1/timpar.lisp.75'
hcuot "Aug  3 11:48:41 1984" 'io1/timpar.qfasl.74'
hcuot "Jun 29 10:55:20 1982" 'io1/ukbd.lisp.24'
hcuot "Jun 29 10:55:35 1982" 'io1/wlr.doc.1'
hcuot "Jul 22 11:15:54 1983" 'io1/xgp.lisp.33'
hcuot "Sep 10 20:20:48 1984" 'io1/xgp.qfasl.33'
hcuot "Aug  1 23:16:45 1982" 'man/.bug.lmman.1'
hcuot "Aug  1 23:17:23 1982" 'man/.dlw.wordab.1'
hcuot "Aug  1 23:17:27 1982" 'man/.forma.text.33'
hcuot "Aug  1 23:17:39 1982" 'man/.machn.compar.1'
hcuot "Aug  1 23:17:42 1982" 'man/30flsp.kst.1'
hcuot "Aug  1 23:17:52 1982" 'man/37vrbl.kst.1'
hcuot "Jan 22 13:30:47 1983" 'man/areas.text.42'
hcuot "May 20 01:12:20 1984" 'man/areas.text.46'
hcuot "Dec  5 06:33:13 1984" 'man/bug-mail.txt.1'
hcuot "Feb 25 12:14:29 1983" 'man/chaos.text.11'
hcuot "Jul 27 06:52:53 1984" 'man/chaos.text.27'
hcuot "May 15 11:12:57 1984" 'man/code.text.35'
hcuot "Jun  1 08:44:46 1984" 'man/code.text.36'
hcuot "Mar 30 12:58:58 1984" 'man/compil.text.98'
hcuot "May 21 06:56:32 1984" 'man/compil.text.103'
hcuot "Jun  4 07:31:51 1984" 'man/compil.text.106'
hcuot "Jun  8 08:33:45 1984" 'man/cumulative.vars.23'
hcuot "Jun  8 08:41:24 1984" 'man/cumulative.vars.24'
hcuot "Mar 31 00:42:46 1984" 'man/db-aid.text.11'
hcuot "Jun  1 07:23:25 1984" 'man/db-aid.text.14'
hcuot "May 15 10:06:19 1984" 'man/debug.text.17'
hcuot "May 21 20:48:55 1984" 'man/debug.text.21'
hcuot "Jun  3 01:58:32 1984" 'man/defstr.text.116'
hcuot "Jun  8 07:14:16 1984" 'man/defstr.text.117'
hcuot "May 16 00:10:23 1984" 'man/errors.text.98'
hcuot "Jun  1 06:32:40 1984" 'man/errors.text.102'
hcuot "May 12 12:05:00 1984" 'man/fd-arr.text.22'
hcuot "May 21 06:53:47 1984" 'man/fd-arr.text.26'
hcuot "May 20 01:12:55 1984" 'man/fd-clo.text.12'
hcuot "May 19 21:21:33 1984" 'man/fd-con.text.27'
hcuot "May 21 01:20:57 1984" 'man/fd-con.text.28'
hcuot "May 21 01:21:21 1984" 'man/fd-dtp.text.18'
hcuot "Jun  5 05:25:24 1984" 'man/fd-dtp.text.19'
hcuot "May 12 10:30:00 1984" 'man/fd-eva.text.36'
hcuot "Jun  1 11:17:42 1984" 'man/fd-eva.text.46'
hcuot "May 11 06:04:25 1984" 'man/fd-fio.text.19'
hcuot "May 21 18:53:05 1984" 'man/fd-fio.text.24'
hcuot "May 16 21:48:07 1984" 'man/fd-flo.text.21'
hcuot "May 19 21:22:03 1984" 'man/fd-flo.text.24'
hcuot "May  3 08:56:02 1984" 'man/fd-fun.text.20'
hcuot "Jun  1 04:27:05 1984" 'man/fd-fun.text.26'
hcuot "May 16 21:07:20 1984" 'man/fd-hac.text.36'
hcuot "Jun  1 09:36:57 1984" 'man/fd-hac.text.47'
hcuot "Mar 14 09:59:33 1984" 'man/fd-loc.text.5'
hcuot "May 20 23:56:54 1984" 'man/fd-loc.text.9'
hcuot "Jun  4 09:29:31 1984" 'man/fd-num.text.36'
hcuot "Jun  8 21:51:15 1984" 'man/fd-num.text.37'
hcuot "Feb  3 09:15:49 1984" 'man/fd-op.text.3'
hcuot "May 19 21:22:43 1984" 'man/fd-op.text.5'
hcuot "May 20 10:25:10 1984" 'man/fd-sg.text.15'
hcuot "Jul 27 06:51:28 1984" 'man/fd-sg.text.16'
hcuot "May 16 21:08:13 1984" 'man/fd-str.text.21'
hcuot "Jun  1 10:34:17 1984" 'man/fd-str.text.27'
hcuot "Mar 14 10:16:25 1984" 'man/fd-sub.text.12'
hcuot "Jun  1 04:32:03 1984" 'man/fd-sub.text.19'
hcuot "May 12 08:37:41 1984" 'man/fd-sym.text.10'
hcuot "May 20 23:56:59 1984" 'man/fd-sym.text.14'
hcuot "May 14 13:42:32 1984" 'man/files.text.14'
hcuot "Jun  1 08:45:51 1984" 'man/files.text.24'
hcuot "Aug  1 23:25:37 1982" 'man/flavor.bolio.1'
hcuot "May  1 06:11:54 1984" 'man/flavor.text.123'
hcuot "Jun  1 11:34:13 1984" 'man/flavor.text.134'
hcuot "Aug  1 23:25:53 1982" 'man/font3.kst.1'
hcuot "May 12 12:24:17 1984" 'man/generic.text.12'
hcuot "May 19 20:30:31 1984" 'man/generic.text.14'
hcuot "Jun  8 06:18:44 1984" 'man/index.temp.1'
hcuot "Jun  8 08:08:53 1984" 'man/index.temp.2'
hcuot "Mar 22 07:21:38 1984" 'man/init.text.14'
hcuot "May 19 20:56:30 1984" 'man/init.text.17'
hcuot "Jun  1 10:32:32 1984" 'man/intro.text.17'
hcuot "Jun  4 07:16:55 1984" 'man/intro.text.18'
hcuot "May  1 22:45:38 1984" 'man/ios.text.241'
hcuot "May 21 18:53:12 1984" 'man/ios.text.247'
hcuot "Aug  1 23:27:18 1982" 'man/looptm.lispm.1'
hcuot "Apr 26 12:50:32 1983" 'man/looptm.lispm.2'
hcuot "Mar 20 07:37:36 1984" 'man/looptm.text.315'
hcuot "Jun  1 04:33:03 1984" 'man/looptm.text.320'
hcuot "May  1 22:46:06 1984" 'man/macros.text.97'
hcuot "May 21 07:37:20 1984" 'man/macros.text.104'
hcuot "May 19 05:54:05 1984" 'man/maksys.text.38'
hcuot "Mar 11 00:43:51 1984" 'man/manual.bolio.25'
hcuot "Jun  1 10:42:32 1984" 'man/manual.fasl.33'
hcuot "Mar 23 09:50:23 1984" 'man/manual.lisp.30'
hcuot "Jun  1 10:30:15 1984" 'man/manual.lisp.33'
hcuot "Jun  1 06:40:13 1984" 'man/manual.text.44'
hcuot "Jun  8 07:37:59 1984" 'man/manual.vars.25'
hcuot "Jun  8 08:31:51 1984" 'man/manual.vars.26'
hcuot "Mar 11 00:21:36 1984" 'man/manual2.bolio.1'
hcuot "Mar 11 00:43:46 1984" 'man/manual2.bolio.2'
hcuot "Jun  8 08:09:29 1984" 'man/manual2.log.6'
hcuot "Mar 23 09:52:08 1984" 'man/manual2.text.7'
hcuot "May 21 20:38:36 1984" 'man/manual2.text.8'
hcuot "Jun  1 06:56:56 1984" 'man/manual2a.10.1'
hcuot "Mar 11 00:43:46 1984" 'man/manual3.bolio.1'
hcuot "Mar 11 00:43:46 1984" 'man/manual3.bolio.2'
hcuot "Mar 12 05:27:45 1984" 'man/manual3.text.1'
hcuot "Jun  1 06:38:17 1984" 'man/manual3a.text.1'
hcuot "Aug  1 23:28:55 1982" 'man/msg.text.8'
hcuot "May 19 06:12:09 1984" 'man/packd.text.104'
hcuot "Jun  1 06:28:39 1984" 'man/packd.text.106'
hcuot "May 19 20:30:22 1984" 'man/patch.text.53'
hcuot "Jun  1 06:29:53 1984" 'man/patch.text.54'
hcuot "Jun  1 10:34:19 1984" 'man/pathnm.text.98'
hcuot "Jun  5 05:31:29 1984" 'man/pathnm.text.99'
hcuot "May 20 01:15:40 1984" 'man/proces.text.55'
hcuot "May 15 11:15:46 1984" 'man/query.text.19'
hcuot "May 20 06:21:25 1984" 'man/query.text.22'
hcuot "May 14 13:42:08 1984" 'man/rdprt.text.20'
hcuot "Jun  1 05:25:06 1984" 'man/rdprt.text.29'
hcuot "May 12 08:31:29 1984" 'man/resour.text.24'
hcuot "May 21 01:20:51 1984" 'man/resour.text.28'
hcuot "Apr  4 02:19:33 1984" 'man/stream.text.33'
hcuot "May 20 06:21:07 1984" 'man/stream.text.37'
hcuot "Oct  7 07:05:57 1982" 'man/testman.bolio.5'
hcuot "Oct  7 02:51:24 1982" 'man/testman.text.2'
hcuot "Mar 31 00:46:11 1984" 'man/time.text.36'
hcuot "May 19 20:30:40 1984" 'man/time.text.40'
hcuot "Jun  1 10:42:15 1984" 'man/title.text.10'
hcuot "Jun  8 04:11:55 1984" 'man/title.text.11'
hcuot "Jul 11 02:29:09 1984" 'network/addr-res.lisp.8'
hcuot "Jan  2 02:34:32 1984" 'network/ether-mini.lisp.10'
hcuot "Nov 21 05:42:13 1984" 'network/host.lisp.120'
hcuot "Nov 28 11:35:58 1984" 'network/host.lisp.121'
hcuot "Sep 10 22:45:38 1984" 'network/host.qfasl.116'
hcuot "Sep  1 15:51:24 1984" 'network/package.lisp.6'
hcuot "Sep  9 03:54:51 1984" 'network/package.lisp.7'
hcuot "Sep  9 05:59:30 1984" 'network/package.qfasl.7'
hcuot "Jul 15 06:00:45 1984" 'network/regions.lisp.1'
hcuot "May 30 07:15:13 1984" 'network/server.lisp.1'
hcuot "May 30 05:54:46 1984" 'network/service.lisp.2'
hcuot "May 30 07:14:54 1984" 'network/service.lisp.3'
hcuot "Jul 13 19:29:53 1984" 'network/simple-ether.lisp.51'
hcuot "Jul 15 06:01:32 1984" 'network/smtp.lisp.1'
hcuot "Jul  4 18:10:43 1984" 'network/symbols.lisp.1'
hcuot "May 30 05:58:43 1984" 'network/symbols.qfasl.1'
hcuot "Dec 14 07:14:16 1984" 'io1/chatst.lisp.67'     # was 'network/chaos/chatst.lisp.67'
hcuot "Jun  6 06:11:05 1984" 'io1/chatst.qfasl.66'    # was 'network/chaos/chatst.qfasl.66'
hcuot "Jun  4 20:56:40 1984" 'io/chsaux.lisp.350'     # was 'network/chaos/chsaux.lisp.350'
hcuot "Aug  8 15:12:21 1984" 'io/chsaux.lisp.359'     # was 'network/chaos/chsaux.lisp.359'
hcuot "Nov  9 20:28:15 1984" 'io/chsaux.lisp.362'     # was 'network/chaos/chsaux.lisp.362'
hcuot "Nov 18 20:15:11 1984" 'io/chsaux.lisp.365'     # was 'network/chaos/chsaux.lisp.365'
hcuot "Dec  6 13:20:08 1984" 'io/chsaux.lisp.366'     # was 'network/chaos/chsaux.lisp.366'
hcuot "Aug 15 07:26:28 1984" 'io/chsaux.qfasl.359'    # was 'network/chaos/chsaux.qfasl.359'
hcuot "Jan 22 19:58:00 1984" 'io/chsncp.lisp.241'     # was 'network/chaos/chsncp.lisp.241'
hcuot "May 20 09:07:01 1984" 'io/chsncp.lisp.242'     # was 'network/chaos/chsncp.lisp.242'
hcuot "Nov 10 09:50:04 1984" 'io/chsncp.lisp.265'     # was 'network/chaos/chsncp.lisp.265'
hcuot "Nov 10 09:50:05 1984" 'io/chsncp.lisp.269'     # was 'network/chaos/chsncp.lisp.269'
hcuot "Sep 11 21:12:45 1984" 'io/chsncp.qfasl.265'    # was 'network/chaos/chsncp.qfasl.265'
hcuot "Jun  4 21:48:49 1984" 'io/chuse.lisp.4'	      # was 'network/chaos/chuse.lisp.4'
hcuot "Sep  4 19:44:32 1984" 'io/chuse.lisp.11'	      # was 'network/chaos/chuse.lisp.11'
hcuot "Oct 20 20:09:37 1984" 'io/chuse.lisp.12'	      # was 'network/chaos/chuse.lisp.12'
hcuot "Nov 26 20:11:26 1984" 'io/chuse.lisp.14'	      # was 'network/chaos/chuse.lisp.14'
hcuot "Sep  4 21:12:29 1984" 'io/chuse.qfasl.11'      # was 'network/chaos/chuse.qfasl.11'
hcuot "Jun  4 20:53:34 1984" 'io1/eftp.lisp.39'	      # was 'network/chaos/eftp.lisp.39'
hcuot "Jun  6 06:12:57 1984" 'io1/eftp.qfasl.39'      # was 'network/chaos/eftp.qfasl.39'
hcuot "Jul  1 21:14:32 1984" 'window/peekch.lisp.30'  # was 'network/chaos/peekch.lisp.30'
hcuot "Sep 10 21:59:11 1984" 'window/peekch.lisp.31'  # was 'network/chaos/peekch.lisp.31'
hcuot "Sep 10 22:44:15 1984" 'window/peekch.qfasl.31' # was 'network/chaos/peekch.qfasl.31'
hcuot "Jan 18 15:59:01 1984" 'io/qfile.lisp.336'      # was 'network/chaos/qfile.lisp.336'
hcuot "Sep 10 21:58:49 1984" 'io/qfile.lisp.353'      # was 'network/chaos/qfile.lisp.353'
hcuot "Nov 21 03:46:45 1984" 'io/qfile.lisp.357'      # was 'network/chaos/qfile.lisp.357'
hcuot "Nov 29 06:54:32 1984" 'io/qfile.lisp.358'      # was 'network/chaos/qfile.lisp.358'
hcuot "Sep 10 22:37:50 1984" 'io/qfile.qfasl.353'     # was 'network/chaos/qfile.qfasl.353'
hcuot "Jul 16 15:26:07 1984" 'network/ip/address.lisp.2'
hcuot "Jul 17 15:27:46 1984" 'network/ip/address.lisp.3'
hcuot "Jul 17 15:27:51 1984" 'network/ip/address.qfasl.3'
hcuot "Jul 17 18:37:11 1984" 'network/ip/hostsnic.lisp.2'
hcuot "Jul 21 07:57:31 1984" 'network/ip/hostsnic.lisp.3'
hcuot "Nov 10 23:02:36 1984" 'network/ip/hostsnic.lisp.4'
hcuot "Nov 16 08:01:27 1984" 'patch/band.win.lisp.1'
hcuot "Nov 16 08:06:05 1984" 'patch/band.win.lisp.2'
hcuot "Nov 16 08:06:21 1984" 'patch/band.win.qfasl.2'
hcuot "Sep 11 23:19:54 1984" 'patch/cadr.patch-directory.1'
hcuot "Jan  3 20:07:15 1985" 'patch/cadr-4.patch-directory.6'
hcuot "Jan  3 20:08:05 1985" 'patch/cadr-4.patch-directory.7'
hcuot "Jan  3 20:13:12 1985" 'patch/cadr-4.patch-directory.8'
hcuot "Dec 10 13:02:15 1984" 'patch/cadr-4-1.lisp.5'
hcuot "Dec 11 12:30:43 1984" 'patch/cadr-4-1.lisp.6'
hcuot "Jan  3 20:03:54 1985" 'patch/cadr-4-1.lisp.7'
hcuot "Dec 10 12:41:09 1984" 'patch/cadr-4-1.qfasl.4'
hcuot "Dec 11 11:26:55 1984" 'patch/cadr-4-1.qfasl.5'
hcuot "Jan  3 20:04:12 1985" 'patch/cadr-4-1.qfasl.7'
hcuot "Jan  3 20:09:24 1985" 'patch/cadr-4-2.lisp.1'
hcuot "Jan  3 20:10:46 1985" 'patch/cadr-4-2.qfasl.1'
hcuot "Nov 16 08:09:19 1984" 'patch/lm27fix.lisp.1'
hcuot "Nov 16 08:09:37 1984" 'patch/lm27fix.qfasl.1'
hcuot "Jun  7 00:30:00 1984" 'patch/system.patch-directory.25'
hcuot "Feb  4 03:12:33 1984" 'patch/system-94.patch-directory.129'
hcuot "Aug 21 21:52:31 1983" 'patch/system-94-41.qfasl.2'
hcuot "Nov  8 09:03:14 1983" 'patch/system-94-42.qfasl.1'
hcuot "Nov 13 05:20:58 1983" 'patch/system-94-43.qfasl.2'
hcuot "Nov 30 02:11:39 1983" 'patch/system-97.patch-directory.76'
hcuot "Nov  9 22:51:17 1983" 'patch/system-97-25.qfasl.1'
hcuot "Nov 11 20:27:52 1983" 'patch/system-97-26.qfasl.1'
hcuot "Nov 29 23:17:29 1983" 'patch/system-97-27.qfasl.1'
hcuot "Nov 30 02:11:04 1983" 'patch/system-97-28.qfasl.1'
hcuot "Nov 28 09:40:52 1984" 'patch/system-98.patch-directory.303'
hcuot "Nov 28 16:02:04 1984" 'patch/system-98.patch-directory.304'
hcuot "Nov 24 00:42:57 1983" 'patch/system-98-1.lisp.5'
hcuot "Dec 23 08:14:14 1983" 'patch/system-98-10.lisp.15'
hcuot "Dec 26 09:56:51 1983" 'patch/system-98-11.lisp.19'
hcuot "Dec 27 07:15:02 1983" 'patch/system-98-12.lisp.16'
hcuot "Dec 24 06:37:44 1983" 'patch/system-98-13.lisp.4'
hcuot "Dec 27 08:58:42 1983" 'patch/system-98-14.lisp.15'
hcuot "Dec 28 10:09:13 1983" 'patch/system-98-15.lisp.7'
hcuot "Dec 29 10:08:59 1983" 'patch/system-98-16.lisp.6'
hcuot "Jan  1 05:30:38 1984" 'patch/system-98-17.lisp.18'
hcuot "Jan  1 15:49:39 1984" 'patch/system-98-18.lisp.10'
hcuot "Jan  3 05:54:10 1984" 'patch/system-98-19.lisp.20'
hcuot "Nov 30 05:36:10 1983" 'patch/system-98-2.lisp.12'
hcuot "Jan  2 07:53:30 1984" 'patch/system-98-20.lisp.4'
hcuot "Jan  3 06:16:42 1984" 'patch/system-98-21.lisp.2'
hcuot "Jan  3 09:49:01 1984" 'patch/system-98-22.lisp.6'
hcuot "Jan  4 10:19:59 1984" 'patch/system-98-23.lisp.10'
hcuot "Jan  3 10:57:14 1984" 'patch/system-98-24.lisp.3'
hcuot "Jan  5 23:39:41 1984" 'patch/system-98-25.lisp.8'
hcuot "Jan  7 12:40:39 1984" 'patch/system-98-26.lisp.6'
hcuot "Jan 12 16:11:12 1984" 'patch/system-98-27.lisp.7'
hcuot "Jan 10 02:43:59 1984" 'patch/system-98-28.lisp.3'
hcuot "Jan 15 03:33:30 1984" 'patch/system-98-29.lisp.12'
hcuot "Dec  6 14:55:14 1983" 'patch/system-98-3.lisp.16'
hcuot "Jan 30 04:20:17 1984" 'patch/system-98-30.lisp.22'
hcuot "Feb  1 10:45:59 1984" 'patch/system-98-31.lisp.18'
hcuot "Jan 27 09:08:05 1984" 'patch/system-98-32.lisp.8'
hcuot "Feb  8 17:54:19 1984" 'patch/system-98-33.lisp.26'
hcuot "Feb  1 13:00:29 1984" 'patch/system-98-34.lisp.1'
hcuot "Feb 15 23:07:07 1984" 'patch/system-98-35.lisp.9'
hcuot "Feb  3 09:30:52 1984" 'patch/system-98-36.lisp.1'
hcuot "Feb 22 23:51:54 1984" 'patch/system-98-37.lisp.10'
hcuot "Mar 12 01:18:12 1984" 'patch/system-98-38.lisp.4'
hcuot "Mar 24 20:37:28 1984" 'patch/system-98-39.lisp.20'
hcuot "Dec  5 14:56:31 1983" 'patch/system-98-4.lisp.7'
hcuot "Apr  3 12:57:33 1984" 'patch/system-98-40.lisp.43'
hcuot "Apr  6 23:33:48 1984" 'patch/system-98-41.lisp.10'
hcuot "Mar 21 21:54:25 1984" 'patch/system-98-42.lisp.2'
hcuot "Mar 15 09:21:58 1984" 'patch/system-98-43.lisp.1'
hcuot "Apr 17 21:47:49 1984" 'patch/system-98-44.lisp.22'
hcuot "Apr 21 23:27:40 1984" 'patch/system-98-45.lisp.5'
hcuot "Apr  6 09:35:55 1984" 'patch/system-98-46.lisp.1'
hcuot "May  8 10:23:06 1984" 'patch/system-98-47.lisp.37'
hcuot "Apr 18 08:16:58 1984" 'patch/system-98-48.lisp.1'
hcuot "May 29 18:44:39 1984" 'patch/system-98-49.lisp.8'
hcuot "Dec  9 04:28:54 1983" 'patch/system-98-5.lisp.11'
hcuot "Jun  6 00:30:09 1984" 'patch/system-98-50.lisp.39'
hcuot "Jun  6 00:34:15 1984" 'patch/system-98-50.qfasl.39'
hcuot "May  1 11:40:21 1984" 'patch/system-98-51.lisp.1'
hcuot "May  1 11:40:31 1984" 'patch/system-98-51.qfasl.1'
hcuot "May 10 09:26:54 1984" 'patch/system-98-52.lisp.1'
hcuot "May 10 09:27:04 1984" 'patch/system-98-52.qfasl.1'
hcuot "May 12 08:33:07 1984" 'patch/system-98-53.lisp.2'
hcuot "May 12 08:33:12 1984" 'patch/system-98-53.qfasl.2'
hcuot "May 23 04:29:09 1984" 'patch/system-98-54.lisp.4'
hcuot "May 23 04:29:30 1984" 'patch/system-98-54.qfasl.4'
hcuot "May 28 07:59:01 1984" 'patch/system-98-55.lisp.3'
hcuot "May 28 07:59:51 1984" 'patch/system-98-55.qfasl.3'
hcuot "May 22 02:26:36 1984" 'patch/system-98-56.lisp.2'
hcuot "May 22 02:26:50 1984" 'patch/system-98-56.qfasl.2'
hcuot "Jun  4 19:45:26 1984" 'patch/system-98-57.lisp.23'
hcuot "Jun  4 20:08:38 1984" 'patch/system-98-57.qfasl.23'
hcuot "May 25 01:26:35 1984" 'patch/system-98-58.lisp.1'
hcuot "May 25 01:26:44 1984" 'patch/system-98-58.qfasl.1'
hcuot "Jun  5 21:11:43 1984" 'patch/system-98-59.lisp.3'
hcuot "Jun  5 21:11:53 1984" 'patch/system-98-59.qfasl.3'
hcuot "Dec 13 22:06:04 1983" 'patch/system-98-6.lisp.17'
hcuot "Jun 13 11:08:07 1984" 'patch/system-98-60.lisp.5'
hcuot "Jun 13 11:08:13 1984" 'patch/system-98-60.qfasl.5'
hcuot "Jun  9 10:10:30 1984" 'patch/system-98-61.lisp.2'
hcuot "Jun  9 10:10:39 1984" 'patch/system-98-61.qfasl.2'
hcuot "Jun 17 10:05:51 1984" 'patch/system-98-62.lisp.12'
hcuot "Jun 20 00:27:54 1984" 'patch/system-98-62.qfasl.12'
hcuot "Jul  2 21:43:53 1984" 'patch/system-98-63.lisp.18'
hcuot "Jul  2 21:44:01 1984" 'patch/system-98-63.qfasl.18'
hcuot "Jun 15 10:19:50 1984" 'patch/system-98-64.lisp.1'
hcuot "Jun 29 09:07:30 1984" 'patch/system-98-64.qfasl.1'
hcuot "Jul  2 21:55:49 1984" 'patch/system-98-65.lisp.10'
hcuot "Jul  2 21:56:02 1984" 'patch/system-98-65.qfasl.10'
hcuot "Jul  9 21:08:09 1984" 'patch/system-98-66.lisp.9'
hcuot "Jul  9 21:08:19 1984" 'patch/system-98-66.qfasl.9'
hcuot "Jun 29 09:22:38 1984" 'patch/system-98-67.lisp.1'
hcuot "Jun 29 09:22:46 1984" 'patch/system-98-67.qfasl.1'
hcuot "Jul 18 18:57:10 1984" 'patch/system-98-68.lisp.5'
hcuot "Jul 18 18:57:21 1984" 'patch/system-98-68.qfasl.5'
hcuot "Aug 14 20:39:14 1984" 'patch/system-98-69.lisp.1'
hcuot "Aug 14 20:39:24 1984" 'patch/system-98-69.qfasl.1'
hcuot "Dec 16 03:40:10 1983" 'patch/system-98-7.lisp.7'
hcuot "Aug 29 15:25:48 1984" 'patch/system-98-70.lisp.1'
hcuot "Aug 29 15:25:55 1984" 'patch/system-98-70.qfasl.1'
hcuot "Oct 13 01:02:56 1984" 'patch/system-98-71.lisp.3'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-71.qfasl.3'
hcuot "Oct 14 21:03:15 1984" 'patch/system-98-72.lisp.2'
hcuot "Oct 14 21:03:36 1984" 'patch/system-98-72.qfasl.2'
hcuot "Oct 11 08:17:25 1984" 'patch/system-98-73.lisp.2'
hcuot "Oct 11 14:50:01 1984" 'patch/system-98-73.qfasl.2'
hcuot "Oct 13 01:03:51 1984" 'patch/system-98-74.lisp.1'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-74.qfasl.1'
hcuot "Oct 14 06:56:53 1984" 'patch/system-98-75.lisp.1'
hcuot "Oct 14 06:57:21 1984" 'patch/system-98-75.qfasl.1'
hcuot "Oct 14 20:58:47 1984" 'patch/system-98-76.lisp.1'
hcuot "Oct 14 20:59:14 1984" 'patch/system-98-76.qfasl.1'
hcuot "Oct 20 18:08:35 1984" 'patch/system-98-77.lisp.1'
hcuot "Nov 12 13:24:48 1984" 'patch/system-98-77.qfasl.1'
hcuot "Nov 12 12:57:12 1984" 'patch/system-98-78.lisp.5'
hcuot "Nov 12 12:57:43 1984" 'patch/system-98-78.qfasl.5'
hcuot "Nov 20 16:35:13 1984" 'patch/system-98-79.lisp.8'
hcuot "Nov 21 00:34:09 1984" 'patch/system-98-79.ncp.3'
hcuot "Nov 20 16:35:23 1984" 'patch/system-98-79.qfasl.8'
hcuot "Nov 20 23:51:27 1984" 'patch/system-98-79-chsncp.lisp.3'
hcuot "Dec 18 01:24:01 1983" 'patch/system-98-8.lisp.12'
hcuot "Nov 26 21:21:29 1984" 'patch/system-98-80.lisp.1'
hcuot "Nov 26 21:21:56 1984" 'patch/system-98-80.qfasl.1'
hcuot "Nov 28 15:53:15 1984" 'patch/system-98-81.lisp.1'
hcuot "Nov 28 15:58:45 1984" 'patch/system-98-81.lisp.2'
hcuot "Nov 28 15:53:39 1984" 'patch/system-98-81.qfasl.1'
hcuot "Nov 28 15:59:12 1984" 'patch/system-98-81.qfasl.2'
hcuot "Dec 22 17:18:46 1983" 'patch/system-98-9.lisp.9'
hcuot "Oct 14 15:17:53 1984" 'patch/system-98-9.qfasl.9'
hcuot "Dec 14 12:27:18 1984" 'patch/system-99.patch-directory.72'
hcuot "Dec 25 23:48:35 1984" 'patch/system-99.patch-directory.73'
hcuot "Dec 25 23:49:07 1984" 'patch/system-99.patch-directory.74'
hcuot "Sep 12 17:29:05 1984" 'patch/system-99-1.lisp.2'
hcuot "Sep 12 18:29:34 1984" 'patch/system-99-1.lisp.3'
hcuot "Sep 12 18:30:01 1984" 'patch/system-99-1.qfasl.3'
hcuot "Nov  9 20:03:39 1984" 'patch/system-99-10.lisp.30'
hcuot "Nov  9 21:17:06 1984" 'patch/system-99-10.lisp.31'
hcuot "Nov  9 21:32:38 1984" 'patch/system-99-10.qfasl.31'
hcuot "Nov 14 11:59:40 1984" 'patch/system-99-11.lisp.13'

# tid/3336

hcuot "Oct 27 23:52:56 1983" 'sys2/hash.lisp.81'		     # Author: RMS
hcuot "Apr 15 08:59:29 1984" 'sys2/hash.lisp.82'		     # Author: MLY
hcuot "Jun  5 01:39:39 1984" 'sys2/hash.lisp.83'		     # Author: MLY
hcuot "Jun  6 04:25:44 1984" 'sys2/hash.qfasl.83'		     # Author: RMS
hcuot "Oct 27 23:52:41 1983" 'sys2/hashfl.lisp.19'		     # Author: RMS
hcuot "Nov  1 23:49:35 1983" 'sys2/hashfl.lisp.20'		     # Author: RMS
hcuot "Apr 16 21:35:12 1984" 'sys2/hashfl.lisp.24'		     # Author: MLY
hcuot "Jun  5 01:33:55 1984" 'sys2/hashfl.lisp.25'		     # Author: MLY
hcuot "Jun 14 01:00:50 1984" 'sys2/hashfl.lisp.26'		     # Author: MERMAN.JAN
hcuot "Jun 14 01:02:32 1984" 'sys2/hashfl.lisp.27'		     # Author: MERMAN.JAN
hcuot "Jun 14 01:02:46 1984" 'sys2/hashfl.lisp.28'		     # Author: MERMAN.JAN
hcuot "Jun  6 04:26:59 1984" 'sys2/hashfl.qfasl.25'		     # Author: RMS
hcuot "Jun 21 07:26:29 1984" 'sys2/hashfl.qfasl.28'		     # Author: MLY
hcuot "Jan  7 13:03:49 1984" 'sys2/host.lisp.105'		     # Author: DANIEL.G.MLY
hcuot "May 20 08:26:42 1984" 'sys2/host.lisp.106'		     # Author: RPK
hcuot "Jun  5 03:17:20 1984" 'sys2/host.lisp.107'		     # Author: MLY
hcuot "Jun 10 10:30:34 1984" 'sys2/host.lisp.108'		     # Author: MLY
hcuot "Jun 12 09:17:05 1984" 'sys2/host.lisp.109'		     # Author: MLY
hcuot "Jun 15 09:41:58 1984" 'sys2/host.lisp.110'		     # Author: RPK
hcuot "Jun  6 04:28:44 1984" 'sys2/host.qfasl.107'		     # Author: RMS
hcuot "Jun 10 12:09:57 1984" 'sys2/host.qfasl.108'		     # Author: MLY
hcuot "Jun 12 10:21:21 1984" 'sys2/host.qfasl.109'		     # Author: MLY
hcuot "Jun 15 09:45:45 1984" 'sys2/host.qfasl.110'		     # Author: RPK
hcuot "Jun 29 08:57:31 1982" 'sys2/let.lisp.7'			     # Author: RMS
hcuot "Apr  7 15:51:47 1984" 'sys2/let.lisp.8'			     # Author: MLY
hcuot "Aug 14 03:09:26 1983" 'sys2/let.qfasl.7'			     # Author: RMS
hcuot "Aug 20 14:33:08 1983" 'sys2/lmmac.lisp.278'		     # Author: RMS
hcuot "Oct 30 05:20:24 1983" 'sys2/lmmac.lisp.299'		     # Author: RMS
hcuot "May 10 11:30:29 1984" 'sys2/lmmac.lisp.348'		     # Author: MLY
hcuot "May 20 10:25:41 1984" 'sys2/lmmac.lisp.352'		     # Author: RMS
hcuot "Jun  4 08:44:01 1984" 'sys2/lmmac.lisp.356'		     # Author: RMS
hcuot "Jun 13 22:16:20 1984" 'sys2/lmmac.lisp.360'		     # Author: MERMAN.JAN
hcuot "Jun  6 01:44:24 1984" 'sys2/lmmac.qfasl.356'		     # Author: RMS
hcuot "Jun  9 13:35:46 1984" 'sys2/lmmac.qfasl.357'		     # Author: MLY
hcuot "Jun 13 19:31:22 1984" 'sys2/lmmac.qfasl.358'		     # Author: MLY
hcuot "Jun 21 03:24:01 1984" 'sys2/lmmac.qfasl.360'		     # Author: MLY
hcuot "Aug 13 09:44:29 1983" 'sys2/login.lisp.76'		     # Author: RMS
hcuot "May  9 12:09:16 1984" 'sys2/login.lisp.83'		     # Author: MLY
hcuot "Jun  5 01:16:24 1984" 'sys2/login.lisp.84'		     # Author: MLY
hcuot "Jun  6 04:32:14 1984" 'sys2/login.qfasl.84'		     # Author: RMS
hcuot "Nov 30 01:23:16 1983" 'sys2/loop.lisp.795'		     # Author: RMS
hcuot "Jun 13 22:42:07 1984" 'sys2/loop.lisp.796'		     # Author: MERMAN.JAN
hcuot "Jun 15 17:03:27 1984" 'sys2/loop.lisp.797'		     # Author: MERMAN.JAN
hcuot "Jun 20 18:08:26 1984" 'sys2/loop.lisp.798'		     # Author: MERMAN.JAN
hcuot "Jun  6 04:33:32 1984" 'sys2/loop.qfasl.795'		     # Author: RMS
hcuot "Jun 21 07:28:18 1984" 'sys2/loop.qfasl.798'		     # Author: MLY
hcuot "Nov 18 14:36:42 1983" 'sys2/maksys.lisp.166'		     # Author: RMS
hcuot "Jan 25 07:41:37 1984" 'sys2/maksys.lisp.174'		     # Author: RMS
hcuot "Jun  5 01:11:51 1984" 'sys2/maksys.lisp.175'		     # Author: MLY
hcuot "Jun 16 20:18:39 1984" 'sys2/maksys.lisp.176'		     # Author: MLY
hcuot "Jun  6 04:40:23 1984" 'sys2/maksys.qfasl.175'		     # Author: RMS
hcuot "Jun 16 20:18:52 1984" 'sys2/maksys.qfasl.176'		     # Author: MLY
hcuot "Jul 27 10:36:50 1983" 'sys2/matrix.lisp.18'		     # Author: RMS
hcuot "Mar 14 19:02:00 1984" 'sys2/matrix.lisp.23'		     # Author: MLY
hcuot "Apr  9 18:08:41 1984" 'sys2/matrix.lisp.26'		     # Author: KAB
hcuot "Jun  6 12:54:58 1984" 'sys2/matrix.qfasl.26'		     # Author: RMS
hcuot "Nov 12 01:30:30 1983" 'sys2/meth.lisp.58'		     # Author: RMS
hcuot "Jun  6 23:55:50 1984" 'sys2/meth.lisp.59'		     # Author: MERMAN.JAN
hcuot "Jun 13 18:38:56 1984" 'sys2/meth.lisp.60'		     # Author: MERMAN.JAN
hcuot "Jun 17 06:43:45 1984" 'sys2/meth.lisp.61'		     # Author: MLY
hcuot "Jun 17 07:08:28 1984" 'sys2/meth.qfasl.61'		     # Author: MLY
hcuot "Jun  4 03:02:35 1984" 'sys2/numdef.lisp.1'		     # Author: MLY
hcuot "Jun  9 08:58:51 1984" 'sys2/numdef.lisp.4'		     # Author: MLY
hcuot "Jun 13 08:03:45 1984" 'sys2/numdef.lisp.6'		     # Author: NGL
hcuot "Jun 18 17:02:30 1984" 'sys2/numdef.lisp.7'		     # Author: MLY
hcuot "Jun 18 17:39:26 1984" 'sys2/numdef.lisp.8'		     # Author: MLY
hcuot "Jun 24 17:56:48 1984" 'sys2/numdef.lisp.9'		     # Author: MLY
hcuot "Jun 13 08:03:53 1984" 'sys2/numdef.qfasl.6'		     # Author: NGL
hcuot "Jun 21 05:53:02 1984" 'sys2/numdef.qfasl.8'		     # Author: MLY
hcuot "Sep 22 21:11:37 1983" 'sys2/numer.lisp.34'		     # Author: RMS
hcuot "Nov 10 02:28:15 1983" 'sys2/numer.lisp.41'		     # Author: RMS
hcuot "Mar 13 22:55:49 1984" 'sys2/numer.lisp.43'		     # Author: RMS
hcuot "Jun  4 02:59:32 1984" 'sys2/numer.lisp.45'		     # Author: MLY
hcuot "Jun  5 00:50:11 1984" 'sys2/numer.lisp.46'		     # Author: MLY
hcuot "Jun  8 21:16:39 1984" 'sys2/numer.lisp.47'		     # Author: MLY
hcuot "Jun  9 08:58:54 1984" 'sys2/numer.lisp.48'		     # Author: MLY
hcuot "Jun 13 07:50:07 1984" 'sys2/numer.lisp.49'		     # Author: NGL
hcuot "Jun 13 08:06:00 1984" 'sys2/numer.lisp.50'		     # Author: NGL
hcuot "Jun 13 08:06:34 1984" 'sys2/numer.lisp.51'		     # Author: NGL
hcuot "Jun 13 08:08:43 1984" 'sys2/numer.lisp.52'		     # Author: NGL
hcuot "Jun 14 09:46:37 1984" 'sys2/numer.lisp.53'		     # Author: RMS
hcuot "Jun 14 10:03:20 1984" 'sys2/numer.lisp.54'		     # Author: RMS
hcuot "Jun 17 05:47:53 1984" 'sys2/numer.lisp.55'		     # Author: MLY
hcuot "Jun  6 04:47:26 1984" 'sys2/numer.qfasl.46'		     # Author: RMS
hcuot "Jun 10 10:57:58 1984" 'sys2/numer.qfasl.48'		     # Author: MLY
hcuot "Jun 13 07:54:18 1984" 'sys2/numer.qfasl.49'		     # Author: NGL
hcuot "Jun 13 08:08:52 1984" 'sys2/numer.qfasl.52'		     # Author: NGL
hcuot "Jun 14 09:46:42 1984" 'sys2/numer.qfasl.53'		     # Author: RMS
hcuot "Jun 14 10:17:00 1984" 'sys2/numer.qfasl.54'		     # Author: RMS
hcuot "Jun 17 05:50:32 1984" 'sys2/numer.qfasl.55'		     # Author: MLY
hcuot "Mar 22 04:17:42 1984" 'sys2/patch.lisp.150'		     # Author: MLY
hcuot "May 10 11:30:23 1984" 'sys2/patch.lisp.151'		     # Author: MLY
hcuot "Jun  5 00:46:42 1984" 'sys2/patch.lisp.152'		     # Author: MLY
hcuot "Jun  6 05:09:32 1984" 'sys2/patch.lisp.153'		     # Author: MLY
hcuot "Jun 13 13:49:50 1984" 'sys2/patch.lisp.154'		     # Author: MLY
hcuot "Jun  6 04:48:58 1984" 'sys2/patch.qfasl.152'		     # Author: RMS
hcuot "Jun 10 10:59:15 1984" 'sys2/patch.qfasl.153'		     # Author: MLY
hcuot "Jun 13 20:01:57 1984" 'sys2/patch.qfasl.154'		     # Author: MLY
hcuot "Apr  5 21:46:25 1984" 'sys2/plane.lisp.30'		     # Author: MLY
hcuot "May 31 14:08:31 1984" 'sys2/plane.lisp.31'		     # Author: MLY
hcuot "Jun  6 04:51:31 1984" 'sys2/plane.qfasl.31'		     # Author: RMS
hcuot "Feb 17 11:36:34 1984" 'sys2/proces.lisp.153'		     # Author: MLY
hcuot "May 13 02:31:33 1984" 'sys2/proces.lisp.154'		     # Author: MLY
hcuot "May 20 15:33:58 1984" 'sys2/proces.lisp.155'		     # Author: MLY
hcuot "Jun 14 09:28:49 1984" 'sys2/proces.lisp.156'		     # Author: RMS
hcuot "Jun 14 09:38:01 1984" 'sys2/proces.lisp.157'		     # Author: RMS
hcuot "Jun  6 04:55:18 1984" 'sys2/proces.qfasl.155'		     # Author: RMS
hcuot "Jun 14 09:29:02 1984" 'sys2/proces.qfasl.156'		     # Author: RMS
hcuot "Jun 14 09:38:13 1984" 'sys2/proces.qfasl.157'		     # Author: RMS
hcuot "Dec 26 03:13:49 1983" 'sys2/prodef.lisp.39'		     # Author: RMS
hcuot "May 13 02:30:56 1984" 'sys2/prodef.lisp.40'		     # Author: MLY
hcuot "Jun  4 06:23:40 1984" 'sys2/prodef.lisp.41'		     # Author: MLY
hcuot "Jun 13 17:49:05 1984" 'sys2/prodef.lisp.42'		     # Author: MERMAN.JAN
hcuot "Jun 13 20:06:12 1984" 'sys2/prodef.lisp.43'		     # Author: MERMAN.JAN
hcuot "Jun 13 21:27:52 1984" 'sys2/prodef.lisp.44'		     # Author: MERMAN.JAN
hcuot "Jun 14 01:32:10 1984" 'sys2/prodef.lisp.45'		     # Author: MERMAN.JAN
hcuot "Jun 15 17:55:34 1984" 'sys2/prodef.lisp.46'		     # Author: MERMAN.JAN
hcuot "Jun 21 06:01:12 1984" 'sys2/prodef.lisp.47'		     # Author: MLY
hcuot "Jun  6 02:08:42 1984" 'sys2/prodef.qfasl.41'		     # Author: RMS
hcuot "Jun 13 19:48:03 1984" 'sys2/prodef.qfasl.42'		     # Author: MLY
hcuot "Jun 21 05:51:54 1984" 'sys2/prodef.qfasl.46'		     # Author: MLY
hcuot "Jun 21 06:01:41 1984" 'sys2/prodef.qfasl.47'		     # Author: MLY
hcuot "Apr  1 19:50:03 1984" 'sys2/qtrace.lisp.148'		     # Author: MLY
hcuot "Apr 22 04:59:26 1984" 'sys2/qtrace.lisp.149'		     # Author: RMS
hcuot "Jun  6 05:27:40 1984" 'sys2/qtrace.qfasl.149'		     # Author: RMS
hcuot "Nov  5 04:47:16 1983" 'sys2/rat.lisp.34'			     # Author: RMS
hcuot "Mar 29 11:34:31 1984" 'sys2/rat.lisp.37'			     # Author: MLY
hcuot "Jun  4 03:02:01 1984" 'sys2/rat.lisp.38'			     # Author: MLY
hcuot "Jun  4 23:24:29 1984" 'sys2/rat.lisp.39'			     # Author: MLY
hcuot "Jun  6 01:41:26 1984" 'sys2/rat.lisp.40'			     # Author: RMS
hcuot "Jun  6 01:42:00 1984" 'sys2/rat.lisp.41'			     # Author: RMS
hcuot "Jun  6 05:49:57 1984" 'sys2/rat.lisp.42'			     # Author: MLY
hcuot "Jun  7 16:48:46 1984" 'sys2/rat.lisp.43'			     # Author: MLY
hcuot "Jun 14 21:17:48 1984" 'sys2/rat.lisp.44'			     # Author: RMS
hcuot "Jun  6 05:28:48 1984" 'sys2/rat.qfasl.41'		     # Author: RMS
hcuot "Jun 10 12:26:15 1984" 'sys2/rat.qfasl.43'		     # Author: MLY
hcuot "Jun 21 08:21:04 1984" 'sys2/rat.qfasl.44'		     # Author: MLY
hcuot "Aug  6 12:26:38 1983" 'sys2/resour.lisp.17'		     # Author: RMS
hcuot "Nov  7 22:06:04 1983" 'sys2/resour.lisp.18'		     # Author: RMS
hcuot "Apr 15 07:32:26 1984" 'sys2/resour.lisp.26'		     # Author: MLY
hcuot "Jun  6 11:46:53 1984" 'sys2/resour.lisp.27'		     # Author: MLY
hcuot "Jun 24 06:09:43 1984" 'sys2/resour.lisp.28'		     # Author: MLY
hcuot "Jun  6 05:35:50 1984" 'sys2/resour.qfasl.26'		     # Author: RMS
hcuot "Jun 10 12:32:26 1984" 'sys2/resour.qfasl.27'		     # Author: MLY
hcuot "Feb  8 14:52:48 1984" 'sys2/selev.lisp.21'		     # Author: MLY
hcuot "May 11 13:54:50 1984" 'sys2/selev.lisp.22'		     # Author: MLY
hcuot "Jun  6 05:37:21 1984" 'sys2/selev.qfasl.22'		     # Author: RMS
hcuot "Oct 12 09:45:31 1983" 'sys2/setf.lisp.51'		     # Author: RMS
hcuot "Nov 10 20:13:54 1983" 'sys2/setf.lisp.52'		     # Author: RMS
hcuot "Apr  5 21:47:51 1984" 'sys2/setf.lisp.85'		     # Author: MLY
hcuot "May 20 08:10:47 1984" 'sys2/setf.lisp.86'		     # Author: RMS
hcuot "Jun  4 19:59:04 1984" 'sys2/setf.lisp.89'		     # Author: RMS
hcuot "Jun 14 09:05:31 1984" 'sys2/setf.lisp.90'		     # Author: RMS
hcuot "Jun 17 06:31:52 1984" 'sys2/setf.lisp.91'		     # Author: MLY
hcuot "Jun  6 01:58:39 1984" 'sys2/setf.qfasl.89'		     # Author: RMS
hcuot "Jun 14 09:05:38 1984" 'sys2/setf.qfasl.90'		     # Author: RMS
hcuot "Jun 17 07:36:58 1984" 'sys2/setf.qfasl.91'		     # Author: MLY
hcuot "Dec 22 17:32:02 1983" 'sys2/setf.xqfasl.67'		     # Author: RMS
hcuot "Oct  6 16:24:42 1983" 'sys2/sgdefs.lisp.53'		     # Author: RMS
hcuot "Jan 12 14:29:47 1984" 'sys2/sgdefs.lisp.54'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 02:11:03 1984" 'sys2/sgdefs.qfasl.54'		     # Author: RMS
hcuot "Dec 31 23:26:30 1982" 'sys2/step.lisp.58'		     # Author: CENT
hcuot "Mar  2 23:25:35 1983" 'sys2/step.lisp.60'		     # Author: CENT
hcuot "Sep 20 00:20:05 1983" 'sys2/step.lisp.61'		     # Author: RMS
hcuot "Oct 29 19:56:25 1983" 'sys2/step.lisp.63'		     # Author: CENT
hcuot "Dec 22 02:05:09 1983" 'sys2/step.lisp.65'		     # Author: RMS
hcuot "Apr 22 04:59:17 1984" 'sys2/step.lisp.66'		     # Author: RMS
hcuot "Jun 17 06:30:55 1984" 'sys2/step.lisp.67'		     # Author: MLY
hcuot "Nov 10 18:53:55 1983" 'sys2/step.qfasl.63'		     # Author: RMS
hcuot "Jun  6 05:40:08 1984" 'sys2/step.qfasl.66'		     # Author: RMS
hcuot "Jun 17 07:16:01 1984" 'sys2/step.qfasl.67'		     # Author: MLY
hcuot "Apr  6 10:08:15 1984" 'sys2/string.lisp.130'		     # Author: RMS
hcuot "May 13 04:32:48 1984" 'sys2/string.lisp.132'		     # Author: MLY
hcuot "Jun  4 22:14:12 1984" 'sys2/string.lisp.133'		     # Author: MLY
hcuot "Jun 15 06:55:18 1984" 'sys2/string.lisp.134'		     # Author: RMS
hcuot "Jun 15 07:14:54 1984" 'sys2/string.lisp.135'		     # Author: RMS
hcuot "Jun 15 17:44:09 1984" 'sys2/string.lisp.136'		     # Author: MLY
hcuot "Jun  6 05:43:56 1984" 'sys2/string.qfasl.133'		     # Author: RMS
hcuot "Jun 15 06:56:11 1984" 'sys2/string.qfasl.134'		     # Author: RMS
hcuot "Jun 21 08:30:48 1984" 'sys2/string.qfasl.136'		     # Author: MLY
hcuot "Aug  4 13:25:26 1983" 'sys2/struct.lisp.286'		     # Author: RMS
hcuot "Nov 12 20:33:12 1983" 'sys2/struct.lisp.287'		     # Author: RMS
hcuot "Nov 29 05:45:39 1983" 'sys2/struct.lisp.292'		     # Author: RMS
hcuot "Mar  7 23:22:17 1984" 'sys2/struct.lisp.309'		     # Author: MLY
hcuot "May 18 06:09:04 1984" 'sys2/struct.lisp.311'		     # Author: RMS
hcuot "Jun  5 03:17:48 1984" 'sys2/struct.lisp.316'		     # Author: MLY
hcuot "Jun  6 00:29:40 1984" 'sys2/struct.lisp.317'		     # Author: MLY
hcuot "Jun 10 17:00:34 1984" 'sys2/struct.lisp.318'		     # Author: MLY
hcuot "Jun 12 15:41:29 1984" 'sys2/struct.lisp.319'		     # Author: MLY
hcuot "Jun 16 22:07:01 1984" 'sys2/struct.lisp.320'		     # Author: MLY
hcuot "Jun 12 16:09:43 1984" 'sys2/struct.qfasl.319'		     # Author: RMS
hcuot "Jun 16 22:07:30 1984" 'sys2/struct.qfasl.320'		     # Author: MLY
hcuot "Nov 10 11:06:56 1983" 'sys2/system.lisp.82'		     # Author: RMS
hcuot "Apr 27 13:19:33 1984" 'sys2/system.lisp.89'		     # Author: MLY
hcuot "May 25 20:24:44 1984" 'sys2/system.lisp.93'		     # Author: MLY
hcuot "Jun  7 04:10:40 1984" 'sys2/system.lisp.94'		     # Author: MLY
hcuot "Jun 18 15:29:19 1984" 'sys2/system.lisp.97'		     # Author: MLY
hcuot "Nov 10 11:07:02 1983" 'sys2/system.qfasl.82'		     # Author: RMS
hcuot "Jun  6 05:47:05 1984" 'sys2/system.qfasl.93'		     # Author: RMS
hcuot "Jun 10 12:33:34 1984" 'sys2/system.qfasl.94'		     # Author: MLY
hcuot "Jun 13 09:44:38 1984" 'sys2/system.qfasl.95'		     # Author: MLY
hcuot "Jun 15 22:23:48 1984" 'sys2/system.qfasl.96'		     # Author: MLY
hcuot "Jun 18 15:29:30 1984" 'sys2/system.qfasl.97'		     # Author: MLY
hcuot "Jun 23 07:07:52 1983" 'sys2/unfasl.lisp.15'		     # Author: RMS
hcuot "Aug  3 08:04:28 1983" 'sys2/unfasl.lisp.16'		     # Author: RMS
hcuot "Oct 25 00:31:34 1983" 'sys2/unfasl.qfasl.16'		     # Author: RMS
hcuot "Oct 22 14:37:42 1983" 'sys2/usymld.lisp.183'		     # Author: DANIEL.G.MLY
hcuot "Nov 12 09:18:33 1983" 'sys2/usymld.lisp.185'		     # Author: RMS
hcuot "Feb 17 16:46:44 1984" 'sys2/usymld.lisp.186'		     # Author: MLY
hcuot "Nov 14 02:54:56 1983" 'sys2/usymld.qfasl.185'		     # Author: RMS
hcuot "Jan  3 08:50:32 1984" 'tape/copy.lisp.128'		     # Author: RPK
hcuot "Feb 16 13:56:19 1984" 'tape/copy.lisp.133'		     # Author: LMFILE
hcuot "Jan  3 09:50:47 1984" 'tape/copy.qfasl.128'		     # Author: RPK
hcuot "Jan 19 16:25:49 1984" 'tape/ddoc.text.4'			     # Author: RPK
hcuot "May 12 05:49:07 1984" 'tape/ddoc.text.8'			     # Author: RPK
hcuot "Jan 19 16:25:57 1984" 'tape/fdump.lisp.18'		     # Author: RPK
hcuot "Feb 16 13:56:28 1984" 'tape/fdump.lisp.24'		     # Author: LMFILE
hcuot "May 12 05:29:43 1984" 'tape/fdump.lisp.27'		     # Author: RPK
hcuot "Jan 10 04:56:50 1984" 'tape/fdump-def.lisp.5'		     # Author: RPK
hcuot "Feb 16 13:56:36 1984" 'tape/fdump-def.lisp.8'		     # Author: LMFILE
hcuot "May 12 05:52:14 1984" 'tape/fdump-def.lisp.12'		     # Author: RPK
hcuot "Jan  2 09:37:10 1984" 'tape/fdump-def.qfasl.1'		     # Author: RPK
hcuot "Jan 10 03:54:15 1984" 'tape/fdump-file-cdate-i.lisp.1'	     # Author: RPK
hcuot "Jan 10 04:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'	     # Author: RPK
hcuot "Jan 19 16:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'	     # Author: RPK
hcuot "Feb 16 13:56:44 1984" 'tape/fdump-r.lisp.4'		     # Author: LMFILE
hcuot "May 12 05:29:45 1984" 'tape/fdump-r.lisp.5'		     # Author: RPK
hcuot "Jan  3 10:36:11 1984" 'tape/magtape.directory.11'	     # Author: RPK
hcuot "Oct 26 20:41:54 1983" 'tape/magtape-14.directory.14'	     # Author: RPK
hcuot "Mar  8 06:56:47 1983" 'tape/magtape-14-1.qfasl.1'	     # Author: RPK
hcuot "Apr 25 09:51:48 1983" 'tape/magtape-14-3.qfasl.1'	     # Author: RPK
hcuot "May 19 04:11:34 1983" 'tape/magtape-14-4.qfasl.3'	     # Author: RPK
hcuot "Oct 26 20:41:16 1983" 'tape/magtape-14-5.qfasl.1'	     # Author: RPK
hcuot "Feb 16 14:09:51 1984" 'tape/magtape-22.directory.12'	     # Author: LMFILE
hcuot "Feb 16 14:24:04 1984" 'tape/magtape-22.directory.13'	     # Author: LMFILE
hcuot "Jan  7 22:40:45 1984" 'tape/magtape-22-1.lisp.1'		     # Author: RMS
hcuot "Jan  7 22:40:56 1984" 'tape/magtape-22-1.qfasl.1'	     # Author: RMS
hcuot "Jan  7 23:28:27 1984" 'tape/magtape-22-2.lisp.1'		     # Author: RMS
hcuot "Jan  7 23:28:40 1984" 'tape/magtape-22-2.qfasl.1'	     # Author: RMS
hcuot "Jan  8 00:41:18 1984" 'tape/magtape-22-3.lisp.1'		     # Author: RMS
hcuot "Jan  8 00:41:44 1984" 'tape/magtape-22-3.qfasl.1'	     # Author: RMS
hcuot "Jan 13 13:06:26 1984" 'tape/magtape-22-4.lisp.1'		     # Author: GJC
hcuot "Jan 13 13:06:35 1984" 'tape/magtape-22-4.qfasl.1'	     # Author: GJC
hcuot "Jan 19 17:40:22 1984" 'tape/magtape-22-5.lisp.1'		     # Author: RPK
hcuot "Jan 19 17:40:32 1984" 'tape/magtape-22-5.qfasl.1'	     # Author: RPK
hcuot "Feb 16 14:23:22 1984" 'tape/magtape-22-6.lisp.1'		     # Author: LMFILE
hcuot "Feb 16 14:23:28 1984" 'tape/magtape-22-6.qfasl.1'	     # Author: LMFILE
hcuot "Jan 13 12:25:26 1984" 'tape/mtaux.lisp.79'		     # Author: GJC
hcuot "Jan 19 17:04:02 1984" 'tape/mtaux.lisp.80'		     # Author: RPK
hcuot "Jan  3 09:52:48 1984" 'tape/mtaux.qfasl.77'		     # Author: RPK
hcuot "Jun 20 06:21:53 1983" 'tape/mtdefs.lisp.28'		     # Author: RPK
hcuot "Dec 16 15:34:10 1983" 'tape/mtdefs.lisp.30'		     # Author: RPK
hcuot "Jan  3 09:46:18 1984" 'tape/mtdefs.qfasl.30'		     # Author: RPK
hcuot "Jan  7 23:43:06 1984" 'tape/mtstr.lisp.86'		     # Author: RMS
hcuot "Jan 11 05:40:52 1984" 'tape/mtstr.lisp.87'		     # Author: RPK
hcuot "Jan  3 09:47:58 1984" 'tape/mtstr.qfasl.85'		     # Author: RPK
hcuot "Jan  3 08:50:55 1984" 'tape/odump.lisp.1'		     # Author: RPK
hcuot "Jan  3 10:33:05 1984" 'tape/odump.qfasl.1'		     # Author: RPK
hcuot "May 12 05:29:46 1984" 'tape/package.lisp.1'		     # Author: RPK
hcuot "Jan  3 07:59:49 1984" 'tape/pdp10.lisp.1'		     # Author: RPK
hcuot "Feb  9 07:10:16 1984" 'tape/rmunit.lisp.1'		     # Author: RPK
hcuot "May 12 07:27:48 1984" 'tape/rmunit.lisp.2'		     # Author: MLY
hcuot "May 12 08:31:18 1984" 'tape/rmunit.lisp.3'		     # Author: RPK
hcuot "May 12 05:29:46 1984" 'tape/system.lisp.3'		     # Author: RPK
hcuot "Jan  3 04:42:55 1984" 'tape/tm.lisp.23'			     # Author: RMS
hcuot "May 12 05:29:47 1984" 'tape/tm.lisp.25'			     # Author: RPK
hcuot "Jan  3 04:43:02 1984" 'tape/tmdefs.lisp.6'		     # Author: RMS
hcuot "May 12 05:29:48 1984" 'tape/tmdefs.lisp.7'		     # Author: RPK
hcuot "Jan 19 16:26:52 1984" 'tape/unit.lisp.5'			     # Author: RPK
hcuot "May 12 05:29:49 1984" 'tape/unit.lisp.6'			     # Author: RPK
hcuot "May 12 07:27:24 1984" 'tape/unit.lisp.7'			     # Author: MLY
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.1'			     # Author: RPK
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.2'			     # Author: MLY
hcuot "Jan 19 16:27:08 1984" 'tape/new/mtdefs.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:49 1984" 'tape/new/mtdefs.lisp.3'		     # Author: RPK
hcuot "May 12 07:28:11 1984" 'tape/new/mtdefs.lisp.4'		     # Author: MLY
hcuot "May 12 07:45:03 1984" 'tape/new/mtdefs.qfasl.4'		     # Author: RPK
hcuot "Jan 11 06:38:45 1984" 'tape/new/mtrqb.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:49 1984" 'tape/new/mtrqb.lisp.3'		     # Author: RPK
hcuot "Jan 11 06:38:21 1984" 'tape/new/mtstr.lisp.3'		     # Author: RPK
hcuot "May 12 05:29:50 1984" 'tape/new/mtstr.lisp.4'		     # Author: RPK
hcuot "May 12 08:31:35 1984" 'tape/new/mtstr.lisp.5'		     # Author: RPK
hcuot "Jan 19 16:27:21 1984" 'tape/new/tmunit.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:50 1984" 'tape/new/tmunit.lisp.5'		     # Author: RPK
hcuot "Jan 11 06:38:10 1984" 'tape/new/weunit.lisp.2'		     # Author: RPK
hcuot "May 12 05:29:51 1984" 'tape/new/weunit.lisp.3'		     # Author: RPK
hcuot "Jan  3 06:11:30 1984" 'tape/to-merge/-read-.-this-.1'	     # Author: RPK
hcuot "Oct  9 20:43:50 1983" 'tape/to-merge/copy.lisp.130'	     # Author: RMS
hcuot "Sep 24 21:21:09 1983" 'tape/to-merge/mtaux.lisp.71'	     # Author: RMS
hcuot "Sep 24 21:27:33 1983" 'tape/to-merge/mtdefs.lisp.49'	     # Author: RMS
hcuot "Oct  9 20:11:44 1983" 'tape/to-merge/mtstr.lisp.90'	     # Author: RMS
hcuot "Feb  3 08:23:56 1983" 'test/-read-.-this-.1'		     # Author: HDT
hcuot "Nov 20 23:29:49 1982" 'ubin/dcfu.uload.4'		     # Author: AKR
hcuot "Aug  4 07:23:05 1982" 'ubin/memd.uload.1'		     # Author: RMS
hcuot "May  7 01:22:05 1983" 'ubin/ucadr.locs.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:18:51 1983" 'ubin/ucadr.locs.257'		     # Author: RMS
hcuot "Mar  3 08:22:09 1984" 'ubin/ucadr.locs.309'		     # Author: RMS
hcuot "Jun  8 05:31:58 1984" 'ubin/ucadr.locs.310'		     # Author: RMS
hcuot "Jun  8 23:07:57 1984" 'ubin/ucadr.locs.311'		     # Author: MLY
hcuot "Jun 16 21:32:16 1984" 'ubin/ucadr.locs.312'		     # Author: MLY
hcuot "Jun 16 22:57:16 1984" 'ubin/ucadr.locs.313'		     # Author: MLY
hcuot "Jun 17 01:45:52 1984" 'ubin/ucadr.locs.314'		     # Author: MLY
hcuot "May  7 01:19:58 1983" 'ubin/ucadr.mcr.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:16:41 1983" 'ubin/ucadr.mcr.257'		     # Author: RMS
hcuot "Mar  3 08:20:07 1984" 'ubin/ucadr.mcr.309'		     # Author: RMS
hcuot "Jun  8 05:29:50 1984" 'ubin/ucadr.mcr.310'		     # Author: RMS
hcuot "Jun  8 23:03:26 1984" 'ubin/ucadr.mcr.311'		     # Author: MLY
hcuot "Jun 16 21:29:59 1984" 'ubin/ucadr.mcr.312'		     # Author: MLY
hcuot "Jun 16 22:54:55 1984" 'ubin/ucadr.mcr.313'		     # Author: MLY
hcuot "Jun 17 01:42:17 1984" 'ubin/ucadr.mcr.314'		     # Author: MLY
hcuot "May  7 01:20:31 1983" 'ubin/ucadr.sym.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:17:12 1983" 'ubin/ucadr.sym.257'		     # Author: RMS
hcuot "Nov 28 19:18:44 1983" 'ubin/ucadr.sym.305'		     # Author: RMS
hcuot "Dec 29 05:39:42 1983" 'ubin/ucadr.sym.306'		     # Author: RMS
hcuot "Jan 23 03:03:17 1984" 'ubin/ucadr.sym.308'		     # Author: RMS
hcuot "Mar  3 08:20:33 1984" 'ubin/ucadr.sym.309'		     # Author: RMS
hcuot "Jun  8 05:30:18 1984" 'ubin/ucadr.sym.310'		     # Author: RMS
hcuot "Jun 11 22:34:45 1984" 'ubin/ucadr.sym.311'		     # Author: RMS
hcuot "Jun 16 21:44:49 1984" 'ubin/ucadr.sym.312'		     # Author: MLY
hcuot "Jun 16 22:55:25 1984" 'ubin/ucadr.sym.313'		     # Author: MLY
hcuot "Jun 17 01:42:47 1984" 'ubin/ucadr.sym.314'		     # Author: MLY
hcuot "May  7 01:22:07 1983" 'ubin/ucadr.tbl.239'		     # Author: RMS.G.DULCEY
hcuot "Aug 26 14:18:53 1983" 'ubin/ucadr.tbl.257'		     # Author: RMS
hcuot "Mar  3 08:22:10 1984" 'ubin/ucadr.tbl.309'		     # Author: RMS
hcuot "Jun  8 05:32:00 1984" 'ubin/ucadr.tbl.310'		     # Author: RMS
hcuot "Jun  8 23:08:00 1984" 'ubin/ucadr.tbl.311'		     # Author: MLY
hcuot "Jun 16 21:32:18 1984" 'ubin/ucadr.tbl.312'		     # Author: MLY
hcuot "Jun 16 22:57:19 1984" 'ubin/ucadr.tbl.313'		     # Author: MLY
hcuot "Jun 17 01:45:55 1984" 'ubin/ucadr.tbl.314'		     # Author: MLY
hcuot "Apr  9 11:19:01 1983" 'ucadr/cadldb.lisp.20'		     # Author: RMS
hcuot "Jul 26 10:31:51 1983" 'ucadr/cadldb.qfasl.20'		     # Author: RMS
hcuot "Jun 29 10:56:11 1982" 'ucadr/cadtlk.mid.9'		     # Author: RMS
hcuot "Jun 29 10:56:32 1982" 'ucadr/chaos.test.1'		     # Author: RMS
hcuot "Jun 29 10:56:46 1982" 'ucadr/dcfu.text.23'		     # Author: RMS
hcuot "Dec 22 06:46:28 1982" 'ucadr/dcfu.uload.3'		     # Author: AKR
hcuot "Jun 29 10:59:34 1982" 'ucadr/memd.lisp.26'		     # Author: RMS
hcuot "Jun 29 10:59:39 1982" 'ucadr/mmtest.lisp.15'		     # Author: RMS
hcuot "Jun 29 10:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1' # Author: RMS
hcuot "Nov 17 06:26:42 1982" 'ucadr/packed.lisp.119'		     # Author: RMS
hcuot "Oct 14 23:41:23 1983" 'ucadr/packed.lisp.124'		     # Author: ALR
hcuot "Jun 29 11:00:13 1982" 'ucadr/praid.lisp.21'		     # Author: RMS
hcuot "Jun 29 11:00:18 1982" 'ucadr/promh.text.9'		     # Author: RMS
hcuot "Jul 20 09:27:19 1983" 'ucadr/uc-arith.lisp.18'		     # Author: RMS
hcuot "Nov 18 14:23:35 1983" 'ucadr/uc-arith.lisp.25'		     # Author: RMS
hcuot "Apr 29 00:12:20 1984" 'ucadr/uc-arith.lisp.27'		     # Author: RMS
hcuot "Apr 30 23:43:08 1984" 'ucadr/uc-arith.lisp.28'		     # Author: RMS
hcuot "May 20 23:14:24 1984" 'ucadr/uc-arith.lisp.29'		     # Author: RMS
hcuot "Jun  5 23:24:56 1984" 'ucadr/uc-arith.lisp.30'		     # Author: RMS
hcuot "Jun  5 23:38:51 1984" 'ucadr/uc-arith.lisp.31'		     # Author: RMS
hcuot "Jun  8 20:31:34 1984" 'ucadr/uc-arith.lisp.32'		     # Author: MLY
hcuot "Apr  3 12:38:40 1983" 'ucadr/uc-array.lisp.27'		     # Author: RMS
hcuot "Jul 23 10:01:24 1983" 'ucadr/uc-array.lisp.28'		     # Author: NGL
hcuot "Nov 29 21:22:31 1983" 'ucadr/uc-array.lisp.59'		     # Author: RMS
hcuot "May 20 23:41:03 1984" 'ucadr/uc-array.lisp.60'		     # Author: RMS
hcuot "Jun  8 20:31:25 1984" 'ucadr/uc-array.lisp.61'		     # Author: MLY
hcuot "Jun 16 20:59:03 1984" 'ucadr/uc-array.lisp.62'		     # Author: MLY
hcuot "Jun 17 01:36:02 1984" 'ucadr/uc-array.lisp.63'		     # Author: MLY
hcuot "Mar 31 23:16:21 1983" 'ucadr/uc-array-cache.lisp.1'	     # Author: RMS
hcuot "Mar 31 23:15:30 1983" 'ucadr/uc-cadr.lisp.6'		     # Author: RMS
hcuot "Jul 29 11:02:16 1983" 'ucadr/uc-cadr.lisp.7'		     # Author: RMS
hcuot "Jun  2 03:53:36 1984" 'ucadr/uc-cadr.lisp.8'		     # Author: RMS
hcuot "Jul 23 10:18:19 1983" 'ucadr/uc-call-return.lisp.37'	     # Author: NGL
hcuot "Sep 23 05:03:26 1983" 'ucadr/uc-call-return.lisp.61'	     # Author: RMS
hcuot "Jan 27 08:37:59 1984" 'ucadr/uc-call-return.lisp.97'	     # Author: RMS
hcuot "Mar 22 06:55:28 1984" 'ucadr/uc-call-return.lisp.98'	     # Author: RMS
hcuot "May 19 03:59:07 1984" 'ucadr/uc-call-return.lisp.99'	     # Author: RMS
hcuot "Jun  8 04:16:44 1984" 'ucadr/uc-call-return.lisp.100'	     # Author: RMS
hcuot "Jun 17 01:36:13 1984" 'ucadr/uc-call-return.lisp.101'	     # Author: MLY
hcuot "Oct 11 07:19:04 1982" 'ucadr/uc-chaos.lisp.1'		     # Author: RG
hcuot "Oct 15 01:27:48 1982" 'ucadr/uc-cold-disk.lisp.2'	     # Author: RG
hcuot "Jun  8 11:15:36 1983" 'ucadr/uc-cold-disk.lisp.9'	     # Author: RMS
hcuot "Nov 14 10:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'	     # Author: RMS
hcuot "Oct 11 07:18:14 1982" 'ucadr/uc-disk.lisp.1'		     # Author: RG
hcuot "Nov 14 08:21:19 1983" 'ucadr/uc-disk.lisp.2'		     # Author: RMS
hcuot "Jun  4 07:41:28 1983" 'ucadr/uc-fctns.lisp.26'		     # Author: RMS
hcuot "Sep 13 09:27:33 1983" 'ucadr/uc-fctns.lisp.40'		     # Author: RMS
hcuot "Apr 11 00:36:59 1984" 'ucadr/uc-fctns.lisp.73'		     # Author: PGS
hcuot "Apr 17 23:46:13 1984" 'ucadr/uc-fctns.lisp.74'		     # Author: MLY
hcuot "May 11 05:41:36 1984" 'ucadr/uc-fctns.lisp.75'		     # Author: RMS
hcuot "Jun  8 20:31:12 1984" 'ucadr/uc-fctns.lisp.76'		     # Author: MLY
hcuot "Jun 16 20:58:42 1984" 'ucadr/uc-fctns.lisp.77'		     # Author: MLY
hcuot "Jun 16 21:24:03 1984" 'ucadr/uc-fctns.lisp.78'		     # Author: MLY
hcuot "Jun 16 22:40:24 1984" 'ucadr/uc-fctns.lisp.79'		     # Author: MLY
hcuot "Apr  3 12:37:59 1983" 'ucadr/uc-hacks.lisp.3'		     # Author: RMS
hcuot "Oct 17 16:11:57 1983" 'ucadr/uc-hacks.lisp.5'		     # Author: RMS
hcuot "Feb  4 09:53:01 1983" 'ucadr/uc-interrupt.lisp.4'	     # Author: RMS
hcuot "Oct 30 00:16:52 1983" 'ucadr/uc-interrupt.lisp.7'	     # Author: RMS
hcuot "Oct 13 05:05:41 1982" 'ucadr/uc-logical.lisp.2'		     # Author: RG
hcuot "Jul 23 11:00:06 1983" 'ucadr/uc-logical.lisp.7'		     # Author: NGL
hcuot "Mar  3 04:56:48 1984" 'ucadr/uc-logical.lisp.8'		     # Author: RMS
hcuot "Apr  4 07:09:21 1983" 'ucadr/uc-macrocode.lisp.9'	     # Author: RMS
hcuot "Oct 19 12:16:21 1983" 'ucadr/uc-macrocode.lisp.22'	     # Author: RMS
hcuot "Nov 14 02:49:50 1983" 'ucadr/uc-macrocode.lisp.28'	     # Author: RMS
hcuot "Oct 11 20:53:14 1982" 'ucadr/uc-mc.lisp.1'		     # Author: RG
hcuot "Nov 14 02:47:25 1983" 'ucadr/uc-mc.lisp.2'		     # Author: RMS
hcuot "Apr  5 09:49:33 1983" 'ucadr/uc-meter.lisp.3'		     # Author: RMS
hcuot "Jul 23 12:00:11 1983" 'ucadr/uc-meter.lisp.4'		     # Author: NGL
hcuot "Aug  1 09:39:57 1983" 'ucadr/uc-meter.lisp.5'		     # Author: RMS
hcuot "Jun  5 10:47:27 1983" 'ucadr/uc-page-fault.lisp.7'	     # Author: RMS
hcuot "Oct 17 16:11:44 1983" 'ucadr/uc-page-fault.lisp.10'	     # Author: RMS
hcuot "Nov 21 09:24:14 1983" 'ucadr/uc-page-fault.lisp.13'	     # Author: RMS
hcuot "Apr  5 09:49:02 1983" 'ucadr/uc-parameters.lisp.204'	     # Author: RMS
hcuot "Oct 29 22:36:55 1983" 'ucadr/uc-parameters.lisp.222'	     # Author: RMS
hcuot "Dec 28 07:16:45 1983" 'ucadr/uc-parameters.lisp.228'	     # Author: RMS
hcuot "May  1 09:20:26 1984" 'ucadr/uc-parameters.lisp.229'	     # Author: RMS
hcuot "Jun  2 03:53:42 1984" 'ucadr/uc-parameters.lisp.230'	     # Author: RMS
hcuot "Oct 11 07:18:51 1982" 'ucadr/uc-pup.lisp.1'		     # Author: RG
hcuot "Nov 16 10:34:29 1983" 'ucadr/uc-stack-closure.lisp.3'	     # Author: RMS
hcuot "Jan 23 03:33:37 1984" 'ucadr/uc-stack-closure.lisp.6'	     # Author: RMS
hcuot "Jun 15 06:04:18 1984" 'ucadr/uc-stack-closure.lisp.7'	     # Author: RMS
hcuot "Jun 15 06:08:23 1984" 'ucadr/uc-stack-closure.lisp.8'	     # Author: RMS
hcuot "Nov 17 06:07:32 1982" 'ucadr/uc-stack-groups.lisp.3'	     # Author: RMS
hcuot "Apr  5 09:49:39 1983" 'ucadr/uc-stack-groups.lisp.4'	     # Author: RMS
hcuot "Jul 23 11:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'	     # Author: NGL
hcuot "Feb 15 09:14:27 1983" 'ucadr/uc-storage-allocation.lisp.12'   # Author: HDT
hcuot "Apr  3 12:39:03 1983" 'ucadr/uc-storage-allocation.lisp.13'   # Author: RMS
hcuot "Jul 29 10:34:22 1983" 'ucadr/uc-storage-allocation.lisp.15'   # Author: RMS
hcuot "May 19 04:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'   # Author: RMS
hcuot "Jul 20 10:00:15 1983" 'ucadr/uc-string.lisp.13'		     # Author: RMS
hcuot "Aug 28 00:10:40 1983" 'ucadr/uc-string.lisp.20'		     # Author: RMS
hcuot "Apr  6 09:54:27 1984" 'ucadr/uc-string.lisp.23'		     # Author: RMS
hcuot "Jun  8 05:21:08 1984" 'ucadr/uc-string.lisp.24'		     # Author: RMS
hcuot "Apr  3 12:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'	     # Author: RMS
hcuot "Jul 20 06:38:27 1983" 'ucadr/uc-transporter.lisp.6'	     # Author: RMS
hcuot "Aug 15 14:07:26 1983" 'ucadr/uc-transporter.lisp.9'	     # Author: RMS
hcuot "Dec 29 05:09:16 1983" 'ucadr/uc-transporter.lisp.22'	     # Author: RMS
hcuot "May  1 09:20:21 1984" 'ucadr/uc-transporter.lisp.23'	     # Author: RMS
hcuot "Apr  4 09:20:18 1983" 'ucadr/uc-tv.lisp.3'		     # Author: RMS
hcuot "Jul 23 12:18:17 1983" 'ucadr/uc-tv.lisp.4'		     # Author: NGL
hcuot "Apr  5 03:29:11 1984" 'ucadr/uc-tv.lisp.5'		     # Author: RMS
hcuot "Jun 29 11:00:58 1982" 'ucadr/ucadlr.text.746'		     # Author: RMS
hcuot "Nov 14 01:30:39 1983" 'ucadr/ucode.lisp.19'		     # Author: RMS
hcuot "Aug 25 05:24:02 1982" 'wind/baswin.text.7'		     # Author: RMS
hcuot "Aug  9 02:24:49 1983" 'wind/blink.text.21'		     # Author: RMS
hcuot "Nov 16 05:33:30 1983" 'wind/choice.text.94'		     # Author: RMS
hcuot "Jan 22 09:23:05 1984" 'wind/choice.text.95'		     # Author: RMS
hcuot "Jul 23 04:52:44 1983" 'wind/edges.text.14'		     # Author: RMS
hcuot "Aug 25 05:25:39 1982" 'wind/emack.fasl.1'		     # Author: RMS
hcuot "Aug 25 05:25:27 1982" 'wind/emack.lisp.36'		     # Author: RMS
hcuot "Apr  7 15:53:58 1984" 'wind/emack.lisp.37'		     # Author: MLY
hcuot "Jul  6 04:46:40 1983" 'wind/fonts.text.17'		     # Author: RMS
hcuot "Aug  9 02:21:25 1983" 'wind/frames.text.14'		     # Author: RMS
hcuot "Aug  8 05:44:03 1983" 'wind/grafix.text.24'		     # Author: RMS
hcuot "Aug  8 07:11:30 1983" 'wind/input.text.24'		     # Author: RMS
hcuot "Sep 30 08:33:21 1983" 'wind/input.text.26'		     # Author: RMS
hcuot "Aug 25 05:26:36 1982" 'wind/lstfla.lisp.5'		     # Author: RMS
hcuot "Apr  7 15:55:43 1984" 'wind/lstfla.lisp.6'		     # Author: MLY
hcuot "Aug  8 06:15:43 1983" 'wind/margin.text.20'		     # Author: RMS
hcuot "Aug 23 15:42:25 1983" 'wind/misc.text.24'		     # Author: RMS
hcuot "Aug  8 11:24:58 1983" 'wind/mouse.text.33'		     # Author: RMS
hcuot "Aug 25 05:27:06 1982" 'wind/operat.bolio.1'		     # Author: RMS
hcuot "Aug 25 05:26:46 1982" 'wind/operat.text.45'		     # Author: RMS
hcuot "Aug 25 05:27:12 1982" 'wind/outlin.text.2'		     # Author: RMS
hcuot "Aug  9 04:51:45 1983" 'wind/output.text.27'		     # Author: RMS
hcuot "Oct 29 02:54:29 1983" 'wind/output.text.28'		     # Author: RMS
hcuot "Aug  8 07:11:36 1983" 'wind/select.text.21'		     # Author: RMS
hcuot "Nov 19 01:30:08 1983" 'wind/select.text.22'		     # Author: RMS
hcuot "Aug  8 10:42:49 1983" 'wind/tscrol.text.37'		     # Author: RMS
hcuot "Jul  6 05:25:11 1983" 'wind/typout.text.17'		     # Author: RMS
hcuot "Aug  9 03:11:35 1983" 'wind/windo1.text.51'		     # Author: RMS
hcuot "Feb  4 11:57:38 1984" 'wind/windo1.text.52'		     # Author: RMS
hcuot "Jul  3 06:44:16 1983" 'wind/windoc.bolio.14'		     # Author: GSB
hcuot "Jun 21 06:34:00 1983" 'wind/windoc.dict.1'		     # Author: RMS
hcuot "Aug  9 03:12:27 1983" 'wind/windoc.log.12'		     # Author: RMS
hcuot "Aug  9 03:10:00 1983" 'wind/windoc.text.15'		     # Author: RMS
hcuot "Aug  9 03:23:20 1983" 'wind/windoc.vars.33'		     # Author: RMS
hcuot "Aug 25 05:30:20 1982" 'wind/window.gloss.1'		     # Author: RMS
hcuot "Aug 25 05:30:32 1982" 'wind/window.manual.1'		     # Author: RMS
hcuot "Aug 25 05:30:42 1982" 'wind/window.methds.1'		     # Author: RMS
hcuot "Aug 25 05:30:47 1982" 'wind/winman.text.1'		     # Author: RMS
hcuot "Sep 20 00:19:55 1983" 'window/basstr.lisp.346'		     # Author: RMS
hcuot "Apr  7 20:52:51 1984" 'window/basstr.lisp.361'		     # Author: MLY
hcuot "May 26 11:21:27 1984" 'window/basstr.lisp.363'		     # Author: MLY
hcuot "Jun 13 21:10:16 1984" 'window/basstr.lisp.364'		     # Author: MERMAN.JAN
hcuot "Jun 14 07:12:33 1984" 'window/basstr.lisp.365'		     # Author: RPK
hcuot "Jun 15 10:17:11 1984" 'window/basstr.lisp.366'		     # Author: RPK
hcuot "Jun 15 10:17:39 1984" 'window/basstr.lisp.367'		     # Author: RPK
hcuot "Jun 24 22:24:20 1984" 'window/basstr.lisp.368'		     # Author: RMS
hcuot "Jun 24 23:32:56 1984" 'window/basstr.lisp.369'		     # Author: RMS
hcuot "Jun 25 00:03:58 1984" 'window/basstr.lisp.370'		     # Author: RMS
hcuot "Jun  6 08:05:24 1984" 'window/basstr.qfasl.363'		     # Author: RMS
hcuot "Jun 14 07:36:20 1984" 'window/basstr.qfasl.365'		     # Author: RPK
hcuot "Jun 21 08:50:10 1984" 'window/basstr.qfasl.367'		     # Author: MLY
hcuot "Nov 30 01:23:44 1983" 'window/baswin.lisp.553'		     # Author: RMS
hcuot "Apr 15 07:28:51 1984" 'window/baswin.lisp.559'		     # Author: MLY
hcuot "Jun  6 11:46:15 1984" 'window/baswin.lisp.561'		     # Author: MLY
hcuot "Jun 12 16:36:34 1984" 'window/baswin.qfasl.561'		     # Author: RMS
hcuot "Mar  3 04:18:07 1984" 'window/choice.lisp.110'		     # Author: RMS
hcuot "Apr 22 11:43:14 1984" 'window/choice.lisp.111'		     # Author: MLY
hcuot "Jun  5 21:49:08 1984" 'window/choice.lisp.113'		     # Author: RMS
hcuot "Jun 12 04:53:07 1984" 'window/choice.lisp.114'		     # Author: ECC
hcuot "Jun 12 17:00:43 1984" 'window/choice.qfasl.114'		     # Author: RMS
hcuot "Aug  3 10:36:59 1983" 'window/cold.lisp.105'		     # Author: RMS
hcuot "Nov 10 07:11:35 1983" 'window/cold.lisp.112'		     # Author: RMS
hcuot "Mar 26 10:45:19 1984" 'window/cold.lisp.127'		     # Author: MLY
hcuot "Jun 17 04:26:37 1984" 'window/cold.lisp.128'		     # Author: MLY
hcuot "Jun  6 02:20:03 1984" 'window/cold.qfasl.127'		     # Author: RMS
hcuot "Jun 17 04:29:23 1984" 'window/cold.qfasl.128'		     # Author: MLY
hcuot "Oct 12 16:29:35 1983" 'window/color.lisp.65'		     # Author: ALR
hcuot "Dec 11 05:41:19 1983" 'window/color.lisp.66'		     # Author: RMS
hcuot "Jun 13 10:38:51 1984" 'window/color.lisp.67'		     # Author: ECC
hcuot "Jun  6 12:10:26 1984" 'window/color.qfasl.66'		     # Author: RMS
hcuot "Jun 13 20:40:01 1984" 'window/color.qfasl.67'		     # Author: MLY
hcuot "Jun 29 09:12:56 1982" 'window/cometh.lisp.23'		     # Author: RMS
hcuot "Aug  4 12:29:09 1983" 'window/cometh.lisp.26'		     # Author: RMS
hcuot "Oct 25 04:13:43 1983" 'window/cometh.oqfasl.26'		     # Author: RMS
hcuot "Jun 18 07:31:49 1984" 'window/cometh.qfasl.26'		     # Author: RMS
hcuot "Jun 29 09:13:09 1982" 'window/csrpos.lisp.9'		     # Author: RMS
hcuot "Jun  6 08:55:30 1984" 'window/csrpos.qfasl.9'		     # Author: RMS
hcuot "Nov 17 09:07:53 1983" 'window/eh.lisp.293'		     # Author: RMS
hcuot "Mar 22 07:07:03 1984" 'window/eh.lisp.320'		     # Author: RMS
hcuot "May  7 01:12:47 1984" 'window/eh.lisp.321'		     # Author: RMS
hcuot "Jun  9 13:24:43 1984" 'window/eh.lisp.327'		     # Author: MLY
hcuot "Jun 14 10:33:38 1984" 'window/eh.lisp.328'		     # Author: RMS
hcuot "Jun 16 01:04:40 1984" 'window/eh.lisp.329'		     # Author: RMS
hcuot "Jun 16 11:01:24 1984" 'window/eh.lisp.330'		     # Author: MLY
hcuot "Jun 17 04:54:46 1984" 'window/eh.lisp.331'		     # Author: MLY
hcuot "Jun 14 10:57:20 1984" 'window/eh.qfasl.328'		     # Author: RMS
hcuot "Jun 16 01:04:57 1984" 'window/eh.qfasl.329'		     # Author: RMS
hcuot "Jun 16 11:01:39 1984" 'window/eh.qfasl.330'		     # Author: MLY
hcuot "Jun 17 04:55:05 1984" 'window/eh.qfasl.331'		     # Author: MLY
hcuot "Feb 21 03:48:18 1984" 'window/ehc.lisp.220'		     # Author: MLY
hcuot "May 15 14:03:22 1984" 'window/ehc.lisp.223'		     # Author: MLY
hcuot "Jun  6 06:08:48 1984" 'window/ehc.lisp.226'		     # Author: MLY
hcuot "Jun 17 02:21:36 1984" 'window/ehc.qfasl.226'		     # Author: RMS
hcuot "Feb 21 03:39:03 1984" 'window/ehf.lisp.195'		     # Author: MLY
hcuot "May 11 13:24:25 1984" 'window/ehf.lisp.197'		     # Author: MLY
hcuot "Jun  9 13:25:09 1984" 'window/ehf.lisp.207'		     # Author: MLY
hcuot "Jun 11 13:59:18 1984" 'window/ehf.lisp.208'		     # Author: RMS
hcuot "Jun 12 20:33:43 1984" 'window/ehf.lisp.209'		     # Author: RMS
hcuot "Jun 14 11:31:38 1984" 'window/ehf.lisp.210'		     # Author: RMS
hcuot "Jun 14 11:48:33 1984" 'window/ehf.lisp.211'		     # Author: RMS
hcuot "Jun 21 02:52:44 1984" 'window/ehf.lisp.212'		     # Author: MLY
hcuot "Jun  6 12:20:09 1984" 'window/ehf.qfasl.203'		     # Author: RMS
hcuot "Jun 13 07:51:51 1984" 'window/ehf.qfasl.209'		     # Author: MLY
hcuot "Jun 14 11:32:04 1984" 'window/ehf.qfasl.210'		     # Author: RMS
hcuot "Jun 14 11:48:52 1984" 'window/ehf.qfasl.211'		     # Author: RMS
hcuot "Jun 21 09:49:53 1984" 'window/ehf.qfasl.212'		     # Author: MLY
hcuot "Jan 11 14:33:56 1984" 'window/ehw.lisp.107'		     # Author: DANIEL.G.MLY
hcuot "Mar 22 11:38:18 1984" 'window/ehw.lisp.108'		     # Author: MLY
hcuot "May 16 12:21:58 1984" 'window/ehw.lisp.109'		     # Author: RMS
hcuot "Jun  6 12:44:48 1984" 'window/ehw.qfasl.109'		     # Author: RMS
hcuot "Nov 23 16:24:15 1983" 'window/fed.lisp.194'		     # Author: DANIEL.G.MLY
hcuot "Feb  6 14:55:35 1984" 'window/fed.lisp.199'		     # Author: MLY
hcuot "Jun  6 12:01:32 1984" 'window/fed.qfasl.199'		     # Author: RMS
hcuot "Jun 16 15:41:36 1983" 'window/frame.lisp.164'		     # Author: RMS
hcuot "Apr 11 03:28:15 1984" 'window/frame.lisp.165'		     # Author: MLY
hcuot "Jun  6 08:41:54 1984" 'window/frame.qfasl.165'		     # Author: RMS
hcuot "Jun  4 03:03:15 1984" 'window/graphics.lisp.1'		     # Author: MLY
hcuot "Jun  6 08:13:00 1984" 'window/graphics.qfasl.1'		     # Author: RMS
hcuot "Jan  7 14:22:48 1984" 'window/inspct.lisp.152'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 10:01:37 1984" 'window/inspct.lisp.153'		     # Author: MLY
hcuot "Jun 12 20:10:18 1984" 'window/inspct.lisp.154'		     # Author: RMS
hcuot "Jun  6 08:56:04 1984" 'window/inspct.qfasl.152'		     # Author: RMS
hcuot "Jun 12 19:27:35 1984" 'window/inspct.qfasl.153'		     # Author: RMS
hcuot "Jun 13 20:23:19 1984" 'window/inspct.qfasl.154'		     # Author: MLY
hcuot "Oct 30 06:32:13 1983" 'window/menu.lisp.98'		     # Author: RMS
hcuot "Nov 18 10:54:40 1983" 'window/menu.lisp.102'		     # Author: RMS
hcuot "Feb  5 10:43:00 1984" 'window/menu.lisp.103'		     # Author: MLY
hcuot "Jun  6 08:15:38 1984" 'window/menu.qfasl.103'		     # Author: RMS
hcuot "Dec 28 10:07:37 1983" 'window/mouse.lisp.245'		     # Author: RMS
hcuot "May  1 23:20:08 1984" 'window/mouse.lisp.246'		     # Author: MLY
hcuot "May 16 19:35:36 1984" 'window/mouse.lisp.247'		     # Author: MLY
hcuot "Jun  6 08:02:14 1984" 'window/mouse.qfasl.247'		     # Author: RMS
hcuot "Nov 19 02:27:34 1983" 'window/peek.lisp.144'		     # Author: RMS
hcuot "Jun  9 13:23:51 1984" 'window/peek.lisp.145'		     # Author: MLY
hcuot "Jun  6 09:06:22 1984" 'window/peek.qfasl.144'		     # Author: RMS
hcuot "Jun 12 19:34:25 1984" 'window/peek.qfasl.145'		     # Author: RMS
hcuot "Oct 12 19:07:51 1983" 'window/peekch.lisp.26'		     # Author: ALR
hcuot "May 25 02:14:17 1984" 'window/peekch.lisp.27'		     # Author: RPK
hcuot "Oct 25 05:15:23 1983" 'window/peekch.qfasl.26'		     # Author: RMS
hcuot "Jun 29 09:46:44 1982" 'window/peekfs.lisp.9'		     # Author: RMS
hcuot "Oct 25 05:17:11 1983" 'window/peekfs.qfasl.9'		     # Author: RMS
hcuot "Jun 29 09:46:50 1982" 'window/quest.lisp.42'		     # Author: RMS
hcuot "Apr  7 15:56:29 1984" 'window/quest.lisp.43'		     # Author: MLY
hcuot "Nov 30 05:28:54 1983" 'window/rh.lisp.146'		     # Author: RMS
hcuot "Mar 27 09:03:32 1984" 'window/rh.lisp.159'		     # Author: MLY
hcuot "Apr 18 09:15:27 1984" 'window/rh.lisp.160'		     # Author: MLY
hcuot "Jun 18 11:07:59 1984" 'window/rh.qfasl.160'		     # Author: RMS
hcuot "Nov 21 08:04:19 1983" 'window/scred.lisp.106'		     # Author: RMS
hcuot "May  3 19:34:28 1984" 'window/scred.lisp.107'		     # Author: RPK
hcuot "Jun  6 11:46:58 1984" 'window/scred.lisp.110'		     # Author: MLY
hcuot "Jun 18 04:57:58 1984" 'window/scred.lisp.111'		     # Author: MLY
hcuot "Jun 12 19:20:04 1984" 'window/scred.qfasl.110'		     # Author: RMS
hcuot "Jun 21 08:55:35 1984" 'window/scred.qfasl.111'		     # Author: MLY
hcuot "Jul 25 10:19:44 1983" 'window/scrman.lisp.165'		     # Author: RMS
hcuot "Oct 25 05:28:26 1983" 'window/scrman.qfasl.165'		     # Author: RMS
hcuot "Jul 25 10:21:03 1983" 'window/scroll.lisp.173'		     # Author: RMS
hcuot "Oct  6 17:48:59 1983" 'window/scroll.lisp.175'		     # Author: RMS
hcuot "Oct 25 05:31:33 1983" 'window/scroll.qfasl.175'		     # Author: RMS
hcuot "Feb  4 00:57:09 1984" 'window/sheet.lisp.554'		     # Author: MLY
hcuot "May 27 15:39:23 1984" 'window/sheet.lisp.555'		     # Author: MLY
hcuot "Jun  6 11:46:04 1984" 'window/sheet.lisp.557'		     # Author: MLY
hcuot "Jun 12 16:27:47 1984" 'window/sheet.qfasl.557'		     # Author: RMS
hcuot "Feb 16 16:22:54 1984" 'window/shwarm.lisp.318'		     # Author: RPK
hcuot "Apr  6 08:58:01 1984" 'window/shwarm.lisp.319'		     # Author: RPK
hcuot "May 20 22:13:35 1984" 'window/shwarm.lisp.321'		     # Author: RMS
hcuot "Jun  4 03:03:20 1984" 'window/shwarm.lisp.322'		     # Author: MLY
hcuot "Jun  6 07:48:16 1984" 'window/shwarm.qfasl.322'		     # Author: RMS
hcuot "Nov 21 10:54:11 1983" 'window/stream.lisp.116'		     # Author: RMS
hcuot "Apr  7 20:17:05 1984" 'window/stream.lisp.134'		     # Author: MLY
hcuot "May 27 15:38:37 1984" 'window/stream.lisp.136'		     # Author: MLY
hcuot "Jun  6 11:46:33 1984" 'window/stream.lisp.139'		     # Author: MLY
hcuot "Jun 17 13:29:19 1984" 'window/stream.lisp.140'		     # Author: RG
hcuot "Jun 17 13:48:05 1984" 'window/stream.lisp.141'		     # Author: RG
hcuot "Jun 18 05:50:48 1984" 'window/stream.lisp.142'		     # Author: MLY
hcuot "Jun 18 06:53:08 1984" 'window/stream.lisp.143'		     # Author: MLY
hcuot "Jun  6 08:10:42 1984" 'window/stream.qfasl.138'		     # Author: RMS
hcuot "Jun 12 16:46:29 1984" 'window/stream.qfasl.139'		     # Author: RMS
hcuot "Jun 17 13:29:26 1984" 'window/stream.qfasl.140'		     # Author: RG
hcuot "Jun 17 14:23:22 1984" 'window/stream.qfasl.141'		     # Author: RG
hcuot "Jun 18 11:13:24 1984" 'window/stream.qfasl.143'		     # Author: LISPM
hcuot "Oct 25 06:19:35 1983" 'window/supdup.lisp.266'		     # Author: RMS
hcuot "Apr  6 09:36:45 1984" 'window/supdup.lisp.269'		     # Author: RPK
hcuot "Jun  6 09:12:53 1984" 'window/supdup.lisp.271'		     # Author: MLY
hcuot "Jun 12 20:14:53 1984" 'window/supdup.lisp.272'		     # Author: RMS
hcuot "Jun 20 03:02:58 1984" 'window/supdup.lisp.273'		     # Author: MLY
hcuot "Jun 21 09:08:21 1984" 'window/supdup.lisp.274'		     # Author: RG
hcuot "Jun 25 00:38:41 1984" 'window/supdup.lisp.275'		     # Author: RMS
hcuot "Jun 12 19:37:59 1984" 'window/supdup.qfasl.271'		     # Author: RMS
hcuot "Jun 18 04:29:44 1984" 'window/supdup.qfasl.272'		     # Author: MLY
hcuot "Jun 21 09:00:50 1984" 'window/supdup.qfasl.273'		     # Author: MLY
hcuot "Nov 29 05:56:40 1983" 'window/sysmen.lisp.173'		     # Author: RMS
hcuot "Mar 18 04:49:43 1984" 'window/sysmen.lisp.176'		     # Author: MLY
hcuot "Jun  6 11:46:38 1984" 'window/sysmen.lisp.177'		     # Author: MLY
hcuot "Jun 12 16:48:40 1984" 'window/sysmen.qfasl.177'		     # Author: RMS
hcuot "Jun 29 09:51:17 1982" 'window/task.list.1'		     # Author: RMS
hcuot "Dec 26 03:16:03 1983" 'window/tscrol.lisp.69'		     # Author: RMS
hcuot "Jun  6 11:47:06 1984" 'window/tscrol.lisp.70'		     # Author: MLY
hcuot "Jun  6 08:39:29 1984" 'window/tscrol.qfasl.69'		     # Author: RMS
hcuot "Jun 12 19:25:15 1984" 'window/tscrol.qfasl.70'		     # Author: RMS
hcuot "Nov 20 12:48:36 1983" 'window/tvdefs.lisp.277'		     # Author: RMS
hcuot "Apr  7 19:54:59 1984" 'window/tvdefs.lisp.278'		     # Author: MLY
hcuot "Jun 17 13:39:06 1984" 'window/tvdefs.lisp.281'		     # Author: RG
hcuot "Jun  6 06:00:00 1984" 'window/tvdefs.lisp.282'		     # Author: RG
hcuot "Jun 12 16:19:52 1984" 'window/tvdefs.qfasl.280'		     # Author: RMS
hcuot "Jun 17 14:03:59 1984" 'window/tvdefs.qfasl.282'		     # Author: RG
hcuot "Aug 20 14:24:51 1983" 'window/typwin.lisp.105'		     # Author: RMS
hcuot "Mar  4 08:08:46 1984" 'window/typwin.lisp.117'		     # Author: MLY
hcuot "May  1 23:22:28 1984" 'window/typwin.lisp.118'		     # Author: MLY
hcuot "Jun  6 08:36:19 1984" 'window/typwin.qfasl.118'		     # Author: RMS
hcuot "Nov 12 03:33:28 1983" 'window/wholin.lisp.84'		     # Author: RMS
hcuot "Jan 18 20:56:57 1984" 'window/wholin.lisp.85'		     # Author: RPK
hcuot "Jun  6 11:02:31 1984" 'window/wholin.lisp.86'		     # Author: MLY
hcuot "Jun 17 13:03:11 1984" 'window/wholin.lisp.87'		     # Author: RG
hcuot "Jun  6 08:00:00 1984" 'window/wholin.qfasl.85'		     # Author: RMS
hcuot "Jun 12 16:44:13 1984" 'window/wholin.qfasl.86'		     # Author: RMS
hcuot "Jun 17 13:03:32 1984" 'window/wholin.qfasl.87'		     # Author: RG
hcuot "Dec  9 23:26:17 1983" 'window/winddoc.lisp.2'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 06:10:18 1984" 'zmail/bug.zmail.1'		     # Author: RMS
hcuot "Apr 14 07:50:23 1983" 'zmail/button.lisp.21'		     # Author: RMS
hcuot "Feb 19 14:39:57 1984" 'zmail/button.lisp.22'		     # Author: MLY
hcuot "Apr 11 03:28:22 1984" 'zmail/button.lisp.23'		     # Author: MLY
hcuot "Nov 15 10:36:26 1983" 'zmail/button.qfasl.21'		     # Author: RMS
hcuot "Feb 14 11:23:32 1983" 'zmail/cometh.lisp.49'		     # Author: RMS
hcuot "Sep 23 08:01:04 1983" 'zmail/cometh.lisp.50'		     # Author: RMS
hcuot "Apr  7 15:57:16 1984" 'zmail/cometh.lisp.51'		     # Author: MLY
hcuot "Nov 24 00:17:41 1983" 'zmail/cometh.qfasl.50'		     # Author: RMS
hcuot "Nov 15 10:44:02 1983" 'zmail/comnds.lisp.567'		     # Author: RMS
hcuot "Dec 31 03:02:15 1983" 'zmail/comnds.lisp.572'		     # Author: DANIEL.G.MLY
hcuot "Apr 18 09:42:20 1984" 'zmail/comnds.lisp.578'		     # Author: RPK
hcuot "Apr 22 00:16:43 1984" 'zmail/comnds.lisp.579'		     # Author: RMS
hcuot "Nov 22 11:19:13 1983" 'zmail/comnds.qfasl.567'		     # Author: RMS
hcuot "Aug 17 20:36:10 1983" 'zmail/defs.lisp.268'		     # Author: RMS
hcuot "Dec 11 02:49:48 1983" 'zmail/defs.lisp.269'		     # Author: DANIEL.G.MLY
hcuot "Apr 18 09:42:50 1984" 'zmail/defs.lisp.270'		     # Author: RPK
hcuot "Nov 15 08:58:36 1983" 'zmail/defs.qfasl.268'		     # Author: RMS
hcuot "Oct 28 07:04:23 1983" 'zmail/filter.lisp.343'		     # Author: RMS
hcuot "Nov 28 13:49:03 1983" 'zmail/filter.lisp.346'		     # Author: RMS
hcuot "Feb 23 13:39:32 1984" 'zmail/filter.lisp.350'		     # Author: MLY
hcuot "Apr 11 06:36:04 1984" 'zmail/filter.lisp.352'		     # Author: MLY
hcuot "Nov 28 14:03:40 1983" 'zmail/filter.qfasl.346'		     # Author: RMS
hcuot "Jun 29 11:22:50 1982" 'zmail/info.mail.1'		     # Author: RMS
hcuot "Nov 22 10:13:20 1983" 'zmail/lex733.lisp.10'		     # Author: RMS
hcuot "Dec  3 14:55:46 1983" 'zmail/lex733.lisp.13'		     # Author: RMS
hcuot "Apr 30 15:49:02 1984" 'zmail/lex733.lisp.14'		     # Author: MLY
hcuot "Dec  3 14:56:06 1983" 'zmail/lex733.qfasl.5'		     # Author: RMS
hcuot "Jun 29 11:23:06 1982" 'zmail/lm.lisp.3'			     # Author: RMS
hcuot "Apr  7 15:57:50 1984" 'zmail/lm.lisp.4'			     # Author: MLY
hcuot "Jun 29 11:23:09 1982" 'zmail/lmcsrv.lisp.4'		     # Author: RMS
hcuot "Apr  7 15:58:15 1984" 'zmail/lmcsrv.lisp.5'		     # Author: MLY
hcuot "Sep 21 04:59:02 1983" 'zmail/lmfile.lisp.3'		     # Author: RMS
hcuot "Apr  7 15:59:55 1984" 'zmail/lmfile.lisp.4'		     # Author: MLY
hcuot "Dec 18 08:47:50 1983" 'zmail/lmfile.qfasl.3'		     # Author: RMS
hcuot "Oct 24 03:34:02 1983" 'zmail/mail.lisp.299'		     # Author: RMS
hcuot "Apr 22 00:16:27 1984" 'zmail/mail.lisp.306'		     # Author: RMS
hcuot "Apr 30 15:49:21 1984" 'zmail/mail.lisp.307'		     # Author: MLY
hcuot "May 16 11:06:22 1984" 'zmail/mail.lisp.308'		     # Author: RMS
hcuot "Nov 15 10:08:53 1983" 'zmail/mail.qfasl.299'		     # Author: RMS
hcuot "Sep 21 23:30:47 1983" 'zmail/mfhost.lisp.55'		     # Author: EB.TFC
hcuot "Oct  9 11:09:21 1983" 'zmail/mfhost.lisp.56'		     # Author: RMS
hcuot "Apr  7 16:02:19 1984" 'zmail/mfhost.lisp.57'		     # Author: MLY
hcuot "Nov 15 09:51:50 1983" 'zmail/mfhost.qfasl.56'		     # Author: RMS
hcuot "Dec  3 22:47:32 1983" 'zmail/mfiles.lisp.320'		     # Author: RMS
hcuot "Dec 11 10:55:49 1983" 'zmail/mfiles.lisp.321'		     # Author: DANIEL.G.MLY
hcuot "Apr 11 06:35:11 1984" 'zmail/mfiles.lisp.322'		     # Author: MLY
hcuot "Nov 15 09:44:21 1983" 'zmail/mfiles.qfasl.319'		     # Author: RMS
hcuot "Mar 23 11:01:45 1983" 'zmail/mult.lisp.22'		     # Author: RMS
hcuot "Jun 16 10:40:07 1983" 'zmail/mult.lisp.23'		     # Author: RMS
hcuot "Apr  7 16:02:56 1984" 'zmail/mult.lisp.24'		     # Author: MLY
hcuot "Nov 15 10:34:39 1983" 'zmail/mult.qfasl.23'		     # Author: RMS
hcuot "Dec 10 23:37:49 1983" 'zmail/parse.lisp.52'		     # Author: DANIEL.G.MLY
hcuot "Nov 15 11:02:07 1983" 'zmail/patch.directory.13'		     # Author: RMS
hcuot "Oct 28 07:03:34 1983" 'zmail/patch-51.directory.21'	     # Author: RMS
hcuot "Aug 23 00:25:50 1983" 'zmail/patch-51-1.lisp.1'		     # Author: RMS
hcuot "Aug 23 00:25:56 1983" 'zmail/patch-51-1.qfasl.1'		     # Author: RMS
hcuot "Sep  7 21:56:01 1983" 'zmail/patch-51-2.lisp.1'		     # Author: RMS
hcuot "Sep  7 21:56:11 1983" 'zmail/patch-51-2.qfasl.1'		     # Author: RMS
hcuot "Sep 21 23:30:38 1983" 'zmail/patch-51-3.lisp.6'		     # Author: EB.TFC
hcuot "Sep 21 23:49:06 1983" 'zmail/patch-51-3.qfasl.6'		     # Author: EB.TFC
hcuot "Sep 21 23:26:54 1983" 'zmail/patch-51-4.lisp.2'		     # Author: EB.TFC
hcuot "Sep 21 23:52:02 1983" 'zmail/patch-51-4.qfasl.2'		     # Author: EB.TFC
hcuot "Sep 23 08:11:23 1983" 'zmail/patch-51-5.lisp.2'		     # Author: RMS
hcuot "Sep 23 08:11:28 1983" 'zmail/patch-51-5.qfasl.2'		     # Author: RMS
hcuot "Sep 26 05:52:32 1983" 'zmail/patch-51-6.lisp.1'		     # Author: RMS
hcuot "Sep 26 05:52:40 1983" 'zmail/patch-51-6.qfasl.1'		     # Author: RMS
hcuot "Oct 14 07:56:33 1983" 'zmail/patch-51-7.lisp.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 14 07:56:39 1983" 'zmail/patch-51-7.qfasl.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 22 08:30:39 1983" 'zmail/patch-51-8.lisp.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 22 08:30:48 1983" 'zmail/patch-51-8.qfasl.1'		     # Author: DANIEL.G.MLY
hcuot "Oct 28 07:02:36 1983" 'zmail/patch-51-9.lisp.1'		     # Author: RMS
hcuot "Oct 28 07:02:49 1983" 'zmail/patch-51-9.qfasl.1'		     # Author: RMS
hcuot "Apr 23 17:32:33 1984" 'zmail/patch-53.directory.46'	     # Author: MLY
hcuot "Apr 26 05:12:09 1984" 'zmail/patch-53.directory.47'	     # Author: MLY
hcuot "Dec  7 12:43:09 1983" 'zmail/patch-53-1.lisp.2'		     # Author: RMS
hcuot "Dec  7 12:43:52 1983" 'zmail/patch-53-1.qfasl.2'		     # Author: RMS
hcuot "Jan 30 06:21:26 1984" 'zmail/patch-53-10.lisp.1'		     # Author: RPK
hcuot "Jan 30 06:21:32 1984" 'zmail/patch-53-10.qfasl.1'	     # Author: RPK
hcuot "Feb 16 07:57:45 1984" 'zmail/patch-53-11.lisp.2'		     # Author: MLY
hcuot "Feb 16 07:57:48 1984" 'zmail/patch-53-11.qfasl.2'	     # Author: MLY
hcuot "Feb 23 13:40:40 1984" 'zmail/patch-53-12.lisp.2'		     # Author: MLY
hcuot "Feb 23 13:40:45 1984" 'zmail/patch-53-12.qfasl.2'	     # Author: MLY
hcuot "Mar  4 08:41:33 1984" 'zmail/patch-53-13.lisp.1'		     # Author: MLY
hcuot "Mar  4 08:41:37 1984" 'zmail/patch-53-13.qfasl.1'	     # Author: MLY
hcuot "Mar 24 17:24:31 1984" 'zmail/patch-53-14.lisp.2'		     # Author: RMS
hcuot "Mar 24 17:24:35 1984" 'zmail/patch-53-14.qfasl.2'	     # Author: RMS
hcuot "Apr 11 07:05:23 1984" 'zmail/patch-53-15.lisp.3'		     # Author: MLY
hcuot "Apr 11 07:05:32 1984" 'zmail/patch-53-15.qfasl.3'	     # Author: MLY
hcuot "Apr 18 09:41:32 1984" 'zmail/patch-53-16.lisp.1'		     # Author: RPK
hcuot "Apr 18 09:41:38 1984" 'zmail/patch-53-16.qfasl.1'	     # Author: RPK
hcuot "Apr 22 00:46:53 1984" 'zmail/patch-53-17.lisp.2'		     # Author: RMS
hcuot "Apr 22 00:47:01 1984" 'zmail/patch-53-17.qfasl.2'	     # Author: RMS
hcuot "Dec  6 05:18:26 1983" 'zmail/patch-53-2.lisp.1'		     # Author: RMS
hcuot "Dec  6 05:18:36 1983" 'zmail/patch-53-2.qfasl.1'		     # Author: RMS
hcuot "Dec 13 06:15:17 1983" 'zmail/patch-53-3.lisp.2'		     # Author: RMS
hcuot "Dec 13 06:15:23 1983" 'zmail/patch-53-3.qfasl.2'		     # Author: RMS
hcuot "Dec 14 08:54:56 1983" 'zmail/patch-53-5.lisp.1'		     # Author: RMS
hcuot "Dec 14 08:55:02 1983" 'zmail/patch-53-5.qfasl.1'		     # Author: RMS
hcuot "Jan  3 18:55:45 1984" 'zmail/patch-53-6.lisp.2'		     # Author: PGS
hcuot "Jan  3 18:55:54 1984" 'zmail/patch-53-6.qfasl.2'		     # Author: PGS
hcuot "Jan  1 01:08:53 1984" 'zmail/patch-53-7.lisp.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 01:09:00 1984" 'zmail/patch-53-7.qfasl.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 15:59:26 1984" 'zmail/patch-53-8.lisp.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 15:59:30 1984" 'zmail/patch-53-8.qfasl.3'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 16:00:18 1984" 'zmail/patch-53-9.lisp.2'		     # Author: DANIEL.G.MLY
hcuot "Jan  1 16:00:22 1984" 'zmail/patch-53-9.qfasl.2'		     # Author: DANIEL.G.MLY
hcuot "Jun 29 11:28:11 1982" 'zmail/poop.text.35'		     # Author: RMS
hcuot "Mar  1 21:59:09 1983" 'zmail/profil.lisp.104'		     # Author: RMS
hcuot "Mar 24 17:25:30 1984" 'zmail/profil.lisp.117'		     # Author: RMS
hcuot "Apr 11 03:29:40 1984" 'zmail/profil.lisp.118'		     # Author: MLY
hcuot "Apr 16 21:36:09 1984" 'zmail/profil.lisp.119'		     # Author: MLY
hcuot "Jun 21 02:53:43 1984" 'zmail/profil.lisp.120'		     # Author: MLY
hcuot "Nov 28 14:14:24 1983" 'zmail/profil.qfasl.110'		     # Author: RMS
hcuot "Feb 14 10:57:47 1983" 'zmail/refer.lisp.5'		     # Author: RMS
hcuot "Apr  7 16:03:55 1984" 'zmail/refer.lisp.6'		     # Author: MLY
hcuot "Nov 15 09:57:14 1983" 'zmail/refer.qfasl.5'		     # Author: RMS
hcuot "Jun 24 05:45:55 1983" 'zmail/rfc733.lisp.54'		     # Author: GUMBY
hcuot "Nov 22 11:12:10 1983" 'zmail/rfc733.lisp.55'		     # Author: RMS
hcuot "Apr 30 14:38:24 1984" 'zmail/rfc733.lisp.56'		     # Author: MLY
hcuot "Nov 22 11:32:32 1983" 'zmail/rfc733.qfasl.55'		     # Author: RMS
hcuot "Dec  8 07:00:35 1982" 'zmail/top.lisp.497'		     # Author: RMS
hcuot "Apr 16 07:17:47 1983" 'zmail/top.lisp.540'		     # Author: RMS
hcuot "Aug 17 20:36:16 1983" 'zmail/top.lisp.546'		     # Author: RMS
hcuot "Dec 31 03:02:48 1983" 'zmail/top.lisp.551'		     # Author: DANIEL.G.MLY
hcuot "Jun 15 17:35:56 1984" 'zmail/top.lisp.552'		     # Author: MERMAN.JAN
hcuot "Nov 24 00:24:32 1983" 'zmail/top.qfasl.549'		     # Author: RMS
hcuot "Nov  8 08:17:55 1983" 'zmail/window.lisp.333'		     # Author: RMS
hcuot "Dec 31 03:03:32 1983" 'zmail/window.lisp.339'		     # Author: DANIEL.G.MLY
hcuot "Apr 11 06:35:53 1984" 'zmail/window.lisp.340'		     # Author: MLY
hcuot "Nov 15 10:17:12 1983" 'zmail/window.qfasl.333'		     # Author: RMS
hcuot "Jun 29 11:04:18 1982" 'zwei/.comnd.text.1'		     # Author: RMS
hcuot "Jun 29 11:04:27 1982" 'zwei/atsign.xfile.1'		     # Author: RMS
hcuot "Mar 16 12:02:10 1983" 'zwei/bdired.lisp.39'		     # Author: HANSON
hcuot "Dec 28 08:42:04 1983" 'zwei/bdired.lisp.40'		     # Author: RMS
hcuot "Jun  9 13:24:16 1984" 'zwei/bdired.lisp.41'		     # Author: MLY
hcuot "Jun  6 10:55:53 1984" 'zwei/bdired.qfasl.40'		     # Author: RMS
hcuot "Jun 12 19:59:33 1984" 'zwei/bdired.qfasl.41'		     # Author: RMS
hcuot "Jan 27 19:35:00 1983" 'zwei/bug.bugs7.1'			     # Author: RMS
hcuot "Jun 26 01:05:20 1984" 'zwei/bug.zwei.1'			     # Author: OPERATOR
hcuot "Oct  8 10:11:11 1983" 'zwei/bug-zwei.text.1'		     # Author: TIM
hcuot "Jun 29 11:04:29 1982" 'zwei/bugs.bugs.1'			     # Author: RMS
hcuot "Jun 29 11:05:20 1982" 'zwei/bugs.bugs6.1'		     # Author: RMS
hcuot "Jun 29 11:05:52 1982" 'zwei/bugs.status.1'		     # Author: RMS
hcuot "Feb 19 15:27:21 1984" 'zwei/coma.lisp.100'		     # Author: MLY
hcuot "Mar 15 09:23:53 1984" 'zwei/coma.lisp.101'		     # Author: TIM
hcuot "Jun  6 10:25:29 1984" 'zwei/coma.qfasl.101'		     # Author: RMS
hcuot "Nov  7 05:37:56 1983" 'zwei/comb.lisp.91'		     # Author: RMS
hcuot "Apr  5 18:02:13 1984" 'zwei/comb.lisp.92'		     # Author: MLY
hcuot "Apr 30 15:47:59 1984" 'zwei/comb.lisp.93'		     # Author: MLY
hcuot "Jun  6 10:28:35 1984" 'zwei/comb.qfasl.93'		     # Author: RMS
hcuot "Feb 16 01:16:24 1984" 'zwei/comc.lisp.196'		     # Author: MLY
hcuot "Jun  5 03:44:12 1984" 'zwei/comc.lisp.199'		     # Author: MLY
hcuot "Jun  6 07:02:27 1984" 'zwei/comc.lisp.200'		     # Author: MLY
hcuot "Jun  9 13:24:12 1984" 'zwei/comc.lisp.201'		     # Author: MLY
hcuot "Jun  6 10:31:54 1984" 'zwei/comc.qfasl.200'		     # Author: RMS
hcuot "Jun 12 19:57:27 1984" 'zwei/comc.qfasl.201'		     # Author: RMS
hcuot "Jan  2 02:23:32 1984" 'zwei/comd.lisp.158'		     # Author: RMS
hcuot "Apr 11 03:27:57 1984" 'zwei/comd.lisp.159'		     # Author: MLY
hcuot "May 26 00:21:29 1984" 'zwei/comd.lisp.160'		     # Author: MLY
hcuot "Jun  6 06:58:32 1984" 'zwei/comd.lisp.161'		     # Author: MLY
hcuot "Jun  6 10:34:36 1984" 'zwei/comd.qfasl.161'		     # Author: RMS
hcuot "Dec 13 05:42:14 1983" 'zwei/come.lisp.132'		     # Author: RMS
hcuot "Apr  5 17:57:40 1984" 'zwei/come.lisp.133'		     # Author: MLY
hcuot "Jun  6 10:38:00 1984" 'zwei/come.qfasl.133'		     # Author: RMS
hcuot "Mar 24 17:00:22 1984" 'zwei/comf.lisp.95'		     # Author: RMS
hcuot "May 16 11:06:54 1984" 'zwei/comf.lisp.96'		     # Author: RMS
hcuot "Jun  6 10:40:15 1984" 'zwei/comf.qfasl.96'		     # Author: RMS
hcuot "Jul 10 09:07:15 1983" 'zwei/comg.lisp.37'		     # Author: RMS
hcuot "Mar 13 07:14:44 1984" 'zwei/comg.lisp.38'		     # Author: RMS
hcuot "Jun  5 03:52:51 1984" 'zwei/comg.lisp.39'		     # Author: MLY
hcuot "Jun  6 10:43:09 1984" 'zwei/comg.qfasl.39'		     # Author: RMS
hcuot "Mar 31 15:25:45 1984" 'zwei/comh.lisp.7'			     # Author: MLY
hcuot "Apr 16 21:22:48 1984" 'zwei/comh.lisp.8'			     # Author: MLY
hcuot "Jun  3 21:52:14 1984" 'zwei/comh.lisp.9'			     # Author: MLY
hcuot "Jun  6 06:26:27 1984" 'zwei/comh.lisp.10'		     # Author: MLY
hcuot "Jun  6 10:45:05 1984" 'zwei/comh.qfasl.10'		     # Author: RMS
hcuot "Mar 31 13:33:54 1984" 'zwei/coms.lisp.81'		     # Author: MLY
hcuot "Apr 22 04:59:03 1984" 'zwei/coms.lisp.82'		     # Author: RMS
hcuot "May 16 11:06:10 1984" 'zwei/coms.lisp.83'		     # Author: RMS
hcuot "Jun 24 17:05:08 1984" 'zwei/coms.lisp.84'		     # Author: MLY
hcuot "Jun  6 10:46:26 1984" 'zwei/coms.qfasl.83'		     # Author: RMS
hcuot "Dec 27 08:12:23 1983" 'zwei/comtab.lisp.301'		     # Author: RMS
hcuot "Mar 31 15:25:07 1984" 'zwei/comtab.lisp.307'		     # Author: MLY
hcuot "May 26 00:40:29 1984" 'zwei/comtab.lisp.308'		     # Author: MLY
hcuot "Jun  5 06:32:26 1984" 'zwei/comtab.lisp.309'		     # Author: MLY
hcuot "Jun  6 07:23:51 1984" 'zwei/comtab.lisp.310'		     # Author: MLY
hcuot "Jun 21 02:53:00 1984" 'zwei/comtab.lisp.311'		     # Author: MLY
hcuot "Jun 21 09:42:03 1984" 'zwei/comtab.lisp.312'		     # Author: RG
hcuot "Jun 24 17:04:41 1984" 'zwei/comtab.lisp.313'		     # Author: MLY
hcuot "Jun  6 09:51:01 1984" 'zwei/comtab.qfasl.310'		     # Author: RMS
hcuot "Jun 21 09:32:51 1984" 'zwei/comtab.qfasl.311'		     # Author: MLY
hcuot "Jun 21 10:05:23 1984" 'zwei/comtab.qfasl.312'		     # Author: MLY
hcuot "Jan 19 21:30:52 1984" 'zwei/defs.lisp.144'		     # Author: MLY
hcuot "Mar 18 04:51:40 1984" 'zwei/defs.lisp.145'		     # Author: MLY
hcuot "Jun  6 07:54:47 1984" 'zwei/defs.lisp.146'		     # Author: MLY
hcuot "Jun  9 13:23:57 1984" 'zwei/defs.lisp.147'		     # Author: MLY
hcuot "Jun 12 17:45:35 1984" 'zwei/defs.lisp.148'		     # Author: RMS
hcuot "Jun 12 17:47:16 1984" 'zwei/defs.lisp.149'		     # Author: RMS
hcuot "Jun 12 17:53:09 1984" 'zwei/defs.lisp.150'		     # Author: RMS
hcuot "Jun 21 02:53:12 1984" 'zwei/defs.lisp.151'		     # Author: MLY
hcuot "Jun  6 09:21:38 1984" 'zwei/defs.qfasl.146'		     # Author: RMS
hcuot "Jun 12 17:53:40 1984" 'zwei/defs.qfasl.150'		     # Author: RMS
hcuot "Jun 21 09:11:01 1984" 'zwei/defs.qfasl.151'		     # Author: MLY
hcuot "Feb  9 19:45:22 1984" 'zwei/dired.lisp.299'		     # Author: RMS
hcuot "Apr 30 15:48:15 1984" 'zwei/dired.lisp.300'		     # Author: MLY
hcuot "May 28 11:58:08 1984" 'zwei/dired.lisp.301'		     # Author: MLY
hcuot "Jun  6 06:26:45 1984" 'zwei/dired.lisp.302'		     # Author: MLY
hcuot "Jun  6 10:49:26 1984" 'zwei/dired.qfasl.302'		     # Author: RMS
hcuot "Dec 27 08:59:04 1983" 'zwei/displa.lisp.149'		     # Author: RMS
hcuot "Feb  7 13:56:37 1984" 'zwei/displa.lisp.151'		     # Author: MLY
hcuot "Jun  5 06:32:38 1984" 'zwei/displa.lisp.152'		     # Author: MLY
hcuot "Jun  9 13:24:03 1984" 'zwei/displa.lisp.153'		     # Author: MLY
hcuot "Jun  6 09:54:42 1984" 'zwei/displa.qfasl.152'		     # Author: RMS
hcuot "Jun 12 19:50:49 1984" 'zwei/displa.qfasl.153'		     # Author: RMS
hcuot "Apr  8 07:11:15 1983" 'zwei/doc.lisp.67'			     # Author: RMS.G.DEVON
hcuot "Feb 19 15:26:09 1984" 'zwei/doc.lisp.72'			     # Author: MLY
hcuot "Mar 13 07:14:21 1984" 'zwei/doc.lisp.73'			     # Author: RMS
hcuot "Jun 21 02:53:18 1984" 'zwei/doc.lisp.74'			     # Author: MLY
hcuot "Jun  6 10:57:16 1984" 'zwei/doc.qfasl.73'		     # Author: RMS
hcuot "Jun 21 09:36:39 1984" 'zwei/doc.qfasl.74'		     # Author: MLY
hcuot "Jun 29 11:10:53 1982" 'zwei/emacs.comdif.1'		     # Author: RMS
hcuot "Aug 10 10:28:26 1983" 'zwei/fasupd.lisp.29'		     # Author: RMS
hcuot "Apr  7 16:04:21 1984" 'zwei/fasupd.lisp.30'		     # Author: MLY
hcuot "Apr  7 16:05:03 1984" 'zwei/fasupd.lisp.31'		     # Author: MLY
hcuot "Jun  6 10:59:09 1984" 'zwei/fasupd.qfasl.31'		     # Author: RMS
hcuot "Jan  4 00:13:46 1984" 'zwei/files.lisp.192'		     # Author: RMS
hcuot "Mar 31 14:05:40 1984" 'zwei/files.lisp.193'		     # Author: MLY
hcuot "Jun  9 13:24:21 1984" 'zwei/files.lisp.194'		     # Author: MLY
hcuot "Jun  6 10:59:51 1984" 'zwei/files.qfasl.193'		     # Author: RMS
hcuot "Jun 12 20:01:07 1984" 'zwei/files.qfasl.194'		     # Author: RMS
hcuot "Feb  2 10:15:30 1984" 'zwei/font.lisp.86'		     # Author: RMS
hcuot "May  9 11:50:27 1984" 'zwei/font.lisp.87'		     # Author: MLY
hcuot "May 22 00:58:47 1984" 'zwei/font.lisp.88'		     # Author: MLY
hcuot "Jun  6 10:18:08 1984" 'zwei/font.qfasl.88'		     # Author: RMS
hcuot "Dec 31 01:48:22 1983" 'zwei/for.lisp.60'			     # Author: RMS
hcuot "Jun  6 09:59:00 1984" 'zwei/for.qfasl.60'		     # Author: RMS
hcuot "Jan  4 00:50:12 1984" 'zwei/history.lisp.15'		     # Author: RMS
hcuot "Jun  6 10:16:42 1984" 'zwei/history.qfasl.15'		     # Author: RMS
hcuot "Dec 22 10:01:04 1983" 'zwei/host.lisp.20'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 11:04:19 1984" 'zwei/host.qfasl.20'		     # Author: RMS
hcuot "Oct 25 23:06:09 1983" 'zwei/indent.lisp.103'		     # Author: RMS
hcuot "Jun  8 20:12:23 1984" 'zwei/indent.lisp.104'		     # Author: MLY
hcuot "Jun  6 10:01:41 1984" 'zwei/indent.qfasl.103'		     # Author: RMS
hcuot "Jun 12 19:55:04 1984" 'zwei/indent.qfasl.104'		     # Author: RMS
hcuot "Jan 16 21:21:27 1984" 'zwei/info.zwei.1'			     # Author: RMS
hcuot "Jul 20 11:20:46 1983" 'zwei/insert.lisp.32'		     # Author: RMS
hcuot "Apr  7 16:06:12 1984" 'zwei/insert.lisp.33'		     # Author: MLY
hcuot "Jun  6 10:04:08 1984" 'zwei/insert.qfasl.33'		     # Author: RMS
hcuot "Dec 13 05:41:54 1983" 'zwei/ispell.lisp.39'		     # Author: RMS
hcuot "May 16 11:06:43 1984" 'zwei/ispell.lisp.40'		     # Author: RMS
hcuot "Jun  6 11:05:37 1984" 'zwei/ispell.qfasl.40'		     # Author: RMS
hcuot "Dec 28 21:06:08 1983" 'zwei/kbdmac.lisp.46'		     # Author: DANIEL.G.MLY
hcuot "Jun  6 07:17:18 1984" 'zwei/kbdmac.lisp.47'		     # Author: MLY
hcuot "Jun  6 10:20:12 1984" 'zwei/kbdmac.qfasl.47'		     # Author: RMS
hcuot "Dec 24 08:43:28 1983" 'zwei/lparse.lisp.31'		     # Author: RMS
hcuot "Jun  6 11:06:47 1984" 'zwei/lparse.qfasl.31'		     # Author: RMS
hcuot "Mar 15 09:24:11 1984" 'zwei/macros.lisp.137'		     # Author: TIM
hcuot "May 16 22:12:00 1984" 'zwei/macros.lisp.139'		     # Author: MLY
hcuot "Jun 18 17:18:16 1984" 'zwei/macros.lisp.141'		     # Author: MLY
hcuot "Jun  6 09:25:29 1984" 'zwei/macros.qfasl.140'		     # Author: RMS
hcuot "Jun 21 09:15:13 1984" 'zwei/macros.qfasl.141'		     # Author: MLY
hcuot "Jan 29 23:12:53 1984" 'zwei/meth.lisp.45'		     # Author: MLY
hcuot "Mar 18 09:39:19 1984" 'zwei/meth.lisp.47'		     # Author: MLY
hcuot "Jun  6 07:17:09 1984" 'zwei/meth.lisp.48'		     # Author: MLY
hcuot "Jun  6 10:06:00 1984" 'zwei/meth.qfasl.48'		     # Author: RMS
hcuot "Jan  2 02:23:39 1984" 'zwei/modes.lisp.127'		     # Author: RMS
hcuot "May 25 20:16:25 1984" 'zwei/modes.lisp.130'		     # Author: MLY

# tid/5237

hcuot "Jun  5 21:11:43 1984" 'patch/system-98-59.lisp.3'
hcuot "Jun  5 21:11:53 1984" 'patch/system-98-59.qfasl.3'
hcuot "Dec 13 22:06:04 1983" 'patch/system-98-6.lisp.17'
hcuot "Jun 13 11:08:07 1984" 'patch/system-98-60.lisp.5'
hcuot "Jun 13 11:08:13 1984" 'patch/system-98-60.qfasl.5'
hcuot "Jun  9 10:10:30 1984" 'patch/system-98-61.lisp.2'
hcuot "Jun  9 10:10:39 1984" 'patch/system-98-61.qfasl.2'
hcuot "Jun 17 10:05:51 1984" 'patch/system-98-62.lisp.12'
hcuot "Jun 20 00:27:54 1984" 'patch/system-98-62.qfasl.12'
hcuot "Jul  2 21:43:53 1984" 'patch/system-98-63.lisp.18'
hcuot "Jul  2 21:44:01 1984" 'patch/system-98-63.qfasl.18'
hcuot "Jun 15 10:19:50 1984" 'patch/system-98-64.lisp.1'
hcuot "Jun 29 09:07:30 1984" 'patch/system-98-64.qfasl.1'
hcuot "Jul  2 21:55:49 1984" 'patch/system-98-65.lisp.10'
hcuot "Jul  2 21:56:02 1984" 'patch/system-98-65.qfasl.10'
hcuot "Jul  9 21:08:09 1984" 'patch/system-98-66.lisp.9'
hcuot "Jul  9 21:08:19 1984" 'patch/system-98-66.qfasl.9'
hcuot "Jun 29 09:22:38 1984" 'patch/system-98-67.lisp.1'
hcuot "Jun 29 09:22:46 1984" 'patch/system-98-67.qfasl.1'
hcuot "Jul 18 18:57:10 1984" 'patch/system-98-68.lisp.5'
hcuot "Jul 18 18:57:21 1984" 'patch/system-98-68.qfasl.5'
hcuot "Aug 14 20:39:14 1984" 'patch/system-98-69.lisp.1'
hcuot "Aug 14 20:39:24 1984" 'patch/system-98-69.qfasl.1'
hcuot "Dec 16 03:40:10 1983" 'patch/system-98-7.lisp.7'
hcuot "Aug 29 15:25:48 1984" 'patch/system-98-70.lisp.1'
hcuot "Aug 29 15:25:55 1984" 'patch/system-98-70.qfasl.1'
hcuot "Oct 13 01:02:56 1984" 'patch/system-98-71.lisp.3'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-71.qfasl.3'
hcuot "Oct 14 21:03:15 1984" 'patch/system-98-72.lisp.2'
hcuot "Oct 14 21:03:36 1984" 'patch/system-98-72.qfasl.2'
hcuot "Oct 11 08:17:25 1984" 'patch/system-98-73.lisp.2'
hcuot "Oct 11 14:50:01 1984" 'patch/system-98-73.qfasl.2'
hcuot "Oct 13 01:03:51 1984" 'patch/system-98-74.lisp.1'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-74.qfasl.1'
hcuot "Oct 14 06:56:53 1984" 'patch/system-98-75.lisp.1'
hcuot "Oct 14 06:57:21 1984" 'patch/system-98-75.qfasl.1'
hcuot "Oct 14 20:58:47 1984" 'patch/system-98-76.lisp.1'
hcuot "Oct 14 20:59:14 1984" 'patch/system-98-76.qfasl.1'
hcuot "Oct 20 18:08:35 1984" 'patch/system-98-77.lisp.1'
hcuot "Nov 12 13:24:48 1984" 'patch/system-98-77.qfasl.1'
hcuot "Nov 12 12:40:36 1984" 'patch/system-98-78.lisp.3'
hcuot "Nov 12 12:47:04 1984" 'patch/system-98-78.lisp.4'
hcuot "Nov 12 12:57:12 1984" 'patch/system-98-78.lisp.5'
hcuot "Nov 12 12:40:43 1984" 'patch/system-98-78.qfasl.3'
hcuot "Nov 12 12:47:46 1984" 'patch/system-98-78.qfasl.4'
hcuot "Nov 12 12:57:43 1984" 'patch/system-98-78.qfasl.5'
hcuot "Dec 18 01:24:01 1983" 'patch/system-98-8.lisp.12'
hcuot "Dec 22 17:18:46 1983" 'patch/system-98-9.lisp.9'
hcuot "Oct 14 15:17:53 1984" 'patch/system-98-9.qfasl.9'
hcuot "Nov  9 02:11:46 1984" 'patch/system-99.patch-directory.46'
hcuot "Nov  9 21:14:01 1984" 'patch/system-99.patch-directory.47'
hcuot "Nov 11 02:51:51 1984" 'patch/system-99.patch-directory.48'
hcuot "Sep 12 17:29:05 1984" 'patch/system-99-1.lisp.2'
hcuot "Sep 12 18:29:34 1984" 'patch/system-99-1.lisp.3'
hcuot "Sep 12 18:30:01 1984" 'patch/system-99-1.qfasl.3'
hcuot "Nov  9 09:09:48 1984" 'patch/system-99-10.lisp.29'
hcuot "Nov  9 20:03:39 1984" 'patch/system-99-10.lisp.30'
hcuot "Nov  9 21:17:06 1984" 'patch/system-99-10.lisp.31'
hcuot "Nov  9 21:32:38 1984" 'patch/system-99-10.qfasl.31'
hcuot "Nov  9 09:07:57 1984" 'patch/system-99-11.lisp.7'
hcuot "Nov  9 10:24:37 1984" 'patch/system-99-11.lisp.8'
hcuot "Nov 12 13:08:14 1984" 'patch/system-99-11.lisp.9'
hcuot "Nov 12 13:39:41 1984" 'patch/system-99-11.qfasl.9'
hcuot "Nov  9 21:16:27 1984" 'patch/system-99-12.lisp.12'
hcuot "Nov 10 08:15:45 1984" 'patch/system-99-12.lisp.13'
hcuot "Nov 10 10:58:28 1984" 'patch/system-99-12.lisp.14'
hcuot "Nov  9 21:51:54 1984" 'patch/system-99-12.qfasl.12'
hcuot "Sep 12 17:50:00 1984" 'patch/system-99-2.lisp.1'
hcuot "Sep 12 18:28:00 1984" 'patch/system-99-2.lisp.2'
hcuot "Sep 12 18:28:11 1984" 'patch/system-99-2.qfasl.2'
hcuot "Sep 13 22:51:26 1984" 'patch/system-99-3.lisp.4'
hcuot "Sep 15 02:40:56 1984" 'patch/system-99-3.lisp.5'
hcuot "Sep 15 02:41:07 1984" 'patch/system-99-3.qfasl.5'
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.6'
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.7'
hcuot "Sep 26 15:23:50 1984" 'patch/system-99-4.qfasl.6'
hcuot "Sep 26 16:02:44 1984" 'patch/system-99-5.lisp.9'
hcuot "Sep 26 19:13:08 1984" 'patch/system-99-5.lisp.10'
hcuot "Sep 26 19:13:19 1984" 'patch/system-99-5.qfasl.10'
hcuot "Sep 23 10:12:25 1984" 'patch/system-99-6.lisp.1'
hcuot "Sep 26 23:57:10 1984" 'patch/system-99-6.lisp.2'
hcuot "Sep 29 13:04:03 1984" 'patch/system-99-6.lisp.3'
hcuot "Sep 29 13:04:07 1984" 'patch/system-99-6.qfasl.3'
hcuot "Oct 16 07:25:45 1984" 'patch/system-99-7.lisp.10'
hcuot "Oct 16 07:26:00 1984" 'patch/system-99-7.qfasl.10'
hcuot "Oct 16 13:48:46 1984" 'patch/system-99-8.lisp.9'
hcuot "Oct 16 13:49:03 1984" 'patch/system-99-8.qfasl.9'
hcuot "Oct 23 23:36:34 1984" 'patch/system-99-9.lisp.16'
hcuot "Oct 23 23:37:01 1984" 'patch/system-99-9.qfasl.16'
hcuot "Sep  9 23:45:44 1984" 'patch/zmail.patch-directory.2'
hcuot "Sep  9 23:46:32 1984" 'patch/zmail.patch-directory.3'
hcuot "Oct 14 10:11:40 1984" 'patch/zmail-54.patch-directory.4'
hcuot "Oct 14 10:12:47 1984" 'patch/zmail-54.patch-directory.5'
hcuot "Oct 14 10:52:38 1984" 'patch/zmail-54.patch-directory.6'
hcuot "Sep 26 07:22:03 1984" 'patch/zmail-54-1.lisp.2'
hcuot "Sep 26 07:22:09 1984" 'patch/zmail-54-1.qfasl.2'
hcuot "Oct 14 10:12:26 1984" 'patch/zmail-54-2.lisp.1'
hcuot "Oct 14 10:12:30 1984" 'patch/zmail-54-2.qfasl.1'
hcuot "Nov 13 04:40:20 1984" 'site/-read-.-me-.1'
hcuot "Jun  7 21:41:36 1983" 'site/amord.system.1'
hcuot "Sep 12 16:43:47 1984" 'site/arlo.system.3'
hcuot "Sep 12 17:12:06 1984" 'site/arlo.system.4'
hcuot "Oct  6 23:10:29 1984" 'site/arlo.system.5'
hcuot "Mar  1 20:46:00 1984" 'site/daedalus.system.12'
hcuot "Jun 15 08:20:59 1984" 'site/extra.hosts.3'
hcuot "Aug  8 13:33:52 1984" 'site/extra.hosts.4'
hcuot "Nov 12 12:34:36 1984" 'site/hsttbl.lisp.117'
hcuot "Nov 12 12:38:58 1984" 'site/hsttbl.lisp.118'
hcuot "Nov 12 13:01:13 1984" 'site/hsttbl.lisp.119'
hcuot "Nov 11 03:31:19 1984" 'site/hsttbl.qfasl.115'
hcuot "Nov 12 12:22:51 1984" 'site/hsttbl.qfasl.116'
hcuot "Nov 12 13:10:15 1984" 'site/hsttbl.qfasl.119'
hcuot "Jun 27 10:42:03 1984" 'site/its-sys-pathname-translations.lisp.1'
hcuot "Sep 12 19:47:08 1984" 'site/lmlocs.lisp.150'
hcuot "Sep 20 21:51:48 1984" 'site/lmlocs.lisp.151'
hcuot "Oct 14 07:34:49 1984" 'site/lmlocs.lisp.152'
hcuot "Oct 18 19:48:34 1984" 'site/lmlocs.lisp.153'
hcuot "Oct 28 00:18:07 1984" 'site/lmlocs.lisp.154'
hcuot "Nov 10 04:27:59 1984" 'site/lmlocs.qfasl.154'
hcuot "May 29 10:51:31 1984" 'site/mit-specific.system.2'
hcuot "May 29 11:16:43 1984" 'site/mitspecific.lisp.2'
hcuot "Sep 10 20:21:25 1984" 'site/mitspecific.qfasl.2'
hcuot "Sep 15 05:24:00 1984" 'site/netu.lisp.7'
hcuot "Nov 11 04:29:37 1984" 'site/netu.qfasl.7'
hcuot "Apr 19 19:23:11 1983" 'site/nrll.sysdef.1'
hcuot "Apr 20 04:02:11 1983" 'site/nrll.system.2'
hcuot "Apr 20 05:01:35 1983" 'site/orll.system.1'
hcuot "Nov 17 07:35:43 1983" 'site/patch.directory.15'
hcuot "Sep 10 20:21:40 1984" 'site/patch.directory.16'
hcuot "Sep 15 03:02:37 1984" 'site/patch-22.directory.12'
hcuot "Sep 15 05:23:40 1984" 'site/patch-22.directory.13'
hcuot "May 29 11:08:21 1984" 'site/patch-22-1.lisp.3'
hcuot "May 29 11:08:25 1984" 'site/patch-22-1.qfasl.3'
hcuot "Jun 27 10:01:14 1984" 'site/patch-22-2.lisp.2'
hcuot "Jun 27 10:01:17 1984" 'site/patch-22-2.qfasl.2'
hcuot "Jul 17 20:15:42 1984" 'site/patch-22-3.lisp.1'
hcuot "Jul 17 20:15:54 1984" 'site/patch-22-3.qfasl.1'
hcuot "Jul 30 20:42:21 1984" 'site/patch-22-4.lisp.1'
hcuot "Jul 30 20:42:24 1984" 'site/patch-22-4.qfasl.1'
hcuot "Sep 15 05:23:24 1984" 'site/patch-22-5.lisp.1'
hcuot "Sep 15 05:28:44 1984" 'site/patch-22-5.lisp.2'
hcuot "Sep 15 05:23:28 1984" 'site/patch-22-5.qfasl.1'
hcuot "Sep 15 05:28:48 1984" 'site/patch-22-5.qfasl.2'
hcuot "Sep 10 20:21:42 1984" 'site/patch-23.directory.1'
hcuot "Aug  2 04:22:36 1983" 'site/rll.system.4'
hcuot "Nov 10 23:04:43 1984" 'site/site.lisp.116'
hcuot "Nov 11 03:08:09 1984" 'site/site.lisp.117'
hcuot "Nov 12 13:17:33 1984" 'site/site.lisp.118'
hcuot "Nov 12 13:17:36 1984" 'site/site.qfasl.118'
hcuot "Mar 10 01:45:08 1983" 'site/spice-plot.system.1'
hcuot "Aug  3 02:24:19 1984" 'site/sys.translations.3'
hcuot "Aug 26 09:26:22 1984" 'site/sys.translations.4'
hcuot "Nov 13 04:36:47 1984" 'site/sys.translations.6'
hcuot "Apr 11 05:40:19 1984" 'site/yaps.system.4'
hcuot "Jun 29 08:32:33 1982" 'sys/-read-.-this-.1'
hcuot "Nov 12 09:18:17 1983" 'sys/cadrlp.lisp.147'
hcuot "Feb  5 06:14:08 1984" 'sys/cadrlp.lisp.148'
hcuot "Sep  9 03:39:01 1984" 'sys/cadrlp.lisp.149'
hcuot "Sep  9 03:45:18 1984" 'sys/cadrlp.qfasl.149'
hcuot "Jun 29 08:33:09 1982" 'sys/cadsym.lisp.24'
hcuot "Apr  7 15:44:05 1984" 'sys/cadsym.lisp.25'
hcuot "Jul 31 18:51:23 1982" 'sys/cdmp.lisp.47'
hcuot "Jun 16 21:47:26 1984" 'sys/cdmp.lisp.52'
hcuot "Sep  8 23:46:19 1984" 'sys/cdmp.qfasl.52'
hcuot "Aug  1 02:43:23 1984" 'sys/clpack.lisp.142'
hcuot "Sep 10 06:15:02 1984" 'sys/clpack.lisp.151'
hcuot "Sep 12 17:49:30 1984" 'sys/clpack.lisp.152'
hcuot "Sep 10 07:09:12 1984" 'sys/clpack.qfasl.151'
hcuot "Aug 15 18:29:02 1983" 'sys/compat.lisp.30'
hcuot "Apr  7 15:45:18 1984" 'sys/compat.lisp.32'
hcuot "Dec 17 03:13:31 1983" 'sys/eval.lisp.23'
hcuot "Jul 30 18:15:20 1984" 'sys/eval.lisp.69'
hcuot "Sep  9 04:13:21 1984" 'sys/eval.lisp.78'
hcuot "Oct 17 13:03:39 1984" 'sys/eval.lisp.83'
hcuot "Oct 29 04:42:55 1984" 'sys/eval.lisp.84'
hcuot "Nov  2 09:41:24 1984" 'sys/eval.lisp.85'
hcuot "Nov  5 10:14:56 1984" 'sys/eval.lisp.86'
hcuot "Sep  9 05:39:13 1984" 'sys/eval.qfasl.78'
hcuot "Jul 12 04:46:05 1984" 'sys/genric.lisp.24'
hcuot "Aug 29 05:16:10 1984" 'sys/genric.lisp.27'
hcuot "Oct 14 06:44:02 1984" 'sys/genric.lisp.28'
hcuot "Aug 29 07:59:43 1984" 'sys/genric.qfasl.27'
hcuot "Oct 27 23:53:09 1983" 'sys/ltop.lisp.436'
hcuot "Jun 29 12:33:08 1984" 'sys/ltop.lisp.487'
hcuot "Oct  6 12:01:14 1984" 'sys/ltop.lisp.495'
hcuot "Oct  8 20:58:59 1984" 'sys/ltop.lisp.496'
hcuot "Sep 11 07:20:26 1984" 'sys/ltop.qfasl.494'
hcuot "Oct 24 03:41:44 1982" 'sys/ma.lisp.305'
hcuot "Aug  1 22:39:34 1984" 'sys/ma.qfasl.305'
hcuot "Jun 29 08:36:30 1982" 'sys/madefs.lisp.7'
hcuot "Jul 30 03:18:23 1984" 'sys/madefs.qfasl.7'
hcuot "Oct 13 00:33:14 1983" 'sys/maopt.lisp.4'
hcuot "Aug  1 22:44:12 1984" 'sys/maopt.qfasl.4'
hcuot "Oct 25 04:08:13 1983" 'sys/mc.lisp.353'
hcuot "Nov 16 09:21:33 1983" 'sys/mc.lisp.354'
hcuot "Aug  1 22:45:44 1984" 'sys/mc.qfasl.354'
hcuot "Jan  4 00:08:41 1983" 'sys/mlap.lisp.51'
hcuot "Aug  1 22:48:15 1984" 'sys/mlap.qfasl.51'
hcuot "Jun 25 23:40:50 1983" 'sys/pack4.lisp.286'
hcuot "Dec 16 20:32:14 1983" 'sys/qcdefs.lisp.128'
hcuot "Sep  8 23:30:46 1984" 'sys/qcdefs.lisp.149'
hcuot "Oct 17 13:21:56 1984" 'sys/qcdefs.lisp.152'
hcuot "Sep  9 20:00:57 1984" 'sys/qcdefs.qfasl.149'
hcuot "Oct 19 19:57:43 1983" 'sys/qcfasd.lisp.229'
hcuot "Sep 10 23:05:15 1984" 'sys/qcfasd.lisp.248'
hcuot "Sep 10 23:05:43 1984" 'sys/qcfasd.qfasl.248'
hcuot "Mar 28 03:58:18 1984" 'sys/qcfile.lisp.307'
hcuot "Aug 15 03:18:22 1984" 'sys/qcfile.lisp.321'
hcuot "Oct 29 04:42:33 1984" 'sys/qcfile.lisp.323'
hcuot "Sep  7 02:12:10 1984" 'sys/qcfile.qfasl.322'
hcuot "Jun 13 07:50:37 1984" 'sys/qclap.lisp.236'
hcuot "Sep  8 23:30:16 1984" 'sys/qclap.lisp.244'
hcuot "Sep  9 20:16:55 1984" 'sys/qclap.qfasl.244'
hcuot "Apr 26 07:24:10 1984" 'sys/qcluke.lisp.23'
hcuot "Aug 30 13:51:41 1984" 'sys/qcluke.lisp.26'
hcuot "Aug 30 20:38:31 1984" 'sys/qcluke.qfasl.26'
hcuot "Nov 29 23:18:23 1983" 'sys/qcopt.lisp.95'
hcuot "Aug  2 05:15:22 1984" 'sys/qcopt.lisp.126'
hcuot "Oct 24 07:49:36 1984" 'sys/qcopt.lisp.136'
hcuot "Nov  6 13:41:16 1984" 'sys/qcopt.lisp.137'
hcuot "Sep  9 20:13:24 1984" 'sys/qcopt.qfasl.133'
hcuot "Jul  5 03:23:04 1984" 'sys/qcp1.lisp.547'
hcuot "Aug 28 06:30:17 1984" 'sys/qcp1.lisp.550'
hcuot "Sep 22 09:44:11 1984" 'sys/qcp1.lisp.563'
hcuot "Oct 28 21:42:02 1984" 'sys/qcp1.lisp.569'
hcuot "Nov  2 09:40:55 1984" 'sys/qcp1.lisp.572'
hcuot "Sep  9 20:03:10 1984" 'sys/qcp1.qfasl.562'
hcuot "Jun 17 14:30:49 1984" 'sys/qcp2.lisp.246'
hcuot "Aug 28 06:18:42 1984" 'sys/qcp2.lisp.252'
hcuot "Oct 28 21:41:46 1984" 'sys/qcp2.lisp.261'
hcuot "Sep  9 20:09:07 1984" 'sys/qcp2.qfasl.259'
hcuot "Aug 17 17:51:51 1983" 'sys/qcpeep.lisp.31'
hcuot "Feb 17 10:46:04 1984" 'sys/qcpeep.lisp.34'
hcuot "Aug  3 03:31:47 1984" 'sys/qcpeep.lisp.36'
hcuot "Aug  3 03:31:56 1984" 'sys/qcpeep.qfasl.36'
hcuot "Jan 11 21:27:40 1984" 'sys/qev.lisp.289'
hcuot "Aug 19 20:43:32 1983" 'sys/qfasl.lisp.432'
hcuot "Jun 26 20:57:53 1984" 'sys/qfasl.lisp.459'
hcuot "Aug 15 03:17:24 1984" 'sys/qfasl.lisp.461'
hcuot "Oct 29 04:42:43 1984" 'sys/qfasl.lisp.462'
hcuot "Aug 15 05:35:35 1984" 'sys/qfasl.qfasl.461'
hcuot "Jun 12 07:51:12 1984" 'sys/qfctns.lisp.753'
hcuot "Aug 31 17:55:02 1984" 'sys/qfctns.lisp.769'
hcuot "Oct 17 12:24:56 1984" 'sys/qfctns.lisp.770'
hcuot "Nov  5 10:12:13 1984" 'sys/qfctns.lisp.773'
hcuot "Aug 31 17:59:24 1984" 'sys/qfctns.qfasl.769'
hcuot "Nov 28 17:40:06 1983" 'sys/qmisc.lisp.590'
hcuot "Jun 24 17:57:39 1984" 'sys/qmisc.lisp.642'
hcuot "Sep 26 12:38:57 1984" 'sys/qmisc.lisp.655'
hcuot "Oct 29 04:38:09 1984" 'sys/qmisc.lisp.658'
hcuot "Aug 31 22:20:48 1984" 'sys/qmisc.qfasl.652'
hcuot "Jun 21 18:36:00 1983" 'sys/qnew.lisp.19'
hcuot "Apr  3 15:55:26 1984" 'sys/qnew.lisp.20'
hcuot "Aug 15 05:54:18 1984" 'sys/qnew.qfasl.20'
hcuot "Jun 17 06:31:08 1984" 'sys/qrand.lisp.395'
hcuot "Sep  4 23:43:16 1984" 'sys/qrand.lisp.408'
hcuot "Oct 30 14:45:31 1984" 'sys/qrand.lisp.410'
hcuot "Sep  4 23:45:01 1984" 'sys/qrand.qfasl.408'
hcuot "Oct 26 06:10:35 1983" 'sys/qwmcr.lisp.20'
hcuot "Sep  8 23:46:37 1984" 'sys/qwmcr.qfasl.20'
hcuot "Jun 18 08:57:37 1983" 'sys/recom.lisp.33'
hcuot "Aug 27 08:28:30 1982" 'sys/sgfctn.lisp.57'
hcuot "Aug 15 06:13:05 1984" 'sys/sgfctn.qfasl.57'
hcuot "Oct 10 07:20:38 1983" 'sys/sort.lisp.59'
hcuot "Aug 15 06:13:45 1984" 'sys/sort.qfasl.59'
hcuot "Jul 25 11:40:34 1984" 'sys/sysdcl.lisp.177'
hcuot "Sep  9 03:54:55 1984" 'sys/sysdcl.lisp.185'
hcuot "Sep 25 07:05:36 1984" 'sys/sysdcl.lisp.186'
hcuot "Oct 18 19:56:06 1984" 'sys/sysdcl.qfasl.186'
hcuot "Jun 12 08:06:02 1984" 'sys/types.lisp.48'
hcuot "Sep 25 07:28:40 1984" 'sys/types.lisp.70'
hcuot "Sep  9 03:39:45 1984" 'sys/types.qfasl.69'
hcuot "Jun 29 08:49:58 1982" 'sys/ucinit.qfasl.1'
hcuot "May 26 00:21:06 1984" 'sys2/advise.lisp.35'
hcuot "May 29 18:44:47 1984" 'sys2/advise.lisp.37'
hcuot "Aug 15 03:43:06 1984" 'sys2/advise.qfasl.37'
hcuot "Oct  6 17:50:44 1983" 'sys2/analyze.lisp.16'
hcuot "Sep 11 07:03:29 1984" 'sys2/analyze.lisp.17'
hcuot "Sep 26 09:58:58 1984" 'sys2/analyze.lisp.18'
hcuot "Sep 11 07:03:49 1984" 'sys2/analyze.qfasl.17'
hcuot "Aug 22 00:06:13 1983" 'sys2/band.lisp.43'
hcuot "Jul 27 08:09:35 1984" 'sys2/band.lisp.44'
hcuot "Aug 15 03:44:10 1984" 'sys2/band.qfasl.44'
hcuot "Jul 30 03:02:02 1984" 'sys2/character.lisp.16'
hcuot "Sep 25 07:22:45 1984" 'sys2/character.lisp.21'
hcuot "Sep  7 22:04:06 1984" 'sys2/character.qfasl.20'
hcuot "Nov 23 09:48:18 1983" 'sys2/class.lisp.88'
hcuot "Jun 15 05:56:23 1984" 'sys2/class.lisp.99'
hcuot "Sep  4 21:31:45 1984" 'sys2/class.qfasl.99'
hcuot "May 15 15:48:48 1984" 'sys2/clmac.lisp.3'
hcuot "Aug 24 10:12:36 1984" 'sys2/clmac.lisp.4'
hcuot "Aug 29 03:55:00 1984" 'sys2/clmac.qfasl.4'
hcuot "Apr  7 15:50:06 1984" 'sys2/cmany.lisp.46'
hcuot "Apr  7 15:50:53 1984" 'sys2/condit.lisp.2'
hcuot "Feb  8 13:06:51 1984" 'sys2/defmac.lisp.74'
hcuot "May 16 22:43:32 1984" 'sys2/defmac.lisp.75'
hcuot "Aug 29 15:31:35 1984" 'sys2/defmac.lisp.78'
hcuot "Oct 29 04:37:08 1984" 'sys2/defmac.lisp.79'
hcuot "Aug 29 21:55:29 1984" 'sys2/defmac.qfasl.78'
hcuot "Jun 26 20:57:47 1984" 'sys2/defsel.lisp.69'
hcuot "Aug 29 02:45:38 1984" 'sys2/defsel.lisp.70'
hcuot "Aug 29 07:26:03 1984" 'sys2/defsel.qfasl.70'
hcuot "Nov 17 12:07:18 1983" 'sys2/disass.lisp.90'
hcuot "Jun 24 16:25:09 1984" 'sys2/disass.lisp.92'
hcuot "Oct 11 15:06:46 1984" 'sys2/disass.lisp.93'
hcuot "Oct 30 02:18:23 1984" 'sys2/disass.lisp.94'
hcuot "Aug  1 22:38:35 1984" 'sys2/disass.qfasl.92'
hcuot "Mar 22 07:07:02 1984" 'sys2/eh.lisp.320'
hcuot "Jun 17 04:54:46 1984" 'sys2/eh.lisp.331'
hcuot "Sep  8 18:50:38 1984" 'sys2/eh.lisp.336'
hcuot "Sep 12 18:24:24 1984" 'sys2/eh.lisp.337'
hcuot "Nov  9 10:44:53 1984" 'sys2/eh.lisp.338'
hcuot "Sep  9 05:53:06 1984" 'sys2/eh.qfasl.336'
hcuot "May 15 14:03:23 1984" 'sys2/ehc.lisp.223'
hcuot "Jun  6 06:08:49 1984" 'sys2/ehc.lisp.226'
hcuot "Sep  4 01:04:00 1984" 'sys2/ehc.lisp.233'
hcuot "Nov  9 11:09:25 1984" 'sys2/ehc.lisp.234'
hcuot "Sep  7 22:04:40 1984" 'sys2/ehc.qfasl.233'
hcuot "Jun  9 13:25:10 1984" 'sys2/ehf.lisp.207'
hcuot "Sep  5 23:49:56 1984" 'sys2/ehf.lisp.224'
hcuot "Sep 11 21:06:12 1984" 'sys2/ehf.lisp.225'
hcuot "Oct 14 10:01:23 1984" 'sys2/ehf.lisp.226'
hcuot "Nov  9 10:52:43 1984" 'sys2/ehf.lisp.227'
hcuot "Sep 11 21:22:50 1984" 'sys2/ehf.qfasl.225'
hcuot "Nov  9 11:13:07 1984" 'sys2/ehsys.lisp.1'
hcuot "Mar 22 11:38:18 1984" 'sys2/ehw.lisp.108'
hcuot "May 16 12:21:59 1984" 'sys2/ehw.lisp.109'
hcuot "Sep  8 01:04:14 1984" 'sys2/ehw.qfasl.109'
hcuot "Oct  9 08:05:36 1983" 'sys2/encaps.lisp.19'
hcuot "Apr 15 04:06:10 1984" 'sys2/encaps.lisp.23'
hcuot "Jul 30 01:28:00 1984" 'sys2/encaps.lisp.27'
hcuot "Aug 15 04:10:54 1984" 'sys2/encaps.qfasl.27'
hcuot "Jul 30 18:54:40 1984" 'sys2/flavor.lisp.272'
hcuot "Sep 10 23:44:11 1984" 'sys2/flavor.lisp.280'
hcuot "Oct 13 10:45:00 1984" 'sys2/flavor.lisp.282'
hcuot "Sep 11 05:12:37 1984" 'sys2/flavor.qfasl.280'
hcuot "Apr 29 00:39:18 1984" 'sys2/gc.lisp.165'
hcuot "Jul 13 12:08:09 1984" 'sys2/gc.lisp.169'
hcuot "Oct 14 11:12:49 1984" 'sys2/gc.lisp.170'
hcuot "Oct 28 21:17:34 1984" 'sys2/gc.lisp.171'
hcuot "Oct 28 21:20:15 1984" 'sys2/gc.lisp.172'
hcuot "Aug 15 04:34:41 1984" 'sys2/gc.qfasl.169'
hcuot "Jun  5 01:39:39 1984" 'sys2/hash.lisp.83'
hcuot "Jul 31 02:22:01 1984" 'sys2/hash.lisp.87'
hcuot "Oct 13 11:53:10 1984" 'sys2/hash.lisp.88'
hcuot "Aug 15 04:51:05 1984" 'sys2/hash.qfasl.87'
hcuot "Apr 16 21:35:12 1984" 'sys2/hashfl.lisp.24'
hcuot "Jun 14 01:02:46 1984" 'sys2/hashfl.lisp.28'
hcuot "Jul 12 09:33:09 1984" 'sys2/hashfl.lisp.29'
hcuot "Oct 13 11:53:30 1984" 'sys2/hashfl.lisp.30'
hcuot "Oct 24 03:53:53 1984" 'sys2/hashfl.lisp.31'
hcuot "Aug 15 04:52:39 1984" 'sys2/hashfl.qfasl.29'
hcuot "Apr  7 15:51:47 1984" 'sys2/let.lisp.8'
hcuot "Jun  4 08:44:01 1984" 'sys2/lmmac.lisp.356'
hcuot "Aug 31 20:13:38 1984" 'sys2/lmmac.lisp.372'
hcuot "Oct 16 10:19:20 1984" 'sys2/lmmac.lisp.375'
hcuot "Oct 27 00:04:10 1984" 'sys2/lmmac.lisp.377'
hcuot "Oct 30 01:55:36 1984" 'sys2/lmmac.lisp.378'
hcuot "Nov  9 10:49:17 1984" 'sys2/lmmac.lisp.379'
hcuot "Nov  9 21:10:12 1984" 'sys2/lmmac.lisp.380'
hcuot "Aug 31 21:54:02 1984" 'sys2/lmmac.qfasl.372'
hcuot "May  9 12:09:16 1984" 'sys2/login.lisp.83'
hcuot "Aug  2 19:51:26 1984" 'sys2/login.lisp.85'
hcuot "Sep  4 02:04:30 1984" 'sys2/login.lisp.87'
hcuot "Sep  4 03:06:25 1984" 'sys2/login.qfasl.87'
hcuot "Nov 30 01:23:16 1983" 'sys2/loop.lisp.795'
hcuot "Jun 20 18:08:26 1984" 'sys2/loop.lisp.798'
hcuot "Oct 23 23:14:32 1984" 'sys2/loop.lisp.799'
hcuot "Oct 24 08:03:31 1984" 'sys2/loop.qfasl.799'
hcuot "Jan 25 07:41:37 1984" 'sys2/maksys.lisp.174'
hcuot "Jun 16 20:18:39 1984" 'sys2/maksys.lisp.176'
hcuot "Sep 13 22:53:32 1984" 'sys2/maksys.lisp.180'
hcuot "Sep  4 22:16:36 1984" 'sys2/maksys.qfasl.178'
hcuot "Mar 14 19:02:00 1984" 'sys2/matrix.lisp.23'
hcuot "Apr  9 18:08:41 1984" 'sys2/matrix.lisp.26'
hcuot "Aug 30 03:20:25 1984" 'sys2/matrix.qfasl.26'
hcuot "Jun 17 06:43:45 1984" 'sys2/meth.lisp.61'
hcuot "Sep  4 21:35:26 1984" 'sys2/meth.lisp.63'
hcuot "Sep  4 21:35:37 1984" 'sys2/meth.qfasl.63'
hcuot "Jun 26 20:55:29 1984" 'sys2/numdef.lisp.10'
hcuot "Sep  8 00:54:22 1984" 'sys2/numdef.lisp.11'
hcuot "Oct  6 11:43:45 1984" 'sys2/numdef.lisp.12'
hcuot "Sep 10 22:30:29 1984" 'sys2/numdef.qfasl.11'
hcuot "Mar 13 22:55:49 1984" 'sys2/numer.lisp.43'
hcuot "Jul  2 21:42:29 1984" 'sys2/numer.lisp.58'
hcuot "Aug 28 08:11:20 1984" 'sys2/numer.lisp.60'
hcuot "Oct  6 11:43:26 1984" 'sys2/numer.lisp.61'
hcuot "Sep 10 22:32:20 1984" 'sys2/numer.qfasl.60'
hcuot "Mar 22 04:17:42 1984" 'sys2/patch.lisp.150'
hcuot "Jul 26 13:25:55 1984" 'sys2/patch.lisp.158'
hcuot "Oct 20 20:21:29 1984" 'sys2/patch.lisp.165'
hcuot "Aug 15 05:22:21 1984" 'sys2/patch.qfasl.158'
hcuot "May 31 14:08:31 1984" 'sys2/plane.lisp.31'
hcuot "Aug 30 00:33:36 1984" 'sys2/plane.lisp.32'
hcuot "Aug 30 00:45:45 1984" 'sys2/plane.qfasl.32'
hcuot "Feb 17 11:36:34 1984" 'sys2/proces.lisp.153'
hcuot "Jun 14 09:38:01 1984" 'sys2/proces.lisp.157'
hcuot "Nov  9 21:05:07 1984" 'sys2/proces.lisp.158'
hcuot "Aug 15 05:28:32 1984" 'sys2/proces.qfasl.157'
hcuot "May 13 02:30:56 1984" 'sys2/prodef.lisp.40'
hcuot "Aug 31 19:58:32 1984" 'sys2/prodef.lisp.48'
hcuot "Aug 31 19:59:54 1984" 'sys2/prodef.qfasl.48'
hcuot "Apr 22 04:59:26 1984" 'sys2/qtrace.lisp.149'
hcuot "Sep  4 18:52:18 1984" 'sys2/qtrace.lisp.151'
hcuot "Sep  4 21:43:34 1984" 'sys2/qtrace.qfasl.151'
hcuot "Jun  4 03:02:01 1984" 'sys2/rat.lisp.38'
hcuot "Sep  4 22:08:45 1984" 'sys2/rat.lisp.46'
hcuot "Sep 10 22:47:13 1984" 'sys2/rat.qfasl.46'
hcuot "Aug  6 12:26:38 1983" 'sys2/resour.lisp.17'
hcuot "Jun 24 06:09:43 1984" 'sys2/resour.lisp.28'
hcuot "Oct 24 03:51:30 1984" 'sys2/resour.lisp.29'
hcuot "Nov 10 09:26:41 1984" 'sys2/resour.lisp.31'
hcuot "Aug 15 06:08:50 1984" 'sys2/resour.qfasl.28'
hcuot "Feb  8 14:52:48 1984" 'sys2/selev.lisp.21'
hcuot "Aug 27 10:43:29 1984" 'sys2/selev.lisp.23'
hcuot "Aug 29 03:54:21 1984" 'sys2/selev.qfasl.23'
hcuot "May 20 08:10:47 1984" 'sys2/setf.lisp.86'
hcuot "Jun 17 06:31:52 1984" 'sys2/setf.lisp.91'
hcuot "Sep 26 12:38:00 1984" 'sys2/setf.lisp.96'
hcuot "Oct 29 09:56:33 1984" 'sys2/setf.lisp.97'
hcuot "Aug 31 18:48:05 1984" 'sys2/setf.qfasl.95'
hcuot "Jan 12 14:29:47 1984" 'sys2/sgdefs.lisp.54'
hcuot "Aug 15 03:40:43 1984" 'sys2/sgdefs.qfasl.54'
hcuot "Jun 17 06:30:55 1984" 'sys2/step.lisp.67'
hcuot "Aug  8 10:12:17 1984" 'sys2/step.lisp.70'
hcuot "Sep 26 08:01:17 1984" 'sys2/step.lisp.72'
hcuot "Aug 15 06:15:30 1984" 'sys2/step.qfasl.70'
hcuot "Apr  6 10:08:15 1984" 'sys2/string.lisp.130'
hcuot "Jul 31 23:41:47 1984" 'sys2/string.lisp.141'
hcuot "Sep 25 07:20:50 1984" 'sys2/string.lisp.147'
hcuot "Sep 10 07:15:39 1984" 'sys2/string.qfasl.146'
hcuot "Nov 29 05:45:39 1983" 'sys2/struct.lisp.292'
hcuot "May 18 06:09:04 1984" 'sys2/struct.lisp.311'
hcuot "Jul 31 23:42:25 1984" 'sys2/struct.lisp.322'
hcuot "Aug 14 22:20:04 1984" 'sys2/struct.qfasl.322'
hcuot "Aug  3 08:04:28 1983" 'sys2/unfasl.lisp.16'
hcuot "Aug  2 19:52:14 1984" 'sys2/unfasl.lisp.17'
hcuot "Oct  9 13:52:31 1984" 'sys2/unfasl.lisp.19'
hcuot "Sep 11 07:52:22 1984" 'sys2/unfasl.qfasl.18'
hcuot "Oct 22 14:37:42 1983" 'sys2/usymld.lisp.183'
hcuot "Feb 17 16:46:44 1984" 'sys2/usymld.lisp.186'
hcuot "Sep  9 03:39:09 1984" 'sys2/usymld.lisp.187'
hcuot "Sep  9 03:49:15 1984" 'sys2/usymld.qfasl.187'
hcuot "Jan  3 08:50:32 1984" 'tape/copy.lisp.128'
hcuot "Feb 16 13:56:19 1984" 'tape/copy.lisp.133'
hcuot "Jan  3 09:50:47 1984" 'tape/copy.qfasl.128'
hcuot "Jan 19 16:25:49 1984" 'tape/ddoc.text.4'
hcuot "May 12 05:49:07 1984" 'tape/ddoc.text.8'
hcuot "Jan 19 16:25:57 1984" 'tape/fdump.lisp.18'
hcuot "Feb 16 13:56:28 1984" 'tape/fdump.lisp.24'
hcuot "May 12 05:29:43 1984" 'tape/fdump.lisp.27'
hcuot "Jan 10 04:56:50 1984" 'tape/fdump-def.lisp.5'
hcuot "Feb 16 13:56:36 1984" 'tape/fdump-def.lisp.8'
hcuot "May 12 05:52:14 1984" 'tape/fdump-def.lisp.12'
hcuot "Jan  2 09:37:10 1984" 'tape/fdump-def.qfasl.1'
hcuot "Jan 10 03:54:15 1984" 'tape/fdump-file-cdate-i.lisp.1'
hcuot "Jan 10 04:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'
hcuot "Jan 19 16:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'
hcuot "Feb 16 13:56:44 1984" 'tape/fdump-r.lisp.4'
hcuot "May 12 05:29:45 1984" 'tape/fdump-r.lisp.5'
hcuot "Jan  3 10:36:11 1984" 'tape/magtape.directory.11'
hcuot "Oct 26 20:41:54 1983" 'tape/magtape-14.directory.14'
hcuot "Mar  8 06:56:47 1983" 'tape/magtape-14-1.qfasl.1'
hcuot "Apr 25 09:51:48 1983" 'tape/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:34 1983" 'tape/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:16 1983" 'tape/magtape-14-5.qfasl.1'
hcuot "Feb 16 14:24:04 1984" 'tape/magtape-22.directory.13'
hcuot "Jan  7 22:40:45 1984" 'tape/magtape-22-1.lisp.1'
hcuot "Jan  7 22:40:56 1984" 'tape/magtape-22-1.qfasl.1'
hcuot "Jan  7 23:28:27 1984" 'tape/magtape-22-2.lisp.1'
hcuot "Jan  7 23:28:40 1984" 'tape/magtape-22-2.qfasl.1'
hcuot "Jan  8 00:41:18 1984" 'tape/magtape-22-3.lisp.1'
hcuot "Jan  8 00:41:44 1984" 'tape/magtape-22-3.qfasl.1'
hcuot "Jan 13 13:06:26 1984" 'tape/magtape-22-4.lisp.1'
hcuot "Jan 13 13:06:35 1984" 'tape/magtape-22-4.qfasl.1'
hcuot "Jan 19 17:40:22 1984" 'tape/magtape-22-5.lisp.1'
hcuot "Jan 19 17:40:32 1984" 'tape/magtape-22-5.qfasl.1'
hcuot "Feb 16 14:23:22 1984" 'tape/magtape-22-6.lisp.1'
hcuot "Feb 16 14:23:28 1984" 'tape/magtape-22-6.qfasl.1'
hcuot "Jan 13 12:25:26 1984" 'tape/mtaux.lisp.79'
hcuot "Jan 19 17:04:02 1984" 'tape/mtaux.lisp.80'
hcuot "Jan  3 09:52:48 1984" 'tape/mtaux.qfasl.77'
hcuot "Jun 20 06:21:53 1983" 'tape/mtdefs.lisp.28'
hcuot "Dec 16 15:34:10 1983" 'tape/mtdefs.lisp.30'
hcuot "Jan  3 09:46:18 1984" 'tape/mtdefs.qfasl.30'
hcuot "Jan  7 23:43:06 1984" 'tape/mtstr.lisp.86'
hcuot "Jan 11 05:40:52 1984" 'tape/mtstr.lisp.87'
hcuot "Jan  3 09:47:58 1984" 'tape/mtstr.qfasl.85'
hcuot "Jan  3 08:50:55 1984" 'tape/odump.lisp.1'
hcuot "Jan  3 10:33:05 1984" 'tape/odump.qfasl.1'
hcuot "May 12 05:29:46 1984" 'tape/package.lisp.1'
hcuot "Jan  3 07:59:49 1984" 'tape/pdp10.lisp.1'
hcuot "May 12 08:31:18 1984" 'tape/rmunit.lisp.3'
hcuot "May 12 05:29:46 1984" 'tape/system.lisp.3'
hcuot "Jan  3 04:42:55 1984" 'tape/tm.lisp.23'
hcuot "May 12 05:29:47 1984" 'tape/tm.lisp.25'
hcuot "Jan  3 04:43:02 1984" 'tape/tmdefs.lisp.6'
hcuot "May 12 05:29:48 1984" 'tape/tmdefs.lisp.7'
hcuot "May 12 07:27:24 1984" 'tape/unit.lisp.7'
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.1'
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.2'
hcuot "May 12 05:29:49 1984" 'tape/new/mtdefs.lisp.3'
hcuot "May 12 07:28:11 1984" 'tape/new/mtdefs.lisp.4'
hcuot "May 12 07:45:03 1984" 'tape/new/mtdefs.qfasl.4'
hcuot "Jan 11 06:38:45 1984" 'tape/new/mtrqb.lisp.2'
hcuot "May 12 05:29:49 1984" 'tape/new/mtrqb.lisp.3'
hcuot "May 12 05:29:50 1984" 'tape/new/mtstr.lisp.4'
hcuot "May 12 08:31:35 1984" 'tape/new/mtstr.lisp.5'
hcuot "Jan 19 16:27:21 1984" 'tape/new/tmunit.lisp.2'
hcuot "May 12 05:29:50 1984" 'tape/new/tmunit.lisp.5'
hcuot "Jan 11 06:38:10 1984" 'tape/new/weunit.lisp.2'
hcuot "May 12 05:29:51 1984" 'tape/new/weunit.lisp.3'
hcuot "Nov 20 23:29:49 1982" 'ubin/dcfu.uload.4'
hcuot "Aug  4 07:23:05 1982" 'ubin/memd.uload.1'
hcuot "May  7 01:22:05 1983" 'ubin/ucadr.locs.239'
hcuot "Aug 26 14:18:51 1983" 'ubin/ucadr.locs.257'
hcuot "Mar  3 08:22:09 1984" 'ubin/ucadr.locs.309'
hcuot "Jun 17 01:45:52 1984" 'ubin/ucadr.locs.314'
hcuot "Sep 11 21:24:11 1984" 'ubin/ucadr.locs.320'
hcuot "May  7 01:19:58 1983" 'ubin/ucadr.mcr.239'
hcuot "Aug 26 14:16:41 1983" 'ubin/ucadr.mcr.257'
hcuot "Mar  3 08:20:07 1984" 'ubin/ucadr.mcr.309'
hcuot "Jun 17 01:42:17 1984" 'ubin/ucadr.mcr.314'
hcuot "Sep 11 21:21:19 1984" 'ubin/ucadr.mcr.320'
hcuot "May  7 01:20:31 1983" 'ubin/ucadr.sym.239'
hcuot "Aug 26 14:17:12 1983" 'ubin/ucadr.sym.257'
hcuot "Mar  3 08:20:33 1984" 'ubin/ucadr.sym.309'
hcuot "Jun 17 01:42:47 1984" 'ubin/ucadr.sym.314'
hcuot "Sep 11 21:22:04 1984" 'ubin/ucadr.sym.320'
hcuot "May  7 01:22:07 1983" 'ubin/ucadr.tbl.239'
hcuot "Aug 26 14:18:53 1983" 'ubin/ucadr.tbl.257'
hcuot "Mar  3 08:22:10 1984" 'ubin/ucadr.tbl.309'
hcuot "Jun 17 01:45:55 1984" 'ubin/ucadr.tbl.314'
hcuot "Sep 11 21:24:18 1984" 'ubin/ucadr.tbl.320'
hcuot "Apr  9 11:19:01 1983" 'ucadr/cadldb.lisp.20'
hcuot "Jul 26 10:31:51 1983" 'ucadr/cadldb.qfasl.20'
hcuot "Jun 29 10:56:11 1982" 'ucadr/cadtlk.mid.9'
hcuot "Jun 29 10:56:32 1982" 'ucadr/chaos.test.1'
hcuot "Jun 29 10:56:46 1982" 'ucadr/dcfu.text.23'
hcuot "Dec 22 06:46:28 1982" 'ucadr/dcfu.uload.3'
hcuot "Jun 29 10:59:34 1982" 'ucadr/memd.lisp.26'
hcuot "Jun 29 10:59:39 1982" 'ucadr/mmtest.lisp.15'
hcuot "Jun 29 10:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1'
hcuot "Nov 17 06:26:42 1982" 'ucadr/packed.lisp.119'
hcuot "Oct 14 23:41:23 1983" 'ucadr/packed.lisp.124'
hcuot "Jun 29 11:00:13 1982" 'ucadr/praid.lisp.21'
hcuot "Jun 29 11:00:18 1982" 'ucadr/promh.text.9'
hcuot "Nov 18 14:23:35 1983" 'ucadr/uc-arith.lisp.25'
hcuot "Apr 30 23:43:08 1984" 'ucadr/uc-arith.lisp.28'
hcuot "Jun 27 07:40:14 1984" 'ucadr/uc-arith.lisp.33'
hcuot "Oct  6 10:49:21 1984" 'ucadr/uc-arith.lisp.34'
hcuot "Jul 23 10:01:24 1983" 'ucadr/uc-array.lisp.28'
hcuot "Nov 29 21:22:31 1983" 'ucadr/uc-array.lisp.59'
hcuot "Jun 17 01:36:02 1984" 'ucadr/uc-array.lisp.63'
hcuot "Mar 31 23:16:21 1983" 'ucadr/uc-array-cache.lisp.1'
hcuot "Jul 29 11:02:16 1983" 'ucadr/uc-cadr.lisp.7'
hcuot "Jun  2 03:53:36 1984" 'ucadr/uc-cadr.lisp.8'
hcuot "Jan 27 08:37:59 1984" 'ucadr/uc-call-return.lisp.97'
hcuot "Jul 27 04:48:47 1984" 'ucadr/uc-call-return.lisp.103'
hcuot "Sep 11 21:15:57 1984" 'ucadr/uc-call-return.lisp.108'
hcuot "Oct 11 07:19:04 1982" 'ucadr/uc-chaos.lisp.1'
hcuot "Oct 15 01:27:48 1982" 'ucadr/uc-cold-disk.lisp.2'
hcuot "Jun  8 11:15:36 1983" 'ucadr/uc-cold-disk.lisp.9'
hcuot "Nov 14 10:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'
hcuot "Oct 11 07:18:14 1982" 'ucadr/uc-disk.lisp.1'
hcuot "Nov 14 08:21:19 1983" 'ucadr/uc-disk.lisp.2'
hcuot "Sep 13 09:27:33 1983" 'ucadr/uc-fctns.lisp.40'
hcuot "May 11 05:41:36 1984" 'ucadr/uc-fctns.lisp.75'
hcuot "Jun 16 21:24:03 1984" 'ucadr/uc-fctns.lisp.78'
hcuot "Aug  3 10:30:30 1984" 'ucadr/uc-fctns.lisp.84'
hcuot "Apr  3 12:37:59 1983" 'ucadr/uc-hacks.lisp.3'
hcuot "Oct 17 16:11:57 1983" 'ucadr/uc-hacks.lisp.5'
hcuot "Feb  4 09:53:01 1983" 'ucadr/uc-interrupt.lisp.4'
hcuot "Oct 30 00:16:52 1983" 'ucadr/uc-interrupt.lisp.7'
hcuot "Jul 23 11:00:06 1983" 'ucadr/uc-logical.lisp.7'
hcuot "Mar  3 04:56:48 1984" 'ucadr/uc-logical.lisp.8'
hcuot "Apr  4 07:09:21 1983" 'ucadr/uc-macrocode.lisp.9'
hcuot "Nov 14 02:49:50 1983" 'ucadr/uc-macrocode.lisp.28'
hcuot "Jul  2 11:39:55 1984" 'ucadr/uc-macrocode.lisp.29'
hcuot "Oct 11 20:53:14 1982" 'ucadr/uc-mc.lisp.1'
hcuot "Nov 14 02:47:25 1983" 'ucadr/uc-mc.lisp.2'
hcuot "Apr  5 09:49:33 1983" 'ucadr/uc-meter.lisp.3'
hcuot "Jul 23 12:00:11 1983" 'ucadr/uc-meter.lisp.4'
hcuot "Aug  1 09:39:57 1983" 'ucadr/uc-meter.lisp.5'
hcuot "Jun  5 10:47:27 1983" 'ucadr/uc-page-fault.lisp.7'
hcuot "Oct 17 16:11:44 1983" 'ucadr/uc-page-fault.lisp.10'
hcuot "Nov 21 09:24:14 1983" 'ucadr/uc-page-fault.lisp.13'
hcuot "Oct 29 22:36:55 1983" 'ucadr/uc-parameters.lisp.222'
hcuot "Dec 28 07:16:45 1983" 'ucadr/uc-parameters.lisp.228'
hcuot "Jun  2 03:53:42 1984" 'ucadr/uc-parameters.lisp.230'
hcuot "Oct 11 07:18:51 1982" 'ucadr/uc-pup.lisp.1'
hcuot "Nov 16 10:34:29 1983" 'ucadr/uc-stack-closure.lisp.3'
hcuot "Jan 23 03:33:37 1984" 'ucadr/uc-stack-closure.lisp.6'
hcuot "Jul 27 04:49:03 1984" 'ucadr/uc-stack-closure.lisp.10'
hcuot "Aug 31 19:43:35 1984" 'ucadr/uc-stack-closure.lisp.11'
hcuot "Apr  5 09:49:39 1983" 'ucadr/uc-stack-groups.lisp.4'
hcuot "Jul 23 11:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'
hcuot "Jul 29 10:34:22 1983" 'ucadr/uc-storage-allocation.lisp.15'
hcuot "May 19 04:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'
hcuot "Jul 20 10:00:15 1983" 'ucadr/uc-string.lisp.13'
hcuot "Jul 28 20:32:30 1984" 'ucadr/uc-string.lisp.25'
hcuot "Sep  6 20:03:25 1984" 'ucadr/uc-string.lisp.26'
hcuot "Apr  3 12:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'
hcuot "Aug 15 14:07:26 1983" 'ucadr/uc-transporter.lisp.9'
hcuot "Dec 29 05:09:16 1983" 'ucadr/uc-transporter.lisp.22'
hcuot "May  1 09:20:21 1984" 'ucadr/uc-transporter.lisp.23'
hcuot "Apr  4 09:20:18 1983" 'ucadr/uc-tv.lisp.3'
hcuot "Jul 23 12:18:17 1983" 'ucadr/uc-tv.lisp.4'
hcuot "Apr  5 03:29:11 1984" 'ucadr/uc-tv.lisp.5'
hcuot "Jun 29 11:00:58 1982" 'ucadr/ucadlr.text.746'
hcuot "Nov 14 01:30:39 1983" 'ucadr/ucode.lisp.19'
hcuot "Aug 25 05:24:02 1982" 'wind/baswin.text.7'
hcuot "Aug  9 02:24:49 1983" 'wind/blink.text.21'
hcuot "Nov 16 05:33:30 1983" 'wind/choice.text.94'
hcuot "Jan 22 09:23:05 1984" 'wind/choice.text.95'
hcuot "Jul 23 04:52:44 1983" 'wind/edges.text.14'
hcuot "Aug 25 05:25:39 1982" 'wind/emack.fasl.1'
hcuot "Aug 25 05:25:27 1982" 'wind/emack.lisp.36'
hcuot "Apr  7 15:53:58 1984" 'wind/emack.lisp.37'
hcuot "Jul  6 04:46:40 1983" 'wind/fonts.text.17'
hcuot "Aug  9 02:21:25 1983" 'wind/frames.text.14'
hcuot "Aug  8 05:44:03 1983" 'wind/grafix.text.24'
hcuot "Aug  8 07:11:30 1983" 'wind/input.text.24'
hcuot "Sep 30 08:33:21 1983" 'wind/input.text.26'
hcuot "Aug 25 05:26:36 1982" 'wind/lstfla.lisp.5'
hcuot "Apr  7 15:55:43 1984" 'wind/lstfla.lisp.6'
hcuot "Aug  8 06:15:43 1983" 'wind/margin.text.20'
hcuot "Aug 23 15:42:25 1983" 'wind/misc.text.24'
hcuot "Aug  8 11:24:58 1983" 'wind/mouse.text.33'
hcuot "Aug 25 05:27:06 1982" 'wind/operat.bolio.1'
hcuot "Aug 25 05:26:46 1982" 'wind/operat.text.45'
hcuot "Aug 25 05:27:12 1982" 'wind/outlin.text.2'
hcuot "Aug  9 04:51:45 1983" 'wind/output.text.27'
hcuot "Oct 29 02:54:29 1983" 'wind/output.text.28'
hcuot "Aug  8 07:11:36 1983" 'wind/select.text.21'
hcuot "Nov 19 01:30:08 1983" 'wind/select.text.22'
hcuot "Aug  8 10:42:49 1983" 'wind/tscrol.text.37'
hcuot "Jul  6 05:25:11 1983" 'wind/typout.text.17'
hcuot "Aug  9 03:11:35 1983" 'wind/windo1.text.51'
hcuot "Feb  4 11:57:38 1984" 'wind/windo1.text.52'
hcuot "Jul  3 06:44:16 1983" 'wind/windoc.bolio.14'
hcuot "Jun 21 06:34:00 1983" 'wind/windoc.dict.1'
hcuot "Aug  9 03:12:27 1983" 'wind/windoc.log.12'
hcuot "Aug  9 03:10:00 1983" 'wind/windoc.text.15'
hcuot "Aug  9 03:23:20 1983" 'wind/windoc.vars.33'
hcuot "Aug 25 05:30:20 1982" 'wind/window.gloss.1'
hcuot "Aug 25 05:30:32 1982" 'wind/window.manual.1'
hcuot "Aug 25 05:30:42 1982" 'wind/window.methds.1'
hcuot "Aug 25 05:30:47 1982" 'wind/winman.text.1'
hcuot "Apr  7 20:52:51 1984" 'window/basstr.lisp.361'
hcuot "Jul  7 11:23:35 1984" 'window/basstr.lisp.371'
hcuot "Sep  5 18:47:15 1984" 'window/basstr.lisp.372'
hcuot "Sep  7 19:42:22 1984" 'window/basstr.qfasl.372'
hcuot "Apr 15 07:28:51 1984" 'window/baswin.lisp.559'
hcuot "Jun  6 11:46:15 1984" 'window/baswin.lisp.561'
hcuot "Sep  6 21:24:14 1984" 'window/baswin.lisp.562'
hcuot "Sep  7 19:37:06 1984" 'window/baswin.qfasl.562'
hcuot "Mar  3 04:18:07 1984" 'window/choice.lisp.110'
hcuot "Apr 22 11:43:14 1984" 'window/choice.lisp.111'
hcuot "Aug  5 02:39:30 1984" 'window/choice.lisp.116'
hcuot "Sep  7 23:32:54 1984" 'window/choice.qfasl.116'
hcuot "Aug  3 10:36:59 1983" 'window/cold.lisp.105'
hcuot "Nov 10 07:11:35 1983" 'window/cold.lisp.112'
hcuot "Jun 17 04:26:37 1984" 'window/cold.lisp.128'
hcuot "Aug 29 00:50:25 1984" 'window/cold.lisp.129'
hcuot "Aug 29 02:53:22 1984" 'window/cold.qfasl.129'
hcuot "Dec 11 05:41:19 1983" 'window/color.lisp.66'
hcuot "Oct 14 20:56:06 1984" 'window/color.lisp.69'
hcuot "Aug 30 00:54:42 1984" 'window/color.qfasl.67'
hcuot "Jun 29 09:12:56 1982" 'window/cometh.lisp.23'
hcuot "Aug  4 12:29:09 1983" 'window/cometh.lisp.26'
hcuot "Aug  3 22:17:13 1984" 'window/cometh.qfasl.26'
hcuot "Jun 29 09:13:09 1982" 'window/csrpos.lisp.9'
hcuot "Aug  3 22:53:14 1984" 'window/csrpos.qfasl.9'
hcuot "Nov 23 16:24:15 1983" 'window/fed.lisp.194'
hcuot "Feb  6 14:55:35 1984" 'window/fed.lisp.199'
hcuot "Sep  8 00:51:09 1984" 'window/fed.qfasl.199'
hcuot "Jun 16 15:41:36 1983" 'window/frame.lisp.164'
hcuot "Apr 11 03:28:15 1984" 'window/frame.lisp.165'
hcuot "Sep  8 00:59:41 1984" 'window/frame.qfasl.165'
hcuot "Jun  4 03:03:15 1984" 'window/graphics.lisp.1'
hcuot "Aug  3 22:06:31 1984" 'window/graphics.qfasl.1'
hcuot "Jun  6 10:01:37 1984" 'window/inspct.lisp.153'
hcuot "Oct  8 20:57:28 1984" 'window/inspct.lisp.155'
hcuot "Oct 30 02:18:29 1984" 'window/inspct.lisp.157'
hcuot "Nov 10 09:18:59 1984" 'window/inspct.lisp.158'
hcuot "Sep  7 23:26:22 1984" 'window/inspct.qfasl.154'
hcuot "Oct 30 06:32:13 1983" 'window/menu.lisp.98'
hcuot "Feb  5 10:43:00 1984" 'window/menu.lisp.103'
hcuot "Sep  7 18:30:18 1984" 'window/menu.lisp.104'
hcuot "Oct 20 19:40:52 1984" 'window/menu.lisp.105'
hcuot "Sep  7 18:43:14 1984" 'window/menu.qfasl.104'
hcuot "May  1 23:20:08 1984" 'window/mouse.lisp.246'
hcuot "May 16 19:35:36 1984" 'window/mouse.lisp.247'
hcuot "Oct 11 04:37:53 1984" 'window/mouse.lisp.248'
hcuot "Aug  3 12:23:18 1984" 'window/mouse.qfasl.247'
hcuot "Jul 16 15:26:31 1984" 'window/peek.lisp.147'
hcuot "Aug  5 03:49:54 1984" 'window/peek.lisp.149'
hcuot "Sep  7 18:30:52 1984" 'window/peek.lisp.153'
hcuot "Sep  7 18:50:42 1984" 'window/peek.qfasl.153'
hcuot "May 25 02:14:17 1984" 'window/peekch.lisp.27'
hcuot "Jun 29 09:46:44 1982" 'window/peekfs.lisp.9'
hcuot "Sep  7 18:30:42 1984" 'window/peekfs.lisp.10'
hcuot "Sep  7 19:08:30 1984" 'window/peekfs.qfasl.10'
hcuot "Apr  7 15:56:29 1984" 'window/quest.lisp.43'
hcuot "Nov 30 05:28:54 1983" 'window/rh.lisp.146'
hcuot "Apr 18 09:15:27 1984" 'window/rh.lisp.160'
hcuot "Sep 11 21:05:34 1984" 'window/rh.lisp.162'
hcuot "Sep 11 21:34:52 1984" 'window/rh.qfasl.162'
hcuot "Nov 21 08:04:19 1983" 'window/scred.lisp.106'
hcuot "May  3 19:34:28 1984" 'window/scred.lisp.107'
hcuot "Oct  9 11:48:09 1984" 'window/scred.lisp.112'
hcuot "Aug  3 22:27:31 1984" 'window/scred.qfasl.111'
hcuot "Jul 25 10:19:44 1983" 'window/scrman.lisp.165'
hcuot "Aug  3 11:54:11 1984" 'window/scrman.qfasl.165'
hcuot "Oct  6 17:48:59 1983" 'window/scroll.lisp.175'
hcuot "Aug  5 02:39:39 1984" 'window/scroll.lisp.176'
hcuot "Aug  5 02:45:11 1984" 'window/scroll.qfasl.176'
hcuot "Feb  4 00:57:09 1984" 'window/sheet.lisp.554'
hcuot "Jun  6 11:46:04 1984" 'window/sheet.lisp.557'
hcuot "Aug  3 11:56:23 1984" 'window/sheet.qfasl.557'
hcuot "May 20 22:13:35 1984" 'window/shwarm.lisp.321'
hcuot "Jul 24 08:14:29 1984" 'window/shwarm.lisp.324'
hcuot "Sep  7 18:33:12 1984" 'window/shwarm.lisp.328'
hcuot "Oct 31 06:40:50 1984" 'window/shwarm.lisp.331'
hcuot "Nov  1 11:35:45 1984" 'window/shwarm.lisp.332'
hcuot "Sep  7 19:31:11 1984" 'window/shwarm.qfasl.328'
hcuot "Nov 21 10:54:11 1983" 'window/stream.lisp.116'
hcuot "May 27 15:38:37 1984" 'window/stream.lisp.136'
hcuot "Aug  8 15:04:05 1984" 'window/stream.lisp.144'
hcuot "Sep  8 19:06:44 1984" 'window/stream.lisp.145'
hcuot "Sep  9 05:51:21 1984" 'window/stream.qfasl.145'
hcuot "Jun 12 20:14:53 1984" 'window/supdup.lisp.272'
hcuot "Jul  5 03:33:56 1984" 'window/supdup.lisp.276'
hcuot "Aug  3 23:14:36 1984" 'window/supdup.qfasl.276'
hcuot "Nov 29 05:56:40 1983" 'window/sysmen.lisp.173'
hcuot "Mar 18 04:49:43 1984" 'window/sysmen.lisp.176'
hcuot "Oct 11 08:55:04 1984" 'window/sysmen.lisp.178'
hcuot "Aug  3 22:22:16 1984" 'window/sysmen.qfasl.177'
hcuot "Jun 29 09:51:17 1982" 'window/task.list.1'
hcuot "Sep  6 04:26:39 1984" 'window/telnet-code.lisp.5'
hcuot "Sep  6 05:00:41 1984" 'window/telnet-code.lisp.6'
hcuot "Sep  1 06:28:56 1984" 'window/telnet-front-hack.lisp.1'
hcuot "Dec 26 03:16:03 1983" 'window/tscrol.lisp.69'
hcuot "Jun  6 11:47:06 1984" 'window/tscrol.lisp.70'
hcuot "Oct 11 04:37:48 1984" 'window/tscrol.lisp.73'
hcuot "Nov  9 20:17:36 1984" 'window/tscrol.lisp.74'
hcuot "Jul 30 02:54:26 1984" 'window/tscrol.qfasl.72'
hcuot "Apr  7 19:54:59 1984" 'window/tvdefs.lisp.278'
hcuot "Jul 25 03:06:56 1984" 'window/tvdefs.lisp.283'
hcuot "Aug 29 01:21:19 1984" 'window/tvdefs.lisp.284'
hcuot "Aug 29 09:10:22 1984" 'window/tvdefs.qfasl.284'
hcuot "Aug 20 14:24:51 1983" 'window/typwin.lisp.105'
hcuot "May  1 23:22:28 1984" 'window/typwin.lisp.118'
hcuot "Sep  7 23:40:15 1984" 'window/typwin.qfasl.118'
hcuot "Jan 18 20:56:57 1984" 'window/wholin.lisp.85'
hcuot "Jul  7 06:08:43 1984" 'window/wholin.lisp.88'
hcuot "Sep  4 20:08:19 1984" 'window/wholin.lisp.90'
hcuot "Nov  2 03:03:28 1984" 'window/wholin.lisp.91'
hcuot "Sep  4 21:02:13 1984" 'window/wholin.qfasl.90'
hcuot "Dec  9 23:26:17 1983" 'window/winddoc.lisp.2'
hcuot "Nov 12 08:24:14 1984" 'zmail/bug.zmail.1'
hcuot "Apr 11 03:28:22 1984" 'zmail/button.lisp.23'
hcuot "Jul 13 07:17:38 1984" 'zmail/button.lisp.24'
hcuot "Sep  9 20:58:54 1984" 'zmail/button.qfasl.24'
hcuot "Apr  7 15:57:16 1984" 'zmail/cometh.lisp.51'
hcuot "Sep  9 21:11:39 1984" 'zmail/cometh.qfasl.51'
hcuot "Apr 22 00:16:43 1984" 'zmail/comnds.lisp.579'
hcuot "Jul 13 07:22:36 1984" 'zmail/comnds.lisp.580'
hcuot "Sep  9 23:58:08 1984" 'zmail/comnds.lisp.581'
hcuot "Sep 26 12:37:47 1984" 'zmail/comnds.lisp.582'
hcuot "Oct 14 10:23:33 1984" 'zmail/comnds.lisp.583'
hcuot "Sep 10 07:59:56 1984" 'zmail/comnds.qfasl.581'
hcuot "Aug 17 20:36:10 1983" 'zmail/defs.lisp.268'
hcuot "Apr 18 09:42:50 1984" 'zmail/defs.lisp.270'
hcuot "Jul 13 07:20:38 1984" 'zmail/defs.lisp.272'
hcuot "Sep  9 18:35:08 1984" 'zmail/defs.lisp.273'
hcuot "Sep  9 19:19:35 1984" 'zmail/defs.qfasl.273'
hcuot "Feb 23 13:39:32 1984" 'zmail/filter.lisp.350'
hcuot "Apr 11 06:36:04 1984" 'zmail/filter.lisp.352'
hcuot "Jul 13 07:22:09 1984" 'zmail/filter.lisp.353'
hcuot "Sep 10 00:51:46 1984" 'zmail/filter.lisp.355'
hcuot "Sep 25 07:29:51 1984" 'zmail/filter.lisp.356'
hcuot "Sep 10 08:16:57 1984" 'zmail/filter.qfasl.355'
hcuot "Jun 29 11:22:50 1982" 'zmail/info.mail.1'
hcuot "Dec  3 14:55:46 1983" 'zmail/lex733.lisp.13'
hcuot "Apr 30 15:49:02 1984" 'zmail/lex733.lisp.14'
hcuot "Sep 10 05:59:29 1984" 'zmail/lex733.qfasl.1'
hcuot "Apr  7 15:57:50 1984" 'zmail/lm.lisp.4'
hcuot "Apr  7 15:58:15 1984" 'zmail/lmcsrv.lisp.5'
hcuot "Jul 13 07:23:12 1984" 'zmail/lmfile.lisp.5'
hcuot "Sep  9 20:30:09 1984" 'zmail/lmfile.qfasl.5'
hcuot "May 16 11:06:22 1984" 'zmail/mail.lisp.308'
hcuot "Jul 13 07:22:22 1984" 'zmail/mail.lisp.310'
hcuot "Sep  9 23:58:19 1984" 'zmail/mail.lisp.311'
hcuot "Sep 10 08:07:59 1984" 'zmail/mail.qfasl.311'
hcuot "Apr  7 16:02:19 1984" 'zmail/mfhost.lisp.57'
hcuot "Jul 13 07:23:16 1984" 'zmail/mfhost.lisp.58'
hcuot "Sep  9 20:25:31 1984" 'zmail/mfhost.qfasl.58'
hcuot "Apr 11 06:35:11 1984" 'zmail/mfiles.lisp.322'
hcuot "Jul 13 07:22:59 1984" 'zmail/mfiles.lisp.323'
hcuot "Sep  9 23:58:00 1984" 'zmail/mfiles.lisp.324'
hcuot "Sep 10 07:49:43 1984" 'zmail/mfiles.qfasl.324'
hcuot "Apr  7 16:02:56 1984" 'zmail/mult.lisp.24'
hcuot "Jul 13 07:19:28 1984" 'zmail/mult.lisp.25'
hcuot "Sep  9 20:57:38 1984" 'zmail/mult.qfasl.25'
hcuot "Dec 10 23:37:49 1983" 'zmail/parse.lisp.52'
hcuot "Nov 15 11:02:07 1983" 'zmail/patch.directory.13'
hcuot "Aug 23 00:25:50 1983" 'zmail/patch-51-1.lisp.1'
hcuot "Sep  7 21:56:01 1983" 'zmail/patch-51-2.lisp.1'
hcuot "Sep 21 23:30:38 1983" 'zmail/patch-51-3.lisp.6'
hcuot "Sep 21 23:26:54 1983" 'zmail/patch-51-4.lisp.2'
hcuot "Sep 23 08:11:23 1983" 'zmail/patch-51-5.lisp.2'
hcuot "Sep 26 05:52:32 1983" 'zmail/patch-51-6.lisp.1'
hcuot "Oct 14 07:56:33 1983" 'zmail/patch-51-7.lisp.1'
hcuot "Oct 14 07:56:39 1983" 'zmail/patch-51-7.qfasl.1'
hcuot "Oct 22 08:30:39 1983" 'zmail/patch-51-8.lisp.1'
hcuot "Oct 22 08:30:48 1983" 'zmail/patch-51-8.qfasl.1'
hcuot "Oct 28 07:02:36 1983" 'zmail/patch-51-9.lisp.1'
hcuot "Oct 28 07:02:49 1983" 'zmail/patch-51-9.qfasl.1'
hcuot "Jun 29 04:21:37 1984" 'zmail/patch-53.directory.49'
hcuot "Oct 14 10:56:00 1984" 'zmail/patch-53.directory.50'
hcuot "Oct 14 10:59:23 1984" 'zmail/patch-53.directory.51'
hcuot "Dec  7 12:43:52 1983" 'zmail/patch-53-1.qfasl.2'
hcuot "Jan 30 06:21:26 1984" 'zmail/patch-53-10.lisp.1'
hcuot "Jan 30 06:21:32 1984" 'zmail/patch-53-10.qfasl.1'
hcuot "Feb 16 07:57:45 1984" 'zmail/patch-53-11.lisp.2'
hcuot "Feb 16 07:57:48 1984" 'zmail/patch-53-11.qfasl.2'
hcuot "Feb 23 13:40:40 1984" 'zmail/patch-53-12.lisp.2'
hcuot "Feb 23 13:40:45 1984" 'zmail/patch-53-12.qfasl.2'
hcuot "Mar  4 08:41:33 1984" 'zmail/patch-53-13.lisp.1'
hcuot "Mar  4 08:41:37 1984" 'zmail/patch-53-13.qfasl.1'
hcuot "Mar 24 17:24:31 1984" 'zmail/patch-53-14.lisp.2'
hcuot "Mar 24 17:24:35 1984" 'zmail/patch-53-14.qfasl.2'
hcuot "Apr 11 07:05:23 1984" 'zmail/patch-53-15.lisp.3'
hcuot "Apr 11 07:05:32 1984" 'zmail/patch-53-15.qfasl.3'
hcuot "Apr 18 09:41:32 1984" 'zmail/patch-53-16.lisp.1'
hcuot "Apr 18 09:41:38 1984" 'zmail/patch-53-16.qfasl.1'
hcuot "Apr 22 00:46:53 1984" 'zmail/patch-53-17.lisp.2'
hcuot "Apr 22 00:47:01 1984" 'zmail/patch-53-17.qfasl.2'
hcuot "Jun 29 04:21:13 1984" 'zmail/patch-53-18.lisp.1'
hcuot "Jun 29 08:53:32 1984" 'zmail/patch-53-18.qfasl.1'
hcuot "Oct 14 10:57:28 1984" 'zmail/patch-53-19.lisp.1'
hcuot "Oct 14 10:57:55 1984" 'zmail/patch-53-19.qfasl.1'
hcuot "Dec  6 05:18:26 1983" 'zmail/patch-53-2.lisp.1'
hcuot "Dec  6 05:18:36 1983" 'zmail/patch-53-2.qfasl.1'
hcuot "Dec 13 06:15:17 1983" 'zmail/patch-53-3.lisp.2'
hcuot "Dec 13 06:15:23 1983" 'zmail/patch-53-3.qfasl.2'
hcuot "Dec 14 08:54:56 1983" 'zmail/patch-53-5.lisp.1'
hcuot "Dec 14 08:55:02 1983" 'zmail/patch-53-5.qfasl.1'
hcuot "Jan  3 18:55:45 1984" 'zmail/patch-53-6.lisp.2'
hcuot "Jan  3 18:55:54 1984" 'zmail/patch-53-6.qfasl.2'
hcuot "Jan  1 01:08:53 1984" 'zmail/patch-53-7.lisp.3'
hcuot "Jan  1 01:09:00 1984" 'zmail/patch-53-7.qfasl.3'
hcuot "Jan  1 15:59:26 1984" 'zmail/patch-53-8.lisp.3'
hcuot "Jan  1 15:59:30 1984" 'zmail/patch-53-8.qfasl.3'
hcuot "Jan  1 16:00:18 1984" 'zmail/patch-53-9.lisp.2'
hcuot "Jan  1 16:00:22 1984" 'zmail/patch-53-9.qfasl.2'
hcuot "Jun 29 11:28:11 1982" 'zmail/poop.text.35'
hcuot "Apr 16 21:36:09 1984" 'zmail/profil.lisp.119'
hcuot "Jul 13 07:22:00 1984" 'zmail/profil.lisp.121'
hcuot "Sep 11 06:21:26 1984" 'zmail/profil.lisp.124'
hcuot "Sep 11 06:21:59 1984" 'zmail/profil.qfasl.124'
hcuot "Apr  7 16:03:55 1984" 'zmail/refer.lisp.6'
hcuot "Jul 13 07:22:56 1984" 'zmail/refer.lisp.7'
hcuot "Sep  9 20:29:01 1984" 'zmail/refer.qfasl.7'
hcuot "Apr 30 14:38:24 1984" 'zmail/rfc733.lisp.56'
hcuot "Jul 13 07:16:29 1984" 'zmail/rfc733.lisp.57'
hcuot "Sep  9 21:03:17 1984" 'zmail/rfc733.qfasl.57'
hcuot "Dec 31 03:02:48 1983" 'zmail/top.lisp.551'
hcuot "Jul 13 06:57:38 1984" 'zmail/top.lisp.553'
hcuot "Sep  9 23:57:54 1984" 'zmail/top.lisp.554'
hcuot "Sep 26 12:37:36 1984" 'zmail/top.lisp.555'
hcuot "Sep 10 07:45:42 1984" 'zmail/top.qfasl.554'
hcuot "Apr 11 06:35:53 1984" 'zmail/window.lisp.340'
hcuot "Jul 13 07:22:49 1984" 'zmail/window.lisp.342'
hcuot "Sep  9 23:58:28 1984" 'zmail/window.lisp.343'
hcuot "Sep 10 08:13:55 1984" 'zmail/window.qfasl.343'
hcuot "Jun 29 11:04:18 1982" 'zwei/.comnd.text.1'
hcuot "Jun 29 11:04:27 1982" 'zwei/atsign.xfile.1'
hcuot "Jun  9 13:24:16 1984" 'zwei/bdired.lisp.41'
hcuot "Aug  5 04:08:03 1984" 'zwei/bdired.qfasl.41'
hcuot "Jan 27 19:35:00 1983" 'zwei/bug.bugs7.1'
hcuot "Nov 10 01:41:13 1984" 'zwei/bug.zwei.1'
hcuot "Oct  8 10:11:11 1983" 'zwei/bug-zwei.text.1'
hcuot "Jun 29 11:04:29 1982" 'zwei/bugs.bugs.1'
hcuot "Jun 29 11:05:20 1982" 'zwei/bugs.bugs6.1'
hcuot "Jun 29 11:05:52 1982" 'zwei/bugs.status.1'
hcuot "Mar 15 09:23:53 1984" 'zwei/coma.lisp.101'
hcuot "Jul 28 20:07:07 1984" 'zwei/coma.lisp.102'
hcuot "Sep 26 12:38:20 1984" 'zwei/coma.lisp.103'
hcuot "Aug  4 00:18:12 1984" 'zwei/coma.qfasl.102'
hcuot "Apr  5 18:02:13 1984" 'zwei/comb.lisp.92'
hcuot "Oct 11 08:58:19 1984" 'zwei/comb.lisp.95'
hcuot "Aug  4 00:20:56 1984" 'zwei/comb.qfasl.94'
hcuot "Jun  9 13:24:12 1984" 'zwei/comc.lisp.201'
hcuot "Sep  9 04:13:16 1984" 'zwei/comc.lisp.204'
hcuot "Sep  9 05:47:20 1984" 'zwei/comc.qfasl.204'
hcuot "Jan  2 02:23:32 1984" 'zwei/comd.lisp.158'
hcuot "Aug  5 01:13:21 1984" 'zwei/comd.lisp.165'
hcuot "Oct 30 12:53:47 1984" 'zwei/comd.lisp.169'
hcuot "Sep  7 22:43:25 1984" 'zwei/comd.qfasl.167'
hcuot "Dec 13 05:42:14 1983" 'zwei/come.lisp.132'
hcuot "Apr  5 17:57:40 1984" 'zwei/come.lisp.133'
hcuot "Aug  4 00:28:48 1984" 'zwei/come.qfasl.133'
hcuot "Mar 24 17:00:22 1984" 'zwei/comf.lisp.95'
hcuot "Aug  5 00:20:06 1984" 'zwei/comf.lisp.98'
hcuot "Oct 13 11:52:22 1984" 'zwei/comf.lisp.100'
hcuot "Sep  9 05:48:56 1984" 'zwei/comf.qfasl.99'
hcuot "Jun  5 03:52:51 1984" 'zwei/comg.lisp.39'
hcuot "Aug 15 03:17:48 1984" 'zwei/comg.lisp.40'
hcuot "Aug 29 09:32:06 1984" 'zwei/comg.qfasl.40'
hcuot "Apr 16 21:22:48 1984" 'zwei/comh.lisp.8'
hcuot "Aug  5 00:04:40 1984" 'zwei/comh.lisp.13'
hcuot "Aug  5 00:04:47 1984" 'zwei/comh.qfasl.13'
hcuot "Mar 31 13:33:54 1984" 'zwei/coms.lisp.81'
hcuot "Apr 22 04:59:03 1984" 'zwei/coms.lisp.82'
hcuot "May 16 11:06:10 1984" 'zwei/coms.lisp.83'
hcuot "Jul  8 18:15:48 1984" 'zwei/coms.lisp.85'
hcuot "Aug  5 03:58:57 1984" 'zwei/coms.qfasl.85'
hcuot "Mar 31 15:25:07 1984" 'zwei/comtab.lisp.307'
hcuot "Jun  6 07:23:51 1984" 'zwei/comtab.lisp.310'
hcuot "Sep  5 23:14:03 1984" 'zwei/comtab.lisp.317'
hcuot "Nov 10 10:57:26 1984" 'zwei/comtab.lisp.320'
hcuot "Sep  7 22:39:57 1984" 'zwei/comtab.qfasl.317'
hcuot "Jan 19 21:30:52 1984" 'zwei/defs.lisp.144'
hcuot "Jun 12 17:53:09 1984" 'zwei/defs.lisp.150'
hcuot "Sep 11 20:44:53 1984" 'zwei/defs.lisp.155'
hcuot "Sep 11 21:19:07 1984" 'zwei/defs.qfasl.155'
hcuot "Feb  9 19:45:22 1984" 'zwei/dired.lisp.299'
hcuot "Jul 26 08:30:44 1984" 'zwei/dired.lisp.303'
hcuot "Nov  9 11:13:58 1984" 'zwei/dired.lisp.306'
hcuot "Aug 29 09:33:34 1984" 'zwei/dired.qfasl.304'
hcuot "Dec 27 08:59:04 1983" 'zwei/displa.lisp.149'
hcuot "Feb  7 13:56:37 1984" 'zwei/displa.lisp.151'
hcuot "Aug  5 02:39:21 1984" 'zwei/displa.lisp.155'
hcuot "Sep  6 18:54:03 1984" 'zwei/displa.lisp.157'
hcuot "Sep  7 22:46:25 1984" 'zwei/displa.qfasl.157'
hcuot "Feb 19 15:26:09 1984" 'zwei/doc.lisp.72'
hcuot "Jun 21 02:53:18 1984" 'zwei/doc.lisp.74'
hcuot "Aug  5 04:09:10 1984" 'zwei/doc.qfasl.74'
hcuot "Jun 29 11:10:53 1982" 'zwei/emacs.comdif.1'
hcuot "Aug 10 10:28:26 1983" 'zwei/fasupd.lisp.29'
hcuot "Apr  7 16:05:03 1984" 'zwei/fasupd.lisp.31'
hcuot "Aug  5 04:10:49 1984" 'zwei/fasupd.qfasl.31'
hcuot "Jan  4 00:13:46 1984" 'zwei/files.lisp.192'
hcuot "Jul  2 11:40:05 1984" 'zwei/files.lisp.195'
hcuot "Oct 14 09:27:47 1984" 'zwei/files.lisp.196'
hcuot "Aug  5 04:11:25 1984" 'zwei/files.qfasl.195'
hcuot "Feb  2 10:15:30 1984" 'zwei/font.lisp.86'
hcuot "May 22 00:58:47 1984" 'zwei/font.lisp.88'
hcuot "Aug  4 00:11:45 1984" 'zwei/font.qfasl.88'
hcuot "Jul 28 21:24:34 1984" 'zwei/for.lisp.61'
hcuot "Aug  5 02:39:16 1984" 'zwei/for.lisp.62'
hcuot "Aug  5 03:53:34 1984" 'zwei/for.qfasl.62'
hcuot "Jan  4 00:50:12 1984" 'zwei/history.lisp.15'
hcuot "Sep 11 21:05:42 1984" 'zwei/history.lisp.16'
hcuot "Sep 11 21:33:45 1984" 'zwei/history.qfasl.16'
hcuot "Dec 22 10:01:04 1983" 'zwei/host.lisp.20'
hcuot "Aug  5 04:15:39 1984" 'zwei/host.qfasl.20'
hcuot "Oct 25 23:06:09 1983" 'zwei/indent.lisp.103'
hcuot "Oct 11 08:56:06 1984" 'zwei/indent.lisp.105'
hcuot "Aug  3 23:57:17 1984" 'zwei/indent.qfasl.104'
hcuot "Jan 16 21:21:27 1984" 'zwei/info.zwei.1'
hcuot "Jul 20 11:20:46 1983" 'zwei/insert.lisp.32'
hcuot "Apr  7 16:06:12 1984" 'zwei/insert.lisp.33'
hcuot "Nov  5 05:31:43 1984" 'zwei/insert.lisp.35'
hcuot "Aug  3 23:59:24 1984" 'zwei/insert.qfasl.33'
hcuot "Jul  8 18:10:53 1984" 'zwei/ispell.lisp.41'
hcuot "Aug  5 04:16:46 1984" 'zwei/ispell.qfasl.41'
hcuot "Dec 28 21:06:08 1983" 'zwei/kbdmac.lisp.46'
hcuot "Jun  6 07:17:18 1984" 'zwei/kbdmac.lisp.47'
hcuot "Sep  6 00:19:24 1984" 'zwei/kbdmac.lisp.48'
hcuot "Sep  7 23:06:51 1984" 'zwei/kbdmac.qfasl.48'

# tid/606

hcuot "Mar 22 06:37:20 1983" '-read-.-this-.2'
hcuot "Sep  3 22:58:12 1984" 'cc/cadld.lisp.8'
hcuot "Sep  9 05:13:51 1984" 'cc/cadld.qfasl.8'
hcuot "Sep 12 06:20:06 1984" 'cc/cadreg.lisp.4'
hcuot "Jul 24 04:17:33 1983" 'cc/cc.help.4'
hcuot "Sep 12 00:22:33 1984" 'cc/cc.lisp.50'
hcuot "Sep 12 06:20:40 1984" 'cc/cc.qfasl.50'
hcuot "Oct  9 08:57:54 1983" 'cc/ccdisk.lisp.106'
hcuot "Sep  9 05:19:18 1984" 'cc/ccdisk.qfasl.106'
hcuot "Dec 27 06:00:38 1983" 'cc/ccgsyl.lisp.6'
hcuot "Sep  9 04:58:38 1984" 'cc/ccgsyl.qfasl.6'
hcuot "Aug 17 03:47:14 1983" 'cc/ccwhy.lisp.12'
hcuot "Sep  9 05:17:29 1984" 'cc/ccwhy.qfasl.12'
hcuot "Apr  7 15:02:25 1984" 'cc/chploc.lisp.5'
hcuot "Sep  9 05:23:30 1984" 'cc/chploc.qfasl.5'
hcuot "Mar  9 20:11:02 1984" 'cc/dcfu.uload.2'
hcuot "Sep  9 05:28:27 1984" 'cc/dcheck.lisp.7'
hcuot "Jul  2 21:56:55 1982" 'cc/dcheck.loop.1'
hcuot "Sep  9 05:36:25 1984" 'cc/dcheck.qfasl.7'
hcuot "Sep  9 05:03:26 1984" 'cc/diags.lisp.159'
hcuot "Sep  9 05:04:08 1984" 'cc/diags.qfasl.159'
hcuot "Jan  3 20:00:50 1985" 'cc/dmon.lisp.57'
hcuot "Sep  9 05:10:55 1984" 'cc/dmon.qfasl.56'
hcuot "Dec  7 22:33:10 1984" 'cc/junk..1'
hcuot "Sep  9 03:31:10 1984" 'cc/lcadmc.lisp.31'
hcuot "Sep  9 04:49:35 1984" 'cc/lcadmc.qfasl.31'
hcuot "Apr  7 15:04:15 1984" 'cc/lcadrd.lisp.95'
hcuot "Sep  9 04:58:54 1984" 'cc/lcadrd.qfasl.95'
hcuot "Jun 20 01:44:48 1983" 'cc/ldbg.lisp.45'
hcuot "Sep  9 05:12:40 1984" 'cc/ldbg.qfasl.45'
hcuot "Nov 12 08:26:07 1983" 'cc/lqfmac.lisp.17'
hcuot "Sep  9 04:48:57 1984" 'cc/lqfmac.qfasl.17'
hcuot "Oct 26 04:06:09 1983" 'cc/patch.directory.3'
hcuot "Sep  8 02:21:16 1984" 'cc/patch-3.directory.23'
hcuot "Dec  1 14:12:53 1983" 'cc/patch-3-1.qfasl.1'
hcuot "Sep  8 02:20:42 1984" 'cc/patch-3-10.lisp.1'
hcuot "Sep  8 02:20:45 1984" 'cc/patch-3-10.qfasl.1'
hcuot "Dec 18 00:50:34 1983" 'cc/patch-3-2.qfasl.2'
hcuot "Dec 27 05:59:31 1983" 'cc/patch-3-3.qfasl.1'
hcuot "Dec 27 19:59:00 1983" 'cc/patch-3-4.qfasl.1'
hcuot "Jan 23 07:16:31 1984" 'cc/patch-3-5.qfasl.2'
hcuot "Jan 27 09:11:57 1984" 'cc/patch-3-6.qfasl.1'
hcuot "Jun 11 23:06:48 1984" 'cc/patch-3-7.qfasl.1'
hcuot "Jul  7 01:29:34 1984" 'cc/patch-3-8.qfasl.1'
hcuot "Sep  6 20:28:13 1984" 'cc/patch-3-9.lisp.1'
hcuot "Sep  6 20:28:20 1984" 'cc/patch-3-9.qfasl.1'
hcuot "Sep  9 05:29:20 1984" 'cc/qf.lisp.126'
hcuot "Sep  9 05:33:23 1984" 'cc/qf.qfasl.126'
hcuot "Jul  7 01:39:01 1984" 'cc/salvag.lisp.38'
hcuot "Sep  9 05:23:45 1984" 'cc/salvag.qfasl.38'
hcuot "Apr  7 15:05:00 1984" 'cc/zero.lisp.15'
hcuot "Sep  9 05:13:29 1984" 'cc/zero.qfasl.15'
hcuot "May 27 23:12:18 1986" 'chaos/hosts.text.392'
hcuot "Oct  9 13:51:43 1984" 'cold/coldld.lisp.84'
hcuot "Sep 11 07:47:22 1984" 'cold/coldld.qfasl.83'
hcuot "Sep 11 07:43:22 1984" 'cold/coldpk.lisp.25'
hcuot "Sep 11 07:43:37 1984" 'cold/coldpk.qfasl.25'
hcuot "Aug 30 06:45:32 1984" 'cold/coldut.lisp.100'
hcuot "Aug 30 09:00:56 1984" 'cold/coldut.qfasl.100'
hcuot "Sep  5 20:29:29 1984" 'cold/defmic.lisp.200'
hcuot "Feb 13 01:46:43 1985" 'cold/docmic.lisp.41'
hcuot "Mar 12 00:16:37 1985" 'cold/export.lisp.31'
hcuot "Feb 28 16:10:43 1985" 'cold/global.lisp.644'
hcuot "Aug 15 04:46:01 1984" 'cold/global.qfasl.634'
hcuot "Feb 28 15:51:49 1985" 'cold/lisp.lisp.2'
hcuot "Feb 15 05:33:28 1985" 'cold/mini.lisp.90'
hcuot "Aug 15 05:19:46 1984" 'cold/mini.qfasl.88'
hcuot "Nov 11 03:18:52 1984" 'cold/minisr.exe.1'
hcuot "Nov 11 03:18:01 1984" 'cold/minisr.mid.44'
hcuot "Feb 12 09:35:01 1985" 'cold/qcom.lisp.583'
hcuot "Dec  6 09:29:11 1984" 'cold/qdefs.lisp.388'
hcuot "Feb 17 09:27:51 1985" 'cold/system.lisp.106'
hcuot "Aug 15 06:23:06 1984" 'cold/system.qfasl.102'
hcuot "Apr  7 15:06:03 1984" 'demo/abacus.lisp.20'
hcuot "Sep  8 01:13:00 1984" 'demo/abacus.qfasl.20'
hcuot "Dec 13 05:41:22 1983" 'demo/alarm.lisp.50'
hcuot "Jul  4 21:19:15 1985" 'demo/alarm.qfasl.50'
hcuot "Aug 16 10:56:31 1983" 'demo/beeps.lisp.8'
hcuot "Oct 26 23:25:37 1983" 'demo/beeps.qfasl.8'
hcuot "Apr  7 15:06:23 1984" 'demo/cafe.lisp.8'
hcuot "Jun  6 13:00:32 1984" 'demo/cafe.qfasl.8'
hcuot "Nov 13 00:18:36 1983" 'demo/colorhack.lisp.7'
hcuot "Nov 13 07:19:50 1983" 'demo/colorhack.qfasl.7'
hcuot "Jun 20 02:52:15 1983" 'demo/colxor.lisp.52'
hcuot "Oct 26 23:29:52 1983" 'demo/colxor.qfasl.52'
hcuot "Jul 21 17:00:24 1982" 'demo/craze.lisp.2'
hcuot "Aug 14 10:34:04 1983" 'demo/craze.qfasl.2'
hcuot "Dec  9 04:29:51 1983" 'demo/crock.lisp.6'
hcuot "Jul  6 09:06:15 1985" 'demo/crock.qfasl.6'
hcuot "Apr  7 16:38:29 1984" 'demo/ctest.lisp.1'
hcuot "Mar 31 18:14:03 1984" 'demo/dc.lisp.4'
hcuot "Sep  8 01:10:53 1984" 'demo/dc.qfasl.4'
hcuot "Apr  7 15:07:09 1984" 'demo/deutsc.lisp.34'
hcuot "Jun  6 13:04:44 1984" 'demo/deutsc.qfasl.34'
hcuot "Apr  7 15:07:35 1984" 'demo/dlwhak.lisp.37'
hcuot "Jun  6 13:05:34 1984" 'demo/dlwhak.qfasl.37'
hcuot "Apr  7 15:08:37 1984" 'demo/docscr.lisp.6'
hcuot "Jun  6 13:07:54 1984" 'demo/docscr.qfasl.6'
hcuot "Sep  5 19:58:44 1984" 'demo/doctor.lisp.10'
hcuot "Sep  7 23:25:44 1984" 'demo/doctor.qfasl.10'
hcuot "Jun 20 02:52:55 1983" 'demo/geb.lisp.27'
hcuot "Oct 26 23:38:48 1983" 'demo/geb.qfasl.27'
hcuot "Jun 20 02:53:09 1983" 'demo/hakdef.lisp.14'
hcuot "Sep  8 01:14:00 1984" 'demo/hakdef.qfasl.14'
hcuot "Apr  7 15:09:41 1984" 'demo/hcedit.lisp.28'
hcuot "Jun  6 13:08:50 1984" 'demo/hcedit.qfasl.28'
hcuot "Apr  7 15:11:41 1984" 'demo/liss.lisp.5'
hcuot "Nov 10 12:46:06 1983" 'demo/munch.lisp.14'
hcuot "Sep  8 01:09:22 1984" 'demo/munch.qfasl.14'
hcuot "Apr  7 15:24:04 1984" 'demo/npaint.lisp.1'
hcuot "Aug 16 10:56:24 1983" 'demo/ohacks.lisp.35'
hcuot "Oct 26 23:44:57 1983" 'demo/ohacks.qfasl.35'
hcuot "Apr  7 15:12:09 1984" 'demo/organ.lisp.18'
hcuot "Jun  6 13:09:56 1984" 'demo/organ.qfasl.18'
hcuot "Jun 20 02:52:00 1983" 'demo/pfom.lisp.31'
hcuot "Aug 14 10:44:10 1983" 'demo/pfom.qfasl.31'
hcuot "Oct 24 20:42:24 1983" 'demo/qix.lisp.3'
hcuot "Oct 26 23:47:45 1983" 'demo/qix.qfasl.3'
hcuot "Jul 26 10:08:11 1983" 'demo/rotate.lisp.5'
hcuot "Oct 26 23:48:20 1983" 'demo/rotate.qfasl.5'
hcuot "Aug 20 21:03:08 1983" 'demo/rotcir.lisp.5'
hcuot "Oct 26 23:49:12 1983" 'demo/rotcir.qfasl.5'
hcuot "Dec 27 14:24:52 1983" 'demo/treedv.lisp.4'
hcuot "Dec 27 14:25:03 1983" 'demo/treedv.qfasl.4'
hcuot "Jul 20 14:04:52 1982" 'demo/tvbgar.qfasl.1'
hcuot "Apr  7 15:39:50 1984" 'demo/versat.lisp.1'
hcuot "Apr  7 15:41:20 1984" 'demo/votrax.lisp.1'
hcuot "Oct 22 02:49:11 1983" 'demo/what.lisp.19'
hcuot "Oct 26 23:54:43 1983" 'demo/what.qfasl.19'
hcuot "Apr  7 15:42:46 1984" 'demo/words.lisp.1'
hcuot "Sep  6 23:42:51 1984" 'demo/worm.lisp.9'
hcuot "Sep 10 07:44:09 1984" 'demo/worm.qfasl.9'
hcuot "Dec 13 05:41:19 1983" 'demo/worm-trails.lisp.15'
hcuot "Sep  8 01:08:09 1984" 'demo/worm-trails.qfasl.15'
hcuot "Jul 20 14:05:17 1982" 'demo/wormch.ast.1'
hcuot "Jul 20 14:05:20 1982" 'demo/wormch.qfasl.1'
hcuot "Jun 15 10:28:10 1984" 'distribution/dist.lisp.8'
hcuot "Feb 21 00:09:00 1984" 'distribution/dist.qfasl.7'
hcuot "Feb 16 13:57:28 1984" 'distribution/lmi-filter.lisp.2'
hcuot "Aug  1 21:21:44 1982" 'doc/array.intent.1'
hcuot "Aug  1 21:21:50 1982" 'doc/bmcode.text.4'
hcuot "Jun 17 04:33:51 1986" 'doc/bug.lispm.1'
hcuot "Aug  1 21:24:26 1982" 'doc/bug.lispm10.1'
hcuot "Aug  1 21:25:10 1982" 'doc/bug.lispm11.1'
hcuot "Aug  1 21:26:02 1982" 'doc/bug.lispm12.1'
hcuot "Aug  1 21:22:04 1982" 'doc/bug.lispm13.1'
hcuot "Nov 30 00:47:02 1982" 'doc/bug.lispm14.1'
hcuot "Jan 11 22:41:27 1983" 'doc/bug.lispm15.1'
hcuot "Feb 24 02:11:04 1983" 'doc/bug.lispm16.1'
hcuot "Apr 18 07:53:19 1983" 'doc/bug.lispm17.1'
hcuot "Jun  1 20:37:41 1983" 'doc/bug.lispm18.1'
hcuot "May 23 13:26:53 1984" 'doc/bug.lispm18.2'
hcuot "Aug  4 04:36:47 1983" 'doc/bug.lispm19.1'
hcuot "Sep 23 01:01:25 1984" 'doc/bug.lispm19.2'
hcuot "Oct  5 19:38:55 1983" 'doc/bug.lispm20.1'
hcuot "Nov 12 23:29:45 1983" 'doc/bug.lispm21.1'
hcuot "Jan  7 20:27:51 1984" 'doc/bug.lispm22.1'
hcuot "May 16 00:33:17 1984" 'doc/bug.lispm23.1'
hcuot "Nov 16 10:43:03 1984" 'doc/bug.lispm24.1'
hcuot "Nov 16 11:05:09 1984" 'doc/bug.lispm25.1'
hcuot "Nov 16 11:20:32 1984" 'doc/bug.lispm26.1'
hcuot "Jan 30 17:49:13 1985" 'doc/bug.lispm27.2'
hcuot "Jan 30 17:52:12 1985" 'doc/bug.lispm28.1'
hcuot "Jan 30 17:46:04 1985" 'doc/bug.lispm29.1'
hcuot "Jan 30 17:46:24 1985" 'doc/bug.lispm30.1'
hcuot "Apr  4 01:29:30 1985" 'doc/bug.lispm31.1'
hcuot "Apr 20 02:31:42 1985" 'doc/bug.lispm32.1'
hcuot "May 16 18:40:47 1985" 'doc/bug.lispm33.1'
hcuot "Jul  4 03:00:11 1985" 'doc/bug.lispm34.1'
hcuot "Oct 23 04:48:16 1985" 'doc/bug.lispm35.1'
hcuot "Jan 27 10:51:04 1986" 'doc/bug.lispm36.1'
hcuot "Aug  1 21:23:42 1982" 'doc/bug.lispm9.1'
hcuot "Aug  1 21:26:50 1982" 'doc/bug.not-info-lispm!.1'
hcuot "Aug  1 21:26:56 1982" 'doc/cadr.text.164'
hcuot "Jun 11 20:23:31 1986" 'doc/cadr-mail.txt.1'
hcuot "Aug  1 21:27:24 1982" 'doc/cells.text.4'
hcuot "Aug  1 21:27:33 1982" 'doc/char.text.18'
hcuot "Aug  1 21:27:39 1982" 'doc/chead.text.4'
hcuot "Nov  1 18:57:53 1982" 'doc/chfile.text.3'
hcuot "Aug  1 21:27:57 1982" 'doc/chod1.drw.1'
hcuot "Aug  1 21:28:07 1982" 'doc/chodam.drw.1'
hcuot "Aug  1 21:28:16 1982" 'doc/chodi.drw.1'
hcuot "Aug  1 21:28:26 1982" 'doc/chodtm.drw.1'
hcuot "Jun 16 01:54:24 1984" 'doc/clisp-mail.txt.1'
hcuot "Aug  1 21:28:32 1982" 'doc/closur.text.12'
hcuot "Aug  1 21:28:39 1982" 'doc/cold.tags.1'
hcuot "Jan  2 07:51:49 1984" 'doc/common.lisp.9'
hcuot "Aug  1 21:28:51 1982" 'doc/cons.text.93'
hcuot "Dec 22 10:50:05 1985" 'doc/converse.bugs.1'
hcuot "Aug  1 21:29:11 1982" 'doc/csoft.text.20'
hcuot "Aug  1 21:29:32 1982" 'doc/cursor.answer.1'
hcuot "Aug  1 21:29:39 1982" 'doc/dfs.text.12'
hcuot "Aug  1 21:29:45 1982" 'doc/disk.text.22'
hcuot "Mar 22 22:45:51 1986" 'doc/doc-changes-mail.txt.1'
hcuot "Aug  1 21:29:53 1982" 'doc/doctor.text.5'
hcuot "Aug  1 21:30:04 1982" 'doc/eddoc.text.6'
hcuot "Aug  1 21:30:11 1982" 'doc/edfnd.text.11'
hcuot "Aug  1 21:30:16 1982" 'doc/eined.text.5'
hcuot "Aug  1 21:30:23 1982" 'doc/error.lights.1'
hcuot "Aug  1 21:30:31 1982" 'doc/fasld.text.1'
hcuot "Aug  1 21:30:35 1982" 'doc/fcfs.text.10'
hcuot "Aug  1 21:42:29 1982" 'doc/fig.fil.2'
hcuot "Aug  1 21:42:35 1982" 'doc/fig1.drw.1'
hcuot "Aug  1 21:42:44 1982" 'doc/fig2.drw.1'
hcuot "Aug  1 21:42:54 1982" 'doc/fig3.drw.1'
hcuot "Aug  1 21:43:02 1982" 'doc/fig5.drw.1'
hcuot "Aug  1 21:43:11 1982" 'doc/format.text.77'
hcuot "Aug  1 21:43:25 1982" 'doc/gctim.text.5'
hcuot "Aug  1 21:43:32 1982" 'doc/goto.text.1'
hcuot "Aug  1 21:43:38 1982" 'doc/if.answer.1'
hcuot "Apr 30 00:32:36 1986" 'doc/info.lispm.1'
hcuot "Aug  1 21:44:12 1982" 'doc/info.lispm1.1'
hcuot "Aug  1 21:43:42 1982" 'doc/info.lispm2.1'
hcuot "Jan  7 18:46:34 1983" 'doc/instal.newsys.2'
hcuot "Aug  1 21:45:03 1982" 'doc/io.text.3'
hcuot "Aug  1 21:45:09 1982" 'doc/iob.text.9'
hcuot "Aug  1 21:45:16 1982" 'doc/kbds.text.8'
hcuot "Aug  1 21:45:58 1982" 'doc/lmacro.text.1'
hcuot "Aug  1 21:46:05 1982" 'doc/lmcomp.text.3'
hcuot "Aug  1 22:00:01 1982" 'doc/lmfns.text.8'
hcuot "Aug  1 22:00:23 1982" 'doc/lmnuc.text.156'
hcuot "Aug  1 22:00:41 1982" 'doc/lmtape.text.1'
hcuot "Aug  1 23:10:27 1982" 'doc/macro.text.28'
hcuot "Aug  1 23:10:33 1982" 'doc/mcdoc.text.12'
hcuot "Aug  1 23:10:38 1982" 'doc/mcrdoc.text.3'
hcuot "Aug  1 23:10:44 1982" 'doc/menu.text.1'
hcuot "Aug  1 23:10:50 1982" 'doc/mess.text.5'
hcuot "Aug  1 23:10:58 1982" 'doc/meter.text.7'
hcuot "Aug  1 23:11:06 1982" 'doc/mouse.text.3'
hcuot "Aug  1 23:11:13 1982" 'doc/name.text.5'
hcuot "Aug  1 23:11:17 1982" 'doc/nboot.text.19'
hcuot "Oct 24 21:45:35 1982" 'doc/nes.text.2'
hcuot "Nov 26 08:19:34 1984" 'doc/netwrk.msg.1'
hcuot "Aug  1 23:12:05 1982" 'doc/packd.text.5'
hcuot "Aug  1 23:12:15 1982" 'doc/paging.text.41'
hcuot "Aug  1 23:12:27 1982" 'doc/paper.text.105'
hcuot "Aug  1 23:12:48 1982" 'doc/proces.text.6'
hcuot "Aug  1 23:12:55 1982" 'doc/progr.text.29'
hcuot "Aug  1 23:13:06 1982" 'doc/qev.text.1'
hcuot "Aug  1 23:13:09 1982" 'doc/rename.text.21'
hcuot "Aug  1 23:13:13 1982" 'doc/sgmods.text.15'
hcuot "Aug  1 23:13:17 1982" 'doc/ss201.msg.1'
hcuot "Aug  1 23:13:20 1982" 'doc/stackg.text.4'
hcuot "Aug  1 23:13:24 1982" 'doc/storag.text.39'
hcuot "Aug  1 23:13:33 1982" 'doc/sys204.flavor.1'
hcuot "Aug  1 23:13:38 1982" 'doc/sys204.msg.1'
hcuot "Aug  1 23:13:44 1982" 'doc/sys210.msg.1'
hcuot "Sep 19 06:01:03 1983" 'doc/sys286.msg.5'
hcuot "Aug  1 23:14:08 1982" 'doc/sys74.msg.1'
hcuot "Aug  1 23:14:12 1982" 'doc/sys78.msg.1'
hcuot "Aug  1 23:14:16 1982" 'doc/sys79.msg.1'
hcuot "Aug  1 23:14:21 1982" 'doc/sys85.msg.1'
hcuot "Aug  1 23:14:27 1982" 'doc/sys86.msg.1'
hcuot "Aug 30 09:31:33 1982" 'doc/sys87.msg.2'
hcuot "Oct 12 09:32:12 1982" 'doc/sys88.msg.8'
hcuot "Oct 31 19:32:24 1982" 'doc/sys89.msg.9'
hcuot "Feb 22 01:00:09 1983" 'doc/sys91.msg.10'
hcuot "Mar 17 05:11:14 1983" 'doc/sys93.msg.13'
hcuot "Jun  2 12:26:24 1983" 'doc/sys94.msg.12'
hcuot "Sep 19 09:34:26 1983" 'doc/sys97.msg.7'
hcuot "Dec 24 06:19:52 1983" 'doc/sys98.defstruct.6'
hcuot "Apr 29 00:34:35 1984" 'doc/sys98.msg.34'
hcuot "Dec 15 03:24:42 1983" 'doc/sys98.packages.2'
hcuot "Nov 28 10:26:08 1984" 'doc/sys99.msg.41'
hcuot "Aug  1 23:14:41 1982" 'doc/tvdoc.text.34'
hcuot "Sep 23 01:23:03 1985" 'doc/unaddr.text.56'
hcuot "Aug  1 23:15:16 1982" 'doc/zwei.answer.1'
hcuot "Aug 19 15:56:59 1983" 'doc/zweidoc.txt.1'
hcuot "Oct 23 04:52:40 1985" 'doc/^bug.lispm^.1'
hcuot "Feb  8 05:09:02 1985" 'eh/eh.lisp.340'
hcuot "Sep  9 05:53:06 1984" 'eh/eh.qfasl.336'
hcuot "Feb 17 10:09:48 1985" 'eh/ehc.lisp.236'
hcuot "Sep  7 22:04:40 1984" 'eh/ehc.qfasl.233'
hcuot "Feb  7 13:02:56 1985" 'eh/ehf.lisp.228'
hcuot "Sep 11 21:22:50 1984" 'eh/ehf.qfasl.225'
hcuot "Nov  9 11:13:07 1984" 'eh/ehsys.lisp.1'
hcuot "May 16 12:21:59 1984" 'eh/ehw.lisp.109'
hcuot "Sep  8 01:04:14 1984" 'eh/ehw.qfasl.109'
hcuot "Feb 13 13:01:19 1985" 'eh/errmac.lisp.2'
hcuot "Aug 28 01:12:14 1985" 'eh/she.lisp.1'
hcuot "Dec  5 06:43:25 1984" 'file/bugs.mail.1'
hcuot "Jul 22 15:24:10 1982" 'file/clear.lisp.1'
hcuot "Sep 14 08:15:08 1984" 'file/copy.lisp.131'
hcuot "Jan  3 09:07:03 1984" 'file/copy.qfasl.128'
hcuot "May 14 22:28:15 1986" 'file/fs.directory.14'
hcuot "Jul 22 15:28:50 1982" 'file/fs.improv.1'
hcuot "Sep 11 06:45:16 1984" 'file/fs.lisp.77'
hcuot "Sep 11 06:48:35 1984" 'file/fs.qfasl.77'
hcuot "Nov 21 22:05:22 1984" 'file/fs-48.directory.16'
hcuot "Jan  5 00:02:49 1984" 'file/fs-48-1.lisp.1'
hcuot "Jan  5 00:03:05 1984" 'file/fs-48-1.qfasl.1'
hcuot "Jan 18 17:50:40 1984" 'file/fs-48-2.lisp.4'
hcuot "Jan 27 07:15:20 1984" 'file/fs-48-3.lisp.2'
hcuot "Jan 27 07:15:27 1984" 'file/fs-48-3.qfasl.2'
hcuot "May 16 10:04:05 1984" 'file/fs-48-4.lisp.2'
hcuot "May 16 10:04:09 1984" 'file/fs-48-4.qfasl.2'
hcuot "Jun 10 14:19:39 1984" 'file/fs-48-5.lisp.1'
hcuot "Jun 10 14:19:43 1984" 'file/fs-48-5.qfasl.1'
hcuot "Nov 21 22:04:24 1984" 'file/fs-48-6.lisp.1'
hcuot "Nov 21 22:04:35 1984" 'file/fs-48-6.qfasl.1'
hcuot "Jul 16 17:33:45 1984" 'file/fs-49.directory.3'
hcuot "Jul 16 17:33:11 1984" 'file/fs-49-1.lisp.1'
hcuot "Jul 16 17:33:18 1984" 'file/fs-49-1.qfasl.1'
hcuot "Sep 12 01:49:42 1984" 'file/fs-50.directory.1'
hcuot "Sep 15 02:45:55 1984" 'file/fs-51.directory.1'
hcuot "Apr 12 03:33:51 1985" 'file/fs-52.directory.1'
hcuot "May 14 22:28:18 1986" 'file/fs-53.directory.1'
hcuot "Apr 13 12:21:42 1985" 'file/fsacc.lisp.6'
hcuot "May 14 22:26:59 1986" 'file/fsacc.qfasl.6'
hcuot "Sep 12 05:09:25 1984" 'file/fsdefs.lisp.177'
hcuot "May 14 22:10:20 1986" 'file/fsdefs.qfasl.177'
hcuot "Nov  6 15:13:45 1982" 'file/fsdoc.text.6'
hcuot "Sep 12 05:09:30 1984" 'file/fsguts.lisp.371'
hcuot "May 14 22:17:16 1986" 'file/fsguts.qfasl.371'
hcuot "Sep 11 06:21:11 1984" 'file/fsname.lisp.106'
hcuot "Jul  5 11:03:05 1984" 'file/fsname.qfasl.104'
hcuot "Sep 12 05:09:42 1984" 'file/fsstr.lisp.107'
hcuot "May 14 22:14:57 1986" 'file/fsstr.qfasl.107'
hcuot "Sep 14 05:24:39 1984" 'file/hogs.lisp.6'
hcuot "Sep 14 04:59:04 1984" 'file/hogs.qfasl.4'
hcuot "Dec 14 02:31:32 1984" 'file/lmpars.lisp.113'
hcuot "Sep 10 20:39:55 1984" 'file/lmpars.qfasl.110'
hcuot "Apr  7 15:14:57 1984" 'file/login.lisp.26'
hcuot "Jul 22 15:35:33 1982" 'file/login.qfasl.25'
hcuot "Jan  3 06:30:18 1984" 'file/magtape.directory.9'
hcuot "Oct 26 20:41:54 1983" 'file/magtape-14.directory.14'
hcuot "Mar  8 06:56:29 1983" 'file/magtape-14-1.lisp.1'
hcuot "Mar  8 06:56:47 1983" 'file/magtape-14-1.qfasl.1'
hcuot "Mar 29 08:21:02 1983" 'file/magtape-14-2.lisp.1'
hcuot "Apr 25 09:51:40 1983" 'file/magtape-14-3.lisp.1'
hcuot "Apr 25 09:51:48 1983" 'file/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:18 1983" 'file/magtape-14-4.lisp.3'
hcuot "May 19 04:11:35 1983" 'file/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:05 1983" 'file/magtape-14-5.lisp.1'
hcuot "Oct 26 20:41:17 1983" 'file/magtape-14-5.qfasl.1'
hcuot "Jan  3 08:48:40 1984" 'file/mtaux.lisp.77'
hcuot "Dec 16 15:34:10 1983" 'file/mtdefs.lisp.30'
hcuot "Jan  3 08:49:38 1984" 'file/mtstr.lisp.85'
hcuot "Jan  3 08:50:55 1984" 'file/odump.lisp.1'
hcuot "May 15 00:29:35 1986" 'file/server.directory.7'
hcuot "Mar 27 23:45:30 1985" 'file/server.lisp.154'
hcuot "May 15 00:22:55 1986" 'file/server.qfasl.154'
hcuot "May 15 00:29:39 1986" 'file/server-10.directory.1'
hcuot "May 26 12:42:23 1984" 'file/server-8.directory.14'
hcuot "Jan  4 06:49:04 1984" 'file/server-8-1.lisp.1'
hcuot "Jan  4 06:49:14 1984" 'file/server-8-1.qfasl.1'
hcuot "Jan  5 00:06:06 1984" 'file/server-8-2.lisp.1'
hcuot "Jan  5 00:06:20 1984" 'file/server-8-2.qfasl.1'
hcuot "May 26 12:36:29 1984" 'file/server-8-3.lisp.4'
hcuot "May 26 12:36:34 1984" 'file/server-8-3.qfasl.4'
hcuot "Feb 16 15:28:03 1984" 'file/server-8-4.lisp.1'
hcuot "Feb 16 15:28:08 1984" 'file/server-8-4.qfasl.1'
hcuot "May 26 12:39:28 1984" 'file/server-8-5.lisp.2'
hcuot "May 26 12:39:34 1984" 'file/server-8-5.qfasl.2'
hcuot "Sep 15 02:35:57 1984" 'file/server-9.directory.1'
hcuot "Jul 13 07:23:33 1984" 'file/zmail.lisp.5'
hcuot "Sep  9 20:30:21 1984" 'file/zmail.qfasl.5'
hcuot "Jan  4 01:55:28 1984" 'file2/anydir.lisp.201'
hcuot "Aug  3 07:41:49 1984" 'file2/anydir.qfasl.201'
hcuot "Jan 18 17:35:23 1984" 'file2/area.lisp.22'
hcuot "Aug  3 07:33:44 1984" 'file2/area.qfasl.22'
hcuot "Jul 22 15:59:30 1982" 'file2/bfsplm.text.4'
hcuot "Dec 25 09:02:12 1985" 'file2/bug-lmfile-mail.txt.1'
hcuot "Jul 22 15:59:57 1982" 'file2/chgnod.lisp.5'
hcuot "Aug  3 07:40:05 1984" 'file2/chgnod.qfasl.5'
hcuot "Jan 18 17:46:24 1984" 'file2/complt.lisp.17'
hcuot "Aug  3 07:50:25 1984" 'file2/complt.qfasl.17'
hcuot "Aug  3 07:19:33 1984" 'file2/defs.lisp.190'
hcuot "Nov 21 19:17:17 1984" 'file2/defs.qfasl.190'
hcuot "Jan 29 07:16:03 1984" 'file2/diread.lisp.61'
hcuot "Aug  3 07:48:32 1984" 'file2/diread.qfasl.61'
hcuot "Jul 22 16:02:31 1982" 'file2/doc.text.12'
hcuot "Jan 18 17:50:00 1984" 'file2/dump.lisp.29'
hcuot "Aug  3 07:56:23 1984" 'file2/dump.qfasl.29'
hcuot "Jan 18 17:34:11 1984" 'file2/files.lisp.122'
hcuot "Aug  3 07:30:22 1984" 'file2/files.qfasl.122'
hcuot "Dec 18 10:50:47 1983" 'file2/free.lisp.48'
hcuot "Aug  3 07:26:33 1984" 'file2/free.qfasl.48'
hcuot "Feb  9 18:19:25 1986" 'file2/fs-fc-mail.txt.1'
hcuot "Jan 18 17:30:16 1984" 'file2/gc.lisp.19'
hcuot "Aug  3 07:32:53 1984" 'file2/gc.qfasl.19'
hcuot "Jan 18 17:29:20 1984" 'file2/io.lisp.94'
hcuot "Aug  3 07:28:01 1984" 'file2/io.qfasl.94'
hcuot "Feb  1 03:07:28 1984" 'file2/link.lisp.47'
hcuot "Aug  3 07:37:26 1984" 'file2/link.qfasl.47'
hcuot "Aug  3 08:02:34 1984" 'file2/lmfile.directory.4'
hcuot "Sep 30 07:33:31 1983" 'file2/lmfile-2.directory.10'
hcuot "Jun 16 23:34:55 1983" 'file2/lmfile-2-1.lisp.1'
hcuot "Jul  6 22:59:00 1983" 'file2/lmfile-2-2.lisp.1'
hcuot "Jul  7 07:33:09 1983" 'file2/lmfile-2-3.lisp.2'
hcuot "Sep 30 07:32:51 1983" 'file2/lmfile-2-4.lisp.1'
hcuot "Jan 29 08:54:05 1984" 'file2/lmfile-3.directory.7'
hcuot "Jan  4 01:56:37 1984" 'file2/lmfile-3-1.lisp.1'
hcuot "Jan  4 01:56:48 1984" 'file2/lmfile-3-1.qfasl.1'
hcuot "Jan 29 08:53:39 1984" 'file2/lmfile-3-3.lisp.1'
hcuot "Jan 29 08:53:44 1984" 'file2/lmfile-3-3.qfasl.1'
hcuot "Aug  3 08:02:37 1984" 'file2/lmfile-4.directory.1'
hcuot "Jul 22 16:07:13 1982" 'file2/maint.text.9'
hcuot "Dec  2 15:30:00 1984" 'file2/maiser.lisp.13'
hcuot "Aug  3 08:01:34 1984" 'file2/maiser.qfasl.9'
hcuot "Jan 18 17:37:55 1984" 'file2/node.lisp.162'
hcuot "Aug  3 07:34:26 1984" 'file2/node.qfasl.162'
hcuot "Nov 21 18:30:20 1984" 'file2/pack.lisp.83'
hcuot "Nov 21 19:37:41 1984" 'file2/pack.qfasl.83'
hcuot "Nov 21 05:43:10 1984" 'file2/pathnm.lisp.163'
hcuot "Sep 11 06:27:31 1984" 'file2/pathnm.qfasl.162'
hcuot "Dec 18 22:59:00 1983" 'file2/pdp10.lisp.20'
hcuot "Aug  3 07:38:34 1984" 'file2/pdp10.qfasl.20'
hcuot "Nov 23 09:57:38 1982" 'file2/remote.directory.10'
hcuot "Dec 12 07:15:02 1982" 'file2/remote.lisp.30'
hcuot "Dec 30 05:30:12 1982" 'file2/remote-23.directory.4'
hcuot "Jan 29 08:53:17 1984" 'file2/repair.lisp.1'
hcuot "Dec 18 10:20:56 1983" 'file2/rmdefs.lisp.10'
hcuot "Aug  3 09:02:57 1984" 'file2/rmdefs.qfasl.10'
hcuot "Jan 18 17:42:38 1984" 'file2/salvag.lisp.22'
hcuot "Aug  3 07:40:37 1984" 'file2/salvag.qfasl.22'
hcuot "Nov 21 21:59:39 1984" 'file2/server.lisp.52'
hcuot "Nov 21 19:41:01 1984" 'file2/server.qfasl.50'
hcuot "Jan 18 17:45:18 1984" 'file2/spcdir.lisp.92'
hcuot "Aug  3 07:45:33 1984" 'file2/spcdir.qfasl.92'
hcuot "Aug  3 08:54:13 1984" 'file2/stream.lisp.210'
hcuot "Aug  3 08:57:08 1984" 'file2/stream.qfasl.210'
hcuot "Apr  7 15:16:07 1984" 'file2/system.lisp.32'
hcuot "May 23 03:23:41 1984" 'file2/system.qfasl.32'
hcuot "Jul 22 16:18:48 1982" 'file2/view.text.9'
hcuot "Jan 18 17:22:34 1984" 'file2/xrmdefs.lisp.11'
hcuot "Jan 18 17:05:07 1984" 'file2/xserver.lisp.44'
hcuot "Jun 18 05:38:36 1984" 'fonts/13fgb.qfasl.6'
hcuot "Jun 18 05:38:32 1984" 'fonts/16fg.qfasl.5'
hcuot "Jun 18 05:38:21 1984" 'fonts/18fg.qfasl.5'
hcuot "Jun 18 05:38:12 1984" 'fonts/20vr.qfasl.5'
hcuot "Jun 18 05:38:08 1984" 'fonts/25fr3.qfasl.5'
hcuot "Jun 18 05:38:03 1984" 'fonts/31vr.qfasl.5'
hcuot "Jun 18 05:37:56 1984" 'fonts/40vr.qfasl.5'
hcuot "Jun 18 05:37:52 1984" 'fonts/40vshd.qfasl.5'
hcuot "Jun 18 05:37:43 1984" 'fonts/43vxms.qfasl.10'
hcuot "Jun 18 05:37:40 1984" 'fonts/5x5.qfasl.10'
hcuot "Jun 18 05:37:37 1984" 'fonts/abacus.qfasl.5'
hcuot "Jun 18 05:37:34 1984" 'fonts/apl14.qfasl.4'
hcuot "Jun 18 05:37:30 1984" 'fonts/arr10.qfasl.5'
hcuot "Sep 10 20:20:10 1984" 'fonts/bigfnt.qfasl.10'
hcuot "Jul 20 14:17:55 1982" 'fonts/bigold.qfasl.1'
hcuot "Jun 18 05:37:22 1984" 'fonts/bigvg.qfasl.4'
hcuot "Jun 18 05:37:20 1984" 'fonts/color-5x5.qfasl.4'
hcuot "Jun 18 05:37:16 1984" 'fonts/color-cptfont.qfasl.3'
hcuot "Jun 18 05:37:12 1984" 'fonts/color-medfnt.qfasl.4'
hcuot "Jun 18 05:37:07 1984" 'fonts/color-mouse.qfasl.4'
hcuot "Jun 18 05:37:04 1984" 'fonts/courier.qfasl.5'
hcuot "Jul 20 14:18:26 1982" 'fonts/cptfon.qfasl.3'
hcuot "Jun 18 05:37:01 1984" 'fonts/cptfont.qfasl.19'
hcuot "Jun 18 05:36:58 1984" 'fonts/cptfontb.qfasl.7'
hcuot "Jun 18 05:36:54 1984" 'fonts/cyr.qfasl.5'
hcuot "Jun 18 05:36:50 1984" 'fonts/cyr12.qfasl.5'
hcuot "Jun 18 05:36:46 1984" 'fonts/ent.qfasl.5'
hcuot "Dec 10 23:35:35 1983" 'fonts/equivalence.lisp.1'
hcuot "Jun 18 05:36:42 1984" 'fonts/gach10.qfasl.3'
hcuot "Jun 18 05:36:35 1984" 'fonts/gach10b.qfasl.3'
hcuot "Jun 18 05:36:27 1984" 'fonts/gach12.qfasl.3'
hcuot "Jul 20 14:18:41 1982" 'fonts/gfr.archiv.1'
hcuot "Jun 18 05:36:20 1984" 'fonts/hippo10.qfasl.4'
hcuot "Jun 18 05:36:14 1984" 'fonts/hippo18.qfasl.3'
hcuot "Jun 18 05:36:11 1984" 'fonts/hl10.qfasl.9'
hcuot "Jun 18 05:36:07 1984" 'fonts/hl10b.qfasl.9'
hcuot "Jun 18 05:36:03 1984" 'fonts/hl12.qfasl.10'
hcuot "Jun 18 05:35:59 1984" 'fonts/hl12b.qfasl.15'
hcuot "Jun 18 05:35:56 1984" 'fonts/hl12b1.qfasl.3'
hcuot "Jun 18 05:35:52 1984" 'fonts/hl12bi.qfasl.10'
hcuot "Jun 18 05:35:49 1984" 'fonts/hl12i.qfasl.11'
hcuot "Jun 18 05:35:45 1984" 'fonts/hl18.qfasl.6'
hcuot "Jun 18 05:35:42 1984" 'fonts/hl6.qfasl.9'
hcuot "Jun 18 05:35:39 1984" 'fonts/hl7.qfasl.9'
hcuot "Jun 18 05:35:34 1984" 'fonts/icons.qfasl.3'
hcuot "Jun 18 05:35:30 1984" 'fonts/invisible.qfasl.3'
hcuot "Jun 18 05:35:27 1984" 'fonts/medfnb.qfasl.8'
hcuot "Jun 18 05:35:17 1984" 'fonts/medfnt.qfasl.9'
hcuot "Jun 18 05:35:13 1984" 'fonts/mets.qfasl.9'
hcuot "Jun 18 05:35:07 1984" 'fonts/metsi.qfasl.9'
hcuot "Jun 18 05:35:00 1984" 'fonts/mit.qfasl.5'
hcuot "Jun 18 05:34:56 1984" 'fonts/mouse.qfasl.9'
hcuot "Jun 18 05:34:53 1984" 'fonts/narrow.qfasl.5'
hcuot "Jun 18 05:34:50 1984" 'fonts/panes.qfasl.3'
hcuot "Nov 15 12:23:32 1983" 'fonts/prt12b.qfasl.2'
hcuot "Jun 18 05:34:45 1984" 'fonts/s30chs.qfasl.5'
hcuot "Jun 18 05:34:41 1984" 'fonts/s35ger.qfasl.3'
hcuot "Jun 18 05:34:35 1984" 'fonts/sail12.qfasl.6'
hcuot "Jun 18 05:34:32 1984" 'fonts/search.qfasl.9'
hcuot "Jun 18 05:34:28 1984" 'fonts/ship.qfasl.6'
hcuot "Oct  9 11:53:23 1984" 'fonts/storybook.qfasl.1'
hcuot "Oct  9 11:54:06 1984" 'fonts/storybookbold.qfasl.1'
hcuot "Jun 18 05:34:24 1984" 'fonts/tally.qfasl.5'
hcuot "Jul 20 14:20:57 1982" 'fonts/times.9rom.1'
hcuot "Jun 18 05:34:20 1984" 'fonts/tiny.qfasl.5'
hcuot "Jun 18 05:34:15 1984" 'fonts/tog.qfasl.5'
hcuot "Jun 18 05:34:12 1984" 'fonts/tr10.qfasl.9'
hcuot "Jun 18 05:34:09 1984" 'fonts/tr10b.qfasl.8'
hcuot "Jun 18 05:34:04 1984" 'fonts/tr10bi.qfasl.7'
hcuot "Jun 18 05:34:00 1984" 'fonts/tr10i.qfasl.7'
hcuot "Jun 18 05:33:57 1984" 'fonts/tr10ic.qfasl.4'
hcuot "Jun 18 05:33:53 1984" 'fonts/tr12.qfasl.11'
hcuot "Jun 18 05:33:49 1984" 'fonts/tr12b.qfasl.17'
hcuot "Jun 18 05:33:45 1984" 'fonts/tr12b1.qfasl.8'
hcuot "Jun 18 05:33:42 1984" 'fonts/tr12bi.qfasl.9'
hcuot "Jun 18 05:33:37 1984" 'fonts/tr12i.qfasl.13'
hcuot "Jun 18 05:33:30 1984" 'fonts/tr18.qfasl.7'
hcuot "Jun 18 05:33:16 1984" 'fonts/tr18b.qfasl.3'
hcuot "Jun 18 05:33:12 1984" 'fonts/tr8.qfasl.8'
hcuot "Jun 18 05:33:07 1984" 'fonts/tr8b.qfasl.8'
hcuot "Jun 18 05:33:04 1984" 'fonts/tr8i.qfasl.6'
hcuot "Jun 18 05:33:00 1984" 'fonts/tvbug.qfasl.5'
hcuot "Jun 18 05:32:57 1984" 'fonts/tvfont.qfasl.7'
hcuot "Jun 18 05:32:50 1984" 'fonts/worm.qfasl.4'
hcuot "Sep 15 08:28:36 1984" 'io/crdtbl.lisp.35'
hcuot "Aug 31 17:23:39 1984" 'io/crdtbl.qfasl.1'
hcuot "Nov 28 00:27:38 1984" 'io/disk.lisp.292'
hcuot "Nov 21 19:22:39 1984" 'io/disk.qfasl.291'
hcuot "May 20 13:21:35 1984" 'io/dledit.lisp.52'
hcuot "Nov 21 19:34:29 1984" 'io/dledit.qfasl.52'
hcuot "Apr  5 08:37:55 1985" 'io/dribbl.lisp.37'
hcuot "Aug 15 04:10:20 1984" 'io/dribbl.qfasl.36'
hcuot "Aug 30 21:58:54 1984" 'io/find-plausible-partitions.lisp.1'
hcuot "Feb  8 05:25:40 1985" 'io/format.lisp.241'
hcuot "Sep  7 22:25:02 1984" 'io/format.qfasl.234'
hcuot "Jan 22 15:20:39 1983" 'io/format-macro.lisp.2'
hcuot "Aug  3 06:29:26 1984" 'io/format-macro.qfasl.2'
hcuot "Dec 10 12:33:13 1984" 'io/fread.lisp.30'
hcuot "Oct  4 18:25:58 1985" 'io/fread.qfasl.30'
hcuot "Aug 10 08:29:02 1984" 'io/grind.lisp.146'
hcuot "Aug 29 23:06:51 1984" 'io/grind.qfasl.145'
hcuot "Feb 27 02:58:01 1985" 'io/print.lisp.183'
hcuot "Sep 10 07:18:21 1984" 'io/print.qfasl.178'
hcuot "Dec  9 09:44:53 1984" 'io/qio.lisp.217'
hcuot "Aug 31 05:38:38 1984" 'io/qio.qfasl.214'
hcuot "Apr  7 15:18:03 1984" 'io/rcomp.lisp.10'
hcuot "Nov  1 11:32:34 1984" 'io/rddefs.lisp.62'
hcuot "Sep  7 22:28:41 1984" 'io/rddefs.qfasl.61'
hcuot "Sep 15 08:33:22 1984" 'io/rdtbl.lisp.169'
hcuot "Sep 15 08:32:16 1984" 'io/rdtbl.qfasl.167'
hcuot "Nov 20 19:22:01 1984" 'io/read.lisp.437'
hcuot "Aug 15 06:03:40 1984" 'io/read.qfasl.432'
hcuot "Dec  6 10:14:19 1984" 'io/rtc.lisp.47'
hcuot "Sep 10 05:53:33 1984" 'io/rtc.qfasl.46'
hcuot "Jan 27 05:19:23 1984" 'io/simple-ether.lisp.1'
hcuot "Mar  2 05:04:00 1985" 'io/stream.lisp.111'
hcuot "Sep  4 05:40:02 1984" 'io/stream.qfasl.108'
hcuot "Oct 30 05:27:53 1983" 'io/strmdoc.lisp.2'
hcuot "Dec  1 03:13:54 1984" 'io/unibus.lisp.26'
hcuot "Aug 15 06:24:34 1984" 'io/unibus.qfasl.25'
hcuot "Nov 29 06:55:23 1984" 'io/access.lisp.13'   # was 'io/file/access.lisp.13'
hcuot "Sep 11 07:23:13 1984" 'io/access.qfasl.8'   # was 'io/file/access.qfasl.8'
hcuot "Oct 25 04:23:11 1983" 'io/baldir.lisp.114'  # was 'io/file/baldir.lisp.114'
hcuot "Aug 15 08:28:24 1984" 'io/baldir.qfasl.114' # was 'io/file/baldir.qfasl.114'
hcuot "Feb 15 05:03:22 1985" 'io/logical.lisp.1'   # was 'io/file/logical.lisp.1'
hcuot "Feb 13 05:15:41 1985" 'io/open.lisp.180'	   # was 'io/file/open.lisp.180'
hcuot "Sep 11 06:36:49 1984" 'io/open.qfasl.174'   # was 'io/file/open.qfasl.174'
hcuot "May 17 07:21:01 1985" 'io/pathnm.lisp.538'  # was 'io/file/pathnm.lisp.538'
hcuot "Sep  7 23:08:16 1984" 'io/pathnm.qfasl.528' # was 'io/file/pathnm.qfasl.528'
hcuot "Feb 24 08:18:10 1985" 'io/pathst.lisp.181'  # was 'io/file/pathst.lisp.181'
hcuot "Sep  7 23:13:26 1984" 'io/pathst.qfasl.173' # was 'io/file/pathst.qfasl.173'
hcuot "Jun 29 10:46:28 1982" 'io1/10leaf.points.1'
hcuot "Mar 17 08:10:51 1983" 'io1/as8748.lisp.40'
hcuot "Sep 16 11:09:14 1982" 'io1/as8751.lisp.29'
hcuot "Apr  7 15:18:57 1984" 'io1/cdrive.lisp.103'
hcuot "Apr  7 15:19:19 1984" 'io1/chatst.lisp.66'
hcuot "Sep 11 21:11:30 1984" 'io1/conver.lisp.147'
hcuot "Sep 11 21:37:37 1984" 'io1/conver.qfasl.147'
hcuot "Jun 29 10:49:20 1982" 'io1/door.bin.1'
hcuot "Jun 29 10:49:15 1982" 'io1/door.text.2'
hcuot "Jan 28 01:44:08 1984" 'io1/dplt.lisp.109'
hcuot "Apr  7 15:21:38 1984" 'io1/draw.lisp.23'
hcuot "Feb 13 01:04:09 1984" 'io1/eftp.bin-4.1'
hcuot "Nov 28 23:44:32 1983" 'io1/eftp.bin-5.1'
hcuot "Mar 31 07:20:29 1985" 'io1/eftp.bin-6.1'
hcuot "Dec 18 20:24:31 1983" 'io1/eftp.lisp.38'
hcuot "Jun  5 06:27:46 1984" 'io1/fntcnv.lisp.83'
hcuot "Aug 30 00:16:54 1984" 'io1/fntcnv.qfasl.83'
hcuot "Apr  7 15:22:34 1984" 'io1/fntdef.lisp.20'
hcuot "Feb  7 05:12:08 1985" 'io1/fquery.lisp.46'
hcuot "Aug  3 06:30:55 1984" 'io1/fquery.qfasl.45'
hcuot "Jun 29 10:50:23 1982" 'io1/hacks.lisp.190'
hcuot "Jun 27 12:05:25 1984" 'io1/hardcopy.lisp.1'
hcuot "Aug 15 05:53:21 1984" 'io1/hardcopy.qfasl.1'
hcuot "Aug 24 16:56:29 1983" 'io1/inc.lisp.8'
hcuot "Aug 15 04:58:05 1984" 'io1/inc.qfasl.8'
hcuot "Jun 13 22:49:50 1984" 'io1/infix.lisp.11'
hcuot "Aug 15 04:59:17 1984" 'io1/infix.qfasl.11'
hcuot "Jun 24 17:23:23 1984" 'io1/meter.lisp.42'
hcuot "Aug 30 03:21:38 1984" 'io1/meter.qfasl.42'
hcuot "Jun 29 10:50:59 1982" 'io1/mouse.text.11'
hcuot "Aug  3 07:01:51 1984" 'io1/output.lisp.38'
hcuot "Sep  7 22:29:43 1984" 'io1/output.qfasl.38'
hcuot "Nov 28 23:41:27 1983" 'io1/press.bin-5.3'
hcuot "Oct 13 05:30:20 1984" 'io1/press.lisp.147'
hcuot "Aug 30 03:16:40 1984" 'io1/press.qfasl.146'
hcuot "Apr  7 15:25:35 1984" 'io1/promp.lisp.13'
hcuot "Aug  3 12:06:58 1984" 'io1/reldmp.lisp.12'
hcuot "Aug  3 22:01:31 1984" 'io1/reldmp.qfasl.12'
hcuot "Apr  7 15:29:25 1984" 'io1/relld.lisp.10'
hcuot "Sep  7 17:09:42 1984" 'io1/rfontw.bin-5.1'
hcuot "Mar 31 07:19:47 1985" 'io1/rfontw.bin-6.1'
hcuot "Sep  6 22:22:47 1984" 'io1/rfontw.lisp.82'
hcuot "Sep  6 22:25:03 1984" 'io1/rfontw.qfasl.82'
hcuot "Nov 28 22:09:46 1983" 'io1/rfontx.lisp.75'
hcuot "Sep  6 22:16:41 1984" 'io1/rfontx.qfasl.75'
hcuot "May 12 06:21:02 1984" 'io1/serial.lisp.32'
hcuot "Aug 15 06:11:14 1984" 'io1/serial.qfasl.32'
hcuot "Jul  2 16:55:54 1984" 'io1/srccom.lisp.37'
hcuot "Aug 30 03:24:08 1984" 'io1/srccom.qfasl.37'
hcuot "Apr 23 11:39:47 1984" 'io1/swar.lisp.12'
hcuot "May 31 12:30:55 1985" 'io1/swar.qfasl.12'
hcuot "Nov  6 12:48:06 1984" 'io1/time.lisp.110'
hcuot "Aug  3 11:46:51 1984" 'io1/time.qfasl.105'
hcuot "Oct 20 19:28:27 1984" 'io1/timpar.lisp.75'
hcuot "Aug  3 11:48:41 1984" 'io1/timpar.qfasl.74'
hcuot "Jun 29 10:55:20 1982" 'io1/ukbd.lisp.24'
hcuot "Jun 29 10:55:35 1982" 'io1/wlr.doc.1'
hcuot "Jul 22 11:15:54 1983" 'io1/xgp.lisp.33'
hcuot "Sep 10 20:20:48 1984" 'io1/xgp.qfasl.33'
hcuot "Aug  1 23:16:45 1982" 'man/.bug.lmman.1'
hcuot "Aug  1 23:17:23 1982" 'man/.dlw.wordab.1'
hcuot "Aug  1 23:17:27 1982" 'man/.forma.text.33'
hcuot "Aug  1 23:17:39 1982" 'man/.machn.compar.1'
hcuot "Aug  1 23:17:42 1982" 'man/30flsp.kst.1'
hcuot "Aug  1 23:17:52 1982" 'man/37vrbl.kst.1'
hcuot "May 20 01:12:20 1984" 'man/areas.text.46'
hcuot "Oct 29 03:30:47 1985" 'man/bug-mail.txt.1'
hcuot "Jul 27 06:52:53 1984" 'man/chaos.text.27'
hcuot "Jun  1 08:44:46 1984" 'man/code.text.36'
hcuot "Jun  4 07:31:51 1984" 'man/compil.text.106'
hcuot "Jun  8 08:41:24 1984" 'man/cumulative.vars.24'
hcuot "Jun  1 07:23:25 1984" 'man/db-aid.text.14'
hcuot "May 21 20:48:55 1984" 'man/debug.text.21'
hcuot "Jun  8 07:14:16 1984" 'man/defstr.text.117'
hcuot "Jun  1 06:32:40 1984" 'man/errors.text.102'
hcuot "May 21 06:53:47 1984" 'man/fd-arr.text.26'
hcuot "May 20 01:12:55 1984" 'man/fd-clo.text.12'
hcuot "May 21 01:20:57 1984" 'man/fd-con.text.28'
hcuot "Jun  5 05:25:24 1984" 'man/fd-dtp.text.19'
hcuot "Jun  1 11:17:42 1984" 'man/fd-eva.text.46'
hcuot "May 21 18:53:05 1984" 'man/fd-fio.text.24'
hcuot "May 19 21:22:03 1984" 'man/fd-flo.text.24'
hcuot "Jun  1 04:27:05 1984" 'man/fd-fun.text.26'
hcuot "Jun  1 09:36:57 1984" 'man/fd-hac.text.47'
hcuot "May 20 23:56:54 1984" 'man/fd-loc.text.9'
hcuot "Jun  8 21:51:15 1984" 'man/fd-num.text.37'
hcuot "May 19 21:22:43 1984" 'man/fd-op.text.5'
hcuot "Jul 27 06:51:28 1984" 'man/fd-sg.text.16'
hcuot "Jun  1 10:34:17 1984" 'man/fd-str.text.27'
hcuot "Jun  1 04:32:03 1984" 'man/fd-sub.text.19'
hcuot "May 20 23:56:59 1984" 'man/fd-sym.text.14'
hcuot "Jun  1 08:45:51 1984" 'man/files.text.24'
hcuot "Aug  1 23:25:37 1982" 'man/flavor.bolio.1'
hcuot "Jun  1 11:34:13 1984" 'man/flavor.text.134'
hcuot "Aug  1 23:25:53 1982" 'man/font3.kst.1'
hcuot "May 19 20:30:31 1984" 'man/generic.text.14'
hcuot "Jun  8 08:08:53 1984" 'man/index.temp.2'
hcuot "May 19 20:56:30 1984" 'man/init.text.17'
hcuot "Jun  4 07:16:55 1984" 'man/intro.text.18'
hcuot "May 21 18:53:12 1984" 'man/ios.text.247'
hcuot "Apr 26 12:50:32 1983" 'man/looptm.lispm.2'
hcuot "Jun  1 04:33:03 1984" 'man/looptm.text.320'
hcuot "May 21 07:37:20 1984" 'man/macros.text.104'
hcuot "May 19 05:54:05 1984" 'man/maksys.text.38'
hcuot "Mar 11 00:43:51 1984" 'man/manual.bolio.25'
hcuot "Jun  1 10:42:32 1984" 'man/manual.fasl.33'
hcuot "Jun  1 10:30:15 1984" 'man/manual.lisp.33'
hcuot "Jun  1 06:40:13 1984" 'man/manual.text.44'
hcuot "Jun  8 08:31:51 1984" 'man/manual.vars.26'
hcuot "Mar 11 00:43:46 1984" 'man/manual2.bolio.2'
hcuot "Jun  8 08:09:29 1984" 'man/manual2.log.6'
hcuot "May 21 20:38:36 1984" 'man/manual2.text.8'
hcuot "Jun  1 06:56:56 1984" 'man/manual2a.10.1'
hcuot "Mar 11 00:43:46 1984" 'man/manual3.bolio.1'
hcuot "Mar 12 05:27:45 1984" 'man/manual3.text.1'
hcuot "Jun  1 06:38:17 1984" 'man/manual3a.text.1'
hcuot "Aug  1 23:28:55 1982" 'man/msg.text.8'
hcuot "Jun  1 06:28:39 1984" 'man/packd.text.106'
hcuot "Jun  1 06:29:53 1984" 'man/patch.text.54'
hcuot "Jun  5 05:31:29 1984" 'man/pathnm.text.99'
hcuot "May 20 01:15:40 1984" 'man/proces.text.55'
hcuot "May 20 06:21:25 1984" 'man/query.text.22'
hcuot "Jun  1 05:25:06 1984" 'man/rdprt.text.29'
hcuot "May 21 01:20:51 1984" 'man/resour.text.28'
hcuot "May 20 06:21:07 1984" 'man/stream.text.37'
hcuot "Oct  7 07:05:57 1982" 'man/testman.bolio.5'
hcuot "Oct  7 02:51:24 1982" 'man/testman.text.2'
hcuot "May 19 20:30:40 1984" 'man/time.text.40'
hcuot "Jun  8 04:11:55 1984" 'man/title.text.11'
hcuot "Jul 11 02:29:09 1984" 'network/addr-res.lisp.8'
hcuot "Jan  2 02:34:32 1984" 'network/ether-mini.lisp.10'
hcuot "Nov 28 11:35:58 1984" 'network/host.lisp.121'
hcuot "Sep 10 22:45:38 1984" 'network/host.qfasl.116'
hcuot "Sep  9 03:54:51 1984" 'network/package.lisp.7'
hcuot "Sep  9 05:59:30 1984" 'network/package.qfasl.7'
hcuot "Jul 15 06:00:45 1984" 'network/regions.lisp.1'
hcuot "May 30 07:15:13 1984" 'network/server.lisp.1'
hcuot "May 30 07:14:54 1984" 'network/service.lisp.3'
hcuot "Jul 13 19:29:53 1984" 'network/simple-ether.lisp.51'
hcuot "Jul 15 06:01:32 1984" 'network/smtp.lisp.1'
hcuot "Jul  4 18:10:43 1984" 'network/symbols.lisp.1'
hcuot "May 30 05:58:43 1984" 'network/symbols.qfasl.1'
hcuot "Dec 14 07:14:16 1984" 'io1/chatst.lisp.67'     # was 'network/chaos/chatst.lisp.67'
hcuot "Jun  6 06:11:05 1984" 'io1/chatst.qfasl.66'    # was 'network/chaos/chatst.qfasl.66'
hcuot "Dec  6 13:20:08 1984" 'io/chsaux.lisp.366'     # was 'network/chaos/chsaux.lisp.366'
hcuot "Aug 15 07:26:28 1984" 'io/chsaux.qfasl.359'    # was 'network/chaos/chsaux.qfasl.359'
hcuot "Mar 12 01:23:31 1985" 'io/chsncp.lisp.270'     # was 'network/chaos/chsncp.lisp.270'
hcuot "Sep 11 21:12:45 1984" 'io/chsncp.qfasl.265'    # was 'network/chaos/chsncp.qfasl.265'
hcuot "Nov 26 20:11:26 1984" 'io/chuse.lisp.14'	      # was 'network/chaos/chuse.lisp.14'
hcuot "Sep  4 21:12:29 1984" 'io/chuse.qfasl.11'      # was 'network/chaos/chuse.qfasl.11'
hcuot "Jun  4 20:53:34 1984" 'io1/eftp.lisp.39'	      # was 'network/chaos/eftp.lisp.39'
hcuot "Jun  6 06:12:57 1984" 'io1/eftp.qfasl.39'      # was 'network/chaos/eftp.qfasl.39'
hcuot "Sep 10 21:59:11 1984" 'window/peekch.lisp.31'  # was 'network/chaos/peekch.lisp.31'
hcuot "Sep 10 22:44:15 1984" 'window/peekch.qfasl.31' # was 'network/chaos/peekch.qfasl.31'
hcuot "Apr 13 05:45:29 1985" 'io/qfile.lisp.360'      # was 'network/chaos/qfile.lisp.360'
hcuot "Sep 10 22:37:50 1984" 'io/qfile.qfasl.353'     # was 'network/chaos/qfile.qfasl.353'
hcuot "Jul 17 15:27:46 1984" 'network/ip/address.lisp.3'
hcuot "Jul 17 15:27:51 1984" 'network/ip/address.qfasl.3'
hcuot "Nov 10 23:02:36 1984" 'network/ip/hostsnic.lisp.4'
hcuot "Nov 16 08:06:05 1984" 'patch/band.win.lisp.2'
hcuot "Nov 16 08:06:21 1984" 'patch/band.win.qfasl.2'
hcuot "Sep 11 23:19:54 1984" 'patch/cadr.patch-directory.1'
hcuot "Oct 22 19:47:31 1985" 'patch/cadr-4.patch-directory.12'
hcuot "Jan 28 07:01:11 1985" 'patch/cadr-4-1.lisp.8'
hcuot "Jan 28 07:01:19 1985" 'patch/cadr-4-1.qfasl.8'
hcuot "Jan  3 20:09:24 1985" 'patch/cadr-4-2.lisp.1'
hcuot "Jan  3 20:10:46 1985" 'patch/cadr-4-2.qfasl.1'
hcuot "Oct 22 19:45:59 1985" 'patch/cadr-4-3.lisp.1'
hcuot "Oct 22 19:46:16 1985" 'patch/cadr-4-3.qfasl.1'
hcuot "Nov 16 08:09:19 1984" 'patch/lm27fix.lisp.1'
hcuot "Nov 16 08:09:37 1984" 'patch/lm27fix.qfasl.1'
hcuot "Jun  7 00:30:00 1984" 'patch/system.patch-directory.25'
hcuot "Feb  4 03:12:33 1984" 'patch/system-94.patch-directory.129'
hcuot "Aug 21 21:52:31 1983" 'patch/system-94-41.qfasl.2'
hcuot "Nov  8 09:03:14 1983" 'patch/system-94-42.qfasl.1'
hcuot "Nov 13 05:20:58 1983" 'patch/system-94-43.qfasl.2'
hcuot "Nov 30 02:11:39 1983" 'patch/system-97.patch-directory.76'
hcuot "Nov  9 22:51:17 1983" 'patch/system-97-25.qfasl.1'
hcuot "Nov 11 20:27:52 1983" 'patch/system-97-26.qfasl.1'
hcuot "Nov 29 23:17:29 1983" 'patch/system-97-27.qfasl.1'
hcuot "Nov 30 02:11:04 1983" 'patch/system-97-28.qfasl.1'
hcuot "Nov 28 16:02:04 1984" 'patch/system-98.patch-directory.304'
hcuot "Nov 24 00:42:57 1983" 'patch/system-98-1.lisp.5'
hcuot "Dec 23 08:14:14 1983" 'patch/system-98-10.lisp.15'
hcuot "Dec 26 09:56:51 1983" 'patch/system-98-11.lisp.19'
hcuot "Dec 27 07:15:02 1983" 'patch/system-98-12.lisp.16'
hcuot "Dec 24 06:37:44 1983" 'patch/system-98-13.lisp.4'
hcuot "Dec 27 08:58:42 1983" 'patch/system-98-14.lisp.15'
hcuot "Dec 28 10:09:13 1983" 'patch/system-98-15.lisp.7'
hcuot "Dec 29 10:08:59 1983" 'patch/system-98-16.lisp.6'
hcuot "Jan  1 05:30:38 1984" 'patch/system-98-17.lisp.18'
hcuot "Jan  1 15:49:39 1984" 'patch/system-98-18.lisp.10'
hcuot "Jan  3 05:54:10 1984" 'patch/system-98-19.lisp.20'
hcuot "Nov 30 05:36:10 1983" 'patch/system-98-2.lisp.12'
hcuot "Jan  2 07:53:30 1984" 'patch/system-98-20.lisp.4'
hcuot "Jan  3 06:16:42 1984" 'patch/system-98-21.lisp.2'
hcuot "Jan  3 09:49:01 1984" 'patch/system-98-22.lisp.6'
hcuot "Jan  4 10:19:59 1984" 'patch/system-98-23.lisp.10'
hcuot "Jan  3 10:57:14 1984" 'patch/system-98-24.lisp.3'
hcuot "Jan  5 23:39:41 1984" 'patch/system-98-25.lisp.8'
hcuot "Jan  7 12:40:39 1984" 'patch/system-98-26.lisp.6'
hcuot "Jan 12 16:11:12 1984" 'patch/system-98-27.lisp.7'
hcuot "Jan 10 02:43:59 1984" 'patch/system-98-28.lisp.3'
hcuot "Jan 15 03:33:30 1984" 'patch/system-98-29.lisp.12'
hcuot "Dec  6 14:55:14 1983" 'patch/system-98-3.lisp.16'
hcuot "Jan 30 04:20:17 1984" 'patch/system-98-30.lisp.22'
hcuot "Feb  1 10:45:59 1984" 'patch/system-98-31.lisp.18'
hcuot "Jan 27 09:08:05 1984" 'patch/system-98-32.lisp.8'
hcuot "Feb  8 17:54:19 1984" 'patch/system-98-33.lisp.26'
hcuot "Feb  1 13:00:29 1984" 'patch/system-98-34.lisp.1'
hcuot "Feb 15 23:07:07 1984" 'patch/system-98-35.lisp.9'
hcuot "Feb  3 09:30:52 1984" 'patch/system-98-36.lisp.1'
hcuot "Feb 22 23:51:54 1984" 'patch/system-98-37.lisp.10'
hcuot "Mar 12 01:18:12 1984" 'patch/system-98-38.lisp.4'
hcuot "Mar 24 20:37:28 1984" 'patch/system-98-39.lisp.20'
hcuot "Dec  5 14:56:31 1983" 'patch/system-98-4.lisp.7'
hcuot "Apr  3 12:57:33 1984" 'patch/system-98-40.lisp.43'
hcuot "Apr  6 23:33:48 1984" 'patch/system-98-41.lisp.10'
hcuot "Mar 21 21:54:25 1984" 'patch/system-98-42.lisp.2'
hcuot "Mar 15 09:21:58 1984" 'patch/system-98-43.lisp.1'
hcuot "Apr 17 21:47:49 1984" 'patch/system-98-44.lisp.22'
hcuot "Apr 21 23:27:40 1984" 'patch/system-98-45.lisp.5'
hcuot "Apr  6 09:35:55 1984" 'patch/system-98-46.lisp.1'
hcuot "May  8 10:23:06 1984" 'patch/system-98-47.lisp.37'
hcuot "Apr 18 08:16:58 1984" 'patch/system-98-48.lisp.1'
hcuot "May 29 18:44:39 1984" 'patch/system-98-49.lisp.8'
hcuot "Dec  9 04:28:54 1983" 'patch/system-98-5.lisp.11'
hcuot "Jun  6 00:30:09 1984" 'patch/system-98-50.lisp.39'
hcuot "Jun  6 00:34:15 1984" 'patch/system-98-50.qfasl.39'
hcuot "May  1 11:40:21 1984" 'patch/system-98-51.lisp.1'
hcuot "May  1 11:40:31 1984" 'patch/system-98-51.qfasl.1'
hcuot "May 10 09:26:54 1984" 'patch/system-98-52.lisp.1'
hcuot "May 10 09:27:04 1984" 'patch/system-98-52.qfasl.1'
hcuot "May 12 08:33:07 1984" 'patch/system-98-53.lisp.2'
hcuot "May 12 08:33:12 1984" 'patch/system-98-53.qfasl.2'
hcuot "May 23 04:29:09 1984" 'patch/system-98-54.lisp.4'
hcuot "May 23 04:29:30 1984" 'patch/system-98-54.qfasl.4'
hcuot "May 28 07:59:01 1984" 'patch/system-98-55.lisp.3'
hcuot "May 28 07:59:51 1984" 'patch/system-98-55.qfasl.3'
hcuot "May 22 02:26:36 1984" 'patch/system-98-56.lisp.2'
hcuot "May 22 02:26:50 1984" 'patch/system-98-56.qfasl.2'
hcuot "Jun  4 19:45:26 1984" 'patch/system-98-57.lisp.23'
hcuot "Jun  4 20:08:38 1984" 'patch/system-98-57.qfasl.23'
hcuot "May 25 01:26:35 1984" 'patch/system-98-58.lisp.1'
hcuot "May 25 01:26:44 1984" 'patch/system-98-58.qfasl.1'
hcuot "Jun  5 21:11:43 1984" 'patch/system-98-59.lisp.3'
hcuot "Jun  5 21:11:53 1984" 'patch/system-98-59.qfasl.3'
hcuot "Dec 13 22:06:04 1983" 'patch/system-98-6.lisp.17'
hcuot "Jun 13 11:08:07 1984" 'patch/system-98-60.lisp.5'
hcuot "Jun 13 11:08:13 1984" 'patch/system-98-60.qfasl.5'
hcuot "Jun  9 10:10:30 1984" 'patch/system-98-61.lisp.2'
hcuot "Jun  9 10:10:39 1984" 'patch/system-98-61.qfasl.2'
hcuot "Jun 17 10:05:51 1984" 'patch/system-98-62.lisp.12'
hcuot "Jun 20 00:27:54 1984" 'patch/system-98-62.qfasl.12'
hcuot "Jul  2 21:43:53 1984" 'patch/system-98-63.lisp.18'
hcuot "Jul  2 21:44:01 1984" 'patch/system-98-63.qfasl.18'
hcuot "Jun 15 10:19:50 1984" 'patch/system-98-64.lisp.1'
hcuot "Jun 29 09:07:30 1984" 'patch/system-98-64.qfasl.1'
hcuot "Jul  2 21:55:49 1984" 'patch/system-98-65.lisp.10'
hcuot "Jul  2 21:56:02 1984" 'patch/system-98-65.qfasl.10'
hcuot "Jul  9 21:08:09 1984" 'patch/system-98-66.lisp.9'
hcuot "Jul  9 21:08:19 1984" 'patch/system-98-66.qfasl.9'
hcuot "Jun 29 09:22:38 1984" 'patch/system-98-67.lisp.1'
hcuot "Jun 29 09:22:46 1984" 'patch/system-98-67.qfasl.1'
hcuot "Jul 18 18:57:10 1984" 'patch/system-98-68.lisp.5'
hcuot "Jul 18 18:57:21 1984" 'patch/system-98-68.qfasl.5'
hcuot "Aug 14 20:39:14 1984" 'patch/system-98-69.lisp.1'
hcuot "Aug 14 20:39:24 1984" 'patch/system-98-69.qfasl.1'
hcuot "Dec 16 03:40:10 1983" 'patch/system-98-7.lisp.7'
hcuot "Aug 29 15:25:48 1984" 'patch/system-98-70.lisp.1'
hcuot "Aug 29 15:25:55 1984" 'patch/system-98-70.qfasl.1'
hcuot "Oct 13 01:02:56 1984" 'patch/system-98-71.lisp.3'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-71.qfasl.3'
hcuot "Oct 14 21:03:15 1984" 'patch/system-98-72.lisp.2'
hcuot "Oct 14 21:03:36 1984" 'patch/system-98-72.qfasl.2'
hcuot "Oct 11 08:17:25 1984" 'patch/system-98-73.lisp.2'
hcuot "Oct 11 14:50:01 1984" 'patch/system-98-73.qfasl.2'
hcuot "Oct 13 01:03:51 1984" 'patch/system-98-74.lisp.1'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-74.qfasl.1'
hcuot "Oct 14 06:56:53 1984" 'patch/system-98-75.lisp.1'
hcuot "Oct 14 06:57:21 1984" 'patch/system-98-75.qfasl.1'
hcuot "Oct 14 20:58:47 1984" 'patch/system-98-76.lisp.1'
hcuot "Oct 14 20:59:14 1984" 'patch/system-98-76.qfasl.1'
hcuot "Oct 20 18:08:35 1984" 'patch/system-98-77.lisp.1'
hcuot "Nov 12 13:24:48 1984" 'patch/system-98-77.qfasl.1'
hcuot "Nov 12 12:57:12 1984" 'patch/system-98-78.lisp.5'
hcuot "Nov 12 12:57:43 1984" 'patch/system-98-78.qfasl.5'
hcuot "Nov 20 16:35:13 1984" 'patch/system-98-79.lisp.8'
hcuot "Nov 21 00:34:09 1984" 'patch/system-98-79.ncp.3'
hcuot "Nov 20 16:35:23 1984" 'patch/system-98-79.qfasl.8'
hcuot "Nov 20 23:51:27 1984" 'patch/system-98-79-chsncp.lisp.3'
hcuot "Dec 18 01:24:01 1983" 'patch/system-98-8.lisp.12'
hcuot "Nov 26 21:21:29 1984" 'patch/system-98-80.lisp.1'
hcuot "Nov 26 21:21:56 1984" 'patch/system-98-80.qfasl.1'
hcuot "Nov 28 15:58:45 1984" 'patch/system-98-81.lisp.2'
hcuot "Nov 28 15:59:12 1984" 'patch/system-98-81.qfasl.2'
hcuot "Dec 22 17:18:46 1983" 'patch/system-98-9.lisp.9'
hcuot "Oct 14 15:17:53 1984" 'patch/system-98-9.qfasl.9'
hcuot "Sep 10 03:01:34 1985" 'patch/system-99.patch-directory.122'
hcuot "Sep 12 18:29:34 1984" 'patch/system-99-1.lisp.3'
hcuot "Sep 12 18:30:01 1984" 'patch/system-99-1.qfasl.3'
hcuot "Nov  9 21:17:06 1984" 'patch/system-99-10.lisp.31'
hcuot "Nov  9 21:32:38 1984" 'patch/system-99-10.qfasl.31'
hcuot "Nov 14 12:08:03 1984" 'patch/system-99-11.lisp.14'
hcuot "Nov 14 12:08:43 1984" 'patch/system-99-11.qfasl.14'
hcuot "Dec  2 01:02:57 1984" 'patch/system-99-12.lisp.44'
hcuot "Dec  2 01:18:35 1984" 'patch/system-99-12.qfasl.44'
hcuot "Dec  4 19:45:55 1984" 'patch/system-99-13.lisp.17'
hcuot "Dec  4 19:46:23 1984" 'patch/system-99-13.qfasl.17'
hcuot "Dec 14 13:48:14 1984" 'patch/system-99-14.lisp.25'
hcuot "Dec 14 13:48:35 1984" 'patch/system-99-14.qfasl.25'
hcuot "Dec 14 15:09:41 1984" 'patch/system-99-15.lisp.7'
hcuot "Dec 14 15:59:42 1984" 'patch/system-99-15.qfasl.7'
hcuot "Jan 28 06:53:36 1985" 'patch/system-99-16.lisp.3'
hcuot "Jan 28 06:53:43 1985" 'patch/system-99-16.qfasl.3'
hcuot "Feb 12 02:32:11 1985" 'patch/system-99-17.lisp.11'
hcuot "Feb 12 02:32:27 1985" 'patch/system-99-17.qfasl.11'
hcuot "Feb 12 02:41:59 1985" 'patch/system-99-18.lisp.36'
hcuot "Feb 12 02:42:16 1985" 'patch/system-99-18.qfasl.36'
hcuot "Feb 17 09:34:09 1985" 'patch/system-99-19.lisp.26'
hcuot "Feb 17 10:27:41 1985" 'patch/system-99-19.qfasl.26'
hcuot "Sep 12 18:28:00 1984" 'patch/system-99-2.lisp.2'
hcuot "Sep 12 18:28:11 1984" 'patch/system-99-2.qfasl.2'
hcuot "Feb 15 08:05:58 1985" 'patch/system-99-20.lisp.6'
hcuot "Feb 15 08:06:09 1985" 'patch/system-99-20.qfasl.6'
hcuot "Feb 18 16:45:52 1985" 'patch/system-99-21.lisp.17'
hcuot "Feb 27 19:42:42 1985" 'patch/system-99-21.qfasl.17'
hcuot "Feb 28 16:07:50 1985" 'patch/system-99-22.lisp.14'
hcuot "Feb 28 16:08:05 1985" 'patch/system-99-22.qfasl.14'
hcuot "May 17 07:06:11 1985" 'patch/system-99-23.lisp.13'
hcuot "May 17 07:06:19 1985" 'patch/system-99-23.qfasl.13'
hcuot "May 17 06:59:53 1985" 'patch/system-99-24.lisp.7'
hcuot "May 17 07:25:44 1985" 'patch/system-99-24.qfasl.7'
hcuot "May  3 02:50:42 1985" 'patch/system-99-25.lisp.7'
hcuot "May  3 02:51:00 1985" 'patch/system-99-25.qfasl.7'
hcuot "May  3 03:50:29 1985" 'patch/system-99-26.lisp.5'
hcuot "May  3 03:50:39 1985" 'patch/system-99-26.qfasl.5'
hcuot "May 17 07:23:15 1985" 'patch/system-99-27.lisp.4'
hcuot "May 17 07:23:21 1985" 'patch/system-99-27.qfasl.4'
hcuot "May 17 07:05:54 1985" 'patch/system-99-28.lisp.1'
hcuot "Sep 15 02:40:56 1984" 'patch/system-99-3.lisp.5'
hcuot "Sep 15 02:41:07 1984" 'patch/system-99-3.qfasl.5'
hcuot "Sep 10 08:08:44 1985" 'patch/system-99-33.lisp.8'
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.6'
hcuot "Sep 26 15:23:50 1984" 'patch/system-99-4.qfasl.6'
hcuot "Sep 26 19:13:08 1984" 'patch/system-99-5.lisp.10'
hcuot "Sep 26 19:13:19 1984" 'patch/system-99-5.qfasl.10'
hcuot "Sep 29 13:04:03 1984" 'patch/system-99-6.lisp.3'
hcuot "Sep 29 13:04:07 1984" 'patch/system-99-6.qfasl.3'
hcuot "Oct 16 07:25:45 1984" 'patch/system-99-7.lisp.10'
hcuot "Oct 16 07:26:00 1984" 'patch/system-99-7.qfasl.10'
hcuot "Oct 16 13:48:46 1984" 'patch/system-99-8.lisp.9'
hcuot "Oct 16 13:49:03 1984" 'patch/system-99-8.qfasl.9'
hcuot "Oct 23 23:36:34 1984" 'patch/system-99-9.lisp.16'
hcuot "Oct 23 23:37:01 1984" 'patch/system-99-9.qfasl.16'
hcuot "Sep  9 23:46:32 1984" 'patch/zmail.patch-directory.3'
hcuot "Mar 17 02:13:01 1985" 'patch/zmail-54.patch-directory.10'
hcuot "Sep 26 07:22:03 1984" 'patch/zmail-54-1.lisp.2'
hcuot "Sep 26 07:22:09 1984" 'patch/zmail-54-1.qfasl.2'
hcuot "Oct 14 10:12:26 1984" 'patch/zmail-54-2.lisp.1'
hcuot "Oct 14 10:12:30 1984" 'patch/zmail-54-2.qfasl.1'
hcuot "Nov 30 06:36:29 1984" 'patch/zmail-54-3.lisp.1'
hcuot "Nov 30 06:36:35 1984" 'patch/zmail-54-3.qfasl.1'
hcuot "Mar 17 02:13:18 1985" 'patch/zmail-54-4.lisp.3'
hcuot "Mar 17 02:13:25 1985" 'patch/zmail-54-4.qfasl.3'
hcuot "Nov 13 04:40:20 1984" 'site/-read-.-me-.1'
hcuot "Jan 10 19:02:06 1985" 'site/hsttbl.lisp.124'
hcuot "May  3 23:47:46 1985" 'site/hsttbl.qfasl.124'
hcuot "Sep 20 16:57:24 1985" 'site/lmlocs.lisp.162'
hcuot "Jan 25 00:56:53 1985" 'site/lmlocs.qbin.1'
hcuot "May  3 23:47:12 1985" 'site/lmlocs.qfasl.161'
hcuot "Jan 10 18:44:03 1985" 'site/site.lisp.1'
hcuot "May  3 23:46:36 1985" 'site/site.qfasl.1'
hcuot "Jun 29 08:32:33 1982" 'sys/-read-.-this-.1'
hcuot "Dec 11 12:29:53 1984" 'sys/cadrlp.lisp.152'
hcuot "Oct  4 18:13:47 1985" 'sys/cadrlp.qfasl.152'
hcuot "Apr  7 15:44:05 1984" 'sys/cadsym.lisp.25'
hcuot "Jun 16 21:47:26 1984" 'sys/cdmp.lisp.52'
hcuot "Oct  4 18:24:42 1985" 'sys/cdmp.qfasl.52'
hcuot "Feb 28 15:05:47 1985" 'sys/clpack.lisp.153'
hcuot "Sep 10 07:09:12 1984" 'sys/clpack.qfasl.151'
hcuot "Apr  7 15:45:18 1984" 'sys/compat.lisp.32'
hcuot "Jul  1 18:34:34 1985" 'sys/eval.lisp.97'
hcuot "Sep  9 05:39:13 1984" 'sys/eval.qfasl.78'
hcuot "Feb 12 10:14:08 1985" 'sys/fspec.lisp.1'
hcuot "Jan 30 17:13:11 1985" 'sys/genric.lisp.33'
hcuot "Jan 30 17:13:36 1985" 'sys/genric.qfasl.33'
hcuot "Feb 13 15:44:30 1985" 'sys/ltop.lisp.498'
hcuot "Sep 11 07:20:26 1984" 'sys/ltop.qfasl.494'
hcuot "Oct 24 03:41:44 1982" 'sys/ma.lisp.305'
hcuot "Aug  1 22:39:34 1984" 'sys/ma.qfasl.305'
hcuot "Jun 29 08:36:30 1982" 'sys/madefs.lisp.7'
hcuot "Jul 30 03:18:23 1984" 'sys/madefs.qfasl.7'
hcuot "Oct 13 00:33:14 1983" 'sys/maopt.lisp.4'
hcuot "Aug  1 22:44:12 1984" 'sys/maopt.qfasl.4'
hcuot "Nov 16 09:21:33 1983" 'sys/mc.lisp.354'
hcuot "Aug  1 22:45:44 1984" 'sys/mc.qfasl.354'
hcuot "Jan  4 00:08:41 1983" 'sys/mlap.lisp.51'
hcuot "Aug  1 22:48:15 1984" 'sys/mlap.qfasl.51'
hcuot "Jun 25 23:40:50 1983" 'sys/pack4.lisp.286'
hcuot "Jan 30 12:11:13 1985" 'sys/qcdefs.lisp.153'
hcuot "Sep  9 20:00:57 1984" 'sys/qcdefs.qfasl.149'
hcuot "Sep 10 23:05:15 1984" 'sys/qcfasd.lisp.248'
hcuot "Sep 10 23:05:43 1984" 'sys/qcfasd.qfasl.248'
hcuot "Jan 30 16:48:52 1985" 'sys/qcfile.lisp.324'
hcuot "Sep  7 02:12:10 1984" 'sys/qcfile.qfasl.322'
hcuot "Sep  8 23:30:16 1984" 'sys/qclap.lisp.244'
hcuot "Sep  9 20:16:55 1984" 'sys/qclap.qfasl.244'
hcuot "Aug 30 13:51:41 1984" 'sys/qcluke.lisp.26'
hcuot "Aug 30 20:38:31 1984" 'sys/qcluke.qfasl.26'
hcuot "Nov  6 13:41:16 1984" 'sys/qcopt.lisp.137'
hcuot "Sep  9 20:13:24 1984" 'sys/qcopt.qfasl.133'
hcuot "Dec 11 19:38:25 1984" 'sys/qcp1.lisp.573'
hcuot "Sep  9 20:03:10 1984" 'sys/qcp1.qfasl.562'
hcuot "Oct 28 21:41:46 1984" 'sys/qcp2.lisp.261'
hcuot "Sep  9 20:09:07 1984" 'sys/qcp2.qfasl.259'
hcuot "Aug  3 03:31:47 1984" 'sys/qcpeep.lisp.36'
hcuot "Aug  3 03:31:56 1984" 'sys/qcpeep.qfasl.36'
hcuot "Jan 11 21:27:40 1984" 'sys/qev.lisp.289'
hcuot "Feb 26 10:27:46 1985" 'sys/qfasl.lisp.463'
hcuot "Aug 15 05:35:35 1984" 'sys/qfasl.qfasl.461'
hcuot "Nov 17 01:51:17 1984" 'sys/qfctns.lisp.774'
hcuot "Aug 31 17:59:24 1984" 'sys/qfctns.qfasl.769'
hcuot "Dec 14 08:04:10 1984" 'sys/qmisc.lisp.659'
hcuot "Aug 31 22:20:48 1984" 'sys/qmisc.qfasl.652'
hcuot "Apr  3 15:55:26 1984" 'sys/qnew.lisp.20'
hcuot "Aug 15 05:54:18 1984" 'sys/qnew.qfasl.20'
hcuot "Jul  9 20:51:08 1985" 'sys/qrand.lisp.412'
hcuot "Sep  4 23:45:01 1984" 'sys/qrand.qfasl.408'
hcuot "Dec 11 14:56:51 1984" 'sys/qwmcr.lisp.22'
hcuot "Dec 11 14:56:58 1984" 'sys/qwmcr.qfasl.22'
hcuot "Jun 18 08:57:37 1983" 'sys/recom.lisp.33'
hcuot "Aug 27 08:28:30 1982" 'sys/sgfctn.lisp.57'
hcuot "Aug 15 06:13:05 1984" 'sys/sgfctn.qfasl.57'
hcuot "Oct 10 07:20:38 1983" 'sys/sort.lisp.59'
hcuot "Aug 15 06:13:45 1984" 'sys/sort.qfasl.59'
hcuot "May  3 03:12:41 1985" 'sys/sysdcl.lisp.193'
hcuot "Oct  4 17:41:35 1985" 'sys/sysdcl.qfasl.193'
hcuot "Jan 29 02:23:45 1985" 'sys/types.lisp.72'
hcuot "Sep  9 03:39:45 1984" 'sys/types.qfasl.69'
hcuot "Jun 29 08:49:58 1982" 'sys/ucinit.qfasl.1'
hcuot "Feb 18 08:47:48 1985" 'sys2/advise.lisp.38'
hcuot "Aug 15 03:43:06 1984" 'sys2/advise.qfasl.37'
hcuot "Feb 24 11:42:25 1985" 'sys2/analyze.lisp.19'
hcuot "Sep 11 07:03:49 1984" 'sys2/analyze.qfasl.17'
hcuot "Jul 27 08:09:35 1984" 'sys2/band.lisp.44'
hcuot "Nov 24 08:31:51 1984" 'sys2/band.qfasl.46'
hcuot "Feb  4 07:18:04 1985" 'sys2/character.lisp.22'
hcuot "Sep  7 22:04:06 1984" 'sys2/character.qfasl.20'
hcuot "Jun 15 05:56:23 1984" 'sys2/class.lisp.99'
hcuot "Sep  4 21:31:45 1984" 'sys2/class.qfasl.99'
hcuot "Aug 24 10:12:36 1984" 'sys2/clmac.lisp.4'
hcuot "Aug 29 03:55:00 1984" 'sys2/clmac.qfasl.4'
hcuot "Apr  7 15:50:06 1984" 'sys2/cmany.lisp.46'
hcuot "Apr  7 15:50:53 1984" 'sys2/condit.lisp.2'
hcuot "Feb 13 16:25:14 1985" 'sys2/defmac.lisp.80'
hcuot "Aug 29 21:55:29 1984" 'sys2/defmac.qfasl.78'
hcuot "Aug 29 02:45:38 1984" 'sys2/defsel.lisp.70'
hcuot "Aug 29 07:26:03 1984" 'sys2/defsel.qfasl.70'
hcuot "Feb 15 07:49:03 1985" 'sys2/describe.lisp.3'
hcuot "Dec 14 13:05:57 1984" 'sys2/disass.lisp.94'
hcuot "Aug  1 22:38:35 1984" 'sys2/disass.qfasl.92'
hcuot "Nov 28 15:50:50 1984" 'sys2/encaps.lisp.28'
hcuot "Aug 15 04:10:54 1984" 'sys2/encaps.qfasl.27'
hcuot "Feb 11 06:03:31 1985" 'sys2/flavor.lisp.283'
hcuot "Sep 11 05:12:37 1984" 'sys2/flavor.qfasl.280'
hcuot "Dec 14 09:01:18 1984" 'sys2/gc.lisp.174'
hcuot "Aug 15 04:34:41 1984" 'sys2/gc.qfasl.169'
hcuot "Mar  2 14:03:29 1985" 'sys2/hash.lisp.89'
hcuot "Aug 15 04:51:05 1984" 'sys2/hash.qfasl.87'
hcuot "Mar  6 06:23:07 1985" 'sys2/hashfl.lisp.33'
hcuot "Aug 15 04:52:39 1984" 'sys2/hashfl.qfasl.29'
hcuot "Apr  7 15:51:47 1984" 'sys2/let.lisp.8'
hcuot "May  3 03:13:45 1985" 'sys2/lmmac.lisp.389'
hcuot "Aug 31 21:54:02 1984" 'sys2/lmmac.qfasl.372'
hcuot "Sep  4 02:04:30 1984" 'sys2/login.lisp.87'
hcuot "Sep  4 03:06:25 1984" 'sys2/login.qfasl.87'
hcuot "Dec  9 06:37:37 1984" 'sys2/loop.lisp.829'
hcuot "Oct 24 08:03:31 1984" 'sys2/loop.qfasl.799'
hcuot "May  3 03:54:59 1985" 'sys2/macarr.lisp.2'
hcuot "May  3 03:55:13 1985" 'sys2/macarr.qfasl.2'
hcuot "Sep 13 22:53:32 1984" 'sys2/maksys.lisp.180'
hcuot "Sep  4 22:16:36 1984" 'sys2/maksys.qfasl.178'
hcuot "Apr  9 18:08:41 1984" 'sys2/matrix.lisp.26'
hcuot "Aug 30 03:20:25 1984" 'sys2/matrix.qfasl.26'
hcuot "Sep  4 21:35:26 1984" 'sys2/meth.lisp.63'
hcuot "Sep  4 21:35:37 1984" 'sys2/meth.qfasl.63'
hcuot "Oct  6 11:43:45 1984" 'sys2/numdef.lisp.12'
hcuot "Sep 10 22:30:29 1984" 'sys2/numdef.qfasl.11'
hcuot "Dec 14 08:58:42 1984" 'sys2/numer.lisp.62'
hcuot "Sep 10 22:32:20 1984" 'sys2/numer.qfasl.60'
hcuot "May  2 01:02:09 1986" 'sys2/patch.lisp.166'
hcuot "Aug 15 05:22:21 1984" 'sys2/patch.qfasl.158'
hcuot "Aug 30 00:33:36 1984" 'sys2/plane.lisp.32'
hcuot "Aug 30 00:45:45 1984" 'sys2/plane.qfasl.32'
hcuot "Feb 13 15:50:42 1985" 'sys2/proces.lisp.159'
hcuot "Aug 15 05:28:32 1984" 'sys2/proces.qfasl.157'
hcuot "Feb 13 15:59:15 1985" 'sys2/prodef.lisp.49'
hcuot "Aug 31 19:59:54 1984" 'sys2/prodef.qfasl.48'
hcuot "Dec  6 01:41:34 1984" 'sys2/qtrace.lisp.152'
hcuot "Sep  4 21:43:34 1984" 'sys2/qtrace.qfasl.151'
hcuot "Sep  4 22:08:45 1984" 'sys2/rat.lisp.46'
hcuot "Sep 10 22:47:13 1984" 'sys2/rat.qfasl.46'
hcuot "Nov 10 09:26:41 1984" 'sys2/resour.lisp.31'
hcuot "Aug 15 06:08:50 1984" 'sys2/resour.qfasl.28'
hcuot "Feb 13 14:19:42 1985" 'sys2/selev.lisp.24'
hcuot "Aug 29 03:54:21 1984" 'sys2/selev.qfasl.23'
hcuot "Oct 29 09:56:33 1984" 'sys2/setf.lisp.97'
hcuot "Aug 31 18:48:05 1984" 'sys2/setf.qfasl.95'
hcuot "Feb  7 06:26:28 1985" 'sys2/sgdefs.lisp.57'
hcuot "Aug 15 03:40:43 1984" 'sys2/sgdefs.qfasl.54'
hcuot "Sep 26 08:01:17 1984" 'sys2/step.lisp.72'
hcuot "Aug 15 06:15:30 1984" 'sys2/step.qfasl.70'
hcuot "Sep 25 07:20:50 1984" 'sys2/string.lisp.147'
hcuot "Sep 10 07:15:39 1984" 'sys2/string.qfasl.146'
hcuot "Jul 31 23:42:25 1984" 'sys2/struct.lisp.322'
hcuot "Aug 14 22:20:04 1984" 'sys2/struct.qfasl.322'
hcuot "Oct  9 13:52:31 1984" 'sys2/unfasl.lisp.19'
hcuot "Sep 11 07:52:22 1984" 'sys2/unfasl.qfasl.18'
hcuot "Oct 22 20:07:22 1985" 'sys2/usymld.lisp.188'
hcuot "Oct 22 20:07:43 1985" 'sys2/usymld.qfasl.188'
hcuot "Feb 16 13:56:19 1984" 'tape/copy.lisp.133'
hcuot "Jan  3 09:50:47 1984" 'tape/copy.qfasl.128'
hcuot "May 12 05:49:07 1984" 'tape/ddoc.text.8'
hcuot "May 12 05:29:43 1984" 'tape/fdump.lisp.27'
hcuot "May 12 05:52:14 1984" 'tape/fdump-def.lisp.12'
hcuot "Jan  2 09:37:10 1984" 'tape/fdump-def.qfasl.1'
hcuot "Jan 10 04:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'
hcuot "Jan 19 16:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'
hcuot "May 12 05:29:45 1984" 'tape/fdump-r.lisp.5'
hcuot "Jan  3 10:36:11 1984" 'tape/magtape.directory.11'
hcuot "Oct 26 20:41:54 1983" 'tape/magtape-14.directory.14'
hcuot "Mar  8 06:56:47 1983" 'tape/magtape-14-1.qfasl.1'
hcuot "Apr 25 09:51:48 1983" 'tape/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:34 1983" 'tape/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:16 1983" 'tape/magtape-14-5.qfasl.1'
hcuot "Feb 16 14:24:04 1984" 'tape/magtape-22.directory.13'
hcuot "Jan  7 22:40:45 1984" 'tape/magtape-22-1.lisp.1'
hcuot "Jan  7 22:40:56 1984" 'tape/magtape-22-1.qfasl.1'
hcuot "Jan  7 23:28:27 1984" 'tape/magtape-22-2.lisp.1'
hcuot "Jan  7 23:28:40 1984" 'tape/magtape-22-2.qfasl.1'
hcuot "Jan  8 00:41:18 1984" 'tape/magtape-22-3.lisp.1'
hcuot "Jan  8 00:41:44 1984" 'tape/magtape-22-3.qfasl.1'
hcuot "Jan 13 13:06:26 1984" 'tape/magtape-22-4.lisp.1'
hcuot "Jan 13 13:06:35 1984" 'tape/magtape-22-4.qfasl.1'
hcuot "Jan 19 17:40:22 1984" 'tape/magtape-22-5.lisp.1'
hcuot "Jan 19 17:40:32 1984" 'tape/magtape-22-5.qfasl.1'
hcuot "Feb 16 14:23:22 1984" 'tape/magtape-22-6.lisp.1'
hcuot "Feb 16 14:23:28 1984" 'tape/magtape-22-6.qfasl.1'
hcuot "Jan 19 17:04:02 1984" 'tape/mtaux.lisp.80'
hcuot "Jan  3 09:52:48 1984" 'tape/mtaux.qfasl.77'
hcuot "Dec 16 15:34:10 1983" 'tape/mtdefs.lisp.30'
hcuot "Jan  3 09:46:18 1984" 'tape/mtdefs.qfasl.30'
hcuot "Jan 11 05:40:52 1984" 'tape/mtstr.lisp.87'
hcuot "Jan  3 09:47:58 1984" 'tape/mtstr.qfasl.85'
hcuot "Jan  3 08:50:55 1984" 'tape/odump.lisp.1'
hcuot "Jan  3 10:33:05 1984" 'tape/odump.qfasl.1'
hcuot "May 12 05:29:46 1984" 'tape/package.lisp.1'
hcuot "Jan  3 07:59:49 1984" 'tape/pdp10.lisp.1'
hcuot "May 12 08:31:18 1984" 'tape/rmunit.lisp.3'
hcuot "May 12 05:29:46 1984" 'tape/system.lisp.3'
hcuot "May 12 05:29:47 1984" 'tape/tm.lisp.25'
hcuot "May 12 05:29:48 1984" 'tape/tmdefs.lisp.7'
hcuot "May 12 07:27:24 1984" 'tape/unit.lisp.7'
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.1'
hcuot "May 12 07:28:11 1984" 'tape/new/mtdefs.lisp.4'
hcuot "May 12 07:45:03 1984" 'tape/new/mtdefs.qfasl.4'
hcuot "May 12 05:29:49 1984" 'tape/new/mtrqb.lisp.3'
hcuot "May 12 08:31:35 1984" 'tape/new/mtstr.lisp.5'
hcuot "May 12 05:29:50 1984" 'tape/new/tmunit.lisp.5'
hcuot "May 12 05:29:51 1984" 'tape/new/weunit.lisp.3'
hcuot "Nov 20 23:29:49 1982" 'ubin/dcfu.uload.4'
hcuot "Aug  4 07:23:05 1982" 'ubin/memd.uload.1'
hcuot "Feb  7 05:35:36 1986" 'ubin/ucadr.loc.322'
hcuot "Sep 11 21:24:11 1984" 'ubin/ucadr.locs.320'
hcuot "Feb  7 05:33:22 1986" 'ubin/ucadr.mcr.322'
hcuot "Feb  7 05:33:48 1986" 'ubin/ucadr.sym.322'
hcuot "Feb  7 05:35:38 1986" 'ubin/ucadr.tbl.322'
hcuot "Apr  9 11:19:01 1983" 'ucadr/cadldb.lisp.20'
hcuot "Jul 26 10:31:51 1983" 'ucadr/cadldb.qfasl.20'
hcuot "Jun 29 10:56:11 1982" 'ucadr/cadtlk.mid.9'
hcuot "Jun 29 10:56:32 1982" 'ucadr/chaos.test.1'
hcuot "Jun 29 10:56:46 1982" 'ucadr/dcfu.text.23'
hcuot "Dec 22 06:46:28 1982" 'ucadr/dcfu.uload.3'
hcuot "Jun 29 10:59:34 1982" 'ucadr/memd.lisp.26'
hcuot "Jun 29 10:59:39 1982" 'ucadr/mmtest.lisp.15'
hcuot "Jun 29 10:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1'
hcuot "Oct 14 23:41:23 1983" 'ucadr/packed.lisp.124'
hcuot "Jun 29 11:00:13 1982" 'ucadr/praid.lisp.21'
hcuot "Jun 29 11:00:18 1982" 'ucadr/promh.text.9'
hcuot "Oct  6 10:49:21 1984" 'ucadr/uc-arith.lisp.34'
hcuot "Jun 17 01:36:02 1984" 'ucadr/uc-array.lisp.63'
hcuot "Mar 31 23:16:21 1983" 'ucadr/uc-array-cache.lisp.1'
hcuot "Jun  2 03:53:36 1984" 'ucadr/uc-cadr.lisp.8'
hcuot "Feb 12 05:46:46 1985" 'ucadr/uc-call-return.lisp.109'
hcuot "Oct 11 07:19:04 1982" 'ucadr/uc-chaos.lisp.1'
hcuot "Nov 14 10:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'
hcuot "Nov 14 08:21:19 1983" 'ucadr/uc-disk.lisp.2'
hcuot "Jun 13 02:12:27 1985" 'ucadr/uc-fctns.lisp.85'
hcuot "Oct 17 16:11:57 1983" 'ucadr/uc-hacks.lisp.5'
hcuot "Dec 10 09:51:17 1984" 'ucadr/uc-interrupt.lisp.9'
hcuot "Mar  3 04:56:48 1984" 'ucadr/uc-logical.lisp.8'
hcuot "Jul  2 11:39:55 1984" 'ucadr/uc-macrocode.lisp.29'
hcuot "Nov 14 02:47:25 1983" 'ucadr/uc-mc.lisp.2'
hcuot "Aug  1 09:39:57 1983" 'ucadr/uc-meter.lisp.5'
hcuot "Nov 21 09:24:14 1983" 'ucadr/uc-page-fault.lisp.13'
hcuot "Dec 10 08:16:18 1984" 'ucadr/uc-parameters.lisp.230'
hcuot "Oct 11 07:18:51 1982" 'ucadr/uc-pup.lisp.1'
hcuot "Feb 12 05:47:05 1985" 'ucadr/uc-stack-closure.lisp.12'
hcuot "Jul 23 11:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'
hcuot "May 19 04:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'
hcuot "Sep  6 20:03:25 1984" 'ucadr/uc-string.lisp.26'
hcuot "Apr  3 12:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'
hcuot "May  1 09:20:21 1984" 'ucadr/uc-transporter.lisp.23'
hcuot "Apr  5 03:29:11 1984" 'ucadr/uc-tv.lisp.5'
hcuot "Jun 29 11:00:58 1982" 'ucadr/ucadlr.text.746'
hcuot "Nov 14 01:30:39 1983" 'ucadr/ucode.lisp.19'
hcuot "Aug 25 05:24:02 1982" 'wind/baswin.text.7'
hcuot "Aug  9 02:24:49 1983" 'wind/blink.text.21'
hcuot "Jan 22 09:23:05 1984" 'wind/choice.text.95'
hcuot "Jul 23 04:52:44 1983" 'wind/edges.text.14'
hcuot "Aug 25 05:25:39 1982" 'wind/emack.fasl.1'
hcuot "Apr  7 15:53:58 1984" 'wind/emack.lisp.37'
hcuot "Jul  6 04:46:40 1983" 'wind/fonts.text.17'
hcuot "Aug  9 02:21:25 1983" 'wind/frames.text.14'
hcuot "Aug  8 05:44:03 1983" 'wind/grafix.text.24'
hcuot "Sep 30 08:33:21 1983" 'wind/input.text.26'
hcuot "Apr  7 15:55:43 1984" 'wind/lstfla.lisp.6'
hcuot "Aug  8 06:15:43 1983" 'wind/margin.text.20'
hcuot "Aug 23 15:42:25 1983" 'wind/misc.text.24'
hcuot "Aug  8 11:24:58 1983" 'wind/mouse.text.33'
hcuot "Aug 25 05:27:06 1982" 'wind/operat.bolio.1'
hcuot "Aug 25 05:26:46 1982" 'wind/operat.text.45'
hcuot "Aug 25 05:27:12 1982" 'wind/outlin.text.2'
hcuot "Oct 29 02:54:29 1983" 'wind/output.text.28'
hcuot "Nov 19 01:30:08 1983" 'wind/select.text.22'
hcuot "Aug  8 10:42:49 1983" 'wind/tscrol.text.37'
hcuot "Jul  6 05:25:11 1983" 'wind/typout.text.17'
hcuot "Feb  4 11:57:38 1984" 'wind/windo1.text.52'
hcuot "Jul  3 06:44:16 1983" 'wind/windoc.bolio.14'
hcuot "Jun 21 06:34:00 1983" 'wind/windoc.dict.1'
hcuot "Aug  9 03:12:27 1983" 'wind/windoc.log.12'
hcuot "Aug  9 03:10:00 1983" 'wind/windoc.text.15'
hcuot "Aug  9 03:23:20 1983" 'wind/windoc.vars.33'
hcuot "Aug 25 05:30:20 1982" 'wind/window.gloss.1'
hcuot "Aug 25 05:30:32 1982" 'wind/window.manual.1'
hcuot "Aug 25 05:30:42 1982" 'wind/window.methds.1'
hcuot "Aug 25 05:30:47 1982" 'wind/winman.text.1'
hcuot "Dec  7 11:20:04 1984" 'window/basstr.lisp.373'
hcuot "Sep  7 19:42:22 1984" 'window/basstr.qfasl.372'
hcuot "Sep  6 21:24:14 1984" 'window/baswin.lisp.562'
hcuot "Sep  7 19:37:06 1984" 'window/baswin.qfasl.562'
hcuot "Aug  5 02:39:30 1984" 'window/choice.lisp.116'
hcuot "Sep  7 23:32:54 1984" 'window/choice.qfasl.116'
hcuot "Aug 29 00:50:25 1984" 'window/cold.lisp.129'
hcuot "Aug 29 02:53:22 1984" 'window/cold.qfasl.129'
hcuot "Oct 14 20:56:06 1984" 'window/color.lisp.69'
hcuot "Aug 30 00:54:42 1984" 'window/color.qfasl.67'
hcuot "Aug  4 12:29:09 1983" 'window/cometh.lisp.26'
hcuot "Aug  3 22:17:13 1984" 'window/cometh.qfasl.26'
hcuot "Feb  3 01:32:16 1985" 'window/csrpos.lisp.10'
hcuot "Aug  3 22:53:14 1984" 'window/csrpos.qfasl.9'
hcuot "Dec 14 00:47:42 1984" 'window/fed.lisp.200'
hcuot "Sep  8 00:51:09 1984" 'window/fed.qfasl.199'
hcuot "Apr 11 03:28:15 1984" 'window/frame.lisp.165'
hcuot "Sep  8 00:59:41 1984" 'window/frame.qfasl.165'
hcuot "Jun  4 03:03:15 1984" 'window/graphics.lisp.1'
hcuot "Aug  3 22:06:31 1984" 'window/graphics.qfasl.1'
hcuot "Jan 30 16:30:23 1985" 'window/inspct.lisp.159'
hcuot "Sep  7 23:26:22 1984" 'window/inspct.qfasl.154'
hcuot "Oct 20 19:40:52 1984" 'window/menu.lisp.105'
hcuot "Sep  7 18:43:14 1984" 'window/menu.qfasl.104'
hcuot "Oct 11 04:37:53 1984" 'window/mouse.lisp.248'
hcuot "Aug  3 12:23:18 1984" 'window/mouse.qfasl.247'
hcuot "Sep  7 18:30:52 1984" 'window/peek.lisp.153'
hcuot "Sep  7 18:50:42 1984" 'window/peek.qfasl.153'
hcuot "May 25 02:14:17 1984" 'window/peekch.lisp.27'
hcuot "Sep  7 18:30:42 1984" 'window/peekfs.lisp.10'
hcuot "Sep  7 19:08:30 1984" 'window/peekfs.qfasl.10'
hcuot "Apr  7 15:56:29 1984" 'window/quest.lisp.43'
hcuot "Sep 11 21:05:34 1984" 'window/rh.lisp.162'
hcuot "Sep 11 21:34:52 1984" 'window/rh.qfasl.162'
hcuot "Oct  9 11:48:09 1984" 'window/scred.lisp.112'
hcuot "Aug  3 22:27:31 1984" 'window/scred.qfasl.111'
hcuot "Dec 14 06:43:19 1984" 'window/scrman.lisp.166'
hcuot "Aug  3 11:54:11 1984" 'window/scrman.qfasl.165'
hcuot "Aug  5 02:39:39 1984" 'window/scroll.lisp.176'
hcuot "Aug  5 02:45:11 1984" 'window/scroll.qfasl.176'
hcuot "Dec  7 06:53:21 1984" 'window/sheet.lisp.558'
hcuot "Aug  3 11:56:23 1984" 'window/sheet.qfasl.557'
hcuot "Mar 12 04:08:36 1985" 'window/shwarm.lisp.334'
hcuot "Sep  7 19:31:11 1984" 'window/shwarm.qfasl.328'
hcuot "Sep  8 19:06:44 1984" 'window/stream.lisp.145'
hcuot "Sep  9 05:51:21 1984" 'window/stream.qfasl.145'
hcuot "Jul  5 03:33:56 1984" 'window/supdup.lisp.276'
hcuot "Aug  3 23:14:36 1984" 'window/supdup.qfasl.276'
hcuot "Oct 11 08:55:04 1984" 'window/sysmen.lisp.178'
hcuot "Aug  3 22:22:16 1984" 'window/sysmen.qfasl.177'
hcuot "Jun 29 09:51:17 1982" 'window/task.list.1'
hcuot "Sep  6 05:00:41 1984" 'window/telnet-code.lisp.6'
hcuot "Sep  1 06:28:56 1984" 'window/telnet-front-hack.lisp.1'
hcuot "Nov  5 18:22:00 1985" 'window/tscrol.lisp.75'
hcuot "Jul 30 02:54:26 1984" 'window/tscrol.qfasl.72'
hcuot "Jan 28 12:05:54 1985" 'window/tvdefs.lisp.286'
hcuot "Aug 29 09:10:22 1984" 'window/tvdefs.qfasl.284'
hcuot "May  1 23:22:28 1984" 'window/typwin.lisp.118'
hcuot "Sep  7 23:40:15 1984" 'window/typwin.qfasl.118'
hcuot "Dec 11 08:01:17 1984" 'window/wholin.lisp.92'
hcuot "Sep  4 21:02:13 1984" 'window/wholin.qfasl.90'
hcuot "Dec  9 23:26:17 1983" 'window/winddoc.lisp.2'
hcuot "May 15 01:09:03 1986" 'zmail/bug.zmail.1'
hcuot "Apr 13 01:45:40 1985" 'zmail/bug.zmail1.1'
hcuot "Jul 13 07:17:38 1984" 'zmail/button.lisp.24'
hcuot "Sep  9 20:58:54 1984" 'zmail/button.qfasl.24'
hcuot "Apr  7 15:57:16 1984" 'zmail/cometh.lisp.51'
hcuot "Sep  9 21:11:39 1984" 'zmail/cometh.qfasl.51'
hcuot "Oct 14 10:23:33 1984" 'zmail/comnds.lisp.583'
hcuot "Sep 10 07:59:56 1984" 'zmail/comnds.qfasl.581'
hcuot "Aug 17 20:36:10 1983" 'zmail/defs.lisp.268'
hcuot "Mar 17 02:11:04 1985" 'zmail/defs.lisp.274'
hcuot "Sep  9 19:19:35 1984" 'zmail/defs.qfasl.273'
hcuot "Sep 25 07:29:51 1984" 'zmail/filter.lisp.356'
hcuot "Sep 10 08:16:57 1984" 'zmail/filter.qfasl.355'
hcuot "Jun 29 11:22:50 1982" 'zmail/info.mail.1'
hcuot "Apr 30 15:49:02 1984" 'zmail/lex733.lisp.14'
hcuot "Sep 10 05:59:29 1984" 'zmail/lex733.qfasl.1'
hcuot "Apr  7 15:57:50 1984" 'zmail/lm.lisp.4'
hcuot "Apr  7 15:58:15 1984" 'zmail/lmcsrv.lisp.5'
hcuot "Jul 13 07:23:12 1984" 'zmail/lmfile.lisp.5'
hcuot "Sep  9 20:30:09 1984" 'zmail/lmfile.qfasl.5'
hcuot "Mar 17 02:10:55 1985" 'zmail/mail.lisp.312'
hcuot "Sep 10 08:07:59 1984" 'zmail/mail.qfasl.311'
hcuot "Nov 30 06:35:32 1984" 'zmail/mfhost.lisp.59'
hcuot "Sep  9 20:25:31 1984" 'zmail/mfhost.qfasl.58'
hcuot "Sep  9 23:58:00 1984" 'zmail/mfiles.lisp.324'
hcuot "Sep 10 07:49:43 1984" 'zmail/mfiles.qfasl.324'
hcuot "Jul 13 07:19:28 1984" 'zmail/mult.lisp.25'
hcuot "Sep  9 20:57:38 1984" 'zmail/mult.qfasl.25'
hcuot "Dec 10 23:37:49 1983" 'zmail/parse.lisp.52'
hcuot "Nov 15 11:02:07 1983" 'zmail/patch.directory.13'
hcuot "Aug 23 00:25:50 1983" 'zmail/patch-51-1.lisp.1'
hcuot "Sep  7 21:56:01 1983" 'zmail/patch-51-2.lisp.1'
hcuot "Sep 21 23:30:38 1983" 'zmail/patch-51-3.lisp.6'
hcuot "Sep 21 23:26:54 1983" 'zmail/patch-51-4.lisp.2'
hcuot "Sep 23 08:11:23 1983" 'zmail/patch-51-5.lisp.2'
hcuot "Sep 26 05:52:32 1983" 'zmail/patch-51-6.lisp.1'
hcuot "Oct 14 07:56:33 1983" 'zmail/patch-51-7.lisp.1'
hcuot "Oct 22 08:30:39 1983" 'zmail/patch-51-8.lisp.1'
hcuot "Oct 28 07:02:36 1983" 'zmail/patch-51-9.lisp.1'
hcuot "Mar 24 10:39:39 1985" 'zmail/patch-53.directory.55'
hcuot "Dec  7 12:43:52 1983" 'zmail/patch-53-1.qfasl.2'
hcuot "Jan 30 06:21:26 1984" 'zmail/patch-53-10.lisp.1'
hcuot "Jan 30 06:21:32 1984" 'zmail/patch-53-10.qfasl.1'
hcuot "Feb 16 07:57:45 1984" 'zmail/patch-53-11.lisp.2'
hcuot "Feb 16 07:57:48 1984" 'zmail/patch-53-11.qfasl.2'
hcuot "Feb 23 13:40:40 1984" 'zmail/patch-53-12.lisp.2'
hcuot "Feb 23 13:40:45 1984" 'zmail/patch-53-12.qfasl.2'
hcuot "Mar  4 08:41:33 1984" 'zmail/patch-53-13.lisp.1'
hcuot "Mar  4 08:41:37 1984" 'zmail/patch-53-13.qfasl.1'
hcuot "Mar 24 17:24:31 1984" 'zmail/patch-53-14.lisp.2'
hcuot "Mar 24 17:24:35 1984" 'zmail/patch-53-14.qfasl.2'
hcuot "Apr 11 07:05:23 1984" 'zmail/patch-53-15.lisp.3'
hcuot "Apr 11 07:05:32 1984" 'zmail/patch-53-15.qfasl.3'
hcuot "Apr 18 09:41:32 1984" 'zmail/patch-53-16.lisp.1'
hcuot "Apr 18 09:41:38 1984" 'zmail/patch-53-16.qfasl.1'
hcuot "Apr 22 00:46:53 1984" 'zmail/patch-53-17.lisp.2'
hcuot "Apr 22 00:47:01 1984" 'zmail/patch-53-17.qfasl.2'
hcuot "Jun 29 04:21:13 1984" 'zmail/patch-53-18.lisp.1'
hcuot "Jun 29 08:53:32 1984" 'zmail/patch-53-18.qfasl.1'
hcuot "Oct 14 10:57:28 1984" 'zmail/patch-53-19.lisp.1'
hcuot "Oct 14 10:57:55 1984" 'zmail/patch-53-19.qfasl.1'
hcuot "Dec  6 05:18:26 1983" 'zmail/patch-53-2.lisp.1'
hcuot "Dec  6 05:18:36 1983" 'zmail/patch-53-2.qfasl.1'
hcuot "Mar 18 19:53:14 1985" 'zmail/patch-53-20.lisp.1'
hcuot "Mar 18 19:53:32 1985" 'zmail/patch-53-20.qfasl.1'
hcuot "Mar 24 10:39:04 1985" 'zmail/patch-53-21.lisp.1'
hcuot "Mar 24 10:39:07 1985" 'zmail/patch-53-21.qfasl.1'
hcuot "Dec 13 06:15:17 1983" 'zmail/patch-53-3.lisp.2'
hcuot "Dec 13 06:15:23 1983" 'zmail/patch-53-3.qfasl.2'
hcuot "Dec 14 08:54:56 1983" 'zmail/patch-53-5.lisp.1'
hcuot "Dec 14 08:55:02 1983" 'zmail/patch-53-5.qfasl.1'
hcuot "Jan  3 18:55:45 1984" 'zmail/patch-53-6.lisp.2'
hcuot "Jan  3 18:55:54 1984" 'zmail/patch-53-6.qfasl.2'
hcuot "Jan  1 01:08:53 1984" 'zmail/patch-53-7.lisp.3'
hcuot "Jan  1 01:09:00 1984" 'zmail/patch-53-7.qfasl.3'
hcuot "Jan  1 15:59:26 1984" 'zmail/patch-53-8.lisp.3'
hcuot "Jan  1 15:59:30 1984" 'zmail/patch-53-8.qfasl.3'
hcuot "Jan  1 16:00:18 1984" 'zmail/patch-53-9.lisp.2'
hcuot "Jan  1 16:00:22 1984" 'zmail/patch-53-9.qfasl.2'
hcuot "Jun 29 11:28:11 1982" 'zmail/poop.text.35'
hcuot "Sep 11 06:21:26 1984" 'zmail/profil.lisp.124'
hcuot "Sep 11 06:21:59 1984" 'zmail/profil.qfasl.124'
hcuot "Jul 13 07:22:56 1984" 'zmail/refer.lisp.7'
hcuot "Sep  9 20:29:01 1984" 'zmail/refer.qfasl.7'
hcuot "Jul 13 07:16:29 1984" 'zmail/rfc733.lisp.57'
hcuot "Sep  9 21:03:17 1984" 'zmail/rfc733.qfasl.57'
hcuot "Sep 26 12:37:36 1984" 'zmail/top.lisp.555'
hcuot "Sep 10 07:45:42 1984" 'zmail/top.qfasl.554'
hcuot "Sep  9 23:58:28 1984" 'zmail/window.lisp.343'
hcuot "Sep 10 08:13:55 1984" 'zmail/window.qfasl.343'
hcuot "Dec 14 06:00:40 1984" 'zmail/manual/manual.text.1'
hcuot "Jun  8 09:14:17 1983" 'zmail/manual/top.txt.1'
hcuot "Jun 29 11:04:18 1982" 'zwei/.comnd.text.1'
hcuot "Jun 29 11:04:27 1982" 'zwei/atsign.xfile.1'
hcuot "Sep 10 02:58:25 1985" 'zwei/bdired.lisp.42'
hcuot "Aug  5 04:08:03 1984" 'zwei/bdired.qfasl.41'
hcuot "Jan 27 19:35:00 1983" 'zwei/bug.bugs7.1'
hcuot "May 19 08:22:50 1986" 'zwei/bug.zwei.1'
hcuot "Oct  8 10:11:11 1983" 'zwei/bug-zwei.text.1'
hcuot "Jun 29 11:04:29 1982" 'zwei/bugs.bugs.1'
hcuot "Jun 29 11:05:20 1982" 'zwei/bugs.bugs6.1'
hcuot "Jun 29 11:05:52 1982" 'zwei/bugs.status.1'
hcuot "Sep 10 03:09:08 1985" 'zwei/coma.lisp.106'
hcuot "Aug  4 00:18:12 1984" 'zwei/coma.qfasl.102'
hcuot "Sep 10 03:19:04 1985" 'zwei/comb.lisp.96'
hcuot "Aug  4 00:20:56 1984" 'zwei/comb.qfasl.94'
hcuot "Sep 10 03:41:03 1985" 'zwei/comc.lisp.206'
hcuot "Sep  9 05:47:20 1984" 'zwei/comc.qfasl.204'
hcuot "Sep 10 03:46:41 1985" 'zwei/comd.lisp.170'
hcuot "Sep  7 22:43:25 1984" 'zwei/comd.qfasl.167'
hcuot "Sep 10 03:59:20 1985" 'zwei/come.lisp.135'
hcuot "Aug  4 00:28:48 1984" 'zwei/come.qfasl.133'
hcuot "Sep 10 04:17:00 1985" 'zwei/comf.lisp.103'
hcuot "Sep  9 05:48:56 1984" 'zwei/comf.qfasl.99'
hcuot "Sep 10 04:28:53 1985" 'zwei/comg.lisp.42'
hcuot "Aug 29 09:32:06 1984" 'zwei/comg.qfasl.40'
hcuot "Sep 10 04:31:25 1985" 'zwei/comh.lisp.14'
hcuot "Aug  5 00:04:47 1984" 'zwei/comh.qfasl.13'
hcuot "Sep 10 04:35:43 1985" 'zwei/coms.lisp.86'
hcuot "Aug  5 03:58:57 1984" 'zwei/coms.qfasl.85'
hcuot "Jan 31 15:07:30 1985" 'zwei/comtab.lisp.322'
hcuot "Sep  7 22:39:57 1984" 'zwei/comtab.qfasl.317'
hcuot "Sep 10 04:45:19 1985" 'zwei/defs.lisp.157'
hcuot "Sep 11 21:19:07 1984" 'zwei/defs.qfasl.155'
hcuot "Sep 10 05:16:32 1985" 'zwei/dired.lisp.309'
hcuot "Aug 29 09:33:34 1984" 'zwei/dired.qfasl.304'
hcuot "Sep 10 07:58:18 1985" 'zwei/displa.lisp.159'
hcuot "Sep  7 22:46:25 1984" 'zwei/displa.qfasl.157'
hcuot "Sep 10 08:08:05 1985" 'zwei/doc.lisp.77'
hcuot "Aug  5 04:09:10 1984" 'zwei/doc.qfasl.74'
hcuot "Jun 29 11:10:53 1982" 'zwei/emacs.comdif.1'
hcuot "Apr  7 16:05:03 1984" 'zwei/fasupd.lisp.31'
hcuot "Aug  5 04:10:49 1984" 'zwei/fasupd.qfasl.31'
hcuot "May 17 07:22:10 1985" 'zwei/files.lisp.198'
hcuot "Aug  5 04:11:25 1984" 'zwei/files.qfasl.195'
hcuot "May 22 00:58:47 1984" 'zwei/font.lisp.88'
hcuot "Aug  4 00:11:45 1984" 'zwei/font.qfasl.88'
hcuot "Aug  5 02:39:16 1984" 'zwei/for.lisp.62'
hcuot "Aug  5 03:53:34 1984" 'zwei/for.qfasl.62'
hcuot "Mar 27 10:35:18 1985" 'zwei/grind.definition.1'
hcuot "Jan 29 10:47:23 1985" 'zwei/history.lisp.18'
hcuot "Sep 11 21:33:45 1984" 'zwei/history.qfasl.16'
hcuot "Dec 22 10:01:04 1983" 'zwei/host.lisp.20'
hcuot "Aug  5 04:15:39 1984" 'zwei/host.qfasl.20'
hcuot "Feb 15 07:54:51 1985" 'zwei/indent.lisp.107'
hcuot "Aug  3 23:57:17 1984" 'zwei/indent.qfasl.104'
hcuot "Jan 16 21:21:27 1984" 'zwei/info.zwei.1'
hcuot "Nov  5 05:31:43 1984" 'zwei/insert.lisp.35'
hcuot "Aug  3 23:59:24 1984" 'zwei/insert.qfasl.33'
hcuot "Jul  8 18:10:53 1984" 'zwei/ispell.lisp.41'
hcuot "Aug  5 04:16:46 1984" 'zwei/ispell.qfasl.41'
hcuot "Sep  6 00:19:24 1984" 'zwei/kbdmac.lisp.48'
hcuot "Sep  7 23:06:51 1984" 'zwei/kbdmac.qfasl.48'
hcuot "Dec 24 08:43:28 1983" 'zwei/lparse.lisp.31'
hcuot "Aug  5 04:17:46 1984" 'zwei/lparse.qfasl.31'
hcuot "Mar 24 15:33:02 1985" 'zwei/macros.lisp.150'
hcuot "Sep  7 22:33:54 1984" 'zwei/macros.qfasl.147'
hcuot "Jan 30 12:03:17 1985" 'zwei/meth.lisp.49'
hcuot "Aug  4 00:00:57 1984" 'zwei/meth.qfasl.48'
hcuot "Jan 31 11:32:35 1985" 'zwei/modes.lisp.139'
hcuot "Sep  7 22:36:22 1984" 'zwei/modes.qfasl.138'
hcuot "Mar  2 14:56:03 1985" 'zwei/mouse.lisp.98'
hcuot "Aug  5 04:22:39 1984" 'zwei/mouse.qfasl.96'
hcuot "Jul  3 23:23:20 1984" 'zwei/nprim.lisp.34'
hcuot "Aug  4 00:08:03 1984" 'zwei/nprim.qfasl.34'
hcuot "Feb 15 05:39:26 1985" 'zwei/pated.lisp.33'
hcuot "Aug  5 04:24:35 1984" 'zwei/pated.qfasl.25'
hcuot "Apr  7 16:06:39 1984" 'zwei/pl1mod.lisp.14'
hcuot "Aug  5 04:25:49 1984" 'zwei/pl1mod.qfasl.14'
hcuot "Dec  1 20:23:59 1984" 'zwei/poss.lisp.90'
hcuot "Aug  5 04:31:53 1984" 'zwei/poss.qfasl.87'
hcuot "Sep 26 12:38:14 1984" 'zwei/primit.lisp.175'
hcuot "Aug  5 03:55:55 1984" 'zwei/primit.qfasl.174'
hcuot "Mar 12 04:13:17 1985" 'zwei/screen.lisp.468'
hcuot "Sep  7 22:54:21 1984" 'zwei/screen.qfasl.466'
hcuot "Jul 30 02:57:19 1984" 'zwei/search.lisp.86'
hcuot "Sep  7 22:30:55 1984" 'zwei/search.qfasl.86'
hcuot "Feb 15 08:02:40 1985" 'zwei/sectio.lisp.273'
hcuot "Aug  5 04:40:42 1984" 'zwei/sectio.qfasl.266'
hcuot "Sep  7 00:35:06 1984" 'zwei/stream.lisp.168'
hcuot "Sep  7 22:50:12 1984" 'zwei/stream.qfasl.168'
hcuot "Jan 20 03:45:33 1983" 'zwei/teach-zmacs.text.2'
hcuot "Apr 13 11:52:55 1985" 'zwei/zmacs.lisp.522'
hcuot "Sep  7 23:01:03 1984" 'zwei/zmacs.qfasl.518'
hcuot "Feb  1 00:19:56 1985" 'zwei/zmnew.lisp.36'
hcuot "Sep 10 07:42:59 1984" 'zwei/zmnew.qfasl.35'
hcuot "Apr  7 16:07:07 1984" 'zwei/zymurg.lisp.42'
hcuot "Sep  7 17:25:02 1984" 'zwei/zymurg.qfasl.42'

# tid/671 -- last so we get the correct time stamps on possible old
#    files with a the same version number.

hcuot "Mar 22 06:37:20 1983" '-read-.-this-.2'
hcuot "Sep  3 22:58:12 1984" 'cc/cadld.lisp.8'
hcuot "Sep  9 05:13:51 1984" 'cc/cadld.qfasl.8'
hcuot "Sep 12 06:20:06 1984" 'cc/cadreg.lisp.4'
hcuot "Jul 24 04:17:33 1983" 'cc/cc.help.4'
hcuot "Sep 12 00:22:33 1984" 'cc/cc.lisp.50'
hcuot "Sep 12 06:20:40 1984" 'cc/cc.qfasl.50'
hcuot "Oct  9 08:57:54 1983" 'cc/ccdisk.lisp.106'
hcuot "Sep  9 05:19:18 1984" 'cc/ccdisk.qfasl.106'
hcuot "Dec 27 06:00:38 1983" 'cc/ccgsyl.lisp.6'
hcuot "Sep  9 04:58:38 1984" 'cc/ccgsyl.qfasl.6'
hcuot "Aug 17 03:47:14 1983" 'cc/ccwhy.lisp.12'
hcuot "Sep  9 05:17:29 1984" 'cc/ccwhy.qfasl.12'
hcuot "Apr  7 15:02:25 1984" 'cc/chploc.lisp.5'
hcuot "Sep  9 05:23:30 1984" 'cc/chploc.qfasl.5'
hcuot "Mar  9 20:11:02 1984" 'cc/dcfu.uload.2'
hcuot "Sep  9 05:28:27 1984" 'cc/dcheck.lisp.7'
hcuot "Jul  2 21:56:55 1982" 'cc/dcheck.loop.1'
hcuot "Sep  9 05:36:25 1984" 'cc/dcheck.qfasl.7'
hcuot "Sep  9 05:03:26 1984" 'cc/diags.lisp.159'
hcuot "Sep  9 05:04:08 1984" 'cc/diags.qfasl.159'
hcuot "Jan  3 20:00:50 1985" 'cc/dmon.lisp.57'
hcuot "Sep  9 05:10:55 1984" 'cc/dmon.qfasl.56'
hcuot "Dec  7 22:33:10 1984" 'cc/junk..1'
hcuot "Sep  9 03:31:10 1984" 'cc/lcadmc.lisp.31'
hcuot "Sep  9 04:49:35 1984" 'cc/lcadmc.qfasl.31'
hcuot "Apr  7 15:04:15 1984" 'cc/lcadrd.lisp.95'
hcuot "Sep  9 04:58:54 1984" 'cc/lcadrd.qfasl.95'
hcuot "Jun 20 01:44:48 1983" 'cc/ldbg.lisp.45'
hcuot "Sep  9 05:12:40 1984" 'cc/ldbg.qfasl.45'
hcuot "Nov 12 08:26:07 1983" 'cc/lqfmac.lisp.17'
hcuot "Sep  9 04:48:57 1984" 'cc/lqfmac.qfasl.17'
hcuot "Oct 26 04:06:09 1983" 'cc/patch.directory.3'
hcuot "Sep  8 02:21:16 1984" 'cc/patch-3.directory.23'
hcuot "Dec  1 14:12:53 1983" 'cc/patch-3-1.qfasl.1'
hcuot "Sep  8 02:20:42 1984" 'cc/patch-3-10.lisp.1'
hcuot "Sep  8 02:20:45 1984" 'cc/patch-3-10.qfasl.1'
hcuot "Dec 18 00:50:34 1983" 'cc/patch-3-2.qfasl.2'
hcuot "Dec 27 05:59:31 1983" 'cc/patch-3-3.qfasl.1'
hcuot "Dec 27 19:59:00 1983" 'cc/patch-3-4.qfasl.1'
hcuot "Jan 23 07:16:31 1984" 'cc/patch-3-5.qfasl.2'
hcuot "Jan 27 09:11:57 1984" 'cc/patch-3-6.qfasl.1'
hcuot "Jun 11 23:06:48 1984" 'cc/patch-3-7.qfasl.1'
hcuot "Jul  7 01:29:34 1984" 'cc/patch-3-8.qfasl.1'
hcuot "Sep  6 20:28:13 1984" 'cc/patch-3-9.lisp.1'
hcuot "Sep  6 20:28:20 1984" 'cc/patch-3-9.qfasl.1'
hcuot "Sep  9 05:29:20 1984" 'cc/qf.lisp.126'
hcuot "Sep  9 05:33:23 1984" 'cc/qf.qfasl.126'
hcuot "Jul  7 01:39:01 1984" 'cc/salvag.lisp.38'
hcuot "Sep  9 05:23:45 1984" 'cc/salvag.qfasl.38'
hcuot "Apr  7 15:05:00 1984" 'cc/zero.lisp.15'
hcuot "Sep  9 05:13:29 1984" 'cc/zero.qfasl.15'
hcuot "May 27 23:12:18 1986" 'chaos/hosts.text.392'
hcuot "Oct  9 13:51:43 1984" 'cold/coldld.lisp.84'
hcuot "Sep 11 07:47:22 1984" 'cold/coldld.qfasl.83'
hcuot "Sep 11 07:43:22 1984" 'cold/coldpk.lisp.25'
hcuot "Sep 11 07:43:37 1984" 'cold/coldpk.qfasl.25'
hcuot "Aug 30 06:45:32 1984" 'cold/coldut.lisp.100'
hcuot "Aug 30 09:00:56 1984" 'cold/coldut.qfasl.100'
hcuot "Sep  5 20:29:29 1984" 'cold/defmic.lisp.200'
hcuot "Feb 13 01:46:43 1985" 'cold/docmic.lisp.41'
hcuot "Mar 12 00:16:37 1985" 'cold/export.lisp.31'
hcuot "Feb 28 16:10:43 1985" 'cold/global.lisp.644'
hcuot "Aug 15 04:46:01 1984" 'cold/global.qfasl.634'
hcuot "Feb 28 15:51:49 1985" 'cold/lisp.lisp.2'
hcuot "Feb 15 05:33:28 1985" 'cold/mini.lisp.90'
hcuot "Aug 15 05:19:46 1984" 'cold/mini.qfasl.88'
hcuot "Nov 11 03:18:52 1984" 'cold/minisr.exe.1'
hcuot "Nov 11 03:18:01 1984" 'cold/minisr.mid.44'
hcuot "Feb 12 09:35:01 1985" 'cold/qcom.lisp.583'
hcuot "Dec  6 09:29:11 1984" 'cold/qdefs.lisp.388'
hcuot "Feb 17 09:27:51 1985" 'cold/system.lisp.106'
hcuot "Aug 15 06:23:06 1984" 'cold/system.qfasl.102'
hcuot "Apr  7 15:06:03 1984" 'demo/abacus.lisp.20'
hcuot "Sep  8 01:13:00 1984" 'demo/abacus.qfasl.20'
hcuot "Dec 13 05:41:22 1983" 'demo/alarm.lisp.50'
hcuot "Jul  4 21:19:15 1985" 'demo/alarm.qfasl.50'
hcuot "Aug 16 10:56:31 1983" 'demo/beeps.lisp.8'
hcuot "Oct 26 23:25:37 1983" 'demo/beeps.qfasl.8'
hcuot "Apr  7 15:06:23 1984" 'demo/cafe.lisp.8'
hcuot "Jun  6 13:00:32 1984" 'demo/cafe.qfasl.8'
hcuot "Nov 13 00:18:36 1983" 'demo/colorhack.lisp.7'
hcuot "Nov 13 07:19:50 1983" 'demo/colorhack.qfasl.7'
hcuot "Jun 20 02:52:15 1983" 'demo/colxor.lisp.52'
hcuot "Oct 26 23:29:52 1983" 'demo/colxor.qfasl.52'
hcuot "Jul 21 17:00:24 1982" 'demo/craze.lisp.2'
hcuot "Aug 14 10:34:04 1983" 'demo/craze.qfasl.2'
hcuot "Dec  9 04:29:51 1983" 'demo/crock.lisp.6'
hcuot "Jul  6 09:06:15 1985" 'demo/crock.qfasl.6'
hcuot "Apr  7 16:38:29 1984" 'demo/ctest.lisp.1'
hcuot "Mar 31 18:14:03 1984" 'demo/dc.lisp.4'
hcuot "Sep  8 01:10:53 1984" 'demo/dc.qfasl.4'
hcuot "Apr  7 15:07:09 1984" 'demo/deutsc.lisp.34'
hcuot "Jun  6 13:04:44 1984" 'demo/deutsc.qfasl.34'
hcuot "Apr  7 15:07:35 1984" 'demo/dlwhak.lisp.37'
hcuot "Jun  6 13:05:34 1984" 'demo/dlwhak.qfasl.37'
hcuot "Apr  7 15:08:37 1984" 'demo/docscr.lisp.6'
hcuot "Jun  6 13:07:54 1984" 'demo/docscr.qfasl.6'
hcuot "Sep  5 19:58:44 1984" 'demo/doctor.lisp.10'
hcuot "Sep  7 23:25:44 1984" 'demo/doctor.qfasl.10'
hcuot "Jun 20 02:52:55 1983" 'demo/geb.lisp.27'
hcuot "Oct 26 23:38:48 1983" 'demo/geb.qfasl.27'
hcuot "Jun 20 02:53:09 1983" 'demo/hakdef.lisp.14'
hcuot "Sep  8 01:14:00 1984" 'demo/hakdef.qfasl.14'
hcuot "Apr  7 15:09:41 1984" 'demo/hcedit.lisp.28'
hcuot "Jun  6 13:08:50 1984" 'demo/hcedit.qfasl.28'
hcuot "Apr  7 15:11:41 1984" 'demo/liss.lisp.5'
hcuot "Nov 10 12:46:06 1983" 'demo/munch.lisp.14'
hcuot "Sep  8 01:09:22 1984" 'demo/munch.qfasl.14'
hcuot "Apr  7 15:24:04 1984" 'demo/npaint.lisp.1'
hcuot "Aug 16 10:56:24 1983" 'demo/ohacks.lisp.35'
hcuot "Oct 26 23:44:57 1983" 'demo/ohacks.qfasl.35'
hcuot "Apr  7 15:12:09 1984" 'demo/organ.lisp.18'
hcuot "Jun  6 13:09:56 1984" 'demo/organ.qfasl.18'
hcuot "Jun 20 02:52:00 1983" 'demo/pfom.lisp.31'
hcuot "Aug 14 10:44:10 1983" 'demo/pfom.qfasl.31'
hcuot "Oct 24 20:42:24 1983" 'demo/qix.lisp.3'
hcuot "Oct 26 23:47:45 1983" 'demo/qix.qfasl.3'
hcuot "Jul 26 10:08:11 1983" 'demo/rotate.lisp.5'
hcuot "Oct 26 23:48:20 1983" 'demo/rotate.qfasl.5'
hcuot "Aug 20 21:03:08 1983" 'demo/rotcir.lisp.5'
hcuot "Oct 26 23:49:12 1983" 'demo/rotcir.qfasl.5'
hcuot "Dec 27 14:24:52 1983" 'demo/treedv.lisp.4'
hcuot "Dec 27 14:25:03 1983" 'demo/treedv.qfasl.4'
hcuot "Jul 20 14:04:52 1982" 'demo/tvbgar.qfasl.1'
hcuot "Apr  7 15:39:50 1984" 'demo/versat.lisp.1'
hcuot "Apr  7 15:41:20 1984" 'demo/votrax.lisp.1'
hcuot "Oct 22 02:49:11 1983" 'demo/what.lisp.19'
hcuot "Oct 26 23:54:43 1983" 'demo/what.qfasl.19'
hcuot "Apr  7 15:42:46 1984" 'demo/words.lisp.1'
hcuot "Sep  6 23:42:51 1984" 'demo/worm.lisp.9'
hcuot "Sep 10 07:44:09 1984" 'demo/worm.qfasl.9'
hcuot "Dec 13 05:41:19 1983" 'demo/worm-trails.lisp.15'
hcuot "Sep  8 01:08:09 1984" 'demo/worm-trails.qfasl.15'
hcuot "Jul 20 14:05:17 1982" 'demo/wormch.ast.1'
hcuot "Jul 20 14:05:20 1982" 'demo/wormch.qfasl.1'
hcuot "Jun 15 10:28:10 1984" 'distribution/dist.lisp.8'
hcuot "Feb 21 00:09:00 1984" 'distribution/dist.qfasl.7'
hcuot "Feb 16 13:57:28 1984" 'distribution/lmi-filter.lisp.2'
hcuot "Aug  1 21:21:44 1982" 'doc/array.intent.1'
hcuot "Aug  1 21:21:50 1982" 'doc/bmcode.text.4'
hcuot "Mar 30 10:07:46 1987" 'doc/bug.idx.1'
hcuot "Aug 31 16:11:13 1987" 'doc/bug.lispm.1'
hcuot "Aug  1 21:24:26 1982" 'doc/bug.lispm10.1'
hcuot "Aug  1 21:25:10 1982" 'doc/bug.lispm11.1'
hcuot "Aug  1 21:26:02 1982" 'doc/bug.lispm12.1'
hcuot "Aug  1 21:22:04 1982" 'doc/bug.lispm13.1'
hcuot "Nov 30 00:47:02 1982" 'doc/bug.lispm14.1'
hcuot "Jan 11 22:41:27 1983" 'doc/bug.lispm15.1'
hcuot "Feb 24 02:11:04 1983" 'doc/bug.lispm16.1'
hcuot "Apr 18 07:53:19 1983" 'doc/bug.lispm17.1'
hcuot "Jun  1 20:37:41 1983" 'doc/bug.lispm18.1'
hcuot "May 23 13:26:53 1984" 'doc/bug.lispm18.2'
hcuot "Aug  4 04:36:47 1983" 'doc/bug.lispm19.1'
hcuot "Sep 23 01:01:25 1984" 'doc/bug.lispm19.2'
hcuot "Oct  5 19:38:55 1983" 'doc/bug.lispm20.1'
hcuot "Nov 12 23:29:45 1983" 'doc/bug.lispm21.1'
hcuot "Jan  7 20:27:51 1984" 'doc/bug.lispm22.1'
hcuot "May 16 00:33:17 1984" 'doc/bug.lispm23.1'
hcuot "Nov 16 10:43:03 1984" 'doc/bug.lispm24.1'
hcuot "Nov 16 11:05:09 1984" 'doc/bug.lispm25.1'
hcuot "Nov 16 11:20:32 1984" 'doc/bug.lispm26.1'
hcuot "Jan 30 17:49:13 1985" 'doc/bug.lispm27.2'
hcuot "Jan 30 17:52:12 1985" 'doc/bug.lispm28.1'
hcuot "Jan 30 17:46:04 1985" 'doc/bug.lispm29.1'
hcuot "Jan 30 17:46:24 1985" 'doc/bug.lispm30.1'
hcuot "Apr  4 01:29:30 1985" 'doc/bug.lispm31.1'
hcuot "Apr 20 02:31:42 1985" 'doc/bug.lispm32.1'
hcuot "May 16 18:40:47 1985" 'doc/bug.lispm33.1'
hcuot "Jul  4 03:00:11 1985" 'doc/bug.lispm34.1'
hcuot "Oct 23 04:48:16 1985" 'doc/bug.lispm35.1'
hcuot "Jan 27 10:51:04 1986" 'doc/bug.lispm36.1'
hcuot "Oct 15 03:25:22 1986" 'doc/bug.lispm37.1'
hcuot "Oct 15 03:25:35 1986" 'doc/bug.lispm38.1'
hcuot "Nov 10 22:44:08 1986" 'doc/bug.lispm39.1'
hcuot "Feb 26 19:04:12 1987" 'doc/bug.lispm40.1'
hcuot "Jun 23 04:53:46 1987" 'doc/bug.lispm41.1'
hcuot "Aug 10 04:46:21 1987" 'doc/bug.lispm42.1'
hcuot "Aug  1 21:23:42 1982" 'doc/bug.lispm9.1'
hcuot "Aug  1 21:26:50 1982" 'doc/bug.not-info-lispm!.1'
hcuot "Aug 31 06:01:12 1987" 'doc/bug-lispm-recent-bboard.idx.1'
hcuot "Aug 31 16:11:20 1987" 'doc/bug-lispm-recent-bboard.txt.1'
hcuot "Aug  1 21:26:56 1982" 'doc/cadr.text.164'
hcuot "Aug  3 00:58:18 1987" 'doc/cadr-mail.txt.1'
hcuot "Aug  1 21:27:24 1982" 'doc/cells.text.4'
hcuot "Aug  1 21:27:33 1982" 'doc/char.text.18'
hcuot "Aug  1 21:27:39 1982" 'doc/chead.text.4'
hcuot "Nov  1 18:57:53 1982" 'doc/chfile.text.3'
hcuot "Aug  1 21:27:57 1982" 'doc/chod1.drw.1'
hcuot "Aug  1 21:28:07 1982" 'doc/chodam.drw.1'
hcuot "Aug  1 21:28:16 1982" 'doc/chodi.drw.1'
hcuot "Aug  1 21:28:26 1982" 'doc/chodtm.drw.1'
hcuot "Jun 16 01:54:24 1984" 'doc/clisp-mail.txt.1'
hcuot "Aug  1 21:28:32 1982" 'doc/closur.text.12'
hcuot "Aug  1 21:28:39 1982" 'doc/cold.tags.1'
hcuot "Jan  2 07:51:49 1984" 'doc/common.lisp.9'
hcuot "Aug  1 21:28:51 1982" 'doc/cons.text.93'
hcuot "Dec 22 10:50:05 1985" 'doc/converse.bugs.1'
hcuot "Aug  1 21:29:11 1982" 'doc/csoft.text.20'
hcuot "Aug  1 21:29:32 1982" 'doc/cursor.answer.1'
hcuot "Aug  1 21:29:39 1982" 'doc/dfs.text.12'
hcuot "Aug  1 21:29:45 1982" 'doc/disk.text.22'
hcuot "Mar 30 10:11:32 1987" 'doc/doc-changes-mail.idx.1'
hcuot "Mar 22 22:45:51 1986" 'doc/doc-changes-mail.txt.1'
hcuot "Aug  1 21:29:53 1982" 'doc/doctor.text.5'
hcuot "Aug  1 21:30:04 1982" 'doc/eddoc.text.6'
hcuot "Aug  1 21:30:11 1982" 'doc/edfnd.text.11'
hcuot "Aug  1 21:30:16 1982" 'doc/eined.text.5'
hcuot "Aug  1 21:30:23 1982" 'doc/error.lights.1'
hcuot "Aug  1 21:30:31 1982" 'doc/fasld.text.1'
hcuot "Aug  1 21:30:35 1982" 'doc/fcfs.text.10'
hcuot "Aug  1 21:42:29 1982" 'doc/fig.fil.2'
hcuot "Aug  1 21:42:35 1982" 'doc/fig1.drw.1'
hcuot "Aug  1 21:42:44 1982" 'doc/fig2.drw.1'
hcuot "Aug  1 21:42:54 1982" 'doc/fig3.drw.1'
hcuot "Aug  1 21:43:02 1982" 'doc/fig5.drw.1'
hcuot "Aug  1 21:43:11 1982" 'doc/format.text.77'
hcuot "Aug  1 21:43:25 1982" 'doc/gctim.text.5'
hcuot "Aug  1 21:43:32 1982" 'doc/goto.text.1'
hcuot "Aug  1 21:43:38 1982" 'doc/if.answer.1'
hcuot "Mar 30 10:11:32 1987" 'doc/info.idx.1'
hcuot "Aug 19 05:36:45 1987" 'doc/info.lispm.1'
hcuot "Aug  1 21:44:12 1982" 'doc/info.lispm1.1'
hcuot "Aug  1 21:43:42 1982" 'doc/info.lispm2.1'
hcuot "Jan  7 18:46:34 1983" 'doc/instal.newsys.2'
hcuot "Aug  1 21:45:03 1982" 'doc/io.text.3'
hcuot "Aug  1 21:45:09 1982" 'doc/iob.text.9'
hcuot "Aug  1 21:45:16 1982" 'doc/kbds.text.8'
hcuot "Aug  1 21:45:58 1982" 'doc/lmacro.text.1'
hcuot "Aug  1 21:46:05 1982" 'doc/lmcomp.text.3'
hcuot "Aug  1 22:00:01 1982" 'doc/lmfns.text.8'
hcuot "Aug  1 22:00:23 1982" 'doc/lmnuc.text.156'
hcuot "Aug  1 22:00:41 1982" 'doc/lmtape.text.1'
hcuot "Aug  1 23:10:27 1982" 'doc/macro.text.28'
hcuot "Aug  1 23:10:33 1982" 'doc/mcdoc.text.12'
hcuot "Aug  1 23:10:38 1982" 'doc/mcrdoc.text.3'
hcuot "Aug  1 23:10:44 1982" 'doc/menu.text.1'
hcuot "Aug  1 23:10:50 1982" 'doc/mess.text.5'
hcuot "Aug  1 23:10:58 1982" 'doc/meter.text.7'
hcuot "Aug  1 23:11:06 1982" 'doc/mouse.text.3'
hcuot "Aug  1 23:11:13 1982" 'doc/name.text.5'
hcuot "Aug  1 23:11:17 1982" 'doc/nboot.text.19'
hcuot "Oct 24 21:45:35 1982" 'doc/nes.text.2'
hcuot "Nov 26 08:19:34 1984" 'doc/netwrk.msg.1'
hcuot "Aug  1 23:12:05 1982" 'doc/packd.text.5'
hcuot "Aug  1 23:12:15 1982" 'doc/paging.text.41'
hcuot "Aug  1 23:12:27 1982" 'doc/paper.text.105'
hcuot "Aug  1 23:12:48 1982" 'doc/proces.text.6'
hcuot "Aug  1 23:12:55 1982" 'doc/progr.text.29'
hcuot "Aug  1 23:13:06 1982" 'doc/qev.text.1'
hcuot "Aug  1 23:13:09 1982" 'doc/rename.text.21'
hcuot "Aug  1 23:13:13 1982" 'doc/sgmods.text.15'
hcuot "Aug  1 23:13:17 1982" 'doc/ss201.msg.1'
hcuot "Aug  1 23:13:20 1982" 'doc/stackg.text.4'
hcuot "Aug  1 23:13:24 1982" 'doc/storag.text.39'
hcuot "Aug  1 23:13:33 1982" 'doc/sys204.flavor.1'
hcuot "Aug  1 23:13:38 1982" 'doc/sys204.msg.1'
hcuot "Aug  1 23:13:44 1982" 'doc/sys210.msg.1'
hcuot "Sep 19 06:01:03 1983" 'doc/sys286.msg.5'
hcuot "Aug  1 23:14:08 1982" 'doc/sys74.msg.1'
hcuot "Aug  1 23:14:12 1982" 'doc/sys78.msg.1'
hcuot "Aug  1 23:14:16 1982" 'doc/sys79.msg.1'
hcuot "Aug  1 23:14:21 1982" 'doc/sys85.msg.1'
hcuot "Aug  1 23:14:27 1982" 'doc/sys86.msg.1'
hcuot "Aug 30 09:31:33 1982" 'doc/sys87.msg.2'
hcuot "Oct 12 09:32:12 1982" 'doc/sys88.msg.8'
hcuot "Oct 31 19:32:24 1982" 'doc/sys89.msg.9'
hcuot "Feb 22 01:00:09 1983" 'doc/sys91.msg.10'
hcuot "Mar 17 05:11:14 1983" 'doc/sys93.msg.13'
hcuot "Jun  2 12:26:24 1983" 'doc/sys94.msg.12'
hcuot "Sep 19 09:34:26 1983" 'doc/sys97.msg.7'
hcuot "Dec 24 06:19:52 1983" 'doc/sys98.defstruct.6'
hcuot "Apr 29 00:34:35 1984" 'doc/sys98.msg.34'
hcuot "Dec 15 03:24:42 1983" 'doc/sys98.packages.2'
hcuot "Nov 28 10:26:08 1984" 'doc/sys99.msg.41'
hcuot "Aug  1 23:14:41 1982" 'doc/tvdoc.text.34'
hcuot "Sep 23 01:23:03 1985" 'doc/unaddr.text.56'
hcuot "Aug  1 23:15:16 1982" 'doc/zwei.answer.1'
hcuot "Aug 19 15:56:59 1983" 'doc/zweidoc.txt.1'
hcuot "Feb  8 05:09:02 1985" 'eh/eh.lisp.340'
hcuot "Sep  9 05:53:06 1984" 'eh/eh.qfasl.336'
hcuot "Feb 17 10:09:48 1985" 'eh/ehc.lisp.236'
hcuot "Sep  7 22:04:40 1984" 'eh/ehc.qfasl.233'
hcuot "Feb  7 13:02:56 1985" 'eh/ehf.lisp.228'
hcuot "Dec  8 18:28:16 1986" 'eh/ehf.lisp.229'
hcuot "Sep 11 21:22:50 1984" 'eh/ehf.qfasl.225'
hcuot "Nov  9 11:13:07 1984" 'eh/ehsys.lisp.1'
hcuot "May 16 12:21:59 1984" 'eh/ehw.lisp.109'
hcuot "Sep  8 01:04:14 1984" 'eh/ehw.qfasl.109'
hcuot "Feb 13 13:01:19 1985" 'eh/errmac.lisp.2'
hcuot "Aug 28 01:12:14 1985" 'eh/she.lisp.1'
hcuot "Dec  5 06:43:25 1984" 'file/bugs.mail.1'
hcuot "Jul 22 15:24:10 1982" 'file/clear.lisp.1'
hcuot "Sep 14 08:15:08 1984" 'file/copy.lisp.131'
hcuot "Jan  3 09:07:03 1984" 'file/copy.qfasl.128'
hcuot "May 14 22:28:15 1986" 'file/fs.directory.14'
hcuot "Jul 22 15:28:50 1982" 'file/fs.improv.1'
hcuot "Sep 11 06:45:16 1984" 'file/fs.lisp.77'
hcuot "Sep 11 06:48:35 1984" 'file/fs.qfasl.77'
hcuot "Nov 21 22:05:22 1984" 'file/fs-48.directory.16'
hcuot "Jan  5 00:02:49 1984" 'file/fs-48-1.lisp.1'
hcuot "Jan  5 00:03:05 1984" 'file/fs-48-1.qfasl.1'
hcuot "Jan 18 17:50:40 1984" 'file/fs-48-2.lisp.4'
hcuot "Jan 27 07:15:20 1984" 'file/fs-48-3.lisp.2'
hcuot "Jan 27 07:15:27 1984" 'file/fs-48-3.qfasl.2'
hcuot "May 16 10:04:05 1984" 'file/fs-48-4.lisp.2'
hcuot "May 16 10:04:09 1984" 'file/fs-48-4.qfasl.2'
hcuot "Jun 10 14:19:39 1984" 'file/fs-48-5.lisp.1'
hcuot "Jun 10 14:19:43 1984" 'file/fs-48-5.qfasl.1'
hcuot "Nov 21 22:04:24 1984" 'file/fs-48-6.lisp.1'
hcuot "Nov 21 22:04:35 1984" 'file/fs-48-6.qfasl.1'
hcuot "Jul 16 17:33:45 1984" 'file/fs-49.directory.3'
hcuot "Jul 16 17:33:11 1984" 'file/fs-49-1.lisp.1'
hcuot "Jul 16 17:33:18 1984" 'file/fs-49-1.qfasl.1'
hcuot "Sep 12 01:49:42 1984" 'file/fs-50.directory.1'
hcuot "Sep 15 02:45:55 1984" 'file/fs-51.directory.1'
hcuot "Apr 12 03:33:51 1985" 'file/fs-52.directory.1'
hcuot "May 14 22:28:18 1986" 'file/fs-53.directory.1'
hcuot "Apr 13 12:21:42 1985" 'file/fsacc.lisp.6'
hcuot "May 14 22:26:59 1986" 'file/fsacc.qfasl.6'
hcuot "Sep 12 05:09:25 1984" 'file/fsdefs.lisp.177'
hcuot "May 14 22:10:20 1986" 'file/fsdefs.qfasl.177'
hcuot "Nov  6 15:13:45 1982" 'file/fsdoc.text.6'
hcuot "Sep 12 05:09:30 1984" 'file/fsguts.lisp.371'
hcuot "May 14 22:17:16 1986" 'file/fsguts.qfasl.371'
hcuot "Sep 11 06:21:11 1984" 'file/fsname.lisp.106'
hcuot "Jul  5 11:03:05 1984" 'file/fsname.qfasl.104'
hcuot "Sep 12 05:09:42 1984" 'file/fsstr.lisp.107'
hcuot "May 14 22:14:57 1986" 'file/fsstr.qfasl.107'
hcuot "Sep 14 05:24:39 1984" 'file/hogs.lisp.6'
hcuot "Sep 14 04:59:04 1984" 'file/hogs.qfasl.4'
hcuot "Dec 14 02:31:32 1984" 'file/lmpars.lisp.113'
hcuot "Sep 10 20:39:55 1984" 'file/lmpars.qfasl.110'
hcuot "Apr  7 15:14:57 1984" 'file/login.lisp.26'
hcuot "Jul 22 15:35:33 1982" 'file/login.qfasl.25'
hcuot "Jan  3 06:30:18 1984" 'file/magtape.directory.9'
hcuot "Oct 26 20:41:54 1983" 'file/magtape-14.directory.14'
hcuot "Mar  8 06:56:29 1983" 'file/magtape-14-1.lisp.1'
hcuot "Mar  8 06:56:47 1983" 'file/magtape-14-1.qfasl.1'
hcuot "Mar 29 08:21:02 1983" 'file/magtape-14-2.lisp.1'
hcuot "Apr 25 09:51:40 1983" 'file/magtape-14-3.lisp.1'
hcuot "Apr 25 09:51:48 1983" 'file/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:18 1983" 'file/magtape-14-4.lisp.3'
hcuot "May 19 04:11:35 1983" 'file/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:05 1983" 'file/magtape-14-5.lisp.1'
hcuot "Oct 26 20:41:17 1983" 'file/magtape-14-5.qfasl.1'
hcuot "Jan  3 08:48:40 1984" 'file/mtaux.lisp.77'
hcuot "Dec 16 15:34:10 1983" 'file/mtdefs.lisp.30'
hcuot "Jan  3 08:49:38 1984" 'file/mtstr.lisp.85'
hcuot "Jan  3 08:50:55 1984" 'file/odump.lisp.1'
hcuot "May 15 00:29:35 1986" 'file/server.directory.7'
hcuot "Mar 27 23:45:30 1985" 'file/server.lisp.154'
hcuot "May 15 00:22:55 1986" 'file/server.qfasl.154'
hcuot "May 15 00:29:39 1986" 'file/server-10.directory.1'
hcuot "May 26 12:42:23 1984" 'file/server-8.directory.14'
hcuot "Jan  4 06:49:04 1984" 'file/server-8-1.lisp.1'
hcuot "Jan  4 06:49:14 1984" 'file/server-8-1.qfasl.1'
hcuot "Jan  5 00:06:06 1984" 'file/server-8-2.lisp.1'
hcuot "Jan  5 00:06:20 1984" 'file/server-8-2.qfasl.1'
hcuot "May 26 12:36:29 1984" 'file/server-8-3.lisp.4'
hcuot "May 26 12:36:34 1984" 'file/server-8-3.qfasl.4'
hcuot "Feb 16 15:28:03 1984" 'file/server-8-4.lisp.1'
hcuot "Feb 16 15:28:08 1984" 'file/server-8-4.qfasl.1'
hcuot "May 26 12:39:28 1984" 'file/server-8-5.lisp.2'
hcuot "May 26 12:39:34 1984" 'file/server-8-5.qfasl.2'
hcuot "Sep 15 02:35:57 1984" 'file/server-9.directory.1'
hcuot "Jul 13 07:23:33 1984" 'file/zmail.lisp.5'
hcuot "Sep  9 20:30:21 1984" 'file/zmail.qfasl.5'
hcuot "Jan  4 01:55:28 1984" 'file2/anydir.lisp.201'
hcuot "Aug  3 07:41:49 1984" 'file2/anydir.qfasl.201'
hcuot "Jan 18 17:35:23 1984" 'file2/area.lisp.22'
hcuot "Aug  3 07:33:44 1984" 'file2/area.qfasl.22'
hcuot "Jul 22 15:59:30 1982" 'file2/bfsplm.text.4'
hcuot "Dec 25 09:02:12 1985" 'file2/bug-lmfile-mail.txt.1'
hcuot "Jul 22 15:59:57 1982" 'file2/chgnod.lisp.5'
hcuot "Aug  3 07:40:05 1984" 'file2/chgnod.qfasl.5'
hcuot "Jan 18 17:46:24 1984" 'file2/complt.lisp.17'
hcuot "Aug  3 07:50:25 1984" 'file2/complt.qfasl.17'
hcuot "Aug  3 07:19:33 1984" 'file2/defs.lisp.190'
hcuot "Nov 21 19:17:17 1984" 'file2/defs.qfasl.190'
hcuot "Jan 29 07:16:03 1984" 'file2/diread.lisp.61'
hcuot "Aug  3 07:48:32 1984" 'file2/diread.qfasl.61'
hcuot "Jul 22 16:02:31 1982" 'file2/doc.text.12'
hcuot "Jan 18 17:50:00 1984" 'file2/dump.lisp.29'
hcuot "Aug  3 07:56:23 1984" 'file2/dump.qfasl.29'
hcuot "Jan 18 17:34:11 1984" 'file2/files.lisp.122'
hcuot "Aug  3 07:30:22 1984" 'file2/files.qfasl.122'
hcuot "Dec 18 10:50:47 1983" 'file2/free.lisp.48'
hcuot "Aug  3 07:26:33 1984" 'file2/free.qfasl.48'
hcuot "Feb  9 18:19:25 1986" 'file2/fs-fc-mail.txt.1'
hcuot "Jan 18 17:30:16 1984" 'file2/gc.lisp.19'
hcuot "Aug  3 07:32:53 1984" 'file2/gc.qfasl.19'
hcuot "Jan 18 17:29:20 1984" 'file2/io.lisp.94'
hcuot "Aug  3 07:28:01 1984" 'file2/io.qfasl.94'
hcuot "Feb  1 03:07:28 1984" 'file2/link.lisp.47'
hcuot "Aug  3 07:37:26 1984" 'file2/link.qfasl.47'
hcuot "Aug  3 08:02:34 1984" 'file2/lmfile.directory.4'
hcuot "Sep 30 07:33:31 1983" 'file2/lmfile-2.directory.10'
hcuot "Jun 16 23:34:55 1983" 'file2/lmfile-2-1.lisp.1'
hcuot "Jul  6 22:59:00 1983" 'file2/lmfile-2-2.lisp.1'
hcuot "Jul  7 07:33:09 1983" 'file2/lmfile-2-3.lisp.2'
hcuot "Sep 30 07:32:51 1983" 'file2/lmfile-2-4.lisp.1'
hcuot "Jan 29 08:54:05 1984" 'file2/lmfile-3.directory.7'
hcuot "Jan  4 01:56:37 1984" 'file2/lmfile-3-1.lisp.1'
hcuot "Jan  4 01:56:48 1984" 'file2/lmfile-3-1.qfasl.1'
hcuot "Jan 29 08:53:39 1984" 'file2/lmfile-3-3.lisp.1'
hcuot "Jan 29 08:53:44 1984" 'file2/lmfile-3-3.qfasl.1'
hcuot "Aug  3 08:02:37 1984" 'file2/lmfile-4.directory.1'
hcuot "Jul 22 16:07:13 1982" 'file2/maint.text.9'
hcuot "Dec  2 15:30:00 1984" 'file2/maiser.lisp.13'
hcuot "Aug  3 08:01:34 1984" 'file2/maiser.qfasl.9'
hcuot "Jan 18 17:37:55 1984" 'file2/node.lisp.162'
hcuot "Aug  3 07:34:26 1984" 'file2/node.qfasl.162'
hcuot "Nov 21 18:30:20 1984" 'file2/pack.lisp.83'
hcuot "Nov 21 19:37:41 1984" 'file2/pack.qfasl.83'
hcuot "Nov 21 05:43:10 1984" 'file2/pathnm.lisp.163'
hcuot "Sep 11 06:27:31 1984" 'file2/pathnm.qfasl.162'
hcuot "Dec 18 22:59:00 1983" 'file2/pdp10.lisp.20'
hcuot "Aug  3 07:38:34 1984" 'file2/pdp10.qfasl.20'
hcuot "Nov 23 09:57:38 1982" 'file2/remote.directory.10'
hcuot "Dec 12 07:15:02 1982" 'file2/remote.lisp.30'
hcuot "Dec 30 05:30:12 1982" 'file2/remote-23.directory.4'
hcuot "Jan 29 08:53:17 1984" 'file2/repair.lisp.1'
hcuot "Dec 18 10:20:56 1983" 'file2/rmdefs.lisp.10'
hcuot "Aug  3 09:02:57 1984" 'file2/rmdefs.qfasl.10'
hcuot "Jan 18 17:42:38 1984" 'file2/salvag.lisp.22'
hcuot "Aug  3 07:40:37 1984" 'file2/salvag.qfasl.22'
hcuot "Nov 21 21:59:39 1984" 'file2/server.lisp.52'
hcuot "Nov 21 19:41:01 1984" 'file2/server.qfasl.50'
hcuot "Jan 18 17:45:18 1984" 'file2/spcdir.lisp.92'
hcuot "Aug  3 07:45:33 1984" 'file2/spcdir.qfasl.92'
hcuot "Aug  3 08:54:13 1984" 'file2/stream.lisp.210'
hcuot "Aug  3 08:57:08 1984" 'file2/stream.qfasl.210'
hcuot "Apr  7 15:16:07 1984" 'file2/system.lisp.32'
hcuot "May 23 03:23:41 1984" 'file2/system.qfasl.32'
hcuot "Jul 22 16:18:48 1982" 'file2/view.text.9'
hcuot "Jan 18 17:22:34 1984" 'file2/xrmdefs.lisp.11'
hcuot "Jan 18 17:05:07 1984" 'file2/xserver.lisp.44'
hcuot "Jun 18 05:38:36 1984" 'fonts/13fgb.qfasl.6'
hcuot "Jun 18 05:38:32 1984" 'fonts/16fg.qfasl.5'
hcuot "Jun 18 05:38:21 1984" 'fonts/18fg.qfasl.5'
hcuot "Jun 18 05:38:12 1984" 'fonts/20vr.qfasl.5'
hcuot "Jun 18 05:38:08 1984" 'fonts/25fr3.qfasl.5'
hcuot "Jun 18 05:38:03 1984" 'fonts/31vr.qfasl.5'
hcuot "Jun 18 05:37:56 1984" 'fonts/40vr.qfasl.5'
hcuot "Jun 18 05:37:52 1984" 'fonts/40vshd.qfasl.5'
hcuot "Jun 18 05:37:43 1984" 'fonts/43vxms.qfasl.10'
hcuot "Jun 18 05:37:40 1984" 'fonts/5x5.qfasl.10'
hcuot "Jun 18 05:37:37 1984" 'fonts/abacus.qfasl.5'
hcuot "Jun 18 05:37:34 1984" 'fonts/apl14.qfasl.4'
hcuot "Jun 18 05:37:30 1984" 'fonts/arr10.qfasl.5'
hcuot "Sep 10 20:20:10 1984" 'fonts/bigfnt.qfasl.10'
hcuot "Jul 20 14:17:55 1982" 'fonts/bigold.qfasl.1'
hcuot "Jun 18 05:37:22 1984" 'fonts/bigvg.qfasl.4'
hcuot "Jun 18 05:37:20 1984" 'fonts/color-5x5.qfasl.4'
hcuot "Jun 18 05:37:16 1984" 'fonts/color-cptfont.qfasl.3'
hcuot "Jun 18 05:37:12 1984" 'fonts/color-medfnt.qfasl.4'
hcuot "Jun 18 05:37:07 1984" 'fonts/color-mouse.qfasl.4'
hcuot "Jun 18 05:37:04 1984" 'fonts/courier.qfasl.5'
hcuot "Jul 20 14:18:26 1982" 'fonts/cptfon.qfasl.3'
hcuot "Jun 18 05:37:01 1984" 'fonts/cptfont.qfasl.19'
hcuot "Jun 18 05:36:58 1984" 'fonts/cptfontb.qfasl.7'
hcuot "Jun 18 05:36:54 1984" 'fonts/cyr.qfasl.5'
hcuot "Jun 18 05:36:50 1984" 'fonts/cyr12.qfasl.5'
hcuot "Jun 18 05:36:46 1984" 'fonts/ent.qfasl.5'
hcuot "Dec 10 23:35:35 1983" 'fonts/equivalence.lisp.1'
hcuot "Jun 18 05:36:42 1984" 'fonts/gach10.qfasl.3'
hcuot "Jun 18 05:36:35 1984" 'fonts/gach10b.qfasl.3'
hcuot "Jun 18 05:36:27 1984" 'fonts/gach12.qfasl.3'
hcuot "Jul 20 14:18:41 1982" 'fonts/gfr.archiv.1'
hcuot "Jun 18 05:36:20 1984" 'fonts/hippo10.qfasl.4'
hcuot "Jun 18 05:36:14 1984" 'fonts/hippo18.qfasl.3'
hcuot "Jun 18 05:36:11 1984" 'fonts/hl10.qfasl.9'
hcuot "Jun 18 05:36:07 1984" 'fonts/hl10b.qfasl.9'
hcuot "Jun 18 05:36:03 1984" 'fonts/hl12.qfasl.10'
hcuot "Jun 18 05:35:59 1984" 'fonts/hl12b.qfasl.15'
hcuot "Jun 18 05:35:56 1984" 'fonts/hl12b1.qfasl.3'
hcuot "Jun 18 05:35:52 1984" 'fonts/hl12bi.qfasl.10'
hcuot "Jun 18 05:35:49 1984" 'fonts/hl12i.qfasl.11'
hcuot "Jun 18 05:35:45 1984" 'fonts/hl18.qfasl.6'
hcuot "Jun 18 05:35:42 1984" 'fonts/hl6.qfasl.9'
hcuot "Jun 18 05:35:39 1984" 'fonts/hl7.qfasl.9'
hcuot "Jun 18 05:35:34 1984" 'fonts/icons.qfasl.3'
hcuot "Jun 18 05:35:30 1984" 'fonts/invisible.qfasl.3'
hcuot "Jun 18 05:35:27 1984" 'fonts/medfnb.qfasl.8'
hcuot "Jun 18 05:35:17 1984" 'fonts/medfnt.qfasl.9'
hcuot "Jun 18 05:35:13 1984" 'fonts/mets.qfasl.9'
hcuot "Jun 18 05:35:07 1984" 'fonts/metsi.qfasl.9'
hcuot "Jun 18 05:35:00 1984" 'fonts/mit.qfasl.5'
hcuot "Jun 18 05:34:56 1984" 'fonts/mouse.qfasl.9'
hcuot "Jun 18 05:34:53 1984" 'fonts/narrow.qfasl.5'
hcuot "Jun 18 05:34:50 1984" 'fonts/panes.qfasl.3'
hcuot "Nov 15 12:23:32 1983" 'fonts/prt12b.qfasl.2'
hcuot "Jun 18 05:34:45 1984" 'fonts/s30chs.qfasl.5'
hcuot "Jun 18 05:34:41 1984" 'fonts/s35ger.qfasl.3'
hcuot "Jun 18 05:34:35 1984" 'fonts/sail12.qfasl.6'
hcuot "Jun 18 05:34:32 1984" 'fonts/search.qfasl.9'
hcuot "Jun 18 05:34:28 1984" 'fonts/ship.qfasl.6'
hcuot "Oct  9 11:53:23 1984" 'fonts/storybook.qfasl.1'
hcuot "Oct  9 11:54:06 1984" 'fonts/storybookbold.qfasl.1'
hcuot "Jun 18 05:34:24 1984" 'fonts/tally.qfasl.5'
hcuot "Jul 20 14:20:57 1982" 'fonts/times.9rom.1'
hcuot "Jun 18 05:34:20 1984" 'fonts/tiny.qfasl.5'
hcuot "Jun 18 05:34:15 1984" 'fonts/tog.qfasl.5'
hcuot "Jun 18 05:34:12 1984" 'fonts/tr10.qfasl.9'
hcuot "Jun 18 05:34:09 1984" 'fonts/tr10b.qfasl.8'
hcuot "Jun 18 05:34:04 1984" 'fonts/tr10bi.qfasl.7'
hcuot "Jun 18 05:34:00 1984" 'fonts/tr10i.qfasl.7'
hcuot "Jun 18 05:33:57 1984" 'fonts/tr10ic.qfasl.4'
hcuot "Jun 18 05:33:53 1984" 'fonts/tr12.qfasl.11'
hcuot "Jun 18 05:33:49 1984" 'fonts/tr12b.qfasl.17'
hcuot "Jun 18 05:33:45 1984" 'fonts/tr12b1.qfasl.8'
hcuot "Jun 18 05:33:42 1984" 'fonts/tr12bi.qfasl.9'
hcuot "Jun 18 05:33:37 1984" 'fonts/tr12i.qfasl.13'
hcuot "Jun 18 05:33:30 1984" 'fonts/tr18.qfasl.7'
hcuot "Jun 18 05:33:16 1984" 'fonts/tr18b.qfasl.3'
hcuot "Jun 18 05:33:12 1984" 'fonts/tr8.qfasl.8'
hcuot "Jun 18 05:33:07 1984" 'fonts/tr8b.qfasl.8'
hcuot "Jun 18 05:33:04 1984" 'fonts/tr8i.qfasl.6'
hcuot "Jun 18 05:33:00 1984" 'fonts/tvbug.qfasl.5'
hcuot "Jun 18 05:32:57 1984" 'fonts/tvfont.qfasl.7'
hcuot "Jun 18 05:32:50 1984" 'fonts/worm.qfasl.4'
hcuot "Sep 15 08:28:36 1984" 'io/crdtbl.lisp.35'
hcuot "Aug 31 17:23:39 1984" 'io/crdtbl.qfasl.1'
hcuot "Nov 28 00:27:38 1984" 'io/disk.lisp.292'
hcuot "Nov 21 19:22:39 1984" 'io/disk.qfasl.291'
hcuot "May 20 13:21:35 1984" 'io/dledit.lisp.52'
hcuot "Nov 21 19:34:29 1984" 'io/dledit.qfasl.52'
hcuot "Apr  5 08:37:55 1985" 'io/dribbl.lisp.37'
hcuot "Aug 15 04:10:20 1984" 'io/dribbl.qfasl.36'
hcuot "Aug 30 21:58:54 1984" 'io/find-plausible-partitions.lisp.1'
hcuot "Feb  8 05:25:40 1985" 'io/format.lisp.241'
hcuot "Sep  7 22:25:02 1984" 'io/format.qfasl.234'
hcuot "Jan 22 15:20:39 1983" 'io/format-macro.lisp.2'
hcuot "Aug  3 06:29:26 1984" 'io/format-macro.qfasl.2'
hcuot "Dec 10 12:33:13 1984" 'io/fread.lisp.30'
hcuot "Oct  4 18:25:58 1985" 'io/fread.qfasl.30'
hcuot "Aug 10 08:29:02 1984" 'io/grind.lisp.146'
hcuot "Aug 29 23:06:51 1984" 'io/grind.qfasl.145'
hcuot "Feb 27 02:58:01 1985" 'io/print.lisp.183'
hcuot "Sep 10 07:18:21 1984" 'io/print.qfasl.178'
hcuot "Dec  9 09:44:53 1984" 'io/qio.lisp.217'
hcuot "Aug 31 05:38:38 1984" 'io/qio.qfasl.214'
hcuot "Jun 20 08:21:38 1986" 'io/random-walk.lisp.1'
hcuot "Apr  7 15:18:03 1984" 'io/rcomp.lisp.10'
hcuot "Nov  1 11:32:34 1984" 'io/rddefs.lisp.62'
hcuot "Sep  7 22:28:41 1984" 'io/rddefs.qfasl.61'
hcuot "Sep 15 08:33:22 1984" 'io/rdtbl.lisp.169'
hcuot "Sep 15 08:32:16 1984" 'io/rdtbl.qfasl.167'
hcuot "Nov 20 19:22:01 1984" 'io/read.lisp.437'
hcuot "Aug 15 06:03:40 1984" 'io/read.qfasl.432'
hcuot "Dec  6 10:14:19 1984" 'io/rtc.lisp.47'
hcuot "Sep 10 05:53:33 1984" 'io/rtc.qfasl.46'
hcuot "Jan 27 05:19:23 1984" 'io/simple-ether.lisp.1'
hcuot "Mar  2 05:04:00 1985" 'io/stream.lisp.111'
hcuot "Sep  4 05:40:02 1984" 'io/stream.qfasl.108'
hcuot "Oct 30 05:27:53 1983" 'io/strmdoc.lisp.2'
hcuot "Dec  1 03:13:54 1984" 'io/unibus.lisp.26'
hcuot "Jun 20 06:04:54 1986" 'io/unibus.lisp.27'
hcuot "Aug 15 06:24:34 1984" 'io/unibus.qfasl.25'
hcuot "Nov 29 06:55:23 1984" 'io/access.lisp.13'   # was 'io/file/access.lisp.13'
hcuot "Sep 11 07:23:13 1984" 'io/access.qfasl.8'   # was 'io/file/access.qfasl.8'
hcuot "Oct 25 04:23:11 1983" 'io/baldir.lisp.114'  # was 'io/file/baldir.lisp.114'
hcuot "Aug 15 08:28:24 1984" 'io/baldir.qfasl.114' # was 'io/file/baldir.qfasl.114'
hcuot "Feb 15 05:03:22 1985" 'io/logical.lisp.1'   # was 'io/file/logical.lisp.1'
hcuot "Feb 13 05:15:41 1985" 'io/open.lisp.180'	   # was 'io/file/open.lisp.180'
hcuot "Sep 11 06:36:49 1984" 'io/open.qfasl.174'   # was 'io/file/open.qfasl.174'
hcuot "May 17 07:21:01 1985" 'io/pathnm.lisp.538'  # was 'io/file/pathnm.lisp.538'
hcuot "Sep  7 23:08:16 1984" 'io/pathnm.qfasl.528' # was 'io/file/pathnm.qfasl.528'
hcuot "Feb 24 08:18:10 1985" 'io/pathst.lisp.181'  # was 'io/file/pathst.lisp.181'
hcuot "Sep  7 23:13:26 1984" 'io/pathst.qfasl.173' # was 'io/file/pathst.qfasl.173'
hcuot "Jun 29 10:46:28 1982" 'io1/10leaf.points.1'
hcuot "Mar 17 08:10:51 1983" 'io1/as8748.lisp.40'
hcuot "Sep 16 11:09:14 1982" 'io1/as8751.lisp.29'
hcuot "Apr  7 15:18:57 1984" 'io1/cdrive.lisp.103'
hcuot "Apr  7 15:19:19 1984" 'io1/chatst.lisp.66'
hcuot "Sep 11 21:11:30 1984" 'io1/conver.lisp.147'
hcuot "Sep 11 21:37:37 1984" 'io1/conver.qfasl.147'
hcuot "Jun 29 10:49:20 1982" 'io1/door.bin.1'
hcuot "Jun 29 10:49:15 1982" 'io1/door.text.2'
hcuot "Jan 28 01:44:08 1984" 'io1/dplt.lisp.109'
hcuot "Apr  7 15:21:38 1984" 'io1/draw.lisp.23'
hcuot "Feb 13 01:04:09 1984" 'io1/eftp.bin-4.1'
hcuot "Nov 28 23:44:32 1983" 'io1/eftp.bin-5.1'
hcuot "Mar 31 07:20:29 1985" 'io1/eftp.bin-6.1'
hcuot "Dec 18 20:24:31 1983" 'io1/eftp.lisp.38'
hcuot "Jun  5 06:27:46 1984" 'io1/fntcnv.lisp.83'
hcuot "Aug 30 00:16:54 1984" 'io1/fntcnv.qfasl.83'
hcuot "Apr  7 15:22:34 1984" 'io1/fntdef.lisp.20'
hcuot "Feb  7 05:12:08 1985" 'io1/fquery.lisp.46'
hcuot "Aug  3 06:30:55 1984" 'io1/fquery.qfasl.45'
hcuot "Jun 29 10:50:23 1982" 'io1/hacks.lisp.190'
hcuot "Jun 27 12:05:25 1984" 'io1/hardcopy.lisp.1'
hcuot "Aug 15 05:53:21 1984" 'io1/hardcopy.qfasl.1'
hcuot "Aug 24 16:56:29 1983" 'io1/inc.lisp.8'
hcuot "Aug 15 04:58:05 1984" 'io1/inc.qfasl.8'
hcuot "Jun 13 22:49:50 1984" 'io1/infix.lisp.11'
hcuot "Aug 15 04:59:17 1984" 'io1/infix.qfasl.11'
hcuot "Jun 24 17:23:23 1984" 'io1/meter.lisp.42'
hcuot "Aug 30 03:21:38 1984" 'io1/meter.qfasl.42'
hcuot "Jun 29 10:50:59 1982" 'io1/mouse.text.11'
hcuot "Aug  3 07:01:51 1984" 'io1/output.lisp.38'
hcuot "Sep  7 22:29:43 1984" 'io1/output.qfasl.38'
hcuot "Nov 28 23:41:27 1983" 'io1/press.bin-5.3'
hcuot "Oct 13 05:30:20 1984" 'io1/press.lisp.147'
hcuot "Aug 30 03:16:40 1984" 'io1/press.qfasl.146'
hcuot "Apr  7 15:25:35 1984" 'io1/promp.lisp.13'
hcuot "Aug  3 12:06:58 1984" 'io1/reldmp.lisp.12'
hcuot "Aug  3 22:01:31 1984" 'io1/reldmp.qfasl.12'
hcuot "Apr  7 15:29:25 1984" 'io1/relld.lisp.10'
hcuot "Sep  7 17:09:42 1984" 'io1/rfontw.bin-5.1'
hcuot "Mar 31 07:19:47 1985" 'io1/rfontw.bin-6.1'
hcuot "Sep  6 22:22:47 1984" 'io1/rfontw.lisp.82'
hcuot "Sep  6 22:25:03 1984" 'io1/rfontw.qfasl.82'
hcuot "Nov 28 22:09:46 1983" 'io1/rfontx.lisp.75'
hcuot "Sep  6 22:16:41 1984" 'io1/rfontx.qfasl.75'
hcuot "May 12 06:21:02 1984" 'io1/serial.lisp.32'
hcuot "Aug 15 06:11:14 1984" 'io1/serial.qfasl.32'
hcuot "Jul  2 16:55:54 1984" 'io1/srccom.lisp.37'
hcuot "Aug 30 03:24:08 1984" 'io1/srccom.qfasl.37'
hcuot "Apr 23 11:39:47 1984" 'io1/swar.lisp.12'
hcuot "May 31 12:30:55 1985" 'io1/swar.qfasl.12'
hcuot "Nov  6 12:48:06 1984" 'io1/time.lisp.110'
hcuot "Aug  3 11:46:51 1984" 'io1/time.qfasl.105'
hcuot "Oct 20 19:28:27 1984" 'io1/timpar.lisp.75'
hcuot "Aug  3 11:48:41 1984" 'io1/timpar.qfasl.74'
hcuot "Jun 29 10:55:20 1982" 'io1/ukbd.lisp.24'
hcuot "Jun 29 10:55:35 1982" 'io1/wlr.doc.1'
hcuot "Jul 22 11:15:54 1983" 'io1/xgp.lisp.33'
hcuot "Sep 10 20:20:48 1984" 'io1/xgp.qfasl.33'
hcuot "Aug  1 23:16:45 1982" 'man/.bug.lmman.1'
hcuot "Aug  1 23:17:23 1982" 'man/.dlw.wordab.1'
hcuot "Aug  1 23:17:27 1982" 'man/.forma.text.33'
hcuot "Aug  1 23:17:39 1982" 'man/.machn.compar.1'
hcuot "Aug  1 23:17:42 1982" 'man/30flsp.kst.1'
hcuot "Aug  1 23:17:52 1982" 'man/37vrbl.kst.1'
hcuot "May 20 01:12:20 1984" 'man/areas.text.46'
hcuot "Oct 29 03:30:47 1985" 'man/bug-mail.txt.1'
hcuot "Jul 27 06:52:53 1984" 'man/chaos.text.27'
hcuot "Jun  1 08:44:46 1984" 'man/code.text.36'
hcuot "Jun  4 07:31:51 1984" 'man/compil.text.106'
hcuot "Jun  8 08:41:24 1984" 'man/cumulative.vars.24'
hcuot "Jun  1 07:23:25 1984" 'man/db-aid.text.14'
hcuot "May 21 20:48:55 1984" 'man/debug.text.21'
hcuot "Jun  8 07:14:16 1984" 'man/defstr.text.117'
hcuot "Jun  1 06:32:40 1984" 'man/errors.text.102'
hcuot "May 21 06:53:47 1984" 'man/fd-arr.text.26'
hcuot "May 20 01:12:55 1984" 'man/fd-clo.text.12'
hcuot "May 21 01:20:57 1984" 'man/fd-con.text.28'
hcuot "Jun  5 05:25:24 1984" 'man/fd-dtp.text.19'
hcuot "Jun  1 11:17:42 1984" 'man/fd-eva.text.46'
hcuot "May 21 18:53:05 1984" 'man/fd-fio.text.24'
hcuot "May 19 21:22:03 1984" 'man/fd-flo.text.24'
hcuot "Jun  1 04:27:05 1984" 'man/fd-fun.text.26'
hcuot "Jun  1 09:36:57 1984" 'man/fd-hac.text.47'
hcuot "May 20 23:56:54 1984" 'man/fd-loc.text.9'
hcuot "Jun  8 21:51:15 1984" 'man/fd-num.text.37'
hcuot "May 19 21:22:43 1984" 'man/fd-op.text.5'
hcuot "Jul 27 06:51:28 1984" 'man/fd-sg.text.16'
hcuot "Jun  1 10:34:17 1984" 'man/fd-str.text.27'
hcuot "Jun  1 04:32:03 1984" 'man/fd-sub.text.19'
hcuot "May 20 23:56:59 1984" 'man/fd-sym.text.14'
hcuot "Jun  1 08:45:51 1984" 'man/files.text.24'
hcuot "Aug  1 23:25:37 1982" 'man/flavor.bolio.1'
hcuot "Jun  1 11:34:13 1984" 'man/flavor.text.134'
hcuot "Aug  1 23:25:53 1982" 'man/font3.kst.1'
hcuot "May 19 20:30:31 1984" 'man/generic.text.14'
hcuot "Jun  8 08:08:53 1984" 'man/index.temp.2'
hcuot "May 19 20:56:30 1984" 'man/init.text.17'
hcuot "Jun  4 07:16:55 1984" 'man/intro.text.18'
hcuot "May 21 18:53:12 1984" 'man/ios.text.247'
hcuot "Apr 26 12:50:32 1983" 'man/looptm.lispm.2'
hcuot "Jun  1 04:33:03 1984" 'man/looptm.text.320'
hcuot "May 21 07:37:20 1984" 'man/macros.text.104'
hcuot "May 19 05:54:05 1984" 'man/maksys.text.38'
hcuot "Mar 11 00:43:51 1984" 'man/manual.bolio.25'
hcuot "Jun  1 10:42:32 1984" 'man/manual.fasl.33'
hcuot "Jun  1 10:30:15 1984" 'man/manual.lisp.33'
hcuot "Jun  1 06:40:13 1984" 'man/manual.text.44'
hcuot "Jun  8 08:31:51 1984" 'man/manual.vars.26'
hcuot "Mar 11 00:43:46 1984" 'man/manual2.bolio.2'
hcuot "Jun  8 08:09:29 1984" 'man/manual2.log.6'
hcuot "May 21 20:38:36 1984" 'man/manual2.text.8'
hcuot "Jun  1 06:56:56 1984" 'man/manual2a.10.1'
hcuot "Mar 11 00:43:46 1984" 'man/manual3.bolio.1'
hcuot "Mar 12 05:27:45 1984" 'man/manual3.text.1'
hcuot "Jun  1 06:38:17 1984" 'man/manual3a.text.1'
hcuot "Aug  1 23:28:55 1982" 'man/msg.text.8'
hcuot "Jun  1 06:28:39 1984" 'man/packd.text.106'
hcuot "Jun  1 06:29:53 1984" 'man/patch.text.54'
hcuot "Jun  5 05:31:29 1984" 'man/pathnm.text.99'
hcuot "May 20 01:15:40 1984" 'man/proces.text.55'
hcuot "May 20 06:21:25 1984" 'man/query.text.22'
hcuot "Jun  1 05:25:06 1984" 'man/rdprt.text.29'
hcuot "May 21 01:20:51 1984" 'man/resour.text.28'
hcuot "May 20 06:21:07 1984" 'man/stream.text.37'
hcuot "Oct  7 07:05:57 1982" 'man/testman.bolio.5'
hcuot "Oct  7 02:51:24 1982" 'man/testman.text.2'
hcuot "May 19 20:30:40 1984" 'man/time.text.40'
hcuot "Jun  8 04:11:55 1984" 'man/title.text.11'
hcuot "Jul 11 02:29:09 1984" 'network/addr-res.lisp.8'
hcuot "Jan  2 02:34:32 1984" 'network/ether-mini.lisp.10'
hcuot "Nov 28 11:35:58 1984" 'network/host.lisp.121'
hcuot "Sep 10 22:45:38 1984" 'network/host.qfasl.116'
hcuot "Sep  9 03:54:51 1984" 'network/package.lisp.7'
hcuot "Sep  9 05:59:30 1984" 'network/package.qfasl.7'
hcuot "Jul 15 06:00:45 1984" 'network/regions.lisp.1'
hcuot "May 30 07:15:13 1984" 'network/server.lisp.1'
hcuot "May 30 07:14:54 1984" 'network/service.lisp.3'
hcuot "Jul 13 19:29:53 1984" 'network/simple-ether.lisp.51'
hcuot "Jul 15 06:01:32 1984" 'network/smtp.lisp.1'
hcuot "Jul  4 18:10:43 1984" 'network/symbols.lisp.1'
hcuot "May 30 05:58:43 1984" 'network/symbols.qfasl.1'
hcuot "Dec 14 07:14:16 1984" 'io1/chatst.lisp.67'     # was 'network/chaos/chatst.lisp.67'
hcuot "Jun  6 06:11:05 1984" 'io1/chatst.qfasl.66'    # was 'network/chaos/chatst.qfasl.66'
hcuot "Dec  6 13:20:08 1984" 'io/chsaux.lisp.366'     # was 'network/chaos/chsaux.lisp.366'
hcuot "Aug 15 07:26:28 1984" 'io/chsaux.qfasl.359'    # was 'network/chaos/chsaux.qfasl.359'
hcuot "Mar 12 01:23:31 1985" 'io/chsncp.lisp.270'     # was 'network/chaos/chsncp.lisp.270'
hcuot "Sep 11 21:12:45 1984" 'io/chsncp.qfasl.265'    # was 'network/chaos/chsncp.qfasl.265'
hcuot "Nov 26 20:11:26 1984" 'io/chuse.lisp.14'	      # was 'network/chaos/chuse.lisp.14'
hcuot "Sep  4 21:12:29 1984" 'io/chuse.qfasl.11'      # was 'network/chaos/chuse.qfasl.11'
hcuot "Jun  4 20:53:34 1984" 'io1/eftp.lisp.39'	      # was 'network/chaos/eftp.lisp.39'
hcuot "Jun  6 06:12:57 1984" 'io1/eftp.qfasl.39'      # was 'network/chaos/eftp.qfasl.39'
hcuot "Sep 10 21:59:11 1984" 'window/peekch.lisp.31'  # was 'network/chaos/peekch.lisp.31'
hcuot "Sep 10 22:44:15 1984" 'window/peekch.qfasl.31' # was 'network/chaos/peekch.qfasl.31'
hcuot "Apr 13 05:45:29 1985" 'io/qfile.lisp.360'      # was 'network/chaos/qfile.lisp.360'
hcuot "Sep 10 22:37:50 1984" 'io/qfile.qfasl.353'     # was 'network/chaos/qfile.qfasl.353'
hcuot "Jul 17 15:27:46 1984" 'network/ip/address.lisp.3'
hcuot "Jul 17 15:27:51 1984" 'network/ip/address.qfasl.3'
hcuot "Nov 10 23:02:36 1984" 'network/ip/hostsnic.lisp.4'
hcuot "Nov 16 08:06:05 1984" 'patch/band.win.lisp.2'
hcuot "Nov 16 08:06:21 1984" 'patch/band.win.qfasl.2'
hcuot "Sep 11 23:19:54 1984" 'patch/cadr.patch-directory.1'
hcuot "Oct 22 19:47:31 1985" 'patch/cadr-4.patch-directory.12'
hcuot "Jan 28 07:01:11 1985" 'patch/cadr-4-1.lisp.8'
hcuot "Jan 28 07:01:19 1985" 'patch/cadr-4-1.qfasl.8'
hcuot "Jan  3 20:09:24 1985" 'patch/cadr-4-2.lisp.1'
hcuot "Jan  3 20:10:46 1985" 'patch/cadr-4-2.qfasl.1'
hcuot "Oct 22 19:45:59 1985" 'patch/cadr-4-3.lisp.1'
hcuot "Oct 22 19:46:16 1985" 'patch/cadr-4-3.qfasl.1'
hcuot "Nov 16 08:09:19 1984" 'patch/lm27fix.lisp.1'
hcuot "Nov 16 08:09:37 1984" 'patch/lm27fix.qfasl.1'
hcuot "Jun  7 00:30:00 1984" 'patch/system.patch-directory.25'
hcuot "Feb  4 03:12:33 1984" 'patch/system-94.patch-directory.129'
hcuot "Aug 21 21:52:31 1983" 'patch/system-94-41.qfasl.2'
hcuot "Nov  8 09:03:14 1983" 'patch/system-94-42.qfasl.1'
hcuot "Nov 13 05:20:58 1983" 'patch/system-94-43.qfasl.2'
hcuot "Nov 30 02:11:39 1983" 'patch/system-97.patch-directory.76'
hcuot "Nov  9 22:51:17 1983" 'patch/system-97-25.qfasl.1'
hcuot "Nov 11 20:27:52 1983" 'patch/system-97-26.qfasl.1'
hcuot "Nov 29 23:17:29 1983" 'patch/system-97-27.qfasl.1'
hcuot "Nov 30 02:11:04 1983" 'patch/system-97-28.qfasl.1'
hcuot "Nov 28 16:02:04 1984" 'patch/system-98.patch-directory.304'
hcuot "Nov 24 00:42:57 1983" 'patch/system-98-1.lisp.5'
hcuot "Dec 23 08:14:14 1983" 'patch/system-98-10.lisp.15'
hcuot "Dec 26 09:56:51 1983" 'patch/system-98-11.lisp.19'
hcuot "Dec 27 07:15:02 1983" 'patch/system-98-12.lisp.16'
hcuot "Dec 24 06:37:44 1983" 'patch/system-98-13.lisp.4'
hcuot "Dec 27 08:58:42 1983" 'patch/system-98-14.lisp.15'
hcuot "Dec 28 10:09:13 1983" 'patch/system-98-15.lisp.7'
hcuot "Dec 29 10:08:59 1983" 'patch/system-98-16.lisp.6'
hcuot "Jan  1 05:30:38 1984" 'patch/system-98-17.lisp.18'
hcuot "Jan  1 15:49:39 1984" 'patch/system-98-18.lisp.10'
hcuot "Jan  3 05:54:10 1984" 'patch/system-98-19.lisp.20'
hcuot "Nov 30 05:36:10 1983" 'patch/system-98-2.lisp.12'
hcuot "Jan  2 07:53:30 1984" 'patch/system-98-20.lisp.4'
hcuot "Jan  3 06:16:42 1984" 'patch/system-98-21.lisp.2'
hcuot "Jan  3 09:49:01 1984" 'patch/system-98-22.lisp.6'
hcuot "Jan  4 10:19:59 1984" 'patch/system-98-23.lisp.10'
hcuot "Jan  3 10:57:14 1984" 'patch/system-98-24.lisp.3'
hcuot "Jan  5 23:39:41 1984" 'patch/system-98-25.lisp.8'
hcuot "Jan  7 12:40:39 1984" 'patch/system-98-26.lisp.6'
hcuot "Jan 12 16:11:12 1984" 'patch/system-98-27.lisp.7'
hcuot "Jan 10 02:43:59 1984" 'patch/system-98-28.lisp.3'
hcuot "Jan 15 03:33:30 1984" 'patch/system-98-29.lisp.12'
hcuot "Dec  6 14:55:14 1983" 'patch/system-98-3.lisp.16'
hcuot "Jan 30 04:20:17 1984" 'patch/system-98-30.lisp.22'
hcuot "Feb  1 10:45:59 1984" 'patch/system-98-31.lisp.18'
hcuot "Jan 27 09:08:05 1984" 'patch/system-98-32.lisp.8'
hcuot "Feb  8 17:54:19 1984" 'patch/system-98-33.lisp.26'
hcuot "Feb  1 13:00:29 1984" 'patch/system-98-34.lisp.1'
hcuot "Feb 15 23:07:07 1984" 'patch/system-98-35.lisp.9'
hcuot "Feb  3 09:30:52 1984" 'patch/system-98-36.lisp.1'
hcuot "Feb 22 23:51:54 1984" 'patch/system-98-37.lisp.10'
hcuot "Mar 12 01:18:12 1984" 'patch/system-98-38.lisp.4'
hcuot "Mar 24 20:37:28 1984" 'patch/system-98-39.lisp.20'
hcuot "Dec  5 14:56:31 1983" 'patch/system-98-4.lisp.7'
hcuot "Apr  3 12:57:33 1984" 'patch/system-98-40.lisp.43'
hcuot "Apr  6 23:33:48 1984" 'patch/system-98-41.lisp.10'
hcuot "Mar 21 21:54:25 1984" 'patch/system-98-42.lisp.2'
hcuot "Mar 15 09:21:58 1984" 'patch/system-98-43.lisp.1'
hcuot "Apr 17 21:47:49 1984" 'patch/system-98-44.lisp.22'
hcuot "Apr 21 23:27:40 1984" 'patch/system-98-45.lisp.5'
hcuot "Apr  6 09:35:55 1984" 'patch/system-98-46.lisp.1'
hcuot "May  8 10:23:06 1984" 'patch/system-98-47.lisp.37'
hcuot "Apr 18 08:16:58 1984" 'patch/system-98-48.lisp.1'
hcuot "May 29 18:44:39 1984" 'patch/system-98-49.lisp.8'
hcuot "Dec  9 04:28:54 1983" 'patch/system-98-5.lisp.11'
hcuot "Jun  6 00:30:09 1984" 'patch/system-98-50.lisp.39'
hcuot "Jun  6 00:34:15 1984" 'patch/system-98-50.qfasl.39'
hcuot "May  1 11:40:21 1984" 'patch/system-98-51.lisp.1'
hcuot "May  1 11:40:31 1984" 'patch/system-98-51.qfasl.1'
hcuot "May 10 09:26:54 1984" 'patch/system-98-52.lisp.1'
hcuot "May 10 09:27:04 1984" 'patch/system-98-52.qfasl.1'
hcuot "May 12 08:33:07 1984" 'patch/system-98-53.lisp.2'
hcuot "May 12 08:33:12 1984" 'patch/system-98-53.qfasl.2'
hcuot "May 23 04:29:09 1984" 'patch/system-98-54.lisp.4'
hcuot "May 23 04:29:30 1984" 'patch/system-98-54.qfasl.4'
hcuot "May 28 07:59:01 1984" 'patch/system-98-55.lisp.3'
hcuot "May 28 07:59:51 1984" 'patch/system-98-55.qfasl.3'
hcuot "May 22 02:26:36 1984" 'patch/system-98-56.lisp.2'
hcuot "May 22 02:26:50 1984" 'patch/system-98-56.qfasl.2'
hcuot "Jun  4 19:45:26 1984" 'patch/system-98-57.lisp.23'
hcuot "Jun  4 20:08:38 1984" 'patch/system-98-57.qfasl.23'
hcuot "May 25 01:26:35 1984" 'patch/system-98-58.lisp.1'
hcuot "May 25 01:26:44 1984" 'patch/system-98-58.qfasl.1'
hcuot "Jun  5 21:11:43 1984" 'patch/system-98-59.lisp.3'
hcuot "Jun  5 21:11:53 1984" 'patch/system-98-59.qfasl.3'
hcuot "Dec 13 22:06:04 1983" 'patch/system-98-6.lisp.17'
hcuot "Jun 13 11:08:07 1984" 'patch/system-98-60.lisp.5'
hcuot "Jun 13 11:08:13 1984" 'patch/system-98-60.qfasl.5'
hcuot "Jun  9 10:10:30 1984" 'patch/system-98-61.lisp.2'
hcuot "Jun  9 10:10:39 1984" 'patch/system-98-61.qfasl.2'
hcuot "Jun 17 10:05:51 1984" 'patch/system-98-62.lisp.12'
hcuot "Jun 20 00:27:54 1984" 'patch/system-98-62.qfasl.12'
hcuot "Jul  2 21:43:53 1984" 'patch/system-98-63.lisp.18'
hcuot "Jul  2 21:44:01 1984" 'patch/system-98-63.qfasl.18'
hcuot "Jun 15 10:19:50 1984" 'patch/system-98-64.lisp.1'
hcuot "Jun 29 09:07:30 1984" 'patch/system-98-64.qfasl.1'
hcuot "Jul  2 21:55:49 1984" 'patch/system-98-65.lisp.10'
hcuot "Jul  2 21:56:02 1984" 'patch/system-98-65.qfasl.10'
hcuot "Jul  9 21:08:09 1984" 'patch/system-98-66.lisp.9'
hcuot "Jul  9 21:08:19 1984" 'patch/system-98-66.qfasl.9'
hcuot "Jun 29 09:22:38 1984" 'patch/system-98-67.lisp.1'
hcuot "Jun 29 09:22:46 1984" 'patch/system-98-67.qfasl.1'
hcuot "Jul 18 18:57:10 1984" 'patch/system-98-68.lisp.5'
hcuot "Jul 18 18:57:21 1984" 'patch/system-98-68.qfasl.5'
hcuot "Aug 14 20:39:14 1984" 'patch/system-98-69.lisp.1'
hcuot "Aug 14 20:39:24 1984" 'patch/system-98-69.qfasl.1'
hcuot "Dec 16 03:40:10 1983" 'patch/system-98-7.lisp.7'
hcuot "Aug 29 15:25:48 1984" 'patch/system-98-70.lisp.1'
hcuot "Aug 29 15:25:55 1984" 'patch/system-98-70.qfasl.1'
hcuot "Oct 13 01:02:56 1984" 'patch/system-98-71.lisp.3'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-71.qfasl.3'
hcuot "Oct 14 21:03:15 1984" 'patch/system-98-72.lisp.2'
hcuot "Oct 14 21:03:36 1984" 'patch/system-98-72.qfasl.2'
hcuot "Oct 11 08:17:25 1984" 'patch/system-98-73.lisp.2'
hcuot "Oct 11 14:50:01 1984" 'patch/system-98-73.qfasl.2'
hcuot "Oct 13 01:03:51 1984" 'patch/system-98-74.lisp.1'
hcuot "Oct 13 01:04:03 1984" 'patch/system-98-74.qfasl.1'
hcuot "Oct 14 06:56:53 1984" 'patch/system-98-75.lisp.1'
hcuot "Oct 14 06:57:21 1984" 'patch/system-98-75.qfasl.1'
hcuot "Oct 14 20:58:47 1984" 'patch/system-98-76.lisp.1'
hcuot "Oct 14 20:59:14 1984" 'patch/system-98-76.qfasl.1'
hcuot "Oct 20 18:08:35 1984" 'patch/system-98-77.lisp.1'
hcuot "Nov 12 13:24:48 1984" 'patch/system-98-77.qfasl.1'
hcuot "Nov 12 12:57:12 1984" 'patch/system-98-78.lisp.5'
hcuot "Nov 12 12:57:43 1984" 'patch/system-98-78.qfasl.5'
hcuot "Nov 20 16:35:13 1984" 'patch/system-98-79.lisp.8'
hcuot "Nov 21 00:34:09 1984" 'patch/system-98-79.ncp.3'
hcuot "Nov 20 16:35:23 1984" 'patch/system-98-79.qfasl.8'
hcuot "Nov 20 23:51:27 1984" 'patch/system-98-79-chsncp.lisp.3'
hcuot "Dec 18 01:24:01 1983" 'patch/system-98-8.lisp.12'
hcuot "Nov 26 21:21:29 1984" 'patch/system-98-80.lisp.1'
hcuot "Nov 26 21:21:56 1984" 'patch/system-98-80.qfasl.1'
hcuot "Nov 28 15:58:45 1984" 'patch/system-98-81.lisp.2'
hcuot "Nov 28 15:59:12 1984" 'patch/system-98-81.qfasl.2'
hcuot "Dec 22 17:18:46 1983" 'patch/system-98-9.lisp.9'
hcuot "Oct 14 15:17:53 1984" 'patch/system-98-9.qfasl.9'
hcuot "Apr 28 19:38:10 1987" 'patch/system-99.patch-directory.131'
hcuot "Apr 28 19:41:53 1987" 'patch/system-99.patch-directory.132'
hcuot "Sep 12 18:29:34 1984" 'patch/system-99-1.lisp.3'
hcuot "Sep 12 18:30:01 1984" 'patch/system-99-1.qfasl.3'
hcuot "Nov  9 21:17:06 1984" 'patch/system-99-10.lisp.31'
hcuot "Nov  9 21:32:38 1984" 'patch/system-99-10.qfasl.31'
hcuot "Nov 14 12:08:03 1984" 'patch/system-99-11.lisp.14'
hcuot "Nov 14 12:08:43 1984" 'patch/system-99-11.qfasl.14'
hcuot "Dec  2 01:02:57 1984" 'patch/system-99-12.lisp.44'
hcuot "Dec  2 01:18:35 1984" 'patch/system-99-12.qfasl.44'
hcuot "Dec  4 19:45:55 1984" 'patch/system-99-13.lisp.17'
hcuot "Dec  4 19:46:23 1984" 'patch/system-99-13.qfasl.17'
hcuot "Dec 14 13:48:14 1984" 'patch/system-99-14.lisp.25'
hcuot "Dec 14 13:48:35 1984" 'patch/system-99-14.qfasl.25'
hcuot "Dec 14 15:09:41 1984" 'patch/system-99-15.lisp.7'
hcuot "Dec 14 15:59:42 1984" 'patch/system-99-15.qfasl.7'
hcuot "Jan 28 06:53:36 1985" 'patch/system-99-16.lisp.3'
hcuot "Jan 28 06:53:43 1985" 'patch/system-99-16.qfasl.3'
hcuot "Feb 12 02:32:11 1985" 'patch/system-99-17.lisp.11'
hcuot "Feb 12 02:32:27 1985" 'patch/system-99-17.qfasl.11'
hcuot "Feb 12 02:41:59 1985" 'patch/system-99-18.lisp.36'
hcuot "Feb 12 02:42:16 1985" 'patch/system-99-18.qfasl.36'
hcuot "Feb 17 09:34:09 1985" 'patch/system-99-19.lisp.26'
hcuot "Feb 17 10:27:41 1985" 'patch/system-99-19.qfasl.26'
hcuot "Sep 12 18:28:00 1984" 'patch/system-99-2.lisp.2'
hcuot "Sep 12 18:28:11 1984" 'patch/system-99-2.qfasl.2'
hcuot "Feb 15 08:05:58 1985" 'patch/system-99-20.lisp.6'
hcuot "Feb 15 08:06:09 1985" 'patch/system-99-20.qfasl.6'
hcuot "Feb 18 16:45:52 1985" 'patch/system-99-21.lisp.17'
hcuot "Feb 27 19:42:42 1985" 'patch/system-99-21.qfasl.17'
hcuot "Feb 28 16:07:50 1985" 'patch/system-99-22.lisp.14'
hcuot "Feb 28 16:08:05 1985" 'patch/system-99-22.qfasl.14'
hcuot "May 17 07:06:11 1985" 'patch/system-99-23.lisp.13'
hcuot "May 17 07:06:19 1985" 'patch/system-99-23.qfasl.13'
hcuot "May 17 06:59:53 1985" 'patch/system-99-24.lisp.7'
hcuot "May 17 07:25:44 1985" 'patch/system-99-24.qfasl.7'
hcuot "May  3 02:50:42 1985" 'patch/system-99-25.lisp.7'
hcuot "May  3 02:51:00 1985" 'patch/system-99-25.qfasl.7'
hcuot "May  3 03:50:29 1985" 'patch/system-99-26.lisp.5'
hcuot "May  3 03:50:39 1985" 'patch/system-99-26.qfasl.5'
hcuot "May 17 07:23:15 1985" 'patch/system-99-27.lisp.4'
hcuot "May 17 07:23:21 1985" 'patch/system-99-27.qfasl.4'
hcuot "May 17 07:05:54 1985" 'patch/system-99-28.lisp.1'
hcuot "Dec  8 22:53:28 1986" 'patch/system-99-28.qfasl.1'
hcuot "Sep 10 08:08:44 1985" 'patch/system-99-29.lisp.8'
hcuot "Jul 18 21:48:17 1986" 'patch/system-99-29.lisp.9'
hcuot "Jul 18 21:48:32 1986" 'patch/system-99-29.qfasl.9'
hcuot "Sep 15 02:40:56 1984" 'patch/system-99-3.lisp.5'
hcuot "Sep 15 02:41:07 1984" 'patch/system-99-3.qfasl.5'
hcuot "Jul 18 21:22:29 1986" 'patch/system-99-30.lisp.1'
hcuot "Jul 18 22:02:59 1986" 'patch/system-99-30.lisp.2'
hcuot "Jul 18 22:03:06 1986" 'patch/system-99-30.qfasl.2'
hcuot "Dec  8 18:26:00 1986" 'patch/system-99-31.lisp.1'
hcuot "Dec  8 18:26:07 1986" 'patch/system-99-31.qfasl.1'
hcuot "Apr 28 19:41:16 1987" 'patch/system-99-32.lisp.1'
hcuot "Apr 28 19:41:21 1987" 'patch/system-99-32.qfasl.1'
hcuot "Sep 26 15:23:43 1984" 'patch/system-99-4.lisp.6'
hcuot "Sep 26 15:23:50 1984" 'patch/system-99-4.qfasl.6'
hcuot "Sep 26 19:13:08 1984" 'patch/system-99-5.lisp.10'
hcuot "Sep 26 19:13:19 1984" 'patch/system-99-5.qfasl.10'
hcuot "Sep 29 13:04:03 1984" 'patch/system-99-6.lisp.3'
hcuot "Sep 29 13:04:07 1984" 'patch/system-99-6.qfasl.3'
hcuot "Oct 16 07:25:45 1984" 'patch/system-99-7.lisp.10'
hcuot "Oct 16 07:26:00 1984" 'patch/system-99-7.qfasl.10'
hcuot "Oct 16 13:48:46 1984" 'patch/system-99-8.lisp.9'
hcuot "Oct 16 13:49:03 1984" 'patch/system-99-8.qfasl.9'
hcuot "Oct 23 23:36:34 1984" 'patch/system-99-9.lisp.16'
hcuot "Oct 23 23:37:01 1984" 'patch/system-99-9.qfasl.16'
hcuot "Sep  9 23:46:32 1984" 'patch/zmail.patch-directory.3'
hcuot "Mar 17 02:13:01 1985" 'patch/zmail-54.patch-directory.10'
hcuot "Sep 26 07:22:03 1984" 'patch/zmail-54-1.lisp.2'
hcuot "Sep 26 07:22:09 1984" 'patch/zmail-54-1.qfasl.2'
hcuot "Oct 14 10:12:26 1984" 'patch/zmail-54-2.lisp.1'
hcuot "Oct 14 10:12:30 1984" 'patch/zmail-54-2.qfasl.1'
hcuot "Nov 30 06:36:29 1984" 'patch/zmail-54-3.lisp.1'
hcuot "Nov 30 06:36:35 1984" 'patch/zmail-54-3.qfasl.1'
hcuot "Mar 17 02:13:18 1985" 'patch/zmail-54-4.lisp.3'
hcuot "Mar 17 02:13:25 1985" 'patch/zmail-54-4.qfasl.3'
hcuot "Nov 13 04:40:20 1984" 'site/-read-.-me-.1'
hcuot "Jan 10 19:02:06 1985" 'site/hsttbl.lisp.124'
hcuot "May  3 23:47:46 1985" 'site/hsttbl.qfasl.124'
hcuot "Sep 20 16:57:24 1985" 'site/lmlocs.lisp.162'
hcuot "Jan 25 00:56:53 1985" 'site/lmlocs.qbin.1'
hcuot "May  3 23:47:12 1985" 'site/lmlocs.qfasl.161'
hcuot "Jan 10 18:44:03 1985" 'site/site.lisp.1'
hcuot "May  3 23:46:36 1985" 'site/site.qfasl.1'
hcuot "Jun 29 08:32:33 1982" 'sys/-read-.-this-.1'
hcuot "Dec 11 12:29:53 1984" 'sys/cadrlp.lisp.152'
hcuot "Oct  4 18:13:47 1985" 'sys/cadrlp.qfasl.152'
hcuot "Apr  7 15:44:05 1984" 'sys/cadsym.lisp.25'
hcuot "Jun 16 21:47:26 1984" 'sys/cdmp.lisp.52'
hcuot "Oct  4 18:24:42 1985" 'sys/cdmp.qfasl.52'
hcuot "Feb 28 15:05:47 1985" 'sys/clpack.lisp.153'
hcuot "Sep 10 07:09:12 1984" 'sys/clpack.qfasl.151'
hcuot "Apr  7 15:45:18 1984" 'sys/compat.lisp.32'
hcuot "Jul  1 18:34:34 1985" 'sys/eval.lisp.97'
hcuot "Sep  9 05:39:13 1984" 'sys/eval.qfasl.78'
hcuot "Feb 12 10:14:08 1985" 'sys/fspec.lisp.1'
hcuot "Jan 30 17:13:11 1985" 'sys/genric.lisp.33'
hcuot "Jan 30 17:13:36 1985" 'sys/genric.qfasl.33'
hcuot "Feb 13 15:44:30 1985" 'sys/ltop.lisp.498'
hcuot "Sep 11 07:20:26 1984" 'sys/ltop.qfasl.494'
hcuot "Oct 24 03:41:44 1982" 'sys/ma.lisp.305'
hcuot "Aug  1 22:39:34 1984" 'sys/ma.qfasl.305'
hcuot "Jun 29 08:36:30 1982" 'sys/madefs.lisp.7'
hcuot "Jul 30 03:18:23 1984" 'sys/madefs.qfasl.7'
hcuot "Oct 13 00:33:14 1983" 'sys/maopt.lisp.4'
hcuot "Aug  1 22:44:12 1984" 'sys/maopt.qfasl.4'
hcuot "Nov 16 09:21:33 1983" 'sys/mc.lisp.354'
hcuot "Aug  1 22:45:44 1984" 'sys/mc.qfasl.354'
hcuot "Jan  4 00:08:41 1983" 'sys/mlap.lisp.51'
hcuot "Aug  1 22:48:15 1984" 'sys/mlap.qfasl.51'
hcuot "Jun 25 23:40:50 1983" 'sys/pack4.lisp.286'
hcuot "Jan 30 12:11:13 1985" 'sys/qcdefs.lisp.153'
hcuot "Sep  9 20:00:57 1984" 'sys/qcdefs.qfasl.149'
hcuot "Sep 10 23:05:15 1984" 'sys/qcfasd.lisp.248'
hcuot "Sep 10 23:05:43 1984" 'sys/qcfasd.qfasl.248'
hcuot "Jan 30 16:48:52 1985" 'sys/qcfile.lisp.324'
hcuot "Sep  7 02:12:10 1984" 'sys/qcfile.qfasl.322'
hcuot "Sep  8 23:30:16 1984" 'sys/qclap.lisp.244'
hcuot "Sep  9 20:16:55 1984" 'sys/qclap.qfasl.244'
hcuot "Aug 30 13:51:41 1984" 'sys/qcluke.lisp.26'
hcuot "Aug 30 20:38:31 1984" 'sys/qcluke.qfasl.26'
hcuot "Nov  6 13:41:16 1984" 'sys/qcopt.lisp.137'
hcuot "Sep  9 20:13:24 1984" 'sys/qcopt.qfasl.133'
hcuot "Dec 11 19:38:25 1984" 'sys/qcp1.lisp.573'
hcuot "Sep  9 20:03:10 1984" 'sys/qcp1.qfasl.562'
hcuot "Oct 28 21:41:46 1984" 'sys/qcp2.lisp.261'
hcuot "Sep  9 20:09:07 1984" 'sys/qcp2.qfasl.259'
hcuot "Aug  3 03:31:47 1984" 'sys/qcpeep.lisp.36'
hcuot "Aug  3 03:31:56 1984" 'sys/qcpeep.qfasl.36'
hcuot "Jan 11 21:27:40 1984" 'sys/qev.lisp.289'
hcuot "Feb 26 10:27:46 1985" 'sys/qfasl.lisp.463'
hcuot "Aug 15 05:35:35 1984" 'sys/qfasl.qfasl.461'
hcuot "Nov 17 01:51:17 1984" 'sys/qfctns.lisp.774'
hcuot "Aug 31 17:59:24 1984" 'sys/qfctns.qfasl.769'
hcuot "Dec 14 08:04:10 1984" 'sys/qmisc.lisp.659'
hcuot "Aug 31 22:20:48 1984" 'sys/qmisc.qfasl.652'
hcuot "Apr  3 15:55:26 1984" 'sys/qnew.lisp.20'
hcuot "Aug 15 05:54:18 1984" 'sys/qnew.qfasl.20'
hcuot "Jul  9 20:51:08 1985" 'sys/qrand.lisp.412'
hcuot "Sep  4 23:45:01 1984" 'sys/qrand.qfasl.408'
hcuot "Dec 11 14:56:51 1984" 'sys/qwmcr.lisp.22'
hcuot "Dec 11 14:56:58 1984" 'sys/qwmcr.qfasl.22'
hcuot "Jun 18 08:57:37 1983" 'sys/recom.lisp.33'
hcuot "Aug 27 08:28:30 1982" 'sys/sgfctn.lisp.57'
hcuot "Aug 15 06:13:05 1984" 'sys/sgfctn.qfasl.57'
hcuot "Oct 10 07:20:38 1983" 'sys/sort.lisp.59'
hcuot "Aug 15 06:13:45 1984" 'sys/sort.qfasl.59'
hcuot "May  3 03:12:41 1985" 'sys/sysdcl.lisp.193'
hcuot "Oct  4 17:41:35 1985" 'sys/sysdcl.qfasl.193'
hcuot "Jan 29 02:23:45 1985" 'sys/types.lisp.72'
hcuot "Sep  9 03:39:45 1984" 'sys/types.qfasl.69'
hcuot "Jun 29 08:49:58 1982" 'sys/ucinit.qfasl.1'
hcuot "Feb 18 08:47:48 1985" 'sys2/advise.lisp.38'
hcuot "Aug 15 03:43:06 1984" 'sys2/advise.qfasl.37'
hcuot "Feb 24 11:42:25 1985" 'sys2/analyze.lisp.19'
hcuot "Sep 11 07:03:49 1984" 'sys2/analyze.qfasl.17'
hcuot "Jul 27 08:09:35 1984" 'sys2/band.lisp.44'
hcuot "Nov 24 08:31:51 1984" 'sys2/band.qfasl.46'
hcuot "Feb  4 07:18:04 1985" 'sys2/character.lisp.22'
hcuot "Sep  7 22:04:06 1984" 'sys2/character.qfasl.20'
hcuot "Jun 15 05:56:23 1984" 'sys2/class.lisp.99'
hcuot "Sep  4 21:31:45 1984" 'sys2/class.qfasl.99'
hcuot "Aug 24 10:12:36 1984" 'sys2/clmac.lisp.4'
hcuot "Aug 29 03:55:00 1984" 'sys2/clmac.qfasl.4'
hcuot "Apr  7 15:50:06 1984" 'sys2/cmany.lisp.46'
hcuot "Apr  7 15:50:53 1984" 'sys2/condit.lisp.2'
hcuot "Feb 13 16:25:14 1985" 'sys2/defmac.lisp.80'
hcuot "Aug 29 21:55:29 1984" 'sys2/defmac.qfasl.78'
hcuot "Aug 29 02:45:38 1984" 'sys2/defsel.lisp.70'
hcuot "Aug 29 07:26:03 1984" 'sys2/defsel.qfasl.70'
hcuot "Feb 15 07:49:03 1985" 'sys2/describe.lisp.3'
hcuot "Dec 14 13:05:57 1984" 'sys2/disass.lisp.94'
hcuot "Aug  1 22:38:35 1984" 'sys2/disass.qfasl.92'
hcuot "Nov 28 15:50:50 1984" 'sys2/encaps.lisp.28'
hcuot "Aug 15 04:10:54 1984" 'sys2/encaps.qfasl.27'
hcuot "Feb 11 06:03:31 1985" 'sys2/flavor.lisp.283'
hcuot "Sep 11 05:12:37 1984" 'sys2/flavor.qfasl.280'
hcuot "Dec 14 09:01:18 1984" 'sys2/gc.lisp.174'
hcuot "Aug 15 04:34:41 1984" 'sys2/gc.qfasl.169'
hcuot "Mar  2 14:03:29 1985" 'sys2/hash.lisp.89'
hcuot "Aug 15 04:51:05 1984" 'sys2/hash.qfasl.87'
hcuot "Mar  6 06:23:07 1985" 'sys2/hashfl.lisp.33'
hcuot "Aug 15 04:52:39 1984" 'sys2/hashfl.qfasl.29'
hcuot "Apr  7 15:51:47 1984" 'sys2/let.lisp.8'
hcuot "May  3 03:13:45 1985" 'sys2/lmmac.lisp.389'
hcuot "Aug 31 21:54:02 1984" 'sys2/lmmac.qfasl.372'
hcuot "Sep  4 02:04:30 1984" 'sys2/login.lisp.87'
hcuot "Sep  4 03:06:25 1984" 'sys2/login.qfasl.87'
hcuot "Dec  9 06:37:37 1984" 'sys2/loop.lisp.829'
hcuot "Oct 24 08:03:31 1984" 'sys2/loop.qfasl.799'
hcuot "May  3 03:54:59 1985" 'sys2/macarr.lisp.2'
hcuot "May  3 03:55:13 1985" 'sys2/macarr.qfasl.2'
hcuot "Sep 13 22:53:32 1984" 'sys2/maksys.lisp.180'
hcuot "Sep  4 22:16:36 1984" 'sys2/maksys.qfasl.178'
hcuot "Apr  9 18:08:41 1984" 'sys2/matrix.lisp.26'
hcuot "Aug 30 03:20:25 1984" 'sys2/matrix.qfasl.26'
hcuot "Sep  4 21:35:26 1984" 'sys2/meth.lisp.63'
hcuot "Sep  4 21:35:37 1984" 'sys2/meth.qfasl.63'
hcuot "Oct  6 11:43:45 1984" 'sys2/numdef.lisp.12'
hcuot "Sep 10 22:30:29 1984" 'sys2/numdef.qfasl.11'
hcuot "Dec 14 08:58:42 1984" 'sys2/numer.lisp.62'
hcuot "Sep 10 22:32:20 1984" 'sys2/numer.qfasl.60'
hcuot "May  2 01:02:09 1986" 'sys2/patch.lisp.166'
hcuot "Dec  8 19:41:50 1986" 'sys2/patch.lisp.167'
hcuot "Aug 15 05:22:21 1984" 'sys2/patch.qfasl.158'
hcuot "Aug 30 00:33:36 1984" 'sys2/plane.lisp.32'
hcuot "Aug 30 00:45:45 1984" 'sys2/plane.qfasl.32'
hcuot "Feb 13 15:50:42 1985" 'sys2/proces.lisp.159'
hcuot "Aug 15 05:28:32 1984" 'sys2/proces.qfasl.157'
hcuot "Feb 13 15:59:15 1985" 'sys2/prodef.lisp.49'
hcuot "Aug 31 19:59:54 1984" 'sys2/prodef.qfasl.48'
hcuot "Dec  6 01:41:34 1984" 'sys2/qtrace.lisp.152'
hcuot "Sep  4 21:43:34 1984" 'sys2/qtrace.qfasl.151'
hcuot "Sep  4 22:08:45 1984" 'sys2/rat.lisp.46'
hcuot "Sep 10 22:47:13 1984" 'sys2/rat.qfasl.46'
hcuot "Nov 10 09:26:41 1984" 'sys2/resour.lisp.31'
hcuot "Aug 15 06:08:50 1984" 'sys2/resour.qfasl.28'
hcuot "Feb 13 14:19:42 1985" 'sys2/selev.lisp.24'
hcuot "Aug 29 03:54:21 1984" 'sys2/selev.qfasl.23'
hcuot "Oct 29 09:56:33 1984" 'sys2/setf.lisp.97'
hcuot "Aug 31 18:48:05 1984" 'sys2/setf.qfasl.95'
hcuot "Feb  7 06:26:28 1985" 'sys2/sgdefs.lisp.57'
hcuot "Aug 15 03:40:43 1984" 'sys2/sgdefs.qfasl.54'
hcuot "Sep 26 08:01:17 1984" 'sys2/step.lisp.72'
hcuot "Aug 15 06:15:30 1984" 'sys2/step.qfasl.70'
hcuot "Sep 25 07:20:50 1984" 'sys2/string.lisp.147'
hcuot "Sep 10 07:15:39 1984" 'sys2/string.qfasl.146'
hcuot "Jul 31 23:42:25 1984" 'sys2/struct.lisp.322'
hcuot "Aug 14 22:20:04 1984" 'sys2/struct.qfasl.322'
hcuot "Oct  9 13:52:31 1984" 'sys2/unfasl.lisp.19'
hcuot "Sep 11 07:52:22 1984" 'sys2/unfasl.qfasl.18'
hcuot "Oct 22 20:07:22 1985" 'sys2/usymld.lisp.188'
hcuot "Oct 22 20:07:43 1985" 'sys2/usymld.qfasl.188'
hcuot "Feb 16 13:56:19 1984" 'tape/copy.lisp.133'
hcuot "Jan  3 09:50:47 1984" 'tape/copy.qfasl.128'
hcuot "May 12 05:49:07 1984" 'tape/ddoc.text.8'
hcuot "May 12 05:29:43 1984" 'tape/fdump.lisp.27'
hcuot "May 12 05:52:14 1984" 'tape/fdump-def.lisp.12'
hcuot "Jan  2 09:37:10 1984" 'tape/fdump-def.qfasl.1'
hcuot "Jan 10 04:12:46 1984" 'tape/fdump-file-cdate-i.lisp.2'
hcuot "Jan 19 16:26:13 1984" 'tape/fdump-file-cdate-i.qfasl.2'
hcuot "May 12 05:29:45 1984" 'tape/fdump-r.lisp.5'
hcuot "Jan  3 10:36:11 1984" 'tape/magtape.directory.11'
hcuot "Oct 26 20:41:54 1983" 'tape/magtape-14.directory.14'
hcuot "Mar  8 06:56:47 1983" 'tape/magtape-14-1.qfasl.1'
hcuot "Apr 25 09:51:48 1983" 'tape/magtape-14-3.qfasl.1'
hcuot "May 19 04:11:34 1983" 'tape/magtape-14-4.qfasl.3'
hcuot "Oct 26 20:41:16 1983" 'tape/magtape-14-5.qfasl.1'
hcuot "Feb 16 14:24:04 1984" 'tape/magtape-22.directory.13'
hcuot "Jan  7 22:40:45 1984" 'tape/magtape-22-1.lisp.1'
hcuot "Jan  7 22:40:56 1984" 'tape/magtape-22-1.qfasl.1'
hcuot "Jan  7 23:28:27 1984" 'tape/magtape-22-2.lisp.1'
hcuot "Jan  7 23:28:40 1984" 'tape/magtape-22-2.qfasl.1'
hcuot "Jan  8 00:41:18 1984" 'tape/magtape-22-3.lisp.1'
hcuot "Jan  8 00:41:44 1984" 'tape/magtape-22-3.qfasl.1'
hcuot "Jan 13 13:06:26 1984" 'tape/magtape-22-4.lisp.1'
hcuot "Jan 13 13:06:35 1984" 'tape/magtape-22-4.qfasl.1'
hcuot "Jan 19 17:40:22 1984" 'tape/magtape-22-5.lisp.1'
hcuot "Jan 19 17:40:32 1984" 'tape/magtape-22-5.qfasl.1'
hcuot "Feb 16 14:23:22 1984" 'tape/magtape-22-6.lisp.1'
hcuot "Feb 16 14:23:28 1984" 'tape/magtape-22-6.qfasl.1'
hcuot "Jan 19 17:04:02 1984" 'tape/mtaux.lisp.80'
hcuot "Jan  3 09:52:48 1984" 'tape/mtaux.qfasl.77'
hcuot "Dec 16 15:34:10 1983" 'tape/mtdefs.lisp.30'
hcuot "Jan  3 09:46:18 1984" 'tape/mtdefs.qfasl.30'
hcuot "Jan 11 05:40:52 1984" 'tape/mtstr.lisp.87'
hcuot "Jan  3 09:47:58 1984" 'tape/mtstr.qfasl.85'
hcuot "Jan  3 08:50:55 1984" 'tape/odump.lisp.1'
hcuot "Jan  3 10:33:05 1984" 'tape/odump.qfasl.1'
hcuot "May 12 05:29:46 1984" 'tape/package.lisp.1'
hcuot "Jan  3 07:59:49 1984" 'tape/pdp10.lisp.1'
hcuot "May 12 08:31:18 1984" 'tape/rmunit.lisp.3'
hcuot "May 12 05:29:46 1984" 'tape/system.lisp.3'
hcuot "May 12 05:29:47 1984" 'tape/tm.lisp.25'
hcuot "May 12 05:29:48 1984" 'tape/tmdefs.lisp.7'
hcuot "May 12 07:27:24 1984" 'tape/unit.lisp.7'
hcuot "Jan  3 08:01:02 1984" 'tape/vms.lisp.1'
hcuot "May 12 07:28:11 1984" 'tape/new/mtdefs.lisp.4'
hcuot "May 12 07:45:03 1984" 'tape/new/mtdefs.qfasl.4'
hcuot "May 12 05:29:49 1984" 'tape/new/mtrqb.lisp.3'
hcuot "May 12 08:31:35 1984" 'tape/new/mtstr.lisp.5'
hcuot "May 12 05:29:50 1984" 'tape/new/tmunit.lisp.5'
hcuot "May 12 05:29:51 1984" 'tape/new/weunit.lisp.3'
hcuot "Nov 20 23:29:49 1982" 'ubin/dcfu.uload.4'
hcuot "Aug  4 07:23:05 1982" 'ubin/memd.uload.1'
hcuot "Feb  7 05:35:36 1986" 'ubin/ucadr.loc.322'
hcuot "Sep 11 21:24:11 1984" 'ubin/ucadr.locs.320'
hcuot "Sep 11 21:21:19 1984" 'ubin/ucadr.mcr.320'
hcuot "Feb  7 05:33:22 1986" 'ubin/ucadr.mcr.322'
hcuot "Sep 11 21:22:04 1984" 'ubin/ucadr.sym.320'
hcuot "Feb  7 05:33:48 1986" 'ubin/ucadr.sym.322'
hcuot "Sep 11 21:24:18 1984" 'ubin/ucadr.tbl.320'
hcuot "Feb  7 05:35:38 1986" 'ubin/ucadr.tbl.322'
hcuot "Apr  9 11:19:01 1983" 'ucadr/cadldb.lisp.20'
hcuot "Jul 26 10:31:51 1983" 'ucadr/cadldb.qfasl.20'
hcuot "Jun 29 10:56:11 1982" 'ucadr/cadtlk.mid.9'
hcuot "Jun 29 10:56:32 1982" 'ucadr/chaos.test.1'
hcuot "Jun 29 10:56:46 1982" 'ucadr/dcfu.text.23'
hcuot "Dec 22 06:46:28 1982" 'ucadr/dcfu.uload.3'
hcuot "Jun 29 10:59:34 1982" 'ucadr/memd.lisp.26'
hcuot "Jun 29 10:59:39 1982" 'ucadr/mmtest.lisp.15'
hcuot "Jun 29 10:56:41 1982" 'ucadr/obsolete-cold-load-maker.lisp.1'
hcuot "Oct 14 23:41:23 1983" 'ucadr/packed.lisp.124'
hcuot "Jun 29 11:00:13 1982" 'ucadr/praid.lisp.21'
hcuot "Jun 29 11:00:18 1982" 'ucadr/promh.text.9'
hcuot "Oct  6 10:49:21 1984" 'ucadr/uc-arith.lisp.34'
hcuot "Jun 17 01:36:02 1984" 'ucadr/uc-array.lisp.63'
hcuot "Mar 31 23:16:21 1983" 'ucadr/uc-array-cache.lisp.1'
hcuot "Jun  2 03:53:36 1984" 'ucadr/uc-cadr.lisp.8'
hcuot "Feb 12 05:46:46 1985" 'ucadr/uc-call-return.lisp.109'
hcuot "Oct 11 07:19:04 1982" 'ucadr/uc-chaos.lisp.1'
hcuot "Nov 14 10:06:43 1983" 'ucadr/uc-cold-disk.lisp.16'
hcuot "Nov 14 08:21:19 1983" 'ucadr/uc-disk.lisp.2'
hcuot "Jun 13 02:12:27 1985" 'ucadr/uc-fctns.lisp.85'
hcuot "Oct 17 16:11:57 1983" 'ucadr/uc-hacks.lisp.5'
hcuot "Dec 10 09:51:17 1984" 'ucadr/uc-interrupt.lisp.9'
hcuot "Mar  3 04:56:48 1984" 'ucadr/uc-logical.lisp.8'
hcuot "Jul  2 11:39:55 1984" 'ucadr/uc-macrocode.lisp.29'
hcuot "Nov 14 02:47:25 1983" 'ucadr/uc-mc.lisp.2'
hcuot "Aug  1 09:39:57 1983" 'ucadr/uc-meter.lisp.5'
hcuot "Nov 21 09:24:14 1983" 'ucadr/uc-page-fault.lisp.13'
hcuot "Dec 10 08:16:18 1984" 'ucadr/uc-parameters.lisp.230'
hcuot "Oct 11 07:18:51 1982" 'ucadr/uc-pup.lisp.1'
hcuot "Feb 12 05:47:05 1985" 'ucadr/uc-stack-closure.lisp.12'
hcuot "Jul 23 11:47:54 1983" 'ucadr/uc-stack-groups.lisp.5'
hcuot "May 19 04:22:04 1984" 'ucadr/uc-storage-allocation.lisp.16'
hcuot "Sep  6 20:03:25 1984" 'ucadr/uc-string.lisp.26'
hcuot "Apr  3 12:37:55 1983" 'ucadr/uc-track-mouse.lisp.2'
hcuot "May  1 09:20:21 1984" 'ucadr/uc-transporter.lisp.23'
hcuot "Apr  5 03:29:11 1984" 'ucadr/uc-tv.lisp.5'
hcuot "Jun 29 11:00:58 1982" 'ucadr/ucadlr.text.746'
hcuot "Nov 14 01:30:39 1983" 'ucadr/ucode.lisp.19'
hcuot "Aug 25 05:24:02 1982" 'wind/baswin.text.7'
hcuot "Aug  9 02:24:49 1983" 'wind/blink.text.21'
hcuot "Jan 22 09:23:05 1984" 'wind/choice.text.95'
hcuot "Jul 23 04:52:44 1983" 'wind/edges.text.14'
hcuot "Aug 25 05:25:39 1982" 'wind/emack.fasl.1'
hcuot "Apr  7 15:53:58 1984" 'wind/emack.lisp.37'
hcuot "Jul  6 04:46:40 1983" 'wind/fonts.text.17'
hcuot "Aug  9 02:21:25 1983" 'wind/frames.text.14'
hcuot "Aug  8 05:44:03 1983" 'wind/grafix.text.24'
hcuot "Sep 30 08:33:21 1983" 'wind/input.text.26'
hcuot "Apr  7 15:55:43 1984" 'wind/lstfla.lisp.6'
hcuot "Aug  8 06:15:43 1983" 'wind/margin.text.20'
hcuot "Aug 23 15:42:25 1983" 'wind/misc.text.24'
hcuot "Aug  8 11:24:58 1983" 'wind/mouse.text.33'
hcuot "Aug 25 05:27:06 1982" 'wind/operat.bolio.1'
hcuot "Aug 25 05:26:46 1982" 'wind/operat.text.45'
hcuot "Aug 25 05:27:12 1982" 'wind/outlin.text.2'
hcuot "Oct 29 02:54:29 1983" 'wind/output.text.28'
hcuot "Nov 19 01:30:08 1983" 'wind/select.text.22'
hcuot "Aug  8 10:42:49 1983" 'wind/tscrol.text.37'
hcuot "Jul  6 05:25:11 1983" 'wind/typout.text.17'
hcuot "Feb  4 11:57:38 1984" 'wind/windo1.text.52'
hcuot "Jul  3 06:44:16 1983" 'wind/windoc.bolio.14'
hcuot "Jun 21 06:34:00 1983" 'wind/windoc.dict.1'
hcuot "Aug  9 03:12:27 1983" 'wind/windoc.log.12'
hcuot "Aug  9 03:10:00 1983" 'wind/windoc.text.15'
hcuot "Aug  9 03:23:20 1983" 'wind/windoc.vars.33'
hcuot "Aug 25 05:30:20 1982" 'wind/window.gloss.1'
hcuot "Aug 25 05:30:32 1982" 'wind/window.manual.1'
hcuot "Aug 25 05:30:42 1982" 'wind/window.methds.1'
hcuot "Aug 25 05:30:47 1982" 'wind/winman.text.1'
hcuot "Dec  7 11:20:04 1984" 'window/basstr.lisp.373'
hcuot "Sep  7 19:42:22 1984" 'window/basstr.qfasl.372'
hcuot "Sep  6 21:24:14 1984" 'window/baswin.lisp.562'
hcuot "Jul 18 21:24:35 1986" 'window/baswin.lisp.563'
hcuot "Sep  7 19:37:06 1984" 'window/baswin.qfasl.562'
hcuot "Aug  5 02:39:30 1984" 'window/choice.lisp.116'
hcuot "Sep  7 23:32:54 1984" 'window/choice.qfasl.116'
hcuot "Aug 29 00:50:25 1984" 'window/cold.lisp.129'
hcuot "Aug 29 02:53:22 1984" 'window/cold.qfasl.129'
hcuot "Oct 14 20:56:06 1984" 'window/color.lisp.69'
hcuot "Aug 30 00:54:42 1984" 'window/color.qfasl.67'
hcuot "Aug  4 12:29:09 1983" 'window/cometh.lisp.26'
hcuot "Aug  3 22:17:13 1984" 'window/cometh.qfasl.26'
hcuot "Feb  3 01:32:16 1985" 'window/csrpos.lisp.10'
hcuot "Aug  3 22:53:14 1984" 'window/csrpos.qfasl.9'
hcuot "Dec 14 00:47:42 1984" 'window/fed.lisp.200'
hcuot "Sep  8 00:51:09 1984" 'window/fed.qfasl.199'
hcuot "Apr 11 03:28:15 1984" 'window/frame.lisp.165'
hcuot "Sep  8 00:59:41 1984" 'window/frame.qfasl.165'
hcuot "Jun  4 03:03:15 1984" 'window/graphics.lisp.1'
hcuot "Aug  3 22:06:31 1984" 'window/graphics.qfasl.1'
hcuot "Jan 30 16:30:23 1985" 'window/inspct.lisp.159'
hcuot "Sep  7 23:26:22 1984" 'window/inspct.qfasl.154'
hcuot "Oct 20 19:40:52 1984" 'window/menu.lisp.105'
hcuot "Sep  7 18:43:14 1984" 'window/menu.qfasl.104'
hcuot "Oct 11 04:37:53 1984" 'window/mouse.lisp.248'
hcuot "Aug  3 12:23:18 1984" 'window/mouse.qfasl.247'
hcuot "Sep  7 18:30:52 1984" 'window/peek.lisp.153'
hcuot "Sep  7 18:50:42 1984" 'window/peek.qfasl.153'
hcuot "May 25 02:14:17 1984" 'window/peekch.lisp.27'
hcuot "Sep  7 18:30:42 1984" 'window/peekfs.lisp.10'
hcuot "Sep  7 19:08:30 1984" 'window/peekfs.qfasl.10'
hcuot "Apr  7 15:56:29 1984" 'window/quest.lisp.43'
hcuot "Sep 11 21:05:34 1984" 'window/rh.lisp.162'
hcuot "Sep 11 21:34:52 1984" 'window/rh.qfasl.162'
hcuot "Oct  9 11:48:09 1984" 'window/scred.lisp.112'
hcuot "Aug  3 22:27:31 1984" 'window/scred.qfasl.111'
hcuot "Dec 14 06:43:19 1984" 'window/scrman.lisp.166'
hcuot "Aug  3 11:54:11 1984" 'window/scrman.qfasl.165'
hcuot "Aug  5 02:39:39 1984" 'window/scroll.lisp.176'
hcuot "Aug  5 02:45:11 1984" 'window/scroll.qfasl.176'
hcuot "Dec  7 06:53:21 1984" 'window/sheet.lisp.558'
hcuot "Aug  3 11:56:23 1984" 'window/sheet.qfasl.557'
hcuot "Mar 12 04:08:36 1985" 'window/shwarm.lisp.334'
hcuot "Sep  7 19:31:11 1984" 'window/shwarm.qfasl.328'
hcuot "Sep  8 19:06:44 1984" 'window/stream.lisp.145'
hcuot "Sep  9 05:51:21 1984" 'window/stream.qfasl.145'
hcuot "Jul  5 03:33:56 1984" 'window/supdup.lisp.276'
hcuot "Apr 28 19:42:05 1987" 'window/supdup.lisp.277'
hcuot "Aug  3 23:14:36 1984" 'window/supdup.qfasl.276'
hcuot "Oct 11 08:55:04 1984" 'window/sysmen.lisp.178'
hcuot "Aug  3 22:22:16 1984" 'window/sysmen.qfasl.177'
hcuot "Jun 29 09:51:17 1982" 'window/task.list.1'
hcuot "Sep  6 05:00:41 1984" 'window/telnet-code.lisp.6'
hcuot "Sep  1 06:28:56 1984" 'window/telnet-front-hack.lisp.1'
hcuot "Nov  5 18:22:00 1985" 'window/tscrol.lisp.75'
hcuot "Jul 30 02:54:26 1984" 'window/tscrol.qfasl.72'
hcuot "Jan 28 12:05:54 1985" 'window/tvdefs.lisp.286'
hcuot "Aug 29 09:10:22 1984" 'window/tvdefs.qfasl.284'
hcuot "May  1 23:22:28 1984" 'window/typwin.lisp.118'
hcuot "Sep  7 23:40:15 1984" 'window/typwin.qfasl.118'
hcuot "Dec 11 08:01:17 1984" 'window/wholin.lisp.92'
hcuot "Sep  4 21:02:13 1984" 'window/wholin.qfasl.90'
hcuot "Dec  9 23:26:17 1983" 'window/winddoc.lisp.2'
hcuot "Mar 30 10:11:15 1987" 'zmail/bug.idx.1'
hcuot "Aug 29 01:45:56 1987" 'zmail/bug.zmail.1'
hcuot "Apr 13 01:45:40 1985" 'zmail/bug.zmail1.1'
hcuot "Jul 13 07:17:38 1984" 'zmail/button.lisp.24'
hcuot "Sep  9 20:58:54 1984" 'zmail/button.qfasl.24'
hcuot "Apr  7 15:57:16 1984" 'zmail/cometh.lisp.51'
hcuot "Sep  9 21:11:39 1984" 'zmail/cometh.qfasl.51'
hcuot "Oct 14 10:23:33 1984" 'zmail/comnds.lisp.583'
hcuot "Sep 10 07:59:56 1984" 'zmail/comnds.qfasl.581'
hcuot "Aug 17 20:36:10 1983" 'zmail/defs.lisp.268'
hcuot "Mar 17 02:11:04 1985" 'zmail/defs.lisp.274'
hcuot "Sep  9 19:19:35 1984" 'zmail/defs.qfasl.273'
hcuot "Sep 25 07:29:51 1984" 'zmail/filter.lisp.356'
hcuot "Sep 10 08:16:57 1984" 'zmail/filter.qfasl.355'
hcuot "Jun 29 11:22:50 1982" 'zmail/info.mail.1'
hcuot "Apr 30 15:49:02 1984" 'zmail/lex733.lisp.14'
hcuot "Sep 10 05:59:29 1984" 'zmail/lex733.qfasl.1'
hcuot "Apr  7 15:57:50 1984" 'zmail/lm.lisp.4'
hcuot "Apr  7 15:58:15 1984" 'zmail/lmcsrv.lisp.5'
hcuot "Jul 13 07:23:12 1984" 'zmail/lmfile.lisp.5'
hcuot "Sep  9 20:30:09 1984" 'zmail/lmfile.qfasl.5'
hcuot "Mar 17 02:10:55 1985" 'zmail/mail.lisp.312'
hcuot "Sep 10 08:07:59 1984" 'zmail/mail.qfasl.311'
hcuot "Nov 30 06:35:32 1984" 'zmail/mfhost.lisp.59'
hcuot "Sep  9 20:25:31 1984" 'zmail/mfhost.qfasl.58'
hcuot "Sep  9 23:58:00 1984" 'zmail/mfiles.lisp.324'
hcuot "Sep 10 07:49:43 1984" 'zmail/mfiles.qfasl.324'
hcuot "Jul 13 07:19:28 1984" 'zmail/mult.lisp.25'
hcuot "Sep  9 20:57:38 1984" 'zmail/mult.qfasl.25'
hcuot "Dec 10 23:37:49 1983" 'zmail/parse.lisp.52'
hcuot "Nov 15 11:02:07 1983" 'zmail/patch.directory.13'
hcuot "Aug 23 00:25:50 1983" 'zmail/patch-51-1.lisp.1'
hcuot "Sep  7 21:56:01 1983" 'zmail/patch-51-2.lisp.1'
hcuot "Sep 21 23:30:38 1983" 'zmail/patch-51-3.lisp.6'
hcuot "Sep 21 23:26:54 1983" 'zmail/patch-51-4.lisp.2'
hcuot "Sep 23 08:11:23 1983" 'zmail/patch-51-5.lisp.2'
hcuot "Sep 26 05:52:32 1983" 'zmail/patch-51-6.lisp.1'
hcuot "Oct 14 07:56:33 1983" 'zmail/patch-51-7.lisp.1'
hcuot "Oct 22 08:30:39 1983" 'zmail/patch-51-8.lisp.1'
hcuot "Oct 28 07:02:36 1983" 'zmail/patch-51-9.lisp.1'
hcuot "Mar 24 10:39:39 1985" 'zmail/patch-53.directory.55'
hcuot "Dec  7 12:43:52 1983" 'zmail/patch-53-1.qfasl.2'
hcuot "Jan 30 06:21:26 1984" 'zmail/patch-53-10.lisp.1'
hcuot "Jan 30 06:21:32 1984" 'zmail/patch-53-10.qfasl.1'
hcuot "Feb 16 07:57:45 1984" 'zmail/patch-53-11.lisp.2'
hcuot "Feb 16 07:57:48 1984" 'zmail/patch-53-11.qfasl.2'
hcuot "Feb 23 13:40:40 1984" 'zmail/patch-53-12.lisp.2'
hcuot "Feb 23 13:40:45 1984" 'zmail/patch-53-12.qfasl.2'
hcuot "Mar  4 08:41:33 1984" 'zmail/patch-53-13.lisp.1'
hcuot "Mar  4 08:41:37 1984" 'zmail/patch-53-13.qfasl.1'
hcuot "Mar 24 17:24:31 1984" 'zmail/patch-53-14.lisp.2'
hcuot "Mar 24 17:24:35 1984" 'zmail/patch-53-14.qfasl.2'
hcuot "Apr 11 07:05:23 1984" 'zmail/patch-53-15.lisp.3'
hcuot "Apr 11 07:05:32 1984" 'zmail/patch-53-15.qfasl.3'
hcuot "Apr 18 09:41:32 1984" 'zmail/patch-53-16.lisp.1'
hcuot "Apr 18 09:41:38 1984" 'zmail/patch-53-16.qfasl.1'
hcuot "Apr 22 00:46:53 1984" 'zmail/patch-53-17.lisp.2'
hcuot "Apr 22 00:47:01 1984" 'zmail/patch-53-17.qfasl.2'
hcuot "Jun 29 04:21:13 1984" 'zmail/patch-53-18.lisp.1'
hcuot "Jun 29 08:53:32 1984" 'zmail/patch-53-18.qfasl.1'
hcuot "Oct 14 10:57:28 1984" 'zmail/patch-53-19.lisp.1'
hcuot "Oct 14 10:57:55 1984" 'zmail/patch-53-19.qfasl.1'
hcuot "Dec  6 05:18:26 1983" 'zmail/patch-53-2.lisp.1'
hcuot "Dec  6 05:18:36 1983" 'zmail/patch-53-2.qfasl.1'
hcuot "Mar 18 19:53:14 1985" 'zmail/patch-53-20.lisp.1'
hcuot "Mar 18 19:53:32 1985" 'zmail/patch-53-20.qfasl.1'
hcuot "Mar 24 10:39:04 1985" 'zmail/patch-53-21.lisp.1'
hcuot "Mar 24 10:39:07 1985" 'zmail/patch-53-21.qfasl.1'
hcuot "Dec 13 06:15:17 1983" 'zmail/patch-53-3.lisp.2'
hcuot "Dec 13 06:15:23 1983" 'zmail/patch-53-3.qfasl.2'
hcuot "Dec 14 08:54:56 1983" 'zmail/patch-53-5.lisp.1'
hcuot "Dec 14 08:55:02 1983" 'zmail/patch-53-5.qfasl.1'
hcuot "Jan  3 18:55:45 1984" 'zmail/patch-53-6.lisp.2'
hcuot "Jan  3 18:55:54 1984" 'zmail/patch-53-6.qfasl.2'
hcuot "Jan  1 01:08:53 1984" 'zmail/patch-53-7.lisp.3'
hcuot "Jan  1 01:09:00 1984" 'zmail/patch-53-7.qfasl.3'
hcuot "Jan  1 15:59:26 1984" 'zmail/patch-53-8.lisp.3'
hcuot "Jan  1 15:59:30 1984" 'zmail/patch-53-8.qfasl.3'
hcuot "Jan  1 16:00:18 1984" 'zmail/patch-53-9.lisp.2'
hcuot "Jan  1 16:00:22 1984" 'zmail/patch-53-9.qfasl.2'
hcuot "Jun 29 11:28:11 1982" 'zmail/poop.text.35'
hcuot "Sep 11 06:21:26 1984" 'zmail/profil.lisp.124'
hcuot "Sep 11 06:21:59 1984" 'zmail/profil.qfasl.124'
hcuot "Jul 13 07:22:56 1984" 'zmail/refer.lisp.7'
hcuot "Sep  9 20:29:01 1984" 'zmail/refer.qfasl.7'
hcuot "Jul 13 07:16:29 1984" 'zmail/rfc733.lisp.57'
hcuot "Sep  9 21:03:17 1984" 'zmail/rfc733.qfasl.57'
hcuot "Sep 26 12:37:36 1984" 'zmail/top.lisp.555'
hcuot "Sep 10 07:45:42 1984" 'zmail/top.qfasl.554'
hcuot "Sep  9 23:58:28 1984" 'zmail/window.lisp.343'
hcuot "Sep 10 08:13:55 1984" 'zmail/window.qfasl.343'
hcuot "Dec 14 06:00:40 1984" 'zmail/manual/manual.text.1'
hcuot "Jun  8 09:14:17 1983" 'zmail/manual/top.txt.1'
hcuot "Jun 29 11:04:18 1982" 'zwei/.comnd.text.1'
hcuot "Jun 29 11:04:27 1982" 'zwei/atsign.xfile.1'
hcuot "Sep 10 02:58:25 1985" 'zwei/bdired.lisp.42'
hcuot "Aug  5 04:08:03 1984" 'zwei/bdired.qfasl.41'
hcuot "Jan 27 19:35:00 1983" 'zwei/bug.bugs7.1'
hcuot "Nov 16 09:01:12 1986" 'zwei/bug.idx.1'
hcuot "Jul 31 15:05:36 1987" 'zwei/bug.zwei.1'
hcuot "Oct  8 10:11:11 1983" 'zwei/bug-zwei.text.1'
hcuot "Jun 29 11:04:29 1982" 'zwei/bugs.bugs.1'
hcuot "Jun 29 11:05:20 1982" 'zwei/bugs.bugs6.1'
hcuot "Jun 29 11:05:52 1982" 'zwei/bugs.status.1'
hcuot "Sep 10 03:09:08 1985" 'zwei/coma.lisp.106'
hcuot "Aug  4 00:18:12 1984" 'zwei/coma.qfasl.102'
hcuot "Sep 10 03:19:04 1985" 'zwei/comb.lisp.96'
hcuot "Aug  4 00:20:56 1984" 'zwei/comb.qfasl.94'
hcuot "Sep 10 03:41:03 1985" 'zwei/comc.lisp.206'
hcuot "Sep  9 05:47:20 1984" 'zwei/comc.qfasl.204'
hcuot "Sep 10 03:46:41 1985" 'zwei/comd.lisp.170'
hcuot "Sep  7 22:43:25 1984" 'zwei/comd.qfasl.167'
hcuot "Sep 10 03:59:20 1985" 'zwei/come.lisp.135'
hcuot "Aug  4 00:28:48 1984" 'zwei/come.qfasl.133'
hcuot "Sep 10 04:17:00 1985" 'zwei/comf.lisp.103'
hcuot "Sep  9 05:48:56 1984" 'zwei/comf.qfasl.99'
hcuot "Sep 10 04:28:53 1985" 'zwei/comg.lisp.42'
hcuot "Aug 29 09:32:06 1984" 'zwei/comg.qfasl.40'
hcuot "Sep 10 04:31:25 1985" 'zwei/comh.lisp.14'
hcuot "Aug  5 00:04:47 1984" 'zwei/comh.qfasl.13'
hcuot "Sep 10 04:35:43 1985" 'zwei/coms.lisp.86'
hcuot "Aug  5 03:58:57 1984" 'zwei/coms.qfasl.85'
hcuot "Jan 31 15:07:30 1985" 'zwei/comtab.lisp.322'
hcuot "Sep  7 22:39:57 1984" 'zwei/comtab.qfasl.317'
hcuot "Sep 10 04:45:19 1985" 'zwei/defs.lisp.157'
hcuot "Sep 11 21:19:07 1984" 'zwei/defs.qfasl.155'
hcuot "Sep 10 05:16:32 1985" 'zwei/dired.lisp.309'
hcuot "Dec  8 18:35:41 1986" 'zwei/dired.lisp.311'
hcuot "Aug 29 09:33:34 1984" 'zwei/dired.qfasl.304'
hcuot "Sep 10 07:58:18 1985" 'zwei/displa.lisp.159'
hcuot "Sep  7 22:46:25 1984" 'zwei/displa.qfasl.157'
hcuot "Sep 10 08:08:05 1985" 'zwei/doc.lisp.77'
hcuot "Aug  5 04:09:10 1984" 'zwei/doc.qfasl.74'
hcuot "Jun 29 11:10:53 1982" 'zwei/emacs.comdif.1'
hcuot "Apr  7 16:05:03 1984" 'zwei/fasupd.lisp.31'
hcuot "Aug  5 04:10:49 1984" 'zwei/fasupd.qfasl.31'
hcuot "May 17 07:22:10 1985" 'zwei/files.lisp.198'
hcuot "Aug  5 04:11:25 1984" 'zwei/files.qfasl.195'
hcuot "May 22 00:58:47 1984" 'zwei/font.lisp.88'
hcuot "Aug  4 00:11:45 1984" 'zwei/font.qfasl.88'
hcuot "Aug  5 02:39:16 1984" 'zwei/for.lisp.62'
hcuot "Aug  5 03:53:34 1984" 'zwei/for.qfasl.62'
hcuot "Mar 27 10:35:18 1985" 'zwei/grind.definition.1'
hcuot "Jan 29 10:47:23 1985" 'zwei/history.lisp.18'
hcuot "Sep 11 21:33:45 1984" 'zwei/history.qfasl.16'
hcuot "Dec 22 10:01:04 1983" 'zwei/host.lisp.20'
hcuot "Aug  5 04:15:39 1984" 'zwei/host.qfasl.20'
hcuot "Feb 15 07:54:51 1985" 'zwei/indent.lisp.107'
hcuot "Aug  3 23:57:17 1984" 'zwei/indent.qfasl.104'
hcuot "Jan 16 21:21:27 1984" 'zwei/info.zwei.1'
hcuot "Nov  5 05:31:43 1984" 'zwei/insert.lisp.35'
hcuot "Aug  3 23:59:24 1984" 'zwei/insert.qfasl.33'
hcuot "Jul  8 18:10:53 1984" 'zwei/ispell.lisp.41'
hcuot "Aug  5 04:16:46 1984" 'zwei/ispell.qfasl.41'
hcuot "Sep  6 00:19:24 1984" 'zwei/kbdmac.lisp.48'
hcuot "Sep  7 23:06:51 1984" 'zwei/kbdmac.qfasl.48'
hcuot "Dec 24 08:43:28 1983" 'zwei/lparse.lisp.31'
hcuot "Aug  5 04:17:46 1984" 'zwei/lparse.qfasl.31'
hcuot "Mar 24 15:33:02 1985" 'zwei/macros.lisp.150'
hcuot "Sep  7 22:33:54 1984" 'zwei/macros.qfasl.147'
hcuot "Jan 30 12:03:17 1985" 'zwei/meth.lisp.49'
hcuot "Aug  4 00:00:57 1984" 'zwei/meth.qfasl.48'
hcuot "Jan 31 11:32:35 1985" 'zwei/modes.lisp.139'
hcuot "Sep  7 22:36:22 1984" 'zwei/modes.qfasl.138'
hcuot "Mar  2 14:56:03 1985" 'zwei/mouse.lisp.98'
hcuot "Aug  5 04:22:39 1984" 'zwei/mouse.qfasl.96'
hcuot "Jul  3 23:23:20 1984" 'zwei/nprim.lisp.34'
hcuot "Aug  4 00:08:03 1984" 'zwei/nprim.qfasl.34'
hcuot "Feb 15 05:39:26 1985" 'zwei/pated.lisp.33'
hcuot "Aug  5 04:24:35 1984" 'zwei/pated.qfasl.25'
hcuot "Apr  7 16:06:39 1984" 'zwei/pl1mod.lisp.14'
hcuot "Aug  5 04:25:49 1984" 'zwei/pl1mod.qfasl.14'
hcuot "Dec  1 20:23:59 1984" 'zwei/poss.lisp.90'
hcuot "Aug  5 04:31:53 1984" 'zwei/poss.qfasl.87'
hcuot "Sep 26 12:38:14 1984" 'zwei/primit.lisp.175'
hcuot "Aug  5 03:55:55 1984" 'zwei/primit.qfasl.174'
hcuot "Mar 12 04:13:17 1985" 'zwei/screen.lisp.468'
hcuot "Sep  7 22:54:21 1984" 'zwei/screen.qfasl.466'
hcuot "Jul 30 02:57:19 1984" 'zwei/search.lisp.86'
hcuot "Sep  7 22:30:55 1984" 'zwei/search.qfasl.86'
hcuot "Feb 15 08:02:40 1985" 'zwei/sectio.lisp.273'
hcuot "Aug  5 04:40:42 1984" 'zwei/sectio.qfasl.266'
hcuot "Sep  7 00:35:06 1984" 'zwei/stream.lisp.168'
hcuot "Sep  7 22:50:12 1984" 'zwei/stream.qfasl.168'
hcuot "Jan 20 03:45:33 1983" 'zwei/teach-zmacs.text.2'
hcuot "Apr 13 11:52:55 1985" 'zwei/zmacs.lisp.522'
hcuot "Sep  7 23:01:03 1984" 'zwei/zmacs.qfasl.518'
hcuot "Feb  1 00:19:56 1985" 'zwei/zmnew.lisp.36'
hcuot "Sep 10 07:42:59 1984" 'zwei/zmnew.qfasl.35'
hcuot "Apr  7 16:07:07 1984" 'zwei/zymurg.lisp.42'
hcuot "Sep  7 17:25:02 1984" 'zwei/zymurg.qfasl.42'
