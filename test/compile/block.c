/*!
 ***********************************************************************
 *  \file
 *      block.c
 *
 *  \brief
 *      Block functions
 *
 *  \author
 *      Main contributors (see contributors.h for copyright, address and affiliation details)
 *      - Inge Lille-Langøy          <inge.lille-langoy@telenor.com>
 *      - Rickard Sjoberg            <rickard.sjoberg@era.ericsson.se>
 ***********************************************************************
 */
/*! \file
 *     contributors.h
 *  \brief
 *     List of contributors and copyright information.
 *
 *  \par Copyright statements
    \verbatim
   H.26L TML coder/decoder

   Copyright (C) 2000 by
      Telenor Satellite Services, Norway
      Ericsson Radio Systems, Sweden
      TELES AG, Germany
      Nokia Inc., USA
      Nokia Corporation, Finland
      Siemens AG, Germany
      Heinrich-Hertz-Institute for Communication Technology GmbH, Germany
      University of Hannover, Institut of Communication Theory and Signal Processing,Germany
      TICSP, Tampere University of Technology, Finland
      Munich University of Technology, Institute for Communications Engineering, Germany
      Videolocus, Canada
      Motorola Inc., USA
      Microsoft Corp., USA
      Apple Computer, Inc.
      RealNetworks, Inc., USA
      Thomson, Inc., USA
  \endverbatim
  \par Full Contact Information
  \verbatim

      Lowell Winger                   <lwinger@videolocus.com><lwinger@uwaterloo.ca>
      Guy Côté                        <gcote@videolocus.com>
      Michael Gallant                 <mgallant@videolocus.com>
      VideoLocus Inc.
      97 Randall Dr.
      Waterloo, ON, Canada  N2V1C5

      Inge Lille-Langøy               <inge.lille-langoy@telenor.com>
      Telenor Satellite Services
      P.O.Box 6914 St.Olavs plass
      N-0130 Oslo, Norway

      Rickard Sjoberg                 <rickard.sjoberg@era.ericsson.se>
      Ericsson Radio Systems
      KI/ERA/T/VV
      164 80 Stockholm, Sweden

      Stephan Wenger                  <stewe@cs.tu-berlin.de>
      TU Berlin / TELES AG
      Sekr. FR 6-3
      Franklinstr. 28-29
      D-10587 Berlin, Germany

      Jani Lainema                    <jani.lainema@nokia.com>
      Nokia Inc. / Nokia Research Center
      6000 Connection Drive
      Irving, TX 75039, USA

      Sebastian Purreiter             <sebastian.purreiter@mch.siemens.de>
      Siemens AG
      ICM MD MP RD MCH 83
      P.O.Box 80 17 07
      D-81617 Munich, Germany

      Thomas Wedi                     <wedi@tnt.uni-hannover.de>
      University of Hannover
      Institut of Communication Theory and Signal Processing
      Appelstr. 9a
      30167 Hannover, Germany

      Guido Heising                   <heising@hhi.de>
      Heinrich-Hertz-Institute
      Einsteinufer 37
      10587 Berlin
      Germany

      Gabi Blaettermann               <blaetter@hhi.de>
      Heinrich-Hertz-Institute
      Einsteinufer 37
      10587 Berlin
      Germany

      Detlev Marpe                    <marpe@hhi.de>
      Heinrich-Hertz-Institute
      Einsteinufer 37
      10587 Berlin
      Germany

      Ragip Kurceren                  <ragip.kurceren@nokia.com>
      Nokia Inc. / Nokia Research Center
      6000 Connection Drive
      Irving, TX 75039, USA

      Viktor Varsa                    <viktor.varsa@nokia.com>
      Nokia Inc. / Nokia Research Center
      6000 Connection Drive
      Irving, TX 75039, USA

      Ye-Kui Wang                     <wyk@ieee.org>
      Tampere University of Technology
      Tampere International Center for Signal Processing
      33720 Tampere, Finland

      Ari Hourunranta                 <ari.hourunranta@nokia.com>
      Nokia Corporation / Nokia Mobile Phones
      P.O. Box 88
      33721 Tampere, Finland

      Yann Le Maguet                  <yann.lemaguet@philips.com>
      Philips Research France

      Dong Tian                       <tian@cs.tut.fi>
      Tampere University of Technology
      Tampere International Center for Signal Processing
      33720 Tampere, Finland

      Miska M. Hannuksela             <miska.hannuksela@nokia.com>
      Nokia Corporation / Nokia Mobile Phones
      P.O. Box 88
      33721 Tampere, Finland

      Karsten Suehring                <suehring@hhi.de>
      Heinrich-Hertz-Institute
      Einsteinufer 37
      10587 Berlin
      Germany

      Heiko Schwarz                   <hschwarz@hhi.de>
      Heinrich-Hertz-Institute
      Einsteinufer 37
      10587 Berlin
      Germany

      Tobias Oelbaum                  <drehvial@gmx.net>
      Institute for Communications Engineering
      Munich University of Technology
      Germany

      Limin Wang                      <liwang@gi.com>
      Krit Panusopone                 <kpanusopone@gi.com>
      Rajeev Gandhi                   <rgandhi@gi.com>
      Yue Yu                          <yyu@gi.com>
      Motorola Inc.
      6450 Sequence Drive
      San Diego, CA 92121 USA

      Feng Wu                         <fengwu@microsoft.com>
      Alexis Michael Tourapis         <alexismt@ieee.org>
      Xiaoyan Sun                     <sunxiaoyan@msrchina.research.microsoft.com>
      Microsoft Research Asia
      3/F, Beijing Sigma Center
      No.49, Zhichun Road, Hai Dian District,
      Beijing China 100080

      Yoshihiro Kikuchi               <yoshihiro.kikuchi@toshiba.co.jp>
      Takeshi Chujoh                  <takeshi.chujoh@toshiba.co.jp>
      Toshiba Corporation
      Research and Development Center
      Kawasaki 212-8582, Japan

      Shinya Kadono                   <kadono@drl.mei.co.jp>
      Matsushita Electric Industrial Co., Ltd.
      1006 Kadoma, Kadoma
      Osaka 663-8113, Japan

      Dzung Hoang                     <dzung.hoang@conexant.com>
      Eric Viscito                    <eric.viscito@conexant.com>
      Conexant Systems. Inc.
      MPEG Compression Group
      20450 Stevens Creek Blvd.
      Cupertino, CA 95014

      Barry Haskell
      Apple Computer, Inc.            <bhaskell@apple.com>
      2 Infinite Loop
      Cupertino, California 95014

      Greg Conklin
      RealNetworks, Inc.              <gregc@real.com>
      2601 Elliott Ave
      Seattle, WA 98101

      Jill Boyce                      <jill.boyce@thomson.net>
      Cristina Gomila                 <cristina.gomila@thomson.net>
      Alexis Tourapis                 <alexandros.tourapis@thomson.net>
      Thomson
      2 Independence Way
      Princeton, NJ 08540
  \endverbatim
*/
/*
 * stdlib.h
 *
 * Definitions for common types, variables, and functions.
 */
/* Provide support for both ANSI and non-ANSI environments.  */
/* Some ANSI environments are "broken" in the sense that __STDC__ cannot be
   relied upon to have it's intended meaning.  Therefore we must use our own
   concoction: _HAVE_STDC.  Always use _HAVE_STDC instead of __STDC__ in newlib
   sources!

   To get a strict ANSI C environment, define macro __STRICT_ANSI__.  This will
   "comment out" the non-ANSI parts of the ANSI header files (non-ANSI header
   files aren't affected).  */
/* newlib.h.  Generated by configure.  */
/* newlib.hin.  Generated automatically from configure.in by autoheader.  */
/* EL/IX level */
/* #undef _ELIX_LEVEL */
/* Newlib version */
/* long long type support in IO functions like printf/scanf enabled */
/* long double type support in IO functions like printf/scanf enabled */
/* #undef _WANT_IO_LONG_DOUBLE */
/* Positional argument support in printf functions enabled.  */
/* #undef _WANT_IO_POS_ARGS */
/* Multibyte supported */
/* #undef _MB_CAPABLE */
/* MB_LEN_MAX */
/* ICONV enabled */
/* #undef _ICONV_ENABLED */
/* Enable ICONV external CCS files loading capabilities */
/* #undef _ICONV_ENABLE_EXTERNAL_CCS */
/* Define if the linker supports .preinit_array/.init_array/.fini_array
 * sections.  */
/* #undef  HAVE_INITFINI_ARRAY */
/* True if atexit() may dynamically allocate space for cleanup
   functions.  */
/* Define if the compiler supports aliasing an array to an address.  */
/*
 * Iconv encodings enabled ("to" direction)
 */
/* #undef _ICONV_TO_ENCODING_BIG5 */
/* #undef _ICONV_TO_ENCODING_CP775 */
/* #undef _ICONV_TO_ENCODING_CP850 */
/* #undef _ICONV_TO_ENCODING_CP852 */
/* #undef _ICONV_TO_ENCODING_CP855 */
/* #undef _ICONV_TO_ENCODING_CP866 */
/* #undef _ICONV_TO_ENCODING_EUC_JP */
/* #undef _ICONV_TO_ENCODING_EUC_TW */
/* #undef _ICONV_TO_ENCODING_EUC_KR */
/* #undef _ICONV_TO_ENCODING_ISO_8859_1 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_10 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_11 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_13 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_14 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_15 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_2 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_3 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_4 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_5 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_6 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_7 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_8 */
/* #undef _ICONV_TO_ENCODING_ISO_8859_9 */
/* #undef _ICONV_TO_ENCODING_ISO_IR_111 */
/* #undef _ICONV_TO_ENCODING_KOI8_R */
/* #undef _ICONV_TO_ENCODING_KOI8_RU */
/* #undef _ICONV_TO_ENCODING_KOI8_U */
/* #undef _ICONV_TO_ENCODING_KOI8_UNI */
/* #undef _ICONV_TO_ENCODING_UCS_2 */
/* #undef _ICONV_TO_ENCODING_UCS_2_INTERNAL */
/* #undef _ICONV_TO_ENCODING_UCS_2BE */
/* #undef _ICONV_TO_ENCODING_UCS_2LE */
/* #undef _ICONV_TO_ENCODING_UCS_4 */
/* #undef _ICONV_TO_ENCODING_UCS_4_INTERNAL */
/* #undef _ICONV_TO_ENCODING_UCS_4BE */
/* #undef _ICONV_TO_ENCODING_UCS_4LE */
/* #undef _ICONV_TO_ENCODING_US_ASCII */
/* #undef _ICONV_TO_ENCODING_UTF_16 */
/* #undef _ICONV_TO_ENCODING_UTF_16BE */
/* #undef _ICONV_TO_ENCODING_UTF_16LE */
/* #undef _ICONV_TO_ENCODING_UTF_8 */
/* #undef _ICONV_TO_ENCODING_WIN_1250 */
/* #undef _ICONV_TO_ENCODING_WIN_1251 */
/* #undef _ICONV_TO_ENCODING_WIN_1252 */
/* #undef _ICONV_TO_ENCODING_WIN_1253 */
/* #undef _ICONV_TO_ENCODING_WIN_1254 */
/* #undef _ICONV_TO_ENCODING_WIN_1255 */
/* #undef _ICONV_TO_ENCODING_WIN_1256 */
/* #undef _ICONV_TO_ENCODING_WIN_1257 */
/* #undef _ICONV_TO_ENCODING_WIN_1258 */
/*
 * Iconv encodings enabled ("from" direction)
 */
/* #undef _ICONV_FROM_ENCODING_BIG5 */
/* #undef _ICONV_FROM_ENCODING_CP775 */
/* #undef _ICONV_FROM_ENCODING_CP850 */
/* #undef _ICONV_FROM_ENCODING_CP852 */
/* #undef _ICONV_FROM_ENCODING_CP855 */
/* #undef _ICONV_FROM_ENCODING_CP866 */
/* #undef _ICONV_FROM_ENCODING_EUC_JP */
/* #undef _ICONV_FROM_ENCODING_EUC_TW */
/* #undef _ICONV_FROM_ENCODING_EUC_KR */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_1 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_10 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_11 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_13 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_14 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_15 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_2 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_3 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_4 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_5 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_6 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_7 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_8 */
/* #undef _ICONV_FROM_ENCODING_ISO_8859_9 */
/* #undef _ICONV_FROM_ENCODING_ISO_IR_111 */
/* #undef _ICONV_FROM_ENCODING_KOI8_R */
/* #undef _ICONV_FROM_ENCODING_KOI8_RU */
/* #undef _ICONV_FROM_ENCODING_KOI8_U */
/* #undef _ICONV_FROM_ENCODING_KOI8_UNI */
/* #undef _ICONV_FROM_ENCODING_UCS_2 */
/* #undef _ICONV_FROM_ENCODING_UCS_2_INTERNAL */
/* #undef _ICONV_FROM_ENCODING_UCS_2BE */
/* #undef _ICONV_FROM_ENCODING_UCS_2LE */
/* #undef _ICONV_FROM_ENCODING_UCS_4 */
/* #undef _ICONV_FROM_ENCODING_UCS_4_INTERNAL */
/* #undef _ICONV_FROM_ENCODING_UCS_4BE */
/* #undef _ICONV_FROM_ENCODING_UCS_4LE */
/* #undef _ICONV_FROM_ENCODING_US_ASCII */
/* #undef _ICONV_FROM_ENCODING_UTF_16 */
/* #undef _ICONV_FROM_ENCODING_UTF_16BE */
/* #undef _ICONV_FROM_ENCODING_UTF_16LE */
/* #undef _ICONV_FROM_ENCODING_UTF_8 */
/* #undef _ICONV_FROM_ENCODING_WIN_1250 */
/* #undef _ICONV_FROM_ENCODING_WIN_1251 */
/* #undef _ICONV_FROM_ENCODING_WIN_1252 */
/* #undef _ICONV_FROM_ENCODING_WIN_1253 */
/* #undef _ICONV_FROM_ENCODING_WIN_1254 */
/* #undef _ICONV_FROM_ENCODING_WIN_1255 */
/* #undef _ICONV_FROM_ENCODING_WIN_1256 */
/* #undef _ICONV_FROM_ENCODING_WIN_1257 */
/* #undef _ICONV_FROM_ENCODING_WIN_1258 */
/* This file can define macros to choose variations of the IEEE float
   format:

   _FLT_LARGEST_EXPONENT_IS_NORMAL

    Defined if the float format uses the largest exponent for finite
    numbers rather than NaN and infinity representations.  Such a
    format cannot represent NaNs or infinities at all, but it's FLT_MAX
    is twice the IEEE value.

   _FLT_NO_DENORMALS

    Defined if the float format does not support IEEE denormals.  Every
    float with a zero exponent is taken to be a zero representation.

   ??? At the moment, there are no equivalent macros above for doubles and
   the macros are not fully supported by --enable-newlib-hw-fp.

   __IEEE_BIG_ENDIAN

        Defined if the float format is big endian.  This is mutually exclusive
        with __IEEE_LITTLE_ENDIAN.

   __IEEE_LITTLE_ENDIAN

        Defined if the float format is little endian.  This is mutually exclusive
        with __IEEE_BIG_ENDIAN.

   Note that one of __IEEE_BIG_ENDIAN or __IEEE_LITTLE_ENDIAN must be specified for a
   platform or error will occur.

   __IEEE_BYTES_LITTLE_ENDIAN

        This flag is used in conjunction with __IEEE_BIG_ENDIAN to describe a situation
    whereby multiple words of an IEEE floating point are in big endian order, but the
    words themselves are little endian with respect to the bytes.

   _DOUBLE_IS_32_BITS

        This is used on platforms that support double by using the 32-bit IEEE
        float type.

   _FLOAT_ARG

        This represents what type a float arg is passed as.  It is used when the type is
        not promoted to double.

*/
/* necv70 was __IEEE_LITTLE_ENDIAN. */
/* 070323,JJ,add for PACDSP */
/* exceptions first */
/* 16 bit integer machines */
/* For the PowerPC eabi, force the _impure_ptr to be in .sdata */
/* This block should be kept in sync with GCC's limits.h.  The point
   of having these definitions here is to not include limits.h, which
   would pollute the user namespace, while still using types of the
   the correct widths when deciding how to define __int32_t and
   __int64_t.  */
/* End of block that should be kept in sync with GCC's limits.h.  */
/* Define return type of read/write routines.  In POSIX, the return type
   for read()/write() is "ssize_t" but legacy newlib code has been using
   "int" for some time.  If not specified, "int" is defaulted.  */
/*070323,JJ,add for pacdsp */
/* First try to figure out whether we really are in an ANSI C environment.  */
/* FIXME: This probably needs some work.  Perhaps sys/config.h can be
   prevailed upon to give us a clue.  */
/* Support gcc's __attribute__ facility.  */
/*  ISO C++.  */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef unsigned int size_t;
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
   symbols in the _FOO_T_ family, stays defined even after its
   corresponding type is defined).  If we define wchar_t, then we
   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
   we undef _WCHAR_T_, then we must also define rune_t, since
   headers like runetype.h assume that if machine/ansi.h is included,
   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
   the same type." */
/* FreeBSD 5 can't be handled well using "traditional" logic above
   since it no longer defines _BSD_RUNE_T_ yet still desires to export
   rune_t in some cases... */
typedef int wchar_t;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* This header file provides the reentrancy.  */
/* WARNING: All identifiers here must begin with an underscore.  This file is
   included by stdio.h and others and we therefore must only use identifiers
   in the namespace allotted to us.  */
/* Provide support for both ANSI and non-ANSI environments.  */
/* Some ANSI environments are "broken" in the sense that __STDC__ cannot be
   relied upon to have it's intended meaning.  Therefore we must use our own
   concoction: _HAVE_STDC.  Always use _HAVE_STDC instead of __STDC__ in newlib
   sources!

   To get a strict ANSI C environment, define macro __STRICT_ANSI__.  This will
   "comment out" the non-ANSI parts of the ANSI header files (non-ANSI header
   files aren't affected).  */
/* ANSI C namespace clean utility typedefs */
/* This file defines various typedefs needed by the system calls that support
   the C library.  Basically, they're just the POSIX versions with an '_'
   prepended.  This file lives in the `sys' directory so targets can provide
   their own if desired (or they can put target dependant conditionals here).
*/
/* dummy lock routines for single-threaded aps */
typedef int _LOCK_T;
typedef int _LOCK_RECURSIVE_T;
typedef long _off_t;
typedef long long _off64_t;
typedef int _ssize_t;
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef unsigned int wint_t;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Conversion state information.  */
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    unsigned char __wchb[4];
  } __value; /* Value so far.  */
} _mbstate_t;
typedef _LOCK_RECURSIVE_T _flock_t;
/* Iconv descriptor type */
typedef void *_iconv_t;
typedef unsigned long __ULong;
/*
 * If _REENT_SMALL is defined, we make struct _reent as small as possible,
 * by having nearly everything possible allocated at first use.
 */
struct _Bigint
{
  struct _Bigint *_next;
  int _k, _maxwds, _sign, _wds;
  __ULong _x[1];
};
/* needed by reentrant structure */
struct __tm
{
  int __tm_sec;
  int __tm_min;
  int __tm_hour;
  int __tm_mday;
  int __tm_mon;
  int __tm_year;
  int __tm_wday;
  int __tm_yday;
  int __tm_isdst;
};
/*
 * atexit() support.
 */
struct _on_exit_args {
 void * _fnargs[32]; /* user fn args */
 void * _dso_handle[32];
 /* Bitmask is set if user function takes arguments.  */
 __ULong _fntypes; /* type of exit routine -
                   Must have at least _ATEXIT_SIZE bits */
 /* Bitmask is set if function was registered via __cxa_atexit.  */
 __ULong _is_cxa;
};
struct _atexit {
 struct _atexit *_next; /* next in list */
 int _ind; /* next index in this table */
 void (*_fns[32])(void); /* the table itself */
        struct _on_exit_args * _on_exit_args_ptr;
};
/*
 * Stdio buffers.
 *
 * This and __FILE are defined here because we need them for struct _reent,
 * but we don't want stdio.h included when stdlib.h is.
 */
struct __sbuf {
 unsigned char *_base;
 int _size;
};
/*
 * We need fpos_t for the following, but it doesn't have a leading "_",
 * so we use _fpos_t instead.
 */
typedef long _fpos_t; /* XXX must match off_t in <sys/types.h> */
    /* (and must be `long' for now) */
/*
 * Stdio state variables.
 *
 * The following always hold:
 *
 *    if (_flags&(__SLBF|__SWR)) == (__SLBF|__SWR),
 *        _lbfsize is -_bf._size, else _lbfsize is 0
 *    if _flags&__SRD, _w is 0
 *    if _flags&__SWR, _r is 0
 *
 * This ensures that the getc and putc macros (or inline functions) never
 * try to write or read from a file that is in `read' or `write' mode.
 * (Moreover, they can, and do, automatically switch from read mode to
 * write mode, and back, on "r+" and "w+" files.)
 *
 * _lbfsize is used only to make the inline line-buffered output stream
 * code as compact as possible.
 *
 * _ub, _up, and _ur are used when ungetc() pushes back more characters
 * than fit in the current _bf, or when ungetc() pushes back a character
 * that does not match the previous one in _bf.  When this happens,
 * _ub._base becomes non-nil (i.e., a stream has ungetc() data iff
 * _ub._base!=NULL) and _up and _ur save the current values of _p and _r.
 */
/*
 * struct __sFILE_fake is the start of a struct __sFILE, with only the
 * minimal fields allocated.  In __sinit() we really allocate the 3
 * standard streams, etc., and point away from this fake.
 */
struct __sFILE_fake {
  unsigned char *_p; /* current position in (some) buffer */
  int _r; /* read space left for getc() */
  int _w; /* write space left for putc() */
  short _flags; /* flags, below; this FILE is free if 0 */
  short _file; /* fileno, if Unix descriptor, else -1 */
  struct __sbuf _bf; /* the buffer (at least 1 byte, if !NULL) */
  int _lbfsize; /* 0 or -_bf._size, for inline putc */
  struct _reent *_data;
};
/* CHECK_STD_INIT() comes from stdio/local.h; be sure to include that.  */
struct __sFILE {
  unsigned char *_p; /* current position in (some) buffer */
  int _r; /* read space left for getc() */
  int _w; /* write space left for putc() */
  short _flags; /* flags, below; this FILE is free if 0 */
  short _file; /* fileno, if Unix descriptor, else -1 */
  struct __sbuf _bf; /* the buffer (at least 1 byte, if !NULL) */
  int _lbfsize; /* 0 or -_bf._size, for inline putc */
  struct _reent *_data;
  /* operations */
  void * _cookie; /* cookie passed to io functions */
  int (*_read) (void * _cookie, char *_buf, int _n);
  int (*_write) (void * _cookie, const char *_buf, int _n);
  _fpos_t (*_seek) (void * _cookie, _fpos_t _offset, int _whence);
  int (*_close) (void * _cookie);
  /* separate buffer for long sequences of ungetc() */
  struct __sbuf _ub; /* ungetc buffer */
  unsigned char *_up; /* saved _p when _p is doing ungetc data */
  int _ur; /* saved _r when _r is counting ungetc data */
  /* tricks to meet minimum requirements even when malloc() fails */
  unsigned char _ubuf[3]; /* guarantee an ungetc() buffer */
  unsigned char _nbuf[1]; /* guarantee a getc() buffer */
  /* separate buffer for fgetline() when line crosses buffer boundary */
  struct __sbuf _lb; /* buffer for fgetline() */
  /* Unix stdio files get aligned to block boundaries on fseek() */
  int _blksize; /* stat.st_blksize (may be != _bf._size) */
  int _offset; /* current lseek offset */
};
typedef struct __sFILE __FILE;
struct _glue
{
  struct _glue *_next;
  int _niobs;
  __FILE *_iobs;
};
/*
 * rand48 family support
 *
 * Copyright (c) 1993 Martin Birgmeier
 * All rights reserved.
 *
 * You may redistribute unmodified or modified versions of this source
 * code provided that the above copyright notice and this and the
 * following conditions are retained.
 *
 * This software is provided ``as is'', and comes with no warranties
 * of any kind. I shall in no event be liable for anything that happens
 * to anyone/anything when using this software.
 */
struct _rand48 {
  unsigned short _seed[3];
  unsigned short _mult[3];
  unsigned short _add;
  /* Put this in here as well, for good luck.  */
  unsigned long long _rand_next;
};
/* How big the some arrays are.  */

struct _mprec
{
  /* used by mprec routines */
  struct _Bigint *_result;
  int _result_k;
  struct _Bigint *_p5s;
  struct _Bigint **_freelist;
};
struct _misc_reent
{
  /* miscellaneous reentrant data */
  char *_strtok_last;
  _mbstate_t _mblen_state;
  _mbstate_t _wctomb_state;
  _mbstate_t _mbtowc_state;
  char _l64a_buf[8];
  int _getdate_err;
  _mbstate_t _mbrlen_state;
  _mbstate_t _mbrtowc_state;
  _mbstate_t _mbsrtowcs_state;
  _mbstate_t _wcrtomb_state;
  _mbstate_t _wcsrtombs_state;
};

/*
 * struct _reent
 *
 * This structure contains *all* globals needed by the library.
 * It's raison d'etre is to facilitate threads by making all library routines
 * reentrant.  IE: All state information is contained here.
 *
 * This version of _reent is layed our with "int"s in pairs, to help
 * ports with 16-bit int's but 32-bit pointers, align nicely.  */
struct _reent
{
  /* FILE is a big struct and may change over time.  To try to achieve binary
     compatibility with future versions, put stdin,stdout,stderr here.
     These are pointers into member __sf defined below.  */
  __FILE *_stdin, *_stdout, *_stderr; /* XXX */
  int _errno; /* local copy of errno */
  int _inc; /* used by tmpnam */
  char *_emergency;
  int __sdidinit; /* 1 means stdio has been init'd */
  int _current_category; /* used by setlocale */
  const char *_current_locale;
  struct _mprec *_mp;
  void (*__cleanup) (struct _reent *);
  int _gamma_signgam;
  /* used by some fp conversion routines */
  int _cvtlen; /* should be size_t */
  char *_cvtbuf;
  struct _rand48 *_r48;
  struct __tm *_localtime_buf;
  char *_asctime_buf;
  /* signal info */
  void (**(_sig_func))(int);
  /* atexit stuff */
  struct _atexit *_atexit;
  struct _atexit _atexit0;
  struct _glue __sglue; /* root of glue chain */
  __FILE *__sf; /* file descriptors */
  struct _misc_reent *_misc; /* strtok, multibyte states */
  char *_signal_buf; /* strsignal */
};
extern const struct __sFILE_fake __sf_fake_stdin;
extern const struct __sFILE_fake __sf_fake_stdout;
extern const struct __sFILE_fake __sf_fake_stderr;
/* Only built the assert() calls if we are built with debugging.  */
/* Generic _REENT check macro.  */
/* Handle the dynamically allocated rand48 structure. */
/*
 * All references to struct _reent are via this pointer.
 * Internally, newlib routines that need to reference it should use _REENT.
 */
extern struct _reent *_impure_ptr ;
extern struct _reent *const _global_impure_ptr ;
void _reclaim_reent (struct _reent *);
/* #define _REENT_ONLY define this to get only reentrant routines */
/* place holder so platforms may add stdlib.h extensions */
/* libc/include/alloca.h - Allocate memory on stack */
/* Written 2000 by Werner Almesberger */
/* Rearranged for general inclusion by stdlib.h.
   2001, Corinna Vinschen <vinschen@redhat.com> */
/* Provide support for both ANSI and non-ANSI environments.  */
/* Some ANSI environments are "broken" in the sense that __STDC__ cannot be
   relied upon to have it's intended meaning.  Therefore we must use our own
   concoction: _HAVE_STDC.  Always use _HAVE_STDC instead of __STDC__ in newlib
   sources!

   To get a strict ANSI C environment, define macro __STRICT_ANSI__.  This will
   "comment out" the non-ANSI parts of the ANSI header files (non-ANSI header
   files aren't affected).  */
/* This header file provides the reentrancy.  */
/* WARNING: All identifiers here must begin with an underscore.  This file is
   included by stdio.h and others and we therefore must only use identifiers
   in the namespace allotted to us.  */

typedef struct
{
  int quot; /* quotient */
  int rem; /* remainder */
} div_t;
typedef struct
{
  long quot; /* quotient */
  long rem; /* remainder */
} ldiv_t;
typedef struct
{
  long long int quot; /* quotient */
  long long int rem; /* remainder */
} lldiv_t;
extern int __mb_cur_max;
void abort(void);
int abs(int);
int atexit (void (*__func)(void));
double atof (const char *__nptr);
float atoff (const char *__nptr);
int atoi (const char *__nptr);
int _atoi_r (struct _reent *, const char *__nptr);
long atol (const char *__nptr);
long _atol_r (struct _reent *, const char *__nptr);
void * bsearch (const void * __key, const void * __base, size_t __nmemb, size_t __size, int (* _compar) (const void *, const void *));
void * calloc (size_t __nmemb, size_t __size);
div_t div (int __numer, int __denom);
void exit (int __status);
void free (void *);
char * getenv (const char *__string);
char * _getenv_r (struct _reent *, const char *__string);
char * _findenv (const char *, int *);
char * _findenv_r (struct _reent *, const char *, int *);
long labs (long);
ldiv_t ldiv (long __numer, long __denom);
void * malloc (size_t __size);
int mblen (const char *, size_t);
int _mblen_r (struct _reent * a, const char * b, size_t c, _mbstate_t * d);
int mbtowc (wchar_t *, const char *, size_t);
int _mbtowc_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
int wctomb (char *, wchar_t);
int _wctomb_r (struct _reent *, char *, wchar_t, _mbstate_t *);
size_t mbstowcs (wchar_t *, const char *, size_t);
size_t _mbstowcs_r (struct _reent *, wchar_t *, const char *, size_t, _mbstate_t *);
size_t wcstombs (char *, const wchar_t *, size_t);
size_t _wcstombs_r (struct _reent *, char *, const wchar_t *, size_t, _mbstate_t *);
int mkstemp (char *);
char * mktemp (char *);
void qsort (void * __base, size_t __nmemb, size_t __size, int(*_compar)(const void *, const void *));
int rand (void);
void * realloc (void * __r, size_t __size);
void srand (unsigned __seed);
double strtod (const char *__n, char **__end_PTR);
double _strtod_r (struct _reent *,const char *__n, char **__end_PTR);
float strtof (const char *__n, char **__end_PTR);
/* the following strtodf interface is deprecated...use strtof instead */
long strtol (const char *__n, char **__end_PTR, int __base);
long _strtol_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
unsigned long strtoul (const char *__n, char **__end_PTR, int __base);
unsigned long _strtoul_r (struct _reent *,const char *__n, char **__end_PTR, int __base);
int system (const char *__string);
long a64l (const char *__input);
char * l64a (long __input);
char * _l64a_r (struct _reent *,long __input);
int on_exit (void (*__func)(int, void *),void * __arg);
void _Exit (int __status);
int putenv (char *__string);
int _putenv_r (struct _reent *, char *__string);
int setenv (const char *__string, const char *__value, int __overwrite);
int _setenv_r (struct _reent *, const char *__string, const char *__value, int __overwrite);
char * gcvt (double,int,char *);
char * gcvtf (float,int,char *);
char * fcvt (double,int,int *,int *);
char * fcvtf (float,int,int *,int *);
char * ecvt (double,int,int *,int *);
char * ecvtbuf (double, int, int*, int*, char *);
char * fcvtbuf (double, int, int*, int*, char *);
char * ecvtf (float,int,int *,int *);
char * dtoa (double, int, int, int *, int*, char**);
int rand_r (unsigned *__seed);
double drand48 (void);
double _drand48_r (struct _reent *);
double erand48 (unsigned short [3]);
double _erand48_r (struct _reent *, unsigned short [3]);
long jrand48 (unsigned short [3]);
long _jrand48_r (struct _reent *, unsigned short [3]);
void lcong48 (unsigned short [7]);
void _lcong48_r (struct _reent *, unsigned short [7]);
long lrand48 (void);
long _lrand48_r (struct _reent *);
long mrand48 (void);
long _mrand48_r (struct _reent *);
long nrand48 (unsigned short [3]);
long _nrand48_r (struct _reent *, unsigned short [3]);
unsigned short *
       seed48 (unsigned short [3]);
unsigned short *
       _seed48_r (struct _reent *, unsigned short [3]);
void srand48 (long);
void _srand48_r (struct _reent *, long);
long long atoll (const char *__nptr);
long long _atoll_r (struct _reent *, const char *__nptr);
long long llabs (long long);
lldiv_t lldiv (long long __numer, long long __denom);
long long strtoll (const char *__n, char **__end_PTR, int __base);
long long _strtoll_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
unsigned long long strtoull (const char *__n, char **__end_PTR, int __base);
unsigned long long _strtoull_r (struct _reent *, const char *__n, char **__end_PTR, int __base);
void cfree (void *);
void unsetenv (const char *__string);
void _unsetenv_r (struct _reent *, const char *__string);
char * _dtoa_r (struct _reent *, double, int, int, int *, int*, char**);
void * _malloc_r (struct _reent *, size_t);
void * _calloc_r (struct _reent *, size_t, size_t);
void _free_r (struct _reent *, void *);
void * _realloc_r (struct _reent *, void *, size_t);
void _mstats_r (struct _reent *, char *);
int _system_r (struct _reent *, const char *);
void __eprintf (const char *, const char *, unsigned int, const char *);

/*!
 ************************************************************************
 *  \file
 *     global.h
 *  \brief
 *     global definitions for for H.26L decoder.
 *  \author
 *     Copyright (C) 1999  Telenor Satellite Services,Norway
 *                         Ericsson Radio Systems, Sweden
 *
 *     Inge Lille-Langøy               <inge.lille-langoy@telenor.com>
 *
 *     Telenor Satellite Services
 *     Keysers gt.13                       tel.:   +47 23 13 86 98
 *     N-0130 Oslo,Norway                  fax.:   +47 22 77 79 80
 *
 *     Rickard Sjoberg                 <rickard.sjoberg@era.ericsson.se>
 *
 *     Ericsson Radio Systems
 *     KI/ERA/T/VV
 *     164 80 Stockholm, Sweden
 *
 ************************************************************************
 */
/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *    @(#)stdio.h    5.3 (Berkeley) 3/15/86
 */
/*
 * NB: to fit things in six character monocase externals, the
 * stdio code uses the prefix `__s' for stdio objects, typically
 * followed by a three-character attempt at a mnemonic.
 */
/* Provide support for both ANSI and non-ANSI environments.  */
/* Some ANSI environments are "broken" in the sense that __STDC__ cannot be
   relied upon to have it's intended meaning.  Therefore we must use our own
   concoction: _HAVE_STDC.  Always use _HAVE_STDC instead of __STDC__ in newlib
   sources!

   To get a strict ANSI C environment, define macro __STRICT_ANSI__.  This will
   "comment out" the non-ANSI parts of the ANSI header files (non-ANSI header
   files aren't affected).  */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* stdarg.h for GNU.
   Note that the type used in va_arg is supposed to match the
   actual type **after default promotions**.
   Thus, va_arg (..., short) is not valid.  */
/* Copyright (C) 1989, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.15  Variable arguments  <stdarg.h>
 */
/* Define __gnuc_va_list.  */
typedef char* __builtin_va_list;
//typedef __builtin_va_list __gnuc_va_list;

/* Define the standard macros for the user,
   if this invocation was from the user program.  */
/*
 * <sys/reent.h> defines __FILE, _fpos_t.
 * They must be defined there because struct _reent needs them (and we don't
 * want reent.h to include this file.
 */
/* This header file provides the reentrancy.  */
/* WARNING: All identifiers here must begin with an underscore.  This file is
   included by stdio.h and others and we therefore must only use identifiers
   in the namespace allotted to us.  */
/* unified sys/types.h:
   start with sef's sysvi386 version.
   merge go32 version -- a few ifdefs.
   h8300hms, h8300xray, and sysvnecv70 disagree on the following types:

   typedef int gid_t;
   typedef int uid_t;
   typedef int dev_t;
   typedef int ino_t;
   typedef int mode_t;
   typedef int caddr_t;

   however, these aren't "reasonable" values, the sysvi386 ones make far
   more sense, and should work sufficiently well (in particular, h8300
   doesn't have a stat, and the necv70 doesn't matter.) -- eichin
 */
/* Provide support for both ANSI and non-ANSI environments.  */
/* Some ANSI environments are "broken" in the sense that __STDC__ cannot be
   relied upon to have it's intended meaning.  Therefore we must use our own
   concoction: _HAVE_STDC.  Always use _HAVE_STDC instead of __STDC__ in newlib
   sources!

   To get a strict ANSI C environment, define macro __STRICT_ANSI__.  This will
   "comment out" the non-ANSI parts of the ANSI header files (non-ANSI header
   files aren't affected).  */
/*
 *  $Id: _types.h,v 1.2 2007/04/08 18:38:35 yclin Exp $
 */
/*
 * Guess on types by examining *_MIN / *_MAX defines.
 */
/* Fall back to POSIX versions from <limits.h> */
/* newlib.h.  Generated by configure.  */
/* newlib.hin.  Generated automatically from configure.in by autoheader.  */
/* if do not have #include_next support, then we
   have to define the limits here. */
/* `_GCC_LIMITS_H_' is what GCC's file defines.  */
/* Number of bits in a `char'.  */
/* Maximum length of a multibyte character.  */
/* Minimum and maximum values a `signed char' can hold.  */
/* Maximum value an `unsigned char' can hold.  (Minimum is 0).  */
/* Minimum and maximum values a `char' can hold.  */
/* Minimum and maximum values a `signed short int' can hold.  */
/* Maximum value an `unsigned short int' can hold.  (Minimum is 0).  */
/* Minimum and maximum values a `signed int' can hold.  */
/* Maximum value an `unsigned int' can hold.  (Minimum is 0).  */
/* Minimum and maximum values a `signed long int' can hold.
   (Same as `int').  */
/* Maximum value an `unsigned long int' can hold.  (Minimum is 0).  */
/* Minimum and maximum values a `signed long long int' can hold.  */
/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0).  */
typedef signed char __int8_t ;
typedef unsigned char __uint8_t ;
typedef signed short __int16_t;
typedef unsigned short __uint16_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef signed long long __int64_t;
typedef unsigned long long __uint64_t;
/* POSIX mandates LLONG_MAX in <limits.h> */
/* ANSI C namespace clean utility typedefs */
/* This file defines various typedefs needed by the system calls that support
   the C library.  Basically, they're just the POSIX versions with an '_'
   prepended.  This file lives in the `sys' directory so targets can provide
   their own if desired (or they can put target dependant conditionals here).
*/
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */
/* snaroff@next.com says the NeXT needs this.  */
/* Irix 5.1 needs this.  */
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */
/* Signed type of difference of two pointers.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
typedef int ptrdiff_t;
/* If this symbol has done its job, get rid of it.  */
/* Unsigned type of `sizeof' something.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */
/* A null pointer constant.  */
/* Offset of member MEMBER in a struct of type TYPE. */
/*
 *  The following section is RTEMS specific and is needed to more
 *  closely match the types defined in the BSD machine/types.h.
 *  This is needed to let the RTEMS/BSD TCP/IP stack compile.
 */
typedef long int __off_t;
typedef int __pid_t;
typedef long long int __loff_t;
/* To ensure the stat struct's layout doesn't change when sizeof(int), etc.
   changes, we assume sizeof short and long never change and have all types
   used to define struct stat use them and not int where possible.
   Where not possible, _ST_INTxx are used.  It would be preferable to not have
   such assumptions, but until the extra fluff is necessary, it's avoided.
   No 64 bit targets use stat yet.  What to do about them is postponed
   until necessary.  */
/* also defined in mingw/gmon.h and in w32api/winsock[2].h */
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
typedef unsigned short ushort; /* System V compatibility */
typedef unsigned int uint; /* System V compatibility */
typedef unsigned long clock_t;
typedef long time_t;
/* Time Value Specification Structures, P1003.1b-1993, p. 261 */
struct timespec {
  time_t tv_sec; /* Seconds */
  long tv_nsec; /* Nanoseconds */
};
struct itimerspec {
  struct timespec it_interval; /* Timer period */
  struct timespec it_value; /* Timer expiration */
};
typedef long daddr_t;
typedef char * caddr_t;
typedef unsigned short ino_t;
/*
 * All these should be machine specific - right now they are all broken.
 * However, for all of Cygnus' embedded targets, we want them to all be
 * the same.  Otherwise things like sizeof (struct stat) might depend on
 * how the file was compiled (e.g. -mint16 vs -mint32, etc.).
 */
typedef short dev_t;
typedef long off_t;
typedef unsigned short uid_t;
typedef unsigned short gid_t;
typedef int pid_t;
typedef long key_t;
typedef _ssize_t ssize_t;
typedef unsigned int mode_t;
typedef unsigned short nlink_t;
/* We don't define fd_set and friends if we are compiling POSIX
   source, or if we have included (or may include as indicated
   by __USE_W32_SOCKETS) the W32api winsock[2].h header which
   defines Windows versions of them.   Note that a program which
   includes the W32api winsock[2].h header must know what it is doing;
   it must not call the cygwin32 select function.
*/
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 */
typedef long fd_mask;
/* We use a macro for fd_set so that including Sockets.h afterwards
   can work.  */
typedef struct _tag_types_fd_set {
 fd_mask fds_bits[(((64)+(((sizeof (fd_mask) * 8))-1))/((sizeof (fd_mask) * 8)))];
} _types_fd_set;
typedef unsigned long clockid_t;
typedef unsigned long timer_t;
typedef unsigned long useconds_t;
typedef long suseconds_t;
/*
 *  Written by Joel Sherrill <joel@OARcorp.com>.
 *
 *  COPYRIGHT (c) 1989-2000.
 *
 *  On-Line Applications Research Corporation (OAR).
 *
 *  Permission to use, copy, modify, and distribute this software for any
 *  purpose without fee is hereby granted, provided that this entire notice
 *  is included in all copies of any software which is or includes a copy
 *  or modification of this software.
 *
 *  THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 *  WARRANTY.  IN PARTICULAR,  THE AUTHOR MAKES NO REPRESENTATION
 *  OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS
 *  SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 *
 *  $Id: features.h,v 1.2 2007/04/08 18:38:35 yclin Exp $
 */
/* RTEMS adheres to POSIX -- 1003.1b with some features from annexes.  */
/* Cygwin will probably never have full posix compliance due to little things
 * like an inability to set the stackaddress. Cygwin is also using void *
 * pointers rather than structs to ensure maximum binary compatability with
 * previous releases.
 * This means that we don't use the types defined here, but rather in
 * <cygwin/types.h>
 */
/* POSIX Barrier Types */
/* POSIX Spin Lock Types */
/* POSIX Reader/Writer Lock Types */

typedef __FILE FILE;
typedef _fpos_t fpos_t;
/* This header file provides the reentrancy.  */
/* WARNING: All identifiers here must begin with an underscore.  This file is
   included by stdio.h and others and we therefore must only use identifiers
   in the namespace allotted to us.  */
/* Internal locking macros, used to protect stdio functions.  In the
   general case, expand to nothing. Use __SSTR flag in FILE _flags to
   detect if FILE is private to sprintf/sscanf class of functions; if
   set then do nothing as lock is not initialised. */
 /* RD and WR are never simultaneously asserted */
/*
 * The following three definitions are for ANSI C, which took them
 * from System V, which stupidly took internal interface macros and
 * made them official arguments to setvbuf(), without renaming them.
 * Hence, these ugly _IOxxx names are *supposed* to appear in user code.
 *
 * Although these happen to match their counterparts above, the
 * implementation does not rely on that (so these could be renumbered).
 */
/*
 * Functions defined in ANSI C standard.
 */
FILE * tmpfile (void);
char * tmpnam (char *);
int fclose (FILE *);
int fflush (FILE *);
FILE * freopen (const char *, const char *, FILE *);
void setbuf (FILE *, char *);
int setvbuf (FILE *, char *, int, size_t);
int fprintf (FILE *, const char *, ...);
int fscanf (FILE *, const char *, ...);
int printf (const char *, ...);
int scanf (const char *, ...);
int sscanf (const char *, const char *, ...);
int vfprintf (FILE *, const char *, ...);
int vprintf (const char *, ...);
int vsprintf (char *, const char *, ...);
int fgetc (FILE *);
char * fgets (char *, int, FILE *);
int fputc (int, FILE *);
int fputs (const char *, FILE *);
int getc (FILE *);
int getchar (void);
char * gets (char *);
int putc (int, FILE *);
int putchar (int);
int puts (const char *);
int ungetc (int, FILE *);
size_t fread (void *, size_t _size, size_t _n, FILE *);
size_t fwrite (const void * , size_t _size, size_t _n, FILE *);
int fgetpos (FILE *, fpos_t *);
int fseek (FILE *, long, int);
int fsetpos (FILE *, const fpos_t *);
long ftell ( FILE *);
void rewind (FILE *);
void clearerr (FILE *);
int feof (FILE *);
int ferror (FILE *);
void perror (const char *);
FILE * fopen (const char *_name, const char *_type);
int sprintf (char *, const char *, ...);
int remove (const char *);
int rename (const char *, const char *);
int fseeko (FILE *, off_t, int);
off_t ftello ( FILE *);
int asiprintf (char **, const char *, ...);
int asprintf (char **, const char *, ...);
int dprintf (int, const char *, ...);
int fcloseall (void);
int fiprintf (FILE *, const char *, ...);
int fiscanf (FILE *, const char *, ...);
int iprintf (const char *, ...);
int iscanf (const char *, ...);
int siprintf (char *, const char *, ...);
int siscanf (const char *, const char *, ...);
int snprintf (char *, size_t, const char *, ...);
int sniprintf (char *, size_t, const char *, ...);
char * tempnam (const char *, const char *);
int vasiprintf (char **, const char *, ...);
int vasprintf (char **, const char *, ...);
int vdprintf (int, const char *, ...);
int vsniprintf (char *, size_t, const char *, ...);
int vsnprintf (char *, size_t, const char *, ...);
int vfiprintf (FILE *, const char *, ...);
int vfiscanf (FILE *, const char *, ...);
int vfscanf (FILE *, const char *, ...);
int viprintf (const char *, ...);
int viscanf (const char *, ...);
int vscanf (const char *, ...);
int vsiscanf (const char *, const char *, ...);
int vsscanf (const char *, const char *, ...);
/*
 * Routines in POSIX 1003.1.
 */
FILE * fdopen (int, const char *);
int fileno (FILE *);
int getw (FILE *);
int pclose (FILE *);
FILE * popen (const char *, const char *);
int putw (int, FILE *);
void setbuffer (FILE *, char *, int);
int setlinebuf (FILE *);
int getc_unlocked (FILE *);
int getchar_unlocked (void);
void flockfile (FILE *);
int ftrylockfile (FILE *);
void funlockfile (FILE *);
int putc_unlocked (int, FILE *);
int putchar_unlocked (int);
/*
 * Recursive versions of the above.
 */
int _asiprintf_r (struct _reent *, char **, const char *, ...);
int _asprintf_r (struct _reent *, char **, const char *, ...);
int _dprintf_r (struct _reent *, int, const char *, ...);
int _fcloseall_r (struct _reent *);
FILE * _fdopen_r (struct _reent *, int, const char *);
FILE * _fopen_r (struct _reent *, const char *, const char *);
int _fclose_r (struct _reent *, FILE *);
char * _fgets_r (struct _reent *, char *, int, FILE *);
int _fiscanf_r (struct _reent *, FILE *, const char *, ...);
int _fputc_r (struct _reent *, int, FILE *);
int _fputs_r (struct _reent *, const char *, FILE *);
size_t _fread_r (struct _reent *, void *, size_t _size, size_t _n, FILE *);
int _fscanf_r (struct _reent *, FILE *, const char *, ...);
int _fseek_r (struct _reent *, FILE *, long, int);
long _ftell_r (struct _reent *, FILE *);
size_t _fwrite_r (struct _reent *, const void * , size_t _size, size_t _n, FILE *);
int _getc_r (struct _reent *, FILE *);
int _getc_unlocked_r (struct _reent *, FILE *);
int _getchar_r (struct _reent *);
int _getchar_unlocked_r (struct _reent *);
char * _gets_r (struct _reent *, char *);
int _iprintf_r (struct _reent *, const char *, ...);
int _iscanf_r (struct _reent *, const char *, ...);
int _mkstemp_r (struct _reent *, char *);
char * _mktemp_r (struct _reent *, char *);
void _perror_r (struct _reent *, const char *);
int _printf_r (struct _reent *, const char *, ...);
int _putc_r (struct _reent *, int, FILE *);
int _putc_unlocked_r (struct _reent *, int, FILE *);
int _putchar_unlocked_r (struct _reent *, int);
int _putchar_r (struct _reent *, int);
int _puts_r (struct _reent *, const char *);
int _remove_r (struct _reent *, const char *);
int _rename_r (struct _reent *, const char *_old, const char *_new);
int _scanf_r (struct _reent *, const char *, ...);
int _siprintf_r (struct _reent *, char *, const char *, ...);
int _siscanf_r (struct _reent *, const char *, const char *, ...);
int _sniprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _snprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _sprintf_r (struct _reent *, char *, const char *, ...);
int _sscanf_r (struct _reent *, const char *, const char *, ...);
char * _tempnam_r (struct _reent *, const char *, const char *);
FILE * _tmpfile_r (struct _reent *);
char * _tmpnam_r (struct _reent *, char *);
int _ungetc_r (struct _reent *, int, FILE *);
int _vasiprintf_r (struct _reent *, char **, const char *, ...);
int _vasprintf_r (struct _reent *, char **, const char *, ...);
int _vdprintf_r (struct _reent *, int, const char *, ...);
int _vfiprintf_r (struct _reent *, FILE *, const char *, ...);
int _vfprintf_r (struct _reent *, FILE *, const char *, ...);
int _viprintf_r (struct _reent *, const char *, ...);
int _vprintf_r (struct _reent *, const char *, ...);
int _vsiprintf_r (struct _reent *, char *, const char *, ...);
int _vsprintf_r (struct _reent *, char *, const char *, ...);
int _vsniprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _vsnprintf_r (struct _reent *, char *, size_t, const char *, ...);
int _vfiscanf_r (struct _reent *, FILE *, const char *, ...);
int _vfscanf_r (struct _reent *, FILE *, const char *, ...);
int _viscanf_r (struct _reent *, const char *, ...);
int _vscanf_r (struct _reent *, const char *, ...);
int _vsscanf_r (struct _reent *, const char *, const char *, ...);
int _vsiscanf_r (struct _reent *, const char *, const char *, ...);
ssize_t __getdelim (char **, size_t *, int, FILE *);
ssize_t __getline (char **, size_t *, FILE *);
/*
 * Routines internal to the implementation.
 */
int __srget_r (struct _reent *, FILE *);
int __swbuf_r (struct _reent *, int, FILE *);
/*
 * Stdio function-access interface.
 */
FILE *funopen (const void * _cookie, int (*readfn)(void * _cookie, char *_buf, int _n), int (*writefn)(void * _cookie, const char *_buf, int _n), fpos_t (*seekfn)(void * _cookie, fpos_t _off, int _whence), int (*closefn)(void * _cookie));
/*
 * The __sfoo macros are here so that we can
 * define function versions in the C library.
 */
/*
 * This has been tuned to generate reasonable code on the vax using pcc
 */
/* fast always-buffered version, true iff error */

/*!
 **************************************************************************
 * \file defines.h
 *
 * \brief
 *    Headerfile containing some useful global definitions
 *
 * \author
 *    Detlev Marpe
 *    Copyright (C) 2000 HEINRICH HERTZ INSTITUTE All Rights Reserved.
 *
 * \date
 *    21. March 2001
 **************************************************************************
 */
// CAVLC
//--- block types for CABAC ----
// #define _LEAKYBUCKET_
// Quantization parameter range
/* 4x4 intra prediction modes */
// 16x16 intra prediction modes
// 8x8 chroma intra prediction modes
//#define DECODE_MB_BFRAME 2
//Start code and Emulation Prevention need this to be defined in identical manner at encoder and decoder
/*!
 **************************************************************************************
 * \file
 *    parsetcommon.h
 * \brief
 *    Picture and Sequence Parameter Sets, structures common to encoder and decoder
 *    This code reflects JVT version xxx
 *  \date 25 November 2002
 * \author
 *    Main contributors (see contributors.h for copyright, address and affiliation details)
 *      - Stephan Wenger        <stewe@cs.tu-berlin.de>
 ***************************************************************************************
 */
// In the JVT syntax, frequently flags are used that indicate the presence of
// certain pieces of information in the NALU.  Here, these flags are also
// present.  In the encoder, those bits indicate that the values signalled to
// be present are meaningful and that this part of the syntax should be
// written to the NALU.  In the decoder, the flag indicates that information
// was received from the decoded NALU and should be used henceforth.
// The structure names were chosen as indicated in the JVT syntax
/*!
 **************************************************************************
 * \file defines.h
 *
 * \brief
 *    Headerfile containing some useful global definitions
 *
 * \author
 *    Detlev Marpe
 *    Copyright (C) 2000 HEINRICH HERTZ INSTITUTE All Rights Reserved.
 *
 * \date
 *    21. March 2001
 **************************************************************************
 */
//! Boolean Type
typedef enum {
  FALSE,
  TRUE
} Boolean;
typedef struct
{
  unsigned cpb_cnt; // ue(v)
  unsigned bit_rate_scale; // u(4)
  unsigned cpb_size_scale; // u(4)
    unsigned bit_rate_value [32]; // ue(v)
    unsigned cpb_size_value[32]; // ue(v)
    unsigned vbr_cbr_flag[32]; // u(1)
  unsigned initial_cpb_removal_delay_length_minus1; // u(5)
  unsigned cpb_removal_delay_length_minus1; // u(5)
  unsigned dpb_output_delay_length_minus1; // u(5)
  unsigned time_offset_length; // u(5)
} hrd_parameters_t;
typedef struct
{
  Boolean aspect_ratio_info_present_flag; // u(1)
    unsigned aspect_ratio_idc; // u(8)
      unsigned sar_width; // u(16)
      unsigned sar_height; // u(16)
  Boolean overscan_info_present_flag; // u(1)
    Boolean overscan_appropriate_flag; // u(1)
  Boolean video_signal_type_present_flag; // u(1)
    unsigned video_format; // u(3)
    Boolean video_full_range_flag; // u(1)
    Boolean colour_description_present_flag; // u(1)
      unsigned colour_primaries; // u(8)
      unsigned transfer_characteristics; // u(8)
      unsigned matrix_coefficients; // u(8)
  Boolean chroma_location_info_present_flag; // u(1)
    unsigned chroma_location_frame; // ue(v)
    unsigned chroma_location_field; // ue(v)
  Boolean timing_info_present_flag; // u(1)
    unsigned num_units_in_tick; // u(32)
    unsigned time_scale; // u(32)
    Boolean fixed_frame_rate_flag; // u(1)
  Boolean nal_hrd_parameters_present_flag; // u(1)
    hrd_parameters_t nal_hrd_parameters; // hrd_paramters_t
  Boolean vcl_hrd_parameters_present_flag; // u(1)
    hrd_parameters_t vcl_hrd_parameters; // hrd_paramters_t
  // if ((nal_hrd_parameters_present_flag || (vcl_hrd_parameters_present_flag))
    Boolean low_delay_hrd_flag; // u(1)
  Boolean bitstream_restriction_flag; // u(1)
    Boolean motion_vectors_over_pic_boundaries_flag; // u(1)
    unsigned max_bytes_per_pic_denom; // ue(v)
    unsigned max_bits_per_mb_denom; // ue(v)
    unsigned log2_max_mv_length_vertical; // ue(v)
    unsigned log2_max_mv_length_horizontal; // ue(v)
    unsigned max_dec_frame_reordering; // ue(v)
    unsigned max_dec_frame_buffering; // ue(v)
} vui_seq_parameters_t;
typedef struct
{
  Boolean Valid; // indicates the parameter set is valid
  unsigned pic_parameter_set_id; // ue(v)
  unsigned seq_parameter_set_id; // ue(v)
  Boolean entropy_coding_mode_flag; // u(1)
  // if( pic_order_cnt_type < 2 )  in the sequence parameter set
  Boolean pic_order_present_flag; // u(1)
  unsigned num_slice_groups_minus1; // ue(v)
    unsigned slice_group_map_type; // ue(v)
    // if( slice_group_map_type = = 0 )
      unsigned run_length_minus1[8]; // ue(v)
    // else if( slice_group_map_type = = 2 )
      unsigned top_left[8]; // ue(v)
      unsigned bottom_right[8]; // ue(v)
    // else if( slice_group_map_type = = 3 || 4 || 5
      Boolean slice_group_change_direction_flag; // u(1)
      unsigned slice_group_change_rate_minus1; // ue(v)
    // else if( slice_group_map_type = = 6 )
      unsigned num_slice_group_map_units_minus1; // ue(v)
      unsigned *slice_group_id; // complete MBAmap u(v)
  unsigned num_ref_idx_l0_active_minus1; // ue(v)
  unsigned num_ref_idx_l1_active_minus1; // ue(v)
  Boolean weighted_pred_flag; // u(1)
  Boolean weighted_bipred_idc; // u(2)
  int pic_init_qp_minus26; // se(v)
  int pic_init_qs_minus26; // se(v)
  int chroma_qp_index_offset; // se(v)
  Boolean deblocking_filter_control_present_flag; // u(1)
  Boolean constrained_intra_pred_flag; // u(1)
  Boolean redundant_pic_cnt_present_flag; // u(1)
  Boolean vui_pic_parameters_flag; // u(1)
} pic_parameter_set_rbsp_t;
typedef struct
{
  Boolean Valid; // indicates the parameter set is valid
  unsigned profile_idc; // u(8)
  Boolean constrained_set0_flag; // u(1)
  Boolean constrained_set1_flag; // u(1)
  Boolean constrained_set2_flag; // u(1)
  unsigned level_idc; // u(8)
  unsigned seq_parameter_set_id; // ue(v)
  unsigned log2_max_frame_num_minus4; // ue(v)
  unsigned pic_order_cnt_type;
  // if( pic_order_cnt_type == 0 )
  unsigned log2_max_pic_order_cnt_lsb_minus4; // ue(v)
  // else if( pic_order_cnt_type == 1 )
    Boolean delta_pic_order_always_zero_flag; // u(1)
    int offset_for_non_ref_pic; // se(v)
    int offset_for_top_to_bottom_field; // se(v)
    unsigned num_ref_frames_in_pic_order_cnt_cycle; // ue(v)
    // for( i = 0; i < num_ref_frames_in_pic_order_cnt_cycle; i++ )
      int offset_for_ref_frame[255]; // se(v)
  unsigned num_ref_frames; // ue(v)
  Boolean gaps_in_frame_num_value_allowed_flag; // u(1)
  unsigned pic_width_in_mbs_minus1; // ue(v)
  unsigned pic_height_in_map_units_minus1; // ue(v)
  Boolean frame_mbs_only_flag; // u(1)
  // if( !frame_mbs_only_flag )
    Boolean mb_adaptive_frame_field_flag; // u(1)
  Boolean direct_8x8_inference_flag; // u(1)
  Boolean frame_cropping_flag; // u(1)
    unsigned frame_cropping_rect_left_offset; // ue(v)
    unsigned frame_cropping_rect_right_offset; // ue(v)
    unsigned frame_cropping_rect_top_offset; // ue(v)
    unsigned frame_cropping_rect_bottom_offset; // ue(v)
  Boolean vui_parameters_present_flag; // u(1)
    vui_seq_parameters_t vui_seq_parameters; // vui_seq_parameters_t
} seq_parameter_set_rbsp_t;
pic_parameter_set_rbsp_t *AllocPPS ();
seq_parameter_set_rbsp_t *AllocSPS ();
void FreePPS (pic_parameter_set_rbsp_t *pps);
void FreeSPS (seq_parameter_set_rbsp_t *sps);
typedef unsigned char byte; //!<  8 bit unsigned
typedef int int32;
typedef unsigned int u_int32;
pic_parameter_set_rbsp_t *active_pps;
seq_parameter_set_rbsp_t *active_sps;
// global picture format dependend buffers, mem allocation in decod.c ******************
int **refFrArr; //!< Array for reference frames of each block
byte **imgY_ref; //!< reference frame find snr
byte ***imgUV_ref;
int ReMapRef[20];
// B pictures
int Bframe_ctr;
int frame_no;
// For MB level frame/field coding
int TopFieldForSkip_Y[16][16];
int TopFieldForSkip_UV[2][16][16];
char errortext[300]; //!< buffer for error message for exit with error()
int g_new_frame;
/***********************************************************************
 * T y p e    d e f i n i t i o n s    f o r    T M L
 ***********************************************************************
 */
//! Data Partitioning Modes
typedef enum
{
  PAR_DP_1, //<! no data partitioning is supported
  PAR_DP_3, //<! data partitioning with 3 partitions
} PAR_DP_TYPE;
//! Output File Types
typedef enum
{
  PAR_OF_ANNEXB, //<! Current TML description
  PAR_OF_RTP, //<! RTP Packet Output format
//  PAR_OF_IFF    //<! Interim File Format
} PAR_OF_TYPE;
//! Boolean Type
/*typedef enum {
  FALSE,
  TRUE
} Boolean;
*/
//! definition of H.26L syntax elements
typedef enum {
  SE_HEADER,
  SE_PTYPE,
  SE_MBTYPE,
  SE_REFFRAME,
  SE_INTRAPREDMODE,
  SE_MVD,
  SE_CBP_INTRA,
  SE_LUM_DC_INTRA,
  SE_CHR_DC_INTRA,
  SE_LUM_AC_INTRA,
  SE_CHR_AC_INTRA,
  SE_CBP_INTER,
  SE_LUM_DC_INTER,
  SE_CHR_DC_INTER,
  SE_LUM_AC_INTER,
  SE_CHR_AC_INTER,
  SE_DELTA_QUANT_INTER,
  SE_DELTA_QUANT_INTRA,
  SE_BFRAME,
  SE_EOS,
  SE_MAX_ELEMENTS //!< number of maximum syntax elements, this MUST be the last one!
} SE_type; // substituting the definitions in element.h
typedef enum {
  INTER_MB,
  INTRA_MB_4x4,
  INTRA_MB_16x16
} IntraInterDecision;
typedef enum {
  BITS_TOTAL_MB,
  BITS_HEADER_MB,
  BITS_INTER_MB,
  BITS_CBP_MB,
  BITS_COEFF_Y_MB,
  BITS_COEFF_UV_MB,
  MAX_BITCOUNTER_MB
} BitCountType;
typedef enum {
  NO_SLICES,
  FIXED_MB,
  FIXED_RATE,
  CALLBACK,
  FMO
} SliceMode;
typedef enum {
  UVLC,
  CABAC
} SymbolMode;
typedef enum {
  FRAME,
  TOP_FIELD,
  BOTTOM_FIELD
} PictureStructure; //!< New enum for field processing
typedef enum {
  P_SLICE = 0,
  B_SLICE,
  I_SLICE,
  SP_SLICE,
  SI_SLICE
} SliceType;
/***********************************************************************
 * D a t a    t y p e s   f o r  C A B A C
 ***********************************************************************
 */
//! struct to characterize the state of the arithmetic coding engine
typedef struct
{
  unsigned int Dlow, Drange;
  unsigned int Dvalue;
  unsigned int Dbuffer;
  int Dbits_to_go;
  byte *Dcodestrm;
  int *Dcodestrm_len;
} DecodingEnvironment;
typedef DecodingEnvironment *DecodingEnvironmentPtr;
//! struct for context management
typedef struct
{
  unsigned short state; // index into state-table CP
  unsigned char MPS; // Least Probable Symbol 0/1 CP
} BiContextType;
typedef BiContextType *BiContextTypePtr;
/**********************************************************************
 * C O N T E X T S   F O R   T M L   S Y N T A X   E L E M E N T S
 **********************************************************************
 */
typedef struct
{
  BiContextType mb_type_contexts [4][11];
  BiContextType b8_type_contexts [2][9];
  BiContextType mv_res_contexts [2][10];
  BiContextType ref_no_contexts [2][6];
  BiContextType delta_qp_contexts[4];
  BiContextType mb_aff_contexts [4];
} MotionInfoContexts;
typedef struct
{
  BiContextType ipr_contexts [2];
  BiContextType cipr_contexts[4];
  BiContextType cbp_contexts [3][4];
  BiContextType bcbp_contexts[8][4];
  BiContextType map_contexts [8][15];
  BiContextType last_contexts[8][15];
  BiContextType one_contexts [8][5];
  BiContextType abs_contexts [8][5];
  BiContextType fld_map_contexts [8][15];
  BiContextType fld_last_contexts[8][15];
} TextureInfoContexts;
//*********************** end of data type definition for CABAC *******************
/***********************************************************************
 * N e w   D a t a    t y p e s   f o r    T M L
 ***********************************************************************
 */
struct img_par;
struct inp_par;
struct stat_par;
/*! Buffer structure for decoded referenc picture marking commands */
typedef struct DecRefPicMarking_s
{
  int memory_management_control_operation;
  int difference_of_pic_nums_minus1;
  int long_term_pic_num;
  int long_term_frame_idx;
  int max_long_term_frame_idx_plus1;
  struct DecRefPicMarking_s *Next;
} DecRefPicMarking_t;
//! Syntaxelement
typedef struct syntaxelement
{
  int type; //!< type of syntax element for data part.
  int value1; //!< numerical value of syntax element
  int value2; //!< for blocked symbols, e.g. run/level
  int len; //!< length of code
  int inf; //!< info part of UVLC code
  unsigned int bitpattern; //!< UVLC bitpattern
  int context; //!< CABAC context
  int k; //!< CABAC context for coeff_count,uv
  //! for mapping of UVLC to syntaxElement
  void (*mapping)(int len, int info, int *value1, int *value2);
  //! used for CABAC: refers to actual coding method of each individual syntax element type
  void (*reading)(struct syntaxelement *, struct inp_par *, struct img_par *, DecodingEnvironmentPtr);
} SyntaxElement;
//! Macroblock
typedef struct macroblock
{
  int qp;
  int slice_nr;
  int delta_quant; //!< for rate control
  struct macroblock *mb_available_up; //!< pointer to neighboring MB (CABAC)
  struct macroblock *mb_available_left; //!< pointer to neighboring MB (CABAC)
  // some storage of macroblock syntax elements for global access
  int mb_type;
  int mvd[2][(16/4)][(16/4)][2]; //!< indices correspond to [forw,backw][block_y][block_x][x,y]
  int cbp, cbp_blk ;
  unsigned long cbp_bits;
  int is_skip;
  int i16mode;
  int b8mode[4];
  int b8pdir[4];
  int ei_flag;
  int LFDisableIdc;
  int LFAlphaC0Offset;
  int LFBetaOffset;
  int c_ipred_mode; //!< chroma intra prediction mode
  int mb_field;
  int mbAddrA, mbAddrB, mbAddrC, mbAddrD;
  int mbAvailA, mbAvailB, mbAvailC, mbAvailD;
} Macroblock;
//! Bitstream
typedef struct
{
  // CABAC Decoding
  int read_len; //!< actual position in the codebuffer, CABAC only
  int code_len; //!< overall codebuffer length, CABAC only
  // UVLC Decoding
  int frame_bitoffset; //!< actual position in the codebuffer, bit-oriented, UVLC only
  int bitstream_length; //!< over codebuffer lnegth, byte oriented, UVLC only
  // ErrorConcealment
  byte *streamBuffer; //!< actual codebuffer for read bytes
  int ei_flag; //!< error indication, 0: no error, else unspecified error
} Bitstream;
//! DataPartition
typedef struct datapartition
{
  Bitstream *bitstream;
  DecodingEnvironment de_cabac;
  int (*readSyntaxElement)(SyntaxElement *, struct img_par *, struct inp_par *, struct datapartition *);
          /*!< virtual function;
               actual method depends on chosen data partition and
               entropy coding method  */
} DataPartition;
//! Slice
typedef struct
{
  int ei_flag; //!< 0 if the partArr[0] contains valid information
  int qp;
  int picture_type; //!< picture type
  PictureStructure structure; //!< Identify picture structure type
  int start_mb_nr; //!< MUST be set by NAL even in case of ei_flag == 1
  int max_part_nr;
  int dp_mode; //!< data partioning mode
  int next_header;
//  int                 last_mb_nr;    //!< only valid when entropy coding == CABAC
  DataPartition *partArr; //!< array of partitions
  MotionInfoContexts *mot_ctx; //!< pointer to struct of context models for use in CABAC
  TextureInfoContexts *tex_ctx; //!< pointer to struct of context models for use in CABAC
  int ref_pic_list_reordering_flag_l0;
  int *remapping_of_pic_nums_idc_l0;
  int *abs_diff_pic_num_minus1_l0;
  int *long_term_pic_idx_l0;
  int ref_pic_list_reordering_flag_l1;
  int *remapping_of_pic_nums_idc_l1;
  int *abs_diff_pic_num_minus1_l1;
  int *long_term_pic_idx_l1;
  int (*readSlice)(struct img_par *, struct inp_par *);
  int LFDisableIdc; //!< Disable loop filter on slice
  int LFAlphaC0Offset; //!< Alpha and C0 offset for filtering slice
  int LFBetaOffset; //!< Beta offset for filtering slice
  int pic_parameter_set_id; //!<the ID of the picture parameter set the slice is reffering to
} Slice;
//****************************** ~DM ***********************************
// image parameters
typedef struct img_par
{
  int number; //<! frame number
  unsigned current_mb_nr; // bitstream order
  unsigned num_dec_mb;
  int current_slice_nr;
  int *intra_block;
  int tr; //<! temporal reference, 8 bit, wrapps at 255
  int tr_old; //<! old temporal reference, for detection of a new frame, added by WYK
  int qp; //<! quant for the current frame
  int qpsp; //<! quant for SP-picture predicted frame
  int sp_switch; //<! 1 for switching sp, 0 for normal sp
  int direct_type; //<! 1 for Spatial Direct, 0 for Temporal
  int type; //<! image type INTER/INTRA
  int width;
  int height;
  int width_cr; //<! width chroma
  int height_cr; //<! height chroma
  int mb_y;
  int mb_x;
  int block_y;
  int pix_y;
  int pix_x;
  int pix_c_y;
  int block_x;
  int pix_c_x;
  int allrefzero;
  int mpr[16][16]; //<! predicted block
  int m7[16][16]; //<! final 4x4 block. Extended to 16x16 for ABT
  int cof[4][6][4][4]; //<! correction coefficients from predicted
  int cofu[4];
  int **ipredmode; //<! prediction type [90][74]
  int quad_t[256];
  int constrained_intra_pred_flag; //<! if 1, prediction only from other Intra MBs
  int ***nz_coeff;
  int **siblock;
  int cod_counter; //<! Current count of number of skipped macroblocks in a row
  int structure; //<! Identify picture structure type
  int structure_old; //<! temp fix for multi slice per picture
  int pstruct_next_P;
  // B pictures
  Slice *currentSlice; //<! pointer to current Slice data struct
  Macroblock *mb_data; //<! array containing all MBs of a whole frame
  int subblock_x;
  int subblock_y;
  int is_intra_block;
  int is_v_block;
  // For MB level frame/field coding
  int MbaffFrameFlag;
  int **field_anchor;
  DecRefPicMarking_t *dec_ref_pic_marking_buffer; //<! stores the memory management control operations
  int disposable_flag; //!< flag for disposable frame, 1:disposable
  int num_ref_idx_l0_active; //!< number of forward reference
  int num_ref_idx_l1_active; //!< number of backward reference
  int slice_group_change_cycle;
  // JVT-D101
  int redundant_slice_flag;
  int redundant_pic_cnt;
  int explicit_B_prediction;
  // End JVT-D101
  // POC200301: from unsigned int to int
           int toppoc; //poc for this top field // POC200301
           int bottompoc; //poc of bottom field of frame
           int framepoc; //poc of this frame // POC200301
  unsigned int frame_num; //frame_num for this frame
  unsigned int field_pic_flag;
  unsigned int bottom_field_flag;
  //the following should probably go in sequence parameters
  // unsigned int log2_max_frame_num_minus4;
  unsigned int pic_order_cnt_type;
  // for poc mode 0, POC200301
  // unsigned int log2_max_pic_order_cnt_lsb_minus4;
  // for poc mode 1, POC200301
  unsigned int delta_pic_order_always_zero_flag;
           int offset_for_non_ref_pic;
           int offset_for_top_to_bottom_field;
  unsigned int num_ref_frames_in_pic_order_cnt_cycle;
           int offset_for_ref_frame[255];
  // POC200301
  //the following is for slice header syntax elements of poc
  // for poc mode 0.
  unsigned int pic_order_cnt_lsb;
           int delta_pic_order_cnt_bottom;
  // for poc mode 1.
           int delta_pic_order_cnt[2];
  // ////////////////////////
  // for POC mode 0:
    signed int PicOrderCntMsb;
  unsigned int PrevPicOrderCntLsb;
    signed int CurrPicOrderCntMsb;
  // for POC mode 1:
  unsigned int AbsFrameNum;
    signed int ExpectedPicOrderCnt, PicOrderCntCycleCnt, FrameNumInPicOrderCntCycle;
  unsigned int PreviousFrameNum, FrameNumOffset;
           int ExpectedDeltaPerPicOrderCntCycle;
           int PreviousPOC, ThisPOC;
           int PreviousFrameNumOffset;
  // /////////////////////////
  //weighted prediction
  unsigned int weighted_pred_flag;
  unsigned int weighted_bipred_idc;
  unsigned int luma_log2_weight_denom;
  unsigned int chroma_log2_weight_denom;
  int ***wp_weight; // weight in [list][index][component] order
  int ***wp_offset; // offset in [list][index][component] order
  int ****wbp_weight; //weight in [list][fw_index][bw_index][component] order
  int wp_round_luma;
  int wp_round_chroma;
  unsigned int apply_weights;
  unsigned int pic_order_present_flag;
  int idr_flag;
  int nal_reference_idc; //!< nal_reference_idc from NAL unit
  int idr_pic_id;
  int MaxFrameNum;
  unsigned PicWidthInMbs;
  unsigned PicHeightInMapUnits;
  unsigned FrameHeightInMbs;
  unsigned PicHeightInMbs;
  unsigned PicSizeInMbs;
  unsigned FrameSizeInMbs;
  int no_output_of_prior_pics_flag;
  int long_term_reference_flag;
  int adaptive_ref_pic_buffering_flag;
  int model_number;
} ImageParameters;
extern ImageParameters *img;
extern struct snr_par *snr;
// signal to noice ratio parameters
struct snr_par
{
  float snr_y; //<! current Y SNR
  float snr_u; //<! current U SNR
  float snr_v; //<! current V SNR
  float snr_y1; //<! SNR Y(dB) first frame
  float snr_u1; //<! SNR U(dB) first frame
  float snr_v1; //<! SNR V(dB) first frame
  float snr_ya; //<! Average SNR Y(dB) remaining frames
  float snr_ua; //<! Average SNR U(dB) remaining frames
  float snr_va; //<! Average SNR V(dB) remaining frames
};
int tot_time;
// input parameters from configuration file
struct inp_par
{
  char infile[100]; //<! Telenor H.26L input
  char outfile[100]; //<! Decoded YUV 4:2:0 output
  char reffile[100]; //<! Optional YUV 4:2:0 reference file for SNR measurement
  int FileFormat; //<! File format of the Input file, PAR_OF_ANNEXB or PAR_OF_RTP
  int dpb_size; //<! Frame buffer size
};
extern struct inp_par *input;
typedef struct pix_pos
{
  int available;
  int mb_addr;
  int x;
  int y;
  int pos_x;
  int pos_y;
} PixelPos;
// files
FILE *p_out; //<! pointer to output YUV file
//FILE *p_out2;                    //<! pointer to debug output YUV file
FILE *p_ref; //<! pointer to input original reference YUV file file
FILE *p_log; //<! SNR file
// prototypes
void init_conf(struct inp_par *inp, char *config_filename);
void report(struct inp_par *inp, struct img_par *img, struct snr_par *snr);
void init(struct img_par *img);
void malloc_slice(struct inp_par *inp, struct img_par *img);
void free_slice(struct inp_par *inp, struct img_par *img);
int decode_one_frame(struct img_par *img,struct inp_par *inp, struct snr_par *snr);
void init_frame(struct img_par *img, struct inp_par *inp);
void exit_frame(struct img_par *img, struct inp_par *inp);
void DeblockFrame(struct img_par *img, byte **imgY, byte ***imgUV ) ;
int read_new_slice();
void decode_one_slice(struct img_par *img,struct inp_par *inp);
void start_macroblock(struct img_par *img,struct inp_par *inp, int CurrentMBInScanOrder);
int read_one_macroblock(struct img_par *img,struct inp_par *inp);
void read_ipred_modes(struct img_par *img,struct inp_par *inp);
int decode_one_macroblock(struct img_par *img,struct inp_par *inp);
int exit_macroblock(struct img_par *img,struct inp_par *inp, int eos_bit);
void readMotionInfoFromNAL (struct img_par *img,struct inp_par *inp);
void readCBPandCoeffsFromNAL(struct img_par *img,struct inp_par *inp);
void copyblock_sp(struct img_par *img,int block_x,int block_y);
void itrans_sp_chroma(struct img_par *img,int ll);
void itrans(struct img_par *img,int ioff,int joff,int i0,int j0);
void itrans_sp(struct img_par *img,int ioff,int joff,int i0,int j0);
int intrapred(struct img_par *img,int ioff,int joff,int i4,int j4);
void itrans_2(struct img_par *img);
int intrapred_luma_16x16(struct img_par *img,int predmode);
void intrapred_chroma(struct img_par *img, int uv);
int sign(int a , int b);
// SLICE function pointers
int (*nal_startcode_follows) ();
// NAL functions TML/CABAC bitstream
int uvlc_startcode_follows();
int cabac_startcode_follows();
void free_Partition(Bitstream *currStream);
// ErrorConcealment
void reset_ec_flags();
void error(char *text, int code);
// dynamic mem allocation
int init_global_buffers(struct inp_par *inp, struct img_par *img);
void free_global_buffers(struct inp_par *inp, struct img_par *img);
void frame_postprocessing(struct img_par *img, struct inp_par *inp);
void field_postprocessing(struct img_par *img, struct inp_par *inp);
int bottom_field_picture(struct img_par *img,struct inp_par *inp);
void init_top(struct img_par *img, struct inp_par *inp);
void init_bottom(struct img_par *img, struct inp_par *inp);
void decode_frame_slice(struct img_par *img,struct inp_par *inp, int current_header);
void decode_field_slice(struct img_par *img,struct inp_par *inp, int current_header);
int RBSPtoSODB(byte *streamBuffer, int last_byte_pos);
int EBSPtoRBSP(byte *streamBuffer, int end_bytepos, int begin_bytepos);
// For MB level frame/field coding
void init_super_macroblock(struct img_par *img,struct inp_par *inp);
void exit_super_macroblock(struct img_par *img,struct inp_par *inp);
int decode_super_macroblock(struct img_par *img,struct inp_par *inp);
void decode_one_Copy_topMB(struct img_par *img,struct inp_par *inp);
void SetOneRefMV(struct img_par* img);
int peekSyntaxElement_UVLC(SyntaxElement *sym, struct img_par *img, struct inp_par *inp, struct datapartition *dP);
void fill_wp_params(struct img_par *img);
void reset_wp_params(struct img_par *img);
void FreePartition (DataPartition *dp, int n);
DataPartition *AllocPartition();
void tracebits2(const char *trace_str, int len, int info);
/*!
 ************************************************************************
 * \file block.h
 *
 * \author
 *  Inge Lille-Langøy               <inge.lille-langoy@telenor.com>    \n
 *  Telenor Satellite Services                                         \n
 *  P.O.Box 6914 St.Olavs plass                                        \n
 *  N-0130 Oslo, Norway
 *
 ************************************************************************
 */
/*!
 ************************************************************************
 *  \file
 *     global.h
 *  \brief
 *     global definitions for for H.26L decoder.
 *  \author
 *     Copyright (C) 1999  Telenor Satellite Services,Norway
 *                         Ericsson Radio Systems, Sweden
 *
 *     Inge Lille-Langøy               <inge.lille-langoy@telenor.com>
 *
 *     Telenor Satellite Services
 *     Keysers gt.13                       tel.:   +47 23 13 86 98
 *     N-0130 Oslo,Norway                  fax.:   +47 22 77 79 80
 *
 *     Rickard Sjoberg                 <rickard.sjoberg@era.ericsson.se>
 *
 *     Ericsson Radio Systems
 *     KI/ERA/T/VV
 *     164 80 Stockholm, Sweden
 *
 ************************************************************************
 */
extern const byte QP_SCALE_CR[52] ;
extern const int dequant_coef[6][4][4];
/*!
 ************************************************************************
 * \file image.h
 *
 ************************************************************************
 */
/*!
 ***********************************************************************
 *  \file
 *      mbuffer.h
 *
 *  \brief
 *      Frame buffer functions
 *
 *  \author
 *      Main contributors (see contributors.h for copyright, address and affiliation details)
 *      - Karsten Sühring          <suehring@hhi.de>
 ***********************************************************************
 */
/*!
 ************************************************************************
 *  \file
 *     global.h
 *  \brief
 *     global definitions for for H.26L decoder.
 *  \author
 *     Copyright (C) 1999  Telenor Satellite Services,Norway
 *                         Ericsson Radio Systems, Sweden
 *
 *     Inge Lille-Langøy               <inge.lille-langoy@telenor.com>
 *
 *     Telenor Satellite Services
 *     Keysers gt.13                       tel.:   +47 23 13 86 98
 *     N-0130 Oslo,Norway                  fax.:   +47 22 77 79 80
 *
 *     Rickard Sjoberg                 <rickard.sjoberg@era.ericsson.se>
 *
 *     Ericsson Radio Systems
 *     KI/ERA/T/VV
 *     164 80 Stockholm, Sweden
 *
 ************************************************************************
 */
//! definition a picture (field or frame)
typedef struct storable_picture
{
  PictureStructure structure;
  int poc;
  int top_poc;
  int bottom_poc;
  int order_num;
  int ref_pic_num[6][20];
  int pic_num;
  int long_term_pic_num;
  int long_term_frame_idx;
  int is_long_term;
  int used_for_reference;
  int size_x, size_y, size_x_cr, size_y_cr;
  int chroma_vector_adjustment;
  int coded_frame;
  int mb_adaptive_frame_field_flag;
  byte ** imgY;
  byte *** imgUV;
  byte * mb_field; //<! field macroblock indicator
  int *** ref_idx; //<! reference picture   [list][subblock_x][subblock_y]
                             //   [list][mb_nr][subblock_x][subblock_y]
  int *** ref_pic_id; //<! reference picture identifier [list][subblock_x][subblock_y]
                             //   (not  simply index)
  int **** mv; //<! motion vector       [list][subblock_x][subblock_y][component]
  byte ** moving_block;
  struct storable_picture *top_field; // for mb aff, if frame for referencing the top field
  struct storable_picture *bottom_field; // for mb aff, if frame for referencing the bottom field
  struct storable_picture *frame; // for mb aff, if field for referencing the combined frame
} StorablePicture;
//! Frame Stores for Decoded Picture Buffer
typedef struct frame_store
{
  int is_used; //<! 0=empty; 1=top; 2=bottom; 3=both fields (or frame)
  int is_reference; //<! 0=not used for ref; 1=top used; 2=bottom used; 3=both fields (or frame) used
  int is_long_term; //<! 0=not used for ref; 1=top used; 2=bottom used; 3=both fields (or frame) used
  int is_non_existent;
  unsigned frame_num;
  int frame_num_wrap;
  int long_term_frame_idx;
  int is_output;
  int poc;
  StorablePicture *frame;
  StorablePicture *top_field;
  StorablePicture *bottom_field;
} FrameStore;
//! Decoded Picture Buffer
typedef struct decoded_picture_buffer
{
  FrameStore **fs;
  FrameStore **fs_ref;
  FrameStore **fs_ltref;
  unsigned size;
  unsigned used_size;
  unsigned ref_frames_in_buffer;
  unsigned ltref_frames_in_buffer;
  int last_output_poc;
  int max_long_term_pic_idx;
} DecodedPictureBuffer;
extern DecodedPictureBuffer dpb;
extern StorablePicture **listX[6];
extern int listXsize[6];
void init_dpb(struct inp_par *inp);
void free_dpb();
FrameStore* alloc_frame_store();
void free_frame_store(FrameStore* f);
StorablePicture* alloc_storable_picture(PictureStructure type, int size_x, int size_y, int size_x_cr, int size_y_cr);
void free_storable_picture(StorablePicture* p);
void store_picture_in_dpb(StorablePicture* p);
void flush_dpb();
void dpb_split_field(FrameStore *fs);
void dpb_combine_field(FrameStore *fs);
void init_lists(int currSliceType, PictureStructure currPicStructure);
void reorder_ref_pic_list(StorablePicture **list, int *list_size,
                                      int num_ref_idx_lX_active_minus1, int *remapping_of_pic_nums_idc,
                                      int *abs_diff_pic_num_minus1, int *long_term_pic_idx);
void init_mbaff_lists();
void alloc_ref_pic_list_reordering_buffer(Slice *currSlice);
void free_ref_pic_list_reordering_buffer(Slice *currSlice);
// this one is empty. keep it, maybe we will move some image.c function
// declarations here
extern StorablePicture *dec_picture;
void find_snr(struct snr_par *snr, StorablePicture *p, FILE *p_ref);
void get_block(int ref_frame, StorablePicture **list, int x_pos, int y_pos, struct img_par *img, int block[4][4]);
int picture_order(struct img_par *img);
/*!
 *************************************************************************************
 * \file mb_access.h
 *
 * \brief
 *    Functions for macroblock neighborhoods
 *
 * \author
 *     Main contributors (see contributors.h for copyright, address and affiliation details)
 *     - Karsten Sühring          <suehring@hhi.de>
 *************************************************************************************
 */
void CheckAvailabilityOfNeighbors();
void getNeighbour(int curr_mb_nr, int xN, int yN, int luma, PixelPos *pix);
void getLuma4x4Neighbour (int curr_mb_nr, int block_x, int block_y, int rel_x, int rel_y, PixelPos *pix);
void getChroma4x4Neighbour (int curr_mb_nr, int block_x, int block_y, int rel_x, int rel_y, PixelPos *pix);
int mb_is_available(int mbAddr, int currMbAddr);
void get_mb_pos (int mb_addr, int *x, int*y);
void get_mb_block_pos (int mb_addr, int *x, int*y);
static const int quant_coef[6][4][4] = {
  {{13107, 8066,13107, 8066},{ 8066, 5243, 8066, 5243},{13107, 8066,13107, 8066},{ 8066, 5243, 8066, 5243}},
  {{11916, 7490,11916, 7490},{ 7490, 4660, 7490, 4660},{11916, 7490,11916, 7490},{ 7490, 4660, 7490, 4660}},
  {{10082, 6554,10082, 6554},{ 6554, 4194, 6554, 4194},{10082, 6554,10082, 6554},{ 6554, 4194, 6554, 4194}},
  {{ 9362, 5825, 9362, 5825},{ 5825, 3647, 5825, 3647},{ 9362, 5825, 9362, 5825},{ 5825, 3647, 5825, 3647}},
  {{ 8192, 5243, 8192, 5243},{ 5243, 3355, 5243, 3355},{ 8192, 5243, 8192, 5243},{ 5243, 3355, 5243, 3355}},
  {{ 7282, 4559, 7282, 4559},{ 4559, 2893, 4559, 2893},{ 7282, 4559, 7282, 4559},{ 4559, 2893, 4559, 2893}}
};
static const int A[4][4] = {
  { 16, 20, 16, 20},
  { 20, 25, 20, 25},
  { 16, 20, 16, 20},
  { 20, 25, 20, 25}
};

void itrans_sp_chroma(struct img_par *img,int ll)
{
  int i,j,i1,j2,ilev,n2,n1,j1,mb_y;
  int m5[4];
  int predicted_chroma_block[16/2][16/2],mp1[4];
  int qp_per,qp_rem,q_bits;
  int qp_per_sp,qp_rem_sp,q_bits_sp,qp_const2;
  qp_per = ((img->qp<0?img->qp:QP_SCALE_CR[img->qp])-0)/6;
  qp_rem = ((img->qp<0?img->qp:QP_SCALE_CR[img->qp])-0)%6;
  q_bits = 15 +qp_per;
  qp_per_sp = ((img->qpsp<0?img->qpsp:QP_SCALE_CR[img->qpsp])-0)/6;
  qp_rem_sp = ((img->qpsp<0?img->qpsp:QP_SCALE_CR[img->qpsp])-0)%6;
  q_bits_sp = 15 +qp_per_sp;
  qp_const2=(1<<q_bits_sp)/2; //sp_pred
  if (img->sp_switch || img->type == SI_SLICE)
  {
    qp_per = ((img->qpsp < 0 ? img->qpsp : QP_SCALE_CR[img->qpsp]) - 0) / 6;
    qp_rem = ((img->qpsp < 0 ? img->qpsp : QP_SCALE_CR[img->qpsp]) - 0) % 6;
    q_bits = 15 + qp_per;
  }
  for (j=0; j < 16/2; j++)
  for (i=0; i < 16/2; i++)
  {
    predicted_chroma_block[i][j]=img->mpr[i][j];
    img->mpr[i][j]=0;
  }
  for (n2=0; n2 <= 4; n2 += 4)
  {
    for (n1=0; n1 <= 4; n1 += 4)
    {
      //  Horizontal transform.
      for (j=0; j < 4; j++)
      {
        mb_y=n2+j;
        for (i=0; i < 2; i++)
        {
          i1=3-i;
          m5[i]=predicted_chroma_block[i+n1][mb_y]+predicted_chroma_block[i1+n1][mb_y];
          m5[i1]=predicted_chroma_block[i+n1][mb_y]-predicted_chroma_block[i1+n1][mb_y];
        }
        predicted_chroma_block[n1][mb_y] =(m5[0]+m5[1]);
        predicted_chroma_block[n1+2][mb_y]=(m5[0]-m5[1]);
        predicted_chroma_block[n1+1][mb_y]=m5[3]*2+m5[2];
        predicted_chroma_block[n1+3][mb_y]=m5[3]-m5[2]*2;
      }
      //  Vertical transform.
      for (i=0; i < 4; i++)
      {
        j1=n1+i;
        for (j=0; j < 2; j++)
        {
          j2=3-j;
          m5[j]=predicted_chroma_block[j1][n2+j]+predicted_chroma_block[j1][n2+j2];
          m5[j2]=predicted_chroma_block[j1][n2+j]-predicted_chroma_block[j1][n2+j2];
        }
        predicted_chroma_block[j1][n2+0]=(m5[0]+m5[1]);
        predicted_chroma_block[j1][n2+2]=(m5[0]-m5[1]);
        predicted_chroma_block[j1][n2+1]=m5[3]*2+m5[2];
        predicted_chroma_block[j1][n2+3]=m5[3]-m5[2]*2;
      }
    }
  }
  //     2X2 transform of DC coeffs.
  mp1[0]=(predicted_chroma_block[0][0]+predicted_chroma_block[4][0]+predicted_chroma_block[0][4]+predicted_chroma_block[4][4]);
  mp1[1]=(predicted_chroma_block[0][0]-predicted_chroma_block[4][0]+predicted_chroma_block[0][4]-predicted_chroma_block[4][4]);
  mp1[2]=(predicted_chroma_block[0][0]+predicted_chroma_block[4][0]-predicted_chroma_block[0][4]-predicted_chroma_block[4][4]);
  mp1[3]=(predicted_chroma_block[0][0]-predicted_chroma_block[4][0]-predicted_chroma_block[0][4]+predicted_chroma_block[4][4]);
  for (n1=0; n1 < 2; n1 ++)
  for (n2=0; n2 < 2; n2 ++)
  {
    ilev=((img->cof[n1+ll][4+n2][0][0]*dequant_coef[qp_rem][0][0]*A[0][0]<< qp_per) >>5)+mp1[n1+n2*2] ;
    mp1[n1+n2*2]=sign((abs(ilev)* quant_coef[qp_rem_sp][0][0]+ 2 * qp_const2)>> (q_bits_sp+1),ilev)*dequant_coef[qp_rem_sp][0][0]<<qp_per_sp;
  }
  for (n2=0; n2 < 2; n2 ++)
  for (n1=0; n1 < 2; n1 ++)
  for (i=0;i< 4; i++)
  for (j=0;j< 4; j++)
  {
  // recovering coefficient since they are already dequantized earlier
    img->cof[n1+ll][4+n2][i][j] = (img->cof[n1+ll][4+n2][i][j] >> qp_per) / dequant_coef[qp_rem][i][j];
    ilev=((img->cof[n1+ll][4+n2][i][j]*dequant_coef[qp_rem][i][j]*A[i][j]<< qp_per) >>6)+predicted_chroma_block[n1*4 +i][n2*4 +j] ;
    img->cof[n1+ll][4+n2][i][j] = sign((abs(ilev) * quant_coef[qp_rem_sp][i][j] + qp_const2)>> q_bits_sp,ilev)*dequant_coef[qp_rem_sp][i][j]<<qp_per_sp;
  }
  img->cof[0+ll][4][0][0]=(mp1[0]+mp1[1]+mp1[2]+mp1[3])>>1;
  img->cof[1+ll][4][0][0]=(mp1[0]-mp1[1]+mp1[2]-mp1[3])>>1;
  img->cof[0+ll][5][0][0]=(mp1[0]+mp1[1]-mp1[2]-mp1[3])>>1;
  img->cof[1+ll][5][0][0]=(mp1[0]-mp1[1]-mp1[2]+mp1[3])>>1;
}

// Notation for comments regarding prediction and predictors.
// The pels of the 4x4 block are labelled a..p. The predictor pels above
// are labelled A..H, from the left I..L, and from above left X, as follows:
//
//  X A B C D E F G H
//  I a b c d
//  J e f g h
//  K i j k l
//  L m n o p
//
// Predictor array index definitions
/*!
 ***********************************************************************
 * \brief
 *    makes and returns 4x4 blocks with all 5 intra prediction modes
 *
 * \return
 *    DECODING_OK   decoding of intraprediction mode was sucessfull            \n
 *    SEARCH_SYNC   search next sync element as errors while decoding occured
 ***********************************************************************
 */
int intrapred(
  struct img_par *img, //!< image parameters
  int ioff, //!< pixel offset X within MB
  int joff, //!< pixel offset Y within MB
  int img_block_x, //!< location of block X, multiples of 4
  int img_block_y) //!< location of block Y, multiples of 4
{
  int i,j;
  int s0;
  int img_y,img_x;
  int PredPel[13]; // array of predictor pels
  byte **imgY = dec_picture->imgY;
  PixelPos pix_a[4];
  PixelPos pix_b, pix_c, pix_d;
  int block_available_up;
  int block_available_left;
  int block_available_up_left;
  int block_available_up_right;
  int mb_nr=img->current_mb_nr;
  byte predmode = img->ipredmode[img_block_x][img_block_y];
  img_x=img_block_x*4;
  img_y=img_block_y*4;
  for (i=0;i<4;i++)
  {
    getNeighbour(mb_nr, ioff -1 , joff +i , 1, &pix_a[i]);
  }
  getNeighbour(mb_nr, ioff , joff -1 , 1, &pix_b);
  getNeighbour(mb_nr, ioff +4 , joff -1 , 1, &pix_c);
  getNeighbour(mb_nr, ioff -1 , joff -1 , 1, &pix_d);
  pix_c.available = pix_c.available && !(((ioff==4)||(ioff==12)) && ((joff==4)||(joff==12)));
  if (img->constrained_intra_pred_flag)
  {
    for (i=0, block_available_left=1; i<4;i++)
      block_available_left &= pix_a[i].available ? img->intra_block[pix_a[i].mb_addr]: 0;
    block_available_up = pix_b.available ? img->intra_block [pix_b.mb_addr] : 0;
    block_available_up_right = pix_c.available ? img->intra_block [pix_c.mb_addr] : 0;
    block_available_up_left = pix_d.available ? img->intra_block [pix_d.mb_addr] : 0;
  }
  else
  {
    block_available_left = pix_a[0].available;
    block_available_up = pix_b.available;
    block_available_up_right = pix_c.available;
    block_available_up_left = pix_d.available;
  }
  // form predictor pels
  if (block_available_up)
  {
    (PredPel[1]) = imgY[pix_b.pos_y][pix_b.pos_x+0];
    (PredPel[2]) = imgY[pix_b.pos_y][pix_b.pos_x+1];
    (PredPel[3]) = imgY[pix_b.pos_y][pix_b.pos_x+2];
    (PredPel[4]) = imgY[pix_b.pos_y][pix_b.pos_x+3];
  }
  else
  {
    (PredPel[1]) = (PredPel[2]) = (PredPel[3]) = (PredPel[4]) = 128;
  }
  if (block_available_up_right)
  {
    (PredPel[5]) = imgY[pix_c.pos_y][pix_c.pos_x+0];
    (PredPel[6]) = imgY[pix_c.pos_y][pix_c.pos_x+1];
    (PredPel[7]) = imgY[pix_c.pos_y][pix_c.pos_x+2];
    (PredPel[8]) = imgY[pix_c.pos_y][pix_c.pos_x+3];
  }
  else
  {
    (PredPel[5]) = (PredPel[6]) = (PredPel[7]) = (PredPel[8]) = (PredPel[4]);
  }
  if (block_available_left)
  {
    (PredPel[9]) = imgY[pix_a[0].pos_y][pix_a[0].pos_x];
    (PredPel[10]) = imgY[pix_a[1].pos_y][pix_a[1].pos_x];
    (PredPel[11]) = imgY[pix_a[2].pos_y][pix_a[2].pos_x];
    (PredPel[12]) = imgY[pix_a[3].pos_y][pix_a[3].pos_x];
  }
  else
  {
    (PredPel[9]) = (PredPel[10]) = (PredPel[11]) = (PredPel[12]) = 128;
  }
  if (block_available_up_left)
  {
    (PredPel[0]) = imgY[pix_d.pos_y][pix_d.pos_x];
  }
  else
  {
    (PredPel[0]) = 128;
  }
  switch (predmode)
  {
  case 2: /* DC prediction */
    s0 = 0;
    if (block_available_up && block_available_left)
    {
      // no edge
      s0 = ((PredPel[1]) + (PredPel[2]) + (PredPel[3]) + (PredPel[4]) + (PredPel[9]) + (PredPel[10]) + (PredPel[11]) + (PredPel[12]) + 4)/(2*4);
    }
    else if (!block_available_up && block_available_left)
    {
      // upper edge
      s0 = ((PredPel[9]) + (PredPel[10]) + (PredPel[11]) + (PredPel[12]) + 2)/4;
    }
    else if (block_available_up && !block_available_left)
    {
      // left edge
      s0 = ((PredPel[1]) + (PredPel[2]) + (PredPel[3]) + (PredPel[4]) + 2)/4;
    }
    else //if (!block_available_up && !block_available_left)
    {
      // top left corner, nothing to predict from
      s0 = 128;
    }
    for (j=0; j < 4; j++)
    {
      for (i=0; i < 4; i++)
      {
        // store DC prediction
        img->mpr[i+ioff][j+joff] = s0;
      }
    }
    break;
  case 0: /* vertical prediction from block above */
    if (!block_available_up)
      printf ("warning: Intra_4x4_Vertical prediction mode not allowed at mb %d\n",img->current_mb_nr);
    for(j=0;j<4;j++)
      for(i=0;i<4;i++)
        img->mpr[i+ioff][j+joff]=imgY[pix_b.pos_y][pix_b.pos_x+i];/* store predicted 4x4 block */
    break;
  case 1: /* horizontal prediction from left block */
    if (!block_available_left)
      printf ("warning: Intra_4x4_Horizontal prediction mode not allowed at mb %d\n",img->current_mb_nr);
    for(j=0;j<4;j++)
      for(i=0;i<4;i++)
        img->mpr[i+ioff][j+joff]=imgY[pix_a[j].pos_y][pix_a[j].pos_x]; /* store predicted 4x4 block */
    break;
  case 4:
    if ((!block_available_up)||(!block_available_left)||(!block_available_up_left))
      printf ("warning: Intra_4x4_Diagonal_Down_Right prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][3+joff] = ((PredPel[12]) + 2*(PredPel[11]) + (PredPel[10]) + 2) / 4;
    img->mpr[0+ioff][2+joff] =
    img->mpr[1+ioff][3+joff] = ((PredPel[11]) + 2*(PredPel[10]) + (PredPel[9]) + 2) / 4;
    img->mpr[0+ioff][1+joff] =
    img->mpr[1+ioff][2+joff] =
    img->mpr[2+ioff][3+joff] = ((PredPel[10]) + 2*(PredPel[9]) + (PredPel[0]) + 2) / 4;
    img->mpr[0+ioff][0+joff] =
    img->mpr[1+ioff][1+joff] =
    img->mpr[2+ioff][2+joff] =
    img->mpr[3+ioff][3+joff] = ((PredPel[9]) + 2*(PredPel[0]) + (PredPel[1]) + 2) / 4;
    img->mpr[1+ioff][0+joff] =
    img->mpr[2+ioff][1+joff] =
    img->mpr[3+ioff][2+joff] = ((PredPel[0]) + 2*(PredPel[1]) + (PredPel[2]) + 2) / 4;
    img->mpr[2+ioff][0+joff] =
    img->mpr[3+ioff][1+joff] = ((PredPel[1]) + 2*(PredPel[2]) + (PredPel[3]) + 2) / 4;
    img->mpr[3+ioff][0+joff] = ((PredPel[2]) + 2*(PredPel[3]) + (PredPel[4]) + 2) / 4;
    break;
  case 3:
    if (!block_available_up)
      printf ("warning: Intra_4x4_Diagonal_Down_Left prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][0+joff] = ((PredPel[1]) + (PredPel[3]) + 2*((PredPel[2])) + 2) / 4;
    img->mpr[1+ioff][0+joff] =
    img->mpr[0+ioff][1+joff] = ((PredPel[2]) + (PredPel[4]) + 2*((PredPel[3])) + 2) / 4;
    img->mpr[2+ioff][0+joff] =
    img->mpr[1+ioff][1+joff] =
    img->mpr[0+ioff][2+joff] = ((PredPel[3]) + (PredPel[5]) + 2*((PredPel[4])) + 2) / 4;
    img->mpr[3+ioff][0+joff] =
    img->mpr[2+ioff][1+joff] =
    img->mpr[1+ioff][2+joff] =
    img->mpr[0+ioff][3+joff] = ((PredPel[4]) + (PredPel[6]) + 2*((PredPel[5])) + 2) / 4;
    img->mpr[3+ioff][1+joff] =
    img->mpr[2+ioff][2+joff] =
    img->mpr[1+ioff][3+joff] = ((PredPel[5]) + (PredPel[7]) + 2*((PredPel[6])) + 2) / 4;
    img->mpr[3+ioff][2+joff] =
    img->mpr[2+ioff][3+joff] = ((PredPel[6]) + (PredPel[8]) + 2*((PredPel[7])) + 2) / 4;
    img->mpr[3+ioff][3+joff] = ((PredPel[7]) + 3*((PredPel[8])) + 2) / 4;
    break;
  case 5:/* diagonal prediction -22.5 deg to horizontal plane */
    if ((!block_available_up)||(!block_available_left)||(!block_available_up_left))
      printf ("warning: Intra_4x4_Vertical_Right prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][0+joff] =
    img->mpr[1+ioff][2+joff] = ((PredPel[0]) + (PredPel[1]) + 1) / 2;
    img->mpr[1+ioff][0+joff] =
    img->mpr[2+ioff][2+joff] = ((PredPel[1]) + (PredPel[2]) + 1) / 2;
    img->mpr[2+ioff][0+joff] =
    img->mpr[3+ioff][2+joff] = ((PredPel[2]) + (PredPel[3]) + 1) / 2;
    img->mpr[3+ioff][0+joff] = ((PredPel[3]) + (PredPel[4]) + 1) / 2;
    img->mpr[0+ioff][1+joff] =
    img->mpr[1+ioff][3+joff] = ((PredPel[9]) + 2*(PredPel[0]) + (PredPel[1]) + 2) / 4;
    img->mpr[1+ioff][1+joff] =
    img->mpr[2+ioff][3+joff] = ((PredPel[0]) + 2*(PredPel[1]) + (PredPel[2]) + 2) / 4;
    img->mpr[2+ioff][1+joff] =
    img->mpr[3+ioff][3+joff] = ((PredPel[1]) + 2*(PredPel[2]) + (PredPel[3]) + 2) / 4;
    img->mpr[3+ioff][1+joff] = ((PredPel[2]) + 2*(PredPel[3]) + (PredPel[4]) + 2) / 4;
    img->mpr[0+ioff][2+joff] = ((PredPel[0]) + 2*(PredPel[9]) + (PredPel[10]) + 2) / 4;
    img->mpr[0+ioff][3+joff] = ((PredPel[9]) + 2*(PredPel[10]) + (PredPel[11]) + 2) / 4;
    break;
  case 7:/* diagonal prediction -22.5 deg to horizontal plane */
    if (!block_available_up)
      printf ("warning: Intra_4x4_Vertical_Left prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][0+joff] = ((PredPel[1]) + (PredPel[2]) + 1) / 2;
    img->mpr[1+ioff][0+joff] =
    img->mpr[0+ioff][2+joff] = ((PredPel[2]) + (PredPel[3]) + 1) / 2;
    img->mpr[2+ioff][0+joff] =
    img->mpr[1+ioff][2+joff] = ((PredPel[3]) + (PredPel[4]) + 1) / 2;
    img->mpr[3+ioff][0+joff] =
    img->mpr[2+ioff][2+joff] = ((PredPel[4]) + (PredPel[5]) + 1) / 2;
    img->mpr[3+ioff][2+joff] = ((PredPel[5]) + (PredPel[6]) + 1) / 2;
    img->mpr[0+ioff][1+joff] = ((PredPel[1]) + 2*(PredPel[2]) + (PredPel[3]) + 2) / 4;
    img->mpr[1+ioff][1+joff] =
    img->mpr[0+ioff][3+joff] = ((PredPel[2]) + 2*(PredPel[3]) + (PredPel[4]) + 2) / 4;
    img->mpr[2+ioff][1+joff] =
    img->mpr[1+ioff][3+joff] = ((PredPel[3]) + 2*(PredPel[4]) + (PredPel[5]) + 2) / 4;
    img->mpr[3+ioff][1+joff] =
    img->mpr[2+ioff][3+joff] = ((PredPel[4]) + 2*(PredPel[5]) + (PredPel[6]) + 2) / 4;
    img->mpr[3+ioff][3+joff] = ((PredPel[5]) + 2*(PredPel[6]) + (PredPel[7]) + 2) / 4;
    break;
  case 8:/* diagonal prediction -22.5 deg to horizontal plane */
    if (!block_available_left)
      printf ("warning: Intra_4x4_Horizontal_Up prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][0+joff] = ((PredPel[9]) + (PredPel[10]) + 1) / 2;
    img->mpr[1+ioff][0+joff] = ((PredPel[9]) + 2*(PredPel[10]) + (PredPel[11]) + 2) / 4;
    img->mpr[2+ioff][0+joff] =
    img->mpr[0+ioff][1+joff] = ((PredPel[10]) + (PredPel[11]) + 1) / 2;
    img->mpr[3+ioff][0+joff] =
    img->mpr[1+ioff][1+joff] = ((PredPel[10]) + 2*(PredPel[11]) + (PredPel[12]) + 2) / 4;
    img->mpr[2+ioff][1+joff] =
    img->mpr[0+ioff][2+joff] = ((PredPel[11]) + (PredPel[12]) + 1) / 2;
    img->mpr[3+ioff][1+joff] =
    img->mpr[1+ioff][2+joff] = ((PredPel[11]) + 2*(PredPel[12]) + (PredPel[12]) + 2) / 4;
    img->mpr[3+ioff][2+joff] =
    img->mpr[1+ioff][3+joff] =
    img->mpr[0+ioff][3+joff] =
    img->mpr[2+ioff][2+joff] =
    img->mpr[2+ioff][3+joff] =
    img->mpr[3+ioff][3+joff] = (PredPel[12]);
    break;
  case 6:/* diagonal prediction -22.5 deg to horizontal plane */
    if ((!block_available_up)||(!block_available_left)||(!block_available_up_left))
      printf ("warning: Intra_4x4_Horizontal_Down prediction mode not allowed at mb %d\n",img->current_mb_nr);
    img->mpr[0+ioff][0+joff] =
    img->mpr[2+ioff][1+joff] = ((PredPel[0]) + (PredPel[9]) + 1) / 2;
    img->mpr[1+ioff][0+joff] =
    img->mpr[3+ioff][1+joff] = ((PredPel[9]) + 2*(PredPel[0]) + (PredPel[1]) + 2) / 4;
    img->mpr[2+ioff][0+joff] = ((PredPel[0]) + 2*(PredPel[1]) + (PredPel[2]) + 2) / 4;
    img->mpr[3+ioff][0+joff] = ((PredPel[1]) + 2*(PredPel[2]) + (PredPel[3]) + 2) / 4;
    img->mpr[0+ioff][1+joff] =
    img->mpr[2+ioff][2+joff] = ((PredPel[9]) + (PredPel[10]) + 1) / 2;
    img->mpr[1+ioff][1+joff] =
    img->mpr[3+ioff][2+joff] = ((PredPel[0]) + 2*(PredPel[9]) + (PredPel[10]) + 2) / 4;
    img->mpr[0+ioff][2+joff] =
    img->mpr[2+ioff][3+joff] = ((PredPel[10]) + (PredPel[11]) + 1) / 2;
    img->mpr[1+ioff][2+joff] =
    img->mpr[3+ioff][3+joff] = ((PredPel[9]) + 2*(PredPel[10]) + (PredPel[11]) + 2) / 4;
    img->mpr[0+ioff][3+joff] = ((PredPel[11]) + (PredPel[12]) + 1) / 2;
    img->mpr[1+ioff][3+joff] = ((PredPel[10]) + 2*(PredPel[11]) + (PredPel[12]) + 2) / 4;
    break;
  default:
    printf("Error: illegal intra_4x4 prediction mode: %d\n",predmode);
    return 1;
    break;
  }
  return 0;
}
/*!
 ***********************************************************************
 * \return
 *    best SAD
 ***********************************************************************
 */
int intrapred_luma_16x16(struct img_par *img, //!< image parameters
                         int predmode) //!< prediction mode
{
  int s0=0,s1,s2;
  int i,j;
  int ih,iv;
  int ib,ic,iaa;
  byte **imgY=dec_picture->imgY;
  int mb_nr=img->current_mb_nr;
  PixelPos up; //!< pixel position p(0,-1)
  PixelPos left[17]; //!< pixel positions p(-1, -1..15)
  int up_avail, left_avail, left_up_avail;
  s1=s2=0;
  for (i=0;i<17;i++)
  {
    getNeighbour(mb_nr, -1 , i-1 , 1, &left[i]);
  }
  getNeighbour(mb_nr, 0 , -1 , 1, &up);
  if (!img->constrained_intra_pred_flag)
  {
    up_avail = up.available;
    left_avail = left[1].available;
    left_up_avail = left[0].available;
  }
  else
  {
    up_avail = up.available ? img->intra_block[up.mb_addr] : 0;
    for (i=1, left_avail=1; i<17;i++)
      left_avail &= left[i].available ? img->intra_block[left[i].mb_addr]: 0;
    left_up_avail = left[0].available ? img->intra_block[left[0].mb_addr]: 0;
  }
  switch (predmode)
  {
  case 0: // vertical prediction from block above
    if (!up_avail)
      error ("invalid 16x16 intra pred Mode VERT_PRED_16",500);
    for(j=0;j<16;j++)
      for(i=0;i<16;i++)
        img->mpr[i][j]=imgY[up.pos_y][up.pos_x+i];// store predicted 16x16 block
    break;
  case 1: // horisontal prediction from left block
    if (!left_avail)
      error ("invalid 16x16 intra pred Mode VERT_PRED_16",500);
    for(j=0;j<16;j++)
      for(i=0;i<16;i++)
        img->mpr[i][j]=imgY[left[j+1].pos_y][left[j+1].pos_x]; // store predicted 16x16 block
    break;
  case 2: // DC prediction
    s1=s2=0;
    for (i=0; i < 16; i++)
    {
      if (up_avail)
        s1 += imgY[up.pos_y][up.pos_x+i]; // sum hor pix
      if (left_avail)
        s2 += imgY[left[i+1].pos_y][left[i+1].pos_x]; // sum vert pix
    }
    if (up_avail && left_avail)
      s0=(s1+s2+16)>>5; // no edge
    if (!up_avail && left_avail)
      s0=(s2+8)>>4; // upper edge
    if (up_avail && !left_avail)
      s0=(s1+8)>>4; // left edge
    if (!up_avail && !left_avail)
      s0=128; // top left corner, nothing to predict from
    for(i=0;i<16;i++)
      for(j=0;j<16;j++)
      {
        img->mpr[i][j]=s0;
      }
    break;
  case 3:// 16 bit integer plan pred
    if (!up_avail || !left_up_avail || !left_avail)
      error ("invalid 16x16 intra pred Mode PLANE_16",500);
    ih=0;
    iv=0;
    for (i=1;i<9;i++)
    {
      if (i<8)
        ih += i*(imgY[up.pos_y][up.pos_x+7+i] - imgY[up.pos_y][up.pos_x+7-i]);
      else
        ih += i*(imgY[up.pos_y][up.pos_x+7+i] - imgY[left[0].pos_y][left[0].pos_x]);
      iv += i*(imgY[left[8+i].pos_y][left[8+i].pos_x] - imgY[left[8-i].pos_y][left[8-i].pos_x]);
    }
    ib=(5*ih+32)>>6;
    ic=(5*iv+32)>>6;
    iaa=16*(imgY[up.pos_y][up.pos_x+15]+imgY[left[16].pos_y][left[16].pos_x]);
    for (j=0;j< 16;j++)
    {
      for (i=0;i< 16;i++)
      {
        img->mpr[i][j]=((0) > ((((iaa+(i-7)*ib +(j-7)*ic + 16)>>5) < (255) ? ((iaa+(i-7)*ib +(j-7)*ic + 16)>>5) : (255))) ? (0) : ((((iaa+(i-7)*ib +(j-7)*ic + 16)>>5) < (255) ? ((iaa+(i-7)*ib +(j-7)*ic + 16)>>5) : (255))));
      }
    }// store plane prediction
    break;
  default:
    { // indication of fault in bitstream,exit
      printf("illegal 16x16 intra prediction mode input: %d\n",predmode);
      return 1;
    }
  }
  return 0;
}
void intrapred_chroma(struct img_par *img, int uv)
{
  int i,j, ii, jj, ioff, joff;
  byte ***imgUV = dec_picture->imgUV;
  int js0=0;
  int js1=0;
  int js2=0;
  int js3=0;
  int js[2][2];
  int pred;
  int ih, iv, ib, ic, iaa;
  int mb_nr=img->current_mb_nr;
  Macroblock *currMB = &img->mb_data[img->current_mb_nr];
  PixelPos up; //!< pixel position p(0,-1)
  PixelPos left[9]; //!< pixel positions p(-1, -1..8)
  int up_avail, left_avail, left_up_avail;
  for (i=0;i<9;i++)
  {
    getNeighbour(mb_nr, -1 , i-1 , 0, &left[i]);
  }
  getNeighbour(mb_nr, 0 , -1 , 0, &up);
  if (!img->constrained_intra_pred_flag)
  {
    up_avail = up.available;
    left_avail = left[1].available;
    left_up_avail = left[0].available;
  }
  else
  {
    up_avail = up.available ? img->intra_block[up.mb_addr] : 0;
    for (i=1, left_avail=1; i<9;i++)
      left_avail &= left[i].available ? img->intra_block[left[i].mb_addr]: 0;
    left_up_avail = left[0].available ? img->intra_block[left[0].mb_addr]: 0;
  }
  if (currMB->c_ipred_mode == 0)
  {
    for(i=0;i<4;i++)
    {
      if(up_avail)
      {
        js0=js0+imgUV[uv][up.pos_y][up.pos_x+i];
        js1=js1+imgUV[uv][up.pos_y][up.pos_x+i+4];
      }
      if(left_avail)
      {
        js2=js2+imgUV[uv][left[1+i].pos_y][left[1+i].pos_x];
        js3=js3+imgUV[uv][left[1+i+4].pos_y][left[1+i+4].pos_x];
      }
    }
    if(up_avail && left_avail)
    {
      js[0][0]=(js0+js2+4)/8;
      js[1][0]=(js1+2)/4;
      js[0][1]=(js3+2)/4;
      js[1][1]=(js1+js3+4)/8;
    }
    if(up_avail && !left_avail)
    {
      js[0][0]=(js0+2)/4;
      js[1][0]=(js1+2)/4;
      js[0][1]=(js0+2)/4;
      js[1][1]=(js1+2)/4;
    }
    if(left_avail && !up_avail)
    {
      js[0][0]=(js2+2)/4;
      js[1][0]=(js2+2)/4;
      js[0][1]=(js3+2)/4;
      js[1][1]=(js3+2)/4;
    }
    if(!up_avail && !left_avail)
    {
      js[0][0]=128;
      js[1][0]=128;
      js[0][1]=128;
      js[1][1]=128;
    }
  }
  for (j=0;j<2;j++)
  {
    joff=j*4;
    for(i=0;i<2;i++)
    {
      ioff=i*4;
      switch (currMB->c_ipred_mode)
      {
      case 0:
        for (ii=0; ii<4; ii++)
          for (jj=0; jj<4; jj++)
          {
            img->mpr[ii+ioff][jj+joff]=js[i][j];
          }
        break;
      case 1:
        if (!left_avail)
          error("unexpected HOR_PRED_8 chroma intra prediction mode",-1);
        for (jj=0; jj<4; jj++)
        {
          pred = imgUV[uv][left[1+jj+joff].pos_y][left[1+jj+joff].pos_x];
          for (ii=0; ii<4; ii++)
            img->mpr[ii+ioff][jj+joff]=pred;
        }
        break;
      case 2:
        if (!up_avail)
          error("unexpected VERT_PRED_8 chroma intra prediction mode",-1);
        for (ii=0; ii<4; ii++)
        {
          pred = imgUV[uv][up.pos_y][up.pos_x+ii+ioff];
          for (jj=0; jj<4; jj++)
            img->mpr[ii+ioff][jj+joff]=pred;
        }
        break;
      case 3:
        if (!left_up_avail || !left_avail || !up_avail)
          error("unexpected PLANE_8 chroma intra prediction mode",-1);
        ih=iv=0;
        for (ii=1;ii<5;ii++)
        {
          if (ii<4)
            ih += ii*(imgUV[uv][up.pos_y][up.pos_x+3+ii] - imgUV[uv][up.pos_y][up.pos_x+3-ii]);
          else
            ih += ii*(imgUV[uv][up.pos_y][up.pos_x+3+ii] - imgUV[uv][left[0].pos_y][left[0].pos_x]);
          iv += ii*(imgUV[uv][left[4+ii].pos_y][left[4+ii].pos_x] - imgUV[uv][left[4-ii].pos_y][left[4-ii].pos_x]);
        }
        ib=(17*ih+16)>>5;
        ic=(17*iv+16)>>5;
        iaa=16*(imgUV[uv][up.pos_y][up.pos_x+7]+imgUV[uv][left[8].pos_y][left[8].pos_x]);
        for (ii=0; ii<4; ii++)
          for (jj=0; jj<4; jj++)
            img->mpr[ii+ioff][jj+joff]=((0) > (((255) < ((iaa+(ii+ioff-3)*ib +(jj+joff-3)*ic + 16)>>5) ? (255) : ((iaa+(ii+ioff-3)*ib +(jj+joff-3)*ic + 16)>>5))) ? (0) : (((255) < ((iaa+(ii+ioff-3)*ib +(jj+joff-3)*ic + 16)>>5) ? (255) : ((iaa+(ii+ioff-3)*ib +(jj+joff-3)*ic + 16)>>5))));
          break;
      default:
        error("illegal chroma intra prediction mode", 600);
        break;
      }
    }
  }
}
/*!
 ***********************************************************************
 * \brief
 *    Inverse 4x4 transformation, transforms cof to m7
 ***********************************************************************
 */
void itrans(struct img_par *img, //!< image parameters
            int ioff, //!< index to 4x4 block
            int joff, //!<
            int i0, //!<
            int j0) //!<
{
  int i,j,i1,j1;
  int m5[4];
  int m6[4];
  // horizontal
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
    {
      m5[i]=img->cof[i0][j0][i][j];
    }
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (i=0;i<2;i++)
    {
      i1=3-i;
      img->m7[i][j]=m6[i]+m6[i1];
      img->m7[i1][j]=m6[i]-m6[i1];
    }
  }
  // vertical
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      m5[j]=img->m7[i][j];
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (j=0;j<2;j++)
    {
      j1=3-j;
      img->m7[i][j] =((0) > (((255) < ((m6[j]+m6[j1]+(img->mpr[i+ioff][j+joff] <<6)+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(img->mpr[i+ioff][j+joff] <<6)+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]+m6[j1]+(img->mpr[i+ioff][j+joff] <<6)+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(img->mpr[i+ioff][j+joff] <<6)+(1<<(6 -1)))>>6))));
      img->m7[i][j1]=((0) > (((255) < ((m6[j]-m6[j1]+(img->mpr[i+ioff][j1+joff]<<6)+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(img->mpr[i+ioff][j1+joff]<<6)+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]-m6[j1]+(img->mpr[i+ioff][j1+joff]<<6)+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(img->mpr[i+ioff][j1+joff]<<6)+(1<<(6 -1)))>>6))));
    }
  }
}
/*!
 ***********************************************************************
 * \brief
 *    invers  transform
 ***********************************************************************
 */
void itrans_2(
   struct img_par *img) //!< image parameters
{
  int i,j,i1,j1;
  int M5[4];
  int M6[4];
  int qp_per = (img->qp-0)/6;
  int qp_rem = (img->qp-0)%6;
  // horizontal
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
      M5[i]=img->cof[i][j][0][0];
    M6[0]=M5[0]+M5[2];
    M6[1]=M5[0]-M5[2];
    M6[2]=M5[1]-M5[3];
    M6[3]=M5[1]+M5[3];
    for (i=0;i<2;i++)
    {
      i1=3-i;
      img->cof[i ][j][0][0]= M6[i]+M6[i1];
      img->cof[i1][j][0][0]=M6[i]-M6[i1];
    }
  }
  // vertical
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      M5[j]=img->cof[i][j][0][0];
    M6[0]=M5[0]+M5[2];
    M6[1]=M5[0]-M5[2];
    M6[2]=M5[1]-M5[3];
    M6[3]=M5[1]+M5[3];
    for (j=0;j<2;j++)
    {
      j1=3-j;
      img->cof[i][j][0][0] = (((M6[j]+M6[j1])*dequant_coef[qp_rem][0][0]<<qp_per)+2)>>2;
      img->cof[i][j1][0][0]= (((M6[j]-M6[j1])*dequant_coef[qp_rem][0][0]<<qp_per)+2)>>2;
    }
  }
}
void itrans_sp(struct img_par *img, //!< image parameters
               int ioff, //!< index to 4x4 block
               int joff, //!<
               int i0, //!<
               int j0) //!<
{
  int i,j,i1,j1;
  int m5[4];
  int m6[4];
  int predicted_block[4][4],ilev;
  int qp_per = (img->qp-0)/6;
  int qp_rem = (img->qp-0)%6;
  int q_bits = 15 +qp_per;
  int qp_per_sp = (img->qpsp-0)/6;
  int qp_rem_sp = (img->qpsp-0)%6;
  int q_bits_sp = 15 +qp_per_sp;
  int qp_const2=(1<<q_bits_sp)/2; //sp_pred
  if (img->sp_switch || img->type == SI_SLICE)
  {
    qp_per = (img->qpsp-0)/6;
    qp_rem = (img->qpsp-0)%6;
    q_bits = 15 +qp_per;
  }
  for (j=0; j< 4; j++)
  for (i=0; i< 4; i++)
      predicted_block[i][j]=img->mpr[i+ioff][j+joff];
  for (j=0; j < 4; j++)
  {
    for (i=0; i < 2; i++)
    {
      i1=3-i;
      m5[i]=predicted_block[i][j]+predicted_block[i1][j];
      m5[i1]=predicted_block[i][j]-predicted_block[i1][j];
    }
    predicted_block[0][j]=(m5[0]+m5[1]);
    predicted_block[2][j]=(m5[0]-m5[1]);
    predicted_block[1][j]=m5[3]*2+m5[2];
    predicted_block[3][j]=m5[3]-m5[2]*2;
  }
  //  Vertival transform
  for (i=0; i < 4; i++)
  {
    for (j=0; j < 2; j++)
    {
      j1=3-j;
      m5[j]=predicted_block[i][j]+predicted_block[i][j1];
      m5[j1]=predicted_block[i][j]-predicted_block[i][j1];
    }
    predicted_block[i][0]=(m5[0]+m5[1]);
    predicted_block[i][2]=(m5[0]-m5[1]);
    predicted_block[i][1]=m5[3]*2+m5[2];
    predicted_block[i][3]=m5[3]-m5[2]*2;
  }
  for (j=0;j<4;j++)
  for (i=0;i<4;i++)
  {
    // recovering coefficient since they are already dequantized earlier
    img->cof[i0][j0][i][j]=(img->cof[i0][j0][i][j] >> qp_per) / dequant_coef[qp_rem][i][j];
    ilev=((img->cof[i0][j0][i][j]*dequant_coef[qp_rem][i][j]*A[i][j]<< qp_per) >>6)+predicted_block[i][j] ;
    img->cof[i0][j0][i][j]=sign((abs(ilev) * quant_coef[qp_rem_sp][i][j] + qp_const2) >> q_bits_sp, ilev) * dequant_coef[qp_rem_sp][i][j] << qp_per_sp;
  }
  // horizontal
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
    {
      m5[i]=img->cof[i0][j0][i][j];
    }
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (i=0;i<2;i++)
    {
      i1=3-i;
      img->m7[i][j]=m6[i]+m6[i1];
      img->m7[i1][j]=m6[i]-m6[i1];
    }
  }
  // vertical
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      m5[j]=img->m7[i][j];
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (j=0;j<2;j++)
    {
      j1=3-j;
      img->m7[i][j] =((0) > (((255) < ((m6[j]+m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]+m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(1<<(6 -1)))>>6))));
      img->m7[i][j1]=((0) > (((255) < ((m6[j]-m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]-m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(1<<(6 -1)))>>6))));
    }
  }
}
/*!
 ***********************************************************************
 * \brief
 *    The routine performs transform,quantization,inverse transform, adds the diff.
 *    to the prediction and writes the result to the decoded luma frame. Includes the
 *    RD constrained quantization also.
 *
 * \par Input:
 *    block_x,block_y: Block position inside a macro block (0,4,8,12).
 *
 * \par Output:
 *    nonzero: 0 if no levels are nonzero.  1 if there are nonzero levels. \n
 *    coeff_cost: Counter for nonzero coefficients, used to discard expencive levels.
 ************************************************************************
 */
void copyblock_sp(struct img_par *img,int block_x,int block_y)
{
  int sign(int a,int b);
  int i,j,i1,j1,m5[4],m6[4];
  int predicted_block[4][4];
  int qp_per = (img->qpsp-0)/6;
  int qp_rem = (img->qpsp-0)%6;
  int q_bits = 15 +qp_per;
  int qp_const2=(1<<q_bits)/2; //sp_pred
  //  Horizontal transform
  for (j=0; j< 4; j++)
  for (i=0; i< 4; i++)
    predicted_block[i][j]=img->mpr[i+block_x][j+block_y];
  for (j=0; j < 4; j++)
  {
    for (i=0; i < 2; i++)
    {
      i1=3-i;
      m5[i]=predicted_block[i][j]+predicted_block[i1][j];
      m5[i1]=predicted_block[i][j]-predicted_block[i1][j];
    }
    predicted_block[0][j]=(m5[0]+m5[1]);
    predicted_block[2][j]=(m5[0]-m5[1]);
    predicted_block[1][j]=m5[3]*2+m5[2];
    predicted_block[3][j]=m5[3]-m5[2]*2;
  }
  //  Vertival transform
  for (i=0; i < 4; i++)
  {
    for (j=0; j < 2; j++)
    {
      j1=3-j;
      m5[j]=predicted_block[i][j]+predicted_block[i][j1];
      m5[j1]=predicted_block[i][j]-predicted_block[i][j1];
    }
    predicted_block[i][0]=(m5[0]+m5[1]);
    predicted_block[i][2]=(m5[0]-m5[1]);
    predicted_block[i][1]=m5[3]*2+m5[2];
    predicted_block[i][3]=m5[3]-m5[2]*2;
  }
  // Quant
  for (j=0;j < 4; j++)
  for (i=0; i < 4; i++)
    img->m7[i][j]=sign((abs(predicted_block[i][j])* quant_coef[qp_rem][i][j]+qp_const2)>> q_bits,predicted_block[i][j])*dequant_coef[qp_rem][i][j]<<qp_per;
  //     IDCT.
  //     horizontal
  for (j=0;j<4;j++)
  {
    for (i=0;i<4;i++)
    {
      m5[i]=img->m7[i][j];
    }
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (i=0;i<2;i++)
    {
      i1=3-i;
      img->m7[i][j]=m6[i]+m6[i1];
      img->m7[i1][j]=m6[i]-m6[i1];
    }
  }
  // vertical
  for (i=0;i<4;i++)
  {
    for (j=0;j<4;j++)
      m5[j]=img->m7[i][j];
    m6[0]=(m5[0]+m5[2]);
    m6[1]=(m5[0]-m5[2]);
    m6[2]=(m5[1]>>1)-m5[3];
    m6[3]=m5[1]+(m5[3]>>1);
    for (j=0;j<2;j++)
    {
      j1=3-j;
      img->m7[i][j] =((0) > (((255) < ((m6[j]+m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]+m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]+m6[j1]+(1<<(6 -1)))>>6))));
      img->m7[i][j1]=((0) > (((255) < ((m6[j]-m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(1<<(6 -1)))>>6))) ? (0) : (((255) < ((m6[j]-m6[j1]+(1<<(6 -1)))>>6) ? (255) : ((m6[j]-m6[j1]+(1<<(6 -1)))>>6))));
    }
  }
  //  Decoded block moved to frame memory
  for (j=0; j < 4; j++)
    for (i=0; i < 4; i++)
      dec_picture->imgY[img->pix_y+block_y+j][img->pix_x+block_x+i]=img->m7[i][j];
}

int sign(int a , int b)
{
  int x;
  x=abs(a);
  if (b>0)
    return(x);
  else return(-x);
}
