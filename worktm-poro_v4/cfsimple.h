#ifndef __F_NAMES_AND_TYPES__
#define __F_NAMES_AND_TYPES__

/*--------------------------------------------------------------------*/
/* MUST HAVE  ANSI-C ! */

#define _PASTE__(A,B)  A##B
#define _PASTE_(A,B)   _PASTE__(A,B)

/*--------------------------------------------------------------------*/
/* Name options */

#ifndef _F_NAME_UPPER_CASE_
#define _F_NAME_UPPER_CASE_                        1
#endif

#ifndef _F_NAME_LOWER_CASE_
#define _F_NAME_LOWER_CASE_                        2
#endif

#ifndef _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_    3
#endif

/*--------------------------------------------------------------------*/
/* Character options */

#ifndef _F_STRING_HIDE_TRAILING_INT_
#define _F_STRING_HIDE_TRAILING_INT_   1
#endif

#ifndef _F_STRING_HIDE_ADJACENT_INT_
#define _F_STRING_HIDE_ADJACENT_INT_   2
#endif

#ifndef _F_STRING_VMS_STRUCTURE_
#define _F_STRING_VMS_STRUCTURE_       3
#endif

/*--------------------------------------------------------------------*/
/* Basic types, redefined as necessary */

#define _F_CHARACTER         char
#define _F_INTEGER_1         char
#define _F_INTEGER_2         short
#define _F_INTEGER_4         int
#define _F_INTEGER           int
#define _F_REAL              float
#define _F_REAL_4            float
#define _F_REAL_8            double
#define _F_DOUBLE_PRECISION  double
#define _F_COMPLEX           struct { float  real ; float  imag ; }
#define _F_DOUBLE_COMPLEX    struct { double real ; double imag ; }

/*--------------------------------------------------------------------*/
/* Declare an external subroutine */

#if defined(__cplusplus)
#define _F_EXTERN_(TYPE) extern "C" TYPE
#else
#define _F_EXTERN_(TYPE) extern TYPE
#endif

/*--------------------------------------------------------------------*/
/* Environment parameters */

#if defined(sun) || defined(__sun)
/* SUN */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif defined(__alpha) && defined(__unix__)
/* DEC ALPHA */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif ( defined(vax) && defined(unix) ) || \
      ( defined(__vax__) && defined(__unix__) )
/* VAX ULTRIX */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif defined(mips) || defined(__mips)
/* MIPS, includes SGI and DECstation */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif defined(__convex__)
/* CONVEX */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif defined(apollo)
/* Apollo */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_UNKNOWN_

#elif defined(_CRAYT3E)
/* CRAY T3E */

#define _F_NAME_OPTION_    _F_NAME_UPPER_CASE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_ADJACENT_INT_

#undef  _F_INTEGER_1
#undef  _F_INTEGER_2
#undef  _F_INTEGER_4
#undef  _F_REAL
#undef  _F_COMPLEX
#undef  _F_DOUBLE_PRECISION
#undef  _F_DOUBLE_COMPLEX
#define _F_INTEGER_4         short
#define _F_REAL              double
#define _F_COMPLEX           struct { double real ; double imag ; }

#elif defined(_WIN32)

#define _F_NAME_OPTION_    _F_NAME_UPPER_CASE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_ADJACENT_INT_

#undef  _F_EXTERN_
#if defined(__cplusplus)
#define _F_EXTERN_(TYPE) extern "C" TYPE __stdcall
#else
#define _F_EXTERN_(TYPE) extern TYPE __stdcall
#endif

#elif defined(_IBMR2)
/* IBM RS6000 */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#elif defined(__hpux)
/* HPUX */

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

// bag8 : for mac osx
#elif defined(_limux) || defined(__linux) || defined(__APPLE__)

#define _F_NAME_OPTION_    _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_
#define _F_STRING_OPTION_  _F_STRING_HIDE_TRAILING_INT_

#else

#define _F_NAME_OPTION_    _F_NAME_UNKNOWN_
#define _F_STRING_OPTION_  _F_STRING_UNKNOWN_

#endif

/*--------------------------------------------------------------------*/
/* Picked up the options? */

#if _F_NAME_OPTION_   == _F_NAME_UNKNOWN_   || \
    _F_STRING_OPTION_ == _F_STRING_UNKNOWN_
    
#error "The characteristics of this host are currently unknown"

#endif

/*--------------------------------------------------------------------*/
/* NAME USAGE: define 'C' names to be called from FORTRAN as:
 *
 *      #define _name  _F_NAME_(NAME,name)
 *
 * and then used '_name' throughout the 'C' code.
 */

#if _F_NAME_OPTION_ == _F_NAME_LOWER_CASE_TRAILING_UNDERSCORE_

#define _F_NAME_(UN,LN)	       _PASTE_(LN,_)

#elif _F_NAME_OPTION_ == _F_NAME_UPPER_CASE_

#define _F_NAME_(UN,LN)        UN

#elif _F_NAME_OPTION_ == _F_NAME_LOWER_CASE_

#define _F_NAME_(UN,LN)        LN

#else

#error "Unknown 'name' option"

#endif

/*--------------------------------------------------------------------*/

#endif



