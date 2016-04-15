/****************************************************************************
* 
* env.h -- environment flags environment
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* 
****************************************************************************/

// test comment for cvs change

#ifndef ENV_H
#define ENV_H

/* Development environment, ENVxyz, set on command line in compiler.
   The three characters indicate compiler, OS/machine, memory model

   compiler      OS/machine       memory model

   b Borland     a Alpha/OSF-1    1 16-bit real-mode medium model
   d DEC         d DOS            2 286 protected mode
   m Microsoft   o OS/2           3 32-bit addressing
   w Watcom      u Unix           6 64-bit addressing
   z Zortech     w Windows
   g Gnu         v OpenVMS
   h HP          l Linux
   s Sun         h HP-UX
                 s Solaris
                 i Intel Solaris
                 b BSD

   Supported environments at release 6 level
      mw3    Microsoft Visual C++, Windows, 32-bit
	  mw6    Windows 64-bit
      gl3    Gnu C++, Linux, 32-bit
      gh3    Gnu C++, HP-UX 32-bit
      gs3    Gnu C++, Solaris, 32-bit
      gi3    Gnu C++, Intel Solaris, 32-bit

   Previous supported environments
   ENV
      bd1    Borland DOS real mode
      bw2    Borland Windows 286 protected mode
      bw3    Borland Windows 386 protected mode
      da6    DEC Alpha, 64-bit OSF-1
      dv3    DEC Alpha, 32-bit OpenVMS
      md1    Microsoft C, 16-bit real mode DOS
      mw2    Microsoft C, 286 protected mode Windows
      mw2d   Microsoft C, 286 protected mode Windows DLL
      mw3d   Microsoft C, 386 protected mode Windows DLL
      mw3    Microsoft C, 386 protected mode Windows
      sd1    Symantec 16-bit DOS
      sd3    Symantec 32-bit DOS
      wd1    Watcom DOS real mode
      wd3    Watcom DOS 386 protected mode
      ww3d   Watcom 386 protected mode Windows DLL
      zd3    Zortech DOS 386 protected mode
      sol25  Solaris 2.5
      ss3    Sun Workshop C++, Solaris, 32-bit

   Operating systems:
      DOS
      WIN3     (Windows 3.x)
      WIN4     (NT, 95, 98, ...)
      OSF1
      OPENVMS
      SOLARIS
      HPUX

   Machines:
      IBMPC
      ALPHA
      HP
      SUN

   C Compilers:
      MSC      Microsoft C
      ZTC      Zortech C
      DECC     DEC C
      WC       Watcom
      BC       Borland
      SC       Symantec
      SUNC     Sun C
      GNU      Gnu C++

   Pointer sizes:
      P16      PC segmented 16-bit architecture
      P32      Flat 32-bit memory
      P64      Flat 64-bit memory

   The constant OPSYS is used by the opsys/1 built-in predicate so Prolog can
   figure out which system its running under.
*/

/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Mac with Gnu C++    */
/*______________________*/

#ifdef ENVgm6

#define ENVIR aS("OSX")

#define   IBMPC                            // machine 
#define   OSX                            // operating system
#define   LINUX   // steal the linux ifdefs as well, mostly the same
#define   GNU                              // C compiler 
#define   P64                              // pointer size 
#define   OPSYS aS("osx")
#define   UNIX                             // os class
#define   REALS                          // Real type turned on
#endif

/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Mac OS Gnu C        */
/*______________________*/
/*
#ifdef MACOSX
#define ENVIR aS("MAC")
#define   MAC                              // machine
#define   MACOS                            // operating system
#define   GNU                              // C compiler
#define   P32                              // pointer size
#define   OPSYS aS("mac_os")
#define   REALS                            // real type turned on
#define   BIG__ENDIAN                      // endian
#define  UNIX  // MLW: added 11/8/2003
#endif
*/

/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  HP-UX with Gnu C    */
/*______________________*/

#ifdef ENVgh3

#define ENVIR aS("HP-UX")

#define   HP                               // machine
#define   HPUX                             // operating system
#define   GNU                              // C compiler
#define   P32                              // pointer size
#define   OPSYS aS("hp_ux")
#define   UNIX                             // os class
#define   REALS                            // real type turned on
#define   BIG__ENDIAN                       // endian

#endif

/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Linux 64 C++        */
/*______________________*/
// Hopefully this will work for all Linuxs

#ifdef ENVgl6

#define ENVIR aS("Linux")

#define   IBMPC                            // machine 
#define   LINUX   // steal the linux ifdefs as well, mostly the same
#define   GNU                              // C compiler 
#define   P64                              // pointer size 
#define   OPSYS aS("linux")
#define   UNIX                             // os class
#define   REALS                          // Real type turned on
#endif


/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Linux with Gnu C    */
/*______________________*/

#ifdef ENVgl3

#define ENVIR aS("Linux")

#define   IBMPC                            // machine 
#define   LINUX                            // operating system 
#define   GNU                              // C compiler 
#define   P32                              // pointer size 
#define   OPSYS aS("linux")
#define   UNIX                             // os class
#define   REALS                          // Real type turned on

#endif

/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  BSD with Gnu C    */
/*______________________*/

#ifdef ENVgb3

#define ENVIR aS("BSD")

#define   IBMPC                            // machine 
#define   BSD                            // operating system
#define   LINUX   // steal the linux ifdefs as well, mostly the same
#define   GNU                              // C compiler 
#define   P32                              // pointer size 
#define   OPSYS aS("bsd")
#define   UNIX                             // os class
#define   REALS                          // Real type turned on

#endif
/*~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Solaris with Gnu C    */
/*________________________*/

#ifdef ENVgs3

#define ENVIR aS("Solaris")

#define   SUN                              // machine 
#define   SOLARIS                          // operating system 
#define   GNU                              // C compiler 
#define   P32                              // pointer size 
#define   OPSYS aS("solaris")
#define   UNIX                             // os class
#define   REALS                            // Real type turned on
#define   BIG__ENDIAN                       // endian

#endif

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Solaris with Gnu C on Intel x86    */
/*_____________________________________*/

#ifdef ENVgi3

#define ENVIR aS("Solaris")

#define   IBMPC                              // machine 
#define   SOLARIS                          // operating system 
#define   GNU                              // C compiler 
#define   P32                              // pointer size 
#define   OPSYS aS("solaris")
#define   UNIX                             // os class
#define   REALS                            // Real type turned on

#endif

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Solaris with Sun Workshop C++  */
/*_________________________________*/

#ifdef ENVss3

#define ENVIR aS("Solaris")

#define   SUN                              // machine
#define   SOLARIS                          // operating system
#define   SUNC                             // C compiler
#define   P32                              // pointer size
#define   OPSYS aS("solaris")
#define   UNIX                             // os class
#define   BIG__ENDIAN                      // endian

#endif

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*    Windows with VC++        */
/*_____________________________*/

// This used to be ENVmw3dc, to indicate a DLL and
// console version.  With release 5 and up, that is
// the only Windows configuration supported, so it
// is now the simple ENVmw3.

#ifdef ENVmw3

#define ENVIR aS("Windows")

#define   IBMPC       // machine 
#define   WIN4        // operating system 
#define   MSC         // C compiler 
#define   P32         // pointer size 
#define   LIB_DLL     // library type 
#define   WINSTDIO    // standard I/O is OK 
#define   OPSYS aS("windows")
#define   REALS       // real type turned on

#endif

#ifdef ENVmw6

#define ENVIR aS("Windows")

#define   IBMPC       // machine 
#define   WIN4        // operating system 
#define   MSC         // C compiler 
#define   P64         // pointer size 
#define   LIB_DLL     // library type 
#define   WINSTDIO    // standard I/O is OK 
#define   OPSYS aS("windows")
#define   REALS       // real type turned on

#endif
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*          Derived Parameters         */
/*_____________________________________*/

#if defined(WIN3) || defined(WIN4)
#define WINDOWS
#endif

#if  defined(DOS) || defined(WINDOWS)
#define DOSWIN
#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*   Pre r5 Archived Environments      */
/*_____________________________________*/

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Borland 16-bit DOS version  */
/*______________________________*/

#ifdef ENVbd1

#define ENVIR aS("16-bit Real Mode DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   BC          // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Borland 16-bit Windows version  */
/*__________________________________*/

#ifdef ENVbw2

#define ENVIR aS("16-bit Windows")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   BC          // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("windows")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Borland 16-bit Windows version DLL  */
/*______________________________________*/

#ifdef ENVbw2d

#define ENVIR aS("16-bit Windows")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   BC          // C compiler 
#define   P16         // pointer size 
#define   LIB_DLL     // library type 

#define OPSYS aS("windows")

#endif

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Borland 32-bit Windows version  */
/*__________________________________*/

#ifdef ENVbw3

#define ENVIR aS("32-bit Windows")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   BC          // C compiler 
#define   P32         // pointer size 

#define OPSYS aS("windows")

#endif

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Borland 32-bit Quick Windows version  */
/*________________________________________*/

#ifdef ENVbqw3

#define ENVIR aS("32-bit Windows")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   BC          // C compiler 
#define   P32         // pointer size 

#define OPSYS aS("windows")

#endif



/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Alpha 64-bit OSF-1  */
/*______________________*/

#ifdef ENVda6

#define ENVIR aS("64-bit Alpha, DECUnix")

#define   ALPHA       // machine 
#define   OSF1        // operating system 
#define   DECC        // C compiler 
#define   P64         // pointer size 

#define OPSYS aS("osf1")

#endif



/*~~~~~~~~~~~~~~~~~~~~~~*/
/* Alpha 32-bit OpenVMS */
/*______________________*/

#ifdef ENVdv3

#define ENVIR aS("32-bit Alpha, OpenVMS")

#define   ALPHA       // machine 
#define   OPENVMS     // operating system 
#define   DECC        // C compiler 
#define   P32         // pointer size 

#define OPSYS aS("vms")

#endif



/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Solaris with SparcWorks */
/*__________________________*/

#ifdef ENVsol25

#define ENVIR aS("Solaris")

#define   SPARC       // machine 
#define   SOLARIS     // operating system 
#define   SW          // C compiler 
#define   P32         // pointer size 

#define OPSYS aS("solaris")

#define UNIX

#define BIG__ENDIAN        // ray

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Microsoft 16-bit DOS version  */
/*________________________________*/

#ifdef ENVmd1

#define ENVIR aS("16-bit Real Mode DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   MSC         // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Microsoft 286 Protected Mode Windows version */
/*______________________________________________*/

#ifdef ENVmw2

#define ENVIR aS("16-bit Windows")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   MSC         // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("windows")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Microsoft 286 Protected Mode Windows DLL version */
/*__________________________________________________*/

#ifdef ENVmw2d

#define ENVIR aS("16-bit Windows DLL")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   MSC         // C compiler 
#define   P16         // pointer size 
#define   LIB_DLL     // library type 

#define OPSYS aS("windows")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Microsoft 386 Protected Mode Windows version */
/*______________________________________________*/

#ifdef ENVmw3old

#define ENVIR aS("32-bit Windows")

#define   IBMPC                            // machine 
#define   WIN4                             // operating system 
#define   MSC                              // C compiler 
#define   P32                              // pointer size 
//#define   ALIGN32              // makes all internal ops 32-bits for alignment

#define OPSYS aS("windows")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Microsoft 386 Protected Mode Windows DLL version */
/*__________________________________________________*/

#ifdef ENVmw3d

#define ENVIR aS("32-bit Windows DLL")

#define   IBMPC                               // machine 
#define   WIN4                                // operating system 
#define   MSC                                 // C compiler 
#define   P32                                 // pointer size 
#define   LIB_DLL                             // library type 
//#define   ALIGN32             // makes all internal ops 32-bits for alignment

#define OPSYS aS("windows")

#endif



/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Symantec 16-bit DOS version  */
/*_______________________________*/

#ifdef ENVsd1

#define ENVIR aS("16-bit Real Mode DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   SC          // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Symantec 32-bit DOS version  */
/*_______________________________*/

#ifdef ENVsd3

#define ENVIR aS("32-bit DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   SC          // C compiler 
#define   P32         // pointer size 

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*  Watcom 16-bit DOS version  */
/*_____________________________*/

#ifdef ENVwd1

#define ENVIR aS("16-bit Real Mode DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   WC          // C compiler 
#define   P16         // pointer size 

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Watcom 32-bit DOS   */
/*______________________*/

#ifdef ENVwd3

#define ENVIR aS("32-bit DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   WC          // C compiler 
#define   P32         // pointer size 
//#define   ALIGN32            // makes all internal ops 32-bits for alignment

#define OPSYS aS("dos")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* Watcom 386 Protected Mode Windows version DLL */
/*__________________________________________________*/

#ifdef ENVww3d

#define ENVIR aS("32-bit Windows DLL")

#define   IBMPC       // machine 
#define   WIN3        // operating system 
#define   WC          // C compiler 
#define   P32         // pointer size 
#define   LIB_DLL     // library type 
//#define   ALIGN32           // makes all internal ops 32-bits for alignment

#define OPSYS aS("windows")

#endif


/*~~~~~~~~~~~~~~~~~~~~~~*/
/*  Zortech 32-bit DOS  */
/*______________________*/

#ifdef ENVzd3

#define ENVIR aS("32-bit DOS")

#define   IBMPC       // machine 
#define   DOS         // operating system 
#define   ZTC         // C compiler 
#define   P32         // pointer size 
//#define   ALIGN32            // makes all internal ops 32-bits for alignment

#define OPSYS aS("dos")

#endif

#endif //ENV_H

