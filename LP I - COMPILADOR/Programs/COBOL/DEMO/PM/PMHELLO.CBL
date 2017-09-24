      $set ans85 noosvs mf
      ****************************************************************
      * 							     *
      * 							     *
      *                (C) Micro Focus Ltd. 1989,1990                *
      * 							     *
      * 		      PMHELLO.CBL			     *
      * 							     *
      * Example program: Presentation Manager 'Hello World'	     *
      * 							     *
      * To compile, link and run:                                    *
      *                                                              *
      *     cobol pmhello linklib"coblib+os2";                       *
      *     link pmhello;                                            *
      *     pmhello                                                  *
      *                                                              *
      * The LINKLIB directive to the compiler may be omitted if      *
      * it is included in your COBOL.DIR. If it is not specified     *
      * at compile time, then the link step becomes:                 *
      *                                                              *
      *     link pmhello,,,coblib+os2;                               *
      *                                                              *
      * Note that OS2.LIB is required for this program. This module  *
      * is part of the OS/2 Software Developer's Toolkit, and is     *
      * also part of the Microsoft Utilities.                        *
      ****************************************************************
      * 							     *
      * Presentation Manager Programming			     *
      * 							     *
      * Please refer to printed and online documentation for more    *
      * information on PM Presentation Manager programming with      *
      * COBOL.                                                       *
      * 							     *
      ****************************************************************
      * 							     *
      * About PMHELLO						     *
      * 							     *
      * A number of extensions to the COBOL language are used in     *
      * this program, and are noted in comments where they occur.    *
      * See the documentation and release notes for a full           *
      * description of PM Programming facilities for COBOL.          *
      * 							     *
      * This release of COBOL provides some prototype Systems	     *
      * Programming Facilities which enable among other things the   *
      * COBOL programmer to utilize Presentation Manager.	     *
      * 							     *
      * To write your own PM programs in COBOL, we recommend that    *
      * you use this program as a base. 			     *
      * 							     *
      ****************************************************************

      ****************************************************************
      * 							     *
      * COBOL Extension: Special-names. 			     *
      * 							     *
      *     Call-conventions are supported as below.                 *
      * 							     *
      *     The meaning of the numbers is derived from decomposing   *
      *     the number into binary components, with bits having      *
      *     the following meanings:				     *
      * 							     *
      *     0	-   no bits specified means that the standard	     *
      * 	    COBOL Calling conventions are		     *
      * 	    employed.  This means parameters are passed      *
      * 	    on a stack, last named is first pushed on the    *
      * 	    stack.  The parameters are removed from the      *
      *             stack by the CALLing program.                    *
      * 	    Use this for compatibility with existing COBOL   *
      * 	    programs.					     *
      *     1	-   parameters are passed on a stack, first named    *
      * 	    is first pushed.  So you could call this	     *
      *             convention 'REVERSED'.                           *
      *     2	-   The parameters are removed from the stack	     *
      *             by the called routine.                           *
      * 							     *
      * 							     *
      *     So, we get the 'OS2API' convention used by PM as	     *
      *     convention 3. This convention is alternatively known as  *
      *     the PASCAL calling convention.			     *
      * 							     *
      ****************************************************************
	special-names.
	    call-convention 3 is OS2API.

	working-storage section.

      ****************************************************************
      * 							     *
      * PM Toolkit supplies a number of C header files which define  *
      * constants.  In COBOL you may use the H2CPY utility to create *
      * the COBOL datanames and PICTURE clauses from their C         *
      * equivalents. That is what has been done in PMHELLO.          *
      * For more information on the H2CPY utility, please see the    *
      * various printed and on-line documentation files.             *
      *                                                              *
      * In this program, we are using the Wm-Paint message,          *
      * the System Background Color Sysclr-Window and the            *
      * System default window text color Sysclr-WindowText.          *
      * To translate values from C constants to COBOL constants,     *
      * H2CPY uses the following rules:                              *
      * 							     *
      * 		    C		COBOL			     *
      *     Hexadecimal   0xnn		h"nn"			     *
      *     Decimal	    nn		  nn			     *
      * 							     *
      ****************************************************************

        78  Wm-Paint                VALUE   H"23".
        78  Sysclr-Window           VALUE   -20.
        78  Sysclr-WindowText       VALUE   -17.

	01  work-data.
      ****************************************************************
      * 							     *
      * The supplied C header files define data types for all the    *
      * PM data items.  For COBOL we have to use the COBOL data      *
      * types.                                                       *
      * 							     *
      * As a general conversion rule:				     *
      * 							     *
      *     'C' 	COBOL					     *
      *     SHORT	PIC S9(4) COMP-5			     *
      *     USHORT	PIC 9(4)  COMP-5			     *
      *     LONG	PIC S9(9) COMP-5			     *
      *     ULONG	PIC 9(9)  COMP-5			     *
      *     PVOID	POINTER 	    (similarly for other     *
      * 				    pointer types)	     *
      *     LHANDLE     PIC 9(9)  COMP-5    (These are equivalent    *
      *     LHANDLE     POINTER             for PM working)          *
      *                                                              *
      *                         LHANDLE is used for any 32 bit       *
      * 			handle, eg HAB, HMQ, HPS etc.	     *
      * 							     *
      *     NB	PIC 9(4) COMP-5 is identical to PIC X(2) COMP-5      *
      *     NB	PIC 9(9) COMP-5 is identical to PIC X(4) COMP-5      *
      * 							     *
      ****************************************************************
            03  Hab                 POINTER.
            03  Hmq                 POINTER.
            03  HwndClient          PIC 9(9) comp-5.
            03  HwndFrame           PIC 9(9) comp-5.

      ****************************************************************
      * 							     *
      * As an alternative to using the SIZE clause in the CALL	     *
      * statements, we can define data items with the correct	     *
      * size and use that.					     *
      * 							     *
      ****************************************************************
            03  Hwnd-Desktop        PIC 9(9) COMP-5 VALUE 1.

      ****************************************************************
      * 							     *
      * Class styles are defined in the header files. H2CPY will     *
      * give the appropriate numbers. Note that Fcf-ctl-data is      *
      * set to a total of H"00000C3B" for    ... Fcf-Tasklist        *
      *                                     with Fcf-Shellposition   *
      *                                      and Fcf-Minmax          *
      *                                      and Fcf-Sizeborder      *
      *                                      and Fcf-Sysmenu         *
      *                                      and Fcf-Titlebar        *
      * 							     *
      ****************************************************************
            03  CS-Sizeredraw    PIC 9(9) COMP-5   VALUE H"04".
            03  WS-Visible       PIC 9(9) COMP-5   VALUE H"80000000".
            03  Fcf-ctl-data     PIC 9(9) COMP-5   VALUE H"00000c3b".

      ****************************************************************
      * 							     *
      * ASCIIZ strings are not natural with COBOL, and in particular *
      * are not suitable for use as literals.			     *
      * Where ASCIIZ strings are used, they must be declared in      *
      * Working-Storage and followed by a x"00" NULL terminator.     *
      * We use the literal concatenation '&' operator.               *
      * 							     *
      ****************************************************************
            03  MyClass             pic x(9) value 'MyClass' & x'00'.

	    03	loop-flag	    pic x value 'C'.
		88  loop-end		value 'E'.
	    03	bool		    pic 9(4) comp-5.
		88  boolTRUE		value 1.
		88  boolFALSE		value 0.

      ****************************************************************
      * 							     *
      * Structures are supplied in C header files. H2CPY will        *
      * convert them to COBOL format.                                *
      * Below is a QMSG structure, and in LOCAL-STORAGE section      *
      * are examples of RECTL and POINTL structures.		     *
      * 							     *
      ****************************************************************
	    03	qmsg.
		05  qmsghwnd	    pic 9(9) comp-5.
		05  qmsgmsg	    pic 9(4) comp-5.
		05  qmsgmp1	    pic 9(9) comp-5.
		05  qmsgmp2	    pic 9(9) comp-5.
		05  qmsgtime	    pic 9(9) comp-5.
		05  qmsgptl.
		    07	qmsgptlx    pic 9(9) comp-5.
		    07	qmsgptly    pic 9(9) comp-5.

      ****************************************************************
      * 							     *
      * COBOL Extension: Procedure-pointers			     *
      * 							     *
      *     Data pointers are now complemented by procedure pointers *
      * 							     *
      ****************************************************************
	    03	WndProc     procedure-pointer.

      ****************************************************************
      * 							     *
      * COBOL Extension: Local-Storage Section. 		     *
      * COBOL Extension: Recursion				     *
      * 							     *
      *     Any data declared in the LOCAL-STORAGE SECTION is	     *
      *     created freshly for each instance of the program.	     *
      *     This data cannot currently be initialized.               *
      * 							     *
      ****************************************************************
	local-storage section.
        01  hps         pointer.
        01 Rectl.
           05 Rectl-Xleft                    PIC S9(9) COMP-5.
           05 Rectl-Ybottom                  PIC S9(9) COMP-5.
           05 Rectl-Xright                   PIC S9(9) COMP-5.
           05 Rectl-Ytop                     PIC S9(9) COMP-5.

        01  Mresult     pic x(4) comp-5.

	linkage section.
        01  hwnd        pointer.
        01  msg         pic x(2) comp-5.
        01  mp1         pic x(4) comp-5.
	01  redefines mp1.
            03  mp1w1   pic x(2) comp-5.
            03  mp1w2   pic x(2) comp-5.
        01  mp2         pic x(4) comp-5.
	01  redefines mp2.
            03  mp2w1   pic x(2) comp-5.
            03  mp2w2   pic x(2) comp-5.

      ****************************************************************
      * 							     *
      * COBOL Extension: Call-conventions			     *
      * 							     *
      *     This use of the call-convention OS2API (declared above   *
      *     in special-names) means that all the entry points in     *
      *     this program follow the OS2API calling convention unless *
      *     they specify otherwise                                   *
      * 							     *
      ****************************************************************
	procedure division OS2API.
	main section.

      ****************************************************************
      * 							     *
      * COBOL Extension: Call-conventions			     *
      * COBOL Extension: SIZE clause				     *
      * COBOL Extension: RETURNING phrase			     *
      * 							     *
      *     This use of the call-convention OS2API (declared above   *
      *     in special-names) means that the target procedure	     *
      *     follows the OS2API calling convention.		     *
      * 							     *
      *     Passing parameters by value allows explicit sizing.      *
      *     This is to enable distinction between 2 and 4 byte	     *
      *     literals.						     *
      * 							     *
      *     The returning phrase has been added to avoid complicated *
      *     and clumsy use of the RETURN-CODE special register.      *
      * 							     *
      ****************************************************************
	    call OS2API '__WinInitialize'
			using	by value 0 size 2
			returning hab
	    call OS2API '__WinCreateMsgQueue'
			using by value hab
			      by value 0 size 2
			returning hmq

      ****************************************************************
      * 							     *
      * COBOL Extension: Procedure-pointers			     *
      * 							     *
      *     Procedure pointers can be set to point to an entry	     *
      *     point.  The entry point must be valid to be called	     *
      *     at this point in the program.			     *
      * 							     *
      ****************************************************************
	    set WndProc to ENTRY 'WndProc'
	    call OS2API '__WinRegisterClass'
			using by value	   hab
			      by reference MyClass
			      by value	   WndProc
                              by value     Cs-Sizeredraw
			      by value	   0	    size 2
			returning bool
	    if boolTRUE

		call OS2API '__WinCreateStdWindow'
			    using by value     HWND-DESKTOP
                                  by value     Ws-Visible
                                  by reference Fcf-ctl-data
				  by reference MyClass
                                  by reference 'MyTitle' & x'00'
				  by value     0	size 4
				  by value     0	size 2
				  by value     0	size 2
				  by reference hwndClient
			    returning hwndFrame

		if hwndFrame not = 0

      ****************************************************************
      * 							     *
      * This in-line PERFORM implements the message loop.	     *
      * 							     *
      ****************************************************************
		    perform until loop-end
			call OS2API '__WinGetMsg'
				    using by value hab
					  by reference qmsg
					  by value 0	    size 4
					  by value 0	    size 2
					  by value 0	    size 2
				    returning bool

			if boolFALSE
			    set loop-end to true
			else
			    call OS2API '__WinDispatchMsg'
					using by value hab
					      by reference qmsg

		    end-perform

		    call OS2API '__WinDestroyWindow'
				using by value hwndFrame

		end-if

	    end-if

	    call OS2API '__WinDestroyMsgQueue' using by value hmq
	    call OS2API '__WinTerminate'       using by value hab

	    stop run.

	MyWndProc section.
      ****************************************************************
      * 							     *
      * COBOL Extension: ENTRY USING BY VALUE			     *
      * COBOL Extension: Recursion				     *
      * 							     *
      *     To complement the CALL USING BY VALUE, we now allow      *
      *     ENTRY USING BY VALUE.				     *
      * 							     *
      *     COBOL being recursive means that the call to	     *
      *     WinCreateStdWindow (above) can lead to control being     *
      *     passed to this entry point. 			     *
      *     In fact, any of the calls in this section could lead     *
      *     to control being passed to a new instance of this	     *
      *     entry point (hence the need for LOCAL-STORAGE SECTION.)  *
      * 							     *
      ****************************************************************
	entry 'WndProc' using by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.

            move 0 to mresult
	    evaluate msg

      ****************************************************************
      * 							     *
      * The only message we are interested in is the PAINT message   *
      * The sequence of actions is:				     *
      * 							     *
      *     Get Handle-To-Presentation-Space (HPS) for painting      *
      * 			in the client window		     *
      *     Fill the window with the System Background colour	     *
      *     Write the words 'Hello COBOL World' at position (20,20)  *
      *     Release the HPS.					     *
      * 							     *
      ****************************************************************
                when    Wm-Paint
		    call OS2API '__WinBeginPaint'
				using by value hwnd
				      by value 0 size 4
                                      by reference Rectl
                                returning hps

                    call OS2API '__WinQueryWindowRect'
                                using by value hwnd
                                      by reference Rectl

                    call OS2API '__WinDrawText'
                                using by value hps
                                      by value 17 size 2
                                      by reference 'Hello COBOL World'
                                      by reference Rectl
                                      by value Sysclr-WindowText size 4
                                      by value Sysclr-Window     size 4
                                      by value h'8500' size 2

		    call OS2API '__WinEndPaint'
				using by value hps

      ****************************************************************
      * 							     *
      *     All other messages are despatched to the default	     *
      *     window procedure according to the PM rules. 	     *
      * 							     *
      ****************************************************************
		when other
		    call OS2API '__WinDefWindowProc'
				using by value hwnd
				      by value msg
				      by value mp1
				      by value mp2
                                returning Mresult

	    end-evaluate

      ****************************************************************
      * 							     *
      * COBOL Extension: RETURNING phrase			     *
      * 							     *
      *     To complement the RETURNING phrase on the CALL, you      *
      *     can also use the RETURNING phrase on the EXIT.	     *
      * 							     *
      ****************************************************************
            exit program returning Mresult.
