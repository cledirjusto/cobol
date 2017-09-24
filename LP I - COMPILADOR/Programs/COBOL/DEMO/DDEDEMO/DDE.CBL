      $set ans85 noosvs mf

      *******************************************************************
      *                                                                 *
      *     DDE example using COBOL                                     *
      *                                                                 *
      *             by Raymond Obin                                     *
      *             (C) 1990 Micro Focus Ltd.                           *
      *                                                                 *
      *             Written 12 June 1990                                *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     This DDE example can be used with either Excel (with the    *
      *     noted restrictions) or with DDE2, which pretends to be      *
      *     Excel for the purpose of this example.                      *
      *                                                                 *
      *     This DDE example makes use of the DDE Manager which is      *
      *     supplied with the Microsoft OS/2 1.2 Programmers Toolkit.   *
      *                                                                 *
      *     DDEMGR.DLL is supplied with this example, and should be     *
      *     placed in a LIBPATH directory (eg C:\OS2\DLL)               *
      *     The source for DDEMGR is available from Microsoft as part   *
      *     of the Programmers Toolkit, and you should refer to this    *
      *     source code for documentation on the DDE Manager.           *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     Running instructions:                                       *
      *         If you are using DDE and DDE2, either program can be    *
      *         started first, and either can be closed down first.     *
      *         If you are using DDE and Excel, you must start          *
      *         Excel first, but either can be closed down fitst.       *
      *                                                                 *
      *     See 'About this example program' for instruction on how     *
      *     to make these programs communicate.                         *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     The use made of DDEMGR is documented in this program, and   *
      *     is also used in DDE2.  Comments in DDE2 are sparse, but     *
      *     the essence is the same as in this program.                 *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     Comments which refer to DDE Manager conventions/functions   *
      *     are noted with DDE in cols 1-3                              *
      *                                                                 *
DDE   *     A DDE Manager Comment                                       *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     Overview of DDE                                             *
      *                                                                 *
      *     Dynamic Data Exchance (or DDE) is a protocol by which       *
      *     applications can exchange data.  It is set up so that       *
      *     an application can ask to be notified when a particular     *
      *     piece of data changes.                                      *
      *     A trivial example might be a spread sheet wanting to know   *
      *     about stock market shifts.  The spread sheet sets itself    *
      *     up as a DDE Client, and talks to a DDE Server that knows    *
      *     about Stock prices.  The Client asks the Server to update   *
      *     it each time certain stocks change their value, and thus    *
      *     the spread sheet can be kept up to date.                    *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     About this example program                                  *
      *                                                                 *
      *     DDE creates a window with a DDE Status line at the bottom   *
      *     with some scroll bars on the left and a colour block on     *
      *     the top right.                                              *
      *                                                                 *
      *     DDE automatically starts a conversation with 'Excel'        *
      *     ('Excel' is the DDE Name used by Excel and DDE2) and        *
      *     will set R1C1 (ie Position A1) of 'Sheet1' to contain the   *
      *     word 'Red'.  This is the colour of the colour block you     *
      *     will see.                                                   *
      *     If Excel is not already running, DDE will notice when Excel *
      *     is started, and begin the conversation then.                *
      *                                                                 *
      *     You can select Excel and change the colour by typing into   *
      *     cell A1 any of the following values:                        *
      *         Red, Yellow, Blue, Green, Black, White, Cyan, Magenta   *
      *                                                                 *
      *     When you do, you will notice that the colour block will     *
      *     change colour to reflect the value you typed.               *
      *                                                                 *
      *     DDE has told Excel that it wants to know when R1C1 changes, *
      *     and when it receives the new value, it adjusts the Colour   *
      *     block accordingly.                                          *
      *                                                                 *
      *     If you are using DDE2 instead of Excel, you can select      *
      *     a new colour from the list box containing the choices.      *
      *                                                                 *
      *     This is an example of Excel acting as a Server, and DDE     *
      *     as a Client.                                                *
      *                                                                 *
      *     DDE and Excel can be Client and Server at the same time,    *
      *     and the 3 scroll bars illustrate Excel as the Client and    *
      *     DDE as the Server.                                          *
      *                                                                 *
      *     If you take a cell in Excel (eg A3) and enter the formula:  *
      *                                                                 *
      *         =COBOLDDE|TestData!Number1                              *
      *         =              indicates that this is a formula         *
      *          COBOLDDE      is the name of the server application    *
      *                  |     (ALT+124) indicates that a remote link   *
      *                        is being established                     *
      *                   TestData       is the Server Topic            *
      *                           !      separates the topic and the id *
      *                            Number1 is the data itentifier       *
      *                                                                 *
      *     DDE will respond with the the setting of the lower scroll   *
      *     bar.  (Lower scroll bar is 'Number1', middle is 'Number2',  *
      *     upper is 'Number3')                                         *
      *                                                                 *
      *     There is a problem with Excel and the DDE Manager which     *
      *     means that the cell will only be updated the first time     *
      *     you move the scroll bar.  Attempts to isolate this problem  *
      *     have so far been unsuccessful, but there appears to be      *
      *     a small incompatibility between Excel and the DDEMgr.       *
      *                                                                 *
      *     If you use DDE2 instead of Excel, you can move the scroll   *
      *     bars around and continue to see the values updated.         *
      *                                                                 *
      *     The 'hot link' between DDE2 and DDE is created (for all     *
      *     three 'Number's by pressing the Initiate button.            *
      *     Initial values are obtained from DDE for the slider         *
      *     positions, and any changes are reflected.                   *
      *                                                                 *
      *     The 'hot link' can be terminated by pressing 'Terminate'.   *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     There are some stability problems with this example,        *
      *     in particular the machine hangs if you Terminate and        *
      *     Initiate repeatedly.  These problems have not been          *
      *     isolated.                                                   *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     This example make extensive use of the Micro Focus          *
      *     Systems Programming Extensions to COBOL.                    *
      *     See your documentation and the other Presentation Manager   *
      *     sample programs for details of these.                       *
      *                                                                 *
      *******************************************************************

	special-names.
	    call-convention 3 is OS2API.

        working-storage section.

      * PM Window Messages sent the the Client Window

        78  WM-CREATE               value   h"01".
        78  WM-DESTROY              value   h"02".
        78  WM-PAINT                value   h"23".
        78  WM-CONTROL              value   h"30".
        78  WM-HSCROLL              value   h"32".
        78  WM-SIZE                 value   h"07".

      * User message for synchronizing the DDE commencement

        78  WMUSER-INITDDE          value   h"2000".
        78  WMUSER-FREEHSZ          value   h"2001".

      * PM Scroll Bar messages

        78  SBM-SETSCROLLBAR        value   h"01a0".
        78  SBM-SETPOS              value   h"01a1".
        78  SBM-QUERYPOS            value   h"01a2".

        78  SB-LINELEFT             value   1.
        78  SB-LINERIGHT            value   2.
        78  SB-PAGELEFT             value   3.
        78  SB-PAGERIGHT            value   4.
        78  SB-SLIDERTRACK          value   5.
        78  SB-SLIDERPOSITION       value   6.
        78  SB-ENDSCROLL            value   7.

      * Other constants used by PM

        78  SYSCLR-WINDOW           value   -20.
        78  SYSCLR-WINDOWTEXT       value   -17.

        78  SIZEMOVESHOW            value   h"0b".

        78  WC-SCROLLBAR            value   h"ffff0008".

        78  HWND-DESKTOP            value   1.
        78  HWND-TOP                value   3.

      * PM Colour constants

        78  CLR-WHITE               value   -2.
        78  CLR-BLACK               value   -1.
        78  CLR-BLUE                value   1.
        78  CLR-RED                 value   2.
        78  CLR-MAGENTA             value   3.
        78  CLR-GREEN               value   4.
        78  CLR-CYAN                value   5.
        78  CLR-YELLOW              value   6.

DDE   * DDEMGR constants

DDE   * DDE Format used by this program
        78  DDEFMT-TEXT             value   1.

DDE   * DDE Acknowledgement
        78  DDE-FACK                value   1.

DDE   * DDE Request identifiers
        78  XTYP-INIT               VALUE H"0120".  *>  Initialize
        78  XTYP-ADVDATA            VALUE H"0240".  *>  Advise Data
        78  XTYP-EXEC               VALUE H"0340".  *>  Execute Command
        78  XTYP-POKE               VALUE H"0440".  *>  Poke Command
        78  XTYP-ADVSTART           VALUE H"0520".  *>  Start Advise
        78  XTYP-TERM               VALUE H"0610".  *>  Terminate
        78  XTYP-ADVSTOP            VALUE H"0710".  *>  Stop Advise
        78  XTYP-REGISTER           VALUE H"0810".  *>  Register
        78  XTYP-UNREGISTER         VALUE H"0910".  *>  UnRegister
        78  XTYP-ADVREQ             VALUE H"0A80".  *>  Request Advise Data
        78  XTYP-MONITOR            VALUE H"0B10".  *>  Monitor
        78  XTYP-ACK                VALUE H"0C10".  *>  Acknowledge
        78  XTYP-WILDINIT           VALUE H"0D80".  *>  Wild Initialize
        78  XTYP-REQUEST            VALUE H"0E80".  *>  Request Data
        78  XTYP-XFERCOMPLETE       VALUE H"0F10".  *>  Transfer Complete

	01  work-data.
            03  hab                 pointer.
            03  hmq                 pointer.
	    03	hwndClient	    pic 9(9) comp-5.
	    03	hwndFrame	    pic 9(9) comp-5.

            03  CSClass             pic 9(9) comp-5 value h"20000004".
	    03	WSWindow	    pic 9(9) comp-5 value h"80000000".
	    03	ctldata 	    pic 9(9) comp-5 value h"00000c3b".

            03  MyClass             pic x(9) value 'MyClass' & x'00'.

	    03	loop-flag	    pic x value 'C'.
		88  loop-end		value 'E'.
	    03	bool		    pic 9(4) comp-5.
		88  boolTRUE		value 1.
		88  boolFALSE		value 0.

	    03	qmsg.
		05  qmsghwnd	    pic 9(9) comp-5.
		05  qmsgmsg	    pic 9(4) comp-5.
		05  qmsgmp1	    pic 9(9) comp-5.
		05  qmsgmp2	    pic 9(9) comp-5.
		05  qmsgtime	    pic 9(9) comp-5.
		05  qmsgptl.
		    07	qmsgptlx    pic 9(9) comp-5.
		    07	qmsgptly    pic 9(9) comp-5.

            03  WndProc     procedure-pointer.

            03  errorCode           pic 9(4) comp-5.

      * Constant names used to communicate with Excel
      * TestData is the Topic supported by DDE
      * Sheet1 is the Topic we use within Excel
      * R1C1 is the item within the topic in Excel

        78  L-TESTDATA          value 'TestData'.
        78  L-TARGETTOPIC       value 'Sheet1'.
        78  L-TARGETCELL        value 'R1C1'.

DDE   * The DDE Manager provides a string handle feature.
DDE   * You get a handle on a null-terminated string by calling the
DDE   * DDEMgrGetHsz function.  You can then use the string handle (or hsz)
DDE   * like a string.  Equal strings will have equal hszs.
DDE   * Passing text data to DDEMgr functions is often done by passing
DDE   * the hsz rather than a pointer to the string.
DDE   * String handles have a use-count, and you should ensure that
DDE   * any hsz you see actually belongs to you otherwise it may be
DDE   * deallocated by someone else and reused - associated with a
DDE   * different string.
DDE   * If you receive a hsz from DDEMgr, and you want to keep it,
DDE   * you should call DDEMgrIncHszCount to protect it.

      * As part of our initialization, we will initialize a number
      * of string handles and store them here.

        01  DDEDataArea.
            03  DDEName         pic x(20).
            03  DDETopic        pic x(9)  value L-TESTDATA & x"00".
            03  hszDDETopic     pointer.
            03  DDEL-System     pic x(7)  value 'System' & x"00".
            03  hszSystem       pointer.
            03  DDEL-Topics     pic x(7)  value 'Topics' & x"00".
            03  hszTopics       pointer.
            03  DDEL-SysItems   pic x(9)  value 'SysItems' & x"00".
            03  hszSysItems     pointer.
            03  DDEL-Status     pic x(7)  value 'Status' & x"00".
            03  hszStatus       pointer.
            03  TargetApp       pic x(20).
            03  hszTargetApp    pointer.
            03  TargetTopic     pic x(20) value L-TARGETTOPIC & x"00".
            03  hszTargetTopic  pointer.
            03  Item1           pic x(20) value 'Selection' & x"00".

      * We send commands to Excel in the following format:
      *     [command]       the command should be enclosed in square brackets
      *     FORMULA("data","cell")
      *                     any Excel function can be used as a command
      *                     in this case, we are setting Sheet1!R1C1 to
      *                     contain the value "Red"
      *                     You really do need all the ()'s and "'s



            03  Data1           pic x(50) value
                                            '[FORMULA'
                                          & '("Red"'
                                          & ','
                                          & '"'
                                          & L-TARGETTOPIC
                                          & '!'
                                          & L-TARGETCELL
                                          & '")]' & x"00".
            03  Item2           pic x(20) value L-TARGETCELL & x"00".
            03  Data2           pic x(50) value x"00".
            03  ItemXfer        pic x(20).
            03  DataXfer        pic x(50).
            03  XferProc        procedure-pointer.
            03  xtyp            pic 9(4) comp-5.
            78  DDEMsgSize      value 40.
            03  DDEMsg          pic x(DDEMsgSize).
            03  DDEData         pic x(256).
            03  hszItemName     pointer.
            03  pidXfer         pic 9(9) comp-5.
            03  hConversation   pointer.
            03  hDmgConvData    pointer.
            03  RectColour      pic s9(9) comp-5 value CLR-RED.
            03  AdviseControl   occurs 3.
                05  AdivseFlag  pic x   value 'N'.
                    88  AdviseOK        value 'Y'.
                    88  AdviseOff       value 'N'.
                05  AdviseHwnd      pointer.
                05  hszAdviseTopic  pointer.
                05  hszAdviseItem   pointer.

	local-storage section.
        01  hps         pointer.
	01  rcl.
            03  xLeft   pic x(4) comp-5.
            03  redefines xLeft.
                05  xLeftl pic xx comp-5.
                05  xLefth pic xx comp-5.
	    03	yBottom pic x(4) comp-5.
            03  redefines yBottom.
                05  yBottoml pic xx comp-5.
                05  yBottomh pic xx comp-5.
	    03	xRight	pic x(4) comp-5.
	    03	yTop	pic x(4) comp-5.
	01  mresult	pic x(4) comp-5.
        01  DDELocalData.
            03  DDEResult   pic x(4) comp-5.
            03  DDEReply    pic x(40).
            03  DDEReply9   pic 9(3).
            03              pic x comp-5 value 0.
            03  ConversationData.
        78  lszTopicString          value 40.
        78  lszItemString           value 40.
                05  hszTopicString      pic x(lszTopicString).
                05  hszItemString       pic x(lszItemString).

        01  ppl.
            03  x               pic x(4) comp-5.
            03  redefines x.
                05  xl pic xx comp-5.
                05  xh pic xx comp-5.
            03  y               pic x(4) comp-5.
            03  redefines y.
                05  yl pic xx comp-5.
                05  yh pic xx comp-5.
        01  i                   pic 9(9) comp-5.
        01  si                  pic 9(4) comp-5.

	linkage section.
        01  hwnd        pic x(4) comp-5.
        01  msg         pic x(2) comp-5.
        01  mp1         pic x(4) comp-5.
	01  redefines mp1.
            03  mp1w1   pic x(2) comp-5.
            03  mp1w2   pic x(2) comp-5.
        01  mp2         pic x(4) comp-5.
	01  redefines mp2.
            03  mp2w1   pic x(2) comp-5.
            03  mp2w2   pic x(2) comp-5.

        01  hConv           pointer.
        01  hszTopic        pointer.
        01  hszItem         pointer.
        01  usFmt           pic 9(4) comp-5.
        01  usType          pic 9(4) comp-5.
        01  hDmgData        pointer.

	procedure division OS2API.
        main section.

      * Default names for this program and the 'other' program are
      *         DDEName (this program)          COBOLDDE
      *         TargetApp (that program)        Excel
      *
      * You can override these names by starting DDE and DDE2 with a
      * command lines:
      *
      *     DDE MyName YourName
      *     DDE2 YourName MyName

            accept DDEData from command-line
            unstring DDEData delimited by spaces
                    into DDEName TargetApp

            if DDEName = spaces
                move 'COBOLDDE' to DDEName
            end-if
            if TargetApp = spaces
                move 'Excel' to TargetApp
            end-if
            perform varying i from length of DDEName by -1
                        until DDEName(i:1) not = spaces
                move x"00" to DDEName(i:1)
            end-perform
            perform varying i from length of TargetApp by -1
                        until TargetApp(i:1) not = spaces
                move x"00" to TargetApp(i:1)
            end-perform

	    call OS2API '__WinInitialize'
			using	by value 0 size 2
			returning hab
	    call OS2API '__WinCreateMsgQueue'
			using by value hab
			      by value 0 size 2
			returning hmq

	    set WndProc to ENTRY 'WndProc'
	    call OS2API '__WinRegisterClass'
			using by value	   hab
			      by reference MyClass
			      by value	   WndProc
			      by value	   CSClass
			      by value	   0	    size 2
			returning bool
	    if boolTRUE

		call OS2API '__WinCreateStdWindow'
                            using by value     HWND-DESKTOP size 4
				  by value     WSWindow
				  by reference ctldata
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
	entry 'WndProc' using by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.

	    move 0 to mresult
            evaluate msg

                when    WM-CREATE

DDE   * DDE programs have to have a PM message loop.
DDE   * You should not call DDEMgrRegister unless the calling thread
DDE   * has already called WinCreateMsgQueue.

DDE   * Because DDE can call an application at any time (unless callback
DDE   * is disabled, but see DDEMgr documentation for that), a callback
DDE   * routine has to be defined.  This program uses the entry point:
DDE   *     DDEXFER
DDE   *         as the callback routine.
DDE   * See comments there for how that works.

DDE   * Parameters to DDEMgrRegister are:
DDE   *             null-terminated string for this applications name
DDE   *             procedure pointer for the callback routine
DDE   *             timeout value (in milli-seconds)
DDE   *             flags (see DDEMGR documentation for details
DDE   *             reserved for expansion

                    set XferProc to entry 'DDEXFER'

                    call OS2API '__DDEMgrRegister' using
                                by reference DdeName
                                by value     XferProc
                                by value     1000        size 4
                                by value     0           size 4
                                by value     0           size 4
                            returning errorCode

                    if errorCode = 0
                        move 'DDE Register successful' to DDEMsg
                    else
                        move 'DDE Register fail' to DDEMsg
                    end-if

                    call OS2API '__WinPostMsg' using
                                by value hwnd
                                by value WMUSER-INITDDE
                                by value 0 size 4
                                by value 0 size 4


                    set hConversation to NULL

                    move 0 to mp1
                    move 0 to mp2w1
                    move 100 to mp2w2
                    perform varying si from 1 by 1 until si > 3
                        call OS2API '__WinCreateWindow' using
                                    by value hwnd
                                    by value WC-SCROLLBAR size 4
                                    by value 0 size 4
                                    by value 0 size 4
                                    by value 0 size 2
                                    by value 0 size 2
                                    by value 0 size 2
                                    by value 0 size 2
                                    by value hwnd
                                    by value HWND-TOP size 4
                                    by value si
                                    by value 0 size 4
                                    by value 0 size 4
                                    returning AdviseHwnd(si)
                        call OS2API '__WinSendMsg' using
                                    by value AdviseHwnd(si)
                                    by value SBM-SETSCROLLBAR size 2
                                    by value mp1
                                    by value mp2
                    end-perform

                when WMUSER-INITDDE

                    perform GetHszs

      * Initializing the conversation (we are client since we initiate the
      * conversation

                    perform BeginConversation

                when WMUSER-FREEHSZ

                    perform FreeHszs

                    move 'DDE Target not available' to DDEMsg
                    perform Refresh

                when    WM-DESTROY

DDE   * We do our best to terminate correctly, but we don't really look
DDE   * at the error message hard enough...

                    move Data2 to DataXfer
                    move Item2 to ItemXfer
                    move XTYP-ADVSTOP to xtyp
                    perform DDEXfer

                    perform TermConv

                    perform FreeHszs

                    call OS2API '__DDEMgrUnregister'
                            returning errorCode

                when    WM-SIZE

      * We calculate the Scroll Bar size and position based on the
      * size of the Client Area

                    move mp2w1 to xl
                    move mp2w2 to yl
                    divide 3 into xl
                    divide 15 into yl

                    perform varying si from 1 by 1 until si > 3
                        compute yBottoml = yl * ( 4 * si - 1 )
                        compute xLeftl = mp2w1 / 8
                        call OS2API '__WinSetWindowPos' using
                                    by value AdviseHwnd(si)
                                    by value HWND-TOP size 4
                                    by value xLeftl
                                    by value yBottoml
                                    by value xl
                                    by value yl
                                    by value SIZEMOVESHOW size 2

                    end-perform

                when    WM-HSCROLL

      * When a Scroll Bar is touched, we process it.
      * All scroll bars are set up with a range of 0-99

                    move mp1w1 to si
                    perform GetSliderPos
                    evaluate mp2w2
                        when SB-LINELEFT
                            if i > 0
                                subtract 1 from i
                            else
                                move 0 to si
                            end-if
                        when SB-LINERIGHT
                            if i < 100
                                add 1 to i
                            else
                                move 0 to si
                            end-if
                        when SB-PAGELEFT
                            if i > 10
                                subtract 10 from i
                            else
                                move 0 to si
                            end-if
                        when SB-PAGERIGHT
                            if i < 90
                                add 10 to i
                            else
                                move 0 to si
                            end-if
                        when SB-SLIDERPOSITION
                        when SB-SLIDERTRACK
                            move mp2w1 to i
                        when other
                    end-evaluate
                    if si > 0
                        call OS2API '__WinSendMsg' using
                                    by value AdviseHwnd(si)
                                    by value SBM-SETPOS size 2
                                    by value i
                                    by value 0 size 4

DDE   * If we are acting as a DDE Server, we have to notify our Clients
DDE   * each time a piece of important data changes.

                        if AdviseOK(si)
                            perform AdviseData
                        end-if
                    end-if

		when	WM-PAINT
		    call OS2API '__WinBeginPaint'
				using by value hwnd
				      by value 0 size 4
				      by reference rcl
                                returning hps

                    call OS2API '__WinQueryWindowRect'
                                using by value hwnd
                                      by reference rcl

                    call OS2API '__WinFillRect'
                                using by value hps
                                      by reference rcl
                                      by value SYSCLR-WINDOW

                    divide xRight by 2 giving xLeft
                    divide yTop by 2 giving yBottom
                    call OS2API '__WinFillRect'
                                using by value hps
                                      by reference rcl
                                      by value RectColour

                    call OS2API '__WinQueryWindowRect'
                                using by value hwnd
                                      by reference rcl

                    move 18 to yTop

                    call OS2API '__WinDrawText'
                                using by value hps
                                      by value DDEMsgSize size 2
                                      by reference DDEMsg
                                      by reference rcl
                                      by value SYSCLR-WINDOWTEXT size 4
                                      by value SYSCLR-WINDOW     size 4
                                      by value h'8500' size 2

		    call OS2API '__WinEndPaint'
                                using by value hps

		when other
		    call OS2API '__WinDefWindowProc'
				using by value hwnd
				      by value msg
				      by value mp1
				      by value mp2
				returning mresult

	    end-evaluate

            exit program returning mresult.

        GetSliderPos section.
            call OS2API '__WinSendMsg' using
                        by value AdviseHwnd(si)
                        by value SBM-QUERYPOS size 2
                        by value 0 size 4
                        by value 0 size 4
                        returning i.

        AdviseGetData section.
            perform GetSliderPos
            move i to DDEReply9.

DDE   * When some data has changed, we tell DDEMgr of this fact, and
DDE   * he will administer getting this information (via a callback function
DDE   * XTYP-ADVREQ) and passing it on to interested clients.

        AdviseData section.
            call OS2API '__DDEMgrPostAdvise' using
                        by value hszAdviseTopic(si)
                        by value hszAdviseItem(si).

        GetHszs Section.
DDE   * Get hszs for Strings:
            call OS2API '__DDEMgrGetHsz'
                        using by reference TargetApp
                        returning hszTargetApp
            call OS2API '__DDEMgrGetHsz'
                        using by reference TargetTopic
                        returning hszTargetTopic
            call OS2API '__DDEMgrGetHsz'
                        using by reference DDEL-System
                        returning hszSystem
            call OS2API '__DDEMgrGetHsz'
                        using by reference DDEL-Topics
                        returning hszTopics
            call OS2API '__DDEMgrGetHsz'
                        using by reference DDEL-SysItems
                        returning hszSysItems
            call OS2API '__DDEMgrGetHsz'
                        using by reference DDEL-Status
                        returning hszStatus
            call OS2API '__DDEMgrGetHsz'
                        using by reference DDETopic
                        returning hszDDETopic.

        FreeHszs Section.
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszDDETopic
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszStatus
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszSysItems
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszTopics
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszSystem
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszTargetTopic
            call OS2API '__DDEMgrFreeHsz' using
                        by value hszTargetApp.

DDE   * Initializing a conversation involves simply connecting with a
DDE   * topic on a target machine.  The returned Conversation handle is
DDE   * used for all communications.

        InitConv section.
            if hConversation = NULL
                call OS2API '__DDEMgrConnect' using
                             by value hszTargetApp
                                      hszTargetTopic
                                      0 size 4
                            returning hConversation

                if hConversation = NULL

DDE   * DDEMgrGetLastError returns the last noted error,
DDE   * DDEMgrPostError will put up a message box indicating the error

                    call OS2API '__DDEMgrGetLastError'
                                 returning errorCode
                    call OS2API '__DDEMgrPostError'
                                 using by value errorCode

                    move 'DDE Conversation Not Established' to DDEMsg
                else
                    move 'DDE Conversation Established' to DDEMsg
                end-if
                perform Refresh
            end-if.

        BeginConversation Section.
            perform InitConv

DDE   * With a conversation established, we can start talking...
      * The first thing we do is to EXECute a command in Excel.  This
      * actually puts the value "Red" in cell A1 of Sheet1.
      * EXEC is a once only, one way function.

            move Data1 to DataXfer
            move Item1 to ItemXfer
            move XTYP-EXEC to xtyp
            perform DDEXfer

      * We then set up an Advise Loop on Sheet1!R1C1 so that we are notified
      * each time it changes.
DDE   * The actual notification is received in the call back function.

            move Data2 to DataXfer
            move Item2 to ItemXfer
            move XTYP-ADVSTART to xtyp
            perform DDEXfer.

DDE   * To converse with a Conversation handle, we:
DDE   *     Convert the conversation Item to a hsz
DDE   *     Call DDEMgrClientXfer with parameters:
DDE   *         Data to be sent to the Server
DDE   *         Length of the data being sent
DDE   *         Conversation handle
DDE   *         hsz for the item
DDE   *         Format indicator (here we always use TEXT)
DDE   *         Communications type (see XTYP above and below)
DDE   *         timeout value (in milli-seconds)
DDE   *         a value used for assynchronous communications
DDE   *                 (see DDEMgr documentation for details)
DDE   *     It returns a handle to some data.

        DDEXfer section.
            if hConversation not = NULL
                move 1 to i
                inspect DataXfer tallying i for characters
                        before initial x"00"

                call OS2API '__DDEMgrGetHsz'
                            using by reference ItemXfer
                            returning hszItemName
                call OS2API '__DDEMgrClientXfer' using
                            by reference DataXfer
                            by value i
                            by value hConversation
                            by value hszItemName
                            by value DDEFMT-TEXT size 2
                            by value xtyp
                            by value 5000 size 4
                            by reference pidXfer
                        returning hDmgConvData

                call OS2API '__DDEMgrGetLastError'
                        returning errorCode
                call OS2API '__DDEMgrPostError'
                             using by value errorCode

                move spaces to DDEData

DDE   * If we REQUESTed some data from the Server, the hDmgConvData parameter
DDE   * contains that data.  Use DDEMgrGetData to retrieve the data, and
DDE   * DDEMgrFreeData to release the memory used.
DDE   * (Allocating and free resources used by DDEMgr should be done
DDE   * carefully.  This application should free everything it uses and you
DDE   * should be safe to copy it, but more sophisticated applications
DDE   * should refer to the DDEMgr documentation.

                if xtyp = XTYP-REQUEST

                    call OS2API '__DDEMgrGetData' using
                                by value hDmgConvData
                                by reference DDEData
                                by value 255 size 4
                                by value 0 size 4

                    call OS2API '__DDEMgrGetLastError'
                            returning errorCode
                    call OS2API '__DDEMgrPostError'
                                 using by value errorCode
                    call OS2API '__DDEMgrFreeData' using
                                by value hDmgConvData

                end-if

                call OS2API '__DDEMgrFreeHsz'
                            using by value hszItemName
            end-if.

DDE   * Terminating a conversation is merely a case of Disconnecting.

        TermConv section.
            if hConversation not = NULL
                call OS2API '__DDEMgrDisconnect'
                            using by value hConversation
                            returning errorCode

                set hConversation to NULL

                call OS2API '__DDEMgrGetLastError'
                        returning errorCode
            end-if.

        AdviseSelect section.
            move 0 to si
            evaluate hszItemString
                when 'Number1'
                    move 1 to si
                when 'Number2'
                    move 2 to si
                when 'Number3'
                    move 3 to si
                when other
            end-evaluate.

        AdviseRequest section.
            perform varying si from 3 by -1
                    until si = 0
                        or (  hszItem = hszAdviseItem(si) and
                             hszTopic = hszAdviseTopic(si)
                           )
            end-perform
            if si not = 0
                perform AdviseGetData
                move length of DDEReply9 to i
                add 1 to i
                move DDEReply9 to DDEReply
            end-if.

        Refresh Section.
            call OS2API '__WinInvalidateRect'
                        using by value hwndClient
                              by value 0 size 4
                              by value 0 size 2.

DDE   * Callback function from DDEManager.

DDE   * When the DDE Manager needs some information from the application
DDE   * (Client or Server) it calls the registered callback function.

        DDEXferProc Section.
        ENTRY 'DDEXFER' using by value      hConv
                              by value      hszTopic
                              by value      hszItem
                              by value      usFmt
                              by value      usType
                              by value      hDmgData.

            move 0 to DDEResult
            move all x'00' to ConversationData

            call OS2API '__DDEMgrGetHszString' using
                         by value hszTopic
                         by reference hszTopicString
                         by value lszTopicString

            call OS2API '__DDEMgrGetHszString' using
                         by value hszItem
                         by reference hszItemString
                         by value lszItemString

            inspect ConversationData replacing all x"00" by space

            move spaces to DDEReply
            move 1 to i

DDE   * Any of the XTYP functions could be seen here, but we only need
DDE   * to process some of them.

            evaluate usType

DDE   * XTYP-INIT
DDE   *     When another application wants to start a conversation with
DDE   *     this application, DDEMgr uses the XTYP-INIT function to
DDE   *     check whether the desired topic is supported by this
DDE   *     application.
DDE   *     The applciation should return True (1) if it does support
DDE   *     the topic, and False (0) otherwise.

DDE   *     DDE applications should support the System topic in addition
DDE   *     to working topics.

              when XTYP-INIT
                if hszTopic = hszSystem or hszDDETopic
                    exit program returning 1

DDE   * XTYP-WILDINIT
DDE   *     Wild init should return a list of support topics as a string
DDE   *     of hszs.  If a large number of topics are to be supported,
DDE   *     There are advantages in coding a table.

DDE   * Throughout this section we are using i to tally the length
DDE   * of the data collected for return to DDEMgr.

              when XTYP-WILDINIT
                call OS2API '__DDEMgrGetHsz' using
                            by reference 'System' & x"00"
                            returning DDEReply(i:2)
                add 2 to i
                call OS2API '__DDEMgrGetHsz' using
                            by reference DDETopic
                            returning DDEReply(i:2)
                add 2 to i

DDE   * XTYP-REGISTER is received when another application becomes
DDE   * available for DDE transactions.
DDE   * We use this as notification to start a conversation.

              when XTYP-REGISTER
                if hszItem = hszTargetApp
                    call OS2API '__WinSendMsg' using
                                by value hwndClient
                                by value WMUSER-INITDDE size 2
                                by value 0 size 4
                                by value 0 size 4
                end-if

DDE   * XTYP-UNREGISTER is received when an application becomes
DDE   * unavailable for DDE transactions
DDE   * We use this as notification to terminate a conversation.

              when XTYP-UNREGISTER
                if hszItem = hszTargetApp
                    set hConversation to null
                    call OS2API '__WinSendMsg' using
                                by value hwndClient
                                by value WMUSER-FREEHSZ size 2
                                by value 0 size 4
                                by value 0 size 4
                end-if

DDE   * XTYP-REQUEST and XTYP-ADVREQ
DDE   * are the same in function, but different in origin.
DDE   * XTYP-REQUEST is the result of a XTYP-REQUEST conversation started
DDE   * by some client
DDE   * XTYP-ADVREQ is the result of a call to DDEMgrPostAdvise.
DDE   * In both cases, what is required is the value of some pieve of
DDE   * data.

              when XTYP-REQUEST
              when XTYP-ADVREQ
                evaluate hszTopic
                  when hszSystem
DDE   * The System Topic should support the following Items:
DDE   *     Topics - return a list of all supported topics
DDE   *     SysItems - return a list of all supported Items for the System
DDE   *                         Topic
DDE   *     Status - return a Status indicator.
DDE   *     Multiple values are returned with TAB separators.

                    evaluate hszItem
                      when hszTopics
                        string 'System' & x"09"
                               L-TESTDATA
                            delimited by size
                            into DDEReply pointer i
                      when hszSysItems
                        string 'Topics'   & x"09"
                               'SysItems' & x"09"
                               'Status'
                            delimited by size
                            into DDEReply pointer i
                      when hszStatus
                        string 'OK' delimited by size
                          into DDEReply pointer i
                      when other
                    end-evaluate
                  when other
DDE   * If we allowed multiple topics here, we would have to do more
DDE   * work, but we take the easy option.

                    perform AdviseRequest
                end-evaluate

DDE   * When a piece of data changes after a Client has requested notifaction
DDE   * of this fact, a XTYP-ADVDATA message is received by the Client.

DDE   * We check that it is an expected piece of data, and then extrat
DDE   * it from the hDmgData (which we free up)
DDE   * Excel tacks on a CR LF NULL sequence to the end of the data, so
DDE   * we strip these off, and then process the data.

              when XTYP-ADVDATA

      * Topic and Item are reversed here...
                if hszTopicString = L-TARGETTOPIC and
                   hszItemString  = L-TARGETCELL
                    move all x'00' to DDEReply
                    call OS2API '__DDEMgrGetData' using
                                by value hDmgData
                                by reference DDEReply
                                by value 40 size 4
                                by value 0 size 4
                    call OS2API '__DDEMgrFreeData' using
                                by value hDmgData
                    inspect DDEReply replacing all x"00" by spaces
                    inspect DDEReply replacing first x"0d" by space
                    inspect DDEReply replacing first x"0a" by space
                    evaluate DDEReply
                      when 'White'
                        move CLR-WHITE to RectColour
                      when 'Black'
                        move CLR-BLACK to RectColour
                      when 'Blue'
                        move CLR-BLUE to RectColour
                      when 'Red'
                        move CLR-RED to RectColour
                      when 'Magenta'
                        move CLR-MAGENTA to RectColour
                      when 'Green'
                        move CLR-GREEN to RectColour
                      when 'Cyan'
                        move CLR-CYAN to RectColour
                      when 'Yellow'
                        move CLR-YELLOW to RectColour
                    end-evaluate
                    perform Refresh
                    move spaces to DDEReply

DDE   * We also return an Acknowledge signal to the DDEMgr

                    move DDE-FACK to DDEResult
                end-if


DDE   * When a Client requests a server to advise when some data changes,
DDE   * the Server receives a XTYP-ADVSTART function.
DDE   * The server should note the request (here we save the hszs for
DDE   * use when we need to PostAdvise) and return

              when XTYP-ADVSTART
                if usFmt = DDEFMT-TEXT
                    perform AdviseSelect
                    if si >= 1 and si <= 3
                        set AdviseOK(si) to TRUE
                        call OS2API '__DDEMgrIncHszCount' using
                                    by value hszTopic
                        set hszAdviseTopic(si) to hszTopic
                        call OS2API '__DDEMgrIncHszCount' using
                                    by value hszItem
                        set hszAdviseItem(si) to hszItem
                        move 1 to DDEResult
                    end-if
                end-if

DDE   * Similarly, when a Client no longer needs a piece of data, it sends
DDE   * XTYP-ADVSTOP message.

              when XTYP-ADVSTOP
                perform AdviseSelect
                if si >= 1 and si <= 3
                    set AdviseOff(si) to TRUE
                    call OS2API '__DDEMgrFreeHsz' using
                                by value hszAdviseTopic(si)
                    call OS2API '__DDEMgrFreeHsz' using
                                by value hszAdviseItem(si)
                    move 1 to DDEResult
                end-if

              when other
            end-evaluate

DDE   * If there is data to be returned to DDEMgr, we do it by
DDE   * wrapping it in a data structure with DDEMgrPutData.
DDE   * Parameters to DDEMgrPutData are:
DDE   *     Data to pack
DDE   *     length of data
DDE   *     offset within return data structure to place this data
DDE   *     hsz for the Item this data refers to
DDE   *     format of this data
DDE   *     flag - see DDEMGR documentation for details.
DDE   * It returns a handle which should be returned in the EXIT PROGRAM
DDE   * statement.

            if DDEReply not = spaces

                move x"00" to DDEReply(i:1)

                call OS2API '__DDEMgrPutData' using
                            by reference    DDEReply
                            by value        i
                            by value        0 size 4
                            by value        hszItem
                            by value        DDEFMT-TEXT size 2
                            by value        0 size 2
                            returning       DDEResult

                call OS2API '__DDEMgrGetLastError'
                        returning errorCode

            end-if

            exit program returning DDEResult.
