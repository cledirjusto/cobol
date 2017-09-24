      $set ans85 mf noosvs
      ****************************************************************
      *
      *                (C) Micro Focus Ltd. 1989,1990
      *
      *                        PMCALC.CBL
      *
      * Example program: Presentation Manager 'RPN Calculator'
      *
      * Note: Refer to your printed and on-disk documentation for more
      *       information on PM Programming.
      *
      * A number of extensions to the COBOL language are used in
      * this program - see the comments in PMHELLO.CBL for details
      *
      * Application files:
      *     PMCALC.CBL      -   COBOL source file
      *     PMCALC.RES      -   Resources, created from :
      *        PMCALC.PTR   -   Hand icon for Area Sensitive Pointer
      *        PMCALC.ICO   -   Minimize Icon
      *        PMCALC.RC    -   Menu and Accelerate Table definition
      *        PMCALC.DLG   -   Dialog definition
      *
      * Tools used:
      *     COBOL           -   COBOL compiler      .CBL
      *     LINK            -   Linkage editor
      *     RC              -   Resource compiler   .RC .DLG
      *     ICONEDIT        -   Icon editor creates .ICO and .PTR
      *
      * To compile, link, resource-compile, and run:
      *
      *    cobol pmcalc target(286) linklib"coblib+os2";
      *    link pmcalc,,,coblib+os2,pmcalc.def/nod;
      *    rc pmcalc.res pmcalc.exe
      *    pmcalc
      *
      *    The LINKLIB directive is not needed for the compile if you
      *    have it in your COBOL.DIR. Note that OS2.LIB is needed for
      *    this program. This file is available in the Software
      *    Developer's Toolkit and in the Microsoft Utilities.
      *
      *
      * PMCALC.DEF (the definitions file) contains the following:
      *
      *    NAME    PMCALC  WINDOWAPI
      *    PROTMODE
      *    STACKSIZE       16384
      *    EXPORTS
      *        MWndProc    @1
      *        BWndProc    @2
      *        DlgProc     @3
      *
      * This example program uses PM to implement:
      *     A Standard Window with a Client Window Procedure
      *     Action Bar and pull downs (CUA conforming)
      *     Accelerator Keys
      *     Sending and Receiving messages
      *     Child Windows
      *     Subclassing
      *     WindowWords
      *     An Area Sensitive Pointer (The pointer changes shape
      *                                when located over a
      *                                'pressable' button)
      *     Dialog Boxes
      *     Message Boxes
      *     Automatic window repositioning/resizing
      *
      * The OS2 calls used are:
      *
      *     DosGetModHandle     :   Get handle of this module
      *     WinInitialize       :   Initialize PM for this thread
      *     WinTerminate        :   Terminate PM
      *     WinCreateMsgQueue   :   Create message queue
      *     WinDestroyMsgQueue  :   Destroy message queue
      *     WinRegisterClass    :   Register window class
      *     WinSubclassWindow   :   Create a subclass
      *     WinLoadPointer      :   Load .PTR
      *     WinCreateStdWindow  :   Create standard window
      *     WinCreateWindow     :   Create child window
      *     WinDestroyWindow    :   Destroy window
      *     WinGetMsg           :   Get message from message queue
      *     WinDispatchMsg      :   Dispatch message to WinProc
      *     WinSendMsg          :   Synchronous message
      *     WinPostMsg          :   Asynchronous message
      *     WinBeginPaint       :
      *     WinEndPaint         :
      *     WinMessageBox       :   Initiate Message Box
      *     WinDlgBox           :   Initiate Dialog Box
      *     WinDismissDlg       :   Terminate dialog box
      *     WinDefWindowProc    :   Default window proc
      *     WinDefDlgProc       :   Default dialogbox proc
      *     WinSetWindowULong   :   Set window words
      *     WinQueryWindowUShort:   Retrieve window words
      *     WinQueryWindowULong :   Retrieve window words
      *     WinSetWindowText    :   Assign text to child window
      *     WinSendDlgItemMsg   :   Send message to dialog box item
      *     WinSetDlgItemShort  :   Send number to dialog box item
      *     WinQueryDlgItemShort:   Get number from dialog box item
      *     WinSetWindowPos     :   Size and Position window
      *     DosBeep             :   Sound the bell
      *
      ****************************************************************

      ****************************************************************
      *
      *     Enable the PASCAL calling convention (number 3)
      *     and call it OS2API because it is used for OS2API
      *     functions.  (We will use it for COBOL to COBOL calls
      *     as well.) See PMHELLO source for more information on
      *     choosing a calling convention.
      *
      ****************************************************************
        special-names.
            call-convention 3 is OS2API.

        working-storage section.

      ****************************************************************
      *
      * The following are taken from the PM TOOLKIT Header files,
      * using the H2CPY conversion program. For more information on
      * the H2CPY utility, see the printed and online documentation
      * for PM Programming with COBOL.
      *
      ****************************************************************
        78  Wm-Create           	VALUE   H"01".
        78  Wm-Size             	VALUE   H"07".
        78  Wm-Command          	VALUE   H"20".
        78  Wm-Paint            	VALUE   H"23".
        78  Wm-Quit             	VALUE   H"2a".
        78  Wm-Initdlg          	VALUE   H"3b".
        78  Wm-EraseBackground  	VALUE   H"4f".
        78  Wm-MouseMove        	VALUE   H"70".
        78  Em-SetTextLimit     	VALUE   H"0143".

        78  Hwnd-Desktop            VALUE    1.
        78  Hwnd-Top                VALUE    3.
        78  Hwnd-Bottom             VALUE    4.
        78  Wc-Button               VALUE   H"ffff0003".
        78  Wc-Static               VALUE   H"ffff0005".
        78  Qws-Id                  VALUE   H"ffff".
        78  Qwl-User                VALUE    0.
        78  Did-Ok                  VALUE    1.
        78  Did-Cancel              VALUE    2.

      ****************************************************************
      *
      * SIZEMOVESHOW = h"0b" = Swp-Size with Swp-Move with Swp-Show
      *                         combined with logical OR
      *     This is used for WinSetWindowPos by PositionWindow as
      *     the attribute for the WindowPos information.
      *
      ****************************************************************
        78  SIZEMOVESHOW        value   h"0b".

        01  work-data.
            03  hab                 pointer.
            03  hmq                 pointer.
            03  hwndClient          pic 9(9) comp-5.
            03  hwndFrame           pic 9(9) comp-5.
            03  hwndTemp            pic 9(9) comp-5.
            03  hwndMenu            pic 9(9) comp-5.
            03  qmsg.
                05  qmsghwnd        pic 9(9) comp-5.
                05  qmsgmsg         pic 9(4) comp-5.
                05  qmsgmp1         pic 9(9) comp-5.
                05  qmsgmp2         pic 9(9) comp-5.
                05  qmsgtime        pic 9(9) comp-5.
                05  qmsgptl.
                    07  qmsgptlx    pic 9(9) comp-5.
                    07  qmsgptly    pic 9(9) comp-5.

            03  ptrH                pointer.
            03  pidInfo.
                05  pid             pic xx comp-5.
                05  tid             pic xx comp-5.
                05  ppid            pic xx comp-5.

            03  loop-flag           pic x value 'C'.
                88  loop-end            value 'E'.
            03  bool                pic 9(4) comp-5.
                88  boolTRUE            value 1.
                88  boolFALSE           value 0.


      ****************************************************************
      *
      * hwndTable contains details of each of the windows used in
      *         this application.
      *         The fields are:
      *
      *     SquareX:    number of buttons horizontally
      *     SquareY:    number of buttons vertically
      *
      *     id*     The offset into the array of this item,
      *             this is used to calculate the ID and WM for
      *             each button.
      *     hwnd*   Storage for the window handle.  This is only
      *             used for windows with variable contents
      *             (Display and Base in this example.)
      *     text*   The text to put on the button
      *     winXL   The left horizontal position  (in button co-ords)
      *     winYB   The bottom vertical position  (in button co-ords)
      *     winXR   The right horizontal position (in button co-ords)
      *     winYT   The top vertical position     (in button co-ords)
      *
      *               [base][   number display  ]
      *               [Hex] [+/-] [D] [E] [F] [/]
      *               [Dec] [Clx] [A] [B] [C] [*]
      *               [Oct] [x-y] [7] [8] [9] [-]
      *               [Bin]  [^]  [4] [5] [6] [+]
      *               [1/x]  [V]  [1] [2] [3] [Ent]
      *               [y^x] [Clr] [  0  ] [.] [er]
      *
      ****************************************************************
            03  hwndTable-x.
        78  hwndTS          value next.

        78  RelativeButton  value 3.
        78  RelativeGap     value 1.
        78  ButtonGap       value RelativeButton + RelativeGap.
        78  SquareX         value 6.
        78  SquareY         value 7.

        78  idDisplay       value 1.
                05  hwndDisplay     pic 9(9) comp-5.
                05  textDisplay     pic x(5).
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 6.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 6.
        78  id0             value 2.
                05  hwnd0           pic 9(9) comp-5.
                05  text0           pic x(5) value '0'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 0.
        78  id1             value 3.
                05  hwnd1           pic 9(9) comp-5.
                05  text1           pic x(5) value '1'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 1.
        78  id2             value 4.
                05  hwnd2           pic 9(9) comp-5.
                05  text2           pic x(5) value '2'.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 1.
        78  id3             value 5.
                05  hwnd3           pic 9(9) comp-5.
                05  text3           pic x(5) value '3'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 1.
        78  id4             value 6.
                05  hwnd4           pic 9(9) comp-5.
                05  text4           pic x(5) value '4'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 2.
        78  id5             value 7.
                05  hwnd5           pic 9(9) comp-5.
                05  text5           pic x(5) value '5'.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 2.
        78  id6             value 8.
                05  hwnd6           pic 9(9) comp-5.
                05  text6           pic x(5) value '6'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 2.
        78  id7             value 9.
                05  hwnd7           pic 9(9) comp-5.
                05  text7           pic x(5) value '7'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 3.
        78  id8             value 10.
                05  hwnd8           pic 9(9) comp-5.
                05  text8           pic x(5) value '8'.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 3.
        78  id9             value 11.
                05  hwnd9           pic 9(9) comp-5.
                05  text9           pic x(5) value '9'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 3.
        78  idA             value 12.
                05  hwndA           pic 9(9) comp-5.
                05  textA           pic x(5) value 'A'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 4.
        78  idB             value 13.
                05  hwndB           pic 9(9) comp-5.
                05  textB           pic x(5) value 'B'.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 4.
        78  idC             value 14.
                05  hwndC           pic 9(9) comp-5.
                05  textC           pic x(5) value 'C'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 4.
        78  idD             value 15.
                05  hwndD           pic 9(9) comp-5.
                05  textD           pic x(5) value 'D'.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 5.
        78  idE             value 16.
                05  hwndE           pic 9(9) comp-5.
                05  textE           pic x(5) value 'E'.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 5.
        78  idF             value 17.
                05  hwndF           pic 9(9) comp-5.
                05  textF           pic x(5) value 'F'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 5.
        78  idDP            value 18.
                05  hwndDP          pic 9(9) comp-5.
                05  textDP          pic x(5) value '.'.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 0.
        78  idChs           value 19.
                05  hwndChs         pic 9(9) comp-5.
                05  textChs         pic x(5) value '+/-'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 5.
        78  idPlus          value 20.
                05  hwndPlus        pic 9(9) comp-5.
                05  textPlus        pic x(5) value '+'.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 2.
        78  idMinus         value 21.
                05  hwndMinus       pic 9(9) comp-5.
                05  textMinus       pic x(5) value '-'.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 3.
        78  idMultiply      value 22.
                05  hwndMultiply    pic 9(9) comp-5.
                05  textMultiply    pic x(5) value 'x'.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 4.
        78  idDivide        value 23.
                05  hwndDivide      pic 9(9) comp-5.
                05  textDivide      pic x(5) value '/'.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 5.
        78  idEnter         value 24.
                05  hwndEnter       pic 9(9) comp-5.
                05  textEnter       pic x(5) value 'Enter'.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 1.
        78  idRollDown      value 25.
                05  hwndRollDown    pic 9(9) comp-5.
                05  textRollDown    pic x(5) value x'5219'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 0.
        78  idRollUp        value 26.
                05  hwndRollUp      pic 9(9) comp-5.
                05  textRollUp      pic x(5) value x'5218'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 1.
        78  idClear         value 27.
                05  hwndClear       pic 9(9) comp-5.
                05  textClear       pic x(5) value 'Clear'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 4.
        78  idClx           value 28.
                05  hwndClx         pic 9(9) comp-5.
                05  textClx         pic x(5) value 'Clr-X'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 3.
        78  idHex           value 29.
                05  hwndHex         pic 9(9) comp-5.
                05  textHex         pic x(5) value x'1a486578'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 5.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 5.
        78  idDec           value 30.
                05  hwndDec         pic 9(9) comp-5.
                05  textDec         pic x(5) value x'1a446563'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 4.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 4.
        78  idOct           value 31.
                05  hwndOct         pic 9(9) comp-5.
                05  textOct         pic x(5) value x'1a4f6374'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 3.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 3.
        78  idBin           value 32.
                05  hwndBin         pic 9(9) comp-5.
                05  textBin         pic x(5) value x'1a42696e'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 2.
        78  idPower         value 33.
                05  hwndPower       pic 9(9) comp-5.
                05  textPower       pic x(5) value 'y^x'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 0.
        78  idRecip         value 34.
                05  hwndRecip       pic 9(9) comp-5.
                05  textRecip       pic x(5) value '1/x'.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 1.
        78  idXchg          value 35.
                05  hwndXchg        pic 9(9) comp-5.
                05  textXchg        pic x(5) value x'78111079'.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 2.
                05  filler          pic xx   comp-5 value 1.
                05  filler          pic xx   comp-5 value 2.
        78  idBase          value 36.
                05  hwndBase        pic 9(9) comp-5.
                05  textBase        pic x(5).
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 6.
                05  filler          pic xx   comp-5 value 0.
                05  filler          pic xx   comp-5 value 6.

        78  hwndTableEnd    value next.
        78  hwndTableSize   value hwndTableEnd
                                        - hwndTS.
        78  hwndTableCount  value hwndTableSize / 17.
        78  hwndButtonLast  value hwndTableCount - 1.

            03  hwndTable redefines hwndTable-x.
              04  winItem         occurs hwndTableCount.
                05  hwndItem        pic 9(9) comp-5.
                05  hwndText        pic x(5).
                05  winXL           pic xx comp-5.
                05  winYB           pic xx comp-5.
                05  winXR           pic xx comp-5.
                05  winYT           pic xx comp-5.

        78  Wm-User                 VALUE   H"1000".
        78  WM-UPDATEDISPLAY        value   h"1001".
        78  WM-NUMBER               value   h"1002".
        78  WM-ABOUT                value   h"100d".
        78  WM-CUSTOM               value   h"100e".
        78  WM-EXIT                 value   h"100f".
        78  WM-BUTTONS              value   h"1010".
        78  WM-0                    value   WM-BUTTONS + id0.
        78  WM-1                    value   WM-BUTTONS + id1.
        78  WM-2                    value   WM-BUTTONS + id2.
        78  WM-3                    value   WM-BUTTONS + id3.
        78  WM-4                    value   WM-BUTTONS + id4.
        78  WM-5                    value   WM-BUTTONS + id5.
        78  WM-6                    value   WM-BUTTONS + id6.
        78  WM-7                    value   WM-BUTTONS + id7.
        78  WM-8                    value   WM-BUTTONS + id8.
        78  WM-9                    value   WM-BUTTONS + id9.
        78  WM-A                    value   WM-BUTTONS + idA.
        78  WM-B                    value   WM-BUTTONS + idB.
        78  WM-C                    value   WM-BUTTONS + idC.
        78  WM-D                    value   WM-BUTTONS + idD.
        78  WM-E                    value   WM-BUTTONS + idE.
        78  WM-F                    value   WM-BUTTONS + idF.
        78  WM-DP                   value   WM-BUTTONS + idDP.
        78  WM-CHS                  value   WM-BUTTONS + idCHS.
        78  WM-PLUS                 value   WM-BUTTONS + idPlus.
        78  WM-MINUS                value   WM-BUTTONS + idMinus.
        78  WM-MULTIPLY             value   WM-BUTTONS + idMultiply.
        78  WM-DIVIDE               value   WM-BUTTONS + idDivide.
        78  WM-ENTER                value   WM-BUTTONS + idEnter.
        78  WM-ROLLDOWN             value   WM-BUTTONS + idRollDown.
        78  WM-ROLLUP               value   WM-BUTTONS + idRollUp.
        78  WM-CLEAR                value   WM-BUTTONS + idClear.
        78  WM-CLX                  value   WM-BUTTONS + idClx.
        78  WM-HEX                  value   WM-BUTTONS + idHex.
        78  WM-DEC                  value   WM-BUTTONS + idDec.
        78  WM-OCT                  value   WM-BUTTONS + idOct.
        78  WM-BIN                  value   WM-BUTTONS + idBin.
        78  WM-POWER                value   WM-BUTTONS + idPower.
        78  WM-RECIP                value   WM-BUTTONS + idRecip.
        78  WM-XCHG                 value   WM-BUTTONS + idXchg.

        78  DI-BASEENTRY            value   3.
        78  DI-BASE                 value   4.


      ****************************************************************
      *
      * Class styles are defined in the header files. To combine
      * class values into single data names, we have done some
      * logical operations on data values in the COPY file created
      * from the C header files after the H2CPY run.
      *
      *           CSClass     = h"00000004"   is Cs-Sizeredraw
      *                                not  with Cs-Clipchildren
      *           DisplayStyle= h"80000601"   is Ss-Text
      *                                     with Dt-Right
      *                                     with Dt-Vcenter
      *                                     with Ws-Visible
      *           BaseStyle   = h"80000501"   is Ss-Text
      *                                     with Dt-Center
      *                                     with Dt-Vcenter
      *                                     with Ws-Visible
      *           ButtonStyle = h"80000000"   is Ws-Visible
      *           WSWindow    = h"80000000"   is Ws-Visible
      *           Fcf-ctl-data= h"0000cc3f"   is Fcf-Tasklist
      *                                     with Fcf-Shellposition
      *                                      and Fcf-Minmax
      *                                      and Fcf-Sizeborder
      *                                      and Fcf-Sysmenu
      *                                      and Fcf-Titlebar
      *                                      and Fcf-Menu
      *                                      and Fcf-Acceltable
      *                                      and Fcf-Icon
      *
      ****************************************************************
            03  CSClass             pic 9(9) comp-5 value h"00000004".
            03  DisplayStyle        pic 9(9) comp-5 value h"80000601".
            03  BaseStyle           pic 9(9) comp-5 value h"80000501".
            03  ButtonStyle         pic 9(9) comp-5 value h"80000000".
            03  WSWindow            pic 9(9) comp-5 value h"80000000".
            03  Fcf-ctl-data        pic 9(9) comp-5 value h"0000cc3f".

      ****************************************************************
      *
      * ASCIIZ strings to pass to PM.
      *
      ****************************************************************
            03  MyClass             pic x(8) value 'RPNCalc' & x"00".
            03  AboutText           pic x(131) value
                    'This application is written in COBOL.'
                &   ' For more information on writing for PM in COBOL,'
                &   ' see the PMHELLO.CBL example program source.'
                &   x"00".
            03  AboutTitle          pic x(15) value
                   'RPN Calculator' & x"00".

      ****************************************************************
      *
      * Internal registers - we use COMP-3 for efficiency with
      *     very awkward numbers (18 9s in this case)
      *
      *     CalcRegisters is the set of registers
      *     CalcRegister  is a table of the individual registers
      *     Reg           is the value area
      *     RegDP         is the Fraction pointer for entering
      *                             non-integers
      *     modeFlag      indicates what last happened:
      *                             ENTER key
      *                             Number key (or DP)
      *                             Function key
      *
      *     RegInt      )
      *     RegFrac     ) are work areas
      *     RegWork     )
      *     Digit9/X      are used for creating ASCII digits
      *
      *     gBase         is the current (global) base
      *     gBaseD        is the maximum sensible digits in fraction
      *     gBase9        is used for displaying the base
      *
      *     RegSE         is the Size Error indicator
      *
      *     CalcRegisterDisplay is used for displaying a register.
      *
      ****************************************************************
            03  CalcRegisters.
              04  CalcRegister occurs 7.
                05  Reg                 pic s9(9)v9(9) comp-3.
        78  RegX                value 1.
        78  RegY                value 2.
        78  RegZ                value 3.
        78  RegT                value 4.
        78  RegLX               value 5.
            03  RegDP                   pic xx comp-5.
            03  modeFlag                pic x.
                88  modeEnter           value 'E'.
                88  modeFunction        value 'F'.
                88  modeNumber          value 'N'.
            03  RegInt                  pic 9(9) comp-3.
            03  RegFrac                 pic v9(9) comp-3.
            03  RegWork                 pic s9(9)v9(9) comp-3.
            03  Digit9                  pic x  comp-5.
            03  DigitX redefines Digit9 pic x.
            03  gBase                   pic xx comp-5.
            03  gBaseSav                pic xx comp-5.
            03  gBaseD                  pic xx comp-5.
        78  HexgBaseD           value 8.
        78  DecgBaseD           value 9.
        78  OctgBaseD           value 10.
        78  BingBaseD           value 30.
            03  gBase9bra               pic x value '('.
            03  gBase9                  pic z9.
            03  gBase9ket               pic x value ')'.
            03  gBase9Null              pic x value x'00'.
            03  RegSE                   pic x.
                88  SizeError           value 'E'.
                88  NoSizeError         value ' '.

            03  CalcRegisterDisplay.
                05  CalcRegX        pic x(50).
                05                  pic xxxx    value x'20202000'.
                05  CalcRegBase     pic x(70).
                05  CalcReg9 redefines CalcRegBase
                                    pic -(9)9.9(9).

            03  ModHandle       pic xx.
            03  ModName         pic x(7) value "PMCALC" & x"00".

        local-storage section.
        01  hps                 pointer.
        01  Rectl.
            03  Rectl-xLeft           pic S9(9) comp-5.
            03  Rectl-yBottom         pic S9(9) comp-5.
            03  Rectl-xRight          pic S9(9) comp-5.
            03  Rectl-yTop            pic S9(9) comp-5.
        01  rcs.
            03  sxLeft          pic x(2) comp-5.
            03  syBottom        pic x(2) comp-5.
            03  sxRight         pic x(2) comp-5.
            03  syTop           pic x(2) comp-5.

        01  ppl.
            03  x               pic x(4) comp-5.
            03  y               pic x(4) comp-5.
        01  mresult             pic x(4) comp-5.

        01  workarea.
            03  i               pic xx   comp-5.
            03  j               pic xx   comp-5.
            03  cx              pic xx   comp-5.
            03  cy              pic xx   comp-5.
            03  hwndWork        pic xxxx comp-5.
            03  str             pic x(8).
            03  WndProc         procedure-pointer.

        linkage section.
        01  hwnd                pic xxxx comp-5.
        01  msg                 pic xx   comp-5.
        01  mp1                 pic xxxx comp-5.
        01  redefines mp1.
            03  mp1w1           pic xx   comp-5.
            03  mp1w2           pic xx   comp-5.
        01  Style redefines mp1 pic xxxx comp-5.
        01  mp2                 pic xxxx comp-5.
        01  redefines mp2.
            03  mp2w1           pic xx   comp-5.
            03  mp2w2           pic xx   comp-5.
        01  Basex               pic xx   comp-5.
        01  Basey               pic xx   comp-5.
        01  Sizex               pic xx   comp-5.
        01  Sizey               pic xx   comp-5.
        01  PointSize.
            03  psx             pic xxxx comp-5.
            03  psy             pic xxxx comp-5.
        01  Text1               pic x(20).

        procedure division OS2API.
        main section.

            call OS2API '__DosGetModHandle'
                        using           ModName
                                        ModHandle
            if return-code not = 0
                move all x"00" to ModHandle
            end-if

            call OS2API '__WinInitialize'
                        using   by value 0 size 2
                        returning hab
            call OS2API '__WinCreateMsgQueue'
                        using by value hab
                              by value 0 size 2
                        returning hmq

            set WndProc to ENTRY 'MWndProc'
            call OS2API '__WinRegisterClass'
                        using by value     hab
                              by reference MyClass
                              by value     WndProc
                              by value     CSClass
                              by value     0        size 2
                        returning bool
            if boolTRUE

                call OS2API '__WinLoadPointer'
                            using by value Hwnd-Desktop size 4
                                  by value ModHandle
                                  by value 2 size 2
                            returning ptrH

                call OS2API '__WinCreateStdWindow'
                            using by value     Hwnd-Desktop size 4
                                  by value     WSWindow
                                  by reference Fcf-ctl-data
                                  by reference MyClass
                                  by value     0        size 4
                                  by value     0        size 4
                                  by value     ModHandle
                                  by value     1        size 2
                                  by reference hwndClient
                            returning hwndFrame

                if hwndFrame not = 0

      ****************************************************************
      *
      * This in-line PERFORM implements the message loop.
      *
      ****************************************************************
                    perform until loop-end
                        call OS2API '__WinGetMsg'
                                    using by value hab
                                          by reference qmsg
                                          by value 0        size 4
                                          by value 0        size 2
                                          by value 0        size 2
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


      ****************************************************************
      *
      * Calculator's Window Procedure
      *
      ****************************************************************
        MyWndProc-S section.
        entry 'MWndProc' using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2.

            move 0 to mresult
            evaluate msg

      ****************************************************************
      *
      * We process the CREATE message
      *     We create User Buttons for each of the screen areas.
      *
      ****************************************************************
                when Wm-Create
                    call OS2API '__CreateDisplay'
                                using by value hwnd
                                      by value DisplayStyle
                                      by value idDisplay    size 2
                                returning hwndDisplay
                    call OS2API '__CreateDisplay'
                                using by value hwnd
                                      by value BaseStyle
                                      by value idBase       size 2
                                returning hwndBase

                    move id0 to i
                    perform until i > hwndButtonLast
                        call OS2API '__CreateButton'
                                    using by value hwnd
                                          by value i
                                    returning hwndTemp
                        move hwndTemp to hwndItem(i)
                        add 1 to i
                    end-perform
                    move 10 to gBase
                    move 0 to gBAseSav
                    set NoSizeError to true
                    call OS2API '__WinPostMsg'
                                using by value hwnd
                                      by value WM-CLEAR
                                      by value 0 size 4
                                      by value 0 size 4

      ****************************************************************
      *
      * Button Presses come to the Window through the Wm-Command
      *             message, with mp1w1 as the Button Id.
      *         This Button Id is set the same as the message
      *             number which processis it.
      *         So we simply generate a message...
      *         Size Error condition is removed by any button,
      *             which then has no effect.
      *
      ****************************************************************
                when    Wm-Command
                    if mp1w1 > WM-USER
                        if SizeError
                            set NoSizeError to true
                            perform UpdateD
                        else
                            call OS2API '__WinSendMsg'
                                        using by value hwnd
                                              by value mp1w1
                                              by value 0 size 4
                                              by value mp2
                        end-if
                    end-if

      ****************************************************************
      *
      * We process the SIZE message
      *     This involves positioning the individual buttons
      *     in the correct place.
      *
      *     mp2 contains the current size (w1 = x dimensions, w2 = y)
      *
      *     We partition the available space into SquareX by SquareY
      *         boxes, and pass these size chunks to PositionWindow
      *
      ****************************************************************
                when    Wm-Size
                    move mp2w1 to x
                    move mp2w2 to y
                    move SquareX to i
                    multiply ButtonGap by i
                    add RelativeGap to i
                    divide i into x
                    move SquareY to i
                    multiply ButtonGap by i
                    add RelativeGap to i
                    divide i into y

                    perform varying i from 1 by 1
                            until i > hwndTableCount

                        call OS2API '__PositionWindow'
                                    using by value hwndItem(i)
                                          by value winXL(i)
                                          by value winYB(i)
                                          by value winXR(i)
                                          by value winYT(i)
                                          by reference ppl

                    end-perform

      ****************************************************************
      *
      * We have to process the PAINT message
      * The sequence of actions is:
      *
      *     Get Handle-To-Presentation-Space (HPS) for painting
      *                         in the client window
      *     (Any drawing required could be put here...)
      *     Release the HPS.
      *
      *
      * WM-ERASEBACKGROUND is processed to enable the Frame to erase
      *     the backdrop of the ClientArea.
      *
      ****************************************************************
                when    Wm-Paint
                    call OS2API '__WinBeginPaint'
                                using by value hwnd
                                      by value 0 size 4
                                      by reference Rectl
                                returning hps

                    call OS2API '__WinEndPaint'
                                using by value hps

                when    Wm-EraseBackground
                    move 1 to mresult
                    exit program returning mresult


      ****************************************************************
      *
      * We process our User defined messages
      *
      ****************************************************************
      *
      * WM-UPDATEDISPLAY updates the display window with the
      *                  current RegX value in the correct base
      *
      *         We have to watch out for the Error condition
      *         We use COBOL number conversion for Base 10
      *                 and do it by hand for other bases.
      *                 Fractional parts are limited by space
      *                 or sensible significance (gBaseD)
      *         We only update the BASE window if required
      *
      ****************************************************************
                when    WM-UPDATEDISPLAY
                    if SizeError
                        move length of CalcRegX to j
                        subtract 5 from j
                        move 'Error' to CalcRegX(j:5)
                    else
                        move all '0' to CalcRegBase
                        if gBase = 10
      *     COBOL Edited fields are useful for Decimal...
                            move Reg(RegX) to CalcReg9
                            move length of CalcReg9 to i
                        else
      *     Except for Base 10, we have to do it the hard way.
                            move length of CalcRegBase to j
      *     Initialize the DP
                            divide 2 into j
                            move '.' to CalcRegBase(j:1)
                            subtract 1 from j giving i
                            add 1 to j
      *     Fill in the Integral part
                            move Reg(RegX) to RegInt
                            perform until RegInt = 0
                                divide gBase into RegInt
                                        giving RegInt
                                        remainder Digit9
                                perform EnterDigit
                                subtract 1 from i
                            end-perform
      *     If Integer is zero, we show the zero
                            if j = i + 2
                                subtract 1 from i
                            end-if
      *     Fill in the sign
                            if Reg(RegX) < 0
                                move '-' to CalcRegBase(i:1)
                                subtract 1 from i
                            end-if
      *     SPACE fill the unused part
                            move spaces to CalcRegBase(1:i)
      *     Prepare for the fraction part
                            move j to i
                            move length of CalcRegBase to j
      *     Calculate how many digits are sensible
                            if j > gBaseD
                                move gBaseD to j
                                add i to j
                                add 1 to j
                            end-if
      *     Fill in the fraction part
                            move Reg(RegX) to RegFrac
                            perform until RegFrac = 0 or i = j
                                multiply RegFrac by gBase
                                            giving RegFrac
                                                   Digit9
                                perform EnterDigit
                                add 1 to i
                            end-perform
                            move length of CalcRegBase to i
                        end-if

      *     Now transfer to the display area
                        move spaces to CalcRegX
                        move length of CalcRegX to j
                        move 0 to cx
                        move 0 to cy
                        perform until i = 0 or CalcRegBase(i:1) = space
                            if CalcRegBase(i:1) not = '0'
                               or j < (length of CalcRegX)
                                move CalcRegBase(i:1) to CalcRegX(j:1)
                                if cy = 0
                                    add 1 to cx
                                end-if
                                if CalcRegBase(i:1) = '.'
                                    move 1 to cy
                                end-if
                                subtract 1 from j
                            end-if
                            subtract 1 from i
                        end-perform

      *     Special handling for trailing zeros on entered fraction
                        if modeNumber and RegDP > cx
                            subtract cx from RegDP giving cx
                            move j to i
                            subtract cx from j
                            move CalcRegX(i:) to CalcRegX(j:)
                            move length of CalcRegX to i
                            subtract cx from i
                            add 1 to i
                            move all '0' to CalcRegX(i:)
                        end-if
                    end-if

      *     And display...
                    call OS2API '__WinSetWindowText'
                                using by value hwndDisplay
                                      by reference CalcRegX(j:1)
                    if gBase not = gBaseSav
                        move gBase to gBase9
                        move gBase to gBaseSav
                        call OS2API '__WinSetWindowText'
                                    using by value hwndBase
                                          by reference gBase9Bra
                    end-if

      ****************************************************************
      *
      * WM-n messages are dispatched to WM-NUMBER with mp1 as the
      *             number entered
      *
      ****************************************************************
                when    WM-0 thru WM-F
                    subtract WM-0 from msg
                    call OS2API '__WinSendMsg'
                                using by value hwnd
                                      by value WM-NUMBER size 2
                                      by value 0        size 2
                                      by value msg
                                      by value 0        size 4

      ****************************************************************
      *
      * WM-NUMBER must check the validity of the entered digit:
      *                 it must less than the current base
      *                 it must result in a valid number
      *                 fractional digits have to be handled
      *                     in a different way.
      * WM-DP           - Begin entry of fractional part
      * WM-CHS          - X -> -X
      *
      ****************************************************************
                when    WM-NUMBER
                    if mp1w1 >= gBase
                        perform SoundBeep
                    else
                        perform CheckMode
                        if RegDP = 0
                            multiply gBase by Reg(RegX)
                            on size error
                                perform SoundBeep
                            not on size error
                                add mp1w1 to Reg(RegX)
                                on size error
                                    perform SoundBeep
                                    divide gBase into Reg(RegX)
                                end-add
                            end-multiply
                        else
                            move 1 to RegWork
                            perform varying i from 1 by 1
                                    until i > RegDP
                                divide gBase into RegWork
                            end-perform
                            if RegWork not = 0
                                multiply mp1w1 by RegWork
                                add RegWork to Reg(RegX)
                                on size error
                                    perform SoundBeep
                                not on size error
                                    add 1 to RegDP
                                end-add
                            end-if
                        end-if
                        perform UpdateDRaw
                    end-if

                when    WM-DP
                    perform CheckMode
                    if RegDP = 0
                        move 1 to RegDP
                    end-if
                    perform UpdateDRaw

                when    WM-CHS
                    compute Reg(RegX) = - Reg(RegX)
                    perform UpdateDRaw

      ****************************************************************
      * General Functions:
      *     UpdateDRaw  - Update the display
      *     UpdateD     - Update the display after a function
      *     ZeroReg     - Zero a register
      *
      *
      * WM-ENTER        - End input of the current number
      * WM-ROLLUP       - X -> Y -> Z -> T -> X
      * WM-ROLLDOWN     - X -> T -> Z -> Y -> X
      * WM-CLEAR        - 0 -> X, Y, Z, T
      * WM-CLX          - 0 -> X
      * WM-XCHG         - X -> Y -> X
      * WM-PLUS         - Y+X -> X, T -> Z -> Y
      * WM-MINUS        - Y-X -> X, T -> Z -> Y
      * WM-MULTIPLY     - Y*X -> X, T -> Z -> Y
      * WM-DIVIDE       - Y/X -> X, T -> Z -> Y
      * WM-POWER        - Y^X -> X, T -> Z -> Y
      * WM-RECIP        - 1/X -> X
      * WM-HEX          - Set base as 16
      * WM-DEC          - Set base as 10
      * WM-OCT          - Set base as  8
      * WM-BIN          - Set base as  2
      * WM-CUSTOM       - Set base using Dialog Box
      * WM-EXIT         - Exits Calculator
      * WM-ABOUT        - Basic Help
      *
      ****************************************************************
                when    WM-ENTER
                    set modeEnter to true
                    perform RollEnter

                when    WM-ROLLUP
                    perform RollUp
                    perform UpdateD

                when    WM-ROLLDOWN
                    perform RollDown
                    perform UpdateD

                when    WM-CLEAR
                    perform varying i from RegX by 1
                            until i > RegLX
                        call '__ZeroReg' using by value i
                    end-perform
                    perform UpdateD

                when    WM-CLX
                    call '__ZeroReg' using by value RegX size 2
                    perform UpdateD

                when    WM-XCHG
                    move Reg(RegX) to Reg(RegLX)
                    move Reg(RegY) to Reg(RegX)
                    move Reg(RegLX) to Reg(RegY)
                    perform UpdateDRaw

                when    WM-PLUS
                    perform RollAction
                    add Reg(RegLX) to Reg(RegX)
                        on size error set SizeError to true
                    end-add
                    perform UpdateD

                when    WM-MINUS
                    perform RollAction
                    subtract Reg(RegLX) from Reg(RegX)
                        on size error set SizeError to true
                    end-subtract
                    perform UpdateD

                when    WM-MULTIPLY
                    perform RollAction
                    multiply Reg(RegLX) by Reg(RegX)
                        on size error set SizeError to true
                    end-multiply
                    perform UpdateD

                when    WM-DIVIDE
                    perform RollAction
                    divide Reg(RegLX) into Reg(RegX)
                        on size error set SizeError to true
                    end-divide
                    perform UpdateD

                when    WM-POWER
                    perform RollAction
                    compute Reg(RegX) = Reg(RegX) ** Reg(RegLX)
                        on size error set SizeError to true
                    end-compute
                    perform UpdateD

                when    WM-RECIP
                    move Reg(RegX) to Reg(RegLX)
                    compute Reg(RegX) = 1 / Reg(RegX)
                        on size error set SizeError to true
                    end-compute
                    perform UpdateD

                when    WM-HEX
                    move 16 to gBase
                    move HexgBaseD to gBaseD
                    perform UpdateD

                when    WM-DEC
                    move 10 to gBase
                    move DecgBaseD to gBaseD
                    perform UpdateD

                when    WM-OCT
                    move  8 to gBase
                    move OctgBaseD to gBaseD
                    perform UpdateD

                when    WM-BIN
                    move  2 to gBase
                    move BingBaseD to gBaseD
                    perform UpdateD

                when    WM-CUSTOM
                    set WndProc to ENTRY 'DlgProc'
                    call OS2API '__WinDlgBox'
                                using by value Hwnd-Desktop size 4
                                      by value hwnd
                                      by value WndProc
                                      by value ModHandle
                                      by value DI-BASEENTRY size 2
                                      by value 0            size 4

                    move 1 to RegWork
                    move 0 to gBaseD
                    perform until RegWork = 0
                        divide gBase into RegWork
                        add 1 to gBaseD
                    end-perform
                    perform UpdateD

                when    WM-EXIT
                    call OS2API '__WinPostMsg'
                                using by value hwnd
                                      by value Wm-Quit
                                      by value 0 size 4
                                      by value 0 size 4

                when    WM-ABOUT
                    call OS2API '__WinMessageBox'
                                using by value Hwnd-Desktop size 4
                                      by value hwnd
                                      by reference AboutText
                                      by reference AboutTitle
                                      by value 0 size 2
                                      by value 0 size 2

      ****************************************************************
      *
      *  All other messages are despatched to the default
      *  window procedure according to the PM rules.
      *
      ****************************************************************

                when other
                    call OS2API '__WinDefWindowProc'
                                using by value hwnd
                                      by value msg
                                      by value mp1
                                      by value mp2
                                returning mresult

            end-evaluate

            exit program returning mresult.

      ****************************************************************
      *
      * Button Window Procedure (after subclassing)
      *
      *  This simply changes the pointer when over a button
      *
      ****************************************************************
        ButtonWndProc-S section.
        entry 'BWndProc' using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2.

            move 0 to mresult
            evaluate msg
                when Wm-MouseMove
                    call OS2API '__WinQueryWindowUShort'
                                using by value hwnd
                                      by value QWS-ID size 2
                                returning i
                    evaluate i
                        when WM-0 thru WM-F
                            subtract WM-BUTTONS from i
                            subtract id0 from i
                            if i < gBase
                                move 1 to i
                            else
                                move 0 to i
                            end-if
                        when other
                            move 1 to i
                    end-evaluate
                    if i = 1
                        call OS2API '__WinSetPointer'
                                    using by value Hwnd-Desktop size 4
                                          by value ptrH
                    end-if
                    move 1 to mresult

                when other

                    call OS2API '__WinQueryWindowULong'
                                using by value hwnd
                                      by value QWL-USER size 2
                                returning WndProc

                    call OS2API WndProc
                                using by value hwnd
                                      by value msg
                                      by value mp1
                                      by value mp2
                                returning mresult

            end-evaluate

            exit program returning mresult.

      ****************************************************************
      *
      * Dialog Box procedure
      *
      ****************************************************************
        ButtonWndProc-S section.
        entry 'DlgProc' using by value hwnd
                              by value msg
                              by value mp1
                              by value mp2.

            move 0 to mresult
            evaluate msg
                when    Wm-Initdlg
                    call OS2API '__WinSendDlgItemMsg'
                                using by value hwnd
                                      by value DI-BASE         size 2
                                      by value EM-SETTEXTLIMIT size 2
                                      by value 3               size 4
                                      by value 0               size 4
                    call OS2API '__WinSetDlgItemShort'
                                using by value hwnd
                                      by value DI-BASE size 2
                                      by value gBase
                                      by value 0       size 2

                when    Wm-Command
                    evaluate mp1w1
                        when DID-OK
                            call OS2API '__WinQueryDlgItemShort'
                                        using by value hwnd
                                              by value DI-BASE size 2
                                              by reference j
                                              by value 0       size 2
                            if j < 2 or j > 36
                                perform SoundBeep
                            else
                                move j to gBase
                            end-if

                            call OS2API '__WinDismissDlg'
                                        using by value hwnd
                                              by value 1 size 2

                        when DID-CANCEL
                            call OS2API '__WinDIsmissDlg'
                                        using by value hwnd
                                              by value 1 size 2

                    end-evaluate

                when other
                    call OS2API '__WinDefDlgProc'
                                using by value hwnd
                                      by value msg
                                      by value mp1
                                      by value mp2
                                returning mresult

            end-evaluate

            exit program returning mresult.


      ****************************************************************
      *
      * Position Window: We use the input parameters to calculate
      *                  the actual position and size of a window
      *                  And then issue a WinSetWindowPos to
      *                  place the window correctly.
      *
      ****************************************************************
        PositionWindow-S Section.
        entry 'PositionWindow' using by value hwnd
                                     by value BaseX
                                     by value BaseY
                                     by value SizeX
                                     by value SizeY
                                     by reference PointSize.

            move     BaseX       to sxLeft
            multiply ButtonGap   by sxLeft
            add      RelativeGap to sxLeft
            multiply psx         by sxLeft

            move     SizeX       to sxRight
            add      RelativeGap to sxRight
            multiply ButtonGap   by sxRight
            multiply psx         by sxRight
            subtract sxLeft    from sxRight

            move     BaseY       to syBottom
            multiply ButtonGap   by syBottom
            add      RelativeGap to syBottom
            multiply psy         by syBottom

            move     SizeY       to syTop
            add      RelativeGap to syTop
            multiply ButtonGap   by syTop
            multiply psy         by syTop
            subtract syBottom  from syTop

      * The following is more descriptive but less efficient.
      *
      * compute sxLeft   =
      *         psx * ( ButtonGapRatio * BaseX + RelativeGap )
      * compute sxRight  =
      *         psx * ButtonGapRatio * ( SizeX + RelativeGap )
      *               - sxLeft
      * compute syBottom =
      *         psy * ( ButtonGapRatio * BaseY + RelativeGap )
      * compute syTop    =
      *         psy * ButtonGapRatio * ( SizeY + RelativeGap )
      *               - syBottom

            call OS2API '__WinSetWindowPos'
                        using by value hwnd
                              by value Hwnd-Top     size 4
                              by value sxLeft
                              by value syBottom
                              by value sxRight
                              by value syTop
                              by value SIZEMOVESHOW size 2

            exit program.


      ****************************************************************
      *
      * CreateDisplay: Create windows to be used for outputing
      *                numbers to the screen
      *                This is used for the X Register and the BASE
      *
      ****************************************************************
        CreateDisplay-S Section.
        entry 'CreateDisplay' using by value hwnd
                                    by value Style
                                    by value msg.

            call OS2API '__WinCreateWindow'
                        using by value      hwnd
                              by value      WC-STATIC   size 4
                              by value      0           size 4
                              by value      Style
                              by value      0           size 2
                              by value      0           size 2
                              by value      0           size 2
                              by value      0           size 2
                              by value      hwnd
                              by value      Hwnd-Top    size 4
                              by value      msg
                              by value      0           size 4
                              by value      0           size 4

                        returning hwnd

            exit program returning hwnd.


      ****************************************************************
      *
      * CreateButton: Each BUTTON on the screen needs to be
      *               created as a separate window.
      *               We give it a ID (which it sends to its owner
      *               when it is pressed) identical to the
      *               message id of its processing routine.
      *               We also initialize the text.
      *               We subclass the buttons for pointer control
      *
      ****************************************************************
        CreateButton-S Section.
        entry 'CreateButton' using by value hwnd
                                   by value msg.

            move msg to i
            add WM-BUTTONS to i

            call OS2API '__WinCreateWindow'
                        using by value      hwnd
                              by value      Wc-Button   size 4
                              by value      0           size 4
                              by value      ButtonStyle
                              by value      0           size 2
                              by value      0           size 2
                              by value      0           size 2
                              by value      0           size 2
                              by value      hwnd
                              by value      Hwnd-Top    size 4
                              by value      i
                              by value      0           size 4
                              by value      0           size 4

                        returning hwnd

            set WndProc to ENTRY 'BWndProc'
            call OS2API '__WinSubclassWindow'
                        using by value hwnd
                              by value WndProc
                        returning WndProc

            call OS2API '__WinSetWindowULong'
                        using by value hwnd
                              by value Qwl-User size 2
                              by value WndProc

      * The string to WinSetWindowText should be NULL terminated
            move hwndText(msg) to str
            inspect str replacing first space by x'00'

            call OS2API '__WinSetWindowText'
                        using by value      hwnd
                              by reference  str

            exit program returning hwnd.

      ****************************************************************
      *
      * ZeroReg - a function which sets register 'msg' to zero.
      *           This has been localised to minimise the impact
      *           of a change in the internal definition of the
      *           registers.
      *
      ****************************************************************
        ZeroReg-S Section.
        entry 'ZeroReg' using by value msg.

            move 0 to Reg(msg)
            move 0 to RegDP
            exit program.

      ****************************************************************
      *
      * SoundBeep - Sound the bell at 512 HZ (= C') for 50 ms.
      *
      ****************************************************************
        SoundBeep Section.
            call OS2API '__DosBeep'
                        using by value 512 size 2
                              by value  50 size 2.

      ****************************************************************
      *
      * Miscellaneous Subroutines:
      *
      * UpdateDRaw  -   Generate a message to force update of RegX
      * UpdateD     -   Also sets mode to Function
      * RollEnter   -   Register Rolling for ENTER
      * RollUp      -   Register Rolling for ROLLUP
      * RollDown    -   Register Rolling for ROLLDOWN
      * RollAction  -   Register Rolling for PLUS/MINUS etc.
      * EnterDigit  -   Turn Digit9 to ASCII (DigitX) and store
      * CheckMode   -   Handles the mode checking for entry of
      *                     a digit or a DP char.
      *
      ****************************************************************
        UpdateD Section.
            set modeFunction to true
            perform UpdateDRaw.

        UpdateDRaw Section.
            call OS2API '__WinSendMsg'
                        using by value hwnd
                              by value WM-UPDATEDISPLAY size 2
                              by value 0                size 4
                              by value 0                size 4.

        RollEnter Section.
            move Reg(RegZ) to Reg(RegT)
            move Reg(RegY) to Reg(RegZ)
            move Reg(RegX) to Reg(RegY).

        RollUp Section.
            move Reg(RegT) to RegWork
            perform RollEnter
            move RegWork to Reg(RegX).

        RollDown Section.
            perform RollAction
            move Reg(RegLX) to Reg(RegT).

        RollAction Section.
            move Reg(RegX) to Reg(RegLX)
            move Reg(RegY) to Reg(RegX)
            move Reg(RegZ) to Reg(RegY)
            move Reg(RegT) to Reg(RegZ).

        EnterDigit Section.
            if Digit9 > 9
                add 7 to Digit9
            end-if
            add h"30" to Digit9
            move DigitX to CalcRegBase(i:1).

        CheckMode Section.
            if modeFunction
                perform RollEnter
                set modeEnter to true
            end-if
            if modeEnter
                call '__ZeroReg' using by value RegX size 2
            end-if
            set modeNumber to true.
