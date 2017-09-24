      $set ans85 noosvs mf

      *******************************************************************
      *                                                                 *
      *     DDE2 example using COBOL                                    *
      *                                                                 *
      *             by Raymond Obin                                     *
      *             (C) 1990 Micro Focus Ltd.                           *
      *                                                                 *
      *             Written 12 June 1990                                *
      *                                                                 *
      *******************************************************************
      *                                                                 *
      *     This example program should be used in conjunction with     *
      *     the DDE example program.                                    *
      *     This program is an alternative to using EXCEL.              *
      *                                                                 *
      *     For details, see DDE.CBL.                                   *
      *                                                                 *
      *******************************************************************
	special-names.
	    call-convention 3 is OS2API.

        working-storage section.

        copy "DDE2.CPY".

        78  WM-INITDLG              value   h"3b".
        78  WM-CONTROL              value   h"30".
        78  WM-COMMAND              value   h"20".
        78  LM-INSERTITEM           value   h"0161".
        78  LM-SELECTITEM           value   h"0164".
        78  LM-QUERYSELECTION       value   h"0165".
        78  LM-QUERYITEMTEXT        value   h"0168".
        78  LM-SEARCHSTRING         value   h"016b".
        78  LN-SELECT               value   1.
        78  LIT-SORTASCENDING       value   -2.
        78  LIT-FIRST               value   -1.
        78  LIT-FIRST-HIGH          value   h"ffff0000".

        78  HWND-DESKTOP            value   1.

        78  DDEFMT-TEXT             value   1.

        78  DDE-FACK                value   1.

        78  XTYP-INIT               VALUE H"0120".
        78  XTYP-ADVDATA            VALUE H"0240".
        78  XTYP-EXEC               VALUE H"0340".
        78  XTYP-POKE               VALUE H"0440".
        78  XTYP-ADVSTART           VALUE H"0520".
        78  XTYP-TERM               VALUE H"0610".
        78  XTYP-ADVSTOP            VALUE H"0710".
        78  XTYP-REGISTER           VALUE H"0810".
        78  XTYP-UNREGISTER         VALUE H"0910".
        78  XTYP-ADVREQ             VALUE H"0A80".
        78  XTYP-MONITOR            VALUE H"0B10".
        78  XTYP-ACK                VALUE H"0C10".
        78  XTYP-WILDINIT           VALUE H"0D80".
        78  XTYP-REQUEST            VALUE H"0E80".
        78  XTYP-XFERCOMPLETE       VALUE H"0F10".

	01  work-data.
            03  hab                 pointer.
            03  hmq                 pointer.
            03  hwndDlg             pic 9(9) comp-5.
            03  hwndField           pic 9(9) comp-5.
            03  hwndStatus          pic 9(9) comp-5.
            03  hwndListbox         pic 9(9) comp-5.

            03  WndProc     procedure-pointer.

            03  errorCode           pic 9(4) comp-5.

            03  AdviseControl.
                05  AdivseFlag  pic x   value 'N'.
                    88  AdviseOK        value 'Y'.
                    88  AdviseOff       value 'N'.
                05  hszAdviseTopic  pointer.
                05  hszAdviseItem   pointer.

        78  L-TESTDATA          value 'TestData'.
        78  L-TARGETTOPIC       value 'Sheet1'.
        78  L-TARGETCELL        value 'R1C1'.

        01  DDEDataArea.
            03  DDEName         pic x(20).
            03  DDETopic        pic x(9)  value L-TARGETTOPIC & x"00".
            03  hszDDETopic     pointer.
            03  DDEL-System     pic x(7)  value 'System' & x"00".
            03  hszSystem       pointer.
            03  DDEL-Topics     pic x(7)  value 'Topics' & x"00".
            03  hszTopics       pointer.
            03  DDEL-SysItems   pic x(9)  value 'SysItems' & x"00".
            03  hszSysItems     pointer.
            03  DDEL-Status     pic x(7)  value 'Status' & x"00".
            03  hszStatus       pointer.
            03  DDEAdviseItem   pic x(8)  value 'Number0' & x"00".
            03  redefines DDEAdviseItem.
                05              pic x(6).
                05  DDEItem9    pic 9.
                05              pic x.
            03  TargetApp       pic x(20).
            03  hszTargetApp    pointer.
            03  TargetTopic     pic x(20) value L-TESTDATA & x"00".
            03  hszTargetTopic  pointer.
            03  SelectedColour  pic x(21) value 'Red' & x"00".
            03  ItemXfer        pic x(20).
            03  DataXfer        pic x(50).
            03  XferProc        procedure-pointer.
            03  xtyp            pic 9(4) comp-5.
            03  DDEMsg          pic x(30).
            03  DDEData         pic x(256).
            03  hszItemName     pointer.
            03  pidXfer         pic 9(9) comp-5.
            03  hConversation   pointer.
            03  hDmgConvData    pointer.

	local-storage section.
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

        01  i                   pic 9(9) comp-5.
        01  si                  pic 9(4) comp-5.
        01  hwndTemp            pic 9(9) comp-5.

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

            accept DDEData from command-line
            unstring DDEData delimited by spaces
                    into DDEName TargetApp

            if DDEName = spaces
                move 'Excel' to DDEName
            end-if
            if TargetApp = spaces
                move 'COBOLDDE' to TargetApp
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

            move 0 to hwndListbox
            set hConversation to NULL

            set XferProc to entry 'DDEXFER'

            call OS2API '__DDEMgrRegister' using
                        by reference DdeName
                        by value     XferProc
                        by value     1000        size 4
                        by value     0           size 4
                        by value     0           size 4
                    returning errorCode

            if errorCode = 0
                move 'DDE Register successful' & x"00" to DDEMsg
            else
                move 'DDE Register fail' & x"00" to DDEMsg
            end-if

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
                        returning hszDDETopic

            set WndProc to ENTRY 'DlgProc'

            call OS2API '__WinDlgBox' using
                        by value HWND-DESKTOP size 4
                        by value HWND-DESKTOP size 4
                        by value WndProc
                        by value 0 size 2
                        by value IDD-DDEEG size 2
                        by value 0 size 4

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
                        by value hszTargetApp

            call OS2API '__DDEMgrUnregister'

	    call OS2API '__WinDestroyMsgQueue' using by value hmq
	    call OS2API '__WinTerminate'       using by value hab

	    stop run.


        MyDlgProc section.
        entry 'DlgProc' using by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.

	    move 0 to mresult
            evaluate msg

              when    WM-INITDLG
                move hwnd to hwndDlg
                call OS2API '__WinWindowFromID' using
                            by value hwnd
                            by value IDL-COLOUR size 2
                            returning hwndListbox
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'White' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Blue' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Red' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Magenta' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Green' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Cyan' & x"00"
                call OS2API '__WinSendMsg' using
                            by value hwndListbox
                            by value LM-INSERTITEM      size 2
                            by value LIT-SORTASCENDING  size 4
                            by reference 'Yellow' & x"00"

                call OS2API '__WinWindowFromID' using
                            by value hwnd
                            by value IDT-VALUE1 size 2
                            returning hwndField

                call OS2API '__WinSetWindowText' using
                            by value hwndField
                            by reference x'00'

                call OS2API '__WinWindowFromID' using
                            by value hwnd
                            by value IDT-VALUE2 size 2
                            returning hwndField

                call OS2API '__WinSetWindowText' using
                            by value hwndField
                            by reference x'00'

                call OS2API '__WinWindowFromID' using
                            by value hwnd
                            by value IDT-VALUE3 size 2
                            returning hwndField

                call OS2API '__WinSetWindowText' using
                            by value hwndField
                            by reference x'00'

                call OS2API '__WinWindowFromID' using
                            by value hwnd
                            by value IDT-STATUS size 2
                            returning hwndStatus

                perform Refresh

                set hConversation to NULL

              when WM-COMMAND
                evaluate mp1w1
                  when IDB-INITIATE

                    if hConversation not = null
                        exit program returning mresult
                    end-if

                    perform InitConv

                    if hConversation not = null

                        move XTYP-ADVSTART to xtyp
                        perform AdviseLoop

                        move XTYP-REQUEST to xtyp
                        perform varying DDEItem9 from 1 by 1
                                until DDEItem9 > 3
                            move DDEItem9 to si
                            move DDEAdviseItem to ItemXfer

                            perform DDEXfer

                            move DDEData to DDEReply
                            perform UpdateNumbers

                        end-perform

                        move 'DDE Hot-links Active' & x"00" to DDEMsg
                    else
                        move 'DDE Initiate Failed' & x"00" to DDEMsg
                    end-if
                    perform Refresh

                  when IDB-TERMINATE
                  when IDB-EXIT

                    if hConversation not = null

                        move XTYP-ADVSTOP to xtyp
                        perform AdviseLoop

                        perform TermConv

                    end-if

                    if mp1w1 = IDB-EXIT
                        call OS2API '__WinDismissDlg' using
                                    by value hwnd
                                    by value 1 size 2
      *                     This call never returns.
                    end-if

                    move 'DDE Terminated' & x"00" to DDEMsg
                    perform Refresh

                  when other
                end-evaluate

              when WM-CONTROL
                evaluate mp1w1
                  when IDL-COLOUR
                    if mp1w2 = LN-SELECT
                        call OS2API '__WinSendDlgItemMsg' using
                                    by value hwnd
                                    by value IDL-COLOUR size 2
                                    by value LM-QUERYSELECTION size 2
                                    by value LIT-FIRST size 4
                                    by value 0  size 4
                                    returning mp2
                        move 20 to mp2w2
                        call OS2API '__WinSendDlgItemMsg' using
                                    by value hwnd
                                    by value IDL-COLOUR size 2
                                    by value LM-QUERYITEMTEXT size 2
                                    by value mp2
                                    by reference SelectedColour
                        if AdviseOK
                            call OS2API '__DDEMgrPostAdvise' using
                                    by value hszAdviseTopic
                                    by value hszAdviseItem
                        end-if
                    end-if
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

        AdviseLoop Section.
            move x"00" to DataXfer

            perform varying DDEItem9 from 1 by 1
                    until DDEItem9 > 3
                move DDEAdviseItem to ItemXfer
                perform DDEXfer
            end-perform.

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
      *         call OS2API '__DDEMgrPostError'
      *                      using by value errorCode

                move spaces to DDEData

                if xtyp = XTYP-REQUEST

                    call OS2API '__DDEMgrGetData' using
                                by value hDmgConvData
                                by reference DDEData
                                by value 255 size 4
                                by value 0 size 4

                    call OS2API '__DDEMgrGetLastError'
                            returning errorCode
      *             call OS2API '__DDEMgrPostError'
      *                          using by value errorCode
                    call OS2API '__DDEMgrFreeData' using
                                by value hDmgConvData

                end-if

                call OS2API '__DDEMgrFreeHsz'
                            using by value hszItemName

                if hDmgConvData not = NULL
                    move "DDEMgrClientXfer OK" to DDEMsg
                else
                    move "DDEMgrClientXfer Fail" to DDEMsg
                end-if
            end-if.

        InitConv section.
            if hConversation = NULL
                call OS2API '__DDEMgrConnect' using
                             by value hszTargetApp
                                      hszTargetTopic
                                      0 size 4
                            returning hConversation

                if hConversation = NULL
                    call OS2API '__DDEMgrGetLastError'
                                 returning errorCode
                    call OS2API '__DDEMgrPostError'
                                 using by value errorCode
                    move 'DDE connection failed' & x"00" to DDEMsg
                else
                    move 'DDE connected' & x"00" to DDEMsg
                end-if
            end-if.

        UpdateNumbers Section.
            evaluate si
              when 1
                move IDT-VALUE1 to si
              when 2
                move IDT-VALUE2 to si
              when 3
                move IDT-VALUE3 to si
            end-evaluate
            call OS2API '__WinWindowFromID' using
                        by value hwndDlg
                        by value si
                        returning hwndTemp
            call OS2API '__WinSetWindowText' using
                        by value hwndTemp
                        by reference DDEReply.

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

        Refresh Section.
            call OS2API '__WinSetWindowText'
                        using by value hwndStatus
                              by reference DDEMsg.

      * Callback function from DDEManager.
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
            evaluate usType
              when XTYP-INIT
                if hszTopic = hszSystem or hszDDETopic
                    exit program returning 1

              when XTYP-WILDINIT
                call OS2API '__DDEMgrGetHsz' using
                            by reference 'System' & x"00"
                            returning DDEReply(i:2)
                add 2 to i
                call OS2API '__DDEMgrGetHsz' using
                            by reference DDETopic
                            returning DDEReply(i:2)
                add 2 to i

              when XTYP-UNREGISTER
                if hszItem = hszTargetApp
                    set hConversation to null
                    move 'DDE Target not available' & x"00" to DDEMsg
                    perform Refresh
                end-if

              when XTYP-EXEC
                call OS2API '__DDEMgrGetData' using
                            by value hDmgData
                            by reference DDEReply
                            by value 40 size 4
                            by value 0 size 4
                if DDEReply(1:10) = '[FORMULA("'
                    unstring DDEReply(11:) delimited by '"'
                        into SelectedColour
                    inspect SelectedColour
                            replacing first space by x"00"
                    if hwndListbox not = 0
                        call OS2API '__WinSendMsg' using
                                    by value hwndListbox
                                    by value LM-SEARCHSTRING size 2
                                    by value LIT-FIRST-HIGH size 4
                                    by reference SelectedColour
                                    returning i
                        call OS2API '__WinSendMsg' using
                                    by value hwndListbox
                                    by value LM-SELECTITEM size 2
                                    by value i
                                    by value 1 size 4
                        move spaces to DDEReply
                        move DDE-FACK to DDEResult
                    end-if
                end-if

              when XTYP-REQUEST
              when XTYP-ADVREQ
                evaluate hszTopic
                  when hszSystem
                    evaluate hszItem
                      when hszTopics
                        string 'System' & x"09"
                               L-TARGETTOPIC
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
                    string SelectedColour delimited by x"00"
                           into DDEReply pointer i
                end-evaluate

              when XTYP-ADVDATA

      * Topic and Item are reversed here...
                if hszTopicString = L-TESTDATA
                    perform AdviseSelect
                    move all x'00' to DDEReply
                    call OS2API '__DDEMgrGetData' using
                                by value hDmgData
                                by reference DDEReply
                                by value 40 size 4
                                by value 0 size 4
                    call OS2API '__DDEMgrFreeData' using
                                by value hDmgData
                    perform UpdateNumbers
                end-if

              when XTYP-ADVSTART
                if usFmt = DDEFMT-TEXT
                    if hszItemString = L-TARGETCELL
                        set AdviseOK to TRUE
                        call OS2API '__DDEMgrIncHszCount' using
                                    by value hszTopic
                        set hszAdviseTopic to hszTopic
                        call OS2API '__DDEMgrIncHszCount' using
                                    by value hszItem
                        set hszAdviseItem to hszItem
                        move 1 to DDEResult
                    end-if
                end-if

              when XTYP-ADVSTOP
                if hszItemString = L-TARGETCELL
                    set AdviseOff to TRUE
                    call OS2API '__DDEMgrFreeHsz' using
                                by value hszAdviseTopic
                    call OS2API '__DDEMgrFreeHsz' using
                                by value hszAdviseItem
                    move 1 to DDEResult
                end-if

              when other
            end-evaluate

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
