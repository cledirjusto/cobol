           03 SHANDLE                           PIC 9(4) COMP-5.
           03 LHANDLE                           POINTER.
           03 CHAR                              PIC X COMP-X.
           03 SHORT                             PIC S9(4) COMP-5.
           03 LONG                              PIC S9(9) COMP-5.
           03 INT                               PIC S9(4) COMP-5.
           03 UCHAR                             PIC X COMP-X.
           03 USHORT                            PIC 9(4) COMP-5.
           03 ULONG                             PIC 9(9) COMP-5.
           03 UINT                              PIC 9(4) COMP-5.
           03 BYTE                              PIC X COMP-X.
           03 HFILE                             PIC 9(4) COMP-5.
           03 PHFILE                            POINTER.
           03 PSZ                               POINTER.
           03 NPSZ                              PIC 9(4) COMP-5.
           03 PCH                               POINTER.
           03 NPCH                              PIC 9(4) COMP-5.
           03 PFN                               PROCEDURE-POINTER.
           03 NPFN                              PROCEDURE-POINTER.
           03 PPFN                              POINTER.
           03 PBYTE                             POINTER.
           03 NPBYTE                            PIC 9(4) COMP-5.
           03 PCHAR                             POINTER.
           03 PSHORT                            POINTER.
           03 PLONG                             POINTER.
           03 PINT                              POINTER.
           03 PUCHAR                            POINTER.
           03 PUSHORT                           POINTER.
           03 PULONG                            POINTER.
           03 PUINT                             POINTER.
           03 PVOID                             POINTER.
           03 BOOL                              PIC 9(4) COMP-5.
           03 PBOOL                             POINTER.
           03 SEL                               PIC 9(4) COMP-5.
           03 PSEL                              POINTER.
           03 ERRORID                           PIC 9(9) COMP-5.
           03 PERRORID                          POINTER.
           03 HMODULE                           PIC 9(4) COMP-5.
           03 PHMODULE                          POINTER.
           03 PID                               PIC 9(4) COMP-5.
           03 PPID                              POINTER.
           03 TID                               PIC 9(4) COMP-5.
           03 PTID                              POINTER.
           03 HSEM                              POINTER.
           03 PHSEM                             POINTER.
           03 HAB                               POINTER.
           03 PHAB                              POINTER.
           03 HPS                               POINTER.
           03 PHPS                              POINTER.
           03 HDC                               POINTER.
           03 PHDC                              POINTER.
           03 HRGN                              POINTER.
           03 PHRGN                             POINTER.
           03 HBITMAP                           POINTER.
           03 PHBITMAP                          POINTER.
           03 HMF                               POINTER.
           03 PHMF                              POINTER.
           03 COLOR                             PIC S9(9) COMP-5.
           03 PCOLOR                            POINTER.

           03 POINTL.
            05 POINTL-x                         PIC S9(9) COMP-5.
            05 POINTL-y                         PIC S9(9) COMP-5.

           03 PPOINTL                           POINTER.
           03 NPPOINTL                          PIC 9(4) COMP-5.

           03 POINTS.
            05 POINTS-x                         PIC S9(4) COMP-5.
            05 POINTS-y                         PIC S9(4) COMP-5.

           03 PPOINTS                           POINTER.

           03 RECTL.
            05 RECTL-xLeft                      PIC S9(9) COMP-5.
            05 RECTL-yBottom                    PIC S9(9) COMP-5.
            05 RECTL-xRight                     PIC S9(9) COMP-5.
            05 RECTL-yTop                       PIC S9(9) COMP-5.

           03 PRECTL                            POINTER.
           03 NPRECTL                           PIC 9(4) COMP-5.

           03 FILLER OCCURS 8.
            05 STR8                             PIC X COMP-X.

           03 PSTR8                             POINTER.

           03 DRIVDATA.
            05 DRIVDATA-cb                      PIC S9(9) COMP-5.
            05 DRIVDATA-lVersion                PIC S9(9) COMP-5.
            05 FILLER OCCURS 32.
             07 DRIVDATA-szDeviceName           PIC X COMP-X.
            05 FILLER OCCURS 1.
             07 DRIVDATA-abGeneralData          PIC X COMP-X.

           03 PDRIVDATA                         POINTER.

           03 DEVOPENSTRUC.
            05 DEVOPENSTRUC-pszLogAddress       POINTER.
            05 DEVOPENSTRUC-pszDriverName       POINTER.
            05 DEVOPENSTRUC-pdriv               POINTER.
            05 DEVOPENSTRUC-pszDataType         POINTER.
            05 DEVOPENSTRUC-pszComment          POINTER.
            05 DEVOPENSTRUC-pszQueueProcName    POINTER.
            05 DEVOPENSTRU-pszQueueProcParams   POINTER.
            05 DEVOPENSTRUC-pszSpoolerParams    POINTER.
            05 DEVOPENSTRUC-pszNetworkParams    POINTER.

           03 PDEVOPENSTRUC                     POINTER.

           03 FATTRS.
            05 FATTRS-usRecordLength            PIC 9(4) COMP-5.
            05 FATTRS-fsSelection               PIC 9(4) COMP-5.
            05 FATTRS-lMatch                    PIC S9(9) COMP-5.
            05 FILLER OCCURS 32.
             07 FATTRS-szFacename               PIC X COMP-X.
            05 FATTRS-idRegistry                PIC 9(4) COMP-5.
            05 FATTRS-usCodePage                PIC 9(4) COMP-5.
            05 FATTRS-lMaxBaselineExt           PIC S9(9) COMP-5.
            05 FATTRS-lAveCharWidth             PIC S9(9) COMP-5.
            05 FATTRS-fsType                    PIC 9(4) COMP-5.
            05 FATTRS-fsFontUse                 PIC 9(4) COMP-5.

           03 PFATTRS                           POINTER.

           03 FONTMETRICS.
            05 FILLER OCCURS 32.
             07 FONTMETRICS-szFamilyname        PIC X COMP-X.
            05 FILLER OCCURS 32.
             07 FONTMETRICS-szFacename          PIC X COMP-X.
            05 FONTMETRICS-idRegistry           PIC 9(4) COMP-5.
            05 FONTMETRICS-usCodePage           PIC 9(4) COMP-5.
            05 FONTMETRICS-lEmHeight            PIC S9(9) COMP-5.
            05 FONTMETRICS-lXHeight             PIC S9(9) COMP-5.
            05 FONTMETRICS-lMaxAscender         PIC S9(9) COMP-5.
            05 FONTMETRICS-lMaxDescender        PIC S9(9) COMP-5.
            05 FONTMETRICS-lLowerCaseAscent     PIC S9(9) COMP-5.
            05 FONTMETRICS-lLowerCaseDescent    PIC S9(9) COMP-5.
            05 FONTMETRICS-lInternalLeading     PIC S9(9) COMP-5.
            05 FONTMETRICS-lExternalLeading     PIC S9(9) COMP-5.
            05 FONTMETRICS-lAveCharWidth        PIC S9(9) COMP-5.
            05 FONTMETRICS-lMaxCharInc          PIC S9(9) COMP-5.
            05 FONTMETRICS-lEmInc               PIC S9(9) COMP-5.
            05 FONTMETRICS-lMaxBaselineExt      PIC S9(9) COMP-5.
            05 FONTMETRICS-sCharSlope           PIC S9(4) COMP-5.
            05 FONTMETRICS-sInlineDir           PIC S9(4) COMP-5.
            05 FONTMETRICS-sCharRot             PIC S9(4) COMP-5.
            05 FONTMETRICS-usWeightClass        PIC 9(4) COMP-5.
            05 FONTMETRICS-usWidthClass         PIC 9(4) COMP-5.
            05 FONTMETRICS-sXDeviceRes          PIC S9(4) COMP-5.
            05 FONTMETRICS-sYDeviceRes          PIC S9(4) COMP-5.
            05 FONTMETRICS-sFirstChar           PIC S9(4) COMP-5.
            05 FONTMETRICS-sLastChar            PIC S9(4) COMP-5.
            05 FONTMETRICS-sDefaultChar         PIC S9(4) COMP-5.
            05 FONTMETRICS-sBreakChar           PIC S9(4) COMP-5.
            05 FONTMETRICS-sNominalPointSize    PIC S9(4) COMP-5.
            05 FONTMETRICS-sMinimumPointSize    PIC S9(4) COMP-5.
            05 FONTMETRICS-sMaximumPointSize    PIC S9(4) COMP-5.
            05 FONTMETRICS-fsType               PIC 9(4) COMP-5.
            05 FONTMETRICS-fsDefn               PIC 9(4) COMP-5.
            05 FONTMETRICS-fsSelection          PIC 9(4) COMP-5.
            05 FONTMETRICS-fsCapabilities       PIC 9(4) COMP-5.
            05 FONTMETRICS-lSubscriptXSize      PIC S9(9) COMP-5.
            05 FONTMETRICS-lSubscriptYSize      PIC S9(9) COMP-5.
            05 FONTMETRICS-lSubscriptXOffset    PIC S9(9) COMP-5.
            05 FONTMETRICS-lSubscriptYOffset    PIC S9(9) COMP-5.
            05 FONTMETRICS-lSuperscriptXSize    PIC S9(9) COMP-5.
            05 FONTMETRICS-lSuperscriptYSize    PIC S9(9) COMP-5.
            05 FONTMETRIC-lSuperscriptXOffset   PIC S9(9) COMP-5.
            05 FONTMETRIC-lSuperscriptYOffset   PIC S9(9) COMP-5.
            05 FONTMETRICS-lUnderscoreSize      PIC S9(9) COMP-5.
            05 FONTMETRIC-lUnderscorePosition   PIC S9(9) COMP-5.
            05 FONTMETRICS-lStrikeoutSize       PIC S9(9) COMP-5.
            05 FONTMETRICS-lStrikeoutPosition   PIC S9(9) COMP-5.
            05 FONTMETRICS-sKerningPairs        PIC S9(4) COMP-5.
            05 FONTMETRICS-sFamilyClass         PIC S9(4) COMP-5.
            05 FONTMETRICS-lMatch               PIC S9(9) COMP-5.

           03 PFONTMETRICS                      POINTER.
           03 HWND                              POINTER.
           03 PHWND                             POINTER.

           03 WRECT.
            05 WRECT-xLeft                      PIC S9(4) COMP-5.
            05 WRECT-dummy1                     PIC S9(4) COMP-5.
            05 WRECT-yBottom                    PIC S9(4) COMP-5.
            05 WRECT-dummy2                     PIC S9(4) COMP-5.
            05 WRECT-xRight                     PIC S9(4) COMP-5.
            05 WRECT-dummy3                     PIC S9(4) COMP-5.
            05 WRECT-yTop                       PIC S9(4) COMP-5.
            05 WRECT-dummy4                     PIC S9(4) COMP-5.

           03 PWRECT                            POINTER.
           03 NPWRECT                           PIC 9(4) COMP-5.

           03 WPOINT.
            05 WPOINT-x                         PIC S9(4) COMP-5.
            05 WPOINT-dummy1                    PIC S9(4) COMP-5.
            05 WPOINT-y                         PIC S9(4) COMP-5.
            05 WPOINT-dummy2                    PIC S9(4) COMP-5.

           03 PWPOINT                           POINTER.
           03 NPWPOINT                          PIC 9(4) COMP-5.
           03 MPARAM                            POINTER.
           03 PMPARAM                           POINTER.
           03 MRESULT                           POINTER.
           03 PMRESULT                          POINTER.
           03 PFNWP                             PROCEDURE-POINTER.

           03 QVERSDATA.
            05 QVERSDATA-environment            PIC 9(4) COMP-5.
            05 QVERSDATA-version                PIC 9(4) COMP-5.

           03 PQVERSDATA                        POINTER.

           03 SWP.
            05 SWP-fs                           PIC 9(4) COMP-5.
            05 SWP-cy                           PIC S9(4) COMP-5.
            05 SWP-cx                           PIC S9(4) COMP-5.
            05 SWP-y                            PIC S9(4) COMP-5.
            05 SWP-x                            PIC S9(4) COMP-5.
            05 SWP-hwndInsertBehind             POINTER.
            05 SWP-hwnd                         POINTER.

           03 PSWP                              POINTER.

           03 QMSG.
            05 QMSG-hwnd                        POINTER.
            05 QMSG-msg                         PIC 9(4) COMP-5.
            05 QMSG-mp1                         POINTER.
            05 QMSG-mp2                         POINTER.
            05 QMSG-time                        PIC 9(9) COMP-5.
            05 QMSG-ptl.
             07 QMSG-ptl-x                      PIC S9(9) COMP-5.
             07 QMSG-ptl-y                      PIC S9(9) COMP-5.

           03 PQMSG                             POINTER.
           03 HMQ                               POINTER.

           03 CMDMSG.
            05 CMDMSG-source                    PIC 9(4) COMP-5.
            05 CMDMSG-fMouse                    PIC 9(4) COMP-5.
            05 CMDMSG-cmd                       PIC 9(4) COMP-5.
            05 CMDMSG-unused                    PIC 9(4) COMP-5.

           03 MQINFO.
            05 MQINFO-cb                        PIC 9(4) COMP-5.
            05 MQINFO-pid                       PIC 9(4) COMP-5.
            05 MQINFO-tid                       PIC 9(4) COMP-5.
            05 MQINFO-cmsgs                     PIC 9(4) COMP-5.
            05 MQINFO-pReserved                 POINTER.

           03 PMQINFO                           POINTER.

           03 FRAMECDATA.
            05 FRAMECDATA-cb                    PIC 9(4) COMP-5.
            05 FRAMECDATA-flCreateFlags         PIC 9(9) COMP-5.
            05 FRAMECDATA-hmodResources         PIC 9(4) COMP-5.
            05 FRAMECDATA-idResources           PIC 9(4) COMP-5.

           03 PFRAMECDATA                       POINTER.
           03 HPOINTER                          POINTER.

           03 XYWINSIZE.
            05 XYWINSIZE-x                      PIC S9(4) COMP-5.
            05 XYWINSIZE-y                      PIC S9(4) COMP-5.
            05 XYWINSIZE-cx                     PIC S9(4) COMP-5.
            05 XYWINSIZE-cy                     PIC S9(4) COMP-5.
            05 XYWINSIZE-fsWindow               PIC 9(4) COMP-5.

           03 PXYWINSIZE                        POINTER.
           03 HPROGRAM                          POINTER.
           03 PHPROGRAM                         POINTER.
           03 HINI                              POINTER.
           03 PHINI                             POINTER.
           03 HSWITCH                           POINTER.
           03 PHSWITCH                          POINTER.

           03 SWCNTRL.
            05 SWCNTRL-hwnd                     POINTER.
            05 SWCNTRL-hwndIcon                 POINTER.
            05 SWCNTRL-hprog                    POINTER.
            05 SWCNTRL-idProcess                PIC 9(4) COMP-5.
            05 SWCNTRL-idSession                PIC 9(4) COMP-5.
            05 SWCNTRL-uchVisibility            PIC X COMP-X.
            05 SWCNTRL-fbJump                   PIC X COMP-X.
            05 FILLER OCCURS 120.
             07 SWCNTRL-szSwtitle               PIC X COMP-X.
            05 SWCNTRL-fReserved                PIC X COMP-X.

           03 PSWCNTRL                          POINTER.
           03 HAPP                              POINTER.
           03 HLIB                              PIC 9(4) COMP-5.
           03 PHLIB                             POINTER.
      * HATOMTBL is not defined.
           03 HCONVLIST                         POINTER.
           03 HCONV                             POINTER.
           03 HSZ                               POINTER.
           03 PHSZ                              POINTER.
           03 HDMGDATA                          POINTER.
           03 PHDMGDATA                         POINTER.

           03 CONVCONTEXT.
            05 CONVCONTEXT-cb                   PIC 9(9) COMP-5.
            05 CONVCONTEXT-country              PIC 9(4) COMP-5.
            05 CONVCONTEXT-codepage             PIC 9(4) COMP-5.

           03 PCONVCONTEXT                      POINTER.

           03 CONVINFO.
            05 CONVINFO-cb                      PIC 9(9) COMP-5.
            05 CONVINFO-hConvPartner            POINTER.
            05 CONVINFO-hszAppPartner           POINTER.
            05 CONVINFO-hszAppName              POINTER.
            05 CONVINFO-hszTopic                POINTER.
            05 CONVINFO-hszItem                 POINTER.
            05 CONVINFO-usFmt                   PIC 9(4) COMP-5.
            05 CONVINFO-usType                  PIC 9(4) COMP-5.
            05 CONVINFO-usStatus                PIC 9(4) COMP-5.
            05 CONVINFO-usConvst                PIC 9(4) COMP-5.
            05 CONVINFO-LastError               PIC 9(4) COMP-5.
            05 CONVINFO-cc.
             07 CONVINFO-cc-cb                  PIC 9(9) COMP-5.
             07 CONVINFO-cc-country             PIC 9(4) COMP-5.
             07 CONVINFO-cc-codepage            PIC 9(4) COMP-5.

           03 PCONVINFO                         POINTER.
           03 XTYPF-NODATA
           03 XTYPF-ACKREQ
           03 SZDDESYS-TOPIC
           03 SZDDESYS-ITEM-TOPICS
           03 SZDDESYS-ITEM-SYSITEMS
           03 SZDDESYS-ITEM-RTNMSG
           03 SZDDESYS-ITEM-STATUS
           03 SZDDESYS-ITEM-FORMATS
           03 FNDATAXFER                        POINTER.
           03 PFNDATAXFER                       PROCEDURE-POINTER.

           78 AWP-MINIMIZED                     VALUE H"010000".
           78 AWP-MAXIMIZED                     VALUE H"020000".
           78 AWP-RESTORED                      VALUE H"040000".
           78 AWP-ACTIVATE                      VALUE H"080000".
           78 AWP-DEACTIVATE                    VALUE H"100000".

           78 CBK-ALL                           VALUE -1.
           78 CBK-ENABLE                        VALUE -2.

           78 CMDSRC-PUSHBUTTON                 VALUE 1.
           78 CMDSRC-MENU                       VALUE 2.
           78 CMDSRC-ACCELERATOR                VALUE 3.
           78 CMDSRC-OTHER                      VALUE 0.

           78 CONVST-NULL                       VALUE 0.
           78 CONVST-INCOMPLETE                 VALUE 1.
           78 CONVST-TERMINATED                 VALUE 2.
           78 CONVST-CONNECTED                  VALUE 3.
           78 CONVST-INIT1                      VALUE 4.
           78 CONVST-REQSENT                    VALUE 5.
           78 CONVST-DATARCVD                   VALUE 6.
           78 CONVST-POKESENT                   VALUE 7.
           78 CONVST-POKEACKRCVD                VALUE 8.
           78 CONVST-EXECSENT                   VALUE 9.
           78 CONVST-EXECACKRCVD                VALUE 10.
           78 CONVST-ADVSENT                    VALUE 11.
           78 CONVST-UNADVSENT                  VALUE 12.
           78 CONVST-ADVACKRCVD                 VALUE 13.
           78 CONVST-UNADVACKRCVD               VALUE 14.
           78 CONVST-ADVDATASENT                VALUE 15.
           78 CONVST-ADVDATAACKRCVD             VALUE 16.

           78 CQ-FLUSH                          VALUE H"01".
           78 CQ-NEXT                           VALUE H"02".
           78 CQ-PREV                           VALUE H"04".
           78 CQ-COUNT                          VALUE H"08".
           78 CQ-ACTIVEONLY                     VALUE H"10".
           78 CQ-INACTIVEONLY                   VALUE H"20".
           78 CQ-COMPLETEDONLY                  VALUE H"40".
           78 CQ-FAILEDONLY                     VALUE H"80".
           78 CQ-REMOVE                         VALUE H"0100".

           78 CS-MOVENOTIFY                     VALUE H"01".
           78 CS-SIZEREDRAW                     VALUE H"04".
           78 CS-HITTEST                        VALUE H"08".
           78 CS-PUBLIC                         VALUE H"10".
           78 CS-FRAME                          VALUE H"20".
           78 CS-CLIPCHILDREN                   VALUE H"20000000".
           78 CS-CLIPSIBLINGS                   VALUE H"10000000".
           78 CS-PARENTCLIP                     VALUE H"08000000".
           78 CS-SAVEBITS                       VALUE H"04000000".
           78 CS-SYNCPAINT                      VALUE H"02000000".

           78 CURSOR-SOLID                      VALUE 0.
           78 CURSOR-HALFTONE                   VALUE H"01".
           78 CURSOR-FRAME                      VALUE H"02".
           78 CURSOR-FLASH                      VALUE H"04".
           78 CURSOR-SETPOS                     VALUE H"8000".

           78 DBM-NORMAL                        VALUE 0.
           78 DBM-INVERT                        VALUE H"01".
           78 DBM-HALFTONE                      VALUE H"02".
           78 DBM-STRETCH                       VALUE H"04".
           78 DBM-IMAGEATTRS                    VALUE H"08".

           78 DB-PATCOPY                        VALUE 0.
           78 DB-PATINVERT                      VALUE H"01".
           78 DB-DESTINVERT                     VALUE H"02".
           78 DB-AREAMIXMODE                    VALUE H"03".
           78 DB-ROP                            VALUE H"07".
           78 DB-INTERIOR                       VALUE H"08".
           78 DB-AREAATTRS                      VALUE H"10".
           78 DB-STANDARD                       VALUE H"0100".
           78 DB-DLGBORDER                      VALUE H"0200".

           78 DID-OK                            VALUE 1.
           78 DID-CANCEL                        VALUE 2.
           78 DID-ERROR                         VALUE H"FFFF".

           78 DMGCMD-MONITOR                    VALUE H"01".
           78 DMGCMD-CLIENTONLY                 VALUE H"02".
           78 DMGCMD-SECURE                     VALUE H"04".
           78 DMGCMD-CASEINSENSITIVE            VALUE H"08".

           78 DMGERR-NO-ERROR                   VALUE 0.
           78 DMGERR-FIRST                      VALUE H"4000".
           78 DMGERR-POSTMSG-FAILED             VALUE H"4000".
           78 DMGERR-DLL-NOT-INITIALIZED        VALUE H"4001".
           78 DMGERR-PMWIN-ERROR                VALUE H"4002".
           78 DMGERR-NO-CONV-ESTABLISHED        VALUE H"4003".
           78 DMGERR-TIMEOUT                    VALUE H"4004".
           78 DMGERR-REENTRANCY                 VALUE H"4005".
           78 DMGERR-DLL-USAGE                  VALUE H"4006".
           78 DMGERR-BAD-APP-NAME               VALUE H"4007".
           78 DMGERR-DOS-ERROR                  VALUE H"4008".
           78 DMGERR-SYSTOPIC-NOT-SUPPORTED     VALUE H"4009".
           78 DMGERR-INVALID-HDMGDATA           VALUE H"400A".
           78 DMGERR-BUSY                       VALUE H"400B".
           78 DMGERR-NOTPROCESSED               VALUE H"400C".
           78 DMGERR-INVALIDPARAMETER           VALUE H"400D".
           78 DMGERR-MEMORY-ERROR               VALUE H"400E".
           78 DMGERR-POKEACKTIMEOUT             VALUE H"400F".
           78 DMGERR-EXECACKTIMEOUT             VALUE H"4010".
           78 DMGERR-DATAACKTIMEOUT             VALUE H"4011".
           78 DMGERR-SERVER-DIED                VALUE H"4012".
           78 DMGERR-ADVACKTIMEOUT              VALUE H"4013".
           78 DMGERR-UNADVACKTIMEOUT            VALUE H"4014".
           78 DMGERR-ACCESS-DENIED              VALUE H"4015".
           78 DMGERR-UNFOUND-QUEUE-ID           VALUE H"4016".
           78 DMGERR-LAST                       VALUE H"4016".

           78 DRIVER-NAME                       VALUE 1.
           78 DRIVER-DATA                       VALUE 2.

           78 DT-LEFT                           VALUE 0.
           78 DT-EXTERNALLEADING                VALUE H"80".
           78 DT-CENTER                         VALUE H"0100".
           78 DT-RIGHT                          VALUE H"0200".
           78 DT-TOP                            VALUE 0.
           78 DT-VCENTER                        VALUE H"0400".
           78 DT-BOTTOM                         VALUE H"0800".
           78 DT-HALFTONE                       VALUE H"1000".
           78 DT-MNEMONIC                       VALUE H"2000".
           78 DT-WORDBREAK                      VALUE H"4000".
           78 DT-ERASERECT                      VALUE H"8000".
           78 DT-QUERYEXTENT                    VALUE H"02".
           78 DT-TEXTATTRS                      VALUE H"40".

           78 EAF-DEFAULTOWNER                  VALUE H"01".
           78 EAF-UNCHANGEABLE                  VALUE H"02".
           78 EAF-REUSEICON                     VALUE H"04".

           78 FATTR-SEL-ITALIC                  VALUE H"01".
           78 FATTR-SEL-UNDERSCORE              VALUE H"02".
           78 FATTR-SEL-OUTLINE                 VALUE H"08".
           78 FATTR-SEL-STRIKEOUT               VALUE H"10".
           78 FATTR-SEL-BOLD                    VALUE H"20".
           78 FATTR-TYPE-KERNING                VALUE H"04".
           78 FATTR-TYPE-MBCS                   VALUE H"08".
           78 FATTR-TYPE-DBCS                   VALUE H"10".
           78 FATTR-TYPE-ANTIALIASED            VALUE H"20".
           78 FATTR-FONTUSE-NOMIX               VALUE H"02".
           78 FATTR-FONTUSE-OUTLINE             VALUE H"04".
           78 FATTR-FONTUSE-TRANSFORMABLE       VALUE H"08".

           78 FCF-TITLEBAR                      VALUE H"01".
           78 FCF-SYSMENU                       VALUE H"02".
           78 FCF-MENU                          VALUE H"04".
           78 FCF-SIZEBORDER                    VALUE H"08".
           78 FCF-MINBUTTON                     VALUE H"10".
           78 FCF-MAXBUTTON                     VALUE H"20".
           78 FCF-MINMAX                        VALUE H"30".
           78 FCF-VERTSCROLL                    VALUE H"40".
           78 FCF-HORZSCROLL                    VALUE H"80".
           78 FCF-DLGBORDER                     VALUE H"0100".
           78 FCF-BORDER                        VALUE H"0200".
           78 FCF-SHELLPOSITION                 VALUE H"0400".
           78 FCF-TASKLIST                      VALUE H"0800".
           78 FCF-NOBYTEALIGN                   VALUE H"1000".
           78 FCF-NOMOVEWITHOWNER               VALUE H"2000".
           78 FCF-ICON                          VALUE H"4000".
           78 FCF-ACCELTABLE                    VALUE H"8000".
           78 FCF-SYSMODAL                      VALUE H"010000".
           78 FCF-SCREENALIGN                   VALUE H"020000".
           78 FCF-MOUSEALIGN                    VALUE H"040000".
           78 FCF-PALETTE-NORMAL                VALUE H"080000".
           78 FCF-PALETTE-HELP                  VALUE H"100000".
           78 FCF-PALETTE-POPUPODD              VALUE H"200000".
           78 FCF-PALETTE-POPUPEVEN             VALUE H"400000".
           78 FCF-STANDARD                      VALUE H"08CC3F".

           78 FC-NOSETFOCUS                     VALUE H"01".
           78 FC-NOBRINGTOTOP                   VALUE H"01".
           78 FC-NOLOSEFOCUS                    VALUE H"02".
           78 FC-NOBRINGTOPFIRSTWINDOW          VALUE H"02".
           78 FC-NOSETACTIVEFOCUS               VALUE H"03".
           78 FC-NOSETACTIVE                    VALUE H"04".
           78 FC-NOLOSEACTIVE                   VALUE H"08".
           78 FC-NOSETSELECTION                 VALUE H"10".
           78 FC-NOLOSESELECTION                VALUE H"20".

           78 FF-FLASHWINDOW                    VALUE H"01".
           78 FF-ACTIVE                         VALUE H"02".
           78 FF-FLASHHILITE                    VALUE H"04".
           78 FF-OWNERHIDDEN                    VALUE H"08".
           78 FF-DLGDISMISSED                   VALUE H"10".
           78 FF-OWNERDISABLED                  VALUE H"20".
           78 FF-SELECTED                       VALUE H"40".
           78 FF-NOACTIVATESWP                  VALUE H"80".

           78 FM-TYPE-FIXED                     VALUE H"01".
           78 FM-TYPE-LICENSED                  VALUE H"02".
           78 FM-TYPE-KERNING                   VALUE H"04".
           78 FM-TYPE-DBCS                      VALUE H"10".
           78 FM-TYPE-MBCS                      VALUE H"18".
           78 FM-TYPE-64K                       VALUE H"8000".
           78 FM-DEFN-OUTLINE                   VALUE H"01".
           78 FM-DEFN-GENERIC                   VALUE H"8000".
           78 FM-SEL-ITALIC                     VALUE H"01".
           78 FM-SEL-UNDERSCORE                 VALUE H"02".
           78 FM-SEL-NEGATIVE                   VALUE H"04".
           78 FM-SEL-OUTLINE                    VALUE H"08".
           78 FM-SEL-STRIKEOUT                  VALUE H"10".
           78 FM-SEL-BOLD                       VALUE H"20".
           78 FM-CAP-NOMIX                      VALUE H"01".

           78 FS-ICON                           VALUE H"01".
           78 FS-ACCELTABLE                     VALUE H"02".
           78 FS-SHELLPOSITION                  VALUE H"04".
           78 FS-TASKLIST                       VALUE H"08".
           78 FS-NOBYTEALIGN                    VALUE H"10".
           78 FS-NOMOVEWITHOWNER                VALUE H"20".
           78 FS-SYSMODAL                       VALUE H"40".
           78 FS-DLGBORDER                      VALUE H"80".
           78 FS-BORDER                         VALUE H"0100".
           78 FS-SCREENALIGN                    VALUE H"0200".
           78 FS-MOUSEALIGN                     VALUE H"0400".
           78 FS-SIZEBORDER                     VALUE H"0800".
           78 FS-STANDARD                       VALUE H"0F".

           78 HINI-USERPROFILE                  VALUE -1.
           78 HINI-SYSTEMPROFILE                VALUE -2.
           78 HINI-USER                         VALUE -1.
           78 HINI-SYSTEM                       VALUE -2.

           78 HWND-DESKTOP                      VALUE 1.
           78 HWND-OBJECT                       VALUE 2.
           78 HWND-TOP                          VALUE 3.
           78 HWND-BOTTOM                       VALUE 4.
           78 HWND-THREADCAPTURE                VALUE 5.

           78 MAX-ERRSTR                        VALUE 50.
           78 MAX-MONITORSTR                    VALUE 511.

           78 MBID-OK                           VALUE 1.
           78 MBID-CANCEL                       VALUE 2.
           78 MBID-ABORT                        VALUE 3.
           78 MBID-RETRY                        VALUE 4.
           78 MBID-IGNORE                       VALUE 5.
           78 MBID-YES                          VALUE 6.
           78 MBID-NO                           VALUE 7.
           78 MBID-HELP                         VALUE 8.
           78 MBID-ENTER                        VALUE 9.
           78 MBID-ERROR                        VALUE H"FFFF".

           78 MB-OK                             VALUE 0.
           78 MB-OKCANCEL                       VALUE H"01".
           78 MB-RETRYCANCEL                    VALUE H"02".
           78 MB-ABORTRETRYIGNORE               VALUE H"03".
           78 MB-YESNO                          VALUE H"04".
           78 MB-YESNOCANCEL                    VALUE H"05".
           78 MB-CANCEL                         VALUE H"06".
           78 MB-ENTER                          VALUE H"07".
           78 MB-ENTERCANCEL                    VALUE H"08".
           78 MB-NOICON                         VALUE 0.
           78 MB-CUANOTIFICATION                VALUE 0.
           78 MB-ICONQUESTION                   VALUE H"10".
           78 MB-ICONEXCLAMATION                VALUE H"20".
           78 MB-CUAWARNING                     VALUE H"20".
           78 MB-ICONASTERISK                   VALUE H"30".
           78 MB-ICONHAND                       VALUE H"40".
           78 MB-CUACRITICAL                    VALUE H"40".
           78 MB-QUERY                          VALUE H"10".
           78 MB-WARNING                        VALUE H"20".
           78 MB-INFORMATION                    VALUE H"30".
           78 MB-CRITICAL                       VALUE H"40".
           78 MB-ERROR                          VALUE H"40".
           78 MB-DEFBUTTON1                     VALUE 0.
           78 MB-DEFBUTTON2                     VALUE H"0100".
           78 MB-DEFBUTTON3                     VALUE H"0200".
           78 MB-APPLMODAL                      VALUE 0.
           78 MB-SYSTEMMODAL                    VALUE H"1000".
           78 MB-HELP                           VALUE H"2000".
           78 MB-MOVEABLE                       VALUE H"4000".

           78 PM-REMOVE                         VALUE H"01".
           78 PM-NOREMOVE                       VALUE 0.

           78 PROC-NAME                         VALUE 5.
           78 PROC-PARAMS                       VALUE 6.

           78 PSF-LOCKWINDOWUPDATE              VALUE H"01".
           78 PSF-CLIPUPWARDS                   VALUE H"02".
           78 PSF-CLIPDOWNWARDS                 VALUE H"04".
           78 PSF-CLIPSIBLINGS                  VALUE H"08".
           78 PSF-CLIPCHILDREN                  VALUE H"10".
           78 PSF-PARENTCLIP                    VALUE H"20".

           78 QFC-NEXTINCHAIN                   VALUE H"01".
           78 QFC-ACTIVE                        VALUE H"02".
           78 QFC-FRAME                         VALUE H"03".
           78 QFC-SELECTACTIVE                  VALUE H"04".

           78 QID-SYNC                          VALUE 0.
           78 QID-NEWEST                        VALUE -1.
           78 QID-OLDEST                        VALUE -2.

           78 QV-OS2                            VALUE 0.
           78 QV-CMS                            VALUE H"01".
           78 QV-TSO                            VALUE H"02".
           78 QV-TSOBATCH                       VALUE H"03".
           78 QV-OS400                          VALUE H"04".

           78 QW-NEXT                           VALUE 0.
           78 QW-PREV                           VALUE 1.
           78 QW-TOP                            VALUE 2.
           78 QW-BOTTOM                         VALUE 3.
           78 QW-OWNER                          VALUE 4.
           78 QW-PARENT                         VALUE 5.
           78 QW-NEXTTOP                        VALUE 6.
           78 QW-PREVTOP                        VALUE 7.
           78 QW-FRAMEOWNER                     VALUE 8.

           78 RUM-IN                            VALUE 1.
           78 RUM-OUT                           VALUE 2.
           78 RUM-INOUT                         VALUE 3.

           78 SAF-INSTALLEDCMDLINE              VALUE H"01".
           78 SAF-STARTCHILDAPP                 VALUE H"02".

           78 SEVERITY-NOERROR                  VALUE 0.
           78 SEVERITY-WARNING                  VALUE H"04".
           78 SEVERITY-ERROR                    VALUE H"08".
           78 SEVERITY-SEVERE                   VALUE H"0C".
           78 SEVERITY-UNRECOVERABLE            VALUE H"10".

           78 SMD-DELAYED                       VALUE H"01".
           78 SMD-IMMEDIATE                     VALUE H"02".

           78 SSM-SYNCHRONOUS                   VALUE H"01".
           78 SSM-ASYNCHRONOUS                  VALUE H"02".
           78 SSM-MIXED                         VALUE H"03".

           78 ST-CONNECTED                      VALUE H"01".
           78 ST-ADVISE                         VALUE H"02".

           78 SWL-VISIBLE                       VALUE H"04".
           78 SWL-INVISIBLE                     VALUE H"01".
           78 SWL-GRAYED                        VALUE H"02".
           78 SWL-JUMPABLE                      VALUE H"02".
           78 SWL-NOTJUMPABLE                   VALUE H"01".

           78 SWP-SIZE                          VALUE H"01".
           78 SWP-MOVE                          VALUE H"02".
           78 SWP-ZORDER                        VALUE H"04".
           78 SWP-SHOW                          VALUE H"08".
           78 SWP-HIDE                          VALUE H"10".
           78 SWP-NOREDRAW                      VALUE H"20".
           78 SWP-NOADJUST                      VALUE H"40".
           78 SWP-ACTIVATE                      VALUE H"80".
           78 SWP-DEACTIVATE                    VALUE H"0100".
           78 SWP-EXTSTATECHANGE                VALUE H"0200".
           78 SWP-MINIMIZE                      VALUE H"0400".
           78 SWP-MAXIMIZE                      VALUE H"0800".
           78 SWP-RESTORE                       VALUE H"1000".
           78 SWP-FOCUSACTIVATE                 VALUE H"2000".
           78 SWP-FOCUSDEACTIVATE               VALUE H"4000".

           78 SW-SCROLLCHILDREN                 VALUE H"01".
           78 SW-INVALIDATERGN                  VALUE H"02".

           78 TID-TIMEOUT                       VALUE 1.
           78 TID-ABORT                         VALUE 2.
           78 TID-SELFDESTRUCT                  VALUE 3.

           78 WA-WARNING                        VALUE 0.
           78 WA-NOTE                           VALUE 1.
           78 WA-ERROR                          VALUE 2.
           78 WA-CWINALARMS                     VALUE 3.

           78 WM-NULL                           VALUE 0.
           78 WM-CREATE                         VALUE H"01".
           78 WM-DESTROY                        VALUE H"02".
           78 WM-OTHERWINDOWDESTROYED           VALUE H"03".
           78 WM-ENABLE                         VALUE H"04".
           78 WM-SHOW                           VALUE H"05".
           78 WM-MOVE                           VALUE H"06".
           78 WM-SIZE                           VALUE H"07".
           78 WM-ADJUSTWINDOWPOS                VALUE H"08".
           78 WM-CALCVALIDRECTS                 VALUE H"09".
           78 WM-SETWINDOWPARAMS                VALUE H"0A".
           78 WM-QUERYWINDOWPARAMS              VALUE H"0B".
           78 WM-HITTEST                        VALUE H"0C".
           78 WM-ACTIVATE                       VALUE H"0D".
           78 WM-SETFOCUS                       VALUE H"0F".
           78 WM-SETSELECTION                   VALUE H"10".
           78 WM-PPAINT                         VALUE H"11".
           78 WM-PSETFOCUS                      VALUE H"12".
           78 WM-PSYSCOLORCHANGE                VALUE H"13".
           78 WM-PSIZE                          VALUE H"14".
           78 WM-PACTIVATE                      VALUE H"15".
           78 WM-PCONTROL                       VALUE H"16".
           78 WM-COMMAND                        VALUE H"20".
           78 WM-SYSCOMMAND                     VALUE H"21".
           78 WM-HELP                           VALUE H"22".
           78 WM-PAINT                          VALUE H"23".
           78 WM-TIMER                          VALUE H"24".
           78 WM-SEM1                           VALUE H"25".
           78 WM-SEM2                           VALUE H"26".
           78 WM-SEM3                           VALUE H"27".
           78 WM-SEM4                           VALUE H"28".
           78 WM-CLOSE                          VALUE H"29".
           78 WM-QUIT                           VALUE H"2A".
           78 WM-SYSCOLORCHANGE                 VALUE H"2B".
           78 WM-SYSVALUECHANGED                VALUE H"2D".
           78 WM-APPTERMINATENOTIFY             VALUE H"2E".
           78 WM-PRESPARAMCHANGED               VALUE H"2F".
           78 WM-CONTROL                        VALUE H"30".
           78 WM-VSCROLL                        VALUE H"31".
           78 WM-HSCROLL                        VALUE H"32".
           78 WM-INITMENU                       VALUE H"33".
           78 WM-MENUSELECT                     VALUE H"34".
           78 WM-MENUEND                        VALUE H"35".
           78 WM-DRAWITEM                       VALUE H"36".
           78 WM-MEASUREITEM                    VALUE H"37".
           78 WM-CONTROLPOINTER                 VALUE H"38".
           78 WM-CONTROLHEAP                    VALUE H"39".
           78 WM-QUERYDLGCODE                   VALUE H"3A".
           78 WM-INITDLG                        VALUE H"3B".
           78 WM-SUBSTITUTESTRING               VALUE H"3C".
           78 WM-MATCHMNEMONIC                  VALUE H"3D".
           78 WM-SAVEAPPLICATION                VALUE H"3E".
           78 WM-HELPBASE                       VALUE H"0F00".
           78 WM-HELPTOP                        VALUE H"0FFF".
           78 WM-USER                           VALUE H"1000".

           78 WS-VISIBLE                        VALUE H"80000000".
           78 WS-DISABLED                       VALUE H"40000000".
           78 WS-CLIPCHILDREN                   VALUE H"20000000".
           78 WS-CLIPSIBLINGS                   VALUE H"10000000".
           78 WS-PARENTCLIP                     VALUE H"08000000".
           78 WS-SAVEBITS                       VALUE H"04000000".
           78 WS-SYNCPAINT                      VALUE H"02000000".
           78 WS-MINIMIZED                      VALUE H"01000000".
           78 WS-MAXIMIZED                      VALUE H"800000".
           78 WS-GROUP                          VALUE H"010000".
           78 WS-TABSTOP                        VALUE H"020000".
           78 WS-MULTISELECT                    VALUE H"040000".

           78 XTYPF-MASK                        VALUE H"FF".
           78 XTYPF-NOTIFICATION                VALUE H"10".
           78 XTYPF-BOOL                        VALUE H"20".
           78 XTYPF-FLAGS                       VALUE H"40".
           78 XTYPF-DATA                        VALUE H"80".

           78 XTYP-INIT                         VALUE H"0100".
           78 XTYP-ADVDATA                      VALUE H"0200".
           78 XTYP-EXEC                         VALUE H"0300".
           78 XTYP-POKE                         VALUE H"0400".
           78 XTYP-ADVSTART                     VALUE H"0500".
           78 XTYP-TERM                         VALUE H"0600".
           78 XTYP-ADVSTOP                      VALUE H"0700".
           78 XTYP-REGISTER                     VALUE H"0800".
           78 XTYP-UNREGISTER                   VALUE H"0900".
           78 XTYP-ADVREQ                       VALUE H"0A00".
           78 XTYP-MONITOR                      VALUE H"0B00".
           78 XTYP-ACK                          VALUE H"0C00".
           78 XTYP-WILDINIT                     VALUE H"0D00".
           78 XTYP-REQUEST                      VALUE H"0E00".
           78 XTYP-XFERCOMPLETE                 VALUE H"0F00".

           78 XYF-NOAUTOCLOSE                   VALUE H"08".
           78 XYF-MINIMIZED                     VALUE H"04".
           78 XYF-MAXIMIZED                     VALUE H"02".
           78 XYF-INVISIBLE                     VALUE H"01".
           78 XYF-NORMAL                        VALUE 0.

           78 C-TRUE                            VALUE 1.
           78 C-FALSE                           VALUE 0.
           78 WINERR-BASE                       VALUE H"1000".
           78 GPIERR-BASE                       VALUE H"2000".
           78 DEVERR-BASE                       VALUE H"3000".
           78 SPLERR-BASE                       VALUE H"4000".
           78 C-ADDRESS                         VALUE 0.
           78 DATA-TYPE                         VALUE 3.
           78 COMMENT                           VALUE 4.
           78 SPL-PARAMS                        VALUE 7.
           78 NETWORK-PARAMS                    VALUE 8.
           78 FACESIZE                          VALUE 32.
           78 DEFAULT-QUEUE-SIZE                VALUE 0.
           78 MAXNAMEL                          VALUE 60.
           78 MSGF-DDEMGR                       VALUE H"8001".
           78 HDATA-APPOWNED                    VALUE H"01".
           78 TIMEOUT-ASYNC                     VALUE -1.
