     H BNDDIR('CDBNDDIR')
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H OPTION(*NODEBUGIO:*SRCSTMT) DEBUG(*YES)

     FCDACTU01  UF   E           K DISK
     FCDACTR04  IF   E           K DISK    RENAME(CDACTPR:CD@R04)
     FCDCCMR02  IF   E           K DISK
     FCDACT01D  CF   E             WORKSTN SFILE(SFL1:SFLRRN)

      /COPY *LIBL/QCPYSRC,COPYUTILPR

     DPGMSTATUS       SDS
     DUSERID                 254    263
     DPGMNAME            *PROC

     DFILTERSTRING     S             10A   VARYING
     DLoadStatus       S              6A
     DwActiveSts       S              1A   Inz('A')
     DwCreditCard      S              1A   Inz
     DwTestString      S           1000A   Varying
     DwSflTop          S              1A   Inz
     DwIdx             S              2P 0 Inz
     DwCCMAccount      S             12P 0 Inz
     DwCCMPriCust      S              9P 0 Inz
     DwCCMSecCust      S              9P 0 Inz
     DwSflEmpty        S              1A   Inz

     C     K1#ACTR04     Klist
     C                   KFld                    wActiveSts

     C     K2#ACTR04     Klist
     C                   KFld                    wActiveSts
     C                   KFld                    FIRSTKEY

     C     K3#ACTR04     Klist
     C                   KFld                    wActiveSts
     C                   KFld                    LASTKEY

     C     K4#ACTR04     Klist
     C                   KFld                    wActiveSts
     C                   KFld                    PREFILTKEY

     C     K1#CCMR02     Klist
     C                   KFld                    wCCMPriCust
     C                   KFld                    wCCMAccount

     C     K2#CCMR02     Klist
     C                   KFld                    wCCMSecCust
     C                   KFld                    wCCMAccount

     C     *LIKE         DEFINE    ACCTNUM       WACCOUNT
     C     *LIKE         DEFINE    ACCTNUM       FIRSTKEY
     C     *LIKE         DEFINE    ACCTNUM       LASTKEY
     C     *LIKE         DEFINE    ACCTNUM       POSKEY
     C     *LIKE         DEFINE    ACCTNUM       PREFILTKEY
     C     *LIKE         DEFINE    CUSTFLTR      CUSTFLTSV

     C                   MOVE      13            PAGESIZE          2 0
     C                   MOVE      *BLANKS       MSGID             8
     C                   MOVE      ' '           SKIPFLAG          1
     C                   MOVE      *ZEROS        SCANPOS           3 0
     C                   MOVE      ' '           APPLYFLTR         1
     C                   MOVE      ' '           APPLYFLTSV        1
     C                   MOVE      ' '           TRIGREFRESH       1
     C                   MOVE      'ACTIVE'      LoadStatus

      ************************************************************************
      * MAIN PROCESSING                                                      *
      ************************************************************************

     C                   MOVEL     PGMNAME       PGMQ
     C                   WRITE     MSGSFLC

     C                   If        LoadStatus = 'ACTIVE'
     C     K1#ACTR04     SETLL     CDACTR04
     C                   ELSEIF    LoadStatus = 'ALL'
     C     *LOVAL        SETLL     CDACTU01
     C                   ENDIF
     C                   EXSR      $PGDN

     C     1             DOWEQ     1

     C                   If        LoadStatus = 'ACTIVE'
     C                   EVAL      VARYFT = 'F8=All  F9=Filter  F12=Cancel'
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   EVAL      VARYFT = 'F8=Active  F9=Filter  F12=Cancel'
     C                   ENDIF
     C                   WRITE     HEADER1
     C                   WRITE     HEADER2
     C                   WRITE     FOOTER
     C                   WRITE     MSGSFLC
     C                   Z-ADD     1             SFLRRN
     C                   EXFMT     CTL1
     C                   CALL      'CDUTL02C'
     C                   READ      HEADER2
     C                   SETOFF                                       8081

     C     *IN03         IFEQ      *ON
     C     *IN12         OREQ      *ON
     C                   LEAVE
     C                   ENDIF

     C     *IN05         IFEQ      *ON
     C                   EXSR      $REFRESH
     C                   ITER
     C                   ENDIF

     C     *IN06         IFEQ      *ON
     C                   EXSR      $ADD
     C                   If        LoadStatus = 'ACTIVE'
     C     K2#ACTR04     SETLL     CDACTR04
     C                   ELSEIF    LoadStatus = 'ALL'
     C     FIRSTKEY      SETLL     CDACTU01
     C                   ENDIF
     C                   EXSR      $PGDN
     C                   ITER
     C                   ENDIF

     C     *IN08         IFEQ      *ON
     C                   If        LoadStatus = 'ACTIVE'
     C                   Eval      LoadStatus = 'ALL'
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   Eval      LoadStatus = 'ACTIVE'
     C                   ENDIF
     C                   EXSR      $TOGGLE
     C                   ITER
     C                   ENDIF

     C     *IN09         IFEQ      *ON
     C                   EXSR      $SETFILTER
     C                   ITER
     C                   ENDIF

     C     *IN25         IFEQ      *ON
     C     *IN33         ANDEQ     *OFF
     C                   If        LoadStatus = 'ACTIVE'
     C     K3#ACTR04     SETLL     CDACTR04
     C                   ELSEIF    LoadStatus = 'ALL'
     C     LASTKEY       SETLL     CDACTU01
     C                   ENDIF
     C                   EXSR      $PGDN
     C                   ITER
     C                   ENDIF

     C     *IN26         IFEQ      *ON
     C                   EXSR      $PGUP
     C                   ITER
     C                   ENDIF

     C     *IN50         IFEQ      *ON
     C                   ITER
     C                   ENDIF

     C     POSITION      IFNE      *BLANK
     C                   EXSR      $POS
     C                   ITER
     C                   ENDIF

     C     *IN30         IFEQ      *OFF
     C                   EXSR      $READSFL
     C                   ENDIF

     C                   ENDDO

     C                   SETON                                        LR
     C*************************************************************************
     C* $TOGGLE - Toggle between Active and All records                       *
     C*************************************************************************

     C     $TOGGLE       BEGSR
     C                   If        LoadStatus = 'ACTIVE'
     C     K2#ACTR04     SETLL     CDACTR04
     C                   ELSEIF    LoadStatus = 'ALL'
     C     FIRSTKEY      SETLL     CDACTU01
     C                   ENDIF
     C                   EXSR      $PGDN
     C                   ENDSR
     C*************************************************************************
     C* $REFRESH - Refresh subfile                                            *
     C*************************************************************************

     C     $REFRESH      BEGSR
     C                   If        LoadStatus = 'ACTIVE'
     C                   Exsr      $REFRESHACT
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   Exsr      $REFRESHALL
     C                   ENDIF
     C                   ENDSR
     C*************************************************************************
     C* $PGUP - READ BACK TO PAGE UP                                          *
     C*************************************************************************

     C     $PGUP         BEGSR
     C                   If        wSflEmpty = 'Y'
     C                   Eval      FIRSTKEY = *Hival
     C                   EndIf
     C                   Exsr      SflTopLogic
     C                   If        wSflTop = *Blanks
     C                   If        LoadStatus = 'ACTIVE'
     C                   Exsr      $PGUP#ACT
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   Exsr      $PGUP#ALL
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*************************************************************************
     C* SflTopLogic - Check for Pageup                                        *
     C*************************************************************************

     C     SflTopLogic   BegsR
     C                   Eval      wSflTop = *Blanks
     C                   If        LoadStatus = 'ACTIVE'
     C     K2#ACTR04     SETLL     CDACTR04
     C     K1#ACTR04     READPE    CDACTR04
     C                   If        %EOF(CDACTR04)
     C                   Eval      wSflTop = 'Y'
     C                   EndIf
     C                   ELSEIF    LoadStatus = 'ALL'
     C     FIRSTKEY      SETLL     CDACTU01
     C                   READP(N)  CDACTU01
     C                   If        %EOF(CDACTU01)
     C                   Eval      wSflTop = 'Y'
     C                   EndIf
     C                   ENDIF
     C                   ENDSR
     C*************************************************************************
     C* $POS - POSTION TO                                                     *
     C*************************************************************************

     C     $POS          BEGSR
     C                   If        LoadStatus = 'ACTIVE'
     C                   Exsr      $POSACT
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   Exsr      $POSALL
     C                   ENDIF
     C                   ENDSR
     C*************************************************************************
     C* $PGUP#ALL - READ BACK TO PAGE UP                                          *
     C*************************************************************************
     C     $PGUP#ALL     BEGSR
     C
     C                   SETON                                            30
     C                   WRITE     CTL1
     C                   SETOFF                                           30
     C                   Z-ADD     0             SFLRRN

     C                   CLEAR                   READCT            2 0
     C     FIRSTKEY      SETLL     CDACTU01
     C     READCT        DOWLE     PAGESIZE
     C                   READP(N)  CDACTU01                               90
     C     *IN90         IFEQ      *ON
     C
     C     *LOVAL        SETLL     CDACTU01
     C                   LEAVE
     C                   ENDIF

     C                   MOVE      '0'           SKIPFLAG
     C                   EXSR      $FILTER
     C     SKIPFLAG      IFEQ      '1'
     C                   READP(N)  CDACTU01
     C                   ITER
     C                   ENDIF

     C                   ADD       1             READCT
     C                   ENDDO
     C
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $PGUP#ACT - READ BACK TO PAGE UP                                          *
     C*************************************************************************

     C     $PGUP#ACT     BEGSR
     C
     C                   SETON                                            30
     C                   WRITE     CTL1
     C                   SETOFF                                           30
     C                   Z-ADD     0             SFLRRN

     C                   CLEAR                   READCT            2 0
     C     K2#ACTR04     SETLL     CDACTR04
     C     READCT        DOWLE     PAGESIZE
     C     K1#ACTR04     READPE    CDACTR04                               90
     C     *IN90         IFEQ      *ON
     C
     C     K1#ACTR04     SETLL     CDACTR04
     C                   LEAVE
     C                   ENDIF

     C                   MOVE      '0'           SKIPFLAG
     C                   EXSR      $FILTER
     C     SKIPFLAG      IFEQ      '1'
     C     K1#ACTR04     READPE    CDACTR04                               90
     C                   ITER
     C                   ENDIF

     C                   ADD       1             READCT
     C                   ENDDO
     C
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $PGDN - ADVANCE TO NEXT PAGE                                          *
     C*************************************************************************

     C     $PGDN         BEGSR
     C                   Eval      wSflEmpty = 'N'
     C                   Setoff                                       95
     C                   If        LoadStatus = 'ACTIVE'
     C                   Exsr      $PGDN#ACT
     C                   ELSEIF    LoadStatus = 'ALL'
     C                   Exsr      $PGDN#ALL
     C                   ENDIF
     C                   ENDSR
     C*************************************************************************
     C* $PGDNALL - ADVANCE TO NEXT PAGE                                          *
     C*************************************************************************

     C     $PGDN#ALL     BEGSR

     C                   SETOFF                                           31
     C                   SETON                                            30
     C                   WRITE     CTL1
     C                   SETOFF                                           30
     C                   Z-ADD     0             SFLRRN
     C                   Z-ADD     0             FIRSTKEY

     C                   READ(N)   CDACTU01
     C                   DOW       NOT %EOF(CDACTU01)

     C     SFLRRN        IFEQ      PAGESIZE
     C                   SETOFF                                           33
     C                   LEAVE
     C                   ENDIF

     C                   MOVE      '0'           SKIPFLAG
     C                   EXSR      $FILTER
     C     SKIPFLAG      IFEQ      '1'
     C                   READ(N)   CDACTU01
     C                   ITER
     C                   ENDIF

     C                   If        FIRSTKEY = *Zeros
     C                   MOVE      ACCTNUM       FIRSTKEY
     C                   EndIf
     C                   ADD       1             SFLRRN
     C                   EXSR      $FILL
     C                   WRITE     SFL1
     C                   READ(N)   CDACTU01
     C                   ENDDO

     C     SFLRRN        IFEQ      0
     C                   SETON                                            30
     C                   SETON                                            31
     C                   WRITE     EMPTY
     C                   Eval      wSflEmpty = 'Y'
     C                   Seton                                        95
     C                   ELSE
     C                   SETOFF                                           31
     C                   IF        %EOF(CDACTU01)
     C                   SETON                                            33
     C                   ENDIF
     C                   ENDIF

     C                   MOVE      ACCTNUM       LASTKEY
     C                   ENDSR
     C*************************************************************************
     C* $PGDNACT - ADVANCE TO NEXT PAGE                                          *
     C*************************************************************************

     C     $PGDN#ACT     BEGSR

     C                   SETOFF                                           31
     C                   SETON                                            30
     C                   WRITE     CTL1
     C                   SETOFF                                           30
     C                   Z-ADD     0             SFLRRN
     C                   Z-ADD     *Zeros        FIRSTKEY

     C     K1#ACTR04     READE     CDACTR04
     C                   DOW       NOT %EOF(CDACTR04)

     C     SFLRRN        IFEQ      PAGESIZE
     C                   SETOFF                                           33
     C                   LEAVE
     C                   ENDIF

     C                   MOVE      '0'           SKIPFLAG
     C                   EXSR      $FILTER
     C     SKIPFLAG      IFEQ      '1'
     C     K1#ACTR04     READE     CDACTR04
     C                   ITER
     C                   ENDIF

     C                   If        FIRSTKEY = *Zeros
     C                   MOVE      ACCTNUM       FIRSTKEY
     C                   EndIf
     C                   ADD       1             SFLRRN
     C                   EXSR      $FILL
     C                   WRITE     SFL1
     C     K1#ACTR04     READE     CDACTR04
     C                   ENDDO

     C     SFLRRN        IFEQ      0
     C                   SETON                                            30
     C                   SETON                                            31
     C                   WRITE     EMPTY
     C                   Eval      wSflEmpty = 'Y'
     C                   Seton                                        95
     C                   ELSE
     C                   SETOFF                                           31
     C                   IF        %EOF(CDACTR04)
     C                   SETON                                            33
     C                   ENDIF
     C                   ENDIF

     C                   MOVE      ACCTNUM       LASTKEY
     C                   ENDSR
     C*************************************************************************
     C* $POSALL - POSITION TO RECORD                                             *
     C*************************************************************************
     C
     C     $POSALL       BEGSR
     C
     C                   EVAL      POSITION=%TRIM(POSITION)
     C                   EVAL      wTestString = POSITION
     C                   If        Not IsNumeric(wTestString)
     C                   MOVEL     'ERR0016'     MSGID
     C                   SETON                                          81
     C                   EXSR      $ERR
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      FIRSTKEY = %DEC(POSITION:12:0)
     C                   CLEAR                   POSITION
     C     FIRSTKEY      SETLL     CDACTU01
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $POSACT - POSITION TO RECORD                                             *
     C*************************************************************************
     C
     C     $POSACT       BEGSR
     C
     C                   EVAL      POSITION=%TRIM(POSITION)
     C                   EVAL      wTestString = POSITION
     C                   If        Not IsNumeric(wTestString)
     C                   MOVEL     'ERR0016'     MSGID
     C                   SETON                                          81
     C                   EXSR      $ERR
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      FIRSTKEY = %DEC(POSITION:12:0)
     C                   CLEAR                   POSITION
     C     K2#ACTR04     SETLL     CDACTR04
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $REFRESHALL - REFRESH SUBFILE CONTENTS                                   *
     C*************************************************************************

     C     $REFRESHALL   BEGSR

     C                   CLEAR                   POSITION
     C                   CLEAR                   CUSTFLTR
     C                   CLEAR                   APPLYFLTR

     C     *LOVAL        SETLL     CDACTU01
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $REFRESHACT - REFRESH SUBFILE CONTENTS                                   *
     C*************************************************************************

     C     $REFRESHACT   BEGSR

     C                   CLEAR                   POSITION
     C                   CLEAR                   CUSTFLTR
     C                   CLEAR                   APPLYFLTR

     C     K1#ACTR04     SETLL     CDACTR04
     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $FILL - POPULATE SUBFILE DATA                                         *
     C*************************************************************************

     C     $FILL         BEGSR

     C                   EVAL      OPTION = *BLANKS
     C                   EVAL      SFLPCUST = %EDITC(PRICUSTID:'X')
     C                   EVAL      SFLPCUST = %TRIM(SFLPCUST)

     C                   IF        SECCUSTID = *ZEROS
     C                   EVAL      SFLSCUST = '-'
     C                   ELSE
     C                   EVAL      SFLSCUST = %EDITC(SECCUSTID:'X')
     C                   EVAL      SFLSCUST = %TRIM(SFLSCUST)
     C                   ENDIF

     C                   IF        BALANCE = *ZERO
     C                   EVAL      SFLBAL = '$0'
     C                   ELSE
     C                   EVAL      SFLBAL = %EDITC(BALANCE:'N':'$')
     C                   EVAL      SFLBAL = %TRIM(SFLBAL)
     C                   ENDIF

     C                   IF        BALANCE < *ZERO
     C                   EVAL      *IN92 = *ON
     C                   ELSE
     C                   EVAL      *IN92 = *OFF
     C                   ENDIF

     C                   EVAL      SFLACCT = %EDITC(ACCTNUM:'X')
     C                   EVAL      SFLACCT = %TRIM(SFLACCT)
     C                   EVAL      SFLSTS  = %TRIM(ACTIVE)

     C                   ENDSR
     C*************************************************************************
     C* $READSFL - READ SUBFILE OPTIONS                                       *
     C*************************************************************************

     C     $READSFL      BEGSR

     C                   MOVE      *BLANKS       MSGID
     C                   READC     SFL1                                   91
     C     *IN91         DOWEQ     *OFF
     C     OPTION        IFNE      ' '
     C     OPTION        IFEQ      '2'
     C                   EXSR      $UPD
     C                   ELSE
     C     OPTION        IFEQ      '4'
     C                   EXSR      $DELETE
     C                   ELSE
     C     OPTION        IFEQ      '5'
     C                   EXSR      $DTL
     C                   ELSE
     C                   SETON                                        34
     C                   MOVEL     'ERR0001'     MSGID
     C                   SETON                                        60
     C                   EXSR      $ERR
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   READC     SFL1                                   91
     C                   ENDDO

     C     MSGID         IFEQ      *BLANKS
     C     TRIGREFRESH   ANDEQ     *ON
     C                   EXSR      $REFRESH
     C                   ENDIF

     C                   MOVE      *OFF          TRIGREFRESH
     C                   ENDSR
     C*************************************************************************
     C* $DTL - SHOW ACCOUNT DETAILS                                           *
     C*************************************************************************

     C     $DTL          BEGSR
     C
     C                   EVAL      ACCTNUM = %DEC(SFLACCT:16:0)
     C                   CALL      'CDACT02R'
     C                   PARM                    ACCTNUM
     C                   PARM      '1'           VIEWONLY          1
     C                   MOVE      *ON           TRIGREFRESH
     C
     C                   ENDSR
     C*************************************************************************
     C* $UPD - UPDATE ACCOUNT DETAILS                                         *
     C*************************************************************************

     C     $UPD          BEGSR
     C
     C                   EVAL      *IN40 = *Off
     C                   If        SFLSTS = 'A'
     C                   EVAL      ACCTNUM = %DEC(SFLACCT:16:0)
     C                   CALL      'CDACT02R'
     C                   PARM                    ACCTNUM
     C                   PARM      '0'           VIEWONLY
     C                   ElseIf    SFLSTS = 'I'
     C                   EVAL      *IN32 = *On
     C                   EVAL      *IN40 = *On
     C                   EVAL      ACTID = SFLACCT
     C                   EVAL      ERRDELW = 'Cannot edit inactive account.'
     C                   WRITE     HEADER1
     C                   EXFMT     DELERRWIN
     C                   ENDIF
     C
     C                   MOVE      *ON           TRIGREFRESH
     C
     C                   ENDSR
     C*************************************************************************
     C* $ADD - CREATE NEW ACCOUNT                                             *
     C*************************************************************************

     C     $ADD          BEGSR
     C
     C                   CLEAR                   ACCTNUM
     C                   CLEAR                   VIEWONLY
     C                   CALL      'CDACT02R'
     C                   PARM                    ACCTNUM
     C                   PARM                    VIEWONLY
     C
     C                   MOVE      *ON           TRIGREFRESH
     C                   ENDSR
     C*************************************************************************
     C* $SETFILTER - SHOW AND SET FILTER OPTIONS                              *
     C*************************************************************************

     C     $SETFILTER    BEGSR

     C                   MOVE      CUSTFLTR      CUSTFLTSV
     C                   MOVE      APPLYFLTR     APPLYFLTSV

     C                   SETON                                        90
     C                   SETOFF                                       91
     C     1             DOWEQ     1
     C                   EXFMT     FILTERWIN
     C                   SETON                                        90

     C     *IN02         IFEQ      *ON
     C                   CLEAR                   CUSTFLTR
     C                   CLEAR                   APPLYFLTR
     C                   ITER
     C                   ENDIF

     C     *IN12         IFEQ      *ON
     C                   CLEAR                   CUSTFLTR
     C                   MOVE      CUSTFLTSV     CUSTFLTR
     C                   CLEAR                   APPLYFLTR
     C                   MOVE      APPLYFLTSV    APPLYFLTR
     C                   LEAVE
     C                   ENDIF

     C     *IN50         IFEQ      *ON
     C                   ITER
     C                   ENDIF

     C     CUSTFLTR      IFNE      *BLANKS
     C                   EVAL      CUSTFLTR = %TRIM(CUSTFLTR)
     C                   EVAL      wTestString = %TRIM(CUSTFLTR)
     C                   If        Not IsNumeric(wTestString)
     C                   Eval      *IN90 = *Off
     C                   ITER
     C                   ENDIF
     C                   ENDIF

     C                   EXSR      $CHKFILTER
     C                   LEAVE

     C                   ENDDO
     C                   ENDSR
     C*************************************************************************
     C* $CHKFILTER - DETERMINE IF FILTER CRITERIA SHOULD BE APPLIED           *
     C*************************************************************************

     C     $CHKFILTER    BEGSR

     C     CUSTFLTR      IFNE      *BLANKS
     C                   MOVE      '1'           APPLYFLTR
     C                   ELSE
     C                   MOVE      '0'           APPLYFLTR
     C                   ENDIF

     C                   If        LoadStatus = 'ACTIVE'
     C     K2#ACTR04     SETLL     CDACTR04
     C                   ELSEIF    LoadStatus = 'ALL'
     C     FIRSTKEY      SETLL     CDACTU01
     C                   ENDIF

     C                   EXSR      $PGDN

     C                   ENDSR
     C*************************************************************************
     C* $FILTER - DETERMINE WHETHER RECORD MEETS FILTER CRITERIA              *
     C*************************************************************************

     C     $FILTER       BEGSR

     C                   MOVE      PRICUSTID     CUSTIDC           9
     C                   MOVE      '0'           SKIPFLAG
     C
     C     CUSTFLTR      IFNE      *BLANKS
     C                   EVAL      FILTERSTRING = %TRIM(CUSTFLTR)
     C     FILTERSTRING  SCAN      CUSTIDC       SCANPOS
     C     SCANPOS       IFEQ      *ZEROS
     C                   MOVE      '1'           SKIPFLAG
     C                   LEAVESR
     C                   ENDIF
     C                   ENDIF
     C
     C                   ENDSR
     C*************************************************************************
     C* $DELETE - CONFIRM DELETION OF ACCOUNT                                 *
     C*************************************************************************

     C     $DELETE       BEGSR

     C                   EVAL      ACCTNUM = %DEC(SFLACCT:16:0)

     C                   Eval      wCreditCard = 'N'
     C                   Eval      wCCMAccount = *Zeros
     C                   Eval      wCCMPriCust = *Zeros
     C                   Eval      wCCMSecCust = *Zeros
     C                   Eval      wCCMAccount = %Dec(SFLACCT:12:0)
     C                   Eval      DPRICUSTID = *Blanks
     C                   Eval      DSECCUSTID = *Blanks
     C                   Eval      DPRICREDIT = *Zeros
     C                   Eval      DSECCREDIT = *Zeros
     C                   If        SFLPCUST <> *Blanks And
     C                             %Trim(SFLPCUST) <> '-'
     C                   Eval      wCCMPriCust = %Dec(SFLPCUST:9:0)
     C                   ENDIF
     C                   If        SFLSCUST <> *Blanks And
     C                             %Trim(SFLSCUST) <> '-'
     C                   Eval      wCCMSecCust = %Dec(SFLSCUST:9:0)
     C                   ENDIF
     C     K1#CCMR02     Chain     CDCCMR02
     C                   If        %Found(CDCCMR02)
     C                   Eval      wCreditCard = 'Y'
     C                   Eval      DPRICUSTID = %EditC(wCCMPriCust:'X')
     C                   Eval      DPRICREDIT = CARDNUM
     C                   ENDIF
     C     K2#CCMR02     Chain     CDCCMR02
     C                   If        %Found(CDCCMR02)
     C                   Eval      wCreditCard = 'Y'
     C                   Eval      DSECCUSTID = %EditC(wCCMSecCust:'X')
     C                   Eval      DSECCREDIT = CARDNUM
     C                   ENDIF
     C                   EVAL      *IN40 = *Off

     C                   IF        SFLSTS = 'A'
     C                   If        wCreditCard = 'N'
     C                   EVAL      *IN40 = *On
     C     1             DOWEQ     1
     C                   EVAL      ACTID = SFLACCT
     C                   WRITE     HEADER1
     C                   EXFMT     DELWIN

     C     *IN12         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C     *IN50         IFEQ      *ON
     C                   iter
     C                   ENDIF

     C                   EVAL      ACCTNUM = %DEC(SFLACCT:16:0)
     C     ACCTNUM       CHAIN     CDACTU01
     C                   EVAL      ACTIVE = 'I'
     C                   Eval      ActChgDt = %Dec(%Date():*CYMD)
     C                   Eval      ActChgTm = %Dec(%Time)
     C                   Eval      ActChgUsr = UserID
     C                   Eval      ActChgPgm = PgmName
     C                   Update    CDACTPR
     C                   MOVEL     'MSG0009'     MSGID
     C                   EXSR      $ERR
     C                   CLEAR                   MSGID
     C                   LEAVE
     C                   ENDDO
     C                   Else
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN40 = *Off
     C                   EVAL      ACTID = SFLACCT
     C                   EVAL      ERRDELW = 'Cannot inactivate this account '
     C                                       + 'with credit card attached.'
     C                   WRITE     HEADER1
     C                   EXFMT     DELERRWIN
     C                   ENDIF
     C                   ELSEIF    SFLSTS = 'I'
     C                   EVAL      *IN40 = *On
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      ACTID = SFLACCT
     C                   EVAL      ERRDELW = 'Account is already inactive.'
     C                   WRITE     HEADER1
     C                   EXFMT     DELERRWIN
     C                   ENDIF
     C                   MOVE      *ON           TRIGREFRESH

     C                   ENDSR
     C*************************************************************************
     C* $ERR - WRITE TO MESSAGE SUBFILE                                       *
     C*************************************************************************

     C     $ERR          BEGSR
     C                   CALL      'CDUTL01C'
     C                   PARM                    MSGID
     C                   PARM                    PGMQ
     C                   ENDSR

