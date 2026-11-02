     H BNDDIR('CDBNDDIR')
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H OPTION(*NODEBUGIO:*SRCSTMT) DEBUG(*YES)
     H/EJECT

     FCDCCMU01  UF A E           K DISK
     FCDCCMR02  IF   E           K DISK    RENAME(CDCCMPR:CDCCMPR02)
     FCDACTR01  IF   E           K DISK
     FCDCUSR01  IF   E           K DISK
     FCDCCM02D  CF   E             WORKSTN    PREFIX(D) INDDS(INDICATORS)
     F/EJECT

      *-------------------------------------------------------------------*
      * WORKING STORAGE
      *-------------------------------------------------------------------*
     D INDICATORS      DS
     D  F2                             N   OVERLAY(INDICATORS:2)
     D  F3                             N   OVERLAY(INDICATORS:3)
     D  F4                             N   OVERLAY(INDICATORS:4)
     D  F9                             N   OVERLAY(INDICATORS:9)
     D  F12                            N   OVERLAY(INDICATORS:12)
     D  COMMANDKEY                     N   OVERLAY(INDICATORS:50)
     D  SAVEPROTECT                    N   OVERLAY(INDICATORS:61)
     D  ADDMODE                        N   OVERLAY(INDICATORS:70)
     D  VIEWONLY                       N   OVERLAY(INDICATORS:71)
     D  CUSTIDERR                      N   OVERLAY(INDICATORS:77)
     D  ACCTERR                        N   OVERLAY(INDICATORS:78)
     D  NAMEERR                        N   OVERLAY(INDICATORS:79)
     D  HIDECHGTM                      N   OVERLAY(INDICATORS:90)
     D  ALWAYSON                       N   OVERLAY(INDICATORS:99) INZ(*ON)

     D WCARDNUM        S                   LIKE(CARDNUM)
     D WCVV            S                   LIKE(CVV)
     D WCUSTID         S                   LIKE(CUSTID)
     D WACCTNUM        S                   LIKE(ACCTNUM)
     D MESSAGEID       S              8A
     D DUPERRMSG       S            100A
     D TESTSTRING      S           1000A   VARYING

     D dCardNumParam   S             16  0
     D dCvvParam       S              3  0

     D PGMSTATUS      SDS
     D  PGMNAME          *PROC
     D  USERID               254    263

      /COPY QCPYSRC,COPYUTILPR
      /COPY QCPYSRC,COPYQSNDPG

     D/EJECT

     C     *ENTRY        PLIST
     C                   PARM                    PCARDNUM         16 0
     C                   PARM                    PVIEWONLY         1

      *-------------------------------------------------------------------*
      * MAIN PROCESSING
      *-------------------------------------------------------------------*
     C                   IF        PVIEWONLY = *ON
     C                   EVAL      VIEWONLY = *ON
     C                   ELSE
     C                   EVAL      VIEWONLY = *OFF
     C                   ENDIF

     C                   IF        PCARDNUM = *ZEROS
     C                   EVAL      ADDMODE = *ON
     C                   EXSR      $GENCARD
     C                   ELSE
     C                   EXSR      $FILL
     C                   ENDIF

     C                   EVAL      DPGMNAME = PGMNAME
     C                   EVAL      DPGMQ = PGMNAME

     C                   DOW       0 = 0

     C                   WRITE     MSGSFLC
     C                   EXFMT     SCRN1

     C                   SELECT

     C                   WHEN      F2 AND ADDMODE
     C                   EXSR      $CLEARSCREEN

     C                   WHEN      F3 OR F12
     C                   LEAVE

     C                   WHEN      F9 AND NOT ADDMODE
     C                   EVAL      dCardNumParam = DCARDNUM
     C                   CALL      'CDCCA01R'
     C                   PARM                    dCardNumParam
     C                   EVAL      DCARDNUM = dCardNumParam

     C                   WHEN      F4 AND NOT ADDMODE
     C                   EXSR      $PROMPT

     C                   WHEN      COMMANDKEY
     C                   ITER

     C                   WHEN      VIEWONLY
     C                   LEAVE

     C                   OTHER
     C                   EXSR      $CLRINDSMSG
     C                   IF        ADDMODE
     C                   EXSR      $ADD
     C                   ELSE
     C                   EXSR      $UPDATE
     C                   ENDIF
     C                   IF        MESSAGEID = *BLANKS AND (NOT F12 OR NOT F3)
     C                   LEAVE
     C                   ENDIF
     C                   ENDSL

     C                   ENDDO

     C                   SETON                                        LR

      *-------------------------------------------------------------------*
      * $FILL - POPULATE SCREEN
      *-------------------------------------------------------------------*
     C     $FILL         BEGSR

     C     PCARDNUM      CHAIN(N)  CDCCMU01
     C                   EVAL      DCARDNUM = CARDNUM
     C                   EVAL      DCUSTID = %TRIM(%EDITC(CUSTID:'X'))
     C                   EVAL      DACCTNUM = %TRIM(%EDITC(ACCTNUM:'X'))
     C                   EVAL      DSTATUS = STATUS
     C                   EXSR      $STATUSDES
     C                   EVAL      DNAME = %TRIM(NAME)
     C                   MOVE      DNAME         WNAME            25
     C                   EVAL      DEXPIRYMO = %EDITC(EXPIRYMO:'X')
     C                   EVAL      DEXPIRYYR = %EDITC(EXPIRYYR:'X')
     C                   EVAL      DPRIMARYSEC = PRIMARYSEC
     C                   EVAL      DCVV = %EDITC(CVV:'X')
      *
     C                   IF        CCMCRTDT <> *ZEROS
     C                   EVAL      DCCMCRTDT = %DEC(%CHAR(%DATE(CCMCRTDT:*CYMD)
     C                             :*ISO0):8:0)
     C                   ELSE
     C                   EVAL      DCCMCRTDT = *ZEROS
     C                   ENDIF
     C                   EVAL      DCCMCRTTM = CCMCRTTM
     C                   EVAL      DCCMCRTUSR = CCMCRTUSR
     C                   EVAL      DCCMCRTPGM = CCMCRTPGM

     C                   IF        CCMCHGDT <> *ZEROS
     C                   EVAL      DCCMCHGDT = %DEC(%CHAR(%DATE(CCMCHGDT:*CYMD)
     C                             :*ISO0):8:0)
     C                   EVAL      HIDECHGTM = *OFF
     C                   ELSE
     C                   EVAL      DCCMCHGDT = *ZEROS
     C                   EVAL      HIDECHGTM = *ON
     C                   ENDIF
     C                   EVAL      DCCMCHGTM = CCMCHGTM
     C                   EVAL      DCCMCHGUSR = CCMCHGUSR
     C                   EVAL      DCCMCHGPGM = CCMCHGPGM
     C                   ENDSR

      *-------------------------------------------------------------------*
      * $FILLTABLE - POPULATE DB TABLE FIELDS
      *-------------------------------------------------------------------*
     C     $FILLTABLE    BEGSR

     C                   EVAL      CARDNUM = DCARDNUM
     C                   EVAL      CUSTID = %DEC(DCUSTID:9:0)
     C                   EVAL      ACCTNUM = %DEC(DACCTNUM:12:0)
     C                   EVAL      STATUS = DSTATUS
     C                   EVAL      NAME = %TRIM(DNAME)
     C                   EVAL      EXPIRYMO = %DEC(DEXPIRYMO:2:0)
     C                   EVAL      EXPIRYYR = %DEC(DEXPIRYYR:4:0)
     C                   EVAL      PRIMARYSEC = DPRIMARYSEC
     C                   EVAL      CVV = %DEC(DCVV:3:0)

     C                   ENDSR
      *-------------------------------------------------------------------*
      * $CLEARSCREEN - CLEAR INPUT FIELDS
      *-------------------------------------------------------------------*
     C     $CLEARSCREEN  BEGSR

     C                   CLEAR                   DCUSTID
     C                   CLEAR                   DACCTNUM
     C                   CLEAR                   DNAME
     C                   CLEAR                   DPRIMARYSEC

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $CLRINDSMSG - CLEAR ERROR INDICATORS AND MESSAGE
      *-------------------------------------------------------------------*
     C     $CLRINDSMSG   BEGSR

     C                   CALL      'CDUTL02C'
     C                   EVAL      MESSAGEID = *BLANKS
     C                   EVAL      CUSTIDERR = *OFF
     C                   EVAL      ACCTERR = *OFF
     C                   EVAL      NAMEERR = *OFF
     C                   ENDSR

      *-------------------------------------------------------------------*
      * $ADD - ADD A CARD
      *-------------------------------------------------------------------*
     C     $ADD          BEGSR
     C
     C                   EXSR      $CHECKINPUT
     C                   IF        MESSAGEID <> *BLANKS
     C                   EXSR      $MESSAGE
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      WACCTNUM = %DEC(DACCTNUM:12:0)

     C     WACCTNUM      CHAIN     CDACTR01
     C                   IF        %FOUND(CDACTR01)
     C                   IF        ACTIVE = 'I'
     C                   EVAL      MESSAGEID = 'ERR0069'
     C                   EVAL      ACCTERR = *ON
     C                   EXSR      $MESSAGE
     C                   LEAVESR
     C                   ENDIF
     C                   IF        WCUSTID = PRICUSTID
     C                   EVAL      DPRIMARYSEC = 'P'
     C                   ELSEIF    WCUSTID = SECCUSTID
     C                   EVAL      DPRIMARYSEC = 'S'
     C                   ELSE
     C                   EVAL      MESSAGEID = 'ERR0067'
     C                   EVAL      ACCTERR = *ON
     C                   EXSR      $MESSAGE
     C                   LEAVESR
     C                   ENDIF
     C                   ELSE
     C                   EVAL      MESSAGEID = 'ERR0002'
     C                   EVAL      ACCTERR = *ON
     C                   EXSR      $MESSAGE
     C                   LEAVESR
     C                   ENDIF

     C     CCMR2KLST     SETLL     CDCCMR02
     C                   IF        %EQUAL(CDCCMR02)
     C                   EVAL      CUSTIDERR = *ON
     C                   EVAL      ACCTERR = *ON
     C                   EVAL      MESSAGEID = 'CPF9897'

         DUPERRMSG = 'Credit Card ' + %EDITC(DCARDNUM:'X')+ ' is already' +
                      ' attached to this customer account.' ;
         QMHSNDPM(MESSAGEID : 'QCPFMSG   *LIBL' : DUPERRMSG :
           %LEN(%TRIMR(DUPERRMSG)) : '*INFO' : '*' : 0 : '    ' : ERRORCDE);

     C                   LEAVESR
     C                   ENDIF

     C                   DOW       0 = 0
     C                   EVAL      MESSAGEID = 'MSG0008'
     C                   EXSR      $MESSAGE
     C                   EVAL      SAVEPROTECT = *ON
     C                   WRITE     MSGSFLC
     C                   EXFMT     SCRN1
     C                   EVAL      MESSAGEID = *BLANKS
     C                   IF        F3 OR F12
     C                   EVAL      SAVEPROTECT = *OFF
     C                   LEAVESR
     C                   ELSEIF    COMMANDKEY
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

     C                   DOW       1 = 1
     C                   EXFMT     CONFIRM
     C                   IF        F12
     C                   LEAVESR
     C                   ELSEIF    COMMANDKEY
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C
     C                   EXSR      $FILLTABLE
     C                   EVAL      CCMCRTDT = %DEC(%DATE:*CYMD)
     C                   EVAL      CCMCRTDT = %DEC(%DATE:*CYMD)
     C                   EVAL      CCMCRTTM = %DEC(%TIME)
     C                   EVAL      CCMCRTUSR = USERID
     C                   EVAL      CCMCRTPGM = PGMNAME
     C                   WRITE     CDCCMPR

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $UPDATE - UPDATE AN EXISTING CARD
      *-------------------------------------------------------------------*
     C     $UPDATE       BEGSR

     C                   IF        WNAME = DNAME
     C                   EVAL      MESSAGEID = '~~~~~'
     C                   LEAVESR
     C                   ENDIF

     C                   EXSR      $CHECKINPUT
     C                   IF        MESSAGEID <> *BLANKS
     C                   EXSR      $MESSAGE
     C                   LEAVESR
     C                   ENDIF

     C                   DOW       1 = 1
     C                   EXFMT     CONFIRM
     C                   IF        F12
     C                   EVAL      MESSAGEID = '~~~~~'
     C                   LEAVESR
     C                   ELSEIF    COMMANDKEY
     C                   ITER
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

     C     PCARDNUM      CHAIN     CDCCMU01
     C                   EXSR      $FILLTABLE
     C                   EVAL      CCMCHGDT = %DEC(%DATE:*CYMD)
     C                   EVAL      CCMCHGTM = %DEC(%TIME)
     C                   EVAL      CCMCHGUSR = USERID
     C                   EVAL      CCMCHGPGM = PGMNAME
     C                   UPDATE    CDCCMPR

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $CHECKINPUT - VALIDATE USER INPUT
      *-------------------------------------------------------------------*
     C     $CHECKINPUT   BEGSR

     C                   IF        ADDMODE
     C                   EVAL      DNAME = *BLANKS
     C                   EVAL      DPRIMARYSEC = *BLANKS

     C                   TESTN                   DCUSTID                  98
     C                   IF        *IN98 = *ON
     C                   EVAL      MESSAGEID = 'ERR0050'
     C                   EVAL      CUSTIDERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      TESTSTRING = %TRIM(DCUSTID)
     C                   IF        NOT ISNUMERIC(TESTSTRING)
     C                   EVAL      MESSAGEID = 'ERR0051'
     C                   EVAL      CUSTIDERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   IF        %DEC(TESTSTRING:9:0) = 0
     C                   EVAL      MESSAGEID = 'ERR0050'
     C                   EVAL      CUSTIDERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C
     C                   EVAL      WCUSTID = %DEC(DCUSTID:9:0)
     C     WCUSTID       CHAIN     CDCUSR01
     C                   IF        %FOUND(CDCUSR01)
     C                   EXSR      $NAME
     C                   ELSE
     C                   EVAL      MESSAGEID = 'ERR0009'
     C                   EVAL      CUSTIDERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   TESTN                   DACCTNUM                 98
     C                   IF        *IN98 = *ON
     C                   EVAL      MESSAGEID = 'ERR0052'
     C                   EVAL      ACCTERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   EVAL      TESTSTRING = %TRIM(DACCTNUM)
     C                   IF        NOT ISNUMERIC(TESTSTRING)
     C                   EVAL      MESSAGEID = 'ERR0053'
     C                   EVAL      ACCTERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   IF        %DEC(TESTSTRING:12:0) = 0
     C                   EVAL      MESSAGEID = 'ERR0052'
     C                   EVAL      ACCTERR = *ON
     C                   LEAVESR
     C                   ENDIF
     C                   LEAVESR
     C                   ENDIF

     C                   IF        DNAME = *BLANKS
     C                   EVAL      MESSAGEID = 'ERR0055'
     C                   EVAL      NAMEERR = *ON
     C                   LEAVESR
     C                   ENDIF

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $GENCARD - GENERATE A CREDIT CARD NUMBER AT RANDOM
      *-------------------------------------------------------------------*
     C     $GENCARD      BEGSR
     C
     C                   EVAL      dCvvParam = WCVV
     C                   CALL      'CDCCM03R'
     C                   PARM                    WCARDNUM
     C                   PARM                    dCvvParam
     C                   EVAL      WCVV = dCvvParam
     C                   MOVE      WCARDNUM      DCARDNUM
     C                   MOVE      WCVV          DCVV

     C                   EVAL      EXPIRYMO = %SUBDT(%DATE:*MONTHS)
     C                   EVAL      DEXPIRYMO = %EDITC(EXPIRYMO:'X')
     C                   EVAL      EXPIRYYR = %SUBDT(%DATE:*YEARS)
     C                   EVAL      EXPIRYYR = EXPIRYYR + 5
     C                   EVAL      DEXPIRYYR = %EDITC(EXPIRYYR:'X')
     C                   EVAL      DSTATUS = 'G'
     C                   EXSR      $STATUSDES

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $NAME - BUILD NAME
      *-------------------------------------------------------------------*
     C     $NAME         BEGSR

           IF MIDNAME <> *BLANKS;
             DNAME = %TRIM(FIRSTNAME) + ' ' + %TRIM(MIDNAME) + ' ' +
                     %TRIM(LASTNAME);
             IF %LEN(%TRIM(DNAME)) > 25;
               DNAME = %TRIM(FIRSTNAME) + ' ' + %TRIM(%SUBST(MIDNAME:1:1))
                       + ' ' + %TRIM(LASTNAME);
               IF %LEN(%TRIM(DNAME)) > 25;
                 DNAME = %TRIM(FIRSTNAME) + ' ' + %TRIM(LASTNAME);
               ENDIF;
             ENDIF;
           ELSE;
             DNAME = %TRIM(FIRSTNAME) + ' ' + %TRIM(LASTNAME);
           ENDIF;

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $PROMPT - CALL PROMPT SCREEN
      *-------------------------------------------------------------------*
     C     $PROMPT       BEGSR
     C                   IF        DCSRFLD = 'CUSTID'
     C                   CALL      'CDCUS02R'
     C                   PARM                    CUSTID
     C                   PARM                    ALWAYSON

     C                   ELSEIF    DCSRFLD = 'ACCTNUM'
     C                   CALL      'CDACT02R'
     C                   PARM                    ACCTNUM
     C                   PARM                    ALWAYSON
     C                   ENDIF

     C                   ENDSR

      *-------------------------------------------------------------------*
      * $STATUSDES - STATUS DESCRIPTIONS
      *-------------------------------------------------------------------*
     C     $STATUSDES    BEGSR
     C                   IF        DSTATUS = 'G'
     C                   EVAL      DSTATUSDESS = 'Generated New'
     C                   ELSEIF    DSTATUS = 'A'
     C                   EVAL      DSTATUSDESS = 'Active'
     C                   ELSEIF    DSTATUS = 'H'
     C                   EVAL      DSTATUSDESS = 'Hold'
     C                   ELSEIF    DSTATUS = 'D'
     C                   EVAL      DSTATUSDESS = 'Deactivated'
     C                   ELSE
     C                   EVAL      DSTATUSDESS = ' '
     C                   ENDIF
     C                   ENDSR

      *-------------------------------------------------------------------*
      * $MESSAGE - SEND MESSAGE TO MESSAGE SUBFILE
      *-------------------------------------------------------------------*
     C     $MESSAGE      BEGSR
     C                   CALL      'CDUTL01C'
     C                   PARM                    MESSAGEID
     C                   PARM                    DPGMQ
     C                   ENDSR

      *-------------------------------------------------------------------*
      * KEYLIST
      *-------------------------------------------------------------------*
     C     CCMR2KLST     KLIST
     C                   KFLD                    WCUSTID
     C                   KFLD                    WACCTNUM

     C/EJECT
