     H BndDir('CDBNDDIR')
     H DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDCCMU01  UF   E           K Disk
     FCDCCM01D  CF   E             Workstn SFile(Sfl1:dSflRRN)
     F                                     IndDS(IndicatorDS)
     F                                     Prefix(d)

      * - - - - - - - - - - - - - - - *
      * Procedure Definitions
      * - - - - - - - - - - - - - - - *

     D LoadSfl         PR
     D LoadNext        PR
     D LoadPrev        PR
     D Refresh         PR
     D AddCard         PR
     D EditCard        PR
     D ViewCard        PR
     D DeactivateCard  PR
     D CardStatus      PR
     D HandleOptions   PR

     D dCardNumParam   S             16  0

      *** External Program Prototypes ***
     D CardDetail      PR                  ExtPgm('CDCCM02R')
     D                               16  0
     D                                 N

     D CardHistory     PR                  ExtPgm('CDCCA01R')
     D                               16  0

     D ShowMessage     PR                  ExtPgm('CDUTL01C')
     D                                8A   Const
     D                               10A

     D ClearMessages   PR                  ExtPgm('CDUTL02C')

      /COPY *LIBL/QCPYSRC,COPYPSDS
      /COPY *LIBL/QCPYSRC,COPYUTILPR

      * - - - - - - - - - - - - - - - *
      * Field Definitions
      * - - - - - - - - - - - - - - - *

     D IndicatorDS     DS
     D  F3                             N   Overlay(IndicatorDS:3)
     D  F5                             N   Overlay(IndicatorDS:5)
     D  F6                             N   Overlay(IndicatorDS:6)
     D  F12                            N   Overlay(IndicatorDS:12)
     D  PgDn                           N   Overlay(IndicatorDS:25)
     D  PgUp                           N   Overlay(IndicatorDS:26)
     D  SflReset                       N   Overlay(IndicatorDS:30)
     D  SflInit                        N   Overlay(IndicatorDS:31)
     D  SflEnd                         N   Overlay(IndicatorDS:33)
     D  CommandKey                     N   Overlay(IndicatorDS:50)
     D  PositionErr                    N   Overlay(IndicatorDS:81)
     D  SflEmpty                       N   Overlay(IndicatorDS:90)

     D wFirstKey       S                   Like(CardNum)
     D wLastKey        S                   Like(CardNum)
     D wPosition       S                   Like(dPosition)

     D cSflSize        C                   Const(13)

      * - - - - - - - - - - - - - - - *
      * Main Processing
      * - - - - - - - - - - - - - - - *

     C                   Eval      dPgmName = PROC_NAME
     C                   Eval      dPgmQ = PROC_NAME
     C                   CallP     ClearMessages

     C     *LoVal        SetLL     CDCCMU01
     C                   CallP     LoadSfl

     C                   Dow       0 = 0

     C                   Write     Header1
     C                   Write     Header2
     C                   Write     Footer
     C                   Write     MsgSflC
     C                   If        SflEmpty = *On
     C                   Write     Empty
     C                   EndIf
     C                   Exfmt     Ctl1
     C                   Read      Header2

     C                   Eval      PositionErr = *Off
     C                   CallP     ClearMessages

     C                   Select

     C                   When      F3 or F12
     C                   Leave

     C                   When      F5
     C                   Eval      wFirstKey = *LoVal
     C                   CallP     Refresh

     C                   When      F6
     C                   CallP     AddCard

     C                   When      PgUp
     C                   CallP     LoadPrev

     C                   When      PgDn
     C                   CallP     LoadNext

     C                   When      CommandKey
     C*                  //Ignore

     C                   Other
     C                   CallP     HandleEnter

     C                   EndSl

     C                   EndDo

     C                   Eval      *InLR = *On
     C                   Return

      * - - - - - - - - - - - - - - - *
      * LoadSfl
      * - - - - - - - - - - - - - - - *

     P LoadSfl         B
     D LoadSfl         PI

     C                   Eval      SflInit = *Off
     C                   Eval      SflReset = *On
     C                   Write     Ctl1
     C                   Eval      SflReset = *Off
     C                   Eval      SflEmpty = *Off
     C                   Eval      dSflRRN = 1

     C                   Read(N)   CDCCMU01
     C                   If        %EOF(CDCCMU01)
     C                   Eval      dSflRRN = 0
     C                   Eval      CARDNUM = *HiVal
     C                   EndIf
     C
     C                   Eval      wFirstKey = CARDNUM
     C                   Eval      wLastKey = *HiVal
     C                   Dow       Not %EOF(CDCCMU01) and dSflRRN <= cSflSize
     C                   Eval      dCardNum = CARDNUM
     C                   Eval      dCustId = %Trim(%EditC(CUSTID:'X'))
     C                   Eval      dAcctNum = %Trim(%EditC(ACCTNUM:'X'))
     C                   Eval      dPrimarySec = PRIMARYSEC
     C                   Eval      dStatus = STATUS
     C                   Eval      dOption = *Blank
     C                   Write     Sfl1
     C                   Read(N)   CDCCMU01
     C                   Eval      dSflRRN += 1
     C                   Eval      wLastKey = CARDNUM
     C                   EndDo

     C                   If        %EOF(CDCCMU01)
     C                   Eval      SflEnd = *On
     C                   Else
     C                   Eval      SflEnd = *Off
     C                   EndIf

     C                   If        SflEnd = *On
     C                   Eval      wLastKey = *HiVal
     C                   EndIf

     C                   If        dSflRRN = *Zero
     C                   Eval      SflReset = *On
     C                   Eval      SflInit = *On
     C                   Eval      SflEmpty = *On
     C                   Else
     C                   Eval      SflInit = *Off
     C                   Eval      dSflRRN = 1
     C                   EndIf

     P LoadSfl         E

      * - - - - - - - - - - - - - - - *
      * LoadNext
      * - - - - - - - - - - - - - - - *

     P LoadNext        B
     D LoadNext        PI

     C                   If        SflEnd
     C                   Return
     C                   EndIf

     C     wLastKey      SetLL     CDCCMU01
     C                   CallP     LoadSfl

     P LoadNext        E

      * - - - - - - - - - - - - - - - *
      * LoadPrev
      * - - - - - - - - - - - - - - - *

     P LoadPrev        B

     D LoadPrev        PI

     D i               S              2  0
     D SflTop          S              1    Inz

     C     wFirstKey     SetLL     CDCCMU01
     C                   For       i = 0 By 1 to cSflSize
     C                   ReadP(N)  CDCCMU01
     C                   If        %EOF(CDCCMU01)
     C     *LoVal        SetLL     CDCCMU01
     C                   If        i = 0
     C                   Eval      SflTop = 'Y'
     C                   EndIf
     C                   Leave
     C                   EndIf
     C                   EndFor

     C                   If        SflTop = ' '
     C                   CallP     LoadSfl
     C                   EndIf

     P LoadPrev        E

      * - - - - - - - - - - - - - - - *
      * Refresh
      * - - - - - - - - - - - - - - - *

     P Refresh         B

     C                   Clear                   dPosition
     C     wFirstKey     SetLL     CDCCMU01
     C                   CallP     LoadSfl

     P Refresh         E

      * - - - - - - - - - - - - - - - *
      * HandleEnter
      * - - - - - - - - - - - - - - - *

     P HandleEnter     B
     D HandleEnter     PI

     C                   Eval      wPosition = dPosition
     C                   Z-Add     0             ErrSrcCtl         1 0
     C                   If        wPosition <> *Blanks
     C                   CallP     ValSrcCtl(ErrSrcCtl)
     C                   EndIf
     C                   If        ErrSrcCtl = 0
     C                   CallP     SflOptions
     C                   If        wPosition <> *Blanks
     C                   CallP     PositionTo
     C                   EndIf
     C                   EndIf

     P HandleEnter     E

      * - - - - - - - - - - - - - - - *
      * Validate Search Ctl
      * - - - - - - - - - - - - - - - *

     P ValSrcCtl       B

     D ValSrcCtl       PI
     D  ErrSrcCtl                     1  0

     D wCleanValue     S                   Like(dPosition)
     D wTestString     S           1000A   Varying
     D temp            S             19  0
     D Error           S              1

     C                   Eval      dRow = 0
     C                   Eval      dCol = 0
     C                   Eval      wCleanValue = %ScanRpl('-':'':dPosition)
     C                   Eval      wTestString = wCleanValue
     C                   If        IsNumeric(wTestString)
     C                   Monitor
     C                   Eval      temp = %Dec(wCleanValue:19:0)
     C                   On-Error
     C                   Eval      Error = 'Y'
     C                   EndMon
     C                   Else
     C                   Eval      Error = 'Y'
     C                   EndIf
     C
     C                   If        Error = 'Y'
     C                   CallP     ShowMessage('ERR0016':dPgmQ)
     C                   Eval      dRow = 5
     C                   Eval      dCol = 5
     C                   Eval      PositionErr = *On
     C                   Eval      ErrSrcCtl = 1
     C                   Return
     C                   EndIf

     P ValSrcCtl       E

      * - - - - - - - - - - - - - - - *
      * PositionTo
      * - - - - - - - - - - - - - - - *

     P PositionTo      B

     D PositionTo      PI

     D wCleanValue     S                   Like(dPosition)

     C                   Eval      wCleanValue = %ScanRpl('-':'':wPosition)
     C                   If        %Dec(wCleanValue:19:0) > 9999999999999999
     C                   Eval      wFirstKey = *HiVal
     C                   Else
     C                   Eval      wFirstKey = %Dec(wCleanValue:16:0)
     C                   EndIf
     C                   CallP     Refresh

     P PositionTo      E

      * - - - - - - - - - - - - - - - *
      * SflOptions
      * - - - - - - - - - - - - - - - *

     P SflOptions      B

     D SflOptions      PI

     C                   ReadC     Sfl1
     C                   Dow       Not %EOF()
     C                   CallP     HandleOptions
     C                   ReadC     Sfl1
     C                   EndDo

     C                   CallP     Refresh

     P SflOptions      E

      * - - - - - - - - - - - - - - - *
      * AddCard
      * - - - - - - - - - - - - - - - *

     P AddCard         B

     D AddCard         PI

     D wCardNumber     S             16  0 Inz(*Zeros)
     D wViewOnly       S               N   Inz(*Off)

     C                   CallP     CardDetail(wCardNumber:wViewOnly)
     C                   CallP     Refresh

     P AddCard         E

      * - - - - - - - - - - - - - - - *
      * EditCard
      * - - - - - - - - - - - - - - - *

     P EditCard        B

     D EditCard        PI

     D wCardNumber     S             16  0
     D wViewOnly       S               N   Inz(*Off)

     C                   Eval      wCardNumber = dCardNum
     C                   CallP     CardDetail(wCardNumber:wViewOnly)

     P EditCard        E

      * - - - - - - - - - - - - - - - *
      * DeactivateCard
      * - - - - - - - - - - - - - - - *

     P DeactivateCard  B

     D DeactivateCard  PI

     C                   Dow       Not F12
     C                   Write     Header1
     C                   Exfmt     DelWin
     C                   If        CommandKey
     C                   Iter
     C                   Else
     C     dCardNum      Chain(E)  CDCCMU01
     C                   If        Not %Error()
     C                   Eval      STATUS = 'D'
     C                   Eval      CCMCHGDT = %Dec(%Date():*CYMD)
     C                   Eval      CCMCHGTM = %Dec(%Time)
     C                   Eval      CCMCHGUSR = USER
     C                   Eval      CCMCHGPGM = PROC_NAME
     C                   Update    CDCCMPR
     C                   Else
     C                   Iter
     C                   EndIf
     C                   Leave
     C                   EndIf
     C                   EndDo

     P DeactivateCard  E

      * - - - - - - - - - - - - - - - *
      * ViewCard
      * - - - - - - - - - - - - - - - *

     P ViewCard        B

     D ViewCard        PI

     D wCardNumber     S             16  0
     D wViewOnly       S               N   Inz(*On)

     C                   Eval      wCardNumber = dCardNum
     C                   CallP     CardDetail(wCardNumber:wViewOnly)

     P ViewCard        E

      * - - - - - - - - - - - - - - - *
      * CardStatus
      * - - - - - - - - - - - - - - - *

     P CardStatus      B
     D wCardNum        S             16  0

     C                   If        dStatus = 'G' or dStatus = 'H'
     C                   Eval      dUpdateType = 'Activate Card'
     C                   ElseIf    dStatus = 'A'
     C                   Eval      dUpdateType = 'Put Card on Hold'
     C                   EndIf

     C                   Dow       Not F12
     C                   Write     Header1
     C                   Exfmt     StatusWin
     C                   If        CommandKey
     C                   Iter
     C                   Else
     C     dCardNum      Chain(E)  CDCCMU01
     C                   If        Not %Error()
     C                   If        STATUS = 'G' or STATUS = 'H'
     C                   Eval      STATUS = 'A'
     C                   ElseIf    STATUS = 'A'
     C                   Eval      STATUS = 'H'
     C                   EndIf
     C                   Eval      CCMCHGDT = %Dec(%Date():*CYMD)
     C                   Eval      CCMCHGTM = %Dec(%Time)
     C                   Eval      CCMCHGUSR = USER
     C                   Eval      CCMCHGPGM = PROC_NAME
     C                   Update    CDCCMPR
     C                   Leave
     C                   Else
     C                   Iter
     C                   EndIf
     C                   EndIf
     C                   EndDo

     P CardStatus      E

      * - - - - - - - - - - - - - - - *
      * HandleOptions
      * - - - - - - - - - - - - - - - *

     P HandleOptions   B

     D HandleOptions   PI

     C                   Select

     C                   When      dOption = '2'
     C                   If        dStatus = 'D'
     C                   Write     Header1
     C                   Exfmt     DelErrWin
     C                   Else
     C                   CallP     EditCard
     C                   EndIf

     C                   When      dOption = '4'
     C                   If        dStatus = 'D'
     C                   Write     Header1
     C                   Exfmt     DelErrWin
     C                   Else
     C                   CallP     DeactivateCard
     C                   EndIf

     C                   When      dOption = '5'
     C                   CallP     ViewCard

     C                   When      dOption = '7'
     C                   If        dStatus = 'D'
     C                   Write     Header1
     C                   Exfmt     DelErrWin
     C                   Else
     C                   CallP     CardStatus
     C                   EndIf

     C                   When      dOption = '9'
     C                   EVAL      dCardNumParam = dCardNum
     C                   CallP     CardHistory(dCardNumParam)
     C                   EVAL      dCardNum = dCardNumParam

     C                   EndSl

     P HandleOptions   E

