     H BndDir('CDBNDDIR')
     H DftActGrp(*No) ActGrp(*Caller)
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDActU01  UF A E           K Disk
     FCDCusR01  IF   E           K Disk
     FCDCCMR02  IF   E           K Disk
     FCDAct02D  CF   E             Workstn IndDS(IndDS)
     F                                     Prefix(d)

      /COPY *LIBL/QCPYSRC,COPYUTILPR

     D IndDS           DS
     D  F2                             N   Overlay(IndDS:2)
     D  F3                             N   Overlay(IndDS:3)
     D  F4                             N   Overlay(IndDS:4)
     D  F12                            N   Overlay(IndDS:12)
     D  CommandKey                     N   Overlay(IndDS:50)
     D  AddMode                        N   Overlay(IndDS:70)
     D  HighlightSts                   N   Overlay(IndDS:71)
     D  SecCustView                    N   Overlay(IndDS:72)
     D  ViewOnly                       N   Overlay(IndDS:80)
     D  AcctNumErr                     N   Overlay(IndDS:81)
     D  AcctGrpErr                     N   Overlay(IndDS:82)
     D  CredLimitErr                   N   Overlay(IndDS:83)
     D  CashLimitErr                   N   Overlay(IndDS:84)
     D  PriCustErr                     N   Overlay(IndDS:88)
     D  PriCustPosiTo                  N   Overlay(IndDS:51)
     D  SecCustErr                     N   Overlay(IndDS:89)
     D  SecCustPosiTo                  N   Overlay(IndDS:52)
     D  ExpDateErr                     N   Overlay(IndDS:90)
     D  ExpiryDateErr                  N   Overlay(IndDS:92)
     D  AlwaysOn                       N   Overlay(IndDS:99) Inz(*On)

     D PgmStatus      SDS
     D  PgmName          *PROC
     D  UserID               254    263

     D ErrorID         S              8A
     D TestString      S           1000A   Varying
     D TestNumeric     S             15  2
     D ItemsChanged    S               N
     D ScreenState     DS                  LikeRec(Scrn1:*Input)

     D CDACT02R        PI
     D  pAcctNum                           Like(AcctNum)
     D  pViewOnly                      N

      ****************************************************************
      * MAIN PROCESSING                                              *
      ****************************************************************

     C     K1#CCMR02     Klist
     C                   KFld                    SecCustID
     C                   KFld                    AcctNum

     C                   Eval      dPgmQ = PgmName
     C                   Eval      dPgmName = PgmName
     C                   Write     MsgSflC

     C                   If        pAcctNum = *Zeros and Not pViewOnly
     C                   Eval      AddMode = *On
     C                   ExSr      $ClearScreen
     C                   Else
     C                   Eval      ViewOnly = pViewOnly
     C                   ExSr      $FillScreen
     C                   If        pViewOnly = *Off
     C                   ExSr      $SaveState
     C                   EndIf
     C                   EndIf

     C                   Dow       1 = 1

     C                   Write     MsgSflC
     C                   Exfmt     Scrn1

     C                   Clear                   ErrorID
     C                   Call      'CDUTL02C'
     C                   ExSr      $ClearInds

     C                   Select
     C
     C                   When      F2
     C                   ExSr      $ClearScreen

     C                   When      F3 or F12
     C                   Leave
     C
     C                   When      F4 and ViewOnly
     C                   ExSr      $Prompt
     C
     C                   When      F4
     C                   ExSr      $PromptSel
     C
     C                   When      CommandKey
     C*                  Ignore
     C
     C                   Other
     C                   If        Not AddMode
     C                   ExSr      $CheckChange
     C                   If        Not ItemsChanged
     C                   Iter
     C                   EndIf
     C                   EndIf

     C                   If        AddMode
     C                   ExSr      $Add
     C                   If        ErrorID = *Blanks and Not F12
     C                   Leave
     C                   EndIf
     C                   ElseIf    Not ViewOnly
     C                   ExSr      $Update
     C                   EndIf

     C                   EndSl
     C                   EndDo

     C                   Eval      *InLR = *On
     C                   Return

      ****************************************************************
      * $SaveState - Save current state of screen inputs             *
      ****************************************************************

     C     $SaveState    BegSr

     C                   Eval      ScreenState.dAcctNum = dAcctNum
     C                   Eval      ScreenState.dAcctGrp = dAcctGrp
     C                   Eval      ScreenState.dExpiryDate = dExpiryDate
     C                   Eval      ScreenState.dCredLimit = dCredLimit
     C                   Eval      ScreenState.dCashLimit = dCashLimit
     C                   Eval      ScreenState.dPriCustID = dPriCustID
     C                   Eval      ScreenState.dSecCustID = dSecCustID

     C                   EndSr
      ****************************************************************
      * $CheckChange - Determine if items on screen have changed     *
      ****************************************************************

     C     $CheckChange  BegSr

     C                   If        ScreenState.dAcctNum <> dAcctNum
     C                             or ScreenState.dAcctGrp <> dAcctGrp
     C                             or ScreenState.dExpiryDate <> dExpiryDate
     C                             or ScreenState.dCredLimit <> dCredLimit
     C                             or ScreenState.dCashLimit <> dCashLimit
     C                             or ScreenState.dPriCustID <> dPriCustID
     C                             or ScreenState.dSecCustID <> dSecCustID
     C                   Eval      ItemsChanged = *On
     C                   EndIf
     C                   EndSr

      * $FillScreen - Populate screen fields                         *
      ****************************************************************
     C     $FillScreen   BegSr
     C                   Eval      AcctNum = pAcctNum
     C     AcctNum       Chain(N)  CDActU01
     C                   Eval      dAcctNum = %Trim(%EditC(AcctNum:'X'))
     C                   Eval      dAcctGrp = %Trim(AcctGrp)
     C                   Eval      dOpenDate = OpenDate
     C                   Eval      dExpiryDate = ExpiryDate
     C                   If        ACTIVE = 'A'
     C                   Eval      dStatus     = 'ACTIVE'
     C                   Eval      HighlightSts = *Off
     C                   ELSEIF    ACTIVE = 'I'
     C                   Eval      dStatus     = 'INACTIVE'
     C                   Eval      HighlightSts = *On
     C                   ENDIF

     C                   If        ViewOnly
     C                   If        CredLimit = *Zeros
     C                   Eval      dCredLimit = *Blanks
     C                   Else
     C                   Eval      dCredLimit = %Trim(%EditC(CredLimit:'N'))
     C                   EndIf
     C                   If        CashLimit = *Zeros
     C                   Eval      dCashLimit = *Blanks
     C                   Else
     C                   Eval      dCashLimit = %Trim(%EditC(CashLimit:'N'))
     C                   EndIf
     C                   If        Balance = *Zeros
     C                   Eval      dBalance = *Blanks
     C                   Else
     C                   Eval      dBalance = %Trim(%EditC(Balance:'N'))
     C                   EndIf
     C                   If        CurCredit = *Zeros
     C                   Eval      dCurCredit = *Blanks
     C                   Else
     C                   Eval      dCurCredit = %Trim(%EditC(CurCredit:'N'))
     C                   EndIf
     C                   If        CurDebit = *Zeros
     C                   Eval      dCurDebit = *Blanks
     C                   Else
     C                   Eval      dCurDebit = %Trim(%EditC(CurDebit:'N'))
     C                   EndIf
     C                   Else
     C                   If        CredLimit = *Zeros
     C                   Eval      dCredLimit = *Blanks
     C                   Else
     C                   Eval      dCredLimit = %Trim(%EditC(CredLimit:'3'))
     C                   EndIf
     C                   If        CashLimit = *Zeros
     C                   Eval      dCashLimit = *Blanks
     C                   Else
     C                   Eval      dCashLimit = %Trim(%EditC(CashLimit:'3'))
     C                   EndIf
     C                   If        Balance = *Zeros
     C                   Eval      dBalance = *Blanks
     C                   Else
     C                   Eval      dBalance = %Trim(%EditC(Balance:'3'))
     C                   EndIf
     C                   If        CurCredit = *Zeros
     C                   Eval      dCurCredit = *Blanks
     C                   Else
     C                   Eval      dCurCredit = %Trim(%EditC(CurCredit:'3'))
     C                   EndIf
     C                   If        CurDebit = *Zeros
     C                   Eval      dCurDebit = *Blanks
     C                   Else
     C                   Eval      dCurDebit = %Trim(%EditC(CurDebit:'3'))
     C                   EndIf
     C                   EndIf

     C                   Eval      dPriCustID = %Trim(%EditC(PriCustID:'X'))

     C                   If        SecCustID = *Zeros
     C                   Eval      dSecCustID = *Blanks
     C                   Else
     C                   Eval      dSecCustID = %Trim(%EditC(SecCustID:'X'))
     C                   EndIf

     C                   If        ViewOnly
     C                   Eval      SecCustView = *On
     C                   Else
     C     K1#CCMR02     SetLL     CDCCMR02
     C                   If        %Equal(CDCCMR02)
     C                   Eval      SecCustView = *On
     C                   Else
     C                   Eval      SecCustView = *Off
     C                   EndIf
     C                   EndIf

     C                   If        ActCrtDt <> *Zeros
     C                   Eval      dActCrtDt =
     C                              %Dec(%Char(%Date(ActCrtDt:*CYMD):*ISO0):8:0)
     C                   EndIf
     C                   Eval      dActCrtTm = ActCrtTm
     C                   Eval      dActCrtUsr = ActCrtUsr
     C                   Eval      dActCrtPgm = ActCrtPgm
     C                   If        ActChgDt <> *Zeros
     C                   Eval      dActChgDt =
     C                              %Dec(%Char(%Date(ActChgDt:*CYMD):*ISO0):8:0)
     C                   EndIf
     C                   Eval      dActChgTm = ActChgTm
     C                   Eval      dActChgUsr = ActChgUsr
     C                   Eval      dActChgPgm = ActChgPgm
     C                   EndSr

      ****************************************************************
      * $ClearScreen - Clear all input fields
      ****************************************************************
     C     $ClearScreen  BegSr
     C                   If        AddMode
     C                   Eval      dAcctNum = *Blanks
     C                   Eval      dAcctGrp = *Blanks
     C                   Eval      dOpenDate = %Date
     C                   Eval      dExpiryDate = '2099-12-31'
     C                   Eval      dCredLimit = *Blanks
     C                   Eval      dCashLimit = *Blanks
     C                   Eval      dBalance = *Blanks
     C                   Eval      dCurCredit = *Blanks
     C                   Eval      dCurDebit = *Blanks
     C                   Eval      dstatus   = 'ACTIVE'
     C                   Else
     C                   ExSr      $FillScreen
     C                   EndIf

     C                   EndSr

      ****************************************************************
      * $ClearInds - Reset display indicators
      ****************************************************************
     C     $ClearInds    BegSr

     C                   Eval      AcctNumErr = *Off
     C                   Eval      ExpiryDateErr = *Off
     C                   Eval      AcctGrpErr = *Off
     C                   Eval      CredLimitErr = *Off
     C                   Eval      CashLimitErr = *Off
     C                   Eval      PriCustErr = *Off
     C                   Eval      PriCustPosiTo = *Off
     C                   Eval      SecCustErr = *Off
     C                   Eval      SecCustPosiTo = *Off
     C                   Eval      ItemsChanged = *Off

     C                   EndSr

      ****************************************************************
      * $Update - Update records in the account file
      ****************************************************************
     C     $Update       BegSr

     C                   ExSr      $CheckInputs
     C                   If        ErrorID <> *Blanks
     C                   ExSr      $Error
     C                   LeaveSR
     C                   Else

     C                   Dow       Not F12
     C                   Exfmt     Confirm
     C                   Select
     C                   When      F12
     C                   LeaveSR
     C                   When      CommandKey
     C                   Iter
     C                   Other
     C                   Leave
     C                   EndSl

     C                   EndDo
     C                   EndIf

     C     AcctNum       Chain     CDActU01
     C                   If        %Found(CDActU01)
     C                   Eval      AcctNum = %Dec(dAcctNum:12:0)
     C                   Eval      AcctGrp = dAcctGrp
     C                   Eval      OpenDate = dOpenDate
     C                   Eval      ExpiryDate = dExpiryDate
     C                   Eval      CredLimit = %Dec(dCredLimit:9:2)

     C                   If        dCashLimit <> *Blanks
     C                   Eval      CashLimit = %Dec(dCashLimit:9:2)
     C                   Else
     C                   Eval      CashLimit = *Zeros
     C                   EndIf

     C                   If        dBalance <> *Blanks
     C                   Eval      Balance = %Dec(dBalance:9:2)
     C                   Else
     C                   Eval      Balance = *Zeros
     C                   EndIf

     C                   If        dCurCredit <> *Blanks
     C                   Eval      CurCredit = %Dec(dCurCredit:9:2)
     C                   Else
     C                   Eval      CurCredit = *Zeros
     C                   EndIf

     C                   If        dCurDebit <> *Blanks
     C                   Eval      CurDebit = %Dec(dCurDebit:9:2)
     C                   Else
     C                   Eval      CurDebit = *Zeros
     C                   EndIf

     C                   Eval      PriCustID = %Dec(dPriCustID:9:0)

     C                   If        dSecCustID <> *Blanks
     C                   Eval      SecCustID = %Dec(dSecCustID:9:0)
     C                   Else
     C                   Eval      SecCustID = *Zeros
     C                   EndIf

     C                   Eval      ActChgDt = %Dec(%Date():*CYMD)
     C                   Eval      ActChgTm = %Dec(%Time)
     C                   Eval      ActChgUsr = UserID
     C                   Eval      ActChgPgm = PgmName
     C                   Eval      dActChgDt =
     C                              %Dec(%Char(%Date(ActChgDt:*CYMD):*ISO0):8:0)
     C                   Eval      dActChgTm = ActChgTm
     C                   Eval      dActChgUsr = ActChgUsr
     C                   Eval      dActChgPgm = ActChgPgm
     C                   Update    CDActPR
     C                   Eval      ErrorID = 'MSG0004'
     C                   ExSr      $Error
     C                   Else
     C                   Eval      ErrorID = 'UNK0001'
     C                   ExSr      $Error
     C                   EndIf

     C                   EndSr

     C*************************************************************************
     C* $Add - Write a new record to the account table                        *
     C*************************************************************************

     C     $Add          BegSr

     C                   ExSr      $FillTable

     C                   If        ErrorID = *Blanks
     C                   Dow       Not F12
     C                   Exfmt     Confirm
     C                   If        Not CommandKey
     C                   Eval      ActCrtDt = %Dec(%Date():*CYMD)
     C                   Eval      ActCrtTm = %Dec(%Time)
     C                   Eval      ActCrtUsr = UserID
     C                   Eval      ActCrtPgm = PgmName
     C                   Eval      ActChgDt = %Dec(%Date():*CYMD)
     C                   Eval      ActChgTm = %Dec(%Time)
     C                   Eval      ActChgUsr = UserID
     C                   Eval      ActChgPgm = PgmName
     C                   Write     CDActPR
     C                   Leave
     C                   EndIf
     C                   EndDo
     C                   EndIf

     C                   EndSr

     C*************************************************************************
     C* $FillTable - Populate table fields                                    *
     C*************************************************************************

     C     $FillTable    BegSr

     C                   ExSr      $CheckInputs
     C                   If        ErrorID <> *Blanks
     C                   ExSr      $Error
     C                   LeaveSR
     C                   EndIf

     C                   Eval      OpenDate = dOpenDate
     C                   Eval      ExpiryDate = dExpiryDate
     C                   Eval      ACTIVE     = 'A'

     C                   EndSr

     C*************************************************************************
     C* $CheckInputs - Validate values entered                                *
     C*************************************************************************

     C     $CheckInputs  BegSr

     C                   If        dAcctNum = *Blanks
     C                   Eval      AcctNumErr = *On
     C                   Eval      ErrorID = 'ERR0006'
     C                   LeaveSR
     C                   EndIf

     C                   Eval      TestString = %Trim(dAcctNum)
     C                   If        IsNumeric(TestString)
     C                   Eval      AcctNum = %Dec(dAcctNum:15:0)
     C                   Else
     C                   Call      'CDUTL02C'
     C                   Eval      AcctNumErr = *On
     C                   Eval      ErrorID = 'ERR0004'
     C                   LeaveSR
     C                   EndIf

     C                   If        AddMode
     C     AcctNum       SetLL     CDActU01
     C                   If        %Equal(CDActU01)
     C                   Eval      AcctNumErr = *On
     C                   Eval      ErrorID = 'ERR0033'
     C                   LeaveSR
     C                   EndIf
     C                   EndIf

     C                   If        dExpiryDate < %Date()
     C                             and dExpiryDate <> ExpiryDate
     C                   Eval      ExpiryDateErr = *On
     C                   Eval      ErrorID = 'ERR0018'
     C                   LeaveSR
     C                   EndIf

     C                   If        dPriCustID = *Blanks
     C                   Eval      PriCustErr = *On
     C                   Eval      PriCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0008'
     C                   LeaveSR
     C                   EndIf

     C                   Eval      TestString = %Trim(dPriCustID)
     C                   If        IsNumeric(TestString)
     C                   Eval      PriCustID = %Dec(dPriCustID:9:0)
     C                   Else
     C                   Call      'CDUTL02C'
     C                   Eval      PriCustErr = *On
     C                   Eval      PriCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0004'
     C                   LeaveSR
     C                   EndIf

     C                   If        dSecCustID <> *Blanks
     C                   Eval      TestString = %Trim(dSecCustID)
     C                   If        IsNumeric(TestString)
     C                   Eval      SecCustID = %Dec(dSecCustID:9:0)
     C                   Else
     C                   Call      'CDUTL02C'
     C                   Eval      SecCustErr = *On
     C                   Eval      SecCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0004'
     C                   LeaveSR
     C                   EndIf
     C                   Else
     C                   Eval      SecCustID = *Zeros
     C                   EndIf

     C                   If        dSecCustID <> *Blanks
     C                   If        %Dec(dPriCustID:9:0) = %Dec(dSecCustID:9:0)
     C                   Eval      PriCustErr = *On
     C                   Eval      PriCustPosiTo = *On
     C                   Eval      SecCustErr = *On
     C                   Eval      SecCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0066'
     C                   LeaveSR
     C                   EndIf
     C                   EndIf

     C     PriCustID     Chain     CDCusR01
     C                   If        Not %Found(CDCusR01)
     C                   Eval      PriCustErr = *On
     C                   Eval      PriCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0009'
     C                   LeaveSR
     C                   EndIf

     C                   If        dSecCustID <> *Blanks
     C     SecCustID     Chain     CDCusR01
     C                   If        Not %Found(CDCusR01)
     C                   Eval      SecCustErr = *On
     C                   Eval      SecCustPosiTo = *On
     C                   Eval      ErrorID = 'ERR0009'
     C                   LeaveSR
     C                   EndIf
     C                   EndIf

     C                   If        dCredLimit = *Blanks
     C                   Eval      CredLimitErr = *On
     C                   Eval      ErrorID = 'ERR0007'
     C                   LeaveSR
     C                   EndIf

     C                   Eval      TestString = %Trim(dCredLimit)
     C                   If        IsCurrency(TestString)
     C                   Eval      TestNumeric = %Dec(dCredLimit:15:2)
     C                   If        TestNumeric > 9999999.99
     C                   Eval      ErrorID = 'ERR0020'
     C                   Eval      CredLimitErr = *On
     C                   LeaveSR
     C                   EndIf
     C                   Eval      CredLimit = %Dec(dCredLimit:9:2)
     C                   Else
     C                   Call      'CDUTL02C'
     C                   Eval      CredLimitErr = *On
     C                   Eval      ErrorID = 'ERR0015'
     C                   LeaveSR
     C                   EndIf

     C                   If        dCashLimit <> *Blanks
     C                   Eval      TestString = %Trim(dCashLimit)
     C                   If        IsCurrency(TestString)
     C                   Eval      TestNumeric = %Dec(dCashLimit:15:2)
     C                   If        TestNumeric > 9999999.99
     C                   Eval      ErrorID = 'ERR0020'
     C                   Eval      CashLimitErr = *On
     C                   LeaveSR
     C                   EndIf
     C                   Eval      CashLimit = %Dec(dCashLimit:9:2)
     C                   Else
     C                   Call      'CDUTL02C'
     C                   Eval      CashLimitErr = *On
     C                   Eval      ErrorID = 'ERR0015'
     C                   LeaveSR
     C                   EndIf
     C                   Else
     C                   Eval      CashLimit = *Zeros
     C                   EndIf

     C                   If        CashLimit > CredLimit
     C                   Eval      CashLimitErr = *On
     C                   Eval      ErrorID = 'ERR0017'
     C                   LeaveSR
     C                   EndIf
     C                   Eval      AcctGrp = dAcctGrp

     C                   EndSr

     C*************************************************************************
     C* $Prompt - Call Prompt Screen                                          *
     C*************************************************************************
     C
     C     $Prompt       BegSr

     C                   If        DCSRFLD = 'PRICUSTID' And
     C                             dPRICUSTID <> *Blanks
     C                   Call      'CDCUS02R'
     C                   Parm                    PriCustID
     C                   Parm                    AlwaysOn
     C                   Eval      PriCustPosiTo = *On
     C                   ElseIf    DCSRFLD = 'SECCUSTID' And
     C                             dSECCUSTID <> *Blanks
     C                   Call      'CDCUS02R'
     C                   Parm                    SecCustID
     C                   Parm                    AlwaysOn
     C                   Eval      SecCustPosiTo = *On
     C                   EndIf

     C                   EndSr

     C*************************************************************************
     C* $PromptSel - Call Select Screen                                       *
     C*************************************************************************
     C
     C     $PromptSel    BegSr

     C                   If        DCSRFLD = 'PRICUSTID'
     C                   If        AddMode = *Off
     C                   If        dPRICUSTID <> *Blanks
     C                   Call      'CDCUS02R'
     C                   Parm                    PriCustID
     C                   Parm                    AlwaysOn
     C                   EndIf
     C                   Else
     C                   Call      'CDCUS03R'
     C                   Parm                    dPriCustID
     C                   Eval      PriCustPosiTo = *On
     C                   EndIf
     C                   ElseIf    DCSRFLD = 'SECCUSTID'
     C                   Call      'CDCUS03R'
     C                   Parm                    dSecCustID
     C                   Eval      SecCustPosiTo = *On
     C                   EndIf

     C                   EndSr

     C*************************************************************************
     C* $Error - Write to Message Subfile                                     *
     C*************************************************************************
     C
     C     $Error        BegSr
     C                   Call      'CDUTL01C'
     C                   Parm                    ErrorID
     C                   Parm                    dPgmQ
     C                   EndSr

