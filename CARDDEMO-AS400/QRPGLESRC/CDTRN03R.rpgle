     H BndDir('CDBNDDIR')
     H DftActGrp(*No) ActGrp('CDSUB')
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDTRNU01  IF A E           K Disk
     FCDCCMR01  IF   E           K Disk
     FCDTRN02D  CF   E             Workstn IndDS(IndDS)
     F                                     Prefix(d)

      /COPY *LIBL/QCPYSRC,COPYUTILPR

     D IndDS           DS
     D  F2                             N   Overlay(IndDS:2)
     D  F3                             N   Overlay(IndDS:3)
     D  F12                            N   Overlay(IndDS:12)
     D  CommandKey                     N   Overlay(IndDS:50)
     D  Dfltval                        N   Overlay(IndDS:65) Inz(*On)
     D  AddMode                        N   Overlay(IndDS:80) Inz(*On)
     D  TranIDErr                      N   Overlay(IndDS:81)
     D  CardNumErr                     N   Overlay(IndDS:82)
     D  AmountErr                      N   Overlay(IndDS:83)
     D  CategoryErr                    N   Overlay(IndDS:84)
     D  DessErr                        N   Overlay(IndDS:85)
     D  MerchIDErr                     N   Overlay(IndDS:86)
     D  OrigDtErr                      N   Overlay(IndDS:87)
     D  ProcDtErr                      N   Overlay(IndDS:88)
     D  ConfirmInd                     N   Overlay(IndDS:95)

     D PgmStatus      SDS
     D  PgmName          *PROC
     D  UserID               254    263

     D wTodayDt        S               D   Inz(*sys) Datfmt(*iso)
     D wCategCd        S              4  0
     D MessageID       S              8A
     D TestString      S           1000A   Varying

      ****************************************************************
      * MAIN PROCESSING                                              *
      ****************************************************************

     C                   Eval      dFTRTEXT = 'F2=Clear  F3=Exit  F12=Save'

     C                   Eval      dPgmQ = PgmName
     C                   Eval      dPgmName = PgmName
     C                   Eval      dPROCDATE = wTodayDt

     C                   Dow       1 = 1

     C                   Write     MsgSflC
     C                   Exfmt     Scrn1
     C                   ExSr      $ClearInds
     C                   Clear                   MessageID
     C                   Call      'CDUTL02C'

     C                   Select
     C
     C                   When      F2
     C                   ExSr      $ClearScreen

     C                   When      F3 or F12
     C                   Leave

     C                   When      CommandKey = *On
     C
     C                   Other
     C                   Eval      wTodayDt = dPROCDATE
     C                   ExSr      $Add
     C                   If        MessageID = *Blanks
     C                   Leave
     C                   EndIf

     C                   EndSl
     C                   EndDo

                         *InLr = F3;
     C                   Return

      ****************************************************************
      * $ClearScreen - Clear all screen fields
      ****************************************************************
     C     $ClearScreen  BegSr

     C                   Clear                   dTrnID
     C                   Clear                   dCardNum
     C                   Clear                   dTypeCd
     C                   Clear                   dCategCd
     C                   Clear                   dSource
     C                   Clear                   dDess
     C                   Clear                   dAmountEdt
     C                   Clear                   dMerchID
     C                   Clear                   dMerchName
     C                   Clear                   dOrigDate
     C                   Reset                   wTodayDt
     C                   Reset                   Dfltval

     C                   Eval      dProcDate = wTodayDt

     C                   EndSr

      ****************************************************************
      * $Add - Add a new transaction
      ****************************************************************
     C     $Add          BegSr

     C                   ExSr      $CheckInput
     C                   If        MessageID <> *Blanks
     C                   LeaveSr
     C                   EndIf

     C                   Eval      dYesNo = ' '
     C                   Eval      ConfirmInd = *On
     C                   Write     ZConfirm
     C                   Exfmt     ZConfirm
     C                   Eval      ConfirmInd = *Off
     C                   If        dYesNo <> 'Y'
     C                   Eval      MessageID = '~~~~~'
     C                   LeaveSr
     C                   EndIf

     C                   Eval      TrnID = dTrnID
     C                   Eval      CardNum = dCardNum
     C                   Eval      TypeCd = dTypeCd
     C                   Eval      CategCd = '0000'
     C                   Eval      wCategCd = %Dec(dCategCd:4:0)
     C                   Move      wCategCd      CategCd
     C                   Eval      Source = dSource
     C                   Eval      Dess = %Trim(dDess)
     C                   Eval      Amount = %Dec(dAmountEdt:11:2)
     C                   Eval      MerchID = dMerchID
     C                   Eval      TrnStatus = 's'
     C                   Eval      OrigDate = dOrigDate
     C                   Eval      ProcDate = dProcDate
     C                   Eval      TrnCrtDt = %Dec(%Date:*CYMD)
     C                   Eval      TrnCrtTm = %Dec(%Time)
     C                   Eval      TrnCrtUsr = UserID
     C                   Eval      TrnCrtPgm = PgmName
     C                   Write     CDTRNPR
     C                   Eval      F3 = '1'
     C
     C                   EndSr

      ****************************************************************
      * $CheckInput - Validate user input on adds
      ****************************************************************
     C     $CheckInput   BegSr

     C     dTrnID        SetLL     CDTRNU01
     C                   If        %Equal(CDTRNU01)
     C                   Eval      TranIDErr = *On
     C                   Eval      MessageID = 'ERR0047'
     C                   ExSr      $Message
     C                   EndIf

     C     dCardNum      SetLL     CDCCMR01
     C                   If        Not %Equal(CDCCMR01)
     C                   Eval      CardNumErr = *On
     C                   Eval      MessageID = 'ERR0034'
     C                   ExSr      $Message
     C                   EndIf

     C                   If        dCategCd = *Blanks
     C                   Eval      CategoryErr = *On
     C                   Eval      MessageID = 'ERR0036'
     C                   ExSr      $Message
     C                   Else
     C                   Monitor
     C                   If        %Dec(dCategCd:4:0)> 0 and
     C                             %Dec(dCategCd:4:0)< 6000
     C                   Else
     C                   Eval      CategoryErr = *On
     C                   Eval      MessageID = 'ERR0036'
     C                   ExSr      $Message
     C                   EndIf
     C                   On-Error
     C                   Eval      CategoryErr = *On
     C                   Eval      MessageID = 'ERR0036'
     C                   ExSr      $Message
     C                   EndMon
     C                   EndIf

     C                   If        dDess = *Blanks
     C                   Eval      DessErr = *On
     C                   Eval      MessageID = 'ERR0038'
     C                   ExSr      $Message
     C                   EndIf

     C                   Eval      TestString = %Trim(dAmountEdt)
     C                   If        Not IsCurrency(TestString) Or
     C                             TestString = *Blanks
     C                   Eval      AmountErr = *On
     C                   Eval      MessageID = 'ERR0015'
     C                   ExSr      $Message
     C                   ElseIf    %Dec(dAmountEdt:11:2) = 0
     C                   Eval      AmountErr = *On
     C                   Eval      MessageID = 'ERR0015'
     C                   ExSr      $Message
     C                   EndIf

     C                   If        dMerchID = *ALL'0'
     C                   Eval      MerchIDErr = *On
     C                   Eval      MessageID = 'ERR0039'
     C                   ExSr      $Message
     C                   EndIf

     C                   If        dOrigDate > %Date()
     C                   Eval      OrigDtErr = *On
     C                   Eval      MessageID = 'ERR0043'
     C                   ExSr      $Message
     C                   ElseIf    %Diff(%Date():dOrigDate:*Days) > 365
     C                   Eval      OrigDtErr = *On
     C                   Eval      MessageID = 'ERR0040'
     C                   ExSr      $Message
     C                   EndIf

     C                   If        dProcDate > %Date()
     C                   Eval      ProcDtErr = *On
     C                   Eval      MessageID = 'ERR0044'
     C                   ExSr      $Message
     C                   ElseIf    dProcDate < dOrigDate
     C                   Eval      ProcDtErr = *On
     C                   Eval      MessageID = 'ERR0042'
     C                   ExSr      $Message
     C                   ElseIf    %Diff(%Date():dProcDate:*D) > 30
     C                   Eval      ProcDtErr = *On
     C                   Eval      MessageID = 'ERR0041'
     C                   ExSr      $Message
     C                   EndIf

     C                   EndSr

      ****************************************************************
      * $ClearInds - Clear error indicators
      ****************************************************************
     C     $ClearInds    BegSr

     C                   Eval      TranIDErr = *Off
     C                   Eval      CardNumErr = *Off
     C                   Eval      CategoryErr = *Off
     C                   Eval      DessErr = *Off
     C                   Eval      AmountErr = *Off
     C                   Eval      MerchIDErr = *Off
     C                   Eval      OrigDtErr = *Off
     C                   Eval      ProcDtErr = *Off
     C                   Eval      Dfltval = *Off

     C                   EndSr

      ****************************************************************
      * $Message - Show error messages
      ****************************************************************

     C     $Message      BegSr

     C                   Call      'CDUTL01C'
     C                   Parm                    MessageID
     C                   Parm                    PgmName

     C                   EndSr

