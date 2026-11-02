     H DftActGrp(*No) ActGrp(*Caller)
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDTRNR01  IF   E           K Disk
     FCDAUTR02  IF   E           K Disk
     FCDTRN02D  CF   E             Workstn IndDS(IndDS) Prefix(s)

     D IndDS           DS
     D F3                              N   Overlay(IndDS:3)
     D F4                              N   Overlay(IndDS:4)
     D F12                             N   Overlay(IndDS:12)
     D CommandKey                      N   Overlay(IndDS:50)
     D RevAudit                        N   Overlay(IndDS:79)
     D AlwaysOn                        N   Overlay(IndDS:99) Inz(*On)

     D OrigLoadDS      DS                  LikeRec(CDTRNPR)

     D ModifiedDS    E DS                  ExtName(CDTRNP) Prefix(s)
     D wDTEA1        E                     ExtFld(TRNCRTDT)
     D wTMEA1        E                     ExtFld(TRNCRTTM)
     D sUSRA1        E                     ExtFld(TRNCRTUSR)
     D sPGMA1        E                     ExtFld(TRNCRTPGM)
     D wDTEA2        E                     ExtFld(TRNCHGDT)
     D wTMEA2        E                     ExtFld(TRNCHGTM)
     D sUSRA2        E                     ExtFld(TRNCHGUSR)
     D sPGMA2        E                     ExtFld(TRNCHGPGM)

     D ErrFlg          S              1A   Inz
     D tCARDNUM        S                   Like(CARDNUM)

      ****************************************************************
      * MAIN PROCESSING                                              *
      ****************************************************************

     C                   Eval      sPGMNAME = 'CDTRN02R'
     C                   Eval      sPGMQ    = 'CDTRN02R'
     C                   Eval      sFTRTEXT = 'F3=Exit  F12=Cancel'
     C                   Reset                   IndDS
     C                   Write     MsgSflC

     C                   Dou       F4 = *Off

     C                   Clear                   ModifiedDS
     C                   Clear                   OrigLoadDS
     C                   ExSr      LoadDsp
     C                   If        ErrFlg = 'Y'
     C                   Reset                   ErrFlg
     C                   Return
     C                   EndIf
     C                   Write     MsgSflC
     C                   Exfmt     Scrn1
     C                   If        F4
     C                   ExSr      Prompt
     C                   Else
     C                   Leave
     C                   EndIf

     C                   EndDo

     C                   Return

      ****************************************************************
      * Load Display
      ****************************************************************

     C     LoadDsp       BegSr

     C     pTrnID        Chain     CDTRNPR       OrigLoadDS

     C                   Monitor
     C
     C                   Eval      ModifiedDS = OrigLoadDS
     C                   Eval      sSTATUS = OrigLoadDS.TRNSTATUS
     C                   ExSr      StatusDtl
     C                   Eval      sAMOUNTDSP =
     C                               %Trim(%EditC(OrigLoadDS.AMOUNT:'3':'$'))
     C                   Eval      sDTEA1 =
     C                               %Dec(%Char(%Date(wDTEA1:*CYMD):*ISO0):8:0)
     C                   Eval      sTMEA1 = wTMEA1

     C                   If        wDTEA2 <> 0
     C                   Eval      sDTEA2 =
     C                               %Dec(%Char(%Date(wDTEA2:*CYMD):*ISO0):8:0)
     C                   Else
     C                   Eval      sDTEA2 = 0
     C                   EndIf
     C                   Eval      sTMEA2 = wTMEA2

     C                   If        sSTATUS = 'a' or sSTATUS = 'X'
     C                   Eval      RevAudit = *On
     C                   Else
     C                   Eval      RevAudit = *Off
     C                   EndIf

     C                   Eval      sDTEB1 = sDTEA2
     C                   Eval      sTMEB1 = sTMEA2
     C                   Eval      sUSRB1 = sUSRA2
     C                   Eval      sPGMB1 = sPGMA2

     C     pTrnID        Setll     CDAUTR02
     C     pTrnID        ReadE     CDAUTR02
     C                   If        Not %EoF()
     C                   Eval      sDTEA2 = %Dec(AUTHDATE)
     C                   Eval      sTMEA2 = %Dec(AUTHTIME)
     C                   Eval      sUSRA2 = AUTHUSER
     C                   Eval      sPGMA2 = AUTHPGM
     C                   EndIf

     C     pTrnID        ReadE     CDAUTR02
     C                   If        Not %EoF()
     C                   Eval      sDTEB2 = %Dec(AUTHDATE)
     C                   Eval      sTMEB2 = %Dec(AUTHTIME)
     C                   Eval      sUSRB2 = AUTHUSER
     C                   Eval      sPGMB2 = AUTHPGM
     C                   Else
     C                   Clear                   sDTEB2
     C                   Clear                   sTMEB2
     C                   Clear                   sUSRB2
     C                   Clear                   sPGMB2
     C                   EndIf

     C                   On-Error
     C                   Eval      ErrFlg = 'Y'
     C                   EndMon

     C                   EndSr

      ****************************************************************
      * Prompt - Call Prompt Screen
      ****************************************************************
     C     Prompt        BegSr

     C                   Eval      tCARDNUM = OrigLoadDS.CARDNUM

     C                   If        sCSRFLD = 'CARDNUM'
     C                   Call      'CDCCM02R'
     C                   Parm                    tCARDNUM
     C                   Parm                    AlwaysOn
     C                   EndIf

     C                   EndSr

      ****************************************************************
      * Status Detail
      ****************************************************************

     C     StatusDtl     BegSr

     C                   If        sSTATUS = 's'
     C                   Eval      sStatusDtl = 'Submitted'
     C                   ElseIf    sSTATUS = 'A'
     C                   Eval      sStatusDtl = 'Approved'
     C                   ElseIf    sSTATUS = 'R'
     C                   Eval      sStatusDtl = 'Rejected'
     C                   ElseIf    sSTATUS = 'a'
     C                   Eval      sStatusDtl = 'Reversal Submitted'
     C                   ElseIf    sSTATUS = 'X'
     C                   Eval      sStatusDtl = 'Reversed'
     C                   ElseIf    sSTATUS = 'I'
     C                   Eval      sStatusDtl = 'Inactivated'
     C                   Else
     C                   Eval      sStatusDtl = 'Error'
     C                   EndIf

     C                   EndSr

      ****************************************************************
      * InzSr
      ****************************************************************

     C     *InzSr        BegSr

     C     *Entry        Plist
     C                   Parm                    pTrnID           15 0

     C                   EndSr

