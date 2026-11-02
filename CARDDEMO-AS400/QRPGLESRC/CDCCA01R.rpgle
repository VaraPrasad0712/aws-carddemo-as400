     H DftActGrp(*No) ActGrp(*Caller)
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDCCAR01  IF   E           K Disk    Prefix(R1#)
     FCDCCA01D  CF   E             Workstn SFile(Sfl1:wRRN)
     F                                     IndDS(IndicatorDS)

     D IndicatorDS     DS
     D  iExit                          N   Overlay(IndicatorDS:3)
     D  iPrevious                      N   Overlay(IndicatorDS:12)
     D  iSflDsp                        N   Overlay(IndicatorDS:30)
     D  iSflDspCtl                     N   Overlay(IndicatorDS:31)
     D  iSflClr                        N   Overlay(IndicatorDS:32)
     D  iSflEnd                        N   Overlay(IndicatorDS:33)

     D wLeaveProgram   S              1A   Inz
     D wRRN            S              4P 0 Inz

     C     K1#CCA        KList
     C                   KFld                    pCardNum

     C     *Entry        Plist
     C                   Parm                    pCardNum         16 0

     C                   Eval      wLeaveProgram = ' '
     C                   Eval      PGMNAME = 'CDCCA01R'
     C                   Eval      CARDNUM = pCardNum
     C     K1#CCA        SetLL     CDCCAR01
     C                   Exsr      LoadSubFile

     C                   DoU       wLeaveProgram = 'Y'
     C                   Write     HEADER1
     C                   Write     FOOTER
     C                   Eval      iSflDspCtl = *On
     C                   ExFmt     CTL1
     C                   Eval      iSflDspCtl = *Off
     C                   If        iExit = *On Or iPrevious = *On
     C                   Eval      wLeaveProgram = 'Y'
     C                   EndIf
     C                   EndDo

     C                   Return

     C     LoadSubFile   BegSR

     C                   Exsr      ClearSubfile

     C     K1#CCA        ReadE     CDCCAR01
     C                   DoW       Not %EOF(CDCCAR01) AND wRRN < 9999

     C                   Eval      ACTIVITY = R1#ACTIVITY
     C                   Eval      DESS = R1#DESS
     C                   Eval      CCAUSER = R1#CCAUSER
     C                   Eval      CCATMSTAMP = R1#CCATMSTAMP
     C                   Eval      wRRN += 1
     C                   Write     SFL1

     C     K1#CCA        ReadE     CDCCAR01
     C                   EndDo

     C                   If        %EOF(CDCCAR01)
     C                   Eval      iSflEnd = *On
     C                   Else
     C                   Eval      iSflEnd = *Off
     C                   EndIf

     C                   If        wRRN > 0
     C                   Eval      iSflDsp = *On
     C                   Else
     C                   Eval      iSflDsp = *Off
     C                   EndIf

     C                   EndSR

     C     ClearSubfile  BegSR
     C                   Eval      wRRN = *Zeros
     C                   Eval      iSflClr = *On
     C                   Write     CTL1
     C                   Eval      iSflClr = *Off
     C                   Eval      iSflEnd = *Off
     C                   EndSR

