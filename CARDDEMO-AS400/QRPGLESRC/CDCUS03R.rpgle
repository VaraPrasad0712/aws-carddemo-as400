     H DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDCUSR01  IF   E           K Disk    Prefix(R1#)
     FCDCUS03D  CF   E             Workstn SFile(Sfl1:wRRN)
     F                                     IndDS(IndicatorDS)

     D IndicatorDS     DS
     D  iExit                          N   Overlay(IndicatorDS:3)
     D  iRefresh                       N   Overlay(IndicatorDS:5)
     D  iPrevious                      N   Overlay(IndicatorDS:12)
     D  iSflDsp                        N   Overlay(IndicatorDS:30)
     D  iSflDspCtl                     N   Overlay(IndicatorDS:31)
     D  iSflClr                        N   Overlay(IndicatorDS:32)
     D  iSflEnd                        N   Overlay(IndicatorDS:33)

     D wRRN            S              4P 0 Inz
     D wLeaveProgram   S              1A   Inz
     D wRcdSelected    S              1A   Inz

     C     K1#CUSR01     KList
     C                   KFld                    wCustID           9 0

     C     *Entry        Plist
     C                   Parm                    PCUSTID           9

     C                   Eval      PGMNAME = 'CDCUS03R'
     C     *Loval        SetLL     CDCUSR01
     C                   Exsr      LoadSubFile

     C                   Eval      wLeaveProgram = 'N'
     C                   DoW       wLeaveProgram = 'N'
     C                   Eval      POSITION = *Zeros
     C                   Write     HEADER1
     C                   Write     HEADER2
     C                   Write     FOOTER
     C                   Eval      iSflDspCtl = *On
     C                   ExFmt     CTL1
     C                   Eval      iSflDspCtl = *Off
     C                   Read      HEADER2
     C                   If        iExit = *On Or iPrevious = *On
     C                   Eval      wLeaveProgram = 'Y'
     C                   ElseIf    iRefresh = *On
     C                   Eval      iRefresh = *Off
     C     *Loval        SetLL     CDCUSR01
     C                   Exsr      LoadSubFile
     C                   Else
     C                   If        POSITION = *Zeros
     C                   Exsr      ProcessSflOpt
     C                   If        wRcdSelected = 'Y'
     C                   Eval      wLeaveProgram = 'Y'
     C                   EndIf
     C                   Else
     C     POSITION      SetLL     CDCUSR01
     C                   Exsr      LoadSubFile
     C                   EndIf
     C                   EndIf
     C                   EndDo

     C                   Eval      *InLR = *On

     C     LoadSubFile   BegSR

     C                   Exsr      ClearSubfile

     C                   Read      CDCUSR01
     C                   DoW       Not %EOF(CDCUSR01) AND wRRN < 9999

     C                   Eval      CUSTID = %EditC(R1#CUSTID:'X')
     C                   Eval      FIRSTNAME = R1#FIRSTNAME
     C                   Eval      LASTNAME = R1#LASTNAME
     C                   Eval      wRRN += 1
     C                   Write     SFL1

     C                   Read      CDCUSR01
     C                   EndDo

     C                   If        %EOF(CDCUSR01)
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

     C     ProcessSflOpt BegSR
     C                   Eval      wRcdSelected = 'N'
     C                   If        iSflDsp = *On
     C                   ReadC     Sfl1
     C                   DoW       Not %EOF()
     C                   If        OPTION = 'X'
     C                   Eval      pCustID = CUSTID
     C                   Eval      wRcdSelected = 'Y'
     C                   LeaveSR
     C                   EndIf
     C                   ReadC     Sfl1
     C                   EndDo
     C                   EndIf
     C                   EndSR

     C     ClearSubfile  BegSR
     C                   Eval      wRRN = *Zeros
     C                   Eval      iSflClr = *On
     C                   Write     CTL1
     C                   Eval      iSflClr = *Off
     C                   Eval      iSflEnd = *Off
     C                   EndSR

