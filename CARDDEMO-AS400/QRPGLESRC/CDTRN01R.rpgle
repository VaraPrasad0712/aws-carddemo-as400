**Free

  Ctl-Opt DftActGrp(*No) Option(*NoDebugIO:*SrcStmt);

  Dcl-F CDTRN01D WorkStn IndDs(Ds#IndDs) Sfile(SFL1:WRRN);
  Dcl-F CDTRNR01 Disk Usage(*Input) Keyed;

  Dcl-Pr ClrPgmMsg ExtPgm('QMHRMVPM');
    wCallStkEnt Char(10) Const;
    wCallStkCntr Int(10) Const;
    wMessageKey Char(4);
    wMessageRmv CHar(10);
    wErrorCode Pointer Const;
  End-Pr;

  Dcl-Pr DisplayTrn ExtPgm('CDTRN02R');
    wTrnKey Packed(15:0);
  End-Pr;

  Dcl-Pr AddTrn ExtPgm('CDTRN03R');
  End-Pr;

  Dcl-Pr SndPgmMsg ExtPgm('QMHSNDPM');
    wMessageID  Char(7) Const;
    wQualMsgF  Char(20) Const;
    wMsgData Char(256) Const;
    wMsgDtaLen Int(10) Const;
    wMsgType Char(10) Const;
    wCallStkEnt Char(10) Const;
    wCallStkCntr Int(10) Const;
    wMessageKey Char(4);
    wErrorCode Pointer Const;
  End-Pr;

  Dcl-Ds Ds#IndDs;
    iExit Ind Pos(03) Inz;
    iCancel Ind Pos(12) Inz;
    iRefresh Ind Pos(05) Inz;
    iAdd Ind Pos(06) Inz;
    iPageUP Ind Pos(25) Inz;
    iPageDown Ind Pos(26) Inz;

    iSflDsp Ind Pos(40) Inz;
    iSflDspCtl Ind Pos(41) Inz;
    iSflClr Ind Pos(42) Inz;
    iSflEnd Ind Pos(43) Inz;
    iSflNxtChg Ind Pos(44) Inz;
    iSflOptErr Ind Pos(50) Inz;

  End-Ds;

  Dcl-Ds *N PSDS;
    wProgramName Char(10) Pos(1);
  End-Ds;

  Dcl-Ds OptArr Qualified Dim(9999) Inz;
    SflKey Char(15);
    SflOpt Char(1);
  End-Ds;

  Dcl-S wMessageID Char(7) Inz('CPF9898');
  Dcl-S wQualMsgF Char(20) Inz('QCPFMSG   *LIBL');
  Dcl-S wMsgData Char(256) Inz;
  Dcl-S wMsgDtaLen Int(10) Inz;
  Dcl-S wMsgType Char(10)  Inz('*DIAG');
  Dcl-S wCallStkEnt Char(10) Inz('*');
  Dcl-S wCallStkCntr Int(10) Inz;
  Dcl-S wMessageKey Char(4) Inz;
  Dcl-S wMessageRmv Char(10);

  Dcl-S wRRN Packed(4:0) Inz;
  Dcl-S wPos Packed(4:0) Inz;
  Dcl-S wIdx Packed(4:0) Inz;
  Dcl-S wCntr Packed(4:0) Inz;
  Dcl-S wLeaveProgram Char(1) Inz;
  Dcl-S wError Char(1) Inz;
  Dcl-S wMessage Char(256) Inz;
  Dcl-S wFKey Packed(15:0) Inz;
  Dcl-S wLKey Packed(15:0) Inz;
  Dcl-S wTrnKey Packed(15:0) Inz;
  Dcl-C cPageSize Const(13);
  Dcl-S wPosTo Char(15) Inz;

  PGMNAME = wProgramName;
  PGMQ = wProgramName;
  SetGt *Hival CDTRNR01;
  ExSR LoadSubfile;

  wLeaveProgram = 'N';
  DoW wLeaveProgram = 'N';
    Write HEADER;
    Write FOOTER;
    Write MSGSFLC;
    iSflDspCtl = *On;
    ExFmt CTL1;
    iSflDspCtl = *Off;
    ExSr ClearErrSubfile;
    Select;
      When iExit = *On Or iCancel = *On;
        iExit = *Off;
        iCancel = *Off;
        wLeaveProgram = 'Y';
      When iRefresh = *On;
        iRefresh = *Off;
        ExSr RefreshSubfile;
      When iPageUp = *On;
        ExSr LoadSubFileOpts;
        If wError = 'N';
          ExSr PageUpSubfile;
        EndIf;
        iPageUp = *Off;
      When iPageDown = *On;
        ExSr LoadSubFileOpts;
        If wError = 'N';
          ExSr PageDownSubfile;
        EndIf;
        iPageDown = *Off;
      When iAdd = *On;
        iAdd = *Off;
        AddTrn();
      Other;
        ExSr LoadSubFileOpts;
        If wError = 'N';
          ExSr ProcessSflOpts;
          If D1POSTO > 0;
            wPosTo = %Char(D1POSTO);
            wPosTo = %ScanRpl(' ':'9':wPosTo);
            D1POSTO = %Dec(wPosTo:15:0);
            SetGt D1POSTO CDTRNR01;
            D1POSTO = 0;
          Else;
            SetGt wFKey CDTRNR01;
          EndIf;
          ExSr LoadSubfile;
        EndIf;
    EndSl;
  EndDo;

  *InLr = *On;

  BegSr LoadSubfile;

    ExSr ClearSubfile;

    ReadP CDTRNR01;
    Dow Not %EOF(CDTRNR01) AND wRRN < cPageSize;

      If wFKey = *Zeros;
        wFKey = TRNID;
        iSflDsp = *On;
      EndIf;
      wLKey = TRNID;

      D1STRANID = %EditC(TRNID:'X');
      wPos = %LookUp(D1STRANID:OptArr(*).SflKey:1:wIdx);
      If wPos = *Zeros;
        D1SOPTION = *Blanks;
      Else;
        D1SOPTION = OptArr(wPos).SflOpt;
      EndIf;
      D1SDATE = %Dec(ProcDate);
      D1SAmount = %Trim(%EditC(Amount:'3':'$'));
      D1SDESS = Dess;
      D1SSTATUS = TrnStatus;

      wRRN += 1;
      Write SFL1;

      ReadP CDTRNR01;
    EndDo;

    If %EOF(CDTRNR01);
      iSflEnd = *On;
    Else;
      iSflEnd = *Off;
    EndIf;

  EndSr;

  BegSr ProcessSflOpts;

    OptArr(wIdx+1).SflKey = x'00';

    For wCntr = 1 To wIdx;
      If OptArr(wCntr).SflOpt = '5';
        wTrnKey = %Int(OptArr(wCntr).SflKey);
        CallP DisplayTrn(wTrnKey);
      EndIf;
    EndFor;
    Clear OptArr;
    wIdx = *Zeros;
    Read Header;

  EndSr;

  BegSr LoadSubFileOpts;

    wError = 'N';
    If iSflDsp = *On;
      ReadC Sfl1;
      DoW Not %EOF();
        If D1SOPTION <> ' ' and D1SOPTION <> '5';
          wError = 'Y';
          iSflOptErr = *On;
          iSflNxtChg = *On;
          Update SFL1;
          iSflNxtChg = *Off;
          iSflOptErr = *Off;
          wMessage = 'Invalid option';
          ExSr WriteErrToSfl;
        Else;
          iSflOptErr = *Off;
          Update SFL1;
          wPos = %LookUp(D1STRANID:OptArr(*).SflKey:1:wIdx);
          If D1SOPTION = *Blanks;
            If wPos <> *Zeros;
              Clear OptArr(wPos);
            EndIf;
          ElseIf D1SOPTION = '5';
            If wPos = *Zeros;
              wIdx += 1;
              OptArr(wIdx).SflOpt = D1SOPTION;
              OptArr(wIdx).SflKey = D1STRANID;
            Else;
              OptArr(wPos).SflOpt = D1SOPTION;
              OptArr(wPos).SflKey = D1STRANID;
            EndIf;
          EndIf;
        EndIf;
        ReadC Sfl1;
      EndDo;
    EndIf;

  EndSr;

  BegSr PageUpSubfile;

    SetGt wFKey CDTRNR01;

    For wCntr = 1 To cPageSize;
      Read CDTRNR01;
      wTrnKey = TRNID;
      If %EOF(CDTRNR01);
        If wCntr = 1;
          wMessage = 'You have reached the top of the page';
          ExSr WriteErrToSfl;
        EndIf;
        SetGt *Hival CDTRNR01;
        ExSr LoadSubfile;
        LeaveSr;
      EndIf;
    EndFor;
    SetGt wTrnKey CDTRNR01;
    ExSr LoadSubfile;

  EndSr;

  BegSr PageDownSubfile;

    SetLL wLKey CDTRNR01;
    ReadP CDTRNR01;
    If %EOF(CDTRNR01);
      wMessage = 'You have reached the bottom of the page';
      ExSr WriteErrToSfl;
      SetGt wFKey CDTRNR01;
    Else;
      SetLL wLKey CDTRNR01;
    EndIf;
    ExSr LoadSubfile;

  EndSr;

  BegSr RefreshSubfile;

    SetLL *Hival CDTRNR01;
    ExSr LoadSubfile;

  EndSr;

  BegSr ClearErrSubfile;
    wMessageRmv = '*ALL';
    CallP  ClrPgmMsg(wCallStkEnt:wCallStkCntr:wMessageKey:
                     wMessageRmv:*Null);
  EndSr;

  BegSr WriteErrToSfl;

    wMsgData = %Trim(wMessage);
    wMsgDtaLen = %Len(%Trim(wMessage));
    CallP  SndPgmMsg(wMessageID:wQualMsgF:wMsgData:wMsgDtaLen:
                     wMsgType:wCallStkEnt:wCallStkCntr:
                     wMessageRmv:*Null);
  EndSr;

  BegSr ClearSubfile;

    wRRN = *Zeros;
    wFKey = *Zeros;
    wLKey = *Zeros;
    iSflClr = *On;
    Write CTL1;
    iSflClr = *Off;
    iSflEnd = *Off;
    If iPageUp Or iPageDown;
      //do not clear
    Else;
      Clear OptArr;
      wIdx = *Zeros;
    EndIf;

  EndSr;

