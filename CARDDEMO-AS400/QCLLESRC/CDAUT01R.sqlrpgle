**Free

Ctl-Opt DftActGrp(*No) ActGrp('CDSQL')
        Option(*NoDebugIO:*SrcStmt);

Dcl-F CDAUT01D WorkStn SFile(SFL1:rrn) IndDS(IndicatorDS);

//*** External Program Prototypes ***
Dcl-PR ClearMessages ExtPgm('CDUTL02C') End-PR;

Dcl-PR TranDetail ExtPgm('CDTRN02R');
  *N Packed(15:0);
End-PR;

Dcl-PR CardDetail ExtPgm('CDCCM02R');
  *N Packed(16:0);
  *N Ind;
End-PR;

//*** Service Program Prototypes ***
//Copy *LIBL/QCPYSRC,COPYUTILPR

Dcl-S ptr_ind Pointer Inz(%addr(*in));
Dcl-DS IndicatorDS Based(ptr_ind);
  F3 Ind Pos(03);
  F4 Ind Pos(04);
  F5 Ind Pos(05);
  F12 Ind Pos(12);
  PageDown Ind Pos(25);
  PageUp Ind Pos(26);
  SflReset Ind Pos(30);
  SflInit Ind Pos(31);
  SflEnd Ind Pos(33);
  SflEmpty Ind Pos(90);
End-DS;

Dcl-DS DBLoadDS ExtName('CDAUTP') End-DS;

Dcl-Ds PmtArr Qualified Dim(cPageSize);
  SflTRN Like(TRNID);
  SflCCM Like(CARDNUM);
End-Ds;

Dcl-S AlwaysOn Ind Inz(*On);
Dcl-S rrn Like(SFLRRN);
Dcl-S wTopKey Like(AUTHID);
Dcl-S wCursorKey Like(AUTHID);
Dcl-C cPageSize Const(16);
Dcl-C Down Const('D');
Dcl-C Up Const('U');
Dcl-S FirstRowFetch Uns(5);
Dcl-S LastRowFetch Uns(5);


//*** Main Line Process ***

PgmQ = 'CDAUT01R';
Dashes = *All'-';
F5 = *On;

Dou F3 or F12;

  Select;

    When F5;
      Row = 4;
      Col = 43;
      Refresh();

    When PageDown;
      LoadNext();

    When PageUp;
      LoadPrev();

    When F4;
      Prompt();

    Other;
      If Position <> 0;
        Refresh();
      EndIf;

  EndSl;

  Clear Position;
  Write Header1;
  Write Header2;
  Write Footer;
  Write MsgSflC;
  If SflEmpty = *On;
    Write Empty;
  EndIf;
  Exfmt Ctl1;
  Read Header2;

EndDo;

Return;


//*** Refresh ***

Dcl-Proc Refresh;

  If PageUp;
    //do nothing
  ElseIf F5 = *Off;
    wCursorKey = Position;
  Else;
    wCursorKey = 999999999999999999;
  EndIf;

  SetCursorDn();

  Clear wTopKey;
  LoadSfl(Down);

End-Proc;


//*** Set Cursor To Fetch Next Data ***

Dcl-Proc SetCursorDn;

  Exec sql
    close authcursordn;

  Exec sql
    declare authcursordn scroll cursor for
      select AUTHID, TRNID, CARDNUM, RESPONSE, RSPREASON, 
             FRAUDFLAG, AUTHDATE, AUTHTIME, AUTHUSER, AUTHPGM
        from CDAUTP
        where AUTHID <= :wCursorKey
          order by AUTHID DESS
            for read only;

  Exec sql
    open authcursordn;

End-Proc;


//*** Set Cursor To Fetch Prev Data ***

Dcl-Proc SetCursorUp;

  Exec sql
    close authcursorup;

  Exec sql
    declare authcursorup scroll cursor for
      select AUTHID, TRNID, CARDNUM, RESPONSE, RSPREASON, 
             FRAUDFLAG, AUTHDATE, AUTHTIME, AUTHUSER, AUTHPGM
        from CDAUTP
        where AUTHID > :wCursorKey
          order by AUTHID ASC
            limit :cPageSize
              for read only;

  Exec sql
    open authcursorup;

End-Proc;


//*** Load Subfile ***

Dcl-Proc LoadSfl;

  Dcl-Pi *N;
    Parm1 Char(1) Const;
  End-Pi;

  If Parm1 = Down;
    Clear PmtArr;
    ResetSfl();
    FetchNextDn();
  Else;
    SetCursorUp();
    FetchNextUp();
    If LastRowFetch = 1;
      wCursorKey = AUTHID;
      Refresh();
    ElseIf FirstRowFetch = 1;
      wCursorKey = 999999999999999999;
      Refresh();
    EndIf;
    Return;
  EndIf;

  Dow sqlcode >= 0 and sqlcode < 100;

    If rrn = 0;
      wTopKey = AUTHID;
    EndIf;

    rrn = rrn + 1;
    PmtArr(rrn).SflTRN = TRNID;
    PmtArr(rrn).SflCCM = CARDNUM;
    Write SFL1;
    If rrn = cPageSize;
      If Parm1 = Down;
        FetchNextDn();
        If sqlcode = 100;
          SflEnd = *On;
        Else;
          Exec sql
            fetch prior from authcursordn
              into :DBLoadDS;
        EndIf;
      EndIf;
      Leave;
    EndIf;

    FetchNextDn();

  EndDo;

  If rrn < cPageSize;
    SflEnd = *On;
  EndIf;

  If rrn = *Zero;
    SflReset = *On;
    SflInit = *On;
    SflEmpty = *On;
  Else;
    SflInit = *Off;
  EndIf;

End-Proc;


//*** Fetch Next Down ***

Dcl-Proc FetchNextDn;

  Exec sql
    fetch next from authcursordn
      into :DBLoadDS;

End-Proc;

//*** Fetch Next Up ***

Dcl-Proc FetchNextUp;

  Exec sql
    fetch first from authcursorup
      into :DBLoadDS;

  Exec sql
    get diagnostics :FirstRowFetch = Row_Count;

  Exec sql
    fetch last from authcursorup
      into :DBLoadDS;

  Exec sql
    get diagnostics :LastRowFetch = Row_Count;

End-Proc;


//*** Reset Indicators ***

Dcl-Proc ResetSfl;

  SflInit = *Off;
  SflReset = *On;
  Write Ctl1;
  SflReset = *Off;
  SflEmpty = *Off;
  SflEnd = *Off;
  rrn = *Zero;

End-Proc;


//*** Load Next ***

Dcl-Proc LoadNext;

  If SflEnd;
    Return;
  EndIf;

  LoadSfl(Down);

End-Proc;


//*** Load Previous ***

Dcl-Proc LoadPrev;

  wCursorKey = wTopKey;

  LoadSfl(Up);

End-Proc;


Dcl-Proc Prompt;

  If CSRFLD = 'TRNID';
    If PmtArr(ROW-6).SflTRN > 0;
      TranDetail(PmtArr(ROW-6).SflTRN);
    EndIf;
  ElseIf CSRFLD = 'CARDNUM';
    If PmtArr(ROW-6).SflCCM > 0;
      CardDetail(PmtArr(ROW-6).SflCCM : AlwaysOn);
    EndIf;
  EndIf;

End-Proc;

