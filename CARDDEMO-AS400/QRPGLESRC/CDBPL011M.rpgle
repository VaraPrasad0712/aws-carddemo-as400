      *******************************************************************
      *  Program      : CDBPL011R                                       *
      *  Author       : Subu                                            *
      *  Date         : 05/31/2024                                      *
      *  Description  : Batch Authorization                             *
      *******************************************************************

     H*DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt)

     FCDTRNR02  UF   E           K Disk

     FCDCCMR01  IF   E           K Disk    Prefix(CCM_)

     FCDACTR04  UF   E           K Disk    Prefix(ACT_)

     FCDAUTU01  O    E           K Disk    Prefix(AUT_)

      /Copy QCPYSRC,COPYBPLAUT

     D $               PR                  EXTPGM('CDBPL011R')
     D $EngTrnSts                     1

     D                 PI
     D $EngTrnSts                     1

     D SubmitSts       C                   's'
     D RejectedSts     C                   'R'
     D ApprovedSts     C                   'A'
     D SubmitRevSts    C                   'a'
     D ReversedSts     C                   'X'
     D*InactRevSts     C                   'I'
     D ErrorSts        C                   '$'
     D Delay           S              2    Inz('01')
     D Dly             S           3000    Varying
     D GeneratedAuth   S                   Like(AUT_AUTHID)
     D @CT             S             30    Dim(7) CTData PerRcd(1)

      ************************************************************************
      * Main Line                                                            *
      ************************************************************************

       Setll ($EngTrnSts) CDTRNR02;

       DoW Not %ShtDn;
         ReadE(E) ($EngTrnSts) CDTRNR02;
         If %Error();
           // Rcd Lck
         ElseIf %Eof();
           Sleep();
           Setll ($EngTrnSts) CDTRNR02;
         ElseIf %Found();
           TRNCHGDT = %Dec(%Date():*CYMD);
           TRNCHGTM = %Dec(%Time());
           TRNCHGUSR = User;
           TRNCHGPGM = Proc_Name;
           ExSR Process_TRN;
         EndIf;
       EndDo;

       Return;

      ************************************************************************
      * Subroutines and Procs                                                *
      ************************************************************************

       BegSR *InzSr;

         If $EngTrnSts <> SubmitSts and $EngTrnSts <> SubmitRevSts;
           Return;
         EndIf;

         AUT_AUTHUSER = User;
         AUT_AUTHPGM = Proc_Name;

       EndSR;

       BegSR Process_TRN;

         AUT_FRAUDFLAG = *Blanks;
         AUT_RSPREASON = *Blanks;
         TRNSTATUS = RejectedSts;
         GenAUT#(GeneratedAuth);

         Chain (CARDNUM) CDCCMR01;
         If Not %Found();
           AUT_RSPREASON = @CT(1);
           ExSR WrtAUT_UpdTRN_UpdACT;
           LeaveSR;
         EndIf;

         If $EngTrnSts = SubmitSts;
           If CCM_STATUS <> 'A';
             If CCM_STATUS = 'H';
               AUT_FRAUDFLAG = 'Y';
             EndIf;
             AUT_RSPREASON = @CT(2);
             ExSR WrtAUT_UpdTRN_UpdACT;
             LeaveSR;
           Else;
             If (CCM_EXPIRYYR < %SubDt(ORIGDATE:*Years)) or
                ((CCM_EXPIRYYR = %SubDt(ORIGDATE:*Years)) and
                (CCM_EXPIRYMO < %SubDt(ORIGDATE:*Months)));
                  AUT_RSPREASON = @CT(3);
                  ExSR WrtAUT_UpdTRN_UpdACT;
                  LeaveSR;
             EndIf;
             Chain(NE) ('A':CCM_ACCTNUM) CDACTR04;
             If Not %Error() and %Found() and ACT_EXPIRYDATE >= PROCDATE;
               // continue
             Else;
               AUT_RSPREASON = @CT(4);
               ExSR WrtAUT_UpdTRN_UpdACT;
               LeaveSR;
             EndIf;
           EndIf;
         TRNSTATUS = ApprovedSts;
         Else;                                // SubmitRevSts
           TRNSTATUS = ReversedSts;
         EndIf;

         CalcUpd$Bal();
         ExSR WrtAUT_UpdTRN_UpdACT;

       EndSR;

       BegSR WrtAUT_UpdTRN_UpdACT;

         AUT_AUTHID = GeneratedAuth;
         AUT_TRNID = TRNID;
         AUT_CARDNUM = CARDNUM;
         //AUT_RSPREASON = CN(1);     //move up
         AUT_AUTHDATE = %Date();
         AUT_AUTHTIME = %Time();

         If TRNSTATUS = ErrorSts;
           AUT_RESPONSE = 'F';
         ElseIf TRNSTATUS = RejectedSts;
           AUT_RESPONSE = 'P';
         Else;
           AUT_RESPONSE = 'P';
           Update CDACTPR;
         EndIf;

         Write CDAUTPR;

         Update CDTRNPR;

       EndSR;

       BegSR RcdLck_TRN;

         *IN60 = *On;
         DoW *IN60 = *On;
           Setll ($EngTrnSts) CDTRNR02;
           ReadE(E) ($EngTrnSts) CDTRNR02;
           If Not %Error();
             *IN60 = *Off;
           EndIf;
         EndDo;

       EndSR;

       Dcl-Proc Sleep;

         Dly = 'DLYJOB DLY(' +%CHAR (DELAY) + ')';
         QCmdPrcs (Dly:%Len(Dly));

       End-Proc;

       Dcl-Proc CalcUpd$Bal;

         Dcl-Pi *n End-Pi;

         Dcl-S wkAmount Packed(20:5);
         Dcl-S wkCurDebit Like(ACT_CURDEBIT);
         Dcl-S wkCurCredit Like(ACT_CURCREDIT);

         *IN60 = *On;
         DoW *IN60 = *On;
           Chain(E) ('A':CCM_ACCTNUM) CDACTR04;
           If Not %Error() and %Found();
             *IN60 = *Off;
           EndIf;
         EndDo;

         If TRNSTATUS = ApprovedSts;
           wkAmount = ACT_BALANCE + AMOUNT;
           If wkAmount > ACT_CREDLIMIT;
            Unlock CDACTR04;
            AUT_RSPREASON = @CT(5);
            TRNSTATUS = RejectedSts;
            Return;
           EndIf;
           Monitor;
           wkCurDebit = ACT_CURDEBIT + AMOUNT;
           On-Error;
              Unlock CDACTR04;
              AUT_RSPREASON = @CT(6);
              TRNSTATUS = ErrorSts;
              Return;
           EndMon;
           ACT_CURDEBIT = wkCurDebit;
           ACT_BALANCE = ACT_CURDEBIT - ACT_CURCREDIT;
         Else;
           Monitor;
           wkCurCredit = ACT_CURCREDIT + AMOUNT;
           On-Error;
              Unlock CDACTR04;
              AUT_RSPREASON = @CT(7);
              TRNSTATUS = ErrorSts;
              Return;
           EndMon;
           ACT_CURCREDIT = wkCurCredit;
           ACT_BALANCE = ACT_CURDEBIT - ACT_CURCREDIT;
         EndIf;

       End-Proc;

** @CT
CREDIT CARD NOT FOUND
CREDIT CARD NOT ACTIVE
CREDIT CARD EXPIRED
ACCOUNT EXPIRED OR INACTIVE
CREDIT LIMIT EXCEEDED
IT ERROR - CURDEBIT OVERFLOW
IT ERROR - CURCREDIT OVERFLOW
