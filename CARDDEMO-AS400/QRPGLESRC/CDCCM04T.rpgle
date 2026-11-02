     H DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDCCAU01  IF A E           K Disk    Usropn InfSr(PSSR_Sr)
     F                                     InfDS(InfStsDS)

     D MainEntry       PR                  ExtPgm('CDCCM04T')
     D  Trgbuffer                          LikeDS(TrgDS)
     D  Trgbufferlen                 10I 0

     D MainEntry       PI
     D  Trgbuffer                          LikeDS(TrgDS)
     D  Trgbufferlen                 10I 0

     D PgmSts         SDS
     D  UserID               254    263

     D InfStsDS        DS
     D  FileSts          *Status

     D TrgDS           DS
     D  File                         10
     D  Library                      10
     D  Member                       10
     D  Event                         1
     D  Time                          1
     D  Commitlocklev                 1
     D                                3
     D  Ccsid                        10I 0
     D  Rrn                          10I 0
     D                                4
     D  Beforecoffset                10I 0
     D  Beforeclen                   10I 0
     D  Befnulloffset                10I 0
     D  Befnulllen                   10I 0
     D  Aftercoffset                 10I 0
     D  Afterclen                    10I 0
     D  Aftnulloffset                10I 0
     D  Aftnulllen                   10I 0

     D Beforecptr      S               *
     D Old           E DS                  Extname(CDCCMP) Qualified
     D                                     Based(Beforecptr)

     D Aftercptr       S               *
     D New           E DS                  Extname(CDCCMP) Qualified
     D                                     Based(Aftercptr)


      //Mainline

        Beforecptr = %Addr(Trgbuffer) + Trgbuffer.Beforecoffset;
        Aftercptr = %Addr(Trgbuffer) + Trgbuffer.Aftercoffset;

        Select;

         When Trgbuffer.Event = '1';

           If Not %Open(CDCCAU01);
             Open CDCCAU01;
           EndIf;

           CARDNUM = New.CARDNUM;
           CCASEQNO = 1;
           ACTIVITY = 'GEN';
           DESS = 'NEW CARD GENERATED';
           CCAUSER = New.CCMCRTUSR;
           CCATMSTAMP = %Timestamp();

           Write CDCCAPR;

         When Trgbuffer.Event = '2';

           If Not %Open(CDCCAU01);
             Open CDCCAU01;
           EndIf;

           CARDNUM = Old.CARDNUM;
           CCASEQNO = 1;
           ACTIVITY = 'DEL';
           DESS = %Char(Old.CUSTID) + '~~' + %Char(Old.ACCTNUM);
           CCAUSER = UserID;
           CCATMSTAMP = %Timestamp();

           Write CDCCAPR;

         When Trgbuffer.Event = '3';

           If Not %Open(CDCCAU01);
             Open CDCCAU01;
           EndIf;

           SetGt (old.CARDNUM) CDCCAU01;
           ReadP CDCCAU01;
           If Old.CARDNUM = CARDNUM;
             CCASEQNO = CCASEQNO + 1;
           Else;
             CCASEQNO = 1;
             CARDNUM = Old.CARDNUM;
           EndIf;

           If Old.STATUS <> New.STATUS;
             ACTIVITY = 'STS';
             DESS = 'STATUS CHANGED FROM "' + Old.STATUS + '"';
           ElseIf Old.NAME <> New.NAME;
             ACTIVITY = 'NAME';
             DESS = Old.NAME;
           Else;
             Close CDCCAU01;
             Return;
           EndIf;

           CCAUSER = New.CCMCHGUSR;
           CCATMSTAMP = %Timestamp();
           Write CDCCAPR;

        EndSl;

        *Inlr = *On;

        BegSr PSSR_Sr;

         If FileSts = 01021;
           CCASEQNO = CCASEQNO + 1;
           Write CDCCAPR;
           Close CDCCAU01;
         Else;
           Dump;
         EndIf;

        EndSr '*CANCL';

