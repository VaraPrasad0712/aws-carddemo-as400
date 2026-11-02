     H BndDir('CDBNDDIR')
     H DftActGrp(*No) ActGrp('QILE')
     H Option(*NoDebugIO:*SrcStmt)

     FCDCusU01  UF   E           K Disk
     FCDActR02  IF   E           K Disk
     FCDActR03  IF   E           K Disk    Rename(CDACTPR:CDACTPR3)
     FCDCus01D  CF   E             Workstn SFile(Sfl1:dSflRRN)
     F                                     IndDS(IndicatorDS)
     F                                     Prefix(d)

      *****************************************************************
      * Procedure Definitions                                         *
      *****************************************************************

     D LoadSfl         PR
     D LoadNext        PR
     D LoadPrev        PR
     D Refresh         PR
     D AddCustomer     PR
     D EditCustomer    PR
     D ViewCustomer    PR
     D DeleteCustomer  PR
     D HandleOptions   PR
     D Message         PR
     D                                8A   Const

      *** External Program Prototypes ***
     D CustomerDetail  PR                  ExtPgm('CDCUS02R')
     D  CustomerID                    9  0
     D  ViewOnly                       N

     D ClearMessages   PR                  ExtPgm('CDUTL02C')

      *** Service Program Prototypes ***
      /Copy *LIBL/QCPYSRC,COPYUTILPR

      *****************************************************************
      * Field Definitions                                             *
      *****************************************************************

     D IndicatorDS     DS
     D  F3                             N   Overlay(IndicatorDS:3)
     D  F5                             N   Overlay(IndicatorDS:5)
     D  F6                             N   Overlay(IndicatorDS:6)
     D  F12                            N   Overlay(IndicatorDS:12)
     D  PageDown                       N   Overlay(IndicatorDS:25)
     D  PageUp                         N   Overlay(IndicatorDS:26)
     D  SflReset                       N   Overlay(IndicatorDS:30)
     D  SflInit                        N   Overlay(IndicatorDS:31)
     D  SflEnd                         N   Overlay(IndicatorDS:33)
     D  CommandKey                     N   Overlay(IndicatorDS:50)
     D  PositionError                  N   Overlay(IndicatorDS:81)
     D  SflEmpty                       N   Overlay(IndicatorDS:90)

     D ProgramStatus  SDS
     D PgmName           *Proc

     D cPageSize       C                   Const(13)
     D wFirstKey       S                   Like(CustID)
     D wLastKey        S                   Like(CustID)
     D wPosition       S                   Like(dPosition)

      *****************************************************************
      * Main Processing                                               *
      *****************************************************************

      /Free

       dPgmName = PgmName;
       dPgmQ = PgmName;
       ClearMessages();
       SetLL *loval CDCusU01;
       LoadSfl();

       Dow 0 = 0;

         Write Header1;
         Write Header2;
         Write Footer;
         Write MsgSflC;
         If SflEmpty = *On;
           Write Empty;
         EndIf;
         Exfmt Ctl1;
         Read Header2;
         ClearMessages();

         PositionError = *Off;

         Select;

           When F3 or F12;
             Leave;

           When F5;
             wFirstKey = *loval;
             Refresh();

           When F6;
             AddCustomer();

           When PageDown;
             LoadNext();

           When PageUp;
             LoadPrev();

           When CommandKey;
             // Ignore

           Other;
             HandleEnter();

          EndSl;

        EndDo;

        *InLR = *On;
        Return;

       /End-Free

      * - - - - - - - - - - - - - - - *
      * LoadSfl
      * - - - - - - - - - - - - - - - *
     P LoadSfl         B

     D LoadSfl         PI

      /Free

       SflInit = *Off;
       SflReset = *On;
       Write Ctl1;
       SflReset = *Off;
       SflEmpty = *Off;
       dSflRRN = 1;

       Read(N) CDCusU01;
       If %EOF(CDCusU01);
        dSflRRN = 0;
        CustID = wFirstKey;
       EndIf;

       wFirstKey = CustID;
       wLastKey = *hival;
       Dow Not %EOF(CDCusU01) and dSflRRN <= cPageSize;
         dCustID = %Trim(%EditC(CustID:'X'));
         dFirstName = %Trim(FirstName);
         dLastName = %Trim(LastName);
         dOption = *Blank;
         Write Sfl1;
         Read(N) CDCusU01;
         dSflRRN += 1;
         wLastKey = CustID;
       EndDo;

       SflEnd = %EOF(CDCusU01);
       If SflEnd = *On;
         wLastKey = *hival;
       EndIf;

       If dSflRRN = *Zero;
         SflReset = *On;
         SflInit = *On;
         SflEmpty = *On;
       Else;
         SflInit = *Off;
         dSflRRN = 1;
       EndIf;

      /End-Free

     P LoadSfl         E

      * - - - - - - - - - - - - - - - *
      * LoadNext
      * - - - - - - - - - - - - - - - *
     P LoadNext        B

     D LoadNext        PI

      /Free

       If SflEnd;
         Return;
       EndIf;

       SetLL wLastKey CDCusU01;
       LoadSfl();

      /End-Free

     P LoadNext        E

      * - - - - - - - - - - - - - - - *
      * LoadPrev
      * - - - - - - - - - - - - - - - *
     P LoadPrev        B

     D LoadPrev        PI

     D i               S              2  0
     D SflTop          S              1    Inz

      /Free

       SetLL wFirstKey CDCusU01;
       For i = 0 By 1 to cPageSize;
         ReadP(N) CDCusU01;
         If %EOF(CDCusU01);
           SetLL *loVal CDCusU01;
           If i = 0;
             SflTop = 'Y';
           EndIf;
           Leave;
         EndIf;
       EndFor;

       If SflTop = ' ';
         LoadSfl();
       EndIf;

      /End-Free

     P LoadPrev        E

      * - - - - - - - - - - - - - - - *
      * Refresh
      * - - - - - - - - - - - - - - - *
     P Refresh         B
     D Refresh         PI

      /Free

       Clear dPosition;
       SetLL wFirstKey CDCusU01;
       LoadSfl();

      /End-Free

     P Refresh         E
      * - - - - - - - - - - - - - - - *
      * HandleEnter
      * - - - - - - - - - - - - - - - *
     P HandleEnter     B
     D HandleEnter     PI

      /Free

       wPosition = dPosition;
       If ValSrcCtl() = *On;
         SflOptions();
         If wPosition <> *Blanks;
           PositionTo();
         EndIf;
       EndIf;

      /End-Free

     P HandleEnter     E

      * - - - - - - - - - - - - - - - *
      * Validate Search Ctl
      * - - - - - - - - - - - - - - - *
     P ValSrcCtl       B

     D ValSrcCtl       PI              N

     D wTestString     S           1000A   Varying

      /Free

       wTestString = wPosition;
       If IsNumeric(wTestString);
         dRow = 0;
         dCol = 0;
       Else;
         Message('ERR0016');
         PositionError = *On;
         dRow = 5;
         dCol = 5;
         Return *Off;
       EndIf;

       Return *On;

      /End-Free

     P ValSrcCtl       E

      * - - - - - - - - - - - - - - - *
      * PositionTo
      * - - - - - - - - - - - - - - - *
     P PositionTo      B

     D PositionTo      PI

     C                   Eval      wFirstKey = %Dec(wPosition:9:0)
     C                   CallP     Refresh

     P PositionTo      E

      * - - - - - - - - - - - - - - - *
      * SflOptions
      * - - - - - - - - - - - - - - - *

     P SflOptions      B

     D SflOptions      PI

      /Free

        ReadC Sfl1;
        Dow Not %EOF();
          HandleOptions();
          ReadC Sfl1;
        EndDo;

        Refresh();

      /End-Free

     P SflOptions      E

      * - - - - - - - - - - - - - - - *
      * AddCustomer
      * - - - - - - - - - - - - - - - *
     P AddCustomer     B

     D AddCustomer     PI

     D wCustomerID     S              9  0 Inz(*Zeros)
     D wViewOnly       S               N   Inz(*Off)

      /Free

       CustomerDetail(wCustomerID:wViewOnly);
       Refresh();

      /End-Free

     P AddCustomer     E

      * - - - - - - - - - - - - - - - *
      * EditCustomer
      * - - - - - - - - - - - - - - - *
     P EditCustomer    B

     D EditCustomer    PI

     D wCustomerID     S              9  0 Inz(*Zeros)
     D wViewOnly       S               N   Inz(*Off)

      /Free

       wCustomerID = %Dec(dCustID:9:0);
       CustomerDetail(wCustomerID:wViewOnly);

      /End-Free

     P EditCustomer    E

      * - - - - - - - - - - - - - - - *
      * DeleteCustomer
      * - - - - - - - - - - - - - - - *
     P DeleteCustomer  B

     D DeleteCustomer  PI

     D wCustomerID     S              9  0

      /Free

       Write Header1;

       wCustomerID = %Dec(dCustID:9:0);
       Setll wCustomerID CDActR02;
       If %Equal(CDActR02);
         ReadE wCustomerID CDActR02;
         dActId = %EditC(AcctNum:'X');
         Exfmt DelErrWin;
         Return;
       Else;
         Setll wCustomerID CDActR03;
         If %Equal(CDActR03);
           ReadE wCustomerID CDActR03;
           dActId = %EditC(AcctNum:'X');
           Exfmt DelErrWin;
           Return;
         EndIf;
       EndIf;

       Dow Not F12;
         Exfmt DelWin;
         If CommandKey;
           Iter;
         Else;
           Chain(E) wCustomerID CDCusU01;
           If Not %Error();
             Delete CDCusPR;
           Else;
             Iter;
           EndIf;
           Leave;
         EndIf;
       EndDo;

      /End-Free

     P DeleteCustomer  E

      * - - - - - - - - - - - - - - - *
      * ViewCustomer
      * - - - - - - - - - - - - - - - *
     P ViewCustomer    B

     D ViewCustomer    PI

     D wCustomerID     S              9  0 Inz(*Zeros)
     D wViewOnly       S               N   Inz(*On)

      /Free

       wCustomerID = %Dec(dCustID:9:0);
       CustomerDetail(wCustomerID:wViewOnly);

      /End-Free

     P ViewCustomer    E

      * - - - - - - - - - - - - - - - *
      * HandleOptions
      * - - - - - - - - - - - - - - - *
     P HandleOptions   B

     D HandleOptions   PI

      /Free

         Select;

          When dOption = '2';
             EditCustomer();

          When dOption = '4';
             DeleteCustomer();

          When dOption = '5';
             ViewCustomer();

          EndSl;

      /End-Free

     P HandleOptions   E

      * - - - - - - - - - - - - - - - *
      * Message
      * - - - - - - - - - - - - - - - *
     P Message         B

     D Message         PI
     D  MessageID                     8A   Const

     D wMessageID      S              8A

     C                   Eval      wMessageID = MessageID
     C                   Call      'CDUTL01C'
     C                   Parm                    wMessageID
     C                   Parm                    dPgmQ

     P Message         E

