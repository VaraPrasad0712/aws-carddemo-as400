     H BndDir('CDBNDDIR')
     H DftActGrp(*No) ActGrp(*Caller)
     H Option(*NoDebugIO:*SrcStmt)

     FCDCusU01  UF A E           K Disk
     FCDSTCP    IF   E           K Disk
     FCDCus02D  CF   E             Workstn Prefix(d) IndDS(Indicators)

      *** External Program Prototypes ***
     D SendMessage     PR                  ExtPgm('CDUTL01C')
     D                                8A   Const
     D                               10A   Const

     D ClearMessage    PR                  ExtPgm('CDUTL02C')

      *** External Service Prototypes ***
      /Copy *LIBL/QCPYSRC,COPYUTILPR

      *** Procedure Prototypes ***
     D ClearScreen     PR
     D ClearInds       PR
     D FillScreen      PR
     D FillTable       PR
     D UpdateCustomer  PR
     D AddCustomer     PR
     D SaveStateDB     PR
     D CheckChange     PR
     D CheckInput      PR              N
     D BuildPhone      PR            10  0
     D                                3A
     D                                3A
     D                                4A

     D Message         PR
     D                                8A   Const

     D ScreenState     DS                  LikeRec(Scrn1)

      *** Display Indicators ***
     D Indicators      DS
     D  F3                             N   Overlay(Indicators:3)
     D  F4                             N   Overlay(Indicators:4)
     D  F12                            N   Overlay(Indicators:12)
     D  CommandKey                     N   Overlay(Indicators:50)
     D  AddMode                        N   Overlay(Indicators:70)
     D  ViewOnly                       N   Overlay(Indicators:80)
     D  CustIDErr                      N   Overlay(Indicators:81)
     D  SSNErr                         N   Overlay(Indicators:82)
     D  BirthdayErr                    N   Overlay(Indicators:83)
     D  FICOScoreErr                   N   Overlay(Indicators:84)
     D  FirstNameErr                   N   Overlay(Indicators:85)
     D  LastNameErr                    N   Overlay(Indicators:86)
     D  AddressErr                     N   Overlay(Indicators:87)
     D  CityErr                        N   Overlay(Indicators:88)
     D  StateErr                       N   Overlay(Indicators:89)
     D  ZipErr                         N   Overlay(Indicators:90)
     D  CountryErr                     N   Overlay(Indicators:91)
     D  Phone1Err                      N   Overlay(Indicators:92)
     D  Phone2Err                      N   Overlay(Indicators:93)
     D  HideChgTm                      N   Overlay(Indicators:94)
     D  GenderErr                      N   Overlay(Indicators:95)

     D ProgramStatus  SDS
     D PgmName           *Proc
     D UserID                254    263

      *** Working Storage ***
     D ChangeComplete  S               N
     D StateChanged    S               N

      *** Parameters ***
     D CDCUS02R        PI
     D  pCustID                       9  0
     D  pViewOnly                      N

      *--------------------------------------------------------------------*
      * Main Processing                                                    *
      *--------------------------------------------------------------------*

      /Free

       Reset Indicators;
       Reset ChangeComplete;
       Reset StateChanged;
       ClearScreen();

       If pCustID = *Zeros;
         AddMode = *On;
       Else;
         FillScreen();
         SaveStateDB();
       EndIf;

       ViewOnly = pViewOnly;
       dPgmName = PgmName;
       dPgmQ = PgmName;
       ClearMessage();

       Dow 0 = 0;

         Write MsgSflC;
         Exfmt Scrn1;

         Select;

           When F3 or F12;
             Leave;

           When F4 and AddMode;
             Prompt();

           When CommandKey;
             // Ignore

           When AddMode;
             ClearInds();
             ClearMessage();
             AddCustomer();
             If ChangeComplete;
               Leave;
             EndIf;

           When ViewOnly;
             Leave;

           Other;              //Edit Mode
             ClearInds();
             ClearMessage();
             CheckChange();
             If StateChanged = *On;
               UpdateCustomer();
               If ChangeComplete;
                 Leave;
               EndIf;
             EndIf;

         EndSl;

       EndDo;

       Return;

      /End-Free
      *--------------------------------------------------------------------*
      * ClearScreen - Clear entry fields                                   *
      *--------------------------------------------------------------------*
     P ClearScreen     B
     D ClearScreen     PI

      /Free

       dCustID = *Blanks;
       dSSN = *Blanks;
       dBirthYr = *Blanks;
       dBirthMo = *Blanks;
       dBirthDay = *Blanks;
       dDOB = *Blanks;
       dGender = *Blanks;
       dFICOScore = *Blanks;
       dFirstName = *Blanks;
       dMiddleName = *Blanks;
       dLastName = *Blanks;
       dAddress1 = *Blanks;
       dAddress2 = *Blanks;
       dCity = *Blanks;
       dState = *Blanks;
       dZip = *Blanks;
       dCountry = *Blanks;
       dPhone1Area = *Blanks;
       dPhone1F3 = *Blanks;
       dPhone1L4 = *Blanks;
       dPhone2Area = *Blanks;
       dPhone2F3 = *Blanks;
       dPhone2L4 = *Blanks;
       dCusCrtDt = 0;
       dCusCrtTm = 0;
       dCusCrtUsr = *Blanks;
       dCusCrtPgm = *Blanks;
       dCusChgDt = 0;
       dCusChgTm = 0;
       dCusChgUsr = *Blanks;
       dCusChgPgm = *Blanks;

      /End-Free

     P ClearScreen     E

      *--------------------------------------------------------------------*
      * ClearInds - Reset display indicators                               *
      *--------------------------------------------------------------------*
     P ClearInds       B
     D ClearInds       PI

      /Free

       CustIDErr = *Off;
       SSNErr = *Off;
       BirthdayErr = *Off;
       FICOScoreErr = *Off;
       GenderErr = *Off;
       FirstNameErr = *Off;
       LastNameErr = *Off;
       AddressErr = *Off;
       CityErr = *Off;
       StateErr = *Off;
       ZipErr = *Off;
       CountryErr = *Off;
       Phone1Err = *Off;
       Phone2Err = *Off;

      /End-Free

     P ClearInds       E

      *--------------------------------------------------------------------*
      * SaveState - Save current values of screen fields                   *
      *--------------------------------------------------------------------*
     P SaveStateDB     B

      /Free
       ScreenState.dCustID = dCustID;
       ScreenState.dSSN = dSSN;
       ScreenState.dBirthYr = dBirthYr;
       ScreenState.dBirthMo = dBirthMo;
       ScreenState.dBirthDay = dBirthDay;
       ScreenState.dFirstName = dFirstName;
       ScreenState.dLastName = dLastName;
       ScreenState.dMiddleName = dMiddleName;
       ScreenState.dAddress1 = dAddress1;
       ScreenState.dAddress2 = dAddress2;
       ScreenState.dCity = dCity;
       ScreenState.dState = dState;
       ScreenState.dZip = dZip;
       ScreenState.dCountry = dCountry;
       ScreenState.dPhone1Area = dPhone1Area;
       ScreenState.dPhone1F3 = dPhone1F3;
       ScreenState.dPhone1L4 = dPhone1L4;
       ScreenState.dPhone2Area = dPhone2Area;
       ScreenState.dPhone2F3 = dPhone2F3;
       ScreenState.dPhone2L4 = dPhone2L4;
       If dFICOScore = *Blanks;
         ScreenState.dFICOScore = '0';
       Else;
         ScreenState.dFICOScore = dFICOScore;
       EndIf;

      /End-Free

     P SaveStateDB     E
      *--------------------------------------------------------------------*
      * CheckChange - Compare values of screen fields                      *
      *--------------------------------------------------------------------*
     P CheckChange     B

      /Free

       If dFICOScore = *Blanks;
         dFICOScore = '0';
         *In55 = *On;
       EndIf;

       If ScreenState.dCustID <> dCustID
          or ScreenState.dSSN <> dSSN
          or ScreenState.dBirthYr <> dBirthYr
          or ScreenState.dBirthMo <> dBirthMo
          or ScreenState.dBirthDay <> dBirthDay
          or ScreenState.dFirstName <> dFirstName
          or ScreenState.dLastName <> dLastName
          or ScreenState.dMiddleName <> dMiddleName
          or ScreenState.dAddress1 <> dAddress1
          or ScreenState.dAddress2 <> dAddress2
          or ScreenState.dCity <> dCity
          or ScreenState.dState <> dState
          or ScreenState.dZip <> dZip
          or ScreenState.dCountry <> dCountry
          or ScreenState.dPhone1Area <> dPhone1Area
          or ScreenState.dPhone1F3 <> dPhone1F3
          or ScreenState.dPhone1L4 <> dPhone1L4
          or ScreenState.dPhone2Area <> dPhone2Area
          or ScreenState.dPhone2F3 <> dPhone2F3
          or ScreenState.dPhone2L4 <> dPhone2L4;
         StateChanged = *On;
       Else;
         Monitor;
         If %Dec(ScreenState.dFICOScore:3:0) <> %Dec(dFICOScore:3:0);
           StateChanged = *On;
         Else;
           StateChanged = *Off;
         EndIf;
         On-Error;
           StateChanged = *On;
         EndMon;
       EndIf;

       If *In55 = *On;
         dFICOScore = *Blanks;
         *In55 = *Off;
       EndIf;

      /End-Free

     P CheckChange     E
      *--------------------------------------------------------------------*
      * FillScreen - populate all screen fields                            *
      *--------------------------------------------------------------------*
     P FillScreen      B
     D FillScreen      PI

     D wPhoneChar      S             10A

      /Free

       Chain(N) pCustID CDCusU01;
       dCustID = %Trim(%EditC(CustID:'X'));

       If SSN = *Zeros;
         Clear dSSN;
       Else;
         dSSN = %EditC(SSN:'X');
       EndIf;

       If BirthYr = *Zeros;
         Clear dBirthYr;
       Else;
         dBirthYr = %EditC(BirthYr:'X');
       EndIf;
       If BirthMo = *Zeros;
         Clear dBirthMo;
       Else;
         dBirthMo = %EditC(BirthMo:'X');
       EndIf;
       If BirthDay = *Zeros;
         Clear dBirthDay;
       Else;
         dBirthDay = %EditC(BirthDay:'X');
       EndIf;

       If BirthYr <> 0 and BirthMo <> 0 and BirthDay <> 0;
         dDOB = dBirthYr + '-' + dBirthMo + '-' + dBirthDay;
       Else;
         Clear dDOB;
       EndIf;

       If FICOScore = *Zeros;
         Clear dFICOScore;
       Else;
         dFICOScore = %EditC(FICOScore:'X');
       EndIf;

       dGender = Gender;
       dFirstName = %Trim(FirstName);
       dMiddleName = %Trim(MidName);
       dLastName = %Trim(LastName);
       dAddress1 = %Trim(Address1);
       dAddress2 = %Trim(Address2);
       dCity = %Trim(City);
       dState = State;

       If Zip = *Zeros;
         Clear dZip;
       Else;
         dZip = %EditC(Zip:'X');
       EndIf;

       dCountry = Country;

       If Phone1 <> *Zeros;
         wPhoneChar = %EditC(Phone1:'X');
         dPhone1Area = %SubSt(wPhoneChar:1:3);
         dPhone1F3 = %SubSt(wPhoneChar:4:3);
         dPhone1L4 = %SubSt(wPhoneChar:7:4);
       Else;
         Clear dPhone1Area;
         Clear dPhone1F3;
         Clear dPhone1L4;
       EndIf;

       If Phone2 <> *Zeros;
         wPhoneChar = %EditC(Phone2:'X');
         dPhone2Area = %SubSt(wPhoneChar:1:3);
         dPhone2F3 = %SubSt(wPhoneChar:4:3);
         dPhone2L4 = %SubSt(wPhoneChar:7:4);
       Else;
         Clear dPhone2Area;
         Clear dPhone2F3;
         Clear dPhone2L4;
       EndIf;

       If CusCrtDt <> *Zeros;
         dCusCrtDt = %Dec(%Char(%Date(CusCrtDt:*CYMD):*ISO0):8:0);
         dCusCrtTm = CusCrtTm;
         dCusCrtUsr = CusCrtUsr;
         dCusCrtPgm = CusCrtPgm;
       EndIf;

       If CusChgDt <> *Zeros;
         dCusChgDt = %Dec(%Char(%Date(CusChgDt:*CYMD):*ISO0):8:0);
         dCusChgTm = CusChgTm;
         dCusChgUsr = CusChgUsr;
         dCusChgPgm = CusChgPgm;
         HideChgTm = *Off;
       Else;
         HideChgTm = *On;
       EndIf;

      /End-Free

     P FillScreen      E
      *--------------------------------------------------------------------*
      * UpdateCustomer - Validate data and process customer update         *
      *--------------------------------------------------------------------*
     P UpdateCustomer  B
     D UpdateCustomer  PI

      /Free

       If Not CheckInput();
         Return;
       EndIf;

       Dow Not F12;
         Exfmt AddWin;
         If Not CommandKey;
           Chain(E) pCustID CDCusU01;
           If Not %Error() and %Found();
             FillTable();
             CusChgDt = %Dec(%Date():*CYMD);
             CusChgTm = %Dec(%Time);
             CusChgUsr = UserID;
             CusChgPgm = PgmName;
             Update CDCusPR;
             ChangeComplete = *On;
             Leave;
           EndIf;
         EndIf;
       EndDo;

      /End-Free

     P UpdateCustomer  E

      *--------------------------------------------------------------------*
      * AddCustomer - Create a new customer                                *
      *--------------------------------------------------------------------*
     P AddCustomer     B
     D AddCustomer     PI

     D wPhoneChar      S             10A

      /Free

       If Not CheckInput();
         Return;
       EndIf;

       Dow Not F12;
         Exfmt AddWin;
         If Not CommandKey;
           FillTable();
           CusCrtDt = %Dec(%Date():*CYMD);
           CusCrtTm = %Dec(%Time);
           CusCrtUsr = UserID;
           CusCrtPgm = PgmName;
           Write(E) CDCusPR;
           If Not %Error();
             ChangeComplete = *On;
           EndIf;
           Leave;
         EndIf;
       EndDo;

      /End-Free

     P AddCustomer     E

      *--------------------------------------------------------------------*
      * FillTable - Populate table fields for add or update                *
      *--------------------------------------------------------------------*
     P FillTable       B
     D FillTable       PI

     D wPhoneChar      S             10A

      /Free

       CustID = %Dec(dCustID:9:0);

       If dSSN = *Blanks;
         SSN = *Zeros;
       Else;
         SSN = %Dec(dSSN:9:0);
       EndIf;

       If dBirthYr = *Blanks;
         BirthYr = *Zeros;
       Else;
         BirthYr = %Dec(dBirthYr:4:0);
       EndIf;

       If dBirthMo = *Blanks;
         BirthMo = *Zeros;
       Else;
         BirthMo = %Dec(dBirthMo:2:0);
       EndIf;

       If dBirthDay = *Blanks;
         BirthDay = *Zeros;
       Else;
         BirthDay = %Dec(dBirthDay:2:0);
       EndIf;

       If dFICOScore = *Blanks;
         FICOScore = *Zeros;
       Else;
         FICOScore = %Dec(dFICOScore:3:0);
       EndIf;

       Gender = dGender;
       FirstName = %Trim(dFirstName);
       MidName = %Trim(dMiddleName);
       LastName = %Trim(dLastName);
       Address1 = %Trim(dAddress1);
       Address2 = %Trim(dAddress2);
       City = %Trim(dCity);
       State = %Trim(dState);
       Zip = %Dec(dZip:5:0);
       Country = %Trim(dCountry);
       Phone1 = BuildPhone(dPhone1Area:dPhone1F3:dPhone1L4);
       Phone2 = BuildPhone(dPhone2Area:dPhone2F3:dPhone2L4);
      /End-Free

     P FillTable       E

      *--------------------------------------------------------------------*
      * CheckInput - Validate user input                                   *
      *--------------------------------------------------------------------*
     P CheckInput      B
     D CheckInput      PI              N

     D wDateString     S             10A
     D wPhone          S             10A
     D wTestDate       S               D
     D wNumTestString  S           1000A   Varying
     D wTestID         S                   Like(CustID)
     D ErrorFlag       S              1A

      /Free

       If dCustID = *Blanks;
         Message('ERR0010');
         CustIDErr = *On;
         Return *Off;
       EndIf;

       wNumTestString = %Trim(dCustID);
       If Not IsNumeric(wNumTestString);
         Message('ERR0004');
         CustIDErr = *On;
         Return *Off;
       EndIf;

       If %Dec(wNumTestString:9:0) = 0;
         Message('ERR0010');
         CustIDErr = *On;
         Return *Off;
       EndIf;

       If AddMode;
         wTestID = %Dec(dCustID:9:0);
         SetLL wTestID CDCusU01;
         If %Equal(CDCusU01);
           Message('ERR0021');
           CustIDErr = *On;
           ErrorFlag = 'Y';
         EndIf;
       EndIf;

       If dSSN = *Blanks or dSSN = *Zeros;
         Message('ERR0028');
         SSNErr = *On;
         ErrorFlag = 'Y';
       ElseIf %Len(%Trim(dSSN)) < 9;
         Message('ERR0029');
         SSNErr = *On;
         ErrorFlag = 'Y';
       Else;
         wNumTestString = %Trim(dSSN);
         If Not IsNumeric(wNumTestString);
           Message('ERR0004');
           SSNErr = *On;
           ErrorFlag = 'Y';
         EndIf;
       EndIf;

       wNumTestString = dBirthYr + dBirthMo + dBirthDay;
       If wNumTestString <> *Blanks;
         If Not IsNumeric(wNumTestString);
           Message('ERR0004');
           BirthdayErr = *On;
           ErrorFlag = 'Y';
         Else;
         wDateString = dBirthYr + '-' + dBirthMo + '-' + dBirthDay;
         Monitor;
           wTestDate = %Date(wDateString);
         On-Error;
           Message('ERR0011');
           BirthdayErr = *On;
           ErrorFlag = 'Y';
         EndMon;

         If wTestDate > %Date();
           Message('ERR0065');
           BirthdayErr = *On;
           ErrorFlag = 'Y';
         EndIf;
         EndIf;

       Else;
         Message('ERR0031');
         BirthdayErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dGender = 'M' or dGender = 'F' or dGender = 'U';
       Else;
         Message('ERR0049');
         GenderErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       wNumTestString = %Trim(dFICOScore);
       If dFICOScore <> *Blanks and Not IsNumeric(wNumTestString);
         Message('ERR0032');
         FICOScoreErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dFirstName = *Blanks;
         Message('ERR0022');
         FirstNameErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dLastName = *Blanks;
         Message('ERR0023');
         LastNameErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dAddress1 = *Blanks;
         Message('ERR0024');
         AddressErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dCity = *Blanks;
         Message('ERR0025');
         CityErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       If dState = *Blanks;
         Message('ERR0027');
         StateErr = *On;
         ErrorFlag = 'Y';
       Else;
       SetLL dState CDSTCP;
       If Not %Equal(CDSTCP);
         Message('ERR0013');
         StateErr = *On;
         ErrorFlag = 'Y';
       EndIf;
       EndIf;

       If dZip = *Blanks or dZip = *Zeros;
         Message('ERR0026');
         ZipErr = *On;
         ErrorFlag = 'Y';
       ElseIf %Len(%Trim(dZip)) < 5;
         Message('ERR0030');
         ZipErr = *On;
         ErrorFlag = 'Y';
       Else;
       wNumTestString = %Trim(dZip);
       If dZip <> *Blanks and Not IsNumeric(wNumTestString);
         Message('ERR0004');
         ZipErr = *On;
         ErrorFlag = 'Y';
       EndIf;
       EndIf;

       SetLL (dState:dCountry) CDSTCP;
       If Not %Equal(CDSTCP);
         Message('ERR0062');
         CountryErr = *On;
         ErrorFlag = 'Y';
       EndIf;

       wPhone = dPhone1Area + dPhone1F3 + dPhone1L4;
       wNumTestString = wPhone;
       If wPhone <> *Blanks;
         If Not IsNumeric(wNumTestString);
           Message('ERR0004');
           Phone1Err = *On;
           ErrorFlag = 'Y';
         ElseIf %Len(%Trim(wPhone)) < 10;
           Message('ERR0014');
           Phone1Err = *On;
           ErrorFlag = 'Y';
         EndIf;
       Else;
         dPhone1Area = *Blanks;
         dPhone1F3 = *Blanks;
         dPhone1L4 = *Blanks;
       EndIf;

       wPhone = dPhone2Area + dPhone2F3 + dPhone2L4;
       wNumTestString = wPhone;
       If wPhone <> *Blanks;
         If Not IsNumeric(wNumTestString);
           Message('ERR0004');
           Phone2Err = *On;
           ErrorFlag = 'Y';
         ElseIf %Len(%Trim(wPhone)) < 10;
           Message('ERR0014');
           Phone2Err = *On;
           ErrorFlag = 'Y';
         EndIf;
       Else;
         dPhone2Area = *Blanks;
         dPhone2F3 = *Blanks;
         dPhone2L4 = *Blanks;
       EndIf;

       If ErrorFlag = 'Y';
         Return *Off;
       Else;
         Return *On;
       EndIf;


      /End-Free

     P CheckInput      E
      *--------------------------------------------------------------------*
      * BuildPhone - Construct full phone from component strings           *
      *--------------------------------------------------------------------*
     P BuildPhone      B

     D BuildPhone      PI            10  0
     D  CharArea                      3A
     D  CharFirst3                    3A
     D  CharLast4                     4A

     D PhoneNum        S             10  0
     D NewPhone        S             10A
     D NumTestString   S           1000A   Varying

      /Free

       NewPhone = CharArea + CharFirst3 + CharLast4;
       NumTestString = %Trim(NewPhone);
       If Not IsNumeric(NumTestString);
         PhoneNum = *Zeros;
       ElseIf %Len(%Trim(NewPhone)) <> 10;
         PhoneNum = *Zeros;
       Else;
         PhoneNum = %Dec(NewPhone:10:0);
       EndIf;

       Return PhoneNum;

      /End-Free

     P BuildPhone      E
      *--------------------------------------------------------------------*
      * Message - Send error or informationall messages to program queue   *
      *--------------------------------------------------------------------*
     P Message         B
     D Message         PI
     D  MessageID                     8A   Const

     D wMessageID      S              8A
      /Free

       wMessageID = MessageID;
       SendMessage(wMessageID:dPgmQ);

      /End-Free
     P Message         E
      *--------------------------------------------------------------------*
      * Prompt - Gender selection                                          *
      *--------------------------------------------------------------------*
     P Prompt          B

      /Free

       If dCSRFLD = 'GENDER';
         Exfmt GenderWin;
         If dSELGEN = 1;
           dGender = 'M';
         ElseIf dSELGEN = 2;
           dGender = 'F';
         ElseIf dSELGEN = 3;
           dGender = 'U';
         Else;
           dGender = ' ';
         EndIf;
       EndIf;

      /End-Free

     P Prompt          E
