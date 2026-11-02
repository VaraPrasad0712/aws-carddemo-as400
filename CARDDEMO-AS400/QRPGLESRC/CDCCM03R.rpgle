     H DftActGrp(*No) ActGrp(*Caller)
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     FCDCCMR01  IF   E           K Disk

     D DataAreaVal     S              9S 0 DtaAra(CDCCMDTARA)
     D RandomSeed      S              9  0

     D RandomIn        S             10I 0
     D RandomOut       S              8F   Inz
     D ReturnCode      S             12A   Inz
     D Unique          S              1A   Inz

     D cCardSize       C                   Const(100000000000000)
     D cCVVSize        C                   Const(1000)
     D cCardPrefix     C                   Const(3300000000000000)

     D Randomizer      PR                  ExtProc('CEERAN0')
     D                               10I 0
     D                                8F
     D                               12A   Options(*Omit)

     D CDCCM03R        PI
     D  pCardNumber                  16  0
     D  pCVV                          3  0

      /Free

       Reset RandomOut;
       Reset ReturnCode;
       Reset Unique;

       DoU Unique = 'Y';
         In *Lock DataAreaVal;
         DataAreaVal += 1;
         Out DataAreaVal;
         RandomSeed = DataAreaVal;

         RandomIn = RandomSeed;
         Randomizer(RandomIn:RandomOut:ReturnCode);
         RandomOut = RandomOut * cCardSize; // Ensure the # populates all 16 digits
         pCardNumber = %Abs(%DecH(RandomOut:16:0));
         pCardNumber += cCardPrefix;

         Setll pCardNumber CDCCMR01;
         If Not %Equal();
           Unique = 'Y';
         EndIf;
       EndDo;

       RandomIn = *Zero;
       Randomizer(RandomIn:RandomOut:ReturnCode);
       RandomOut = RandomOut * cCVVSize;
       pCVV = %Abs(%DecH(RandomOut:3:0));

       *InLR = *On;
       Return;

      /End-Free
