     H NoMain BndDir('CDBNDDIR')

      /Copy *LIBL/QCPYSRC,COPYUTILPR

      *---------------------------------------------------------------*
      * IsNumeric - Determine if a passed string is a valid number    *
      *---------------------------------------------------------------*
     P IsNumeric       B                   Export
     D IsNumeric       PI              N
     D  pString                    1000A   Varying

     D cValidNumbers   C                   Const('0123456789')
     D wTestString     S           1000A   Varying
     D wScanPosition   S             10I 0

      /Free

       wTestString = %Trim(pString);
       wScanPosition = %Check(cValidNumbers:wTestString);
       If wScanPosition = *Zeros;
         Return *On;
       Else;
         Return *Off;
       EndIf;

      /End-Free

     P IsNumeric       E

      *---------------------------------------------------------------*
      * IsCurrency - Determine if a passed string can map to dollars  *
      *---------------------------------------------------------------*
     P IsCurrency      B                   Export
     D IsCurrency      PI              N
     D  pString                    1000A   Varying

     D cValidNumbers   C                   Const('0123456789')
     D wTestString     S           1000A   Varying
     D wEndPosition    S             10I 0
     D wDecimalPosition...
     D                 S             10I 0

      /Free
       wTestString = %Trim(pString);

       If Not IsCurNum(wTestString);
         Return *Off;
       EndIf;

       wDecimalPosition = %Scan('.':wTestString);

       If wDecimalPosition = *Zero;
         Return *On;
       EndIf;

       wEndPosition = %Len(wTestString);

       If wEndPosition - wDecimalPosition > 2;
         Return *Off;
       Else;
         Return *On;
       EndIf;

      /End-Free

     P IsCurrency      E

      *------------------------------------------------------------------------*
      * IsCurNum - Determine if a passed string is a valid number for currency *
      *------------------------------------------------------------------------*
     P IsCurNum        B                   Export
     D IsCurNum        PI              N
     D  pString                    1000A   Varying

     D cValidNumbers   C                   Const('0123456789.')
     D wTestString     S           1000A   Varying
     D wScanPosition   S             10I 0

      /Free

       wTestString = %Trim(pString);
       wScanPosition = %Check(cValidNumbers:wTestString);
       If wScanPosition = *Zeros;
         Return *On;
       Else;
         Return *Off;
       EndIf;

      /End-Free

     P IsCurNum        E

