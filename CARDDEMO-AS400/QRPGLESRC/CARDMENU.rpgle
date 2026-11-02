     FCDMENUD   CF   E             Workstn

     D MENU1           PR                  ExtPgm('CDCUS01R')
     D MENU2           PR                  ExtPgm('CDACT01R')
     D MENU3           PR                  ExtPgm('CDCCM01R')
     D MENU4           PR                  ExtPgm('CDTRN01R')
     D MENU5           PR                  ExtPgm('CDAUT01R')

      *--------------------------------------------------------------------*
      * Main Processing                                                    *
      *--------------------------------------------------------------------*
      /Free

       Dow 0 = 0;
        CMDPROMPT = *Blanks;
        Exfmt CARDMENU;
        
        CMDPROMPT = %Trim(CMDPROMPT);
        If CMDPROMPT = '1';
          CallP MENU1();
        ElseIf CMDPROMPT = '2';
          CallP MENU2();
        ElseIf CMDPROMPT = '3';
          CallP MENU3();
        ElseIf CMDPROMPT = '4';
          CallP MENU4();
        ElseIf CMDPROMPT = '5';
          CallP MENU5();
        ElseIf CMDPROMPT = '90';
          Leave;
        Else;
          CMDPROMPT = *Blanks; 
        EndIf;
       EndDo;

       Return;
      /End-Free
