     H NoMain
     H Option(*NoDebugIO:*SrcStmt) Debug(*Yes)

     D GenAUT#         PR            18  0
     D  Generated#                   18  0

     D                SDS
     DJobNm                  244    253

     P GenAUT#         B                   Export

     FCDAUTU01  IF   E           K Disk    

     D                 PI            18  0
     D Generated#                    18  0

     D DayofWeek       S              1
     D CurDt           S              6
     D CurTm           S              6
     D Engine#         S              3
     D Ctr             S              2    Inz('01')
     D wkJobNm         S             10
     D Unique          S              1

       CurDt = %Char(%Dec(%Date():*cymd));
       CurTm = %Char(%Dec(%Time():*hms));
       DayofWeek = %Char(%Rem(%Diff(%Date():d'0001-01-01':*Days):7) + 1);
       EvalR wkJobNm = JobNm;
       Engine# = %SubSt(wkJobNm:8:3);

       Generated# = %Dec((CurDt + CurTm + DayofWeek + Engine# + Ctr):18:0);

       DoW Unique = ' ';
         Setll Generated# CDAUTU01;
         If %Equal;
          Generated# = Generated# + 1;
         Else;
          Unique = 'Y';
         EndIf;
       EndDo;

       Return Generated#;

     P GenAUT#         E

