
     ** Prototype for calling QMHSNDPM
     D QMHSNDPM        PR                  ExtPgm('QMHSNDPM')
     D  MsgID                         7A   Const
     D  MsgFile                      20A   Const
     D  MsgData                   32767A   Options (*VarSize) Const
     D  MsgDtaLen                    10I 0 Const
     D  MsgType                      10A   Const
     D  StackEntry                   10A   Const
     D  StackCount                   10I 0 Const
     D  MsgKey                        4A   Const
     D  ErrorCde                  32767A   Options (*VarSize)

     D ErrorCde        DS
     D  BytesProv                    10I 0 Inz(0)
     D  BytesAvail                   10I 0 Inz(0)

