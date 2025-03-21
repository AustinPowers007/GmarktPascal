{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT BUTTONS;

   INTERFACE

      TYPE  BSET = SET OF BYTE;

      PROCEDURE ShutBInfo;
      PROCEDURE FF(VAR S:STRING;C:CHAR;L:BYTE);
      PROCEDURE BFeld(X1,Y1,X2,Y2,C1,C2:WORD);
      PROCEDURE Button(Nr:BSET);
      PROCEDURE PrintButton(Nr:BYTE;X,Y:WORD);
      PROCEDURE SetButton(No:BYTE;X1,Y1,X2,Y2:WORD;T:STRING);
      PROCEDURE ButtonInfo;

      VAR   ButtonC: ARRAY[1..40,1..20,1..20] OF BYTE;
            ButtonP: ARRAY[1..40,1.. 4] OF WORD;
            ButtonI: ARRAY[1..40] OF STRING[60];
            BoxInfo,
            MaxBox:  BYTE;

   IMPLEMENTATION

      USES GRAPH,
           FENSTER,
           GRAPNEU,
           MAUSUNIT;

      { Schlieáen der ButtonInfoLine }

        PROCEDURE ShutBInfo;
           BEGIN
             IF (BoxInfo <> 0) THEN BEGIN
               HIDEMOUSE;
               BoxInfo:=0;
               REMEMBER(4);
               SHOWMOUSE
             END
           END;

      { FrontFill }

        PROCEDURE FF;
           VAR Lauf:BYTE;
           BEGIN
             FOR Lauf:=LENGTH(S)+1 TO L DO
               S:=C+S;
           END;

      { Feldumriá Button }

        PROCEDURE BFeld;
           BEGIN
             HIDEMOUSE;
             SETCOLOR(C1);
             LINE(X1,Y1,X2,Y1);
             LINE(X1,Y1,X1,Y2);
             SETCOLOR(C2);
             LINE(X1,Y2,X2,Y2);
             LINE(X2,Y1,X2,Y2);
             SETCOLOR(0);
             SHOWMOUSE
           END;

      { Buttons von Dateien lesen }

        PROCEDURE Button;
           VAR Name:STRING;
               T:TEXT;
               Lauf,Lauf1,No:BYTE;
               BLine: ARRAY[1..40] OF STRING;
           BEGIN
             FOR No:=1 TO 40 DO
               IF No IN Nr THEN BEGIN
                 STR(No:1,Name);
                 FF(Name,'0',4);
                 Name:='B'+Name+'.GSS';
                 ASSIGN(T,Name);
                 {$I-} RESET(T); {$I+}
                 IF (IORESULT = 0) THEN BEGIN
                   READLN(T,ButtonI[No]);
                   FOR Lauf:=1 TO 20 DO BEGIN
                     READLN(T,BLine[Lauf]);
                     FOR Lauf1:=1 TO 20 DO
                       ButtonC[No,Lauf,Lauf1]:=ORD(BLine[Lauf][Lauf1])-48;
                   END;
                   {$I-} CLOSE(T); {$I+}
                   IF (IORESULT <> 0) THEN ;
                 END;
               END;
           END;

      { Button ausdrucken }

        PROCEDURE PrintButton;
           VAR Lauf1,Lauf2:WORD;
           BEGIN
             HIDEMOUSE;
             IF (Nr > MaxBox) THEN MaxBox:=Nr;
             ButtonP[Nr,1]:=X;
             ButtonP[Nr,2]:=Y;
             ButtonP[Nr,3]:=X+20;
             ButtonP[Nr,4]:=Y+20;
             FOR Lauf1:=1 TO 20 DO
               FOR Lauf2:=1 TO 20 DO
                 PutPixel(X-1+Lauf1,Y-1+Lauf2,ButtonC[Nr,Lauf2,Lauf1]);
             SHOWMOUSE
           END;

       { Nicht gespeicherten Button erzeugen }

         PROCEDURE SetButton;
            BEGIN
              IF (No > MaxBox) THEN MaxBox:=No;
              ButtonI[No]:=T;
              ButtonP[No,1]:=X1;
              ButtonP[No,2]:=Y1;
              ButtonP[No,3]:=X2;
              ButtonP[No,4]:=Y2;
            END;

       { Infotext von Button darstellen }

         PROCEDURE ButtonInfo;
            VAR Lauf:  BYTE;
                L,
                Start: WORD;
                Found:BOOLEAN;
            BEGIN
              Found:=FALSE;
              FOR Lauf:=1 TO MaxBox DO
                IF (MouseInBox(ButtonP[Lauf,1],ButtonP[Lauf,2],ButtonP[Lauf,3],ButtonP[Lauf,4])) THEN BEGIN
                  Found:=TRUE;
                  IF (Lauf <> BoxInfo) THEN BEGIN
                    ShutBInfo;
                    BoxInfo:=Lauf;
                    L:=LENGTH(ButtonI[Lauf])*8+20;
                    IF ButtonP[Lauf,1]+L > 630 THEN
                      Start:=630-L
                    ELSE
                      Start:=ButtonP[Lauf,1];
                    HIDEMOUSE;
                    BRAIN(4,Start,ButtonP[Lauf,2]+23,Start+L,ButtonP[Lauf,2]+35);
                    SETFILLSTYLE(1,5);
                    BAR(Start,ButtonP[Lauf,2]+23,Start+L,ButtonP[Lauf,2]+35);
                    BFeld(Start+2,ButtonP[Lauf,2]+23,Start+L,ButtonP[Lauf,2]+35,4,0);
                    OUTTEXTXY(Start+10,ButtonP[Lauf,2]+26,ButtonI[Lauf]);
                    SHOWMOUSE;
                  END;
                END;
                IF NOT(Found) THEN ShutBInfo
            END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
