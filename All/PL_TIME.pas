{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Jun 23 11:07:10 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
{ Routinen zur Zeiteinstellung }

   { Variables Feld nach Bezugsgr��en: Button 9, BMargin, BSize }

   { Ruhebild herstellen }

     PROCEDURE DeActivate(X1,Y1:WORD;F1,F2,Max:BYTE);
        CONST C: ARRAY[BOOLEAN,1..2] OF BYTE = ((5,5),(7,0));
        VAR   Lauf:BYTE;
        VAR CX_Index1 : BOOLEAN;
            CX_Index2 : BOOLEAN;
        BEGIN
          BFeld(X1,Y1,X1+Max*(BSize+BMargin)+2,Y1+BSize+4,0,7);
          FOR Lauf:=1 TO Max DO BEGIN
            CX_Index1 := (Lauf = F1) OR (Lauf = F2);
            CX_Index2 := (Lauf=F1) OR (Lauf=F2);
            BFeld(X1+BMargin+(Lauf-1)*(BSize+BMargin),Y1+BMargin,
                  X1+         Lauf   *(BSize+BMargin),Y1+BSize+BMargin,
                  C[CX_Index1,1],
                  C[CX_Index2,2]);
          END;
        END;

   { Button hervorheben }

        PROCEDURE Activate(X1,Y1:WORD;T,Max:BYTE);
           CONST C: ARRAY[BOOLEAN,1..2] OF BYTE = ((0,7),(7,0));
           VAR Lauf:BYTE;
           BEGIN
             BFeld(X1,Y1,X1+Max*(BSize+BMargin)+2,Y1+BSize+4,7,0);
             FOR Lauf:=1 TO Max DO
               BFeld(X1+BMargin+(Lauf-1)*(BSize+BMargin),Y1+BMargin,
                     X1+         Lauf   *(BSize+BMargin),Y1+BSize+BMargin,
                     C[Lauf=T,1],C[Lauf=T,2]);
           END;


   { Einstellungen �ndern }

     PROCEDURE Time_Aendern(No:BYTE;X1,Y1:WORD);
        CONST CHARS: ARRAY[1..3] OF CHAR = ('V','N','A');
              NUMMS: ARRAY[1..5] OF CHAR = ('1','2','3','4','5');
        VAR   Lauf,
              MerkeS,
              MerkeE: BYTE;
              Sign:  CHAR;
              Ende:  BOOLEAN;
              Txt:   STRING[12];
              PressCount,X,Y: WORD;


        BEGIN
          ShutBInfo;
          Activate(X1,Y1,0,MaxTime);

          MerkeS   := TimeNrS;
          MerkeE   := TimeNrE;
          TimeSOld := TimeS;
          TimeEOld := TimeE;
          TimeNrS:=0;
          TimeNrE:=0;
          Ende:=FALSE;

          REPEAT
            IF LEFTBUTTON THEN BEGIN
              FOR Lauf:=1 TO 4 DO
                IF (MOUSEINBOX(X1+BMargin+(Lauf-1)*(BSize+BMargin),Y1+BMargin,
                               X1+         Lauf   *(BSize+BMargin),Y1+BSize+BMargin)) THEN BEGIN
                  IF (TimeNrS = 0) THEN BEGIN
                    IF (Lauf < 4) THEN BEGIN
                      TimeNrS:=Lauf;
                      REPEAT
                      UNTIL ButtonReleaseInfo(0,PressCount,X,Y);
                      Activate(X1,Y1,Lauf,MaxTime)
                    END
                  END ELSE BEGIN
                    IF (Lauf > TimeNrS) THEN BEGIN
                      TimeNrE:=Lauf;
                      REPEAT
                      UNTIL ButtonReleaseInfo(0,PressCount,X,Y);
                      Activate(X1,Y1,Lauf,MaxTime)
                    END
                  END
                END;
            END;
          UNTIL ((TimeNrS >= 1) AND (TimeNrS <= 4)) AND
                ((TimeNrE >= 1) AND (TimeNrE <= 4)) OR Ende;
          TimeS:=ATimeS[TimeNrS];
          TimeE:=ATimeE[TimeNrE];
          Changes:=TRUE;
          SetNewField(TRUE,0,0,0,0);
        END;
{============================
 Versionshistorie
 $Log:$
 ============================}
