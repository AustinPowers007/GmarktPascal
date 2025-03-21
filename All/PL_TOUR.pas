{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Sep 17 09:37:04 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
   { Tournummer „ndern }

     PROCEDURE Tour_Aendern(No:BYTE;X1,Y1:WORD);
        CONST CHARS: ARRAY[1..3] OF CHAR = ('V','N','A');
              NUMMS: ARRAY[1..10] OF CHAR = ('1','2','3','4','5','6','7','8','9','0');
        VAR   Lauf,
              Merke: BYTE;
              Sign:  CHAR;
              Ende:  BOOLEAN;
              Txt:   STRING[12];
              T1,T2: BYTE;
              PressCount,X,Y: WORD;

        BEGIN
          ShutBInfo;
          Activate(X1,Y1,0,MaxTour);

          Merke:=TNr;
          T1:=0;
          T2:=0;
          Ende:=FALSE;

          REPEAT
              FOR Lauf:=1 TO MaxTour DO
                IF (MOUSEINBOX(X1+BMargin+(Lauf-1)*(BSize+BMargin),Y1+BMargin,
                               X1+         Lauf   *(BSize+BMargin),Y1+BSize+BMargin)) THEN BEGIN
                  T1:=Lauf;
                  T2:=1;
                  TNr:=(T1-1)*10+T2;
                  REPEAT
                  UNTIL ButtonReleaseInfo(0,PressCount,X,Y);
                  Activate(X1,Y1,T1,MaxTour);
                END;
          UNTIL ((T1 >= 1) AND (T1 <= 3));
          TimeNrS:=T1;
          TimeNrE:=T1+1;
          TimeS:=ATimeS[T1];
          TimeE:=ATimeE[T1+1];
          HoleTour;
          Changes:=TRUE;
          SetNewField(TRUE,0,0,0,0);
        END;
{============================
 Versionshistorie
 $Log:$
 ============================}
