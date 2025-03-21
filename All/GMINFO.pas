{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMInfo;
{$O+}

{$F+}

INTERFACE


   FUNCTION AInfoAendern(Nr:BYTE):BOOLEAN; FAR;
   PROCEDURE ShowAnfangsInfo;
   PROCEDURE InitAnfangsInput;

IMPLEMENTATION
   USES CASHDATA,
        CASHNEU,
        DOS,
        PLACEWIN,
        ZUSAETZE;



   CONST AnzahlIZeilen = 20;
   CONST InfoDateiName : STRING = 'GMARKT.INF';

   TYPE TInfotyp = ARRAY[1..AnzahlIZeilen] OF STRING;

   VAR AnfangsInfo :TEingabeFensterPtr;
       InfoZeile : TInfoTyp;

   FUNCTION ReadAnfangsInfo(VAR InfoZeile:TInfoTyp):BOOLEAN;
      VAR Datei :Text;
          Lauf  : BYTE;
      BEGIN
         ReadAnfangsInfo := FALSE;
         IF SeekFile(InfoDateiName) THEN BEGIN
            Lauf := 0;
            ASSIGN(Datei,InfoDateiName);
            {$I-}
               RESET(Datei);
            {$I+}
            WHILE NOT EOF(Datei) AND (Lauf < AnzahlIZeilen) DO BEGIN
              INC(Lauf);
              READLN(Datei,InfoZeile[Lauf]);
            END;
            Close(Datei);
            IF Lauf > 0 THEN
              ReadAnfangsInfo := TRUE;
         END;
      END;

   PROCEDURE WriteAnfangsInfo(VAR InfoZeile:TInfoTyp);
      VAR Datei :Text;
          Lauf  : BYTE;
      BEGIN
         ASSIGN(Datei,InfoDateiName);
         {$I-}
            REWRITE(Datei);
            FOR Lauf := 1 TO AnzahlIZeilen DO
               WRITELN(Datei,InfoZeile[Lauf]);
            CLOSE(Datei);
         {$I+}
      END;

   FUNCTION AInfoAendern;
      VAR   Ergebnis : INTEGER;
            Ende : BOOLEAN;

      BEGIN
         Ende := FALSE;
         AnfangsInfo^.Save('');
         REPEAT
            Ergebnis:=AnfangsInfo^.Input;
            CASE Ergebnis OF
               27 : BEGIN
                       IF AnfangsInfo^.Changed('') THEN
                          Ende := Request.Act('Žnderungen verwerfen ?')
                       ELSE
                          Ende := TRUE;
                       IF Ende THEN
                          AnfangsInfo^.Restore('');
                    END;
               -68 : BEGIN
                        IF AnfangsInfo^.Changed('') THEN BEGIN
                           WriteAnfangsInfo(InfoZeile);
                           InfoBox.Act(2,'Die Informationen wurden gespeichert');
                           Ende :=TRUE;
                        END
                        ELSE
                           Ende := TRUE;
                     END;
            END;
         UNTIL Ende;
         AnfangsInfo^.ClearBackground;
         AInfoAendern := TRUE;
      END;

   PROCEDURE ShowAnfangsInfo;
      VAR   EndCode : INTEGER;
            Lauf : BYTE;

      BEGIN
         IF ReadAnfangsInfo(InfoZeile) THEN BEGIN
            WRITE(CHR(7),CHR(7),CHR(7));
            AnfangsInfo^.Show('');
            EndCode := InputKey;
            AnfangsInfo^.ClearBackground;
            IF Request.Act('Nachricht l”schen ?') THEN BEGIN
               EraseFile(InfoDateiName);
               FOR Lauf := 1 TO AnzahlIZeilen DO
                  InfoZeile[Lauf] := '';
            END;
         END;
      END;

   PROCEDURE InitAnfangsInput;
      VAR Lauf : BYTE;
      BEGIN
         FOR Lauf := 1 TO 10 DO
             InfoZeile[Lauf] := '';
         NEW(AnfangsInfo);
         AnfangsInfo^.Init(70,50,570,400,0,'Information','ASIN3',Aktiv);
         AnfangsInfo^.AddString('','1','Info:',1,1,InfoZeile[1],'',50);
         FOR Lauf := 2 TO AnzahlIZeilen DO
            AnfangsInfo^.AddString('','1','',1,Lauf,InfoZeile[Lauf],'',50);
         AnfangsInfo^.AlignFields;
      END;




END.
{============================
 Versionshistorie
 $Log:$
 ============================}
