{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMT200DR;


INTERFACE

   TYPE TT200Zeile = RECORD
                       Fahr,
                       Tour  : STRING;
                       SchAn,
                       OpenP,
                       BLen,
                       SollK : LONGINT;
                       SollU : REAL;
                       IstK,
                       IstS,
                       IstL  : LONGINT;
                       Bar,
                       SchS,
                       IstLW,
                       SollLW,
                       IstU  : REAL;
                     END;

    PROCEDURE DruckeT200Kopf(Datum:STRING);
    PROCEDURE DruckeT200Zeile(AktZeile:TT200Zeile;PrintIst:BOOLEAN);
    PROCEDURE DruckeT200Fuss(PrintIst:BOOLEAN);

IMPLEMENTATION
   USES ZUSAETZE,
        PRINTER;
   VAR AbSum : TT200Zeile;

   PROCEDURE DruckeT200Kopf;
      VAR Lauf : BYTE;

      BEGIN
         AbSum.SollK := 0;
         AbSum.SollU := 0;
         AbSum.IstK  := 0;
         AbSum.IstS  := 0;
         AbSum.IstL  := 0;
         AbSum.IstU  := 0;
         {$I-}
            WRITELN(LST);
            WRITELN(LST,'     Tourbegleitzettel : T 200',REPLICATE(' ',32),'Datum ',Datum);
            WRITELN(LST);
            WRITELN(LST);
            WRITELN(LST,'       Tour- | | Soll-  |  Soll-  || Ist-     |',
                        ' Ist-     | Ist-    | Ist-');
            WRITELN(LST,'       nr.   | | KÑsten |  Umsatz || Normal-K.|',
                        ' Sonder K.| Leergut | Umsatz');
            WRITELN(LST,'     --------+-+--------+---------++----------+',
                        '----------+---------+--------');
         {$I-}
      END;

   PROCEDURE DruckeT200Zeile;

      BEGIN
         AbSum.SollK:= AbSum.SollK+aktZeile.SollK;
         AbSum.SollU:= AbSum.SollU+aktZeile.SollU;
         AbSum.IstK:= AbSum.IstK+aktZeile.IstK;
         AbSum.IstS:= AbSum.IstS+aktZeile.IstS;
         AbSum.IstL:= AbSum.IstL+aktZeile.IstL;
         AbSum.IstU:= AbSum.IstU+aktZeile.IstU;
         {$I-}
            WRITE(LST,'       ',aktZeile.Tour,' | | ',
                        PrintLongOrLine(aktZeile.SollK,5),'  | ',
                        PrintRealOrLine(aktZeile.SollU,7,2),' || ');
            IF PrintIst THEN
               WRITELN(LST,'  ',PrintLongOrLine(aktZeile.IstK,5),'  |   ',
                                PrintLongOrLine(aktZeile.IstS,5),'  |  ',
                                PrintLongOrLine(aktZeile.IstL,5),'  | ',
                                PrintRealOrLine(aktZeile.IstU,7,2))
            ELSE
               WRITELN(LST,'         |          |         |       ');
            WRITELN(LST,'     --------+-+--------+---------++----------+',
                        '----------+---------+--------');
         {$I+}
     END;

   PROCEDURE DruckeT200Fuss;

      BEGIN
         {$I-}
            WRITELN(LST);
            WRITELN(LST);
            WRITELN(LST,'     --------+-+--------+---------++----------+',
                        '----------+---------+--------');
            WRITE(LST,'     Summen: | | ',PrintLongOrLine(AbSum.SollK,5),'  | ',
                        PrintRealOrLine(AbSum.SollU,7,2),' || ');
            IF PrintIst THEN
               WRITELN(LST,'  ',PrintLongOrLine(AbSum.IstK,5),'  |   ',
                                PrintLongOrLine(AbSum.IstS,5),'  |  ',
                                PrintLongOrLine(AbSum.IstL,5),'  | ',
                                PrintRealOrLine(AbSum.IstU,7,2))
            ELSE
               WRITELN(LST,'         |          |         |       ');
            WRITELN(LST,'     --------+-+--------+---------++----------+',
                        '----------+---------+--------');
            WRITELN(LST);
            WRITELN(LST);
            WRITELN(LST,'     Alle Lieferscheine sind vorhanden, alle Begleitzettel T100 des Tages');
            WRITELN(LST,'     sind vorhanden, geprÅft und OK. Die Daten fÅr T210 sind Åbertragen.');
            WRITELN(LST);
            WRITELN(LST,'     Zu diesem Beleg werden alle T100 und alle Lieferscheine T010 mit');
            WRITELN(LST,'     abgelegt. Der Einzahlungsbeleg der Bank ist angeheftet.');
            WRITELN(LST);
            WRITELN(LST,'     Fall Differenzen oder Unstimmigkeiten bemerkt wurden, wurden diese');
            WRITELN(LST,'     umseitig genau beschrieben und abgezeichnet. Eine Kopie wurde an die');
            WRITELN(LST,'     GeschÑftsleitung Åbergeben.');
            WRITELN(LST);
            WRITELN(LST);
            WRITELN(LST,'          ______________________                 ______________________');
            WRITELN(LST,'            Sachbearbeiter(in)                         PrÅfer(in)');
            WRITELN(LST,CHR(12));
         {$I+}
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
