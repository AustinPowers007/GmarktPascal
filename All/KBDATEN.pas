{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 16:48:20 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT  KBDATEN;

INTERFACE

   USES ZUSAETZE;

   { Verwendete Indizes
     1 = Laufende Nr, eindeutig
     2 = TagesDatum + TourNr+ LiefNr , nicht eindeutig
     3 = Kundennummer + Datum+ LiefNr, nicht eindeutig
     4 = Monat + Belegnr
     5 = Belegdatum + TourNr + LiefNr, nicht eindeutig
   }

   CONST KBSatzTypen =8;
         KBTourTyp  = 2;
         KBScheckTyp = 7;

   CONST KBSatzBez : ARRAY[1..KBSatzTypen] OF STRING[14 ] =
            ('Bestand',
             'Tourabrechnung',
             'Kasseneingang',
             'Kassenausgang',
             'Sonstiges',
             'Privat',
             'Scheck',
             'Bank');
   CONST KBSTGruppen : ARRAY[1..KBSatzTypen] OF STRING[12] =
          ('','','','',
           '','','','');
   CONST KBstKonten : ARRAY[1..KBSatzTypen] OF STRING[12] =
          ('','','','',
           '','','','');
   CONST KBSTBuchungsTypen : ARRAY[1..KBSatzTypen] OF CHAR =
          ('E','E','E','A','E','A','E','A');

   TYPE TKBDatenPtr = ^TKBDaten;
        TKBDaten = RECORD
      LfdNr        : LONGINT;
      TagesDatum   : TDatum;
      BelegDatum   : TDatum;
      BelegNr      : STRING[3];
      KNr,
      LiefNr       : LONGINT;
      BTxt         : ARRAY[1..3] OF STRING[25];
      Konto        : STRING[12];
      Gruppe       : STRING[12];
      Betrag       : REAL;
      TourNr       : WORD;
      KBSatzTyp    : BYTE;
   END;

   CONST KBIndexNo = 5;
        { Anzahl der verwendeten Indices }

   FUNCTION  CreateKBIndex(IndexNo:BYTE;Daten:TKBDaten):STRING;
     { Erzeugt den Schluessel zu den Indices aus einem Datensatz }
   FUNCTION  GetDBFromKBIndex(IndexNo:BYTE;Schluessel:STRING):STRING;
     { Liefert die Datenbankkennung fÅr den Åbergebenen SchlÅssel }
   FUNCTION  GetKB_FKPart(IndexNo:BYTE;Schluessel:STRING):STRING;
     { Liefert den Front-SchlÅsselteil bei DateiÅbergreifenden Indizes }
   PROCEDURE InitTKBDaten(VAR Daten:TKBDaten);
     {Initialisiert KassenbucheintrÑge }
   FUNCTION  KBGleich(KBEintrag1,KBEintrag2:TKBDaten):BOOLEAN;
     { Vergleicht zwei KassenbucheintrÑge miteinander }

IMPLEMENTATION

   PROCEDURE InitTKBDaten;
      VAR Lauf : BYTE;
      BEGIN
         WITH Daten DO BEGIN
            LfdNr := 0;
            Betrag := 0;
            TagesDatum := '';
            BelegDatum := '';
            BelegNr := '';
            FOR Lauf := 1 TO 3 DO
               BTxt[Lauf] := '';
            Konto := '';
            Gruppe :='';
            TourNr := 0;
            LiefNr := 0;
            KBSatzTyp := 1;
            KNr := 0;
         END;
      END;

   FUNCTION CreateKBIndex;
      FUNCTION CreateKBSatzStr:STRING;
         VAR Ergebnis : STRING[5];
         BEGIN
            IF (KBSTBuchungsTypen[Daten.KBSatzTyp] = 'A') THEN
               Ergebnis :='ZZZZZ'
            ELSE
               Ergebnis :='XXXXX';
            CASE Daten.KBSatzTyp OF
               KBTourTyp    : Ergebnis:= 'TABR';
               KBScheckTyp  : Ergebnis:= LeadingZeros(DAten.LiefNr,5);
            END;
            CreateKBSatzStr := Ergebnis;
         END;

      VAR Ergebnis : STRING;
          TN: WORD;
      BEGIN
         Ergebnis := '';
         WITH Daten DO BEGIN
            CASE IndexNo OF
               1 : Ergebnis := LeadingZeros(LfdNr,7);
               2 : BEGIN
                      IF (TourNr = 0) THEN
                        CASE KBSatzTyp OF
                          1: TN:=1;
                          8: TN:=202;
                        ELSE TN:=201
                        END
                      ELSE
                        TN:=TourNr;
                      Ergebnis := COPY(Tagesdatum,7,2)+COPY(Tagesdatum,4,2)+
                                  COPY(Tagesdatum,1,2)+LeadingZeros(TN,3);
                      Ergebnis := Ergebnis + CreateKBSatzStr;
                   END;
               3 : BEGIN
                      Ergebnis := LeadingZeros(KNr,5)+COPY(Tagesdatum,7,2)+COPY(Tagesdatum,4,2)+
                                       COPY(Tagesdatum,1,2);
                      Ergebnis := Ergebnis+CreateKBSatzStr;
                   END;
               4 : BEGIN
                      Ergebnis :=COPY(Tagesdatum,7,2)+COPY(Tagesdatum,4,2)+BelegNr;
                   END;
               5 : BEGIN
                      Ergebnis := COPY(Belegdatum,7,2)+COPY(Belegdatum,4,2)+
                                  COPY(Belegdatum,1,2)+LeadingZeros(TourNr,3);
                      Ergebnis := Ergebnis + CreateKBSatzStr;
                   END;
            END;
         END;
         CreateKBIndex := Ergebnis;
      END;

   FUNCTION GETKB_FKPart;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := '';
         CASE IndexNo OF
           1,2   : Ergebnis := Schluessel;
           3     : Ergebnis := COPY(Schluessel,1,5);
           4     : Ergebnis := Schluessel;
           5     : Ergebnis := Schluessel;
         END;
         GetKB_FKPart := Ergebnis;
      END;

   FUNCTION GetDBFromKBIndex;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := '';
         CASE IndexNo OF
            1   : Ergebnis:= '';
            2   : Ergebnis:=COPY(Schluessel,3,2)+COPY(Schluessel,1,2);
            3   : Ergebnis:=COPY(Schluessel,8,2)+COPY(Schluessel,6,2);
            4   : Ergebnis:=COPY(Schluessel,3,2)+COPY(Schluessel,1,2);
            5   : Ergebnis:= '';
         END;
         GetDBFromKBIndex := Ergebnis;
      END;

   FUNCTION KBGleich;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := TRUE;
         WITH KBEintrag1 DO BEGIN
            Ergebnis := Ergebnis AND (TagesDatum = KBEintrag2.TagesDatum)
                                 AND (Belegdatum = KBEintrag2.BelegDatum)
                                 AND (BelegNr = KBEintrag2.BelegNr)
                                 AND (Konto = KBEintrag2.Konto)
                                 AND (Gruppe = KBEintrag2.Gruppe)
                                 AND (Betrag = KBEintrag2.Betrag)
                                 AND (LiefNr = KBEintrag2.LiefNr)
                                 AND (KNr = KBEintrag2.KNr)
                                 AND (TourNr = KBEintrag2.TourNr)
                                 AND (KbsatzTyp = KBEintrag2.KBSatzTyp);
            IF Ergebnis THEN BEGIN
               FOR Lauf := 1 TO 3 DO
                  Ergebnis := Ergebnis AND (BTxt[Lauf] = KBEintrag2.Btxt[Lauf]);
            END;
         END;
         KBGleich := Ergebnis;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
