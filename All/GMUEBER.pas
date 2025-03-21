{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jul 19 12:21:50 GMT+02:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMUeber;

INTERFACE
   USES GMDATEI,
        LIEDATEN,
        ZUSAETZE;

   VAR  AktLTag,
        NaechsterLTag :TDatum;
   CONST AnzBereiche = 6;
   CONST AnzUebSpalten = AnzBereiche+2;
   CONST SpTitel : ARRAY[1..AnzUebSpalten] OF STRING[7] =
                          ('Schwelm','Barmen',
                           'Ronsdf.','Elberf.',
                           'Cronenb','Vohw.',
                           'Neue K.','Gesamt');
   VAR BTArray : ARRAY[1..TourAbs,1..AnzBereiche,1..6] OF BOOLEAN;

   PROCEDURE ShowKundenUebersicht;
   PROCEDURE HideKundenUebersicht;
   PROCEDURE SubFromKSum(Lieferung:TLiefDaten);
   PROCEDURE AddToKSum(Lieferung:TLiefDaten);
   PROCEDURE KUeberSichtDruck(UebDatum:TDatum);
   FUNCTION  FindKXPos(PQEin:LONGINT):BYTE;
   FUNCTION  UebTagAnzeigen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                        VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;
   PROCEDURE InitUebersichtInput;
   PROCEDURE InitBTArray(Bereich:BYTE;BtString:STRING);
   FUNCTION  GetUebDatum(Nr:BYTE):STRING;



IMPLEMENTATION

   USES CASHNEU,
        ASTDATA,
        GMSETUP,
        CASHDATA,
        PLACEWIN,
        GMBASE,
        PRINTER,
        KUNDATEN;

   VAR  PQ:           ARRAY[72..97,73..90,1..4] OF BYTE;

   TYPE TKUebItem = RECORD
           Kunden,
           Kaesten : LONGINT;
        END;
   CONST AnzZeilen  = 7;
   CONST ZeTitel : ARRAY[1..AnzZeilen] OF STRING[5] =
                          ('Vorm.','',
                           'Nachm','',
                           'Abend','',
                           'S:');

   TYPE TKUebArray = ARRAY[1..AnzUebSpalten,1..AnzZeilen] OF TKUebItem;
   TYPE TKUebDaten = RECORD
          Tag     : TDatum;
          OldDay  : BOOLEAN;
          Werte   : TKUebArray;
          KAKunRel: REAL;
        END;

   VAR AktUebersicht : TKUebDaten;
       TagAnzeige : STRING;

   CONST UFileNamen : ARRAY[1..2] OF STRING[12] = ('UHEUTE.DAT',
                                                   'UMORGEN.DAT');
   VAR UebFile : FILE OF TKuebDaten;

   VAR AktKBereich: STRING;
       Kundenuebersicht : ARRAY[1..2,1..AnzZeilen] OF LONGINT;
       KundeUebersichtFenster,
       UebDatumFenster,
       UebersichtFenster : TEingabeFensterPtr;


  PROCEDURE InitBTArray;
     VAR Lauf1,
         Lauf2 : BYTE;

         Front,Rest: STRING;
         TAbsName : STRING[10];
     BEGIN
        Rest:=UPPER(BtString);
        IF (Rest<> '') THEN BEGIN
           FOR Lauf2 := 1 TO 6 DO BEGIN
              SplitString(Rest,';',Front,Rest);
              FOR Lauf1 := 1 TO TourAbs DO BEGIN
                 TabsName:=MakeTString(Lauf1,0);
                 IF (POS(TabsName[1],Front) > 0) THEN
                    BTArray[Lauf1,Bereich,Lauf2] := TRUE
                 ELSE
                    BTArray[Lauf1,Bereich,Lauf2] := FALSE;
              END;
           END;
        END ELSE BEGIN
           FOR Lauf1 := 1 TO TourAbs DO
              FOR Lauf2 := 1 TO 6 DO
                BTArray[Lauf1,Bereich,Lauf2] := TRUE;
        END;
     END;

   PROCEDURE DispTagString(Datum:STRING);
      VAR Txt : STRING;
          OldTagString :STRING;
      BEGIN
         OldTagString := TagAnzeige;
         IF ValidDate(Datum) THEN BEGIN
            Txt :=DayOfWeekStr(Datum);
            IF SameDate(Datum,ActDate) THEN BEGIN
               Txt := 'Heute - '+Txt;
            END ELSE BEGIN
               IF Later(Datum,ActDate) THEN BEGIN
                  IF SameDate(PrevDay(Datum),ActDate) THEN
                     Txt :='Morgen - '+Txt
                  ELSE IF SameDate(PrevDay(PrevDay(Datum)),ActDate) THEN
                     Txt :='öbermorgen - '+Txt
                  ELSE
                     Txt :='DemnÑchst -'+Txt;
               END ELSE
                  Txt := 'Alt - '+Txt;
            END;
         END ELSE
            TXT :='';
         IF (Txt <> '') THEN
            Txt:=CenterString(txt,26);
         IF (OldTagString <> Txt) THEN BEGIN
            IF NOT UebersichtFenster^.Geschlossen THEN
               UebersichtFenster^.ClearConst('35');
            TagAnzeige := Txt;
            IF NOT UebersichtFenster^.Geschlossen THEN
               UebersichtFenster^.Refresh('35');
         END;
      END;


   FUNCTION GetUebDatum;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := '';
         IF (Nr >= 1) AND (Nr <= 3) THEN BEGIN
            Ergebnis := AktUebersicht.Tag;
         END;
         GetUebDatum := Ergebnis;
      END;

   FUNCTION FindKXPos;
       VAR PQs:         STRING[5];
           L1,L2,L3:    INTEGER;
           X1:          BYTE;
           Ergebnis : BYTE;


       BEGIN
          PQs := L2S(PQEIn,0);
          L1 := S2I(COPY(PQs,1,2));
          L2 := S2I(COPY(PQs,3,2));
          L3 := S2I(COPY(PQs,5,1));
          IF (L3 = 0) THEN
             L3:=2;
          IF ((L1 < 72) OR (L1 > 97)) OR ((L2 < 73) OR (L2 > 90))THEN
             X1:=0
          ELSE
             X1:=PQ[L1,L2,L3];
          CASE X1 OF
             14        : Ergebnis:=1;
             13,12,10,9: Ergebnis:=2;
             11        : Ergebnis:=3;
             8,7,6,4,3 : Ergebnis:=4;
             5         : Ergebnis:=5;
             2,1       : Ergebnis:=6
             ELSE        Ergebnis:=7;
          END;
          FindKXPos := Ergebnis;
       END;

   FUNCTION FindKYPos(Lieferung:TLiefDaten;OldDay:BOOLEAN):BYTE;
      VAR Ergebnis : BYTE;

       BEGIN
         IF OldDay THEN
            Ergebnis := (Lieferung.TourNr-1) DIV 5 +1
         ELSE
            Ergebnis:= (Lieferung.TourNr) DIV 50;
         Ergebnis:= (Ergebnis-1)* 2 +2;
         CASE Ergebnis OF
            2 : IF NotLaterTime(Lieferung.Bis[1],'11:00') THEN
                   DEC(Ergebnis);
            4 : IF NotLaterTime(Lieferung.Bis[1],'16:00') THEN
                   DEC(Ergebnis);
            6 : IF NotLaterTime(Lieferung.Bis[1],'20:00') THEN
                   DEC(Ergebnis);
         END;
         FindKYPos := Ergebnis;
       END;

   PROCEDURE KueberSichtInit(VAR Daten:TKuebDaten);
      VAR Lauf1,
          Lauf2 : BYTE;

      BEGIN
         WITH Daten DO BEGIN
            Tag := '';
            KaKunRel := 0;
            FOR Lauf1 := 1 TO 8 DO
               FOR Lauf2 := 1 TO 7 DO BEGIN
                  Werte[Lauf1,Lauf2].Kunden := 0;
                  Werte[Lauf1,Lauf2].Kaesten := 0;
               END;
         END;
      END;

   PROCEDURE ReadUebDatum(Nr : BYTE);
      VAR Pfad : STRING;
      BEGIN
         IF (Nr = 1) OR (Nr = 2)  THEN BEGIN
            Pfad := MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'));
            IF SeekFile(Pfad+UFileNamen[Nr]) THEN BEGIN
               {$I-}
               ASSIGN(UebFile,Pfad+UFileNamen[Nr]);
               RESET(UebFile);
               READ(UebFile,AktUebersicht);
               CLOSE(UebFile);
               {$I+}
            END ELSE BEGIN
               KueberSichtInit(AktUebersicht);
            END;
         END;
      END;

   PROCEDURE WriteUebDatum(Nr : BYTE);
      VAR Pfad : STRING;
      BEGIN
         IF (Nr = 1) OR (Nr = 2)  THEN BEGIN
            Pfad := MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'));
            IF ValidPath(Pfad) THEN BEGIN
               ASSIGN(UebFile,Pfad+UFileNamen[Nr]);
               {$I-}
               REWRITE(UebFile);
               WRITE(UebFile,AktUebersicht);
               CLOSE(UebFile);
               {$I+}
            END ELSE
               FaultBox.Act(2,'UngÅltige Pfadangabe, bitte INI-Datei '+
                            'ÅberprÅfen.');
         END;
      END;

   FUNCTION NaechsterTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         IF ValidDate(InpString) THEN BEGIN
            InpString:= NextWorkDay(InpString);
            DispTagString(InpString);
         END ELSE
            WRITE(CHR(7));
         Abbruch := FALSE;
         Check:= FALSE;
         NaechsterTag := FALSE;
      END;

   FUNCTION VorherigerTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         IF ValidDate(InpString) THEN BEGIN
            InpString:= PrevWorkDay(InpString);
            DispTagString(InpString);
         END ELSE
            WRITE(CHR(7));
         Abbruch := FALSE;
         Check:= FALSE;
         VorherigerTag := FALSE;
      END;

   FUNCTION GMUeberAction:BOOLEAN;FAR;
      BEGIN
         DispTagString(UebDatum);
         UeberSichtFenster^.Refresh('35');
         GMUeberAction := FALSE;
      END;


   PROCEDURE ResetPQ;
      VAR L1,L2,L3: BYTE;
      BEGIN
        FOR L1:=72 TO 97 DO
          FOR L2:=73 TO 90 DO
            FOR L3:=1 TO 4 DO
              PQ[L1,L2,L3]:=0;
      END;

   {$I BEREICH.PQ }



   PROCEDURE SubFromUebersicht(Lieferung:TLiefDaten);
      VAR Kunde : TKundenDaten;
      VAR Lauf :BYTE;
          X,
          Y   : BYTE;
          Anz : LONGINT;
          D   : BOOLEAN;
      BEGIN
         KundDB.BeginTransAction;
         IF NOT KundDb.GetRec(1,Lieferung.Kundennummer,Kunde) THEN
            d:= KundDb.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
         KundDB.EndTransaction;
         X:= FindKXPos(S2L(Kunde.PlanQ));
         Y:= FindKYPos(Lieferung,AktUebersicht.OldDay);
         Anz := 0;
         WITH Lieferung DO
            FOR Lauf := 1 TO 30 DO BEGIN
               IF (Positionen[Lauf].KAFL = 1) AND
                  NOT(Positionen[Lauf].ArtNr = '01971') AND
                  NOT(Positionen[Lauf].ArtNr = '01972') AND
                  NOT(Positionen[Lauf].ArtNr = '01973') AND
                  NOT(Positionen[Lauf].ArtNr = '01974') THEN
                  Anz := Anz+Positionen[Lauf].Menge;
               END;
         WITH AktUebersicht DO BEGIN
            Werte[x,y].Kunden := Werte[x,y].Kunden-1;
            Werte[x,y].Kaesten := Werte[x,y].Kaesten-Anz;
            Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kunden := Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kunden-1;
            Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kaesten := Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kaesten-Anz;
            Werte[x,AnzZeilen].Kunden := Werte[x,AnzZeilen].Kunden-1;
            Werte[x,AnzZeilen].Kaesten := Werte[x,AnzZeilen].Kaesten-Anz;
            Werte[AnzUebSpalten,AnzZeilen].Kunden := Werte[AnzUebSpalten,AnzZeilen].Kunden-1;
            Werte[AnzUebSpalten,AnzZeilen].Kaesten := Werte[AnzUebSpalten,AnzZeilen].Kaesten-Anz;
            IF Werte[AnzUebSpalten,AnzZeilen].Kunden <> 0 THEN
               KaKunRel := Werte[AnzUebSpalten,AnzZeilen].Kaesten/Werte[AnzUebSpalten,AnzZeilen].Kunden
            ELSE
               KaKunRel :=0;
         END;
      END;

   PROCEDURE AddToUebersicht(Lieferung:TLiefDaten);
      VAR Kunde : TKundenDaten;
      VAR Lauf,
          X,
          Y   : BYTE;
          Anz : LONGINT;
          D   : BOOLEAN;
      BEGIN
         KundDB.BeginTransaction;
         IF NOT KundDb.GetRec(1,Lieferung.Kundennummer,Kunde) THEN
           d:= KundDb.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
         KundDB.EndTransaction;
         X:= FindKXPos(S2L(Kunde.PlanQ));
         Y:= FindKYPos(Lieferung,AktUebersicht.OldDay);
         Anz := 0;
         WITH Lieferung DO
            FOR Lauf := 1 TO 30 DO BEGIN
               IF (Positionen[Lauf].KAFL = 1) AND
                  NOT(Positionen[Lauf].ArtNr = '01971') AND
                  NOT(Positionen[Lauf].ArtNr = '01972') AND
                  NOT(Positionen[Lauf].ArtNr = '01973') AND
                  NOT(Positionen[Lauf].ArtNr = '01974') THEN
                    Anz := Anz+Positionen[Lauf].Menge;
            END;
         WITH AktUebersicht DO BEGIN
            Werte[x,y].Kunden := Werte[x,y].Kunden+1;
            Werte[x,y].Kaesten := Werte[x,y].Kaesten+Anz;
            Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kunden := Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kunden+1;
            Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kaesten := Werte[AnzUebSpalten,((y-1) DIV 2)*2+1].Kaesten+Anz;
            Werte[x,AnzZeilen].Kunden := Werte[x,AnzZeilen].Kunden+1;
            Werte[x,AnzZeilen].Kaesten := Werte[x,AnzZeilen].Kaesten+Anz;
            Werte[AnzUebSpalten,AnzZeilen].Kunden := Werte[AnzUebSpalten,AnzZeilen].Kunden+1;
            Werte[AnzUebSpalten,AnzZeilen].Kaesten := Werte[AnzUebSpalten,AnzZeilen].Kaesten+Anz;
            IF Werte[AnzUebSpalten,AnzZeilen].Kunden <> 0 THEN
               KaKunRel := Werte[AnzUebSpalten,AnzZeilen].Kaesten/Werte[AnzUebSpalten,AnzZeilen].Kunden
            ELSE
               KaKunRel :=0;
         END;
      END;

   PROCEDURE SubFromKSum;
      VAR UebTag : BYTE;

      BEGIN
         UebTag:= 0;
         IF (Lieferung.Liefertag <> '') THEN BEGIN
            IF (Lieferung.Liefertag = AktLTag) THEN
               UebTag:= 1;
            IF (Lieferung.Liefertag = NaechsterLTag) THEN
               UebTag:= 2;
         END;
         IF (Lieferung.KNR <> '00000') AND ((UebTag = 1) OR (UebTag = 2)) THEN BEGIN
            ReadUebDatum(UebTag);
            SubFromUebersicht(Lieferung);
            WriteUebDatum(UebTag);
         END;
      END;

   PROCEDURE AddToKSum;
      VAR UebTag : BYTE;

      BEGIN
         UebTag:= 0;
         IF (Lieferung.LieferTag <> '') THEN BEGIN
            IF (Lieferung.Liefertag = AktLTag) THEN
               UebTag:= 1;
            IF (Lieferung.Liefertag = NaechsterLTag) THEN
               UebTag:= 2;
         END;
         IF (Lieferung.KNR <> '00000') AND ((UebTag = 1) OR (UebTag = 2))THEN BEGIN
            ReadUebDatum(UebTag);
            AddToUebersicht(Lieferung);
            WriteUebDatum(UebTag);
         END;
      END;

   PROCEDURE UebTagLesen(Datum:STRING);
      VAR   UebTag :BYTE;
      VAR   AbTour,
            BisTour : STRING;
            TagGes  : BYTE;
      VAR   Lieferung : TLiefDaten;
            Ende : BOOLEAN;
            Taste : INTEGER;

      BEGIN
         UebTag := 0;
         IF (Datum = AktLTag) THEN
            UebTag := 1;
         IF (Datum = NaechsterLTag) THEN
            UebTag := 2;
         IF (Datum <> AktLTag) AND (Datum <> NaechsterLTag) THEN
            UebTag := 3;
         IF (((UebTag >= 1) AND (UebTag <= 3))) THEN BEGIN
            KUebersichtInit(AktUebersicht);
            IF ((UebTag = 1) OR (UebTag = 2)) THEN BEGIN
               ReadUebDatum(UebTag);
            END;
            IF (AktUebersicht.Tag = '') THEN BEGIN
               Taste := 0;
               Ende:= FALSE;
               StatusBox.Act('Erstelle gerade öbersicht ',
                             ' fÅr den '+Datum);
               AktUeberSicht.Tag := Datum;
               AktUebersicht.OldDay := IsOldDay(Datum);
               TagGes := MakeIFieldVal(TourAbs+1,0);
               abTour := MakeDateIndex(Datum,MakeTabsIndex(TagGes,-1,AktUebersicht.OldDay));
               bisTour := MakeDateIndex(Datum,MakeTabsIndex(TagGes,1,AktUebersicht.OldDay));
               IF LieferDB.BeginTransaction THEN BEGIN
                  IF LieferDB.StartIntervall(3,abTour,BisTour,Lieferung) THEN BEGIN
                      REPEAT
                         AddToUebersicht(Lieferung);
                         Ende := (UebTag = 3) AND (KeyPress(Taste)) AND (Taste = 27);
                      UNTIL NOT (LieferDB.GetIntNext(Lieferung)) OR Ende;
                  END;
                  IF NOT LieferDB.EndTransaction THEN
                     FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                           LieferDB.GetTransactionErrMsg);
               END ELSE BEGIN
                  FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                     LieferDB.GetTransactionErrMsg);
               END;
               StatusBox.Hide;
               IF ((UebTag = 1) OR (UebTag = 2)) THEN
                  WriteUebDatum(UebTag);
            END;
         END;
      END;

   PROCEDURE UebDatumAnzeigen(Datum:STRING);
      VAR Lauf : BYTE;
      BEGIN
         UebTagLesen(Datum);
         UebDatum := AktUebersicht.Tag;
      END;

   FUNCTION UebTagAnzeigen;

      VAR   EndCode : INTEGER;
            Ergebnis: INTEGER;
            L : LONGINT;
            C : INTEGER;
      BEGIN
         MoreEval := FALSE;
         UebDatum := AktLTag;
         DispTagString(UebDatum);
         UebDatumAnzeigen(UebDatum);
         UeberSichtFenster^.Input;
         UeberSichtFenster^.Hide;
         Abbruch     := FALSE;
         Check := FALSE;
         UebTagAnzeigen := FALSE;
      END;

   PROCEDURE StartUpUebTag;
      VAR   Pfad : STRING;

      BEGIN
         Pfad := MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'));
         ReadUebDatum(1);
         IF (AktUebersicht.Tag <> AktLTag) THEN
            EraseFile(Pfad+UFileNamen[1]);
         ReadUebDatum(2);
         IF (AktUebersicht.Tag <> NaechsterLTag) THEN
            EraseFile(Pfad+UFileNamen[2]);
         KuebersichtInit(AktUebersicht);
         UebTagLesen(AktLTag);
         UebTagLesen(NaechsterLTag);
         UebDatum := AktUebersicht.Tag;
      END;

   FUNCTION RefreshUebersicht(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Pfad : STRING;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check := TRUE;
         RefreshUebersicht := TRUE;
         IF Request.Act('Sollen die Uebersichtstage erneut aus '+
                        'der Datenbank aufgebaut werden ?') THEN BEGIN
            Pfad := MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'));
            EraseFile(Pfad+UFileNamen[1]);
            EraseFile(Pfad+UFileNamen[2]);
            StartUpUebTag;
            UebersichtFenster^.Refresh('');
         END;
      END;
   FUNCTION CRDispTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check := TRUE;
         CrDispTag := TRUE;
         IF ValidDate(InpString) THEN BEGIN
            UebDatumAnzeigen(InpString);
            UebersichtFenster^.Refresh('');
         END;
      END;

   PROCEDURE KUeberSichtDruck;

      VAR UebTag :BYTE;
      VAR Lauf1,Lauf2 :BYTE;

      CONST Vorne : ARRAY[1..AnzZeilen] OF STRING[13] = ('Vormittags  ',
                                                         '            ',
                                                         'Nachmittags ',
                                                         '            ',
                                                         'Abends      ',
                                                         '            ',
                                                         'Gesamt      ');
      BEGIN
         WITH AktUebersicht DO BEGIN
            {$I-}
            WRITE(LST,'     öbersicht Lieferfahrten :',Tag);
            WRITELN(LST,REPLICATE(' ',10),'Stand : ',ActDate,',',ActTime);
            WRITELN(LST);
            WRITELN(LST,REPLICATE(' ',17),'| Schwelm ',
                                          '| Barmen  ',
                                          '| Ronsdf. ',
                                          '| Elberf. ',
                                          '| Cronenb ',
                                          '| Vohw.   ',
                                          '| Neue K. ',
                                          '| Gesamt  ');
            WRITELN(LST,REPLICATE(' ',17),'|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst',
                                          '|Kund|KÑst');
            WRITELN(LST,'     ',REPLICATE('-',12),
                                          '+----+----',
                                          '+----+----',
                                          '+----+----',
                                          '+----+----',
                                          '+----+----',
                                          '+----+----',
                                          '+----+----',
                                          '+----+----');
            FOR Lauf2 := 1 TO AnzZeilen DO BEGIN
               WRITE(LST,'     ');
               WRITE(LST,Vorne[Lauf2]);
               FOR Lauf1 := 1 TO AnzUebSpalten DO BEGIN
                  WRITE(LST,'|');
                  IF Werte[Lauf1,Lauf2].Kunden = 0 THEN
                     WRITE(LST,'    ')
                  ELSE
                     WRITE(LST,Werte[Lauf1,Lauf2].Kunden:4);
                  WRITE(LST,'|');
                  IF Werte[Lauf1,Lauf2].Kaesten = 0 THEN
                     WRITE(LST,'    ')
                  ELSE
                     WRITE(LST,Werte[Lauf1,Lauf2].Kaesten:4);
               END;
               WRITELN(LST);
            END;
            WRITELN(LST);
            WRITELN(LST);
            WRITELN(LST,'     Gesamt       -- KÑsten ',Werte[AnzUebSpalten,AnzZeilen].Kaesten:4,' / Kunden ',
                        Werte[AnzUebSpalten,AnzZeilen].Kunden:4,' = ',
                        PrintReal0(KaKunRel,5,2),' KÑsten / Kunden');
         END;
         WRITELN(LST,CHR(12));
         {$I+}
      END;

   PROCEDURE ShowKundenUebersicht;
      VAR X   : BYTE;
          Lauf : BYTE;
      BEGIN
         IF KundeUebersichtFenster^.Geschlossen THEN BEGIN
            X:= FindKXPos(S2L(Kunde.PlanQ));
            AktKBereich := SpTitel[X];
            ReadUebDatum(1);
            FOR Lauf := 1 TO AnzZeilen DO BEGIN
               KundenUebersicht[1,Lauf] := AktUebersicht.Werte[X,Lauf].Kunden;
            END;
            ReadUebDatum(2);
            FOR Lauf := 1 TO AnzZeilen DO BEGIN
               KundenUebersicht[2,Lauf] := AktUebersicht.Werte[X,Lauf].Kunden;
            END;
            KundeUebersichtFenster^.Show('');
         END;
      END;

   PROCEDURE HideKundenUebersicht;
      BEGIN
         KundeUebersichtFenster^.Hide;
      END;

   PROCEDURE InitUebersichtInput;
      VAR ULine,
          Lauf1,
          Lauf2: BYTE;
      BEGIN
         ResetPQ;
         SetPQ;
         AktLTag := ActDate;
         NaechsterLTag := NextWorkDay(AktLTag);
         NEW(UebDatumFenster);
         UebDatumFenster^.Init(205,210,435,270,0,'öbersichtstag','ASIN3',Aktiv);
         UebDatumFenster^.AddDate('','1','Datum:',1,1,UebDatum,'');

         NEW(KundeUebersichtFenster);
         KundeUebersichtFenster^.Init(530,5,640,155,4,'','',Aktiv);
         KundeUebersichtFenster^.DisableShadow;
         WITH KundeUebersichtFenster^ DO BEGIN
            AddCString('0',2,0,AktKBereich);
            AddConst('0',0,1,'Heute');
            AddConst('0',6,1,'Morg.');
            AddHLine('0',0,10,2,0);
            AddHLine('0',0,10,2,-2);
            Uline := 0;
            FOR Lauf2 := 1 TO AnzZeilen-1 DO BEGIN
               FOR Lauf1 := 1 TO 2 DO BEGIN
                  AddCLong('0',((Lauf1-1)*6),Lauf2+ULine+2,KundenUebersicht[Lauf1,Lauf2],3);
                  AddCLong('0',((Lauf1-1)*6),Lauf2+ULine+2,KundenUebersicht[Lauf1,Lauf2],3);
               END;
               IF ((Lauf2 MOD 2) = 0) THEN BEGIN
                     INC(Uline);
                     AddHLine('0',0,10,Lauf2+ULine+2,0);
               END;
            END;
            AddVline('0',5,1,AnzZeilen+Uline,0);
            AddCrossPoint('0',5,AnzZeilen+1+Uline,2);
         END;

         NEW(UebersichtFenster);
         UeberSichtFenster^.Init(5,220,635,430,4,'öbersichtstag','',Aktiv);
         WITH UEbersichtFenster^ DO BEGIN
            WITH AktUebersicht DO BEGIN
               DisableShadow;
               AddCString   ('35',35,1,TagAnzeige);
               SetCColor    ('35',8,6);
               SetCHeight    ('35',3);
               AddDate('','101','öbersichtstag',1,1,UebDatum,'');
               FOR Lauf1 := 1 TO AnzUebSpalten DO
                  AddConst('0',3+(8*Lauf1),3,SpTitel[Lauf1]);
               ULine := 0;
               FOR Lauf2 := 1 TO AnzZeilen DO BEGIN
                  AddConst('0',2,4+Lauf2+ULine,ZeTitel[Lauf2]);
                  FOR Lauf1 := 1 TO AnzUebSpalten DO BEGIN
                    AddCLong('0',3+(Lauf1*8),Lauf2+ULine+4,Werte[Lauf1,Lauf2].Kunden,3);
                    AddCLong('0',7+(Lauf1*8),Lauf2+ULine+4,Werte[Lauf1,Lauf2].Kaesten,3);
                  END;
                  IF (Lauf2 MOD 2 ) = 0 THEN BEGIN
                     INC(Uline);
                     AddHLine('0',1,74,Lauf2+ULine+4,0);
                  END;
               END;
               AddCReal('0',4,11+Uline,KaKunRel,3,2);
               AddHLine('0',1,74,4,0);
               AddHLine('0',1,74,4,-2);
               AddVline('0',10,3,AnzZeilen+4+ULine,-2);
               AddVline('0',10,4,AnzZeilen+4+ULine,-2);
               FOR Lauf1 := 1 TO AnzUebSpalten+1 DO BEGIN
                   AddVLine ('0',(Lauf1*8)+2,3,AnzZeilen+4+ULine,0);
                   AddVLine('0',(Lauf1*8)+2,3,AnzZeilen+4+ULine,-2);
                   AddVLine ('0',(Lauf1*8)+6,4,AnzZeilen+4+ULine,0);
               END;
            END;
            SetActionFunc('_',GMUeberAction);
            AddActionKey('_',27,ButtonQuit,FALSE,'');
            AddActionKey('_',-72,DoNothing,FALSE,'');
            AddActionKey('_',-80,DoNothing,FALSE,'');
            AddActionKey('_',9,DoNothing,FALSE,'');
            AddActionKey('_',-15,DoNothing,FALSE,'');
            AddActionKey('101',43,NaechsterTag,FALSE,'NÑchster Tag');
            AddActionKey('101',-60,RefreshUebersicht,FALSE,'öbersicht neuaufbauen');
            AddActionKey('101',13,CRDispTag,FALSE,
                              'Informationen zu eingebenem Tag anzeigen');
            AddActionKey('101',45,VorherigerTag,FALSE,'Vorheriger Tag');
            AddActionKey('_',-88,ButtonQuit,FALSE,'ZurÅck in Lieferschein');
            SetFKeyQuit(FALSE);
            SETCRQuit(FALSE);
         END;
         StartUpUebTag;
      END;


END.
{============================
 Versionshistorie
 $Log:$
 ============================}
