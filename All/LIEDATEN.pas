{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 17:10:12 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT LIEDaten;

INTERFACE

USES ZUSAETZE;

  CONST TAbsT = 10;
  CONST TourAbs = 3;
  CONST TGesamt = TAbsT*TourAbs;
  CONST TAbschnitt : ARRAY[1..TourAbs+2] OF STRING[14]=('Vormittags',
                                                'Nachmittags',
                                                'Abends',
                                                'Tag Gesamt',
                                                'Laden');

  CONST TourEintZeit: ARRAY[1..TourAbs] OF STRING[5] = ('12:00','16:00','24:00');
  CONST BisZeit : ARRAY[1..TourAbs] OF STRING[5] = ('13:00','17:00','22:00');
  CONST VonZeit : ARRAY[1..TourAbs] OF STRING[5] = ('08:00','13:00','17:00');


  TYPE TLiefPos   = RECORD
      Menge  : LONGINT;
      KAFL   : LONGINT;
      ArtNr  : STRING[5];
      ArtBez : STRING[40];
      Preis  : REAL;
      Pfand  : REAL;
  END;

  TYPE TLiefDaten = RECORD
      LiefNr       : STRING[5];
      Kundennummer : STRING[13];
      Knr          : STRING[5];
      LieferTag    : TDatum;
      TourNr       : WORD;
      TourLfdNr    : LONGINT;
      Fahrer       : STRING[30];
      Von,
      Bis          : ARRAY[1..3] OF TZeit;
      Positionen   : ARRAY[1..30] OF TLiefPos;
      Bemerkungen  : ARRAY[1..3] OF STRING[80];
      T100Bearbeitet: BOOLEAN;
      SollKaesten,
      IstNormal,
      IstSonder,
      IstLeerGut   : LONGINT;
      SollUmsatz,
      IstUmsatz    : REAL;
  END;

  FUNCTION MakeTAbsIndex(IFieldTNr:BYTE;Art:INTEGER; OldDay:BOOLEAN):WORD;
  { Erzeugt aus dem Wert eines Eingabefeldes der  Angabe Åber Art des Index
    und dem Typ des Datenbanktages eine Tour.
    Die Art des Indexs ist festgelegt auf
       0 = nur die angegebene Tour
       1 = hîchste Tournummer im Abschnitt
      -1 = niedrigste Tournummer im Abschnitt.
  FÅr gesamte Tage  wird bei den letzten beiden Typen die letzte Tour des letzten
  Tagesabschnittes bzw. die erste Tour des ersten Tagesabschnitt geliefert.}
  FUNCTION TNr2IField(TourNr:WORD):BYTE;
  {Erzeugt aus der  Tournummer in der Datenbank einen Eingabefeldwert }

  FUNCTION  MakeTNr(TAbs,ATour:BYTE;OldDay:BOOLEAN):BYTE;
  { Erzeugt aus Tagesabschnitt und Abschnittstournummer sowie dem
     Typ des Datenbanktages die zugehîrige TourNummer }
  PROCEDURE SplitTNr(TourNr:BYTE;VAR TAbs,ATour:BYTE);
  { Trennt die Tournummern in den DatensÑtzen in Tagesabsschnitt
    und laufende Tournummer im Abschnitt }
  FUNCTION ValidTNr(TourNr:BYTE;OldDay:BOOLEAN):BOOLEAN;
  { öberprÅft ob die angegebene Tournummer
     in der Datenbank zulÑssig ist }
  PROCEDURE SplitIFieldTNr(TourNr:BYTE;VAR TAbs,ATour:BYTE);
  { Trennt die Tournummer aus einem Eingaberollfeld in Tagesabsschnitt
    und laufende Tournummer im Abschnitt.
    Liefert fÅr Tagesabschnitte die Abschnittsnummer und die Tournummer 0}
  FUNCTION  MakeIFieldTStr(TourNr:BYTE):STRING;
  { Erzeugt den zu einer Tournummer aus dem Eingaberollfeld
     gehîrigen Tournamen }
  FUNCTION  MakeShortIFieldTStr(TourNr:BYTE):STRING;
  { Erzeugt den zu einer Tournummer aus dem Eingaberollfeld
    gehîrigen Tournamen in der Form AT[T] wobei A den Tagesabschnitt
    und T die Tournummer angibt. Wenn TabsT >= 10 dann wird die Tournummer
    zweistellig mit folgender Leerstelle angegeben.
     }
  FUNCTION  MakeIFieldVal(TAbs,ATour:BYTE):BYTE;
  { Erzeugt aus Tagesabschnitt und Tournummer den Eingabefeldwert }
  FUNCTION  MakeIFieldTNr(TourNr:BYTE;OldDay:BOOLEAN):WORD;
  { erzeugt aus der Tournummer im Eingabefeld und der Information ob der
    Liefertag neue Tournummer oder alte enthÑlt die Tournummer fÅr die
    Eingabe }
  FUNCTION  MakeTString(TAbs,ATour:BYTE):STRING;
  { Erzeugt aus Tagesabschnitt und Abschnittstournummer den zugehîrigen
     Tournamen, bei Tournummer mit dem Wert 0 liefert die Funktion den
     Tagesabschnitt }
  FUNCTION  MakeShortTString(TAbs,ATour:BYTE):STRING;
  { Erzeugt aus Tagesabschnitt und Abschnittstournummer den zugehîrigen
     Tournamen, bei Tournummer mit dem Wert 0 liefert die Funktion den
     TagesabschnittDer Tourname wird dargestell in der Form AT[T] wobei A
     den Tagesabschnitt und T die Tournummer angibt.
     Wenn TabsT >= 10 dann wird die Tournummer zweistellig mit folgender
     Leerstelle angegeben. }
  FUNCTION  MakeTourName(TourNr:WORD):STRING;
  { Erzeugt aus der Tournummer in der Datenbank den zugehîrigen
     Tournamen }
  FUNCTION  MakeShortTourName(TourNr:WORD):STRING;
  { Erzeugt aus der Tournummer in der Datenbank den zugehîrigen
     Tournamen in der Form AT[T] wobei A den Tagesabschnitt
    und T die Tournummer angibt. Wenn TabsT >= 10 dann wird die Tournummer
    zweistellig mit folgender Leerstelle angegeben.}
  FUNCTION  MakeMonthStr(Datum:STRING):STRING;
  { Erzeugt aus einem Datum die zugehîrige Monatsdateikennung in der
    Form MMJJ }
  FUNCTION  FirstMonthDay(Month:STRING):STRING;
  { Erzeugt aus einem Monat der Forma den ersten Abrechnungstag}
  FUNCTION  LastMonthDay(Month:STRING):STRING;
  { Erzeugt aus einem Monat der Forma den ersten Abrechnungstag}
  PROCEDURE InitTLiefDaten(VAR Daten:TLiefDaten);
  { Initialisiert die Lieferdaten }
  PROCEDURE Lie2Mask(VAR Daten:TLiefDaten);
  { öbertrÑgt die Lieferdaten in die Eingabemaske }
  PROCEDURE Mask2Lie(VAR Daten:TLiefDaten;OldDay:BOOLEAN);
   { öbertrÑgt die Eingabemaske in die Lieferdaten}
  FUNCTION  NextLNr(Nr:STRING):STRING;
   { Liefert die nÑchste Lieferscheinnummer nach drr Åbergebenen Nummer }
  FUNCTION  MakeLNr(Nr:LONGINT):STRING;
   { Transformiert eine Nummer in eine Lieferscheinnummer,
     momentan werden hier nur fÅhrende Nullen erzeugt.}
  FUNCTION  MakeDateIndex(Tag:STRING;Tour:WORD):STRING;
   { Erzeugt aus Tag und Tour den zugehîrigen IndexschlÅssel }
  FUNCTION  GetDateIndexMonth(Key:STRING):STRING;
   { Liefert den Monat aus einem IndexschlÅssel des Typs DateIndex}
  FUNCTION  MakeDLfdIndex(Tag:STRING;Tour:WORD;TourLfdNr:LONGINT):STRING;
   { Erzeugt aus Tag,Tour und laufender Tournummer den
     zugehîrigen IndexschlÅssel }
  FUNCTION  GetDLfdIndexMonth(Key:STRING):STRING;
   { Liefert den Monat aus einem IndexschlÅssel des Typs DLfdIndex}
  FUNCTION  GetDateFromKey(Key:STRING):STRING;
   { Liefert Datum+Tour aus einem IndexschlÅssel des Typs DLfdIndex}
  FUNCTION  MakeKundIndex(KNr:STRING;Liefertag:STRING;Tour:WORD):STRING;
   { Erzeugt aus Kunde,Tag  und Tour den zugehîrigen IndexschlÅssel }
  FUNCTION  GetKundIndexMonth(Key:STRING):STRING;
   { Liefert den Monat aus einem IndexschlÅssel des Typs KundIndex}
  FUNCTION  GetKundFromKey(Key:STRING):STRING;
   { Liefert den Kunden aus einem IndexschlÅssel des Typs KundIndex}
  FUNCTION  MakeFahrIndex(Fahrer:STRING;Liefertag:STRING;Tour:WORD):STRING;
   { Erzeugt aus Fahrer,Tag und Tour den zugehîrigen IndexschlÅssel }
  FUNCTION  GetFahrIndexMonth(Key:STRING):STRING;
   { Liefert den Monat aus einem IndexschlÅssel des Typs FahrIndex}
  FUNCTION  GetFahrFromKey(Key:STRING):STRING;
   { Liefert den Fahrer aus einem IndexschlÅssel des Typs KundIndex}
  FUNCTION  LowMonthDate(Month:STRING):STRING;
   { Liefert den ersten Datenbanktag fÅr den Åbergebenen
     Monat }
  FUNCTION  HighMonthDate(Month:STRING):STRING;
   { Liefert den letzten Datenbank fÅr den Åbergebenen
     Monat }
  FUNCTION EmptyLiefPos(Lieferung:Tliefdaten;NR:BYTE):BOOLEAN;
    { öberprÅft ob die angegebene Lieferposition eine
      leere Zeile enthÑlt }

  FUNCTION  LGleich(Lieferung1,Lieferung2:TLiefDaten):BOOLEAN;
    { Vergleicht zwie Lieferscheine miteinander }

IMPLEMENTATION

   FUNCTION MakeTAbsIndex;
      VAR Ergebnis : WORD;
          LastTour,
          LTAbs,LATour : BYTE;
      BEGIN
         Ergebnis := 0;
         CASE Art OF
            0 : BEGIN
                   Ergebnis := MakeIFieldTnr(IFieldTNr,OldDay);
                END;
            1 : BEGIN
                   SplitIFieldTNr(IFieldTNr,LTabs,LATour);
                   IF (LATour <> 0) AND (LTabs <= TourAbs) THEN BEGIN
                      Ergebnis := MakeIFieldTnr(IFieldTNr,OldDay);
                   END ELSE BEGIN
                      IF (LTabs = (TourAbs+1)) THEN
                         LTabs := TourAbs;
                      IF OldDay THEN
                         LATour :=5
                      ELSE
                         LATour:=TAbsT;
                      IF (LTabs = TourAbs+2) THEN
                         LATour := 0;
                      Ergebnis :=MakeTNr(LTabs,LATour,OldDay);
                   END;
                END;
            -1: BEGIN
                   SplitIFieldTNr(IFieldTNr,LTabs,LATour);
                   IF (LATour <> 0) AND (LTabs <= TourAbs) THEN BEGIN
                      Ergebnis := MakeIFieldTnr(IFieldTNr,OldDay);
                   END ELSE BEGIN
                      IF (LTAbs = (TourAbs+1)) THEN
                         LTabs := 1;
                      IF (LTabs = TourAbs+2) THEN
                         LATour := 0;
                      Ergebnis :=MakeTNr(LTabs,1,OldDay);
                   END;
                END;
         END;
         MakeTAbsIndex:= Ergebnis;
      END;

   FUNCTION MakeIFieldTStr;
      VAR TAbs,ATour: BYTE;

      BEGIN
         SplitIFieldTNr(TourNr,TAbs,ATour);
         MakeIFieldTStr := MakeTString(TAbs,ATour);
      END;

   FUNCTION MakeShortIFieldTStr;
      VAR TAbs,ATour: BYTE;

      BEGIN
         SplitIFieldTNr(TourNr,TAbs,ATour);
         MakeShortIFieldTStr := MakeShortTString(TAbs,ATour);
      END;

   FUNCTION MakeIFieldVal;
      VAR Ergebnis : BYTE;
      BEGIN
         Ergebnis := 0;
         IF (TAbs <= TourAbs) AND (TAbs > 0) AND
            (ATour > 0) AND (ATour <= TAbsT) THEN BEGIN
            Ergebnis := (TAbs-1)*TabsT+ATour;
         END ELSE BEGIN
            IF (Tabs <= (Tourabs+2)) AND (Tabs > 0) AND (ATour = 0) THEN
               Ergebnis := TabsT*TourAbs+Tabs;
         END;
         MakeIFieldVal := Ergebnis;
      END;

   FUNCTION MakeTourName;
      VAR TAbs,ATour : BYTE;
      BEGIN
         SplitTNr(TourNr,TAbs,ATour);
         MakeTourName := MakeTString(TAbs,ATour);
      END;

   FUNCTION MakeShortTourName;
      VAR TAbs,ATour : BYTE;
      BEGIN
         SplitTNr(TourNr,TAbs,ATour);
         MakeShortTourName := MakeShortTString(TAbs,ATour);
      END;

   FUNCTION MakeTString;
      BEGIN
         IF (TAbs > TourAbs) OR (ATour = 0) THEN
             MakeTString := TAbschnitt[TAbs]
         ELSE
             MakeTString := TAbschnitt[TAbs]+' '+B2S(ATour,0);
      END;

   FUNCTION MakeShortTString;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := TAbschnitt[Tabs][1];
         IF (TAbs <= TourAbs) AND (ATour <> 0) THEN
            Ergebnis := Ergebnis+B2S(ATour,0);
         IF TAbsT >= 10 THEN
            Ergebnis := Ergebnis + REPLICATE(' ',3-Length(Ergebnis))
         ELSE
            Ergebnis := Ergebnis + REPLICATE(' ',2-Length(Ergebnis));
         MakeShortTString := Ergebnis;
      END;

   PROCEDURE SplitTNr;
      BEGIN
         IF TourNr < 50 THEN BEGIN
            TAbs := ((Tournr -1) DIV 5) + 1;
            ATour :=((TourNr -1) MOD 5) + 1;
         END ELSE BEGIN
            TAbs := (Tournr DIV 50);
            ATour :=(TourNr MOD 50);
         END;
      END;

   FUNCTION ValidTNR;
      VAR Ergebnis : BOOLEAN;
          LTabs,
          LAtour   : BYTE;
      BEGIN
         SplitTnr(TourNr,LTabs,LATour);
         Ergebnis := (MakeTnr(LTabs,LATour,OldDay) = TourNr);
         Ergebnis := Ergebnis AND (LTabs > 0);
         IF (LTabs = TourAbs+2) THEN
            Ergebnis := Ergebnis AND (LATour = 0)
         ELSE BEGIN
            Ergebnis := Ergebnis AND (LTabs <= TourAbs) AND (LATour > 0);
            Ergebnis := Ergebnis AND ((LATour <= 5) OR
                ((NOT OldDay) AND (LATour <= TabsT)));
         END;
         ValidTNr := Ergebnis;
      END;


   FUNCTION MakeTNr;
      VAR Ergebnis : BYTE;
      BEGIN
         Ergebnis := 0;
         IF OldDay THEN BEGIN
            IF (ATour <= 5) THEN
               Ergebnis := 5*(TAbs-1)+ATour;
            IF (TAbs = 5) THEN
               Ergebnis := 20;
         END ELSE BEGIN
            Ergebnis := 50*Tabs+ATour;
         END;
         MakeTNr := Ergebnis;
      END;

   PROCEDURE SplitIFieldTNr;
      BEGIN
         TAbs := ((TourNr -1) DIV TAbsT) +1;
         ATour :=((TourNr -1) MOD TAbsT) +1;
         IF (TAbs > TourAbs) THEN BEGIN
            TAbs := ATour;
            ATour := 0;
         END;
      END;

   FUNCTION MakeIFieldTNr;
      VAR Ergebnis : WORD;
          TAbs,
          ATour   : BYTE;

      BEGIN
         Ergebnis := 0;
         SplitIFieldTNr(TourNr,TAbs,ATour);
         Ergebnis:=MakeTNr(TAbs,ATour,OldDay);
         MakeIFieldTNr := Ergebnis;
      END;

   FUNCTION TNr2IField;
      VAR LTabs,
          LTour : BYTE;
      BEGIN
         SplitTNr(TourNr,LTabs,LTour);
         TNr2IField := MakeIFieldVal(LTabs,LTour);
      END;

   FUNCTION MakeMonthStr;
      VAR Monat : BYTE;
          Jahr  : BYTE;
      BEGIN
         Monat := S2L(Datum[4]+Datum[5]);
         Jahr  := S2L(Datum[7]+Datum[8]);
         IF S2L(LeftStr(Datum,2)) > 20 THEN BEGIN
            Monat := Monat+1;
            IF Monat = 13 THEN BEGIN
               Monat := 1;
               Jahr := Jahr +1;
               IF Jahr = 100 THEN
                  Jahr := 0;
            END;
         END;
         MakeMonthStr := LeadingZeros(Monat,2)+LeadingZeros(Jahr,2);
      END;

   FUNCTION FirstMonthDay;
      VAR Monat,Jahr :BYTE;
      BEGIN
         Monat := S2l(Month[1]+Month[2]);
         JAHR := S2l(Month[3]+Month[4]);
         DEC(Monat);
         IF (Monat = 0) THEN BEGIN
            Monat := 12;
            IF (Jahr = 0) THEN
               Jahr := 99
            ELSE
                DEC(Jahr)
         END;
         FirstMonthDay := '21.'+LeadingZeros(Monat,2)+'.'+LeadingZeros(Jahr,2);
      END;

   FUNCTION LastMonthDay;
      BEGIN
         LastMonthDay := '20.'+COPY(Month,1,2)+'.'+Copy(Month,3,2);
      END;

   PROCEDURE InitTLiefDaten;
     VAR Lauf: BYTE;

     BEGIN
         WITH Daten DO BEGIN
         LiefNr := '';Kundennummer := '';Knr := '';
         Fahrer := '';
         TourNr := 1;
         TourLfdNr := 0;
         T100Bearbeitet := FALSE;
         FOR Lauf := 1 TO 30 DO
           WITH Positionen[Lauf] DO BEGIN
             Menge := 0;ArtNr := '';ArtBez := '';Preis := 0;
             KAFL:= 0;Pfand:=0;
           END;
         LieferTag := '';
         FOR Lauf := 1 TO 3 DO BEGIN
               Von[Lauf] := '';
               Bis[Lauf] :='';
               Bemerkungen[Lauf] := '';
         END;
         SollKaesten := 0;
         SollUmsatz := 0;
         IstUmsatz:= 0;
         IstLeergut := 0;
         IstNormal := 0;
         IstSonder := 0
       END;
	  END;

   PROCEDURE Lie2Mask;
     VAR Lauf: BYTE;
         HString : STRING;
         TAbs,
         ATour   : BYTE;

     BEGIN
        WITH Daten DO BEGIN
           FOR Lauf := 1 TO 30 DO BEGIN
              IF (Positionen[Lauf].ArtNr = '99999') THEN
                 Positionen[Lauf].ArtNr := '888'
              ELSE
                 Positionen[Lauf].ArtNr :=
                   COPY(Positionen[Lauf].ArtNr,
                        3,LENGTH(Positionen[Lauf].ArtNr)-2);

           END;
           SplitTNr(TourNr,TAbs,ATour);
           TourNr := MakeIFieldVal(Tabs,ATour);
	END;
     END;

   PROCEDURE Mask2Lie;
     VAR Lauf: BYTE;
         HString : STRING;

     BEGIN
        WITH Daten DO BEGIN
           FOR Lauf := 1 TO 30 DO BEGIN
              IF (Positionen[Lauf].ArtNr = '888') THEN
                 Positionen[Lauf].ArtNr := '99999'
              ELSE IF  (Positionen[Lauf].ArtNr <> '') THEN
                 Positionen[Lauf].ArtNr := '01'+Positionen[Lauf].ArtNr;
           END;
           TourNr := MakeIFieldTNr(TourNr,OldDay);
	END;
     END;

   FUNCTION EmptyLiefPos;
     VAR Ergebnis : BOOLEAN;

     BEGIN
        Ergebnis := FALSE;
        WITH Lieferung.Positionen[Nr]  DO BEGIN
          Ergebnis := Ergebnis OR (ArtNr <> '') OR (Menge <> 0) OR
                     (ArtBez <> '') OR (Preis <> 0);
        END;
        EmptyLiefPos := NOT Ergebnis;
     END;


   FUNCTION LGleich;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := TRUE;
         WITH Lieferung1 DO BEGIN
            Ergebnis := Ergebnis AND (LiefNr = Lieferung2.LiefNr)
                                 AND (Kundennummer = Lieferung2.Kundennummer)
                                 AND (Knr = Lieferung2.Knr)
                                 AND (Fahrer = Lieferung2.Fahrer)
                                 AND (TourNr = Lieferung2.TourNr)
                                 AND (TourLfdNr = Lieferung2.TourLfdNr)
                                 AND (T100Bearbeitet = Lieferung2.T100Bearbeitet)
                                 AND (LieferTag = Lieferung2.Liefertag)
                                 AND (SollKaesten = Lieferung2.SollKaesten)
                                 AND (IstLeergut = Lieferung2.IstLeergut)
                                 AND (SollUmsatz = Lieferung2.SollUmsatz)
                                 AND (IstUmsatz = Lieferung2.IstUmsatz)
                                 AND (IstNormal = Lieferung2.IstNormal)
                                 AND (IstSonder = Lieferung2.IstSonder);
            IF Ergebnis THEN BEGIN
               FOR Lauf := 1 TO 3 DO
                  Ergebnis := Ergebnis AND (Von[Lauf] = Lieferung2.Von[Lauf])
                                       AND (Bis[Lauf] = Lieferung2.Bis[Lauf])
                                       AND (Bemerkungen[Lauf] = Lieferung2.Bemerkungen[Lauf]);
               IF Ergebnis THEN BEGIN
                  FOR Lauf := 1 TO 30 DO
                     Ergebnis := Ergebnis AND (Positionen[Lauf].Menge = Lieferung2.Positionen[Lauf].Menge)
                                          AND (Positionen[Lauf].KAFL = Lieferung2.Positionen[Lauf].KAFL)
                                          AND (Positionen[Lauf].ArtNr = Lieferung2.Positionen[Lauf].ArtNr)
                                          AND (Positionen[Lauf].ArtBez = Lieferung2.Positionen[Lauf].ArtBez)
                                          AND (Positionen[Lauf].Pfand = Lieferung2.Positionen[Lauf].Pfand)
                                          AND (Positionen[Lauf].Preis = Lieferung2.Positionen[Lauf].Preis);

               END;
            END;
         END;
         LGleich := Ergebnis;
       END;


  FUNCTION MakeLNr;
  BEGIN
    MakeLnr:=LeadingZeros(Nr,5);
  END;

  FUNCTION NextLNr;
     VAR AktLnr : LONGINT;

  BEGIN
    { Angepasst am 08.01.2000, bei öberschreiten der Grenze von 99999
      wird die Lieferscheinnummer des nÑchsten Lieferscheins mit 1
      belegt }
    AktLNr := S2L(Nr)+1;
    IF (AktLNr = 100000) THEN
       AktLNr := 1;
    NextLNr :=LeadingZeros(AktLNr,5);
  END;

  FUNCTION MakeDateIndex;
    VAR Ergebnis,
        ZwErgeb : STRING;
    BEGIN
      Ergebnis :=Date2DBIndex(Tag);
      STR(Tour,ZWErgeb);
      WHILE LENGTH(ZwErgeb) < 3 DO
        ZwErgeb:='0'+ZwErgeb;
      Ergebnis := Ergebnis+ZwErgeb;
      MakeDateIndex := Ergebnis;
    END;

  FUNCTION GetDateIndexMonth;
    VAR Datum : STRING;
    BEGIN
       Datum := DBIndex2Date(COPY(Key,1,8));
       GetDateIndexMonth:= MakeMonthStr(Datum);
    END;

  FUNCTION MakeDLfdIndex;
    BEGIN
      MakeDLfdIndex:= MakeDateIndex(Tag,Tour)+LeadingZeros(TourLfdNr,4);
    END;

  FUNCTION GetDLfdIndexMonth;
     BEGIN
       GetDLfdIndexMonth := GetDateIndexMonth(Key);
     END;

  FUNCTION GetDateFromKey;
     BEGIN
       GetDateFromKey := COPY(Key,3,13);
     END;

  FUNCTION MakeKundIndex;
    BEGIN
      MakeKundIndex := KNr+REPLICATE(' ',5-LENGTH(Knr))+MakeDateIndex(Liefertag,Tour);
    END;

  FUNCTION GetKundIndexMonth;
     BEGIN
       GetKundIndexMonth := GetDateIndexMonth(RightStr(Key,6));
     END;

  FUNCTION GetKundFromKey;
     BEGIN
       GetKundFromKey := COPY(Key,1,5);
     END;

  FUNCTION MakeFahrIndex;
    BEGIN
      MakeFahrIndex := Fahrer+REPLICATE(' ',30-LENGTH(Fahrer))+
                       MakeDateIndex(Liefertag,Tour);
    END;

  FUNCTION GetFahrIndexMonth;
     BEGIN
       GetFahrIndexMonth := GetDateIndexMonth(RightStr(Key,31));
     END;


  FUNCTION GetFahrFromKey;
     BEGIN
       GetFahrFromKey := COPY(Key,1,30);
     END;
  FUNCTION LowMonthDate;
     VAR PMonth: STRING;
     BEGIN
        PMonth := PrevMonth(Month);
        LowMonthDate:= '21.'+Copy(PMonth,1,2)+'.'+Copy(PMonth,3,2);
     END;
  FUNCTION HighMonthDate;
     BEGIN
        HighMonthDate:= '20.'+Copy(Month,1,2)+'.'+Copy(Month,3,2);
     END;



END.



{============================
 Versionshistorie
 $Log:$
 ============================}
