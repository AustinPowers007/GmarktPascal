{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 17:55:18 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GLDRUCK;

INTERFACE

   USES LIEDATEN,
        KUNDATEN;

   PROCEDURE LDrucken(Lieferung:TLiefDaten);

   FUNCTION  Tourplanandruck(Lieferung:TLiefDaten;Kunde:TKundenDaten;
                             KundeGefunden:BOOLEAN;Nr:BYTE):BOOLEAN;
   PROCEDURE OPsDrucken;
   FUNCTION  DruckeTourplanUeberSchrift(Lieferung:TLiefDaten):BOOLEAN;


   FUNCTION DruckeAktuelleLieferung(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;

   PROCEDURE WriteTextDatei(Datei:STRING);

IMPLEMENTATION

   USES  GMT100Dr,
        GMDATEI,
        GMCONST,
         GMPRT,
         GMBASE,
         GMSETUP,
         STRINGS,
         ZUSAETZE,
         CASHDATA,
         PLACEWIN,
         PRINTER;


   TYPE TPfandPtr = ^TPfandRec;
        TPfandRec = RECORD
                       NextPRec : TPfandPtr;
                       Pfand    : REAL;
                       Menge    : LONGINT;
                       Ausgabe  : STRING[40];
                    END;

   VAR Zeilen : WORD;

   PROCEDURE WritelnPrt(Txt:STRING);
      BEGIN
         {$I-}
            WRITELN(LST,Txt);
            INC(Zeilen);
         {$I+}
      END;

   PROCEDURE WriteTextDatei(Datei:STRING);
      VAR T:    TEXT;
          Stng: STRING;
      BEGIN
        ASSIGN(T,Datei);
        {$I-} RESET(T); {$I+}
        IF (IORESULT = 0) THEN BEGIN
          WHILE (IORESULT = 0)AND NOT EOF(T) DO  BEGIN
            {$I-} READLN(T,Stng); {$I+}
            WritelnPrt(Stng);
          END;
          {$I-}
          WRITE(LST,CHR(12));
          {$I+}
        END;
        {$I-} CLOSE(T); {$I+}
        IF (IORESULT <> 0) THEN ;
      END;


   FUNCTION EtagenText(Etage:STRING):STRING;
      Var Txt : STRING;
      BEGIN
         Txt := '';
         IF (Etage = 'G') THEN
            Txt := 'Ware kommt in die Garage.';
         IF (Etage = 'K') THEN
            Txt := 'Ware kommt in den Keller.';
         IF (Etage = 'E') THEN
            Txt := 'Erdgescho�';
         IF (Etage ='HP') THEN
            Txt := 'Hochpaterre';
         IF (Etage = 'TP') THEN
            Txt := 'Tiefpaterre';
         IF IsNumber(Etage) THEN
            Txt := Etage+'. Etage';
         EtagenText:= Txt;
      END;

   FUNCTION PrintAdresseTour(Stng:STRING;Lieferung:TLiefDaten;Kunde:TKundenDaten;
                          KundeGefunden:BOOLEAN):BOOLEAN;

      VAR Txt      : STRING;
          TxtArray : ARRAY[1..9]  OF STRING[80];
          MaxArrayPos : BYTE;
          Lauf     : BYTE;
          WegVorh  : BOOLEAN;
      BEGIN
         {$I-}
         Writeln(LST,Stng);
         Writeln(LST);
         FOR Lauf := 1 TO 9 DO
            TxtArray[Lauf]:= '';
         MaxArrayPos := 0;
         WITH Kunde DO BEGIN

            WegVorh := FALSE;
            Txt := '';
            IF KundeGefunden THEN
               Writeln(LST,Anr[LiefAnrede]) { Zeile  12}
            ELSE
               Writeln(LST);
            IF KundeGefunden THEN BEGIN
               Txt := '';
               IF Firma THEN
                  Txt :=LiefName
               ELSE BEGIN
                  IF (LiefTitel <>'') THEN
                     Txt := LiefTitel+' ';
                  IF (LiefVorname <> '') THEN
                     Txt :=Txt+LiefVorname+' ';
                  Txt := Txt+LiefName;
               END;
               INC(MaxArrayPos);TxtArray[MaxArrayPos]:= Txt;
               IF Firma AND (LiefAPartner <> '') THEN BEGIN
                  INC(MaxArrayPos);
                  TxtArray[MaxArrayPos]:=Anr[LiefAPAnrede]+' ';
                  IF (LiefAPTitel <> '') THEN
                     TxtArray[MaxArrayPos] :=TxtArray[MaxArrayPos]+LiefApTitel+' ';
                  TxtArray[MaxArrayPos] :=TxtArray[MaxArrayPos]+LiefAPartner;
               END;
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := LiefStrasse;
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := L2S(LiefPlz,5)+'  '+LiefOrt;;
               INC(MaxArrayPos);
               TxtArray[MaxArrayPos] := 'Telefon:'+PrintString(Telefon[1],46)+
                                        'PQ:   '+PlanQ;
               INC(MaxArrayPos);
               TxtArray[MaxArrayPos] := Replicate(' ',54)+'Falk: '+PlanqFalk;
               WITH Lieferung DO BEGIN
                  TxtArray[1] := PrintString(TxtArray[1],54)+'Von '+PrintString(Von[1],5)+
                                       ' bis '+PrintString(Bis[1],5);
                  TxtArray[2] := PrintString(TxtArray[2],54)+'Von '+PrintString(Von[2],5)+
                                       ' bis '+PrintString(Bis[2],5);
               END;
               FOR Lauf := 1 TO MaxArrayPos DO
                  Writeln(LST,TxtArray[Lauf]);
               Writeln(LST);
               IF (ZahlWeise <> 1) THEN BEGIN
                  IF ((ZahlWeise >= 0) AND (ZahlWeise <= ZahlweiseTypen)) THEN
                     Writeln(LST,ZahlWeiseText[ZahlWeise]);
               END;
               IF (Etage <> '') THEN BEGIN
                  Writeln(LST,'Wegbeschreibung  '+EtagenText(Etage));
                  WegVorh := TRUE;
               END;
               FOR Lauf := 1 TO 8 DO
                  IF WegBes[Lauf] <> '' THEN BEGIN
                     IF NOT WegVorh THEN BEGIN
                        Writeln(LST,'Wegbeschreibung  '+WegBes[Lauf]);
                        WegVorh := TRUE;
                     END ELSE
                        Writeln(LST,'                 '+WegBes[Lauf]);
                  END;
            END;
         END;
         Writeln(LST,REPLICATE('-',78));
         {$I+}
         PrintAdresseTour:= (IORESULT = 0)
      END;

   PROCEDURE PrintAdresse(Kunde:TKundenDaten;
                          KundeGefunden:BOOLEAN);

      VAR Txt      : STRING;
          TxtArray : ARRAY[1..9] OF STRING[80];
          MaxArrayPos : BYTE;
          Lauf     : BYTE;
          HLauf   : BYTE;
          WegZeilen,
          Lauf2 : BYTE;
          LeerZeilenVorher : BYTE;
          LeerZeichenVorher: BYTE;

      BEGIN
         LeerZeilenVorher := 0;
         LeerZeilenVorher := S2B(GMEnvObj^.GetEntry('VERTIADRV'));
         LeerZeichenVorher := 0;
         LeerZeichenVorher := S2B(GMEnvObj^.GetEntry('HORIZADRV'));
         WHILE LeerZeilenVorher > 0 DO BEGIN
            WritelnPrt('');
            LeerZeilenVorher := LeerZeilenVorher-1;
         END;

         FOR Lauf := 1 TO 9 DO
            TxtArray[Lauf] := REPLICATE(' ',LeerZeichenVorher);
         {$I-}
         WITH Kunde DO BEGIN
            Lauf := 1;
            WegZeilen:= 0;
            IF KundeGefunden THEN BEGIN
               MaxArrayPos := 0;
               Txt:='';
               IF KundeGefunden THEN
                  Txt := Anr[RechAnrede];
               INC(MaxArrayPos);
               TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+Txt;
               Txt:='';
               IF Firma THEN BEGIN
                  Txt :=RechName
               END ELSE BEGIN
                  IF (RechTitel <> '') THEN
                     Txt := Txt+Rechtitel+' ';
                  IF (RechVorname <> '') THEN
                     Txt :=Txt+RechVorname+' ';
                  Txt := Txt+RechName;
               END;
               INC(MaxArrayPos);
               TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+Txt;
               IF Firma AND (RechAPartner <> '') THEN BEGIN
                  INC(MaxArrayPos);
                  TxtArray[MaxArrayPos]:=TxtArray[MaxArrayPos]+Anr[RechAPAnrede]+' ';
                  IF (RechAPTitel <> '') THEN
                     TxtArray[MaxArrayPos]:=TxtArray[MaxArrayPos]+RechAPTitel+' ';
                  TxtArray[MaxArrayPos]:=TxtArray[MaxArrayPos]+RechAPartner;
               END;
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+RechStrasse;
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+L2S(RechPlz,5)+'  '+RechOrt;
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+'';
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+'';
               INC(MaxArrayPos);TxtArray[MaxArrayPos] := TxtArray[MaxArrayPos]+'Telefon:'+Telefon[1];
            END;
            HLauf :=0;
            IF (Etage <> '') THEN BEGIN
               TxtArray[1] :=PrintString(TxtArray[1],38)+EtagenText(Etage);
               HLauf := 1;
            END;
            FOR Lauf:= 1 TO WegZeilen DO
               TxtArray[Lauf+HLauf] := PrintString(TxtArray[Lauf+HLauf],38)+WegBes[Lauf];
            IF MaxArrayPos < (WegZeilen+HLauf) THEN
               MaxArrayPos := WegZeilen+HLauf;
            FOR Lauf := 1 TO MaxArrayPos DO
               WritelnPrt(TxtArray[Lauf]);
         END;
         {$I+}
      END;

   PROCEDURE DruckeLiefKopf(Lieferung:TliefDaten;Kunde:TKundenDaten;
                            KundeGefunden:BOOLEAN;Seite:BYTE;PosTitel:BOOLEAN);
      VAR Txt : STRING;
          KAdr : STRING;
          TAbs,
          ATour : BYTE;
      BEGIN
           KAdr:= ' ';
           IF KundeGefunden THEN
              KAdr := PrintLong0(Kunde.LieferANR,1);
           SplitTNr(Lieferung.TourNr,TAbs,ATour);
           WritelnPrt('');
           WritelnPrt(StrPas(PrintStyles[Bold])+GMEnvObj^.GetEntry('KOPFZEILE1')
                      +StrPas(PrintStyles[NoBold]));
           WritelnPrt(StrPas(PrintStyles[Bold])+GMEnvObj^.GetEntry('KOPFZEILE2')+
                      StrPas(PrintStyles[NoBold]));
           WritelnPrt('');
           WritelnPrt(REPLICATE('=',78));

           IF (PrgLaden) THEN BEGIN
             Txt :='� Quittung Barverkauf                                        '+
                    StrPas(PrintStyles[Bold])+' Datum: '+Lieferung.Liefertag+
                    StrPas(PrintStyles[NoBold])+'�';
             WritelnPrt(Txt);
           END ELSE BEGIN
             Txt :='| '+StrPas(PrintStyles[Bold])+'Kundennr.: '+
                    Lieferung.KNr+StrPas(PrintStyles[NoBold])+'  |'+KAdr+'| '+
                    'Lieferschein/Rechnung '+Lieferung.LiefNr+' |'+
                    StrPas(PrintStyles[Bold])+' Liefertag: '+
                    Lieferung.Liefertag+'/'+TAbschnitt[TAbs][1]+
                    PrintString(L2S(ATour,0),2)+StrPas(PrintStyles[NoBold])+'|';
             WritelnPrt(Txt);
             IF (Seite > 1) THEN
                Txt := '| '+REPLICATE(' ',18)+'| |'+REPLICATE(' ',20)+'Seite '+L2S(Seite,2)+' | '
             ELSE
                Txt := '| '+REPLICATE(' ',18)+'| |'+REPLICATE(' ',28)+' | ';
             Txt := Txt+StrPas(PrintStyles[Bold])+
                      'Von '+PrintString(Lieferung.Von[1],5)+
                     ' bis '+PrintString(Lieferung.Bis[1],5)+StrPas(PrintStyles[NoBold])+ '    |';
             WritelnPrt(Txt);
           END;

           WritelnPrt(REPLICATE('=',78));

           IF (Seite = 1) AND NOT (PrgLaden) THEN
              PrintAdresse(Kunde,KundeGefunden);
           IF PosTitel THEN BEGIN
              WritelnPrt('');
              WritelnPrt(StrPas(PrintStyles[Uline])+'Pos Menge '+' Artikel'+REPLICATE(' ',34)+' Einzelpreis  '+
                         'GesamtPreis '+StrPas(PrintStyles[NoUline]));
           END;
      END;

   PROCEDURE DruckeLiefFuss;
      BEGIN
           WHILE (Zeilen < PrinterCodes.PageLength -3) DO
              WritelnPrt('');
           WritelnPrt(REPLICATE('-',78));
           WritelnPrt(StrPas(PrintStyles[Bold])+
                      REPLICATE(' ',((PrinterCodes.LineLengthNorm-LENGTH(GMEnvObj^.GetEntry('FUSSZEILE1'))) DIV 2))+
                      GMEnvObj^.GetEntry('FUSSZEILE1')+StrPas(PrintStyles[NoBold]));
           {$I-}
           Write(LST,StrPas(PrintStyles[Bold])+
                      REPLICATE(' ',((PrinterCodes.LineLengthNorm-LENGTH(GMEnvObj^.GetEntry('FUSSZEILE2'))) DIV 2))+
                      GMEnvObj^.GetEntry('FUSSZEILE2')+StrPas(PrintStyles[NoBold]));
           WritelnPrt(CHR(12));
           {$I+}
           Zeilen := 0;
      END;

   { *****************************************************************
     *  Lieferschein drucken
     ***************************************************************** }


   PROCEDURE LDrucken;

      CONST FootLineLength   = 3;
            PfandBlockLength = 7;
            MustBlockLength  = 6;

      VAR GewNoLength : INTEGER;
          MaxPosLines : INTEGER;
      VAR Druckdaten,
          OPDaten : TKundenDaten;
      VAR Seitenzahl : WORD;
          PrintFootLine,
          PrintPfandBlock,
          ShortGewNumbers,
          WegVorh    : BOOLEAN;
          Taste : INTEGER;
          AnzProZeile,
          AnzZeich : BYTE;
          PtrLauf,
          PfandListe : TPfandPtr;
          MWST       : REAL;
          MWSTAnteil :REAL;
          PfandPos,
          PosLauf,
          L,Lauf : WORD;
          PosUnterStrich,
          LetztePos :WORD;
          ZwischenWert,
          ZwischenSumme1,
          ZwischenSumme2,
          ZwischenSumme3,
          GesamtSumme : REAL;
          OPGefunden: BOOLEAN;
          KundeGefunden,
          D: BOOLEAN;
          KeineUebertragung : BOOLEAN;
          Txt : STRING;
          T: STRING[3];
          LW:STRING[10];
          LeerWert,
          BlockCount,GewNo,
          GewAnfang,GewEnde : LONGINT;

      PROCEDURE NewPfand(VAR DataPtr:TPfandPtr;Menge:LONGINT;
                           Pfand:REAL;PPos:WORD);
         VAR NewPtr : TPfandPtr;
         BEGIN
            NEW(NewPtr);
            NewPtr^.NextPRec := NIL;
            NewPtr^.Menge := Menge;
            NewPtr^.Pfand := Pfand;
            NewPtr^.Ausgabe := 'Pfand (Pos:'+L2S(PPos,0);
            DataPtr:= NewPtr;
         END;

      BEGIN
         KeineUebertragung := FALSE;
         FOR Lauf:= 1 TO 19 DO BEGIN
            IF (Lieferung.Positionen[Lauf].ArtNr = '777') OR
               (Lieferung.Positionen[Lauf].ArtNr = '01777') THEN
               KeineUebertragung := TRUE;
         END;
         InitTKundenDaten(Druckdaten);
         OpGefunden := FALSE;
         KundDB.BeginTransaction;
         Kundegefunden := KundDB.GetRec(1,Lieferung.Kundennummer,Druckdaten);
         IF NOT KundeGefunden THEN
            KundeGefunden := KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.KNR),0),Druckdaten);
         IF KundeGefunden AND (NOT KeineUebertragung) AND
             ((Druckdaten.Zahlweise = 0) OR (Druckdaten.Zahlweise = 1) OR
              (Druckdaten.Zahlweise = 8) OR (Druckdaten.Zahlweise = 9)) AND 
              (NOT Lieferung.T100Bearbeitet) THEN BEGIN
            IF KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.KNR),0),Opdaten) THEN BEGIN
               FOR Lauf := 1 TO 10 DO
                  WITH OPDaten.Oposten[Lauf] DO BEGIN
                      OPGefunden := OPGefunden OR ((LiefNr <> '') OR (Betrag <> 0)
                               OR (Art <> #0) OR (Bemerk <> ''));
                  END;
            END;
         END;
         KundDB.EndTransaction;
         IF NOT KundeGefunden THEN
            FaultBox.Act(0,'Lieferschein '+Lieferung.LiefNr+', Kunde '+Lieferung.KNR+
                           ' ist nicht in Kundendatenbank');
         IF OPGefunden THEN
            FaultBox.Act(0,'Bei Kunde '+Lieferung.Knr+ ' existieren noch offene Posten. '+
                  'Lieferschein mu� eventuell erst in Tour gespeichert werden.');
         Seitenzahl:=1;
         Taste := 0;
         WegVorh := FALSE;
         Zeilen := 0;
         LetztePos := 30;
         GewNoLength := 0;
         IF (Lieferung.Von[3] <> '') THEN BEGIN
            GewNoLength := S2L(Lieferung.Bis[3]);
            IF GewNoLength < S2L(Lieferung.Von[3]) THEN
               GewNoLength := GewNoLength+10000;
            GewNoLength := GewNoLength-S2L(Lieferung.Von[3]);
            GewNoLength := GewNoLength DIV 5 + 1;
         END;
         MaxPosLines := PrinterCodes.PageLength-MustBlockLength;
         IF (GewNoLength > 0) THEN BEGIN
            DEC(MaxPosLines);
            DEC(GewNoLength);
         END;
         PrintFootLine := TRUE;
         ShortGewNumbers := FALSE;
         PrintPfandBlock := TRUE;
         WITH Lieferung DO BEGIN
            WHILE (LetztePos > 0) AND (Positionen[LetztePos].Menge = 0)
                  AND (Positionen[LetztePos].ArtBez = '')
                  AND (Positionen[LetztePos].Preis = 0)
                  AND (Positionen[LetztePos].Pfand = 0) DO
               DEC(LetztePos);
            PosUnterStrich := LetztePos;
            WHILE (PosUnterStrich > 0)
                  AND (Positionen[PosUnterStrich].Preis = 0)
                  AND ((Positionen[PosUnterStrich].ArtBez = '')
                       OR (COPY(Positionen[PosUnterStrich].ArtBez,1,5) = 'Pfand')) DO
                  DEC(PosUnterStrich);
         END;
         DruckeLiefKopf(Lieferung,Druckdaten,KundeGefunden,Seitenzahl,TRUE);
         ZwischenSumme1 := 0;ZwischenSumme2 := 0; Zwischensumme3 := 0;
         GesamtSumme := 0;
         Lauf := 1;
         Poslauf := 0;
         WHILE (Lauf<= LetztePos) DO
            WITH Lieferung.Positionen[Lauf] DO BEGIN
               IF ((Preis <> 0) OR
                  ((ArtBez <>'') AND (COPY(ArtBez,1,5) <> 'Pfand'))) THEN BEGIN
                  INC(PosLauf);
                  ZwischenWert := Preis*Menge;
                  ZwischenSumme1 := ZwischenSumme1 +ZwischenWert;
                  Txt := ' '+L2S(PosLauf,2)+' ';
                  IF Menge <> 0 THEN
                     Txt :=Txt+L2S(Menge,5);
                  Txt := Txt+REPLICATE(' ',9-LENGTH(Txt));
                  Txt := Txt+'  '+Artbez+REPLICATE(' ',43-LENGTH(ArtBez));
                  IF (Lauf = PosUnterStrich) THEN
                     Txt := Txt + StrPas(PrintStyles[ULine]);
                  IF Preis <> 0 THEN
                     Txt := Txt+R2S(Preis,6,2)+' '+Rechnungswaehrung+'  '
                  ELSE
                     Txt:=Txt+REPLICATE(' ',12);
                  IF ZwischenWert <> 0 THEN
                     Txt := Txt+R2S(ZwischenWert,7,2)+' '+Rechnungswaehrung
                  ELSE
                     Txt := Txt+REPLICATE(' ',10);
                  IF (Lauf = PosUnterStrich) THEN
                     Txt := Txt + StrPas(PrintStyles[NoULine]);
                  IF (Zeilen = (PrinterCodes.PageLength - FootLineLength)) THEN BEGIN
                     DruckeLiefFuss;
                     INC(Seitenzahl);
                     DruckeLiefKopf(Lieferung,Druckdaten,
                                    KundeGefunden,Seitenzahl,TRUE);
                  END;
                  WritelnPrt(Txt);
               END;
               INC(Lauf);
            END;
         IF ZwischenSumme1 <> 0 THEN BEGIN
            IF (Zeilen >= (PrinterCodes.PageLength - FootLineLength-2)) THEN BEGIN
               DruckeLiefFuss;
               INC(Seitenzahl);
               DruckeLiefKopf(Lieferung,Druckdaten,
                              KundeGefunden,Seitenzahl,TRUE);

            END;
            WritelnPrt(StrPas(PrintStyles[Bold])+REPLICATE(' ',48)+'Summe ohne Pfand  '+R2S(ZwischenSumme1,7,2)+
                       ' '+Rechnungswaehrung+ StrPas(PrintStyles[NoBold]));
            WritelnPrt(REPLICATE('-',78));
         END;
         Lauf := 1;
         PfandPos := PosLauf;
         PosLauf := 0;
         PfandListe := NIL;
         WHILE (Lauf<= LetztePos) DO
             WITH Lieferung.Positionen[Lauf] DO BEGIN
                IF ((Preis <> 0) OR
                   ((ArtBez <>'') AND (COPY(ArtBez,1,5) <> 'Pfand'))) THEN
                   INC(PosLauf);
                IF ((Pfand <> 0) AND (Menge <> 0) AND
                   (COPY(ArtBez,1,5) <> 'Pfand')) THEN BEGIN
                   IF (PfandListe = NIL) THEN
                      NewPfand(PfandListe,Menge,Pfand,PosLauf)
                   ELSE BEGIN
                      PtrLauf := PfandListe;
                      WHILE (PtrLauf^.Pfand <> Pfand) AND
                            (PtrLauf^.NextPRec <> NIL) DO
                         PtrLauf := PtrLauf^.NextPRec;
                      IF (PtrLauf^.Pfand <> Pfand) THEN
                         NewPfand(PtrLauf^.NextPRec,Menge,Pfand,PosLauf)
                      ELSE BEGIN
                         PtrLauf^.Menge := PtrLauf^.Menge+Menge;
                         PtrLauf^.Ausgabe := PtrLauf^.Ausgabe+','+
                                             L2S(PosLauf,0);
                      END;
                   END;
                END ELSE
                   IF (COPY(ArtBez,1,5) = 'Pfand') AND (Menge <> 0) THEN BEGIN
                      INC(PfandPos);
                      Txt := ' '+L2S(PfandPos,2)+' '+L2S(Menge,5)+'  ';
                      ZwischenWert := Pfand*Menge;
                      ZwischenSumme2 := ZwischenSumme2 +ZwischenWert;
                      Txt:=Txt+Artbez+REPLICATE(' ',43-LENGTH(ArtBez));
                      Txt:=Txt + R2S(Pfand,6,2)+' '+Rechnungswaehrung+'  '+R2S(ZwischenWert,7,2)+
                           ' '+Rechnungswaehrung;
                      IF (Zeilen = (PrinterCodes.PageLength - FootLineLength)) THEN BEGIN
                         DruckeLiefFuss;
                         INC(Seitenzahl);
                         DruckeLiefKopf(Lieferung,Druckdaten,
                                        KundeGefunden,Seitenzahl,TRUE);
                      END;
                      WritelnPrt(Txt);
                   END;
                INC(Lauf);
             END;
         PtrLauf := PfandListe;
         IF (PtrLauf <> NIL) THEN BEGIN
            WHILE (PtrLauf <> NIL)  DO BEGIN
               ZwischenWert := PtrLauf^.Menge*PtrLauf^.Pfand;
               ZwischenSumme2 := ZwischenSumme2 + ZwischenWert;
               Txt := '    '+L2S(PtrLauf^.Menge,5)+'  ';
               Txt:=Txt+PtrLauf^.Ausgabe+')'+
                    REPLICATE(' ',42-LENGTH(PtrLauf^.Ausgabe));
               IF (PtrLauf^.NextPRec = NIL) THEN
                  Txt := Txt + StrPas(PrintStyles[ULine]);
               Txt:=Txt + R2S(PtrLauf^.Pfand,6,2)+' '+Rechnungswaehrung+'  '+R2S(ZwischenWert,7,2)+
                    ' '+Rechnungswaehrung;
               IF (PtrLauf^.NextPRec = NIL) THEN
                  Txt := Txt + StrPas(PrintStyles[NoULine]);
               IF (Zeilen = (PrinterCodes.PageLength - FootLineLength)) THEN BEGIN
                  DruckeLiefFuss;
                  INC(Seitenzahl);
                  DruckeLiefKopf(Lieferung,Druckdaten,
                                 KundeGefunden,Seitenzahl,TRUE);
               END;
               WritelnPrt(Txt);
               PfandListe := PtrLauf;
               PtrLauf := PtrLauf^.NextPRec;
               DISPOSE (PfandListe);
            END;
         END;
         IF ZwischenSumme2 <> 0 THEN BEGIN
            IF (Zeilen >= (PrinterCodes.PageLength - FootLineLength -2)) THEN BEGIN
               DruckeLiefFuss;
               INC(Seitenzahl);
               DruckeLiefKopf(Lieferung,Druckdaten,
                              KundeGefunden,Seitenzahl,FALSE);
            END;
            WritelnPrt(REPLICATE(' ',54)+'Pfand Summe '+StrPas(PrintStyles[NoBold])+
                       R2S(ZwischenSumme2,7,2)+' '+Rechnungswaehrung+StrPas(PrintStyles[NoBold]));
            WritelnPrt(REPLICATE('-',78));
         END;
         Gesamtsumme := Zwischensumme1+Zwischensumme2;
         IF (Zeilen >= (PrinterCodes.PageLength -
                              FootLineLength-2)) THEN BEGIN
            DruckeLiefFuss;
            INC(Seitenzahl);
            DruckeLiefKopf(Lieferung,Druckdaten,
                           KundeGefunden,Seitenzahl,FALSE);
         END;
         WritelnPrt(StrPas(PrintStyles[Bold])+REPLICATE(' ',48)+'Summe mit Pfand    '+
                    R2S(Gesamtsumme,6,2)+' '+Rechnungswaehrung+StrPas(PrintStyles[NoBold]));
         WritelnPrt(REPLICATE('=',78));
         IF (Zeilen > MaxPosLines) THEN BEGIN
            DruckeLiefFuss;
            INC(Seitenzahl);
            DruckeLiefKopf(Lieferung,Druckdaten,
                           KundeGefunden,Seitenzahl,FALSE);
         END ELSE BEGIN
            IF ((MaxPosLines-FootLineLength-
                PfandBlockLength-GewNoLength-Zeilen) < 0) THEN BEGIN
               PrintFootLine := FALSE;
               IF ((MaxPosLines-PfandBlockLength-
                  GewNoLength-Zeilen) < 0) THEN BEGIN
                  ShortGewNumbers:= TRUE;
                  IF ((MaxPosLines-PfandBlockLength-Zeilen) < 0) THEN BEGIN
                     PrintPfandBlock := FALSE;
                  END
               END;
            END;
         END;
         WritelnPrt('');
         LeerWert:=0;
         FOR L:=1 TO PfandTAnzahl DO BEGIN
           IF (LeergutArray[L] <> 0) THEN
             Leerwert:=Leerwert+PfandTypen[L]*LeergutArray[L];
           STR(LeerWert/100:10:2,LW);
         END;
         IF (PrgLaden) THEN
           WritelnPrt(StrPas(PrintStyles[Bold])+'Leergutr�ckgabe'+
                      REPLICATE(' ',32)+
                      'Leergutr�ckgabe'+LW+' '+Rechnungswaehrung+
                      StrPAs(PrintStyles[NoBold]))
         ELSE
           WritelnPrt(StrPas(PrintStyles[Bold])+'Leergutr�ckgabe, wenn nicht passend'+
                      REPLICATE(' ',13)+
                      'Leergutr�ckgabe'+' _________ '+Rechnungswaehrung+
                      StrPAs(PrintStyles[NoBold]));
         IF PrintPfandblock THEN BEGIN
            AnzProZeile := 7;
            IF PfandTAnzahl > AnzProZeile THEN BEGIN
               IF ((PfandTAnzahl MOD 2) = 1) THEN
                  AnzProZeile := (PfandTAnzahl DIV 2)+1
               ELSE
                  AnzProZeile := (PfandTAnzahl DIV 2);
               IF (AnzProZeile >7) THEN
                  AnzProZeile := 7;
            END;
            WritelnPrt('');
            Txt := '';
            FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
               IF ((Lauf MOD AnzProZeile) = 1) AND (Lauf <> 1) THEN BEGIN
                  WritelnPrt(Txt);
                  WritelnPrt('');
                  Txt := '';
               END;
               Txt := Txt+PrintLong((PfandTypen[Lauf] DIV 100),3)+','+LeadingZeros((PfandTypen[Lauf] MOD 100),2);
               IF (LeergutArray[Lauf] <> 0) THEN BEGIN
                 STR(LeergutArray[Lauf]:3,T);
                 Txt := Txt+StrPAs(PrintStyles[Bold])+T+StrPAs(PrintStyles[NoBold])+' '
               END ELSE
                 Txt := Txt+ '___ ';
            END;
            IF (Txt <> '') THEN BEGIN
               WritelnPrt(Txt);
               WritelnPrt('');
            END;
            Txt := '';
            AnzProZeile := 7;
            IF PfandNegAnzahl > AnzProZeile THEN BEGIN
               IF ((PfandNegAnzahl MOD 2) = 1) THEN
                  AnzProZeile := (PfandNegAnzahl DIV 2)+1
               ELSE
                  AnzProZeile := (PfandNegAnzahl DIV 2);
               IF (AnzProZeile >7) THEN
                  AnzProZeile := 7;
            END;
            IF NOT(PrgLaden) THEN
              FOR Lauf := 1 TO PfandNegAnzahl DO BEGIN
                 IF ((Lauf MOD AnzProZeile) = 1) AND (Lauf <> 1) THEN BEGIN
                    WritelnPrt(Txt);
                    WritelnPrt('');
                    Txt := '';
                 END;
                 Txt := Txt+' -'+PrintLong((NegPfand[Lauf] DIV 100),1)+','+LeadingZeros((NegPfand[Lauf] MOD 100),2);
                 IF (LeergutArray[Lauf+9] < 0) THEN BEGIN
                   STR(LeergutArray[Lauf+9]:3,T);
                   Txt := Txt+StrPAs(PrintStyles[Bold])+T+StrPAs(PrintStyles[NoBold])+' '
                 END ELSE
                   Txt := Txt+ '___ ';
              END ELSE
                Txt:='';
            IF (Txt <> '') THEN BEGIN
               WritelnPrt(Txt);
               WritelnPrt('');
            END;
         END;
         WritelnPrt(REPLICATE('=',78));
         WritelnPrt('|'+REPLICATE(' ',43)+'|'+REPLICATE(' ',32)+'|');
         MWST:=S2R(GMEnvObj^.GetEntry('MWST'));
         IF (PrgLaden) THEN BEGIN
           GesamtSumme:=GesamtSumme-LeerWert/100;
           MwstAnteil:= GesamtSumme/(1+(MWst/100))*(MwSt/100);
           WritelnPrt('|'+StrPas(PrintStyles[Bold])+
                      ' Gesamtsumme enth�lt '+R2S(MWST,2,0)+
                      '% MwSt.'+R2S(MwStAnteil,8,2)+' '+Rechnungswaehrung+
                      ' |  '+
                      'Gesamtsumme    '+R2S(GesamtSumme,10,2)+' '+Rechnungswaehrung+' '+
                      StrPAs(PrintStyles[NoBold])+'|');
         END ELSE
           WritelnPrt('|'+StrPas(PrintStyles[Bold])+
                      ' Gesamtsumme enth�lt '+R2S(MWST,2,0)+
                      '% MwSt.________ '+Rechnungswaehrung+
                      ' |  '+
                      'Gesamtsumme    '+'__________ '+Rechnungswaehrung+' '+
                      StrPAs(PrintStyles[NoBold])+'|');

         WritelnPrt(REPLICATE('=',78));
         IF (Lieferung.Von[3] <> '') THEN BEGIN
            {
            IF ShortGewNumbers THEN
               WritelnPrt('Gewinn-Nummern - von '+
                          Lieferung.Von[3]+ ' bis '+ Lieferung.Bis[3])
            ELSE BEGIN
               Txt := 'Gewinn-Nummern - ';
               GewAnfang := S2l(Lieferung.Von[3]);
               GewEnde := S2L(Lieferung.Bis[3]);
               IF (GewEnde < GewAnfang) THEN
                  GewEnde := GewEnde + 10000;
                  BlockCount := 1;
               FOR Lauf := GewAnfang TO GewEnde DO BEGIN
                  IF (BlockCount = 6) THEN BEGIN
                     WritelnPrt(Txt);
                     Txt :='                 ';
                     BlockCount := 1;
                  END;
                  IF (Lauf >= 10000) THEN
                     GewNo := Lauf-10000
                  ELSE
                     GewNo := Lauf;
                  INC(BlockCount);
                  Txt :=Txt+LeadingZeros(GewNo,5);
                  IF (BlockCount < 6) AND (Lauf < GewEnde) THEN
                     Txt := Txt+',';
               END;
               IF (BlockCount <> 1) THEN
                  WritelnPrt(Txt);
            END;
            }
         END;
         IF PrintFootLine THEN
            DruckeLiefFuss
         ELSE BEGIN
            Zeilen := 0;
            {$I-}
               WRITELnPrt(CHR(12));
            {$I+}
         END;
      END;


   FUNCTION Tourplanandruck;
      VAR AnzZeich : WORD;
          Lauf     : BYTE;
          WegVorh  : BOOLEAN;
          T:         TEXT;
          Stng:STRING;
      BEGIN
        WITH Kunde DO BEGIN
          Stng:=CHR(Nr+64)+')  Kundennummer: '+LeadingZeros(LfdNr,5)+
                REPLICATE(' ',32)+'Lieferschein: '+Lieferung.LiefNr;

          TourPlanandruck:=PrintAdresseTour(Stng,Lieferung,Kunde,KundeGefunden);
        END;
      END;

  FUNCTION DruckeTourplanUeberSchrift(Lieferung:TLiefDaten):BOOLEAN;
     VAR TAbs,ATour : BYTE;
     BEGIN
        SplitTnr(Lieferung.TourNr,TAbs,ATour);
        {$I-}
           Writeln(LST,'       Datum : '+Lieferung.LieferTag+'  Tour : '+
                       MakeTString(TAbs,ATour));
           Writeln(LST,'');
           Writeln(LST,'       Fahrer :'+Lieferung.Fahrer);
           Writeln(LST,'');
           Writeln(LST,REPLICATE('-',78));
        {$I+}
        DruckeTourPlanUeberschrift:= (IORESULT = 0)
     END;

   FUNCTION DruckeAktuelleLieferung;

      VAR LSave : TLiefDaten;
          OldDay : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         LSave:=Lieferung;
         PreparePosSave(Lieferung);
         OldDay := IsOldDay(Lieferung.Liefertag);
         Mask2Lie(Lieferung,OldDay);
         WITH Lieferung DO
            LDrucken(Lieferung);
         Lieferung:=LSave;
         Abbruch := FALSE;
         Check := FALSE;
         DruckeAktuelleLieferung := FALSE;
      END;

   PROCEDURE OPsDrucken;
     TYPE TOpListePtr = ^TOPListe;
          TOpListe    = RECORD
                            Daten : TOposten;
                            Kunde : STRING[5];
                            Next : TOPListePtr;
                        END;
      VAR OPListe,Lptr : TOPListePtr;
      VAR Fehler : BOOLEAN;
      VAR KGelesen : BOOLEAN;
          OPGefunden : BOOLEAN;
          EndSummeGedruckt : BOOLEAN;
          GUThabenGefunden : BOOLEAN;
          Lauf     : BYTE;
          DatNam   : STRING[80];
          ZeilenNr : BYTE;
          SeitenNr : BYTE;
          EndSumme : REAL;
      PROCEDURE PrintOPFuss(VAR MitEndSumme:BOOLEAN);
         VAR Lauf : BYTE;
             EndLauf : BYTE;

         BEGIN
            IF (ZeilenNr  <> 0) OR (MitEndSumme) THEN BEGIN
               IF (ZeilenNr < 55) THEN BEGIN
                  IF MitEndSumme AND (ZeilenNr < 53) THEN BEGIN
                     {$I-}
                        WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                              REPLICATE('-',11)+'+');
                     {$I+}
                     {$I-}
                      WRITELN(LST,'   |'+REPLICATE(' ',7)+'|'+REPLICATE(' ',7)+'|'+
                                  PrintString('G E S A M T',42)+'| '+
                              PrintReal(EndSumme,7,2,TRUE)+' |');
                     {$I+}
                  END ELSE BEGIN
                     IF MitEndSumme THEN
                        MitEndSumme := FALSE;
                  END;
                  ZeilenNr := 55;
               END ELSE
                  MitEndSumme := FALSE;
               {$I-}
                  WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                          REPLICATE('-',11)+'+');
               {$I+}
            END;
         END;

      PROCEDURE PrintOpKopf;
         BEGIN
            {$I-}
               WRITELN(LST,'   Offene Posten Liste '+REPLICATE(' ',10)+
                           'Stand: '+ActDate+REPLICATE(' ',17)+ 'Seite '+B2S(SeitenNr,0));
               WRITELN(LST);
               WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                       REPLICATE('-',11)+'+');
               WRITELN(LST,'   | Kunde | LS-Nr | '+PrintString('Bemerkung',40)+' | Betrag    |');
               WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                       REPLICATE('-',11)+'+');
            {$I+}
         END;
      PROCEDURE PrintGHKopf;
         BEGIN
            {$I-}
               WRITELN(LST,'   Guthaben Liste'+REPLICATE(' ',17)+
                           'Stand: '+ActDate+REPLICATE(' ',17)+ 'Seite '+B2S(SeitenNr,0));
               WRITELN(LST);
               WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                       REPLICATE('-',11)+'+');
               WRITELN(LST,'   | Kunde | LS-Nr | '+PrintString('Bemerkung',40)+' | Betrag    |');
               WRITELN(LST,'   +'+REPLICATE('-',7)+'+'+REPLICATE('-',7)+'+'+REPLICATE('-',42)+'+'+
                       REPLICATE('-',11)+'+');
            {$I+}
         END;

      PROCEDURE PrintOpZeile(Kunde:STRING;Daten:TOPosten;Guthaben: BOOLEAN);
         VAR Ausgabe : REAL;
             KeineEndSumme : BOOLEAN;

         BEGIN
            WITH Daten DO BEGIN
               IF (ZeilenNr = 55) THEN BEGIN
                  KeineEndSumme := FALSE;
                  PrintOPFuss(KeineEndSumme);
                  {$I-}
                  WRITE(LST,CHR(12));
                  {$I+}
                  INC(SeitenNr);
                  ZeilenNr:= 0;
               END;
               INC(ZeilenNr);
               IF (ZeilenNr = 1) THEN BEGIN
                  IF GutHaben THEN
                     PrintGHKopf
                  ELSE
                     PrintOpKopf;
               END;
               {$I-}
               WRITELN(LST,'   | '+Kunde+' | '+
               PrintString(LiefNr,5)+' | '+PrintString(Bemerk,40)+
               ' | '+PrintReal(Betrag,7,2,TRUE)+' |');
               {$I+}
               EndSumme := EndSumme + Betrag;
            END;
         END;

         PROCEDURE ClearOPListe;
            VAR DPtr,
                LPtr : TOpListePtr;
            BEGIN
               LPtr := OpListe;
               WHILE LPtr <> NIL DO BEGIN
                  DPtr := LPtr;
                  LPtr := Lptr^.Next;
                  DISPOSE(DPtr);
               END;
               OpListe := NIL;
            END;

         FUNCTION AddOP(Daten : TOposten):BOOLEAN;
            VAR NPTr : TOPListePtr;
                OLPtr,
                LPtr : TOpListePtr;
                NDatum,
                ADatum :TDatum;
                Ergebnis : BOOLEAN;
            FUNCTION FindDatum(Txt:STRING):STRING;
               VAR HPointPos,
                   PointPos: BYTE;
                   WString : STRING;
                   Ergebnis: TDatum;
                   Ende : BOOLEAN;
                   StartTxt,EndTxt : BYTE;
                   JWert,
                   Lauf : BYTE;
                   Tag,Monat,Jahr,
                   WorkTxt  : STRING[10];
               BEGIN
                  Ergebnis := '';
                  PointPos:= POS('.',Txt);
                  IF (PointPos <> 0) THEN BEGIN
                     WString := Txt;
                     Ende := FALSE;
                     WHILE (NOT Ende) DO BEGIN
                        IF (PointPos > 1) THEN BEGIN
                           StartTxt := PointPos-2;
                           IF (StartTxt < 1) THEN
                              StartTxt := 1;
                           EndTxt := PointPos +5;
                           IF (EndTxt > LENGTH(WString)) THEN
                              EndTxt := LENGTH(WString);
                           WorkTxt := COPY(WString,StartTxt,EndTxt-StartTxt+1);
                           EndTxt := LENGTH(WorkTxt)+1;
                           FOR Lauf := LENGTH(WorkTxt) DOWNTO 1 DO BEGIN
                              IF (NOT(WorkTxt[Lauf] IN[' ','0'..'9','.'])) THEN
                                 EndTxt := Lauf;
                           END;
                           IF (EndTxt > 1) THEN
                              WorkTxt := COPY(WorkTxt,1,EndTxt-1)
                           ELSE
                              WorkTxt := '';
                           WorkTxt := RemoveAllSpaces(WorkTxt);
                           IF (LENGTH(WorkTxt) > 3) THEN BEGIN
                              HPointPos := POS('.',WorkTxt);
                              IF (HPointPos <= 3) THEN BEGIN
                                 Tag := COPY(WorkTxt,1,HPointPos-1);
                                 WorkTxt := COPY(WorkTxt,HPointPos+1,LENGTH(WorkTxt)-HPointPos);
                                 HPointPos := POS('.',Worktxt);
                                 Jahr := '';
                                 IF (HPointPos = 0) THEN
                                    Monat := WorkTxt
                                 ELSE BEGIN
                                    Monat :=COPY(WorkTxt,1,HPointPos-1);
                                    Jahr := COPY(WorkTxt,HPointPos+1,LENGTH(WorkTxt)-HPointPos);
                                 END;
                                 IF (LENGTH(Tag) <=2) AND (LENGTH(Tag) <> 0) AND
                                    (LENGTH(Monat) <=2) AND (LENGTH(Monat) <> 0) AND
                                    (LENGTH(Jahr) <=2) THEN BEGIN
                                    Tag := LeadingZeros(S2L(Tag),2);
                                    Monat := LeadingZeros(S2L(Monat),2);
                                    IF (Jahr = '') THEN BEGIN
                                       Jahr := COPY(ActDate,7,2);
                                       IF ValidDate(Tag+'.'+Monat+'.'+Jahr) THEN BEGIN
                                          IF Later(Tag+'.'+Monat+'.'+Jahr,ActDate) THEN BEGIN
                                             JWert := S2L(Jahr);
                                             IF (JWert = 0) THEN
                                                JWert := 99
                                             ELSE
                                                DEC(JWert);
                                             Jahr := LeadingZeros(JWert,2);
                                          END;
                                       END;
                                    END;
                                    IF ValidDate(Tag+'.'+Monat+'.'+Jahr) THEN BEGIN
                                       Ende := TRUE;
                                       Ergebnis:=Tag+'.'+Monat+'.'+Jahr;
                                    END;
                                 END;
                              END
                           END;
                        END;
                        IF NOT Ende THEN BEGIN
                           IF (PointPos = 0) THEN
                              Ende := TRUE
                           ELSE BEGIN
                              WString := COPY(WString,PointPos,LENGTH(WString)-PointPos+1);
                              PointPos := POS('.',WString);
                              Ende := (PointPos = 0);
                           END;
                        END;
                     END;
                  END;
                  FindDatum := Ergebnis;
               END;

            BEGIN
               Ergebnis := FALSE;
               IF (MaxAvail > SIZEOF(TOPListe)) THEN BEGIN
                  NEW(NPtr);
                  NPTr^.Next := NIL;
                  NPtr^.Daten := Daten;
                  NPtr^.Kunde := LeadingZeros(Kunde.LfdNr,5);
                  IF (OPListe = NIL) THEN BEGIN
                     OPliste := NPTr;
                     Ergebnis := TRUE;
                  END ELSE BEGIN
                     LPtr := OPListe;
                     NDatum := FindDatum(NPtr^.Daten.Bemerk);
                     ADatum := FindDatum(LPtr^.Daten.Bemerk);
                     IF ((Ndatum <> '') AND (ADatum = '')) OR
                        ((Ndatum <> '') AND (ADatum <> '') AND
                         Later(ADatum,NDatum)) THEN BEGIN
                        NPtr^.Next:= OPListe;
                        OpListe:= NPtr;
                        Ergebnis := TRUE;
                     END ELSE BEGIN
                        REPEAT
                           OLPtr := LPtr;
                           LPtr:= LPtr^.Next;
                           IF (Lptr = NIL) THEN BEGIN
                              OLPtr^.Next:= NPtr;
                              NPtr^.Next := LPtr;
                              Ergebnis := TRUE;
                           END ELSE BEGIN
                              ADatum := FindDatum(LPtr^.Daten.Bemerk);
                              IF ((Ndatum <> '') AND (ADatum = '')) OR
                                 ((Ndatum <> '') AND (ADatum <> '') AND
                                   Later(ADatum,NDatum)) THEN BEGIN
                                 OLPtr^.Next:= NPtr;
                                 NPtr^.Next := LPtr;
                                 Ergebnis := TRUE;
                              END;
                           END;
                        UNTIL Ergebnis;
                     END;
                  END;
               END;
               AddOp := Ergebnis;
            END;
      BEGIN
         OpListe := NIL;
         KundDB.BeginTransaction;
         KGelesen := KundDB.StartIntervall(1,MakeLfdNr(0,0),MakeLfdNr(99999,0),Kunde);
         KundDB.EndTransaction;
         IF KGelesen THEN BEGIN
            OPGefunden := FALSE;
            DatNam:=MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'))+'PRTFile.TXT';
            PrtToFile(DatNam);
            ZeilenNr := 0;
            SeitenNr := 1;
            KundDB.BeginTransaction;
            EndSumme := 0;
            Fehler := FALSE;
            REPEAT
               IF (Kunde.LieferANr = 0) THEN BEGIN
                  StatusBox.Act('Bearbeite gerade ','Kunde '+ LeadingZeros(Kunde.LfdNr,5));
                  FOR Lauf := 1 TO 10 DO BEGIN
                     WITH Kunde.Oposten[Lauf] DO BEGIN
                        IF (LiefNr <> '') OR (Betrag <> 0)
                           OR (Bemerk <> '')  AND ((Art ='G') OR (Art ='O'))THEN BEGIN
                           OPGefunden := TRUE;
                           IF (Art = 'G') THEN BEGIN
                              PrintOpZeile(LeadingZeros(Kunde.LfdNr,5),Kunde.Oposten[Lauf],TRUE);
                              GutHabengefunden := TRUE;
                           END ELSE BEGIN
                              Fehler := NOT AddOP(Kunde.Oposten[Lauf]) OR Fehler;
                           END;
                        END;
                     END;
                  END;
               END;
            UNTIL NOT KundDB.GetIntNext(Kunde);
            KundDB.EndTransaction;
            IF Fehler THEN
               FaultBox.Act(2,'Zuwenig Speicherplatz, '+
                              'offene Posten Liste evtl. unvollst�ndig.');

            IF GutHabenGefunden THEN BEGIN
               EndSummeGedruckt := TRUE;
               PrintOPFuss(EndSummeGedruckt);
               IF NOT EndSummeGedruckt THEN BEGIN
                  {$I-}
                    WRITE(LST,CHR(12));
                  {$I+}
                  PrintGHKopf;
                  EndSummeGedruckt := TRUE;
                  PrintOPFuss(EndSummeGedruckt);
               END;
            END;
            IF (OpListe <> NIL) THEN BEGIN
               IF GutHabengefunden THEN BEGIN
                  {$I-}
                     WRITE(LST,CHR(12));
                  {$I+}
               END;
               EndSumme := 0;
               ZeilenNr := 0;
               SeitenNr := 1;
               LPtr := OpListe;
               WHILE (LPTr <> NIL) DO BEGIN
                  PrintOpZeile(LPtr^.Kunde,LPtr^.Daten,FALSE);
                  LPtr := LPtr^.Next;
               END;
               EndSummeGedruckt := TRUE;
               PrintOPFuss(EndSummeGedruckt);
               ClearOPListe;
               IF NOT EndSummeGedruckt THEN BEGIN
                  {$I-}
                    WRITE(LST,CHR(12));
                  {$I+}
                  PrintOPKopf;
                  EndSummeGedruckt := TRUE;
                  PrintOPFuss(EndSummeGedruckt);
               END;
            END;
            StatusBox.Hide;
            PrinterToPrt;
            IF OpGefunden THEN BEGIN
               StatusBox.Act('�bergebe gerade Datei','an Drucker');
               WriteTextDatei(DatNam);
               StatusBox.Hide;
            END;
         END;
      END;
BEGIN

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
