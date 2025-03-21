{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 16:48:38 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
 UNIT GMLIEFED;

INTERFACE

   USES ASTDATA;

   VAR  LFenster1,
        LFenster2:  TASFensterPtr;

   PROCEDURE InitLieferInput;

IMPLEMENTATION

   USES GMDATEI,
        GMUEBER,
        GMSCROLL,
        GMT100DR,
        GMKUNED,
        GMFAHRER,
        KUNDATEN,
        LIEDATEN,
        GMCRT,
        PRINTER,
        GMCALC,
        GMSETUP,
        GMABRECH,
        ARTDaten,
        CASHNEU,
        PLACEWIN,
        ZUSAETZE,
        CASHDATA,
        GMBASE;


   VAR   NeuerLieferschein : BOOLEAN;
   VAR   ZeitIstDefault : BOOLEAN;
         STDBestIstDefault : BOOLEAN;
         ZeitUeberprueft : BOOLEAN;
         ZeitUVon,
         ZeitUBis    : TZeit;
         BerUeberprueft : BOOLEAN;
         BUBer,
         BUTag       : BYTE;
         BUTAbs       : BYTE;
         HRequest    : TRequest;
         STripleQuest: TTripleQuest;
         LBestaetigen: TEingabeFensterPtr;
         TagAnzeige : STRING[26];
         VollSum,
         LeerSum,
         GesaSum: REAL;
         SearchFenster : TSearchFensterPtr;
         RNummer,
         BNummer : LONGINT;

   PROCEDURE GetLiefStatus;
      BEGIN
         CASE Lieferung.Bemerkungen[1][1] OF
           'J': LiefStatus:='+';
           'N': LiefStatus:='-'
           ELSE LiefStatus:='0'
         END;
      END;

   PROCEDURE LeergutToBemerk;
      VAR Bem:  STRING[160];
          Lauf: BYTE;
          T:    STRING[10];
      BEGIN
        Bem:='JNNTAI=';
        FOR Lauf:=1 TO PfandTAnzahl DO BEGIN
          IF (LeergutArray[Lauf] <> 0) THEN BEGIN
            STR(PfandTypen[Lauf],T);
            Bem:=Bem+T+':';
            STR(LeergutArray[Lauf],T);
            Bem:=Bem+T+';'
          END
        END;
        Lieferung.Bemerkungen[1]:=COPY(Bem,1,80);
        IF (LENGTH(Bem) > 80) THEN
          Lieferung.Bemerkungen[2]:=COPY(Bem,81,80);
      END;

   FUNCTION OPRueckUebertragen(VAR KHelp:TkundenDaten;VAR Lieferung:TLiefDaten;VAR LPos,OPEintrag:BYTE;
               VAR OPNichtUebertragen:BOOLEAN):BOOLEAN;
      VAR Lauf :BYTE;
          OPZEile1,
          OPZeile2 : STRING[80];
          LFOk : BOOLEAN;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         FOR Lauf := 10 DOWNTO 1 DO
            WITH KHelp.Oposten[Lauf] DO BEGIN
               IF ((Art =' ') OR (Art = #0)) AND (LiefNr = '') AND (Betrag = 0) AND
                  (Bemerk = '') THEN
                  OPEintrag := Lauf;
            END;
         IF (OPEintrag < 11) THEN BEGIN
            WITH Lieferung.Positionen[LPos] DO BEGIN
               KHelp.Oposten[OPEintrag].LiefNr := COPY(ArtBez,1,5);
               KHelp.Oposten[OPEintrag].Betrag := ABS(Preis);
               KHelp.Oposten[OPEintrag].Bemerk := COPY(ArtBez,9,LENGTH(ArtBez)-8);
               IF (POS('Guthaben vom',KHelp.Oposten[OPEintrag].Bemerk) > 0) THEN
                  KHelp.Oposten[OPEintrag].Art := 'G';
               IF (POS('offen vom ',KHelp.Oposten[OPEintrag].Bemerk) > 0) THEN
                  KHelp.Oposten[OPEintrag].Art := 'O';
               OPZeile1 :=ActDate+';'+ActTime+';'+'OP-R�CK�BERTRAGEN'+';'+
                  LeadingZeros(KHelp.LfdNr,5)+';'+Lieferung.LiefNr;
               WITH KHelp.Oposten[OpEintrag] DO
                  OPZeile2 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
               LFOk := OPLogFile^.WritelnLog(OpZeile1);
               LFOk := OPLogFile^.WritelnLog(OPZeile2) AND LFOk;
               IF NOT LFOK THEN BEGIN
                  FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                 'Bitte folgende Infos eintragen: '+OpZeile1+
                                 ' '+OpZeile2);
               END;
               Ergebnis := TRUE;
            END;
         END ELSE BEGIN
            OPNichtUebertragen:= TRUE;
         END;
         OPRueckUebertragen := Ergebnis;
      END;

   FUNCTION BerechnePreis:BOOLEAN;
      VAR Lauf : BYTE;
          VollSummeAlt,
          LeerSummeAlt,
          GesamtSummeAlt : REAL;
      BEGIN
        VollSummeAlt := VollSum;
        LeerSummeAlt := LeerSum;
        GesamtSummeAlt := GesaSum;
        VollSum:=0;
        LeerSum:=0;
        GesaSum:=0;
        FOR Lauf:=1 To 19 DO
          WITH Lieferung.Positionen[Lauf] DO BEGIN
             IF (Pfand <> 0) THEN
               PiP[Lauf]:=Preis+Pfand
             ELSE
               Pip[Lauf]:=0;
             IF (Menge > 0) AND (ArtNr <> '') AND LiefPosEd[Lauf] THEN BEGIN
               VollSum:=VollSum+Menge*Preis;
               LeerSum:=LeerSum+Menge*Pfand;
            END;
          END;
        GesaSum:=VollSum+LeerSum;
        BerechnePreis := (VollSum <> VollSummeAlt) OR (LeerSum <> LeerSummeALT) OR
                          (GesaSum <> GesamtSummeAlt);
     END;

   FUNCTION UpdateTourNr:BOOLEAN;
     VAR Ergebnis : BOOLEAN;
         TLauf : BYTE;
     BEGIN
        TLauf := CalculateTagesAbschnitt(Lieferung.Knr,Lieferung.Von[1]);
        Ergebnis := TRUE;
        IF TLauf <> 0 THEN BEGIN
           IF (TLauf = (TourAbs +2))THEN
              Lieferung.TourNr := MakeIFieldVal(TLauf,0)
           ELSE
              Lieferung.TourNr := MakeIFieldVal(TLauf,1);

        END ELSE
           Ergebnis := FALSE;
        UpDateTourNr := Ergebnis;
     END;



   PROCEDURE DispTagString(Datum:TDatum);
      VAR Txt : STRING[26];
          OldTagString :STRING[26];
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
                     Txt :='�bermorgen - '+Txt
                  ELSE
                     Txt :='Demn�chst -'+Txt;
               END ELSE
                  Txt := 'Alt - '+Txt;
            END;
         END ELSE
            TXT :='';
         IF (Txt <> '') THEN
            Txt:=CenterString(txt,26);
         IF (OldTagString <> Txt) THEN BEGIN
            IF NOT LFenster1^.Geschlossen THEN
               LFenster1^.ClearConst('35');
            TagAnzeige := Txt;
            IF NOT LFenster1^.Geschlossen THEN
               LFenster1^.Refresh('35');
         END;
      END;

   PROCEDURE ZeigeLieferscheinZuLSNummer;
      VAR SK :BOOLEAN;
      BEGIN
         Lie2Mask(Lieferung);
         BerechnePreis;
         GetLiefStatus;

         LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
         LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
         AktFahrer := Lieferung.Fahrer;
         DispTagString(Lieferung.Liefertag);
         LFenster1^.Refresh('');
         Sk := FALSE;
         IF (Lieferung.Knr <> '') THEN BEGIN
            KundDB.BeginTransaction;
            Sk:=KundDb.GetRec(1,Lieferung.Kundennummer,Kunde);
            IF NOT SK THEN
              SK:=KundDb.GetRec(1,MakeLfdNr(S2L(Lieferung.KNr),0),Kunde);
            KundDB.EndTransaction;
         END;

         IF SK THEN
            ShowKundenInfo
         ELSE
            ClearKundenInfo;
      END;


   FUNCTION LFenster1AInput:BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
          VonZ : TZeit;
          BisZ : TZeit;
          TLauf : BYTE;
      VAR FocusId : STRING;
      BEGIN
         FocusId := LFenster1^.GetFocusID;
         IF (FocusId = '32') OR (FocusId = '31') THEN BEGIN
            Ergebnis := FALSE;
            IF NeuerLieferschein AND (Lieferung.Liefertag = ActDate) THEN BEGIN
               IF (FocusID = '31') AND (Lieferung.Von[1] <> '')
                  AND NotLaterTime(Lieferung.Von[1],ActTime) THEN BEGIN
                  Lieferung.Von[1]:= NextHour(COPY(ActTime,1,2)+':00');
                  FaultBox.Act(2,'F�r den heutigen Tag kann nur noch nach der '+
                                 'aktuellen Uhrzeit angenommen werden');
                  LFenster1^.Refresh('31');
               END;
               IF (FocusID = '32') AND (Lieferung.Bis[1] <> '')
                  AND NotLaterTime(Lieferung.Bis[1],ActTime) THEN BEGIN
                  Lieferung.Bis[1]:=
                     NextHour(NextHour(NextHour(COPY(ActTime,1,2)+':00')));
                  FaultBox.Act(2,'F�r den heutigen Tag kann nur noch nach der '+
                                 'aktuellen Uhrzeit angenommen werden');
                  LFenster1^.Refresh('32');
               END;
            END;
            IF Kunde.LiefSZeit THEN BEGIN
               VonZ:= Lieferung.Von[1];
               BisZ := Lieferung.Bis[1];
               Ergebnis := CheckLZeitRaum(Lieferung.Liefertag,VonZ,BisZ) ;
               IF (BisZ <> Lieferung.Bis[1]) THEN BEGIN
                  Lieferung.Bis[1]:= BisZ;
                  LFenster1^.Refresh('32');
               END;
               IF (VonZ <> Lieferung.Von[1]) THEN BEGIN
                  Lieferung.Von[1]:= VonZ;
                  LFenster1^.Refresh('31');
               END;
               IF NOT Ergebnis THEN BEGIN
                  FaultBox.Act(2,'Falscher Zeitraum, Kunde ist Innenstadtkunde !!!');
               END;
               Ergebnis := TRUE;
            END;
            IF NeuerLieferschein THEN BEGIN
               IF UpdateTourNr THEN
                  LFenster1^.Refresh('7');
            END;
         END;
         LFenster1AInput := Ergebnis;
      END;


   PROCEDURE LKurzDrucken(Lieferung:TLiefdaten);
      VAR   Druckerstatus : LONGINT;
            DName    : STRING;
            LogDatei : TEXT;
            HCounter : WORD;
            Lauf : WORD;
            LetztePos :WORD;
            NormalDruck,
            D: BOOLEAN;
      BEGIN
         DName := ActDate;
         DELETE(DName,6,1);
         DELETE(DName,3,1);
         DName := MakeFilePath(GMENVObj^.GetEntry('LOGDIR'))+Dname+'.LOG';
         ASSIGN(LogDatei,Dname);
         IF SeekFile(DName) THEN
            {$I-}
            APPEND(LogDatei)
            {$I+}
         ELSE
            {$I-}
            REWRITE(LogDatei);
            {$I+}
         WITH Lieferung DO BEGIN
            LetztePos := 30;
            FOR Lauf := 30 DOWNTO 1 DO
               WITH Positionen[Lauf] DO
                  IF ((Menge = 0) AND (ArtBez = '') AND (Preis = 0) AND
                      (Pfand = 0)) THEN
                     LetztePos := Lauf -1;
            {$I-}
            WRITE(LogDAtei,LiefNr,'-',Kundennummer,'/',COPY(LieferTag,1,2),'/',COPY(Von[1],1,2)
                  ,'/',COPY(Bis[1],1,2),'-');
            {$I+}
            Lauf := 1;
            HCounter := 1;
            WHILE Lauf<= LetztePos DO BEGIN
               WITH Positionen[Lauf] DO BEGIN
                  NormalDruck := TRUE;
                  IF ArtDB.SeekRec(1,MakeArtNr(S2L(ArtNr))) THEN BEGIN
                     IF (ArtBez <> ArtDB.Rec.Artikel.ArtBez) OR
                        (Preis <> ArtDB.Rec.Artikel.Preiska) THEN
                        NormalDruck := FALSE;
                  END ELSE
                     NormalDruck := FALSE;
                  IF NormalDruck THEN BEGIN
                     IF HCounter = 9 THEN BEGIN
                        HCounter := 0;
                        {$I-}
                        WRITELN(LogDatei);
                        WRITE(LogDatei,REPLICATE(' ',21));
                        {$I+}
                     END;
                     INC(HCounter);
                     {$I-}
                     WRITE(LogDatei,LeadingZeros(Menge,2),
                     COPY(MakeArtNr(S2l(ArtNr)),3,3),'/');
                     {$I+}
                  END ELSE BEGIN
                     HCounter := 9;
                     {$I-}
                     WRITELN(Logdatei);
                     WRITE(Logdatei,'     ');
                     {$I+}
                     IF Menge <> 0 THEN
                        {$I-}
                        WRITE(Logdatei,Menge:5)
                        {I+}
                     ELSE
                        {$I-}
                        WRITE(Logdatei,REPLICATE(' ',5));
                        {$I+}
                     {$I-}
                     WRITE(logDatei,'  ',Artbez,REPLICATE(' ',45-LENGTH(ArtBez)));
                     {$I+}
                     IF Preis <> 0 THEN
                        {$I-}
                        WRITE(LogDatei,Preis:6:2,' ',Rechnungswaehrung,' ');
                        {$I+}
                  END;
               END;
               INC(Lauf);
            END;
            {$I-}
            WRITELN(LogDatei);
            WRITELN(LogDatei);
            {$I+}
         END;
         {$I-}
         CLOSE(LogDatei);
         {$I+}
      END;

   FUNCTION LFenster1Action:BOOLEAN;FAR;
      VAR FocusFK : BYTE;
          FocusId :STRING;
          NewFlag : BOOLEAN;
          Lauf   : BYTE;
          FocusS,FocusR  : STRING;
      BEGIN
         FocusID := LFenster1^.GetFocusID;
         SplitString(FocusID,'_',FocusS,FocusR);
         FocusFK := S2B(FocusS);
         NewFlag := IsStdZeit;
         IF (NewFlag <> ZeitIstDefault) THEN BEGIN
            ZeitIstDefault := NewFlag;
            IF ZeitIstDefault THEN BEGIN
               LFenster1^.SetDefault('31');
               LFenster1^.SetDefault('32');
            END ELSE BEGIN
               LFenster1^.ClearDefault('31');
               LFenster1^.ClearDefault('32');
            END;
            LFenster1^.Refresh('31');
            LFenster1^.Refresh('32');
         END;
         NewFlag:= IsStdBest;
         IF NewFlag <> STdBestIstDefault THEN BEGIN
            StdBestIstDefault := NewFlag;
            IF NewFlag THEN BEGIN
               FOR Lauf := 1 TO AnzSTdPos DO BEGIN
                   LFenster1^.SetDefault('0'+B2S(Lauf,0)+'_');
               END;
            END ELSE BEGIN
               FOR Lauf := 1 TO AnzSTdPos DO BEGIN
                  LFenster1^.ClearDefault('0'+B2S(Lauf,0)+'_');
               END;
            END;
            FOR Lauf := 1 TO AnzStdPos DO
               LFenster1^.Refresh('0'+B2S(Lauf,0));
         END;
         CASE FocusFK OF
             88: BEGIN END;
             30..32 : BEGIN
                         IF NOT LFenster1^.Geschlossen THEN
                            ShowKundenUebersicht;
                      END;
             1..6   : BEGIN
                        IF NOT LFenster1^.Geschlossen THEN
                           HideKundenUebersicht;
                           IF BerechnePreis THEN BEGIN
                              LFenster1^.Refresh('10');
                              LFenster1^.Refresh('21');
                           END;
                        END;
            ELSE    IF NOT LFenster1^.Geschlossen THEN
                       HideKundenUebersicht;
         END;
         DispTagString(Lieferung.Liefertag);
         LFenster1^.Refresh('35');
         LFenster1Action := FALSE;
      END;



   FUNCTION LSSpeichern:BOOLEAN;
      VAR Gespeichert,
          Ergebnis : BOOLEAN;
      VAR LSavV,
          LSavB:      STRING[5];
          OldDay   : BOOLEAN;
          TLauf : BYTE;
          TFound : BOOLEAN;
          LTAbs,LAtour : BYTE;
          LTourHelp,
          LTourHelp1: WORD;
          Sign:CHAR;
          LSave : TLiefDaten;
          VTime : BOOLEAN;
          LEdSave : ARRAY[1..30] OF BOOLEAN;
          PipSave : ARRAY[1..30] OF REAL;

          lVon,
          LBis   :TZeit;
          FText : STRING;
      VAR Lauf : BYTE;


      BEGIN
         HideKundenUebersicht;
         Gespeichert:=TRUE;
         OldDay := IsOldDay(Lieferung.Liefertag);
         SplitIFieldTNr(Lieferung.TourNr,LTAbs,LATour);
         IF (NOT OldDay) OR  ((LATour <= 5) AND (LATour >= 1))
            OR (LTabs = TourAbs+2) THEN BEGIN
            IF (COPY(Lieferung.Liefertag,7,2) <> COPY(ActDate,7,2)) THEN
              Gespeichert:=Request.Act('Falsches Jahr ! Speichern?');
            IF Kunde.LiefSZeit AND Gespeichert THEN BEGIN
               LVon := Lieferung.Von[1];
               LBis := Lieferung.Bis[1];
               VTime := CheckLZeitRaum(Lieferung.Liefertag,LVon,LBis);
               Gespeichert := VTime AND (LVon = Lieferung.Von[1]) AND
                                        (LBis = Lieferung.Bis[1]);
               IF NOT Gespeichert THEN BEGIN
                  FaultBox.Act(3,'Kunde ist Innenstadtkunde. '+
                                 'Bitte den Lieferzeitraum �berpr�fen.');
                  Lieferung.Von[1]:= LVon;
                  Lieferung.Bis[1]:= LBis;
                  LFenster1^.Refresh('');
               END;
            END;
            IF Gespeichert THEN BEGIN
               LSave := Lieferung;
               FOR Lauf := 1 TO 30 DO BEGIN
                  LEdSave[Lauf] := LiefPosEd[Lauf];
                  PipSave[Lauf] := Pip[Lauf];
               END;
               Mask2Lie(Lieferung,OldDay);{ Code f�r Preisliste   }
               PreparePosSave(Lieferung);
               Gespeichert := FALSE;
               FOR Lauf := 1 TO 19 DO
                  WITH Lieferung.Positionen[Lauf] DO
                     Gespeichert := Gespeichert OR
                     ((Menge <> 0) AND (ArtNr <> '01902') AND (ArtNr <> '01903'));
               IF Gespeichert THEN BEGIN
                  Lieferung.LiefNr:=LeadingZeros(S2L(Lieferung.LiefNr),5);
                  BerechneSoll(Lieferung);        { Sollumsatz            }
                  Lieferung.Fahrer:=AktFahrer;    { Fahrer zuordnen       }
                  IF (NOT NeuerLieferschein) THEN BEGIN
                     { TourNr ggf. korrigieren }
                     LTourHelp := Lieferung.TourNr;
                     IF NOT ValidTNr(LTourHelp,OldDay) THEN BEGIN
                        SplitTNr(Lieferung.TourNr,LTabs,LATour);
                        LTourHelp1:= LTourHelp;
                        LTourHelp := MakeTNr(LTabs,LAtour,OldDay);
                        IF NOT ValidTnr(LTourHelp,OldDay) THEN
                           LTourHelp := 1;
                        FaultBox.Act(0,'Falsche Tournr '+LeadingZeros(LTourHelp1,5)+
                                      ' korregiert auf '+LeadingZeros(LTourHelp,5));
                     END;
                     Lieferung.TourNr := LTourHelp;
                     SplitTNr(Lieferung.TourNr,LTabs,LATour);
                     TLauf := CalculateTagesAbschnitt(Lieferung.Knr,Lieferung.Von[1]);
                     IF (TLauf <> LTabs) THEN BEGIN
                        Lieferung := LSave;
                        Lieferung.Fahrer:=AktFahrer;
                        Lieferung.TourNr:=MakeTabsIndex(MakeIFieldVal(TLauf,1),0,OldDay);
                        Lieferung.TourNr:=Tnr2IField(Lieferung.TourNr);
                        Gespeichert := FALSE;
                        FText := 'Lieferzeitraum stimmt nicht mit Tagesabschnitt �berein. '+
                                 'Tournummer ge�ndert.';
                        IF (Lieferung.TourLFdNr <> 0) THEN BEGIN
                           Lieferung.TourLfdNr := 0;
                           FText:=FText+' Laufende Nummer gel�scht.'
                        END;
                        IF (Lieferung.Fahrer <> '') THEN BEGIN
                           Lieferung.Fahrer := '';
                           aktFahrer := Lieferung.Fahrer;
                           FText:=FText+' Fahrer gel�scht.'
                        END;
                        FText:= FText+' Lieferschein wurde nicht gespeichert.';
                        FaultBox.Act(0,FText);
                        LSave := Lieferung;
                     END;
                  END ELSE BEGIN
                     { TourNr festlegen }
                     IF UpdateTourNr THEN BEGIN
                        Lieferung.TourNr := MakeTabsIndex(Lieferung.TourNr,0,OldDay);
                     END ELSE BEGIN
                        FaultBox.Act(0,'Ung�ltige Startzeit bei Lieferzeitraum eingegeben, '+
                                       'Lieferschein kann nicht gespeichert werden.');
                        Gespeichert := FALSE;
                     END;
                  END;
               END ELSE BEGIN
                  Gespeichert := FALSE;
                  FaultBox.Act(2,'Keine g�ltige Position im Lieferschein. '+
                                 'Lieferschein kann nicht gespeichert werden.');
               END;
               IF Gespeichert THEN BEGIN
                  IF LieferDB.BeginTransaction THEN BEGIN
                     IF (NOT NeuerLieferschein) THEN
                        Gespeichert := LieferDB.ChangeRec(Lieferung)
                     ELSE BEGIN
                        Lieferung.LiefNr:=NextLNr(LieferDB.GetLfdLNr);
                        FOR Lauf := 1 TO 19 DO BEGIN
                          IF (Lieferung.Positionen[Lauf].ArtNr = '01908') THEN
                             Lieferung.Positionen[Lauf].ArtBez :=
                               Lieferung.Positionen[Lauf].ArtBez+Lieferung.LiefNr;
                        END;
                        FOR Lauf := 19 DOWNTO 1 DO BEGIN
                           IF (Lieferung.Positionen[Lauf].ArtNr = '01902') OR
                              (Lieferung.Positionen[Lauf].ArtNr = '01903') THEN BEGIN
                              DelPosition(Lauf,Lieferung);
                           END;
                        END;
                        Gespeichert := LieferDB.NewRec(Lieferung);
                        IF Gespeichert THEN
                           LieferDB.SetLfdLNr(Lieferung.LiefNr);
                     END;
                     IF NOT LieferDB.EndTransaction THEN
                        FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                                        LieferDB.GetTransactionErrMsg);
                  END ELSE BEGIN
                     FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                                     LieferDB.GetTransactionErrMsg);
                  END;
                  IF Gespeichert THEN BEGIN
                    FOR Lauf := 1 TO 30 DO
                        LiefPosEd[Lauf] := TRUE;
                     InfoBox.Act (2,'Lieferschein wurde gespeichert.');
                     LKurzDrucken(Lieferung);        { Eingabe zus�tzlich sichern }
                     AddToKSum(Lieferung);
                  END ELSE BEGIN
                     FaultBox.Act (2,'Datensatz konnte nicht gespeichert werden.');
                  END;
               END;
               IF NOT Gespeichert THEN BEGIN
                  Lieferung := LSave;
                  FOR Lauf := 1 TO 30 DO BEGIN
                     LiefPosEd[Lauf] := LEdSave[Lauf];
                     Pip[Lauf] := PipSave[Lauf];
                  END;
               END;
            END;
         END ELSE BEGIN
            FaultBox.Act(2,'Unzul�ssige Tournummer, Bitte �ndern');
            Gespeichert := FALSE;
         END;
         LSSpeichern := Gespeichert;
      END;

   FUNCTION ZeitLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         Taste := 0;
         InpString := '';
         Lieferung.Von[1] := '';
         Lieferung.Bis[1] := '';
         LFenster1^.Refresh('31');
         LFenster1^.Refresh('32');
         Abbruch := FALSE;
         Check := FALSE;
         ZeitLoeschen := TRUE;
      END;

   FUNCTION SucheHandyFile:BOOLEAN;FAR;
      VAR FocusStr : STRING;
          L:WORD;
      BEGIN
        IF (ActionTMod = A) OR (ActionText = '') THEN
          Abrechnen;
        SucheHandyFile:=FALSE
      END;


   FUNCTION ZweiteSeite(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusStr : STRING;
      BEGIN
         MoreEval := FALSE;
         HideKundenUebersicht;
         ClearKundenInfo;
         LFenster1^.Hide;
         IF (Taste = 13) OR (Taste =9) OR (Taste = -80) THEN
            LFenster2^.SetFocusOnId('07');
         Taste := LFenster2^.Input;
         LFenster2^.Hide;
         FocusStr:=  LFenster1^.GetFocusId;
         LFenster1^.Show('');
         IF (FocusStr <> '101') AND (FocusStr <> '102') THEN
            ShowKundenInfo;
         Taste := 0;
         Abbruch := FALSE;
         Check := FALSE;
         ZweiteSeite := TRUE;
      END;

   FUNCTION ProgrammEnde(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := Request.Act('Wollen Sie das Programm beenden ?');
         IF Ergebnis THEN
            Abbruch := TRUE;
         Check := FALSE;
         ProgrammEnde := Ergebnis;
      END;


   FUNCTION F3ZweiteSeite(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis:=ZweiteSeite(Taste,Nr,InpString,Abbruch,Check,MoreEval);
         F3ZweiteSeite := FALSE;
      END;

   FUNCTION ErsteSeite(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         Abbruch := TRUE;
         Check := FALSE;
         ErsteSeite := TRUE;
         IF (Taste = -15) OR (Taste = -72) THEN
            LFenster1^.SetFocusOnId('06')
         ELSE
            LFenster1^.SetFocusOnId('30');
         Taste := 0;
      END;

   FUNCTION DnToTab(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := TRUE;
         Taste := 9;
         Abbruch := FALSE;
         Check:= TRUE;
         DnToTab := TRUE;
      END;

   FUNCTION UpToSTab(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := TRUE;
         Taste := -15;
         Abbruch := FALSE;
         Check:= TRUE;
         UpToSTab := TRUE;
      END;


   FUNCTION PositionLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          KundeGefunden : BOOLEAN;
          KHelp :TKundenDaten;
          LoeschenErlaubt: BOOLEAN;
          OPUebertragen : BOOLEAN;
          OPEintrag : BYTE;
          OPZeile1: STRING[80];
          LFOk : BOOLEAN;
          OPNichtUebertragen : BOOLEAN;
       BEGIN
          MoreEval := FALSE;
          FocusID := LFenster1^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS);
          LoeschenErlaubt := TRUE;
          IF ((Lieferung.Positionen[FocusFK].ArtNr='902') OR
             (Lieferung.Positionen[FocusFK].ArtNr='903')) THEN BEGIN
             IF Lieferung.T100Bearbeitet THEN BEGIN
                FaultBox.Act(2,'Der offene Posten wurde schon abgerechnet.'+
                   ' ein L�schen ist nicht m�glich.');
                LoeschenErlaubt := FALSE;
             END ELSE BEGIN
                KundDb.BeginTransaction;
                KundeGefunden :=KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.KNR),0),KHelp);
                KundDB.EndTransaction;
                OPUebertragen := FALSE;
                IF KundeGefunden THEN BEGIN
                   OPUebertragen := OPRueckUebertragen(KHelp,Lieferung,FocusFK,OPEintrag,OPNichtUebertragen);
                END;
                IF OpUebertragen THEN BEGIN
                   KundDb.BeginTransaction;
                   OPUebertragen :=KundDB.ChangeRec(KHelp);
                   KundDB.EndTransaction;
                   IF (NOT OPUebertragen) THEN BEGIN
                      OPZeile1 :=ActDate+';'+ActTime+';'+'K-SCHREIBFEHLER'+';'+LeadingZeros(KHelp.LfdNr,5)+
                           ';'+Lieferung.LiefNr;
                      LFOk := OPLogFile^.WritelnLog(OpZeile1);
                      IF NOT LFOK THEN BEGIN
                         FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                         'Bitte folgende Infos eintragen: '+OpZeile1);
                      END;
                   END;
                END;
                LoeschenErlaubt := OPUebertragen;
                IF NOT LoeschenErlaubt THEN BEGIN
                   FaultBox.Act(2,'offener Posten konnte nicht in Kundensatz '+
                            'eingetragen werden. L�schen nicht m�glich');
                END

             END;
          END;
          IF LoeschenErlaubt THEN
             DelPosition(FocusFK,Lieferung);
          BerechnePreis;
          LFenster1^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          PositionLoeschen := TRUE;
          Check := FALSE;
       END;

   FUNCTION PositionS2Loeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          KundeGefunden : BOOLEAN;
          KHelp :TKundenDaten;
          LoeschenErlaubt: BOOLEAN;
          OPUebertragen : BOOLEAN;
          OPEintrag : BYTE;
          OPZeile1: STRING[80];
          LFOk : BOOLEAN;
          OPNichtUebertragen : BOOLEAN;
       BEGIN
          MoreEval := FALSE;
          FocusID := LFenster2^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS);
          LoeschenErlaubt := TRUE;
          IF ((Lieferung.Positionen[FocusFK].ArtNr='902') OR
             (Lieferung.Positionen[FocusFK].ArtNr='903')) THEN BEGIN
             IF Lieferung.T100Bearbeitet THEN BEGIN
                FaultBox.Act(2,'Der offene Posten wurde schon abgerechnet.'+
                   ' ein L�schen ist nicht m�glich.');
                LoeschenErlaubt := FALSE;
             END ELSE BEGIN
                KundDb.BeginTransaction;
                KundeGefunden :=KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.KNR),0),KHelp);
                KundDB.EndTransaction;
                OPUebertragen := FALSE;
                IF KundeGefunden THEN BEGIN
                   OPUebertragen := OPRueckUebertragen(KHelp,Lieferung,FocusFK,OPEintrag,OPNichtUebertragen);
                END;
                IF OpUebertragen THEN BEGIN
                   KundDb.BeginTransaction;
                   OPUebertragen :=KundDB.ChangeRec(KHelp);
                   KundDB.EndTransaction;
                   IF (NOT OPUebertragen) THEN BEGIN
                      OPZeile1 :=ActDate+';'+ActTime+';'+'K-SCHREIBFEHLER'+';'+LeadingZeros(KHelp.LfdNr,5)+
                           ';'+Lieferung.LiefNr;
                      LFOk := OPLogFile^.WritelnLog(OpZeile1);
                      IF NOT LFOK THEN BEGIN
                         FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                         'Bitte folgende Infos eintragen: '+OpZeile1);
                      END;
                   END;
                END;
                LoeschenErlaubt := OPUebertragen;
                IF NOT LoeschenErlaubt THEN BEGIN
                   FaultBox.Act(2,'offener Posten konnte nicht in Kundensatz '+
                            'eingetragen werden. L�schen nicht m�glich');
                END

             END;
          END;
          IF LoeschenErlaubt THEN
             DelPosition(FocusFK,Lieferung);
          LFenster2^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          PositionS2Loeschen := TRUE;
          Check := FALSE;
       END;

   FUNCTION LoescheFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         InpString:= '';
         LoescheFeld := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
      END;

   PROCEDURE TransferKDaten(VAR Lieferung:TLiefDaten;Kunde:TKundenDaten);
      VAR Lauf : BYTE;
          TLauf: WORD;
          AnzStdBest : BYTE;
          ArtBezGet : STRING;
          OpsEingetragen : BOOLEAN;
          AnzEingetragen : BYTE;
      BEGIN
         StdBestIstDefault := FALSE;
         ZeitIstDefault := FALSE;
         AnzSTdPos := 0;
         FOR Lauf := 1 TO 5 DO BEGIN
             LFenster1^.ClearDefault('0'+B2S(Lauf,0)+'_');
         END;
         WITH Lieferung DO BEGIN
            Knr := LeadingZeros(Kunde.LfdNr,5);
            Kundennummer := MakeLfdNr(Kunde.LfdNr,Kunde.LieferANr);
            IF (NeuerLieferschein) THEN BEGIN
               IF (Von[1] = '') THEN
                  Von[1] := Kunde.StdVon;
               IF (Bis[1] = '') THEN
                  Bis[1] := Kunde.StdBis;
               TLauf := 0;
               FOR Lauf := 1 TO 5 DO BEGIN
                  LiefPosEd[Lauf] := TRUE;
               END;
               Lauf := 5;
               WHILE (AnzStdPos = 0) AND (Lauf >=1) DO BEGIN
                  WITH Kunde.StdBest[Lauf] DO
                    IF (Menge <> 0) OR (ArtNr <> '') OR (ArtBez <> '')
                       OR (Preis <> 0) OR Uebernehmen THEN
                       AnzStdPos := Lauf;
                  DEC(Lauf);
               END;
               FOR Lauf := 1 TO AnzStdPos DO BEGIN
                  IF (Kunde.StdBest[Lauf].ArtNr <> '') THEN BEGIN
                     Positionen[Lauf].Menge := Kunde.StdBest[Lauf].Menge;
                     Positionen[Lauf].ArtNr := Kunde.StdBest[Lauf].ArtNr;
                     ArtBezGet :=Positionen[Lauf].ArtNr;
                     SeekArt(ArtBezGet,Lauf);
                     LiefPosEd[Lauf] := Kunde.StdBest[Lauf].Uebernehmen;
                     IF (Positionen[Lauf].ArtBez <> Kunde.StdBest[Lauf].ArtBez) AND
                        (Positionen[Lauf].ArtNr <> '908') THEN BEGIN
                         Positionen[Lauf].ArtBez:=Kunde.StdBest[Lauf].ArtBez;
                     END;
                     {&&&&&&&&&}
                     IF (Positionen[Lauf].ArtNr >='900') THEN BEGIN
                        Positionen[Lauf].Preis := Kunde.StdBest[Lauf].Preis;
                     END;
                     IF (Positionen[Lauf].ArtNr = '990') THEN BEGIN
                        Positionen[Lauf].Preis := Kunde.StdBest[Lauf].Preis;
                     END;
                     IF (Positionen[Lauf].ArtNr = '997') THEN BEGIN
                        Positionen[Lauf].Preis := Kunde.StdBest[Lauf].Preis;
                     END;
                  END;
               END;
               IF (AnzStdPos > 0) THEN BEGIN
                  StdBestIstDefault := TRUE;
                  FOR Lauf := 1 TO AnzStdPos DO BEGIN
                      LFenster1^.SetDefault('0'+B2S(Lauf,0)+'_');
                  END
               END;
               OpsEintragen(FALSE,FALSE,OpsEingetragen,AnzEingetragen);
            END;
            { Belegung von Zeitdefaultwert }
            ZeitIstDefault := IsStdZeit;
            IF ZeitIstDefault THEN BEGIN
               LFenster1^.SetDefault('31');
               LFenster1^.SetDefault('32');
            END ELSE BEGIN
               LFenster1^.ClearDefault('31');
               LFenster1^.ClearDefault('32');
            END;
            IF (AnzStdPos > 0) AND (NOT StdBestIstDefault) THEN BEGIN
               StdBestIstDefault := IsStdBest;
               IF StdBestIstDefault THEN BEGIN
                  FOR Lauf := 1 TO AnzSTdPos DO BEGIN
                      LFenster1^.SetDefault('0'+B2S(Lauf,0)+'_');
                  END;
               END;
            END;
         END;
      END;

   PROCEDURE ResetPfand;
      VAR Lauf:BYTE;
      BEGIN
        VollSum:=0;
        LeerSum:=0;
        GesaSum:=0;
        FOR Lauf:=1 TO 30 DO
          PiP[Lauf]:=0;
      END;

   PROCEDURE KundenFeldAktiv;
      VAR Lauf : BYTE;
      BEGIN
         LFenster1^.Disable('');
         FOR Lauf := 1 TO 6 DO
            LFenster1^.SetToggle(LeadingZeros(Lauf,2),FALSE);
         LFenster1^.Enable('101');
         LFenster1^.SetFocusOnId('101');
         ClearKundenInfo;
         NeuerLieferschein := TRUE;
      END;

   PROCEDURE LSFeldAktiv;
      VAR Lauf : BYTE;
          SeekResult : BOOLEAN;
      BEGIN
         WITH LFenster1^ DO BEGIN
           Enable('');
           FOR Lauf := 1 TO 6 DO
             SetToggle(LeadingZeros(Lauf,2),TRUE);
           Disable('10');
           Disable('21');
           IF (PrgLaden) THEN BEGIN
             Disable('30');
             Disable('31');
             Disable('32');
           END;
           Disable('33');
           Disable('101');
           Disable('102');
           FOR Lauf := 1 TO 30 DO
             LiefPosEd[Lauf] := TRUE;
           Enable('102');
           SetFocusOnId('102');
         END;
         SeekResult := (Lieferung.LiefNr <> '');
         IF SeekResult THEN BEGIN
            IF LieferDB.BeginTransaction THEN BEGIN
               SeekResult := LieferDB.GetRec(1,Lieferung.LiefNr,Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                            LieferDB.GetTransactionErrMsg);
            END;
         END;
         IF NOT SeekResult THEN BEGIN
            InitTLiefDaten(Lieferung);
         END;
         ZeigeLieferscheinzuLSNummer;
         NeuerLieferschein := FALSE;
      END;

   PROCEDURE NeuerLS;
      VAR Lauf : BYTE;
      BEGIN
        WITH LFenster1^ DO BEGIN
          Enable('');
          FOR Lauf := 1 TO 6 DO
            SetToggle(LeadingZeros(Lauf,2),TRUE);
          Disable('10');
          Disable('101');
          Disable('102');
          Disable('15');
          Disable('16');
          Disable('21');
          Disable('7');
          IF (PrgLaden) THEN BEGIN
            Disable('30');
            Disable('31');
            Disable('32');
          END;
          Disable('33');
        END;
         ShowKundenInfo;
         NeuerLieferschein := TRUE;
      END;

   PROCEDURE LSAendern;
      VAR Lauf : BYTE;
      BEGIN
        WITH LFenster1^ DO BEGIN
          Enable('');
          FOR Lauf := 1 TO 6 DO
            SetToggle(LeadingZeros(Lauf,2),TRUE);
          Disable('10');
          Disable('21');
          Disable('101');
          Disable('102');
          IF (PrgLaden) THEN BEGIN
            Disable('30');
            Disable('31');
            Disable('32');
          END;
          Disable('33');
        END;
         FOR Lauf := 1 TO 30 DO
            LiefPosEd[Lauf] := TRUE;
         ShowKundenInfo;
         NeuerLieferschein := FALSE;
      END;

   FUNCTION AndereSortierung(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         INC(KSuchKennung);
         IF (KSuchKennung = 5) THEN
            KSuchKennung :=1;
         SearchFenster^.Hide;
         AndereSortierung := TRUE;
         MoreEval := FALSE;
         Abbruch :=FALSE;
         Check := FALSE;
      END;


   FUNCTION KSeekFenster:STRING;
      CONST SFTitel : ARRAY[1..4] OF STRING = ('Kunden',
                                               'Strassennamen',
                                               'Firmenkunden',
                                               'Privatkunden');
      VAR   Ende : BOOLEAN;
            Endcode :INTEGER;
            AktPos     :  BYTE;
      BEGIN
         Ende := FALSE;
         EndCode := 0;
         KSuchKennung := 1;
         REPEAT
             SearchFenster^.SetTitel(SFTitel[KSuchKennung]);
             EndCode := SearchFenster^.Input;
             CASE EndCode OF
                 13,-68 : BEGIN
                             AktPos := SearchFenster^.ResultRow;
                             KLfd:=LeadingZeros(KunArray[aktPos].Knr,5);
                             KLfdNo := KunArray[aktPos].Knr;
                             KSeekFenster := MakeLfdNr(KunArray[aktPos].Knr,KunArray[aktPos].LAdr);
                             Ende := TRUE;
                          END;
                 27     : BEGIN
                             KSeekFenster := '';
                             Ende := TRUE;
                          END;
             END;
         UNTIL Ende;
         SearchFenster^.Hide;
      END;

   FUNCTION GetKAdr(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;FAR;
      BEGIN
         KundDB.BeginTransaction;
         KundDB.GetRec(1,MakeLfdNr(Kunde.LfdNr,(Nr-1)),Kunde);
         KunddB.EndTransaction;
         GetKAdr:= FALSE;
      END;

   FUNCTION ChangeKundenLAdr:BOOLEAN;
         VAR   KAuswahlMenue : TAuswahlMenuePtr;
               AnzAnschr     : BYTE;
               SaveSTdPos : BYTE;
               Lauf : BYTE;
               KHelp : TKundenDaten;
               EndCode : INTEGER;
         BEGIN
            ChangeKundenLAdr := FALSE;
            AnzAnschr := 0;
            KHelp := Kunde;
            IF KundDB.BeginTransaction THEN BEGIN
               IF KundDB.StartIntervall(1,MakeLfdNr(Kunde.LfdNr,0),MakeLfdNr(Kunde.LfdNr,9),KHelp) THEN BEGIN
                  NEW(KAuswahlMenue);
                  KAuswahlMenue^.Init(15,7,2,'Adressen','ASIN2',7,2);
                  REPEAT
                    KAuswahlMenue^.MenuItem('',KHelp.LiefStrasse,GetKAdr);
                    INC(AnzAnschr);
                  UNTIL NOT KundDB.GetIntNext(KHelp);
                  KHelp := Kunde;
                  KundDB.EndTransaction;
                  Endcode := KAuswahlMenue^.Select(TRUE);
                  ChangeKundenLAdr := (Endcode <> 27);
                  DISPOSE(KAuswahlMenue);
               END;
            END;
            IF (KHelp.LieferANr <> Kunde.LieferANR) THEN BEGIN
               IF AnzStdPos < 5 THEN BEGIN
                  FOR Lauf := 19-(5-AnzStdPos) DOWNTO AnzStdPos DO BEGIN
                     Lieferung.Positionen[Lauf+(5-AnzStdPos)]:=
                     Lieferung.Positionen[Lauf];
                     LiefPosED[Lauf+(5-AnzStdPos)]:=LiefPosED[Lauf];
                  END;
               END;
               TransferKDaten(Lieferung,Kunde);
               IF (AnzStdPos < 5) THEN BEGIN
                  FOR Lauf := AnzStdPos+1 TO 19-(5-AnzStdPos) DO BEGIN
                     Lieferung.Positionen[Lauf]:=
                     Lieferung.Positionen[Lauf+(5-AnzStdPos)];
                     LiefPosED[Lauf]:=LiefPosED[Lauf+(5-AnzStdPos)];
                  END;
               END;
               BerechnePreis;
            END ELSE
               Kunde := KHelp;
            LFenster1^.Refresh('');
         END;

   FUNCTION AndereAnschrift(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         HideKundenUebersicht;
         ChangeKundenLAdr;
         ShowKundeninfo;
         ShowKundenUebersicht;
         Abbruch := FALSE;
         Check := FALSE;
         AndereAnschrift := FALSE;
      END;

   FUNCTION ZuKundenFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR KnrSik:STRING;
      BEGIN
         MoreEval := FALSE;
         LFenster2^.Hide;
         KnrSik :=Lieferung.Knr;
         InitTLiefdaten(Lieferung);
         Lieferung.Knr:=KnrSik;
         BerechnePreis;
         GetLiefStatus;
         Lieferung.LieferTag := '';
         InpString:='';
         AktFahrer:= Lieferung.Fahrer;
         LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
         LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
         DispTagString(Lieferung.Liefertag);
         Taste := 0;
         ZuKundenFeld := TRUE;
         KundenFeldAktiv;
         LFenster1^.Refresh('');
      END;

   FUNCTION ESCZuKundenFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis :BOOLEAN;
          OldDay : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         HideKundenUebersicht;
         Ergebnis := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
         IF NOT (LFenster1^.Changed('') OR LFenster2^.Changed('')) OR
            Request.Act('Wollen Sie die Eingaben verwerfen ? ') THEN BEGIN
            IF (NOT NeuerLieferschein) THEN BEGIN
               LFenster1^.Restore('');
               LFenster2^.Restore('');
               OldDay:= IsOldDay(Lieferung.Liefertag);
               Mask2Lie(Lieferung,OldDay);
               AddToKSum(Lieferung);
            END;
            Ergebnis := ZuKundenFeld(Taste,Nr,InpString,Abbruch,Check,MoreEval);
         END;
         ESCZuKundenFeld := Ergebnis;
      END;


   FUNCTION Seite2ZuKFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis :BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := FALSE;
         IF NOT (LFenster1^.Changed('') OR LFenster2^.Changed('')) OR
            Request.Act('Wollen Sie die Eingaben verwerfen ? ') THEN BEGIN
            InitTLiefdaten(Lieferung);
            BerechnePreis;
            GetLiefStatus;
            Lieferung.LieferTag := '';
            AktFahrer:= Lieferung.Fahrer;
            LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
            LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
            DispTagString(Lieferung.Liefertag);
            Lieferung.Knr := Klfd;
            Taste := 0;
            Ergebnis := TRUE;
            KundenFeldAktiv;
         END;
         Abbruch := TRUE;
         Check:= FALSE;
         Seite2zuKFeld := Ergebnis;
      END;

   FUNCTION F7zuLiefNr(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis :BOOLEAN;
          LNrSave : STRING;
      BEGIN
         MoreEval := FALSE;
         HideKundenUebersicht;
         Ergebnis := TRUE;
         LNrSave := Lieferung.LiefNr;
         InitTLiefdaten(Lieferung);
         Lieferung.LiefNr := LNrSave;
         ClearKundenInfo;
         BerechnePreis;
         GetLiefStatus;
         Lieferung.LieferTag := '';
         AktFahrer:= Lieferung.Fahrer;
         LDatum := '';
         LUhrzeit:= '';
         DispTagString(Lieferung.Liefertag);
         Lieferung.Knr := '';
         Taste := 0;
         Ergebnis := TRUE;
         LSFeldAktiv;
         LFenster1^.Refresh('');
         Abbruch := FALSE;
         Check:= FALSE;
         F7ZuLiefNr := Ergebnis;
      END;

   FUNCTION WechselzuLSNr(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis :BOOLEAN;
          OldDay,
          DOWechsel:BOOLEAN;

          LNrSave : STRING;
          ChoiceRes : INTEGER;
      BEGIN
         MoreEval := FALSE;
         HideKundenUebersicht;
         DOWechsel := TRUE;
         ChoiceRes:= 0;
         IF LFenster1^.Changed('') OR LFenster2^.Changed('') THEN BEGIN
            DOWechsel := FALSE;
            STripleQuest.SetChoice('   Ja    ','  Nein   ',' Zur�ck  ');
            ChoiceRes:= STripleQuest.Act('Am Lieferschein wurden �nderungen durchgef�hrt. '+
                             ' Soll der Lieferschein gespeichert werden ?');
            STripleQuest.SetChoice('   Ja    ',' �ndern  ','  Nein   ');
            IF ChoiceRes = 1 THEN
              DOWechsel := LSSpeichern
            ELSE BEGIN
               DOWechsel:=(ChoiceRes <> -1);
            END;
         END;
         Ergebnis := FALSE;
         IF DOWechsel THEN BEGIN
            IF (NOT NeuerLieferschein) AND (ChoiceRes = 0)THEN BEGIN
               LFenster1^.Restore('');
               LFenster2^.Restore('');
               OldDay:= IsOldDay(Lieferung.Liefertag);
               Mask2Lie(Lieferung,OldDay);
               AddToKSum(Lieferung);
            END;
            Ergebnis := TRUE;
            LNrSave := Lieferung.LiefNr;
            InitTLiefdaten(Lieferung);
            Lieferung.LiefNr := LNrSave;
            ClearKundenInfo;
            BerechnePreis;
            GetLiefStatus;
            Lieferung.LieferTag := '';
            AktFahrer:= Lieferung.Fahrer;
            LDatum := '';
            LUhrzeit:= '';
            DispTagString(Lieferung.Liefertag);
            Lieferung.Knr := '';
            Taste := 0;
            Ergebnis := TRUE;
            LSFeldAktiv;
            LFenster1^.Refresh('');
         END;
         Abbruch := FALSE;
         Check:= FALSE;
         WechselZuLSNr := Ergebnis;
      END;

   FUNCTION S2WechselzuLSNr(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis :BOOLEAN;
          OldDay,
          DOWechsel:BOOLEAN;

          LNrSave : STRING;
          ChoiceRes : INTEGER;
      BEGIN
         MoreEval := FALSE;
         DOWechsel := TRUE;
         ChoiceRes := 0;
         IF LFenster1^.Changed('') OR LFenster2^.Changed('') THEN BEGIN
            DOWechsel := FALSE;
            STripleQuest.SetChoice('   Ja    ','  Nein   ',' Zur�ck  ');
            ChoiceRes:= STripleQuest.Act('Am Lieferschein wurden �nderungen durchgef�hrt. '+
                             ' Soll der Lieferschein gespeichert werden ?');
            STripleQuest.SetChoice('   Ja    ',' �ndern  ','  Nein   ');
            IF ChoiceRes = 1 THEN
              DOWechsel := LSSpeichern
            ELSE
               DOWechsel:=(ChoiceRes <> -1)
         END;
         Ergebnis := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
         IF DOWechsel THEN BEGIN
            IF (NOT NeuerLieferschein) AND (ChoiceRes = 0)THEN BEGIN
               LFenster1^.Restore('');
               LFenster2^.Restore('');
               OldDay:= IsOldDay(Lieferung.Liefertag);
               Mask2Lie(Lieferung,OldDay);
               AddToKSum(Lieferung);
            END;
            Ergebnis := TRUE;
            LNrSave := Lieferung.LiefNr;
            InitTLiefdaten(Lieferung);
            Lieferung.LiefNr := LNrSave;
            ClearKundenInfo;
            BerechnePreis;
            GetLiefStatus;
            Lieferung.LieferTag := '';
            AktFahrer:= Lieferung.Fahrer;
            LDatum := '';
            LUhrzeit:= '';
            DispTagString(Lieferung.Liefertag);
            Lieferung.Knr := '';
            Taste := 0;
            Ergebnis := TRUE;
            LSFeldAktiv;
            Abbruch := TRUE;
            Check := FALSE;
         END;
         S2WechselZuLSNr := Ergebnis;
      END;

   FUNCTION SeekArtLS1(VAR ArtNr:STRING; ArrayPos:WORD):BOOLEAN;FAR;
      BEGIN
         SeekArtLs1:= SeekArt(ArtNr,ArrayPos);
         IF BerechnePreis THEN BEGIN
            LFenster1^.Refresh('10');
            LFenster1^.Refresh('21');
         END;
         LFenster1^.Refresh(LeadingZeros(ArrayPos,2));
      END;

   FUNCTION SeekArtLS2(VAR ArtNr:STRING; ArrayPos:WORD):BOOLEAN;FAR;
      BEGIN
         SeekArtLs2:= SeekArt(ArtNr,ArrayPos);
         IF BerechnePreis THEN BEGIN
            LFenster2^.Refresh('10');
            LFenster2^.Refresh('21');
         END;
         LFenster2^.Refresh(LeadingZeros(ArrayPos,2));
      END;

   FUNCTION PositionenOk(VAR ErsteFalsch:BYTE):BOOLEAN;
      VAR Lauf : BYTE;
          Ende : BOOLEAN;

      BEGIN
         ErsteFalsch := 0;
         Lauf := 1;
         Ende := FALSE;
         WHILE (Lauf <= 19) AND NOT Ende DO BEGIN
            WITH Lieferung.Positionen[Lauf] DO BEGIN
               IF (LiefPosEd[Lauf]) AND (Menge = 0)
                  AND ((ArtNr <> '') OR (ArtBez <>'') OR (Preis <> 0)) THEN BEGIN
                  ErsteFalsch := Lauf;
                  Ende := TRUE;
                  FaultBox.Act(2,'Keine Menge in Position '+B2S(ErsteFalsch,0)+
                      '. Lieferschein kann nicht gespeichert werden.');
               END;
            END;
            INC(Lauf);
         END;
         PositionenOk := (ErsteFalsch =0);
      END;

   FUNCTION TagAbschnittOk:BOOLEAN;
      VAR Bereich : BYTE;
          G,
          AktTag,
          AktTabs : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := TRUE;
         IF (Kunde.LfdNr <> 0) THEN BEGIN
            Bereich:=FindKXPos(S2L(Kunde.PlanQ));
            IF (Bereich <= AnzBereiche) THEN BEGIN
               AktTag:=DayOfWeek(Lieferung.Liefertag);
               SplitIFieldTNr(Lieferung.TourNr,AktTabs,G);
               IF ((BUTag <> AktTag) OR
                   (AktTabs <>BUTabs) OR
                   (Bereich <> BUBer) OR NOT BerUeberprueft) THEN BEGIN
                  BUTag := AktTag;
                  BUBer := Bereich;
                  BUTabs := AktTabs;
                  BerUeberprueft := TRUE;
                  Ergebnis := BTArray[BUTabs,BUBer,BUTag];
                  IF NOT Ergebnis THEN
                     FaultBox.Act(3,'Achtung! Dieser Stadtteil wird '+
                                  'in diesem Tagesabschnitt nicht angefahren');
               END;
            END;
         END;
         TagAbschnittOk := Ergebnis;
      END;

   FUNCTION LZeitOk(VAR VonOk:BOOLEAN):BOOLEAN;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;

      BEGIN
         IF (Lieferung.KNr <> '00000') AND
             ((ZeitUVon <> Lieferung.Von[1]) OR
              (ZeitUBis <> Lieferung.Bis[1]) OR NOT ZeitUeberprueft) THEN BEGIN
            WITH Lieferung DO BEGIN
               Ergebnis :=ValidTime(Von[1]) AND
                          ValidTime(Bis[1]) AND
                          (NotLaterTime(NextHour(Von[1]),Bis[1]));
               IF NOT Ergebnis THEN BEGIN
                  VonOk :=ValidTime(Von[1]);
                  ZeitUVon := Von[1];
                  ZeitUBis := Bis[1];
                  ZeitUeberprueft := TRUE;
                  FaultBox.Act(2,'Fehler im Lieferzeitraum '+
                       '. Lieferschein kann nicht gespeichert werden.');
               END
            END;
         END ELSE
            Ergebnis := TRUE;
         LZeitOk := Ergebnis;
      END;

   FUNCTION F10BestSpeichern(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Gespeichert : BOOLEAN;
          Ergebnis    : BOOLEAN;
          PosNr : BYTE;
          VonOk : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         PosNr := 0;
         IF (PrgLaden) THEN
           LeergutToBemerk;
         Gespeichert := PositionenOk(PosNr);
         Ergebnis := TRUE;
         HideKundenUebersicht;
         IF Gespeichert THEN BEGIN
            VonOk := TRUE;
            IF NOT(PrgLaden) THEN
              Gespeichert := LZeitOk(VonOk)
            ELSE
              Gespeichert := TRUE;
            IF Gespeichert THEN BEGIN
               Gespeichert := TagAbschnittOk;
               IF Gespeichert THEN BEGIN
                  Gespeichert := LSSpeichern;
                  IF Gespeichert THEN BEGIN
                     ZeitUVon := '';
                     ZeitUBis := '';
                     ZeitUeberprueft := FALSE;
                     BUTag := 0;
                     BUTAbs := 0;
                     BUBer := 0;
                     BerUeberprueft := FALSE;
                     InitTliefDaten(Lieferung);
                     BerechnePreis;
                     GetLiefStatus;
                     Lieferung.LieferTag := '';
                     AktFahrer:= Lieferung.Fahrer;
                     LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
                     LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
                     DispTagString(Lieferung.Liefertag);
                     Lieferung.Knr := Klfd;
                     Taste := 0;
                     Ergebnis := TRUE;
                     KundenFeldAktiv;
                     LFenster1^.Refresh('');
                  END;
               END;
            END ELSE BEGIN
               IF VonOk THEN
                  LFenster1^.SetFocusOnId('32')
               ELSE
                  LFenster1^.SetFocusOnId('31');
            END;
         END ELSE BEGIN
            IF (PosNr < 7) THEN
               LFenster1^.SetFocusOnId(LeadingZeros(PosNr,2)+'_1');
         END;
         IF NOT Gespeichert THEN
            Taste := 0;
         LFenster1^.Refresh('');
         Abbruch := FALSE;
         Check:= FALSE;
         F10BestSpeichern:= Ergebnis;
      END;

   FUNCTION F10S2BestSpeichern(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         LFenster2^.Hide;
         LFenster1^.Show('');
         ShowKundenInfo;
         F10S2BestSpeichern:=F10BestSpeichern(Taste,Nr,InpSTring,Abbruch,Check,MoreEval);
         Abbruch:= TRUE;
      END;


   FUNCTION LFenster1Focus1(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
        MoreEval := FALSE;
        HideKundenUebersicht;
        LFenster1^.SetFocusOnId('30');
        Abbruch := FALSE;
        Taste := 0;
        LFenster1Focus1 := TRUE;
      END;

   FUNCTION LFenster2Focus1(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
        MoreEval := FALSE;
        LFenster1^.SetFocusOnId('30');
        Abbruch := TRUE;
        Taste := 0;
        LFenster2Focus1 := TRUE;
      END;

   PROCEDURE NeueBestellung;
      VAR Lauf : BYTE;
          x,y: BYTE;
          LDatAlt : TDatum;
          LTourAlt : BYTE;
          LHelp : TLiefDaten;
      BEGIN
         NeuerLieferschein := TRUE;
         BUTag := 0;
         BUTAbs := 0;
         BUBer := 0;
         BerUeberprueft := FALSE;
         ZeitUBis := '';
         ZeitUVon := '';
         ZeitUeberprueft := FALSE;
         AnzStdPos := 0;
         ResetPfand;
         InitLeerGut;
         FOR Lauf := 1 TO 30 DO
             LiefPosEd[Lauf] := TRUE;
         { Defaultwerte }
         LDatAlt := NaechsterLTag;
         InitTLiefDaten(Lieferung);
         LTourAlt := Lieferung.TourNr;
         Lieferung.LieferTag:=LDatAlt;
         IF (PrgLaden) THEN
           Lieferung.Liefertag:=ActDate;
         TransferKDaten(Lieferung,Kunde);
         Lieferung.Bemerkungen[3] := ActDate+ActTime;
         BerechnePreis;
         GetLiefStatus;
         Lieferung.Fahrer:='';
         AktFahrer:='';
         LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
         LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);


         { Dateiorganisationsfehlermeldungen }
         IF LieferDB.BeginTransaction THEN BEGIN
            IF Lieferdb.StartIntervall(2,MakeKundIndex(Lieferung.Knr,ActDate,0),
                                         MakeKundIndex(Lieferung.Knr,
                                                       '31.12.'+LeadingZeros(CJahrhundertWechsel,2),999),LHelp) THEN BEGIN
               IF NOT(PrgLaden) THEN
                 InfoBox.Act(2,'Kunde wird bereits am '+LHelp.Liefertag+' beliefert. ');
            END;
            Lieferung.LiefNr := '';
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                              LieferDB.GetTransactionErrMsg);
         END ELSE BEGIN
            FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                           LieferDB.GetTransactionErrMsg);
         END;
         LFenster1^.Save('');
         LFenster2^.Save('');
      END;

   FUNCTION KSuchen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                             VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
          KErgebnis : STRING;

      BEGIN
         MoreEval := FALSE;
         Ergebnis:= FALSE;
         KErgebnis:= KSeekFenster;
         IF (KErgebnis <> '') THEN BEGIN
            KundDB.BeginTransaction;
            Ergebnis := KundDB.GetRec(1,KErgebnis,Kunde);
            KundDB.EndTransaction;
            KLfdNo := Kunde.LfdNr;
            InpSTring:= LeadingZeros(KlfdNo,5);
            KLfd := InpString;
            NeueBestellung;
            NeuerLS;
            Taste := 0;
            Ergebnis  := TRUE;
         END;
         KSuchen := Ergebnis;
      END;

   FUNCTION CRNeueBestellung(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                             VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR DONeu: BOOLEAN;
          Ergebnis : BOOLEAN;
          KErgebnis : STRING;
          KNrIdx : STRING;
      BEGIN
         MoreEval := FALSE;
         Ergebnis:= FALSE;
         IF RemoveAllSpaces(InpString) ='' THEN BEGIN
            Ergebnis :=KSuchen(Taste,Nr,InpString,Abbruch,Check,MoreEval);
         END ELSE BEGIN
            KNrIdx := MakeLfdNr(S2l(InpString),0);
            IF ((InpString <> '00000') AND (KnrIdx <> '000000'))
              OR (InpString ='00000') THEN BEGIN
              KundDb.BeginTransaction;
              DoNeu := KundDB.GetRec(1,MakeLfdNr(S2l(InpString),0),Kunde);
              KundDB.EndTransaction;
              IF NOT DONeu THEN BEGIN
                 FaultBox.Act(2,'Eingegeben Kundennummer existiert nicht');
              END ELSE BEGIN
                 KLfdNo := S2L(InpString);
                 InpSTring:= LeadingZeros(KlfdNo,5);
                 KLfd := InpString;
                 NeueBestellung;
                 NeuerLS;
                 GetLiefStatus;
                 Taste := 0;
                 Ergebnis := TRUE;
              END;
           END;
         END;
         CRNeueBestellung:= Ergebnis;
         Abbruch := FALSE;
         Check:= FALSE;
      END;


   FUNCTION LieferungLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LNrSave : STRING;
          SeekResult : BOOLEAN;
          OldDay : BOOLEAN;
          Ergebnis     : BOOLEAN;
          KHelp : TKundenDaten;
          LPosLauf,
          Lauf,
          OPEintrag : BYTE;
          OPZeile1,
          OPZeile2 : STRING[80];
          KundeGefunden,
          OPGefunden,
          OPNichtUebertragen,
          LFOk : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := FALSE;
         IF LieferDB.BeginTransaction THEN BEGIN
            SeekResult := LieferDB.GetRec(1,InpString,Lieferung);
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                              LieferDB.GetTransactionErrMsg);
            IF SeekResult THEN BEGIN
               Lie2Mask(Lieferung);
               KundDb.BeginTransaction;
               IF NOT KundDb.GetRec(1,Lieferung.Kundennummer,Kunde) THEN
                  KundDb.GetRec(1,MakeLfdNr(S2l(Lieferung.Knr),0),Kunde);
               KundDb.EndTransaction;
               ShowKundenInfo;
               BerechnePreis;
               GetLiefStatus;
               LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
               LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
               AktFahrer := Lieferung.Fahrer;
               DispTagString(Lieferung.Liefertag);
               LFenster1^.Refresh('');
               IF HRequest.Act('Wollen Sie diesen Lieferschein l�schen ?') THEN BEGIN
                  RNummer := Random(998)+1;
                  BNummer := 0;
                  LBestaetigen^.InputWithBrain;
                  IF NOT LBestaetigen^.Abbruch AND (Bnummer = Rnummer) THEN BEGIN
                     IF LieferDB.BeginTransaction THEN BEGIN
                        IF NOT Lieferung.T100Bearbeitet THEN BEGIN
                           KundDB.BeginTransaction;
                           KundeGefunden :=KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.KnR),0),KHelp);
                           KundDB.EndTransaction;
                           OpGefunden := FALSE;
                           OPNichtUebertragen := FALSE;
                           LPosLauf := 1;
                           OPEintrag := 1;
                           WHILE (LPosLauf <= 19) DO BEGIN
                              WITH Lieferung.Positionen[LPosLauf] DO BEGIN
                                 IF (((ArtNr = '01902') OR (ArtNr = '01903')) OR ((LENGTH(ArtNr) =3) AND
                                      ((ArtNr = '902') OR (ArtNr= '903'))))
                                    AND (OpEintrag <> 11) THEN BEGIN
                                    OPGefunden:= TRUE;
                                    OPEintrag := 11;
                                    IF KundeGefunden THEN BEGIN
                                       OPRueckUebertragen(KHelp,Lieferung,LPosLauf,OPEintrag,OPNichtUebertragen);
                                       IF OPNichtUebertragen THEN BEGIN
                                          FaultBox.Act(0,'Keine freie Position in OP-Liste, '+
                                             ' kann den offenen Posten nicht eintragen. '+
                                             ' offenen Posten aus Lieferschein in Kundensatz '+
                                              LeadingZeros(KHelp.LfdNr,5)+
                                             ' �bertragen und erneut l�schen.');
                                       END;
                                    END;
                                 END;
                              END;
                              INC(LPosLauf);
                           END;
                        END;
                        Ergebnis := TRUE;
                        IF (OPGefunden) AND (NOT OPNichtUebertragen) AND (KundeGefunden) THEN BEGIN
                           KundDb.BeginTransaction;
                           Ergebnis := KundDB.CHangeRec(KHelp);
                           KundDB.EndTransaction;
                           IF NOT Ergebnis THEN BEGIN
                              OPZeile1 :=ActDate+';'+ActTime+';'+'K-SCHREIBFEHLER'+';'+LeadingZeros(KHelp.LfdNr,5)+
                                 ';'+Lieferung.LiefNr;
                              LFOk := OPLogFile^.WritelnLog(OpZeile1);
                              IF NOT LFOK THEN BEGIN
                                 FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                    'Bitte folgende Infos eintragen: '+OpZeile1);
                              END;
                           END;
                        END;
                        IF Ergebnis THEN
                           Ergebnis :=LieferDB.DelRec(Lieferung.LiefNr);
                        IF NOT LieferDB.EndTransaction THEN
                           FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                                      LieferDB.GetTransactionErrMsg);
                     END;
                     IF Ergebnis THEN BEGIN
                        OldDay := IsOldDay(Lieferung.Liefertag);
                        Mask2Lie(Lieferung,OldDay);
                        SubFromKSum(Lieferung);
                        InitTLiefdaten(Lieferung);
                        BerechnePreis;
                        GetLiefStatus;
                        Lieferung.LieferTag := '';
                        AktFahrer:= Lieferung.Fahrer;
                        LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
                        LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
                        DispTagString(Lieferung.Liefertag);
                        Lieferung.Knr := Klfd;
                        Taste := 0;
                        InfoBox.Act(2,'Lieferschein gel�scht');
                        KundenFeldAktiv;
                     END ELSE BEGIN
                        FaultBox.Act(0,'Fehler beim L�schen des Lieferscheins');
                     END;
                  END ELSE
                    FaultBox.Act(2,'L�schvorgang abgebrochen.');
               END;
               IF NOT Ergebnis THEN BEGIN
                  LNrSave := Lieferung.LiefNr;
                  InitTLiefdaten(Lieferung);
                  BerechnePreis;
                  GetLiefstatus;
                  Lieferung.LieferTag := '';
                  AktFahrer:= Lieferung.Fahrer;
                  LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
                  LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
                  DispTagString(Lieferung.Liefertag);
                  Lieferung.Knr := Klfd;
                  Lieferung.LiefNr:= LNrSave;
                  Taste := 0;
               END;
               LFenster1^.Refresh('');
               ClearKundenInfo;
            END ELSE BEGIN
               FaultBox.Act(0,'Lieferschein existiert nicht');
            END
         END ELSE BEGIN
            FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                           LieferDB.GetTransactionErrMsg);
         END;
         Abbruch := FALSE;
         Check:= FALSE;
         LieferungLoeschen := Ergebnis;
      END;

   FUNCTION AlteLieferungInput(VAR ChoiceRes:INTEGER;MoreEval:BOOLEAN):INTEGER;
      VAR  EndCode : INTEGER;
           KGefunden : BOOLEAN;
           Done : BOOLEAN;
      BEGIN
         REPEAT
            IF (ChoiceRes <> 2) THEN BEGIN
               Lie2Mask(Lieferung);
               BerechnePreis;
               GetLiefStatus;
               LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
               LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
               AktFahrer := Lieferung.Fahrer;
               DispTagString(Lieferung.Liefertag);
               LFenster1^.Refresh('');
               KundDb.BeginTransaction;
               KGefunden := KunddB.GetRec(1,Lieferung.Kundennummer,Kunde);
               IF NOT KGefunden THEN
                  KGefunden := KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
               KundDb.EndTransaction;
               IF NOT KGefunden THEN BEGIN
                  FaultBox.Act(2,'Kunde '+Lieferung.Knr+ ' existiert nicht!!');
                  InitTKundendaten(Kunde);
                  Kunde.LfdNr := S2l(Lieferung.Knr);
               END;

               ShowKundenInfo;
            END;
            EndCode := InputKey;
            CASE EndCode OF
               42 : BEGIN
                       ChoiceRes := 1;
                       MoreEval := FALSE;
                       Done := FALSE;
                    END;
               -65: BEGIN
                       ChoiceRes := 0;
                       MoreEval:= TRUE;
                       Done := TRUE;
                    END;
               27 : BEGIN
                       ChoiceRes := -1;
                       Done := TRUE;
                       MoreEval := FALSE;
                    END;
               13 : BEGIN
                       MoreEval := FALSE;
                       Done := TRUE;
                       ChoiceRes := 0;
                    END;
               ELSE BEGIN
                   ChoiceRes := 2;
                   Done := FALSE;
                   MoreEval:= FALSE;
               END;
            END;
            IF (ChoiceRes = 1) THEN BEGIN
               IF LieferDB.BeginTransaction THEN BEGIN
                  IF NOT LieferDB.GetIntNext(Lieferung) THEN BEGIN
                     ChoiceRes := -1;
                     Done:= TRUE;
                     MoreEval := FALSE;
                  END;
                  IF NOT LieferDB.EndTransaction THEN
                     FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                                LieferDB.GetTransactionErrMsg);
               END ELSE BEGIN
                  FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                                  LieferDB.GetTransactionErrMsg);
               END;
            END;
         UNTIL Done;
         AlteLieferungInput := EndCode;
      END;

   PROCEDURE AlteLieferungAendern;
      VAR LHelp : TLiefDAten;
      BEGIN
         KLfd := Lieferung.Knr;
         ShowKundeninfo;
         KlfdNo := Kunde.LfdNr;
         NeuerLieferschein := FALSE;
         ZeitUBis := '';
         ZeitUVon := '';
         ZeitUeberprueft := FALSE;
         BUTag := 0;
         BUTAbs := 0;
         BUBer := 0;
         BerUeberprueft := FALSE;
         DispTagString(Lieferung.Liefertag);
         IF LieferDB.BeginTransaction THEN BEGIN
            IF LieferDB.GetRec(1,Lieferung.LiefNr,LHelp) THEN
               SubFromKSum(LHelp);
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
               LieferDB.GetTransactionErrMsg);

         END ELSE BEGIN
            FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
              LieferDB.GetTransactionErrMsg);
         END;
         LFenster1^.Save('');
         LFenster2^.Save('');
         LFenster1^.ClearConst('35');
         LFenster2^.ClearConst('35');
         LFenster1^.SetFocusOnId('30');
         LSAendern;
         IF Lieferung.T100Bearbeitet THEN
            InfoBox.Act(2,'Achtung, es wurden schon Daten '+
                          'in Tourbegleitzettel eingetragen');
      END;

   FUNCTION AlteLieferung(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LSave : TLiefDaten;
          SeekResult : BOOLEAN;
          ChoiceRes : INTEGER;
          NLScheinSave : BOOLEAN;
          FocusK: STRING;
          Lauf : BYTE;
          EndCode : INTEGER;
          LPosEdSave   : ARRAY[1..30] OF BOOLEAN;
          KSave : TKundenDaten;
          ZeitDSave,
          StdBestDsave,
          OldDay: BOOLEAN;
          DOSearch: BOOLEAN;
          LdatSave : TDatum;
          LUhrSave : TZeit;
          InpSTringSave,
          AFahrerSave,
          aktKNRIStart,
          aktKNrIEnd : STRING;
      BEGIN
         MoreEval := FALSE;
         DoSearch := TRUE;
         HideKundenUebersicht;
         InpStringSave := Inpstring;
         Focusk := LFenster1^.GetFocusID;
         IF NeuerLieferschein THEN BEGIN
            LSave := Lieferung;
            LDatSave:= LDatum;
            LUhrSave:= LUhrzeit;
            AFahrerSave := AktFahrer;
            FOR Lauf := 1 TO 30 DO BEGIN
               LPosEdSave[Lauf]:= LiefPosEd[Lauf];
               LiefPosEd[Lauf] := TRUE;
            END;
            NLScheinSave := NeuerLieferschein;
         END;
         IF (NeuerLieferschein) OR NOT (LFenster1^.Changed('') OR LFenster2^.Changed('')) OR
            Request.Act('Wollen Sie die Eingaben verwerfen ?') THEN BEGIN
            IF (NOT NeuerLieferschein) THEN BEGIN
                  LFenster1^.Restore('');
                  LFenster2^.Restore('');
                  OldDay := IsOldDay(Lieferung.Liefertag);
                  Mask2Lie(Lieferung,OldDay);
                  AddToKSum(Lieferung)
            END;
            aktKNRIStart:=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),'20.12.'+LeadingZeros(CJahrhundertwechsel,2),999);
            aktKNRIEnd  :=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),'01.01.'+LeadingZeros(CJahrhundertwechsel+1,2),000);
            ChoiceRes := -1;
            IF LieferDB.BeginTransaction THEN BEGIN
               SeekResult:=LieferDB.StartIntervall(2,
                           aktKnrIStart,aktKnrIEnd,Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END;
            ChoiceRes:= -1;
            MoreEval := FALSE;
            IF SeekResult THEN BEGIN
               KSave := Kunde;
               ZeitDSave := ZeitIstDefault;
               StdBestDSave := StdBestIstDefault;
               LFenster1^.ClearDefault('31');
               LFenster1^.ClearDefault('32');
               FOR Lauf := 1 TO 5 DO
                  LFenster1^.ClearDefault(LeadingZeros(Lauf,2)+'_');
               EndCode := AlteLieferungInput(ChoiceRes,MoreEval);
               IF (ChoiceRes = 0) AND (EndCode <> -65) THEN BEGIN
                  AlteLieferungAendern;
                  InpStringSave := Lieferung.Liefertag;
               END;
               IF (ChoiceRes = -1) THEN BEGIN
                  IF ZeitDSave THEN BEGIN
                     LFenster1^.SetDefault('31');
                     LFenster1^.SetDefault('32');
                  END;
                  IF StdBestDSave THEN BEGIN
                     FOR Lauf := 1 TO AnzStdPos DO
                         LFenster1^.SetDefault('0'+B2S(Lauf,0)+'_');
                  END;
                  Kunde:= KSave;
                  ShowKundenInfo;
               END;
            END ELSE
               FaultBox.Act(2,'Keinen Lieferschein gefunden.');
            IF (ChoiceRes = -1) AND (NeuerLieferschein) THEN BEGIN
               NeuerLieferschein := NLScheinSave;
               Lieferung:= LSave;
               FOR Lauf := 1 TO 30 DO
                  LiefPosEd[Lauf]:= LPosEdSave[Lauf];
               NeuerLieferschein := NLScheinSave;
               AktFahrer := AFahrerSave;
               Ldatum:= LDatSave;
               LUhrzeit:= LUhrSave;
               BerechnePreis;
               GetLiefStatus;
               KLfd := Lieferung.Knr;
               KundDb.BeginTransaction;
               IF NOT KunddB.GetRec(1,Lieferung.Kundennummer,Kunde) THEN
                  KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
               KundDb.EndTransaction;
               KlfdNo := Kunde.LfdNr;
               DispTagString(Lieferung.Liefertag);
               ShowKundenInfo;
               LFenster1^.SetFocusOnID(FocusK);
            END;
            LFenster1^.Refresh('');
            IF (ChoiceRes = 0) AND (EndCode = -65) THEN
                  LSFeldAktiv;
         END;
         LFenster1Action;
         IF (NOT MoreEval) THEN
            Taste := 0
         ELSE
            Taste := EndCode;
         InpString := InpStringSave;
         Abbruch := FALSE;
         Check:= FALSE;
         AlteLieferung := TRUE;
      END;

   FUNCTION AlteLieferungKNr(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LSave : TLiefDaten;
          SeekResult : BOOLEAN;
          ChoiceRes : INTEGER;
          NLScheinSave : BOOLEAN;
          FocusK: STRING;
          Lauf : BYTE;
          LPosEdSave   : ARRAY[1..30] OF BOOLEAN;
          KSave : TKundenDaten;
          EndCode : INTEGER;
          ZeitDSave,
          StdBestDsave:BOOLEAN;
          DOSearch: BOOLEAN;
          LDatSave :TDatum;
          LUhrSave : TZeit;
          AFahrerSave,
          InpSTringSave,
          aktKNRIStart,
          aktKNrIEnd : STRING;
      BEGIN
         MoreEval := FALSE;
         DoSearch := TRUE;
         HideKundenUebersicht;
         InpStringSave := Inpstring;
         IF ((RemoveAllSpaces(InpString) <> '') AND IsNumber(InpString)) THEN BEGIN
            Kunde.LfdNr := S2L(InpString);
         END ELSE
            DoSearch := FALSE;
         IF DOSearch THEN BEGIN
            LSave := Lieferung;
            FOR Lauf := 1 TO 30 DO BEGIN
                LPosEdSave[Lauf]:= LiefPosEd[Lauf];
                LiefPosEd[Lauf] := TRUE;
            END;
            NLScheinSave := NeuerLieferschein;
            AFahrerSave := AktFahrer;
            LDatSave :=LDatum;
            LUhrSave := LUhrZeit;
            aktKNRIStart:=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),'20.12.'+LeadingZeros(CJahrhundertwechsel,2),999);
            aktKNRIEnd  :=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),'01.01.'+LeadingZeros(CJahrhundertwechsel+1,2),000);
            IF LieferDB.BeginTransaction THEN BEGIN
               SeekResult:=LieferDB.StartIntervall(2,
                           aktKnrIStart,aktKnrIEnd,Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
               END;
            ChoiceRes:= -1;
            MoreEval := FALSE;
            IF SeekResult THEN BEGIN
               LsAendern;
               KSave := Kunde;
               ZeitDSave := ZeitIstDefault;
               StdBestDSave := StdBestIstDefault;
               LFenster1^.ClearDefault('31');
               LFenster1^.ClearDefault('32');
               FOR Lauf := 1 TO 5 DO
                   LFenster1^.ClearDefault(LeadingZeros(Lauf,2)+'_');
               EndCode := AlteLieferungInput(ChoiceRes,MoreEval);
               IF (ChoiceRes = 0) AND (EndCode <> -65)THEN BEGIN
                  AlteLieferungAendern;
                  InpStringSave:= Lieferung.Liefertag;
              END;
            END ELSE
               FaultBox.Act(2,'Keinen Lieferschein gefunden.');
            IF (ChoiceRes = -1) THEN BEGIN
               NeuerLieferschein := NLScheinSave;
               Lieferung:= LSave;
               FOR Lauf := 1 TO 30 DO
                   LiefPosEd[Lauf]:= LPosEdSave[Lauf];
               NeuerLieferschein := NLScheinSave;
               BerechnePreis;
               GetLiefStatus;
               KLfd := Lieferung.Knr;
               KlfdNo := Kunde.LfdNr;
               AktFahrer := AFahrerSave;
               LDatum := LDatSave;
               LUhrZeit := LUhrSave;
               DispTagString(Lieferung.Liefertag);
               ClearKundenInfo;
               Lieferung.Knr := InpStringsave;
               KundenFeldAktiv;
            END;
            LFenster1^.Refresh('');
            IF (ChoiceRes = 0) THEN BEGIN
               IF (EndCode = -65) THEN BEGIN
                  LSFeldAktiv;
               END ELSE
                  LSAendern;
            END;
            LFenster1Action;
         END;
         IF (NOT MoreEval) THEN
            Taste := 0
         ELSE
            Taste := EndCode;
         InpString := InpStringSave;
         Abbruch := FALSE;
         Check:= FALSE;
         AlteLieferungKnr := TRUE;
      END;

   FUNCTION LSPlusMinus(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR SeekResult: BOOLEAN;
          SK : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         IF (RemoveAllSpaces(InpString) = '') THEN BEGIN
            IF LieferDB.BeginTransaction THEN BEGIN
               InpString:=LieferDB.GetLfdLNr;
               SeekResult := LieferDB.GetRec(1,InpString,Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                            LieferDB.GetTransactionErrMsg);
            END;
         END ELSE BEGIN
            IF Taste = 43 THEN BEGIN
               InpString := LeadingZeros(S2l(InpString)+1,5);
            END ELSE BEGIN
               InpString := LeadingZeros(S2l(InpString)-1,5);
            END;
            IF LieferDB.BeginTransaction THEN BEGIN
               SeekResult := LieferDB.GetRec(1,InpString,Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                            LieferDB.GetTransactionErrMsg);
            END;
         END;
         IF NOT SeekResult THEN BEGIN
            InitTLiefDaten(Lieferung);
            Lieferung.LiefNr := InpString;
            WRITE(Chr(7));
         END;
         GetLiefStatus;
         ZeigeLieferscheinZuLSNummer;
         Check := FALSE;
         Abbruch := FALSE;
         LSPlusMinus := FALSE;
      END;

   FUNCTION CRLieferschein(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LSave : TLiefDaten;
          LNrSave : STRING;
          SeekResult : BOOLEAN;
          ChoiceRes : INTEGER;
          NLScheinSave : BOOLEAN;
          Lauf : BYTE;
          Ergebnis : BOOLEAN;
          LPosEdSave   : ARRAY[1..30] OF BOOLEAN;
          LNr : LONGINT;
          aktKNRIStart,
          aktKNrIEnd : STRING;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
         Ergebnis:= FALSE;
         IF (RemoveAllSpaces(InpString) ='') THEN BEGIN
            Ergebnis := ZuKundenFeld(Taste,Nr,InpString,Abbruch,Check,MoreEval);
         END ELSE BEGIN
            IF LieferDB.BeginTransaction THEN BEGIN
               LNr := S2l(InpString);
               SeekResult:=LieferDB.GetRec(1,LeadingZeros(LNr,5),Lieferung);
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                            LieferDB.GetTransactionErrMsg);
            END;
            IF SeekResult THEN BEGIN
               SubFromKSum(Lieferung);
               Lie2Mask(Lieferung);
               BerechnePreis;
               GetLiefStatus;
               InpString := LeadingZeros(Lnr,5);
               LDatum := COPY(Lieferung.Bemerkungen[3],1,8);
               LUhrzeit:= COPY(Lieferung.Bemerkungen[3],9,5);
               AktFahrer := Lieferung.Fahrer;
               DispTagString(Lieferung.Liefertag);
               LFenster1^.Refresh('');
               KLfd := Lieferung.Knr;
               KundDb.BeginTransaction;
               IF NOT KunddB.GetRec(1,Lieferung.Kundennummer,Kunde) THEN
                  KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
               KundDb.EndTransaction;
               ShowKundeninfo;
               KlfdNo := Kunde.LfdNr;
               NeuerLieferschein := FALSE;
               DispTagString(Lieferung.Liefertag);
               LFenster1^.Save('');
               LFenster2^.Save('');
               LFenster1^.ClearConst('35');
               LFenster2^.ClearConst('35');
               LSAendern;
               IF Lieferung.T100Bearbeitet THEN
                  InfoBox.Act(2,'Achtung, es wurden schon Daten '+
                                'in Tourbegleitzettel eingetragen');
               Ergebnis:= TRUE;
            END ELSE BEGIN
               FaultBox.Act(2,'Lieferschein existiert nicht');
               Ergebnis:= FALSE;
               LNrSave:= InpString;
               InitTLiefDaten(Lieferung);
               Lieferung.LiefNr := InpString;
            END;
            LFenster1^.Refresh('');
            Abbruch := FALSE;
            Check:= FALSE;
         END;
         CRLieferschein := Ergebnis;
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

   FUNCTION ZeitSpeichern(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
       BEGIN
          MoreEval := FALSE;
          Abbruch:= FALSE;
          Check:= TRUE;
          Kunde.StdVon := Lieferung.Von[1];
          Kunde.StdBis := Lieferung.Bis[1];
          KundDb.BeginTransaction;
          KunDDb.ChangeRec(Kunde);
          KundDb.EndTransaction;
          ZeitIstDefault := TRUE;
          LFenster1^.SetDefault('31');
          LFenster1^.SetDefault('32');
          LFenster1^.Refresh('31');
          LFenster1^.Refresh('32');
          ZeitSpeichern := FALSE;
       END;

   FUNCTION StdBestSpeichern(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                             VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR ULauf,
          DLauf,
          Lauf : BYTE;
          UebertragenEnde,
          MehrAlsStd: BOOLEAN;
          OPFound : BOOLEAN;
          ULaufMax : BYTE;
          PosSik : TLiefPos;
          LiefEdSik :BOOLEAN;
          PipSik : REAL;
          ErsteLeerePos : BYTE;

      BEGIN
         MoreEval := FALSE;
         MehrAlsStd := FALSE;
         FOR Lauf := 6 TO 19 DO BEGIN
             WITH Lieferung.Positionen[Lauf] DO
                  MehrAlsStd:= MehrAlsStd OR (Menge <> 0) OR
                               (ArtNr <> '') OR (ArtBez <> '') OR (Preis <> 0);

         END;
         IF (NOT MehrAlsStd) OR Request.Act('Es wurden mehr als 5 Positionen eingegeben. '+
            'Wollen Sie die ersten 5 Positionen als '+
            'Standardbestellung speichern ?') THEN BEGIN
            FOR Lauf := 1 TO 5 DO BEGIN
               WITH Kunde.StdBest[Lauf] DO BEGIN
                  Menge := 0;
                  ArtNr := '';
                  ArtBez := '';
                  Preis := 0;
                  Uebernehmen := FALSE;
               END;
            END;
            ULauf := 1;
            ULaufMax := 5;
            UebertragenEnde := TRUE;
            OPFound := FALSE;
            Lauf := 1;
            ErsteLeerePos:= 6;
            REPEAT
               WITH Lieferung.Positionen[Lauf] DO BEGIN
                  UebertragenEnde := (Menge = 0) AND (ArtNr = '') AND
                                     (ArtBez = '') AND (Preis = 0);
                  IF NOT UebertragenEnde THEN BEGIN
                     IF (ArtNr <> '903') AND (ArtNr <> '902') THEN BEGIN
                        Kunde.StdBest[Ulauf].Menge := Menge;
                        Kunde.StdBest[Ulauf].ArtNr := ArtNr;
                        Kunde.StdBest[Ulauf].ArtBez := ArtBez;
                        Kunde.StdBest[Ulauf].Preis := Preis;
                        Kunde.StdBest[Ulauf].Uebernehmen := LiefPosEd[lauf];
                        INC(Ulauf);
                     END ELSE BEGIN
                        DEC(ULaufMax);
                        OPFound := TRUE;
                     END;
                  END ELSE BEGIN
                     ErsteLeerePos := Lauf;
                  END;
                  INC(Lauf);
               END;
            UNTIL UebertragenEnde OR (Lauf>5);
            IF OPFound AND (ErsteLeerePos >2)THEN BEGIN
               FOR Lauf := 1 TO ErsteLeerePos-1 DO BEGIN
                  IF (Lieferung.Positionen[Lauf].ArtNr = '903') OR
                     (Lieferung.Positionen[Lauf].ArtNr = '902') THEN BEGIN
                     IF (Lauf < ErsteLeerePos-1) THEN BEGIN
                        PosSik := Lieferung.Positionen[Lauf];
                        LiefEdSik := LiefPosEd[Lauf];
                        PipSik := Pip[Lauf];
                        FOR DLauf := Lauf TO ErsteLeerePos-2 DO BEGIN
                          Lieferung.Positionen[DLauf] := Lieferung.Positionen[DLauf+1];
                          LiefPosEd[DLauf] := LiefPosEd[DLauf+1];
                          Pip[DLauf]:=Pip[Dlauf+1];
                        END;
                        Lieferung.Positionen[ErsteLeerePos-1]:= PosSik;
                        LiefPosEd[ErsteLeerePos-1]:= LiefEdSik;
                        Pip[ErsteLeerePos-1]:= PipSik;
                     END;
                  END;
               END;
            END;
            DEC(ULauf);
            KundDb.BeginTransaction;
            KunDDb.ChangeRec(Kunde);
            KundDb.EndTransaction;
            AnzStdPos :=  Ulauf;
            FOR Lauf:= 1 TO 5 DO BEGIN
               LFenster1^.ClearDefault('0'+B2S(Lauf,0)+'_');
            END;
            IF (AnzStdPos > 0) THEN BEGIN
               StdBestIstDefault := TRUE;
               FOR Lauf := 1 TO AnzStdPos DO BEGIN
                  LFenster1^.SetDefault('0'+B2S(Lauf,0)+'_');
               END;
            END ELSE
               StdBestIstDefault := FALSE;
            LFenster1^.Refresh('');
         END;
         Abbruch := FALSE;
         Check:= TRUE;
         StdBestSpeichern := FALSE;
      END;



   FUNCTION LFenster1ArtBezAction:BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          ArtBezGet,
          ArtBEzSik : STRING;
          NP  : REAL;
          NpPf:REAL;
          NPMark: LONGINT;
          AktArtNr : LONGINT;
          PfandSik : REAL;

      BEGIN
         FocusID := LFenster1^.GetFocusID;
         SplitString(FocusID,'_',FocusS,FocusR);
         FocusFK := S2B(FocusS);
         WITH Lieferung.Positionen[FocusFK] DO BEGIN
            IF (ArtNr ='990') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 301) AND (AktArtNr <= 495) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := '10 Fl. '+ ArtBez;
                     ArtNr := '990';
                     NP := (Preis -2.49)/2+2.49;
                     NpMark:=TRUNC(NP);
                     NpPf :=TRUNC((Np-NpMark)*100)/100;
                     IF (NpPf <=0.48) THEN
                        NpPf :=0.48
                     ELSE
                        NpPf :=0.98;
                     Preis := NPMark+NpPf;
                     Pfand := PfandSik;

                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='971') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 301) AND (AktArtNr <= 495) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '971';
                     Preis := TRUNC((Preis/15+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='972') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 001) AND (AktArtNr <= 022) OR
                  (AktArtNr >= 026) AND (AktArtNr <= 027) OR
                  (AktArtNr >= 031) AND (AktArtNr <= 099) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '972';
                     Preis := TRUNC((Preis/9+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='973') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 069) AND (AktArtNr <= 069) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '973';
                     Preis := TRUNC((Preis/4+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='974') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 201) AND (AktArtNr <= 210) OR
                  (AktArtNr >= 023) AND (AktArtNr <= 030) AND NOT(AktArtNr = 026) AND NOT (AktArtNr = 027) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '974';
                     Preis := TRUNC((Preis/9+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;

            IF (ArtNr ='997') AND (ArtBez <> '') AND ISNumber(ArtBez) THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr < 100) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := '6 Fl. '+ ArtBez;
                     ArtNr := '997';
                     NP := (Preis -2.49)/2+2.49;
                     NpMark:=TRUNC(NP);
                     NpPf :=TRUNC((Np-NpMark)*100)/100;
                     IF (NpPf <=0.48) THEN
                        NpPf :=0.48
                     ELSE
                        NpPf :=0.98;
                     Preis := NPMark+NpPf;
                     Pfand := PfandSik;
                     LFenster1^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster1^.Refresh('10');
                        LFenster1^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBEzSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster1^.Refresh(LeadingZeros(FocusFk,2));
               END;
            END;
         END;
         LFenster1ArtBezAction := LFenster1Action;
      END;

   FUNCTION LFenster2Action:BOOLEAN;FAR;
      VAR FocusFK : BYTE;
          FocusId :STRING;
          NewFlag : BOOLEAN;
          Lauf   : BYTE;
          FocusS,FocusR  : STRING;
      BEGIN
         IF BerechnePreis THEN BEGIN
            LFenster2^.Refresh('20');
         END;
         LFenster2Action := FALSE;
      END;

   FUNCTION LFenster2ArtBezAction:BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          AktArtNr : LONGINT;
          ArtBezGet,
          ArtBEzSik : STRING;
          NP  : REAL;
          NpPf:REAL;
          PfandSik : REAL;
          NPMark: LONGINT;

      BEGIN
         FocusID := LFenster2^.GetFocusID;
         SplitString(FocusID,'_',FocusS,FocusR);
         FocusFK := S2B(FocusS);
         WITH Lieferung.Positionen[FocusFK] DO BEGIN
            IF (ArtNr ='990') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 301) AND (AktArtNr <= 495) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := '10 Fl. '+ ArtBez;
                     ArtNr := '990';
                     NP := (Preis -2.49)/2+2.49;
                     NpMark:=TRUNC(NP);
                     NpPf :=TRUNC((Np-NpMark)*100)/100;
                     IF (NpPf <=0.48) THEN
                        NpPf :=0.48
                     ELSE
                        NpPf :=0.98;
                     Preis := NPMark+NpPf;
                     Pfand := PfandSik;
                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='971') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 301) AND (AktArtNr <= 495) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '971';
                     Preis := TRUNC((Preis/15+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='972') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 001) AND (AktArtNr <= 022) OR
                  (AktArtNr >= 026) AND (AktArtNr <= 027) OR
                  (AktArtNr >= 031) AND (AktArtNr <= 099) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '972';
                     Preis := TRUNC((Preis/9+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='973') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 069) AND (AktArtNr <= 069) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '973';
                     Preis := TRUNC((Preis/4+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;
            IF (ArtNr ='974') AND (ArtBez <> '')THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 201) AND (AktArtNr <= 210) OR
                  (AktArtNr >= 023) AND (AktArtNr <= 030) AND NOT(AktArtNr = 026) AND NOT (AktArtNr = 027) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := 'Flasche(n) '+ ArtBez;
                     ArtNr := '974';
                     Preis := TRUNC((Preis/9+0.05)*10)/10;
                     Pfand  := PfandSik;

                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;

            IF (ArtNr ='997') AND (ArtBez <> '') AND ISNumber(ArtBez) THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr < 100) THEN BEGIN
                  PfandSik := Pfand;
                  IF SeekArt(ArtBezGet,FocusFK) THEN BEGIN
                     ArtBez := '6 Fl. '+ ArtBez;
                     ArtNr := '997';
                     NP := (Preis -2.49)/2+2.49;
                     NpMark:=TRUNC(NP);
                     NpPf :=TRUNC((Np-NpMark)*100)/100;
                     IF (NpPf <=0.48) THEN
                        NpPf :=0.48
                     ELSE
                        NpPf :=0.98;
                     Preis := NPMark+NpPf;
                     Pfand := PfandSik;
                     LFenster2^.Refresh(LeadingZeros(FocusFk,2));
                     IF BerechnePreis THEN BEGIN
                        LFenster2^.Refresh('10');
                        LFenster2^.Refresh('21');
                     END;
                  END ELSE
                     ArtBez  := ArtBEzSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  LFenster2^.Refresh(LeadingZeros(FocusFk,2));
               END
            END;

         END;
         LFenster2ArtBezAction := LFenster2Action;
      END;

   FUNCTION RechnerAufrufen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         StartCalculator(GesaSum,Lieferung);
         Abbruch :=FALSE;
         Check := FALSE;
         RechnerAufrufen := FALSE;
      END;

   FUNCTION KundeBearbeiten(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;

      VAR   EndCode : INTEGER;
            Ergebnis: INTEGER;
            DOAender : BOOLEAN;
            DONeu : BOOLEAN;
            HighestKNr:LONGINT;
            L : LONGINT;
            C : INTEGER;
      BEGIN
         MoreEval := FALSE;
         DOAender := FALSE;
         IF (RemoveAllSpaces(InpString) <> '') THEN BEGIN
            KundDb.BeginTransaction;
            DOAender := KundDb.GetRec(1,MakeLfdNr(S2l(InpString),0),Kunde);
            KundDb.EndTransaction;
         END;
         IF DOAender THEN BEGIN
            DOKAender;
            Klfd:=LeadingZeros(Kunde.LfdNr,5);
            KlfdNo := Kunde.LfdNr;
            InpString := Klfd;
            Lieferung.Knr := Klfd;
            Taste := 0;
         END ELSE BEGIN
            DONeu :=(RemoveAllSpaces(InpString) = '');
            IF NOT DONeu THEN BEGIN
               KundDb.BeginTransaction;
               HighestKnr := KundDb.GetLfdNr;
               KundDb.EndTransaction;
               IF (HighestKnr < S2l(InpString)) THEN BEGIN
                  FaultBox.Act(2,'Kundennummer ist zu hoch, bitte normal neuanlegen');
               END ELSE BEGIN
                  DONeu := Request.Act('Kundennummer existiert nicht. Neuanlegen?');
               END;
            END;
            IF DoNeu THEN BEGIN
               KlfdNo := S2L(InpString);
               Klfd := LeadingZeros(KLfdNo,5);
               DOKNeu(Klfd,KLfdNo);
               InpString:= KLfd;
               Lieferung.Knr := Klfd;
               Taste := 0;
            END;
         END;
         Abbruch     := FALSE;
         Check:= FALSE;
         KundeBearbeiten  := TRUE;
      END;

   FUNCTION LZeitRaumFestlegen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR AktKey :CHAR;
          LTabs : BYTE;
          AktTagAbs,
          AktTNr : BYTE;
          FocusId :STRING;
          Ergebnis : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         FocusId :=LFenster1^.GetFocusId;
         Ergebnis := FALSE;
         IF (NOT Kunde.LiefSZeit) THEN BEGIN
            SplitIFieldTNr(Lieferung.TourNr,AktTagAbs,AktTnr);
            AktKey := UPCASE(CHR(Taste));
            CASE AktKey OF
              'V' : LTabs := 1;
              'N' : LTabs := 2;
              'A' : LTabs := 3;
           END;
           Lieferung.Von[1] := VonZeit[LTabs];
           Lieferung.Bis[1] := BisZeit[LTabs];
           LFenster1^.Refresh('31');
           LFenster1^.Refresh('32');
           IF (LTabs <> AktTagAbs) THEN BEGIN
              Lieferung.TourNr := MakeIFieldVal(LTabs,1);
              LFenster1^.Refresh('7');
           END;
           LFenster1^.SetFocusOnId('01_1');
           IF (FocusId = '31') THEN
              InpString:= Lieferung.Von[1]
           ELSE
              InpString:= Lieferung.Bis[1];
           Ergebnis := TRUE;

         END ELSE
           FaultBox.Act(2,'Kunde ist Innenstadtkunde. �ndern des Lieferzeitraumes '+
                          ' nur �ber normale Eingabe m�glich');
         Abbruch := FALSE;
         Taste := 0;
         Check := FALSE;
         LZeitRaumFestLegen := Ergebnis;
      END;

   PROCEDURE InitLieferInput;
      VAR Lauf :BYTE;

      BEGIN
         InitCalcInput;
         NEW(LBestaetigen);
         LBestaetigen^.Init(200,360,460,440,0,'L�schen best�tigen','ASIN3',Aktiv);
         WITH LBestaetigen^ DO BEGIN
            AddCLong('0',6,1,RNummer,3);
            AddConst('0',0,1,'Bitte');
            AddConst('0',10,1,'zum L�schen eingeben');
            AddLongInt('','01','Best�tigung:',1,2,BNummer,'',3,0,999);
         END;

         NEW(LFenster1);
         LFenster1^.Init(15,5,625,210,0,'','ASIN3',Aktiv);
         WITH Lieferung DO BEGIN
            WITH LFenster1^ DO BEGIN
               AddNumText   ('','101','Kunde:', 1,0,KNr,'',5);
               SetActionFunc('101',SucheHandyFile);
               AddNumText   ('','102','LS:',   20,0,LiefNr,'',5);

               AddDate      ('','10','Annahme:',48,0,LDatum,'');
               AddTime      ('','10','',        66,0,LUhrzeit,'');
               AddHLine     ('0',0,73,1,0);
               AddDate      ('','30','Liefertag :',1,2,Liefertag,'');
               AddTime      ('','31','Von', 1,3,Von[1],'');
               AddTime      ('','32','bis',12,3,Bis[1],'');
               AddRoll      ('','7','Tour:',25,2,TourNr,'',14);
               FOR Lauf := 1 TO TourAbs*TAbsT DO
                  AddItem   ( '7',MakeIFieldTStr(Lauf));
               FOR Lauf := 1 TO TourAbs+2 DO
                  AddItem   ( '7',MakeTString(Lauf,0));
               FOR Lauf := 1 TO TourAbs+1 DO
                  DisableItem('7',Lauf+TourAbs*TAbsT);
               AddLongint   ('','16','/',46,2,TourLfdNr,'',2,0,99);
               AddFahrerFeld('','15','',54,2,AktFahrer,'',30,FsuchFenster,NeuerFahrer,AendereFahrer);
               AddReal      ('','10','',55,3,VollSum,'',7,2,-999.99,9999.99);
               AddReal      ('','10','',64,3,GesaSum,'',7,2,-999.99,9999.99);
               AddHLine  ('0',0,73,4,0);
               AddConst('0',1,5,'Menge Art-Nr. Bezeichnung                                Preis    incl.');
               FOR Lauf := 1 TO 5 DO BEGIN
                  AddTabZeile('','0'+B2S(Lauf,0),'',1,Lauf+5,LiefPosED[Lauf],'');
                  AddLongInt('0'+B2S(Lauf,0),'1','',1, Lauf+5,Positionen[Lauf].Menge,'',3,0,999);
                  AddArtNum('0'+B2S(Lauf,0),'2','',6, Lauf+5,Positionen[Lauf].ArtNr,'',3,SeekArtLs1,Lauf);
                  AddString('0'+B2S(Lauf,0),'3','',14,Lauf+5,Positionen[Lauf].ArtBez,'',40);
                  AddReal('0'+B2S(Lauf,0),'4','',55,Lauf+5,Positionen[Lauf].Preis,'',7,2,
                                 -999.99,999.99);
                  AddReal('0'+B2S(Lauf,0),'21','',64,Lauf+5,PiP[Lauf],'',7,2,-999.99,999.99)
               END;
               AddTabZeile('','06','',1,11,LiefPosEd[6],'');
               AddLongInt('06','1','',1,11,Positionen[6].Menge,'',3,0,999);
               AddArtNum('06','2','',6,11,Positionen[6].ArtNr,'',3,SeekArtLs1,6);
               AddString('06','3','',14,11,Positionen[6].ArtBez,'',40);
               AddReal('06','4','',55,11,Positionen[6].Preis,'',7,2,
                             -999.99,999.99);
               AddReal('06','21','',64,11,PiP[6],'',7,2,-999.99,999.99);
               AddString    ('','33','Status:', 35,0,LiefStatus,'',1);
                        Disable('33');

               AddActionKey('06_4',13,ZweiteSeite,TRUE,'');
               AddActionKey('06',-80,ZweiteSeite,TRUE,'');
               AddActionKey('06_4',-80,ZweiteSeite,TRUE,'');
               AddActionKey('06_4',9,ZweiteSeite,TRUE,'');
               FOR Lauf := 1 TO 6 DO BEGIN
                  AddActionKey('0'+B2S(Lauf,0)+'_',-62,PositionLoeschen,FALSE,'Aktuelle Position l�schen');
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-72,UpToSTab,FALSE,'');
                  SetToggle('0'+B2S(Lauf,0),TRUE);
               END;
               SetActionFunc('_',LFenster1Action);
               Disable('10');
               Disable('21');
               IF (PrgLaden) THEN BEGIN
                 Disable('30');
                 Disable('31');
                 Disable('32');
               END;

               FOR Lauf := 1 TO 6 DO BEGIN
                  SetActionFunc('0'+B2S(Lauf,0),LFenster1Action);
                  SetActionFunc('0'+B2s(Lauf,0),LFenster1ArtBezAction);
                  AddActionKey('0'+B2S(Lauf,0),-64,StdBestSpeichern,TRUE,'als Standardbestellung speichern');
               END;
               SetActionFunc('06',LFenster1Action);
               AddCString   ('35',24,3,TagAnzeige);
               SetCColor    ('35',8,6);
               SetCHeight   ('35',3);
               SetFKeyQuit(FALSE);
               SetCRQuit(FALSE);
               AddActionKey('_',-61,F3ZweiteSeite,FALSE,'auf die zweite Seite wechseln');

               AddActionKey ('',-73,LFenster1Focus1,TRUE,'zur�ck an Anfang des Lieferscheins');
               AddActionKey ('30',43,NaechsterTag,FALSE,'N�chster Liefertag');
               AddActionKey ('30',45,VorherigerTag,FALSE,'Vorheriger Liefertag');
               AddActionKey ('',42,AlteLieferung,FALSE,'Alte Lieferscheine suchen');
               AddActionKey ('30',-60,AndereAnschrift,FALSE,'Lieferanschrift �ndern');
               AddActionKey ('31',-64,Zeitspeichern,TRUE,'als Standardbestellzeit speichern');
               AddActionKey ('31',-62,Zeitloeschen,FALSE,'Lieferzeitraum l�schen');
               AddActionKey ('31',86,LZeitraumfestlegen,FALSE,'Lieferzeitraum Vormittag');
               AddActionKey ('31',118,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('31',78,LZeitraumfestlegen,FALSE,'Lieferzeitraum Nachmittag');
               AddActionKey ('31',110,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('31',65,LZeitraumfestlegen,FALSE,'Lieferzeitraum Abend');
               AddActionKey ('31',97,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('32',-64,Zeitspeichern,TRUE,'als Standardbestellzeit speichern');
               AddActionKey ('32',-62,Zeitloeschen,FALSE,'Lieferzeitraum l�schen');
               AddActionKey ('32',86,LZeitraumfestlegen,FALSE,'Lieferzeitraum Vormittag');
               AddActionKey ('32',118,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('32',78,LZeitraumfestlegen,FALSE,'Lieferzeitraum Nachmittag');
               AddActionKey ('32',110,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('32',65,LZeitraumfestlegen,FALSE,'Lieferzeitraum Abend');
               AddActionKey ('32',97,LZeitraumfestlegen,FALSE,'');
               AddActionKey ('',27,EsczuKundenFeld,FALSE,'ins Kundenfeld wechseln');
               AddActionKey ('',-68,F10BestSpeichern,TRUE,'Bestellung speichern');
               AddActionKey ('_',-65,WechselZuLSNr,FALSE,'Wechsel in Liefernummer');
               AddActionKey ('',-19,RechnerAufrufen,FALSE,'Rechner aufrufen');

               AddActionKey('101',27,ProgrammEnde,FALSE,'Programm beenden');
               AddActionKey('101',8,LoescheFeld,FALSE,'Feld l�schen');
               AddActionKey('101',-73,DoNothing,FALSE,'');
               AddActionKey('101',42,AlteLieferungKnr,FALSE,'');
               AddActionKey('101',-19,DoNothing,FALSE,'');
               AddActionKey('101',-65,F7zuLiefNr,FALSE,'Wechsel in Liefernummer');
               AddActionKey('101',-68,DoNothing,FALSE,'');
               AddActionKey('101',-61,DoNothing,FALSE,'');
               AddActionKey('101',9,DoNothing,FALSE,'');
               AddActionKey('101',-80,DoNothing,FALSE,'');
               AddActionKey('101',-72,DoNothing,FALSE,'');
               AddActionKey('101',-15,DoNothing,FALSE,'');
               AddActionKey('101',13,CRNeueBestellung,FALSE,'Kunden suchen');
               AddActionKey('101',-64,KSuchen,FALSE,'Kunden suchen');
               AddActionKey('101',43,KundeBearbeiten,FALSE,'Kunde bearbeiten');

               AddActionKey('102',-68,DoNothing,FALSE,'');
               AddActionKey('102',-73,DoNothing,FALSE,'');
               AddActionKey('102',-19,DoNothing,FALSE,'');
               AddActionKey('102',-72,DoNothing,FALSE,'');
               AddActionKey('102',-80,DoNothing,FALSE,'');
               AddActionKey('102',-63,DoNothing,FALSE,'');
               AddActionKey('102',-61,DoNothing,FALSE,'');
               AddActionKey('102',9,DoNothing,FALSE,'');
               AddActionKey('102',-15,DoNothing,FALSE,'');
               AddActionKey('102',42,DoNothing,FALSE,'');
               AddActionKey('102',13,CrLieferschein,FALSE,'Lieferschein �ndern');
               AddActionKey('102',45,LSPlusMinus,FALSE,'Vorheriger Lieferschein');
               AddActionKey('102',43,LSPlusMinus,FALSE,'N�chster Lieferschein');
               AddActionKey('102',-62,LieferungLoeschen,FALSE,'L�schen des Lieferscheins');
               AddActionKey('102',27,ZuKundenFeld,FALSE,'Wechsel in Kundennummer');
               AddActionKey('102',-65,ZuKundenFeld,FALSE,'Wechsel in Kundennummer');
               AddActionKey('102',8,LoescheFeld,FALSE,'Feld l�schen');
               SetAInputFunc(LFenster1AInput);
            END;

            NEW(LFenster2);
            LFenster2^.Init(15,3,625,315,0,'Lieferschein bearbeiten (2)','ASIN3',Aktiv);
            WITH LFenster2^ DO BEGIN
               AddString ('','33','Kunde:',   1,0,KNr,'',5);
               AddNumText('','33','LS:',     20,0,LiefNr,'',5);
               AddString ('','33','Status:', 35,0,LiefStatus,'',1);
               AddDate   ('','33','Annahme:',48,0,LDatum,'');
               AddTime   ('','33','',66,0,LUhrzeit,'');
               AddHLine  ('1',0,73,1,0);
               AddDate   ('','33','Liefertag :',1,2,Liefertag,'');
               AddTime   ('','31','Von', 1,3,Von[1],'');
               AddTime   ('','32','bis',12,3,Bis[1],'');
               AddRoll   ('','34','Tour:',25,2,TourNr,'',13);
               FOR Lauf := 1 TO TourAbs*TabsT DO
                  AddItem('34',MakeIFieldTStr(Lauf));
               FOR Lauf := 1 TO TourAbs+2 DO
                  AddItem   ( '34',MakeTString(Lauf,0));
               FOR Lauf := 1 TO TourAbs+1 DO
                  DisableItem('34',Lauf+TourAbs*TAbsT);
               AddLongint('','33','/',46,2,TourLfdNr,'',2,0,99);
               AddFahrerFeld('','33','',54,2,AktFahrer,'',30,FsuchFenster,NeuerFahrer,AendereFahrer);
               AddReal      ('','20','',55,3,VollSum,'',7,2,-999.99,999.99);
               AddReal      ('','20','',64,3,GesaSum,'',7,2,-999.99,999.99);
               AddHLine  ('0',0,73,4,0);
               AddConst('0',1,5,'Menge Art-Nr. Bezeichnung                                Preis    incl.');
               FOR Lauf := 7 TO 19 DO BEGIN
                  AddTabZeile('','0'+B2S(Lauf,0),'',1,Lauf-1,LiefPosEd[Lauf],'');
                  AddLongInt('0'+B2S(Lauf,0),'1','',1, Lauf-1,Positionen[Lauf].Menge,'',3,0,999);
                  AddArtNum('0'+B2S(Lauf,0),'2','',6, Lauf-1,Positionen[Lauf].ArtNr,'',3,SeekArtLs2,Lauf);
                  AddString('0'+B2S(Lauf,0),'3','',14,Lauf-1,Positionen[Lauf].ArtBez,'',40);
                  AddReal('0'+B2S(Lauf,0),'4','',55,Lauf-1,Positionen[Lauf].Preis,'',
                                 7,2,-999.99,999.99);
                  AddReal('0'+B2S(Lauf,0),'21','',64,Lauf-1,PiP[Lauf],'',7,2,-999.99,999.99);
                  SetToggle('0'+B2S(Lauf,0),TRUE);
                  SetActionFunc('0'+B2S(Lauf,0),LFenster2ArtBezAction);
                  AddActionKey('0'+B2S(Lauf,0)+'_',-62,PositionS2Loeschen,FALSE,'Aktuelle Position l�schen');
               END;


               AddActionKey('',-65,S2WechselzuLsNr,FALSE,'Wechsel in Liefernummer');
               AddActionKey('07',-15,ErsteSeite,TRUE,'');
               AddActionKey('07_1',-15,ErsteSeite,TRUE,'');
               AddActionKey('07',-72,ErsteSeite,TRUE,'');
               AddActionKey('07_1',-72,ErsteSeite,TRUE,'');
               AddActionKey('019_4',13,ErsteSeite,TRUE,'');
               AddActionKey('019',-80,ErsteSeite,TRUE,'');
               AddActionKey('019_4',-80,ErsteSeite,TRUE,'');
               FOR Lauf := 7 TO 19 DO BEGIN
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-72,UpToSTab,FALSE,'');
               END;
               AddCString   ('35',22,3,TagAnzeige);
               SetCColor    ('35',8,6);
               SetCHeight   ('35',3);
               Disable('33');
               Disable('31');
               Disable('32');
               Disable('34');
               Disable('21');
               Disable('20');

               SetFKeyQuit(FALSE);
               SetCRQuit(FALSE);
               AddActionKey('',27,Seite2zuKFeld,FALSE,'ins Kundenfeld wechseln');
               ADDActionKey('',-61,ErsteSeite,TRUE,'auf die erste Seite');
               AddActionKey('_',-60,DoNothing,FALSE,'');
               AddActionKey('',-73,LFenster2Focus1,TRUE,'zur�ck an Anfang des Lieferscheins');
               AddActionKey('',-68,F10S2BestSpeichern,TRUE,'Bestellung speichern');
               AddActionKey ('',-19,RechnerAufrufen,FALSE,'Rechner aufrufen');
            END;
         END;
         STripleQuest.Init;
         STripleQuest.NewCenter(320,400);
         STripleQuest.SetChoice('   Ja    ',' �ndern  ','  Nein   ');
         HRequest.Init;
         HRequest.NewCenter(320,400);

         NEW(SearchFenster);
         SearchFenster^.Init(85,95,555,305,0,'Kundenliste','',Aktiv,ScrollKunde,SearchKunde,OutputKunde,KundenSEFunc);
         SearchFenster^.SetFirstScrollLine(1);
         SearchFenster^.AddActionKey('_',-64,AndereSortierung,FALSE,'Sortierung �ndern');
         KundenFeldAktiv;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
