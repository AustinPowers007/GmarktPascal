{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 17:55:02 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMBASE;

INTERFACE
   USES GMDATEI,
        LIEDATEN,
        ZUSAETZE,
        KUNDATEN;

   CONST PrgLaden = FALSE;

   CONST PfandTAnzahl = 15;
         PfandNegAnzahl = 4;
         PfandGkasten = 9;
         PfandTFlaschen = 4;
         PfandTKaesten = 4;
   VAR   PfandTypen :ARRAY[1..PfandTAnzahl] OF WORD;
   VAR   NegPfand : ARRAY[1..PfandNegAnzahl] OF WORD;
   VAR   PfandFlaschen :ARRAY[1..PfandTFlaschen] OF WORD;
   VAR   PfandKaesten  :ARRAY[1..PfandTKaesten] OF WORD;

   VAR  LiefPosEd : ARRAY[1..30] OF BOOLEAN;
        Pip       :  ARRAY[1..30] OF REAL;
        AnzStdPos :BYTE;
        KLfd      : STRING;
        RechnungsWaehrung : STRING[3];
        KLfdNo    : LONGINT;
        LDatum :    TDatum;
        LUhrZeit :  TZeit;
        UebDatum   : TDatum;
        AktFahrer,
        AktZusatz,
        AktEingFahrer: STRING;
        LiefStatus: STRING[1];
        LeerGutArray : ARRAY[1..PfandTAnzahl] OF LONGINT;

   FUNCTION SeekArt(VAR ArtNr:STRING; ArrayPos:WORD):BOOLEAN;

   PROCEDURE DelPosition(DelPos:BYTE;VAR Daten:TLiefDaten);
   PROCEDURE PreparePosSave(VAR Daten:TLiefDaten);
   FUNCTION IsStdZeit:BOOLEAN;
   FUNCTION IsStdBest:BOOLEAN;
   FUNCTION IsOldDay(Datum:STRING):BOOLEAN;
   FUNCTION CheckLZeitRaum(Tag:Tdatum;VAR Von,Bis:TZeit):BOOLEAN;
   PROCEDURE InitLZRaum;
   FUNCTION CalculateTagesAbschnitt(KNR:STRING;Von:TZeit):BYTE;
   PROCEDURE InitPfandDaten(Pfandsummen:STRING;FlaschenPfandsummen:STRING;
                            KaestenPfandSummen:STRING);
   PROCEDURE InitWaehrung(Waehrungssymbol:STRING);


IMPLEMENTATION
   USES  ARTDATEN,
         GMSETUP,
         PLACEWIN;

   TYPE  LZRaum = RECORD
                     Von,
                     Bis  : TZeit;
                  END;

   VAR   Lieferzeitraum : ARRAY[1..6,1..2] OF LZRaum;
   FUNCTION CalculateTagesAbschnitt;
      VAR Ergebnis : BYTE;
          TLauf    : BYTE;
          TFound   : BOOLEAN;
      BEGIN
         Ergebnis := 0;
         IF (Knr <> '00000') THEN BEGIN
            TLauf := 1;
            TFound := FALSE;
            REPEAT
               IF (Von <= TourEintZeit[TLauf]) THEN BEGIN
                  TFound := TRUE;
                  Ergebnis := TLauf;
               END ELSE
                 INC(TLauf);
            UNTIL (TLauf > TourAbs) OR TFound;
         END ELSE
            Ergebnis :=TourAbs+2;
         CalculateTagesAbschnitt := Ergebnis;
      END;

   PROCEDURE InitLZRaum;
      CONST LKW : ARRAY[1..6] OF STRING[4] =('LZMO'      ,'LZDI',
                                             'LZMI'      ,'LZDO',
                                             'LZFR'      ,'LZSA');
      VAR Lauf1,Lauf2 : BYTE;
      VAR InpString : STRING;
          ErsteZeit,ZweiteZeit,
          Front,Rest: STRING;

      BEGIN
         FOR Lauf1 := 1 TO 6 DO
            FOR Lauf2 := 1 TO 2 DO BEGIN
               LieferZeitraum[Lauf1,Lauf2].Von := '';
               LieferZeitraum[Lauf1,Lauf2].Bis := '';
            END;
         FOR Lauf1 := 1 TO 6 DO BEGIN
            InpString := GMEnvObj^.GetEntry(LKW[Lauf1]);
            IF (InpSTring <> '') THEN BEGIN
               SplitString(InpString,';',ErsteZeit,ZweiteZeit);
               SplitString(ErsteZeit,'-',Front,Rest);
               LieferZeitRaum[Lauf1,1].Von:= Front;
               LieferZeitRaum[Lauf1,1].Bis:= Rest;
               IF (ZweiteZeit <> '') THEN BEGIN
                  SplitString(ZweiteZeit,'-',Front,Rest);
                  LieferZeitRaum[Lauf1,2].Von:= Front;
                  LieferZeitRaum[Lauf1,2].Bis:= Rest;
               END;
            END;
         END;
      END;

   FUNCTION CheckLZeitraum;
     VAR AktTag : BYTE;
         Zeitraum :BYTE;
         Ergebnis :BOOLEAN;
         VonDiff,
         BisDiff,
         VonZ1,
         BisZ1,
         VonZ2,
         BisZ2  : LONGINT;
         Zeit1Treffer,
         Zeit2Treffer : BYTE;

         Zeit1Exists,Zeit2Exists,
         Zeit1Ok,Zeit2Ok : BOOLEAN;
         Zeit1VonOk,
         Zeit1BisOk,
         Zeit2VonOk,
         Zeit2BisOk :BOOLEAN;
     BEGIN
        AktTag := DayOfWeek(Tag);
        Zeit1Exists :=(LieferZeitraum[AktTag,1].Von <> '') AND
                      (LieferZeitraum[AktTag,1].Bis <> '');
        Zeit2Exists :=(LieferZeitraum[AktTag,2].Von <> '') AND
                      (LieferZeitraum[AktTag,2].Bis <> '');
        Zeit1VonOk := TRUE;
        Zeit1BisOk := TRUE;
        Zeit2VonOk := TRUE;
        Zeit2BisOk := TRUE;
        IF Zeit1Exists AND (Von <> '') THEN
           Zeit1VonOk := NotEarlierTime(Von,LieferZeitraum[AktTag,1].Von) AND
                         NotLaterTime(Von,LieferZeitraum[AktTag,1].Bis);
        IF Zeit1Exists AND (Bis <> '') THEN
           Zeit1BisOk := NotEarlierTime(Bis,LieferZeitraum[AktTag,1].Von) AND
                         NotLaterTime(Bis,LieferZeitraum[AktTag,1].Bis);
        IF Zeit2Exists AND (Von <> '') THEN
           Zeit2VonOk := NotEarlierTime(Von,LieferZeitraum[AktTag,2].Von) AND
                         NotLaterTime(Von,LieferZeitraum[AktTag,2].Bis);
        IF Zeit2Exists AND (Bis <> '') THEN
           Zeit2BisOk := NotEarlierTime(Bis,LieferZeitraum[AktTag,2].Von) AND
                         NotLaterTime(Bis,LieferZeitraum[AktTag,2].Bis);
        Zeit1Ok := Zeit1VonOk AND Zeit1BisOk;
        Zeit2Ok := Zeit2VonOk AND Zeit2BisOk;
        Ergebnis := (Zeit1Exists AND Zeit1Ok) OR (Zeit2Exists AND Zeit2Ok);
        IF NOT Ergebnis THEN BEGIN
           Zeitraum := 1;
           IF Zeit2Exists THEN BEGIN
              Zeit1Treffer := 0;
              Zeit2Treffer := 0;
              IF Zeit1VonOk THEN
                 INC(Zeit1Treffer);
              IF Zeit1BisOk THEN
                 INC(Zeit1Treffer);
              IF Zeit2VonOk THEN
                 INC(Zeit2Treffer);
              IF Zeit2BisOk THEN
                 INC(Zeit2Treffer);
              IF (Zeit1Treffer < Zeit2Treffer) THEN
                 ZeitRaum := 2;
              IF (Zeit1Treffer = Zeit2Treffer) THEN BEGIN
                 VonZ1 := 0;
                 BisZ1 := 0;
                 VonZ2 := 0;
                 BisZ2 := 0;
                 VonDiff := 0;
                 IF (Von<> '') THEN BEGIN
                    IF Zeit1VonOk THEN
                       VonZ1 := 0
                    ELSE
                       VonZ1:=CompareTime(Von,LieferZeitraum[AktTag,1].Von);
                    IF Zeit2VonOk THEN
                       VonZ2 := 0
                    ELSE
                       VonZ2:=CompareTime(Von,LieferZeitraum[AktTag,2].Von);
                    VonDiff := VonZ2-VonZ1;
                 END;
                 IF (Bis <> '') THEN BEGIN
                    IF Zeit1BisOk THEN
                       BisZ1 := 0
                    ELSE
                       BisZ1:=CompareTime(LieferZeitraum[AktTag,1].Bis,Bis);
                    IF Zeit2BisOk THEN
                       BisZ2 := 0
                    ELSE
                       BisZ2:=CompareTime(LieferZeitraum[AktTag,2].Bis,Bis);
                    BisDiff := BisZ2-BisZ1;
                 END;
                 IF (VonDiff +BisDiff) > 0 THEN
                    INC(ZeitRaum);
              END;
           END;
           IF (ZeitRaum = 1) THEN BEGIN
              Von := LieferZeitraum[AktTag,1].Von;
              Bis:= LieferZeitraum[AktTag,1].Bis;
           END;
           IF (ZeitRaum = 2) THEN BEGIN
              Von := LieferZeitraum[AktTag,2].Von;
              Bis:= LieferZeitraum[AktTag,2].Bis;
           END;
        END ELSE BEGIN
           IF (Von <> '') OR (Bis <> '') THEN BEGIN
              IF Zeit1Ok THEN
                 ZeitRaum := 1;
              IF Zeit2Ok THEN
                 Zeitraum := 2;
              IF (Von = '') THEN
                 Von := LieferZeitraum[AktTag,Zeitraum].Von;
              IF (Bis = '') THEN
                 Bis := LieferZeitraum[AktTag,Zeitraum].Bis;
           END;
        END;
        CheckLZeitRaum := Ergebnis;
     END;

   FUNCTION IsOldDay;
      VAR OldDay : BOOLEAN;
      BEGIN
         OldDay := FALSE;
         IF LieferDB.BeginTransaction THEN BEGIN
            OldDay :=  LieferDb.IsOldDataBaseDay(Datum);
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
               LieferDB.GetTransactionErrMsg);
         END
         ELSE BEGIN
            FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
            LieferDB.GetTransactionErrMsg);
         END;
         IsOldDay:= OldDay;
      END;

   FUNCTION SeekArt;
      VAR   L     : LONGINT;
            C     : INTEGER;
            Pr,
            Pf    : REAL;
            ArtNrSeek: STRING;
      BEGIN
         SeekArt := FALSE;
         IF ArtNr = '888' THEN
            ArtNrSeek := '99999'
         ELSE
            ArtNrSeek := MakeArtNr(S2L('01'+ArtNr));
         IF ArtDB.SeekRec(1,ArtNrSeek) THEN BEGIN
            WITH ArtDB.Rec.Artikel DO BEGIN
               IF (ArtBez <> '') THEN
                  Lieferung.Positionen[ArrayPos].ArtBez := ArtBez;
               IF (ArtNr = '01908') THEN
                 Lieferung.Positionen[ArrayPos].ArtBez:=
                 Lieferung.Positionen[ArrayPos].ArtBez+' '+Lieferung.LiefNr;
               Pr := 0; PF:=0;
               Lieferung.Positionen[ArrayPos].KAFL := Typ;
               {IF (Typ = 1) THEN BEGIN}
               IF (PrgLaden) THEN
                 Pr:=PreisKAL
               ELSE
                 Pr := PreisKA;
               PF := PfandGes;
               {END ELSE
               IF (Copy(ArtBez,1,5) = 'Pfand') THEN
               Pf:=PfandGes;}
               IF Pr <> 0 THEN
                  Lieferung.Positionen[ArrayPos].Preis := Pr;
               Lieferung.Positionen[ArrayPos].Pfand := Pf;
               SeekArt := TRUE;
            END;
         END ELSE
            ArtNr := '';
      END;


   PROCEDURE DelPosition;
      VAR Lauf : BYTE;
      BEGIN
         IF (DelPos < 19) THEN BEGIN
             FOR Lauf := DelPos+1 TO 19 DO BEGIN
                Daten.Positionen[Lauf-1]:= Daten.Positionen[Lauf];
                LiefPosEd[Lauf-1] := LiefPosEd[Lauf];
               Pip[Lauf-1] := Pip[Lauf];
            END;
         END;
         WITH Daten.Positionen[19] DO BEGIN
              Menge := 0;ArtNr := '';ArtBez := '';Preis := 0;
              KAFL:= 0;Pfand:=0;
         END;
          LiefPosEd[19] := TRUE;
          Pip[19] := 0;
      END;

   PROCEDURE PreparePosSave;
      VAR NotEmptyFound : BOOLEAN;
          Lauf:BYTE;

      BEGIN
         NotEmptyFound := FALSE;
         NotEmptyFound := NOT EmptyLiefPos(Daten,19);
         IF (NOT LiefPosEd[19]) THEN BEGIN
            WITH Daten.Positionen[19] DO BEGIN
                    Menge := 0;ArtNr := '';ArtBez := '';Preis := 0;
                    KAFL:= 0;Pfand:=0;
            END;
            NotEmptyFound := FALSE;
         END;
         FOR Lauf := 18 DOWNTO 1 DO BEGIN
            NotEmptyFound := NotEmptyFound OR (NOT EmptyLiefPos(Daten,Lauf));
            IF (EmptyLiefPos(Daten,Lauf) OR (NOT LiefPosEd[Lauf] )) AND NotEmptyFound THEN BEGIN
               DelPosition(Lauf,Daten);
            END;
         END;
      END;

   FUNCTION IsStdZeit;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := (Kunde.StdVon <> '') OR (Kunde.StdBis <> '');
         IF Ergebnis THEN
            Ergebnis :=(Lieferung.Von[1] = Kunde.StdVon) AND
                       (Lieferung.Bis[1] = Kunde.StdBis);
         IsStdZeit := Ergebnis;
      END;

   FUNCTION IsStdBest;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         IF (AnzStdPos > 0) THEN BEGIN
            Ergebnis := TRUE;
            Lauf :=1;
            WHILE Ergebnis AND (Lauf <= AnzStdPos) DO BEGIN
               Ergebnis := Ergebnis AND (LiefPosED[Lauf] = Kunde.StdBest[Lauf].Uebernehmen);
               WITH Lieferung.Positionen[Lauf] DO
                  Ergebnis := Ergebnis AND ((Menge <> 0) OR (ArtBez <> '') OR (ArtNr <> '') OR
                                            (Preis <> 0)) AND
                               (Menge = Kunde.StdBest[Lauf].Menge) AND
                               (ArtNr = Kunde.StdBest[Lauf].ArtNr) AND
                               (ArtBez = Kunde.StdBest[Lauf].ArtBez) AND
                               (Preis = Kunde.StdBest[Lauf].Preis);
               INC(Lauf);
            END;
         END;
         IsStdBest := Ergebnis;
      END;

      PROCEDURE InitPfandDaten(Pfandsummen:STRING;FlaschenPfandSummen:STRING;
                               KaestenPfandSummen:STRING);
        VAR LAUF : BYTE;
            Rest: STRING;
            ErsterEintrag: STRING;

      BEGIN
         { ‹bernahme der Pfandsummen aus dem ¸bergebenen String}
         FOR Lauf := 1 TO PfandTAnzahl DO
            Pfandtypen[Lauf] := 0;
         Rest := Pfandsummen;
         Lauf := 1;
         REPEAT
             SplitString(Rest,';',ErsterEintrag,Rest);
             IF ErsterEintrag <> '' THEN BEGIN
                Pfandtypen[Lauf] := R2L(S2R(ErsterEintrag)*100);
             END;
             Lauf := Lauf + 1;
         UNTIL (Rest = '') OR (Lauf > PfandTAnzahl);

         { öbernahme der negativen Pfandsummen aus dem Åbergebenen String}
         FOR Lauf := 1 TO PfandNegAnzahl DO
            NegPfand [Lauf] := 0;
         Rest := FlaschenPfandSummen;
         Lauf := 1;
         REPEAT
             SplitString(Rest,';',ErsterEintrag,Rest);
             IF ErsterEintrag <> '' THEN BEGIN
                NegPfand[Lauf] := R2L(S2R(ErsterEintrag)* 100);
             END;
             Lauf := Lauf + 1;
         UNTIL (Rest = '') OR (Lauf > PfandNegAnzahl);
         { öbernahme der Flaschen Pfandsummen aus dem Åbergebenen String}
         FOR Lauf := 1 TO PfandTFlaschen DO
            PfandFlaschen [Lauf] := 0;
         Rest := FlaschenPfandSummen;
         Lauf := 1;
         REPEAT
             SplitString(Rest,';',ErsterEintrag,Rest);
             IF ErsterEintrag <> '' THEN BEGIN
                PfandFlaschen[Lauf] := R2L(S2R(ErsterEintrag)* 100);
             END;
             Lauf := Lauf + 1;
         UNTIL (Rest = '') OR (Lauf > PfandTFlaschen);
         { öbernahme der Kaesten Pfandsummen aus dem Åbergebenen String}
         FOR Lauf := 1 TO PfandTKaesten DO
            PfandKaesten [Lauf] := 0;
         Rest := KaestenPfandSummen;
         Lauf := 1;
         REPEAT
             SplitString(Rest,';',ErsterEintrag,Rest);
             IF ErsterEintrag <> '' THEN BEGIN
                PfandKaesten[Lauf] := R2L(S2R(ErsterEintrag)* 100);
             END;
             Lauf := Lauf + 1;
         UNTIL (Rest = '') OR (Lauf > PfandTKaesten);
      END;

      PROCEDURE InitWaehrung(Waehrungssymbol:STRING);
      BEGIN
         RechnungsWaehrung := FillUp(' ',LeftStr(Waehrungssymbol,3),3,3);
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
