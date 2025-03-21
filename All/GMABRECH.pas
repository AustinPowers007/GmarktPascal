{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 16:50:26 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMAbrech;
{$N+}
{$G+}

INTERFACE

  PROCEDURE InitAbrechInput;
  FUNCTION  Abrech(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;
  PROCEDURE Abrechnen;
  FUNCTION  OpsEintragen(Protokoll,LangeNummer:BOOLEAN;VAR Uebertragen:BOOLEAN;VAR UebertragenBis:BYTE):BOOLEAN;

IMPLEMENTATION

   USES GMDATEI,
        GMPRT,
        GLDRUCK,
        GMBASE,
        COPYUNIT,
        LIEDATEN,
        KUNDATEN,
        GMT100DR,
        GMT200DR,
        GMDELAY,
        B100Typ,
        ASTDATA,
        CASHDATA,
        CASHNEU,
        PLACEWIN,
        PRINTER,
        GMSETUP,
        GMCONST,
        KBDaten,
        GMCRT,
        ZUSAETZE,
        GRAPH,GRAPNEU;

   TYPE LeergutS =   RECORD
                        LeerZahl: ARRAY[1..PfandTAnzahl] OF LONGINT;
                      END;

        LeergutF =   FILE OF LeergutS;

        TLiefSatz =   RECORD
                       LNr,
                       KNr:   STRING[5];
                       Don:   BOOLEAN;
                       LeerG: ARRAY[1..PfandTAnzahl] OF INTEGER;
                       Zal:   STRING[1];
                       Sonst,
                       Summe,
                       Zahlt,
                       Sheck: DOUBLE;
                     END;


   VAR  StylesLauf  : TPrintStyles;

        TourText : STRING;

        LDay:         STRING[8];
        LTur:         WORD;



   VAR  LeergutFenster,
        SuchFenster,
        LLeerFenster,
        LSchlFenster: TASFensterPtr;
        Fenster:      TBrainFensterPtr;
        Barzahlung: REAL;

        SuchNr,
        laktTourKey:  STRING;
        AktuelleTour :WORD;
        DLSchluessel: TDatum;
        Printdest,
        LAktTNr:      BYTE;
        EndCode:      INTEGER;
        Ende,
        Problem,
        SchonBerechnet,
        FirstCall:    BOOLEAN;
        HlpFile:      TEXT;
        AbSum:        TT200Zeile;

        LF:  LeergutF;
        LS:  LeergutS;
        LiefS : TLiefSatz;
        LeerBool: ARRAY[1.. 3] OF BOOLEAN;
        AktLeerZahl: ARRAY[1.. 4] OF REAL;
        AktLeerBack: ARRAY[1..PfandTAnzahl] OF LONGINT;
        LeerIstWert: REAL;
        AktLiefEigen,
        AktLiefOk,
        AktLiefNOk : BYTE;

   PROCEDURE OPsUndLiefZahlAendern(Lieferung:TLiefDaten);FORWARD;


   FUNCTION OpsEintragen;
      VAR Kunde : TKundenDaten;
          KundeGefunden : BOOLEAN;
          ErsteLeerPos,
          OLauf : BYTE;
          TLauf : BYTE;
          Ergebnis : BOOLEAN;
          OPZeile1,
          OpZeile2 : STRING[80];
          LFOK : BOOLEAN;
          KeineUebertragung : BOOLEAN;

      BEGIN
         Ergebnis := FALSE;
         Uebertragen := FALSE;
         KundDb.BeginTransaction;
         KundeGefunden := KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
         KundDB.EndTransaction;
         KeineUebertragung := FALSE;
         FOR OLauf := 1 TO 19 DO BEGIN
            IF (Lieferung.Positionen[OLauf].ArtNr = '01777') OR
               (Lieferung.Positionen[OLauf].ArtNr = '777')  THEN
               KeineUebertragung := TRUE;
         END;
         IF KundeGefunden OR KeineUebertragung THEN BEGIN
            IF ((Kunde.Zahlweise = 1)  OR (Kunde.ZahlWeise >= 8)
               OR (Kunde.ZahlWeise = 0)) AND (NOT KeineUebertragung) THEN BEGIN
               ErsteLeerPos := 0;
               OLauf := 1;
               WHILE (ErsteLeerPos = 0) AND (OLauf < 19) DO BEGIN
                  IF Lieferung.Positionen[OLauf].Menge = 0 THEN
                     ErsteLeerPos := Olauf;
                  INC(OLauf);
               END;
               TLauf := ErsteLeerPos;
               UebertragenBis := 10;
               FOR OLauf := 1 TO 10 DO BEGIN
                  IF ((Kunde.Oposten[OLauf].Art = 'G') OR
                     (Kunde.Oposten[OLauf].Art = 'O'))  THEN BEGIN
                     Ergebnis := TRUE;
                     IF (TLauf <= 19) THEN BEGIN
                        Lieferung.Positionen[TLauf].Menge := 1;
                        Lieferung.Positionen[TLauf].ArtBez :=
                        Kunde.Oposten[OLauf].LiefNr+' - '+
                        Kunde.Oposten[OLauf].Bemerk;
                        IF Kunde.Oposten[OLauf].Art = 'G' THEN BEGIN
                           Lieferung.Positionen[TLauf].ArtNr := '903';
                           Lieferung.Positionen[TLauf].Preis :=
                                 (-1)*Kunde.Oposten[OLauf].Betrag;
                        END ELSE BEGIN
                           Lieferung.Positionen[TLauf].ArtNr := '902';
                           Lieferung.Positionen[TLauf].Preis :=
                                 Kunde.Oposten[OLauf].Betrag;
                        END;
                        IF LangeNummer THEN
                           Lieferung.Positionen[TLauf].ArtNr  := '01'+
                              Lieferung.Positionen[TLauf].ArtNr;
                        IF Protokoll THEN BEGIN
                           OPZeile1 :=ActDate+';'+ActTime+';'+'OP-EINGETRAGEN'';'+Lieferung.KNR+
                              ';'+Lieferung.LiefNr;
                           WITH Kunde.Oposten[OLauf] DO
                              OPZeile2 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
                           LFOk := OPLogFile^.WritelnLog(OpZeile1);
                           LFOk := OPLogFile^.WritelnLog(OPZeile2) AND LFOk;
                           IF NOT LFOK THEN BEGIN
                              FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                 'Bitte folgende Infos eintragen: '+OpZeile1+
                                 ' '+OpZeile2);
                           END;
                        END;
                        UeberTragen := TRUE;
                     END ELSE BEGIN
                        IF (TLauf = 20) THEN BEGIN
                           UebertragenBis := Olauf-1;
                        END;
                     END;
                     INC(Tlauf);
                  END;
               END;
            END;
         END ELSE BEGIN
            FaultBox.Act(0,'Keinen Zugriff auf Kunde '+Lieferung.Knr+
                           ' bitte offene Posten ÅberprÅfen.');
         END;
         OpsEintragen := Ergebnis;
      END;


   FUNCTION NaechsterLieferschein(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;FORWARD;

   FUNCTION SammelLeerGutIstLeer(LS :LeergutS):BOOLEAN;
      VAR Ergebnis : BOOLEAN;
          Lauf : BYTE;
      BEGIN
         Ergebnis := TRUE;
         FOR Lauf:=1 TO PfandTAnzahl DO BEGIN
            IF (LS.LeerZahl[Lauf] <> 0) THEN
               Ergebnis := FALSE;
         END;
         SammelLeerGutIstLeer := Ergebnis;
      END;
      
   PROCEDURE InitLS(VAR LS1:LeergutS);
      VAR Lauf:BYTE;
      BEGIN
         FOR Lauf:=1 TO PfandTAnzahl DO
            LS1.LeerZahl[Lauf]:=0;
      END;
      
   FUNCTION OpenLeergut(Datum:STRING;Tour:WORD):BOOLEAN;
      VAR Datei:STRING;
          LGPfad : STRING;
          LS1:  LeergutS;
          Ergebnis : BOOLEAN;


      BEGIN
         LGPfad := MakeFilePath(GMEnvObj^.GetEntry('LGUTDIR'));
         InitLS(LS);
         Ergebnis:=TRUE;
         Datei:=COPY(Datum,7,2)+COPY(Datum,4,2)+COPY(Datum,1,2)+'.'+
         CutBSpaces(MakeShortTourName(Tour));;
         IF ValidPath(LGPfad) THEN BEGIN
            ASSIGN(LF,LGPfad+Datei);
            {$I-}
            RESET(LF);
            {$I+}
            IF (IORESULT <> 0) THEN BEGIN
               {$I-}
               REWRITE(LF);
               WRITE(LF,LS);
               {$I+}
               IF (IORESULT <> 0) THEN BEGIN
                  {$I-}
                  CLOSE(LF);
                  {$I+}
                  Ergebnis:=FALSE
               END
            END ELSE BEGIN
               {$I-}
               READ(LF,LS);
               {$I+}
               IF (IORESULT <> 0) THEN BEGIN
                  {$I-}
                  REWRITE(LF);
                  WRITE(LF,LS);
                  {$I+}
                  IF (IORESULT <> 0) THEN BEGIN
                     {$I-}
                     CLOSE(LF);
                     {$I+}
                     Ergebnis:=FALSE
                  END
               END;
            END;
         END ELSE BEGIN
            FaultBox.Act(0,'Pfad '+LGPfad+' nicht gefunden, Leergutdatei '+
                           ' nicht geîffnet');
            Ergebnis := FALSE;
         END;
         OpenLeerGut := Ergebnis;
      END;

   PROCEDURE CloseLeergut;
      BEGIN
         {$I-}
         CLOSE(LF);
         {$I+}
         IF (IORESULT <> 0) THEN
            FaultBox.Act(0,'Fehler beim Schliessen der Leergutdatei');
      END;

   FUNCTION ReadIstLeergut(Datum:STRING;Tour:WORD):BOOLEAN;
      BEGIN
         IF (OpenLeergut(Datum,Tour)) THEN
            CloseLeergut
         ELSE
            FaultBox.Act(0,'Leergutdatei konnte nicht geîffnet werden');
      END;

   PROCEDURE WriteIstLeergut(Datum:STRING;Tour:WORD);
      VAR LS1:LeerGutS;

      BEGIN
         LS1:=LS;
         IF NOT (OpenLeergut(Datum,Tour)) THEN
            FaultBox.Act(2,'Fehler beim ôffnen der Leergutdatei')
         ELSE BEGIN
            {$I-}
            RESET(LF);
            WRITE(LF,LS1);
            {$I+}
            IF (IORESULT <> 0) THEN
               FaultBox.Act(2,'Fehler beim Eintrag in Leergutdatei');
          END;
          LS := LS1;
          CloseLeergut;
      END;

   FUNCTION GetLeergutIst:REAL;
      VAR Summe:REAL;
          PfandW:REAL;
          Lauft:BYTE;
          OldDay : BOOLEAN;

      BEGIN
         OldDay := IsOldDay(DLSchluessel);
         ReadIstLeergut(DLSchluessel,MakeIFieldTNr(lakttNr,OldDay));
         Summe:=0;
         FOR Lauft:=1 TO PfandTAnzahl DO BEGIN
            IF (LS.LeerZahl[Lauft] <> 0) THEN
               PfandW := PfandTypen[Lauft]/100;
            Summe:=Summe+LS.LeerZahl[Lauft]* PfandW;
         END;
         GetLeergutIst:=Summe
      END;

   { GÅltige und ungÅltige Felder in Maske }

   PROCEDURE Enable(BA:BYTE);
      VAR Lauf: BYTE;
      BEGIN
         WITH LeergutFenster^ DO BEGIN
            FOR Lauf:=2 TO 5 DO
                Disable(B2S(Lauf,0));
            FOR Lauf:=6 TO 7 DO
                Enable (B2S(Lauf,0));
            CASE BA OF
               1: Enable('4');
               2: FOR Lauf:=5 TO 7 DO
                     Enable(B2S(Lauf,0));
               3: FOR Lauf:=3 TO 5 DO
                     Enable (B2S(Lauf,0));
               4: FOR Lauf:=6 TO 7 DO
                     Disable(B2S(Lauf,0));
            END
         END
      END;

   { Nummer fÅr Datenreset und GÅltige Felder ermitteln }
   FUNCTION  GetEnableNum(P1,P2,P3:CHAR):BYTE;
      VAR Zahl:BYTE;
      BEGIN
         Zahl:=0;
         IF (P1='J') THEN
            Zahl:=1;
         IF (P2='J') THEN
            INC(Zahl,2);
         IF (P3='J') THEN
            INC(Zahl,4);
         CASE Zahl OF
            0: GetEnableNum:=4;
            1: GetEnableNum:=3;
            3: GetEnableNum:=1;
            5: GetEnableNum:=2
         END
      END;
   { Datenreset }

   PROCEDURE InitLeer(BA:BYTE);
      CONST LB: ARRAY[1..4,1..3] OF BOOLEAN = ((TRUE, TRUE, FALSE),
                                               (TRUE, FALSE,TRUE),
                                               (TRUE, FALSE,FALSE),
                                               (FALSE,FALSE,FALSE));
      VAR Lauf:BYTE;
      BEGIN
         FOR Lauf:=1 TO  3 DO
            LeerBool[Lauf]:=LB[BA,Lauf];
         FOR Lauf:=1 TO  PfandTAnzahl DO
            AktLeerBack[Lauf]:=0;
         Enable(BA);
      END;
   
   PROCEDURE TransferLieferungToAktLeer(AktLieferung:TLiefDaten);
      VAR PLauf,
          Lauf : BYTE;
          PFound : BOOLEAN;
          Sorte,
          Anzahl:LONGINT;
          Bemerk,
          JetztStr,
          AnzahlStr,
          SortenStr,
          WorkStr : STRING;
      BEGIN
         FOR Lauf:=1 TO 4 DO
            AktLeerZahl[Lauf]:=0;
         FOR Lauf:=1 TO PfandTAnzahl DO
            AktLeerBack[Lauf]:=0;
         WITH AktLieferung DO BEGIN
            InitLeer(GetEnableNum(Bemerkungen[1,1],Bemerkungen[1,2],Bemerkungen[1,3]));
            Bemerk:=Bemerkungen[1]+Bemerkungen[2];
            WorkStr:=COPY(Bemerk,8,LENGTH(Bemerk)-7);
            REPEAT
                SplitString(WorkStr,';',JetztStr,WorkStr);
                SplitString(JetztStr,':',SortenStr,AnzahlStr);
                Sorte :=S2L(Sortenstr);
                Anzahl :=S2L(AnzahlStr);
                CASE Sorte OF
                    991: AktLeerZahl[1] := (Anzahl/100);
                    994: AktLeerZahl[4] := (Anzahl/100);
                    ELSE BEGIN
                         PLauf := 1;
                         PFound := FALSE;
                         WHILE (Plauf <= PfandTAnzahl) AND (NOT PFound) DO BEGIN
                            IF (PfandTypen[PLauf] = Sorte) THEN BEGIN
                               PFound := TRUE;
                               AktLeerBack[PLauf]:= Anzahl;
                            END;
                           INC(PLauf);
                        END;
                    END;
                END;
             UNTIL LENGTH(WorkStr) = 0;
          END;
       END;
     
   PROCEDURE TourInSammelLeergutUebertragen(DLSchluessel :TDatum;aktuelleTour:WORD);
     VAR DatensatzGefunden : BOOLEAN;
         Bereitsgefragt : BOOLEAN;
         Uebertragen : BOOLEAN;
         GefundenesLeergut : LeergutS;
         Lauf : BYTE;
     BEGIN
       LieferDB.BeginTransaction;
       Datensatzgefunden:=LieferDB.StartIntervall(4,MakeDLfdIndex(DLSchluessel,aktuelleTour,0),
                       MakeDLfdIndex(DLSchluessel,aktuelleTour,999),Lieferung);
       IF Datensatzgefunden THEN BEGIN
          InitLS(GefundenesLeergut);
          Bereitsgefragt := FALSE;
          Uebertragen := TRUE;
          REPEAT
            IF (COPY(Lieferung.Bemerkungen[1],4,4) = 'TAI=') THEN BEGIN
               IF NOT Bereitsgefragt THEN BEGIN
                  Bereitsgefragt := TRUE;
                  Uebertragen := Request.Act('Es wurden bereits Daten in den Lieferschein'+
                                             'eingetragen, soll das Sammelleergut '+
                                             'aus diesen Daten ermittelt werden?' );
               END;
               IF Uebertragen THEN BEGIN
                  TransferLieferungToAktLeer(Lieferung);
                  FOR Lauf := 1 TO PfandTAnzahl DO
                     GefundenesLeerGut.LeerZahl[Lauf] := GefundenesLeerGut.LeerZahl[Lauf]+ AktLeerBack[Lauf];
               END;
            END;
          UNTIL ((NOT Uebertragen) OR NOT LieferDB.GetIntNext(Lieferung));
          IF Uebertragen THEN BEGIN
             LS := GefundenesLeergut;
          END;
       END;
       LieferDB.EndTransaction;
     END;

   PROCEDURE DatenSpeichern;

      PROCEDURE FillLeergut;
         VAR AnzS:   STRING[10];
             Lauf:   BYTE;
             Bemerk: STRING[240];
         BEGIN
           WITH Lieferung DO BEGIN
             IstSonder  :=0;
             IstNormal  :=0;
             IstLeerGut :=0;
             IF (SollKaesten > 9) THEN IstSonder:=SollKaesten
                                  ELSE IstNormal:=SollKaesten;
             Bemerk:='TAI=';
             FOR Lauf:=3 DOWNTO 1 DO
               IF (LeerBool[Lauf]) THEN
                 Bemerk:='J'+Bemerk
               ELSE
                 Bemerk:='N'+Bemerk;
             FOR Lauf:=1 TO PfandTAnzahl DO
               IF (AktLeerBack[Lauf] <> 0) THEN BEGIN
                 STR(AktLeerBack[Lauf]:1,AnzS);
                 IF (Lauf <= PfandGKasten) THEN
                    INC(IstLeerGut,AktLeerBack[Lauf]);
                 Bemerk:=Bemerk+W2S(PfandTypen[Lauf],0)+':'+AnzS+';';
               END;

             IF (AktLeerZahl[1] <> 0) THEN BEGIN
               STR((AktLeerZahl[1]*100):1:0,AnzS);
               Bemerk:=Bemerk+'991:'+AnzS+';'
             END;
             IF (AktLeerZahl[4] <> 0) THEN BEGIN
               STR((AktLeerZahl[4]*100):1:0,AnzS);
               Bemerk:=Bemerk+'994:'+AnzS+';'
             END;

             SollUmsatz:=AktLeerZahl[2];
             IstUmsatz :=Barzahlung;
             T100Bearbeitet:=TRUE;
             IF (IstNormal > 9) THEN BEGIN
               IstSonder:=IstNormal;
               IstNormal:=0;
             END;
             IF (LeerBool[1] = FALSE) THEN BEGIN
               IstNormal:=0;
               IstSonder:=0;
               IstLeergut:=0
             END;
             Bemerkungen[1]:=COPY(Bemerk,  1,80);
             Bemerkungen[2]:=COPY(Bemerk, 81,80);
           END;
         END;

      BEGIN
         FillLeergut;
         OPsundLiefZahlAendern(Lieferung);
         LieferDb.BeginTransaction;
         LieferDB.ChangeRec(Lieferung);
         LieferDb.EndTransaction;
      END;


   { Arbeit nach Eingabe von "Beliefert" }

   FUNCTION BoolWork1(VAR Eingabe:STRING;VAR IC:INTEGER;VAR Cpos:BYTE):BOOLEAN;FAR;
      VAR Taste:INTEGER;
      VAR Nr:BYTE;
      VAR InpString:STRING;
      VAR Abbruch,Check,MoreEval:BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Problem:=FALSE;
         IF SchonBerechnet AND ((IC = 48) OR (IC = 78) OR (IC =110)) AND (LeerGutFenster^.GetVal('001') = 'J') THEN BEGIN
            Problem := NOT Request.Act('Es wurden schon Daten in diesen '+
                                       'Lieferschein eingetragen. Sind sie '+
                                       'sicher das der Kunde nicht beliefert wurde?');



         END;
         IF NOT Problem THEN BEGIN
            CASE IC OF
                48,78,110: BEGIN
                              InitLeer(4);
                              Eingabe:='N'
                           END;
                43,74,106: BEGIN
                              InitLeer(3);
                              Eingabe:='J'
                           END;
            END;
            IF (SchonBerechnet) THEN
               CASE IC OF
                   -80,13: IF NOT(Request.Act('Beleg OK! Trotzdem Ñndern?')) THEN
                              Problem:=TRUE
                           ELSE
                              SchonBerechnet:=FALSE;
               END;
            IF NOT(Problem) THEN
               CASE IC OF
                  -80,13,48,78,
                  110,43,74,106: BEGIN
                                    IF (FirstCall) THEN BEGIN
                                       AktLeerZahl[1]:=0;
                                       AktLeerZahl[2]:=0;
                                       AktLeerZahl[3]:=0;
                                       BarZahlung := 0;
                                       AktLeerZahl[4]:=0;
                                       FirstCall:=FALSE
                                    END;
                                 END;
            END;
         END;
         IF Problem THEN BEGIN
            WRITE(CHR(7)); {*******}
            NaechsterLieferschein(Taste,Nr,InpString,Abbruch,Check,MoreEval);
            Eingabe := LeerGutFenster^.GetVal('001');
         END;
         LeergutFenster^.Refresh('');
         BoolWork1:=Problem;
      END;

   { Arbeit nach Eingabe von "Passend" }

   FUNCTION BoolWork2(VAR Eingabe:STRING;VAR IC:INTEGER;VAR Cpos:BYTE):BOOLEAN;FAR;

      PROCEDURE Berechnung;
         VAR Lauf:  BYTE;
             PfandWert: WORD;
             PLauf : BYTE;
             PFound:BOOLEAN;
         BEGIN
            FOR Lauf:=1 TO PfandTAnzahl DO
               AktLeerBack[Lauf]:=0;
            FOR Lauf:=1 TO 30 DO
               WITH Lieferung.Positionen[Lauf] DO BEGIN
                  PfandWert := R2L(Pfand*100);
                  PLauf := 1;
                  PFound := FALSE;
                  WHILE (Plauf <= PfandTAnzahl) AND (NOT PFound) DO BEGIN
                     IF (PfandTypen[PLauf] = PfandWert) THEN BEGIN
                        PFound := TRUE;
                        INC(AktLeerBack[PLauf],Menge);
                     END;
                     INC(PLauf);
                  END;
                  IF NOT PFound THEN BEGIN
                     AktLeerZahl[1]:=AktLeerZahl[1]+Menge*Pfand
                  END
               END;
           END;

      BEGIN
         CASE IC OF
            48,78,110,43,74,106: BEGIN
                                    AktLeerZahl[1]:=0;
                                    AktLeerZahl[4]:=0;
                                 END;
         END;
         CASE IC OF
            43,74,106: BEGIN
                          InitLeer(1);
                          Berechnung
                       END;
            48,78,110: IF LeerBool[3] THEN
                          InitLeer(2)
                       ELSE
                          InitLeer(3);
         END;
         LeergutFenster^.Refresh('');
         BoolWork2:=FALSE;
      END;

   { Arbeit nach Eingabe von "Ohne" }

   FUNCTION BoolWork3(VAR Eingabe:STRING;VAR IC:INTEGER;VAR Cpos:BYTE):BOOLEAN;FAR;

       BEGIN
          CASE IC OF
             43,74,106: BEGIN
                           InitLeer(2)
                        END;
             48,78,110: IF LeerBool[2] THEN
                           InitLeer(1)
                        ELSE
                           InitLeer(3)
          END;
          CASE IC OF
            48,78,110,43,74,106: BEGIN
                                    AktLeerZahl[1]:=0;
                                    AktLeerZahl[4]:=0;
                                 END;
          END;
          LeergutFenster^.Refresh('');
          BoolWork3:=FALSE;
       END;

    PROCEDURE ReadLeergut;
       VAR WorkStr,
           Bemerk,
           JetztStr:STRING[240];
           E:       STRING;
           C : INTEGER;
           Lauf,
           B:       BYTE;
           Summe: REAL;
           PLauf : BYTE;
           PFound : BOOLEAN;
       BEGIN
          FOR Lauf:=1 TO 4 DO
             AktLeerZahl[Lauf]:=0;
          FOR Lauf:=1 TO PfandTAnzahl DO
             AktLeerBack[Lauf]:=0;
          IF (COPY(Lieferung.Bemerkungen[1],4,4) = 'TAI=') THEN BEGIN
                SchonBerechnet:=TRUE;
                TransferLieferungToAktLeer(Lieferung);
                AktLeerZahl[2]:=Lieferung.SollUmsatz;
                BarZahlung := Lieferung.IstUmsatz;
                AktLeerZahl[3]:=Barzahlung+AktLeerZahl[4];
          END ELSE BEGIN
             SchonBerechnet:=FALSE;
             c:= 74;
             BoolWork2(E,C,B);
             Summe:=0;
             FOR Lauf:=1 TO 30 DO
                WITH Lieferung.Positionen[Lauf] DO
                   IF (Preis <> 0) THEN
                      Summe:=Summe+Menge*Preis;
                AktLeerZahl[3]:=Summe;
             BarZahlung := AktLeerZahl[3];
          END;
       END;

    FUNCTION AbrechAInputFunc:BOOLEAN;FAR;
       VAR FocusID:STRING;
       BEGIN
          FocusID := LeerGutFenster^.GetFocusID;
          IF (FocusID = '6') OR  (FocusID = '7') THEN BEGIN
             AktLeerZahl[3] := Barzahlung+AktLeerZahl[4];
             LeerGutFenster^.Refresh('101');
          END;
          AbrechAInputFunc := TRUE;
       END;

   FUNCTION T100Drucken(Datum:STRING;Tour:WORD;WriteKB:BOOLEAN;DruckAnz:WORD):BOOLEAN;
      VAR aktTour : STRING;
          aktV,
          AktB   : STRING;
          LD,
          NLfd   : LONGINT;
          D       : BOOLEAN;
          OldDay : BOOLEAN;
          LeerIst,
          LeerSoll: REAL;
          DatNam : STRING[80];
          KBHelp : TKBDaten;

      PROCEDURE ScheckINKBEintragen;
         BEGIN
            IF KassenDB^.BeginTransaction THEN BEGIN
               IF KassenDB^.GetRec(5,CreateKBIndex(5,KBEintrag^),KBHelp) THEN BEGIN
                  IF (KBEintrag^.TourNr <> 0) THEN BEGIN
                     KBHelp.Betrag := KBEintrag^.Betrag;
                     IF NOT KassenDB^.ChangeRec(KBHelp) THEN BEGIN
                        FaultBox.Act(0,'Kassenbucheintrag fÅr Tour konnte nicht .'+
                                       ' geÑndert werden, bitte erneut abrechnen.');
                     END;
                  END ELSE BEGIN
                     FaultBox.Act(0,'Keine zulÑssige Tournr, '+
                                     'Eintragung ins Kassenbuch nicht mîglich');

                  END;
               END ELSE BEGIN
                  NLfd :=KassenDB^.GetLfdNr;
                  INC(Nlfd);
                  IF (NLfd < 0) THEN
                     Nlfd := 0;
                  KBEintrag^.LfdNr := Nlfd;
                  IF NOT KassenDB^.NewRec(KBEintrag^) THEN BEGIN
                     FaultBox.Act(0,'Kassenbucheintrag fÅr Tour konnte nicht .'+
                                    ' angelegt werden, bitte erneut abrechnen.');
                  END ELSE BEGIN
                     KassenDB^.SetLfdNr(Nlfd);
                  END;
               END;
               IF NOT KassenDB^.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                                     KassenDB^.GetTransactionErrMsg);
            END ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                                     KassenDB^.GetTransactionErrMsg);
            END;
         END;

      PROCEDURE SummeInKBEintragen;
         VAR KBHelp : TKBdaten;
             NLfd : LONGINT;
         BEGIN
            IF KassenDB^.BeginTransaction THEN BEGIN
               IF KassenDB^.GetRec(5,CreateKBIndex(5,KBEintrag^),KBHelp) THEN BEGIN
                  IF (KBEintrag^.TourNr <> 0) THEN BEGIN
                     KBHelp.Betrag := KBEintrag^.Betrag;
                     IF NOT KassenDB^.ChangeRec(KBHelp) THEN BEGIN
                        FaultBox.Act(0,'Kassenbucheintrag fÅr Tour konnte nicht .'+
                                       ' geÑndert werden, bitte erneut abrechnen.');
                     END;
                  END ELSE BEGIN
                     FaultBox.Act(0,'Keine zulÑssige Tournr, '+
                                     'Eintragung ins Kassenbuch nicht mîglich');

                  END;
               END ELSE BEGIN
                  NLfd :=KassenDB^.GetLfdNr;
                  INC(Nlfd);
                  IF (NLfd < 0) THEN
                     Nlfd := 0;
                  KBEintrag^.LfdNr := Nlfd;
                  IF NOT KassenDB^.NewRec(KBEintrag^) THEN BEGIN
                     FaultBox.Act(0,'Kassenbucheintrag fÅr Tour konnte nicht .'+
                                    ' angelegt werden, bitte erneut abrechnen.');
                  END ELSE BEGIN
                     KassenDB^.SetLfdNr(Nlfd);
                  END;
               END;
               IF NOT KassenDB^.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                                     KassenDB^.GetTransactionErrMsg);
           END ELSE BEGIN
              FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                                     KassenDB^.GetTransactionErrMsg);
           END;
         END;

      BEGIN
         LeerIst:=0;
         LeerSoll:=0;
         InitStartText;
         T100Drucken := TRUE;
         OldDay := IsOldDay(Datum);
         aktV := MakeDLfdIndex(Datum,Tour,0);
         aktB := MakeDLfdIndex(Datum,Tour,9999);
         DatNam:=MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'))+'PRTFile.TXT';
         PrtToFile(DatNam);
         LieferDB.BEGINTransaction;
         d:=LieferDB.StartIntervall(4,aktv,aktB,Lieferung);
         LieferDB.EndTransaction;
         IF D THEN BEGIN
            DruckeT100Kopf(Lieferung);
            REPEAT
               StatusBox.Act('Drucke gerade Tour',Datum+' - '+
                             MakeTourName(Lieferung.TourNr));
               IF (Lieferung.SollKaesten = 0) THEN BEGIN
                  BerechneSoll(Lieferung);
                  LieferDB.BEGINTransaction;
                  d:= LieferDB.ChangeRec(Lieferung);
                  LieferDB.EndTransaction;
                  IF NOT D THEN
                     InfoBox.Act(0,'Fehler bei Sollberechnung in T100 Drucken');
               END;
               IF DruckeT100Zeile(Lieferung) AND WriteKB THEN BEGIN
                  ScheckInKBEintragen;
               END;
               LieferDB.BEGINTransaction;
               d:= LieferDB.GetIntNext(Lieferung);
               LieferDB.EndTransaction;
            UNTIL NOT D;
            DruckeT100Fuss;
            IF WriteKB THEN
              SummeInKBEintragen;
            StatusBox.Hide;
            IF DruckAnz > 0 THEN BEGIN
              PrinterToPrt;
              StatusBox.Act('öbergebe gerade Datei','an Drucker');
              FOR LD:=1 TO DruckAnz DO
                WriteTextDatei(DatNam);
              StatusBox.Hide
            END
         END ELSE
            T100Drucken := FALSE;
      END;

   FUNCTION Berechnung:BOOLEAN;
      VAR Summe: REAL;
          PfandW : REAL;
          Diff:  Double;
          Lauf:  BYTE;
          DiffS: STRING[10];
          ScheckUeberzahlung,
          NotPayed,
          Ja:    BOOLEAN;
      BEGIN
         Summe:=0;
         NotPayed:=FALSE;
         AktLeerZahl[3] := Barzahlung+AktLeerZahl[4];
         IF (LeerBool[1]) THEN BEGIN
            IF (ABS(AktLeerZahl[3]) < 0.01) THEN BEGIN
               IF NOT(SchonBerechnet) THEN BEGIN
                  IF (Request.Act('Hat der Kunde bezahlt?')) THEN BEGIN
                     InfoBox.Act(0,'Bitte tragen Sie die Zahlung ein!');
                     Berechnung:=FALSE;
                     EXIT;
                  END ELSE
                     NotPayed:=TRUE
               END ELSE BEGIN
                  NotPayed:=TRUE;
               END;
            END;
            { Vollgutberechnung; }
            FOR Lauf:=1 TO 19 DO
               WITH Lieferung.Positionen[Lauf] DO
                  IF (Preis <> 0) OR (Pfand <> 0)THEN
                     Summe:=Summe+Menge*(Preis+Pfand);
               { LeergutBerechnung }
            FOR Lauf:=1 TO PfandTAnzahl DO BEGIN
               IF (AktLeerBack[Lauf] <> 0) THEN
                  PfandW := PfandTypen[Lauf]/100;
               Summe:=Summe-AktLeerBack[Lauf]* PfandW;
            END;
            IF (AktLeerZahl[1] <> 0) THEN
               Summe:=Summe+AktLeerZahl[1];
         END;
         AktLeerZahl[2]:=Summe;
         LeergutFenster^.Refresh('');
         Diff:=AktLeerZahl[3] - AktLeerZahl[2];
         ScheckUeberzahlung:= (Diff >= 0.01) AND (AktLeerZahl[4] > 0.01);
         STR(Diff:4:2,DiffS);
         IF NOT(NotPayed) AND (Diff >= 0.01) AND NOT Scheckueberzahlung THEN BEGIN
            InfoBox.Act(0,DiffS+' '+Rechnungswaehrung+' zuviel berechnet! Der Betrag wird erstattet!');
         END;
         IF NOT(NotPayed) THEN BEGIN
            IF (Diff > -5) AND (Diff <= -0.01) THEN BEGIN
               InfoBox.Act(0,DiffS+' '+Rechnungswaehrung+' zuwenig berechnet!'+
                            'Die Differenz zahlen Sie!');
               AktLeerZahl[3]:=AktLeerZahl[2];
            END;
            IF (Diff <= -5) THEN BEGIN
               InfoBox.Act(0,DiffS+' '+Rechnungswaehrung+' zuwenig berechnet!'+
                                   'Wir rufen den Kunden an!');
            END
         END ELSE
            IF (NOT(SchonBerechnet)) THEN BEGIN
               STR(AktLeerZahl[2]:4:2,DiffS);
               IF NOT(Request.Act('Ist '+DiffS+' der offene Betrag?')) THEN
                  FaultBox.Act(0,'Sprechen Sie bitte mit '+
                                 'der BÅroleitung!');
            END;
         Berechnung:=TRUE
      END;


   PROCEDURE ZusatzInfoAnzeigen;
      VAR KundeGefunden : BOOLEAN;
      PROCEDURE PrintKunde;
         VAR KStr: STRING[10];
             l,o,r,u : WORD;
         VAR   FillInfo : FILLSETTINGSTYPE;
         BEGIN
            Fenster^.Show('');
            GetFillSettings(FillInfo);
            l := Fenster^.Left;
            o := Fenster^.Top;
            u := Fenster^.Bottom;
            r := Fenster^.Right;
            SETFStyle(Fenster^.Fensterfarbe);
            BAR(L+10,o+17,r-10,u-10);
            SETFillStyle(FillInfo.Pattern,FillInfo.Color);
            SETCOLOR(0);
            SETTEXTSTYLE(1,0,2);
            IF KundeGefunden THEN BEGIN
               KStr := LeadingZeros(Kunde.LfdNr,5);
               IF (Kunde.LieferANr <> 0) THEN
                  Kstr := Kstr+'/'+B2S(Kunde.LieferANr,0)
               ELSE
                  KStr := KStr+'  ';
            END ELSE
               KStr :=Lieferung.KNr+'!!';
            OUTTEXTXY(323, 40,'  Kunde       Lieferschein');
            SETTEXTSTYLE(1,0,4);
                OUTTEXTXY(320, 80,KStr+'   '+Lieferung.LiefNr);
            SETTEXTSTYLE(1,0,2);
            STR(Lieferung.TourLfdNr:1,KStr);
            OUTTEXTXY(335,140,'Tour: '+MakeTourName(Lieferung.TourNr)+' / '+KStr);
            SETTEXTSTYLE(0,0,0);
         END;
    BEGIN
       KundDb.BeginTransaction;
       KundeGefunden :=KUNDDB.GetRec(1,Lieferung.Kundennummer,Kunde);
       IF NOT KundeGefunden AND (COPY(Lieferung.Kundennummer,1,5) <> Lieferung.Knr) THEN
          KundeGefunden :=KUNDDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),Kunde);
       KundDB.EndTransaction;
       PrintKunde;
       { Leerguteingabe lesen }
       ReadLeergut;
    END;

  FUNCTION NaechsterLieferschein;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check := FALSE;
         IF Berechnung THEN BEGIN
            DatenSpeichern;
            LeerGutFenster^.SetFocusOnId('001');
            Taste := 0;
            LieferDB.BeginTransaction;
            Abbruch:=NOT LieferDB.GetIntNext(Lieferung);
            LieferDB.EndTransaction;
            IF Abbruch THEN BEGIN
               IF Request.Act('Drucken ?') THEN BEGIN
                  T100Drucken(DLSchluessel,aktuelleTour,TRUE,1);
               END;
            END;
         END;
         ZusatzInfoAnzeigen;
         LeergutFenster^.Refresh('');
         NaechsterLieferschein := TRUE;
      END;

  FUNCTION VorherigerLieferschein(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         MoreEval := FALSE;
         LeerGutFenster^.SetFocusOnId('001');
         Taste := 0;
         Abbruch := FALSE;
         Check := FALSE;
         LieferDB.BeginTransaction;
         IF NOT LieferDB.GetIntPrev(Lieferung) THEN BEGIN
           WRITE(CHR(7));
           LieferDB.GetIntTop(Lieferung);
         END;
         LieferDB.EndTransaction;
         ZusatzInfoAnzeigen;
         LeergutFenster^.Refresh('');
         VorherigerLieferschein := TRUE;
      END;

  FUNCTION LieferscheinSuchen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LSave : TLiefDaten;
          Gefunden : BOOLEAN;
          EndCode  : INTEGER;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check := FALSE;

         IF Berechnung THEN BEGIN
            EndCode :=SuchFenster^.InputWithBrain;
            IF (EndCode <> 27) THEN BEGIN
               LSave := Lieferung;
               LeerGutFenster^.SetFocusOnId('001');
               Taste := 0;
               LieferDB.BeginTransaction;
               Gefunden:=LieferDB.GetIntTop(Lieferung);
               IF Gefunden THEN BEGIN
                  Gefunden := FALSE;
                  REPEAT
                     Gefunden := (Lieferung.LiefNr = SuchNr);
                  UNTIL Gefunden OR NOT LieferDB.GetIntNext(Lieferung);
               END;
               IF NOT Gefunden THEN BEGIN
                  FaultBox.Act(0,'Lieferscheinnummer existiert nicht in dieser Tour');
                  LieferDB.GetRec(1,LSave.LiefNr,Lieferung);
               END ELSE BEGIN
                  AktLiefOk := 0;
                  AktLiefNok:= 0;
                  AktLiefEigen := 0;
                  IF Lieferung.T100Bearbeitet THEN BEGIN
                      IF (Lieferung.Bemerkungen[1][1] = 'J') THEN
                         INC(AktLiefOk)
                      ELSE
                         INC(AktLiefNOk);
                  END;
               END;
               LieferDB.EndTransaction;
               ZusatzInfoAnzeigen;
               LeerGutFenster^.Refresh('');
            END;
         END;
         LieferscheinSuchen := TRUE;
      END;

  FUNCTION TourDrucken(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         Abbruch := FALSE;
         MoreEval := FALSE;
         Check := FALSE;
         IF Berechnung THEN BEGIN
            DatenSpeichern;
            Abbruch := TRUE;
            Check := FALSE;
            T100Drucken(DLSchluessel,aktuelleTour,TRUE,1);
         END;
         TourDrucken := TRUE;
      END;

  FUNCTION TourInKB(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                    VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
         Abbruch := FALSE;
         MoreEval := FALSE;
         Check := FALSE;
         IF Berechnung THEN BEGIN
            DatenSpeichern;
            Abbruch := TRUE;
            Check := FALSE;
            T100Drucken(DLSchluessel,aktuelleTour,TRUE,0);
         END;
         TourInKB := TRUE;
      END;


   FUNCTION T100Edit(Nr:BYTE):BOOLEAN;FAR;
      VAR Lauf    : BYTE;
          D       : BOOLEAN;
          OldDay : BOOLEAN;
          Tourvorhanden : BOOLEAN;
          EndeTourEingabe : BOOLEAN;

      BEGIN
         { Toureingabe }
         DLSchluessel := '';
         LAktTnr := 1;
         REPEAT
            EndeTourEingabe := TRUE;
            EndCode := LSchlFenster^.InputWithBrain;
            IF ((EndCode = 13) OR (EndCode = -68))
               AND (DLSchluessel = '') THEN BEGIN
               EndeTourEingabe := FALSE;
               FaultBox.Act(0,'Sie mÅssen ein Datum eintragen!');

            END;
         UNTIL EndeTourEingabe;
         IF (EndCode = 13) OR (EndCode = -68) THEN BEGIN
            { Leergutbearbeitung }
            OldDay:= IsOldDay(DLSchluessel);
            aktuelleTour:= MakeIFieldTnr(lAktTnr,OldDay);
            {Neu, erst pruefen, ob die Tour ¸berhaupt existiert}
            LieferDB.BeginTransaction;
            TourVorhanden:=LieferDB.StartIntervall(4,MakeDLfdIndex(DLSchluessel,aktuelleTour,0),
                       MakeDLfdIndex(DLSchluessel,aktuelleTour,999),Lieferung);
            LieferDB.EndTransaction;
            IF TourVorhanden  THEN BEGIN
               IF ReadIstLeergut(DLSchluessel,aktuelleTour) THEN BEGIN
                  IF SammelLeerGutIstLeer(LS) THEN BEGIN
                     TourInSammelLeerGutUebertragen(DLSchluessel,aktuelleTour);
                  END;
                  EndCode:= LLeerFenster^.InputWithBrain;
               END;
               { Tourbearbeitung }
               IF (EndCode = -68) OR (EndCode = 13) THEN BEGIN
                  WriteIstLeergut(DLSchluessel,aktuelleTour);
                  { Tour vorhanden }
                  FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
                     SetLeerZa(Lauf,LS.LeerZahl[Lauf]);
                  END;
                  LieferDB.BeginTransaction;
                  d:=LieferDB.StartIntervall(4,MakeDLfdIndex(DLSchluessel,aktuelleTour,0),
                          MakeDLfdIndex(DLSchluessel,aktuelleTour,999),Lieferung);
                  LieferDB.EndTransaction;
                  IF D THEN BEGIN
                     NEW(Fenster);
                     Fenster^.Init(300,10,620,200,7,7,0,'Lieferung an:');
                     ZusatzInfoAnzeigen;
                     REPEAT
                        FirstCall:=TRUE;
                        EndCode:=LeergutFenster^.Input;
                     UNTIL LeergutFenster^.Abbruch;
                     LeerGutFenster^.Hide;
                     Fenster^.Hide;
                     Dispose(Fenster,Release);
                  END ELSE
                     InfoBox.Act(2,'Diese Tour existiert nicht!');
               END;
            END ELSE BEGIN
               InfoBox.Act(2,'Diese Tour existiert nicht!');
            END;
         END;
         T100Edit :=TRUE;
      END;


   PROCEDURE InitAbrechInput;
      VAR Lauf,
          L : WORD;
          XOfs : BYTE;
          LString : STRING;
          MinusWert : LONGINT;
          Lauf1,
          Lauf2 : BYTE;

      BEGIN
         NEW(SuchFenster);
         SuchFenster^.Init(220,150,420,230,0,'Suchen','ASIN3',Aktiv);
         WITH Suchfenster^ DO BEGIN
            AddString('','1','Lieferschein:',  1, 1,SuchNr,'',5);
            SetFKeyQuit(FALSE);
            AddActionKey('',-68,ButtonBreak,TRUE,'Lieferschein suchen');
            AddActionKey('',27,ButtonBreak,FALSE,'ZurÅck ins Leergutfenster');
         END;


         NEW(LSchlFenster);
         LSchlFenster^.Init(205,190,435,290,0,'Liefertag','ASIN3',Aktiv);
         WITH LSchlFenster^ DO BEGIN
            AddDate('','1','Datum:',1,1,DLSchluessel,'');
            AddRoll('','2','Tour :',1,2,LAktTNr,'',14);
            FOR Lauf := 1 TO TourAbs*TabsT DO
               AddItem('2',MakeIFieldTStr(Lauf));
            AlignFields;
            SetFKeyQuit(FALSE);
            AddActionKey('',-68,ButtonBreak,TRUE,'Eingegebene Tour bearbeiten');
            AddActionKey('',27,ButtonBreak,FALSE,'ZurÅck ins TourplanmenÅ');
         END;

         Xofs :=1;
         NEW(LLeerFenster);
         LLeerFenster^.Init(10,10,280,220,0,'Sammel-Leergut','ASIN3',Aktiv);
         WITH LLeerFenster^ DO BEGIN
            FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
               LString:= W2S((PfandTypen[Lauf] DIV 100),2)+','+
                         LeadingZeros((PfandTypen[Lauf] MOD 100),2)+':';
               IF Lauf >= 10 THEN
                  XOfs := 15;
               MinusWert := 0;
               CASE Lauf OF
                 10 : MinusWert :=-20;
                 11 : MinusWert:= -30;
                 12 : MinusWert:= -10;
                 13 : MinusWert:= -12;
                 ELSE MinusWert:= 0;
               END;
               AddLongInt('','3',LString,XOfs,1+ (Lauf-1) MOD 9,LS.LeerZahl[ Lauf],'',3,MinusWert  ,999);
            END;
            AlignFields;
            SetCRQuit(FALSE);
            SETFKeyQuit(FALSE);
            AddActionKey('',-68,ButtonBreak,TRUE,'Eingaben Åbernehmen und weiter');
            AddActionKey('',27,ButtonBreak,FALSE,'ZurÅck ins TourplanmenÅ');
         END;
         NEW(LeerGutFenster);
         LeergutFenster^.Init(10,10,280,410,0,'Abrechnung','ASIN3',Aktiv);

         WITH LeergutFenster^ DO BEGIN
            AddBool('','001','Beliefert:',1, 1,LeerBool[ 1],'');
            AlignFields;
            AddReal('','101','Bezahlt:',  1,2,AktLeerZahl[ 3],'',7,2,-999.99,9999.99);
            Disable('101');
            AddReal('','6','Bar:',  1,3,Barzahlung,'',7,2,-999.99,9999.99);
            AddReal('','7','Scheck:',   1,4,AktLeerZahl[ 4],'',7,2,-999.99,9999.99);
            AlignFields;
            AddHLine      ('6',0,31,5,0);
            AddBool('','4','Passende LeergutrÅckgabe:', 1, 6,LeerBool[ 2],'');
            AddBool('','5','  Ohne   LeergutrÅckgabe:', 1, 7,LeerBool[ 3],'');
            AlignFields;
            AddConst      ('3',1,9,' Genaue  LeergutrÅckgabe');
            AddHLine      ('3',0,31,10,0);
            XOfs := 1;
            FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
               LString:= W2S((PfandTypen[Lauf] DIV 100),2)+','+
                         LeadingZeros((PfandTypen[Lauf] MOD 100),2)+':';
               IF Lauf = 10 THEN
                  AlignFields;
               IF Lauf >= 10 THEN
                  XOfs := 15;
               MinusWert := 0;
               CASE Lauf OF
                 10 : MinusWert :=-20;
                 11 : MinusWert:= -30;
                 12 : MinusWert:= -10;
                 13 : MinusWert:= -12;
                 ELSE MinusWert:= 0;
               END;
               AddLongInt('','3',LString,XOfs,11+ (Lauf-1) MOD 9,AktLeerBack[ Lauf],'',4,MinusWert  ,999);
            END;
            AddReal('','3','Sonst:',15,17,AktLeerZahl[ 1],'',7,2,-999.99,9999.99);
            AlignFields;
            AddReal('','2','Summe:',    1,22,AktLeerZahl[ 2],'',7,2,-999.99,9999.99);
            AlignFields;
            Disable('2');
            SetSEvalFunc('001',BoolWork1);
            SetSEvalFunc('4',BoolWork2);
            SetSEvalFunc('5',BoolWork3);
            SEtAInputFunc(AbrechAInputFunc);
            AddActionKey('',27,ButtonBreak,FALSE,'ZurÅck ins TourplanmenÅ');
            AddActionKey('',-68,NaechsterLieferschein,TRUE,'Speichern und nÑchster Lieferschein');
            AddActionKey('',-66,VorherigerLieferschein,TRUE,'Vorherigen Lieferschein bearbeiten');
            AddActionKey('',-60,LieferscheinSuchen,TRUE,'Lieferschein suchen');
            AddActionKey('',-63,TourDrucken,TRUE,'T100 drucken und zurÅck ins TourplanmenÅ');
            AddActionKey('',-62,TourInKB,TRUE,'T100 abrechnen ohne zu drucken');
         END;
      END;

  FUNCTION Abrech;
     BEGIN
        T100Edit(1);
        Abrech := TRUE;
     END;


   PROCEDURE ToLiefSatz(Txt:STRING);
      VAR Lauf: BYTE;
          C:    INTEGER;

      FUNCTION ToVal(Txt:STRING;Fst,L:LONGINT):REAL;
         VAR Part:  STRING;
             Value: REAL;
             C:     INTEGER;
         BEGIN
           Part:=COPY(Txt,Fst,L);
           IF (POS(',',Part) <> 0) THEN BEGIN
             Part[POS(',',Part)]:='.';
             VAL(Part,Value,C);
           END ELSE BEGIN
             VAL(Part,Value,C);
             Value:=Value/100
           END;
           ToVal:=Value
         END;

      BEGIN
        LiefS.KNr:=COPY(Txt, 4,5);
        LiefS.LNr:=COPY(Txt,39,5);
        LiefS.Don:=COPY(Txt,38,1)='X';
        LiefS.Zal:=COPY(Txt,44,1);
        FOR Lauf:=1 TO PfandTAnzahl DO
          LiefS.LeerG[Lauf]:=ORD(Txt[44+Lauf])-128;
        LiefS.Sonst:=ToVal(Txt,60,7);
        LiefS.Summe:=ToVal(Txt,67,7);
        LiefS.Zahlt:=ToVal(Txt,74,7);
        LiefS.Sheck:=ToVal(Txt,81,7);
      END;


   PROCEDURE ToNotSatz(Txt:STRING);
      VAR Lauf:BYTE;
      BEGIN
        LiefS.KNr:=COPY(Txt, 4,5);
        LiefS.LNr:=COPY(Txt,39,5);
        LiefS.Don:=FALSE;
        FOR Lauf:=1 TO PfandTAnzahl DO
          LiefS.LeerG[Lauf]:=0;
        LiefS.Sonst:=0;
        LiefS.Summe:=0;
        LiefS.Zahlt:=0;
        LiefS.Sheck:=0;
      END;

   PROCEDURE ToLieferDB;

      PROCEDURE FillLeergut;
         VAR AnzS:   STRING[10];
             Lauf:   BYTE;
             Bemerk: STRING[240];
         BEGIN
           WITH Lieferung DO BEGIN
             IstSonder  :=0;
             IstNormal  :=0;
             IstLeerGut :=0;
             IF (SollKaesten > 9) THEN IstSonder:=SollKaesten
                                  ELSE IstNormal:=SollKaesten;
             IF (LiefS.Don) THEN BEGIN
               Bemerk:='JNNTAI=';
             END ELSE
               Bemerk:='NNNTAI=';

             FOR Lauf:=1 TO PfandTAnzahl DO
               IF (LiefS.LeerG[Lauf] <> 0) THEN BEGIN
                 STR(LiefS.LeerG[Lauf]:1,AnzS);
                 IF (Lauf <= PfandGKasten) THEN
                    INC(IstLeerGut,LiefS.LeerG[Lauf]);
                 Bemerk:=Bemerk+W2S(PfandTypen[Lauf],0)+':'+AnzS+';';
               END;

             IF (LiefS.Sonst <> 0) THEN BEGIN
               STR((LiefS.Sonst*100):1:0,AnzS);
               Bemerk:=Bemerk+'991:'+AnzS+';'
             END;
             IF (LiefS.Sheck <> 0) THEN BEGIN
               STR((LiefS.Sheck*100):1:0,AnzS);
               Bemerk:=Bemerk+'994:'+AnzS+';'
             END;
             SollUmsatz:=LiefS.Summe;
             IstUmsatz :=LiefS.Zahlt;
             T100Bearbeitet:=TRUE;
             IF NOT(LiefS.Don) THEN BEGIN
               IstNormal:=0;
               IstSonder:=0;
               IstLeergut:=0
             END;
             Bemerkungen[1]:=COPY(Bemerk,  1,80);
             Bemerkungen[2]:=COPY(Bemerk, 81,80);
           END;
         END;

      VAR OK:BOOLEAN;
          T1,T2:STRING[2];
          LFound : BOOLEAN;

      BEGIN
        LieferDb.BeginTransaction;
        LFound :=LieferDB.GetRec(1,LiefS.LNr,Lieferung);
        LieferDB.EndTransaction;
        IF LFound THEN BEGIN
           IF (Lieferung.Knr = LiefS.KNr) THEN BEGIN
              OK:=TRUE;
              IF (LDay = '') THEN BEGIN
                 LDay:=Lieferung.LieferTag
              END ELSE BEGIN
                IF (LDay <> Lieferung.Liefertag) THEN
                   OK:=FALSE;
              END;
              IF (LTur =  0) THEN BEGIN
                  LTur:=Lieferung.TourNr
              END ELSE BEGIN
                IF (LTur <> Lieferung.TourNr) THEN
                    OK:=FALSE;
              END;
              IF (OK) THEN BEGIN
                 FillLeergut;
                 OPsUndLiefZahlAendern(Lieferung);
                 LieferDB.BeginTransaction;
                 LieferDB.ChangeRec(Lieferung);
                 LieferDB.EndTransaction;
              END ELSE BEGIN
                 WITH Lieferung DO BEGIN
                    STR(LTur:1,T1);
                    STR(TourNr:1,T2);
                    AddStartText('     Kunde Nr.: '+KNr+'('+LiefNr+') umgelegt von '
                                 +LDay+' ('+T1+') nach '+
                                 Liefertag+' ('+T2+')');
                 END;
              END;
           END ELSE BEGIN
              AddStartText('Kundennummer ungleich bei '+Lieferung.KNr+'  '+LiefS.KNr);
           END
        END ELSE BEGIN
           AddStartText(LiefS.KNr+'  nicht gefunden!');
        END;
      END;


   PROCEDURE OPsundLiefZahlAendern(Lieferung:TLiefDaten);
      VAR KundeGefunden : BOOLEAN;
          Teilzahlung : BOOLEAN;
          ALteOps,
          OPsvorhanden,
          NeuerOP,
          OPEingetragen : BOOLEAN;
          OPBetrag      : REAL;
          OPEintrag     : BYTE;
          LPosLauf      : BYTE;
          Lauf          : BYTE;
          KHelp         : TKundenDaten;
          Beliefert     : BOOLEAN;
          OPZeile1,
          OpZeile2 : STRING[80];
          SUmsatz,
          IstUmsatz : REAL;
          Bemerk : STRING[100];
          JetztStr :STRING[15];
          WorkStr : STRING[100];
          P : BYTE;
          Sorte : WORD;
          Anzahl : LONGINT;
          LFOK : BOOLEAN;
          NOkSik,
          OkSik,
          EigenSik : LONGINT;
          LAnzChanged : BOOLEAN;

      PROCEDURE ChangeStatus;
         BEGIN
            IF (Kunde.ZahlWeise = 0) AND (Kunde.LiefOk >= 3) THEN BEGIN
               IF (Kunde.LiefNOk <> 0) THEN
                  Kunde.ZahlWeise := 8
               ELSE
                  Kunde.ZahlWeise := 1;
            END;
         END;


      BEGIN
         OPsVorhanden := FALSE;
         {OpEintrag := 11;}
         SUmsatz := 0;
         Bemerk:=Lieferung.Bemerkungen[1]+Lieferung.Bemerkungen[2];
         WorkStr:=COPY(Bemerk,8,LENGTH(Bemerk)-7);
         Beliefert := (Bemerk[1] = 'J');
         OkSik := 0;
         NOkSik := 0;
         EigenSik := 0;
         LAnzChanged := FALSE;
         REPEAT
            P:=POS(';',WorkStr);
            JetztStr:=COPY(WorkStr,1,P-1);
            WorkStr:=COPY(WorkStr,P+1,LENGTH(WorkStr)-P);
            P:=POS(':',JetztStr);
            Sorte := S2W(COPY(JetztStr,1,P-1));
            Anzahl := S2L(COPY(JetztStr,P+1,LENGTH(JetztStr)-P));
            CASE Sorte OF
              994: Sumsatz := SUmsatz+(Anzahl/100);
            END;
         UNTIL LENGTH(WorkStr) = 0;
         IstUmsatz := Lieferung.IsTUmsatz+SUmsatz;
         TeilZahlung := (Lieferung.SollUmsatz >IstUmsatz);
         OPBetrag := ABS(Lieferung.SollUmsatz-IstUmsatz);
         NeuerOP := (OpBetrag >= 0.01) AND (TeilZahlung OR (SUmsatz = 0));
         OPEingetragen := FALSE;
         AlteOps := FALSE;
         FOR Lauf := 1 TO 19 DO BEGIN
           AlteOps:= AlteOps OR
             (Lieferung.Positionen[Lauf].ArtNr = '01903') OR
             (Lieferung.Positionen[Lauf].ArtNr = '01902');
         END;
         KundDB.BeginTransaction;
         KundeGefunden := KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.Knr),0),KHelp);
         KundDB.EndTransaction;
         IF KundeGefunden THEN BEGIN
            OkSik := Kunde.LiefOk;
            NOkSik := Kunde.LiefNOk;
            EigenSik := Kunde.LiefEigen;
            Kunde.LiefOk := Kunde.LiefOk - AktLiefOk;
            Kunde.LiefNOk := Kunde.LiefNOk - AktLiefNOk;
            Kunde.LiefEigen := Kunde.LiefEigen - AktLiefEigen;
            IF Beliefert THEN
               INC(Kunde.LiefOk)
            ELSE
               INC(Kunde.LiefNOk);
            LAnzChanged := (Kunde.LiefOk <> OkSik) OR (Kunde.LiefNOk <>NokSik) OR
                           (Kunde.LiefEigen <> EigenSik);
            OPEintrag := 0;
            FOR Lauf := 1 TO 10 DO
               With KHelp.Oposten[Lauf] DO
                 IF (LiefNr = Lieferung.LiefNr) THEN BEGIN
                    OPEintrag := Lauf;
                    OPEingetragen := TRUE;
                 END;
         END;
         IF (NOT OpEingetragen AND NeuerOP) THEN BEGIN
            OPEintrag := 11;
            FOR Lauf := 10 DOWNTO 1 DO
               WITH KHelp.Oposten[Lauf] DO BEGIN
                  IF ((Art =' ') OR (Art = #0)) AND (LiefNr = '') AND (Betrag = 0) AND
                     (Bemerk = '') THEN
                     OPEintrag := Lauf;
               END;
            IF (OpEintrag = 11) THEN BEGIN
               FaultBox.Act(0,'Keine freie Position in OP-Liste, '+
                              ' kann den offenen Posten nicht eintragen. '+
                              ' offenen Posten von Kunde '+LeadingZeros(KHelp.LfdNr,5)+
                              ' bearbeiten und erneut abrechnen.');
            END;
         END;
         OpsVorhanden := AlteOPs OR NeuerOP OR OPEingetragen;
         IF KundeGefunden AND (OPsVorhanden OR LAnzChanged) THEN BEGIN
            IF OpsVorhanden THEN BEGIN
               IF NeuerOP OR OpEingetragen THEN BEGIN
                  IF (OpEingetragen) AND ((NOT NeuerOP) OR (NOT Beliefert)) THEN BEGIN
                     IF (OpEintrag < 10) THEN BEGIN
                        FOR Lauf := OpEintrag+1 TO 10 DO
                           KHelp.Oposten[Lauf-1] := KHelp.Oposten[Lauf];
                     END;
                     WITH KHelp.Oposten[10] DO BEGIN
                        Betrag := 0;
                        LiefNr :='';
                        Bemerk := '';
                        Art :=#0;
                     END;
                  END;
                  IF NeuerOP AND Beliefert THEN BEGIN
                     IF (OpEintrag <> 11) THEN BEGIN
                        WITH KHelp.Oposten[OpEintrag] DO BEGIN
                           IF TeilZahlung THEN BEGIN
                              Bemerk := 'offen vom ';
                              Art := 'O';
                           END ELSE BEGIN
                              Art := 'G';
                              Bemerk := 'Guthaben vom ';
                           END;
                           Bemerk := Bemerk+Lieferung.Liefertag+' '+MakeShortTourName(Lieferung.TourNr);
                           Betrag := OPBetrag;
                           LiefNr := Lieferung.LiefNr;
                           IF OpEingetragen THEN
                              OPZeile1 :=ActDate+';'+ActTime+';'+'OP-GEéNDERT'+';'+LeadingZeros(KHelp.LfdNr,5)+
                             ';'+Lieferung.LiefNr
                           ELSE
                              OPZeile1 :=ActDate+';'+ActTime+';'+'OP-NEU'+';'+LeadingZeros(KHelp.LfdNr,0)+
                             ';'+Lieferung.LiefNr;
                           WITH KHelp.Oposten[OpEintrag] DO
                              OPZeile2 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
                           LFOk := OPLogFile^.WritelnLog(OpZeile1);
                           LFOk := OPLogFile^.WritelnLog(OPZeile2) AND LFOk;
                           IF NOT LFOK THEN BEGIN
                              FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                 'Bitte folgende Infos eintragen: '+OpZeile1+
                                 ' '+OpZeile2);
                           END;
                        END;
                     END;
                  END;
               END;
               IF NOT Beliefert AND AlteOPS THEN BEGIN
                  LPosLauf := 1;
                  OPEintrag := 1;
                  WHILE (LPosLauf <= 19) DO BEGIN
                     WITH Lieferung.Positionen[LPosLauf] DO BEGIN
                        IF ((ArtNr = '01902') OR (ArtNr = '01903')) AND (OpEintrag <> 11) THEN BEGIN
                           OPEintrag := 11;
                           FOR Lauf := 10 DOWNTO 1 DO
                              WITH KHelp.Oposten[Lauf] DO BEGIN
                                 IF ((Art =' ') OR (Art = #0)) AND (LiefNr = '') AND (Betrag = 0) AND
                                    (Bemerk = '') THEN
                                    OPEintrag := Lauf;
                              END;
                           IF (OPEintrag < 11) THEN BEGIN
                              KHelp.Oposten[OPEintrag].LiefNr := COPY(ArtBez,1,5);
                              KHelp.Oposten[OPEintrag].Betrag := ABS(Preis);
                              IF (Preis >= 0) THEN
                                 KHelp.Oposten[OPEintrag].Art := 'O'

                              ELSE
                                 KHelp.Oposten[OPEintrag].Art := 'G';

                              IF (POS('Guthaben vom',KHelp.Oposten[OPEintrag].Bemerk) > 0) THEN
                                 KHelp.Oposten[OPEintrag].Art := 'G';
                              IF (POS('offen vom',KHelp.Oposten[OPEintrag].Bemerk) > 0) THEN
                                 KHelp.Oposten[OPEintrag].Art := 'O';
                              OPZeile1 :=ActDate+';'+ActTime+';'+'OP-RöCKöBERTRAGEN'+';'+LeadingZeros(KHelp.LfdNr,5)+
                                ';'+Lieferung.LiefNr;
                              WITH KHelp.Oposten[OpEintrag] DO
                                 OPZeile2 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
                              LFOk := OPLogFile^.WritelnLog(OpZeile1);
                              LFOk := OPLogFile^.WritelnLog(OPZeile2) AND LFOk;
                              IF NOT LFOK THEN BEGIN
                                 FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                    'Bitte folgende Infos eintragen: '+OpZeile1+
                                    ' '+OpZeile2);
                              END;
                           END ELSE BEGIN
                              FaultBox.Act(0,'Keine freie Position in OP-Liste, '+
                                             ' kann den offenen Posten nicht eintragen. '+
                                             ' offenen Posten von Kunde '+LeadingZeros(KHelp.LfdNr,5)+
                                             ' bearbeiten und erneut abrechnen.');
                           END;
                        END;
                     END;
                     INC(LPosLauf);
                  END;
               END;
            END;
            IF LAnzChanged THEN
               ChangeStatus;
            KundDB.BeginTransaction;
            IF NOT KundDB.ChangeRec(KHelp) THEN BEGIN
               FaultBox.Act(0,'Keinen Zugriff auf Kunde '+Lieferung.KNR+
                              ' die énderungen an den offenen Posten konnten nicht Åbertragen'+
                              ' werden. Bitte manuell anhand der Logdatei korregieren.');
               OPZeile1 :=ActDate+';'+ActTime+';'+'K-SCHREIBFEHLER'+';'+LeadingZeros(KHelp.LfdNr,5)+
                           ';'+Lieferung.LiefNr;
               LFOk := OPLogFile^.WritelnLog(OpZeile1);
               IF NOT LFOK THEN BEGIN
                  FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                          'Bitte folgende Infos eintragen: '+OpZeile1);
               END;
            END;
            KundDB.EndTransaction;
         END ELSE BEGIN
            IF OpsVorhanden THEN BEGIN
               FaultBox.Act(0,'Keinen Zugriff auf Kunde '+Lieferung.KNR+
                              ' die offenen Posten konnten nicht aktualisiert'+
                              ' werden. Bitte manuell korregieren.');
            END;
         END;
      END;

   PROCEDURE T100Edit02;
      VAR Lauf:     BYTE;
          D,
          Print,
          DoChange:  BOOLEAN;
          LiefN:     STRING[5];
          T:         TEXT;
          Sign:      CHAR;
          Txt:       STRING;
          FromHandy : STRING;
          Sym1,Sym2: STRING[1];
          ATabs,
          ATour:BYTE;
      BEGIN
         FromHandy := MakeFilePath(GMEnvObj^.GetEntry('FRHANDYDIR'));
         {InitStartText;}

         IF SeekFile(FromHandy+'KUNDEN') THEN BEGIN
            StatusBox.Act('Lese Kundendatei','aus Handyverzeichnis');
            DELAY(3000);
            ASSIGN(T,FromHandy+'KUNDEN');
            {$I-} RESET(T);
               FOR Lauf:=1 TO 25 DO READ(T,Sign);
            {$I+}
            LDay:='';
            LTur:= 0;
            WHILE NOT(EOF(T)) AND (IORESULT = 0) DO BEGIN
               Txt:='';
               {$I-}
               FOR Lauf:=1 TO  3 DO
                  READ(T,Sign);
               FOR Lauf:=1 TO 87 DO BEGIN
                  READ(T,Sign);
                  Txt:=Txt+Sign;
               END;
               {$I+}
               Sym1:=COPY(Txt, 1,1);
               Sym2:=COPY(Txt,38,1);
               { Zuordnung der Lieferdaten }

               IF (Sym2 = 'X') AND (Sym1 <> ' ') AND (Sym1 <> '>') THEN BEGIN
                  ToLiefSatz(Txt);
                  ToLieferDB
               END;

               { Zuordnung der LeergutrÅckgabe }

               IF (Sym1 = ' ') OR (Sym1 = '>') THEN BEGIN
                  FOR Lauf:=1 TO PfandTAnzahl DO
                     SetLeerZa(Lauf,ORD(Txt[44+Lauf])-128);
               END;

               { Zuordnung nicht belieferter Kunden }

               IF (Sym2 = ' ') AND (Sym1 <> ' ') AND (Sym1 <> '>') THEN BEGIN
                  ToNotSatz(Txt);
                  ToLieferDB
               END;
            END;
            {$I-}
            CLOSE(T);
            {$I+}
            StatusBox.Hide;
            SplitTnr(LTur,Atabs,ATour);
            TourText:='K'+COPY(LDay,1,2)+COPY(LDay,4,2)+COPY(LDay,7,2)+'.'+
                   LeadingZeros(LTur,3);
            T100Drucken(LDay,LTur,TRUE,1);
            IF (CopyDatei(FromHandy+'KUNDEN',Fromhandy+TourText) = 0) THEN
              WRITE(CHR(7));
         END
      END;

    PROCEDURE Abrechnen;
        BEGIN
           LAktTNr   := 1;
           PrintDest := 1;
           InitTLiefDaten(Lieferung);
           T100Edit02;
        END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
