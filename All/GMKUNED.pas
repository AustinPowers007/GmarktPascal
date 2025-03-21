{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:03:44 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMKUNED;

INTERFACE
   USES ZUSAETZE,
        GMDATEI;

   PROCEDURE InitKundenInput;

   PROCEDURE ShowKundenInfo;
   PROCEDURE ClearKundenInfo;
   PROCEDURE DOKNeu(VAR KLfd:STRING;VAR KlfdNo:LONGINT);
   PROCEDURE DOKAender;

IMPLEMENTATION

      USES GMCRT,
           PRINTER,
           CASHDATA,
           CASHNEU,
           GMCONST,
           KUNDATEN,
           ARTDATEN,
           GMSETUP,
           GMUEBER,
           LIEDATEN,
           PLACEWIN,
           ASTDATA;

       CONST KFensterAnz = 5;

       VAR LiefInfoString,
           AdressString,
           TelefString,
           ZahlZString,
           APartnerString : STRING[40];
           OPString : STRING[25];
           HSStr,
           ZahlWeiseStr : STRING[25];

       VAR AnschriftenTitel: STRING[25];
       VAR AndereStrassen : ARRAY[0..5] OF STRING[25];
       VAR NamFeld:STRING[40];
       VAR ValTrue : BOOLEAN;
       VAR InKundenFeld : BOOLEAN;
       VAR Saved        : BOOLEAN;
           OldPQ        : STRING[5];
       VAR AlleKFenster : ARRAY[1..5] OF ^TEingabeFenster;
           OPSik : ARRAY[1..10] OF TOPosten;
           OPGeaendert : BOOLEAN;


       VAR NeueAnschrift : BOOLEAN;
           Aendern:BOOLEAN;
       VAR KRAdrFenster,
           KOPostenFenster,
           KAnlInfoFenster,
           Kundeninfo,
           KFenster   :TEingabeFensterPtr;
           KStdFenster:TASFensterPtr;
           KEditMenue :TAuswahlMenuePtr;

{   PROCEDURE BriefNeuKunde;
      VAR Sign1:CHAR;
      BEGIN
        WRITE(CHR(7));
        StatusBox.Act ('Briefbogen einlegen!',
                       'Kundennummer: '+LeadingZeros(Kunde.LfdNr,5));
        Sign1:=READKEY;
        WITH Kunde DO BEGIN
          IF NOT(Firma) THEN BEGIN
            CASE LieferAnr OF
              1: WRITELN(LST,'Herrn');
              2: WRITELN(LST,'Frau');
            END;
            IF (LiefAnrede <> '') THEN
              WRITE(LST,LiefAnrede,' ');
            IF (LiefVorname <> '') THEN
              WRITE(LST,LiefVorname,' ');
            WRITELN(LST,LiefName);
          END ELSE BEGIN
            WRITELN(LST,LiefName);
            IF (LiefAPartner <> '') THEN BEGIN
              CASE LiefAPAnrede OF
                1: WRITE(LST,'z. Hd. Herrn');
                2: WRITE(LST,'z. Hd. Frau');
              END;
              WRITELN(LST,LiefAPartner);
            END;
          END;
          WRITELN(LST,LiefStrasse);
          WRITELN(LST);
          WRITELN(LST,LiefPlz,' ',LiefOrt);
          WRITELN(LST);
          IF NOT(Firma) THEN
            CASE LieferAnr OF
              1: WRITELN(LST,'Sehr geehrter Herr ',LiefName,',');
              2: WRITELN(LST,'Sehr geehrte Frau ',LiefName,',')
            END;
          ELSE
            IF (LiefAPartner <> '') THEN
              CASE LiefAPAnrede OF
                1: WRITELN(LST,'Sehr geehrter Herr ',LiefName,',');
                2: WRITELN(LST,'Sehr geehrte Frau ',LiefName,',')
              END;

        END;
        WRITELN(LST,Kunde.
        StatusBox.Hide
      END;
 }
   PROCEDURE WriteOPAenderung;
      VAR LFOK : BOOLEAN;
          OPZeile1 : STRING[80];
          Lauf : BYTE;
          Geaendert : BOOLEAN;
      FUNCTION OPUnEqual(OP1,OP2:TOposten):BOOLEAN;
         BEGIN
            OPUnEqual:= (Op1.Art <> OP2.Art) OR
                        (Op1.Betrag <> Op2.Betrag) OR
                        (Op1.LiefNr <> Op2.LiefNr) OR
                        (Op1.Bemerk <> Op2.Bemerk);
         END;

      BEGIN
         Geaendert := FALSE;
         FOR Lauf := 1 TO 10 DO BEGIN
            IF OPUnEqual(OPSik[Lauf], Kunde.OPosten[Lauf]) THEN
               Geaendert := TRUE;
         END;
         OPZeile1 :=ActDate+';'+ActTime+';'+'OP-LISTEALT'+';'+LeadingZeros(Kunde.LfdNr,5);
         LFOk := OPLogFile^.WritelnLog(OpZeile1);
         FOR Lauf := 1 TO 10 DO BEGIN
             WITH OpSik[Lauf] DO
                OPZeile1 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
             LFOk := OPLogFile^.WritelnLog(OPZeile1) AND LFOk;
         END;
         OPZeile1 :=ActDate+';'+ActTime+';'+'OP-LISTENEU'+';'+LeadingZeros(Kunde.LfdNr,5);
         LFOk := OPLogFile^.WritelnLog(OpZeile1) AND LFOK;
         FOR Lauf := 1 TO 10 DO BEGIN
             WITH Kunde.Oposten[Lauf] DO
                OPZeile1 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
             LFOk := OPLogFile^.WritelnLog(OPZeile1) AND LFOk;
         END;
         IF NOT LFOK THEN BEGIN
            FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                           'OP-Listendarstellung unvollst�ndig, bitte'+
                           'manuell korregieren');
         END;
      END;



   PROCEDURE ShowKundenInfo;
      VAR Lauf : BYTE;
          KLfdNr : LONGINT;
          KHelp : TKundenDaten;
          OPGut,
          OPOffen : BYTE;
          OPSumme  : REAL;
      BEGIN
         FOR Lauf := 0 TO 5 DO BEGIN
             AndereStrassen[Lauf] :='';
         END;
         LiefInfoString := '';
         APartnerString := '';
         WITH Kunde DO BEGIN
            NamFeld :='';
            CASE LiefAnrede OF
               3  : BEGIN
                       NamFeld:=Anr[LiefAnrede]+' '+LiefName;
                       APartnerString := Anr[LiefAPAnrede]+' '+LiefApartner;
                       APartnerString := PrintString(APartnerString,40);
                    END;
               2,1: BEGIN
                       NamFeld := Anr[LiefAnrede]+' ';
                       IF (LENGTH(LiefName)+LENGTH(LiefVorname) < 35) THEN
                          NamFeld:=NamFeld+LiefVorname+' '+LiefName
                       ELSE BEGIN
                          IF (LENGTH(LiefName)+LENGTH(LiefVorname) < 40) THEN
                             NamFeld:=LiefVorname+' '+LiefName
                          ELSE BEGIN
                             NamFeld := NamFeld+LiefName+', '+LiefVorname;
                          END;
                       END;
                    END;
            END;
            HSStr:= '';
            IF (Betrag <> 0) THEN
               HsStr := 'Hinterlegt: '+PrintReal(Betrag,3,2,TRUE);
            HsStr := PrintString(HSStr,25);
            ZahlWeiseStr := '';
            IF (ZahlWeise >=0) AND (ZahlWeise<=ZahlWeiseTypen) THEN
               ZahlweiseStr := ZahlWeiseText[ZahlWeise];
            ZahlWeiseStr:=PrintString(LeftStr(ZahlWeiseStr,25),25);
            IF (RemoveAllSpaces(ZahlWeiseStr) = '') THEN
               KundenInfo^.SetCColor('36',8,8)
            ELSE
               KundenInfo^.SetCColor('36',8,6);
            TelefString := '';
            ZahlZString := '';
            IF (Telefon[1] <> '') THEN
               TelefString := PrintString('Tel.: '+Telefon[1],25);
            IF (ZahlWeise <> 0) THEN
               ZahlZString:=PrintString('Zahlungsziel: '+B2S(ZahlWeise,0),25);
            AdressString := PrintString(L2S(LiefPlz,0),6)+LiefOrt;
            AdressString := PrintString(AdressString,40);
            NamFeld:=PrintString(LeftStr(NamFeld,40),40);
            IF (Etage <> '') THEN
               LiefInfostring := PrintString('Etage: '+Etage,12)
            ELSE
               LiefInfoString := REPLICATE(' ',12);
            IF Aufzug THEN
               LiefInfostring := LiefInfostring + ' Aufzug';
            LiefInfoString := PrintString(LiefInfoString,25);
            IF LiefSZeit THEN
               LiefInfostring := LiefInfoString+'Innenstadtkunde';
            LiefInfoString:= PrintString(LiefInfoString,40);
         END;
         KlfdNr := Kunde.LfdNr;
         KundDB.BeginTransaction;
         Lauf := 0;
         WHILE (Lauf <= 5) AND KundDB.GetRec(1,MakeLfdNr(KlfdNr,Lauf),KHelp) DO BEGIN
            AndereStrassen[Lauf] := PrintString(KHelp.LiefStrasse,25);
            INC(Lauf);
         END;
         KundDB.EndTransAction;
         KHelp := Kunde;
         IF (Lauf <= 1) THEN BEGIN
            AndereStrassen[0]:='';
            AnschriftenTitel :='';
         END ELSE BEGIN
            AnschriftenTitel :=CenterString('Lieferadressen',25);
         END;
         FOR Lauf := 1 TO 8 DO
            Kunde.WegBes[Lauf]:= PrintString(Kunde.WegBes[Lauf],40);
         FOR Lauf := 1 TO 2 DO
            Kunde.Bemerkung[Lauf]:= PrintString(Kunde.Bemerkung[Lauf],40);
         OPString:= '';
         OPOffen := 0;
         OPGut := 0;
         OPSumme := 0;
         FOR Lauf := 1 TO 10 DO BEGIN
            IF (Kunde.OPosten[Lauf].Art = 'G') THEN BEGIN
               INC(OPGut);
               OPSumme := OPSumme - Kunde.Oposten[Lauf].Betrag;
            END;
            IF (Kunde.OPosten[Lauf].Art = 'O') THEN BEGIN
               INC(OPOffen);
               OPSumme := OPSumme + Kunde.Oposten[Lauf].Betrag;
            END;
         END;
         IF (OpGut <> 0) OR (OpOffen <> 0)  THEN BEGIN
            OPString := 'OP '+PrintLong(OpOffen,2)+'/'+PrintLong(OpGut,2)+
                        '  ';
            IF OPSumme > 0 THEN
                OpString:= OpString+'offen '+PrintReal(OPSumme,3,2,TRUE);
            IF OPSumme < 0 THEN
                OpString:= OpString+'Guthaben '+PrintReal(ABS(OPSumme),3,2,TRUE);
         END;
         OPString:= PrintString(OpString,25);
         Kunde.LiefStrasse := PrintString(Kunde.LiefStrasse,25);
         KundenInfo^.Refresh('');
         Kunde := KHelp;
      END;

   PROCEDURE ClearKundenInfo;
      BEGIN
         KundenInfo^.Hide;
      END;

   PROCEDURE DelStdPos(DelPos:BYTE;VAR Daten:TKundendaten);
      VAR Lauf : BYTE;
      BEGIN
         IF (DelPos < 5) THEN BEGIN
            FOR Lauf := DelPos+1 TO 5 DO BEGIN
                Daten.StdBest[Lauf-1]:= Daten.StdBest[Lauf];
            END;
         END;
         WITH Daten.StdBest[5] DO BEGIN
              Menge := 0;ArtNr := '';ArtBez := '';Preis := 0;
              Uebernehmen := TRUE;
         END;
      END;

   FUNCTION StdPosLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
       BEGIN
          MoreEval := FALSE;
          FocusID := KStdFenster^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS);
          DelStdPos(FocusFK,Kunde);
          KStdFenster^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          StdPosLoeschen := TRUE;
          Check := FALSE;
       END;
   PROCEDURE KKurzDrucken(Kunde:TKundendaten);
      VAR   Druckerstatus : LONGINT;
            DName : STRING;
            LogDatei:TEXT;
            HCounter : WORD;
            Lauf : WORD;
            LetztePos :WORD;
            NormalDruck,
            D: BOOLEAN;
      BEGIN
         DName := ActDate;
         DELETE(DName,6,1);
         DELETE(DName,3,1);
         DName := MakeFilePath(GMEnvObj^.GetEntry('LOGDIR'))+Dname+'.LOG';
         ASSIGN(LogDatei,Dname);
         IF SeekFile(DName) THEN
         {$I-}
            APPEND(LogDatei)
         {$I+}
         ELSE
         {$I-}
            REWRITE(LogDatei);
         {$I+}
         WITH Kunde DO BEGIN
            {$I-}
            WRITE(Logdatei,LeadingZeros(Lfdnr,5),'/');
            WRITE(Logdatei,LiefVorname,'/');
            WRITE(Logdatei,LiefName,'/');
            {WRITELN(Logdatei,Firma);}
            WRITE(Logdatei,'     ',LiefStrasse,'/',LiefOrt,'/');
            WRITELN(Logdatei,Telefon[1]);
            WRITELN(Logdatei);
            {$I+}
         END;
         {$I-}
            CLOSE(LogDatei);
         {$I+}
      END;

   FUNCTION ChangeKFenster:BOOLEAN;FAR;
      BEGIN
            IF (Kunde.LiefAnrede <> 3) THEN BEGIN
               KFenster^.Disable('21');
               KFenster^.Disable('22');
               KFenster^.Disable('23');
               Kunde.LiefApartner :='';
               Kunde.LiefAPAnrede :=1;
               Kunde.LiefAPTitel  :='';
               KFenster^.Enable('4');
            END ELSE BEGIN
               KFenster^.Enable('21');
               KFenster^.Enable('22');
               KFenster^.Enable('23');
               Kunde.LiefVorname :='';
               Kunde.LiefTitel :='';
               KFenster^.Disable('4');
            END;
            IF NOT KFenster^.Geschlossen THEN BEGIN
               KFenster^.Refresh('21');
               KFenster^.Refresh('22');
               KFenster^.Refresh('23');
               KFenster^.Refresh('4');
            END;
         ChangeKFenster := FALSE;
      END;

   PROCEDURE KFensterEdit;
      BEGIN
         KFenster^.Enable('');
         KFenster^.Disable('101');
         KFenster^.Disable('1');
         IF (NOT Aendern) AND (Kunde.LieferANr = 0) THEN BEGIN
            KFenster^.Enable('201');
            KFenster^.Enable('202');
         END ELSE BEGIN
            KFenster^.Disable('201');
            KFenster^.Disable('202');
         END;
      END;

   PROCEDURE KFensterKFeld;
      BEGIN
         KFenster^.Enable('101');
         KFenster^.SetFocusOnId('101')
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

   FUNCTION WegBesLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          Lauf : BYTE;
       BEGIN
          MoreEval := FALSE;
          FocusID := kFenster^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS)-50;
          IF (FocusFk <= 7) THEN
             FOR Lauf := FocusFK TO 7 DO
                Kunde.WegBes[Lauf] := Kunde.WegBes[Lauf+1];
          Kunde.WegBes[8] := '';
          InpString := Kunde.WegBes[FocusFk];
          KFenster^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          WegBesLoeschen := TRUE;
          Check := FALSE;
       END;

   FUNCTION BemerkLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          Lauf : BYTE;
       BEGIN
          MoreEval := FALSE;
          FocusID := kFenster^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS)-60;
          IF (FocusFk = 1) THEN
             Kunde.Bemerkung[1] := Kunde.Bemerkung[2];
          Kunde.Bemerkung[2] := '';
          InpString := Kunde.Bemerkung[FocusFk];
          KFenster^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          BemerkLoeschen := TRUE;
          Check := FALSE;
       END;

   FUNCTION OpostenLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          Lauf : BYTE;
          OPZeile1,
          OPZeile2 :STRING[80];
          LFOk : BOOLEAN;
       BEGIN
          MoreEval := FALSE;
          FocusID := KOpostenFenster^.GetFocusID;
          SplitString(FocusID,'_',FocusS,FocusR);
          FocusFK := S2B(FocusS);
          IF (Kunde.OPosten[FocusFK].Art <> #0) AND (Kunde.OPosten[FocusFK].Art <> ' ') THEN BEGIN
             OPGeaendert := TRUE;
          END;
          IF (FocusFk <= 9) THEN
             FOR Lauf := FocusFK TO 9 DO
                Kunde.Oposten[Lauf] := Kunde.Oposten[Lauf+1];
          WITH Kunde.Oposten[10] DO BEGIN
             Art :=#0;
             Betrag := 0;
             LiefNr :='';
             Bemerk := '';
          END;
          InpString := Kunde.WegBes[FocusFk];
          KOpostenFenster^.Refresh('');
          Taste := 0;
          Abbruch := FALSE;
          OpostenLoeschen := TRUE;
          Check := FALSE;
       END;


   FUNCTION KundeSpeichern(Valid:BOOLEAN):BOOLEAN;
      VAR Gespeichert : BOOLEAN;
          AllChanged : BOOLEAN;
          LEndCode :INTEGER;
          KHelp: TkundenDaten;
          LHelp: TLiefDaten;
          EndUebDatum: TDatum;
          AnfUebDatum: TDatum;
          KLaufIndexSTart,
          KLaufIndexEnd : STRING;
          KNRFeldWert :STRING[10];
          LSeekResult : BOOLEAN;
          UTagChanged : BOOLEAN;
          Lauf : BYTE;
          AndereZahlweise,
          AndereRechAdr : BOOLEAN;
          UpperStr :STRING[1];
          FMessString : STRING;

      BEGIN
         Gespeichert := FALSE;
         FMessString := 'Bitte erst fehlerhafte Eingabe korregieren';
         IF (NeueAnschrift) AND (Kunde.LieferANr = 0) THEN BEGIN
            FOR Lauf := 1 TO KFensterAnz DO
                Valid := Valid AND AlleKFenster[Lauf]^.Valid('');
            IF NOT Valid AND (Kunde.KWerbKz = '') THEN
              FMessString:= 'Sie m�ssen eine Werbekennzeichen eingeben';
            KFenster^.SetFocusOnId('201');
         END;
         IF Valid THEN BEGIN
            FOR Lauf := 1 TO 10 DO BEGIN
                Kunde.Oposten[Lauf].Art:= UPCASE(Kunde.Oposten[Lauf].Art);
            END;
            Kunde.KWerbKz := UPPER(Kunde.KWerbKz);
            IF (NeueAnschrift) OR (NOT Aendern) THEN BEGIN
               IF (Kunde.LieferANr = 0) AND (NOT KRAdrFenster^.Changed('')) THEN BEGIN
                  Kunde.RechAnrede := Kunde.LiefAnrede;
                  Kunde.RechName := Kunde.LiefName;
                  Kunde.RechTitel := Kunde.LiefTitel;
                  Kunde.RechVorname := Kunde.LiefVorname;
                  Kunde.RechStrasse := Kunde.LiefStrasse;
                  Kunde.RechPlz := Kunde.LiefPlz;
                  Kunde.RechOrt := Kunde.LiefOrt;
                  Kunde.RechAPAnrede := Kunde.LiefAPAnrede;
                  Kunde.RechAPTitel := Kunde.LiefAPTitel;
                  Kunde.RechAPartner := Kunde.LiefAPartner;
               END;
               Kunde.Firma := (Kunde.RechAnrede = 3);
               KundDB.BeginTransAction;
               { Nur bei echten Neukunden wird eine 
                 neue Kundennummer eingetragen }
               IF ((Kunde.LieferANr = 0) AND (Kunde.LfdNr = 0))THEN BEGIN
                    Kunde.LfdNr := KundDB.GetLfdNr +1;
               END;
               GeSpeichert :=KundDB.NewRec(Kunde);
               IF Gespeichert AND (Kunde.LieferAnr = 0) THEN BEGIN
                  WriteOPAenderung;
                  FOR Lauf := 1 TO 10 DO
                     OPSik[Lauf] := Kunde.Oposten[Lauf];
               END;
               IF Gespeichert THEN BEGIN
                 IF (Kunde.LfdNr > KundDB.GetLfdNr) THEN
                    KundDB.SetLfdNr(Kunde.LfdNr);
               END;
               KundDB.EndTransAction;
               IF Gespeichert THEN BEGIN
                  IF (Kunde.LieferANR = 0) THEN BEGIN
                     {BriefNeukunde;}
                  END ELSE BEGIN
                     InfoBox.Act (2,'Lieferadresse wurde angelegt.');
                  END;
                  KKurzDrucken(Kunde);
               END ELSE
                  FaultBox.Act (2,'Datensatz konnte nicht '+
                                    'gespeichert werden');
            END ELSE BEGIN
               Kunde.Firma := (Kunde.RechAnrede = 3);
               IF (FindKXPos(S2L(OldPq)) <> FindKXPos(S2l(Kunde.PlanQ))) THEN BEGIN
                  EndUebDAtum:= GetUebDatum(3);
                  AnfUebDatum:= GetUebDatum(1);
                  IF (EndUebDAtum = '') THEN
                     EndUebDatum:=GetUebDatum(2)
                  ELSE IF NOT Later(EndUebdatum,AnfUebDatum) THEN BEGIN
                     AnfUebdatum := EndUebDatum;
                     EndUebDatum := GetUebDatum(2);
                  END ELSE IF NOT Later(EndUebDatum,GetUebDatum(2)) THEN
                     EndUebDatum := GetUebDatum(2);
                  KLaufIndexStart:=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),AnfUebDatum,000);
                  KLaufIndexEnd:=MakeKundIndex(LeadingZeros(Kunde.LfdNr,5),EndUebDatum,999);
                  UTagChanged := FALSE;
                  KNRFeldWert := MakeLfdNr(Kunde.LfdNr,Kunde.LieferANr);
                  LSeekResult := FALSE;
                  IF LieferDB.BeginTransaction THEN BEGIN
                     LSeekResult:=LieferDB.StartIntervall(2,
                                 KLaufIndexStart,KLaufIndexEnd,LHelp);
                     LieferDB.EndTransaction;
                  END;
                  IF LSeekResult THEN BEGIN
                     REPEAT
                        IF (LHelp.Kundennummer = KNrFeldWert) OR
                           ((COPY(LHelp.Kundennummer,1,5) <> COPY(KNrFeldWert,1,5)) AND
                           (Kunde.LieferANr = 0)) THEN BEGIN
                           UTagChanged := TRUE;
                           SubFromKSum(LHelp);
                        END;
                        LieferDB.BeginTransaction;
                        LSeekResult := LieferDB.GetIntNext(LHelp);
                        LieferDB.EndTransaction
                     UNTIL NOT LSeekResult;
                  END;
               END;
               KundDB.BeginTransAction;
               Gespeichert :=KundDB.ChangeRec(Kunde);
               KundDB.EndTransAction;
               IF Gespeichert AND (Kunde.LieferAnr = 0) THEN BEGIN
                  WriteOPAenderung;
                  FOR Lauf := 1 TO 10 DO
                     OPSik[Lauf] := Kunde.Oposten[Lauf];
               END;
               IF (UTagChanged) AND(FindKXPos(S2L(OldPq)) <> FindKXPos(S2l(Kunde.PlanQ))) THEN BEGIN
                  LSeekResult := FALSE;
                  IF LieferDB.BeginTransaction THEN BEGIN
                     LSeekResult:=LieferDB.StartIntervall(2,
                                 KLaufIndexStart,KLaufIndexEnd,LHelp);
                     LieferDB.EndTransaction;
                  END;
                  IF LSeekResult THEN BEGIN
                     REPEAT
                        IF (LHelp.Kundennummer = KNrFeldWert) OR
                           ((COPY(LHelp.Kundennummer,1,5) <> COPY(KNrFeldWert,1,5)) AND
                           (Kunde.LieferANr = 0)) THEN BEGIN
                           AddToKSum(LHelp);
                        END;
                        LieferDB.BeginTransaction;
                        LSeekResult := LieferDB.GetIntNext(LHelp);
                        LieferDB.EndTransaction
                     UNTIL NOT LSeekResult;
                  END;
               END;
               IF Gespeichert THEN BEGIN
                  AllChanged := TRUE;
                  AndereRechAdr :=KRAdrFenster^.Changed('');
                  AndereZahlWeise :=KOpostenFenster^.Changed('99');
                  IF (Kunde.LieferANr = 0) AND (AndereRechAdr OR AndereZahlWeise) THEN BEGIN
                     KundDB.BeginTransAction;
                     Lauf := 1;
                     WHILE (Lauf <= 9) AND KundDb.GetRec(1,MakeLFdNr(Kunde.LfdNr,Lauf),KHelp) DO BEGIN
                        WITH KHelp DO BEGIN
                           IF AndereRechAdr THEN BEGIN
                              Firma := Kunde.Firma;
                              RechName := Kunde.RechName;
                              RechVorname := Kunde.RechVorname;
                              RechAnrede := Kunde.RechAnrede;
                              RechTitel := Kunde.RechTitel;
                              RechStrasse := Kunde.RechStrasse;
                              RechPlz := Kunde.RechPlz;
                              RechOrt := Kunde.RechOrt;
                              RechAPartner:= Kunde.RechAPartner;
                              RechAPTitel:= Kunde.RechAPTitel;
                              RechAPAnrede := Kunde.RechAPAnrede;
                           END;
                           IF AndereZahlWeise THEN BEGIN
                              Zahlweise := Kunde.Zahlweise;
                           END;
                        END;
                        AllChanged := KundDB.ChangeRec(KHelp) AND AllChanged;
                        INC(Lauf);
                     END;
                     KundDB.EndTransAction;
                  END;
               END;
               IF Gespeichert THEN BEGIN
                  InfoBox.Act (2,'Kunde wurde ge�ndert.');
                  KKurzDrucken(Kunde);
                  IF NOT AllChanged THEN BEGIN
                     FaultBox.Act (0,'Die Rechnungsadresse wurde nicht korrekt'+
                                     'auf die anderen Lieferanschriften �bertragen');
                  END;
               END ELSE
                  FaultBox.Act (2,'Datensatz konnte nicht '+
                                    'geaendert werden');
            END
         END ELSE BEGIN
            FaultBox.Act (2,FMessString);
         END;
         IF Gespeichert THEN BEGIN
            NeueAnschrift := FALSE;
            Aendern := TRUE;
         END;
         KundeSpeichern := Gespeichert;
      END;

   FUNCTION SeekKArt(VAR ArtNr:STRING; ArrayPos:WORD):BOOLEAN;FAR;
      VAR   L     : LONGINT;
            C     : INTEGER;
            Pr,
            Pf    : REAL;
            ArtNrSeek : STRING;
      BEGIN
         SeekKArt := FALSE;
         IF ArtNrSeek = '888' THEN
            ArtNrSeek := '99999'
         ELSE
            ArtNrSeek := MakeArtNr(S2L('01'+ArtNr));
         IF ArtDB.SeekRec(1,ArtNrSeek) THEN BEGIN
            WITH ArtDB.Rec.Artikel DO BEGIN
               IF (ArtBez <> '') THEN
                  Kunde.StdBest[ArrayPos].ArtBez := ArtBez;
               IF (ArtNr = '01908') THEN
                 Kunde.StdBest[ArrayPos].ArtBez:=
                 Kunde.StdBest[ArrayPos].ArtBez;
               Pr := PreisKa;
               IF Pr <> 0 THEN
                  Kunde.StdBest[ArrayPos].Preis := Pr;
               Pr := 0; PF:=0;
               SeekKArt := TRUE;
            END;
         END ELSE
            ArtNr :='';
         KStdFenster^.Refresh('');
      END;

   FUNCTION KStdFensterArtBezAction:BOOLEAN;FAR;
      VAR FocusId,
          FocusS,
          FocusR : STRING;
          FocusFK : BYTE;
          ArtBEzSik : STRING;
          NP  : REAL;
          NpPf:REAL;
          NPMark: LONGINT;
          AktArtNr : LONGINT;
          ArtBezGet : STRING;

      BEGIN
         FocusID := KStdFenster^.GetFocusID;
         SplitString(FocusID,'_',FocusS,FocusR);
         FocusFK := S2B(FocusS);
         WITH Kunde.StdBest[FocusFK] DO BEGIN
            IF (ArtNr ='990') AND (ArtBez <> '') AND ISNumber(ArtBez) THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr >= 301) AND (AktArtNr <= 495) THEN BEGIN
                  IF SeekKArt(ArtBezGet,FocusFK) THEN BEGIN
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
                     KStdFenster^.Refresh(LeadingZeros(FocusFk,2));
                  END ELSE
                     ArtBez := ArtBezSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  KStdFenster^.Refresh(LeadingZeros(FocusFk,2));
               END;
            END;
            IF (ArtNr ='997') AND (ArtBez <> '') AND ISNumber(ArtBez) THEN BEGIN
               AktArtNr :=S2l(ArtBez);
               ArtBezSik := ArtBez;
               ArtBezGet := ArtBez;
               IF (AktArtNr < 100) THEN BEGIN
                  IF SeekKArt(ArtBezGet,FocusFK) THEN BEGIN
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
                     KStdFenster^.Refresh(LeadingZeros(FocusFk,2));
                  END ELSE
                     ArtBez  := ArtBEzSik;
               END;
               IF ((ArtBezSik = ArtBez) AND IsNumber(ArtBez)) THEN BEGIN
                  WRITE(CHR(7));
                  ArtBez := '';
                  KStdFenster^.Refresh(LeadingZeros(FocusFk,2));
               END;
            END;
         END;
         KStdFensterArtBezAction := FALSE;
      END;

   FUNCTION WechselInKFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
          SeekResult : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := FALSE;
         IF (NOT KFenster^.Changed('')) OR
            Request.Act('Wollen Sie die Eingaben verwerfen ?') THEN BEGIN
            KFensterKFeld;
            KFenster^.Refresh('');
            KFenster^.SetFocusOnId('101');
            Taste := 0;
            Ergebnis:= TRUE;
            IF (NOT Aendern) THEN BEGIN
                InitTKundenDaten(Kunde);
                KFenster^.Refresh('');
                NeueAnschrift := FALSE;
                Aendern := TRUE;
            END ELSE BEGIN
               IF NeueAnschrift THEN BEGIN
                  KundDb.BeginTransaction;
                  SeekResult := KundDb.GetRec(1,MakeLfdNr(Kunde.LfdNr,Kunde.LieferANr-1),Kunde);
                  IF NOT SeekResult THEN
                     SeekResult := KundDb.GetRec(1,MakeLfdNr(Kunde.LfdNr,0),Kunde);
                  KundDB.EndTransaction;
                  IF NOT SeekResult THEN
                    InitTKundenDaten(Kunde);
                  KFenster^.Refresh('');
                  NeueAnschrift := FALSE;
                  Aendern := TRUE;
               END;
            END;
         END;
         Abbruch := FALSE;
         Check := FALSE;
         WechselInKFeld := Ergebnis;
      END;

   FUNCTION KFeldPlusMinus(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR SeekResult: BOOLEAN;
          SK : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         IF (RemoveAllSpaces(InpString) = '') THEN BEGIN
            KundDb.BeginTransaction;
            InpString:=L2S(KundDB.GetLfdNr,0);
            SeekResult := KundDB.GetRec(1,MakeLfdNr(S2l(InpString),0),Kunde);
            KundDb.EndTransaction;
         END ELSE BEGIN
            KundDb.BeginTransaction;
            IF Taste = 43 THEN BEGIN
               SeekResult := KundDb.GetNext(1,Kunde);
            END ELSE BEGIN
               SeekResult := KundDb.GetPrev(1,Kunde);
            END;
            KundDb.EndTransaction;
         END;
         IF NOT SeekResult THEN BEGIN
            WRITE(Chr(7));
         END ELSE
            InpString := L2S(Kunde.LfdNr,0);
         ChangeKFenster;
         KFenster^.Refresh('');
         Check := FALSE;
         Abbruch := FALSE;
         KFeldPlusMinus := FALSE;
      END;


   FUNCTION CRKFeld(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR SeekResult : BOOLEAN;
          Ergebnis: BOOLEAN;
          Lauf : BYTE;

      BEGIN
         MoreEval := FALSE;
         SeekResult := FALSE;
         KundDB.BeginTransaction;
         IF S2l(InpString) = Kunde.LfdNr THEN
            SeekResult := KundDb.GetRec(1,MakeLfdNr(Kunde.LfdNr,Kunde.LieferANr),Kunde);
         IF NOT SeekResult THEN
           SeekResult := KundDb.GetRec(1,MakeLfdNr(S2l(InpString),0),Kunde);
         KundDb.EndTransaction;
         Abbruch := FALSE;
         Check:= FALSE;
         Ergebnis:= FALSE;
         IF SeekResult THEN BEGIN
            OldPq := Kunde.PlanQ;
            KFensterEdit;
            KFenster^.SetFocusOnId('5');
            KFenster^.Refresh('');
            FOR Lauf := 1 TO 5 DO
               AlleKFenster[Lauf]^.Save('');
            Ergebnis := TRUE;
         END;
         CRKFeld := Ergebnis;
      END;

   FUNCTION SpeichernUndWechsel(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis,
          Gespeichert :BOOLEAN;
          Lauf : BYTE;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
         Gespeichert := KundeSpeichern(TRUE);
         IF Gespeichert THEN BEGIN
            FOR lauf := 1 TO 5 DO
               AlleKFenster[Lauf]^.Save('');
            Ergebnis:=WechselInKFeld(Taste,Nr,InpString,Abbruch,Check,MoreEval);
         END;
         SpeichernUndWechsel:= Ergebnis;
      END;

   FUNCTION AnlInfoEnde(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := KAnlInfoFenster^.Valid('');
         IF NOT Ergebnis THEN BEGIN
            FaultBox.Act(2,'Werbekennzeichen mu� eingeben werden');
            Taste := 0;
            Abbruch := FALSE;
            Check := FALSE;
         END ELSE BEGIN
            IF (Taste = 27) THEN BEGIN
               Abbruch := TRUE;
               Taste := 0;
            END ELSE
              Abbruch := FALSE;
            Check := FALSE;
         END;
         AnlInfoEnde:= Ergebnis;
      END;

   FUNCTION SpeichernUndEnde(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis,
          Gespeichert :BOOLEAN;
          Lauf : BYTE;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Check:= FALSE;
         Gespeichert := KundeSpeichern(TRUE);
         IF Gespeichert THEN BEGIN
            Abbruch :=TRUE;
            Ergebnis := TRUE;
            Saved := TRUE;
         END;
         SpeichernUndEnde:= Ergebnis;
      END;


   FUNCTION AndereFenster(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR EndCode : INTEGER;
      BEGIN
         EndCode :=KEditMenue^.Select(TRUE);
         MoreEval := TRUE;
         Taste := EndCode;
         Check := FALSE;
         Abbruch:= FALSE;
         AndereFenster := FALSE;
      END;


   FUNCTION KFensterValid:BOOLEAN;FAR;
      BEGIN
         KFensterValid := (Kunde.LieferAnr <>0) OR (NOT NeueAnschrift) OR
                         (Kunde.KWerbKz <> '');
      END;



   FUNCTION ChangeKRadrFenster:BOOLEAN;FAR;
      BEGIN
         IF (Kunde.RechAnrede <> 3) THEN BEGIN
            KRadrFenster^.Disable('21');
            KRadrFenster^.Disable('22');
            KRadrFenster^.Disable('23');
            Kunde.RechApartner :='';
            Kunde.RechAPAnrede :=1;
            Kunde.RechAPTitel  :='';
            KRadrFenster^.Enable('4');
         END ELSE BEGIN
            KRadrFenster^.Enable('21');
            KRadrFenster^.Enable('22');
            KRadrFenster^.Enable('23');
            Kunde.RechVorname :='';
            Kunde.RechTitel :='';
            KRadrFenster^.Disable('4');
         END;
         IF NOT KRAdrFenster^.Geschlossen THEN BEGIN
            KRadrFenster^.Refresh('21');
            KRadrFenster^.Refresh('22');
            KRadrFenster^.Refresh('23');
            KRadrFenster^.Refresh('4');
         END;
         ChangeKRAdrFenster := FALSE;
      END;

   FUNCTION GetKAdr(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;FAR;
      BEGIN
         KundDB.BEginTransaction;
         KundDB.GetRec(1,MakeLfdNr(Kunde.LfdNr,(Nr-1)),Kunde);
         OldPq := Kunde.PlanQ;
         KunddB.EndTransaction;
         NeueAnschrift := FALSE;
         GetKAdr:= FALSE;
      END;

   FUNCTION NewKAdr(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;FAR;

      BEGIN
         Kunde.LieferANr:= (Nr-1);
         NeueAnschrift:= TRUE;
         NewKAdr:= FALSE;
      END;

   FUNCTION ChangeKundenLAdr:BOOLEAN;
      VAR   KAuswahlMenue : TAuswahlMenuePtr;
            AnzAnschr     : BYTE;
            EndCode : INTEGER;
            KHelp : TKundendaten;
      BEGIN
         ChangeKundenLAdr := FALSE;
         AnzAnschr := 0;
         IF KundDB.BeginTransaction THEN BEGIN
            IF KundDB.StartIntervall(1,MakeLfdNr(Kunde.LfdNr,0),MakeLfdNr(Kunde.LfdNr,9),KHelp) THEN BEGIN
               NEW(KAuswahlMenue);
               KAuswahlMenue^.Init(15,7,2,'Adressen','ASIN2',7,2);
               REPEAT
                  KAuswahlMenue^.MenuItem('',Khelp.LiefStrasse,GetKAdr);
                  INC(AnzAnschr);
               UNTIL NOT KundDB.GetIntNext(KHelp);
               KundDB.EndTransaction;
               IF (AnzAnschr < 10) THEN
                  KAuswahlMenue^.MenuItem('','Neue Anschrift',NewKAdr);
               Endcode := KAuswahlMenue^.Select(TRUE);
               ChangeKundenLAdr := (Endcode <> 27);
               DISPOSE(KAuswahlMenue);
            END ELSE
               KunddB.EndTransaction;
         END;
      END;

   FUNCTION AndereAdresse(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Gespeichert : BOOLEAN;
          Changed : BOOLEAN;
          Lauf : BYTE;
      BEGIN
         MoreEval := FALSE;
         Changed := FALSE;
         FOR Lauf := 1 TO KFensterAnz DO
            Changed := Changed OR AlleKFenster[Lauf]^.Changed('');
         Gespeichert := Aendern AND (NOT Changed);
         IF (Aendern AND Changed AND Request.Act('Achtung !!! Die '+
                    'aktuellen Aenderungen wurden nicht gesichert '+
                                'sind sie sicher das sie eine andere Anschrift '+
                                'bearbeiten wollen.'))
            OR (NOT Aendern AND
            Request.Act('Zum Anlegen einer zus�tzlichen Adresse'+
                          ' mu� erst eine Adresse angelegt werden.'+
                          ' Soll die aktuelle Eingabe als Standardadresse.'+
                          ' gespeichert werden?')) THEN BEGIN
            Gespeichert := KundeSpeichern(TRUE);
         END;
         IF Gespeichert THEN BEGIN
            IF ChangeKundenLAdr THEN BEGIN
               KFenster^.Refresh('');
               FOR Lauf := 1 TO KFensterAnz DO
                  AlleKFenster[Lauf]^.Save('');
            END;
         END;
         Abbruch :=FALSE;
         Taste := 0;
         Check := FALSE;
         AndereAdresse := TRUE;
      END;

   FUNCTION ZuLieferschein(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
          Lauf : BYTE;
          Changed : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Changed := FALSE;
         FOR Lauf := 1 TO KFensterAnz DO
            Changed := Changed OR AlleKFenster[Lauf]^.Changed('');
         IF (KFenster^.GetFocusId <> '101') AND Changed THEN BEGIN
            Ergebnis := Request.Act('Eingaben verwerfen ?');
            IF Ergebnis THEN BEGIN
               FOR Lauf := 1 TO KFensterAnz DO
                   AlleKFenster[Lauf]^.Restore('');
            END;
         END ELSE
            Ergebnis := TRUE;
         Abbruch := Ergebnis;
         ZuLieferschein:= TRUE;
         Check := FALSE;
         Taste := 0;
      END;

   FUNCTION AdresseLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Lauf : BYTE;
          Changed : BOOLEAN;
          LFehler : BOOLEAN;
          LoeschNr : BYTE;
          LAdrGefunden : BOOLEAN;
          LoeschLauf:BYTE;
          LoeschKnr : LONGINT;
      BEGIN
         MoreEval := FALSE;
         Abbruch := FALSE;
         Changed := FALSE;
         FOR Lauf := 1 TO KFensterAnz DO
           Changed := Changed OR AlleKFenster[Lauf]^.Changed('');
         IF Aendern THEN BEGIN
            IF (Kunde.LieferANr <> 0) THEN BEGIN
               IF NOT NeueAnschrift THEN BEGIN
                  IF Request.Act('Wollen sie die Lieferanschrift '+Kunde.LiefStrasse+
                                 ' des Kunden '+LeadingZeros(Kunde.LfdNR,5)+
                                 ' wirklich l�schen ?') THEN BEGIN
                     LFehler := FALSE;
                     LoeschNr:= Kunde.LieferANr;
                     LoeschKNr := Kunde.LfdNr;
                     KundDB.BeginTransAction;
                     LFehler := NOT KundDB.DelRec(MakeLfDNr(LoeschKNr,LoeschNr));
                     IF NOT LFehler THEN BEGIN
                        InfoBox.Act(2,'Lieferanschrift wurde gel�scht. ');
                        IF (LoeschNr < 9) THEN BEGIN
                           LoeschLauf := 9;
                           LAdrGefunden := FALSE;
                           WHILE LoeschLauf > LoeschNr DO BEGIN
                              IF KundDb.GetRec(1,MakeLfdNr(LoeschKnr,LoeschLauf),Kunde) THEN BEGIN
                                 Kunde.LieferANr := LoeschNr;
                                 LFehler:=NOT KundDb.NewRec(Kunde);
                                 IF NOT LFehler THEN
                                    LFehler:=NOT KundDb.DelRec(MakeLfdNr(LoeschKNr,LoeschLauf));
                                 IF NOT LFehler THEN
                                    InfoBox.Act(2,'Lieferanschrift '+B2S(LoeschLauf,0)+
                                                'wurde als neue Lieferanschrift '+B2S(LoeschNr,0)+
                                                ' gespeichert');
                                 LoeschLauf := LoeschNr;
                                 FOR Lauf := 1 TO KFensterAnz DO
                                     AlleKFenster[Lauf]^.Save('');
                                 Changed := FALSE;
                                 LadrGefunden := TRUE;
                              END ELSE
                                 DEC(LoeschLauf);
                           END;
                           IF NOT LAdrGefunden THEN BEGIN
                              Kunddb.GetRec(1,MakeLfdNr(LoeschKnr,(LoeschNr-1)),Kunde);
                              FOR Lauf := 1 TO KFensterAnz DO
                                 AlleKFenster[Lauf]^.Save('');
                              Changed := FALSE;
                           END;
                        END;
                     END;
                     KundDB.EndTransAction;
                  END;
               END
            END ELSE BEGIN
               IF Request.Act('Wollen sie den Kunden '+LeadingZeros(Kunde.LfdNR,5)+
                  ' - '+Kunde.RechName+
                  ' wirklich l�schen ?') THEN BEGIN
                  KundDB.BeginTransAction;
                  LoeschLauf := 9;
                  LoeschKNr := Kunde.LfdNr;
                  LFehler := FALSE;
                  FOR LoeschLauf := 9 DOWNTO 0 DO BEGIN
                     IF KundDb.GetRec(1,MakeLfdNr(LoeschKnr,LoeschLauf),Kunde) THEN BEGIN
                        IF NOT Kunddb.DelRec(MakeLfdNr(LoeschKnr,LoeschLauf)) THEN BEGIN
                           FaultBox.Act(0,'Fehler beim L�schen von '+MakeLfdNr(LoeschKnr,LoeschLauf));
                           LFehler := TRUE;
                        END
                     END;
                  END;
                  IF NOT LFehler THEN
                     InfoBox.Act(2,'Kunde '+LeadingZeros(LoeschKNR,5)+' wurde gel�scht');
                  KundDB.EndTransAction;
                  Abbruch := TRUE;
               END;
            END;
         END;
         AdresseLoeschen :=TRUE;
         Check := FALSE;
         Taste := 0;
      END;


 { ********************************************************************
   *  Kunden bearbeiten
   *********************************************************************}

   FUNCTION DOKEdit(VAR KLfd:STRING;VAR KLFDNo:LONGINT):BOOLEAN;

      VAR   FensterNr,
            Lauf      : BYTE;
            Ergebnis  :INTEGER;
            Ende      :BOOLEAN;
            LFehler : BOOLEAN;
            LoeschNr : BYTE;
            LAdrGefunden : BOOLEAN;
            LoeschLauf:BYTE;
            LoeschKnr : LONGINT;
            ChangeTxt : STRING;
            LastFocus : BYTE;
            LEndCode :INTEGER;
            Dateizugriff,
            Valid,
            PRESSED,
            Changed    : BOOLEAN;

      BEGIN
         OPGeaendert := FALSE;
         FOR Lauf := 1 TO 10 DO BEGIN
            OPSik[Lauf] := Kunde.Oposten[Lauf];
         END;
         KFensterEdit;
         Ergebnis := 27;
         Ende := FALSE;
         PRESSED := FALSE;
         InKundenFeld := FALSE;
         FOR Lauf := 1 TO KFensterAnz DO
           AlleKFenster[Lauf]^.Save('');
         ChangeKFenster;
         ChangeKRadrFenster;
         LastFocus:= 1;
         Saved := FALSE;
         WHILE NOT Ende DO BEGIN
            KFenster^.SetFocusField(LastFocus);
            Ergebnis:=KFenster^.Input;
            LastFocus :=KFenster^.GetFocusField;
            Ende := KFenster^.Abbruch OR Ende;
         END;
         IF Saved THEN BEGIN
            KlfdNo := Kunde.LfdNr;
            Klfd   := LeadingZeros(KLfdNo,5);
         END;
         KFenster^.Hide;
      END;
 { ********************************************************************
   *  Kunden neuanlegen
   *********************************************************************}

   PROCEDURE DOKneu;

      VAR   FensterNr,
            Lauf      : BYTE;
            Ergebnis  :INTEGER;
            Ende      :BOOLEAN;
            Dateizugriff,
            Valid,
            PRESSED,
            Changed    : BOOLEAN;

      BEGIN
         FensterNr :=1;
         Ergebnis := 27;
         Ende := FALSE;
         InitTKundenDaten(Kunde);
         Kunde.LiefPlz :=42;
         Kunde.LiefOrt := 'Wuppertal';
         KundDB.BeginTransAction;
         IF (KLfdNo = 0) THEN
            Kunde.LfdNr := 0
         ELSE
            Kunde.LfdNr := KlfdNo;
         KundDb.EndTransaction;
         Kunde.AnlDatum:= ActDate;
         Kunde.AnlUhrzeit:= ActTime;
         NeueAnschrift := TRUE;
         Aendern := FALSE;
         DOKEdit(KLfd,KLFdNo);
      END;

   PROCEDURE DOKAender;
      VAR   Ergebnis : INTEGER;
            AnzReg : BYTE;
            KLfd   : STRING;
            KLfdNo : LONGINT;

      BEGIN
         Klfd :='';
         KlfdNo :=0;
         NeueAnschrift := FALSE;
         OldPq := Kunde.PlanQ;
         Aendern := TRUE;
         DoKEdit(Klfd,KlfdNo);
      END;


   FUNCTION EditRechAdresse:INTEGER;
      VAR Ergebnis : INTEGER;
      BEGIN
         IF (Kunde.LieferANr <> 0) THEN BEGIN
            KRAdrFenster^.Show('');
            REPEAT
               Ergebnis := InputKey;
               IF (Ergebnis <> 27) THEN
                  WRITE(CHR(7));
            UNTIL (Ergebnis = 27);
            Ergebnis := 0;
         END ELSE BEGIN
            KRaDrFenster^.SetFocusField(1);
            Ergebnis := KRAdrFenster^.Input;
         END;
         KRadrFenster^.Hide;
         EditRechAdresse := Ergebnis;
      END;

   TYPE TStdBest     = RECORD
                          Menge       : LONGINT;
                          ArtNr       : STRING[5];
                          ArtBez      : STRING[40];
                          Preis       : REAL;
                          Uebernehmen : BOOLEAN;
                       END;

   PROCEDURE PrepareStdFenster;
      VAR Lauf :BYTE;
      BEGIN
         FOR Lauf := 1 TO 5 DO BEGIN
           WITH Kunde.StdBest[Lauf] DO
              IF (Menge = 0) AND (ArtNr = '') AND (ArtBez= '') AND (Preis = 0) THEN
                 Uebernehmen := TRUE;
         END;
      END;

   PROCEDURE SaveStdFenster;
      VAR Lauf :BYTE;
          Lauf2 : BYTE;
         OneFound  : BOOLEAN;
      BEGIN
         OneFound := FALSE;
         FOR Lauf := 5 DOWNTO 1 DO BEGIN
            WITH Kunde.StdBest[Lauf] DO BEGIN
               IF (Menge = 0) AND (ArtNr = '') AND (ArtBez= '') AND (Preis = 0) THEN BEGIN
                  IF OneFound AND (Lauf < 5) THEN BEGIN
                     FOR Lauf2 := Lauf TO 4 DO
                        Kunde.StdBest[Lauf2] := Kunde.StdBest[Lauf2+1];
                     Kunde.StdBest[5].Menge := 0;
                     Kunde.StdBest[5].ArtNr := '';
                     Kunde.StdBest[5].ArtBez:= '';
                     Kunde.StdBest[5].Preis:= 0;
                     Kunde.StdBest[5].Uebernehmen:= FALSE;
                  END ELSE
                     Uebernehmen := FALSE;
               END ELSE BEGIN
                  OneFound := TRUE;
               END;
            END
         END;
      END;

   FUNCTION EditStdFenster:INTEGER;
      VAR Ergebnis :INTEGER;
      BEGIN
          KStdFenster^.SetFocusField(1);
          PrepareStdFenster;
          Ergebnis :=KStdFenster^.Input;
          KStdFenster^.Hide;
          SaveStdFenster;
          EditStdFenster := Ergebnis;
      END;

   FUNCTION EditOpostenFenster:INTEGER;
      VAR Ergebnis:INTEGER;
      BEGIN
          IF (Kunde.LieferAnr = 0) THEN
             KOpostenFenster^.Enable('99')
          ELSE
             KOpostenFenster^.Disable('99');
          KOpostenFenster^.SetFocusField(1);
          Ergebnis :=KOpostenFenster^.Input;
          KOPostenFenster^.Hide;
          EditOpostenFenster := Ergebnis;
      END;

   FUNCTION EditAnlInfo:INTEGER;
      VAR Ergebnis : INTEGER;
      BEGIN
         IF (Kunde.LieferANr <> 0) THEN BEGIN
            KAnLInfoFenster^.Show('');
            REPEAT
               Ergebnis := InputKey;
               IF (Ergebnis <> 27) THEN
                  WRITE(CHR(7));
            UNTIL (Ergebnis = 27);
            Ergebnis := 0;
         END ELSE BEGIN
            KAnlInfoFenster^.SetFocusField(1);
            Ergebnis:=KAnlInfoFenster^.Input;
         END;
         KAnlInfoFenster^.Hide;
         EditAnlInfo:= Ergebnis;
      END;

   FUNCTION EditZusatzFenster(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;FAR;
      VAR Ende : BOOLEAN;
          Ergebnis : INTEGER;
      BEGIN
         Ende := FALSE;
         CASE Nr OF
            1 : Taste:=EditRechAdresse;
            2 : Taste:=EditStdFenster;
            3 : Taste:=EditOPostenFenster;
            4 : Taste:=EditAnlInfo;
         END;
         EditZusatzFenster := FALSE;
      END;

   FUNCTION G_OFeldEval(VAR Eingabe:STRING;VAR InputCode :INTEGER;VAR CPos:Byte):BOOLEAN;FAR;
      VAR FocusID: STRING[10];
          FocusS,FocusR : STRING;
          FocusFK : BYTE;
          WString : STRING[40];
          BinfoPos : BYTE;
          BemChanged : BOOLEAN;
      BEGIN
         IF (InputCode = 32) OR
            (InputCode = 71) OR
            (InputCode = 103) OR
            (InputCode = 79) OR
            (InputCode = 111) THEN BEGIN
           InputCode:= ORD(UpCase(CHR(InputCode)));
           IF ((InputCode = 71) OR (InputCode = 79))THEN BEGIN
              FocusID := KOpostenFenster^.GetFocusID;
              SplitString(FocusID,'_',FocusS,FocusR);
              FocusFK := S2B(FocusS);
              BemChanged := FALSE;
              WString :=Kunde.Oposten[FocusFK].Bemerk;
              IF (WString= '') THEN BEGIN
                 IF (InputCode = 71) THEN
                    WString := 'Guthaben vom '
                 ELSE
                    WString := 'offen vom ';
                 BemChanged := TRUE;
              END ELSE BEGIN
                 IF (InputCode = 71) THEN BEGIN
                    BInfoPos := POS('offen vom ',WString);
                    IF (BInfoPos <> 0) THEN BEGIN
                       DELETE(WString,BInfoPos,LENGTH('offen vom '));
                       INSERT('Guthaben vom ',WString,BInfoPos);
                       BemChanged := TRUE;
                    END;
                 END ELSE BEGIN
                    BInfoPos := POS('Guthaben vom ',WString);
                    IF (BInfoPos <> 0) THEN BEGIN
                       DELETE(WString,BInfoPos,LENGTH('Guthaben vom '));
                       INSERT('offen vom ',WString,BInfoPos);
                       BemChanged := TRUE;
                    END;
                 END;
              END;
              IF BemChanged THEN BEGIN
                 Kunde.Oposten[FocusFK].Bemerk := WString;
                 KOpostenFenster^.Refresh(FocusS);
              END;

           END;
         END ELSE BEGIN
            IF ((InputCode >= 32) AND (InputCode <= 125)) OR (InputCode=129) OR (InputCode=132)
                  OR (InputCode=142) OR (InputCode=148) OR (InputCode=153) OR (InputCode=154) THEN
               InputCode := 0;
         END;
         G_OFeldEval := FALSE;
      END;

   FUNCTION WKZFeldEval(VAR Eingabe:STRING;VAR InputCode :INTEGER;VAR CPos:Byte):BOOLEAN;FAR;
      BEGIN
         IF (InputCode > 0) THEN
            InputCode := ORD(UPCASE(CHR(InputCode)));
         WKzFeldEval := FALSE;
      END;


   PROCEDURE InitKundenInput;
      VAR Lauf : BYTE;


      BEGIN
         NEW(KEditMenue);
         KEditMenue^.Init(15,7,2,'Zusatzinformationen','KINFO',7,2);
         WITH KEditMenue^ DO BEGIN
            MenuItem('1','Rechnungsadresse',EditZusatzFenster);
            MenuItem('2','Standardbestellung',EditZusatzFenster);
            MenuItem('3','Offene Posten',EditZusatzFenster);
            MenuItem('4','AnlageInformationen',EditZusatzFenster);
            AddActionKey('_',27,ButtonBreak0,FALSE,'Zur�ck ins Hauptfenster');
            SetFKeyQuit(FALSE);
         END;
         ValTrue := TRUE;
         NamFeld :='';
         WITH Kunde DO BEGIN
            NEW(KundenInfo);
            KundenInfo^.Init(15,220,625,470,6,'Kunde - ','',Aktiv);
            WITH KundenInfo^  DO BEGIN
               AddHLine  ('001',0,40,4,0);
               AddVLine  ('002',41,0,13,0);
               AddHLine  ('003',0,73,14,0);
               AddConst  ('004',16,4,' Weg ');
               AddCrossPoint('005',41,14,2);
               AddCrossPoint('005',41,10,4);
               AddHLine('005',42,73,10,0);
               AddCrossPoint('005',41,4,5);
               SetCColor ('004',8,6);
               SetCHeight('001',1);
               SetTitelNr(LfdNr);
               AddCString('01',0,0,NamFeld);
               AddCString('02',0,1,LiefStrasse);
               AddCString('03',0,2,AdressString);
               AddCString('04',0,3,ApartnerString);
               AddCString('05',0,5,LiefInfostring);
               FOR Lauf := 1 TO 8 DO
                  AddCString('10',0,Lauf+5,WegBes[Lauf]);
               AddCString('08',0,15,Bemerkung[1]);
               AddCString('09',0,16,Bemerkung[2]);
               AddCString('10',45,15,TelefString);
               AddCString('11',45,16,ZahlZString);
               AddCString('12',45,11,OpString);
               AddCString('13',45,12,HSStr);
               AddCString('36',45,13,ZahlWeiseStr);
               SetCWidth('36',1);
               SetCHeight('36',1);
               SetCColor ('36',8,6);
               AddCstring('37',45,0,AnschriftenTitel);
               SetCColor ('37',8,6);
               SetCHeight('37',2);
               FOR Lauf := 0 TO 5 DO
                  AddCString('13',45,Lauf+2,AndereStrassen[Lauf]);
            END;
            NEW(KFenster);
            KFenster^.Init(60,20,540,435,0,'Personendaten','KINFO1',Aktiv);
            WITH KFenster^ DO BEGIN
               AddLongInt('', '101','Kundennummer:',1,1,LfdNr,'',5,0,99999);
               AlignFields;
               AddByte('', '1','LieferAnschrift:',25,1,LieferAnr,'',5,0,9);
               AlignFields;
               AddString ('', '5','Name:',1,2,LiefName,'',40);
               AlignFields;
               AddString ('', '4','Vorname:',1,3,LiefVorname,'',20);
               AlignFields;
               AddChoice ('', '3','Anrede:',1,4,LiefAnrede,'',5,'');
               AddItem('3',Anr[1]);
               AddItem('3',Anr[2]);
               AddItem('3',Anr[3]);
               AlignFields;
               AddString ('', '4','Titel:',25,4,LiefTitel,'',20);
               AlignFields;
               AddString ('', '7','Strasse:',1,5,LiefStrasse,'',25);
               AlignFields;
               AddLongInt('', '8','Plz:   ',1,6,LiefPLZ,'',5,0,99999);
               AlignFields;
               AddString ('', '9','Ort:',20,6,LiefOrt,'',25);
               AlignFields;
               AddChoice ('', '21','Ansprechpartner:',1,7,LiefAPAnrede,'',5,'');
               AddItem('21',Anr[1]);
               AddItem('21',Anr[2]);
               AlignFields;
               AddString ('', '22','',25,7,LiefAPartner,'',20);
               AlignFields;
               AddString ('', '23','  Titel:',1,8,LiefAPTitel,'',20);
               AlignFields;
               AddString('','10','Telefon:',1,9,Telefonbem[1],'',20); AlignFields;
               AlignFields;
               AddString('','11',':',32,9,Telefon[1],'',15);         AlignFields;
               AlignFields;
               AddString('','12','Telefon:',1,10 ,Telefonbem[2],'',20);AlignFields;
               AlignFields;
               AddString('','13',':',32,10,Telefon[2],'',15);        AlignFields;
               AlignFields;
               AddString('','14','Telefon:',1,11 ,Telefonbem[3],'',20);AlignFields;
               AlignFields;
               AddString('','15',':',32,11 ,Telefon[3],'',15);
               AlignFields;
               AddHLine('1',1,54,12,0);
               AlignFields;
               AddString ('','16','P-Quadrat:',1,13,PlanQ,'',5);
               AlignFields;
               AddString ('','17','P-Quadrat-Falk:',20,13,PlanQFalk,'',10);
               AlignFields;
               AddString('','25','Etage:',1,14,Etage,'',2);
               AlignFields;
               AddSwitch('','26','Innenstadtkunde:',20,14,LiefSZeit,'');
               AlignFields;
               AddSwitch('','27','Aufzug:',1,15,Aufzug,'');
               AlignFields;
               AddString('','201','Geworben:',20,15,KWerbKz,'',2);
               SetSevalFunc('201',WkzFeldEval);
               AlignFields;
               AddString('','202','',34,15,KWerbText,'',20);
               AlignFields;
               AddString('','51','Weg:',1,16,WegBes[1],'',40);
               AddString('','52','    ',1,17,WegBes[2],'',40);
               AddString('','53','    ',1,18,WegBes[3],'',40);
               AddString('','54','    ',1,19,WegBes[4],'',40);
               AddString('','55','    ',1,20,WegBes[5],'',40);
               AddString('','56','    ',1,21,WegBes[6],'',40);
               AddString('','57','    ',1,22,WegBes[7],'',40);
               AddString('','58','    ',1,23,WegBes[8],'',40);
               AlignFields;
               AddString('','61','Bemerkungen:',1,24,Bemerkung[1],'',40);
               AddString('','62','            ',1,25,Bemerkung[2],'',40);
               AlignFields;
               Disable('1');
               AlignFields;
               SetActionFunc('_',ChangeKFenster);
               AddActionKey('',42,WechselInKFeld,FALSE,'Kundennummer �ndern');
               AddActionKey('_',-67,SpeichernUndWechsel,FALSE,
                                'Speichern und zu Kundennummer');
               AddActionKey('_',-68,SpeichernundEnde,FALSE,
                                'Speichern und zur�ck in Lieferschein');
               AddActionKey('_',-63,AndereFenster,FALSE,'Andere Informationen eingeben');
               AddActionKey('_',-60,AndereAdresse,FALSE,'Andere Adresse bearbeiten');
               AddActionKey('_',-62,AdresseLoeschen,FALSE,'Lieferanschrift l�schen');
               AddActionKey('_',27,ZuLieferschein,FALSE,'Ohne Speichern in Lieferschein');
               FOR Lauf := 51 TO 58 DO
                  AddActionKey(B2s(Lauf,0),-62,WegBesLoeschen,FALSE,'Aktuelle Zeile l�schen');
               FOR Lauf := 61 TO 62 DO
                  AddActionKey(B2s(Lauf,0),-62,BemerkLoeschen,FALSE,'Aktuelle Zeile l�schen');

               AddActionKey('101',42,DoNothing,FALSE,'');
               AddActionKey('101',-68,DoNothing,FALSE,'');
               AddActionKey('101',-67,DoNothing,FALSE,'');
               AddActionKey('101',-66,DoNothing,FALSE,'');
               AddActionKey('101',-65,DoNothing,FALSE,'');
               AddActionKey('101',-64,DoNothing,FALSE,'');
               AddActionKey('101',-63,DoNothing,FALSE,'');
               AddActionKey('101',-62,DoNothing,FALSE,'');
               AddActionKey('101',-61,DoNothing,FALSE,'');
               AddActionKey('101',-60,DoNothing,FALSE,'');
               AddACtionKey('101',-80,DoNothing,FALSE,'');
               AddACtionKey('101',-72,DoNothing,FALSE,'');
               AddACtionKey('101',9,DoNothing,FALSE,'');
               AddACtionKey('101',-15,DoNothing,FALSE,'');
               AddActionKey('101',13,CrKFeld,FALSE,'Kunden �ndern');
               AddActionKey('101',45,KFeldPlusMinus,FALSE,'Vorherige Kundennummer');
               AddActionKey('101',43,KFeldPlusMinus,FALSE,'N�chste Kundennummer');
               SetCheckFunc(KFensterValid);
               SetFKeyQuit(FALSE);
               SetCrQuit(FALSE);
            END;

            NEW(KRAdrFenster);
            KRAdrFenster^.Init(60,165,540,325,0,'Rechnungsadresse','KINFO2',Aktiv);
            WITH KRAdrFenster^  DO BEGIN
               AddString ('', '5','Name:',1,1,RechName,'',40);
               AddString ('', '4','Vorname:',1,2,RechVorname,'',20);
               AlignFields;
               AddChoice ('', '3','Anrede:',1,3,RechAnrede,'',5,'');
               AddItem('3',Anr[1]);
               AddItem('3',Anr[2]);
               AddItem('3',Anr[3]);
               AlignFields;
               AddString ('', '4','Titel:',25,3,RechTitel,'',20);
               AlignFields;
               AddString ('', '7','Strasse:',1,4,RechStrasse,'',25);
               AlignFields;
               AddLongInt('', '8','Plz:   ',1,5,RechPLZ,'',5,0,99999);
               AlignFields;
               AddString ('', '9','Ort:',20,5,RechOrt,'',25);
               AlignFields;
               AddChoice ('', '21','Ansprechpartner:',1,6,RechAPAnrede,'',5,'');
               AddItem('21',Anr[1]);
               AddItem('21',Anr[2]);
               AlignFields;
               AddString ('', '22','',25,6,RechAPartner,'',20);
               AlignFields;
               AddString ('', '23','  Titel:',1,7,RechAPTitel,'',20);
               AlignFields;
               SetActionFunc('_',ChangeKRadrFenster);
               AddActionKey('_',27,ButtonBreak0,FALSE,'Zur�ck ins Hauptfenster');
               AddActionKey('_',-68,ButtonMoreEval,FALSE,'Speichern und in Lieferschein');
               AddActionKey('_',-67,ButtonMoreEval,FALSE,'Speichern und in Kundennummer');
               SetFKeyQuit(FALSE);
               SetCRQuit(FALSE);
            END;

            NEW(KAnlInfoFenster);
            KAnlInfoFenster^.Init(100,130,500,335,0,'Anlageinformationen','KINFO3',Aktiv);
            WITH KAnlInfoFenster^  DO BEGIN
               AddDate('','1','',1,1,AnlDatum,'');
               AlignFields;
               AddTime('','1','',30,1,AnlUhrzeit,'');
               AlignFields;

               AlignFields;
               FOR Lauf := 1 TO 4 DO BEGIN
                  AddString('',B2S(Lauf+3,0),'Gespr�ch '+B2s(Lauf,0)+':',
                              1,Lauf+3,GInfo[Lauf],'',3);
               END;
               AddLongInt('','10','Angesprochen:',1,8,Angespr,'',4,0,9999);
               AlignFields;
               AddLongInt('','11','Lief. i.o.:',25,8,LiefOk,'',4,0,9999);
               AlignFields;
               AddLongInt('','12','Nicht bel.:',1,9,LiefNOk,'',4,0,9999);
               AlignFields;
               AddLongInt('','13','Eigenversch.:',25,9,LiefEigen,'',4,0,9999);
               AlignFields;
               Disable('1');
               AddActionKey('_',27,AnlInfoEnde,TRUE,'Zur�ck ins Hauptfenster');
               AddActionKey('_',-68,AnlInfoEnde,TRUE,'Speichern und in Lieferschein');
               AddActionKey('_',-67,AnlInfoEnde,TRUE,'Speichern und in Kundennummer');
               SetFKeyQuit(FALSE);
               SetCrQuit(FALSE);
            END;

            NEW(KOpostenFenster);
            KOpostenFenster^.Init(60,100,580,355,0,'Offene Posten','KINFO5',Aktiv);
            WITH KOpostenFenster^ DO BEGIN
               AddWord('','99','Zahlungweise:',1,1,Zahlweise,'',4,0,9999);
               AddConst('0',1,2,' LiefNr Typ Betrag  Bemerkungen');
               FOR Lauf := 1 TO 10 DO BEGIN
                  AddTabZeile('','0'+B2S(Lauf,0),'',1,Lauf+4,ValTrue,'');
                  WITH Oposten[Lauf] DO BEGIN
                     AddNumText('0'+B2S(Lauf,0),'1','',1,Lauf+2,LiefNr,'',5);
                     AddChar('0'+B2S(Lauf,0),'2','',8,Lauf+2,Art,'');
                     AddReal('0'+B2S(Lauf,0),'3','',11,Lauf+2,Betrag,'',7,2,-999.99,999.99);
                     AddString('0'+B2S(Lauf,0),'4','',20,Lauf+2,Bemerk,'',40);
                     AddActionKey('0'+B2s(Lauf,0),-62,OpostenLoeschen,FALSE,'Aktuelle Zeile l�schen');
                  END;
                  SetSEvalFunc('0'+B2S(Lauf,0)+'_2',G_OFeldEval);
               END;
               FOR Lauf := 1 TO 10 DO BEGIN
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-72,UpToSTab,FALSE,'');
               END;
               AddReal('','20','Betrag:',1,13,Betrag,'',7,2,-999.99,999.99);
               AddDate('','21','Datum Einzahlung:',1,14,EinzahlDat,'');
               AddDate('','21','Datum Auszahlung:',30,14,AuszahlDat,'');
               AddActionKey('_',27,ButtonBreak0,FALSE,'Zur�ck ins Hauptfenster');
               AddActionKey('_',-68,ButtonMoreEval,FALSE,'Speichern und in Lieferschein');
               AddActionKey('_',-67,ButtonMoreEval,FALSE,'Speichern und in Kundennummer');
               SetFKeyQuit(FALSE);
               SetCrQuit(FALSE);
            END;

            NEW(KStdFenster);
            KStdFenster^.Init(40,175,600,330,0,'Standardbestellung','KINFO4',Aktiv);
            WITH KSTdFenster^ DO BEGIN
               AddTime('','0','Von', 1,1,StdVon,'');
               ALignFields;
               AddTime('','0','bis',12,1,StdBis,'');
               AlignFields;
               AddConst('0',1,2,'Menge Art-Nr. Bezeichnung                                Preis');
               FOR Lauf := 1 TO 5 DO BEGIN
                  AddTabZeile('','0'+B2S(Lauf,0),'',1,Lauf+2,StdBest[Lauf].Uebernehmen,'');
                  AddLongInt('0'+B2S(Lauf,0),'1','',1, Lauf+2,StdBest[Lauf].Menge,'',3,0,999);
                  AddArtNum('0'+B2S(Lauf,0),'2','',6, Lauf+2,StdBest[Lauf].ArtNr,'',3,SeekKArt,Lauf);
                  AddString('0'+B2S(Lauf,0),'3','',14,Lauf+2,StdBest[Lauf].ArtBez,'',40);
                  AddReal('0'+B2S(Lauf,0),'4','',55,Lauf+2,StdBest[Lauf].Preis,'',7,2,
                                         -999.99,999.99);
                  SetToggle('0'+B2S(Lauf,0),TRUE);
                  AddActionKey('0'+B2S(Lauf,0),-62,StdPosLoeschen,FALSE,'Aktuelle Position l�schen');
                  SetActionFunc('0'+B2s(Lauf,0),KStdFensterArtBezAction);
               END;
               FOR Lauf := 1 TO 5 DO BEGIN
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_2',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_3',-72,UpToSTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-80,DnToTab,FALSE,'');
                  AddActionKey('0'+B2S(Lauf,0)+'_4',-72,UpToSTab,FALSE,'');
               END;
               Disable('4');
               SetFKeyQuit(FALSE);
               SetCRQuit(FALSE);
               AddActionKey('_',27,ButtonBreak0,FALSE,'Zur�ck ins Hauptfenster');
               AddActionKey('_',-68,ButtonMoreEval,FALSE,'Speichern und in Lieferschein');
               AddActionKey('_',-67,ButtonMoreEval,FALSE,'Speichern und in Kundennummer');
            END;
         END;
         AlleKFenster[1] := KFenster;
         AlleKFenster[2] := KRAdrFenster;
         AlleKFenster[3] := KStdFenster;
         AlleKFenster[4] := KOpostenFenster;
         AlleKFenster[5] := KAnlInfoFenster;
      END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
