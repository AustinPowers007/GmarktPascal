{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Feb 08 20:10:50 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMPLDO;

INTERFACE
   USES ZUSAETZE,
        PL_CONST;

   FUNCTION  NaechsterTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;
   FUNCTION  VorherigerTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;
   PROCEDURE PlDruckeTPlan(PlStart:PlanungsZeiger;FString:TFahrerString);
   PROCEDURE PlDruckeKaesten(PlStart:PlanungsZeiger;Datum:TDatum;TourNr:WORD;FString:TFahrerString);
   FUNCTION  PlSpeichernTPlan(PlStart:PlanungsZeiger;FString:TFahrerString):BOOLEAN;
   FUNCTION  PlLoeschenTPlan(PlStart:PlanungsZeiger):BOOLEAN;

IMPLEMENTATION

   USES GMDATEI,
        GMABRECH,
        GMPRT,
        GMKAEST,
        GMCONST,
        GMBASE,
        PLACEWIN,
        LIEDATEN,
        KUNDATEN,
        PRINTER,
        GMSETUP,
        GLDRUCK;



  PROCEDURE PutPlanListeToHandy(PlZeiger:PlanungsZeiger;Datum:TDatum;TourNr:WORD);
     TYPE  HandyFile = TEXT;

     VAR PlLauf : Planungszeiger;
         SeekResult : BOOLEAN;
     VAR Lauf    : BYTE;
         Ueberschrift,
         KundeGefunden,
         D: BOOLEAN;
         OldDay : BOOLEAN;
         PS,
         TourNrS,
         Anred,
         Liefer,
         Strng,
         Flk,
         LeerGutZ,
         Zahlung,
         ABezT,
         Nrs,Nam:STRING;
         HFPath : STRING;
         HF,JF: HandyFile;
         LaufC:BYTE;
    BEGIN
        PlLauf := PlZeiger;
        IF (PlLauf <> NIL) THEN BEGIN
            HFPath := MakeFilePath(GMEnvObj^.GetEntry('TOHANDYDIR'));
            IF ValidPath(HFPath) THEN BEGIN
               OldDay:= IsOldDay(Datum);
               {$I-}
               ASSIGN(HF,HFPath+'KUNDEN.'+W2S(MakeIFieldTnr(TourNr,OldDay),0));
               REWRITE(HF);
               {$I+}
               LaufC:=64;
               WRITE(HF,'OPLDatabaseFile'+CHR(00)+CHR(15)+CHR(17)+CHR(22)+CHR( 0)+
               CHR(15)+CHR(17)+CHR( 1)+CHR(32)+CHR( 3));

               SeekResult := TRUE;
            END ELSE
               SeekResult := FALSE;
           IF SeekResult THEN BEGIN
              REPEAT
                 LieferDB.BeginTransaction;
                 SeekResult:=LieferDB.GetRec(1,PLLauf^.Dat.LNr,Lieferung);
                 LieferDB.EndTransaction;
                 IF NOT SeekResult THEN
                    FaultBox.Act(2,'Fehler beim Åbertragen von Lieferschein '+
                                   PlLauf^.Dat.Lnr+ 'an Handy Verzeichnis.')
                 ELSE BEGIN
                    KundDB.BeginTransaction;
                    KundeGefunden := Kunddb.GetRec(1,Lieferung.Kundennummer,Kunde);
                    IF NOT KundeGefunden THEN
                       KundeGefunden:=KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.KNr),0),Kunde);
                    IF NOT KundeGefunden THEN BEGIN
                       InitTKundenDaten(Kunde);
                       Kunde.LfdNr := S2L(Lieferung.KNR);
                       FaultBox.Act(0,'Lieferschein '+Lieferung.LiefNr+', Kunde '+Lieferung.KNR+
                           ' ist nicht in Kundendatenbank');
                    END;
                    KundDb.EndTransaction;
                    WITH Kunde DO BEGIN
                       INC(LaufC);
                       STR(LfdNr,Nrs); Nrs:=UPCASE(CHR(LaufC))+': '+REPLICATE('0',5-LENGTH(Nrs))+Nrs;
                       Anred:=Anr[LiefAnrede]; Anred:=Anred+REPLICATE(' ',5-LENGTH(Anred));
                       IF (LiefAnrede = 3) THEN
                          Nam:=LiefName
                       ELSE
                          Nam:=LiefName+', '+LiefVorname[1]+'.'; Nam:=Nam+REPLICATE(' ',25-LENGTH(Nam));
                       Liefer:=Lieferung.LiefNr+REPLICATE(' ',5-LENGTH(Lieferung.LiefNr));
                       IF (Zahlweise > 9) THEN Zahlweise:=8;
                       STR(Zahlweise,Zahlung);
                       LeerGutZ:=CHR(128);
                       FOR Lauf:=1 TO 14 DO
                          LeerGutZ:=LeerGutZ+CHR(128);
                       Strng:=CHR(88)+CHR(16)+CHR(87)+Nrs+Anred+Nam+Liefer+Zahlung+
                       LeerGutZ+'   0.00'+'   0.00'+'   0.00'+'   0.00';
                       WRITE(HF,Strng);
                    END;
                    WITH Lieferung DO BEGIN
                       ASSIGN(JF,HFPath+'LS'+LiefNr+'.'+W2S(TourNr,0));
                       REWRITE(JF);
                       WRITE(JF,'OPLDatabaseFile'+CHR(00)+CHR(15)+CHR(17)+CHR(22)+CHR( 0)+
                       CHR(15)+CHR(17)+CHR( 1)+CHR(32)+CHR( 3));
                       Lauf:=1;
                       WHILE (Positionen[Lauf].ArtBez <> '') AND (Lauf < 31) DO
                          WITH Positionen[Lauf] DO BEGIN
                             STR(Menge:2,Strng);
                             ABezT:=COPY(ArtBez,1,37);
                             IF (LENGTH(ABezT) < 37) THEN
                                ABezT:=ABezT+REPLICATE(' ',37-LENGTH(ABezT));
                             Strng:=Strng+REPLICATE(' ',3-LENGTH(Strng))+ABezT;
                             STR(Preis:7:2,PS);
                             Strng:=Strng+PS;
                             STR(Pfand:7:2,PS);
                             FLK:=CHR(128);
                             Strng:=CHR(56)+CHR(16)+CHR(55)+Strng+PS+FLK;
                             WRITE(JF,Strng);
                             INC(Lauf);
                          END;
                       CLOSE(JF);
                    END;
                 END;
                 PlLauf := PlLauf^.NextIT;
              UNTIL (PlLauf = NIL);
              {$I-}
              CLOSE(HF);
              {$I+}
           END ELSE BEGIN
              FaultBox.Act(0,'Das Verzeichnis '+HFPath+' existiert nicht, '+
                             'öbertragung nicht mîglich');
           END;
        END ELSE BEGIN
           FaultBox.Act(0,'Keine Lieferschein in Tour, Tour wird nicht Åbergeben.');
        END;
     END;

   PROCEDURE PlDruckeKaesten;
      VAR Kaesten : TKaestenObjPtr;
          DatNam : STRING[80];
      BEGIN
         NEW(Kaesten);
         DatNam:=MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'))+'PRTFile.TXT';
         PrtToFile(DatNam);
         Kaesten^.Init;
         Kaesten^.DruckePlanListe(PlStart,Datum,MakeIFieldTStr(TourNr),FString);
         PrinterToPrt;
         WriteTextDatei(DatNam);
         DISPOSE(Kaesten,Done);
      END;

   PROCEDURE PlDruckeTPlan;
     VAR PlLauf : Planungszeiger;
         KundeGefunden : BOOLEAN;
         LfdTNr : BYTE;
         DatNam:STRING[80];
         T:TEXT;
     BEGIN
        DatNam:=MakeFilePath(GMEnvObj^.GetEntry('TOHANDYDIR'))+'TPLan.DAT';
        LieferDB.BeginTransaction;
        PlLauf := PlStart;
        LieferDB.GetRec(1,PLLauf^.Dat.LNr,Lieferung);
        StatusBox.Act('Tourplan '+Lieferung.Fahrer,'');
        Lieferung.Fahrer :=FString;
        Lieferung.TourNr := MakeIFieldTnr(PLLauf^.Dat.Tur,FALSE);
        PrtToFile(DatNam);
        DruckeTourplanUeberSchrift(Lieferung);
        LfdTNr := 1;
        REPEAT
           LieferDB.GetRec(1,PLLauf^.Dat.LNr,Lieferung);
           Lieferung.Fahrer :=FString;
           KundDB.BeginTransaction;
           KundeGefunden:=KundDB.GetRec(1,Lieferung.Kundennummer,Kunde);
           IF NOT KundeGefunden THEN
              KundeGefunden:=KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.KNr),0),Kunde);
           KundDB.EndTransaction;
           StatusBox.Act('Tourplan '+Lieferung.Fahrer,Lieferung.KNr);

           TourPlanAndruck(Lieferung,Kunde,KundeGefunden,LfdTNr);
           INC(LfdTNr);
           PlLauf := PlLauf^.NextIT;
        UNTIL (PlLauf = NIL);
        PrinterToPrt;
        LieferDB.EndTransaction;
        StatusBox.Act('Drucke Tourplan','');
        WriteTextDatei(DatNam);
        StatusBox.Hide;
     END;


   FUNCTION PlSpeichernTPlan;
     VAR PlLauf : Planungszeiger;
         KundeGefunden : BOOLEAN;
         OldDay : BOOLEAN;
         Fehler : BOOLEAN;
         OPsGefunden,
         OPsUebertragen : BOOLEAN;
         D   : BOOLEAN;
         AnzUebertragen : BYTE;
         ErsteLeerPos : BYTE;
         TLfdLauf,
         TLauf : WORD;
         KOpsGespeichert : BOOLEAN;
         OPZeile1,
         OPZeile2 :STRING[80];

         LFOK : BOOLEAN;

     BEGIN
        IF (FString <> '') THEN BEGIN
           OldDay  := IsOldDay(Uebdatum);
           LieferDB.BeginTransaction;
           PlLauf := PlStart;
           Fehler := FALSE;
           TLfdLauf := 1;
           REPEAT
              IF LieferDB.GetRec(1,PLLauf^.Dat.LNr,Lieferung) THEN BEGIN
                 Lieferung.Fahrer := FString;
                 Lieferung.TourNr := MakeIFieldTnr(PLLauf^.Dat.Tur,OldDay);
                 Lieferung.TourLfdNr := TLfdLauf;
                 StatusBox.Act('Tour '+MakeIFieldTStr(PlLauf^.Dat.Tur),'bearbeite Lieferschein '+Lieferung.LiefNr);
                 OPsGefunden:=OpsEintragen(TRUE,TRUE,OpsUebertragen,AnzUebertragen);
                 IF NOT LieferDB.ChangeRec(Lieferung) THEN BEGIN
                    Fehler := TRUE;
                    FaultBox.Act(2,'Fehler beim Schreiben von Lieferschein '+PlLauf^.Dat.LNr);
                    IF OpsUebertragen THEN BEGIN
                       OPZeile1 :=ActDate+';'+ActTime+';'+'L-SCHREIBFEHLER'+';'+Lieferung.KNR+
                            ';'+Lieferung.LiefNr;
                       LFOk := OPLogFile^.WritelnLog(OpZeile1);
                       IF NOT LFOK THEN BEGIN
                          FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                         'Bitte folgende Infos eintragen: '+OpZeile1);
                       END;
                    END;
                 END ELSE BEGIN
                    IF OpsUebertragen THEN BEGIN
                       KOpsGespeichert := FALSE;
                       KundDB.BeginTransaction;
                       IF KundDB.GetRec(1,MakeLfdNr(S2l(Lieferung.Knr),0),Kunde) THEN BEGIN
                          IF (AnzUebertragen = 10) THEN
                             ErsteLeerPos := 1
                          ELSE BEGIN
                             FOR TLauf := AnzUebertragen+1 TO 10 DO BEGIN
                               WITH Kunde.Oposten[TLauf-AnzUebertragen] DO BEGIN
                                 OPZeile1 :=ActDate+';'+ActTime+';'+'OP-AUSGETRAGEN'+';'+
                                   LeadingZeros(Kunde.LfdNr,5);
                                 OPZeile2 :='#'+Art+';'+LiefNr+';'+R2S(Betrag,7,2)+';'+Bemerk;
                               END;
                               LFOk := OPLogFile^.WritelnLog(OpZeile1);
                               LFOk := OPLogFile^.WritelnLog(OPZeile2) AND LFOk;
                               IF NOT LFOK THEN BEGIN
                                  FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                      'Bitte folgende Infos eintragen: '+OpZeile1+
                                      ' '+OpZeile2);
                               END;
                               Kunde.Oposten[TLauf-AnzUebertragen] :=
                                 Kunde.Oposten[Tlauf];
                             END;
                             ErsteLeerPos := 10 - AnzUebertragen+1;
                          END;
                          FOR TLauf := ErsteLeerPos TO 10 DO
                            WITH Kunde.Oposten[Tlauf] DO BEGIN
                                   Art := #0;
                                   Betrag := 0;
                                   LiefNr := '';
                                   Bemerk := '';
                                END;
                          KOpsGespeichert := KundDB.ChangeRec(Kunde);
                          IF NOT KOpsGespeichert THEN BEGIN
                             OPZeile1 :=ActDate+';'+ActTime+';'+'K-SCHREIBFEHLER'+';'+LeadingZeros(Kunde.LfdNr,5);
                             LFOk := OPLogFile^.WritelnLog(OpZeile1);
                             IF NOT LFOK THEN BEGIN
                                FaultBox.Act(0,'Fehler beim Schreiben des OP-LogFiles '+
                                'Bitte folgende Infos eintragen: '+OpZeile1);
                             END;
                          END;
                       END;
                       KundDB.EndTransaction;
                       IF NOT KOpsGespeichert THEN BEGIN
                          FaultBox.Act(0,'Fehler beim Anpassen der offenen Posten '+
                                         ' von Kunde '+Lieferung.KNr+' in Kundendatei '+
                                         ' bitte manuell korregieren');
                       END;
                    END;
                 END;
              END ELSE BEGIN
                 Fehler := TRUE;
                 FaultBox.Act(2,'Fehler beim Lesen von Lieferschein '+PlLauf^.Dat.LNr);
              END;
              INC(TLfdLauf);
              PlLauf := PlLauf^.NextIT;
           UNTIL (PlLauf = NIL);
           LieferDB.EndTransaction;
           IF NOT Fehler THEN BEGIN
              StatusBox.Act('öbergebe Tour '+MakeIFieldTStr(PlStart^.Dat.Tur),'an Handy-Verzeichnis');
              PutPlanListeToHandy(PlStart,Uebdatum,PlStart^.Dat.Tur);
           END;
           StatusBox.Hide;
        END ELSE BEGIN
           FaultBox.Act(3,'Sie mÅssen der Tour erst einen Fahrer zuordnen');
        END;
     END;

   FUNCTION PlLoeschenTPlan(PlStart:PlanungsZeiger):BOOLEAN;
     VAR PlLauf : Planungszeiger;
         KundeGefunden : BOOLEAN;
         OldDay : BOOLEAN;
         Fehler : BOOLEAN;
         D   : BOOLEAN;
         TLauf : WORD;

     BEGIN
        OldDay  := IsOldDay(Uebdatum);
        LieferDB.BeginTransaction;
        PlLauf := PlStart;
        Fehler := FALSE;
        REPEAT
           IF LieferDB.GetRec(1,PLLauf^.Dat.LNr,Lieferung) THEN BEGIN
              Lieferung.Fahrer := '';
              Lieferung.TourLfdNr := 0;
              StatusBox.Act('Tour '+MakeIFieldTStr(PlLauf^.Dat.Tur),'Lîsche Zuordung von Lieferschein '+Lieferung.LiefNr);
              IF NOT LieferDB.ChangeRec(Lieferung) THEN BEGIN
                 Fehler := TRUE;
                 FaultBox.Act(2,'Fehler beim Schreiben von Lieferschein '+PlLauf^.Dat.LNr);
              END;
           END ELSE BEGIN
               Fehler := TRUE;
               FaultBox.Act(2,'Fehler beim Lesen von Lieferschein '+PlLauf^.Dat.LNr);
           END;
           PlLauf := PlLauf^.NextIT;
        UNTIL (PlLauf = NIL);
        LieferDB.EndTransaction;
        StatusBox.Hide;
     END;

   FUNCTION NaechsterTag;
      BEGIN
         MoreEval := FALSE;
         IF ValidDate(InpString) THEN BEGIN
            InpString:= NextWorkDay(InpString);
         END ELSE
            WRITE(CHR(7));
         Abbruch := FALSE;
         Check:= FALSE;
         NaechsterTag := FALSE;
      END;

   FUNCTION VorherigerTag;
      BEGIN
         MoreEval := FALSE;
         IF ValidDate(InpString) THEN BEGIN
            InpString:= PrevWorkDay(InpString);
         END ELSE
            WRITE(CHR(7));
         Abbruch := FALSE;
         Check:= FALSE;
         VorherigerTag := FALSE;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
