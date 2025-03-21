{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 16:55:48 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMKBED;

INTERFACE

   FUNCTION  Kassenbuchfuehren(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN; FAR;
   PROCEDURE InitKBInput;

IMPLEMENTATION

   USES GMDATEI,
        ASTDATA,
        CASHNEU,
        PLACEWIN,
        ZUSAETZE,
        CASHDATA,
        GMBASE,
        GMCRT,
        GMKBDR,
        GMSETUP,
        KBDATEN,
        GMABRECH;

   VAR   KBDruckFenster,
         KBIndexFenster,
         KBSuchFenster,
         KBFenster : TEingabeFensterPtr;

   VAR   NeuerEintrag : BOOLEAN;
         OldAktBTyp : WORD;
   VAR   Suchtag : TDatum;
         Month,
         Year: STRING;
         FromOldFormat: BOOLEAN;

         RNummer,
         BNummer : LONGINT;


   FUNCTION KBEintragSpeichern:BOOLEAN;
      VAR Gespeichert : BOOLEAN;
      VAR Lauf : BYTE;
          KBSik : BYTE;
          TDat : TDatum;


      BEGIN
         KBSik := KBEintrag^.KBSatzTyp;
         TDat := KBEintrag^.Tagesdatum;
{         KBEintrag.TagesDatum := ActDate; }
         Gespeichert := FALSE;
         IF ValidDate(KBEintrag^.BelegDatum) THEN BEGIN
            KBSik := KBEintrag^.KBSatzTyp;
            TDat := KBEintrag^.Tagesdatum;
{            KBEintrag.TagesDatum := ActDate; }
            IF KassenDB^.BeginTransaction THEN BEGIN
               IF NeuerEintrag THEN BEGIN
                  KBEintrag^.LfdNr:=KassenDB^.GetLfdNr+1;
                  Gespeichert := KassenDB^.NewRec(KBEintrag^);
                  IF Gespeichert THEN BEGIN
                     KassenDB^.SetLfdNr(KBEintrag^.LfdNr);
                     NeuerEintrag := FALSE;
                     InfoBox.Act(2,'Kassenbucheintrag angelegt');
                  END;
               END ELSE BEGIN
                  Gespeichert := KassenDB^.ChangeRec(KBEintrag^);
                  IF Gespeichert THEN
                     InfoBox.Act(2,'Kassenbucheintrag ge�ndert');
               END;
               IF NOT KassenDB^.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                                 KassenDB^.GetTransactionErrMsg);
            END ELSE
               FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                           KassenDB^.GetTransactionErrMsg);
            IF NOT Gespeichert THEN BEGIN
               KBEintrag^.KBSatzTyp := KBSik;
               KBEintrag^.Tagesdatum := TDat;
               FaultBox.Act(2,'Kassenbucheintrag konnte nicht gespeichert werden');
            END;
         END ELSE BEGIN
            FaultBox.Act(0,'Belegdatum ist ung�ltig, '+
                           'bitte korrektes Datum eingeben');

         END;
         KBEintragSpeichern := Gespeichert;
      END;

   FUNCTION KbBeenden(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         IF (NOT KBFenster^.Changed('')) OR Request.Act('Die �nderungen wurden noch nicht '+
          'gespeichert. Trotzdem beenden ?') THEN
            Abbruch := TRUE;
         Check := FALSE;
         KBBeEnden := Ergebnis;
      END;


   PROCEDURE ChangeKBFenster;
      VAR Lauf : BYTE;

      BEGIN
         IF (KBEintrag^.KBSatzTyp <> OldAktBTyp) THEN BEGIN
            OldAktBTyp := KbEintrag^.KBSatzTyp;
            IF (KBEintrag^.KBSatzTyp <> KBTourTyp) THEN
               KBFenster^.Disable('50')
            ELSE
               KBFenster^.Enable('50');
            KBFenster^.Refresh('');
         END;
      END;

   PROCEDURE NeuerKBE;
      BEGIN
         ChangeKBFenster;
         NeuerEintrag := TRUE;
      END;

   PROCEDURE KBEAendern;
      BEGIN
         ChangeKBFenster;
         NeuerEintrag := FALSE;
      END;

   PROCEDURE NeuerKBEintrag;

      BEGIN
         InitTKbDaten(KBEintrag^);
         KBEintrag^.TagesDatum := ActDate;
         KBEintrag^.BelegDatum := ActDate;
         NeuerKBE;
         KBFenster^.Save('');
      END;


   FUNCTION F2NeuerEintrag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                             VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR DONeu: BOOLEAN;
          Ergebnis : BOOLEAN;
          KErgebnis : STRING;
          KNrIdx : STRING;
      BEGIN
         MoreEval := FALSE;
         Ergebnis:= FALSE;
         IF (NOT KBFenster^.Changed('')) OR Request.Act('Die �nderungen wurden noch nicht '+
          'gespeichert. Trotzdem neuen Eintrag anlegen ?') THEN BEGIN
           NeuerKBEintrag;
           NeuerKBE;
           Taste := 0;
           Ergebnis := TRUE;
           InpString := KBFenster^.GetVal(KBFenster^.GetFocusID);
           KBFenster^.Save('');
         END;
         KBFenster^.Refresh('');
         F2NeuerEintrag:= Ergebnis;
         Abbruch := FALSE;
         Check:= FALSE;
      END;


   FUNCTION F10KBSpeichern(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR Gespeichert : BOOLEAN;
          KBHelp : TKBDaten;


      BEGIN
         MoreEval := FALSE;
         Gespeichert := TRUE;
         IF (RemoveAllSpaces(KbEintrag^.BelegNr) <> '') THEN BEGIN
            KbHelp := KBEintrag^;
            IF KassenDB^.BeginTransaction THEN BEGIN
               IF KassenDB^.GetRec(4,CreateKbIndex(4,KBHelp),KBHelp) THEN BEGIN
                  IF NeuerEintrag OR (KBHelp.LfdNr <> KBeintrag^.LfdNr) THEN BEGIN
                     Gespeichert := FALSE;
                     FaultBox.Act(0,'Es existiert schon ein Abrechnungseintrag '+
                                    'mit dieser Belegnr . Eintrag wird nicht gespeichert');
                  END;
               END;
               Kassendb^.Endtransaction;
            END;
         END;
         IF Gespeichert AND NeuerEintrag AND (KBEintrag^.KBSatzTyp = KBTourTyp) THEN BEGIN
            KbHelp := KBEintrag^;
            IF KassenDB^.BeginTransaction THEN BEGIN
               IF KassenDB^.GetRec(5,CreateKbIndex(5,KBHelp),KBHelp) THEN BEGIN
                  Gespeichert := FALSE;
                  FaultBox.Act(0,'Es existiert schon ein Abrechnungseintrag '+
                                 'f�r diese Tour . Eintrag wird nicht gespeichert');
               END;
               Kassendb^.Endtransaction;
            END;
         END;
         IF Gespeichert THEN BEGIN
            Gespeichert := KBEintragSpeichern;
            IF Gespeichert THEN BEGIN
               KBFenster^.Save('');
               KBFenster^.Refresh('');
               KBeAendern;
            END;
         END;
         IF NOT Gespeichert THEN
            Taste := 0;
         KBFenster^.Refresh('');
         Abbruch := FALSE;
         Check:= FALSE;

         F10KBSpeichern:=F2NeuerEintrag(Taste,Nr,InpString,Abbruch,Check,MoreEval);
      END;



   FUNCTION KBEintragLoeschen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR LNrSave : STRING;
          SeekResult : BOOLEAN;
          Ergebnis     : BOOLEAN;
      BEGIN
         MoreEval := FALSE;
         Ergebnis := FALSE;
         IF (NOT NeuerEintrag) AND Request.Act('Sind sie sicher das sie den '+
            'Kassenbucheintrag l�schen wollen ?') THEN BEGIN
            IF KassenDB^.BeginTransaction THEN BEGIN
               Ergebnis:=KassenDB^.DelRec(CreateKBIndex(1,KBEintrag^));
               IF Ergebnis THEN BEGIN
                  InfoBox.Act(2,'KassenbuchEintrag gel�scht');
                  KassenDB^.GetRec(2,CreateKBIndex(2,KBEintrag^),KBEintrag^);
               END ELSE
                  FaultBox.Act(0,'Fehler beim L�schen des Lieferscheins');
               IF NOT KassenDB^.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                     KassenDB^.GetTransactionErrMsg);
            END ELSE
              FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                           KassenDB^.GetTransactionErrMsg);
            KBFenster^.Refresh('');
         END;
         Abbruch := FALSE;
         Check:= FALSE;
         KBEintragLoeschen := Ergebnis;
      END;

   FUNCTION KBEintragPlusMinus(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR SeekResult: BOOLEAN;
          SK : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         IF (NOT KBFenster^.Changed('')) OR Request.Act('Die �nderungen wurden noch nicht '+
            'gespeichert. Wollen Sie die Eingaben verwerfen ?') THEN BEGIN
            IF NeuerEintrag THEN BEGIN
               InitTKbDaten(KBEintrag^);
               KbEintrag^.Tagesdatum := ActDate;
               IF KassenDB^.BeginTransaction THEN BEGIN
                  SeekResult := KassenDB^.GetRec(2,CreateKBIndex(2,KBEintrag^),KBEintrag^);
                  IF NOT SeekResult THEN
                     SeekResult:=KassenDB^.GetNext(2,KBEintrag^);
                  IF NOT SeekResult THEN
                     WRITE(CHR(7));
                  IF NOT KassenDB^.Endtransaction THEN
                     FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                      KassenDB^.GetTransactionErrMsg);
               END ELSE
                  FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                               KassenDB^.GetTransactionErrMsg);
            END ELSE BEGIN
               IF KassenDB^.BeginTransaction THEN BEGIN
                 IF (Taste = 43) THEN
                     SeekResult:=KassenDB^.GetNext(2,KBEintrag^)
                  ELSE
                     SeekResult:=KassenDB^.GetPrev(2,KBEintrag^);
                  IF NOT SeekResult THEN
                     WRITE(Chr(7));
                  IF NOT KassenDB^.Endtransaction THEN
                     FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                      KassenDB^.GetTransactionErrMsg);
               END ELSE
                  FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                               KassenDB^.GetTransactionErrMsg);
            END;
            InpString := KBFenster^.GetVal(KBFenster^.GetFocusID);
            KBEAendern;
            KBFenster^.Save('');
            KBFenster^.Refresh('');
         END;
         Check := FALSE;
         Abbruch := FALSE;
         KBEintragPlusMinus := FALSE;
      END;

   FUNCTION KBTagSuchen(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR SeekResult: BOOLEAN;
          SK : BOOLEAN;

      BEGIN
         MoreEval := FALSE;
         IF (NOT KBFenster^.Changed('')) OR Request.Act('Die �nderungen wurden noch nicht '+
            'gespeichert. Wollen Sie die Eingaben verwerfen ?') THEN BEGIN
            Suchtag := KBEintrag^.BelegDatum;
            KBSuchFenster^.Input;
            IF NOT KBSuchFenster^.Abbruch THEN BEGIN
               IF ValidDate(SuchTag) THEN BEGIN
                  InitTKbDaten(KBEintrag^);
                  KbEintrag^.Belegdatum := SuchTag;
                  IF KassenDB^.BeginTransaction THEN BEGIN
                     KassenDB^.GetRec(2,CreateKBIndex(2,KBEintrag^),KBEintrag^);
                     IF NOT KassenDB^.Endtransaction THEN
                        FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                         KassenDB^.GetTransactionErrMsg);
                  END ELSE
                     FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                                  KassenDB^.GetTransactionErrMsg);
               END ELSE
                  FaultBox.Act(2,'Ung�ltiges Datum');
               InpString := KBFenster^.GetVal(KBFenster^.GetFocusID);
               KBFenster^.Save('');
            END;
            KBFenster^.Refresh('');
            KBEAendern;
            KBSuchFenster^.Hide;
         END;

         Check := FALSE;
         Abbruch := FALSE;
         KBTagSuchen := FALSE;
      END;

   FUNCTION KBDrucken(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR MAktMonat: STRING;

      BEGIN
         MoreEval := FALSE;
         Month := COPY(ActDate,4,2);
         Year := COPY(ActDate,7,2);
         KBDruckFenster^.Input;
         IF NOT KBDruckFenster^.Abbruch THEN BEGIN
            IF ((S2B(Month)>= 1) AND (S2B(Month)<= 12)) AND Isnumber(Year) THEN BEGIN
               KBDruck(LeadingZeros(S2l(Month),2)+'.'+LeadingZeros(S2l(Year),2));
            END ELSE
               FaultBox.Act(2,'Ung�ltige Monatsangabe');
         END;
         KBDruckFenster^.Hide;
         Check := FALSE;
         Abbruch := FALSE;
         KBDrucken := FALSE;
      END;
   FUNCTION SchecksDrucken(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR MAktMonat: STRING;

      BEGIN
         MoreEval := FALSE;
         Month := COPY(ActDate,4,2);
         Year := COPY(ActDate,7,2);
         KBDruckFenster^.Input;
         IF NOT KBDruckFenster^.Abbruch THEN BEGIN
            IF ((S2B(Month)>= 1) AND (S2B(Month)<= 12)) AND Isnumber(Year) THEN BEGIN
               ScheckDruck(LeadingZeros(S2l(Month),2)+'.'+LeadingZeros(S2l(Year),2));
            END ELSE
               FaultBox.Act(2,'Ung�ltige Monatsangabe');
         END;
         KBDruckFenster^.Hide;
         Check := FALSE;
         Abbruch := FALSE;
         SchecksDrucken := FALSE;
      END;



   FUNCTION NaechsterTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                         VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
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

   FUNCTION VorherigerTag(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                          VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
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

   FUNCTION KBAInput:BOOLEAN;FAR;
      VAR Changed : BOOLEAN;
      BEGIN
         IF NeuerEintrag AND (KBFenster^.GetFocusId = '102')  THEN BEGIN
            Changed := FALSE;
            IF (KBSTGruppen[KBEintrag^.KBSatzTyp] <> '') AND
               (KBEintrag^.Gruppe = '') THEN BEGIN
               KBEintrag^.Gruppe := KBSTGruppen[KBEintrag^.KBSatzTyp];
               Changed := TRUE;
            END;
            IF (KBSTKonten[KBEintrag^.KBSatzTyp] <> '') AND
               (KBEintrag^.Konto = '') THEN BEGIN
               KBEintrag^.Konto := KBSTKonten[KBEintrag^.KBSatzTyp];
               Changed := TRUE;
            END;
            KBFenster^.Refresh('')
         END;
         ChangeKBFenster;
      END;

   FUNCTION  KBNewIndex(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                        VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;

      PROCEDURE Umstellung(Name:STRING);
         VAR F1: FILE OF TKBDaten;
             KBDaten:    TKBDaten;
             No: LONGINT;
             Pfad: STRING;
         BEGIN
           { Alte Dateien umbenennen und �ffnen }

             KassenDB^.CloseFile;
             Pfad:=GMEnvObj^.GetEntry('KBDIR');
             RenameFile(Pfad+Name+'.AVL',Pfad+'S'+Name+'.AVL');
             RenameFile(Pfad+Name+'.FRE',Pfad+'S'+Name+'.FRE');
             RenameFile(Pfad+Name+'.IDX',Pfad+'S'+Name+'.IDX');

             ASSIGN(F1,Pfad+'S'+Name+'.AVL');
             {$I-} RESET(F1); {$I+}
             IF (IORESULT <> 0) THEN EXIT;
             {$I-} READ(F1,KBDaten); {$I+}
             IF (IORESULT <> 0) THEN EXIT;
             No:=0;

           { Neue Datei �ffnen }

             KassenDB^.OpenFile('',GMEnvObj^.GetEntry('KBDIR'),TRUE,ActDate);

           { Durchlauf }

             WITH KassenDB^ DO BEGIN
               IF BeginTransaction THEN BEGIN
                 REPEAT
                   READ(F1,KBDaten);

                   { Altes Format �ndern }

                     WITH KBDaten DO BEGIN
                       IF FromOldFormat THEN
                         IF (BTxt[1,3] = '.') THEN BEGIN
                           Btxt[1]:=COPY(Btxt[1],14,12);
                           IF (KBSatzTyp = 6) THEN KBSatzTyp:=2;
                         END;
                       IF LfdNr > No THEN No:=LfdNr;
                     END;

                   NewRec(KBDaten);
                   StatusBox.Act(KBDaten.TagesDatum,'');
                 UNTIL EOF(F1) OR KEYPRESSED;
                 WRITE(CHR(7));
                 SetLfdNr(No);
               END;
               EndTransaction;
             END;

           { Dateien schlie�en }

             StatusBox.Hide;
             CLOSE(F1);
         END;

      BEGIN
         MoreEval := FALSE;
         Month := COPY(ActDate,4,2);
         Year  := COPY(ActDate,7,2);
         FromOldFormat := FALSE;
         KBIndexFenster^.Input;
         IF NOT KBIndexFenster^.Abbruch THEN BEGIN
           IF ((S2B(Month)>= 1) AND (S2B(Month)<= 12)) AND Isnumber(Year) THEN BEGIN
             Umstellung('KB_'+LeadingZeros(S2l(Month),2)+LeadingZeros(S2l(Year),2));
           END ELSE
             FaultBox.Act(2,'Ung�ltige Monatsangabe');
         END;
         KBIndexFenster^.Hide;
         Check := FALSE;
         Abbruch := FALSE;
         KBNewIndex := FALSE;
      END;


   PROCEDURE InitKBInput;
      VAR Lauf :BYTE;

      BEGIN
         NEW(KBFenster);
         KBFenster^.Init(100,100,540,300,0,'Kassenbuch','ASIN3',Aktiv);
         WITH KBEintrag^ DO BEGIN
            WITH KBFenster^ DO BEGIN
               AddNumText ('','101','BelegNr:', 1,0,BelegNr,'',3);
               AddDate    ('','999','Letzte �nderung:',20,0,Tagesdatum,'');
               AlignFields;
               AddRoll('','102','Typ:',1,1,KBSatzTyp,'',14);
               FOR Lauf := 1 TO KBSatzTypen DO
                  AddItem('102',KBSatzBez[Lauf]);
               AlignFields;
               AddDate ('','103','Belegdatum:',   1,2,BelegDatum,'');
               AddDate ('','103','Datum Eintrag:',1,3,TagesDatum,'');
               AlignFields;
               AddLongInt  ('','1','Kunde:',1,4,Knr,'',5,0,99999);
               AddLongInt  ('','1','LiefNr:',20,4,LiefNr,'',5,0,99999);
               AddReal  ('','3','Betrag:',1,5,Betrag,'',8,2,-9999.99,99999.99);
               AlignFields;
               AddString('','10','Buchungstext:',1,6,BTxt[1],'',13);
               AddString('','11','',1,7,BTxt[2],'',13);
               AddString('','12','',1,8,BTxt[3],'',25);
               AlignFields;
               AddString('','20','Konto:',1,9,Konto,'',12);
               AddString('','20','Gruppe:',1,10,Konto,'',12);
               AddWord('','50','TourNr:',1,11,TourNr,'',3,0,999);
               AlignFields;
               AddActionKey('',45,KbEintragPlusMinus,FALSE,'Vorheriger Kassenbucheintrag');
               AddActionKey('',43,KBEintragPlusMinus,FALSE,'N�chster Kassenbucheintrag');
               AddActionKey ('',-60,F2NeuerEintrag,FALSE,'Neuen Kassenbucheintrag anlegen');
{               AddActionKey ('',-67,KBNewIndex,FALSE,'Indice �ndern');}
               AddActionKey ('',-68,F10KBSpeichern,TRUE,'Kassebucheintrag speichern');
               AddActionKey('',27,KBBeenden,FALSE,'Zur�ck in Lieferschein');
               AddActionKey('',-63,KBdrucken,FALSE,'Kassenbuch drucken');
               AddActionKey('',-64,Schecksdrucken,FALSE,'Scheckliste drucken');
               AddActionKey('',-65,KBTagSuchen,FALSE,'Buchungstag wechseln');
               AddActionKey('',-62,KBEintragLoeschen,FALSE,'L�schen des Kassenbucheintrages');
               SetAInputFunc(KBAInput);
               AddActionKey ('103',43,NaechsterTag,FALSE,'N�chster Tag');
               AddActionKey ('103',45,VorherigerTag,FALSE,'Vorheriger Tag');
               Disable('999');
               SetFKeyQuit(FALSE);
               SetCRQuit(FALSE);
            END;
            NEW(KBDruckFenster);
            KBDruckFenster^.Init(205,190,435,290,0,'Monat','ASIN3',Aktiv);
            KBDruckFenster^.AddNumText('','1','Monat:',1,1,Month,'',2);
            KBDruckFenster^.AddNumText('','2','Jahr :',1,2,Year,'',2);

            NEW(KBSuchFenster);
            KBSuchFenster^.Init(205,190,435,290,0,'Buchungstag','ASIN3',Aktiv);
            KBSuchFenster^.AddDate('','1','Tag:',1,1,SuchTag,'');

            NEW(KBIndexFenster);
            WITH KBIndexFenster^ DO BEGIN
              Init(205,190,435,290,0,'Neuer Index','ASIN3',Aktiv);
              AddNumText('','1','Monat:',1,1,Month,'',2);
              AddNumText('','2','Jahr :',1,2,Year,'',2);
              AddBool   ('','3','Alt --> Neu:',1,3,FromOldFormat,'');
              AlignFields
            END;

         END;
      END;

   FUNCTION Kassenbuchfuehren;
     BEGIN
        NeuerKBEintrag;
        KBFenster^.SetFocusField(1);
        REPEAT
           KBFenster^.Input;
        UNTIL KBFenster^.Abbruch;
        KBFenster^.Hide;
     END;

BEGIN
  OldAktBTyp := 255;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
