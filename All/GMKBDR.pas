{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Feb 08 20:47:38 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMKBDR;

INTERFACE

   USES KBDaten;

   PROCEDURE KBDruck(Monat:STRING);
   PROCEDURE ScheckDruck(Monat:STRING);

IMPLEMENTATION
   USES GMCRT,
        GMDATEI,
        PRINTER,
        GMPRT,
        GMSETUP,
        GLDRUCK,
        ZUSAETZE,
        PLACEWIN,
        LIEDATEN;
   VAR DatNam : STRING[80];
       KBDruckDaten : TKBDaten;
       KBIndexBegin : STRING[40];
       KBIndexEnd   : STRING[40];
   VAR SeitenNr : BYTE;
       ZeilenNr : BYTE;
       LfdTag,
       LfdBestand:REAL;
       MerkeDatum:TDatum;
       KopfDone,
       BestandDone:BOOLEAN;

   PROCEDURE KBDruck;
      CONST SeitenLaenge = 53;

      PROCEDURE DruckeFuss(MonatsEnde:BOOLEAN);
         CONST txt:ARRAY[BOOLEAN] OF STRING[13] = ('Blattabschluá',
                                                   'Monatabschluá');
         BEGIN
            {$I-}
              IF NOT(BestandDone) THEN BEGIN
                WRITELN(LST,'ÃÄÄÁÄÄÄÄÄÁÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ´');
                WRITELN(LST,'³            ³'+Txt[Monatsende]+'³'+PrintReal(LfdBestand,6,2,FALSE)+
                          '³                                        ³');
                WRITELN(LST,'ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ');
              END;
              WRITELN(LST);
              WRITELN(LST,'Datum:                         Unterschrift:');
            {$I+}
         END;

      FUNCTION DruckeBestand:BOOLEAN;
         CONST T = '³';
         BEGIN
           {$I-}
             INC(ZeilenNr);
             IF (ZeilenNr >= SeitenLaenge) THEN
               WRITELN(LST,'ÀÄÄÁÄÄÄÄÄÁÄÄÄÙ'+PrintString('Tagesabschluá',13)+
                           'Á'+PrintReal(LfdBestand,6,2,FALSE)+
                           'Á'+PrintReal(LfdTag,5,2,FALSE)+
                           'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ')
             ELSE
               WRITELN(LST,'ÃÄÄÅÄÄÄÄÄÅÄÄÄ´'+PrintString('Tagesabschluá',13)+
                           T+PrintReal(LfdBestand,6,2,FALSE)+
                           T+PrintReal(LfdTag,5,2,FALSE)+
                           'ÃÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
           {$I+}
           LfdTag:=0;
           MerkeDatum:=KBDruckDaten.TagesDatum;
           DruckeBestand:=TRUE
         END;

      PROCEDURE DruckeZeile;
         VAR AKtBetrag: REAL;
             B: ARRAY[1..6] OF STRING[8];

         FUNCTION DruckeKopf:BOOLEAN;
            BEGIN
              {$I-}
                 WRITELN(LST,GmEnvObj^.GetEntry('GSNAME'));
                 WRITELN(LST);
                 WRITELN(LST,'Kassenbuch'+REPLICATE(' ',25)+'Monat '+Monat+
                         + REPLICATE(' ',25)+'Seite '+B2s(SeitenNr,0));
                 WRITELN(LST);
                 WRITELN(LST,'ÚÄÄÂÄÄÄÄÄÂÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                 WRITELN(LST,'³TD³B-Dat³T/B³Text         ³Bestand ³TourEin³Bar-Ein³Bar-Aus³Sonstig³  Bank  ³');
                 WRITELN(LST,'ÃÄÄÁÄÄÄÄÄÁÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ´');
                 WRITELN(LST,'³            ³šbertrag     ³'+PrintReal(LfdBestand,6,2,FALSE)+
                             '³                                        ³');
                 WRITELN(LST,'ÃÄÄÂÄÄÄÄÄÂÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ´');
              {$I+}
              DruckeKopf:=(IORESULT = 0);
              ZeilenNr:=9;
            END;

         CONST T = '³';
         VAR   T1:STRING[3];
               L:BYTE;

         BEGIN
            FOR L:=2 TO 5 DO B[L]:='       ';
                             B[1]:='        ';
                             B[6]:='        ';
            IF (KBDruckDaten.TagesDatum <> MerkeDatum) AND
               (MerkeDatum <> '') THEN BestandDone:=DruckeBestand
            ELSE
              MerkeDatum:=KBDruckDaten.TagesDatum;

            IF (ZeilenNr >= SeitenLaenge) THEN BEGIN
               DruckeFuss(FALSE);
               ZeilenNr := 0;
               INC(SeitenNr);
               {$I-}
                  WRITE(LST,CHR(12));
               {$I+}
            END;
            BestandDone:=FALSE;
            IF (ZeilenNr = 0) AND NOT(KopfDone) THEN BEGIN
              IF (KBDruckDaten.KBSatzTyp = 1) AND (SeitenNr = 1) THEN
                LfdBestand:=KBDruckDaten.Betrag;
              KopfDone:=DruckeKopf;
            END;
            IF (KBDruckDaten.KBSatzTyp <> 1) AND
               (KBDruckDaten.KBSatzTyp <> 7) THEN BEGIN
              KopfDone:=FALSE;
              WITH KbDruckDaten DO BEGIN
                AktBetrag := Betrag;
                IF (KBSTBuchungsTypen[KBSatzTyp] = 'A') THEN
                  LfdBestand := LfdBestand - AktBetrag
                ELSE
                  LfdBestand := LfdBestand + AktBetrag;
                IF (KBSatzTyp = KBTourTyp) THEN BEGIN
                  T1:=PrintString(MakeShortTourName(TourNr),3);
                  LfdTag:=LfdTag+AktBetrag
                END ELSE
                  T1:=PrintString(BelegNr,3);

                CASE KBSatzTyp OF
                  1:     B[1]:=PrintReal(AktBetrag,6,2,FALSE);
                  2,3,4: B[KBSatzTyp]:=PrintReal(AktBetrag,5,2,FALSE);
                  5,6:   B[5]:=PrintReal(AktBetrag,5,2,FALSE);
                  7:     BEGIN { Scheck } END;
                  8:     B[6]:=PrintReal(AktBetrag,6,2,FALSE);
                END;
                {$I-}
                  IF (BTxt[2] <> '') THEN BEGIN
                    WRITELN(LST,T+COPY(TagesDatum,1,2)+T+COPY(BelegDatum,1,5)+
                                T+T1+T+PrintString(COPY(BTxt[1],1,13),13)+
                                '³        ³       ³       ³       ³       ³        ³');
                    WRITELN(LST,T+'  '+T+'     '+
                                T+'   '+T+PrintString(COPY(BTxt[2],1,13),13)+
                                T+B[1]+T+B[2]+T+B[3]+T+B[4]+T+B[5]+T+B[6]+T);
                  END ELSE
                    WRITELN(LST,T+COPY(TagesDatum,1,2)+T+COPY(BelegDatum,1,5)+
                                T+T1+T+PrintString(COPY(BTxt[1],1,13),13)+
                                T+B[1]+T+B[2]+T+B[3]+T+B[4]+T+B[5]+T+B[6]+T);
                {$I+}
              END;
              INC(ZeilenNr);
            END;
         END;

      BEGIN
         IF KassenDB^.BeginTransaction THEN BEGIN
            SeitenNr := 1;
            ZeilenNr := 0;
            LfdTag   := 0;
            LfdBestand:=0;
            MerkeDatum:='';
            BestandDone:=FALSE;
            InitTKBDaten(KBDruckdaten);
            KBDruckDaten.TagesDatum  := '01.'+Monat;
            KBDruckDaten.TourNr  := 0;
            KBDruckdaten.KBsatzTyp := 1;
            KBDruckdaten.LiefNr := 0;
            KBIndexBegin := CreateKBIndex(2,KBDruckdaten);
            KbDruckDaten.TagesDatum  := '31.'+Monat;
            KbDruckDaten.TourNr  := 999;
            KBDruckdaten.KBsatzTyp := 1;
            KbDruckdaten.LiefNr := 0;
            KBIndexEnd := CreateKBIndex(2,KBDruckdaten);
            IF KassenDB^.StartIntervall(2,KBIndexBegin,KBindexEnd,KBDruckdaten) THEN BEGIN
               DatNam:=MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'))+'PRTFile.TXT';
               PrtToFile(DatNam);
               KopfDone:=FALSE;

               REPEAT
                 DruckeZeile;
               UNTIL NOT KassenDB^.GetIntNext(KBDruckDaten);
               IF ZeilenNr <> 0 THEN BEGIN
                 DruckeBestand;
                 DruckeFuss(TRUE);
               END;

               PrinterToPrt;
               StatusBox.Act('šbergebe gerade Datei','an Drucker');
               WriteTextDatei(DatNam);
               StatusBox.Hide;
            END ELSE BEGIN
               FaultBox.Act(2,'Keine Kassenbucheintrage fr Monat '+Monat);
            END;
            IF (NOT KassenDB^.EndTransaction) THEN
               FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                            KassenDB^.GetTransactionErrMsg);
         END ELSE
            FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                            KassenDB^.GetTransactionErrMsg);
      END;

   PROCEDURE NewIndex;
      VAR F1: FILE OF TKBDaten;
          No: LONGINT;
      BEGIN
        ASSIGN(F1,'C:\KB_0297.AVL');
        RESET(F1);
        READ(F1,KBDruckDaten);
        No:=0;
        KassenDB^.OpenFile('',GMEnvObj^.GetEntry('KBDIR'),TRUE,ActDate);
        WITH KassenDB^ DO BEGIN
          IF BeginTransaction THEN BEGIN
            REPEAT
              READ(F1,KBDruckDaten);

              { Altes format „ndern }

                WITH KBDruckDaten DO BEGIN
                  IF (BTxt[1,3] = '.') THEN BEGIN
                    Btxt[1]:=COPY(Btxt[1],14,12);
                    IF (KBSatzTyp = 6) THEN KBSatzTyp:=2;
                  END;
                  IF LfdNr > No THEN No:=LfdNr;
                END;

              NewRec(KBDruckDaten);
              StatusBox.Act(KBDruckDaten.TagesDatum,'');
            UNTIL EOF(F1) OR KEYPRESSED;
            WRITE(CHR(7));
            SetLfdNr(No);
          END;
          EndTransaction;
        END;
        StatusBox.Hide;
        KassenDB^.CloseFile;
        CLOSE(F1);
      END;

   PROCEDURE Scheckdruck;
      VAR SchecksGefunden : BOOLEAN;

      PROCEDURE DruckeFuss;
         BEGIN
           {$I-}
              WRITELN(LST,'     +-----------+-------+'+REPLICATE('-',27)+'+----------+');
           {$I+}
         END;
      PROCEDURE DruckeZeile;
         PROCEDURE DruckeKopf;
           BEGIN
              {$I-}
                 WRITELN(LST,'     Scheckliste'+REPLICATE(' ',20)+'Monat '+Monat+
                         + REPLICATE(' ',20)+'Seite '+B2s(SeitenNr,0));
                 WRITELN(LST);
                 WRITELN(LST,'     +-----------+-------+'+REPLICATE('-',27)+'+----------+');
                 WRITELN(LST,'     | Liefertag | Kunde | '+PrintString('Text',25)+' | Betrag   |');
                 WRITELN(LST,'     +-----------+-------+'+REPLICATE('-',27)+'+----------+');
              {$I+}
           END;

         BEGIN
            IF (ZeilenNr >= 55) THEN BEGIN
               DruckeFuss;
               ZeilenNr := 0;
               INC(SeitenNr);
               {$I-}
                  WRITE(LST,CHR(12));
               {$I+}
            END;
            IF (ZeilenNr = 0) THEN
               DruckeKopf;
            WITH KbDruckDaten DO BEGIN
               {$I-}
                  WRITELN(LST,'     | '+BelegDatum+'  | '+LeadingZeros(Knr,5)+' | '+
                          PrintString(BTxt[1],25)+' | '+PrintReal(Betrag,6,2,FALSE)+' |');
               {$I+}

            END;
            INC(ZeilenNr);
         END;

      BEGIN
         IF KassenDB^.BeginTransaction THEN BEGIN
            SchecksGefunden := FALSE;
            SeitenNr := 1;
            ZeilenNr := 0;
            InitTKBDaten(KBDruckdaten);
            KbDruckDaten.TagesDatum  := '01.'+Monat;
            KbDruckDaten.TourNr  := 0;
            KBDruckdaten.KBsatzTyp := 0;
            KbDruckdaten.LiefNr := 0;
            KBIndexBegin := CreateKBIndex(2,KBDruckdaten);
            KbDruckDaten.TagesDatum  := '31.'+Monat;
            KbDruckDaten.TourNr  := 999;
            KBDruckdaten.KBsatzTyp := 1;
            KbDruckdaten.LiefNr := 0;
            KBIndexEnd := CreateKBIndex(2,KBDruckdaten);
            IF KassenDB^.StartIntervall(2,KBIndexBegin,KBindexEnd,KBDruckdaten) THEN BEGIN
               DatNam:=MakeFilePath(GMEnvObj^.GetEntry('BASEDIR'))+'PRTFile.TXT';
               PrtToFile(DatNam);
               REPEAT
                  IF (KBDruckDaten.KbSatzTyp = 2) THEN BEGIN
                     Schecksgefunden := TRUE;
                     DruckeZeile;
                  END;
               UNTIL NOT KassenDB^.GetIntNext(KBDruckDaten);
               IF (ZeilenNr <> 0) THEN
                  DruckeFuss;
               PrinterToPrt;
               IF Schecksgefunden THEN BEGIN
                  StatusBox.Act('šbergebe gerade Datei','an Drucker');
                  WriteTextDatei(DatNam);
                  StatusBox.Hide;
               END;
            END;
            IF NOT SchecksGefunden THEN
               FaultBox.Act(2,'Keine Schecks fr Monat '+Monat);
            IF (NOT KassenDB^.EndTransaction) THEN
               FaultBox.Act(0,'EndTransaction auf KassenDB Fehler: '+
                            KassenDB^.GetTransactionErrMsg);
         END ELSE
            FaultBox.Act(0,'BeginTransaction auf KassenDB Fehler: '+
                            KassenDB^.GetTransactionErrMsg);
      END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
