{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Feb 26 09:01:08 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMKAEST;
   {$O+}

INTERFACE

   USES  PL_CONST,
         GMDATEI,
         GLDRUCK,
         LIEDATEN,
         ARTDATEN,
         ZUSAETZE,
         PLACEWIN,
         PRINTER;

   TYPE  TknrPtr  = ^TKnr;
         TKnr     = RECORD
                       KNR : STRING[5];
                       Next : TKNrPtr;
                    END;
         RegKPtr  = ^RegKasten;

         RegKasten = RECORD
                        Menge :LONGINT;
                        MengeHeute : LONGINT;
                        MengeMorgen:LONGINT;
                        OP    :BOOLEAN;
                        ArtNr :STRING[5];
                        Hnr   :STRING[10];
                        ArtBez:STRING[40];
                        KNrListe : TKNrPtr;
                        Next  : RegKPtr;
                     END;
         APtr = ^Andere;
         Andere = RECORD
                     OP     : BOOLEAN;
                     IsK    : BOOLEAN;
                     MengeHeute : LONGINT;
                     MengeMorgen:LONGINT;
                     Menge  : LONGINT;
                     KNrListe : TKNrPtr;
                     ArtBez :STRING[40];
                     Next   : APtr;
                  END;

   TYPE TKaestenObjPtr = ^TKaestenObj;
        TKaestenObj = OBJECT
        ErsterKasten : RegKPtr;
        ErsterAndere : APtr;
        KeinFehler:BOOLEAN;
        CONSTRUCTOR Init;
        PROCEDURE ReleaseKaesten;
        FUNCTION  LSumKUp(Lieferung:TLiefDaten;Heute:BOOLEAN):BOOLEAN;
        PROCEDURE LKasDruck(DLSchluessel,Tour:STRING;
                          Packzettel:BOOLEAN; FahrerName:STRING);
        FUNCTION DruckePlanListe(PlZeiger:PlanungsZeiger;DLSchluessel:STRING;
                           TName,FName: STRING):BOOLEAN;
        FUNCTION DruckePackZettel(Kennung:BYTE;von,bis:STRING;DLSCHLUESSEL:STRING;
                            TName:STRING):BOOLEAN;
        DESTRUCTOR Done;
     END;



IMPLEMENTATION
   USES GMSETUP;

   CONSTRUCTOR TKaestenObj.Init;
      BEGIN
         ErsterKasten := NIL;
         ErsterAndere := NIL;
         KeinFehler := TRUE;
      END;


   PROCEDURE TKaestenObj.ReleaseKaesten;
      VAR   KPtr1,KPtr2:RegKPtr;
            KunPtr1,KunPtr2 : TKNrPtr;
      VAR   APtr1,APtr2:APtr;

      PROCEDURE ReleaseKunden(KBeginn:TKnrPtr);
          VAR  KunPtr1,KunPtr2 : TKNrPtr;
          BEGIN
             KunPtr1 := KBeginn;
             WHILE KunPtr1 <> NIL DO BEGIN
               KunPtr2:= KunPtr1^.Next;
               DISPOSE(KunPtr1);
               KunPtr1:=KunPtr2;
            END;
          END;
      BEGIN
         KPtr1:=ErsterKasten;
         WHILE KPtr1 <> NIL DO BEGIN
            KPtr2:= KPtr1^.Next;
            ReleaseKunden(KPtr1^.KNrListe);
            DISPOSE(KPtr1);
            KPtr1:=KPtr2;
         END;
         APtr1:=ErsterAndere;
         WHILE APtr1 <> NIL DO BEGIN
            APtr2:= APtr1^.Next;
            ReleaseKunden(APtr1^.KNrListe);
            DISPOSE(APtr1);
            APtr1:=APtr2;
         END;
         ErsterKasten := NIL;
         ErsterAndere := NIL;
      END;


   FUNCTION TkaestenObj.LSumKUp;
      VAR   Lauf : BYTE;
            L : LONGINT;
            C : INTEGER;
            OP  : BOOLEAN;
            ArtNr : STRING;
            NReg  : RegKPtr;
            NKun  : TKnrPtr;

      FUNCTION AddReg(ABez:STRING; Menge:LONGINT;
                         ANr:STRING; Hnr:STRING; KNr:STRING;Heute,OP:BOOLEAN):BOOLEAN;
         VAR   zgr1,zgr2,NReg:RegKPtr;

         BEGIN
            AddReg := FALSE;
            IF ErsterKasten <> NIL THEN BEGIN
               Zgr1 := ErsterKasten;
               Zgr2 := ErsterKasten^.Next;
               IF Zgr1^.ArtBez < ABez THEN
                  WHILE (Zgr1^.Next <> NIL) AND (Zgr2^.ArtBez <= ABez) DO BEGIN
                     Zgr1 := Zgr2;
                     Zgr2 := Zgr2^.Next;
                  END;
               IF Zgr1^.ArtBez = ABez THEN BEGIN
                  Zgr1^.Menge := Zgr1^.Menge+Menge;
                  Zgr1^.OP  := Zgr1^.OP AND OP;
                  IF Heute THEN
                     Zgr1^.MengeHeute := Zgr1^.MengeHeute+Menge
                  ELSE
                    Zgr1^.MengeMorgen := Zgr1^.MengeMorgen+Menge;

                  IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                     NEW(Nkun);
                     NKun^.Knr := Knr;
                     NKun^.Next := Zgr1^.KNrListe;
                     Zgr1^.KnrListe := NKun;
                     AddReg := TRUE;
                  END;
               END
               ELSE
                  IF MAXAVAIL > SIZEOF(RegKasten) THEN BEGIN
                     NEW(NReg);
                     NReg^.KnrListe := NIL;
                     NReg^.Menge := Menge;
                     NReg^.OP  := OP;
                     NReg^.MengeHeute := 0;
                     NReg^.MengeMorgen := 0;
                     IF Heute THEN
                        Nreg^.MengeHeute :=Menge
                     ELSE
                        NReg^.MengeMorgen := Menge;
                     NReg^.ArtBez:= ABez;
                     NReg^.ArtNr := ANr;
                     NReg^.HNr   := HNr;
                     IF ABez < ErsterKasten^.ArtBez THEN BEGIN
                        NReg^.Next  := ErsterKasten;
                        ErsterKasten := NReg;
                     END
                     ELSE BEGIN
                        NReg^.Next  := Zgr1^.Next;
                        Zgr1^.Next  := NReg;
                     END;
                     IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                        NEW(Nkun);
                        NKun^.Knr := Knr;
                        NKun^.Next := Nreg^.KNrListe;
                        NReg^.KnrListe := NKun;
                        AddReg := TRUE;
                     END;
                  END;
            END
            ELSE
               IF MAXAVAIL > SIZEOF(RegKasten) THEN BEGIN
                  NEW(ErsterKasten);
                  ErsterKasten^.KnrListe := NIL;
                  ErsterKasten^.Menge := Menge;
                  ErsterKasten^.MengeHeute := 0;
                  ErsterKasten^.MengeMorgen:= 0;

                  IF Heute THEN
                     ErsterKasten^.MengeHeute :=Menge
                  ELSE
                     ErsterKasten^.MengeMorgen := Menge;
                  ErsterKasten^.ArtBez:= ABez;
                  ErsterKasten^.ArtNr := ANr;
                  ErsterKasten^.HNr   := HNr;
                  ErsterKasten^.OP    := OP;
                  ErsterKasten^.Next  := NIL;
                  IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                     NEW(Nkun);
                     NKun^.Knr := Knr;
                     NKun^.Next := ErsterKasten^.KNrListe;
                     ErsterKasten^.KnrListe := NKun;
                     AddReg := TRUE;
                  END;
                  AddReg := TRUE;
               END;
         END;

      FUNCTION AddAnd(ABez:STRING; Menge:LONGINT; KNR:STRING;Heute,OP:BOOLEAN;
               KAFL:LONGINT):BOOLEAN;
         VAR   zgr1,zgr2,NAND :APtr;
               NKun           :TKnrPtr;
               IsK : BOOLEAN;
         BEGIN
            ISK := (KAFL = 1);
            AddAnd := FALSE;
            IF ErsterAndere <> NIL THEN BEGIN
               Zgr1 := ErsterAndere;
               Zgr2 := ErsterAndere^.Next;
               IF Zgr1^.ArtBez < ABez THEN
                  WHILE (Zgr1^.Next <> NIL) AND (Zgr2^.ArtBez <= ABez) DO BEGIN
                     Zgr1 := Zgr2;
                     Zgr2 := Zgr2^.Next;
                  END;
               IF Zgr1^.ArtBez = ABez THEN BEGIN
                  Zgr1^.Menge := Zgr1^.Menge+Menge;
                  IF Heute THEN
                     Zgr1^.MengeHeute :=Zgr1^.MengeHeute+ Menge
                  ELSE
                     Zgr1^.MengeMorgen :=Zgr1^.MengeMorgen+ Menge;
                  Zgr1^.OP := Zgr1^.OP AND OP;
                  IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                     NEW(Nkun);
                     NKun^.Knr := Knr;
                     NKun^.Next := Zgr1^.KnrListe;
                     Zgr1^.KnrListe := NKun;
                     AddAnd:= TRUE;
                  END;
               END
               ELSE
                  IF MAXAVAIL > SIZEOF(Andere) THEN BEGIN
                     NEW(NAnd);
                     NAnd^.Menge := Menge;
                     NAnd^.MengeMorgen := 0;
                     NAnd^.MengeHeute := 0;
                     IF Heute THEN
                        NAnd^.MengeHeute :=Menge
                     ELSE
                        NAnd^.MengeMorgen := Menge;
                     NAnd^.ArtBez:= ABez;
                     NAnd^.OP := OP;
                     Nand^.IsK :=IsK;
                     NAnd^.KnrListe := NIL;
                     IF ABez < ErsterAndere^.ArtBez THEN BEGIN
                        NAnd^.Next  := ErsterAndere;
                        ErsterAndere := NAnd;
                     END
                     ELSE BEGIN
                        NAnd^.Next  := Zgr1^.Next;
                        Zgr1^.Next  := NAnd;
                     END;

                     IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                        NEW(Nkun);
                        NKun^.Knr := Knr;
                        NKun^.Next := Nand^.KnrListe;
                        NAnd^.KnrListe := NKun;
                        AddAnd:= TRUE;
                     END;
                  END;
            END
            ELSE
               IF MAXAVAIL > SIZEOF(Andere) THEN BEGIN
                  NEW(ErsterAndere);
                  ErsterAndere^.Menge := Menge;
                  ErsterAndere^.ArtBez:= ABez;
                  ErsterAndere^.KnrListe  := NIL;
                  ErsterAndere^.Next  := NIL;
                  ErsterAndere^.IsK := IsK;
                  ErsterAndere^.OP := OP;
                  ErsterAndere^.MengeHeute := 0;
                  ErsterAndere^.MengeMorgen:= 0;
                  IF Heute THEN
                     ErsterAndere^.MengeHeute :=Menge
                  ELSE
                     ErsterAndere^.MengeMorgen := Menge;
                  IF (MAXAVAIL > SIZEOF(TKnr)) THEN BEGIN
                     NEW(Nkun);
                     NKun^.Knr := Knr;
                     NKun^.Next := ErsterAndere^.KnrListe;
                     ErsterAndere^.KnrListe := NKun;
                     AddAnd:= TRUE;
                  END;
                  AddAnd := TRUE;
               END;
         END;

      BEGIN
         FOR Lauf := 1 TO 30 DO
            WITH Lieferung DO
               IF (COPY(Positionen[Lauf].ArtBez,1,5) <> 'Pfand') THEN BEGIN
                  VAL (Positionen[Lauf].ArtNr,L,C);
                  OP:=(Positionen[Lauf].Preis = 0);
                  ArtNr := MakeArtNr(L);
                  IF ArtDB.SeekRec(1,ArtNr) THEN BEGIN
                     IF (ArtDB.Rec.Artikel.Artbez = Positionen[Lauf].ArtBez) AND
                        (Artdb.Rec.Artikel.Hnr[1] <> '')
                     THEN
                        KeinFehler := KeinFehler AND
                        AddReg(Positionen[Lauf].ArtBez,
                        Positionen[Lauf].Menge,
                        Positionen[Lauf].ArtNr,
                        Artdb.Rec.Artikel.Hnr[1],Lieferung.Knr,Heute,OP)
                     ELSE
                        KeinFehler := KeinFehler AND
                        AddAnd(Positionen[Lauf].ArtBez,
                        Positionen[Lauf].Menge,KNR,Heute,OP,
                            Positionen[Lauf].KAFL)
                  END;
               END;
         LSumKUp := KeinFehler;
      END;


   PROCEDURE TKaestenObj.LKasDruck;

      VAR   Druckerstatus : LONGINT;
            Zeilen     : WORD;
            Seitenzahl : WORD;
            AnzKisten : WORD;
            AnzKistenAnd : WORD;
            AnzKKistenAnd : WORD;
            AnzKunden : WORD;
            WegVorh    : BOOLEAN;
            Taste : INTEGER;
            KZgr : TKnrPTr;
            zgr1 :RegKPtr;
            zgr2 :APtr;
            GesamtSumme : REAL;
            D: BOOLEAN;

      BEGIN
         IF (ErsterKasten <> NIL) OR (ErsterAndere <> NIL) THEN BEGIN
            {$I-}
            WRITELN(Lst); WRITELN(Lst);
            IF PackZettel THEN
               WRITELN(Lst,'     P A C K Z E T T E L')
            ELSE BEGIN
               WRITELN(LST,'     ',GMEnvObj^.GetEntry('KOPFZEILE1'));
               WRITELN(LST,'     ',GMEnvObj^.GetEntry('KOPFZEILE2'));
               {WRITELN(Lst,'     Getr„nkedienst Fickel');
               WRITELN(LST,'     Ortelsburger Str. 38');
               WRITELN(LST,'     42277 Wuppertal');
               WRITELN(LST,'     Tel.: 0202/8 19 67');  }
            END;
            WRITELN(Lst); WRITELN(LST);
            IF Packzettel THEN BEGIN
              WRITELN(Lst,'     Liefertag: ',DLSchluessel,' ',Tour);
              WRITELN(Lst,'     ',REPLICATE('-',20+LENGTH(Tour)));
            END ELSE BEGIN
              WRITELN(Lst,'     Liefertag: ',DLSchluessel,' ');
              WRITELN(Lst,'     ',REPLICATE('-',19));
            END;

            WRITELN(LST);
            WRITELN(Lst);
            WRITELN(Lst);
            IF PackZettel THEN
               WRITELN(Lst,'     Menge ',' Bezeichnung',REPLICATE(' ',31))
            ELSE
               WRITELN(Lst,'     Menge ','BSt-Nr  Bezeichnung',REPLICATE(' ',31),
               ' H/M ');
            WRITELN(LST);
            AnzKisten := 0;
            zgr1 := ErsterKasten;
            zgr2 := ErsterAndere;
            WHILE zgr1 <> NIL DO BEGIN
               IF zgr1^.OP THEN
                  Write(LST,' OP  ')
               ELSE
                  WRITE(Lst,'     ');
               WRITE(LST,Zgr1^.Menge:5);
               AnzKisten := AnzKisten +Zgr1^.Menge;
               IF Packzettel THEN BEGIN
                  WRITE(lst,'  ',Zgr1^.ArtBez,REPLICATE(' ',42-LENGTH(Zgr1^.ArtBez)));
                  KZgr := Zgr1^.KnrListe;
                  AnzKunden := 0;
                  WHILE KZgr <> NIL DO BEGIN
                     IF (AnzKunden = 0) THEN
                        WRITE(LST,KZgr^.Knr);
                     IF (AnzKunden > 0) THEN
                        WRITE(LST,'/',KZgr^.Knr);
                     IF (KZgr^.Next <> NIL) AND (AnzKunden = 3) THEN BEGIN
                        WRITELN(LST);
                        WRITE(LST,REPLICATE(' ',54));
                     END;
                     INC(AnzKunden);
                     KZgr := KZgr^.Next;
                     AnzKunden := AnzKunden MOD 4;
                  END;
                  WRITELN(LST);
               END
               ELSE BEGIN
                  IF (LENGTH(CutBSpaces(Zgr1^.Hnr)) = 5) THEN
                     WRITE(Lst,'  ',Zgr1^.HNr)
                  ELSE
                     WRITE(Lst,'       ');
                  WRITE(lst,'  ',Zgr1^.ArtBez,REPLICATE(' ',42-LENGTH(Zgr1^.ArtBez)));
                  WRITELN(lst,Zgr1^.MengeHeute:2,'/',Zgr1^.MengeMorgen:2);
               END;
               Zgr1:=Zgr1^.Next;
            END;
            IF (AnzKisten <> 0)THEN BEGIN
               WRITELN(LST,'       ---');
               WRITELN(LST,'     ',AnzKisten:5);
            END;
            WRITELN(Lst);
            WRITELN(Lst);
            AnzKistenAnd := 0;
            AnzKKistenAnd := 0;
            WHILE zgr2 <> NIL DO BEGIN
               IF zgr2^.OP THEN
                  Write(LST,' OP  ')
               ELSE
                  WRITE(Lst,'     ');
               WRITE(LST,Zgr2^.Menge:5);
               IF Zgr2^.Isk THEN
                  AnzKistenAnd := AnzKistenAnd +Zgr2^.Menge
               ELSE
                  AnzKKistenAnd := AnzKistenAnd +Zgr2^.Menge;
               IF Packzettel THEN BEGIN
                  WRITE(lst,'  ',Zgr2^.ArtBez,REPLICATE(' ',42-LENGTH(Zgr2^.ArtBez)));
                  KZgr := Zgr2^.KnrListe;
                  AnzKunden := 0;
                  WHILE KZgr <> NIL DO BEGIN
                     IF (AnzKunden = 0) THEN
                        WRITE(LST,KZgr^.Knr);
                     IF (AnzKunden > 0) THEN
                        WRITE(LST,'/',KZgr^.Knr);
                     IF (KZgr^.Next <> NIL) AND (AnzKunden = 3) THEN BEGIN
                        WRITELN(LST);
                        WRITE(LST,REPLICATE(' ',54));
                     END;
                     INC(AnzKunden);
                     KZgr := KZgr^.Next;
                     AnzKunden := AnzKunden MOD 4;
                  END;
                  WRITELN(LST);
               END
               ELSE BEGIN
                  WRITE(lst,'         ',Zgr2^.ArtBez,REPLICATE(' ',42-LENGTH(Zgr2^.ArtBez)));
                  WRITELN(lst,Zgr2^.MengeHeute:2,'/',Zgr2^.MengeMorgen:2);
               END;
               Zgr2:=Zgr2^.Next;
            END;
            IF (AnzKistenAnd <> 0) OR (AnzKKistenAnd <> 0)THEN BEGIN
               WRITELN(LST,'       ---');
               WRITELN(LST,'     ',AnzKistenAnd:5,'/',AnzKKistenAnd);
            END;
            IF Packzettel THEN BEGIN
               WRITELN(Lst);
               WRITELN(Lst);
               WRITELN(LST,'     Fahrzeug : _________________');
               WRITELN(LST);
               WRITELN(LST);
               WRITELN(LST,'                              _________    ',Replicate('_',30));
               WRITELN(LST,'                               Prfung     ',
               CenterString(FahrerName,30));
            END;
            {$I+}
         END;
      END;

   FUNCTION TKaestenObj.DruckePackzettel;
      VAR Heute      :BOOLEAN;
          AktFahrer  : STRING;
          Lauf : BYTE;
          RLauf : BYTE;

      BEGIN
         ReleaseKaesten;
         KeinFehler := TRUE;
         IF LieferDB.BeginTransaction THEN BEGIN
            IF LieferDB.StartIntervall(Kennung,von,bis,Lieferung) THEN BEGIN
               AktFahrer:= Lieferung.Fahrer;
               REPEAT
                  Heute := (Lieferung.LieferTag = ActDate);
                  LSumKUp(Lieferung,Heute);
                  WITH Lieferung DO
                     FOR Lauf:=1 TO 30 DO BEGIN
                       IF (Positionen[Lauf].ArtNr = '01908') AND
                          (POS(LiefNr,Positionen[Lauf].ArtBez) > 0) THEN BEGIN
                            FOR RLauf := 1 TO Positionen[Lauf].Menge DO
                                LDrucken(Lieferung);
                       END;
                    END;
               UNTIL NOT (LieferDB.GetIntNext(Lieferung));
               IF NOT KeinFehler THEN
                  FaultBox.Act(0,'Zuwenig Speicherplatz Liste unvollst„ndig');
            END
            ELSE
               FaultBox.Act(2,'Angegebene Tour enth„lt keine Lieferungen');
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                     LieferDB.GetTransactionErrMsg);
         END
         ELSE BEGIN
             FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
               LieferDB.GetTransactionErrMsg);
         END;
         LKasDruck(DLSchluessel,TName,TRUE,aktFahrer);
         ReleaseKaesten;
         DruckePackZettel := KeinFehler;
      END;

   FUNCTION TKaestenObj.DruckePlanListe;
      VAR Heute,
          Nebensortiment: BOOLEAN;
          AktFahrer:      STRING;
          Lauf : BYTE;
          RLauf : BYTE;


      BEGIN
         ReleaseKaesten;
         KeinFehler := TRUE;
         IF (PlZeiger <> NIL) THEN BEGIN
            IF LieferDB.BeginTransaction THEN BEGIN
               REPEAT
                  IF LieferDB.GetRec(1,PLZeiger^.Dat.LNr,Lieferung) THEN BEGIN
                     AktFahrer:= Lieferung.Fahrer;
                     Heute := (Lieferung.LieferTag = ActDate);
                     Nebensortiment:=FALSE;
                     WITH Lieferung DO BEGIN
                       FOR Lauf:=1 TO 30 DO
                         IF (Positionen[Lauf].ArtNr = '01700') THEN BEGIN
                           Positionen[1]:=Positionen[Lauf];
                           Nebensortiment:=TRUE;
                         END;
                       IF Nebensortiment THEN
                         FOR Lauf:=2 TO 30 DO BEGIN
                           Positionen[Lauf].Menge := 0;
                           Positionen[Lauf].KAFl  := 0;
                           Positionen[Lauf].ArtNr :='';
                           Positionen[Lauf].ArtBez:='';
                           Positionen[Lauf].Preis := 0;
                           Positionen[Lauf].Pfand := 0;
                         END;
                     END;
                     LSumKUp(Lieferung,Heute);
                     WITH Lieferung DO
                        FOR Lauf:=1 TO 30 DO BEGIN
                           IF (Positionen[Lauf].ArtNr = '01908') AND
                              (POS(LiefNr,Positionen[Lauf].ArtBez) > 0) THEN
                              FOR RLauf := 1 TO Positionen[Lauf].Menge DO
                                  LDrucken(Lieferung);
                        END;
                  END ELSE
                     KeinFehler := FALSE;
                  PlZeiger := PlZeiger^.NextIt;
               UNTIL (PlZeiger = NIL);
               LKasDruck(DLSchluessel,TName,TRUE,FName);
               ReleaseKaesten;
               LieferDB.EndTransaction;
            END ELSE
               KeinFehler := FALSE;
         END;
         DruckePlanListe := KeinFehler;
      END;

   DESTRUCTOR TkaestenObj.Done;
      BEGIN
        ReleaseKaesten;
      END;


BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
