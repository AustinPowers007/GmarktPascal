{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Dec 17 07:19:28 GMT+01:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMScroll;


INTERFACE
   USES ZUSAETZE,
        KUNDATEN,
        LIEDATEN,
        GMSETUP,
        GMCONST,
        GMDATEI;

   TYPE  TFahrerPTr = ^TFahrer;
         TFahrer = RECORD
                      Name :STRING[30];
                      Abrechnung :STRING[30];
                      Next : TFahrerPtr;
                   END;
   TYPE  TFahrerARec = RECORD
                          Name : STRING[30];
                          Abrechnung : STRING[30];
                          Nummer : BYTE;
                       END;
   TYPE  TKundInfo = RECORD
                        RecNum,
                        KNr  : LONGINT;
                        Strasse : STRING[25];
                        LAdr : BYTE;
                        Name : STRING[40];
                     END;

   TYPE  TLiefInfo = RECORD
                        LiefNr,
                        Knr    : STRING;
                        RecNum,
                        TourNr : LONGINT;
                        Fahrer : STRING[25];
                     END;


   VAR   FahrerArray : ARRAY[1..10] OF TFahrerARec;
         FahrerListe : TFahrerPtr;
         KSuchKennung : BYTE; { Achtung diese KSuchKennung nur als Trick
                                1 entspricht allen Kunden
                                2 Strassennamen
                                3 nur Privatkunden
                                4 nur Firmenkunden
                              }
   VAR   KunArray : ARRAY[1..10] OF TKundInfo;
         LieferArray : ARRAY[1..10] OF TLiefInfo;
         LieferaktTourV,
         LieferAktTourB : STRING;


   FUNCTION FahrerSEFunc(Start:BOOLEAN;VAR Msg:STRING):BOOLEAN;
   FUNCTION SearchFahrer(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN;
   FUNCTION ScrollFahrer(Direct:INTEGER; MaxZeilen:BYTE):INTEGER;
   FUNCTION OutputFahrer(Eingabe:BYTE):STRING;

   FUNCTION KundenSEFunc(START:BOOLEAN;VAR Msg:STRING):BOOLEAN;
   FUNCTION SearchKunde(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN;
   FUNCTION ScrollKunde(Direct:INTEGER; MaxZeilen:BYTE):INTEGER;
   FUNCTION OutputKunde(Eingabe:BYTE):STRING; FAR;

   FUNCTION LieferSEFunc(START:BOOLEAN;VAR Msg:STRING):BOOLEAN;
   FUNCTION SearchLiefer(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN;
   FUNCTION ScrollLiefer(Direct:INTEGER; MaxZeilen:BYTE):INTEGER;
   FUNCTION OutputLiefer(Eingabe:BYTE):STRING;


IMPLEMENTATION
    VAR  AktEingFahrer,
         AktZusatz : STRING;


   FUNCTION FahrerSEFunc;
      VAR FahrDir : STRING;
      PROCEDURE DisposeFahrerListe;
         VAR   FHelp :TFahrerPtr;
               FDel : TFahrerPtr;
         BEGIN
            FHelp:= FahrerListe;
            FDel := FahrerListe;
            WHILE (FHelp <> NIL) DO BEGIN
               FDel := Fhelp;
               FHelp :=FHelp^.Next;
               DISPOSE(FDel);
            END;
            FahrerListe := NIL;
         END;

      PROCEDURE CreateFahrerListe;
         VAR   Datei:TEXT;
               AktRec,
               Fold,
               FHelp : TFahrerPtr;
               FahrerText :STRING;
               FahrerName,
               FahrerAbrechnung : STRING;
         BEGIN
            IF (FahrerListe <> NIL) THEN
               DisposeFahrerListe;
            {$I-}
               ASSIGN(Datei,FahrDir+'FAHRER.TXT');
               RESET(DATEI);
            {$I+}
            NEW(AktRec);
            AktRec^.Name := '';
            AktRec^.Abrechnung := '';
            AktRec^.Next := NIL;
            FahrerListe := AktRec;
            WHILE NOT EOF(Datei) DO BEGIN
               {$I-}
               READLN(Datei,FahrerText);
               {$I+}
               SplitString(FahrerText,';',Fahrername,FahrerAbrechnung);
               IF (CutBSpaces(Fahrername) <> '') THEN BEGIN
                  IF (COPY(FahrerName,1,5) = 'Herr ') THEN
                     DELETE(FahrerName,1,5);
                  NEW(AktRec);
                  AktRec^.Name := FahrerName;
                  AktRec^.Abrechnung := FahrerAbrechnung;
                  FHelp := Fahrerliste;
                  FOld := Fahrerliste;
                  WHILE (FHelp^.Name < FahrerName) AND (FHelp^.Next <> NIL) DO BEGIN
                     FOld := FHelp;
                     FHelp := FHelp^.Next;
                  END;
                  IF (FHelp^.Name < FahrerName) THEN BEGIN
                     AktRec^.Next := FHelp^.Next;
                     FHelp^.Next := AktRec;
                  END
                  ELSE BEGIN
                     AktRec^.Next := Fold^.Next;
                     Fold^.Next := AktRec;
                  END;
               END;
            END;
            {$I-}
            CLOSE(Datei);
            {$I+}
         END;
         VAR Ergebnis : BOOLEAN;


      BEGIN
         FahrDir := MakeFilePath(GMEnvObj^.GetEntry('FAHRDIR'));
         Ergebnis := TRUE;
         IF Start THEN BEGIN
            IF SeekFile(FahrDir+'FAHRER.TXT') THEN BEGIN
               CreateFahrerListe
            END ELSE BEGIN
               Ergebnis := FALSE;
               Msg := 'Dateifehler: '+FahrDir+'FAHRER.TXT';
            END;
         END ELSE BEGIN
            DisposeFahrerListe;
         END;
         FahrerSEFunc := Ergebnis;
      END;


   FUNCTION SearchFahrer;
      VAR   FSEARCH: TFahrerPtr;
            Lauf : BYTE;

      BEGIN
         Lauf := 0;
         FSEARCH := FahrerListe;
         IF (Eingabe <> ' ') THEN BEGIN
            WHILE (CutBSpaces(UPPER(Eingabe)) >
                  CutBSpaces(UPPER(FSEARCH^.Name)))
                  AND (FSEARCH^.Next <> NIL)
            DO BEGIN
               INC(Lauf);
               FSEARCH :=FSEARCH^.Next;
            END;
         END;
         FahrerArray[1].Name := FSEARCH^.Name;
         FahrerArray[1].Abrechnung:= FSEARCH^.Abrechnung;
         FahrerArray[1].Nummer := Lauf;
         SearchFahrer :=TRUE;
      END;



   FUNCTION ScrollFahrer;
      VAR   HLauf,
            FLauf,
            Lauf : BYTE;
            FSEARCH : TFahrerPtr;
      BEGIN
         CASE Direct OF
            0: BEGIN
                  IF (FahrerListe = NIL) THEN
                     ScrollFahrer := -1
                  ELSE BEGIN
                     FLauf := 1;
                     FSEARCH := FahrerListe;
                     WHILE (FLauf < MaxZeilen) AND (FSEARCH^.Next <> NIL) DO
                        INC(FLauf);
                     ScrollFahrer := FLauf;
                  END;
               END;
            -2: BEGIN
                   FahrerArray[1].Name := FahrerListe^.Name;
                   FahrerArray[1].Abrechnung := FahrerListe^.Abrechnung;
                   FahrerArray[1].Nummer :=0;
                   ScrollFahrer := 0;
                END;
            2: BEGIN
                  FLauf := 0;
                  FSEARCH := FahrerListe;
                  WHILE (FSEARCH^.Next <> NIL) DO BEGIN
                     INC(FLauf);
                     FSEARCH :=FSEARCH^.Next;
                  END;
                  FahrerArray[1].Name := FSEARCH^.Name;
                  FahrerArray[1].Abrechnung := FSEARCH^.Abrechnung;
                  FahrerArray[1].Nummer := FLauf;
                  ScrollFahrer := 0;
               END;
            -1: BEGIN
                   IF (FahrerArray[1].Nummer > 0) THEN BEGIN
                      FLauf := 0;
                      FSEARCH := FahrerListe;
                      WHILE (FLauf < (FahrerArray[1].Nummer -1)) DO BEGIN
                         FSEARCH := FSEARCH^.Next;
                         INC(FLauf);
                      END;
                      IF MaxZeilen >= 2 THEN BEGIN
                         FOR Lauf :=MaxZeilen DOWNTO 2 DO
                            FahrerArray[Lauf] := FahrerArray[Lauf-1];
                      END;
                      ScrollFahrer := 0;
                      FahrerArray[1].Name := FSEARCH^.Name;
                      FahrerArray[1].Abrechnung := FSEARCH^.Abrechnung;
                      FahrerArray[1].Nummer := FLauf;
                   END
                   ELSE
                      ScrollFahrer :=-1;
                END;
            1: BEGIN
                  FLauf := 0;
                  FSEARCH := FahrerListe;
                  WHILE (FLauf < FahrerArray[MaxZeilen].Nummer) DO BEGIN
                     FSEARCH:= FSEARCH^.Next;
                     INC(FLauf);
                  END;
                  IF (FSEARCH^.Next <> NIL) THEN BEGIN
                     FSEARCH := FSEARCH^.Next;
                     INC(Flauf);
                     IF MaxZeilen > 1 THEN BEGIN
                        FOR Lauf :=1 TO MaxZeilen-1 DO
                           FahrerArray[Lauf] := FahrerArray[Lauf+1];
                     END;
                     ScrollFahrer := 0;
                     FahrerArray[MaxZeilen].Name := FSEARCH^.Name;
                     FahrerArray[MaxZeilen].Abrechnung := FSEARCH^.Abrechnung;
                     FahrerArray[MaxZeilen].Nummer := FLauf;
                  END
                  ELSE
                     ScrollFahrer := -1;
               END;
         END;

      END;

   FUNCTION OutputFahrer(Eingabe:BYTE):STRING;
      BEGIN
         OutPutFahrer := PrintString(FahrerArray[Eingabe].Name,30)
      END;

{========================================================================
 = Scrollfunktionen fÅr das Kundenfenster
 ========================================================================}
   FUNCTION SearchKunde;
      VAR   Ergebnis : BOOLEAN;
            LEingabe : STRING;
            Lauf : BYTE;
      BEGIN
         CASE KsuchKennung OF
            1 : Leingabe := UPPER(Eingabe);
            2 : LEingabe := UPPER(Eingabe);
            3 : LEingabe := MakeFPIndex(Eingabe,TRUE);
            4 : LEingabe := MakeFPIndex(Eingabe,FALSE);
         END;
         Ergebnis :=KundDB.GetRec(KundDb.ScanIdx,LEingabe,Kunde);
         IF NOT Ergebnis THEN
            Ergebnis :=KundDB.GetIntNext(Kunde);
         KunArray[1].Name   := Kunde.LiefName;
         KunArray[1].Strasse:= Kunde.LiefStrasse;
         KunArray[1].KNr    := Kunde.LfdNr;
         KunArray[1].LAdr    := Kunde.LieferAnr;
         KunArray[1].RecNum := Kunddb.Index[KundDB.ScanIdx].NodeNo;
         SearchKunde:=Ergebnis;
      END;

   FUNCTION ScrollKunde;
      VAR   HLauf,
            Lauf : BYTE;
      BEGIN
         CASE Direct OF
            0: BEGIN
                  Lauf := 1;
                  IF KundDb.GetIntTop(Kunde) THEN BEGIN
                     WHILE  (Lauf < MaxZeilen) AND (KundDb.GetIntNext(Kunde)) DO
                        INC(Lauf);
                     ScrollKunde := Lauf;
                  END
                  ELSE
                     ScrollKunde := -1;
               END;
            -2: BEGIN
                   IF KundDb.GetIntTop(Kunde) THEN BEGIN
                      KunArray[1].Name := Kunde.LiefName;
                      KunArray[1].Strasse:=Kunde.LiefStrasse;
                      KunArray[1].KNr    :=Kunde.LfdNr;
                      KunArray[1].LAdr    := Kunde.LieferAnr;
                      KunArray[1].RecNum := Kunddb.Index[KundDb.ScanIdx].NodeNo;
                      ScrollKunde := 0;
                   END
                   ELSE
                      ScrollKunde := -1;
                END;
            2: BEGIN
                  Kunddb.GetIntBottom(Kunde);
                  KunArray[1].Name := Kunde.LiefName;
                  KunArray[1].Strasse:=Kunde.LiefStrasse;
                  KunArray[1].KNr    :=Kunde.LfdNr;
                  KunArray[1].LAdr    := Kunde.LieferAnr;
                  KunArray[1].RecNum := Kunddb.Index[Kunddb.ScanIdx].NodeNo;
                  ScrollKunde := 0;
               END;
            -1: BEGIN
                   KundDb.GetRec(1,MakeLfdNr(KunArray[1].Knr,KunArray[1].LAdr),Kunde);
                   IF KundDb.GetIntPrev(Kunde) THEN BEGIN
                      IF MaxZeilen >= 2 THEN BEGIN
                         FOR Lauf :=MaxZeilen DOWNTO 2 DO
                            KunArray[Lauf] := KunArray[Lauf-1];
                      END;
                      ScrollKunde := 0;
                      KunArray[1].Name := Kunde.LiefName;
                      KunArray[1].Strasse:=Kunde.LiefStrasse;
                      KunArray[1].LAdr    := Kunde.LieferAnr;
                      KunArray[1].KNr    :=Kunde.LfdNr;
                      KunArray[1].RecNum := Kunddb.Index[KundDB.ScanIdx].NodeNo;
                   END
                   ELSE
                      ScrollKunde :=-1;
                END;
            1: BEGIN
                  KundDb.GetRec(1,MakeLfdNr(KunArray[MaxZeilen].Knr,KunArray[MaxZeilen].LAdr),Kunde);
                  IF KundDb.GetIntNext(Kunde) THEN BEGIN
                     IF MaxZeilen > 1 THEN BEGIN
                        FOR Lauf :=1 TO MaxZeilen-1 DO
                           KunArray[Lauf] := KunArray[Lauf+1];
                     END;
                     ScrollKunde := 0;
                     KunArray[MaxZeilen].Name := Kunde.LiefName;
                     KunArray[MaxZeilen].Strasse:=Kunde.LiefStrasse;
                     KunArray[MaxZeilen].LAdr    := Kunde.LieferAnr;
                     KunArray[MaxZeilen].KNr    :=Kunde.LfdNr;
                     KunArray[MaxZeilen].RecNum := Kunddb.Index[KunddB.ScanIdx].NodeNo;
                  END
                  ELSE
                     ScrollKunde := -1;
               END;
         END;
      END;

   FUNCTION OutputKunde;
      VAR   HString: STRING;
      BEGIN
         HString := '';
         CASE KSuchKennung OF
            1,3,4 : BEGIN
                      IF (KunArray[Eingabe].LAdr = 0) THEN BEGIN
                          HString:=LeadingZeros(KunArray[Eingabe].Knr,5)+' '+
                              COPY(KunArray[Eingabe].Name,1,25);
                      END;
                      HString := PrintString(HString,32)+
                         COPY(KunArray[Eingabe].Strasse,1,20);
                      HString := PrintString(HString,52);
                   END;
            2    : BEGIN
                      HString := PrintString(KunArray[Eingabe].Strasse,26);
                      HString:=HString+LeadingZeros(KunArray[Eingabe].Knr,5)+' '+
                               COPY(KunArray[Eingabe].Name,1,20);
                      HString := PrintString(HString,52);
                   END;
         END;
         OutputKunde := HString;
      END;

   FUNCTION KundenSEFunc;
      VAR Ergebnis : BOOLEAN;

      BEGIN
         Ergebnis := TRUE;
         IF Start THEN BEGIN
            KundDB.BeginTransAction;
            CASE KSuchKennung OF
               1 : BEGIN
                      Kunddb.StartIntervall(2,' ',CHR(255),Kunde);
                   END;
               2 : BEGIN
                      Kunddb.StartIntervall(4,' ',
                                           CHR(255),Kunde);
                   END;
               3 : BEGIN
                      Kunddb.StartIntervall(3,MakeFPIndex(' ',TRUE),
                                           MakeFPIndex(CHR(255),TRUE),Kunde);
                   END;
               4 : BEGIN
                      Kunddb.StartIntervall(3,MakeFPIndex(' ',FALSE),
                                              MakeFPIndex(CHR(255),FALSE),Kunde);
                   END;
            END;
         END ELSE
            KundDb.EndTransaction;
         KundenSEFunc := Ergebnis;
      END;

{========================================================================
 = Scrollfunktionen fÅr das Kundenfenster
 =======================================================================}
   FUNCTION SearchLiefer;
      VAR   Ergebnis : BOOLEAN;
            Lauf : BYTE;
      BEGIN
         SearchLiefer:=FALSE;
      END;

   FUNCTION ScrollLiefer;
      VAR   HLauf,
            Lauf : BYTE;
            D: BOOLEAN;
      BEGIN
         CASE Direct OF
            0: BEGIN
                  Lauf := 1;
                  IF NOT LieferDB.GetRec(4,LieferAktTourV,Lieferung) THEN
                     D:= LieferDB.GetNext(4,Lieferung)
                  ELSE
                     D:= TRUE;
                  IF D THEN BEGIN
                     WHILE  (Lauf < MaxZeilen) AND (LieferDB.Index[4].Key<= LieferAktTourB) AND
                            (LieferDb.GetNext(4,Lieferung))
                     DO
                        IF (LieferDB.Index[4].Key<= LieferAktTourB) THEN
                           INC(Lauf);
                     ScrollLiefer := Lauf;
                  END
                  ELSE
                     ScrollLiefer := -1;
               END;
            -2: BEGIN
                   IF NOT LieferDB.GetRec(4,LieferAktTourV,Lieferung) THEN
                      D:= LieferDB.GetNext(4,Lieferung)
                   ELSE
                      d:=TRUE;
                   IF D THEN BEGIN
                      lieferarray[1].TourNr :=lieferung.TourNr;
                      lieferarray[1].LiefNr :=lieferung.LiefNr;
                      lieferarray[1].KNr    :=lieferung.Knr;
                      lieferarray[1].Fahrer :=lieferung.Fahrer;
                      lieferarray[1].RecNum := lieferdb.Index[4].NodeNo;
                      ScrollLiefer := 0;
                   END
                   ELSE
                      ScrollLiefer := -1;
                END;
            2: BEGIN
                  IF LieferDB.GetRec(4,LieferAktTourB,Lieferung) THEN BEGIN
                     REPEAT
                        ;
                     UNTIL (LieferDB.Index[4].Key> LieferAktTourB) OR
                           (NOT LieferDB.GetNext(4,Lieferung));
                     IF (LieferDB.Index[4].Key> LieferAktTourB) THEN
                        LieferDB.GetPrev(4,Lieferung);
                  END;
                  lieferarray[1].TourNr :=lieferung.TourNr;
                  lieferarray[1].LiefNr :=lieferung.LiefNr;
                  lieferarray[1].KNr    :=lieferung.Knr;
                  lieferarray[1].Fahrer :=lieferung.Fahrer;
                  lieferarray[1].RecNum := lieferdb.Index[4].NodeNo;
                  ScrollLiefer := 0;
               END;
            -1: BEGIN
                   LieferDB.GetRec(1,LieferArray[1].LiefNr,Lieferung);
                   IF LieferDB.GetPrev(4,Lieferung) AND
                      (LieferDB.Index[4].Key >= LieferAktTourV)
                   THEN BEGIN
                      IF MaxZeilen >= 2 THEN BEGIN
                         FOR Lauf :=MaxZeilen DOWNTO 2 DO
                            LieferArray[Lauf] := LieferArray[Lauf-1];
                      END;
                      ScrollLiefer := 0;
                      lieferarray[1].TourNr :=lieferung.TourNr;
                      lieferarray[1].LiefNr :=lieferung.LiefNr;
                      lieferarray[1].KNr    :=lieferung.Knr;
                      lieferarray[1].Fahrer :=lieferung.Fahrer;
                      lieferarray[1].RecNum := lieferdb.Index[4].NodeNo;
                   END
                   ELSE
                      ScrollLiefer :=-1;
                END;
            1: BEGIN
                  LieferDB.GetRec(1,LieferArray[MaxZeilen].LiefNr,Lieferung);
                  IF LieferDB.GetNext(4,Lieferung) AND
                     (LieferDB.Index[4].Key <= LieferAktTourB)
                  THEN BEGIN
                     IF MaxZeilen > 1 THEN BEGIN
                        FOR Lauf :=1 TO MaxZeilen-1 DO
                           LieferArray[Lauf] := LieferArray[Lauf+1];
                     END;
                     ScrollLiefer := 0;
                     lieferarray[MaxZeilen].TourNr :=lieferung.TourNr;
                     lieferarray[MaxZeilen].LiefNr :=lieferung.LiefNr;
                     lieferarray[MaxZeilen].KNr    :=lieferung.Knr;
                     lieferarray[MaxZeilen].Fahrer :=lieferung.Fahrer;
                     lieferarray[MaxZeilen].RecNum := lieferdb.Index[4].NodeNo;
                  END
                  ELSE
                     ScrollLiefer := -1;
               END;
         END;

      END;

   FUNCTION OutputLiefer;
      VAR   TString : STRING;

      BEGIN
         TString := PrintString(MakeTourName(LieferArray[Eingabe].TourNr),14);
         OutPutLiefer :=LieferArray[Eingabe].LiefNr+' '+TString+' '+
            +LieferArray[Eingabe].knr+' '+lieferArray[Eingabe].fahrer+
         REPLICATE(' ',25-LENGTH(LieferArray[Eingabe].fahrer))
      END;

   FUNCTION LieferSEFunc;
      VAR Ergebnis : BOOLEAN;

      BEGIN
         Ergebnis := TRUE;
         IF Start THEN
            LieferDB.BeginTransAction
         ELSE
            LieferDb.EndTransaction;
         LieferSEFunc := Ergebnis;
      END;


END.
{============================
 Versionshistorie
 $Log:$
 ============================}
