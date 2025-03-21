{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Thu Oct 26 16:15:40 GMT+02:00 1995
 Dateihistorie am Ende der Datei
 ============================}
PROGRAM TestSearchFenster;

   USES  CASHNEU,
         PLACEWIN,
         GRAPNEU,
         GRAPH,
         CASHDATA,
         ZUSAETZE,
         ASTDATA,
         KUNDATEN,
         KUNFILE;
   TYPE  KunRec = RECORD
                     Name :STRING[20];
                     Knr  :STRING[5]; 
                  END;

   TYPE  TFahrerPTr = ^TFahrer;
         TFahrer = RECORD
                      Name :STRING[30]; 
                      Next : TFahrerPtr; 
                   END;
   TYPE  TFahrerARec = RECORD
                          Name : STRING[30];
                          Nummer : BYTE;
                       END;
   VAR   FahrerArray : ARRAY[1..10] OF TFahrerARec;

   VAR   Kunddb : TKundenFile;
         SearchFenster2,
         SearchFenster : TSearchFenster;
         EndCode : INTEGER;
         FahrerListe : TFahrerPtr;
         LSchlFenster7: TASFenster;
         aktfahrer :sTring;

   VAR   KunArray : ARRAY[1..10] OF KunRec;
         Kunde   : TKundenDaten;
         Hilfszahl : INTEGER;

   FUNCTION SearchKunde(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN; FAR;
      VAR   Ergebnis : BOOLEAN;
            Lauf : BYTE;
      BEGIN
         Ergebnis :=KundDB.GetRec(2,Eingabe,Kunde); 
         IF NOT Ergebnis THEN
            Ergebnis :=KundDB.GetNext(2,Kunde); 
         FOR Lauf := 1 TO MaxZeilen DO BEGIN
            KunArray[Lauf].Name := Kunde.Name; 
            KunArray[Lauf].Knr := MakeLfdNummer(Kunde.LfdNr); 
         END; 
         SearchKunde:=Ergebnis; 
      END; 
   
   FUNCTION ScrollKunde(Direct:INTEGER; MaxZeilen:BYTE):INTEGER; FAR; 
      VAR   HLauf,
            Lauf : BYTE; 
      BEGIN
         CASE Direct OF
            0: BEGIN
                  Lauf := 1; 
                  IF KundDb.GetTop(2,Kunde) THEN BEGIN
                     WHILE  (Lauf < MaxZeilen) AND (KundDb.GetNext(2,Kunde)) DO
                        INC(Lauf); 
                     ScrollKunde := Lauf; 
                  END 
                  ELSE
                     ScrollKunde := -1; 
               END; 
            -2: BEGIN
                   IF KundDb.GetTop(2,Kunde) THEN BEGIN
                      KunArray[1].Name := Kunde.Name; 
                      KunArray[1].Knr := MakeLfdNummer(Kunde.Lfdnr); 
                      ScrollKunde := 0; 
                   END 
                   ELSE
                      ScrollKunde := -1; 
                END; 
            2: BEGIN
                  Kunddb.GetRec(2,CHR(255),Kunde); 
                  KunArray[1].Name := Kunde.Name; 
                  KunArray[1].Knr := MakeLfdNummer(Kunde.Lfdnr); 
                  ScrollKunde := 0; 
               END; 
            -1: BEGIN
                   KundDb.GetRec(4,KunArray[1].Knr,Kunde); 
                   IF KundDb.GetPrev(2,Kunde) THEN BEGIN
                      IF MaxZeilen >= 2 THEN BEGIN
                         FOR Lauf :=MaxZeilen DOWNTO 2 DO
                            KunArray[Lauf] := KunArray[Lauf-1]; 
                      END; 
                      ScrollKunde := 0; 
                      KunArray[1].Name := Kunde.Name; 
                      KunArray[1].Knr :=MakeLfdNummer(Kunde.LfdNr); 
                   END 
                   ELSE
                      ScrollKunde :=-1; 
                END; 
            1: BEGIN
                  KundDb.GetRec(4,KunArray[MaxZeilen].Knr,Kunde); 
                  IF KundDb.GetNext(2,Kunde) THEN BEGIN
                     IF MaxZeilen > 1 THEN BEGIN
                        FOR Lauf :=1 TO MaxZeilen-1 DO
                           KunArray[Lauf] := KunArray[Lauf+1]; 
                     END; 
                     ScrollKunde := 0; 
                     KunArray[MaxZeilen].Name := Kunde.Name; 
                     KunArray[MaxZeilen].Knr := MakeLfdNummer(Kunde.LfdNr); 
                  END 
                  ELSE
                     ScrollKunde := -1; 
               END; 
            2: BEGIN
                  ScrollKunde := 0; 
               END; 
         END; 
         
      END; 
   
   FUNCTION OutputKunde(Eingabe:BYTE):STRING; FAR; 
      BEGIN
         OutPutKunde := PrintString(KunArray[Eingabe].Name,20)+' -  '
         +KunArray[Eingabe].Knr;
      END;

   PROCEDURE DisposeFahrerListe;
      VAR   FHelp :TFahrerPtr; 
            FDel : TFahrerPtr; 
      BEGIN
         FHelp:= FahrerListe; 
         FDel := FahrerListe; 
         WHILE (FHelp <> NIL) DO BEGIN
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
            Garbage : STRING; 
      BEGIN
         IF (FahrerListe <> NIL) THEN
            DisposeFahrerListe; 
         ASSIGN(Datei,'FAHRER.TXT'); 
         RESET(DATEI); 
         NEW(AktRec); 
         AktRec^.Name := ''; 
         AktRec^.Next := NIL; 
         FahrerListe := AktRec; 
         WHILE NOT EOF(Datei) DO BEGIN
            READLN(Datei,FahrerText); 
            SplitString(FahrerText,';',Fahrername,Garbage); 
            IF (CutBSpaces(Fahrername) <> '') THEN BEGIN
               NEW(AktRec); 
               AktRec^.Name := FahrerName;
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
         CLOSE(Datei); 
      END; 
   
   FUNCTION SearchFahrer(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN; FAR;
      VAR   FSEARCH: TFahrerPtr;
            Lauf : BYTE;
            SuchString : STRING;

      BEGIN
         Lauf := 0;
         FSEARCH := FahrerListe;
         SuchString := UPPER(SuchString);
         IF (Eingabe <> ' ') THEN BEGIN
             WHILE (UPPER(Eingabe) > UPPER(FSEARCH^.Name))
                  AND (FSEARCH^.Next <> NIL)
            DO BEGIN
               INC(Lauf); 
               FSEARCH :=FSEARCH^.Next; 
            END; 
         END;
         FahrerArray[1].Name := FSEARCH^.Name;
         FahrerArray[1].Nummer := Lauf; 
         SearchFahrer :=TRUE; 
      END; 
   
   
   
   FUNCTION ScrollFahrer(Direct:INTEGER; MaxZeilen:BYTE):INTEGER; FAR; 
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
                     FahrerArray[MaxZeilen].Nummer := FLauf; 
                  END 
                  ELSE
                     ScrollFahrer := -1; 
               END; 
         END; 
         
      END; 
   
   FUNCTION OutputFahrer(Eingabe:BYTE):STRING; FAR; 
      BEGIN
         OutPutFahrer := PrintString(FahrerArray[Eingabe].Name,30)
      END; 

   
   

   
   
   
   
   
BEGIN
   FahrerListe := NIL;
   KundDb.OpenFile('KUNDDB');
   SearchFenster.Init(85,95,555,305,0,'Kundenliste','',Aktiv,ScrollKunde,
   SearchKunde,OutputKunde);
   SearchFenster2.Init(85,95,555,305,0,'Fahrerliste','',Aktiv,ScrollFahrer,
   SearchFahrer,OutputFahrer);
   CreateFahrerListe;
   Hilfszahl := -1;
   AktFahrer := 'Herr Miesel';
   LSchlFenster7.Init(125,210,515,270,0,'Fahrer','ASIN3',Aktiv,HilfsZahl,IsValid);
   LSchlFenster7.AddFahrerFeld(1,'Fahrer:',1,1,AktFahrer,'',30,@SearchFenster2);

   EndCode := LschlFenster7.InputWithClear;
   EndCode := SearchFenster2.InputWithClear;
   CLOSEGRAPH;
END.


{============================
 Versionshistorie
 $Log:$
 ============================}
