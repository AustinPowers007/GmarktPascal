{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Thu Oct 31 15:20:12 GMT+01:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMFAHRER;

INTERFACE
   USES GMBASE,
        GMSCROLL,
        GMSETUP,
        CASHNEU,
        ZUSAETZE;

   VAR FSuchFenster :TSearchFensterPtr;


   FUNCTION NeuerFahrer(VAR Aktuell:STRING):BOOLEAN;FAR;
   FUNCTION AendereFahrer(VAR Aktuell:STRING):BOOLEAN;
   PROCEDURE InitFahrerInput;

IMPLEMENTATION
   USES ASTDATA,
        PLACEWIN,
        CASHDATA;
   VAR FahrerEingabe : TASFensterPtr;

   {*********************************************************************
   * Kaesten andrucken
   *********************************************************************}


   FUNCTION NeuerFahrer(VAR Aktuell:STRING):BOOLEAN;
      VAR   Ergebnis : BOOLEAN;
      VAR   FNeu,FAlt : TEXT;
      VAR   EndCode : INTEGER;
            fahrString : STRING;
            FahrDir : STRING;
            fahrName:STRING;
            FahrRest:STRING;

      BEGIN
         fahrDir := MakeFilePath(GMEnvObj^.GetEntry('FAHRDIR'));
         Ergebnis := FALSE;
         AktEingFahrer:= '';
         AktZusatz := '';
         EndCode := FahrerEingabe^.InputWithBrain;
         IF ((EndCode = 13) OR (EndCode = -68)) AND (CutBSpaces(AktEingFahrer) <> '') THEN BEGIN
            IF (COPY(AktEingFahrer,1,5) <> 'Herr ') THEN
               AktEingFahrer := 'Herr '+AktEingFahrer;
            CopyFile(FahrDir+'FAHRER.TXT',FahrDir+'FOLD.TXT');
            {$I-}
            ASSIGN(FAlt,FahrDir+'FOLD.TXT');
            ASSIGN(FNeu,FahrDir+'FAHRER.TXT');
            RESET(Falt);
            REWRITE(FNeu);
            {$I+}
            WHILE NOT EOF(Falt) DO BEGIN
               {$I-}
               READLN(Falt,FahrString);
               {$I+}
               SplitString(FahrString,';',FahrName,FahrRest);
               IF (CutBSpaces(FahrName) <= CutBSpaces(aktEingFahrer)) OR Ergebnis THEN
                  {$I-}
                  WRITELN(FNeu,FahrString)
                  {$I+}
               ELSE BEGIN
                  IF (CutBSpaces(FahrName) <> CutBSpaces(aktEingFahrer)) THEN BEGIN
                     {$I-}
                     WRITELN(FNeu,aktEingFahrer+';'+AktZusatz);
                     {$I+}
                  END;
                  {$I-}
                  WRITELN(FNeu,FahrString);
                  {$I+}
                  Ergebnis := TRUE;
               END;
            END;
            {$I-}
            CLOSE(FAlt);
            CLOSE(FNeu);
            {$I+}
         END;
         NeuerFahrer := Ergebnis;
      END;

   FUNCTION AendereFahrer(VAR Aktuell:STRING):BOOLEAN;
      VAR   Ergebnis : BOOLEAN;
      VAR   FNeu,FAlt : TEXT;
      VAR   EndCode : INTEGER;
            fahrString : STRING;
            fahrName:STRING;
            FahrRest:STRING;
            FSEARCH:TFahrerPtr;
            FahrDir :STRING;
            Msg : STRING;

      BEGIN
         FahrDir:= MakeFilePath(GMEnvObj^.GetEntry('FAHRDIR'));
         Ergebnis := FALSE;
         IF (CutBSpaces(Aktuell) <> '') THEN BEGIN
            AktEingFahrer := Aktuell;
            Msg := '';
            IF FahrerSEFunc(TRUE,Msg) THEN BEGIN
               FSEARCH := FahrerListe;
               WHILE (FSEARCH <> NIL) AND (CutBSpaces(FSEARCH^.Name) <> CutBSpaces(Aktuell)) DO
                 FSEARCH := FSEARCH^.Next;
                 IF (FSEARCH <> NIL) THEN
                   AktZusatz := FSEARCH^.Abrechnung;
                 FahrerSeFunc(FALSE,Msg);
                 FahrerEingabe^.Disable('1');
                 EndCode := FahrerEingabe^.InputWithBrain;
                 FahrerEingabe^.Enable('1');
                 IF ((EndCode = 13) OR (EndCode = -68)) AND (CutBSpaces(AktEingFahrer) <> '') THEN BEGIN
                    IF (COPY(AktEingFahrer,1,5) <> 'Herr ') THEN
                       AktEingFahrer := 'Herr '+AktEingFahrer;
                    CopyFile(Fahrdir+'FAHRER.TXT',FahrDir+'FOLD.TXT');
                   {$I-}
                   ASSIGN(FAlt,FahrDir+'FOLD.TXT');
                   ASSIGN(FNeu,FahrDir+'FAHRER.TXT');
                   RESET(Falt);
                   REWRITE(FNeu);
                   {$I+}
                   WHILE NOT EOF(Falt) DO BEGIN
                      {$I-}
                      READLN(Falt,FahrString);
                      {$I+}
                      SplitString(FahrString,';',FahrName,FahrRest);
                      IF ((CutBSpaces(FahrName) < CutBSpaces(aktEingFahrer)) OR Ergebnis) AND
                         (CutBSpaces(FahrName) <> CutBSpaces(aktFahrer))THEN
                         {$I-}
                         WRITELN(FNeu,FahrString)
                         {$I+}
                      ELSE BEGIN
                         {$I-}
                         WRITELN(FNeu,aktEingFahrer+';'+AktZusatz);
                         {$I+}
                         Ergebnis := TRUE;
                      END;
                   END;
                   CLOSE(FAlt);
                   CLOSE(FNeu);
               END;
            END ELSE
               FaultBox.Act(0,Msg);
         END
         ELSE
            Ergebnis := FALSE;
         IF Ergebnis THEN BEGIN
             Aktuell := AktEingFahrer;
             IF (COPY(Aktuell,1,5) = 'Herr ') THEN
                DELETE(Aktuell,1,5);
         END;
         AendereFahrer := Ergebnis;
      END;

   PROCEDURE InitFahrerInput;
      BEGIN
         NEW(FahrerEingabe);
         FahrerEingabe^.Init(125,210,515,290,0,'Fahrer anlegen','ASIN3',Aktiv);
         FahrerEingabe^.AddString('','1','Fahrer:',1,1,AktEingFahrer,'',30);
         FahrerEingabe^.AddString('','2','Abrechnung:',1,2,AktZusatz,'',30);

         NEW(FSuchFenster);
         FSuchFenster^.Init(85,95,555,305,0,'Fahrer','',Aktiv,ScrollFahrer,SearchFahrer,OutputFahrer,FahrerSEFunc);
      END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
