{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT KUNCONV;

INTERFACE
   USES KUNDATEN,
        KD960708;

   PROCEDURE KOld2KNew(VAR Kold:TKD960708;VAR KNew:TKundendaten);
   PROCEDURE KNew2KOld(VAR KNew:TKundendaten;VAR Kold:TKD960708);




IMPLEMENTATION

   USES ZUSAETZE;

   PROCEDURE CreateEtagenInfo(VAR Daten: TKundendaten);
      VAR  LPlatzPos : BYTE;
           UWegBes   : STRING;
           ProofStr  : STRING;
           Lauf      : BYTE;
           NurEtagenInfo : BOOLEAN;
           AUfzugGef : BOOLEAN;
           ZahlA,
           ZahlE     : BYTE;

      CONST MaxEtagen = 10;

      CONST EtagenStrings :ARRAY[1..MaxEtagen] OF STRING[20] =
              ('ETAGE'      ,'STOCK',
               'KELLER'     ,'GARAGE',
               'TIEFPATERRE','ERDGESCHOSS',
               'PATERRE'    ,'HOCHPATERRE',
               'PARTERRE'   ,'HOCHPARTERRE');


      CONST EtagenInfo :ARRAY[1..MaxEtagen] OF STRING[2] =
            (''  ,'',
             'K' ,'G',
             'TP','E',
             'E','HP',
             'E','HP');

      BEGIN
         WITH Daten DO BEGIN
            Lauf :=1;
            AufzugGef := FALSE;
            WHILE (Lauf <= 8) AND NOT AufzugGef DO BEGIN
              AufzugGef := (POS('AUFZUG',UPPER(WegBes[Lauf])) <> 0);
              INC(Lauf);
            END;
            Aufzug := AufzugGef;
            IF (WegBes[1] <> '') THEN BEGIN
               Lauf := 0;
               UWegBes := UPPER(WegBes[1]);
               REPEAT
                  INC(Lauf);
                  LPlatzPos:=POS(EtagenStrings[Lauf],UWegBes);
               UNTIL (LPlatzPos <> 0) OR (Lauf = 6);
               IF (LPlatzPos <> 0) THEN BEGIN
                  ZahlE:= 0;
                  ZAhlA := 0;
                  Etage := EtagenInfo[Lauf];
                  IF (Etage = '') THEN BEGIN
                     IF (LPlatzPos > 1) THEN BEGIN
                        ZahlE := LPlatzPos-1;
                        WHILE (ZahlE > 1) AND (UWegBes[ZahlE] IN [' ','.']) DO
                           DEC(ZahlE);
                        ZahlA := ZahlE;
                        IF (ZahlA > 1) THEN BEGIN
                           WHILE (ZahlA > 1) AND (UWegBes[ZahlA-1] IN ['0'..'9']) DO
                              DEC(ZAhlA);
                        END;
                        Etage := COPY(UWegBes,ZAhlA,ZAhlE-ZahlA+1);
                     END;
                  END;
                  DELETE (UWegBes,LPlatzPos,LENGTH(EtagenStrings[Lauf]));
                  IF (ZahlE <> 0) AND (ZahlA <> 0) THEN BEGIN
                     DELETE(UWegBes,ZahlA,ZahlE-ZahlA+1);
                  END;
                  UWegBes := RemoveAllSpaces(UWegBes);
                  LPlatzPos := 1;
                  NurEtagenInfo := TRUE;
                  IF (UWegBes <> '') THEN BEGIN
                     WHILE (LPlatzPos <= LENGTH(UWegBEs)) AND NurEtagenInfo DO BEGIN
                        IF NOT (UWegBes[LPlatzPos] IN ['.','!']) THEN
                           NurEtagenInfo := FALSE;
                        INC(LPlatzPos);
                     END;
                  END;
                  IF NurEtagenInfo THEN BEGIN
                     FOR Lauf := 1 TO 7 DO
                        WegBes[Lauf]:=WegBes[Lauf+1];
                     WegBes[8] := '';
                  END;
               END;
            END;
         END;
      END;

   PROCEDURE KOld2KNew;
       VAR Lauf : BYTE;
       BEGIN
          InitTKundendaten(KNew);
          WITH KNEW DO BEGIN
              PlanQ := Kold.PlanQ;
              LfdNr := Kold.LfdNr;
              IF (KOld.Anrede = 3) THEN BEGIN
                 Firma :=TRUE;
                 LiefName := KOld.Firma;
                 RechName := LiefName;
                 IF (S2L(Kold.Vorname) <> 0) OR (Kold.Name <> '') THEN BEGIN
                    LiefAPAnrede := S2L(Kold.Vorname);
                    IF (LiefAPAnrede = 0) THEN
                       LiefAPAnrede := 1;
                    RechAPAnrede := LiefAPAnrede;
                    LiefAPartner := Kold.Name;
                    RechAPartner := LiefAPartner;
                 END;
              END ELSE BEGIN
                 Firma := FALSE;
                 LiefName := Kold.Name;
                 RechName := LiefName;
                 LiefVorname := Kold.Vorname;
                 RechVorname := LiefVorname;
              END;
              RechAnrede := Kold.Anrede;
              RechStrasse := Kold.Strasse;
              RechPlz := Kold.Plz;
              RechOrt := Kold.Ort;
              LiefAnrede := Kold.Anrede;
              LiefStrasse := Kold.Strasse;
              LiefPlz := Kold.Plz;
              LiefOrt := Kold.Ort;
              FOR Lauf := 1 TO 3 DO BEGIN
                 TelefonBem[Lauf] := Kold.TelefonBem[Lauf];
                 Telefon[Lauf]:= Kold.Telefon[Lauf];
              END;
              AnlDatum := Kold.AnlDatum;
              AnlUhrzeit := Kold.AnlUhrZeit;
              FOR Lauf := 1 TO 2 DO BEGIN
                 Bemerkung[Lauf]:= Kold.Bemerkung[Lauf];
              END;
              FOR Lauf := 1 TO 8 DO
                 WegBes[Lauf]:= Kold.WegBes[Lauf];
              Zahlweise := 1;
          END;
          CreateEtagenInfo(KNew);
       END;

   PROCEDURE KNew2KOld;
       VAR Lauf : BYTE;
       BEGIN
          InitTKD960708(KOld);
          WITH KOld DO BEGIN
              Plz := KNew.LiefPlz;
              PlanQ := KNew.PlanQ;
              LfdNr := KNew.LfdNr;
              Anrede := KNew.LiefAnrede;
              IF KNew.Firma THEN BEGIN
                 Firma :=Knew.RechName;
                 IF (Knew.RechAPartner <> '') THEN BEGIN
                     Vorname := L2S(Knew.RechAPAnrede,0);
                     Name := Knew.RechAPartner;
                 END;
              END ELSE BEGIN
                 Name := KNew.RechName;
                 Vorname := KNew.RechVorname;
              END;
              Strasse := KNew.RechStrasse;
              Ort := KNew.RechOrt;
              FOR Lauf := 1 TO 3 DO BEGIN
                 TelefonBem[Lauf] := KNew.TelefonBem[Lauf];
                 Telefon[Lauf]:= KNew.Telefon[Lauf];
              END;
              AnlDatum := KNew.AnlDatum;
              AnlUhrzeit := KNew.AnlUhrZeit;
              FOR Lauf := 1 TO 2 DO
                 Bemerkung[Lauf]:= KNew.Bemerkung[Lauf];
              FOR Lauf := 1 TO 8 DO
                 WegBes[Lauf]:= KNew.WegBes[Lauf];
              Kundennummer :=MakeKundennummer(Plz,0,LfdNr);
          END;
       END;

END.




{============================
 Versionshistorie
 $Log:$
 ============================}
