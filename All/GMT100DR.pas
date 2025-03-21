{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 16:48:00 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMT100DR;
{$O+}

INTERFACE

    USES LIEDATEN,
         PRINTER,
         ZUSAETZE,
         GMDATEI,
         GMCONST;



   CONST WechselGeld  = 100.0;

   TYPE  LeergutS =   RECORD
                         LeerZahl: ARRAY[1..15] OF LONGINT;
                      END;

         LeergutF =   FILE OF LeergutS;

         LiefSatz =   RECORD
                          LNr,
                          KNr:   STRING[5];
                          Don:   BOOLEAN;
                          LeerG: ARRAY[1..15] OF INTEGER;
                          Sonst,
                          Summe,
                          Zahlt,
                          Sheck: DOUBLE;
                        END;

    PROCEDURE DruckeT100Kopf(Lieferung:TLiefDaten);
    FUNCTION  DruckeT100Zeile(Lieferung:TLiefDaten):BOOLEAN;
    PROCEDURE DruckeT100Fuss;
    PROCEDURE ReadLeergut(Lieferung:TLiefDaten);
    PROCEDURE BerechneSoll(VAR Lieferung:TLiefDaten);
    PROCEDURE InitStartText;
    PROCEDURE SetLeerZa(Nr:BYTE;Wert:LONGINT);
    PROCEDURE AddStartText(Txt:STRING);



IMPLEMENTATION

   USES GMBASE,
        GMT200DR,
        KBDATEN;

   VAR StartPosi:    BYTE;
       StartText: ARRAY[1..10] OF STRING[80];

   VAR ABSum : TT200Zeile;


   VAR AktLTag : TDatum;
       AktTnr  : WORD;
   VAR LeerZahl: ARRAY[1.. 4] OF REAL;
       LeerZa,
       LeerBack: ARRAY[1..PfandTAnzahl] OF LONGINT;

   PROCEDURE KBSummeEintragen;
     BEGIN
        WITH KBEintrag^ DO BEGIN
           TagesDatum := ActDate;
           BelegDatum :=AktLTag;
           BelegNr := '';
           BTxt[1] := AbSum.Fahr;
           BTxt[2] := '';
           BTxt[3] := '';
           Konto := '';
           KBSatzTyp := KBTourTyp;
           KNr := 0;
           LiefNr := 0;
           TourNr := AktTnr;
           Betrag := AbSum.Bar; {AbSum.IstU;}
        END;
     END;

   PROCEDURE KBScheckeintragen(ScheckBetrag:REAL);
     BEGIN
        WITH KBEintrag^ DO BEGIN
           TagesDatum := ActDate;
           BelegDatum := Lieferung.Liefertag;
           BelegNr := '';
           BTxt[1] := Lieferung.Knr+' - '+
                     Lieferung.LiefNr;
           BTxt[2] := '';
           BTxt[3] := '';
           Konto := '';
           KBSatzTyp := KBScheckTyp;
           TourNr := AktTNr;
           LiefNr := S2l(Lieferung.LiefNr);
           KNR := S2l(Lieferung.Knr);
           Betrag := ScheckBetrag;
        END;
     END;




   PROCEDURE InitStartText;
      VAR Lauf : BYTE;
      BEGIN
         StartPosi := 0;
         FOR Lauf := 1 TO 10 DO
            StartText[Lauf] := '';
      END;

   PROCEDURE AddStartText;
      BEGIN
         INC(StartPosi);
         StartText[StartPosi] := Txt;
      END;

   PROCEDURE SetLeerZA;
      BEGIN
         LeerZa[Nr] := Wert;
      END;

   PROCEDURE Berechnung(Lieferung:TLiefDaten);
      VAR Lauf:  BYTE;
          PLauf :BYTE;
          PWert : WORD;
          PFound : BOOLEAN;

      BEGIN
         FOR Lauf:=1 TO PfandTAnzahl DO
            LeerBack[Lauf]:=0;
         FOR Lauf:=1 TO 30 DO
            WITH Lieferung.Positionen[Lauf] DO BEGIN
               PFound := FALSE;
               Plauf := 1;
               PWert := R2L(Pfand*100);
               WHILE (Plauf <= PfandTAnzahl) AND (NOT Pfound) DO BEGIN
                  IF (PWert= PfandTypen[PLauf]) THEN BEGIN
                     PFound := TRUE;
                     INC(LeerBack[Plauf],Menge);
                  END;
                  INC(Plauf);
               END;
               IF (NOT Pfound) THEN BEGIN
                  LeerZahl[1]:=LeerZahl[1]+Menge*Pfand;
               END;
            END;
         END;

   PROCEDURE ReadLeergut;
      VAR WorkStr,
          Bemerk,
          JetztStr:STRING[240];
          E:       STRING;
          Lauf,
          B:       BYTE;
          C,P:INTEGER;
          Sorte,
          Anzahl:LONGINT;
          Summe: REAL;
          PLauf :BYTE;
          PWert : WORD;
          PFound : BOOLEAN;

      BEGIN
         WITH Lieferung DO BEGIN
            FOR Lauf:=1 TO 4 DO
               LeerZahl[Lauf]:=0;
            FOR Lauf:=1 TO PfandTAnzahl DO
               LeerBack[Lauf]:=0;
            IF (COPY(Bemerkungen[1],4,4) = 'TAI=') THEN BEGIN
               Bemerk:=Bemerkungen[1]+Bemerkungen[2];
               WorkStr:=COPY(Bemerk,8,LENGTH(Bemerk)-7);
               REPEAT
                  P:=POS(';',WorkStr);
                  JetztStr:=COPY(WorkStr,1,P-1);
                  WorkStr:=COPY(WorkStr,P+1,LENGTH(WorkStr)-P);
                  P:=POS(':',JetztStr);
                  VAL(COPY(JetztStr,1,P-1),Sorte,C);
                  VAL(COPY(JetztStr,P+1,LENGTH(JetztStr)-P),Anzahl,C);
                  CASE Sorte OF
                      991: LeerZahl[1] := (Anzahl/100);
                      994: BEGIN
                              LeerZahl[4] := (Anzahl/100);
                           END;
                      ELSE BEGIN
                              Plauf := 1;
                              PFound := FALSE;
                              WHILE (Plauf <= PfandTAnzahl) AND (NOT Pfound) DO BEGIN
                                 IF (Sorte= PfandTypen[Plauf]) THEN BEGIN
                                    PFound := TRUE;
                                    LeerBack[Plauf]:= Anzahl;
                                 END;
                                 INC(Plauf);
                              END;
                           END;
                  END;
               UNTIL LENGTH(WorkStr) = 0;
               LeerZahl[2]:=SollUmsatz;
               LeerZahl[3]:=IstUmsatz
            END ELSE BEGIN
               Berechnung(Lieferung);
               Summe:=0;
               FOR Lauf:=1 TO 30 DO
                  WITH Lieferung.Positionen[Lauf] DO
                     IF (Preis <> 0) THEN
                        Summe:=Summe+Menge*Preis;
               LeerZahl[3]:=Summe
            END
         END;
      END;

   PROCEDURE BerechneSoll;
      VAR Lauf : BYTE;

      BEGIN
         Lieferung.SollKaesten := 0;
         Lieferung.SollUmsatz := 0;
         FOR Lauf := 1 TO 30 DO
            WITH Lieferung.Positionen[Lauf] DO BEGIN
               IF KAFL = 1 THEN
                  Lieferung.SollKaesten :=Lieferung.SollKaesten+Menge;
               Lieferung.SollUmsatz:= Lieferung.SollUmsatz+Preis*Menge;
            END;
         IF (NOT Lieferung.T100Bearbeitet) THEN BEGIN
            Lieferung.IstNormal := Lieferung.SollKaesten;
            Lieferung.IstLeergut:= Lieferung.SollKaesten;
            Lieferung.IstUmsatz := 0;
         END;
      END;

   PROCEDURE DruckeT100Kopf;
      VAR L:BYTE;
          TNummer: BYTE;
          TName :STRING;
      BEGIN
         AbSum.OpenP := 0;
         AbSum.SchAn := 0;
         AbSum.SollK := 0;
         AbSum.SollU := 0;
         AbSum.IstK  := 0;
         AbSum.IstS  := 0;
         AbSum.IstL  := 0;
         AbSum.SollLW:= 0;
         AbSum.IstLW := 0;
         AbSum.IstU  := 0;
         AbSum.SchS  := 0;
         AbSum.Fahr  :='';
         AbSum.BLen  := 3+StartPosi;
         AktLTag := Lieferung.LieferTag;
         AktTNr := Lieferung.TourNr;

         {$I-}
            FOR L:=1 TO StartPosi DO
               WRITELN(LST,StartText[L]);
            WRITELN(LST);
            WRITELN(LST,'     Tourbegleitzettel: T 100',REPLICATE(' ',33),Lieferung.Liefertag,
                         ' ',MakeShortTourName(Lieferung.TourNr));
            WRITELN(LST);
            WRITELN(LST,'     | Kunde | Lief-S.|| Nor.| Son.| Leer | Umsatz |  Zahlt | O | Scheck |');
         {$I+}
         IF (IORESULT <> 0) THEN ;
       END;

   FUNCTION DruckeT100Zeile;
      VAR Lauf : BYTE;
          ScheckSu: REAL;
          C:        INTEGER;
          GesamtIst : REAL;
          Diff:     STRING[1];
          ScheckS:  STRING[7];

      FUNCTION GetLeerSum:REAL;
         VAR Summe:REAL;
             PfandWert : REAL;
             Lauft:BYTE;
         BEGIN
            Summe:=0;
            FOR Lauft:=1 TO PfandTAnzahl DO BEGIN
               IF (LeerBack[Lauft] <> 0) THEN BEGIN
                  PfandWert := PfandTypen[LaufT]/100;
                  Summe := Summe+LeerBack[LaufT]*PfandWert;
               END;
            END;
            GetLeerSum:=Summe
         END;

      BEGIN
         DruckeT100Zeile := FALSE;
         INC(AbSum.BLen,2);
         IF (Lieferung.Bemerkungen[1][1] = 'J') THEN BEGIN
            ReadLeerGut(Lieferung);
            AbSum.Fahr := Lieferung.Fahrer;
            AbSum.SollK:= AbSum.SollK+Lieferung.SollKaesten;
            AbSum.SollU:= AbSum.SollU+Lieferung.SollUmsatz;
            AbSum.IstK:= AbSum.IstK+Lieferung.IstNormal;
            AbSum.IstS:= AbSum.IstS+Lieferung.IstSonder;
            AbSum.IstL:= AbSum.IstL+Lieferung.IstLeergut;
            AbSum.SollLW:=AbSum.SollLW+GetLeerSum;

            ScheckSu:=LeerZahl[4];
            IF (ABS(LeerZahl[4]) > 0.01) THEN BEGIN
               GesamtIst := LeerZahl[4]+Lieferung.IstUmsatz;
               DruckeT100Zeile := TRUE;
               KBScheckEintragen(LeerZahl[4]);
               STR(LeerZahl[4]:7:2,ScheckS);
               IF (GesamtIst <= Lieferung.SollUmsatz) THEN BEGIN
                  AbSum.IstU:=AbSum.IstU+GesamtIst;
                  Lieferung.IstUmsatz:=GesamtIst;
               END ELSE BEGIN
                  AbSum.IstU:=AbSum.IstU+Lieferung.SollUmsatz;
                  Lieferung.IstUmsatz:=Lieferung.SollUmsatz
               END
            END ELSE BEGIN
               ScheckS:='       ';
               AbSum.IstU:= AbSum.IstU+Lieferung.IstUmsatz;
            END;
            IF (ABS(Lieferung.IstUmsatz - Lieferung.SollUmsatz) >= 0.01) THEN BEGIN
               INC(AbSum.OpenP);
               Diff:='X'
            END ELSE
               Diff:=' ';
            IF (ScheckSu > 0) THEN INC(AbSum.SchAn);
               AbSum.SchS:= AbSum.SchS+ScheckSu;
            {$I-}
               WRITELN(LST,'     |-------|--------||-----|-----|------|--------|--------|---|--------|');
               WRITE(LST,'     | ',Lieferung.Knr,' |  ',Lieferung.LiefNr,' || ');
               WRITELN(LST,Lieferung.IstNormal:3,' | ',
                           Lieferung.IstSonder:3,' | ',
                           Lieferung.IstLeerGut:4,' |',Lieferung.SollUmsatz:7:2, ' |',
                           Lieferung.IstUmsatz:7:2,' | ',Diff,' |',ScheckS,' |');
            {$I+}
         END ELSE BEGIN
            {$I-}
               WRITELN(LST,'     |-------|--------||-----|-----|------|--------|--------|---|--------|');
               WRITELN(LST,'     | ',Lieferung.Knr,' |  ',Lieferung.LiefNr,
                           ' || Kunde nicht beliefert !!!                       | ');
            {$I+}
         END;
         IF (IORESULT <> 0) THEN ;
      END;

   PROCEDURE DruckeT100Fuss;
      VAR L1,L2,
          Lauf:BYTE;
          LeerDiff: REAL;
          Pos,
          DiffBLen: LONGINT;
          LSS,
          LeergutString1,
          LeergutString2: STRING;
          Found:BOOLEAN;

      PROCEDURE SZ(Num:BYTE;Text:STRING);
         BEGIN
           IF (DiffBLen > Num) THEN DEC(DiffBLen) ELSE WRITELN(LST,Text);
         END;


      FUNCTION GetLeergutIst:REAL;
         VAR Summe:REAL;
             PfandWert : REAL;
             Lauft:BYTE;
         BEGIN
            Summe:=0;
            FOR Lauft:=1 TO PfandTAnzahl DO BEGIN
               IF (LeerZa[Lauft] <> 0) THEN BEGIN
                  PfandWert := PfandTypen[LaufT]/100;
                  Summe := Summe+LeerZa[LaufT]*PfandWert;
               END;
            END;
            GetLeergutIst:=Summe
         END;


      BEGIN
         AbSum.IstLW:=GetLeergutIst;
         LeerDiff:=AbSum.SollLW-AbSum.IstLW;
         IF (LeerDiff < 0.01) THEN
            LeerDiff:=0;
         INC(AbSum.Blen,23);
         {$I-}
            WRITELN(LST,'     |================||=====|=====|======|========|========|===|========|');
            WRITE  (LST,'     |     Summen:    ||');
            WRITELN(LST,AbSum.IstK:4,' |',
                        AbSum.IstS:4,' |',
                        AbSum.IstL:5,' |',AbSum.SollU:7:2,' |',Absum.IstU:7:2,' |',AbSum.OpenP:2,' |',AbSum.SchS:7:2,' |');
            WRITELN(LST,'     |===================================================================|');
            IF (AbSum.BLen > 58) THEN
               DiffBLen:=AbSum.Blen-58
            ELSE BEGIN
               FOR L1:=1 TO 59-AbSum.BLen DO
                  WRITELN(LST);
               DiffBlen:=0
            END;
            AbSum.Bar:=AbSum.IstU-AbSum.SchS+LeerDiff;
            WITH (AbSum) DO BEGIN
               Leergutstring1:=')';
               Found:=FALSE;
               FOR Lauf:=PfandTAnzahl DOWNTO 1 DO
                  IF (Found) OR (LeerZa[Lauf] <> 0) THEN BEGIN
                     Found:=TRUE;
                     STR(LeerZa[Lauf],LSS);
                     LeergutString1:='/'+LSS+LeergutString1;
                  END;

               IF (LENGTH(LeergutString1) > 1) THEN
                  LeergutString1[1]:='('
               ELSE
                  LeergutString1:='';
               IF (LENGTH(LeergutString1) > 35) THEN BEGIN
                  Found:=FALSE;
                  Pos:=30;
                  WHILE NOT(Found) DO BEGIN
                     DEC(Pos);
                     IF (LeergutString1[Pos] = '/') THEN
                        Found:=TRUE;
                  END;
                  LeergutString2:='.../'+COPY(LeergutString1,Pos+1,LENGTH(LeergutString1)-Pos);
                  LeergutString1:=COPY(LeergutString1,1,Pos)+'...'
               END;
               SZ(0,'');
               SZ(2,'');
               WRITELN(LST,'     Geldabgabe:         ',Rechnungswaehrung,(IstU+Wechselgeld):8:2,
                           '  Leergut Soll:         ',Rechnungswaehrung,SollLW:11:2);
               SZ(7,'');
               WRITELN(LST,'     + neg.Leergutdiff.: ',Rechnungswaehrung,LeerDiff:8:2,            
                           '  Leergut Ist:          ',Rechnungswaehrung,IstLW:11:2);
               SZ(6,'');
               WRITELN(LST,'     % Schecks (',SchAn:2,'):     ',Rechnungswaehrung,SchS:8:2,
                           '  ',LeergutString1);
               WRITELN(LST,'                                     ',
                           '  ',LeergutString2);
               WRITELN(LST,'     % Bar (T100-B):     ',Rechnungswaehrung,Bar:8:2,
                           '  Leergutdiff.:         ',Rechnungswaehrung,IstLW-SollLW:11:2);
               SZ(5,'');
               WRITELN(LST,'     % Wechselgeld:      ',Rechnungswaehrung,Wechselgeld:8:2,
                           '  Arbeitsmaterial:    Ja: __ Nein: __');
               WRITELN(LST,'     ------------------------------');
               WRITELN(LST,'     Offen (',AbSum.OpenP:2,'):         ',Rechnungswaehrung,SollU-IstU:8:2,
                           '  Kfz. Schluessel:    W-____-____(__)');
               WRITELN(LST,'     ==============================');
               SZ(2,'');
               WRITELN(LST,'     Bargeld und Schecks wurden Åbergeben, Leergutauflistung bestaetigt.');
               SZ(0,'');
               SZ(0,'');
               SZ(0,'');
               WRITELN(LST);
               WRITELN(LST,'         ______________________                ______________________');
               L1:=LENGTH(AbSum.Fahr);
               L2:=(40-L1) DIV 2;
               SZ(0,REPLICATE(' ',L2)+AbSum.Fahr+REPLICATE(' ',53-L2-L1)+'Abrechnung');
            END;
         {$I+}
         IF (IORESULT <> 0) THEN ;
         KBSummeEintragen;
      END;




END.
{============================
 Versionshistorie
 $Log:$
 ============================}
