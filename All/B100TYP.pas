{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:02:22 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT B100Typ;

   INTERFACE

   USES KUNDATEN,KUNFILE,
        LIEDATEN,LIEFFILE,
        PLACEWIN,
        ASTDATA,
        CASHDATA,
        CASHNEU,
        PRINTER,
        GMCONST,
        GMCRT,
        ZUSAETZE,
        GRAPH,GRAPNEU;


      TYPE B100BP =    RECORD
                         Datum:     STRING[ 8];   { Buchungs-/Tourdatum        }
                         Tour:      STRING[ 2];   { TourkÅrzel                 }
                         Betrag:    REAL;         { Buchungsbetrag             }
                         Konto:     STRING[10];   { Gegenkonto fÅr Buchhaltung }
                         Bemerkung: STRING[30];   { Zusatzinformation          }
                       END;

           B100BRec =  RECORD
                         Datum:  STRING[8];                  { Datum der Einzahlung }
                         Auszug: STRING[5];                  { Bankauszug AAA.S     }
                         Anzahl: BYTE;                       { Anzahl der EintrÑge  }
                         Betrag: REAL;                       { Summe der Positionen }
                         Bemerk: ARRAY[1.. 5] OF STRING[60]; { Zusatzinformation    }
                         E:      ARRAY[1..50] OF B100BP;     { EintrÑge             }
                       END;

           B100BFile = FILE OF B100BRec;

           B100SRec =  RECORD
                         Datum:     STRING[ 8];
                         Tour:      STRING[ 2];
                         Betrag:    REAL;
                         Kunde:     STRING[ 5];
                         LieferS:   STRING[ 5];
                         BLZ:       STRING[ 8];
                         ScheckNr:  STRING[12];
                         Bank:      STRING[20];
                         BelegInfo: STRING[ 8];
                         Bemerkung: STRING[30];
                       END;

           B100SFile = FILE OF B100SRec;


      VAR  B100BF:       B100BFile;
           B100BR:       B100BRec;
           B100SF:       B100SFile;
           B100SR:       B100SRec;

           B100BFenster: TASFenster;

           HilfsZahl:    LONGINT;

      FUNCTION AddToB100B(Pfad,DateiName,Tag:STRING;Tour:WORD;Bar:REAL;Bemerkung:STRING):BOOLEAN;
      FUNCTION PrintB100B(Pfad,DateiName:STRING):BOOLEAN;
      FUNCTION ReadyB100B(Pfad,DateiName,Tag:STRING):BOOLEAN;

   IMPLEMENTATION

      PROCEDURE InitB100B;
         VAR L:BYTE;
         BEGIN
           WITH B100BR DO BEGIN
             Datum:='';
             Auszug:='';
             Anzahl:=0;
             Betrag:=0;
             FOR L:=1 TO  5 DO
               Bemerk[L]:='';
             FOR L:=1 TO 50 DO
               WITH B100BR.E[L] DO BEGIN
                 Datum     :='';
                 Tour      :='';
                 Betrag    := 0;
                 Konto     :='';
                 Bemerkung :=''
               END
           END;
         END;

      PROCEDURE InitB100S;
         BEGIN

         END;

      FUNCTION OpenB100B(Pfad,DateiName:STRING):BOOLEAN;
         BEGIN
           OpenB100B:=TRUE;
           ASSIGN(B100BF,Pfad+Dateiname);
           {$I-} RESET(B100BF); {$I+}
           IF (IORESULT <> 0) THEN BEGIN
             {$I-} REWRITE(B100BF); {$I+}
             IF (IORESULT <> 0) THEN
               OpenB100B:=FALSE
             ELSE BEGIN
               InitB100B;
               {$I-} WRITE(B100BF,B100BR); {$I+}
               IF (IORESULT <> 0) THEN
                 OpenB100B:=FALSE
             END
           END ELSE BEGIN
             {$I-} READ(B100BF,B100BR); {$I-}
             IF (IORESULT <> 0) THEN
               OpenB100B:=FALSE
           END
         END;

      FUNCTION OpenB100S(Pfad,DateiName:STRING):BOOLEAN;
         BEGIN
           OpenB100S:=TRUE;
           ASSIGN(B100SF,Pfad+Dateiname);
           {$I-} RESET(B100SF); {$I+}
           IF (IORESULT <> 0) THEN BEGIN
             {$I-} REWRITE(B100SF); {$I+}
             IF (IORESULT <> 0) THEN
               OpenB100S:=FALSE
             ELSE BEGIN
               InitB100S;
               {$I-} WRITE(B100SF,B100SR); {$I+}
               IF (IORESULT <> 0) THEN
                 OpenB100S:=FALSE
             END
           END ELSE BEGIN
             {$I-} READ(B100SF,B100SR); {$I-}
             IF (IORESULT <> 0) THEN
               OpenB100S:=FALSE
           END
         END;

      PROCEDURE CloseB100B;
         BEGIN
           {$I-} Close(B100BF); {$I+}
           IF (IORESULT <> 0) THEN ;
         END;

      PROCEDURE CloseB100S;
         BEGIN
           {$I-} Close(B100SF); {$I+}
           IF (IORESULT <> 0) THEN ;
         END;

      FUNCTION IsNewer(DA,TA,DN,TN:STRING):BYTE;
         VAR ADay,NDay,
             AMon,NMon,
             AYer,NYer: LONGINT;
             C:         INTEGER;
             Diff,
             AWert,
             NWert:     LONGINT;

         FUNCTION ReDo(Tour:STRING):BYTE;
            BEGIN
              ReDo:=0;
              CASE Tour[1] OF
                'V': ReDo:=   ORD(Tour[2])-48;
                'N': ReDo:= 5+ORD(Tour[2])-48;
                'A': ReDo:=10+ORD(Tour[2])-48
              END;
            END;

         BEGIN
           VAL(COPY(DA,1,2),ADay,C);
           VAL(COPY(DA,4,2),AMon,C);
           VAL(COPY(DA,7,2),AYer,C);
           VAL(COPY(DN,1,2),NDay,C);
           VAL(COPY(DN,4,2),NMon,C);
           VAL(COPY(DN,7,2),NYer,C);
           AWert:=(AYer-90)*50000+AMon*4000+ADay*100+ReDo(TA);
           NWert:=(NYer-90)*50000+NMon*4000+NDay*100+ReDo(TN);
           Diff:=AWert-NWert;
           IF Diff > 0 THEN IsNewer:=1;
           IF Diff = 0 THEN IsNewer:=0;
           IF Diff < 0 THEN IsNewer:=2
         END;

      FUNCTION SpeichernB100B(P:BYTE;BNew:B100BP):BOOLEAN;
         BEGIN
           SpeichernB100B:=TRUE;
           WITH B100BR.E[P] DO BEGIN
             Datum     := BNew.Datum;
             Tour      := BNew.Tour;
             Betrag    := BNew.Betrag;
             Bemerkung := BNew.Bemerkung
           END;
           {$I-} RESET(B100BF); {$I+}
           IF (IORESULT = 0) THEN BEGIN
             {$I-} WRITE(B100BF,B100BR); {$I+}
             IF (IORESULT <> 0) THEN
               SpeichernB100B:=FALSE
           END ELSE
             SpeichernB100B:=FALSE
         END;

      FUNCTION AddToB100B;
         VAR BNew:  B100BP;
             L,
             Lauf:  BYTE;
             Ende,
             Found: BOOLEAN;
         BEGIN
           IF NOT(OpenB100B(Pfad,DateiName)) THEN BEGIN
             AddToB100B:=FALSE;
             EXIT
           END ELSE
             AddToB100B:=TRUE;

           { Informationen zuweisen }

             BNew.Datum     := Tag;
             BNew.Tour      := MakeShortTourName(Tour);
             BNew.Betrag    := Bar;
             BNew.Bemerkung := Bemerkung;

           WITH B100BR DO BEGIN

             { Position suchen }

               Lauf  := 0;
               Found := FALSE;
               Ende  := FALSE;
               WHILE NOT(Ende) AND NOT(Found) AND (Lauf < Anzahl) DO BEGIN
                 INC(Lauf);
                 CASE IsNewer(E[Lauf].Datum,E[lauf].Tour,BNew.Datum,BNew.Tour) OF
                   1: Ende:=TRUE;
                   0: Found:=TRUE;
                 ELSE BEGIN END
                 END;
               END;
               IF NOT(Ende) AND NOT(Found) THEN INC(Lauf);

             { Tour bereits gespeichert }

               IF (Found) THEN
                 IF Request.Act('Tour bereits OK! Trotzdem') THEN
                   AddToB100B:=SpeichernB100B(Lauf,BNew);

             { Tour einfÅgen }

               IF (Ende) THEN
                 FOR L:=Anzahl DOWNTO Lauf DO
                   E[L+1]:=E[L];
               IF NOT(Found) THEN BEGIN
                 INC(Anzahl);
                 AddToB100B:=SpeichernB100B(Lauf,BNew);
               END;
           END;
           CloseB100B
         END;

      FUNCTION PrintB100B;
         VAR Lauf:BYTE;
             LaufS:STRING[2];
         BEGIN
           IF NOT(OpenB100B(Pfad,DateiName)) THEN BEGIN
             PrintB100B:=FALSE;
             EXIT
           END ELSE
             PrintB100B:=TRUE;

           { B100 drucken }

           WRITELN(LST);
           WRITELN(LST,'     B100-B      Bankeinzahlungsbeleg (BAR)            Datum: ',B100BR.Datum:8);
           WRITELN(LST,'     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ');
           WRITELN(LST);
           WRITELN(LST,'     ⁄ƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø');
           WRITELN(LST,'     ≥ Lfd. ≥   Datum   ≥ Kurz ≥  Betrag  ≥ Bemerkung                ≥');
           WRITELN(LST,'     √ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥');
           B100Br.Betrag:=0;
           WITH B100BR DO
             FOR Lauf:=1 TO Anzahl DO
               WITH B100BR.E[Lauf] DO BEGIN
                 STR(Lauf:1,LaufS);
                 WRITELN(LST,'     ≥',LaufS:5,' ≥',Datum:10,' ≥',Tour:4,'  ≥',Betrag:9:2,' ≥ ',COPY(Bemerkung,1,24):24,' ≥');
                 B100Br.Betrag:=B100Br.Betrag+Betrag;
               END;
           WRITELN(LST,'     ¿ƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ');
           WRITELN(LST);
           WRITELN(LST,'     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒø');
           WRITELN(LST,'     ≥ B100-E Einzahlungssumme ≥',B100Br.Betrag:9:2,' ≥');
           WRITELN(LST,'     ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒŸ');
           WRITELN(LST);
           FOR Lauf:=22+B100BR.Anzahl TO 60 DO WRITELN(LST);
           WRITELN(LST,'     Legende:        V1..Vn: Vormittagstouren *)');
           WRITELN(LST,'                     N1..Nn: Nachmittagstouren *)');
           WRITELN(LST,'                     A1..An: Abendtouren *)');
           WRITELN(LST,'                     L:      Ladenumsatz **)');
           WRITELN(LST,'                     U1..Un: Umbuchungen **)');
           WRITELN(LST,'                     R1..Rn: Rechnungen ***)');
           WRITELN(LST,'                     P:      Privatentnahme **)');
           WRITELN(LST);
           WRITELN(LST,'                     *)   Summen T100-B lt. Anlage');
           WRITELN(LST,'                     **)  lt. Anlage');
           WRITELN(LST,'                     ***) bar bezahlt lt. Anlage');
           WRITE(LST,CHR(12));
           CloseB100B
         END;

      FUNCTION ReadyB100B;
         BEGIN

         END;

   VAR LaufA,
       LaufX,
       LaufY: BYTE;

   BEGIN
      HilfsZahl:=-1;
      B100BFenster.Init(10,10,640,360,0,'B100-B Bearbeitung','ASIN3',Aktiv);
      WITH B100BFenster DO BEGIN
        FOR LaufA:=0 TO 3 DO BEGIN
          AddConst('1', 5+LaufA*15,1,'Datum');
          AddConst('1', 12+LaufA*15,1,'Tour')
        END;
        FOR LaufA:=0 TO 49 DO BEGIN
          LaufX:=LaufA DIV 13;
          LaufY:=LaufA MOD 13;
          AddDate  ('','1','', 2+LaufX*15,3+LaufY,B100BR.E[LaufA+1].Datum,'');
          AddString('','1','',11+LaufX*15,3+LaufY,B100BR.E[LaufA+1].Tour, '',2);
        END;
      END;
   END.
{============================
 Versionshistorie
 $Log:$
 ============================}
