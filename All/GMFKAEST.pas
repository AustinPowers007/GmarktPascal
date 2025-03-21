{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Feb 08 20:43:42 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMFKAEST;
{$O+}

INTERFACE

     USES LIEDATEN,
          ARTDATEN,
          GMDATEI,
          GMSETUP,
          KUNDATEN,
          ZUSAETZE,
          PRINTER;

     TYPE RegKPtr  = ^RegKasten;
          RegKasten = RECORD
              Menge :LONGINT;
              ArtNr :STRING[5];
              Hnr   :STRING[10];
              ArtBez:STRING[40];
              Typ   : LONGINT;
              Marge: REAL;
              Next  : RegKPtr;
          END;
          APtr = ^Andere;
          Andere = RECORD
              Menge : LONGINT;
              ArtBez :STRING[40];
              Typ    : LONGINT;
              Next   : APtr;
          END;

    PROCEDURE LSumKUp(Lieferung:TLiefDaten;VAR ErsterKasten,ErsterMKasten,
                         ErsterKLaden:RegKPtr;VAR ErsterAndere,ErsterLAndere:APtr;
                        VAR KeinFehler:BOOLEAN);
    PROCEDURE LSumKUpANr(Lieferung:TLiefDaten;VAR ErsterKasten,ErsterMKasten,
                         ErsterKLaden:RegKPtr;VAR ErsterAndere,ErsterLAndere:APtr;
                        VAR KeinFehler:BOOLEAN);
    PROCEDURE LKasDruck(ErsterKasten,ErsterMKasten,ErsterKLaden:RegKPtr;
                        ErsterAndere,ErsterLAndere:APtr;
                        ADatum,EDatum:STRING);

IMPLEMENTATION

   FUNCTION AddReg(VAR KListPtr:RegKPtr;ABez:STRING;Menge:LONGINT;Typ:LONGINT;
                   PKal,EK:REAL;ANr:STRING;Hnr:STRING):BOOLEAN;
      VAR zgr1,zgr2,NReg:RegKPtr;
      BEGIN
         AddReg := FALSE;
         IF KListPtr <> NIL THEN BEGIN
            Zgr1 := KListPtr;
            Zgr2 := KListPtr^.Next;
            IF Zgr1^.ArtBez < ABez THEN
               WHILE (Zgr1^.Next <> NIL) AND (Zgr2^.ArtBez <= ABez) DO BEGIN
                  Zgr1 := Zgr2;
                  Zgr2 := Zgr2^.Next;
               END;
            IF Zgr1^.ArtBez = ABez THEN BEGIN
               Zgr1^.Menge := Zgr1^.Menge+Menge;
               AddReg := TRUE;
            END ELSE
               IF Maxavail > SIZEOF(RegKasten) THEN BEGIN
                  NEW(NReg);
                  NReg^.Menge := Menge;
                  NReg^.ArtBez:= ABez;
                  NReg^.Typ := Typ;
                  IF (EK > 0) THEN
                     NReg^.Marge := PKal-EK
                  ELSE
                     NReg^.Marge := 0;
                  NReg^.ArtNr := ANr;
                  NReg^.HNr   := HNr;
                  IF ABez < KListPtr^.ArtBez THEN BEGIN
                     NReg^.Next  := KListPtr;
                     KListPtr := NReg;
                  END ELSE BEGIN
                     NReg^.Next  := Zgr1^.Next;
                     Zgr1^.Next  := NReg;
                  END;
                  AddReg := TRUE;
               END;
         END ELSE
            IF Maxavail > SIZEOF(RegKasten) THEN BEGIN
               NEW(KListPtr);
               KListPtr^.Menge := Menge;
               KListPtr^.ArtBez:= ABez;
               IF (EK > 0) THEN
                  KListPtr^.Marge := PKal-EK
               ELSE
                  KListPtr^.Marge := 0;
               KListPtr^.Typ := Typ;
               KListPtr^.ArtNr := ANr;
               KListPtr^.HNr   := HNr;
               KListPtr^.Next  := NIL;
               AddReg := TRUE;
            END;
      END;

   FUNCTION AddRegANr(VAR KListPtr:RegKPtr;ABez:STRING;Menge,Typ:LONGINT;
                      PKal,EK:REAL;ANr:STRING;Hnr:STRING):BOOLEAN;
      VAR zgr1,zgr2,NReg:RegKPtr;

      BEGIN
         AddRegANr := FALSE;
         IF KListPtr <> NIL THEN BEGIN
            Zgr1 := KListPtr;
            Zgr2 := KListPtr^.Next;
            IF Zgr1^.ArtNr < ANr THEN
               WHILE (Zgr1^.Next <> NIL) AND (Zgr2^.ArtNr <= ANr) DO BEGIN
                  Zgr1 := Zgr2;
                  Zgr2 := Zgr2^.Next;
               END;
            IF Zgr1^.ArtNr = ANr THEN BEGIN
               Zgr1^.Menge := Zgr1^.Menge+Menge;
               AddRegANr := TRUE;
            END ELSE
               IF Maxavail > SIZEOF(RegKasten) THEN BEGIN
                  NEW(NReg);
                  NReg^.Menge := Menge;
                  NReg^.ArtBez:= ABez;
                  NReg^.Typ := Typ;
                  IF (EK > 0) THEN
                     NReg^.Marge := PKal-EK
                  ELSE
                     NReg^.Marge := 0;
                  NReg^.ArtNr := ANr;
                  NReg^.HNr   := HNr;
                  IF ANr < KListPtr^.ArtNr THEN BEGIN
                     NReg^.Next  := KListPtr;
                     KListPtr := NReg;
                  END ELSE BEGIN
                     NReg^.Next  := Zgr1^.Next;
                     Zgr1^.Next  := NReg;
                  END;
                  AddRegANr := TRUE;
               END;
         END ELSE
            IF Maxavail > SIZEOF(RegKasten) THEN BEGIN
               NEW(KListPtr);
               KListPtr^.Menge := Menge;
               KListPtr^.ArtBez:= ABez;
               KListPtr^.ArtNr := ANr;
               KListPtr^.HNr   := HNr;
               KListPtr^.Typ := Typ;
               IF (EK > 0) THEN
                  KListPtr^.Marge := PKal-EK
               ELSE
                  KListPtr^.Marge := 0;
               KListPtr^.Next  := NIL;
               AddRegANr := TRUE;
            END;
      END;

   FUNCTION AddAnd(VAR AListPtr:APtr;ABez:STRING;Menge,Typ:LONGINT):BOOLEAN;
      VAR zgr1,zgr2,NAND :APtr;

      BEGIN
         AddAnd := FALSE;
         IF AListPtr <> NIL THEN BEGIN
            Zgr1 := AListPtr;
            Zgr2 := AListPtr^.Next;
            IF Zgr1^.ArtBez < ABez THEN
               WHILE (Zgr1^.Next <> NIL) AND (Zgr2^.ArtBez <= ABez) DO BEGIN
                  Zgr1 := Zgr2;
                  Zgr2 := Zgr2^.Next;
               END;
            IF Zgr1^.ArtBez = ABez THEN BEGIN
               Zgr1^.Menge := Zgr1^.Menge+Menge;
               AddAnd := TRUE;
            END ELSE
               IF Maxavail > SIZEOF(Andere) THEN BEGIN
                  NEW(NAnd);
                  NAnd^.Menge := Menge;
                  NAnd^.ArtBez:= ABez;
                  NAnd^.Typ := Typ;
                  IF ABez < AListPtr^.ArtBez THEN BEGIN
                     NAnd^.Next  := AListPtr;
                     AListPtr := NAnd;
                  END ELSE BEGIN
                     NAnd^.Next  := Zgr1^.Next;
                     Zgr1^.Next  := NAnd;
                  END;
                  AddAnd := TRUE;
               END;
         END ELSE
            IF Maxavail > SIZEOF(Andere) THEN BEGIN
               NEW(AListPtr);
               AListPtr^.Menge := Menge;
               AListPtr^.ArtBez:= ABez;
               AListPtr^.Next  := NIL;
               AListPtr^.Typ := Typ;
               AddAnd := TRUE;
            END;
      END;


   PROCEDURE LSumKUp;
      VAR Lauf : BYTE;
          L : LONGINT;
          C : INTEGER;
          ArtNr : STRING;
          NReg  : RegKPtr;

      BEGIN
         WITH Lieferung DO
             FOR Lauf := 1 TO 30 DO
                 IF (COPY(Positionen[Lauf].ArtBez,1,5) <> 'Pfand') THEN BEGIN
                     VAL (Positionen[Lauf].ArtNr,L,C);
                ArtNr := MakeArtNr(L);
                IF ArtDB.SeekRec(1,ArtNr) THEN BEGIN
                   IF (ArtDB.Rec.Artikel.Artbez = Positionen[Lauf].ArtBez) AND
                      (Artdb.Rec.Artikel.Hnr[1] <> '') THEN
                      IF (KNR = '00000') THEN
                         KeinFehler := KeinFehler AND
                                AddReg(ErsterKLaden,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                      ELSE
                         KeinFehler := KeinFehler AND
                                AddReg(ErsterKasten,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                   ELSE
                      IF (KNR = '00000') THEN
                         KeinFehler := KeinFehler AND
                                AddAnd(ErsterLAndere,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ)
                      ELSE
                         IF (ArtDb.Rec.Artikel.Typ = 1) THEN
                            KeinFehler := KeinFehler AND
                                AddReg(ErsterMKasten,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                         ELSE
                            KeinFehler := KeinFehler AND
                                AddAnd(ErsterAndere,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ)
                END;
             END;
       END;


   PROCEDURE LSumKUpANr;
      VAR Lauf : BYTE;
          L : LONGINT;
          C : INTEGER;
          ArtNr : STRING;
          NReg  : RegKPtr;

      BEGIN
         WITH Lieferung DO
             FOR Lauf := 1 TO 30 DO
                 IF (COPY(Positionen[Lauf].ArtBez,1,5) <> 'Pfand') THEN BEGIN
                     VAL (Positionen[Lauf].ArtNr,L,C);
                ArtNr := MakeArtNr(L);
                IF ArtDB.SeekRec(1,ArtNr) THEN BEGIN
                   IF (ArtDB.Rec.Artikel.Artbez = Positionen[Lauf].ArtBez) AND
                      (Artdb.Rec.Artikel.Hnr[1] <> '') THEN
                      IF (KNR = '00000') THEN
                         KeinFehler := KeinFehler AND
                                AddRegANr(ErsterKLaden,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                      ELSE
                         KeinFehler := KeinFehler AND
                                AddRegANr(ErsterKasten,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                   ELSE
                      IF (KNR = '00000') THEN
                         KeinFehler := KeinFehler AND
                                AddAnd(ErsterLAndere,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ)
                      ELSE
                         IF (ArtDb.Rec.Artikel.Typ = 1) THEN
                            KeinFehler := KeinFehler AND
                                AddReg(ErsterMKasten,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ,
                                       ArtDB.Rec.Artikel.PreisKa,
                                       ArtDB.Rec.Artikel.EK_Preis[2],
                                       Positionen[Lauf].ArtNr,
                                       Artdb.Rec.Artikel.Hnr[1])
                         ELSE
                            KeinFehler := KeinFehler AND
                                AddAnd(ErsterAndere,Positionen[Lauf].ArtBez,
                                       Positionen[Lauf].Menge,
                                       ArtDB.Rec.Artikel.Typ)
                END;
             END;
       END;


    PROCEDURE LKasDruck(ErsterKasten,ErsterMKasten,ErsterKLaden:RegKPtr;
                        ErsterAndere,ErsterLAndere:APtr;
                        ADatum,EDatum:STRING);

       VAR Druckerstatus : LONGINT;
           Zeilen     : WORD;
           Seitenzahl : WORD;
           WegVorh    : BOOLEAN;
           Taste : INTEGER;
           zgr1 :RegKPtr;
           zgr2 :APtr;
           GesamtSumme : REAL;
           D: BOOLEAN;
           Margin: REAL;
           Summet,
           Summer: LONGINT;
           MengS:  STRING[3];
           GSName : STRING;

       PROCEDURE PrintHead200;
          BEGIN
             {$I-}
                WRITELN(LST);WRITELN(Lst);
                WRITELN(LST,'   U 200         ',GSName,REPLICATE(' ',46-LENGTH(GSName)),ActDate,',',ActTime);
                WRITELN(LST,'   Monatslieferungen '+Edatum[4]+EDatum[5]+'/'+Edatum[7]+Edatum[8],REPLICATE(' ',20),
                            '                       Seite:',Seitenzahl:2);
                WRITELN(LST);
                WRITELN(LST,'   ÚÄÄÄÄÄÄÄÂ',REPLICATE('Ä',45),'ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                WRITELN(LST,'   ³ Menge ³ Bezeichnung',REPLICATE(' ',33),
                            '³ Art-Nr. ³ Marge  ³');
                WRITELN(LST,'   ÃÄÄÄÄÄÄÄÅ',REPLICATE('Ä',45),'ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
             {$I+}
             Zeilen := 7;
          END;

       PROCEDURE PrintHead201;
          BEGIN
             GSName := GmEnvObj^.GetEntry('GSNAME');
             {$I-}
                WRITELN(LST);WRITELN(Lst);
                WRITELN(LST,'   U 201         ',GSName,REPLICATE(' ',46-LENGTH(GSName)),ActDate,',',ActTime);
                WRITELN(LST,'   Bestellungen Markt '+Edatum[4]+EDatum[5]+'/'+Edatum[7]+Edatum[8],REPLICATE(' ',19),
                            '                       Seite:',Seitenzahl:2);
                WRITELN(LST);
                WRITELN(LST,'   ÚÄÄÄÄÄÄÄÂ',REPLICATE('Ä',45),'ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                WRITELN(LST,'   ³ Menge ³ Bezeichnung',REPLICATE(' ',33),
                            '³ Art-Nr. ³ Marge  ³');
                WRITELN(LST,'   ÃÄÄÄÄÄÄÄÅ',REPLICATE('Ä',45),'ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
             {$I+}
             Zeilen := 7;
          END;

       PROCEDURE PrintHead202;
          BEGIN
             GSName := GmEnvObj^.GetEntry('GSNAME');
             {$I-}
                WRITELN(Lst);WRITELN(Lst);
                WRITELN(LST,'   U 202         ',GSName,REPLICATE(' ',46-LENGTH(GSName)),ActDate,',',ActTime);
                WRITELN(LST,'   Weitere Monatslieferungen '+Edatum[4]+EDatum[5]+'/'+
                            Edatum[7]+Edatum[8],' K„sten',REPLICATE(' ',5),
                            '                       Seite:',Seitenzahl:2);
                WRITELN(LST);
                WRITELN(Lst,'   ÚÄÄÄÄÄÄÄÂ',REPLICATE('Ä',45),'ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                WRITELN(Lst,'   ³ Menge ³ Bezeichnung',REPLICATE(' ',33),
                            '³ Art-Nr. ³ Marge  ³');
                WRITELN(Lst,'   ÃÄÄÄÄÄÄÄÅ',REPLICATE('Ä',45),'ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
             {$I+}
             Zeilen := 7;
          END;

       PROCEDURE PrintHead203;
          BEGIN
             {$I-}
                WRITELN(Lst);WRITELN(Lst);
                WRITELN(LST,'   U 203         ',GSName,REPLICATE(' ',46-LENGTH(GSName)),ActDate,',',ActTime);
                WRITELN(LST,'   Weitere Monatslieferungen '+Edatum[4]+EDatum[5]+'/'+Edatum[7]+Edatum[8],' Sonstiges',
                            '                         Seite:',Seitenzahl:2);
                WRITELN(LST);
                WRITELN(Lst,'   ÚÄÄÄÄÄÄÄÂ',REPLICATE('Ä',45),'ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                WRITELN(Lst,'   ³ Menge ³ Bezeichnung',REPLICATE(' ',33),
                            '³ Art-Nr. ³ Marge  ³');
                WRITELN(Lst,'   ÃÄÄÄÄÄÄÄÅ',REPLICATE('Ä',45),'ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
             {$I+}
             Zeilen := 7;
          END;

       PROCEDURE PrintHead204;
          BEGIN
             {$I-}
                WRITELN(Lst);WRITELN(Lst);
                WRITELN(LST,'   U 204         ',GSName,REPLICATE(' ',46-LENGTH(GSName)),ActDate,',',ActTime);
                WRITELN(LST,'   Weitere Bestellungen Markt '+Edatum[4]+EDatum[5]+'/'+Edatum[7]+Edatum[8],REPLICATE(' ',11),
                            '                       Seite:',Seitenzahl:2);
                WRITELN(LST);
                WRITELN(Lst,'   ÚÄÄÄÄÄÄÄÂ',REPLICATE('Ä',45),'ÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄ¿');
                WRITELN(Lst,'   ³ Menge ³ Bezeichnung',REPLICATE(' ',33),
                            '³ Art-Nr. ³ Marge  ³');
                WRITELN(Lst,'   ÃÄÄÄÄÄÄÄÅ',REPLICATE('Ä',45),'ÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ´');
             {$I+}
             Zeilen := 7;
          END;

    BEGIN
        GSName := GmEnvObj^.GetEntry('GSNAME');
        Zeilen     := 1;
        Seitenzahl := 1;
        Margin := 0;
        SummeR := 0;
        Summet := 0;
        { Liste U 200 Drucken =========================================}
        PrintHead200;
        {$I-}
        zgr1 := ErsterKasten;
        WHILE zgr1 <> NIL DO BEGIN
           INC(Zeilen);
           IF (Zeilen > 56) THEN BEGIN
              WRITELN(LST,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
              WRITELN(LST);
              WRITELN(LST,'                                                                     ...');
              WRITE(LSt,CHR(12));
              Zeilen  := 1;
              INC(SeitenZahl);
              PrintHead200;
           END;
           WRITE(Lst,'   ³ ',Zgr1^.Menge:5,' ³ ');
           WRITE(lst,Zgr1^.ArtBez,REPLICATE(' ',43-LENGTH(Zgr1^.ArtBez)),' ³ ',Zgr1^.Typ:1);
           WRITELN(Lst,Zgr1^.ArtNr:5,'  ³ ',Zgr1^.Marge:6:2,' ³');
           IF (Zgr1^.Marge > 0) THEN BEGIN
              Summer:=Summer+Zgr1^.Menge;
              Margin:=Margin+Zgr1^.Menge*Zgr1^.Marge
           END ELSE
              Summet:=Summet+zgr1^.Menge;
           Zgr1:=Zgr1^.Next;
        END;
        WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
        IF (Zeilen > 52) THEN BEGIN
           WRITE(Lst,CHR(12));
           INC(SeitenZahl);
           PrintHead200;
        END;
        IF (Summer <> 0) THEN
           Margin:=Margin / Summer;
        WRITELN(Lst);
        WRITE(LST,'     Summe: ',Summer:10,'    Marge: ',Margin:10:4,'     Ware ohne EK: ',Summet:8);
        WRITE(LST,CHR(12));

        Zeilen     := 1;
        Seitenzahl := 1;
        SummeR := 0;
        Summet := 0;
        { Liste U 201 Drucken =========================================}
        PrintHead201;
        {$I-}
        zgr1 := ErsterKLaden;
        WHILE zgr1 <> NIL DO BEGIN
           INC(Zeilen);
           IF (Zeilen > 56) THEN BEGIN
              WRITELN(LST,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
              WRITELN(LST);
              WRITELN(LST,'                                                                     ...');
              WRITE(LSt,CHR(12));
              Zeilen  := 1;
              INC(SeitenZahl);
              PrintHead201;
           END;
           WRITE(Lst,'   ³ ',Zgr1^.Menge:5,' ³ ');
           WRITE(lst,Zgr1^.ArtBez,REPLICATE(' ',43-LENGTH(Zgr1^.ArtBez)),' ³ ',Zgr1^.Typ:1);
           WRITELN(Lst,Zgr1^.ArtNr:5,'  ³        ³');
           Summer:=Summer+Zgr1^.Menge;
           Zgr1:=Zgr1^.Next;
        END;
        WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
        IF (Zeilen > 52) THEN BEGIN
           WRITE(Lst,CHR(12));
           INC(SeitenZahl);
           PrintHead201;
        END;
        WRITELN(Lst);
        WRITE(LST,'     Summe: ',Summer:10);
        WRITE(LST,CHR(12));

        Zeilen     := 1;
        Seitenzahl := 1;
        SummeR := 0;
        Summet := 0;
        { Liste U 202 Drucken =========================================}
        PrintHead202;
        {$I-}
        zgr1 := ErsterMKasten;
        WHILE zgr1 <> NIL DO BEGIN
           INC(Zeilen);
           IF (Zeilen > 56) THEN BEGIN
              WRITELN(LST,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
              WRITELN(LST);
              WRITELN(LST,'                                                                     ...');
              WRITE(LSt,CHR(12));
              Zeilen  := 1;
              INC(SeitenZahl);
              PrintHead202;
           END;
           WRITE(Lst,'   ³ ',Zgr1^.Menge:5,' ³ ');
           WRITE(lst,Zgr1^.ArtBez,REPLICATE(' ',43-LENGTH(Zgr1^.ArtBez)),' ³ ',Zgr1^.Typ:1);
           WRITELN(Lst,Zgr1^.ArtNr:5,'  ³        ³');
           Summer:=Summer+Zgr1^.Menge;
           Zgr1:=Zgr1^.Next;
        END;
        WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
        IF (Zeilen > 52) THEN BEGIN
           WRITE(Lst,CHR(12));
           INC(SeitenZahl);
           PrintHead202;
        END;
        WRITELN(Lst);
        WRITE(LST,'     Summe: ',Summer:10);
        WRITE(LST,CHR(12));

        Zeilen     := 1;
        Seitenzahl := 1;
        SummeR := 0;
        Summet := 0;
        zgr2 := ErsterAndere;
        PrintHead203;
        WHILE zgr2 <> NIL DO BEGIN
           INC(Zeilen);
           IF (Zeilen > 56) THEN BEGIN
              WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
              WRITELN(LST);
              WRITELN(LST,'                                                                     ...');
              WRITE(LSt,CHR(12));
              Zeilen  := 1;
              INC(SeitenZahl);
              PrintHead203;
           END;
           WRITE(Lst,'   ³ ',Zgr2^.Menge:5,' ³ ');
           WRITE(lst,Zgr2^.ArtBez,REPLICATE(' ',43-LENGTH(Zgr2^.ArtBez)),' ³');
           WRITELN(LST,' ',Zgr2^.Typ,'       ³        ³');
           Zgr2:=Zgr2^.Next;
        END;
        WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
        WRITE(LST,CHR(12));

        Zeilen     := 1;
        Seitenzahl := 1;
        SummeR := 0;
        Summet := 0;
        zgr2 := ErsterLAndere;
        PrintHead204;
        WHILE zgr2 <> NIL DO BEGIN
           INC(Zeilen);
           IF (Zeilen > 56) THEN BEGIN
              WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
              WRITELN(LST);
              WRITELN(LST,'                                                                     ...');
              WRITE(LSt,CHR(12));
              Zeilen  := 1;
              INC(SeitenZahl);
              PrintHead204;
           END;
           WRITE(Lst,'   ³ ',Zgr2^.Menge:5,' ³ ');
           WRITE(lst,Zgr2^.ArtBez,REPLICATE(' ',43-LENGTH(Zgr2^.ArtBez)),' ³');
           WRITELN(LST,' ',Zgr2^.Typ,'       ³        ³');
           Zgr2:=Zgr2^.Next;
        END;
        WRITELN(Lst,'   ÀÄÄÄÄÄÄÄÁ',REPLICATE('Ä',45),'ÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ');
        WRITELN(LST);
        WRITELN(LST,'   Ende der Liste');
        WRITE(Lst,CHR(12));
      {$I+}
    END;

BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
