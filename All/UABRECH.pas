{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Oct 22 17:53:16 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT UAbrech;

   {$G+}
   {$N+}

INTERFACE

    PROCEDURE Abrechnen;

IMPLEMENTATION

      USES GMSETUP,
           LIEDATEN,
           GMDATEI,
           PRINTER,
           GMT100DR,
           GMCONST,
           COPYUNIT,
           ZUSAETZE;

      TYPE TT200Zeile = RECORD
                          Fahr,
                          Tour  : STRING;
                          SchAn,
                          OpenP,
                          BLen,
                          SollK : LONGINT;
                          SollU : REAL;
                          IstK,
                          IstS,
                          IstL  : LONGINT;
                          Bar,
                          SchS,
                          IstLW,
                          SollLW,
                          IstU  : REAL;
                        END;

           LeergutS =   RECORD
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

   VAR  StylesLauf  : TPrintStyles;

        DLSchluessel: TDatum;
        Printdest,
        LAktTNr:      BYTE;
        EndCode:      INTEGER;
        HlpFile:      TEXT;
        DruckerTyp : STRING[2];
        TourText : STRING[12];

        AbSum:        TT200Zeile;
        LS:           LiefSatz;
        LDay:         STRING[8];
        LTur:         WORD;

  FUNCTION T100Drucken(Datum:STRING;Tour:WORD):BOOLEAN;
     VAR aktTour : STRING;
         aktKeyVon,
         AktKeyBis : STRING;
         D       : BOOLEAN;
         ATabs,
         ATour :BYTE;

         LeerIst,
         LeerSoll: REAL;
     BEGIN
        LeerIst:=0;
        LeerSoll:=0;
        T100Drucken := TRUE;
        aktKeyVon := MakeDlfdIndex(Datum,Tour,0);
        aktKeyBis := MakeDlfdIndex(Datum,Tour,9999);
        SplitTnr(Tour,Atabs,ATour);
        TourText:=COPY(Datum,1,2)+COPY(Datum,4,2)+COPY(Datum,7,2)+'.'+
                  COPY(MakeShortTourName(Tour),1,1)+L2S(ATour,0);
        IF LieferDB.StartIntervall(4,AktKeyVon,AktKeyBis,Lieferung) THEN BEGIN
           DruckeT100Kopf(TRUE,Lieferung);
           REPEAT
              IF (Lieferung.SollKaesten = 0) THEN BEGIN
                 BerechneSoll(Lieferung);
                 d:= LieferDB.ChangeRec(Lieferung);
              END;
              DruckeT100Zeile(TRUE,Lieferung);
           UNTIL NOT (LieferDB.GetIntNext(Lieferung));
           DruckeT100Fuss(TRUE);
        END ELSE
           T100Drucken := FALSE;
    END;

   PROCEDURE ToLiefSatz(Txt:STRING);
      VAR Lauf: BYTE;
          C:    INTEGER;

      FUNCTION ToVal(Txt:STRING;Fst,L:LONGINT):REAL;
         VAR Part:  STRING;
             Value: REAL;
             C:     INTEGER;
         BEGIN
           Part:=COPY(Txt,Fst,L);
           IF (POS(',',Part) <> 0) THEN BEGIN
             Part[POS(',',Part)]:='.';
             VAL(Part,Value,C);
           END ELSE BEGIN
             VAL(Part,Value,C);
             Value:=Value/100
           END;
           ToVal:=Value
         END;

      BEGIN
        LS.KNr:=COPY(Txt, 4,5);
        LS.LNr:=COPY(Txt,39,5);
        LS.Don:=COPY(Txt,38,1)='X';
        FOR Lauf:=1 TO 15 DO
          LS.LeerG[Lauf]:=ORD(Txt[43+Lauf])-128;
        LS.Sonst:=ToVal(Txt,59,7);
        LS.Summe:=ToVal(Txt,66,7);
        LS.Zahlt:=ToVal(Txt,73,7);
        LS.Sheck:=ToVal(Txt,80,7);
      END;


   PROCEDURE ToNotSatz(Txt:STRING);
      VAR Lauf:BYTE;
      BEGIN
        LS.KNr:=COPY(Txt, 4,5);
        LS.LNr:=COPY(Txt,39,5);
        LS.Don:=FALSE;
        FOR Lauf:=1 TO 15 DO
          LS.LeerG[Lauf]:=0;
        LS.Sonst:=0;
        LS.Summe:=0;
        LS.Zahlt:=0;
        LS.Sheck:=0;
      END;

   PROCEDURE ToLieferDB;

      PROCEDURE FillLeergut;
         VAR AnzS:   STRING[10];
             Lauf:   BYTE;
             Bemerk: STRING[240];
         BEGIN
           WITH Lieferung DO BEGIN
             IstSonder  :=0;
             IstNormal  :=0;
             IstLeerGut :=0;
             IF (SollKaesten > 9) THEN IstSonder:=SollKaesten
                                  ELSE IstNormal:=SollKaesten;
             IF (LS.Don) THEN BEGIN
               Bemerk:='JNNTAI=';
             END ELSE
               Bemerk:='NNNTAI=';

             FOR Lauf:=1 TO PfandTAnzahl DO
               IF (LS.LeerG[Lauf] <> 0) THEN BEGIN
                 STR(LS.LeerG[Lauf]:1,AnzS);
                 IF (Lauf <= PfandGKasten) THEN
                    INC(IstLeerGut,LS.LeerG[Lauf]);
                 Bemerk:=Bemerk+W2S(PfandTypen[Lauf],0)+':'+AnzS+';';
               END;

             IF (LS.Sonst <> 0) THEN BEGIN
               STR((LS.Sonst*100):1:0,AnzS);
               Bemerk:=Bemerk+'991:'+AnzS+';'
             END;
             IF (LS.Sheck <> 0) THEN BEGIN
               STR((LS.Sheck*100):1:0,AnzS);
               Bemerk:=Bemerk+'994:'+AnzS+';'
             END;
             SollUmsatz:=LS.Summe;
             IstUmsatz :=LS.Zahlt;
             T100Bearbeitet:=TRUE;
             IF NOT(LS.Don) THEN BEGIN
               IstNormal:=0;
               IstSonder:=0;
               IstLeergut:=0
             END;
             Bemerkungen[1]:=COPY(Bemerk,  1,80);
             Bemerkungen[2]:=COPY(Bemerk, 81,80);
           END;
         END;

      VAR OK:BOOLEAN;
          T1,T2:STRING[2];

      BEGIN
        IF (LieferDB.GetRec(1,LS.LNr,Lieferung)) THEN BEGIN
          IF (Lieferung.Knr = LS.KNr) THEN BEGIN
             OK:=TRUE;
             IF (LDay = '') THEN
                 LDay:=Lieferung.LieferTag
             ELSE
                IF (LDay <> Lieferung.Liefertag) THEN
                   OK:=FALSE;
             IF (LTur =  0) THEN
                 LTur:=Lieferung.TourNr
             ELSE
                IF (LTur <> Lieferung.TourNr) THEN
                   OK:=FALSE;
             IF (OK) THEN BEGIN
                FillLeergut;
                LieferDB.ChangeRec(Lieferung)
             END ELSE BEGIN
                WITH Lieferung DO BEGIN
                   STR(LTur:1,T1);
                   STR(TourNr:1,T2);
                   AddStartText('     Kunde Nr.: '+KNr+'('+LiefNr+') umgelegt von '
                                        +LDay+' ('+T1+') nach '+
                                        Liefertag+' ('+T2+')');
                END;
             END;
          END ELSE BEGIN
             AddStartText('Kundennummer ungleich bei '+Lieferung.KNr+'  '+LS.KNr);
          END
        END ELSE BEGIN
           AddStartText(LS.KNr+'  nicht gefunden!');
        END;
      END;

   PROCEDURE T100Edit;
      VAR Lauf:     BYTE;
          D,
          Print,
          DoChange:  BOOLEAN;
          LiefN:     STRING[5];
          T:         TEXT;
          Sign:      CHAR;
          Txt:       STRING;
          FromHandy : STRING;
          Sym1,Sym2: STRING[1];
      BEGIN
        FromHandy := MakeFilePath(GMEnvObj.GetEntry('FRHANDYDIR'));
        InitStartText;
        ASSIGN(T,FromHandy+'KUNDEN');
        {$I-} RESET(T);
              FOR Lauf:=1 TO 25 DO READ(T,Sign);
        {$I+}
        LDay:='';
        LTur:= 0;
        WHILE NOT(EOF(T)) AND (IORESULT = 0) DO BEGIN
          Txt:='';
          {$I-} FOR Lauf:=1 TO  3 DO READ(T,Sign);
                FOR Lauf:=1 TO 86 DO BEGIN
                  READ(T,Sign);
                  Txt:=Txt+Sign;
                END;
          {$I+}
          Sym1:=COPY(Txt, 1,1);
          Sym2:=COPY(Txt,38,1);

          { Zuordnung der Lieferdaten }

            IF (Sym2 = 'X') AND (Sym1 <> ' ') AND (Sym1 <> '>') THEN BEGIN
              ToLiefSatz(Txt);
              ToLieferDB
            END;

          { Zuordnung der Leergutrckgabe }

            IF (Sym1 = ' ') OR (Sym1 = '>') THEN
              FOR Lauf:=1 TO PfandTAnzahl DO
                SetLeerZa(Lauf,ORD(Txt[43+Lauf])-128);

          { Zuordnung nicht belieferter Kunden }

            IF (Sym2 = ' ') AND (Sym1 <> ' ') AND (Sym1 <> '>') THEN BEGIN
              ToNotSatz(Txt);
              ToLieferDB
            END;
        END;
        {$I-} CLOSE(T); {$I+}
        IF (IORESULT <> 0) THEN ;

        T100Drucken(LDay,LTur);
        T100Drucken(LDay,LTur);
        TourText:='K'+COPY(LDay,1,2)+COPY(LDay,4,2)+COPY(LDay,7,2)+'.'+
                  LeadingZeros(LTur,3);

        IF (CopyDatei(FromHandy+'KUNDEN',FromHandy+TourText) = 0 ) THEN
            WRITE(CHR(7));
      END;

   { Druckerinformationen lesen }

     PROCEDURE PrinterInit;
        VAR Lauf: BYTE;
        BEGIN
          DruckerTyp := GMEnvObj.GetEntry('PRINTER');
          CASE S2L(DruckerTyp) OF
            1: PrinterCodes := EpsonCodes;
            2: PrinterCodes := LaserJetCodes;
          ELSE PrinterCodes := AsciiCodes;
          END;

          Lauf := 0;
          FOR StylesLauf := Bold TO ShiftOut DO BEGIN
            PrintStyles[StylesLauf] :=PrinterCodes.CodeArray^[Lauf];
            INC(Lauf);
          END;
        END;

     PROCEDURE Abrechnen;
        BEGIN
          GMEnvObj.GetSetup('GMARKT');
          PrinterInit;
          LieferDB.OpenFile('LieferDB',GMEnvObj.GetEntry('LIEFDIR'),TRUE,ActDate);
          IF (LieferDB.IsOpen) THEN BEGIN
            LAktTNr   := 1;
            PrintDest := 1;
            InitTLiefDaten(Lieferung);
            T100Edit;
          END;
          LieferDB.CloseFile
        END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
