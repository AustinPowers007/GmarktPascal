{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 16:37:42 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMHANDY;

INTERFACE
   USES ZUSAETZE;

   FUNCTION PutToHandy(Datum:TDatum;TourNr:WORD):BOOLEAN;


IMPLEMENTATION

  USES GMDATEI,
       GMBASE,
       LIEDATEN,
       KUNDATEN,
       PLACEWIN,
       GMSETUP;







  FUNCTION PutToHandy;
      VAR   aktTourV,
            aktTourB: STRING;
  TYPE  HandyFile = TEXT;
  VAR   Lauf    : BYTE;
        Ueberschrift,
        KundeGefunden,
        D: BOOLEAN;
        OldDay : BOOLEAN;
        PS,
        TourNrS,
        Anred,
        Liefer,
        Strng,
        Flk,
        LeerGutZ,
        ABezT,
        Nrs,Nam:STRING;
        HFPath : STRING;
        SeekResult : BOOLEAN;
        HF,JF: HandyFile;
        LaufC:BYTE;
      BEGIN
         PutToHandy := FALSE;
         HFPath := MakeFilePath(GMEnvObj^.GetEntry('TOHANDYDIR'));
         IF ValidPath(HFPath) THEN BEGIN
            OldDay:= IsOldDay(Datum);
            {$I-}
            ASSIGN(HF,HFPath+'KUNDEN.'+W2S(MakeIFieldTnr(TourNr,OldDay),0));
            REWRITE(HF);
            {$I+}
            LaufC:=64;
            WRITE(HF,'OPLDatabaseFile'+CHR(00)+CHR(15)+CHR(17)+CHR(22)+CHR( 0)+
            CHR(15)+CHR(17)+CHR( 1)+CHR(32)+CHR( 3));
            SeekResult := TRUE;
         END ELSE
            SeekResult := FALSE;
         IF SeekResult THEN BEGIN
            aktTourB := MakeDLfdIndex(Datum,MakeTabsIndex(TourNr,1,OldDay),999);
            aktTourV := MakeDLfdIndex(Datum,MakeTAbsIndex(TourNr,-1,OldDay),0);
            IF LieferDB.BeginTransaction THEN BEGIN
               SeekResult :=LieferDB.StartIntervall(4,aktTourV,aktTourb,Lieferung);
               LieferDB.EndTransaction;
            END;
            IF SeekResult THEN BEGIN
               REPEAT
                 KundDB.BeginTransaction;
                 KundeGefunden := Kunddb.GetRec(1,Lieferung.Kundennummer,Kunde);
                 IF NOT KundeGefunden THEN
                    KundeGefunden:=KundDB.GetRec(1,MakeLfdNr(S2L(Lieferung.KNr),0),Kunde);
                 IF NOT KundeGefunden THEN BEGIN
                    InitTKundenDaten(Kunde);
                    Kunde.LfdNr := S2L(Lieferung.KNR);
                    FaultBox.Act(0,'Lieferschein '+Lieferung.LiefNr+', Kunde '+Lieferung.KNR+
                          ' ist nicht in Kundendatenbank');
                 END;
                 KundDb.EndTransaction;
                 WITH Kunde DO BEGIN
                    INC(LaufC);
                    STR(LfdNr,Nrs); Nrs:=UPCASE(CHR(LaufC))+': '+REPLICATE('0',5-LENGTH(Nrs))+Nrs;
                    Anred:=Anr[LiefAnrede]; Anred:=Anred+REPLICATE(' ',5-LENGTH(Anred));
                    IF (LiefAnrede = 3) THEN
                       Nam:=LiefName
                    ELSE
                       Nam:=LiefName+', '+LiefVorname[1]+'.'; Nam:=Nam+REPLICATE(' ',25-LENGTH(Nam));
                    Liefer:=Lieferung.LiefNr+REPLICATE(' ',5-LENGTH(Lieferung.LiefNr));
                    LeerGutZ:=CHR(128);
                    FOR Lauf:=1 TO 14 DO
                        LeerGutZ:=LeerGutZ+CHR(128);
                        Strng:=CHR(87)+CHR(16)+CHR(86)+Nrs+Anred+Nam+Liefer+
                        LeerGutZ+'   0.00'+'   0.00'+'   0.00'+'   0.00';
                        WRITE(HF,Strng);
                 END;
                 WITH Lieferung DO BEGIN
                    ASSIGN(JF,HFPath+'LS'+LiefNr+'.'+W2S(TourNr,0));
                    REWRITE(JF);
                    WRITE(JF,'OPLDatabaseFile'+CHR(00)+CHR(15)+CHR(17)+CHR(22)+CHR( 0)+
                          CHR(15)+CHR(17)+CHR( 1)+CHR(32)+CHR( 3));
                    Lauf:=1;
                    WHILE (Positionen[Lauf].ArtBez <> '') AND (Lauf < 31) DO
                       WITH Positionen[Lauf] DO BEGIN
                          STR(Menge:2,Strng);
                          ABezT:=COPY(ArtBez,1,37);
                          IF (LENGTH(ABezT) < 37) THEN
                             ABezT:=ABezT+REPLICATE(' ',37-LENGTH(ABezT));
                          Strng:=Strng+REPLICATE(' ',3-LENGTH(Strng))+ABezT;
                          STR(Preis:7:2,PS);
                          Strng:=Strng+PS;
                          STR(Pfand:7:2,PS);
                          FLK:=CHR(128);
                          Strng:=CHR(56)+CHR(16)+CHR(55)+Strng+PS+FLK;
                          WRITE(JF,Strng);
                          INC(Lauf);
                       END;
                       CLOSE(JF);
                 END;
                  LieferDB.BeginTransaction;
                  SeekResult :=LieferDB.GetIntNext(Lieferung);
                  LieferDB.EndTransaction;
               UNTIL (NOT SeekResult);
            END ELSE
               FaultBox.Act(2,'Angegebene Tour enth„lt keine Lieferungen');
           {$I-}
           CLOSE(HF);
           {$I+}
            PutToHandy := TRUE;
         END ELSE BEGIN
            FaultBox.Act(0,'Das Verzeichnis '+HFPath+' existiert nicht, '+
                            'šbertragung nicht m”glich');
         END;
      END;





END.

{============================
 Versionshistorie
 $Log:$
 ============================}
