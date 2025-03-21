{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMT100Ed;



INTERFACE
   USES GMDATEI,
        ZUSAETZE;

   PROCEDURE InitT100Input;
   PROCEDURE DoT100Edit(DLSChluessel:TDatum;laktTnr:WORD;OldDay:BOOLEAN);
   FUNCTION  T100Drucken(Datum:STRING; Tour:BYTE):BOOLEAN;

IMPLEMENTATION

   USES LIEDATEN,
        CASHDATA,
        GMT100DR,
        CASHNEU,
        PLACEWIN;
   TYPE  TT100Zeile = RECORD
                         LiefNr : STRING[5];
                         IstNormal,
                         IstSonder,
                         IstLeergut : LONGINT;
                         IstUmsatz  : REAL;
                      END;
   TYPE  TT100ARRAY = ARRAY[1..11] OF TT100Zeile;

   VAR T100Fenster   : TTabFensterPtr;
   VAR LaktTourKey   : STRING;
   VAR T100Tag,
       T100Tour      : STRING;
   VAR Lauf : BYTE;

   VAR   T100ARRAY : TT100ARRAY;


   PROCEDURE InitT100Daten;
      VAR   Lauf :BYTE;

      BEGIN
         FOR Lauf := 1 TO 11 DO
            WITH T100ARRAY[Lauf] DO BEGIN
               LiefNr := '';
               IstNormal := 0;
               IstSonder := 0;
               IstLeergut:= 0;
               IstUmsatz := 0;
            END;
      END;

   PROCEDURE WriteT100ARRAY(POS:BYTE);
      VAR   D: BOOLEAN;

      BEGIN
         D:=LieferDB.GetRec(1,T100ARRAY[POS].LiefNr,Lieferung);
         IF NOT D THEN
            FaultBox.Act(0,'Fehler beim Lesen  von T100 '+L2S(POS,3) );
         Lieferung.IstNormal := T100ARRAY[POS].IstNormal;
         Lieferung.IstSonder := T100ARRAY[POS].IstSonder;
         Lieferung.IstLeergut:= T100ARRAY[POS].IstLeergut;
         Lieferung.IstUmsatz := T100ARRAY[POS].IstUmsatz;
         Lieferung.T100Bearbeitet := TRUE;
         d:=TRUE;
         d:=LieferDB.ChangeRec(Lieferung);
         IF NOT D THEN
            FaultBox.Act(0,'Fehler beim Schreiben von T100 ' );
      END;

   PROCEDURE ReadT100ARRAY(POS:BYTE; Lieferung:TLiefDaten);
      BEGIN
         IF NOT Lieferung.T100Bearbeitet THEN
            BerechneSoll(Lieferung);
         T100ARRAY[POS].LiefNr := Lieferung.LiefNr;
         T100ARRAY[POS].IstNormal := Lieferung.IstNormal;
         T100ARRAY[POS].IstSonder := Lieferung.IstSonder;
         T100ARRAY[POS].IstLeergut := Lieferung.IstLeergut;
         T100ARRAY[POS].IstUmsatz := Lieferung.IstUmsatz;
      END;

   FUNCTION ScrollT100ARRAY(Dir:INTEGER):BOOLEAN; FAR;
      VAR   D: BOOLEAN;
      BEGIN
         ScrollT100ARRAY := FALSE;
         CASE Dir OF
            -1 : BEGIN
                    D:=LieferDB.GetRec(1,T100ARRAY[1].LiefNr,Lieferung);
                    IF (LieferDB.GetPrev(4,Lieferung)) AND
                       (LieferDB.Index[3].Key = laktTourKey)
                    THEN BEGIN
                       ScrollT100ARRAY := TRUE;
                       FOR Lauf := 10 DOWNTO 1 DO
                          T100ARRAY[Lauf+1] := T100ARRAY[Lauf];
                       ReadT100ARRAY(1,Lieferung);
                       WriteT100ARRAY(11);
                    END;
                 END;
            1 : BEGIN
                   D:=LieferDB.GetRec(1,T100ARRAY[10].LiefNr,Lieferung);
                   IF (LieferDB.GetNext(4,Lieferung)) AND
                      (LieferDB.Index[3].Key = laktTourKey)
                   THEN BEGIN
                      ScrollT100ARRAY := TRUE;
                      T100ARRAY[11] := T100ARRAY[1];
                      FOR Lauf := 1 TO 10 DO
                         T100ARRAY[Lauf] := T100ARRAY[Lauf+1];
                      ReadT100ARRAY(10,Lieferung);
                      WriteT100ARRAY(11);
                   END;
                END;
         END;
      END;

   PROCEDURE InitT100Input;
      BEGIN
         NEW(T100Fenster);
         T100Fenster^.Init(60,100,580,380,0,'Tourbegleitzettel T 100','',Aktiv,5,10,ScrollT100ARRAY);
         T100Fenster^.AddConst('0','Liefertag:',3,1);
         T100Fenster^.AddCString('0',T100Tag,14,1);
         T100Fenster^.AddConst('0','Tour:',26,1);
         T100Fenster^.AddCString('0',T100Tour,32,1);
         T100Fenster^.AddHLine('0',2,57,3);
         T100Fenster^.AddVLine('0',10,2,13);
         T100Fenster^.AddVLine('0',21,2,13);
         T100Fenster^.AddVLine('0',32,2,13);
         T100Fenster^.AddVLine('0',45,2,13);
         T100Fenster^.AddConst('0','Lief-Nr.',2,2);
         T100Fenster^.AddConst('0','Ist Normal',11,2);
         T100Fenster^.AddConst('0','Ist Sonder',22,2);
         T100Fenster^.AddConst('0','Ist-Leergut',33,2);
         T100Fenster^.AddConst('0','Ist Umsatz',46,2);
         FOR Lauf := 1 TO 10 DO BEGIN
            T100Fenster^.AddString('','1','',1,Lauf+3,T100ARRAY[Lauf].LiefNr,'',5);
            T100Fenster^.AddLongInt('','0','',11,Lauf+3,T100ARRAY[Lauf].IstNormal,'',4,0,9999);
            T100Fenster^.AddLongInt('','0','',22,Lauf+3,T100ARRAY[Lauf].IstSonder,'',4,0,9999);
            T100Fenster^.AddLongInt('','0','',33,Lauf+3,T100ARRAY[Lauf].IstLeerGut,'',4,0,9999);
            T100Fenster^.AddReal('','0','',46,Lauf+3,T100ARRAY[Lauf].IstUmsatz,'',6,2,-9999,9999);
         END;
         T100Fenster^.Disable('1');
      END;
   PROCEDURE DoT100Edit;
      VAR   EndCode : INTEGER;
            Lauf    : BYTE;
            MRows   : BYTE;
            DOPrint : BOOLEAN;
            d       : BOOLEAN;
      BEGIN
            LaktTourkey := MakeDateIndex(DLSCHLUESSEL,
                             MakeTabsIndex(laktTnr,0,OldDay));
            T100Tag := DLSCHLUESSEL;
            T100Tour := MakeIFieldTStr(laktTNr);
            InitT100Daten;
            T100Fenster^.SetMaxRows(0);
            DOPrint := FALSE;
            IF LieferDB.BeginTransaction THEN BEGIN
               IF NOT LieferDB.GetRec(4,MakeDLfdIndex(DLSchluessel,
                  MakeTabsIndex(laktTnr,0,OldDay),0),Lieferung) THEN
                  D:= LieferDB.GetNext(4,Lieferung);
               IF (Lieferdb.Index[3].Key = laktTourKey) THEN BEGIN
                  Lauf := 1;
                  REPEAT
                     ReadT100ARRAY(Lauf,Lieferung);
                     INC(Lauf);
                     d:= LieferDB.GetNext(4,Lieferung);
                  UNTIL (Lauf = 11) OR (NOT D) OR
                        (LieferDB.Index[3].Key <> LaktTourkey);
                  IF Lauf < 11 THEN
                     T100Fenster^.SetMaxRows(Lauf-1);
                  MRows := Lauf-1;
                  REPEAT
                     EndCode:=T100Fenster^.InputWithClear;
                  UNTIL (EndCode = 27) OR (EndCode = -68);
                  FOR Lauf := 1 TO MRows DO
                     WriteT100ARRAY(Lauf);
                  T100Fenster^.SetMaxRows(0);
                  IF Request.Act('Drucken ?') THEN
                     DOPrint:= TRUE;

               END
               ELSE
                  FaultBox.Act(2,'Angegebene Tour enth„lt keine Lieferungen');
               IF NOT LieferDB.EndTransaction THEN
                  FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
               IF DoPrint THEN BEGIN
                  T100Drucken(DLSchluessel,MakeTabsIndex(LaktTnr,0,OldDay));
               END;

            END
            ELSE BEGIN
               FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
               LieferDB.GetTransactionErrMsg);
            END;
         END;

   {*********************************************************************
   * T100 drucken
   *********************************************************************}


   FUNCTION T100Drucken(Datum:STRING; Tour:BYTE):BOOLEAN;
      VAR   aktTour : STRING;
            D       : BOOLEAN;
            SeekResult:BOOLEAN;
      BEGIN
         T100Drucken := TRUE;
         InitStartText;
         aktTour := MakeDateIndex(Datum,Tour);
         IF LieferDB.BeginTransaction THEN BEGIN
            d:=LieferDB.StartIntervall(4,MakeDlfdIndex(Datum,Tour,0),
                   MakeDlfdIndex(Datum,Tour,9999),Lieferung);
            IF NOT LieferDB.EndTransaction THEN
               FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
               LieferDB.GetTransactionErrMsg);
         END
         ELSE BEGIN
            FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
            LieferDB.GetTransactionErrMsg);
         END;
         IF D THEN BEGIN
            DruckeT100Kopf(TRUE,Lieferung);
            REPEAT
               StatusBox.Act('Drucke gerade Tour',Datum+' - '+
                              MakeTourName(Tour));
               IF (Lieferung.SollKaesten = 0) THEN BEGIN
                  BerechneSoll(Lieferung);
                  IF LieferDB.BeginTransaction THEN BEGIN
                     d:= LieferDB.ChangeRec(Lieferung);
                     IF NOT LieferDB.EndTransaction THEN
                        FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                        LieferDB.GetTransactionErrMsg);
                  END
                  ELSE BEGIN
                     FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                     LieferDB.GetTransactionErrMsg);
                  END;
                  IF NOT D THEN
                     FaultBox.Act(0,'Fehler bei Sollberechnung'+
                     ' in T100 Drucken');
               END;
               DruckeT100Zeile(TRUE,Lieferung);
               IF LieferDB.BeginTransaction THEN BEGIN
                  SeekResult:= LieferDB.GetIntNext(Lieferung);
                  IF NOT LieferDB.EndTransaction THEN
                     FaultBox.Act(0,'EndTransaction auf LieferDB Fehler: '+
                     LieferDB.GetTransactionErrMsg);
               END
               ELSE BEGIN
                  FaultBox.Act(0,'BeginTransaction auf LieferDB Fehler: '+
                  LieferDB.GetTransactionErrMsg);
               END;
            UNTIL (NOT SeekResult);
            DruckeT100Fuss(TRUE);
            StatusBox.Hide;
         END
         ELSE
            T100Drucken := FALSE;
      END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
