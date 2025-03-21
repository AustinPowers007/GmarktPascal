{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT KUNFILE;

INTERFACE

USES KUNDATEN,
     DOS;


CONST KIndexNo = 4;

{ Typenfestlegung fÅr SchlÅssel und Inhalt }

TYPE TKValType = LONGINT;
TYPE TKKeyType = STRING[25];

{ Knotenaufbau }

TYPE KAVL_Node = RECORD
        Left,
        Father,
        Right,
        Prev,
        Next,
        NodeNo : LONGINT;
        Weight : INTEGER;
        Key    : TKKeyType;
        Val    : TKValType;
     END;

TYPE TKIndex = ARRAY [1..KIndexNo] OF KAVL_Node;

TYPE FIndex = FILE OF TKIndex;
TYPE FKundenStamm = FILE OF TKundenDaten;
TYPE FKundenRecs = FILE OF LONGINT;

TYPE TKundenFile = OBJECT
        FDatei    : FKundenStamm;
        FRecFrei  : FKundenRecs;
        FIdx      : FIndex;
        Geoeffnet : BOOLEAN;
        Rec       : TKundenDaten;
        Index     : TKIndex;
        IndexNo   : BYTE;
        DBVerzeichnis      : STRING;
        FileRemainsOpen    : BOOLEAN;
        OnlyTransAction    : BOOLEAN;
        IsTransaction      : BOOLEAN;
        TransactionErrCode : INTEGER;
        ScanIdx   : BYTE;
        ScanStartKey: STRING;
        ScanEndKey : STRING;
        ScanForward : BOOLEAN;

        CONSTRUCTOR OpenFile(DateiName:STRING;Verzeichnis:STRING);
        PROCEDURE ReOpenFile;
        PROCEDURE SetTransactionOnly;
        PROCEDURE SetFreeAccess;
        FUNCTION BeginTransaction:BOOLEAN;
        FUNCTION EndTransAction:BOOLEAN;
        FUNCTION GetTransactionError:INTEGER;
        FUNCTION GetTransactionErrMsg:STRING;
        FUNCTION ReadRec(Pos:LONGINT):BOOLEAN;
        FUNCTION WriteRec:BOOLEAN;
        FUNCTION GetTop(Kennung:BYTE;VAR Daten:TKundendaten):BOOLEAN;
        FUNCTION GetNext(Kennung:BYTE;VAR Daten:TKundendaten):BOOLEAN;
        FUNCTION GetPrev(Kennung:BYTE;VAR Daten:TKundendaten):BOOLEAN;
        FUNCTION NewRec(Daten:TKundenDaten):BOOLEAN;
        FUNCTION GetRec(Kennung:BYTE;Schluessel:STRING;
                            VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION ChangeRec(Daten:TKundenDaten):BOOLEAN;
        FUNCTION SeekRec(Kennung:BYTE;Schluessel:STRING):BOOLEAN;
        FUNCTION DelRec(Schluessel:STRING):BOOLEAN;
        FUNCTION StartIntervall(Kennung:BYTE; Anfang,Ende:STRING;VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION GetIntTop(VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION GetIntBottom(VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION GetIntNext(VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION GetIntPrev(VAR Daten:TKundenDaten):BOOLEAN;
        FUNCTION CloseFile:BOOLEAN;
        FUNCTION GetLfdNr:LONGINT;
        PROCEDURE SetLfdNr(Nr :LONGINT);

        PRIVATE

        FUNCTION TestIntervall:BOOLEAN;
        PROCEDURE CreateNode(KeyVal:TKKeyType;Value:TKValType;
                                actNodeNo:LONGINT;VAR NewNode:KAVL_Node);
        PROCEDURE Insert(NewNode:KAVL_Node);
        PROCEDURE GetNode(NodeNo:LONGINT;VAR Node:KAVL_Node);
        FUNCTION GetRoot(VAR Node:KAVL_Node):BOOLEAN;
        PROCEDURE ChangeNode(AktNode:KAVL_Node);
        FUNCTION Search(KeyVal:TKKeyType;VAR Node:KAVL_Node):BOOLEAN;
        FUNCTION Delete(DelNode:KAVL_Node):BOOLEAN;
        PROCEDURE LeftRotate(NodeNo:LONGINT;NodeWeight,SonWeight:INTEGER);
        PROCEDURE RightRotate(NodeNo:LONGINT;NodeWeight,SonWeight:INTEGER);
     END;

IMPLEMENTATION
   USES ZUSAETZE;
    CONST DefaultKUNDDBName = 'KUNDDB'; {Wird verwendet wenn kein
                                        Name Åbergeben wird}

   CONST TransactionErrMessages : ARRAY[1..4] OF STRING =
     ('active transaction','no transaction',
      'file open','file not closed');


   CONSTRUCTOR TKundenFile.OpenFile;

      VAR FileInfo     : SearchRec;
          Frei         : STRING;
          IdxName      : STRING;
          Lauf         : BYTE;
          Dateizugriff : BOOLEAN;
          AnzRecs      : REAL;

      BEGIN
         DBVerzeichnis := MakeFilePath(Verzeichnis);
         OnlyTransAction := FALSE;
         IsTransAction := FALSE;
         TransActionErrCode := 0;
         Geoeffnet := FALSE;
         IndexNo := 1;
         ScanIdx := 0;
         Dateizugriff := (IORESULT = 0);
         Dateiname := CUTSuffix(Dateiname);
         IF (Dateiname ='') THEN
            DateiName := DefaultKunddbName;
         Dateiname := DBVerzeichnis+Dateiname;
         Frei :=Dateiname+'.FRN';
         IdxName := DateiName+'.IDX';
         DateiName:=Dateiname+'.AVL';

         ASSIGN(FDatei,Dateiname);
         ASSIGN(FRecFrei,Frei);
         ASSIGN(FIdx,IdxName);
         IF NOT SeekFile(DAteiname) THEN BEGIN
            {$I-}
               REWRITE(FDatei);
               REWRITE(FRecFrei);
               REWRITE(FIDx);
            {$I+}
            IF (IORESULT = 0) THEN BEGIN
               InitTKundenDaten(Rec);
               Rec.LfdNr := 0;
               FOR Lauf := 1 TO KIndexNo DO BEGIN
                  Index[Lauf].Key    := '';
                  Index[Lauf].Prev   := -1;
                  Index[Lauf].Next   := -1;
                  Index[Lauf].Weight := 0;
                  Index[Lauf].NodeNo := 0;
                  Index[Lauf].Left   := -1;
                  Index[Lauf].Right  := -1;
                  Index[Lauf].Father := -1;
                  Index[Lauf].Val    := 0;
               END;
               Geoeffnet := WriteRec;
            END;
         END ELSE BEGIN
            IF (CorrectSize(DateiName,SizeOf(TKundenDaten))) AND
               (CorrectSize(IdxName,SizeOf(TKIndex))) AND
               (NumberOfRecs(DateiName,SizeOf(TKundenDaten)) =
                NumberOfRecs(IdxName,SizeOf(TKIndex))) THEN BEGIN
               {$I-}
                  RESET(FDatei);
                  RESET(FIdx);
                  FINDFirst(Frei,AnyFile,FileInfo);
                  IF (DOSERROR = 0) THEN
                     RESET(FRecFrei)
                  ELSE
                     REWRITE(FRecFrei);
                  Dateizugriff := ReadRec(0);
               {$I+}
               Geoeffnet := (IORESULT = 0 );
            END ELSE
               Geoeffnet := FALSE;
         END;
      END;

   PROCEDURE TKundenFile.ReOpenFile;
      VAR  DateiZugriff : BOOLEAN;
      BEGIN
         IF (NOT Geoeffnet) THEN BEGIN
            {$I-}
               RESET(FDatei);
               RESET(FIdx);
               RESET(FRecFrei);
               Dateizugriff := ReadRec(Index[indexNo].NodeNo);
               {$I+}
            Geoeffnet := (IORESULT = 0 );
         END;
      END;

   PROCEDURE TKundenFile.SetTransActionOnly;
      BEGIN
         OnlyTransAction := TRUE;
         IF Geoeffnet THEN
            Geoeffnet := NOT CloseFile;
      END;

   PROCEDURE TKundenFile.SetFreeAccess;
      BEGIN
         OnlyTransAction := FALSE;
         IF NOT Geoeffnet THEN
            ReOpenFile;
      END;




   FUNCTION TKundenFile.BeginTransAction;
      BEGIN
         BeginTransAction:= FALSE;
         IF IsTransAction THEN
            TransActionErrCode := 1
         ELSE BEGIN
            IF (Geoeffnet AND OnlyTransaction) THEN
               TransActionErrCode :=3
            ELSE BEGIN
               FileRemainsOpen := Geoeffnet;
               IF NOT Geoeffnet THEN
                  ReOpenFile;
               IsTransAction := TRUE;
               TransActionErrCode := 0;
               BeginTransAction := TRUE;
            END;
         END;
      END;

   FUNCTION TKundenFile.EndTransAction;
      BEGIN
         EndTransAction:= FALSE;
         IF NOT IsTransAction THEN
            TransActionErrCode := 2
         ELSE BEGIN
            IsTransAction := FALSE;
            TransActionErrCode := 0;
            IF NOT FileRemainsOpen THEN BEGIN
               IF NOT CloseFile THEN
                  TransactionErrCode := 4;
            END;
            EndTransAction := (TransActionErrCode = 0);
         END;
      END;

   FUNCTION TKundenFile.GetTransActionError;
      BEGIN
         GetTransActionError := TransactionErrcode;
      END;

   FUNCTION TKundenFile.GetTransActionErrMsg;
      BEGIN
         IF TransactionErrCode <> 0 THEN
            GetTransActionErrMsg:=TransactionErrMessages[TransactionErrCode]
         ELSE
            GetTransActionErrMsg:= '';
      END;




   FUNCTION TKundenFile.GetLfdNr;
      VAR Dateizugriff : BOOLEAN;

      BEGIN
         Dateizugriff := ReadRec(0);
         GetLfdNr := Rec.LfdNr;
      END;

   PROCEDURE TKundenFile.SetLfdNr;
      VAR Dateizugriff : BOOLEAN;

      BEGIN
         Dateizugriff := ReadRec(0);
         Rec.LfdNr := Nr;
         Dateizugriff := WriteRec;
      END;



   FUNCTION TKundenFile.ReadRec;

      BEGIN
         ReadRec := (IORESULT = 0);
         {$I-}
            SEEK(FIdx,Pos);
            READ(FIdx,Index);
            SEEK(FIdx,Pos);
            SEEK(FDatei,Pos);
            READ(FDatei,Rec);
            SEEK(FDatei,Pos);
         {$I+}
         ReadRec := (IORESULT = 0);
      END;

   FUNCTION TKundenFile.WriteRec;

      BEGIN
         WriteRec := (IORESULT = 0);
         {$I-}
            SEEK(FDatei,Index[IndexNo].NodeNo);
            WRITE(FDatei,Rec);
            SEEK(FDatei,Index[IndexNo].NodeNo);
            SEEK(FIdx,Index[IndexNo].NodeNo);
            WRITE(FIdx,Index);
            SEEK(FIdx,Index[IndexNo].NodeNo);
         {$I+}
         WriteRec:= (IORESULT = 0);
      END;

   FUNCTION TKundenFile.NewRec;
      VAR NRec         : TKundenDaten;
          NewIdx       : TKIndex;
          Neu          : LONGINT;
          Schliessen,
          Dateizugriff : BOOLEAN;
          Lauf         : BYTE;

      BEGIN
         NewRec := FALSE;
         IF Geoeffnet THEN BEGIN
            IF (NOT SeekRec(1,MakeLFdNr(Daten.LfdNr,Daten.LieferANr))) THEN BEGIN
               Schliessen := TRUE;
               IF FILESIZE(FRecFrei) > 0 THEN BEGIN
                  Dateizugriff := (IORESULT = 0);
                  {$I-};
                     SEEK(FRecFrei,FILESIZE(FRecFrei)-1);
                     READ(FRecFrei,Neu);
                     SEEK(FRecFrei,FILESIZE(FRecFrei)-1);
                     TRUNCATE(FRecFrei);
                  {$I+}
                  IF (IORESULT <> 0) THEN
                     Neu := FILESIZE(FDatei)
                  ELSE
                     Schliessen := FALSE;
               END ELSE
                  Neu := FILESIZE(FDatei);
               Rec :=  Daten;
               CreateNode(MakeLfdNr(Daten.LfdNr,Daten.LieferANr),Neu,Neu,NewIdx[1]);
               CreateNode(UPPER(Daten.LiefName),Neu,Neu,NewIdx[2]);
               CreateNode(MakeFPIndex(Daten.RechName,Daten.Firma),Neu,Neu,NewIdx[3]);
               CreateNode(UPPER(Daten.LiefStrasse),Neu,Neu,NewIdx[4]);
               Index := NewIdx;
               Dateizugriff := WriteRec;
               FOR Lauf := 1 TO KIndexNo DO BEGIN
                 IndexNo := Lauf;
                 Insert(NewIdx[Lauf]);
               END;
               GetNode(Neu,NewIdx[KIndexNo]);
               IndexNo :=1;
               Dateizugriff := WriteRec;
               IF Schliessen THEN BEGIN
                  {$I-}
                     CLOSE(FDatei);
                     RESET(FDatei);
                     CLOSE(FIdx);
                     RESET(FIdx);
                  {$I+}
               END;
               NewRec := Dateizugriff AND ReadRec(Neu);
            END;
         END;
      END;


  FUNCTION TKundenFile.GetTop;
    VAR Dateizugriff : BOOLEAN;

    BEGIN
      Dateizugriff:=ReadRec(0);
      IF Dateizugriff AND (Index[Kennung].Next <> -1 )THEN BEGIN
        GetTop := ReadRec(Index[Kennung].Next);
        Daten := Rec;
      END ELSE
        GetTop := FALSE;
      END;

  FUNCTION TKundenFile.GetNext;

    BEGIN
      IF (Index[Kennung].Next <> -1) THEN BEGIN
         GetNext:=ReadRec(Index[Kennung].Next);
         Daten := Rec;
      END ELSE
         GetNext := FALSE;
    END;

  FUNCTION TKundenFile.GetPrev;

    BEGIN
      IF (Index[Kennung].Prev <>0) THEN BEGIN
        GetPrev:=ReadRec(Index[Kennung].Prev);
        Daten := Rec;
      END ELSE
        GetPrev := FALSE;
    END;

   FUNCTION TKundenFile.GetRec;

      BEGIN
         GetRec := FALSE;
         IF SeekRec(Kennung,Schluessel) THEN
            GetRec := TRUE;
       	Daten:=Rec;
      END;

   FUNCTION TKundenFile.ChangeRec;
      VAR Lauf :BYTE;
          NewIdx,
          SaveIdx : Tkindex;
          HIndexString : STRING;

      BEGIN
         ChangeRec:=FALSE;
         IF SeekRec(1,MakeLfdNr(Daten.LfdNr,Daten.LieferANr)) THEN BEGIN
            IF (Daten.LfdNr <> Rec.LfdNr) OR (DAten.LieferANr <> Rec.LieferANr) OR
               (Daten.RechName <> Rec.RechName) OR (Daten.Firma <> Rec.Firma) OR
               (Daten.LiefStrasse <> REc.LiefStrasse)
               THEN BEGIN
               NewIdx := Index;
               SaveIdx := Index;
               FOR Lauf := 1 TO KIndexNo DO BEGIN
                   CASE Lauf OF
                      1 : HIndexString := MakeLfdNr(Daten.LfdNr,Daten.LieferANr);
                      2 : HIndexString := UPPER(Daten.RechName);
                      3 : HIndexString := MakeFPIndex(Daten.RechName,Daten.Firma);
                      4 : HIndexString := UPPER(Daten.LiefStrasse);
                   END;
                   IndexNo := Lauf;
                   IF (SaveIdx[IndexNo].Key <> HIndexString) THEN BEGIN
                      Delete(NewIdx[IndexNo]);
                      CreateNode(HIndexString,SaveIdx[IndexNo].NodeNo,SaveIdx[IndexNo].NodeNo,
                          NewIdx[IndexNo]);
                      Insert(NewIdx[IndexNo]);
                      GetNode(NewIdx[IndexNo].NodeNo,NewIdx[IndexNo]);
                   END;
               END;
               Index := NewIdx;
            END;
            Rec := Daten;
            ChangeRec := WriteRec;
         END;
      END;

   FUNCTION TKundenFile.StartIntervall;
      VAR   Ergebnis : BOOLEAN;

      BEGIN
         ScanIdx := Kennung;
         ScanStartKey := Anfang;
         ScanEndKey := Ende;
         ScanForward := (ScanStartKey <= ScanEndKey);
         Ergebnis := GetIntTop(Daten);
         StartIntervall := Ergebnis;
      END;

   FUNCTION TKundenFile.GetIntTop;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := GetRec(ScanIdx,ScanStartKey,Daten);
         IF NOT Ergebnis THEN BEGIN
            IF ScanForward THEN
               Ergebnis:=GetNext(ScanIdx,Daten)
            ELSE
               Ergebnis:= TRUE;
         END;
         IF Ergebnis THEN
            Ergebnis := TestIntervall;
         GetIntTop := Ergebnis;
      END;

   FUNCTION TKundenFile.GetIntBottom;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := GetRec(ScanIdx,ScanEndKey,Daten);
         IF NOT Ergebnis THEN BEGIN
            IF NOT ScanForward THEN
               Ergebnis:=GetNext(ScanIdx,Daten)
            ELSE
               Ergebnis:= TRUE;
         END;
         IF Ergebnis THEN
            Ergebnis := TestIntervall;
         GetIntBottom := Ergebnis;
      END;

   FUNCTION TKundenFile.GetIntNext;
      VAR   Ergebnis: BOOLEAN;

      BEGIN
         Ergebnis :=FALSE;
         IF (ScanIdx <> 0) THEN BEGIN
            IF ScanForward THEN
               Ergebnis := GetNext(ScanIdx,Daten)
            ELSE
               Ergebnis := GetPrev(ScanIdx,Daten);
            IF Ergebnis THEN
               Ergebnis := TestIntervall;
         END;
         GetIntNext := Ergebnis;
      END;

   FUNCTION TKundenFile.GetIntPrev;
      VAR   Ergebnis: BOOLEAN;

      BEGIN
         Ergebnis :=FALSE;
         IF (ScanIdx <> 0) THEN BEGIN
            IF ScanForward THEN
               Ergebnis := GetPrev(ScanIdx,Daten)
            ELSE
               Ergebnis := GetNext(ScanIdx,Daten);
            IF Ergebnis THEN
               Ergebnis := TestIntervall;
         END;
         GetIntPrev := Ergebnis;
      END;

   FUNCTION TKundenFile.TestIntervall;
      VAR Ergebnis:BOOLEAN;
      BEGIN
         IF ScanForward THEN BEGIN
            Ergebnis := (Index[ScanIdx].Key >= ScanStartKey) AND
                             (Index[ScanIdx].Key <= ScanEndKey)
         END ELSE BEGIN
            Ergebnis := (Index[ScanIdx].Key <= ScanStartKey) AND
                             (Index[ScanIdx].Key >= ScanEndKey)
         END;
         TestIntervall := Ergebnis;
      END;

   FUNCTION TKundenFile.SeekRec;
      VAR Pos          : LONGINT;
          Dateizugriff : BOOLEAN;
          Ergebnis     : BOOLEAN;

      BEGIN
         Ergebnis := FALSE;
         Schluessel := UPPER(Schluessel);
         IndexNo := Kennung;
         IF Geoeffnet THEN BEGIN
            Ergebnis := Search(Schluessel,Index[IndexNo]);
            IF (NOT Ergebnis) AND (Schluessel < Index[IndexNo].Key) THEN
               Dateizugriff := ReadRec(Index[IndexNo].Prev)
            ELSE
               Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
         END;
         SeekRec := Ergebnis;
      END;

   FUNCTION TKundenFile.DelRec;

      VAR DelNum       : LONGINT;
          Hilfe        : INTEGER;
          DelIdx       : TKIndex;
          Dateizugriff : BOOLEAN;
          Lauf         : BYTE;

      BEGIN
         DelRec:=FALSE;
         IF SeekRec(1,Schluessel) THEN BEGIN
            DelIdx := Index;
            DelNum := Index[1].NodeNo;
            Dateizugriff := TRUE;
            FOR Lauf := 1 TO KIndexNo DO BEGIN
               IndexNo := Lauf;
               Dateizugriff := Delete(DelIdx[Lauf]);
            END;
            IF (DelNum = FILESIZE(FDatei) - 1) THEN BEGIN
               {$I-}
                  SEEK(FDatei,DelNum);
                  TRUNCATE(FDatei);
                  SEEK(FIdx,DelNum);
                  TRUNCATE(FIdx);
                  SEEK(FDatei,DelIdx[1].Prev);
               {$I+}
               Hilfe := IORESULT;
            END ELSE BEGIN
               {$I-}
                  SEEK(FRecFrei,FILESIZE(FRecFrei));
                  WRITE(FRecFrei,DelNum);
                  SEEK(FDatei,DelIdx[1].Prev);
               {$I+}
               Hilfe := IORESULT;
           END;
            DelRec:=Dateizugriff;
         END;
      END;

   FUNCTION TKundenFile.CloseFile;

      BEGIN
         CloseFile := (IORESULT  = 0);
         CloseFile := FALSE;
         IF Geoeffnet THEN BEGIN
            {$I-}
               Close(FDatei);
               Close(FRecFrei);
               Close(FIdx);
            {$I+}
            GEoeffnet := NOT (IORESULT = 0);
         END;
         CloseFile := NOT Geoeffnet;
      END;

   PROCEDURE TKundenFile.CreateNode;

      BEGIN
         WITH NewNode DO BEGIN
            Key    :=UPPER(KeyVal);
            Left   := -1;
            Right  := -1;
            Prev   := -1;
            Next   := -1;
            Father := -1;
            NodeNo := actNodeNo;
            Weight := 0;
            Val := Value;
         END;
      END;

   PROCEDURE TKundenFile.GetNode;

      BEGIN
         {$I-}
            SEEK(FIdx,NodeNo);
            READ(FIdx,Index);
            SEEK(FIdx,NodeNo);
         {$I+}
         Node := Index[IndexNo];
      END;

   FUNCTION TKundenFile.GetRoot;

      BEGIN
         GetNode(0,Node);
         IF Node.Right <> -1 THEN BEGIN
            GetNode(Node.Right,Node);
            GetRoot := TRUE;
         END ELSE
           GetRoot := FALSE;
      END;

   PROCEDURE TKundenFile.ChangeNode;

      BEGIN
         {$I-}
            SEEK(FIdx,AktNode.NodeNo);
            READ(FIdx,Index);
            Index[IndexNo] := AktNode;
            SEEK(FIdx,AktNode.NodeNo);
            WRITE(FIdx,Index);
            SEEK(FIdx,Index[IndexNo].NodeNo);
         {$I-}
      END;


   FUNCTION TKundenFile.Search;
      VAR AktNode       : KAVL_Node;
          HelpNode      : KAVL_Node;
          NodeNumber    : LONGINT;
          LowestKeyNode : LONGINT;
          Ende          : BOOLEAN;

      BEGIN
         Search := FALSE;
         IF GetRoot(AktNode) THEN BEGIN
            Ende := FALSE;
            WHILE (NOT Ende) DO BEGIN
               Node := AktNode;
               IF (KeyVal = AktNode.Key) THEN BEGIN
                  LowestKeyNode := AktNode.NodeNo;
                  Search := TRUE;
                  IF (AktNode.Left <> -1) THEN BEGIN
                     GetNode(AktNode.Left,HelpNode);
                     IF (KeyVal = HelpNode.Key) THEN BEGIN
                        AktNode := HelpNode;
                        LowestKeyNode := AktNode.NodeNo;
                     END ELSE
                        IF (AktNode.Prev <> -1) THEN BEGIN
                           GetNode(AktNode.Prev,AktNode);
                           IF (KeyVal <> AktNode.Key) THEN BEGIN
                              Ende := TRUE;
                              GetNode(LowestKeyNode,Node);
                           END;
                        END ELSE
                           Ende := TRUE;
                  END ELSE
                     IF (AktNode.Prev <> -1) THEN BEGIN
                        GetNode(AktNode.Prev,AktNode);
                        IF (KeyVal <> AktNode.Key) THEN BEGIN
                           Ende := TRUE;
                           GetNode(LowestKeyNode,Node);
                        END;
                     END ELSE
                        Ende := TRUE;
               END ELSE BEGIN
                  IF (KeyVal < AktNode.Key) THEN
                     NodeNumber:=AktNode.Left
                  ELSE
                     NodeNumber:=AktNode.Right;
                  IF NodeNumber = -1 THEN
                     Ende := TRUE
                  ELSE
                     GetNode(NodeNumber,AktNode);
               END;
            END;
         END;
      END;


   FUNCTION TKundenFile.Delete; {******* Lîschen eines Knotens ********}
      VAR RootNode,
          AktNode,
          HNode1,
          HNode2   : KAVL_Node;
          OldWeight: INTEGER;
          NodeNo   : LONGINT;
          NewNodeNo: LONGINT;
          Ergebnis : BOOLEAN;
          OldNodeNo    : LONGINT;
          WhichSon : INTEGER;
          Ende     : BOOLEAN;

      { *********** Lîschen eines Knotens aus dem binÑren Baum *********** }

      FUNCTION DeleteBin(AktNode:KAVL_Node;VAR FatherNode:KAVL_Node):INTEGER;

         VAR MinNode  : KAVL_Node;
             HelpNode : KAVL_Node;
             Ende : BOOLEAN;
             NewNodeNo   : LONGINT;
             OldNodeNo    : LONGINT;
             OldFatherNo : LONGINT;

         FUNCTION DeleteMin(RootNode:KAVL_Node;VAR ResultNode:KAVL_Node):INTEGER;

            { Lîscht kleinstes Element des rechten Teilbaums eines
              Knotens und gibt den Knoten als RESULTNODE zurÅck }

            VAR FatherNode,
                AktNode : KAVL_Node;

            BEGIN
               GetNode(RootNode.Right,AktNode);
               WHILE AktNode.Left <> -1 DO
                  GetNode(AktNode.Left,AktNode);
               ResultNode := AktNode;
               GetNode(ResultNode.Father,FatherNode);
               IF (FatherNode.Left = ResultNode.NodeNo) THEN BEGIN
                  DeleteMin := 1;
                  FatherNode.Left := ResultNode.Right
               END ELSE BEGIN
                  DeleteMin := -1;
                  FatherNode.Right := ResultNode.Right;
               END;
               ChangeNode(FatherNode);

               IF (ResultNode.Right <> -1) THEN BEGIN
                  GetNode(ResultNode.Right,AktNode);
                  AktNode.Father := FatherNode.NodeNo;
                  ChangeNode(AktNode);
               END;

            END; { ******************* Ende von DELETEMIN ***************** }

         BEGIN   { ******************* Anfang von DELETEBIN ***************** }

            GetNode(AktNode.Prev,HelpNode); { ** Anpassen des linearen Index ** }
            HelpNode.Next := AktNode.Next;
            ChangeNode(HelpNode);
            IF (AktNode.Next <> -1) THEN BEGIN
               GetNode(AktNode.Next,HelpNode);
               HelpNode.Prev := AktNode.Prev;
               ChangeNode(HelpNode);
            END;

            { ***** Anpassen der Baumstruktur  **** }

            { ***** Erster Fall, Knoten hat zwei Sîhne ****** }

            IF (AktNode.Right <> -1) AND (AktNode.Left <> -1) THEN BEGIN
               DeleteBin := DeleteMin(AktNode,MinNode);
               GetNode(AktNode.NodeNo,AktNode);

               OldNodeNo := AktNode.NodeNo;

               MinNode.Left := AktNode.Left;
               MinNode.Right := AktNode.Right;
               MinNode.Weight := AktNode.Weight;

               IF MinNode.Father <> AktNode.NodeNo THEN
                  OldFatherNo := MinNode.Father
               ELSE
                  OldFatherNo := MinNode.NodeNo;

               MinNode.Father := AktNode.Father;
               ChangeNode(MinNode);
               GetNode(MinNode.Father,AktNode);

               IF (AktNode.Left = OldNodeNo) THEN
                  AktNode.Left := MinNode.NodeNo
               ELSE
                  AktNode.Right := MinNode.NodeNo;
               ChangeNode(AktNode);
               IF MinNode.Left <> -1 THEN BEGIN
                  GetNode(MinNode.Left,AktNode);
                  AktNode.Father := MinNode.NodeNo;
                  ChangeNode(AktNode);
               END;
               IF MinNode.Right <> -1 THEN BEGIN
                  GetNode(MinNode.Right,AktNode);
                  AktNode.Father := MinNode.NodeNo;
                  ChangeNode(AktNode);
               END;

               AktNode := MinNode;
               GetNode(OldFatherNo,FatherNode);
            END ELSE BEGIN

            { ******* Zweiter Fall, Knoten hat maximal einen Sohn ****** }

               OldNodeNo := AktNode.NodeNo;
               GetNode(AktNode.Father,FatherNode);
               IF (AktNode.Left = -1) THEN
                  NewNodeNo := AktNode.Right
               ELSE
                  NewNodeNo := AktNode.Left;

               IF NewNodeNo <> -1 THEN BEGIN
                  GetNode(NewNodeNo,AktNode);
                  AktNode.Father := FatherNode.NodeNo;
                  ChangeNode(AktNode);
               END;
               IF (FatherNode.Left = OldNodeNo) THEN BEGIN
                  DeleteBin := 1;
                  FatherNode.Left := NewNodeNo
               END ELSE BEGIN
                  FatherNode.Right := NewNodeNo;
                  DeleteBin := -1;
               END;
               ChangeNode(FatherNode);
            END;
         END; { ******************* Ende von DELETEBIN ***************** }

      BEGIN { ******************* Anfang von DELETE ***************** }
         Delete:= FALSE;
         WhichSon := DeleteBin(DelNode,AktNode);
         Ende := FALSE;
         WHILE (AktNode.NodeNo <> 0) AND (NOT Ende) DO BEGIN
            AktNode.Weight := AktNode.Weight-WhichSon;
            CASE AktNode.Weight OF
               -1,1 : BEGIN
                         ChangeNode(AktNode);
                         Ende := TRUE;
                      END;

                  0 : ChangeNode(AktNode);

                 -2 : BEGIN
                         GetNode(AktNode.Right,HNode1);
                         CASE HNode1.Weight OF
                             0 : BEGIN
                                    LeftRotate(AktNode.NodeNo,-1,1);
                                    Ende := TRUE;
                                 END;
                            -1 : BEGIN
                                    LeftRotate(AktNode.NodeNo,0,0);
                                    GetNode(AktNode.Right,AktNode);
                                 END;
                             1 : BEGIN
                                    GetNode(HNode1.Left,HNode2);
                                    CASE HNode2.Weight OF
                                        0 : BEGIN
                                               RightRotate(HNode1.NodeNo,0,0);
                                               LeftRotate(AktNode.NodeNo,0,0);
                                            END;
                                        1 : BEGIN
                                               RightRotate(HNode1.NodeNo,-1,0);
                                               LeftRotate(AktNode.NodeNo,0,0);
                                            END;
                                       -1 : BEGIN
                                               RightRotate(HNode1.NodeNo,0,0);
                                               LeftRotate(AktNode.NodeNo,1,0);
                                            END;
                                    END;
                                    GetNode(HNode2.NodeNo,AktNode);
                                 END;
                         END;
                      END;
                 2  : BEGIN
                         GetNode(AktNode.Left,HNode1);
                         CASE HNode1.Weight OF
                             0 : BEGIN
                                    RightRotate(AktNode.NodeNo,1,-1);
                                    Ende := TRUE;
                                 END;
                             1 : BEGIN
                                    RightRotate(AktNode.NodeNo,0,0);
                                    GetNode(AktNode.Left,AktNode);
                                 END;
                            -1 : BEGIN
                                    GetNode(HNode1.Right,HNode2);
                                    CASE HNode2.Weight OF
                                        0 : BEGIN
                                               LeftRotate(HNode1.NodeNo,0,0);
                                               RightRotate(AktNode.NodeNo,0,0);
                                            END;
                                       -1 : BEGIN
                                               LeftRotate(HNode1.NodeNo,1,0);
                                               RightRotate(AktNode.NodeNo,0,0);
                                            END;
                                        1 : BEGIN
                                               LeftRotate(HNode1.NodeNo,0,0);
                                               RightRotate(AktNode.NodeNo,-1,0);
                                            END;
                                    END;
                                    GetNode(HNode2.NodeNo,AktNode);
                                 END;
                         END;
                      END;
            END;
            IF (NOT Ende) THEN BEGIN
               OldNodeNo := AktNode.NodeNo;
               GetNode(AktNode.Father,AktNode);
               IF (AktNode.Left = OldNodeNo) THEN
                  WhichSon := 1
               ELSE
                  WhichSon := -1;
            END;
         END;
         Delete := TRUE;

      END; { ******************* Ende von DELETE ***************** }



   PROCEDURE TKundenFile.Insert;
      VAR FirstNode,
          AktNode,
          HNode1,
          HNode2   : KAVL_Node;
          AktNodeNo   : LONGINT;
          NewNodeNo: LONGINT;
          Ergebnis : BOOLEAN;
          Ende : BOOLEAN;
          D    : BOOLEAN;

      PROCEDURE InsertBin(FirstNode:KAVL_Node;VAR NewNode:KAVL_Node);
         VAR AktNode    : KAVL_Node;
             Ende       : BOOLEAN;
             NextNodeNo : LONGINT;
             NewNodeNo  : LONGINT;
             HelpNode   : KAVL_Node;

         BEGIN
            AktNode := FirstNode;
            NewNodeNo := NewNode.NodeNo;
            Ende := FALSE;
            WHILE NOT Ende DO BEGIN
               IF NewNode.Key < AktNode.Key THEN
                  NextNodeNo := AktNode.Left
               ELSE
                  NextNodeNo := AktNode.Right;
               IF NextNodeNo <> -1 THEN
                  GetNode(NextNodeNo,AktNode)
               ELSE
                  Ende := TRUE;
            END;
            NewNode.Father := AktNode.NodeNo;
            IF NewNode.Key < AktNode.Key THEN BEGIN
               AktNode.Left := NewNodeNo;
               NewNode.Prev := AktNode.Prev;
               NewNode.Next := AktNode.NodeNo;
               AktNode.Prev := NewNodeNo;
               GetNode(NewNode.Prev,HelpNode);
               HelpNode.Next := NewNode.NodeNo;
               ChangeNode(HelpNode);
            END ELSE BEGIN
               AktNode.Right := NewNodeNo;
               NewNode.Prev := AktNode.NodeNo;
               NewNode.Next := AktNode.Next;
               AktNode.Next := NewNodeNo;
               IF NewNode.Next <> -1 THEN BEGIN
                  GetNode(NewNode.Next,HelpNode);
                  HelpNode.Prev := NewNode.NodeNo;
                  ChangeNode(HelpNode);
               END;
            END;
            ChangeNode(NewNode);
            ChangeNode(AktNode);
         END;

      BEGIN
         d:=GetRoot(FirstNode);
         InsertBin(FirstNode,NewNode);
         AktNodeNo := NewNode.Father;
         Ende := FALSE;
         WHILE (AktNodeNo <> 0) AND (NOT Ende) DO BEGIN
            GetNode(AktNodeNo,AktNode);
            WITH AktNode DO BEGIN
               IF NewNode.Key < Key THEN
                  INC(Weight)
               ELSE
                  DEC(Weight);
               Ende := TRUE;
               CASE Weight OF
                     0 : BEGIN
                            ChangeNode(AktNode);
                         END;
                  -1,1 : BEGIN
                            AktNodeNo := Father;
                            Ende := FALSE;
                         END;
                  -2   : BEGIN
                            GetNode(Right,HNode1);
                            CASE HNode1.Weight OF
                               -1 : LeftRotate(AktNodeNo,0,0);
                                1 : BEGIN
                                       GetNode(HNode1.Left,HNode2);
                                       CASE  HNode2.Weight OF
                                          -1 : BEGIN
                                                 RightRotate(Right,0,0);
                                                 LeftRotate(AktNodeNo,1,0);
                                               END;
                                           0 : BEGIN
                                                 RightRotate(Right,0,0);
                                                 LeftRotate(AktNodeNo,0,0);
                                               END;
                                           1 : BEGIN
                                                  RightRotate(Right,-1,0);
                                                  LeftRotate(AktNodeNo,0,0);
                                                END;
                                       END;
                                    END;
                            END;
                         END;
                   2   : BEGIN
                            GetNode(Left,Hnode1);
                            CASE HNode1.Weight OF
                                1 : RightRotate(AktNodeNo,0,0);
                               -1 : BEGIN
                                       GetNode(HNode1.Right,HNode2);
                                       CASE HNode2.Weight OF
                                          1 : BEGIN
                                                 LeftRotate(Left,0,0);
                                                 RightRotate(AktNodeNo,-1,0);
                                              END;
                                          0 : BEGIN
                                                 LeftRotate(Left,0,0);
                                                 RightRotate(AktNodeNo,0,0);
                                              END;
                                         -1 : BEGIN
                                                 LeftRotate(Left,1,0);
                                                 RightRotate(AktNodeNo,0,0);
                                              END;
                                       END;
                                    END;
                            END;
                         END;
                  END;
               END;  {Weight-Case-Ende}
               IF (NOT Ende) THEN
                 ChangeNode(AKtNode);
            END;
      END;

   PROCEDURE TKundenFile.LeftRotate;
      VAR SonNo,
          FatherNo   : LONGINT;
          AktNode,
          RightNode,
          LeftNode,
          FatherNode :KAVL_Node;

      BEGIN
         GetNode(NodeNo,AktNode);
         GetNode(AktNode.Right,RightNode);
         GetNode(AktNode.Father,FatherNode);
         RightNode.Father := AktNode.Father;
         IF FatherNode.Left = NodeNo THEN
            FatherNode.Left := AktNode.Right
         ELSE
            FatherNode.Right := AktNode.Right;

         AktNode.Father := AktNode.Right;
         AktNode.Right := RightNode.Left;
         RightNode.Left:= NodeNo;
         AktNode.Weight:= NodeWeight;
         RightNode.Weight := SonWeight;
         IF AktNode.Right <> -1 THEN BEGIN
            GetNode(AktNode.Right,LeftNode);
            LeftNode.Father := NodeNo;
            ChangeNode(LeftNode);
         END;
         ChangeNode(AktNode);
         ChangeNode(RightNode);
         ChangeNode(FatherNode);
      END;

   PROCEDURE TKundenFile.RightRotate;
      VAR SonNo,
          FatherNo   : LONGINT;
          AktNode,
          LeftNode,
          RightNode,
          FatherNode :KAVL_Node;

      BEGIN
         GetNode(NodeNo,AktNode);
         GetNode(AktNode.Left,LeftNode);
         GetNode(AktNode.Father,FatherNode);
         LeftNode.Father := AktNode.Father;
         IF FatherNode.Left = NodeNo THEN
            FatherNode.Left := AktNode.Left
         ELSE
            FatherNode.Right := AktNode.Left;
         AktNode.Father := AktNode.Left;
         AktNode.Left := LeftNode.Right;
         LeftNode.Right:= NodeNo;
         AktNode.Weight:= NodeWeight;
         LeftNode.Weight := SonWeight;
         IF AktNode.Left <> -1 THEN BEGIN
            GetNode(AktNode.Left,RightNode);
            RightNode.Father := NodeNo;
            ChangeNode(RightNode);
         END;
         ChangeNode(AktNode);
         ChangeNode(LeftNode);
         ChangeNode(FatherNode);
      END;

BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
