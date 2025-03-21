{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Dec 17 08:13:18 GMT+01:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT KBFILE;

INTERFACE

USES KBDATEN,
     ZUSAETZE,
     DOS;


CONST KBMaxSearchMonth = 12;
      { Gibt an wieviele Monate vorher oder spÑter
        in der Datenbank nach einem SchlÅssel der
        nicht direkt mit dem Datenbanktrennenden
        Datenelement verbunden ist an. }



TYPE TKBValType = LONGINT;
     { Typ fÅr den Knotenwert, wird nicht weiter eingesetzt }
TYPE TKBKeyType = STRING[40];
     { Typ fÅr den KnotenschlÅssel }


TYPE KBAVL_Node = RECORD
        Left,
        Father,
        Right,
        Prev,
        Next,
        NodeNo : LONGINT;
        Weight : INTEGER;
        Key    : TKBKeyType;
        Val    : TKBValType;
     END;
     { Typ fÅr den Knoten eines Index, Left, Right, Prev, Next und Father
       enthalten Verweise auf die in der Baumstruktur damit verbundenen
       Knoten. Wenn ein Verweis den Wert -1 enthÑlt existiert an dieser
       Position kein weiterer Datensatz. Der VorgÑnger Prev des ersten
       echten Datensatzes der Datenbank ist der Knoten 0. NodeNo gibt
       die Recordposition innerhalb der Datei an. Weight ist nur fÅr die
       Balanzierung des Baumes wichtig und hat keinen direkten Einflu· auf
       die Indices. Key enthÑlt den zugehîrigen SchlÅsselwert. }

TYPE TKBIndex = ARRAY [1..KBIndexNo] OF KBAVL_Node;
     { Datentyp fÅr alle Indexknoten der Lieferdatenbank.
       Der Datentyp ist Basis der Recordstruktur von FKBIndex }
     TKBIndexType = ARRAY[1..KBIndexNo] OF BYTE;
     { Datentyp fÅr Indextyp :
       0 = normaler Index
       1 = DateiÅbergreifender Index
       2 = DateiÅbergreifender Index,
           Der SchlÅsselaufbau dieses Index ist nur sekundÑr mit der
           Dateitrennung verbunden. Um einen dateiÅbergreifenden Lauf
           Åber ein Wertintervall durchzufÅhren mÅssen hier die Methoden
           ..... verwendet werden}


TYPE FKBIndex = FILE OF TKBIndex;
     { Datentyp fÅr Indexdatei }
TYPE FKBStamm = FILE OF TKBDaten;
     { Datentyp fÅr Lieferdatenbank }
TYPE FKBRecs = FILE OF LONGINT;
     { Datei zur Aufnahme der freien Records innerhalb einer Datenbank }
TYPE FKBHigh = FILE OF LONGINT;
     { Datei zur Aufnahme der hîchsten Liefernummer (dateiÅbergreifend) }

TYPE TKBFilePtr = ^TKBFile;
     TKBFile = OBJECT
        FDatei    : FKBStamm;
        FRecFrei  : FKBRecs;
        FIdx      : FKBIndex;
        FLH       : FKBHigh;
        IsOpen    : BOOLEAN;
        Rec       : TKBDaten;
        Index     : TKBIndex;
        IndexNo   : BYTE;
        IndexType : TKBIndexType;
        ScanIdx   : BYTE;
        ScanStartKey: STRING;
        ScanEndKey : STRING;
        ScanForward : BOOLEAN;
        ScanDBChange : BOOLEAN;
        MultipleFileAccess  : BOOLEAN;
        ActualDB           : STRING;
        ActualMonth        : STRING[4];
        StartMonth         : STRING[4];
        DBDirectory        : STRING;
        FileRemainsOpen    : BOOLEAN;
        OnlyTransAction    : BOOLEAN;
        IsTransaction      : BOOLEAN;
        TransactionErrCode : INTEGER;

        CONSTRUCTOR OpenFile(DateiName:STRING;Verzeichnis:STRING;
                    MultipleFiles:BOOLEAN;Datum:STRING);
        FUNCTION  AssignFiles(DateiName:STRING):BOOLEAN;
        PROCEDURE CreateFLH;
        PROCEDURE ReOpenFile;
        PROCEDURE SetTransactionOnly;
        PROCEDURE SetFreeAccess;
        FUNCTION BeginTransaction:BOOLEAN;
        FUNCTION EndTransAction:BOOLEAN;
        FUNCTION GetTransactionError:INTEGER;
        FUNCTION GetTransactionErrMsg:STRING;
        FUNCTION GetTop(Kennung:BYTE;VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetBottom(Kennung:BYTE;VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetNext(Kennung:BYTE;VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetPrev(Kennung:BYTE;VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION StartIntervall(Kennung:BYTE; Anfang,Ende:STRING;VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetIntTop(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetIntBottom(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetIntNext(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION GetIntPrev(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION NewRec(Daten:TKBDaten):BOOLEAN;
        FUNCTION GetRec(Kennung:BYTE;Schluessel:STRING;
                            VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION ChangeRec(Daten:TKBDaten):BOOLEAN;
        FUNCTION DelRec(Schluessel:STRING):BOOLEAN;
        FUNCTION CloseFile:BOOLEAN;
        FUNCTION GetLfdNr:LONGINT;
        PROCEDURE SetLfdNr(Nr :LONGINT);
        FUNCTION MonthDBExists(Month: STRING):BOOLEAN;
        FUNCTION ChangeMonthDB(Month:STRING):BOOLEAN;
        FUNCTION NextMonthDB:BOOLEAN;
        FUNCTION PrevMonthDB:BOOLEAN;
        FUNCTION NextMonthDBStr(AMonth:STRING):STRING;
        FUNCTION PrevMonthDBStr(AMonth:STRING):STRING;





        PRIVATE
        FUNCTION ContinueInNextDB(Daten:TKBDaten):BOOLEAN;
        FUNCTION ContinueInPRevDB(Daten:TKBDaten):BOOLEAN;
        FUNCTION ScanNextIntervallDB(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION ScanPrevIntervallDB(VAR Daten:TKBDaten):BOOLEAN;
        FUNCTION TestIntervall:BOOLEAN;
        FUNCTION MakeLowMonthKey(Kennung:BYTE;Schluessel:STRING;
                    Month:STRING):STRING;
        FUNCTION MakeHighMonthKey(Kennung:BYTE;Schluessel:STRING;
                    Month:STRING):STRING;
        FUNCTION CreateMonthDB(Month:STRING):BOOLEAN;
        FUNCTION ReadRec(Pos:LONGINT):BOOLEAN;
        FUNCTION WriteRec:BOOLEAN;
        FUNCTION SeekRec(Kennung:BYTE;Schluessel:STRING):BOOLEAN;
        FUNCTION SeekRecMultiple(Kennung:BYTE;Schluessel:STRING):BOOLEAN;
        PROCEDURE CreateNode(KeyVal:TKBKeyType;Value:TKBValType;
                                actNodeNo:LONGINT;VAR NewNode:KBAVL_Node);
        PROCEDURE Insert(NewNode:KBAVL_Node);
        PROCEDURE GetNode(NodeNo:LONGINT;VAR Node:KBAVL_Node);
        FUNCTION GetRoot(VAR Node:KBAVL_Node):BOOLEAN;
        PROCEDURE ChangeNode(AktNode:KBAVL_Node);
        FUNCTION Search(KeyVal:TKBKeyType;VAR Node:KBAVL_Node):BOOLEAN;
        FUNCTION Delete(DelNode:KBAVL_Node):BOOLEAN;
        PROCEDURE LeftRotate(NodeNo:LONGINT;NodeWeight,SonWeight:INTEGER);
        PROCEDURE RightRotate(NodeNo:LONGINT;NodeWeight,SonWeight:INTEGER);
     END;

IMPLEMENTATION

   CONST KBHGFile = 'KBDB.LHG';
   CONST TransactionErrMessages : ARRAY[1..4] OF STRING =
     ('active transaction','no transaction',
      'file open','file not closed');




   CONSTRUCTOR TKBFile.OpenFile;

      VAR FileInfo     : SearchRec;
          Lauf         : BYTE;
          Dateizugriff : BOOLEAN;
          AnzRecs      : REAL;

      BEGIN
         IndexType[1] := 0;
         IndexType[2] := 1;
         IndexType[3] := 2;
         IndexType[4] := 1;
         IndexType[5] := 0;
         MultipleFileAccess := MultipleFiles;
         DBDirectory :=MakeFilePath(Verzeichnis);
         OnlyTransAction := FALSE;
         IsTransAction := FALSE;
         TransActionErrCode := 0;
         IsOpen := FALSE;
         IndexNo := 1;
         IF MultipleFiles THEN BEGIN
            IF (Datum <> '') THEN BEGIN
               ActualMonth := COPY(Datum,4,2)+COPY(Datum,7,2);
            END ELSE BEGIN
               IF (UPPER(COPY(Dateiname,1,4))<> 'KB_') OR NOT
                  ValidMonth(COPY(Dateiname,4,4)) THEN BEGIN
                  ActualMonth := COPY(ActDate,4,2)+COPY(ActDate,7,2);
               END ELSE
                  ActualMonth := COPY(Dateiname,4,4);
            END;
            StartMonth := ActualMonth;
            Dateiname := 'KB_'+ActualMonth;
         END;
         AssignFiles(Dateiname);
         IF NOT SeekFile(MakeFileName(ActualDB,'AVL')) THEN BEGIN
            {$I-}
               REWRITE(FDatei);
               REWRITE(FRecFrei);
               REWRITE(FIDx);
            {$I+}
            IF (IORESULT = 0) THEN BEGIN
               InitTKBDaten(Rec);
               Rec.LfdNr := 0;
               FOR Lauf := 1 TO KBIndexNo DO BEGIN
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
               IsOpen := WriteRec;
            END;
            IF IsOpen THEN BEGIN
               IF MultipleFileAccess THEN BEGIN
                  {$I-}
                     Close(FDatei);
                     Close(FRecFrei);
                     Close(FIdx);
                  {$I+}
                  CreateFLH;
                  {$I-}
                     Reset(FDatei);
                     Reset(FRecFrei);
                     Reset(FIdx);
                  {$I+}
               END;
            END;
         END ELSE BEGIN
            IF (CorrectSize(MakeFileName(ActualDB,'AVL'),SizeOf(TKBDaten))) AND
               (CorrectSize(MakeFileName(ActualDB,'IDX'),SizeOf(TKBIndex))) AND
               (NumberOfRecs(MakeFileName(ActualDB,'AVL'),SizeOf(TKBDaten)) =
                NumberOfRecs(MakeFileName(ActualDB,'IDX'),SizeOf(TKBIndex))) THEN BEGIN
               {$I-}
                  IF MultipleFileAccess THEN BEGIN
                        CreateFLH;
                  END;
                  RESET(FDatei);
                  RESET(FIdx);
                  IF SeekFile(MakeFileName(ActualDB,'FRN')) THEN
                     RESET(FRecFrei)
                  ELSE
                     REWRITE(FRecFrei);
                  Dateizugriff := ReadRec(0);
               {$I+}
               IsOpen := (IORESULT = 0 );
            END ELSE
               IsOpen := FALSE;
         END;
      END;

   FUNCTION TKBFile.AssignFiles;
      VAR Ergebnis : BOOLEAN;
          Frei         : STRING;
          IdxName      : STRING;
          LiefHigh     : STRING;

      BEGIN
         DateiName := DBDirectory+CutSuffix(DateiName);
         ActualDB := Dateiname;
         Frei   :=MakeFileName(Dateiname,'FRN');
         IdxName:=MakeFileName(Dateiname,'IDX');
         DateiName:=MakeFileName(Dateiname,'AVL');
         {$I-}
         ASSIGN(FDatei,Dateiname);
         ASSIGN(FRecFrei,Frei);
         ASSIGN(FIdx,IdxName);
         {$I+}
         Ergebnis := (IORESULT = 0 );
         IF MultipleFileAccess THEN BEGIN
            LiefHigh:=DBDirectory+KBHGFile;
            ASSIGN(FLH,LiefHigh);
         END;
         Ergebnis := Ergebnis AND (IORESULT = 0);
         AssignFiles := Ergebnis;
      END;

   PROCEDURE TKBFile.CreateFLH;
      VAR MLauf : STRING;
          Lauf  : BYTE;
          MaxLief: LONGINT;
          HLief : LONGINT;
          HDB : FKBStamm;
          HDaten : TKBDaten;
      BEGIN
         IF SEEKFile(DBDirectory+KBHGFile) THEN BEGIN
            {$I-}
               RESET(FLH);
               SEEK(FLH,0);
               READ(FLH,MaxLief);
            {$I+}
         END ELSE BEGIN
            {$I-}
               REWRITE(FLH);
            {$I+}
            MaxLief := 0;
         END;
         MLauf := NextMonth(NextMonth(StartMonth));
         Lauf := 6;
         WHILE Lauf > 0 DO BEGIN
            IF MonthDBExists(MLauf) THEN BEGIN
               {$I-}
               ASSIGN(HDB,DBDirectory+'KB_'+MLauf+'.AVL');
               RESET(HDB);
               SEEK(HDB,0);
               READ(HDB,HDaten);
               CLOSE(HDB);
               {$I+}
               HLief := HDaten.LfdNr;
               IF MaxLief < HLief THEN
                  MaxLief := HLief;
            END;
            DEC(Lauf);
            MLauf := PrevMonth(MLauf);
         END;
         {$I-}
            SEEK(FLH,0);
            WRITE(FLH,MaxLief);
            CLOSE(FLH);
            RESET(FLH);
         {$I+}
      END;


   PROCEDURE TKBFile.ReOpenFile;
      VAR  DateiZugriff : BOOLEAN;
      BEGIN
         IF (NOT IsOpen) THEN BEGIN
            {$I-}
               RESET(FDatei);
               RESET(FIdx);
               RESET(FRecFrei);
               IF MultipleFileAccess THEN
                  RESET(FLH);
               Dateizugriff := ReadRec(Index[indexNo].NodeNo);
            {$I+}
            IsOpen := (IORESULT = 0 );
         END;
      END;

   PROCEDURE TKBFile.SetTransActionOnly;
      BEGIN
         OnlyTransAction := TRUE;
         IF IsOpen THEN
            IsOpen := NOT CloseFile;
      END;

   PROCEDURE TKBFile.SetFreeAccess;
      BEGIN
         OnlyTransAction := FALSE;
         IF NOT IsOpen THEN
            ReOpenFile;

      END;


   FUNCTION TKBFile.BeginTransAction;
      BEGIN
         BeginTransAction:= FALSE;
         IF IsTransAction THEN
            TransActionErrCode := 1
         ELSE BEGIN
            IF (IsOpen AND OnlyTransaction) THEN
               TransActionErrCode :=3
            ELSE BEGIN
               FileRemainsOpen := IsOpen;
               IF NOT IsOpen THEN
                  ReOpenFile;
               IsTransAction := TRUE;
               TransActionErrCode := 0;
               BeginTransAction := TRUE;
            END;
         END;
      END;

   FUNCTION TKBFile.EndTransAction;
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

   FUNCTION TKBFile.GetTransActionError;
      BEGIN
         GetTransActionError := TransactionErrcode;
      END;

   FUNCTION TKBFile.GetTransActionErrMsg;
      BEGIN
         IF TransactionErrCode <> 0 THEN
            GetTransActionErrMsg:=TransactionErrMessages[TransactionErrCode]
         ELSE
            GetTransActionErrMsg:= '';
      END;

   FUNCTION TKBFile.MonthDBExists;
      VAR Ergebnis : BOOLEAN;
          Dateiname : STRING;
      BEGIN
         Dateiname := DBDirectory+'KB_'+Month;
         Ergebnis := SeekFile(MakeFileName(Dateiname,'AVL')) AND
                     SeekFile(MakeFileName(Dateiname,'IDX')) AND
                     SeekFile(MakeFileName(Dateiname,'FRN'));
         MonthDBExists:= Ergebnis
      END;

   FUNCTION TKBFile.NextMonthDB;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         IF MultipleFileAccess THEN
            Ergebnis := ChangeMonthDB(NextMonthDBStr(ActualMonth));
         NextMonthDB := Ergebnis;
      END;

   FUNCTION TKBFile.PrevMonthDB;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         IF MultipleFileAccess THEN

            Ergebnis := ChangeMonthDB(PrevMonthDBStr(ActualMonth));
         PrevMonthDB := Ergebnis;
      END;

   FUNCTION TKBFile.NextMonthDBStr;
      VAR Ergebnis : STRING;
          FMonth   : STRING;
          OneFound : BOOLEAN;
          FileInfo : SearchRec;
      CONST SeekPattern = 'KB_*.AVL';
      BEGIN
         OneFound:= FALSE;
         Ergebnis:='';
         FINDFIRST(DBDirectory+SeekPattern,AnyFile,FileInfo);
         WHILE (DOSError = 0) DO BEGIN
             FMonth := COPY(CutSuffix(FileInfo.Name),4,4);
             IF LaterMonth(FMonth,Amonth) THEN BEGIN
                IF OneFound THEN BEGIN
                   IF EarlierMonth(FMonth,Ergebnis) THEN BEGIN
                      IF MonthDBExists(FMonth) THEN
                         Ergebnis := FMonth;
                   END
                END ELSE BEGIN
                   IF MonthDBExists(FMonth) THEN BEGIN
                      Ergebnis := FMonth;
                      OneFound := TRUE;
                   END;
                END;
             END;
             FINDNEXT(FileInfo);
         END;
         NextMonthDBStr := Ergebnis;
      END;

   FUNCTION TKBFile.PrevMonthDBStr;
      VAR Ergebnis : STRING;
          FMonth   : STRING;
          OneFound : BOOLEAN;
          FileInfo : SearchRec;
      CONST SeekPattern = 'KB_*.AVL';
      BEGIN
         OneFound:= FALSE;
         Ergebnis:='';
         FINDFIRST(DBDirectory+SeekPattern,AnyFile,FileInfo);
         WHILE (DOSError = 0) DO BEGIN
             FMonth := COPY(CutSuffix(FileInfo.Name),5,4);
             IF EarlierMonth(FMonth,Amonth) THEN BEGIN
                IF OneFound THEN BEGIN
                   IF LaterMonth(FMonth,Ergebnis) THEN BEGIN
                      IF MonthDBExists(FMonth) THEN
                         Ergebnis := FMonth;
                   END
                END ELSE BEGIN
                   IF MonthDBExists(FMonth) THEN BEGIN
                      Ergebnis := FMonth;
                      OneFound := TRUE;
                   END;
                END;
             END;
             FINDNEXT(FileInfo);
         END;
         PrevMonthDBStr := Ergebnis;
      END;

   FUNCTION TKBFile.ChangeMonthDB;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         IF MultipleFileAccess THEN BEGIN
            IF (Month <> ActualMonth) THEN BEGIN
               IF MonthDBExists(Month) THEN BEGIN
                  IF IsOpen THEN
                     Ergebnis := CloseFile;
                  IF Ergebnis THEN BEGIN
                     ActualMonth := Month;
                     Ergebnis :=AssignFiles('KB_'+Month);
                     IF Ergebnis THEN
                        ReOpenFile;
                  END;
               END
            END ELSE
               Ergebnis := TRUE;
         END;
         ChangeMonthDB := Ergebnis;
      END;


   FUNCTION TKBFile.GetLfdNr;
     VAR Dateizugriff : BOOLEAN;
         Ergebnis : LONGINT;

     BEGIN
       Ergebnis := 0;
       IF MultipleFileAccess THEN BEGIN
          {$I-}
             SEEK(FLH,0);
             READ(FLH,Ergebnis);
          {$I+}
          Dateizugriff := (IORESULT = 0);
       END ELSE BEGIN
          Dateizugriff := ReadRec(0);
          Ergebnis := Rec.LfdNr;
       END;
       GetLfdNr :=Ergebnis;
     END;

   PROCEDURE TKBFile.SetLfdNr;
     VAR Dateizugriff : BOOLEAN;
         LHWert : LONGINT;

     BEGIN
       Dateizugriff := ReadRec(0);
       Rec.LfdNr := Nr;
       Dateizugriff := WriteRec;
       IF MultipleFileAccess THEN BEGIN
          {$I-}
             SEEK(FLH,0);
             WRITE(FLH,Nr);
          {$I+}
       END;
     END;

   FUNCTION TKBFile.ReadRec;

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

   FUNCTION TKBFile.WriteRec;

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

   FUNCTION TKBFile.CreateMonthDB;
      VAR Ergebnis : BOOLEAN;
          HDatei : FKBStamm;
          HIdx : FKBIndex;
          HRecFrei : FKBRecs;
          HDName : STRING;
          HDAten : TKBDaten;
          HIndex : TKBIndex;
          LFHNr : LONGINT;
          Lauf : BYTE;

      BEGIN
         HDName := DBDirectory+'KB_'+Month;
         {$I-}
            ASSIGN(HDatei,MakeFileName(HDName,'AVL'));
            ASSIGN(HIDX,MakeFileName(HDName,'IDX'));
            ASSIGN(HRecFrei,MakeFileName(HDName,'FRN'));
            REWRITE(HDatei);
            REWRITE(HRecFrei);
            REWRITE(HIDx);
         {$I+}
         Ergebnis := (IORESULT = 0);
         LFHNr := GetLFdNr;
         InitTKBDaten(Hdaten);
         HDaten.LfdNr := LFHNr;
         FOR Lauf := 1 TO KBIndexNo DO BEGIN
            HIndex[Lauf].Key    := '';
            HIndex[Lauf].Prev   := -1;
            HIndex[Lauf].Next   := -1;
            HIndex[Lauf].Weight := 0;
            HIndex[Lauf].NodeNo := 0;
            HIndex[Lauf].Left   := -1;
            HIndex[Lauf].Right  := -1;
            HIndex[Lauf].Father := -1;
            HIndex[Lauf].Val    := 0;
         END;
         {$I-}
            SEEK(HDatei,0);
            WRITE(HDatei,HDaten);
            SEEK(HIdx,0);
            WRITE(HIdx,HIndex);
            CLOSE(HDatei);
            CLOSE(HRecFrei);
            CLOSE(HIdx);
         {$I+}
         Ergebnis := Ergebnis AND (IORESULT = 0);
         CreateMonthDB := Ergebnis;
      END;

   FUNCTION TKBFile.NewRec;
      VAR NRec         : TKBDaten;
          NewIdx       : TKBIndex;
          Neu          : LONGINT;
          NRMonth      : STRING[4];
          Schliessen,
          Dateizugriff : BOOLEAN;
          Lauf         : BYTE;
          Ergebnis     : BOOLEAN;

      BEGIN
         Ergebnis :=  FALSE;
         IF IsOpen THEN BEGIN
            IF MultipleFileAccess THEN BEGIN
               NRMonth := COPY(Daten.TagesDatum,4,2)+
                          COPY(Daten.TagesDatum,7,2);
               Ergebnis := (NrMonth = ActualMonth);
               IF NOT Ergebnis THEN  BEGIN
                  IF MonthDBExists(NrMonth) THEN
                     Ergebnis := ChangeMonthDB(NrMonth)
                  ELSE BEGIN
                     Ergebnis := CreateMonthDB(NrMonth);
                     Ergebnis := Ergebnis AND ChangeMonthDB(NrMonth);
                  END;
               END;
            END ELSE
               Ergebnis := TRUE;
            IF Ergebnis THEN BEGIN
               IF (NOT SeekRec(1,CreateKBIndex(1,Daten))) THEN BEGIN
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
                  FOR Lauf := 1 TO  KBIndexNo DO
                     CreateNode(CreateKBIndex(Lauf,Daten),Neu,Neu,NewIdx[Lauf]);
                  Index := NewIdx;
                  Dateizugriff := WriteRec;
                  FOR Lauf := 1 TO KBIndexNo DO BEGIN
                     IndexNo := Lauf;
                     Insert(NewIdx[Lauf]);
                  END;
                  GetNode(Neu,NewIdx[KBIndexNo]);
                  IndexNo := 1;
                  Dateizugriff := WriteRec;
                  IF Schliessen THEN BEGIN
                     {$I-}
                        CLOSE(FDatei);
                        RESET(FDatei);
                        CLOSE(FIdx);
                        RESET(FIdx);
                     {$I+}
                  END;
                  Ergebnis :=  Dateizugriff AND ReadRec(Neu);
               END ELSE
                  Ergebnis := FALSE;
               IF MultipleFileAccess THEN BEGIN
                  Dateizugriff := ChangeMonthDB(StartMonth);
                  Ergebnis := Ergebnis AND Dateizugriff;
               END;
            END;
         END;
         NewRec := Ergebnis;
      END;




   FUNCTION TKBFile.MakeLowMonthKey;
      VAR FirstPart : STRING;
          Ergebnis  : STRING;
      BEGIN
         Ergebnis := '';
         CASE Kennung OF
            1 : Ergebnis := Schluessel;
            2 : Ergebnis := COPY(Month,3,2)+COPY(Month,1,2)+'01';
            3 : Ergebnis := COPY(Month,3,2)+COPY(Month,1,2)+'01'+LeadingZeros(0,3);
         END;
         MakeLowMonthKey := Ergebnis;
      END;

   FUNCTION TKBFile.MakeHighMonthKey;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := '';
         CASE Kennung OF
            1 : Ergebnis := Schluessel;
            2,3 :BEGIN
                    Ergebnis := NextMonth(Month);
                    Ergebnis := '01.'+COPY(Month,1,2)+'.'+COPY(Month,3,2);
                    Ergebnis := PrevDay(Ergebnis);
                    Ergebnis := COPY(Ergebnis,7,2)+COPY(Ergebnis,4,2)+
                                COPY(Ergebnis,1,2);
                    IF (Kennung = 3) THEN
                       Ergebnis := Ergebnis+'999';
                 END;
         END;
         MakeHighMonthKey := Ergebnis;
      END;




  FUNCTION TKBFile.GetBottom;
    VAR DateiZugriff : BOOLEAN;
        Ergebnis : BOOLEAN;

    BEGIN
      Ergebnis := FALSE;
      IF GetTop(Kennung,Daten) THEN BEGIN
         Dateizugriff := ReadRec(0);
         WHILE (Index[Kennung].Right <> -1) OR
               (Index[Kennung].Left <> -1) DO BEGIN
             IF (Index[Kennung].Right <> -1) THEN
               Dateizugriff := ReadRec(Index[Kennung].Right)
             ELSE
               Dateizugriff := ReadRec(Index[Kennung].Left);
         END;
         WHILE (Index[Kennung].Next <> -1) DO
            Dateizugriff := ReadRec(Index[Kennung].Next);
         Daten := Rec;
         Ergebnis := TRUE;
      END ELSE BEGIN
         InitTKBDaten(Daten);
      END;
      GetBottom := Ergebnis;
    END;

  FUNCTION TKBFile.GetTop;
    VAR Dateizugriff : BOOLEAN;

    BEGIN
       Dateizugriff:=ReadRec(0);
       IF Dateizugriff AND (Index[Kennung].Next <> -1 )THEN BEGIN
          GetTop := ReadRec(Index[Kennung].Next);
          Daten := Rec;
       END ELSE BEGIN
          GetTop := FALSE;
          InitTKBDaten(Daten);
       END
    END;

   FUNCTION TKBFile.StartIntervall;
      VAR   Ergebnis : BOOLEAN;

      BEGIN
         ScanIdx := Kennung;
         ScanStartKey := Anfang;
         ScanEndKey := Ende;
         ScanForward := (ScanStartKey <= ScanEndKey);
         ScanDBChange := GETDBFromKbIndex(ScanIdx,ScanStartKey) <>
                         GETDBFromKBIndex(ScanIdx,ScanEndKey);
         Ergebnis :=GetIntTop(Daten);
         StartIntervall := Ergebnis;
      END;

   FUNCTION TKBFile.GetIntTop;
      VAR Ergebnis : BOOLEAN;
          HMonth : STRING;
      BEGIN
         Ergebnis := GetRec(ScanIdx,ScanStartKey,Daten);
         IF (IndexType[ScanIdx] = 2) THEN BEGIN
            IF NOT Ergebnis THEN BEGIN
               IF ScanForward THEN BEGIN
                  IF (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) <
                     GetKB_FKPart(ScanIdx,ScanStartKey)) OR
                    ((GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) =
                     GetKB_FKPart(ScanIdx,ScanStartKey)) AND NOT(TestIntervall))
                     THEN BEGIN
                     Ergebnis := GetNext(ScanIdx,Daten);
                     Ergebnis := TestIntervall;
                  END
               END;
               IF (NOT Ergebnis) AND (NOT MonthDBExists(GetDBFromKBIndex(ScanIdx,ScanStartKey))) AND
                  ScanDBChange THEN BEGIN
                  IF ScanForward THEN BEGIN
                     HMonth := NextMonthDBStr(GetDBFromKBIndex(ScanIdx,ScanStartKey));
                     IF (HMonth <> '') AND (NOT (LaterMonth(Hmonth,
                           GetDBFromKBIndex(Scanidx,ScanEndKey)))) THEN BEGIN
                        IF ChangeMonthDB(HMonth) THEN BEGIN
                           Ergebnis := GetRec(ScanIdx,MakeLowMonthKey(ScanIdx,
                                            ScanStartKey,HMonth),Daten);
                           IF NOT Ergebnis THEN
                              Ergebnis:=GetNext(ScanIdx,Daten);
                        END;
                     END
                  END ELSE BEGIN
                     HMonth := PrevMonthDBStr(GetDBFromKBIndex(ScanIdx,ScanStartKey));
                     IF (HMonth <> '') AND (NOT (EarlierMonth(Hmonth,
                           GetDBFromKBIndex(Scanidx,ScanEndKey)))) THEN BEGIN
                        IF ChangeMonthDB(HMonth) THEN BEGIN
                           Ergebnis := GetRec(ScanIdx,MakeHighMonthKey(ScanIdx,
                                            ScanStartKey,HMonth),Daten);
                        END;
                     END
                  END;
               END;
               Ergebnis := TestIntervall;
               IF (NOT Ergebnis) AND ScanDBChange THEN BEGIN
                  Ergebnis := ScanNextIntervallDB(Daten);
               END;
            END ELSE BEGIN
               Ergebnis := TestIntervall;
            END;
         END ELSE BEGIN
            IF NOT Ergebnis THEN BEGIN
               IF ScanForward THEN
                  Ergebnis:=GetNext(ScanIdx,Daten)
               ELSE
                  Ergebnis:= TRUE;
            END;
            IF Ergebnis THEN
               Ergebnis := TestIntervall;
         END;
         GetIntTop := Ergebnis;
      END;

   FUNCTION TKBFile.GetIntBottom;
      VAR Ergebnis : BOOLEAN;
          HMonth : STRING;
      BEGIN
         Ergebnis := GetRec(ScanIdx,ScanEndKey,Daten);
         IF (IndexType[ScanIdx] = 2) THEN BEGIN
            IF NOT Ergebnis THEN BEGIN
               IF (NOT ScanForward) THEN BEGIN
                  IF (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) <
                     GetKB_FKPart(ScanIdx,ScanEndKey))  OR
                    ((GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) =
                     GetKB_FKPart(ScanIdx,ScanEndKey)) AND NOT(TestIntervall))
                     THEN BEGIN
                     Ergebnis := GetNext(ScanIdx,Daten);
                     Ergebnis := TestIntervall;
                  END
               END;
               IF (NOT Ergebnis) AND (NOT MonthDBExists(GetDBFromKBIndex(ScanIdx,ScanEndKey))) AND
                  ScanDBChange THEN BEGIN
                  IF ScanForward THEN BEGIN
                     HMonth := PrevMonthDBStr(GetDBFromKBIndex(ScanIdx,ScanEndKey));
                     IF (HMonth <> '') AND (NOT (EarlierMonth(Hmonth,
                           GetDBFromKBIndex(Scanidx,ScanStartKey)))) THEN BEGIN
                        IF ChangeMonthDB(HMonth) THEN BEGIN
                           Ergebnis := GetRec(ScanIdx,MakeHighMonthKey(ScanIdx,
                                            ScanEndKey,HMonth),Daten);
                        END;
                     END
                  END ELSE BEGIN
                     HMonth := NextMonthDBStr(GetDBFromKBIndex(ScanIdx,ScanEndKey));
                     IF (HMonth <> '') AND (NOT (LaterMonth(Hmonth,
                           GetDBFromKBIndex(Scanidx,ScanStartKey)))) THEN BEGIN
                        IF ChangeMonthDB(HMonth) THEN BEGIN
                           Ergebnis := GetRec(ScanIdx,MakeLowMonthKey(ScanIdx,
                                            ScanEndKey,HMonth),Daten);
                           IF NOT Ergebnis THEN
                              Ergebnis:=GetNext(ScanIdx,Daten);
                        END;
                     END
                  END;
               END;
               Ergebnis := TestIntervall;
               IF (NOT Ergebnis) AND ScanDBChange THEN BEGIN
                  Ergebnis := ScanPrevIntervallDB(Daten);
               END;
            END ELSE BEGIN
               Ergebnis := TestIntervall;
            END;
         END ELSE BEGIN
            IF NOT Ergebnis THEN BEGIN
               IF (NOT ScanForward) THEN
                  Ergebnis:=GetNext(ScanIdx,Daten)
               ELSE
                  Ergebnis:= TRUE;
            END;
            IF Ergebnis THEN
               Ergebnis := TestIntervall;
         END;
         GetIntBottom := Ergebnis;
      END;

   FUNCTION TKBFile.GetIntNext;
      VAR   Ergebnis: BOOLEAN;

      BEGIN
         Ergebnis :=FALSE;
         IF (ScanIdx <> 0) THEN BEGIN
            IF ScanForward THEN
               Ergebnis := GetNext(ScanIdx,Daten)
            ELSE
               Ergebnis := GetPrev(ScanIdx,Daten);
            IF Ergebnis THEN BEGIN
               Ergebnis := TestIntervall;
               IF NOT Ergebnis AND (IndexType[ScanIdx] = 2) AND
                  ScanDBChange THEN
                  Ergebnis := ScanNextIntervallDB(Daten);
            END;
         END;
         GetIntNext := Ergebnis;
      END;

   FUNCTION TKBFile.GetIntPrev;
      VAR   Ergebnis: BOOLEAN;

      BEGIN
         Ergebnis :=FALSE;
         IF (ScanIdx <> 0) THEN BEGIN
            IF ScanForward THEN
               Ergebnis := GetPrev(ScanIdx,Daten)
            ELSE
               Ergebnis := GetNext(ScanIdx,Daten);
            IF Ergebnis THEN BEGIN
               Ergebnis := TestIntervall;
               IF NOT Ergebnis AND (IndexType[ScanIdx] = 2) AND
                  ScanDBChange THEN
                  Ergebnis := ScanPrevIntervallDB(Daten);
            END;
         END;
         GetIntPrev := Ergebnis;
      END;

   FUNCTION TKBFile.ScanNextIntervallDB;
      VAR Ergebnis : BOOLEAN;
          DBExists : BOOLEAN;
          Month,
          LastMonth : STRING[4];
          Dateizugriff : BOOLEAN;

      BEGIN
        Ergebnis :=FALSE;
        DBExists := TRUE;
        LastMonth := GetDBFromKBIndex(ScanIdx,ScanEndKey);
        WHILE ContinueInNextDB(Daten) AND (NOT Ergebnis) AND DBExists DO BEGIN
           IF ScanForward THEN BEGIN
              Month := NextMonthDBStr(ActualMonth);
              IF (Month <> '') AND EarlierMonth(Month,LastMonth)THEN BEGIN
                 IF ChangeMonthDB(Month) THEN BEGIN
                    IF NOT GetRec(ScanIdx,MakeLowMonthKey(ScanIdx,
                                            ScanStartKey,Month),Daten) THEN
                       Dateizugriff := GetNext(ScanIdx,Daten);
                    Ergebnis := TestIntervall;
                 END ELSE
                   DBExists := FALSE;
              END ELSE BEGIN
                 DbExists := (Month <> '');
              END;
           END ELSE BEGIN
              Month := PrevMonthDBStr(ActualMonth);
              IF (Month <> '') AND LaterMonth(Month,LastMonth)THEN BEGIN
                 IF ChangeMonthDB(Month) THEN BEGIN
                    Dateizugriff:=GetRec(ScanIdx,MakeHighMonthKey(ScanIdx,
                                            ScanStartKey,Month),Daten);
                    Ergebnis := TestIntervall;
                 END ELSE
                    DbExists := FALSE
              END ELSE BEGIN
                 DbExists := (Month <> '');
              END;
           END;
        END;
        ScanNextIntervallDB:= Ergebnis;
      END;

   FUNCTION TKBFile.ScanPrevIntervallDB;
      VAR Ergebnis : BOOLEAN;
          DBExists : BOOLEAN;
          Month,
          FirstMonth : STRING[4];
          Dateizugriff : BOOLEAN;


      BEGIN
        Ergebnis :=FALSE;
        DBExists := TRUE;
        FirstMonth := GetDBFromKBIndex(ScanIdx,ScanStartKey);
        WHILE ContinueInNextDB(Daten) AND (NOT Ergebnis) AND DBExists DO BEGIN
           IF ScanForward THEN BEGIN
              Month := PrevMonthDBStr(ActualMonth);
              IF (Month <> '') AND LaterMonth(Month,FirstMonth)THEN BEGIN
                 IF ChangeMonthDB(Month) THEN BEGIN
                    Dateizugriff := GetRec(ScanIdx,MakeHighMonthKey(ScanIdx,
                                            ScanEndKey,Month),Daten);
                    Ergebnis := TestIntervall;
                 END ELSE
                   DBExists := FALSE;
              END ELSE BEGIN
                 DbExists := (Month <> '');
              END;
           END ELSE BEGIN
              Month := NextMonthDBStr(ActualMonth);
              IF (Month <> '') AND EarlierMonth(Month,FirstMonth)THEN BEGIN
                 IF ChangeMonthDB(Month) THEN BEGIN
                    IF NOT GetRec(ScanIdx,MakeLowMonthKey(ScanIdx,
                                            ScanEndKey,Month),Daten) THEN
                         Dateizugriff := GetNext(ScanIdx,Daten);
                    Ergebnis := TestIntervall;
                 END ELSE
                    DbExists := FALSE
              END ELSE BEGIN
                 DbExists := (Month <> '');
              END;
           END;
        END;
        ScanPrevIntervallDB:= Ergebnis;
      END;

   FUNCTION TKBFile.ContinueInNextDB;
      VAR Ergebnis : BOOLEAN;
          NothingFound : BOOLEAN;
          HData    : TKBDaten;
      BEGIN
        InitTKBDaten(HData);
        NothingFound :=KBGleich(HData,Daten);
        IF NothingFound THEN BEGIN
           IF ScanForward THEN BEGIN
              Ergebnis := EarlierMonth(ActualMonth,
                             GetDBFromKBIndex(ScanIdx,ScanEndKey));
           END ELSE BEGIN
              Ergebnis := LaterMonth(ActualMonth,
                             GetDBFromKBIndex(ScanIdx,ScanEndKey));
           END;
        END ELSE BEGIN
           IF ScanForward THEN BEGIN
              Ergebnis := (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) >
                          GetKB_FKPart(ScanIdx,ScanEndKey)) AND
                          (EarlierMonth(GetDBFromKBIndex(ScanIdx,Index[ScanIdx].Key),
                          GetDBFromKBIndex(ScanIdx,ScanEndKey)))

           END ELSE BEGIN
              Ergebnis := (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) <
                          GetKB_FKPart(ScanIdx,ScanEndKey)) AND
                          (LaterMonth(GetDBFromKBIndex(ScanIdx,Index[ScanIdx].Key),
                           GetDBFromKBIndex(ScanIdx,ScanEndKey)))
           END;
        END;
        ContinueInNextDB := Ergebnis;
      END;

   FUNCTION TKBFile.ContinueInPrevDB;
      VAR Ergebnis : BOOLEAN;
          NothingFound : BOOLEAN;
          HData    : TKBDaten;
      BEGIN
        InitTKBDaten(HData);
        NothingFound :=KBGleich(HData,Daten);
        IF NothingFound THEN BEGIN
           IF ScanForward THEN BEGIN
              Ergebnis := LaterMonth(ActualMonth,
                             GetDBFromKBIndex(ScanIdx,ScanStartKey));
           END ELSE BEGIN
              Ergebnis := EarlierMonth(ActualMonth,
                             GetDBFromKBIndex(ScanIdx,ScanStartKey));
           END;
        END ELSE BEGIN
           IF ScanForward THEN BEGIN
              Ergebnis := (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) <
                          GetKB_FKPart(ScanIdx,ScanStartKey)) AND
                          (LaterMonth(GetDBFromKBIndex(ScanIdx,Index[ScanIdx].Key),
                          GetDBFromKBIndex(ScanIdx,ScanStartKey)))

           END ELSE BEGIN
              Ergebnis := (GetKB_FKPart(ScanIdx,Index[ScanIdx].Key) >
                          GetKB_FKPart(ScanIdx,ScanStartKey)) AND
                          (EarlierMonth(GetDBFromKBIndex(ScanIdx,Index[ScanIdx].Key),
                           GetDBFromKBIndex(ScanIdx,ScanStartKey)))
           END;
        END;
        ContinueInPrevDB := Ergebnis;
      END;

   FUNCTION TKBFile.TestIntervall;
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


  FUNCTION TKBFile.GetNext;
    Var Ergebnis : BOOLEAN;
        ActKey   : STRING;

    BEGIN
      Ergebnis := FALSE;
      IF (Index[Kennung].Next <> -1) THEN BEGIN
         Ergebnis:=ReadRec(Index[Kennung].Next);
         Daten := Rec;
      END ELSE BEGIN
         IF MultipleFileAccess THEN BEGIN
            CASE IndexType[Kennung] OF
               0 : BEGIN
                      Ergebnis := FALSE;
                      InitTKBDaten(Daten);
                   END;
               1 : BEGIN
                     IF NextMonthDB THEN
                        Ergebnis := GetTop(Kennung,Daten);
                   END;
               2 : BEGIN
                      Ergebnis := FALSE;
                      InitTKBDaten(Daten);
                   END;
            END;
         END
      END;
      GetNext := Ergebnis;
    END;

  FUNCTION TKBFile.GetPrev;
    Var Ergebnis : BOOLEAN;

    BEGIN
      Ergebnis := FALSE;
      IF (Index[Kennung].Prev <> 0) AND (Index[Kennung].Prev <> -1) THEN BEGIN
        Ergebnis:=ReadRec(Index[Kennung].Prev);
        Daten := Rec;
      END ELSE BEGIN
         IF MultipleFileAccess THEN BEGIN
            CASE IndexType[Kennung] OF
               0   : BEGIN
                        Ergebnis := FALSE;
                        InitTKBDaten(Daten);
                     END;
               1   : BEGIN
                     IF PrevMonthDB THEN
                        Ergebnis := GetBottom(Kennung,Daten);
                     END;
               2   : BEGIN
                        Ergebnis := FALSE;
                        InitTKBDaten(Daten);
                     END;
            END;
         END
      END;
      GetPrev := Ergebnis;
    END;

   FUNCTION TKBFile.GetRec;
      VAR Ergebnis : BOOLEAN;
          D        : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         InitTKBDaten(Daten);
         IF NOT MultipleFileAccess THEN BEGIN
            IF SeekRec(Kennung,Schluessel) THEN BEGIN
               Ergebnis := TRUE;
            END;
            Daten:= Rec;
         END ELSE BEGIN
            IF SeekRecMultiple(Kennung,Schluessel) THEN BEGIN
               Ergebnis := TRUE;
               Daten := Rec;
            END ELSE BEGIN
               IF (Index[Kennung].NodeNo = 0) THEN BEGIN
                  d:= GetPrev(Kennung,Daten);
               END ELSE
                  Daten := Rec;
            END;
         END;
         GetRec := Ergebnis;
      END;

   FUNCTION TKBFile.ChangeRec;
      VAR HindexString :STRING;
          SaveIdx      :TKBIndex;
          NewIdx       :TKBIndex;
          RecordFound,
          Ergebnis     : BOOLEAN;
          Lauf         : BYTE;
          SaveMonth    : STRING[4];
          NewMonth     : STRING[4];
          Dateizugriff : BOOLEAN;
          IndexChanged : BOOLEAN;

      BEGIN
         RecordFound := FALSE;
         Ergebnis :=FALSE;
         SaveMonth := ActualMonth;
         IF MultipleFileAccess THEN BEGIN
            NewMonth :=COPY(Daten.TagesDatum,4,2)+COPY(Daten.TagesDatum,7,2);
            IF MonthDBExists(NewMonth) THEN
               Dateizugriff:= ChangeMonthDB(NewMonth);
            RecordFound := SeekRecMultiple(1,CreateKBIndex(1,Daten));

         END ELSE
            RecordFound := SeekRec(1,CreateKBIndex(1,Daten));
         IF RecordFound THEN BEGIN
            IndexChanged:= FALSE;
            FOR Lauf := 1 TO KBIndexNo DO BEGIN
              IndexChanged := IndexChanged OR
                  (CreateKBIndex(Lauf,Daten) <> Index[Lauf].Key);
            END;


            IF IndexChanged THEN BEGIN
               IF MultipleFileAccess AND (ActualMonth <> NewMonth) THEN BEGIN
                  Dateizugriff := DelRec(CreateKBIndex(1,Daten));
                  Ergebnis := NewRec(Daten) AND Dateizugriff;
               END ELSE BEGIN
                  NewIdx := Index;
                  SaveIdx := Index;
                  FOR Lauf := 1 TO KBIndexNo DO BEGIN
                      HIndexString := CreateKbIndex(Lauf,Daten);
                      IndexNo := Lauf;
                      IF (SaveIdx[IndexNo].Key <> HIndexString) THEN BEGIN
                         Delete(SaveIdx[IndexNo]);
                         CreateNode(HIndexString,SaveIdx[IndexNo].NodeNo,SaveIdx[IndexNo].NodeNo,
                             NewIdx[IndexNo]);
                         Insert(NewIdx[IndexNo]);
                         GetNode(NewIdx[IndexNo].NodeNo,NewIdx[IndexNo]);
                      END;
                  END;
                  Index := NewIdx;
                  Rec := Daten;
                  Ergebnis := WriteRec;
               END;
            END ELSE BEGIN
               Rec := Daten;
               Ergebnis := WriteRec;
            END;
         END;
         ChangeRec:=Ergebnis;
      END;

   FUNCTION TKBFile.SeekRec;
      VAR Pos          : LONGINT;
          Dateizugriff : BOOLEAN;
          Ergebnis     : BOOLEAN;

      BEGIN
         Ergebnis := FALSE;
         Schluessel := UPPER(Schluessel);
         IndexNo := Kennung;
         IF IsOpen THEN BEGIN
            Ergebnis := Search(Schluessel,Index[IndexNo]);
            IF (NOT Ergebnis) AND (Schluessel < Index[IndexNo].Key) AND
               (Index[IndexNo].Prev <> -1) THEN BEGIN
               Dateizugriff := ReadRec(Index[IndexNo].Prev)
            END ELSE BEGIN
               Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
            END;
         END;
         SeekRec := Ergebnis;
      END;

   FUNCTION TKBFile.SeekRecMultiple;
      VAR Pos          : LONGINT;
          Dateizugriff : BOOLEAN;
          Ergebnis     : BOOLEAN;
          SaveMonth    : STRING[4];

          SearchFMonth : STRING[4];
          SearchPMonth : STRING[4];
          SearchLauf   : BYTE;


      BEGIN
         Ergebnis := FALSE;
         Schluessel := UPPER(Schluessel);
         IndexNo := Kennung;
         IF (IndexType[Kennung]> 0) THEN BEGIN
            Ergebnis := ChangeMonthDB(GetDBFromKBIndex(IndexNo,Schluessel));
            IF IsOpen AND Ergebnis THEN BEGIN
               Ergebnis := Search(Schluessel,Index[IndexNo]);
               IF (NOT Ergebnis) AND (Schluessel < Index[IndexNo].Key) AND
                 (Index[IndexNo].Prev <> -1) THEN
                  Dateizugriff := ReadRec(Index[IndexNo].Prev)
               ELSE
                 Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
            END;
         END ELSE BEGIN
            IF IsOpen THEN BEGIN
               Ergebnis := Search(Schluessel,Index[IndexNo]);
               IF (NOT Ergebnis) AND (Schluessel < Index[IndexNo].Key) THEN
                  Dateizugriff := ReadRec(Index[IndexNo].Prev)
               ELSE
                  Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
            END;
            IF NOT Ergebnis THEN BEGIN
               SaveMonth := ActualMonth;
               SearchFMonth := NextMonth(ActualMonth);
               SearchPMonth := PrevMonth(ActualMonth);
               SearchLauf := KBMaxSearchMonth;
               WHILE (SearchLauf > 0) AND NOT Ergebnis DO BEGIN
                  IF ChangeMonthDB(SearchFMonth) THEN BEGIN
                      Ergebnis := Search(Schluessel,Index[IndexNo]);
                      IF Ergebnis THEN
                         Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
                  END;
                  IF (NOT Ergebnis) THEN BEGIN
                     IF ChangeMonthDB(SearchPMonth) THEN BEGIN
                        Ergebnis := Search(Schluessel,Index[IndexNo]);
                        IF Ergebnis THEN
                           Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
                     END;
                  END;
                  IF NOT Ergebnis THEN BEGIN
                     SearchFMonth := NextMonth(SearchFMonth);
                     SearchPMonth := PrevMonth(SearchPMonth);
                  END;
                  DEC(SearchLauf);
               END;
               IF NOT Ergebnis THEN BEGIN
                  IF ChangeMonthDB(SaveMonth) THEN BEGIN
                     Ergebnis := Search(Schluessel,Index[IndexNo]);
                     IF (NOT Ergebnis) AND (Schluessel < Index[IndexNo].Key) THEN
                        Dateizugriff := ReadRec(Index[IndexNo].Prev)
                     ELSE
                        Dateizugriff := ReadRec(Index[IndexNo].NodeNo);
                  END;
               END;
            END;
         END;
         SeekRecMultiple := Ergebnis;
      END;

   FUNCTION TKBFile.DelRec;

      VAR DelNum       : LONGINT;
          Hilfe        : INTEGER;
          DelIdx       : TKBIndex;
          Dateizugriff : BOOLEAN;
          Lauf         : BYTE;
          Ergebnis     : BOOLEAN;
          RecordFound  : BOOLEAN;
          SaveMonth    : STRING[4];

      BEGIN
         Ergebnis := FALSE;
         RecordFound := FALSE;
         SaveMonth := ActualMonth;
         IF MultipleFileAccess THEN BEGIN
            RecordFound := SeekRecMultiple(1,Schluessel);
         END ELSE
            RecordFound := SeekRec(1,Schluessel);
         IF RecordFound THEN BEGIN
            DelIdx := Index;
            DelNum := Index[1].NodeNo;
            Dateizugriff := TRUE;
            FOR Lauf := 1 TO KBIndexNo DO BEGIN
               IndexNo := Lauf;
               Dateizugriff := Delete(DelIdx[Lauf]);
            END;
            IF (DelNum = FILESIZE(FDatei) - 1) THEN BEGIN
               {$I-}
                  SEEK(FDatei,DelNum);
                  TRUNCATE(FDatei);
                  SEEK(FIdx,DelNum);
                  TRUNCATE(FIdx);
                  SEEK(FDatei,0);
               {$I+}
               Hilfe := IORESULT;
            END ELSE BEGIN
               {$I-}
                  SEEK(FRecFrei,FILESIZE(FRecFrei));
                  WRITE(FRecFrei,DelNum);
                  SEEK(FDatei,0);
               {$I+}
               Hilfe := IORESULT;
            END;
            Ergebnis:=Dateizugriff;
         END;
         IF MultipleFileAccess THEN
            Dateizugriff := ChangeMonthDB(SaveMonth);
         DelRec:=Ergebnis;
      END;

   FUNCTION TKBFile.CloseFile;

      BEGIN
         CloseFile := (IORESULT  = 0);
         CloseFile := FALSE;
         IF IsOpen THEN BEGIN
            {$I-}
               Close(FDatei);
               Close(FRecFrei);
               Close(FIdx);
               IF MultipleFileAccess THEN
                  CLOSE(FLH);
            {$I+}
            IsOpen := NOT(IORESULT = 0);
         END;
         CloseFile := NOT IsOpen;
      END;

   PROCEDURE TKBFile.CreateNode;

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

   PROCEDURE TKBFile.GetNode;

      BEGIN
         {$I-}
            SEEK(FIdx,NodeNo);
            READ(FIdx,Index);
            SEEK(FIdx,NodeNo);
         {$I+}
         Node := Index[IndexNo];
      END;

   FUNCTION TKBFile.GetRoot;

      BEGIN
         GetNode(0,Node);
         IF Node.Right <> -1 THEN BEGIN
            GetNode(Node.Right,Node);
            GetRoot := TRUE;
         END ELSE
           GetRoot := FALSE;
      END;

   PROCEDURE TKBFile.ChangeNode;

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


   FUNCTION TKBFile.Search;
      VAR AktNode       : KBAVL_Node;
          HelpNode      : KBAVL_Node;
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

   FUNCTION TKBFile.Delete; {******* Lîschen eines Knotens ********}
      VAR RootNode,
          AktNode,
          HNode1,
          HNode2    : KBAVL_Node;
          OldWeight : INTEGER;
          NodeNo    : LONGINT;
          NewNodeNo : LONGINT;
          Ergebnis  : BOOLEAN;
          OldNodeNo : LONGINT;
          WhichSon  : INTEGER;
          Ende      : BOOLEAN;

      { *********** Lîschen eines Knotens aus dem binÑren Baum *********** }

      FUNCTION DeleteBin(AktNode:KBAVL_Node;VAR FatherNode:KBAVL_Node):INTEGER;

         VAR MinNode  : KBAVL_Node;
             HelpNode : KBAVL_Node;
             Ende : BOOLEAN;
             NewNodeNo   : LONGINT;
             OldNodeNo    : LONGINT;
             OldFatherNo : LONGINT;

         FUNCTION DeleteMin(RootNode:KBAVL_Node;VAR ResultNode:KBAVL_Node):INTEGER;

            { Lîscht kleinstes Element des rechten Teilbaums eines
              Knotens und gibt den Knoten als RESULTNODE zurÅck }

            VAR FatherNode,
                AktNode : KBAVL_Node;

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
                  DeleteBin := -1;
                  FatherNode.Right := NewNodeNo;
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


   PROCEDURE TKBFile.Insert;
      VAR FirstNode,
          AktNode,
          HNode1,
          HNode2   : KBAVL_Node;
          AktNodeNo   : LONGINT;
          NewNodeNo: LONGINT;
          Ergebnis : BOOLEAN;
          Ende : BOOLEAN;
          D    : BOOLEAN;

      PROCEDURE InsertBin(FirstNode:KBAVL_Node;VAR NewNode:KBAVL_Node);
         VAR AktNode    : KBAVL_Node;
             Ende       : BOOLEAN;
             NextNodeNo : LONGINT;
             NewNodeNo  : LONGINT;
             HelpNode   : KBAVL_Node;

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

   PROCEDURE TKBFile.LeftRotate;
      VAR SonNo,
          FatherNo   : LONGINT;
          AktNode,
          RightNode,
          LeftNode,
          FatherNode :KBAVL_Node;

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

   PROCEDURE TKBFile.RightRotate;
      VAR SonNo,
          FatherNo   : LONGINT;
          AktNode,
          LeftNode,
          RightNode,
          FatherNode :KBAVL_Node;

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
