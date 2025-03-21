{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Nov 26 09:17:24 GMT+01:00 1994
 Dateihistorie am Ende der Datei
 ============================}
UNIT FStruct;
   {$N+}
INTERFACE
   
   TYPE  TIdentifier = STRING[15]; 
         
   TYPE  TFieldStructPtr =^TFieldStruct; 
         TFreeDataStructPtr = ^TFreeDataStruct; 
         TFieldStruct = RECORD
                           NextField   : TFieldStructPtr; 
                           Identifier  : TIdentifier; 
                           FieldOffset : WORD; 
                           FieldType,
                           ElemType      : BYTE; 
                           ElemSize,
                           ElemNo        : WORD; 
                           ElemStructPtr : TFreeDataStructPtr;
                           Len           : WORD;
                        END;
         TSeekFieldStructPtr = ^TSeekFieldStruct;
         TSeekFieldStruct = RECORD
                               NextField  : TSeekFieldStructPtr;
                               SplitChar  : CHAR;
                               Identifier : TIdentifier;
                               RealName   : TIdentifier;
                               Alias      : BOOLEAN;
                               IdentParam : STRING;
                               FieldPtr   : TFieldStructPtr;
                            END; 
         TFieldNames  = ARRAY [65..90] OF TSeekFieldStructPtr;
         TStructPtr   = ^TStruct; 
         TStruct      = RECORD
                           Complete   : BOOLEAN;
                           StructName : TIdentifier;
                           FieldOffSet,
                           Len        : WORD; 
                           FieldList  : TFieldStructPtr;
                           NextStruct : TStructPtr; 
                           FieldNames : TFieldNames;
                        END;
         TFreeDataStruct = OBJECT
                              CONSTRUCTOR Init;
                              PROCEDURE SetFields(FirstField:TFieldStructPtr; NewFieldNames:TFieldNames); VIRTUAL;
                              PROCEDURE SetDataSpace(Data:POINTER); VIRTUAL;
                              FUNCTION  GetDataSpacePtr:POINTER; VIRTUAL;
                              FUNCTION  GetFieldPtr(Ident:STRING):POINTER; VIRTUAL;
                              FUNCTION  GetField(Ident:STRING; VAR Wert):BOOLEAN; VIRTUAL;
                              FUNCTION  GetFieldCopy(Ident:STRING; Wert:POINTER):BOOLEAN; VIRTUAL;
                              FUNCTION  GetFieldInfo(Ident:STRING; VAR Field:TFieldStruct):BOOLEAN; VIRTUAL;
                              FUNCTION  GetIdxStr(Ident:STRING; Attrib:STRING):STRING;
                              FUNCTION  GetFieldStr(Ident:STRING; Attrib:STRING):STRING;
                              FUNCTION  SetField(Ident:STRING; VAR Wert):BOOLEAN; VIRTUAL;
                              FUNCTION  SetFieldStr(Ident:STRING;Buffer:STRING):BOOLEAN;VIRTUAL;
                              FUNCTION  SetFieldCopy(Ident:STRING; Wert:POINTER):BOOLEAN; VIRTUAL;
                              FUNCTION  IsComplete:BOOLEAN; VIRTUAL;
                              FUNCTION  FieldExists(Ident:STRING):BOOLEAN; 
                              FUNCTION  DataSize:WORD; VIRTUAL; 
                              PROCEDURE ResetData; VIRTUAL;
                              FUNCTION  SetFieldStructPtr(Ident:STRING;
                                                             NewDataStruct:TFreeDataStructPtr):BOOLEAN; VIRTUAL; 
                              PROCEDURE AppendField(Field:TFieldStruct); VIRTUAL; 
                              FUNCTION  GetErrNum:BYTE; VIRTUAL; 
                              DESTRUCTOR Done; 
                                 
                                 PRIVATE
                                 Status    : BYTE;
                                 DataPtr   : POINTER;
                                 FieldList : TFieldStructPtr; 
                                 Size      : WORD; 
                                 Complete  : BOOLEAN; 
                                 FieldNames: TFieldNames; 
                              FUNCTION   NewField(Field:TFieldStruct):BOOLEAN; 
                              FUNCTION   SeekField(Ident:STRING; VAR Field:TFieldStructPtr;
                                                   VAR SplitChar:CHAR; VAR IdentParam:STRING):BOOLEAN;
                              PROCEDURE  DisposeFields;
                           END;
         
   TYPE  TSetupObjPtr = ^TSetupObj; 
         TSetupObj = OBJECT
                        CONSTRUCTOR Init; 
                        PROCEDURE GetSetup(FileName:STRING); 
                        PROCEDURE SetDataStruct(SName:STRING; VAR Data:TFreeDataStruct); VIRTUAL; 
                        FUNCTION GetField(SName,FName:STRING; VAR Field:TFieldStruct):BOOLEAN; VIRTUAL;
                        FUNCTION GetStruct(SName:STRING; VAR Struct:TStruct):BOOLEAN; VIRTUAL;
                        FUNCTION AppendField(SName:STRING; Field : TFieldStruct):BOOLEAN; VIRTUAL; 
                        FUNCTION GetErrNum:BYTE; VIRTUAL; 
                        DESTRUCTOR Done; VIRTUAL; 
                           PRIVATE
                           Status         : BYTE; 
                           SetupFileName,
                           LogFileName    : STRING;
                           SetupFile,
                           LogFile        : TEXT; 
                           ParseString    : STRING; 
                           Comment        : BOOLEAN; 
                           OffSet         : WORD; 
                           ActStruct      : TStruct; 
                           StructList     : TStructPtr; 
                           LineCount      : WORD;

                        PROCEDURE ResetActStruct; VIRTUAL;
                        PROCEDURE OpenFile; VIRTUAL;
                        FUNCTION  ParseLine:BYTE; VIRTUAL; 
                        FUNCTION  ReadFile:BYTE; VIRTUAL; 
                        PROCEDURE CloseFile; VIRTUAL;
                        FUNCTION  ParseStructBegin(ParamString:STRING):BOOLEAN; VIRTUAL; 
                        FUNCTION  ParseStructEnd(ParamString:STRING):BOOLEAN; VIRTUAL;
                        FUNCTION  ParseStructField(ParamString:STRING):BOOLEAN; VIRTUAL; 
                        FUNCTION  ParseAlias(ParamString:STRING):BOOLEAN; VIRTUAL;
                        FUNCTION  StructExists(SName:STRING; VAR ResPtr:TStructPtr):BOOLEAN; VIRTUAL; 
                        FUNCTION  FieldExists(FName:STRING; Struct:TStruct; 
                                                 VAR ResPtr:TFieldStructPtr; VAR Alias:BOOLEAN):BOOLEAN; VIRTUAL; 
                        FUNCTION  ValidID(Identifier:STRING):BOOLEAN; 
                     END; 

   PROCEDURE MakeStructField(FName:STRING; Struct:TStruct; VAR Field:TFieldStruct); 
      
IMPLEMENTATION
   
   USES  ZUSAETZE;
   CONST MaxParseKey = 4; 
   CONST KeyWords    : ARRAY[1..MaxParseKey] OF STRING = ('%STRUCT','%ENDSTRUCT',
         '%FDEF','%ALIAS'); 
         
         
   CONST MaxTypeNum = 18;
   CONST TypeStrings : ARRAY[1..MaxTypeNum] OF STRING = ('BOOLEAN' ,'CHAR',
         'BYTE'    ,'WORD',
         'SHORTINT','INTEGER',
         'LONGINT' ,'SINGLE',
         'REAL'    ,'DOUBLE',
         'EXTENDED','COMP',
         'STRING'  ,'ARRAY',
         'STRUCT','DATE','TIME','KW'); 
         
   CONST TypeLen : ARRAY[1..18] OF WORD = (1,1,1,2,1,2,4,4,6,8,10,8,0,0,0,9,6,6); 
         
   FUNCTION InsertIntoFieldNames(Name:STRING;SplitChar:CHAR;IdentParam,RealName:STRING;
                                 Alias:BOOLEAN; FieldPtr:TFieldStructPtr;
                                 VAR Daten:TFieldNames):BOOLEAN;
      VAR   LsPtr,
            NsPtr : TSeekFieldStructPtr;

      BEGIN
         InsertIntoFieldNames := FALSE;
         IF (MAXAVAIL > SIZEOF(TSeekFieldStruct)) THEN BEGIN
            InsertIntoFieldNames := TRUE;
            NEW(NsPtr);
            NsPtr^.Identifier := Name;
            NsPtr^.RealName   := RealName;
            NsPtr^.IdentParam := IdentParam;
            NSPtr^.SplitChar := SplitChar;
            NsPtr^.Alias := Alias;
            NsPtr^.FieldPtr := FieldPtr;
            LsPtr := Daten[ORD(Name[1])];
            NsPtr^.NextField := NIL;
            IF (LsPtr = NIL) THEN
               Daten[ORD(Name[1])] := NsPtr
            ELSE BEGIN
               IF (Name < LSPtr^.Identifier) THEN BEGIN
                  Daten[ORD(Name[1])] := NsPtr; 
                  NsPtr^.NextField := LsPtr; 
               END 
               ELSE BEGIN
                  WHILE (LsPtr^.NextField <> NIL)
                        AND (LsPtr^.NextField^.Identifier < Name) 
                  DO
                     LsPtr := LsPtr^.NextField; 
                  IF (LsPtr^.NextField <> NIL) AND
                     (LsPtr^.NextField^.Identifier = Name) 
                  THEN BEGIN
                     DISPOSE(NsPtr); 
                     InsertIntoFieldNames := FALSE
                  END 
                  ELSE BEGIN
                     NsPtr^.NextField:= LSPtr^.NextField; 
                     LsPtr^.NextField := NSPtr; 
                  END; 
               END; 
            END; 
         END; 
      END; 
   
   {*************************************************************************
   *
   * Initialisierung
   *
   *************************************************************************}
   
   CONSTRUCTOR TFreeDataStruct.Init; 
      VAR   Lauf : BYTE; 
      BEGIN
         FieldList:= NIL; 
         DataPtr  := NIL; 
         Size := 0; 
         Status := 0; 
         Complete := TRUE; 
         FOR Lauf := 65 TO 90 DO
            FieldNames[Lauf] := NIL; 
      END; 
   
   {*************************************************************************
   *
   * Liefert den aufgetretenen Fehler
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetErrNum; 
      
      BEGIN
         GetErrNum := Status; 
      END; 
   
   
   {*************************************************************************
   *
   * Gibt den ben”tigten Speicherplatz an
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.DataSize; 
      BEGIN
         DataSize := Size; 
      END; 
   
   PROCEDURE TFreeDataStruct.SetFields;

      VAR   LPtr    : TFieldStructPtr;
            Field   : TFieldStruct;
            Continue: BOOLEAN;
            Garbage : STRING;
            SplitChar : CHAR;
            LSPtr   : TSeekFieldStructPtr;
            Lauf    : BYTE;
      BEGIN
         IF (FieldList <> NIL) THEN
            DisposeFields;
         LPtr := FirstField;
         Continue := TRUE;
         WHILE (Continue) AND (Lptr <> NIL) DO BEGIN
            Continue := NewField(LPtr^);
            LPtr:=LPtr^.NextField;
         END;
         FOR Lauf := 65 TO 90 DO BEGIN
            LsPtr:= NewFieldNames[Lauf];
            WHILE (LsPtr <> NIL) AND Continue DO BEGIN
               IF LsPtr^.Alias THEN BEGIN
                  SplitChar := #0;
                  SeekField(LsPtr^.RealName,LPtr,SplitChar,Garbage);
                  Continue :=InsertIntoFieldNames(LsPtr^.Identifier,LsPtr^.SplitChar,LsPtr^.IdentParam,LsPtr^.RealName,
                  LsPtr^.Alias,LPtr,FieldNames);
               END;
               LsPtr:=LsPtr^.NextField;
            END;
         END;
         IF NOT Continue THEN
            DisposeFields; 
      END; 
   
   PROCEDURE TFreeDataStruct.DisposeFields; 
      VAR   Result : BOOLEAN; 
            LPtr,
            DPtr   : TFieldStructPtr; 
            LSPtr,
            DSPtr  : TSeekFieldStructPtr;
            Lauf   : BYTE; 
      BEGIN
         LPtr := FieldList; 
         WHILE (LPtr <> NIL) DO BEGIN
            DPtr := LPtr; 
            LPtr := LPtr^.NextField; 
            DISPOSE (DPtr); 
         END; 
         FieldList := NIL; 
         FOR Lauf := 65 TO 90 DO BEGIN
            LsPtr := Fieldnames[Lauf]; 
            WHILE (LsPtr <> NIL) DO BEGIN
               DsPtr := LsPtr; 
               LsPtr := LsPtr^.NextField; 
               DISPOSE (DsPtr); 
            END; 
            FieldNames[Lauf] := NIL; 
         END; 
         Complete := TRUE; 
         Size := 0; 
      END; 
   
   PROCEDURE TFreeDataStruct.AppendField; 
      VAR   Result : BOOLEAN; 
      BEGIN
         IF Complete THEN BEGIN
            Field.FieldOffSet := Size; 
            Result := NewField(Field); 
            IF NOT Result THEN
               DisposeFields; 
         END; 
      END; 
   
   FUNCTION TFreeDataStruct.NewField; 
      
      VAR   Result   : BOOLEAN; 
            LPtr,
            NPtr     : TFieldStructPtr; 
            
      BEGIN
         Result := TRUE; 
         IF (MAXAVAIL > (SIZEOF(TFieldStruct)+SIZEOF(TSeekFieldStruct))) THEN BEGIN
            IF (FieldList = NIL) THEN BEGIN
               NEW(NPtr); 
               NPtr^ := Field; 
               NPtr^.NextField := NIL; 
               FieldList := NPtr; 
               Size := Size + Field.Len; 
               InsertIntoFieldNames(Field.Identifier,#0,'','',FALSE,Nptr,FieldNames);
            END
            ELSE BEGIN
               LPtr := Fieldlist; 
               IF (LPtr^.Identifier < Field.Identifier) THEN BEGIN
                  WHILE (LPtr^.NextField <> NIL) AND
                        (LPtr^.NextField^.Identifier <Field.Identifier)
                  DO
                     LPtr := LPtr^.NextField; 
                  IF (LPtr^.NextField <> NIL) AND
                     (LPtr^.NextField^.Identifier = Field.Identifier)
                  THEN BEGIN
                     Result := FALSE; 
                     Status := 2; 
                  END
                  ELSE BEGIN
                     NEW(NPtr); 
                     NPtr^ := Field; 
                     NPTr^.NextField := LPtr^.NextField; 
                     LPtr^.NextField := NPtr; 
                     Size := Size + Field.Len; 
                     InsertIntoFieldNames(Field.Identifier,#0,'','',FALSE,NPtr,FieldNames);
                  END; 
               END 
               ELSE BEGIN
                  NEW(NPtr); 
                  NPtr^ := Field; 
                  NPTr^.NextField := LPtr; 
                  FieldList := NPtr; 
                  Size := Size + Field.Len; 
                  InsertIntoFieldNames(Field.Identifier,#0,'','',FALSE,NPtr,FieldNames);
               END; 
            END
         END
         ELSE BEGIN
            Status := 1; 
            Result := FALSE; 
            Complete := FALSE; 
         END; 
         NewField := RESULT; 
      END; 
   
   {*************************************************************************
   *
   * Zeigt vollst„ndigen Parsevorgang an
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.IsComplete; 
      BEGIN
         IsComplete := Complete; 
      END; 
   
   {*************************************************************************
   *
   * Setzt den Daten Puffer
   *
   *************************************************************************}
   
   PROCEDURE TFreeDataStruct.SetDataSpace; 
      BEGIN
         DataPtr := Data; 
      END; 
   
   {*************************************************************************
   *
   * Liefert Zeiger auf den Datenpuffer
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetDataSpacePtr; 
      BEGIN
         GetDataSpacePtr :=DataPtr; 
      END; 
   
   {*************************************************************************
   *
   *  Zeiger auf ein Feld holen (Auch Arrayelemente)
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetFieldPtr; 
      VAR   ActField : TFieldStructPtr; 
            ActPtr,
            SvePtr   : POINTER; 
            ActSeg,
            ActOfs   : WORD; 
            ArrayElem :WORD; 
            GetFromStruct : BOOLEAN; 
            SplitChar : CHAR; 
            IdentParam,
            IdentParam2:STRING; 
      BEGIN
         GetFieldPtr := NIL; 
         GetFromStruct := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs:= ActOfs + ActField^.FieldOffSet; 
            IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
               GetFromStruct := TRUE
            ELSE IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
               SplitString(IdentParam,']',IdentParam,IdentParam2); 
               ArrayElem := S2L(IdentParam); 
               ActOfs := ActOfs + ActField^.ElemSize*(ArrayElem-1); 
               IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                  DELETE(IdentParam2,1,1); 
                  IdentParam := IdentParam2; 
                  GetFromStruct := TRUE; 
               END; 
            END; 
            ActPtr := PTR(ActSeg,ActOfs); 
            IF GetFromStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr; 
                  ActField^.ElemStructPtr^.SetDataSpace(ActPtr); 
                  GetFieldPtr := ActField^.ElemStructPtr^.GetFieldPtr(IdentParam); 
                  ActField^.ElemStructPtr^.SetDataSpace(SvePtr); 
               END; 
            END
            ELSE
               GetFieldPtr := ActPtr; 
         END
      END; 
   
   {*************************************************************************
   *
   * Inhalt eines Feldes auslesen (Auch Array und Struct Elemente)
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetField; 
      VAR   ActField : TFieldStructPtr; 
            ActSeg,
            ActOfs   : WORD; 
            ActPtr   : POINTER; 
            SvePtr   : POINTER; 
            GetFromStruct : BOOLEAN; 
            CPLen,
            ArrayElem: WORD; 
            WertPtr  : POINTER; 
            SplitChar : CHAR; 
            IdentParam,
            IdentParam2 :STRING; 
      BEGIN
         GetField:=FALSE; 
         GetFromStruct := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs:= ActOfs + ActField^.FieldOffSet; 
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2); 
                  ArrayElem := S2L(IdentParam); 
                  CPLen := ActField^.ElemSize; 
                  ActOfs := ActOfs + CPLen*(ArrayElem-1); 
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     DELETE(IdentParam2,1,1);
                     IdentParam := IdentParam2;
                     GetFromStruct := TRUE; 
                  END; 
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     GetFromStruct := TRUE; 
            ActPtr := PTR(ActSeg,ActOfs);
            IF GetFromStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr;
                  ActField^.ElemStructPtr^.SetDataSpace(ActPtr);
                  GetField := ActField^.ElemStructPtr^.GetField(IdentParam,Wert);
                  ActField^.ElemStructPtr^.SetDataSpace(SvePtr);
               END
            END
            ELSE BEGIN
               WertPtr := @Wert; 
               MemCopy(ActPtr,WertPtr,CPLen); 
               GetField := TRUE; 
            END; 
         END
      END; 
   
   {*************************************************************************
   *
   * Liefert die Informationen zu einem Feld
   *
   *************************************************************************}

   FUNCTION TFreeDataStruct.GetFieldInfo;
      VAR   SplitChar : CHAR;
            IdentParam,
            IdentParam2:STRING;
            GetFromStruct : BOOLEAN;
            ActField : TFieldStructPtr;

      BEGIN
         GetFieldInfo := FALSE;
         GetFromStruct := FALSE;
         Ident := UPPER(Ident);
         SplitIdent(Ident,SplitChar,Ident,IdentParam);
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            Field := ActField^;
            IF (SplitChar = '[') AND (Field.FieldType = 14) THEN BEGIN
               IF (IdentParam[LENGTH(IdentParam)] = ']') THEN BEGIN
                  Field.FieldType:=Field.ElemType;
               END ELSE BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2);
                  IF (IdentParam2[1] = '.') AND (Field.ElemType = 15)  THEN BEGIN
                     DELETE(IdentParam2,1,1);
                     IdentParam := IdentParam2;
                     GetFromStruct := TRUE;
                  END;
               END;
            END
            ELSE
               IF (SplitChar = '.') AND (Field.FieldType = 15) THEN
                  GetFromStruct := TRUE; 
            
            IF GetFromStruct THEN BEGIN
               IF (Field.ElemStructPtr <> NIL) THEN BEGIN
                  GetFieldInfo:=Field.ElemStructPtr^.GetFieldInfo(IdentParam,Field); 
               END
            END
            ELSE
               GetFieldInfo:=TRUE; 
         END
      END; 
   
   {*************************************************************************
   *
   * Kopiert das angegebene Feld
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetFieldCopy; 
      VAR   ActField : TFieldStructPtr; 
            ActSeg,
            ActOfs   : WORD; 
            ActPtr,
            SvePtr   : POINTER; 
            CPLen,
            ArrayElem: WORD; 
            GetFromStruct : BOOLEAN; 
            SplitChar : CHAR; 
            IdentParam,
            IdentParam2:STRING; 
      BEGIN
         GetFromStruct:=FALSE; 
         GetFieldCopy := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs:= ActOfs + ActField^.FieldOffSet; 
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2); 
                  ArrayElem := S2L(IdentParam); 
                  CPLen := ActField^.ElemSize; 
                  ActOfs := ActOfs + CPLen*(ArrayElem-1); 
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     DELETE(IdentPAram2,1,1);
                     IdentParam := IdentParam2;
                     GetFromStruct := TRUE; 
                  END; 
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     GetFromStruct := TRUE; 
            ActPtr := PTR(ActSeg,ActOfs); 
            IF GetFromStruct THEN BEGIN
               SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr; 
               ActField^.ElemStructPtr^.SetDataSpace(ActPtr); 
               GetFieldCopy:= ActField^.ElemStructPtr^.GetFieldCopy(IdentParam,Wert); 
               ActField^.ElemStructPtr^.SetDataSpace(SvePtr); 
            END
            ELSE BEGIN
               MemCopy(ActPtr,Wert,CPLen); 
               GetFieldCopy := TRUE; 
            END; 
         END
      END; 
   
   {*************************************************************************
   *
   * Liefert das Feld in Stringdarstellung
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetFieldStr; 
      VAR   ActField : TFieldStructPtr; 
            ActSeg,
            ActOfs   : WORD; 
            SvePtr,
            ActPtr   : POINTER; 
            GetFromStruct : BOOLEAN; 
            Len      : BYTE; 
            Dec      : BYTE; 
            CPLen,
            ArrayElem: WORD; 
            ActType  : BYTE; 
            Wert     : STRING; 
            ZwWert   : LONGINT; 
            BytePtr  : ^BYTE; 
            SIPtr    : ^SHORTINT; 
            INTPtr   : ^INTEGER; 
            WordPtr  : ^WORD; 
            LongPtr  : ^LONGINT; 
            SinglePtr: ^SINGLE; 
            RealPtr  : ^REAL; 
            DoublePtr: ^DOUBLE; 
            ExtPtr   : ^EXTENDED; 
            CompPtr  : ^COMP; 
            WertPtr  : POINTER; 
            SplitChar : CHAR; 
            IdentParam,
            IdentParam2:STRING;
      BEGIN
         GetFieldStr :=''; 
         GetFromStruct := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActType := ActField^.FieldType; 
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs:= ActOfs + ActField^.FieldOffSet; 
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2);
                  ArrayElem := S2L(IdentParam); 
                  CPLen := ActField^.ElemSize; 
                  ActType := ActField^.ElemType; 
                  ActOfs := ActOfs + CPLen*(ArrayElem-1); 
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     DELETE(IdentParam2,1,1);
                     IdentParam := IdentParam2;
                     GetFromStruct := TRUE;
                  END;
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     GetFromStruct := TRUE;
            ActPtr := PTR(ActSeg,ActOfs);
            IF GetFromStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr;
                  ActField^.ElemStructPtr^.SetDataSpace(ActPtr);
                  GetFieldStr := ActField^.ElemStructPtr^.GetFieldStr(IdentParam,Attrib);
                  ActField^.ElemStructPtr^.SetDataSpace(SvePtr);
               END;
            END
            ELSE BEGIN
               WertPtr := @Wert;
               MemCopy(ActPTr,WertPtr,CPLen);
               IF ((ActType >= 8) OR (ActType <= 12)) THEN BEGIN
                  Len := 0;
                  Dec := S2B(Attrib);
               END;
               CASE ActType OF
                  2 : GetFieldStr := Wert[0];
                  3 : BEGIN
                         BytePtr := WertPtr; 
                         GetFieldStr:= B2S(BytePtr^,0); 
                      END; 
                  4 : BEGIN
                         WordPtr := WertPtr; 
                         GetFieldStr:= W2S(WordPtr^,0); 
                      END; 
                  5 : BEGIN
                         SiPtr := WertPtr; 
                         GetFieldStr:=SI2S(SiPtr^,0); 
                      END; 
                  6 : BEGIN
                         IntPtr := WertPtr; 
                         GetFieldStr:=I2S(IntPtr^,0); 
                      END; 
                  7 : BEGIN
                         LongPtr := WertPtr; 
                         GetFieldStr:=L2S(LongPtr^,0); 
                      END; 
                  8 : BEGIN
                         SinglePtr := WertPtr; 
                         GetFieldStr:=SG2S(SinglePtr^,0,Dec); 
                      END; 
                  9 : BEGIN
                         RealPtr := WertPtr; 
                         GetFieldStr:=R2S(RealPtr^,0,Dec); 
                      END; 
                  10 : BEGIN
                          DoublePtr := WertPtr; 
                          GetFieldStr:=D2S(DoublePtr^,0,Dec); 
                       END; 
                  11 : BEGIN
                          ExtPtr := WertPtr; 
                          GetFieldStr:=E2S(ExtPtr^,0,Dec); 
                       END; 
                  12 : BEGIN
                          CompPtr := WertPtr; 
                          GetFieldStr:=C2S(CompPtr^,0,Dec); 
                       END; 
                  
                  13: GetFieldStr := Wert; 
                  16: IF (LENGTH(Wert) <> 8) THEN
                         GetFieldStr := '        '
                      ELSE
                         GetFieldStr := Wert[7]+Wert[8]+Wert[4]+Wert[5]+
                         Wert[1]+Wert[2]; 
                  17: IF (LENGTH(Wert) <> 5) THEN
                         GetFieldStr := '     '
                      ELSE
                         GetFieldStr := Wert[1]+Wert[2]+Wert[4]+Wert[5]; 
                  18: IF (LENGTH(Wert) <> 5) THEN
                         GetFieldStr := '     '
                      ELSE
                         GetFieldStr := Wert[4]+Wert[5]+Wert[1]+Wert[2]; 
               END; 
            END; 
         END
      END; 
   
   {*************************************************************************
   *
   * Liefert das angegebene Feld in indexverwertbarer Form
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.GetIdxStr; 
      VAR   ActField : TFieldStructPtr; 
            ActSeg,
            ActOfs   : WORD; 
            ActPtr   : POINTER; 
            Len      : BYTE; 
            CPLen,
            ArrayElem: WORD; 
            ActType  : BYTE; 
            Wert     : STRING; 
            ZwWert   : LONGINT; 
            BytePtr  : ^BYTE; 
            SIPtr    : ^SHORTINT; 
            INTPtr   : ^INTEGER; 
            WordPtr  : ^WORD; 
            LongPtr  : ^LONGINT; 
            WertPtr  : POINTER; 
            IdentParam:STRING;
            SplitChar : CHAR;
      BEGIN
         Ident := UPPER(Ident);
         SplitChar := #0;
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            IF ((ActField^.FieldType >= 2) AND ActField^.FieldType <= 7)) OR
               (ActField^.FieldType = 13) OR            
               ((ActField^.FieldType >= 16) AND ActField^.FieldType <= 18)) THEN BEGIN
               ActType := ActField^.FieldType; 
               ActSeg := SEG(DataPtr^); 
               ActOfs := OFS(DataPtr^); 
               ActOfs:= ActOfs + ActField^.FieldOffSet; 
               CPLen := ActField^.Len; 
               ActPtr := PTR(ActSeg,ActOfs); 
               WertPtr := @Wert; 
               MemCopy(ActPTr,WertPtr,CPLen); 
               Len := 0; 
               IF (COPY(Attrib,1,2) = 'LZ') AND ((ActType >= 2) AND (ActType <= 7)) THEN BEGIN
                  Len:= S2B(COPY(Attrib,3,LENGTH(Attrib)-2)); 
               END; 
               IF (Attrib = 'FR') AND (ActType = 13) THEN BEGIN
                  Len :=CPLen; 
               END; 
               CASE ActType OF
                  2 : GetIdxStr := Wert[0]; 
                  3 : BEGIN
                         BytePtr := WertPtr; 
                         GetIdxStr:= FillUp('0',B2S(BytePtr^,0),Len,Len); 
                      END; 
                  4 : BEGIN
                         WordPtr := WertPtr; 
                         GetIdxStr:= FillUp('0',W2S(WordPtr^,0),Len,Len); 
                      END; 
                  5 : BEGIN
                         SiPtr := WertPtr; 
                         GetIdxStr:=FillUp('0',SI2S(SiPtr^,0),Len,Len); 
                      END; 
                  6 : BEGIN
                         IntPtr := WertPtr; 
                         GetIdxStr:=FillUp('0',I2S(IntPtr^,0),Len,Len); 
                      END; 
                  7 : BEGIN
                         LongPtr := WertPtr; 
                         GetIdxStr:=FillUp('0',L2S(LongPtr^,0),Len,Len); 
                      END; 
                  
                  13: GetIdxStr := FillUpRight(' ',Wert,Len,Len); 
                  16: IF (LENGTH(Wert) <> 8) THEN
                         GetIdxStr := '        '
                      ELSE BEGIN
                         IF (COPY(Wert,7,2) > '50') THEN
                            GetIdxStr := '19'+Wert[7]+Wert[8]+Wert[4]+Wert[5]+
                            Wert[1]+Wert[2]
                         ELSE
                            GetIdxStr := '20'+Wert[7]+Wert[8]+Wert[4]+Wert[5]+
                            Wert[1]+Wert[2]; 
                      END; 
                  17: IF (LENGTH(Wert) <> 5) THEN
                         GetIdxStr := '     '
                      ELSE
                         GetIdxStr := Wert; 
                  18: IF (LENGTH(Wert) <> 5) THEN
                         GetIdxStr := '     '
                      ELSE
                         GetIdxStr := Wert; 
               END; 
            END; 
         END
         ELSE
            GetIdxStr :=''; 
      END; 
   
   {*************************************************************************
   *
   * Belegt das angegebene Feld mit einem Wert
   *
   *************************************************************************}

   FUNCTION TFreeDataStruct.SetField;
      VAR   ActField : TFieldStructPtr; 
            CPLen,
            ValueOfs,
            ActSeg,
            ActOfs   : WORD; 
            SvePtr,
            ActPtr,
            WertPtr  : POINTER; 
            FPtr     : ^BYTE; 
            ArrayElem :WORD; 
            FType,
            SaveFPtrL: BYTE; 
            SetInStruct : BOOLEAN; 
            SplitChar  : CHAR; 
            IdentParam,
            IdentParam2:STRING;
            
      BEGIN
         SetInStruct := FALSE; 
         SetField := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         SaveFPtrL:= 0; 
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs := ActOfs + ActField^.FieldOffSet; 
            FType := ActField^.FieldType; 
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2);
                  ArrayElem := S2L(IdentParam); 
                  CPLen := ActField^.ElemSize; 
                  FType := ActField^.ElemType; 
                  ActOfs := ActOfs + CPLen*(ArrayElem-1); 
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     Delete(IdentParam2,1,1);
                     IdentParam := IdentParam2;
                     SetInStruct := TRUE; 
                  END; 
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     SetInStruct := TRUE; 
            ActPtr := PTR(ActSeg,ActOfs); 
            IF SetInStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr; 
                  ActField^.ElemStructPtr^.SetDAtaSpace(ActPtr); 
                  SetField := ActField^.ElemStructPtr^.SetField(IdentParam,Wert); 
                  ActField^.ElemStructPtr^.SetDAtaSpace(SvePtr); 
               END; 
            END
            ELSE BEGIN
               WertPtr := @Wert; 
               IF ((FType = 13) OR ((Ftype >= 16) AND (FType <= 18))) THEN BEGIN
                  FPtr := WertPtr;
                  IF (FPtr^ > (CPLen-1)) THEN BEGIN
                     SaveFPtrL := FPtr^;
                     FPtr^:= CPLen-1; 
                  END
               END; 
               MemCopy(WertPtr,ActPtr,CPLen); 
               IF (SaveFPtrL > 0) THEN
                  FPtr^ := SaveFPtrL; 
               SetField := TRUE;
            END;
         END
      END; 
   {*************************************************************************
   *
   * Belegt das Feld aus Stringdarstellung
   *
   *************************************************************************}

   FUNCTION TFreeDataStruct.SetFieldStr;
      VAR   ActField : TFieldStructPtr;
            ActSeg,
            ActOfs   : WORD;
            SvePtr,
            ActPtr   : POINTER;
            SetInStruct : BOOLEAN;
            Len      : BYTE;
            Dec      : BYTE;
            CPLen,
            ArrayElem: WORD;
            ActType  : BYTE;
            Wert     : STRING;
            ZwWert   : LONGINT;
            SaveFPtrl : BYTE;
            FPtr,
            BytePtr  : ^BYTE;
            SIPtr    : ^SHORTINT;
            INTPtr   : ^INTEGER;
            WordPtr  : ^WORD;
            LongPtr  : ^LONGINT;
            SinglePtr: ^SINGLE;
            RealPtr  : ^REAL;
            DoublePtr: ^DOUBLE;
            ExtPtr   : ^EXTENDED;
            CompPtr  : ^COMP;
            WertPtr  : POINTER;
            SplitChar : CHAR;
            IdentParam,
            IdentParam2:STRING;
      BEGIN
         SetFieldStr :=FALSE;
         SetInStruct := FALSE;
         SaveFPtrL := 0;
         Ident := UPPER(Ident);
         SplitIdent(Ident,SplitChar,Ident,IdentParam);

         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActType := ActField^.FieldType;
            ActSeg := SEG(DataPtr^);
            ActOfs := OFS(DataPtr^);
            ActOfs:= ActOfs + ActField^.FieldOffSet;
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2);
                  ArrayElem := S2L(IdentParam);
                  CPLen := ActField^.ElemSize;
                  ActType := ActField^.ElemType;
                  ActOfs := ActOfs + CPLen*(ArrayElem-1);
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     DELETE(IdentParam2,1,1);
                     IdentParam := IdentParam2;
                     SetInStruct := TRUE;
                  END;
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     SetInStruct := TRUE;
            ActPtr := PTR(ActSeg,ActOfs);
            IF SetInStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr;
                  ActField^.ElemStructPtr^.SetDataSpace(ActPtr);
                  SetFieldStr := ActField^.ElemStructPtr^.SetFieldStr(IdentParam,Buffer);
                  ActField^.ElemStructPtr^.SetDataSpace(SvePtr);
               END;
            END
            ELSE BEGIN
               WertPtr := @Wert;
               CASE ActType OF
                  2 : Wert[0] :=Buffer[1];
                  3 : BEGIN
                         BytePtr := WertPtr;
                         BytePtr^:= S2B(Buffer);
                      END;
                  4 : BEGIN
                         WordPtr := WertPtr;
                         WordPtr^:= S2W(Buffer);
                      END;
                  5 : BEGIN
                         SIPtr := WertPtr;
                         SIPtr^:= S2SI(Buffer);
                      END;
                  6 : BEGIN
                         IntPtr := WertPtr;
                         IntPtr^ := S2I(Buffer);
                      END;
                  7 : BEGIN
                         LongPtr := WertPtr;
                         LongPtr^:= S2L(Buffer);
                      END;
                  8 : BEGIN
                         SinglePtr := WertPtr;
                         SinglePtr^:= S2SG(Buffer);
                      END;
                  9 : BEGIN
                         RealPtr := WertPtr;
                         RealPtr^:= S2R(Buffer);
                      END;
                  10 :BEGIN
                          DoublePtr := WertPtr;
                          DoublePtr^:= S2D(Buffer);
                      END;
                  11 :BEGIN
                          ExtPtr := WertPtr;
                          ExtPtr^:= S2E(Buffer);
                       END;
                  12 : BEGIN
                          CompPtr := WertPtr;
                          CompPtr^:=S2C(Buffer);
                       END;
                  13: Wert := Buffer;
                  16: IF (LENGTH(Buffer) <> 8) THEN
                         Wert:= '        '
                      ELSE
                         Wert := Buffer[5]+Buffer[6]+'.'+Buffer[3]+Buffer[4]+'.'+
                                 Buffer[1]+Buffer[2];
                  17: IF (LENGTH(Buffer) <> 5) THEN
                         Wert := '     '
                      ELSE BEGIN
                         Wert := Buffer;
                         Insert(':',Wert,3);
                      END;
                  18: IF (LENGTH(Buffer) <> 5) THEN
                         Wert:= '     '
                      ELSE
                         Wert := Buffer[3]+Buffer[4]+'/'+Buffer[1]+Buffer[2];
               END;
               IF ((ActType = 13) OR ((ActType >= 16) AND (ActType <= 18))) THEN BEGIN
                  FPtr := WertPtr;
                  IF (FPtr^ > (CPLen-1)) THEN BEGIN
                     SaveFPtrL := FPtr^;
                     FPtr^:= CPLen-1;
                  END
               END;
               MemCopy(WertPtr,ActPtr,CPLen);
               SetFieldStr := TRUE;
            END;
         END
      END;


   {*************************************************************************
   *
   * Setzt Verweis auf Objekt zur Bearbeitung einer Struktur
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.SetFieldStructPtr; 
      VAR   ActField : TFieldStructPtr; 
            LPtr     : TFieldStructPtr; 
            IdentParam:STRING;
            SplitChar : CHAR;

      BEGIN
         Ident := UPPER(Ident);
         SplitChar := #0;
         IF SeekField(Ident,ActField,SplitChar,IdentParam) AND ((ActField^.FieldType = 15) OR
            ((ActField^.FieldType = 14) AND (ActField^.ElemType = 15)))
         THEN BEGIN
            ActField^.ElemStructPtr := NewDataStruct; 
            SetFieldStructPtr := TRUE; 
         END
         ELSE
            SetFieldStructPtr := FALSE; 
      END; 
   
   {*************************************************************************
   *
   * Belegt ein Feld durch Kopieren
   *
   *************************************************************************}
   
   FUNCTION TFreeDataStruct.SetFieldCopy; 
      VAR   ActField : TFieldStructPtr; 
            CPLen,
            ValueOfs,
            ActSeg,
            ActOfs   : WORD; 
            SvePtr,
            ActPtr   : POINTER; 
            FPtr     : ^BYTE; 
            ArrayElem :WORD; 
            FType,
            SaveFPtrL: BYTE; 
            SetInStruct : BOOLEAN; 
            SplitChar : CHAR; 
            IdentParam,
            IdentParam2:STRING;
            
      BEGIN
         SetInStruct := FALSE; 
         SetFieldCopy := FALSE; 
         Ident := UPPER(Ident); 
         SplitIdent(Ident,SplitChar,Ident,IdentParam); 
         SaveFPtrL:= 0; 
         IF SeekField(Ident,ActField,SplitChar,IdentParam) THEN BEGIN
            ActSeg := SEG(DataPtr^); 
            ActOfs := OFS(DataPtr^); 
            ActOfs := ActOfs + ActField^.FieldOffSet; 
            FType := ActField^.FieldType; 
            IF SplitChar = #0 THEN
               CPLen := ActField^.Len
            ELSE
               IF (SplitChar = '[') AND (ActField^.FieldType = 14) THEN BEGIN
                  SplitString(IdentParam,']',IdentParam,IdentParam2);
                  ArrayElem := S2L(IdentParam);
                  CPLen := ActField^.ElemSize;
                  FType := ActField^.ElemType;
                  ActOfs := ActOfs + CPLen*(ArrayElem-1);
                  IF (IdentParam2[1] = '.') AND (ActField^.ElemType = 15)  THEN BEGIN
                     DELETE(IdentPAram2,1,1);
                     IdentParam := IdentParam2;
                     SetInStruct := TRUE;
                  END;
               END
               ELSE
                  IF (SplitChar = '.') AND (ActField^.FieldType = 15) THEN
                     SetInStruct := TRUE;
            ActPtr := PTR(ActSeg,ActOfs);
            IF SetInStruct THEN BEGIN
               IF (ActField^.ElemStructPtr <> NIL) THEN BEGIN
                  SvePtr := ActField^.ElemStructPtr^.GetDataSpacePtr;
                  ActField^.ElemStructPtr^.SetDataSpace(ActPtr);
                  SetFieldCopy:=ActField^.ElemStructPtr^.SetFieldCopy(IdentParam,Wert);
                  ActField^.ElemStructPtr^.SetDataSpace(SvePtr);
               END;
            END
            ELSE BEGIN
               IF ((FType = 13) OR ((Ftype >= 16) AND (FType <= 18))) THEN BEGIN
                  FPtr := Wert;
                  IF (FPtr^ > (CPLen-1)) THEN BEGIN
                     SaveFPtrL := FPtr^;
                     FPtr^:= CPLen-1;
                  END
               END;
               MemCopy(Wert,ActPtr,CPLen);
               IF (SaveFPtrL > 0) THEN
                  FPtr^ := SaveFPtrL;
               SetFieldCopy := TRUE;
            END;
         END
      END;

   FUNCTION TFreeDataStruct.SeekField;
      VAR   LPtr   : TSeekFieldStructPtr;
            Result : BOOLEAN;
      BEGIN
         LPtr := FieldNames[ORD(Ident[1])];
         WHILE (LPtr <> NIL) AND (LPtr^.Identifier <> Ident) DO
            LPtr := LPtr^.NextField;
         Result := (LPtr <> NIL);
         IF Result THEN BEGIN
            Field := LPtr^.FieldPtr;
            IF (LPtr^.SplitChar <> #0) THEN BEGIN
               IF (SplitChar <> #0) THEN
                  IdentParam := SplitChar+IdentParam;
               IdentParam:= LPtr^.IdentParam+IdentParam;
               SplitChar := LPTr^.SplitChar;
            END;
            Result := (LPtr^.Identifier = Ident);
         END;
         SeekField := Result;
      END;

   FUNCTION TFreeDataStruct.FieldExists;

      VAR   Garbage : TFieldStructPtr;
            Garbage1 : CHAR;
            Garbage2 : STRING;

      BEGIN
         Garbage1:= #0;
         FieldExists := SeekField(Ident,Garbage,Garbage1,Garbage2);
      END;

   PROCEDURE TFreeDataStruct.ResetData;
      VAR   Lauf    : WORD;
            MaxLauf : WORD;
            aktType : BYTE;
            LPtr    : ^BYTE;
            aktIdent : STRING;


      BEGIN
         MaxLauf := DataSize;
         LPtr := DataPtr;
         FOR Lauf := 1 TO MaxLauf DO BEGIN
            LPtr^:=0;
            INC(LPtr);
         END;
      END;

   DESTRUCTOR TFreeDataStruct.Done;
      BEGIN
         Status:= 0;
         DisposeFields;
         DataPtr := NIL;
      END;

   CONSTRUCTOR TSetupObj.Init;
      VAR   Lauf : BYTE;
      BEGIN
         ParseString :='';
         LineCount := 0;
         StructList := NIL;
         Status := 0;
         Comment := FALSE;
         ActStruct.FieldList := NIL;
         FOR Lauf := 65 TO 90 DO
            ActStruct.FieldNames[Lauf] := NIL;
         ResetActStruct;
         OffSet := 0;
      END;

   PROCEDURE TSetupObj.GetSetup;
      VAR   Result :BYTE;
      BEGIN
         IF (Status = 0) THEN BEGIN
            SetupFileName := MakeFileName(FileName,'INI');
            LogFileName := COPY(SetupFileName,1,POS('.',SetupFileName))+'LOG';
            OpenFile;
            IF (Status = 0) THEN
               ReadFile; 
            CloseFile;
         END; 
      END; 
   
   FUNCTION TSetupObj.GetErrNum; 
      BEGIN
         GetErrNum := Status; 
      END; 
   
   FUNCTION TSetupObj.StructExists; 
      VAR   SPTR : TStructPtr; 
            
      BEGIN
         SPTR := StructList; 
         WHILE (SPTR <> NIL) AND (SPTR^.Structname<> Sname) DO
            SPTR := SPTR^.NextStruct; 
         ResPtr := SPTR; 
         StructExists:=(ResPtr <> NIL); 
      END; 
   
   FUNCTION TSetupObj.FieldExists; 
      VAR   FPTR   : TFieldStructPtr; 
            FSPtr  : TSeekFieldStructPTr; 
            Result : BOOLEAN; 
            
      BEGIN
         FName := UPPER(FName); 
         FPtr := Struct.FieldList; 
         Result := FALSE; 
         Alias := FALSE; 
         ResPtr := NIL; 
         IF (FPTR <> NIL) THEN BEGIN
            WHILE (FPTR^.Identifier <> FName) AND (FPTR^.NextField <> NIL) DO
               FPTR := FPTR^.NextField; 
            Result := (FPTR^.Identifier = FName)
         END; 
         ResPtr := FPTR; 
         IF Result THEN BEGIN
            FSPtr := Struct.FieldNames[ORD(FName[1])]; 
            IF (FSPTR <> NIL) THEN BEGIN
               WHILE (FSPTR^.Identifier <> FName) AND (FSPTR^.NextField <> NIL) DO
                  FSPTR := FSPTR^.NextField; 
               IF (FSPTR^.Identifier = FName) THEN
                  Alias := FSPtr^.Alias; 
            END; 
         END; 
         FieldExists:=Result; 
      END; 
   
   FUNCTION TSetupObj.GetStruct; 
      VAR   SPTR : TStructPtr; 
      BEGIN
         SName := UPPER(SName); 
         IF StructExists(SName,SPTR) THEN BEGIN
            Struct := SPTR^; 
            Struct.NextStruct := NIL; 
            GetStruct := TRUE; 
         END 
         ELSE
            GetStruct := FALSE; 
      END; 
   
   FUNCTION TSetupObj.GetField; 
      VAR   Struct     : TStruct; 
            FPtr       : TFieldStructPtr; 
            Alias      : BOOLEAN; 
            
      BEGIN
         SName := UPPER(SName); 
         GetField := FALSE; 
         Alias := FALSE; 
         IF GetStruct(SName,Struct) THEN BEGIN
            FName :=UPPER(FName); 
            IF FieldExists(FName,Struct,FPtr,Alias) AND (NOT Alias) THEN BEGIN
               Field := FPtr^; 
               Field.NextField := NIL; 
               GetField := TRUE; 
            END; 
         END
      END; 
   
   PROCEDURE TSetupObj.SetDataStruct; 
      VAR   SPTR : TStructPtr; 
      BEGIN
         SPTR := StructList; 
         SName := UPPER(Sname); 
         IF StructExists(SName,SPTR) THEN
            Data.SetFields(SPTR^.FieldList,SPTR^.FieldNames); 
      END; 
   
   PROCEDURE TSetupObj.ResetActStruct;
      VAR   DPtr,
            LPtr : TFieldStructPtr; 
            Lauf : BYTE; 
            DsPtr,
            LsPtr:TSeekFieldStructPtr; 
            
      BEGIN
         WITH ActStruct DO BEGIN
            Complete   := TRUE; 
            StructName := ''; 
            FieldOffSet:= 0; 
            Len        := 0; 
            NextStruct := NIL; 
            LPtr := FieldList; 
            WHILE (LPtr <> NIL) DO BEGIN
               DPtr := LPtr; 
               LPtr := LPtr^.NextField; 
               DISPOSE(DPtr); 
            END; 
            FieldList := NIL; 
            FOR Lauf := 65 TO 90 DO BEGIN
               LsPtr := FieldNames[Lauf]; 
               WHILE (LsPtr <> NIL) DO BEGIN
                  DsPtr := LsPtr; 
                  LsPtr := LsPtr^.NextField; 
                  DISPOSE(DsPtr); 
               END; 
               FieldNames[Lauf] := NIL; 
            END; 
         END; 
      END;

   PROCEDURE TSetupObj.OpenFile;
      VAR LocalError : INTEGER;
      BEGIN
         {$I-}
         ASSIGN (LogFile,LogFileName);
         REWRITE(LogFile);
         {$I+}
         LocalError := IORESULT;
         {$I-}
         WRITELN(LogFile,'Parsing struct definition file: ',SetupFileName);
         WRITELN(LogFile);
         {$I+}
         IF SeekFile(SetupFileName)THEN BEGIN
            {$I-}
            ASSIGN (SetupFile,SetupFileName);
            RESET  (SetupFile);
            {$I+}
            LocalError := IORESULT;
            IF (LocalError <> 0) THEN BEGIN
               Status := 2;
               WRITELN(LogFile,'Parse error: unable to open file ');
            END;
         END
         ELSE BEGIN
            Status := 3;
            WRITELN(LogFile,'Parse error: file not found '); 
         END; 
      END; 
   
   FUNCTION TSetupObj.ParseStructBegin; 
      VAR   SPTR :TStructPtr; 
      BEGIN
         ParseStructBegin := FALSE; 
         IF (ActStruct.StructName =  '') THEN BEGIN
            IF ValidId(ParamString) THEN BEGIN
               IF StructExists(ParamString,SPTR) THEN BEGIN
                  {$I-}
                  WRITELN(LogFile,'Parse error: line',LineCount:0,
                  ' - struct ',ParamString,
                  'exists already'); 
                  {$I+}
               END 
               ELSE BEGIN
                  {$I-}
                  WRITELN(LogFile,'  struct ',ParamString); 
                  {$I+}
                  ActStruct.StructName := ParamString; 
                  ParseStructBegin := TRUE; 
               END; 
            END 
            ELSE BEGIN
               {$I-}
               WRITELN(LogFile,'Parse error: line ',LineCount:0,
               ' - no identifier: ',ParamString); 
               {$I+}
            END; 
         END
         ELSE BEGIN
            {$I-}
            WRITELN(LogFile,'Parse error: line ',LineCount:0,'- unclosed struct ',ActStruct.Structname);
            WRITELN(LogFile,'             unable to open ',ParamString); 
            {$I+}
         END; 
      END; 
   
   FUNCTION TSetupObj.ParseStructEnd; 
      VAR   NStructPtr,
            LPtr : TStructPtr; 
            NFieldPtr,
            LNFPtr,
            LOFPtr : TFieldStructPtr; 
            LSOPtr,
            LSNPtr,
            NSPtr  : TSeekFieldStructPtr; 
            Lauf : BYTE; 
            
      BEGIN
         IF (ActStruct.StructName <> '') THEN BEGIN
            IF (MAXAVAIL > SIZEOF(ActStruct)) THEN BEGIN
               NEW(NStructPtr); 
               WITH NStructPtr^ DO BEGIN
                  Complete := TRUE; 
                  StructName := ActStruct.StructName; 
                  OffSet := ActStruct.FieldOffSet; 
                  Len    := ActStruct.Len; 
                  NextStruct:= NIL; 
                  FieldList := NIL; 
                  LNFPtr := FieldList; 
                  LOFPtr := ActStruct.FieldList; 
                  WHILE (LOFPtr <> NIL) DO BEGIN
                     IF (MAXAVAIL > SIZEOF(TFieldStruct)) THEN BEGIN
                        NEW(NFieldPtr); 
                        NFieldPtr^:= LOFPtr^; 
                        NFieldPtr^.NextField  := NIL; 
                        IF (LNFPtr <> NIL) THEN BEGIN
                           LNFPtr^.NextField := NFieldPtr; 
                           LNFPtr := NFieldPtr; 
                        END
                        ELSE BEGIN
                           LNFPtr := NFieldPtr; 
                           FieldList := NFieldPtr; 
                        END; 
                     END
                     ELSE
                        Status := 1; 
                     LOFPtr := LOFPtr^.NextField; 
                  END; 
                  FOR Lauf := 65 TO 90 DO BEGIN
                     LsOPtr := ActStruct.FieldNames[Lauf]; 
                     IF (LSOPtr <> NIL) THEN BEGIN
                        IF (MAXAVAIL > SIZEOF(TSeekFieldStruct)) THEN BEGIN
                           NEW(NSPtr); 
                           FieldNames[Lauf] := NSPtr; 
                           NsPtr^:=LSOPtr^; 
                           NSPtr^.NextField:= NIL; 
                           LSNPtr:=NSPtr; 
                           WHILE (LSOPtr <> NIL) DO BEGIN
                              IF (MAXAVAIL > SIZEOF(TSeekFieldStruct)) THEN BEGIN
                                 NEW(NSPtr); 
                                 NSPTr^:=LSOPtr^; 
                                 NSPtr^.NextField:= NIL; 
                                 LSNPtr^.NextField:=NSPtr; 
                                 LSNPtr := NSPtr; 
                              END 
                              ELSE
                                 Status := 1; 
                              LSOPtr:= LSOPtr^.NextField; 
                           END; 
                        END 
                        ELSE BEGIN
                           Status := 1; 
                           FieldNames[Lauf]:=NIL; 
                        END; 
                     END 
                     ELSE
                        FieldNames[Lauf]:=NIL; 
                  END; 
                  
                  
               END; 
               LPtr := StructList; 
               IF (LPtr = NIL) THEN
                  StructList := NStructPtr
               ELSE BEGIN
                  WHILE (LPtr^.NextStruct <> NIL) DO
                     LPtr := LPtr^.NextStruct; 
                  LPtr^.NextStruct := NStructPtr; 
               END; 
            END
            ELSE
               Status := 1; 
            IF (Status =1) THEN BEGIN
               {$I+}
               WRITELN(LogFile,'  struct ',ActStruct.StructName,' not enough memory',ActStruct.len:0); 
               {$I-}
               ResetActStruct;
               ParseStructEnd := FALSE; 
            END 
            ELSE BEGIN
               {$I+}
               WRITELN(LogFile,'  struct ',ActStruct.StructName,' end. - length: ',ActStruct.len:0); 
               {$I-}
               ResetActStruct;
               ParseStructEnd := TRUE; 
            END; 
         END
         ELSE BEGIN
            {$I-}
            WRITELN(LogFile,'Parse error: line ',LineCount:0,' - no open struct'); 
            {$I+}
            ParseStructEnd := FALSE; 
         END; 
      END; 
   
   FUNCTION TSetupObj.ParseStructField; 
      VAR   FieldName,
            FieldType : STRING; 
            Alias     : BOOLEAN; 
            ErrNum      : BYTE; 
            Count       : BYTE; 
            NewField    : TFieldStruct; 
            LPtr,
            NPtr        : TFieldStructPtr; 
            
      FUNCTION IsNumber(PString:STRING):BOOLEAN; 
         VAR   Count : BYTE; 
               Plen  : BYTE; 
         BEGIN
            PLen := LENGTH(PString); 
            Count := 1; 
            WHILE (Count <= Plen) AND (PString[Count] IN ['0'..'9']) DO
               INC(Count); 
            IsNumber:= (Count > PLen); 
         END; 
      
      FUNCTION CreateStrField(PString:STRING; VAR ErrResult:BYTE):WORD; 
         BEGIN
            IF PString<> '' THEN
               IF IsNumber(PString) THEN
                  CreateStrField:=S2L(PString)+1
               ELSE
                  ErrResult := 4
            ELSE
               CreateStrField:=256;
         END; 
      
      FUNCTION CreateStructField(PString:STRING; VAR ErrResult:BYTE):WORD; 
         VAR   LPtr : TStructPtr; 
               Found: BOOLEAN; 
               Size : WORD; 
               
         BEGIN
            Found := FALSE; 
            LPtr := StructList; 
            Size := 0; 
            WHILE (LPtr <> NIL) AND (Size = 0) DO BEGIN
               IF (LPtr^.StructName = PString) THEN BEGIN
                  Found := TRUE; 
                  Size := LPtr^.Len; 
               END; 
               LPtr := LPtr^.NextStruct; 
            END; 
            IF (NOT Found) THEN
               ErrResult := 7; 
            CreateStructField  := Size; 
         END; 
      
      FUNCTION CreateArrayField(PString:STRING; VAR ErrResult:BYTE):WORD; 
         VAR   ArrayDim,
               ElemType,
               ElemParam : STRING; 
               Count,
               ElemSize,
               Result,
               DimNo  : WORD; 
         BEGIN
            SplitString(PString,':',ArrayDim,PString); 
            IF IsNumber(ArrayDim) THEN BEGIN
               NewField.ElemNo := S2l(ArrayDim); 
               SplitString(PString,':',ElemType,ElemParam); 
               IF (ElemType <> '') THEN BEGIN
                  Count := 1; 
                  WHILE (Count <= MaxTypeNum) AND
                        (Elemtype <> TypeStrings[Count])
                  DO
                     INC(Count); 
                  IF (Count <= MaxTypeNum) THEN BEGIN
                     NewField.ElemType := Count; 
                     CASE Count OF
                        1..12,
                        16..18: BEGIN
                                   NewField.ElemSize := TypeLen[Count]; 
                                   IF (ElemParam <> '') THEN
                                      ErrResult := 5; 
                                END; 
                        13    : NewField.ElemSize := CreateStrField(ElemParam,ErrResult); 
                        14    : NewField.ElemSize := CreateArrayField(ElemParam,ErrResult); 
                        15    : NewField.ElemSize := CreateStructField(ElemParam,ErrResult); 
                     END
                  END
                  ELSE
                     ErrResult := 8; 
               END
               ELSE
                  ErrResult := 6; 
               CreateArrayField := NewField.ElemNo*NewField.ElemSize; 
            END
            ELSE
               ErrResult := 4; 
         END; 
      
      BEGIN
         ParseStructField := FALSE; 
         IF (ActStruct.StructName <> '') THEN BEGIN
            SplitString(ParamString,':',FieldName,ParamString); 
            IF ValidId(FieldName) THEN BEGIN
               SplitString(ParamString,':',FieldType,ParamString); 
               Count := 1; 
               WHILE (Count <= MaxTypeNum) AND
                     (FieldType <> TypeStrings[Count])
               DO
                  INC(Count); 
               IF (Count <= MaxTypeNum) THEN BEGIN
                  IF NOT FieldExists(FieldName,ActStruct,LPtr,Alias) THEN BEGIN
                     ErrNum := 0; 
                     NewField.Identifier :=FieldName; 
                     NewField.FieldType  :=Count; 
                     NewField.FieldOffSet:=ActStruct.FieldOffSet; 
                     NewField.NextField  :=NIL; 
                     NewField.ElemSize   :=0; 
                     NewField.ElemType   :=0; 
                     NewField.ElemNo     :=0; 
                     NewField.ElemStructPtr := NIL; 
                     CASE Count OF
                        1..12,
                        16..18:BEGIN
                                  NewField.Len:=TypeLen[Count]; 
                                  IF (ParamString <> '') THEN
                                     ErrNum := 5; 
                               END; 
                        13    :NewField.Len:=CreateStrField(ParamString,ErrNum); 
                        14    :NewField.Len:=CreateArrayField(ParamString,ErrNum); 
                        15    :NewField.Len:=CreateStructField(ParamString,ErrNum); 
                     END; 
                     IF (ErrNum = 0) THEN BEGIN
                        ActStruct.FieldOffSet := ActStruct.FieldOffSet+NewField.Len; 
                        ActStruct.Len         := ActStruct.Len        +NewField.Len; 
                        IF (MAXAVAIL > SIZEOF(NewField)) THEN BEGIN
                           NEW(NPtr); 
                           NPtr^:=NewField; 
                           IF (LPtr = NIL) THEN
                              ActStruct.FieldList := NPtr
                           ELSE
                              LPtr^.NextField := NPtr; 
                           InsertIntoFieldNames(FieldName,#0,'','',FALSE,NPtr,ActStruct.FieldNames);
                           WITH NewField DO BEGIN
                              {$I-}
                              WRITELN(LogFile,'    field: ',Identifier,', type: ',TypeStrings[Count],
                              ', offset: ',FieldOffSet,', length: ',len); 
                              {$I+}
                           END; 
                           ParseStructField := TRUE; 
                        END 
                        ELSE BEGIN
                           {$I-}
                           WRITE(LogFile,'Parse error: line ',LineCount:0,
                           ' - not enough memory'); 
                           {$I+}
                           Status := 1; 
                        END
                     END
                     ELSE BEGIN
                        {$I-}
                        WRITE(LogFile,'Parse error: line ',LineCount:0,' - '); 
                        {$I+}
                        Status := ErrNum; 
                        CASE ErrNum OF
                           4 : WRITELN(LogFile,' param not a number'); 
                           5 : WRITELN(LogFile,' too much params'); 
                           6 : WRITELN(LogFile,' too low params'); 
                           7 : WRITELN(LogFile,' undefined struct'); 
                           8 : WRITELN(LogFile,' undefined fieldtype'); 
                        END; 
                     END; 
                  END 
                  ELSE BEGIN
                     {$I-}
                     WRITELN(LogFile,'Parse error: line',LineCount:0,
                     ' - field ',FieldName,
                     'exists already'); 
                     {$I+}
                  END; 
               END 
               ELSE BEGIN
                  {$I-}
                  WRITELN(LogFile,'Parse error: line ',LineCount:0,
                  ' - undefined type: ',FieldType); 
                  {$I+}
               END; 
            END
            ELSE BEGIN
               {$I-}
               WRITELN(LogFile,'Parse error: line ',LineCount:0,
               ' - no identifier: ',ParamString); 
               {$I+}
               
            END
         END 
         ELSE BEGIN
            {$I-}
            WRITELN(LogFile,'Parse error: line ',LineCount:0,' - no open struct'); 
            {$I+}
         END; 
      END; 
   
   FUNCTION TSetupObj.ParseAlias;
      VAR   FieldName,
            RealName,
            IdentParam  : STRING; 
            SplitChar   : CHAR; 
            Alias       : BOOLEAN; 
            NSPtr,
            LSPtr       : TSeekFieldStructPtr; 
            ErrNum      : BYTE; 
            Count       : BYTE; 
            NewField    : TFieldStruct; 
            LPtr,
            NPtr        : TFieldStructPtr; 
      BEGIN
         ParseAlias := FALSE;
         IF (ActStruct.StructName <> '') THEN BEGIN
            SplitString(ParamString,':',FieldName,ParamString);
            IF ValidId(FieldName) THEN BEGIN
               SplitIdent(ParamString,SplitChar,RealName,IdentParam);
               IF FieldExists(RealName,ActStruct,LPtr,Alias) THEN BEGIN
                  IF NOT Alias THEN BEGIN
                     IF InsertIntoFieldNames(FieldName,SplitChar,IdentParam,RealName,
                        TRUE,LPtr,ActStruct.FieldNames)
                     THEN BEGIN
                        
                        ErrNum := 0; 
                        {$I-}
                        WRITELN(LogFile,'    alias: ',FieldName,' refers to: ',ParamString); 
                        {$I+}
                        ParseAlias := TRUE; 
                     END 
                     ELSE BEGIN
                        {$I-}
                        WRITELN(LogFile,'Parse error: line ',LineCount:0,
                        ' - not enough memory'); 
                        {$I+}
                        Status := 1; 
                     END; 
                  END 
                  ELSE BEGIN
                     ErrNum := 10; 
                     Status := ErrNum; 
                     {$I-}
                     WRITELN(LogFile,'Parse error: line ',LineCount:0,' - ',
                     'field name is an alias'); 
                     {$I+}
                  END; 
               END 
               ELSE BEGIN
                  ErrNum := 9; 
                  Status := ErrNum; 
                  {$I-}
                  WRITELN(LogFile,'Parse error: line ',LineCount:0,' - ',
                  'undefined field name '); 
                  {$I+}
               END
            END 
            ELSE BEGIN
               {$I-}
               WRITELN(LogFile,'Parse error: line ',LineCount:0,
               ' - no identifier: ',ParamString); 
               {$I+}
               
            END
         END
         ELSE BEGIN
            {$I-}
            WRITELN(LogFile,'Parse error: line ',LineCount:0,' - no open struct'); 
            {$I+}
         END; 
      END; 
   
   FUNCTION TSetupObj.AppendField; 
      VAR   SPTR : TStructPtr; 
            NFPtr,
            FPtr : TFieldStructPtr; 
            Alias: BOOLEAN; 
            
      BEGIN
         AppendField := FALSE; 
         SName := UPPER(SName); 
         IF StructExists(SName,SPTR) THEN BEGIN
            IF NOT FieldExists(Field.Identifier,SPTR^,FPtr,Alias) THEN BEGIN
               IF (MAXAVAIL > SIZEOF(Field)) THEN BEGIN
                  NEW(NFPtr); 
                  Field.NextField := NIL; 
                  Field.FieldOffSet := SPTR^.Len; 
                  SPTR^.Len := SPTR^.Len+Field.Len; 
                  NFPtr^ := Field; 
                  IF (FPtr = NIL) THEN
                     SPTR^.FieldList := NFPtr
                  ELSE
                     FPtr^.NextField := NFPtr;
                  AppendField := InsertIntoFieldNames(SName,#0,'','',FALSE,NFPtr,SPTR^.FieldNames);
               END
               ELSE BEGIN
                  Status :=1 ; 
               END; 
            END; 
         END
      END; 
   
   FUNCTION TSetupObj.ValidId; 
      VAR   Count : BYTE; 
            IDLen : BYTE; 
            Result : BOOLEAN; 
            
      FUNCTION IsChar(ChkChar:CHAR):BOOLEAN; 
         BEGIN
            IsChar := ChkChar IN [#65..#90,#97..#122]
         END; 
      FUNCTION IsValid(ChkChar:CHAR):BOOLEAN; 
         BEGIN
            IsValid := (ChkChar = '_') OR (ChkChar IN ['0'..'9']) OR
            IsChar(ChkChar); 
         END; 
      BEGIN
         Result := FALSE; 
         IdLen := LENGTH(Identifier); 
         IF (IdLen > 0) THEN BEGIN
            Result := IsChar(Identifier[1]); 
            Count := 2; 
            WHILE (Count <= IdLen) AND Result DO BEGIN
               Result := Result AND IsValid(Identifier[Count]); 
               INC(Count); 
            END; 
         END; 
         ValidId:= Result; 
      END; 
   
   
   FUNCTION TSetupObj.ParseLine; 
      VAR   Garbage,
            ReadString,
            ParseKey,
            ParseParam :STRING; 
            Result     :BYTE; 
            Found,
            LineOk,
            ContinueParse : BOOLEAN; 
            DPointPos,
            Lauf,
            CommentEnd,
            CommentBegin : BYTE; 
      BEGIN
         READLN(SetupFile,ReadString); 
         ParseString := ReadString; 
         INC(LineCount); 
         ContinueParse := NOT EOF(SetupFile); 
         CommentEnd   := 0; 
         CommentBegin := 0; 
         IF Comment THEN BEGIN
            SplitString(ParseString,'*/',Garbage,ParseString); 
            Comment := FALSE; 
         END; 
         CommentEnd := 0; 
         REPEAT
            CommentBegin := POS('/*',ParseString); 
            IF (CommentBegin > 0) THEN BEGIN
               CommentEnd := POS('*/',ParseString); 
               IF (CommentEnd = 0) THEN BEGIN
                  Comment := TRUE; 
                  CommentBegin := 0; 
               END; 
               IF (CommentEnd > 0) THEN
                  DELETE(ParseString,
                  CommentBegin,
                  (CommentEnd+2-CommentBegin)); 
            END; 
         UNTIL (CommentBegin = 0); 
         ParseString := Upper(RemoveAllSpaces(ParseString)); 
         IF (ParseString <> '') THEN BEGIN
            SplitString(ParseString,':',ParseKey,ParseParam); 
            LineOk := FALSE; 
            Lauf := 1; 
            Found := FALSE; 
            WHILE (Lauf <= MaxParseKey) AND NOT Found DO BEGIN
               IF (ParseKey = KeyWords[Lauf]) THEN BEGIN
                  CASE Lauf OF
                     1 :LineOk := ParseStructBegin(ParseParam); 
                     2 :LineOk := ParseStructEnd(ParseParam); 
                     3 :LineOk := ParseStructField(ParseParam); 
                     4 :LineOk := ParseAlias(ParseParam); 
                  END; 
                  Found := TRUE; 
               END; 
               INC(Lauf); 
            END; 
            IF NOT Found THEN BEGIN
               {$I-}
               WRITELN(LogFile,'Parse error: line ',LineCount:0,' - ',
               'undefined keyword'); 
               {$I+}
            END; 
            Result := 0; 
            IF NOT ContinueParse THEN
               INC(Result); 
            IF NOT LineOk THEN
               INC(Result,2); 
            IF NOT Found THEN
               INC(Result,4); 
            ParseLine := Result
         END
         ELSE BEGIN
            Result := 0; 
            IF NOT ContinueParse THEN BEGIN
               INC(Result); 
            END; 
            ParseLine := Result; 
         END; 
      END; 
   
   FUNCTION TSetupObj.ReadFile; 
      VAR   Weiter : BYTE; 
      BEGIN
         ParseString := ''; 
         REPEAT
            Weiter := ParseLine; 
         UNTIL (Weiter <> 0); 
         ReadFile := (Weiter-1)*10; 
      END; 

   PROCEDURE TSetupObj.CloseFile;
      VAR LocalError : INTEGER;
      BEGIN
         IF (Status = 0) THEN BEGIN
            {$I-}
            CLOSE(SetupFile);
            {$I+}
            LocalError := IORESULT;
            {$I-}
            WRITELN(LogFile,'Parse finished.');
            {$I+}
         END
         ELSE BEGIN
            {$I-}
            WRITELN(LogFile,'Parse break.');
            {$I+}
            IF (Status < 2) OR (Status > 3) THEN
               {$I-}
               CLOSE(SetupFile);
            {$I+}
            LocalError := IORESULT;
         END;
         {$I-}
         CLOSE(LogFile);
         {$I+}
         LocalError := IORESULT;
      END;

   PROCEDURE MakeStructField;
      BEGIN
         WITH Field DO BEGIN
            FName := Upper(FName);
            NextField := NIL; 
            Identifier := FName; 
            FieldOffSet := 0; 
            FieldType := 15; 
            ElemType := 0; 
            ElemSize := Struct.Len; 
            ElemNo   := 1; 
            ElemStructPtr := NIL; 
            Len      := Struct.Len; 
         END; 
      END; 
   
   DESTRUCTOR TSetupObj.Done; 
      VAR   DPtr,
            LPtr : TStructPtr; 
            
      PROCEDURE DisposeFields(SPTR:TStructPtr); 
         VAR   DPtr,
               LPtr : TFieldStructPtr; 
               Lauf : BYTE; 
               LsPtr,
               DsPtr: TSeekFieldStructPtr; 
         BEGIN
            Lptr := SPTR^.FieldList; 
            WHILE (Lptr <> NIL) DO BEGIN
               Dptr := Lptr; 
               Lptr := Lptr^.NextField; 
               DISPOSE(DPtr); 
            END; 
            SPTR^.FieldList := NIL; 
            FOR Lauf := 65 TO 90 DO BEGIN
               LsPtr := SPTR^.FieldNames[Lauf]; 
               WHILE (LsPtr <> NIL) DO BEGIN
                  Dsptr := Lsptr; 
                  Lsptr := Lsptr^.NextField; 
                  DISPOSE(DsPtr); 
               END; 
               SPTR^.FieldNames[Lauf] := NIL
            END; 
         END; 
      BEGIN
         Lptr := StructList; 
         WHILE (Lptr <> NIL) DO BEGIN
            Dptr := Lptr; 
            Lptr := Lptr^.NextStruct; 
            DisposeFields(Dptr); 
            DISPOSE(DPtr); 
         END; 
         StructList :=NIL; 
      END; 
   
   END.
{============================
 Versionshistorie
 $Log:$
 ============================}
