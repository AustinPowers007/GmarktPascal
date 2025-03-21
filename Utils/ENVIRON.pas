{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Feb 13 12:20:06 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT ENVIRON;
   {$N+}
INTERFACE

   USES  LogFile;
   CONST KeyStartChar = '%';
   CONST MaxKeywords  = 50;
   CONST MaxKType     = 4;
         { Typen  1= String
                  2= Number
                  3= Pfad
                  4= WahrheitsWert (Nur 0 oder 1)}
   TYPE  TEnvIdent    = STRING[15];
         TEnvFieldPtr = ^TEnvField;
         TEnvField = RECORD
                           NextField   : TEnvFieldPtr;
                           Identifier  : TEnvIdent;
                           FieldValue  : STRING;
                        END;


   TYPE  TEnvObjPtr = ^TEnvObj;
         TEnvObj = OBJECT
                        CONSTRUCTOR Init;
                        FUNCTION  AddKey(Kword:STRING;KType:BYTE):BOOLEAN;
                        PROCEDURE GetSetup(FileName:STRING);
                        FUNCTION  GetEntry(Keyword:STRING):STRING;
                        FUNCTION  GetErrNum:BYTE; VIRTUAL;
                        FUNCTION  WarningFound:BOOLEAN;VIRTUAL;
                        DESTRUCTOR Done; VIRTUAL;
                           PRIVATE
                           KeyWords       : ARRAY[1..MaxKeywords] OF TEnvIdent;
                           KeyType        : ARRAY[1..MaxKeywords] OF BYTE;
                           MaxKeys        : BYTE;
                           Status         : BYTE;
                           SetupFileName  : STRING;
                           SetupFile      : TEXT;
                           LogFile        : TLogFile;
                           FieldList      : TEnvFieldPtr;
                           ParseString    : STRING;
                           Warning        : BOOLEAN;
                           Comment        : BOOLEAN;
                           OffSet         : WORD;
                           LineCount      : WORD;

                        PROCEDURE OpenFile; VIRTUAL;
                        FUNCTION  ParseLine:BYTE; VIRTUAL;
                        FUNCTION  ReadFile:BYTE; VIRTUAL;
                        FUNCTION  ParseEntry(Keyword,ParamString:STRING;PType:BYTE):BOOLEAN; VIRTUAL;
                        FUNCTION  AddEntry(Keyword,ParamString:STRING):BOOLEAN; VIRTUAL;
                        PROCEDURE CloseFile; VIRTUAL;
                     END;

IMPLEMENTATION

   USES  ZUSAETZE;


   CONSTRUCTOR TEnvObj.Init;
      VAR   Lauf : BYTE;
      BEGIN
         LogFile.Init;
         ParseString :='';
         LineCount := 0;
         Status := 0;
         FieldList := NIL;
         Comment := FALSE;
         OffSet := 0;
         MaxKeys := 0;
         FOR Lauf := 1 TO MaxKeywords DO BEGIN
            Keywords[Lauf] := '';
            KeyType[Lauf] := 0;
         END;
         Warning := FALSE;
      END;

   FUNCTION TEnvObj.AddKey;
      VAR Ergebnis : BOOLEAN;
          Found    : BOOLEAN;
          Lauf     : BYTE;
          Kstring  : TEnvIdent;
      BEGIN
         Ergebnis := FALSE;
         IF (MaxKeys < MaxKeywords) AND (KType >0)
            AND (KType <= MaxKType) THEN BEGIN
            KString := KeyStartChar+KWord;
            Found := FALSE;
            Lauf := 0;
            WHILE (Lauf < MaxKeys) AND (NOT Found) DO BEGIN
               INC(Lauf);
               Found := (KString = Keywords[Lauf]);
            END;
            Ergebnis := NOT Found;
            IF Ergebnis THEN BEGIN
               INC(MaxKeys);
               Keywords[MaxKeys] := KString;
               KeyType[MaxKeys] := KType;
            END;
         END;
         AddKey := Ergebnis;
      END;

   PROCEDURE TEnvObj.GetSetup;
      VAR   Result :BYTE;
      BEGIN
         IF (Status = 0) THEN BEGIN
            Warning := FALSE;
            SetupFileName := MakeFileName(FileName,'INI');
            OpenFile;
            IF (Status = 0) THEN
               ReadFile;
            CloseFile;
         END;
      END;

   FUNCTION TEnvObj.GetErrNum;
      BEGIN
         GetErrNum := Status;
      END;


   FUNCTION TEnvObj.WarningFound;
      BEGIN
         WarningFound := Warning;
      END;


   PROCEDURE TEnvObj.OpenFile;
      VAR LocalError : INTEGER;
      BEGIN
         LogFile.OpenFile(CutSuffix(SetupFileName));
         LogFile.WriteLnLog('Parsing environment file: '+SetupFileName);
         LogFile.WriteLnLog('');
         IF SeekFile(SetupFileName)THEN BEGIN
            {$I-}
            ASSIGN (SetupFile,SetupFileName);
            RESET  (SetupFile);
            {$I+}
            LocalError := IORESULT;
            IF (LocalError <> 0) THEN BEGIN
               Status := 2;
               LogFile.WritelnLog('Parse error: unable to open file ');
            END;
         END
         ELSE BEGIN
            Status := 3;
            LogFile.WritelnLog('Parse error: file not found ');
         END;
      END;

   FUNCTION TEnvObj.GetEntry;
      VAR Ergebnis : STRING;
          FPtr : TEnvFieldPtr;
      BEGIN
         Ergebnis := '';
         KeyWord := KeyStartChar+UPPER(KeyWord);
         FPTR := FieldList;
         WHILE (FPTR <> NIL) AND (FPTR^.Identifier<> Keyword) DO
            FPTR := FPTR^.NextField;
         IF (FPtr <> NIL) THEN BEGIN
            Ergebnis := FPtr^.FieldValue;
         END;
         GetEntry := Ergebnis;
      END;

   FUNCTION TEnvObj.AddEntry;
      VAR FPtr,NPtr: TEnvFieldPtr;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         FPtr := FieldList;
         WHILE (FPtr <> NIL) AND (FPtr^.Identifier <> Keyword) DO
           FPtr := FPtr^.NextField;
         IF (FPTr <> NIL) THEN BEGIN
            FPtr^.FieldValue := ParamString;
            Ergebnis := TRUE;

         END ELSE BEGIN
            IF (MAXAVAIL < SIZEOF(TEnvField)) THEN BEGIN
               LogFile.WritelnLog('Parse error: line'+L2S(LineCount,0)+
                                    ' - not enough memory ');
            END ELSE BEGIN
               NEW(NPtr);
               NPtr^.NextField := FieldList;
               NPtr^.Identifier := Keyword;
               NPtr^.FieldValue := Paramstring;
               FieldList :=NPtr;
               Ergebnis := TRUE;
            END;
         END;
         IF Ergebnis THEN BEGIN
             LogFile.WritelnLog(Keyword+' set to '+Paramstring);
         END;
         AddEntry := Ergebnis;
      END;

   FUNCTION TEnvObj.ParseEntry;
      VAR   Ergebnis    : BOOLEAN;
      BEGIN
         CASE PType OF
            1: Ergebnis := TRUE;
            2: Ergebnis := IsNumber(ParamString);
            3: BEGIN
                  Ergebnis := TRUE;
                  Warning := NOT ValidPath(ParamString);
               END;
            4: BEGIN
                  Ergebnis := IsNumber(ParamString);
                  IF Ergebnis THEN BEGIN
                     Ergebnis :=(S2L(ParamString) >= 0) AND
                                (S2L(ParamString) <= 1);
                  END;
               END;
         END;
         IF Ergebnis THEN BEGIN
            Ergebnis :=  AddEntry(Keyword,ParamString);
            IF Warning THEN BEGIN
               CASE PType OF
                  1 : ;
                  2 : ;
                  3 : BEGIN
                         LogFile.WritelnLog('Warning: line'+L2S(LineCount,0)+
                                ' - '+ParamString+' path not found');
                      END;
                  4 : ;
               END;
            END;
         END ELSE BEGIN
            CASE PType OF
               1 : ;
               2 : BEGIN
                     LogFile.WritelnLog('Parse error: line'+L2S(LineCount,0)+
                     ' - '+ParamString+' number expected');
                   END;
               2 : BEGIN
                     LogFile.WritelnLog('Parse error: line'+L2S(LineCount,0)+
                     ' - '+ParamString+' 0 or 1 expected');
                   END;
               3 : BEGIN
                     LogFile.WritelnLog('Parse error: line'+L2S(LineCount,0)+
                     ' - '+ParamString+' not a path');
                   END;
               END;
         END;
         ParseEntry:= Ergebnis;
      END;




   FUNCTION TEnvObj.ParseLine;
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
      FUNCTION PrepareLine(aktLine:STRING):STRING;
          VAR StartOfLine: STRING;
          VAR EndOfLine:STRING;
          VAR Rest:STRING;
          VAR TextPart:STRING;

          BEGIN
             StartOfLine:= '';
             TextPart:= '';
             EndOfLine:= '';
             SplitString(aktLine,#39,StartOfLine,Rest);
             IF Rest <> '' THEN BEGIN
                SplitString(Rest,#39,TextPart,EndOfLine);
                IF LENGTH(Rest) = LENGTH(TextPart) THEN BEGIN
                   StartOfLine:= StartOfLine+CHR(39)+Rest;
                   TextPart := '';
                   EndOfLine := '';
                END;
             END;
             PrepareLine:=Upper(RemoveAllSpaces(StartOfLine))+TextPart+
                          Upper(RemoveAllSpaces(EndOfLine));
          END;





      BEGIN
         {$I-}
            READLN(SetupFile,ReadString);
         {$I+}
         INC(LineCount);
         IF (IOResult <> 0) THEN BEGIN
            LogFile.WritelnLog('Parse error: line '+L2S(LineCount,0)+' - '+
               'unable to read line');
         END;
         ParseString := ReadString;
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
         ParseString := PrepareLine(ParseString);
         IF (ParseString <> '') THEN BEGIN
            SplitString(ParseString,'=',ParseKey,ParseParam);
            LineOk := FALSE;
            Lauf := 1;
            Found := FALSE;
            WHILE (Lauf <= MaxKeys) AND NOT Found DO BEGIN
               IF (ParseKey = KeyWords[Lauf]) THEN BEGIN
                  LineOk := ParseEntry(ParseKey,ParseParam,KeyType[Lauf]);
                  Found := TRUE;
               END;
               INC(Lauf);
            END;
            IF NOT Found THEN BEGIN
               LogFile.WritelnLog('Parse error: line '+L2S(LineCount,0)+' - '+
               'undefined keyword');
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

   FUNCTION TEnvObj.ReadFile;
      VAR   Weiter : BYTE;
      BEGIN
         ParseString := '';
         REPEAT
            Weiter := ParseLine;
         UNTIL (Weiter <> 0);
         ReadFile := (Weiter-1)*10;
      END;

   PROCEDURE TEnvObj.CloseFile;
      VAR LocalError : INTEGER;
      BEGIN
         IF (Status = 0) THEN BEGIN
            {$I-}
            CLOSE(SetupFile);
            {$I+}
            LocalError := IORESULT;
            LogFile.WritelnLog('Parse finished.');
         END
         ELSE BEGIN
            LogFile.WritelnLog('Parse break.');
            IF (Status < 2) OR (Status > 3) THEN
               {$I-}
               CLOSE(SetupFile);
               {$I+}
            LocalError := IORESULT;
         END;
         LogFile.CLOSEFile;
         LocalError := IORESULT;
      END;



   DESTRUCTOR TEnvObj.Done;
      VAR DPtr,LPtr :TEnvFieldPtr;
      BEGIN
         Lptr := FieldList;
         WHILE (Lptr <> NIL) DO BEGIN
            Dptr := Lptr;
            Lptr := Lptr^.NextField;
            DISPOSE(DPtr);
         END;
         FieldList :=NIL;
         Logfile.Done;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
