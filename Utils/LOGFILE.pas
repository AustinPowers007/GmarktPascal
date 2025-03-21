{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 16:41:30 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT LogFile;

INTERFACE

   TYPE TLogFilePtr = ^TLogFile;
        TLogFile=OBJECT
           CONSTRUCTOR Init;
           PROCEDURE OpenFile(FName:STRING);VIRTUAL;
           PROCEDURE SetAppend(Value:BOOLEAN);VIRTUAL;
           PROCEDURE SetClose(Value:BOOLEAN);VIRTUAL;

           PROCEDURE StartLog;VIRTUAL;
           PROCEDURE RestartLog;VIRTUAL;
           FUNCTION  GetErrNum:BYTE;VIRTUAL;
           FUNCTION  WriteLnLog(Txt:STRING):BOOLEAN;VIRTUAL;
           FUNCTION  WriteLog(Txt:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE PrintFile;VIRTUAL;
           PROCEDURE CloseFile;VIRTUAL;
           DESTRUCTOR Done;
           PRIVATE
              IsOpen     : BOOLEAN;
              FileAppend : BOOLEAN;
              FileClose  : BOOLEAN;
              LogFile    : TEXT;
              Err        : BYTE;
        END;



IMPLEMENTATION
   USES Dos,
        Zusaetze,
        PRINTER;

   CONSTRUCTOR TLogFile.Init;

      BEGIN
         Err := 0;
         IsOpen := FALSE;
         FileAppend := FALSE;
         FileClose  := FALSE;
      END;

   PROCEDURE TLogFile.SetAppend;
      BEGIN
         FileAppend := Value;
      END;
   PROCEDURE TLogFile.SetClose;
      BEGIN
         FileClose := Value;
      END;

   PROCEDURE TLogFile.OpenFile;
      VAR Name:STRING;
      BEGIN
         Name := MakeFileName(Fname,'LOG');
         {$I-}
            ASSIGN(LogFile,Name);
         {$I+}
         IF (FileAppend AND SeekFile(Name)) THEN BEGIN
            IF NOT FileClose THEN
               StartLog
         END ELSE
            RestartLog;
      END;

   PROCEDURE TLogFile.StartLog;
      BEGIN
         {$I-}
            APPEND(LogFile);
         {$I+}
         IsOpen := (IORESULT = 0);
         IF (IORESULT <> 0) THEN
            Err := 1;
      END;

   PROCEDURE TlogFile.RestartLog;
      BEGIN
         {$I-}
            REWRITE(LogFile);
         {$I+}
         IsOpen := (IORESULT = 0);
         IF (IORESULT <> 0) THEN
            Err := 1;
         IF FileClose THEN
             CloseFile;
      END;


   FUNCTION TLogFile.GetErrNum;
      BEGIN
         GetErrNum := Err;
      END;

   FUNCTION TLogFile.WriteLog;
      BEGIN
         IF FileClose AND (NOT IsOpen) THEN
            StartLog;
         IF (Err = 0) THEN BEGIN
            {$I-}
            WRITE(LogFile,Txt);
            {$I+}
            IF (IORESULT <> 0) THEN
               Err := 2;
         END;
         IF FileClose AND IsOpen THEN
            CloseFile;
         WriteLog := (Err = 0);

      END;

   FUNCTION TLogFile.WriteLnLog;
      BEGIN
         IF FileClose AND (NOT IsOpen) THEN
            StartLog;
         IF (Err = 0) THEN BEGIN
            {$I-}
            WRITELN(LogFile,Txt);
            {$I+}
            IF (IORESULT <> 0) THEN
               Err := 2;
         END;
         IF FileClose AND IsOpen THEN
            CloseFile;
         WriteLnLog := (Err = 0);
      END;

   PROCEDURE TLogFile.CloseFile;
      BEGIN
         {$I-}
            CLOSE(LogFile);
         {$I+}
         IsOpen := (IORESULT <> 0);
         IF IsOpen THEN
            Err := 3;
      END;

   PROCEDURE TLogFile.PrintFile;
      VAR GlobalOpen : BOOLEAN;
          AktLine    : STRING;
          d          : BOOLEAN;
      BEGIN
         GlobalOpen := IsOpen;
         IF IsOpen THEN
            CloseFile;
         IF NOT IsOpen THEN BEGIN
            {$I-}
            RESET(LogFile);
            {$I+}
            D:= (IORESULT = 0);
            WHILE NOT EOF(LogFile) DO BEGIN
                {$I-}
                READLN(LogFile,aktline);
                {$I+}
                D:= (IORESULT = 0);
                {$I-}
                   WRITELN(LST,aktLine);
                {$I+}
                D:= (IORESULT = 0);
            END;
            {$I-}
              WRITE(LST,CHR(12));
            {$I+}
            CloseFile;
         END;
         IF GlobalOpen THEN
            StartLog;
      END;


   DESTRUCTOR TLogFile.Done;
      BEGIN
         IF IsOpen THEN
            CloseFile;
      END;





END.
{============================
 Versionshistorie
 $Log:$
 ============================}
