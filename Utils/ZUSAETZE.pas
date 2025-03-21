{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Feb 13 11:25:34 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT Zusaetze;
   {$N+,E+}

INTERFACE

   CONST CJahrhundertwechsel = 40;
         { Bis zu dem das aktuelle Jahrhundert verwendet wird,
           alle Jahresangaben oberhalb dieser Zahl werden in das
           vorherige Jahrhundert verlegt. Aktuelles Beispiel:
           Das Jahr 40 wird als 2040 verwendet, das Jahr 41 als 1941 }

   TYPE  TDatum = STRING[8];
         { Typ zur Darstellung von Datum}

         TZeit       = STRING[5];
         { Typ zur Darstellung von Zeit}

         TWeek = STRING[5];
         { Typ zur Darstellung von Kalenderwochen}


   FUNCTION BufBlockRead(VAR InFile:FILE; Buffer:POINTER;
                            COUNT:WORD; VAR Result:WORD):BYTE;
   { Liest Bytes aus Datei in Speicherbereich ein}
   FUNCTION BufBlockWrite(VAR OutFile:FILE; Buffer:POINTER;
                             COUNT:WORD; VAR Result:WORD):BYTE;
   { Schreibt Bytes aus Speicherbereich in Datei }


   FUNCTION EraseFile(Name:STRING):BOOLEAN;
   { L�scht die Datei }

   FUNCTION RenameFile(Name,NeuerName:STRING):BOOLEAN;
   { Benennt Datei um }

   FUNCTION NeededCopySpace(Source,Dest:STRING):LONGINT;
   { Gibt den ben�tigten Speicherplatz an }

   FUNCTION CopyFile(Name,NeuerName:STRING):BYTE;

      {Kopiert die angegebene Datei
       Ergebnis  des Kopiervorgangs
             0 : Datei kopiert
             1 : Nicht genug Speicherplatz
             2 : Falscher Sourcepfad/Name
             3 : Falscher Destpfad/Name }

   FUNCTION SeekFile(Name : STRING):BOOLEAN;
     { Sucht die angegebene Datei }

   FUNCTION IsPattern(Name : STRING):BOOLEAN;
     { �berpr�ft ob der �bergebene Dateiname Wildcards enth�lt }

   FUNCTION ValidPath(Pfad : STRING):BOOLEAN;
     { �berpr�ft ob der �bergebene Pfad auf ein g�ltiges Verzeichnis zeigt }

   FUNCTION MakeFilePath(Pfad : STRING):STRING;
     { Erzeugt aus einem Pfad einen Pfad mit abschliessendem Backslash,
       falls notwendig }

   FUNCTION ValidDrive(Drive :STRING):BOOLEAN;
     { �berpr�ft ob das angegebene Laufwerk g�ltig ist }

   FUNCTION SizeofFile(Name : STRING):LONGINT;
     { Liefert die Gr��e der Datei }

   FUNCTION FileDate(Name : STRING):STRING;
     { Liefert Datum der letzten �nderung }

   FUNCTION FileTime(Name : STRING):STRING;
     { Liefert Uhrzeit der letzten �nderung }

   FUNCTION SpaceOfDisk(Path: STRING):LONGINT;
     { Liefert den freien Speicher auf dem im Pfad angegebenen Laufwerk }

   FUNCTION CorrectSize(Name:STRING; RecLength:LONGINT):BOOLEAN;
     { �berpr�ft ob die Datei Records mit der angegebenen Gr��e
       enthalten kann }

   FUNCTION NumberOfRecs(Name:STRING; RecLength:LONGINT):LONGINT;
     { Liefert die Anzahl der Records der Gr��e RECLENGTH in der Datei }

   FUNCTION MakeFileName(FileName,Suffix:STRING):STRING;
     { Erzeugt aus dem Namen und dem Suffix einen Dateiname.
       Die Erweiterung durch den Suffix erfolgt nur wenn der Dateiname
       keinen Suffix enth�lt }

   FUNCTION CutSuffix(Filename:STRING):STRING;
     { Entfernt den Suffix von einem Dateinamen }

   FUNCTION CutPath(FileName:STRING):STRING;
     {L�scht den Pfad aus einem Dateinamen }

   FUNCTION GetPath(FileName:STRING):STRING;
     { Liefert den Pfad ohne Dateiname }

   FUNCTION ExtendFileName(FileName,StdPath:STRING):STRING;
     { Erzeugt einen Zugriffspfad aus Dateiname und
       Standardpfad }


   FUNCTION PrintReal(Nummer:REAL; Vork,NachK:BYTE;SeperateThousand:BOOLEAN):STRING;
      { Formatierte Ausgabe von Real Zahlen }

   FUNCTION PrintReal0(Nummer:REAL; Vork,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Real Zahlen }

   FUNCTION PrintRealOrLine(Nummer:REAL; VorK,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Real Zahlen }

   FUNCTION PrintDouble(Nummer:DOUBLE; Vork,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Double Zahlen }

   FUNCTION PrintDouble0(Nummer:DOUBLE; Vork,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Double Zahlen }

   FUNCTION PrintDoublePoint(Nummer:DOUBLE; Vork,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Double Zahlen }

   FUNCTION PrintDoublePoint0(Nummer:DOUBLE; Vork,NachK:BYTE):STRING;
      { Formatierte Ausgabe von Double Zahlen }

   FUNCTION PrintLong(Nummer:LONGINT; Stellen:BYTE):STRING;
      { Formatierte Ausgabe von Long Zahlen }

   FUNCTION PrintLong0(Nummer:LONGINT; Stellen:BYTE):STRING;
      { Formatierte Ausgabe von Long Zahlen }

   FUNCTION PrintLongOrLine(Nummer:LONGINT; Stellen:BYTE):STRING;
      { Formatierte Ausgabe von Long Zahlen }

   FUNCTION PrintLongOrMinus(Nummer:LONGINT; Stellen:BYTE):STRING;
      { Formatierte Ausgabe von Long Zahlen }

   FUNCTION PrintString(Txt:STRING; Stellen:BYTE):STRING;
      { Linksb�ndige Ausgabe von Strings }

   FUNCTION LeadingZeros(Wert:LONGINT; Anzahl:BYTE):STRING;
      { Erzeugt einen String der maximal <Anzahl> f�hrende
        Nullen enth�lt }

   FUNCTION OnlyNumber(Wert:STRING):STRING;
      { L�scht alle Leerstellen und die f�hrenden Nullen in einem String}

   FUNCTION HexNum(Wert:LONGINT; Stellen:BYTE):STRING;
     { erzeugt eine Heximaldarstellung des �bergebenen Wertes }

   FUNCTION BinNum(Wert:LONGINT; Stellen:BYTE):STRING;
     { erzeugt eine Bin�rdarstellung des �bergebenen Wertes }

   FUNCTION R2S(Wert:REAL; Stellen,NachK:BYTE):STRING;
     {Wandelt Real-Wert in Strings um }

   FUNCTION E2S(Wert:EXTENDED; Stellen,NachK:BYTE):STRING;
     {Wandelt Extended-Wert in Strings um }

   FUNCTION C2S(Wert:COMP; Stellen,NachK:BYTE):STRING;
     {Wandelt Compressed-Wert in Strings um }

   FUNCTION SG2S(Wert:SINGLE; Stellen,NachK:BYTE):STRING;
     {Wandelt Single-Wert in Strings um }

   FUNCTION D2S(Wert:DOUBLE; Stellen,NachK:BYTE):STRING;
     {Wandelt Double-Wert in Strings um }

   FUNCTION L2S(Wert:LONGINT; Stellen:BYTE):STRING;
     {Wandelt Long-Wert in Strings um }

   FUNCTION B2S(Wert:BYTE; Stellen:BYTE):STRING;
     {Wandelt Byte-Wert in Strings um }

   FUNCTION W2S(Wert:WORD; Stellen:BYTE):STRING;
     {Wandelt Word-Wert in Strings um }

   FUNCTION SI2S(Wert:SHORTINT; Stellen:BYTE):STRING;
     {Wandelt Shortint-Wert in Strings um }

   FUNCTION I2S(Wert:INTEGER; Stellen:BYTE):STRING;
     {Wandelt Integer-Wert in Strings um }

   FUNCTION S2L(Txt:STRING):LONGINT;
     {Wandelt String in Long-Wert um }

   FUNCTION S2B(Txt:STRING):BYTE;
     {Wandelt String in Byte-Wert um }

   FUNCTION S2W(Txt:STRING):WORD;
     {Wandelt String in Byte-Wert um }

   FUNCTION S2SI(Txt:STRING):SHORTINT;
     {Wandelt String in Shortint-Wert um }

   FUNCTION S2I(Txt:STRING):INTEGER;
     {Wandelt String in Integer-Wert um }

   FUNCTION S2R(Txt:STRING):REAL;
     {Wandelt String in Real-Wert um }

   FUNCTION S2D(Txt:STRING):DOUBLE;
     {Wandelt String in Double-Wert um }

   FUNCTION S2E(Txt:STRING):EXTENDED;
     {Wandelt String in Extended-Wert um }

   FUNCTION S2SG(Txt:STRING):SINGLE;
     {Wandelt String in Single-Wert um }

   FUNCTION S2C(Txt:STRING):COMP;
     {Wandelt String in Compressed-Wert um }

   FUNCTION H2W(Wert:String):WORD;
     {Wandelt Hexadezimalstering in Word-Wert um }

   FUNCTION R2L(Wert:REAL):LONGINT;
     {Wandelt Real-Wert in Longint-Wert um }

   FUNCTION RoundDbl(Wert:DOUBLE; NachK:BYTE):DOUBLE;
     { Rundet Double-Wert auf angegebene Anzahl Nachkommastellen }

   FUNCTION RoundReal(Wert:REAL; NachK:BYTE):REAL;
     { Rundet Real-Wert auf angegebene Anzahl Nachkommastellen }

   FUNCTION Bittest(TestZahl:LONGINT;BitNummer:BYTE):BOOLEAN;
     { Testet ob angegebenes Bit in der Testzahl gesetzt ist }

   FUNCTION Replicate(Zeichen:CHAR; Anz:BYTE):STRING;
     { Erzeugt eine String aus <Anz>  Zeichen <Zeichen> }

   FUNCTION MidSpace(Txt:STRING):BYTE;
     { Liefert die Position innerhalb eines String die sich am
       n�chsten an der Mitte befindet und eine Leertaste enth�lt }


   FUNCTION FirstMaskChar(Txt:STRING):BYTE;
     { Liefer das Position des ersten Maskenzeichen eines Strings }

   FUNCTION FillUp(FILLCHAR:CHAR; InpString:STRING;
                      MaxLength,MaxFill:BYTE):STRING;
     { F�llt den �bergebenen String bis zur maximalen Gr��e <Maxlength>
       oder der maximalen F�llmenge <Maxfill> links mit dem FILLCHAR auf }

   FUNCTION FillUpRight(FILLCHAR:CHAR; InpString:STRING;
                           MaxLength,MaxFill:BYTE):STRING;
     { F�llt den �bergebenen String bis zur maximalen Gr��e <Maxlength>
       oder der maximalen F�llmenge <Maxfill> rechts mit dem FILLCHAR auf }

   FUNCTION ReplaceChar(OldChar,NewChar:CHAR;InpString:STRING):STRING;
     { Ersetzt alle Vorkommen von <Oldchar> dur <Newchar> }

   FUNCTION CutLSpaces(Txt:STRING):STRING;
     { L�scht alle f�hrenden Leertasten }

   FUNCTION CutRSpaces(Txt:STRING):STRING;
     { L�scht alle Leertasten hinter dem letzten anderen Zeichen }

   FUNCTION CutBSpaces(Txt:STRING):STRING;
     { L�scht f�hrende und nachfolgende Leertasten }

   FUNCTION RemoveAllSpaces(Txt:STRING):STRING;
     { L�scht alle Leertasten im String }

   FUNCTION CenterString(Txt:STRING;MaxLength:BYTE):STRING;
     { Zentriert Txt in einem String der L�nge Maxlength }

   FUNCTION RightPos(SubStr,SearchStr:STRING):BYTE;
     { Sucht das letzte Auftreten von SubStr in Searchstr und liefert
       dessen Anfangsposition }

   FUNCTION LeftStr(Txt:STRING; Last:BYTE):STRING;
     { Liefert ersten <Last> Zeichen des Strings Txt }

   FUNCTION RightStr(Txt:STRING; First:BYTE):STRING;
     { Liefert letzten <First> Zeichen des Strings Txt }

   PROCEDURE SplitString(Txt,Delimiter:STRING; VAR Front,Rest:STRING);
      { Teilt den String Txt am ersten Auftreten von Delimiter,
        wenn Delimiter nicht auftritt, wird der komplette Text in Front #
        geliefert }

{   PROCEDURE SplitString3(Txt,Delimiter:STRING; VAR Front,R1,R2,R3:STRING);}
      { Teilt den String Txt in drei Teile }

   PROCEDURE SplitIdent(Txt:STRING; VAR SplitChar:CHAR; VAR Front,Rest:STRING);
      { Teilt den String Txt am ersten Auftreten von Splitchar }


   FUNCTION Upper(Txt:STRING):STRING;
      { Wandelt Kleinbuchstaben in Gro�bustaben um  }
   FUNCTION Lower(Txt:STRING):STRING;
      { Wandelt Kleinbuchstaben in Gro�bustaben um  }
   FUNCTION LowCase(CH:Char):CHAR;
      { Wandelt Kleinbuchstaben in Gro�bustaben um  }


   FUNCTION IsNumber(PString:STRING):BOOLEAN;
     { Pr�ft ob String  eine Zahl ist }

   FUNCTION IsFloat(PString:STRING):BOOLEAN;
     { Pr�ft ob String  eine Gleitkommazahl ist}

   PROCEDURE SetDefStr(VAR Txt:STRING; Def:STRING);
     { Belegt die Variable Txt mit Wert def falls Txt leer ist}
   PROCEDURE SetDefChr(VAR Txt:CHAR; Def:CHAR);
     { Belegt die Variable Txt mit Wert def falls Txt leer ist}
   PROCEDURE SetDefByte(VAR Zahl:BYTE; Def:BYTE);
     { Belegt die Variable Zahl mit Wert def falls Zahl = 0 ist}
   PROCEDURE SetDefLong(VAR Zahl:LONGINT; Def:LONGINT);
     { Belegt die Variable Zahl mit Wert def falls Zahl = 0 ist}
   PROCEDURE SetDefWord(VAR Zahl:WORD; Def:WORD);
     { Belegt die Variable Zahl mit Wert def falls Zahl = 0 ist}
   PROCEDURE SetDefReal(VAR Zahl:REAL; Def:REAL);
     { Belegt die Variable Zahl mit Wert def falls Zahl = 0 ist}
   PROCEDURE SetDefDouble(VAR Zahl:Double; Def:Double);
     { Belegt die Variable Zahl mit Wert def falls Zahl = 0 ist}

   FUNCTION ActTime:TZeit;
     { Liefert die aktuelle Zeit im Format SS:MM}

   FUNCTION ActSysTime:STRING;
     { Liefert die aktuelle Systemzeit im Format SS:MM:SS}

   FUNCTION ActDate:TDatum;
     { Liefert das aktuelle Datum im Format TT.MM.JJ}

   FUNCTION Date2DBIndex(Datum:STRING):STRING;
     { Erzeugt aus einem Datum der Form TT.MM.JJ eine
       Datenbankindexdarstellung der Form JJJJMMTT }

   FUNCTION DBIndex2Date(DBIndex:STRING):STRING;
     { Erzeugt aus einer Datenbankindexdarstellung der
       Form JJJJMMTT ein Datum der Form TT.MM.JJ }

   FUNCTION ValidTime(Uhrzeit:TZeit):BOOLEAN;
     { �berpr�ft ob die Uhrzeit eine zul�ssige Zeitangabe ist}

   FUNCTION ValidDate(Datum:TDatum):BOOLEAN;
     { �berpr�ft ob das Datum zul�ssig ist}

   FUNCTION ValidWeek(Week:TWeek):BOOLEAN;
     { �berpr�ft ob Week eine zul�ssige Kalenderwoche ist}

   FUNCTION PrevHour(Time:TZeit):STRING;
     { Liefert die vorhergehende Stunde }

   FUNCTION NextHour(Time:TZeit):STRING;
     { Liefert die n�chste Stunde }

   FUNCTION DayOfWeek(Datum:TDatum):BYTE;
     { Liefert den Wochentag einer Datumsangabe }

   FUNCTION DayOfWeekStr(Datum:TDatum):STRING;
     { Liefert den Wochentagtext einer Datumsangabe }

   FUNCTION NewDayOfWeek(Datum:TDatum):BYTE;
     { Liefert den Wochentag einer Datumsangabe }

   FUNCTION NextDay(Datum:TDatum):STRING;
     { Liefert den n�chsten Tag  }

   FUNCTION NextWorkDay(Datum:TDatum):STRING;
     { Liefert den n�chsten Werktag  }

   FUNCTION PrevDay(Datum:TDatum):STRING;
     { Liefert den vorhegehenden Tag  }

   FUNCTION PrevWorkDay(Datum:TDatum):STRING;
     { Liefert den vorhegehenden Werktag  }

   FUNCTION IsWorkDay(Datum:TDatum):BOOLEAN;
      { �berpr�ft ob der Tag ein Werktag ist }

   FUNCTION Earlier   (Day1,Day2:TDatum):BOOLEAN;
      { Pr�ft ob Day1 < Day2 ist }

   FUNCTION NotEarlier(Day1,Day2:TDatum):BOOLEAN;
      { Pr�ft ob Day1 >= Day2 ist }

   FUNCTION Later     (Day1,Day2:TDatum):BOOLEAN;
      { Pr�ft ob Day1 > Day2 ist }

   FUNCTION NotLater  (Day1,Day2:TDatum):BOOLEAN;
      { Pr�ft ob Day1 <= Day2 ist }

   FUNCTION SameDate  (Day1,Day2:TDatum):BOOLEAN;
      { Pr�ft ob Day1 = Day2 ist }

   FUNCTION EarlierTime(Time1,Time2:TZeit):BOOLEAN;
      { Pr�ft ob Time1 < Time2 ist }

   FUNCTION NotEarlierTime(Time1,Time2:TZeit):BOOLEAN;
      { Pr�ft ob Time1 >= Time2 ist }

   FUNCTION LaterTime    (Time1,Time2:TZeit):BOOLEAN;
      { Pr�ft ob Time1 > Time2 ist }

   FUNCTION NotLaterTime  (Time1,Time2:TZeit):BOOLEAN;
      { Pr�ft ob Time1 <= Time2 ist }

   FUNCTION SameTime  (Time1,Time2:TZeit):BOOLEAN;
      { Pr�ft ob Time1 = Time2 ist }
   FUNCTION CompareTime(Time1,Time2:TZeit):LONGINT;
      { Vergleicht Time1 und Time2 und gibt die Anzahl der Minuten
        zwischen den beiden Zeitpunkten zur�ck }


   FUNCTION ValidMonth(Month:STRING):BOOLEAN;
     { Pr�ft ob der �bergebene String eine zul�ssige Monatsangabe
       ist }

   FUNCTION NextMonth(ActMonth:STRING):STRING;
     { Liefert den n�chsten Monat }

   FUNCTION PrevMonth(ActMonth:STRING):STRING;
     { Liefert den vorhergehenden Monat }


   FUNCTION LaterMonth(Month1,Month2:STRING):BOOLEAN;
     { Pr�ft ob Month1 hinter Month2 liegt }

   FUNCTION EarlierMonth(Month1,Month2:STRING):BOOLEAN;
     { Pr�ft ob Month1 vor Month2 liegt }

   FUNCTION NextWeek(Week:TWeek):TWeek;
     { Liefert die n�chste Kalenderwoche }

   FUNCTION PrevWeek(Week:TWeek):TWeek;
     { Liefert die vorhergende Kalenderwoche }

   FUNCTION ChangeWeek(Week:TWeek; Aenderung:INTEGER):TWeek;
     { �ndert die Kalenderwoche um Aenderung Wochen }

   FUNCTION IsEarlierWeek(Week1,Week2:TWeek):BOOLEAN;
      { Pr�ft ob Week1 < Week2 ist }

   FUNCTION IsLaterWeek(Week1,Week2:TWeek):BOOLEAN;
      { Pr�ft ob Week1 > Week2 ist }

   FUNCTION IsSameWeek(Week1,Week2:TWeek):BOOLEAN;
      { Pr�ft ob Week1 = Week2 ist }

   FUNCTION LaterOrSameWeek(Week1,Week2:TWeek):BOOLEAN;
      { Pr�ft ob Week1 >= Week2 ist }

   FUNCTION EarlierOrSameWeek(Week1,Week2:TWeek):BOOLEAN;
      { Pr�ft ob Week1 <= Week2 ist }

   FUNCTION CompareWeek(Week1,Week2:TWeek):INTEGER;
      { Vergleicht zwei Wochen }

   FUNCTION InputKey:INTEGER;
      { Wartet auf Tastendruck und liefert einen Integerwert.
        Alle ASCII-Zeichen von 0 -255 werden unver�ndert zur�ckgeliefert.
        Die erweiterten Tastaturcodes werden negiert zur�ckgeliefert. }

   FUNCTION KeyPress(VAR Taste:INTEGER):BOOLEAN;
      { Pr�ft ob eine Taste gedr�ckt wurde und liefert den Wert in
        Variable Taste }
   FUNCTION KeyId(Taste:INTEGER):STRING;

   PROCEDURE MemCopy(Source,Dest:POINTER; Size:WORD);
      { Kopiert Size Bytes von Source nach Dest }

   FUNCTION  ErrMessage(ErrNum:INTEGER):STRING;
      { Liefert eine Fehlermeldung zur Fehlernummer ErrNum }

IMPLEMENTATION

   USES  DOS,
         GMCRT;

   CONST DowStr : ARRAY[1..7] OF STRING[10] = ('Montag'  ,'Dienstag',
                                               'Mittwoch','Donnerstag',
                                               'Freitag' ,'Samstag',
                                               'Sonntag');
   CONST MonthArray : ARRAY [1..12] OF BYTE = (31,28,31,30,31,30,
         31,31,30,31,30,31);

   CONST XPosition : ARRAY[BOOLEAN,1..12] OF BYTE =
         ((1,3,3,2,5,6,2,4,7,1,3,7),
         (2,4,3,2,5,6,2,4,7,1,3,7));

   CONST Tabelle1  : ARRAY[0..6,1..7] OF BYTE = ((5,4,1,7,6,2,3),
         (6,5,2,1,7,3,4),
         (7,6,3,2,1,4,5),
         (1,7,4,3,2,5,6),
         (2,1,5,4,3,6,7),
         (3,2,6,5,4,7,1),
         (4,3,7,6,5,1,2));

   FUNCTION ActTime;
      VAR   Stunde,
            Minute,
            Sekunde,
            HSek    : WORD; 
            S,
            M       : STRING; 

      BEGIN
         GETTIME(Stunde,Minute,Sekunde,HSek);
         STR(Stunde,S);
         STR(Minute,M);
         IF LENGTH(S)=1 THEN
            S:='0'+S;
         IF LENGTH(M)=1 THEN
            M:='0'+M;
         ActTime:=S+':'+M;
      END;

   FUNCTION ActSysTime;
      VAR   Stunde,
            Minute,
            Sekunde,
            HSek    : WORD;
            Sek,
            S,
            M       : STRING;

      BEGIN
         GETTIME(Stunde,Minute,Sekunde,HSek);
         STR(Stunde,S);
         STR(Minute,M);
         STR(Sekunde,Sek);
         IF LENGTH(S)=1 THEN
            S:='0'+S;
         IF LENGTH(M)=1 THEN
            M:='0'+M;
         IF LENGTH(Sek)=1 THEN
            Sek:='0'+Sek;
         ActSysTime:=S+'.'+M+'.'+Sek;
      END;

   FUNCTION ActDate;
      VAR   Jahr,
            Monat,
            Tag,
            DOW     : WORD;
            Datum,T : STRING; 
            
      BEGIN
         GETDATE(Jahr,Monat,Tag,DOW);
         Jahr := Jahr MOD 100;
         STR(Jahr,Datum);
         STR(Monat,T);
         IF LENGTH(Datum)=1 THEN
            Datum:='0'+Datum; 
         IF LENGTH(T)=1 THEN
            T:='0'+T; 
         Datum :=T+'.'+Datum; 
         STR(Tag,T); 
         IF LENGTH(T)=1 THEN
            T := '0'+T;
         ActDate:= T+'.'+Datum;
      END; 

   FUNCTION BufBlockRead; 
      CONST OFHandle = 0; 
            OFMode   = 2; 

      VAR   FileSeg,
            FileOfs : WORD; 
            ErrCode : BYTE;
            ZWResult: WORD;
            Handle  : WORD; 
            HandlePtr : ^WORD; 
            FModePtr  : ^WORD; 
      BEGIN
         FileSeg := SEG(InFile); 
         FileOFS := OFS(InFile); 
         HandlePtr := PTR(FileSeg,FileOfs+OFHandle); 
         FModePtr := PTR(FileSeg,FileOfs+OFMode); 
         Handle := HandlePtr^; 
         ErrCode := 0;
         ZwResult  := 0; 
         IF (FModePtr^ = $D7B3) THEN BEGIN
            ASM
            PUSH BP
            PUSH SP
            PUSH SS
            PUSH DS
            MOV AH,$3F
            MOV BX,Handle
            MOV CX,Count
            LDS DX,Buffer
            INT 21h
            JC  @BBReadErr
            MOV ZwResult,AX
            JMP @BBReadEnd
            @BBReadErr:
            MOV ErrCode,AL; 
            @BBReadEnd:
            POP DS
            POP SS
            POP SP
            POP BP
         END; 
      END ELSE
      ErrCode := 103; 
   Result := ZwResult;
   IF (Result < Count) THEN
      ErrCode := 255; 
   BufBlockRead := ErrCode; 
   END; 
   
   FUNCTION BufBlockWrite; 
      
      CONST OFHandle = 0; 
            OFMode   = 2; 

      VAR   FileSeg,
            FileOfs : WORD; 
            ErrCode : BYTE; 
            ZWResult: WORD;
            Handle  : WORD; 
            HandlePtr : ^WORD; 
            FModePtr  : ^WORD; 
      BEGIN
         FileSeg := SEG(OutFile); 
         FileOFS := OFS(OutFile); 
         HandlePtr := PTR(FileSeg,FileOfs+OFHandle); 
         FModePtr := PTR(FileSeg,FileOfs+OFMode); 
         Handle := HandlePtr^; 
         ErrCode := 0; 
         ZwResult  := 0; 
         IF (FModePtr^ = $D7B3) THEN BEGIN
            ASM
            PUSH BP
            PUSH SP
            PUSH SS
            PUSH DS
            MOV AH,$40
            MOV BX,Handle
            MOV CX,Count
            LDS DX,Buffer
            INT 21h
            JC  @BBWriteErr
            MOV ZwResult,AX
            JMP @BBWriteEnd
            @BBWriteErr:
            MOV ErrCode,AL;
            @BBWriteEnd:
            POP DS
            POP SS
            POP SP
            POP BP
         END; 
      END ELSE
      ErrCode := 103; 
   Result := ZwResult; 
   IF (Result < Count) THEN
      ErrCode := 255; 
   BufBlockWrite := ErrCode; 
   END;
   
   FUNCTION RenameFile; 
      VAR   DateiHandle  : FILE; 
            FileInfo     : SearchRec; 
            Dateizugriff : BOOLEAN; 
            
      BEGIN
         DOSERROR:= 0;
         FINDFIRST(Name,AnyFile,FileInfo);
         IF (DOSERROR = 0) THEN BEGIN
            FINDFIRST(NeuerName,AnyFile,FileInfo);
            Dateizugriff := TRUE; 
            IF (DOSERROR = 0) THEN
               Dateizugriff := EraseFile(NeuerName); 
            ASSIGN(DateiHandle,Name); 
            {$I-}
            RENAME(DateiHandle,NeuerName); 
            {$I+}
            RenameFile := (IORESULT = 0) AND Dateizugriff;
         END
         ELSE
            RenameFile := FALSE;
      END;

   FUNCTION NeededCopySpace;
      VAR Ergebnis : LONGINT;
          SourceSize,
          DestSize : LONGINT;

      BEGIN
        Ergebnis:= 0;
        SourceSize := SizeOfFile(Source);
        IF (SourceSize <> -1) THEN BEGIN
           Ergebnis := SourceSize;
           DestSize := SizeOfFile(Dest);
           IF (DestSize <> -1) THEN
              Ergebnis:= Ergebnis-DestSize;
        END;
        NeededCopySpace := Ergebnis;
      END;

   FUNCTION SpaceOfDisk;
      VAR Ergebnis :LONGINT;
      BEGIN
         Path := FEXPAND(Path);
         Ergebnis := DISKFREE(ORD(Path[1])-64);
         SpaceOfDisk := Ergebnis;
      END;

   FUNCTION CopyFile;
      VAR   Txt : STRING;
            Count,
            Result,
            MaxMem : WORD;
            BufPtr : POINTER;
            FSource,
            FDest : FILE;
            DestDrive,
            DestPath,
            DestName : STRING;
            AktPos,
            Ergebnis : BYTE;
            CopyValid : BOOLEAN;
            NeededSpace,
            FreeSpace : LONGINT;
            { 0 : Datei kopiert
            1 : Nicht genug Speicherplatz
            2 : Falscher Sourcepfad/Name
            3 : Falscher Destpfad/Name }
      BEGIN
         Ergebnis := 0;
         CopyValid := TRUE;
         IF SeekFile(Name) THEN BEGIN
            DestPath := FEXPAND('');
            AktPos := RightPos('\',NeuerName);
            IF (AktPos > 0) OR (NeuerName[2] = ':') THEN BEGIN
               IF ((NeuerName[2] = ':') AND (AktPos = 1 )) THEN
                  Ergebnis := 3
               ELSE BEGIN
                  IF (AktPos <= 2) THEN
                     AktPos := 2;
                  Destpath:= LeftStr(NeuerName,AktPos);
                  DestName := RightStr(NeuerName,AktPos+1);
               END;
            END
            ELSE
               DestName := NeuerName;
            IF (Ergebnis = 0) THEN BEGIN
               NeededSpace := NeededCopySpace(Name,DestName);
               DestPath := FEXPAND(DestPath);
               FreeSpace := DISKFREE(ORD(DestPath[1])-64);
               IF (FreeSpace > NeededSpace) THEN BEGIN
                  IF (MAXAVAIL > 64000) THEN
                     MaxMem := 64000
                  ELSE
                     MaxMem := MAXAVAIL;
                  GETMEM(BufPtr,MaxMem);
                  ASSIGN(FSource,Name);
                  ASSIGN(FDest,NeuerName);
                  {$I-}
                  RESET(FSource,1);
                  REWRITE(FDest,1);
                  {$I+}
                  WHILE NOT EOF(FSource) DO BEGIN
                     Count := MaxMem;
                     {$I-}
                     BufBlockRead(FSource,BufPtr,Count,Result);
                     BufBlockWrite(FDest,BufPtr,Result,Result);
                     {$I+}
                  END;
                  {$I-}
                  CLOSE(FSource);
                  CLOSE(FDest);
                  {$I+}
                  FREEMEM(BufPtr,MaxMem);
                  Ergebnis := 0;
               END
               ELSE
                  Ergebnis := 1;
            END; 
         END 
         ELSE
            Ergebnis := 2;
         CopyFile := Ergebnis; 
      END; 

   FUNCTION SeekFile;
      VAR   FileInfo     : SearchRec;

      BEGIN
         DOSERROR:= 0;
         FINDFIRST(Name,AnyFile,FileInfo);
         SeekFile := (DOSERROR = 0);
      END;

   FUNCTION ExtendFileName;
      VAR Ergebnis : STRING;
      BEGIN
         Ergebnis := FileName;
         IF NOT SeekFile(FileName) THEN BEGIN
            IF SeekFile(StdPath+FileName) THEN
               Ergebnis := StdPath+FileName;
         END;
         ExtendFileName := Ergebnis;
      END;

   FUNCTION IsPattern;
      BEGIN
         IsPattern := (POS('*',Name) > 0) OR (POS('?',Name) > 0);
      END;

   FUNCTION ValidPath;
      VAR   FileInfo     : SearchRec;
            IsDirectory  : BOOLEAN;

      BEGIN
         DOSERROR:= 0;
         IsDirectory := TRUE;
         IF (Pfad[LENGTH(Pfad)] = '\') THEN
            Pfad := COPY(Pfad,1,LENGTH(Pfad)-1);
         IF (Pfad[2] = ':') THEN
            IsDirectory := ValidDrive(Pfad[1]);
         IF IsDirectory AND (LENGTH(Pfad) > 2) THEN BEGIN
            FINDFIRST(Pfad,Directory,FileInfo);
            IsDirectory := ((FileInfo.Attr AND 16) > 0);
         END;
         ValidPath:= (DOSERROR = 0) AND IsDirectory;
      END;

   FUNCTION MakeFilePath;
      BEGIN
         IF ((Pfad[LENGTH(Pfad)] = '\') OR (Pfad = ''))  OR
            (Pfad[LENGTH(Pfad)] = ':') THEN
            MakeFilePath := Pfad
         ELSE
            MakeFilePath := Pfad +'\';
      END;

   FUNCTION ValidDrive;
      VAR   IsDrive      : BOOLEAN;
            DiskDrives   : BYTE;
            DriveNo      : BYTE;
            Regs         : Registers;
      BEGIN
         IsDrive := TRUE; 
         INTR(17,Regs);  { Ermitteln der Laufwerke }
         IF (Regs.AL AND 1) = 0 THEN
            DiskDrives := 0
         ELSE
            DiskDrives := (Regs.AL AND 192)+1; 
         DriveNo := ORD(UPCASE(Drive[1]))-64; 
         IF (LENGTH(Drive) > 2) OR ((LENGTH(Drive) = 2) AND (Drive[2] <> ':'))
            OR ((DriveNo < 1) OR (DriveNo > 18)) OR
            ((DriveNo < 3) AND (DriveNo > DiskDrives))
         THEN
            IsDrive := FALSE
         ELSE BEGIN
            IsDrive := (DISKSIZE(DriveNo) <> -1);
         END;
         ValidDrive := IsDrive; 
      END;

   FUNCTION SizeofFile;
      VAR   FileInfo : SearchRec;

      BEGIN
         DOSERROR:= 0;
         FINDFIRST(Name,AnyFile,FileInfo);
         IF (DOSERROR = 0) THEN
            SizeofFile := FileInfo.Size
         ELSE
            SizeofFile := -1;
      END;

   FUNCTION FileDate;
      VAR   FileInfo : SearchRec;
            DT : DateTime;

      BEGIN
         DOSERROR:= 0;
         FINDFIRST(Name,AnyFile,FileInfo);
         IF (DOSERROR = 0) THEN BEGIN
            UnpackTime(FileInfo.Time,DT);
            DT.Year := DT.Year MOD 100;
            FileDate:=LeadingZeros(DT.Day,2)+'.'+LeadingZeros(DT.Month,2)+'.'+
                    LeadingZeros(DT.Year,2);
         END ELSE
            FileDate := '';
      END;

   FUNCTION FileTime;
      VAR   FileInfo : SearchRec;
            DT : DateTime;

      BEGIN
         DOSERROR:= 0;
         FINDFIRST(Name,AnyFile,FileInfo);
         IF (DOSERROR = 0) THEN BEGIN
            UnpackTime(FileInfo.Time,DT);
            FileTime:=LeadingZeros(DT.Hour,2)+':'+LeadingZeros(DT.Min,2);
         END ELSE
            FileTime := '';
      END;

   FUNCTION CorrectSize;
      VAR   AnzRecs : DOUBLE;
            FSize   : LONGINT;
            
      BEGIN
         DOSERROR:= 0; 
         CorrectSize := FALSE;
         FSize := SizeOfFile(Name);
         AnzRecs := FSize / RecLength;
         IF (AnzRecs = INT(AnzRecs)) THEN
            CorrectSize := TRUE; 
      END; 

   FUNCTION NumberOfRecs;
      VAR   AnzRecs : DOUBLE;
            FSize   : LONGINT;

      BEGIN
         DOSERROR:= 0;
         FSize := SizeOfFile(Name);
         AnzRecs := FSize / RecLength;
         IF (AnzRecs = INT(AnzRecs)) THEN
             NumberOfRecs :=TRUNC(AnzRecs)
         ELSE
            NumberOfRecs := -1;
     END;

   FUNCTION EraseFile;
      VAR   DateiHandle : FILE; 
            
      BEGIN
         DOSERROR:= 0; 
         ASSIGN(DateiHandle,Name); 
         {$I-}
         ERASE(DateiHandle); 
         {$I+}
         EraseFile := (IORESULT = 0); 
      END; 
   
   {	FUNCTION MakeFileName;
   VAR   BSlashPos : BYTE;
   PointPos  : BYTE;
   FName     : STRING;
   ResName   : STRING;
   BEGIN
   BSlashPos := RightPos('\',FileName);
   FName := COPY(FileName,(BSlashPos+1),(LENGTH(Filename)-BSlashPos));
   PointPos := POS('.',FName);
   IF (PointPos = 0) THEN
   ResName := FileName+'.'+Suffix
   ELSE
   ResName := FileName;
   MakeFileName := ResName;
   END;}
   
   FUNCTION MakeFileName; 
      VAR   Dir   : DirStr; 
            FName : NameStr; 
            Ext   : ExtStr; 
            
      BEGIN
         FSPLIT(FileName,Dir,FName,Ext); 
         IF (Ext = '') THEN
            MakeFileName := Dir+FName+'.'+Suffix
         ELSE
            MakeFileName := FileName; 
      END; 
   
   FUNCTION CutSuffix;
      VAR   Dir   : DirStr; 
            FName : NameStr; 
            Ext   : ExtStr; 
            
      BEGIN
         FSPLIT(FileName,Dir,FName,Ext); 
         CutSuffix := Dir+FName;
      END; 

   FUNCTION CutPath;
      VAR   Dir   : DirStr;
            FName : NameStr;
            Ext   : ExtStr;

      BEGIN
         FSPLIT(FileName,Dir,FName,Ext);
         CutPath := FName+EXT;
      END;

   FUNCTION GetPath;
      VAR   Dir   : DirStr;
            FName : NameStr;
            Ext   : ExtStr;

      BEGIN
         FSPLIT(FileName,Dir,FName,Ext);
         GetPath := Dir
      END;

   FUNCTION PrintReal; 
      VAR   Txt,
            Vor,
            Nach  : STRING; 
            P     : BYTE; 
            
      BEGIN
         STR(Nummer:0:NachK,Txt); 
         P := POS('.',Txt);
         IF (P = 0) THEN BEGIN
            Vor := Txt; 
            Nach := ''; 
         END
         ELSE BEGIN
            Vor := COPY(Txt,1,P-1); 
            Nach:= COPY(Txt,P+1,LENGTH(Txt)-P);
         END; 
         Nach := COPY(Nach,1,NachK); 
         WHILE (LENGTH(Nach) < NachK) DO
            Nach:=Nach+'0'; 
         Txt := Vor; 
         Vor := '';
         IF SeperateThousand THEN
           WHILE (LENGTH(Txt) > 3) DO BEGIN
              Vor := '.'+COPY(Txt,LENGTH(Txt)-2,3)+Vor;
              Txt := COPY(Txt,1,LENGTH(Txt)-3);
           END;
         IF (Txt = '-') THEN
            Vor:=COPY(Vor,2,LENGTH(Vor)-1); 
         Vor := Txt+Vor; 
         IF (Nachk > 0) THEN
            Txt := Vor+','+Nach
         ELSE
            Txt := Vor; 
         WHILE (LENGTH(Txt) < (Vork+NachK)) DO
            Txt := ' '+Txt; 
         PrintReal := Txt; 
      END; 
   
   FUNCTION PrintRealOrLine; 
      
      BEGIN
         IF (Nummer = 0) THEN BEGIN
            IF ((Vork+NachK+1) > 2) THEN
               PrintRealOrLine:=' '+REPLICATE('-',Vork+NachK-1)+' '
            ELSE
               PrintRealOrLine:=REPLICATE('-',Vork+NachK+1);
         END
         ELSE
            PrintRealOrLine:=PrintReal(Nummer,Vork,NachK,TRUE);
      END;

   FUNCTION PrintReal0;

      BEGIN
         IF (Nummer = 0) THEN
            PrintReal0:=REPLICATE(' ',Vork+NachK+1)
         ELSE
            PrintReal0:=PrintReal(Nummer,Vork,NachK,TRUE);
      END; 
   
   FUNCTION PrintDouble;
      VAR   Txt,
            Vor,
            Nach  : STRING;
            P     : BYTE;

      BEGIN
         STR(Nummer:0:NachK,Txt);
         P := POS('.',Txt);
         IF (P = 0) THEN BEGIN
            Vor := Txt;
            Nach := '';
         END
         ELSE BEGIN
            Vor := COPY(Txt,1,P-1);
            Nach:= COPY(Txt,P+1,LENGTH(Txt)-P);
         END;
         Nach := COPY(Nach,1,NachK);
         WHILE (LENGTH(Nach) < NachK) DO
            Nach:=Nach+'0';
         Txt := Vor;
         Vor := '';
         WHILE (LENGTH(Txt) > 3) DO BEGIN
            Vor := '.'+COPY(Txt,LENGTH(Txt)-2,3)+Vor;
            Txt := COPY(Txt,1,LENGTH(Txt)-3);
         END;
         IF (Txt = '-') THEN
            Vor:=COPY(Vor,2,LENGTH(Vor)-1);
         Vor := Txt+Vor;
         IF (Nachk > 0) THEN
            Txt := Vor+','+Nach
         ELSE
            Txt := Vor;
         WHILE (LENGTH(Txt) < (Vork+NachK)) DO
            Txt := ' '+Txt;
         PrintDouble := Txt;
      END;

   FUNCTION PrintDoublePoint;
      VAR   Txt,
            Vor,
            Nach  : STRING;
            P     : BYTE;

      BEGIN
         STR(Nummer:0:NachK,Txt);
         P := POS('.',Txt);
         IF (P = 0) THEN BEGIN
            Vor := Txt;
            Nach := '';
         END
         ELSE BEGIN
            Vor := COPY(Txt,1,P-1);
            Nach:= COPY(Txt,P+1,LENGTH(Txt)-P);
         END;
         Nach := COPY(Nach,1,NachK);
         WHILE (LENGTH(Nach) < NachK) DO
            Nach:=Nach+'0';
         Txt := Vor;
         Vor := '';
         WHILE (LENGTH(Txt) > 3) DO BEGIN
            Vor := '.'+COPY(Txt,LENGTH(Txt)-2,3)+Vor;
            Txt := COPY(Txt,1,LENGTH(Txt)-3);
         END;
         IF (Txt = '-') THEN
            Vor:=COPY(Vor,2,LENGTH(Vor)-1);
         Vor := Txt+Vor;
         IF (Nachk > 0) THEN
            Txt := Vor+'.'+Nach
         ELSE
            Txt := Vor;
         WHILE (LENGTH(Txt) < (Vork+NachK)) DO
            Txt := ' '+Txt;
         PrintDoublePoint := Txt;
      END;


   FUNCTION PrintDouble0;

      BEGIN
         IF (Nummer = 0) THEN
            PrintDouble0:=REPLICATE(' ',Vork+NachK)
         ELSE
            PrintDouble0:=PrintDouble(Nummer,Vork,NachK);
      END;

   FUNCTION PrintDoublePoint0;

      BEGIN
         IF (Nummer = 0) THEN
            PrintDoublePoint0:=REPLICATE(' ',Vork+NachK)
         ELSE
            PrintDoublePoint0:=PrintDoublePoint(Nummer,Vork,NachK);
      END;

   FUNCTION PrintLong;
      VAR   Txt,
            Vor   : STRING;
            P     : BYTE;

      BEGIN
         STR(Nummer,Txt); 
         Vor := ''; 
         WHILE (LENGTH(Txt) > 3) DO BEGIN
            Vor := '.'+COPY(Txt,LENGTH(Txt)-2,3)+Vor; 
            Txt := COPY(Txt,1,LENGTH(Txt)-3); 
         END; 
         IF (Txt = '-') THEN
            Vor:=COPY(Vor,2,LENGTH(Vor)-1); 
         Vor := Txt+Vor; 
         WHILE (LENGTH(Vor) < Stellen) DO
            Vor:= ' '+Vor; 
         PrintLong := Vor; 
      END; 
   
   FUNCTION PrintLongOrLine; 
      
      BEGIN
         IF (Nummer = 0) THEN BEGIN
            IF (Stellen > 2) THEN
               PrintLongOrLine:=' '+REPLICATE('-',Stellen-2)+' '
            ELSE
               PrintLongOrLine:=REPLICATE('-',Stellen); 
         END 
         ELSE
            PrintLongOrLine:=L2S(Nummer,Stellen); 
      END; 

   FUNCTION PrintLongOrMinus;

      BEGIN
         IF (Nummer = 0) THEN BEGIN
            IF (Stellen > 1) THEN
               PrintLongOrMinus:=REPLICATE(' ',Stellen-1)+'-'
            ELSE
               PrintLongOrMinus:='-';
         END
         ELSE
            PrintLongOrMinus:=L2S(Nummer,Stellen);
      END;
   
   FUNCTION PrintLong0(Nummer:LONGINT; Stellen:BYTE):STRING; 
      
      BEGIN
         IF (Nummer = 0) THEN
            PrintLong0:=REPLICATE(' ',Stellen)
         ELSE
            PrintLong0:=PrintLong(Nummer,Stellen); 
      END; 
   
   FUNCTION PrintString; 
      
      BEGIN
         WHILE (LENGTH(Txt) < Stellen) DO
            Txt := Txt+' '; 
         PrintString:= Txt; 
      END;

   FUNCTION HexNum;
      VAR   Ergebnis : STRING; 
            Lauf     : BYTE; 
      CONST HexChars : ARRAY[0..15] OF CHAR = ('0','1','2','3','4','5','6','7',
            '8','9','A','B','C','D','E','F'); 
            
      BEGIN
         Ergebnis := ''; 
         IF (Stellen > 0) THEN BEGIN
            IF Stellen > 8 THEN
               Stellen := 8; 
            FOR Lauf := 1 TO Stellen DO BEGIN
               Ergebnis:= HexChars[Wert MOD 16] + Ergebnis;
               Wert :=Wert SHR 4;
            END;
         END;
         IF (Stellen = 0) THEN BEGIN
            REPEAT
               Ergebnis:= HexChars[Wert MOD 16] + Ergebnis;
               Wert :=Wert SHR 4;
            UNTIL (Wert = 0);
         END;
         HexNum := Ergebnis;
      END;

   FUNCTION BinNum;
      VAR   Ergebnis : STRING;
            Lauf     : BYTE; 

      BEGIN
         Ergebnis := '';
         IF (Stellen > 0) THEN BEGIN
            IF Stellen > 32 THEN
               Stellen := 32;
            FOR Lauf := 1 TO Stellen DO BEGIN
               Ergebnis:= CHR((Wert MOD 2)+48) + Ergebnis;
               Wert :=Wert SHR 1;
            END;
         END;
         IF (Stellen = 0) THEN BEGIN
            REPEAT
              Ergebnis:= CHR((Wert MOD 2)+48) + Ergebnis;
              Wert :=Wert SHR 1;
            UNTIL (Wert = 0);
         END;
         BinNum := Ergebnis;
      END;

   FUNCTION R2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen:NachK,Ergebnis); 
         R2S:= Ergebnis
      END; 
   
   FUNCTION D2S; 
      VAR   Ergebnis : STRING; 
            
      BEGIN
         STR(Wert:Stellen:NachK,Ergebnis); 
         D2S:= Ergebnis
      END; 
   
   FUNCTION E2S; 
      VAR   Ergebnis : STRING; 
            
      BEGIN
         STR(Wert:Stellen:NachK,Ergebnis); 
         E2S:= Ergebnis
      END; 
   
   
   FUNCTION C2S; 
      VAR   Ergebnis : STRING; 
            
      BEGIN
         STR(Wert:Stellen:NachK,Ergebnis); 
         C2S:= Ergebnis
      END; 
   
   FUNCTION SG2S; 
      VAR   Ergebnis : STRING; 
            
      BEGIN
         STR(Wert:Stellen:NachK,Ergebnis); 
         SG2S:= Ergebnis
      END; 
   
   FUNCTION R2L; 
      VAR   Txt      : STRING; 
            Ergebnis : LONGINT; 
            C        : INTEGER; 
            
      BEGIN
         R2L := S2L(R2S(Wert,0,0)); 
      END; 
   
   
   FUNCTION L2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen,Ergebnis); 
         L2S:= Ergebnis
      END; 
   
   FUNCTION B2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen,Ergebnis); 
         B2S:= Ergebnis
      END; 
   
   FUNCTION W2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen,Ergebnis); 
         W2S:= Ergebnis
      END; 
   
   FUNCTION SI2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen,Ergebnis); 
         SI2S:= Ergebnis
      END; 
   
   FUNCTION I2S; 
      VAR   Ergebnis :STRING; 
            
      BEGIN
         STR(Wert:Stellen,Ergebnis); 
         I2S:= Ergebnis
      END; 
   
   FUNCTION S2L; 
      VAR   Ergebnis : LONGINT; 
            C        : INTEGER; 
            
      BEGIN
         VAL(Txt,Ergebnis,C); 
         IF (C <> 0) AND ((Txt[C] = '.')  OR (Txt[C] = ',')) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2L := Ergebnis; 
      END; 
   
   FUNCTION S2B; 
      VAR   Ergebnis : BYTE; 
            C        : INTEGER; 
            
      BEGIN
         VAL(Txt,Ergebnis,C); 
         IF (C <> 0) AND ((Txt[C] = '.')  OR (Txt[C] = ',')) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2B := Ergebnis; 
      END; 
   
   FUNCTION S2W; 
      VAR   Ergebnis : WORD; 
            C        : INTEGER; 
            
      BEGIN
         VAL(Txt,Ergebnis,C); 
         IF (C <> 0) AND ((Txt[C] = '.')  OR (Txt[C] = ',')) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2W := Ergebnis; 
      END; 
   
   FUNCTION S2SI; 
      VAR   Ergebnis : SHORTINT; 
            C        : INTEGER; 
            
      BEGIN
         VAL(Txt,Ergebnis,C); 
         IF (C <> 0) AND ((Txt[C] = '.')  OR (Txt[C] = ',')) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2SI := Ergebnis; 
      END; 
   
   
   FUNCTION S2I; 
      VAR   Ergebnis : INTEGER; 
            C        : INTEGER; 
            
      BEGIN
         VAL(Txt,Ergebnis,C); 
         IF (C <> 0) AND ((Txt[C] = '.')  OR (Txt[C] = ',')) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2I := Ergebnis; 
      END; 
   
   FUNCTION S2R; 
      VAR   Ergebnis : REAL; 
            C        : INTEGER; 
            
      BEGIN
         IF (POS(',',Txt) <> 0) THEN
            Txt[POS(',',Txt)] := '.'; 
         VAL(Txt,Ergebnis,C); 
         IF (C > 0) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2R := Ergebnis; 
      END; 
   
   
   FUNCTION S2D; 
      VAR   Ergebnis : DOUBLE; 
            C        : INTEGER; 
            
      BEGIN
         IF (POS(',',Txt) <> 0) THEN
            Txt[POS(',',Txt)] := '.'; 
         VAL(Txt,Ergebnis,C); 
         IF (C > 0) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2D := Ergebnis; 
      END; 
   
   FUNCTION S2E; 
      VAR   Ergebnis : EXTENDED; 
            C        : INTEGER; 
            
      BEGIN
         IF (POS(',',Txt) <> 0) THEN
            Txt[POS(',',Txt)] := '.'; 
         VAL(Txt,Ergebnis,C); 
         IF (C > 0) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2E := Ergebnis; 
      END; 
   
   FUNCTION S2SG; 
      VAR   Ergebnis : SINGLE; 
            C        : INTEGER; 
            
      BEGIN
         IF (POS(',',Txt) <> 0) THEN
            Txt[POS(',',Txt)] := '.'; 
         VAL(Txt,Ergebnis,C); 
         IF (C > 0) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C); 
         S2SG := Ergebnis; 
      END; 
   
   FUNCTION S2C;
      VAR   Ergebnis : COMP;
            C        : INTEGER;

      BEGIN
         IF (POS(',',Txt) <> 0) THEN
            Txt[POS(',',Txt)] := '.';
         VAL(Txt,Ergebnis,C);
         IF (C > 0) THEN
            VAL(COPY(Txt,1,C-1),Ergebnis,C);
         S2C := Ergebnis;
      END;

   FUNCTION H2W;
      VAR Ergebnis : WORD;
          Lauf : BYTE;
          Mult : WORD;
          Error : BOOLEAN;

      BEGIN
         Error := FALSE;
         Ergebnis := 0;
         Mult := 1;
         Lauf :=LENGTH(Wert);
         WHILE (Lauf > 1) AND (NOT Error) DO BEGIN
            CASE Wert[Lauf]  OF
               '0'..'9' : Ergebnis := Ergebnis + Mult *(ORD(Wert[Lauf])-48);
               'A'..'F' : Ergebnis := Ergebnis + Mult *(ORD(Wert[Lauf])-55);
               ELSE       Error := TRUE;
            END;
            Mult := Mult *16;
            DEC(Lauf);
         END;
         IF NOT Error THEN
           H2W := Ergebnis;
      END;


   FUNCTION RoundDbl;
      VAR   Lauf:BYTE;
            Ergebnis:DOUBLE;
            ZWErg1,ZwErg2,
            NKomma : DOUBLE;
            ZwErg3   : STRING;
            C        : INTEGER;

      BEGIN
         Lauf := 0;
         Ergebnis := Wert;
         WHILE (Lauf < NachK+1) DO BEGIN
            Ergebnis := Ergebnis*10;
            INC(Lauf);
         END;
         STR(Ergebnis:0:0,ZwErg3);
         VAL(ZwErg3,Ergebnis,C);
         ZwErg1 := Ergebnis;
         ZWErg2 := INT(ZwErg1 / 10) * 10;
         NKomma := ZwErg1-ZwErg2;
         ZwErg2 := ZwErg2 / 10;
         IF Nkomma >= 5 THEN
           ZwErg2 := ZwErg2 +1;
         IF NKomma <= -5 THEN
           ZwErg2 := ZwErg2 -1;
         Ergebnis := ZwErg2;
         Lauf := 0;
         WHILE (Lauf < NachK) DO BEGIN
            Ergebnis := Ergebnis/10;
            INC(Lauf);
         END;
         RoundDbl:= Ergebnis;
      END;

   FUNCTION RoundReal;
      VAR   Lauf:BYTE;
            Ergebnis:REAL;
            ZWErg1,ZwErg2,
            NKomma : REAL;
            ZwErg3   : STRING;
            C        : INTEGER;

      BEGIN
         Lauf := 0;
         Ergebnis := Wert;
         WHILE (Lauf < NachK+1) DO BEGIN
            Ergebnis := Ergebnis*10;
            INC(Lauf);
         END;
         STR(Ergebnis:0:0,ZwErg3);
         VAL(ZwErg3,Ergebnis,C);
         ZwErg1 := Ergebnis;
         ZWErg2 := INT(ZwErg1 / 10) * 10;
         NKomma := ZwErg1-ZwErg2;
         ZwErg2 := ZwErg2 / 10;
         IF Nkomma >= 5 THEN
           ZwErg2 := ZwErg2 +1;
         IF NKomma <= -5 THEN
           ZwErg2 := ZwErg2 -1;
         Ergebnis := ZwErg2;
         Lauf := 0;
         WHILE (Lauf < NachK) DO BEGIN
            Ergebnis := Ergebnis/10;
            INC(Lauf);
         END;
         RoundReal:= Ergebnis;
      END;

   FUNCTION LeadingZeros;
      VAR   T : STRING;
            L : LONGINT;

      BEGIN
         STR(Wert,T);
         WHILE (LENGTH(T) < Anzahl) DO
            T:='0'+T;
         LeadingZeros := T;
      END;


   FUNCTION OnlyNumber;
         VAR Ergebnis:STRING;
             Lauf : BYTE;

      BEGIN
        Wert := CutBSpaces(Wert);
        IF (LENGTH(Wert) > 1) THEN BEGIN
           Lauf := 1;
           Ergebnis :='';
           WHILE (Lauf < LENGTH(Wert)) AND (Wert[Lauf] = '0') DO
              INC(Lauf);
           Ergebnis := COPY(Wert,Lauf,(LENGTH(Wert)-Lauf+1));
        END ELSE
           Ergebnis := Wert;
        OnlyNumber:= Ergebnis;
      END;

   FUNCTION FirstMaskChar(Txt:STRING):BYTE;
      VAR   Ende,
            Lauf,
            POS  : BYTE;
      BEGIN
         Ende := LENGTH(Txt);
         Lauf := 1;
         POS :=0;
         WHILE (Lauf<= Ende) AND (POS=0) DO BEGIN
            IF Txt[Lauf] <> ' ' THEN
               POS := Lauf;
            INC(Lauf);
         END;
         FirstMaskChar:= POS
      END;

   FUNCTION MidSpace;
      VAR   Ende    : BOOLEAN;
            MidPos,
            Lauf    : BYTE;

      BEGIN
         Ende := FALSE;
         MidSpace := 0;
         IF (LENGTH (Txt) > 2) THEN BEGIN
            MidPos := LENGTH(Txt) DIV 2+1;
            Lauf := 0;
            WHILE (MidPos+Lauf < LENGTH(Txt)) AND NOT Ende DO BEGIN
                IF (Txt[MidPos-Lauf]=' ') THEN BEGIN
                  MidSpace := MidPos-Lauf;
                  Ende := TRUE;
               END
               ELSE BEGIN
                  IF (Txt[MidPos+Lauf]=' ') THEN BEGIN
                     MidSpace := MidPos+Lauf;
                     Ende := TRUE;
                  END;
               END;
               INC(Lauf);
            END;
         END;
      END;

   FUNCTION FillUp;
      VAR   Tx : STRING;
            Anzahl : BYTE;

      BEGIN
         IF (MaxFill+LENGTH(InpString) > MaxLength) AND (MaxFill > 0) THEN
            Anzahl := MaxLength-LENGTH(InpString)
         ELSE
            Anzahl := MaxFill;
         Tx := REPLICATE(FILLCHAR,Anzahl)+InpString;
         FillUp := Tx;
      END;

   FUNCTION FillUpRight;
      VAR   Tx : STRING;
            Anzahl : BYTE;

      BEGIN

         IF (MaxFill+LENGTH(InpString) > MaxLength) AND (MaxFill > 0) THEN
            Anzahl := MaxLength-LENGTH(InpString)
         ELSE
            Anzahl := MaxFill;
         Tx :=InpString+REPLICATE(FILLCHAR,Anzahl);
         FillUpRight := Tx;
      END;

   FUNCTION ReplaceChar;
      VAR Ergebnis : STRING;
          Lauf : BYTE;
          Ende :BYTE;
      BEGIN
         Ende := LENGTH(InpString);
         Ergebnis := '';
         Lauf := 1;
         WHILE (Lauf <= Ende) DO BEGIN
            IF (InpString[Lauf] = OldChar) THEN
               Ergebnis := Ergebnis + NewChar
            ELSE
               Ergebnis := Ergebnis + InpString[Lauf];
            INC(Lauf);
         END;
         ReplaceChar := Ergebnis;
      END;

   FUNCTION Replicate;

      VAR   Txt: STRING;

      BEGIN
         Txt:='';
         WHILE (Anz > 0) DO BEGIN
            Txt := Txt+Zeichen;
            DEC(Anz);
         END;
         Replicate:=Txt;
      END;

   {                            �����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Alle Tastenkombinationen einlesen                         �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   INTEGER-Code ,positiv f�r ASCII,negativ f�r Sondertaste   �
   ������������������������������������������������������������������������ͼ}

   FUNCTION InputKey;

      VAR   Taste1,
            Taste2 :CHAR;
            T2Wert : BYTE;

      BEGIN
         Taste2 := #0;
         Taste1 := READKEY;
         IF (Taste1 = #0) THEN
            Taste2 := READKEY;
         InputKey := ORD(Taste1) - ORD(Taste2);
      END;

   FUNCTION KeyPress;
      BEGIN
         IF KEYPRESSED THEN BEGIN
            Taste := INPUTKey;
            KeyPress:= TRUE;
         END
         ELSE
            KeyPress := FALSE;
      END;

   FUNCTION CutLSpaces; 
      VAR   Lauf : BYTE; 

      BEGIN
         Lauf := 1;
         WHILE (Txt[Lauf] = ' ') AND (Lauf < 255) DO
            INC(Lauf);
         CutLSpaces:=COPY(Txt,Lauf,LENGTH(Txt)-Lauf+1); 
      END; 
   
   FUNCTION CutRSpaces; 
      VAR   Lauf : BYTE; 
            
      BEGIN
         Lauf := LENGTH(Txt); 
         WHILE (Txt[Lauf] = ' ') AND (Lauf >1) DO
            DEC(Lauf); 
         CutRSpaces:=COPY(Txt,1,Lauf); 
      END;
   
   FUNCTION CutBSpaces; 
      BEGIN
         CutBSpaces:=CutLSpaces(CutRSpaces(Txt)); 
      END; 

   FUNCTION RemoveAllSpaces;
      VAR   Lauf : BYTE;
            ETxt : STRING;
      BEGIN
         ETxt := '';
         FOR Lauf := 1 TO LENGTH(Txt) DO
            IF (Txt[Lauf]<> ' ') THEN
               ETxt := ETxt + Txt[Lauf];
         RemoveAllSpaces:=ETxt;
      END;

   FUNCTION CenterString;
      VAR   Lauf : BYTE;
            FrontLauf : BYTE;
            ETxt : STRING;
      BEGIN
         IF LENGTH(Txt) > MaxLength THEN
            ETxt := Txt
         ELSE BEGIN
           FrontLauf:=(MaxLength - LENGTH(txt)) DIV 2;
           Etxt := REPLICATE(' ',FrontLauf)+Txt;
           Lauf := MaxLength-LENGTH(ETxt);
           Etxt := ETxt + REPLICATE(' ',Lauf);
         END;
         CenterString:=ETxt;
      END; 
   
   FUNCTION RightPos; 
      VAR   Ergebnis,
            SubL,
            SeL,
            AktPos,
            Lauf :BYTE; 
            
      BEGIN
         Ergebnis := 0;
         SubL := LENGTH(SubStr); 
         SeL := LENGTH(SearchStr); 
         IF (SubL <= SeL) THEN BEGIN
            AktPos := SeL-SubL+1; 
            WHILE (AktPos > 0) DO BEGIN
               IF (POS(SubStr,COPY(SearchStr,AktPos,SeL-AktPos+1)) = 1) THEN BEGIN
                  Ergebnis := AktPos; 
                  AktPos := 0; 
               END 
               ELSE
                  DEC(AktPos); 
            END; 
         END;
         RightPos := Ergebnis; 
      END; 
   
   FUNCTION LeftStr; 
      BEGIN
         IF (Last >= LENGTH(Txt)) THEN
            LeftStr := Txt
         ELSE
            LeftStr := COPY(Txt,1,Last);
      END;

   FUNCTION RightStr;
      BEGIN
         IF (First > LENGTH(Txt)) THEN
            RightStr := ''
         ELSE
            RightStr := COPY(Txt,First,LENGTH(Txt)-First+1); 
      END; 

   PROCEDURE SplitString;
      VAR   DelimPos,
            BeginOfRest : BYTE;
      BEGIN
         DelimPos := POS(Delimiter,Txt);
         IF (DelimPos = 0) THEN BEGIN
            Front := Txt; 
            Rest := '';
         END
         ELSE BEGIN
            Front := COPY(Txt,1,DelimPos-1);
            BeginOfRest := DelimPos+LENGTH(Delimiter);
            Rest  := COPY(Txt,BeginOfRest,LENGTH(txt)-BeginOfRest+1);
         END;
      END;

{   PROCEDURE SplitString3;
      VAR   DelimPos,
            BeginOfRest : BYTE;
            R: ARRAY[0..3] OF STRING;
            RC:BYTE;
      BEGIN
         R[0]:='';
         R[1]:='';
         R[2]:='';
         R[3]:='';
         RC := 0;
         WHILE (Txt <> '') AND (RC < 4) DO BEGIN
           DelimPos := POS(Delimiter,Txt);
           IF (DelimPos = 0) THEN BEGIN
              R[RC] := Txt;
              Txt   := '';
           END ELSE BEGIN
             R[RC] := COPY(Txt,1,DelimPos-1);
             BeginOfRest := DelimPos+LENGTH(Delimiter);
             Txt := COPY(Txt,BeginOfRest,LENGTH(txt)-BeginOfRest+1);
             INC(RC);
           END;
         END;
         Front:=R[0];
         R1:=R[1];
         R2:=R[2];
         R3:=R[3];
      END;   }

   PROCEDURE SplitIdent;
      VAR   BeginOfRest : BYTE;
            Len         : BYTE;
      CONST IdentChars  : SET OF CHAR      = ['A'..'Z','a'..'z','_','0'..'9'];
      BEGIN
         BeginOfRest:= 1;
         Len := LENGTH(Txt);
         WHILE (BeginOfRest <= Len) AND (Txt[BeginOfRest] IN IdentChars) DO
            INC(BeginOfRest); 
         IF (BeginOfRest > Len) THEN BEGIN
            Front := Txt; 
            Rest := ''; 
            SplitChar:=#0; 
         END 
         ELSE BEGIN
            Front := COPY(Txt,1,BeginOfRest-1);
            Rest := COPY(Txt,BeginOfRest+1,Len-BeginOfRest);
            Splitchar := Txt[BeginOfRest];
         END;
      END;

   FUNCTION Upper;
      
      VAR   Lauf : BYTE;

      BEGIN
         FOR Lauf := 1 TO LENGTH(Txt) DO
            CASE Txt[Lauf] OF
               'a'..'z' : Txt[Lauf] := UPCASE(Txt[Lauf]); 
               '�'      : Txt[Lauf] := '�'; 
               '�'      : Txt[Lauf] := '�'; 
               '�'      : Txt[Lauf] := '�'; 
            END; 
         UPPER := Txt; 
      END; 
   

   FUNCTION LowCase;

      BEGIN
         CASE Ch OF
            'A'..'Z' : Ch :=CHR(ORD(Ch)+32);
            '�'      : Ch:= '�';
            '�'      : Ch := '�';
            '�'      : Ch := '�';

         END;
         LowCase := CH;
      END;

   FUNCTION Lower;
      VAR Lauf :BYTE;
      BEGIN
         FOR Lauf:= 1 TO LENGTH(Txt) DO
            Txt[Lauf] := LowCase(Txt[Lauf]);
         Lower :=Txt;
      END;


   FUNCTION IsNumber(PString:STRING):BOOLEAN;
         VAR   Count : BYTE;
               Plen  : BYTE;
         BEGIN
            PString := CutBSpaces(PString);
            PLen := LENGTH(PString);
            Count := 1;
            WHILE (Count <= Plen) AND (PString[Count] IN ['0'..'9']) DO
               INC(Count);
            IsNumber:= (Count > PLen);
         END;

   FUNCTION IsFloat(PString:STRING):BOOLEAN;
         VAR   Count : BYTE;
               Plen  : BYTE;
               PointsFound : BYTE;
         BEGIN
            PString := CutBSpaces(PString);
            PLen := LENGTH(PString);
            Count := 1;
            PointsFound := 0;
            WHILE (Count <= Plen) AND (PString[Count] IN ['0'..'9','.']) DO BEGIN
               IF (PString[Count] ='.') THEN
                  INC(PointsFound);
               INC(Count);
            END;
            IsFloat:= (Count > PLen) AND (PointsFound <= 1);
         END;

   FUNCTION Date2DBIndex;
      { Die Funktion geht davon aus, dass das Datum wohlgeformt ist }
      VAR Jahr : WORD;
      BEGIN
         Jahr  := S2L(COPY(Datum,7,LENGTH(Datum)-6));
         IF (Jahr <= CJahrhundertWechsel) THEN
            Jahr := 2000+Jahr;
         IF (Jahr < 100) THEN
            Jahr := 1900+Jahr;
         Date2DBIndex := W2S(Jahr,4)+COPY(Datum,4,2)+COPY(Datum,1,2);
      END;

   FUNCTION DBIndex2Date;
      { Die Funktion geht davon aus, dass der DBIndex wohlgeformt ist }
      BEGIN
        DBIndex2DAte := COPY(DBIndex,7,2)+'.'+COPY(DBIndex,5,2)+'.'+
                        COPY(DBIndex,3,2);
      END;

   FUNCTION ValidDate;
      VAR   MaxTag,
            Monat,
            Tag,
            Jahr : LONGINT;
      BEGIN
         ValidDate := FALSE;
         IF (LENGTH(Datum) = 8) THEN BEGIN
            Tag := S2L(COPY(Datum,1,2));
            Monat := S2L(COPY(Datum,4,2));
            Jahr  := S2L(COPY(Datum,7,2));
            IF ((Monat >= 1) AND (Monat <=12)) THEN BEGIN
               MaxTag := MonthArray[Monat];
               IF (Monat = 2) AND ((Jahr MOD 4) = 0) THEN
                  INC(MaxTag);
               ValidDate:= ((Tag >= 1) AND (Tag <= MaxTag));
            END;
         END;
      END;

   FUNCTION ValidTime;
      VAR   Stunde,Minute:BYTE;
      BEGIN
         ValidTime := FALSE;
         IF (LENGTH(Uhrzeit) =5) AND (Uhrzeit[3] = ':')THEN BEGIN
            Stunde := S2B(COPY(Uhrzeit,1,2));
            Minute := S2B(COPY(Uhrzeit,4,2));
            ValidTime := IsNumber(COPY(Uhrzeit,1,2)) AND
              IsNumber(COPY(Uhrzeit,4,2)) AND (Stunde <24) AND (Minute < 60);
         END;
      END;

   FUNCTION PrevHour;
      VAR Hour :BYTE;
          Ergebnis : STRING;
      BEGIN
         Ergebnis :=  Time;
         IF (LENGTH(Time) = 5) THEN BEGIN
            Hour := S2B(COPY(Time,1,2));
            IF (Hour = 0) THEN
               Hour := 23
            ELSE
               Hour := Hour -1;
             Ergebnis := LeadingZeros(Hour,2)+COPY(Time,3,3);
         END;
         PrevHour := Ergebnis;
      END;

   FUNCTION NextHour;
      VAR Hour :BYTE;
          Ergebnis : STRING;
      BEGIN
         Ergebnis :=  Time;
         IF (LENGTH(Time) = 5) THEN BEGIN
            Hour := S2B(COPY(Time,1,2));
            IF (Hour = 23) THEN
               Hour := 0
            ELSE
               Hour := Hour +1;
             Ergebnis := LeadingZeros(Hour,2)+COPY(Time,3,3);
         END;
         NextHour := Ergebnis;
      END;

   FUNCTION DayOfWeek;
      VAR   Jahr,
            Tag,
            Monat,
            Jahre,
            TageGesamt : LONGINT;
            Lauf : BYTE;

      BEGIN
         DayOfWeek := 0;
         Tag := S2L(COPY(Datum,1,2));
         Monat := S2L(COPY(Datum,4,2));
         Jahr  := S2L(COPY(Datum,7,2));
         IF Jahr < 60 THEN
            Jahre := 100+Jahr-60
         ELSE
            Jahre := Jahr-60;
         TageGesamt := (Jahre * 365) + ((Jahre+3) DIV 4); { Jahre +Schaltjahre }
         FOR Lauf := 1 TO (Monat-1) DO BEGIN
            TageGesamt := TageGesamt + MonthArray[Lauf];
            IF (Lauf = 2) AND ((Jahr MOD 4) = 0) THEN
               INC(TageGesamt);
         END;
         TageGesamt := TageGesamt + (Tag - 1);
         DayOfWeek := ((TageGesamt + 4) MOD 7) + 1;
      END;

   FUNCTION DayOfWeekStr;
      BEGIN
         DayOfWeekStr := DOWStr[DayOfWeek(Datum)];
      END;

   FUNCTION NextDay;
      VAR   MaxTag,
            Tag,
            Monat,
            Jahr,
            NeuerTag,
            NeuerMonat,
            NeuesJahr : LONGINT;
      BEGIN
         Tag  := S2L(COPY(Datum,1,2));
         Monat:= S2L(COPY(Datum,4,2));
         NeuerMonat := Monat;
         Jahr := S2L(COPY(Datum,7,2));
         NeuesJahr := Jahr;
         NeuerTag := Tag+1;
         MaxTag := MonthArray[Monat];
         IF (Monat = 2) AND ((Jahr MOD 4) = 0) THEN
            INC(MaxTag);
         IF (NeuerTag > MaxTag) THEN BEGIN
            NeuerTag := 1;
            INC(NeuerMonat);
            IF (NeuerMonat = 13) THEN BEGIN
               NeuerMonat := 1;
               INC(NeuesJahr);
               IF (NeuesJahr = 100) THEN
                  NeuesJahr := 0;
            END;
         END;
         NextDay := LeadingZeros(NeuerTag,2)+'.'+
         LeadingZeros(NeuerMonat,2)+'.'+
         LeadingZeros(NeuesJahr,2);
      END;

   FUNCTION PrevDay;
      VAR   Tag,
            Monat,
            Jahr,
            NeuerTag,
            NeuerMonat,
            NeuesJahr : LONGINT;
      BEGIN
         Tag  := S2L(COPY(Datum,1,2));
         Monat:= S2L(COPY(Datum,4,2));
         NeuerMonat := Monat;
         Jahr := S2L(COPY(Datum,7,2));
         NeuesJahr := Jahr;
         NeuerTag := Tag-1;
         IF (NeuerTag = 0) THEN BEGIN
            DEC(NeuerMonat);
            IF (NeuerMonat = 0) THEN BEGIN
               NeuerMonat :=12;
               IF (NeuesJahr = 0) THEN
                  NeuesJahr := 99
               ELSE
                  DEC(NeuesJahr);
            END;
            NeuerTag := MonthArray[NeuerMonat];
            IF (NeuerMonat = 2) AND ((Jahr MOD 4) = 0) THEN
               INC(NeuerTag);
         END;
         PrevDay := LeadingZeros(NeuerTag,2)+'.'+
         LeadingZeros(NeuerMonat,2)+'.'+
         LeadingZeros(NeuesJahr,2);
      END;

   FUNCTION NextWorkDay;
      VAR   ZwDatum :STRING;

      BEGIN
         ZwDatum := NextDay(Datum);
         IF (DayOfWeek(ZwDatum) = 7) THEN
            ZwDatum := NextDay(ZwDatum);
         NextWorkDay := ZwDatum;
      END;

   FUNCTION PrevWorkDay;
      VAR   ZwDatum :STRING;

      BEGIN
         ZwDatum := PrevDay(Datum);
         IF (DayOfWeek(ZwDatum) = 7) THEN
            ZwDatum := PrevDay(ZwDatum);
         PrevWorkDay := ZwDatum;
      END;

   FUNCTION IsWorkDay;

      BEGIN
         IsWorkDay := (DayOfWeek(Datum) <> 7);
      END;

  FUNCTION Earlier;
     VAR Jahr1, Jahr2 : WORD;
         Value1,
         Value2 : STRING;
     BEGIN
         Jahr1  := S2L(COPY(Day1,7,2));
         IF (Jahr1 <= CJahrhundertWechsel) THEN BEGIN
            Jahr1 := 2000+Jahr1;
         END ELSE BEGIN
            Jahr1 := 1900+Jahr1;
         END;
         Jahr2  := S2L(COPY(Day2,7,2));
         IF (Jahr2 <= CJahrhundertWechsel) THEN BEGIN
            Jahr2 := 2000+Jahr2;
         END ELSE BEGIN
            Jahr2 := 1900+Jahr2;
         END;
        Value1 := W2S(Jahr1,4)+COPY(Day1,4,2)+COPY(Day1,1,2);
        Value2 := W2S(Jahr2,4)+COPY(Day2,4,2)+COPY(Day2,1,2);
        Earlier := (Value1 < Value2);
     END;

  FUNCTION Later;
     BEGIN
        Later := Earlier(Day2,Day1);
     END;

  FUNCTION NotLater;
     BEGIN
        NotLater:= NOT (Later(Day1,Day2));
     END;

  FUNCTION NotEarlier;
     BEGIN
        NotEarlier:= NOT (Earlier(Day1,Day2));
     END;

  FUNCTION SameDate;
     BEGIN
        SameDate := NotEarlier(Day1,Day2) AND NotEarlier(Day2,Day1);
     END;

  FUNCTION EarlierTime;
     VAR Value1,
         Value2 : LONGINT;
     BEGIN
        Value1 := S2L(COPY(Time1,1,2)+COPY(Time1,4,2));
        Value2 := S2L(COPY(Time2,1,2)+COPY(Time2,4,2));
        EarlierTime := (Value1 < Value2);
     END;

  FUNCTION LaterTime;
     BEGIN
        LaterTime := EarlierTime(Time2,Time1);
     END;

  FUNCTION NotLaterTime;
     BEGIN
        NotLaterTime:= NOT (LaterTime(Time1,Time2));
     END;

  FUNCTION NotEarlierTime;
     BEGIN
        NotEarlierTime:= NOT (EarlierTime(Time1,Time2));
     END;

  FUNCTION SameTime;
     BEGIN
        SameTime := NotEarlierTime(Time1,Time2) AND NotEarlier(Time2,Time1);
     END;

  FUNCTION CompareTime;
     VAR Value1,
         Value2 : LONGINT;
     BEGIN
        Value1 := S2L(COPY(Time1,1,2))*60+S2L(COPY(Time1,4,2));
        Value2 := S2L(COPY(Time2,1,2))*60+S2L(COPY(Time2,4,2));
        CompareTime := Value2-Value1;
     END;

  FUNCTION ValidMonth;
     VAR Ergebnis : BOOLEAN;
         ZwWert : BYTE;

     BEGIN
        Ergebnis := FALSE;
        IF (LENGTH(Month) = 4) AND IsNumber(Month) THEN BEGIN
           ZWWert := S2B(COPY(Month,1,2));
           Ergebnis := (ZWWert <= 12) AND (ZWWert > 0);
        END;
        ValidMonth := Ergebnis;

     END;

  FUNCTION NextMonth;
     VAR Monat,
         Jahr  : BYTE;
     BEGIN
         Monat := S2L(ActMonth[1]+ActMonth[2]);
         Jahr  := S2L(ActMonth[3]+ActMonth[4]);
         Monat := Monat +1;
         IF Monat = 13 THEN BEGIN
             Monat := 1;
             Jahr := Jahr +1;
             IF Jahr = 100 THEN
                Jahr := 0;
          END;
          NextMonth := LeadingZeros(Monat,2)+LeadingZeros(Jahr,2);
     END;

   FUNCTION PrevMonth;
     VAR Monat,
         Jahr  : BYTE;
     BEGIN
         Monat := S2L(ActMonth[1]+ActMonth[2]);
         Jahr  := S2L(ActMonth[3]+ActMonth[4]);
         Monat := Monat -1;
         IF Monat = 0 THEN BEGIN
            Monat := 12;
            IF Jahr = 0 THEN
               Jahr := 99
            ELSE
              Jahr := Jahr -1;
         END;
         PrevMonth := LeadingZeros(Monat,2)+LeadingZeros(Jahr,2);
     END;

   FUNCTION LaterMonth;
      VAR Tag1,
          Tag2   : STRING;
      BEGIN
         Tag1 := '01.'+COPY(Month1,1,2)+'.'+COPY(Month1,3,2);
         Tag2 := '01.'+COPY(Month2,1,2)+'.'+COPY(Month2,3,2);
         LaterMonth := Later(Tag1,Tag2);
      END;

   FUNCTION EarlierMonth;
      VAR Tag1,
          Tag2   : STRING;
      BEGIN
         Tag1 := '01.'+COPY(Month1,1,2)+'.'+COPY(Month1,3,2);
         Tag2 := '01.'+COPY(Month2,1,2)+'.'+COPY(Month2,3,2);
         EarlierMonth := Earlier(Tag1,Tag2);;
      END;

   FUNCTION ValidWeek;
      VAR   NoOfDays,
            BeginofKw,
            Woche,
            Jahr   : LONGINT;
            Ergebnis :BOOLEAN;
            FirstDay : BYTE;


      BEGIN
         Ergebnis  := FALSE;
         IF (LENGTH(Week) = 5) AND (Week[3] ='/')THEN BEGIN
            Ergebnis := IsNumber(Copy(Week,1,2)) AND IsNumber(Copy(Week,4,2));
            IF Ergebnis THEN BEGIN
               Woche := S2L(COPY(Week,1,2));
               Jahr  := S2L(COPY(Week,4,2));
               IF ((Woche >= 1) AND (Woche <= 52)) THEN
                  Ergebnis := TRUE;
               IF (Woche = 53) THEN BEGIN
                  FirstDay := DayOfWeek('01.01.'+COPY(Week,4,2));
                  BeginOfKw := 1;
                  IF (FirstDay > 1) THEN
                     BeginOfKw := BeginOfKw+(8-FirstDay);
                  IF (FirstDay <= 3) THEN
                     BeginOfKw := BeginOfKw+51*7
                  ELSE
                     BeginOfKw := BeginOfKw+52*7;
                  NoOfDays := 365;
                  IF (Jahr MOD 4) = 0 THEN
                     INC(NoOfDays);
                  Ergebnis  := (BeginOfKw < (NoOfDays-1));
               END;
            END;
         END;
         ValidWeek := Ergebnis;
      END;

   FUNCTION NextWeek;
      VAR   WochenNr,
            Jahr     : LONGINT;
            ZwString : TWeek;
      BEGIN
         NextWeek := '';
         IF (LENGTH(Week) = 5) AND (Week[3] ='/')THEN BEGIN
            WochenNr := S2L(COPY(Week,1,2));
            Jahr     := S2L(COPY(Week,4,2));
            INC(WochenNr);
            IF (WochenNr > 52) THEN BEGIN
               ZwString := LeadingZeros(WochenNr,2)+COPY(Week,3,3);
               IF NOT (ValidWeek(ZwString)) THEN BEGIN
                  WochenNr := 1;
                  INC(Jahr);
                  IF (Jahr = 100) THEN
                     Jahr := 0;
               END;
            END;
            NextWeek:=LeadingZeros(WochenNr,2)+'/'+LeadingZeros(Jahr,2);
         END;
      END;

   FUNCTION PrevWeek;
      VAR   WochenNr,
            Jahr     : LONGINT;
            ZwString : TWeek;
      BEGIN
         PrevWeek := '';
         IF (LENGTH(Week) = 5) AND (Week[3] ='/')THEN BEGIN
            WochenNr := S2L(COPY(Week,1,2));
            Jahr     := S2L(COPY(Week,4,2));
            DEC(WochenNr);
            IF (WochenNr < 1) THEN BEGIN
               IF (Jahr = 0) THEN
                  Jahr := 99
               ELSE
                  DEC(Jahr);
               WochenNr := 53;

               ZwString := LeadingZeros(WochenNr,2)+'/'+LeadingZeros(Jahr,2);
               IF NOT (ValidWeek(ZwString)) THEN
                  DEC(WochenNr);
            END;
            PrevWeek:=LeadingZeros(WochenNr,2)+'/'+LeadingZeros(Jahr,2);
         END;
      END;

   FUNCTION ChangeWeek;
      VAR   Lauf     : BYTE;

      BEGIN
         ChangeWeek := '';
         IF (LENGTH(Week) = 5) AND (Week[3] ='/')THEN BEGIN

         IF (Aenderung > 0) THEN BEGIN
            FOR Lauf := 1 TO Aenderung DO
               Week:=NextWeek(Week);
         END;
         IF (Aenderung < 0) THEN BEGIN
            FOR Lauf := 1 TO (Aenderung * (-1)) DO
               Week := PrevWeek(Week);
         END;
         ChangeWeek:=Week;
         END;
      END;

   FUNCTION IsEarlierWeek;
      BEGIN
         IsEarlierWeek:=(CompareWeek(Week1,Week2) = 1);
      END;

   FUNCTION IsLaterWeek;
      BEGIN
         IsLaterWeek:=(CompareWeek(Week1,Week2) = -1);
      END;

   FUNCTION IsSameWeek;
      BEGIN
         IsSameWeek:=(CompareWeek(Week1,Week2) = 0);
      END;

   FUNCTION LaterOrSameWeek;
      BEGIN
         LaterOrSameWeek:=(CompareWeek(Week1,Week2) <= 0);
      END;

   FUNCTION EarlierOrSameWeek;
      BEGIN
         EarlierOrSameWeek:=(CompareWeek(Week1,Week2) >=0 );
      END;

   FUNCTION CompareWeek;
      { Ergebnis:
        1  = Week1 fr�her als Week2
        0  = (Week1 = Week2)
        -1 = Week2 sp�ter als Week2 }

      VAR   Jahr1,
            Jahr2  : WORD;
            Woche1,
            Woche2 : STRING[6];

      BEGIN
         Jahr1  := S2L(COPY(Week1,4,2));
         IF (Jahr1 <= CJahrhundertWechsel) THEN BEGIN
            Jahr1 := 2000+Jahr1;
         END ELSE BEGIN
            Jahr1 := 1900+Jahr1;
         END;
         Jahr2  := S2L(COPY(Week2,4,2));
         IF (Jahr2 <= CJahrhundertWechsel) THEN BEGIN
            Jahr2 := 2000+Jahr2;
         END ELSE BEGIN
            Jahr2 := 1900+Jahr2;
         END;
         Woche1:= W2S(Jahr1,4)+Week1[1]+Week1[2];
         Woche2:= W2S(Jahr2,4)+Week2[1]+Week2[2];
         IF (Woche1 < Woche2) THEN
            CompareWeek := 1
         ELSE
            IF (Woche1 = Woche2) THEN
               CompareWeek := 0
            ELSE
               CompareWeek := -1;
      END;

   FUNCTION NewDayOfWeek;
      VAR   Jahr,
            Tag,
            Monat : WORD;
            DOfW : BYTE;
            XP,
            TZ1,
            TZ2,
            JH,
            JZ : BYTE;

      BEGIN
         Tag := S2L(COPY(Datum,1,2));
         Monat := S2L(COPY(Datum,4,2));
         Jahr  := S2L(COPY(Datum,7,LENGTH(Datum)-6));
         IF (Jahr <= CJahrhundertWechsel) THEN
            Jahr := 2000+Jahr;
         IF (Jahr < 100) THEN
            Jahr := 1900+Jahr;
         JH := Jahr DIV 100;
         JZ := Jahr MOD 100;
         XP := XPosition[((JZ MOD 4 = 0) AND (( JZ <> 0) OR (JH MOD 4 <> 0))),Monat];
         TZ1 := Tabelle1[(Tag MOD 7),XP];
         TZ2 := 7-(JH MOD 7)+ (JZ + (JZ DIV 4)) MOD 7;
         IF (TZ2 > 7) THEN
            TZ2 := TZ2-7;
         DOfW := (TZ1+TZ2-1) MOD 7;

         IF (DofW = 0) THEN DofW := 7;
         NewDayOfWeek := DOfW;
      END;

   PROCEDURE SetDefStr;
      BEGIN
         IF (Txt = '') THEN
            Txt := Def;
      END;

   PROCEDURE SetDefChr;
      BEGIN
         IF (Txt = #0) THEN
            Txt := Def;
      END; 
   PROCEDURE SetDefByte; 
      BEGIN
         IF (Zahl = 0) THEN
            Zahl := Def;
      END; 
   PROCEDURE SetDefLong; 
      BEGIN
         IF (Zahl = 0) THEN
            Zahl := Def;
      END; 
   PROCEDURE SetDefReal;
      BEGIN
         IF (Zahl = 0) THEN
            Zahl := Def; 
      END; 
   PROCEDURE SetDefDouble; 
      BEGIN
         IF (Zahl = 0) THEN
            Zahl := Def; 
      END;
   PROCEDURE SetDefWord; 
      BEGIN
         IF (Zahl = 0) THEN
            Zahl := Def; 
      END; 
   
   PROCEDURE MemCopy; assembler; 
      ASM
      PUSH BP
      PUSH SP
      PUSH SS
      PUSH DS
      CLD; 
      MOV CX,Size; 
      LDS SI,Source; 
      LES DI,Dest; 
      @BeginOfCopyLoop:
      LODSB;
      STOSB; 
      LOOP @BeginOfCopyLoop;
      POP DS
      POP SS
      POP SP
      POP BP
      END;

   FUNCTION  ErrMessage(ErrNum:INTEGER):STRING;
      VAR FaultString :STRING;
      BEGIN
         FaultString := '';
         CASE ErrNum OF
              1: FaultString := 'Invalid function number';
              2: FaultString := 'File not found';
              3: FaultString := 'Path not found';
              4: FaultString := 'Too many open files';
              5: FaultString := 'File access denied';
              6: FaultString := 'Invalid file handle';
             12: FaultString := 'Invalid file access mode';
             15: FaultString := 'Invalid drive number';
             16: FaultString := 'Cannot remove current directory';
             17: FaultString := 'cannot rename across drives';
             18: FaultString := 'no more files';
            100: FaultString := 'Disk read error';
            101: FaultString := 'Disk write error';
            102: FaultString := 'File not assigned';
            103: FaultString := 'File not open';
            104: FaultString := 'File not open for input';
            105: FaultString := 'File not open for output';
            106: FaultString := 'Invalid numeric format';
            150: FaultString := 'Disk is write protected';
            151: FaultString := 'Unknown unit';
            152: FaultString := 'Drive not ready';
            153: FaultString := 'Unknown command';
            154: FaultString := 'CRC error in data';
            155: FaultString := 'Bad drive request structure length';
            156: FaultString := 'Disk seek error';
            157: FaultString := 'Unknown media type';
            158: FaultString := 'Sector not found';
            159: FaultString := 'Printer out of paper';
            160: FaultString := 'Device write fault';
            161: FaultString := 'Device read fault';
            162: FaultString := 'Hardware failure';
         END;
         ErrMessage := FaultString;
      END;
   FUNCTION Bittest;
      VAR HelpLong : LONGINT;
      BEGIN
         HelpLong := 1;
         HelpLong := (HelpLong SHL BitNummer);
         BitTest := ((TestZahl AND HelpLong) <> 0);
      END;

   FUNCTION KeyId;
      CONST CursorBlock: ARRAY[-83..-71] OF STRING[10] =
            ('Entf','Einfg','BildRunter','Runter','Ende','',
             'Rechts','','Links','','BildHoch','Hoch','Pos1');
      CONST TastaturZeile1 : ARRAY[-25..-16] OF STRING[1]=
            ('P','O','I','U','Y','T','R','E','W','Q');
      CONST TastaturZeile2 : ARRAY[-38..-30] OF STRING[1]=
            ('L','K','J','H','G','F','D','S','A');
      CONST TastaturZeile3 : ARRAY[-50..-44] OF STRING[1]=
            ('M','N','B','V','C','X','Z');

      VAR Ergebnis :STRING;
      BEGIN
         CASE Taste OF
            32..255   : Ergebnis := CHR(Taste);
            13        : Ergebnis := 'RETURN';
            27        : Ergebnis := 'ESC';
            8         : Ergebnis := 'BS';
            9         : Ergebnis := 'TAB';
            -25..-16  : Ergebnis := 'ALT-'+TastaturZeile1[Taste];
            -38..-30  : Ergebnis := 'ALT-'+TastaturZeile2[Taste];
            -50..-44  : Ergebnis := 'ALT-'+TastaturZeile3[Taste];
            -15       : Ergebnis := 'Shift-TAB';
            -83..-71  : Ergebnis := CursorBlock[Taste];
            -68..-59  : Ergebnis := 'F'+L2S(Abs(Taste)-58,0);
            -93..-84  : Ergebnis := 'Shift-F'+L2S(Abs(Taste)-83,0);
            -103..-94 : Ergebnis := 'STRG-F'+L2S(Abs(Taste)-93,0);
            -113..-104: Ergebnis := 'ALT-F'+L2S(Abs(Taste)-103,0);
            ELSE Ergebnis := '';
         END;
         KeyId := Ergebnis;
      END;


END.

{============================
 Versionshistorie
 $Log:$
 ============================}
