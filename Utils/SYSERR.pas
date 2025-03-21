{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:04:48 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT SysErr;        { Errorbehandlung fr Grafiksystem
                        benutzt PLACEWIN - InfoBox,FaultBox, RequestBox }


INTERFACE

   PROCEDURE EnableErrHandler;

   PROCEDURE DisableErrHandler;

   PROCEDURE OnlyNewErrors;

   PROCEDURE AllErrors;

   FUNCTION Enabled:BOOLEAN;

   FUNCTION OnlyNew:BOOLEAN;


IMPLEMENTATION
   USES GMCRT,
        GRAPH,
        DOS,
        ZUSAETZE,
        PLACEWIN;

   TYPE TErrInfoFenster = OBJECT(TBrainFenster)
           CONSTRUCTOR Init;
           PROCEDURE Act(Zeile1,Zeile2:STRING);VIRTUAL;
           PROCEDURE ShowWin;VIRTUAL;
           PROCEDURE Hide;VIRTUAL;
        END;

   TYPE TErrRequest = OBJECT(TBrainFenster)
           CONSTRUCTOR Init;
           PROCEDURE ShowWin;VIRTUAL;
           FUNCTION  Act(Meldung : STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Hide;VIRTUAL;
        END;

   TYPE TErrTriplequest = OBJECT(TBrainFenster)
           CONSTRUCTOR Init;
           PROCEDURE ShowWin;VIRTUAL;
           FUNCTION  Act(Meldung : STRING):INTEGER;VIRTUAL;
           PROCEDURE Hide;VIRTUAL;
        END;

   VAR LastErrNum : INTEGER;
       IgnoreFurtherErrors,
       IsEnabled : BOOLEAN;
       I24Save,
       ExitSave   : POINTER;
       Lauf       : LONGINT;
       ErrMsg     : TErrInfoFenster;
       ErrRequest : TErrRequest;
       ErrTripleQuest : TErrTriplequest;


  CONSTRUCTOR TErrInfoFenster.Init;
   BEGIN
     TFenster.Init(170,200,470,280,0,6,2,'Information');
     Groesse := IMAGESIZE(170,200,480,290);
     IF (MAXAVAIL > Groesse) THEN BEGIN
        GETMEM(AlterBildschirm,Groesse);
        AllesErzeugt := TRUE;
     END ELSE BEGIN
        AllesErzeugt := FALSE;
        AlterBildschirm := NIL;
     END;
     Aktiv := FALSE;
   END;

   PROCEDURE TErrInfoFenster.Act;

     VAR Taste : INTEGER;

     BEGIN

       ShowWin;
       WRITE(chr(7));
       Placetext(-1,1,Zeile1,255,255);
       Placetext(-1,2,Zeile2,255,255);
       Placetext(1,3,'Taste...',255,255);
       IF KEYPRESSED THEN Taste :=INPUTKEY;
       Taste := INPUTKEY;
       Hide;
     END;

  PROCEDURE TErrInfoFenster.ShowWin;

    BEGIN
      IF NOT Aktiv THEN BEGIN
        GETIMAGE(Links,Oben,Rechts+10,Unten+10,AlterBildschirm^);
        TFenster.ShowWin;
        Aktiv := TRUE;
      END;
    END;

  PROCEDURE TErrInfoFenster.Hide;

    BEGIN
      IF Aktiv THEN BEGIN
        PUTIMAGE(Links,Oben,AlterBildschirm^,COPYPUT);
        Aktiv := FALSE;
      END;
    END;

  CONSTRUCTOR TErrRequest.Init;

   BEGIN
     TFenster.Init(170,200,470,280,0,7,2,'Best„tigung');
     Groesse := IMAGESIZE(170,200,480,290);
     IF (MAXAVAIL > Groesse) THEN BEGIN
        GETMEM(AlterBildschirm,Groesse);
        AllesErzeugt := TRUE;
     END ELSE BEGIN
        AllesErzeugt := FALSE;
        AlterBildschirm := NIL;
     END;
     Aktiv := FALSE;
   END;

  PROCEDURE TErrRequest.ShowWin;

    BEGIN
      IF NOT Aktiv THEN BEGIN
        GETIMAGE(Links,Oben,Rechts+10,Unten+10,AlterBildschirm^);
        TFenster.ShowWin;
        Aktiv := TRUE;
      END;
    END;

   FUNCTION TErrRequest.Act;
     VAR Ende,
         Zustand : BOOLEAN;
         EndCode : INTEGER;

     BEGIN
       Zustand := FALSE;
       ShowWin;
       Placetext(-1,1,Meldung,255,255);
       Ende := FALSE;
       REPEAT
         IF Zustand THEN BEGIN
           PlaceText(5,2,'  Ja  ',7,2);
           PlaceText(22,2,' Nein ',255,255);
         END ELSE BEGIN
           PlaceText(5,2,'  Ja  ',255,255);
           PlaceText(22,2,' Nein ',7,2);
         END;
         EndCode := InputKey;
         CASE EndCode OF
            27,
            78,
            110 : BEGIN
                   Ende   := TRUE;
                   Zustand:= FALSE;
                 END;
            74,
            106 : BEGIN
                   Ende := TRUE;
                   Zustand:= TRUE;
                  END;
            13 : Ende := TRUE;

            -72,
            -75,
            -77,
            -80,
            -15,
            9  : Zustand := NOT Zustand;
         END;
       UNTIL Ende;
       Act := Zustand;
       Hide;
     END;

  PROCEDURE TErrRequest.Hide;

    BEGIN
      IF Aktiv THEN BEGIN
        PUTIMAGE(Links,Oben,AlterBildschirm^,COPYPUT);
        Aktiv := FALSE;
      END;
    END;
  CONSTRUCTOR TErrTripleQuest.Init;

   BEGIN
     TFenster.Init(170,200,470,280,0,7,2,'Best„tigung');
     Groesse := IMAGESIZE(170,200,480,290);
     IF (MAXAVAIL > Groesse) THEN BEGIN
        GETMEM(AlterBildschirm,Groesse);
        AllesErzeugt := TRUE;
     END ELSE BEGIN
        AllesErzeugt := FALSE;
        AlterBildschirm := NIL;
     END;
     Aktiv := FALSE;
   END;

  PROCEDURE TErrTripleQuest.ShowWin;

    BEGIN
      IF NOT Aktiv THEN BEGIN
        GETIMAGE(Links,Oben,Rechts+10,Unten+10,AlterBildschirm^);
        TFenster.ShowWin;
        Aktiv := TRUE;
      END;
    END;

   FUNCTION TErrTripleQuest.Act;
      VAR Ende    : BOOLEAN;
          Zustand : INTEGER;
          EndCode : INTEGER;
      BEGIN
         Zustand := 1;
         ShowWin;
         Placetext(-1,1,Meldung,255,255);
         Ende := FALSE;
         REPEAT
            CASE Zustand OF
               1 : BEGIN
                      PlaceText(2,2, '   Ja    ',7,2);
                      PlaceText(13,2,'  Nein   ',255,255);
                      PlaceText(24,2,' Abbruch ',255,255);
                   END;
               0 : BEGIN
                      PlaceText(2,2, '   Ja    ',255,255);
                      PlaceText(13,2,'  Nein   ',7,2);
                      PlaceText(24,2,' Abbruch ',255,255);
                   END;
               -1: BEGIN
                      PlaceText(2,2, '   Ja    ',255,255);
                      PlaceText(13,2,'  Nein   ',255,255);
                      PlaceText(24,2,' Abbruch ',7,2);
                   END;
            END;
            EndCode := InputKey;
            CASE EndCode OF
               27,
               65,
               97  : BEGIN
                        Ende   := TRUE;
                        Zustand:= -1;
                     END;
               78,
               110 : BEGIN
                        Ende   := TRUE;
                        Zustand:= 0;
                     END;
               74,
               106 : BEGIN
                        Ende := TRUE;
                        Zustand:= 1;
                     END;
               13  : Ende := TRUE;

               -72,
               -75,
               -15 : BEGIN
                        INC(Zustand);
                        IF (Zustand = 2) THEN
                           Zustand := -1;
                     END;
               -77,
               -80,
               9   : BEGIN
                        DEC(Zustand);
                        IF (Zustand = -2) THEN
                           Zustand := 1;
                     END;
            END;
         UNTIL Ende;
         Act := Zustand;
         Hide;
      END;

  PROCEDURE TErrTripleQuest.Hide;

    BEGIN
      IF Aktiv THEN BEGIN
        PUTIMAGE(Links,Oben,AlterBildschirm^,COPYPUT);
        Aktiv := FALSE;
      END;
    END;


   {$F+}
   PROCEDURE NSysError(Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP : WORD); interrupt;
      VAR EndCode : WORD;
          TEndCode,
          ErrorCode : INTEGER;
          Zeile1,
          Zeile2    : STRING;

      BEGIN
          ASM
            STI
          END;
          EndCode := AX AND $FF00;
          INC(Lauf);
          ErrorCode := DI AND $00FF;
          IF (NOT IgnoreFurtherErrors) OR
           (IgnoreFurtherErrors AND (LastErrNum <> ErrorCode)) THEN BEGIN

            CASE ErrorCode OF
                9 : BEGIN
                       Zeile1 := 'Kein Papier. Wiederholen ?';
                    END;
               10 : BEGIN
                       Zeile1 := 'Drucker OFFLINE. Widerholen ?';
                    END;
               ELSE BEGIN
                       Zeile1 := 'Achtung DOS Fehler';
                       Zeile2 := 'Nummer '+L2S(ErrorCode,2);
                    END;
            END;
            IF ((ErrorCode >= 9) AND (ErrorCode <= 10)) THEN BEGIN
               TEndCode :=ErrTripleQuest.Act(Zeile1);
               CASE TEndCode OF
                  1 :BEGIN
                        LastErrNum := -1;
                        EndCode := Endcode OR $0001;
                     END;
                  0 :BEGIN
                        LastErrNum := ErrorCode;
                        IgnoreFurtherErrors := TRUE;
                        EndCode := Endcode OR $0000;
                     END;
                  -1:BEGIN
                        LastErrNum := ErrorCode;
                        EndCode := Endcode OR $0000;
                     END;
               END;
            END ELSE BEGIN
               ErrMsg.Act(Zeile1,Zeile2);
               IF ErrRequest.Act('Wiederholen ?') THEN BEGIN
                  LastErrNum := -1;
                  EndCode := Endcode OR $0001;
               END ELSE BEGIN
                  LastErrNum := ErrorCode;
                  EndCode := Endcode OR $0000;
               END;
            END;
         END;
         AX := EndCode;
      END;
   {$F-}

   PROCEDURE EnableErrHandler;

      BEGIN
        IF NOT IsEnabled THEN BEGIN
           SetIntVec($24,@NSysError);
           IsEnabled := TRUE;
        END;
      END;

   PROCEDURE DisableErrHandler;

      BEGIN
        IF IsEnabled THEN BEGIN
           SetIntVec($24,I24Save);
           IsEnabled := FALSE;
        END;
      END;

   PROCEDURE OnlyNewErrors;
      BEGIN
        LastErrNum := -1;
        IgnoreFurtherErrors := TRUE;
      END;

   PROCEDURE AllErrors;
      BEGIN
        LastErrNum := -1;
        IgnoreFurtherErrors := FALSE;
      END;

   FUNCTION Enabled;
      BEGIN
         Enabled := IsEnabled;
      END;

   FUNCTION OnlyNew;
      BEGIN
         OnlyNew := IgnoreFurtherErrors;
      END;


  PROCEDURE SysErrExit; FAR;
     BEGIN
        ExitProc := ExitSave;
        IF IsEnabled THEN
           DisableErrHandler;
     END;

BEGIN
   ErrMsg.Init;
   ErrRequest.Init;
   ErrTripleQuest.Init;
   Lauf := 0;
   LastErrNum := -1;
   IgnoreFurtherErrors := FALSE;
   GetIntVec($24,I24Save);
   ExitSave := ExitProc;
   ExitProc := @SysErrExit;
   EnableErrHandler;
END.


{============================
 Versionshistorie
 $Log:$
 ============================}
