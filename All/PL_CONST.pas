{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Apr 12 07:24:26 GMT+02:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT PL_CONST;


INTERFACE

   CONST XZero =   5;
         YZero =  23;
         XSize = 460;
         YSize = 400;

   CONST COL: ARRAY[0..9] OF BYTE = (0,4,3,1,2,0,4,3,1,2);

   TYPE  PlanungsRec = RECORD
                         KNr: INTEGER;
                         Nam: STRING[40];
                         Fir: BOOLEAN;
                         APA: BYTE;
                         APN: STRING[20];
                         Etg: STRING[ 1];
                         LNr,
                         PQu,
                         Fal: STRING[5];
                         Stk: WORD;
                         Tim: ARRAY[1..4] OF STRING[5];
                         Akt: STRING[5];
                         Ste: STRING[25];
                         Weg: ARRAY[1..8] OF STRING[40];
                         Dat: STRING[8];

                         Tur,
                         InT: WORD;
                       END;

         Planungszeiger = ^Planungsliste;

         PlanungsListe = RECORD
                           Next,
                           PrevTi,
                           NextTi,
                           PrevIT,
                           NextIT: Planungszeiger;
                           Dat:  PlanungsRec
                         END;

         Posi = ARRAY[1..3] OF BYTE;
         TFahrerString = STRING[30];



   CONST PQText: ARRAY[0..15] OF STRING[17] = ('Au·erhalb Gebiet',
                                               'Vohwinkel',
                                               'Saurenhaus',
                                               'Elberfeld West',
                                               'Katernberg',
                                               'Cronenberg',
                                               'Elberfeld SÅd',
                                               'Elberfeld Nord',
                                               'öllendahl',
                                               'Barmen SÅd',
                                               'Barmen Nord',
                                               'Ronsdorf',
                                               'Hatzfeld',
                                               'Langerfeld',
                                               'Schwelm',
                                               '');



IMPLEMENTATION


END.
{============================
 Versionshistorie
 $Log:$
 ============================}
