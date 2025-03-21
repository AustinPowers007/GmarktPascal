   { Units zum Lesen der GMARKT-Daten }

   USES  GMBASE,
         GMPRT,
        { GMCONST,}
         PL_CONST,
         ZUSAETZE,
         GMPLDO,
         PLACEWIN,
         CASHDATA,
         CASHNEU,
         GMDATEI,
         LIEDATEN,
         KUNDATEN,
         BUTTONS,
         GMSETUP,
         GMDELAY,
         FENSTER,

   { Units f�r Planung }

         GMCRT,
         GRAPH,
         PRINTER,
         GRAPNEU,
         GMCONST,
         MAUSUNIT;

      TYPE InFeldPtr = ^FeldListe;

           FeldListe = RECORD
                         Next:  InFeldPtr;
                         Kunde: PlanungsZeiger;
                         Pos:   BYTE;
                       END;

   CONST TCol : ARRAY[BOOLEAN] OF BYTE = ( 3,0);

   CONST MaxTime = 4;
         ATimeS: ARRAY[1..MaxTime] OF STRING[5] = ('08:00','13:00','17:00','22:00');
         ATimeE: ARRAY[1..MaxTime] OF STRING[5] = ('08:00','12:59','16:59','22:00');
         ATime:  ARRAY[1..MaxTime] OF STRING[2] = ('08','13','17','22');

   CONST MaxTour = 3;
         ATour:  ARRAY[1..MaxTour] OF STRING[1] = ('V','N','A');
         ATNum:  ARRAY[1..TAbsT]   OF STRING[2] = ('1','2','3','4','5','6','7','8','9','10');

   CONST PQMinOstWest = 72;
         PQMaxOstWest = 97;
         PQMinNordSued = 73;
         PQMaxNordSued = 90;
         PQMinSubQuadrat = 1;
         PQMaxSubQuadrat = 4;



         BSize =   18;
         BMargin =  2;

   TYPE  TFeldTypPtr = ^TFeldTyp;
         TFeldTyp =    ARRAY[1..30,1..30,1..4] OF BYTE;
   VAR   PF:   FILE OF PlanungsRec;
         PR:   PlanungsRec;

         Fahrer:        ARRAY[1..TGesamt] OF STRING[30];
         LaufKunde,
         LaufKiste:     ARRAY[1..TGesamt] OF INTEGER;
         LeerKunde,
         LeerKiste:     ARRAY[1.. 3] OF INTEGER;

         HighX,LowX,
         HighY,LowY:    ARRAY[0.. 3] OF INTEGER;
         MHighX,MLowX,
         MHighY,MLowY,

         Th,Tm,

         GesKistCount,
         GesKundCount:INTEGER;
         THighX,TLowX,
         THighY,TLowY: LONGINT;
         MinKun:       ARRAY[1..TGesamt] OF INTEGER;

         TourStart:    ARRAY[1..TGesamt] OF Planungszeiger;
         TourAktPs,
         TimeStart,
         First,
         Merke,
         AktPr:        PlanungsZeiger;

         Changes:      BOOLEAN;

         Feld:         TFeldTypPtr;
         StartT:       ARRAY[1..TGesamt] OF STRING[5];
         IsPQ,
         TimeS,TimeSOld,
         TimeE,TimeEOld: STRING[5];
         Ths,
         Tms:          STRING[2];
         TimeIs:       STRING[5];
         TimeNrS,
         TimeNrE:      BYTE;

         UebDatumFenster: TEingabeFensterPtr;
         UebDatum:        TDatum;
         Anz     :        LONGINT;

         TNr :BYTE;
         Ende : BOOLEAN;

         PQ:           ARRAY[PQMinOstWest..PQMaxOstWest,
                             PQMinNordSued..PQMaxNordSued,
                             PQMinSubQuadrat..PQMaxSubQuadrat] OF BYTE;
         PQSum:        ARRAY[0..TGesamt,1..2] OF BYTE;
         PQAlle:       WORD;

         TDiffX,
         TDiffY:       INTEGER;
         XLen,
         YLen:         INTEGER;

