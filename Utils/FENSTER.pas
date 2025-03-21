{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Sep 14 08:00:10 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT FENSTER;

   INTERFACE

      PROCEDURE BRAIN(nr,x1,y1,xl,yl:WORD);
      PROCEDURE REMEMBER(nr:BYTE);

   IMPLEMENTATION

      USES  GRAPH;

      VAR   PImage:  ARRAY[1..8] OF POINTER;
            PImageX,
            PImageY,
            PSize:   ARRAY[1..8] OF WORD;


      { Windowtechnik Grafikmodus }

        PROCEDURE BRAIN(nr,x1,y1,xl,yl:WORD);
           BEGIN
             PSize[nr]:=IMAGESIZE(x1,y1,xl,yl);
             GETMEM(PImage[nr],PSize[nr]);
             GETIMAGE(x1,y1,xl,yl,PImage[nr]^);
             PImageX[nr]:=x1;
             PImageY[nr]:=y1
           END;

        PROCEDURE REMEMBER(nr:BYTE);
           BEGIN
             PUTIMAGE(PImageX[nr],PImageY[nr],PImage[nr]^,NormalPut);
             FREEMEM(PImage[nr],PSize[nr])
           END;


   END.
{============================
 Versionshistorie
 $Log:$
 ============================}
