{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Wed Oct 23 15:37:30 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMPRT;

{$I-,S-}

INTERFACE


   PROCEDURE PrtToFile(Name:STRING);
   PROCEDURE PrintertoFile(Datum:STRING);
   PROCEDURE PrintertoPrt;

IMPLEMENTATION

   USES PRINTER;

   PROCEDURE LstBinaryMode;
   INLINE(
      $8B/$1E/Lst/        { MOV   BX,Lst.Handle }
      $B8/$00/$44/        { MOV   AX,4400H      }
      $CD/$21/            { INT   21H           }
      $80/$CA/$20/        { OR    DL,20H        }
      $B6/$00/            { MOV   DH,0          }
      $B8/$01/$44/        { MOV   AX,4401H      }
      $CD/$21);           { INT   21H           }

   PROCEDURE SetToPrinter;

     begin
        CLOSE(Lst);
        ASSIGN(Lst,'LPT1');
        REWRITE(Lst);
        LstBinaryMode;
     end;

   PROCEDURE PrintertoFile(Datum:STRING);
      VAR FName : STRING;

      BEGIN
         FName := 'T'+COPY(Datum,7,2)+COPY(Datum,4,2)+COPY(Datum,1,2)+'.TXT';
         PrtToFile(FName);
      END;

   PROCEDURE PrtToFile;
      VAR FName : STRING;

      BEGIN
         CLOSE(LST);
         ASSIGN(LST,Name);
         REWRITE(LST);
      END;

   PROCEDURE PrintertoPrt;
      BEGIN
        SetToPrinter;
      END;
end.
{============================
 Versionshistorie
 $Log:$
 ============================}
