SELECT 1
USE TABNCM
GOTO TOP
SELECT 2
USE TABIBPV
INDEX ON CLAS_FISC TO TAB
USE TABIBPV INDEX TAB
SELE 1
GOTO TOP
DO WHILE .NOT. EOF()
   vcls=ncm
   SELECT 2
   SEEK VCLS
   IF .NOT. EOF()
      Xaliqnac = aliqnac
      Xaliqimp = aliqimp
      SELECT 1
      REPLACE aliqnac WITH xaliqnac
      replace aliqimp with xaliqimp
   ENDIF
   SELECT 1
   SKIP
enddo                