* Funções para compatibilidade hb30 e xHarbour
* Unimake Software
* Edson / Wandrey

FUNCTION DateTime()
   local dt := hb_DToT( Date(), Time()) 
RETURN dt

FUNCTION TTOS( dDate )
RETURN DtoS(dDate) + "000000.000"