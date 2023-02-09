	            mesano=SUBSTR(DTOC(DATE()),4,2)+ALLTRIM(STR(YEAR(DATE()),4))
		                  PFILE=(FULLPATH(CURDIR())+ "Retorno\"+mesano+"\"+VXML)
      