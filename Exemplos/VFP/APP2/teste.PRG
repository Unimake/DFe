*x=inpbox("Digite a Data","data",DATE())
*x=inpbox("Digite o Valor a Ser CONSULTADO","Valor",0.00,"99999-999")
*? X
x=inpbox("Digite o Valor a Ser CONSULTADO","Valor"," ")
*? X



FUNCTION diasemana(data_sel)
*---------------------------
LOCAL knomedia
DO CASE
   CASE DOW(data_sel) = 1
      	knomedia = "Domingo"
   CASE DOW(data_sel) = 2	
        knomedia = "Segunda-Feira"
        CASE DOW(data_sel) = 3
        	knomedia = "Terça-Feira"
        	CASE DOW(data_sel) = 4
        		knomedia = "Quarta-Feira"
        		CASE DOW(data_sel) = 5	
        		knomedia = "Quinta-Feira"
        		CASE DOW(data_sel) = 6	
        		knomedia = "Sexta-Feira"
        		CASE DOW(data_sel) = 7	
        		knomedia = "Sábado"
        		ENDCASE
        		RETURN knomedia
