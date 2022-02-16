/*

Unimake Software
Exemplo de uso da DLL Unimake.Dfe
Data: 15/02/2022 
Autores: 
- Edson Mundin Ferreira
- Wandrey Mundin Ferreira

*/
Function Main()
   Local aOpcoes
   
   aOpcoes := {}
   AAdd(aOpcoes, "1-Carregar Certificado A1")
   AAdd(aOpcoes, "2-Consulta Status Nfe")
   AAdd(aOpcoes, "3-Consulta Situacao Nfe")
   AAdd(aOpcoes, "4-Inutilizar Numero Nfe")
   
   Do While .T.
      Cls
	  
      nOpcao := Achoice( 2, 2, 20, 30, aOpcoes)

      Cls

      do case
         case LastKey() = 27
              Exit

         case nOpcao = 1
              CarregarCertificadoA1()

         case nOpcao = 2
              ConsultaStatusNfe()

         case nOpcao = 3
              ConsultaSituacaoNfe()

         case nOpcao = 4
              InutilizarNumeroNfe()
      endcase
   EndDo
Return