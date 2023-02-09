/*
Unimake Software

Exemplo de uso da DLL do UniDanfe

Data: 16/02/2022 
Autores: 
- Edson Mundin Ferreira
- Wandrey Mundin Ferreira
*/
#include "hbver.ch"
Function Main()
   Local aOpcoes
   Local nOpcao               

   aOpcoes := {}
   AAdd(aOpcoes, "1-Executar Tela de Configuracoes")
   AAdd(aOpcoes, "2-Imprimir o DANFE/DACTE/DAMDFE/ETC")
   
   Do While .T.
      Cls

      @ 1,2 Say "UniDanfe DLL for " + Version()

      nOpcao := Achoice( 3, 2, 20, 40, aOpcoes)

      Cls

      do case
         case LastKey() = 27
              Exit

         case nOpcao = 1
              TelaConfiguracao()

         case nOpcao = 2
              ImprimirDFE()
      endcase
   EndDo
Return NIL