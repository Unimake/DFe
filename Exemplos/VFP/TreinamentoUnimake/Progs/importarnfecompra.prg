* ---------------------------------------------------------------------------------
* B2B - Importação das notas fiscais de compra através do XML da NFe, utilizando a DLL Unimake.DFe.
* - Cadastrando produtos automaticamente através dos dados dos XML das notas fiscais de compra;
* - Cadastrando fornecedores automaticamente através dos dados dos XML das notas fiscais de compra;
* - Cadastrando a nota fiscal de entrada automaticamente através dos dados do XML das notas fiscais de compra.
* ---------------------------------------------------------------------------------
FUNCTION ImportarNFeCompra()
   LOCAL oErro, oExceptionInterop, oNFeProc, oInfNFe, I, oDet, oDup
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")     
      
   TRY
      oNFeProc = CREATEOBJECT("Unimake.Business.DFe.Xml.NFe.NfeProc")
      oNFeProc = oNFeProc.LoadFromFile("D:\testenfe\41230506117473000150550010000590057431907008-procnfe.xml")
      oInfNFe = oNFeProc.NFe.GetInfNFe(0)

      MESSAGEBOX(oInfNFe.Chave) && Chave da NFe
      
    * Algumas informações do grupo de tag <ide>  
      MESSAGEBOX(oInfNFe.Ide.NNF) && Número da nota fiscal
      MESSAGEBOX(oInfNFe.Ide.DhEmi) && Data de Emissão
      MESSAGEBOX(oInfNFe.Ide.DhEmiField) && Data de Emissão
      MESSAGEBOX(oInfNFe.Ide.DhSaiEnt) && Data de Saída/Entrada
      MESSAGEBOX(oInfNFe.Ide.DhSaiEntField) && Data de Saída/Entrada
      
    * Algumas informações do emitente da nota  
      MESSAGEBOX(oInfNFe.Emit.CNPJ) && CNPJ do emitente da nota
      MESSAGEBOX(oInfNFe.Emit.xNome) && Razão Social do emitente da nota
      MESSAGEBOX(oInfNFe.Emit.EnderEmit.XLgr) && Endereço do emitente - Logradouro
      MESSAGEBOX(oInfNFe.Emit.EnderEmit.CMun) && Endereço do emitente - Código do município
      MESSAGEBOX(oInfNFe.Emit.EnderEmit.XMun) && Endereço do emitente - Nome do município
      
    * Algumas informações dos produtos da nota fiscal
      FOR I = 1 TO oInfNFe.GetDetCount() && Quantidade de produtos que tem no XML
         oDet = oInfNFe.GetDet(I - 1)

         MESSAGEBOX(oDet.NItem) && Número do Item
         MESSAGEBOX(oDet.Prod.CProd) && Código do produto
         MESSAGEBOX(oDet.Prod.XProd) && Descrição do produto
         MESSAGEBOX(oDet.Prod.cEAN) && EAN
         MESSAGEBOX(oDet.Prod.CBarra) && Código de Barra
         MESSAGEBOX(oDet.Prod.NCM) && NCM
         MESSAGEBOX(oDet.Prod.CFOP)
         MESSAGEBOX(oDet.Prod.QCom)
         MESSAGEBOX(oDet.Prod.UCom)
         MESSAGEBOX(oDet.Prod.VUnCom)
         MESSAGEBOX(oDet.Prod.VProd)
         
         IF .NOT. ISNULL(oDet.Imposto.ICMS.ICMSSN101)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN101.Orig)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN101.CSOSN)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN101.PCredSN)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN101.VCredICMSSN)
         ENDIF
         
         IF .NOT. ISNULL(oDet.Imposto.ICMS.ICMSSN102)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN102.Orig)
            MESSAGEBOX(oDet.Imposto.ICMS.ICMSSN102.CSOSN)
         ENDIF
         
         IF .NOT. ISNULL(oDet.Imposto.IPI)
            MESSAGEBOX(oDet.Imposto.IPI.CEnq)
            
            IF .NOT. ISNULL(oDet.Imposto.IPI.IPINT)
               MESSAGEBOX(oDet.Imposto.IPI.IPINT.CST)
            ENDIF 
         ENDIF
      NEXT I
      
    * Algumas informações das cobranças
      MESSAGEBOX(oInfNFe.Cobr.Fat.NFat)  
      MESSAGEBOX(oInfNFe.Cobr.Fat.VOrig)
      MESSAGEBOX(oInfNFe.Cobr.Fat.VDesc)
      MESSAGEBOX(oInfNFe.Cobr.Fat.VLiq)
      
    * Algumas informações das duplicadas
      FOR I = 1 TO oInfNFe.Cobr.GetDupCount() && Quantidade de duplicatas
         oDup = oInfNFe.Cobr.GetDup(I - 1)
         
         MESSAGEBOX(oDup.NDup)
         MESSAGEBOX(oDup.DVenc)
         MESSAGEBOX(oDup.DVencField)
         MESSAGEBOX(oDup.VDup)
      NEXT I  
   
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      