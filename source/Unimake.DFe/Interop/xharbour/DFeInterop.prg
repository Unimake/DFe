Function Main()

 * Testar se o certificado está vencido
   CertificadoVencido()

   ? "Terminou..."
   Inkey(0)
Return

Function CertificadoVencido()
   Local cCertA1Normal := "S:\Contabil\Certificado Digital\Unimake PV.pfx"
   Local cCertA1Vencido := "D:\Wandrey - Casa\Certificados\CEFAC.pfx"
   Local cSenhaCertA1 := "123456"
   Local cSenhaCertA1Vencido := "12345678"

 * Criar o objeto da classe da Unimake.DFe que trabalha com certificado digital
   oDLL     := LwGetObject("Unimake.Security.Platform.CertificadoDigital")

 * Testar um certificado normal, ou seja, que não está vencido.
   oCert    := oDLL:CarregarCertificadoDigitalA1(cCertA1Normal, cSenhaCertA1) //Carregar o certificado digital para uma propriedade
   lVencido := oDLL:Vencido(oCert) //Verificar se o certificado está vencido

   If ! lVencido
      ? "Certificado não está vencido..."
   Else
      ? "Certificado está vencido..."
   Endif

 * Testando um certificado que está vencido
   oCert    := oDLL:CarregarCertificadoDigitalA1(cCertA1Vencido, cSenhaCertA1Vencido) //Carregar o certificado digital para uma propriedade
   lVencido := oDLL:Vencido(oCert) //Verificar se o certificado está vencido

   If ! lVencido
      ? "Certificado não está vencido..."
   Else
      ? "Certificado está vencido..."
   Endif
Return



PROCEDURE LwGetObject( cObject, lMostraErro )
   LOCAL oObj, oErr

   If lMostraErro = NIL
      lMostraErro := .T.
   EndIf

   TRY
      oObj := GetActiveObject( cObject )
   CATCH
      TRY
         oObj := CreateObject( cObject )
      CATCH oErr
         If lMostraErro
            Alert( "ERRO! " + cObject + " não disponível. [" + Ole2TxtError()+ "]" )
         Else
            Throw( oErr )
            RETURN NIL
         EndIf
      END
   END
RETURN oObj