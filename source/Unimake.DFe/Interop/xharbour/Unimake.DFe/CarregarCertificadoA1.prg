* ---------------------------------------------------------------------------------
* Carregando certificado A1
* ---------------------------------------------------------------------------------
Function CarregarCertificadoA1()
   Local Certificado

 * Criar objeto com Certificado A1 informado
   Certificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
   CertificadoSelecionado = Certificado:CarregarCertificadoDigitalA1("C:\Projetos\certificados\UnimakePV.pfx","12345678")

   ? "ID do Certificado....: ", Certificado:GetThumbPrint(CertificadoSelecionado)
   ? "Dados do proprietario: ", Certificado:GetSubject(CertificadoSelecionado)
   ? "Numero de Serie......: ", Certificado:GetSerialNumber(CertificadoSelecionado)
   ? "Validade Inicial.....: ", Certificado:GetNotBefore(CertificadoSelecionado)
   ? "Validade Final.......: ", Certificado:GetNotAfter(CertificadoSelecionado)
   ? "Certificado vencido?.: ", Certificado:Vencido(CertificadoSelecionado)
   ?
   Wait
RETURN