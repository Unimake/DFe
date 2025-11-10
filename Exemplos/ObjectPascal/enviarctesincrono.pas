// ------------------------------------------------------------------
// Enviar CTe no modo síncrono
// ------------------------------------------------------------------

unit EnviarCteSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarCteSincrono = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarCteSincrono.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  // --- Variáveis do XML CTe ---
  oXml: olevariant;         // CTe
  oInfCTe: olevariant;      // InfCTe
  oIde: olevariant;
  oToma3: olevariant;
  oEmit: olevariant;
  oEnderEmit: olevariant;
  oRem: olevariant;
  oEnderReme: olevariant;
  oDest: olevariant;
  oEnderDest: olevariant;
  oVPrest: olevariant;
  oComp: olevariant;
  oImp: olevariant;
  oICMS: olevariant;
  oICMSSN: olevariant;
  oInfCTeNorm: olevariant;
  oInfCarga: olevariant;
  oInfQ: olevariant;
  oInfDoc: olevariant;
  oInfNFe: olevariant;
  oInfModal: olevariant;
  oRodo: olevariant;
  oOcc: olevariant;
  oEmiOcc: olevariant;
  oInfRespTec: olevariant;
  // --- Fim das Variáveis do XML ---

  oAutorizacaoSinc: olevariant;
  chaveCTe: string;
  stringXMLDistribuicaoCTe: string;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 2; // 2 = CTe
  //oConfiguracao.TipoEmissao := 1; // 1=Normal (Já definido no XML)
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // 1. Criar o objeto XML principal (CTe)
    oXml := CreateOleObject('Unimake.Business.DFe.Xml.CTe.CTe');

    // 2. Criar o objeto 'InfCTe'
    oInfCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCTe');
    oXml.InfCTe := IUnknown(oInfCTe); // *** Importante: Ligar o InfCTe ao CTe ***
    oInfCTe.Versao := '4.00';

    // 3. Criar 'Ide'
    oIde := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Ide');
    oInfCTe.Ide := IUnknown(oIde); // Ligar o Ide ao InfCTe
    oIde.CUF := 41; // UFBrasil.PR
    oIde.CCT := '01234567';
    oIde.CFOP := '6352';
    oIde.NatOp := 'PREST.SERV.TRANSP.INDUSTR';
    oIde.&Mod := 57; // ModeloDFe.CTe
    oIde.Serie := 1;
    oIde.NCT := 868;
    oIde.DhEmi := Now;
    oIde.TpImp := 1; // FormatoImpressaoDACTE.NormalPaisagem
    oIde.TpEmis := 1; // TipoEmissao.Normal
    oIde.TpAmb := 2; // TipoAmbiente.Homologacao
    oIde.TpCTe := 0; // TipoCTe.Normal
    oIde.ProcEmi := 0; // ProcessoEmissao.AplicativoContribuinte
    oIde.VerProc := 'UNICO V8.0';
    oIde.CMunEnv := '4118402';
    oIde.XMunEnv := 'PARANAVAI';
    oIde.UFEnv := 41; // UFBrasil.PR
    oIde.Modal := 1; // ModalidadeTransporteCTe.Rodoviario
    oIde.TpServ := 0; // TipoServicoCTe.Normal
    oIde.CMunIni := '4118402';
    oIde.XMunIni := 'PARANAVAI';
    oIde.UFIni := 41; // UFBrasil.PR
    oIde.CMunFim := '3305109';
    oIde.XMunFim := 'SAO JOAO DE MERITI';
    oIde.UFFim := 33; // UFBrasil.RJ
    oIde.Retira := 0; // SimNao.Nao
    oIde.IndIEToma := 1; // IndicadorIEDestinatario.ContribuinteICMS

    oToma3 := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Toma3');
    oIde.Toma3 := IUnknown(oToma3);
    oToma3.Toma := 0; // TomadorServicoCTe.Remetente

    // 4. Criar 'Emit'
    oEmit := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Emit');
    oInfCTe.Emit := IUnknown(oEmit);
    oEmit.CNPJ := '00000000000000';
    oEmit.IE := '9999999999';
    oEmit.XNome := 'XXXXXX XXXXXX XXXXXX';
    oEmit.XFant := 'XXXXXX XXXXXX';
    oEmit.CRT := 3; // CRT.RegimeNormal

    oEnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderEmit');
    oEmit.EnderEmit := IUnknown(oEnderEmit);
    oEnderEmit.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oEnderEmit.Nro := '11111';
    oEnderEmit.XBairro := 'XXXXXXXXXXXXXX';
    oEnderEmit.CMun := 4118402;
    oEnderEmit.XMun := 'PARANAVAI';
    oEnderEmit.CEP := '87700000';
    oEnderEmit.UF := 41; // UFBrasil.PR
    oEnderEmit.Fone := '04433333333';

    // 5. Criar 'Rem' (Remetente)
    oRem := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Rem');
    oInfCTe.Rem := IUnknown(oRem);
    oRem.CNPJ := '00000000000000';
    oRem.IE := '9999999999';
    oRem.XNome := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oRem.XFant := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oRem.Fone := '04433333333';

    oEnderReme := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderReme');
    oRem.EnderReme := IUnknown(oEnderReme);
    oEnderReme.XLgr := 'XXXXXXXXXXXXXXXXXX';
    oEnderReme.Nro := '9999';
    oEnderReme.XBairro := 'XXXXXXXXXXXXXXX';
    oEnderReme.CMun := 4118402;
    oEnderReme.XMun := 'PARANAVAI';
    oEnderReme.CEP := '87700000';
    oEnderReme.UF := 41; // UFBrasil.PR
    oEnderReme.CPais := 1058;
    oEnderReme.XPais := 'BRASIL';

    // 6. Criar 'Dest' (Destinatário)
    oDest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Dest');
    oInfCTe.Dest := IUnknown(oDest);
    oDest.CNPJ := '00000000000000';
    oDest.IE := 'ISENTO';
    oDest.XNome := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

    oEnderDest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderDest');
    oDest.EnderDest := IUnknown(oEnderDest);
    oEnderDest.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXXXXXX';
    oEnderDest.Nro := '55';
    oEnderDest.XBairro := 'CENTRO';
    oEnderDest.CMun := 3305109;
    oEnderDest.XMun := 'SAO JOAO DE MERITI';
    oEnderDest.CEP := '25520570';
    oEnderDest.UF := 33; // UFBrasil.RJ
    oEnderDest.CPais := 1058;
    oEnderDest.XPais := 'BRASIL';

    // 7. Criar 'VPrest'
    oVPrest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.VPrest');
    oInfCTe.VPrest := IUnknown(oVPrest);
    oVPrest.VTPrest := 50.00;
    oVPrest.VRec := 50.00;

    oComp := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Comp');
    oComp.XNome := 'FRETE VALOR';
    oComp.VComp := 50.00;
    oVPrest.AddComp(IUnknown(oComp)); // Adicionar à lista 'Comp'

    // 8. Criar 'Imp' (Imposto)
    oImp := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Imp');
    oInfCTe.Imp := IUnknown(oImp);

    oICMS := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ICMS');
    oImp.ICMS := IUnknown(oICMS);

    oICMSSN := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ICMSSN');
    oICMS.ICMSSN := IUnknown(oICMSSN);
    oICMSSN.CST := '90';
    oICMSSN.IndSN := 1; // SimNao.Sim

    // 9. Criar 'InfCTeNorm'
    oInfCTeNorm := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCTeNorm');
    oInfCTe.InfCTeNorm := IUnknown(oInfCTeNorm);

    // 9a. 'InfCarga'
    oInfCarga := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCarga');
    oInfCTeNorm.InfCarga := IUnknown(oInfCarga);
    oInfCarga.VCarga := 6252.96;
    oInfCarga.ProPred := 'xxxxxxx';

    // InfQ 1
    oInfQ := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfQ');
    oInfQ.CUnid := 01; // CodigoUnidadeMedidaCTe.KG
    oInfQ.TpMed := 'PESO BRUTO';
    oInfQ.QCarga := 320.0000;
    oInfCarga.AddInfQ(IUnknown(oInfQ));

    // InfQ 2
    oInfQ := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfQ');
    oInfQ.CUnid := 00; // CodigoUnidadeMedidaCTe.UNIDADE
    oInfQ.TpMed := 'UNIDADE';
    oInfQ.QCarga := 1.0000;
    oInfCarga.AddInfQ(IUnknown(oInfQ));

    // 9b. 'InfDoc' (Documentos)
    oInfDoc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfDoc');
    oInfCTeNorm.InfDoc := IUnknown(oInfDoc);

    oInfNFe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfNFe');
    oInfNFe.Chave := '41444444444444444444444444444444444444444441';
    oInfDoc.AddInfNFe(IUnknown(oInfNFe)); // Adicionar NFe à lista InfNFe

    // 9c. 'InfModal'
    oInfModal := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfModal');
    oInfCTeNorm.InfModal := IUnknown(oInfModal);
    oInfModal.VersaoModal := '4.00';

    oRodo := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Rodo');
    oInfModal.Rodo := IUnknown(oRodo);
    oRodo.RNTRC := '44444444';

    oOcc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Occ');
    oOcc.NOcc := 810;
    oOcc.DEmi := Now;

    oEmiOcc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EmiOcc');
    oOcc.EmiOcc := IUnknown(oEmiOcc);
    oEmiOcc.CNPJ := '00000000000000';
    oEmiOcc.CInt := '0000000000';
    oEmiOcc.IE := '9999999999';
    oEmiOcc.UF := 41; // UFBrasil.PR
    oEmiOcc.Fone := '04433333333';

    oRodo.AddOcc(IUnknown(oOcc)); // Adicionar Occ à lista

    // 10. Criar 'InfRespTec'
    oInfRespTec := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfRespTec');
    oInfCTe.InfRespTec := IUnknown(oInfRespTec);
    oInfRespTec.CNPJ := '00000000000000';
    oInfRespTec.XContato := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oInfRespTec.Email := 'teste@gmail.com';
    oInfRespTec.Fone := '04433333333';

    // --- FIM DA MONTAGEM DO XML ---

    // Recuperar a chave do CTe para usar depois
    chaveCTe := VarToStr(oInfCTe.Chave);
    ShowMessage('Chave CTe: ' + chaveCTe);

    // 11. Consumir o serviço
    oAutorizacaoSinc := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.AutorizacaoSinc');
    oAutorizacaoSinc.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

    // 12. Tratar o Retorno
    ShowMessage('CStat: ' + VarToStr(oAutorizacaoSinc.Result.CStat) +
                ' - XMotivo: ' + oAutorizacaoSinc.Result.XMotivo);

    if (oAutorizacaoSinc.Result.CStat = 104) or //Lote Recebido com Sucesso
       (oAutorizacaoSinc.Result.CStat = 100) then //CTe Autorizado (caso raro em sinc)
    begin
      if oAutorizacaoSinc.Result.ProtCTe.InfProt.CStat = 100 then //CTe Autorizado
      begin
        ShowMessage('CTe Autorizado com Sucesso!');

        oAutorizacaoSinc.GravarXmlDistribuicao('d:\testenfe');

        // Para pegar o XML de distribuição (CTeProc)
        // Usamos o método GetCteProcResults(chave) que é similar ao do template da NFe
        stringXMLDistribuicaoCTe := VarToStr(oAutorizacaoSinc.GetCteProcResults(chaveCTe));

        ShowMessage(stringXMLDistribuicaoCTe);
      end
      else
      begin
         // Rejeitado
         ShowMessage('CTe Rejeitado: ' +
                      VarToStr(oAutorizacaoSinc.Result.ProtCTe.InfProt.CStat) + ' - ' +
                      oAutorizacaoSinc.Result.ProtCTe.InfProt.XMotivo);
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
