// ------------------------------------------------------------------
// Enviar CTeOS (CT-e Outros Serviços)
// ------------------------------------------------------------------

unit EnviarCteOsSincrono; // Nome da unit mantido do template

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarCteOsSincrono = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarCteOsSincrono.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  // --- Variáveis do XML CTeOS ---
  oXml: olevariant;         // CTeOS
  oInfCTe: olevariant;      // InfCTe
  oIde: olevariant;
  oInfPercurso: olevariant;
  oCompl: olevariant;
  oObsCont: olevariant;
  oEmit: olevariant;
  oEnderEmit: olevariant;
  oToma: olevariant;
  oEnderToma: olevariant;
  oVPrest: olevariant;
  oComp: olevariant;
  oImp: olevariant;
  oICMS: olevariant;
  oICMS00: olevariant;
  oInfTribFed: olevariant;
  oInfCTeNorm: olevariant;
  oInfServico: olevariant;
  oInfQ: olevariant;
  oSeg: olevariant;
  oInfModal: olevariant;
  oRodoOS: olevariant;
  oAutXML: olevariant;
  oInfRespTec: olevariant;
  // --- Fim das Variáveis do XML ---

  oAutorizacao: olevariant; // Usando 'Autorizacao' como no C#
  chaveCTe: string;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 3; // 3 = CTeOS
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // 1. Criar o objeto XML principal (CTeOS)
    oXml := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.CTeOS');
    oXml.Versao := '4.00';

    // 2. Criar o objeto 'InfCTe'
    oInfCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfCTe');
    oXml.InfCTe := IUnknown(oInfCTe); // Ligar o InfCTe ao CTeOS
    oInfCTe.Versao := '4.00';

    // 3. Criar 'Ide'
    oIde := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Ide');
    oInfCTe.Ide := IUnknown(oIde);
    oIde.CUF := 41; // UFBrasil.PR
    oIde.CCT := '12356488';
    oIde.CFOP := '6352';
    oIde.NatOp := 'PREST.SERV.TRANSP.INDUSTR';
    oIde.&Mod := 67; // ModeloDFe.CTeOS
    oIde.Serie := 1;
    oIde.NCT := 861;
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
    oIde.Modal := 01; // ModalidadeTransporteCTe.Rodoviario
    oIde.TpServ := 6; // TipoServicoCTeOS.TransportePessoas
    oIde.CMunIni := '4118402';
    oIde.XMunIni := 'PARANAVAI';
    oIde.UFIni := 41; // UFBrasil.PR
    oIde.CMunFim := '3305109';
    oIde.XMunFim := 'SAO JOAO DE MERITI';
    oIde.UFFim := 33; // UFBrasil.RJ
    oIde.IndIEToma := 1; // IndicadorIEDestinatario.ContribuinteICMS

    // 3a. Lista 'InfPercurso'
    oInfPercurso := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfPercurso');
    oInfPercurso.UFPer := 35; // UFBrasil.SP
    oIde.AddInfPercurso(IUnknown(oInfPercurso));

    // 4. Criar 'Compl' (Complemento)
    oCompl := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Compl');
    oInfCTe.Compl := IUnknown(oCompl);
    oCompl.XObs := 'Teste de observacao';

    oObsCont := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.ObsCont');
    oObsCont.XCampo := 'LEI DA TRANSPARENCIA';
    oObsCont.XTexto := 'O valor aproximado de tributos incidentes sobre o preco deste servico e de R$ 177.33 .(0) Fonte: IBPT';
    oCompl.AddObsCont(IUnknown(oObsCont));

    // 5. Criar 'Emit'
    oEmit := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Emit');
    oInfCTe.Emit := IUnknown(oEmit);
    oEmit.CNPJ := '00000000000000';
    oEmit.IE := '0000000000';
    oEmit.XNome := 'XXXXXXXXXXXXXXXXXXXX';
    oEmit.XFant := 'XXXXXXXXXXXXX';
    oEmit.CRT := 1; // CRT.SimplesNacional

    oEnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.EnderEmit');
    oEmit.EnderEmit := IUnknown(oEnderEmit);
    oEnderEmit.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oEnderEmit.Nro := '00001';
    oEnderEmit.XBairro := 'XXXXXXXXXXXXXX';
    oEnderEmit.CMun := 4118402;
    oEnderEmit.XMun := 'PARANAVAI';
    oEnderEmit.CEP := '87700000';
    oEnderEmit.UF := 41; // UFBrasil.PR
    oEnderEmit.Fone := '04444444444';

    // 6. Criar 'Toma' (Tomador)
    oToma := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Toma');
    oInfCTe.Toma := IUnknown(oToma);
    oToma.CNPJ := '00000000000000';
    oToma.IE := '0000000000';
    oToma.XNome := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oToma.XFant := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oToma.Fone := '04434225480';

    oEnderToma := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.EnderToma');
    oToma.EnderToma := IUnknown(oEnderToma);
    oEnderToma.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oEnderToma.Nro := '00001';
    oEnderToma.XBairro := 'XXXXXXXXXXXXXX';
    oEnderToma.CMun := 4118402;
    oEnderToma.XMun := 'PARANAVAI';
    oEnderToma.CEP := '87700000';
    oEnderToma.UF := 41; // UFBrasil.PR
    oEnderToma.CPais := 1058;
    oEnderToma.XPais := 'BRASIL';

    // 7. Criar 'VPrest'
    oVPrest := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.VPrest');
    oInfCTe.VPrest := IUnknown(oVPrest);
    oVPrest.VTPrest := 2845.15;
    oVPrest.VRec := 2845.15;

    // Comp 1
    oComp := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Comp');
    oComp.XNome := 'VIAGEM TURISMO';
    oComp.VComp := 2356.00;
    oVPrest.AddComp(IUnknown(oComp));

    // Comp 2
    oComp := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Comp');
    oComp.XNome := 'PEDAGIO';
    oComp.VComp := 311.82;
    oVPrest.AddComp(IUnknown(oComp));

    // 8. Criar 'Imp' (Imposto)
    oImp := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Imp');
    oInfCTe.Imp := IUnknown(oImp);
    oImp.VTotTrib := 177.33;

    oICMS := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.ICMS');
    oImp.ICMS := IUnknown(oICMS);

    oICMS00 := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.ICMS00');
    oICMS.ICMS00 := IUnknown(oICMS00);
    oICMS00.CST := '00';
    oICMS00.VBC := 2533.33;
    oICMS00.PICMS := 7.00;
    oICMS00.VICMS := 177.33;

    oInfTribFed := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfTribFed');
    oImp.InfTribFed := IUnknown(oInfTribFed);
    oInfTribFed.VPIS := olevariant(30.00);
    oInfTribFed.VCOFINS := olevariant(3.00);
    oInfTribFed.VIR := olevariant(3.00);
    oInfTribFed.VINSS := olevariant(3.00);
    oInfTribFed.VCSLL := olevariant(3.00);

    // 9. Criar 'InfCTeNorm'
    oInfCTeNorm := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfCTeNorm');
    oInfCTe.InfCTeNorm := IUnknown(oInfCTeNorm);

    // 9a. 'InfServico'
    oInfServico := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfServico');
    oInfCTeNorm.InfServico := IUnknown(oInfServico);
    oInfServico.XDescServ := 'TRANSPORTES DE PESSOINHAS';

    oInfQ := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfQ');
    oInfServico.InfQ := IUnknown(oInfQ);
    oInfQ.QCarga := 1;

    // 9b. 'Seg' (Seguro)
    oSeg := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.Seg');
    oSeg.RespSeg := 4; // ResponsavelSeguroCTeOS.EmitenteCTeOS
    oInfCTeNorm.AddSeg(IUnknown(oSeg));

    // 9c. 'InfModal'
    oInfModal := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfModal');
    oInfCTeNorm.InfModal := IUnknown(oInfModal);
    oInfModal.VersaoModal := '4.00';

    oRodoOS := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.RodoOS');
    oInfModal.RodoOS := IUnknown(oRodoOS);
    oRodoOS.TAF := '999999999999';

    // 10. 'AutXML' (Autorizados a baixar o XML)
    oAutXML := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.AutXML');
    oAutXML.CNPJ := '99999999999999';
    oInfCTe.AddAutXML(IUnknown(oAutXML));

    oAutXML := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.AutXML');
    oAutXML.CNPJ := '99999999999998';
    oInfCTe.AddAutXML(IUnknown(oAutXML));

    // 11. Criar 'InfRespTec'
    oInfRespTec := CreateOleObject('Unimake.Business.DFe.Xml.CTeOS.InfRespTec');
    oInfCTe.InfRespTec := IUnknown(oInfRespTec);
    oInfRespTec.CNPJ := '00000000000000';
    oInfRespTec.XContato := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oInfRespTec.Email := 'teste@gmail.com';
    oInfRespTec.Fone := '04433333333';

    // --- FIM DA MONTAGEM DO XML ---

    // Recuperar a chave do CTe para usar depois
    chaveCTe := VarToStr(oInfCTe.Chave);
    ShowMessage('Chave CTeOS: ' + chaveCTe);

    // 12. Consumir o serviço
    // Nota: O C# usa 'Autorizacao', que é o serviço de envio assíncrono (lote).
    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.CTeOS.Autorizacao');
    oAutorizacao.Executar(IUnknown(oXml), IUnknown(oConfiguracao));

    // 13. Tratar o Retorno
    ShowMessage('CStat Lote: ' + VarToStr(oAutorizacao.Result.CStat) +
                ' - XMotivo: ' + oAutorizacao.Result.XMotivo);

    if oAutorizacao.Result.CStat = 103 then // 103 = Lote Recebido com sucesso
    begin
      // O C# verifica o ProtCTe, o que é ótimo.
      if (oAutorizacao.Result.ProtCTe <> nil) and
         (oAutorizacao.Result.ProtCTe.InfProt.CStat = 100) then //Autorizado
      begin
        ShowMessage('CTeOS Autorizado com Sucesso!');
        oAutorizacao.GravarXmlDistribuicao('c:\testenfe\');
      end
      else if (oAutorizacao.Result.ProtCTe <> nil) then
      begin
        ShowMessage('CTeOS Rejeitado: ' +
                    VarToStr(oAutorizacao.Result.ProtCTe.InfProt.CStat) + ' - ' +
                    oAutorizacao.Result.ProtCTe.InfProt.XMotivo);
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
