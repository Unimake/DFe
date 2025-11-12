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
  oCTe: olevariant;         // CTe
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
  caminhoArquivo: string;
  cteAssinado: string;
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
    oCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.CTe');

    // 2. Criar o objeto 'InfCTe'
    oCTe.InfCTe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCTe');
    oCTe.InfCTe.Versao := '4.00';

    // 3. Criar 'Ide'
    oCTe.InfCTe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Ide');
    oCTe.InfCTe.Ide.CUF := 41; // UFBrasil.PR
    oCTe.InfCTe.Ide.CCT := '01234567';
    oCTe.InfCTe.Ide.CFOP := '6352';
    oCTe.InfCTe.Ide.NatOp := 'PREST.SERV.TRANSP.INDUSTR';
    oCTe.InfCTe.Ide.&Mod := 57; // ModeloDFe.CTe
    oCTe.InfCTe.Ide.Serie := 1;
    oCTe.InfCTe.Ide.NCT := 868;
    oCTe.InfCTe.Ide.DhEmi := Now;
    oCTe.InfCTe.Ide.TpImp := 1; // FormatoImpressaoDACTE.NormalPaisagem
    oCTe.InfCTe.Ide.TpEmis := 1; // TipoEmissao.Normal
    oCTe.InfCTe.Ide.TpAmb := 2; // TipoAmbiente.Homologacao
    oCTe.InfCTe.Ide.TpCTe := 0; // TipoCTe.Normal
    oCTe.InfCTe.Ide.ProcEmi := 0; // ProcessoEmissao.AplicativoContribuinte
    oCTe.InfCTe.Ide.VerProc := 'UNICO V8.0';
    oCTe.InfCTe.Ide.CMunEnv := '4118402';
    oCTe.InfCTe.Ide.XMunEnv := 'PARANAVAI';
    oCTe.InfCTe.Ide.UFEnv := 41; // UFBrasil.PR
    oCTe.InfCTe.Ide.Modal := 1; // ModalidadeTransporteCTe.Rodoviario
    oCTe.InfCTe.Ide.TpServ := 0; // TipoServicoCTe.Normal
    oCTe.InfCTe.Ide.CMunIni := '4118402';
    oCTe.InfCTe.Ide.XMunIni := 'PARANAVAI';
    oCTe.InfCTe.Ide.UFIni := 41; // UFBrasil.PR
    oCTe.InfCTe.Ide.CMunFim := '3305109';
    oCTe.InfCTe.Ide.XMunFim := 'SAO JOAO DE MERITI';
    oCTe.InfCTe.Ide.UFFim := 33; // UFBrasil.RJ
    oCTe.InfCTe.Ide.Retira := 0; // SimNao.Nao
    oCTe.InfCTe.Ide.IndIEToma := 1; // IndicadorIEDestinatario.ContribuinteICMS

    oCTe.InfCTe.Ide.Toma3 := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Toma3');
    oCTe.InfCTe.Ide.Toma3.Toma := 0; // TomadorServicoCTe.Remetente

    // 4. Criar 'Emit'
    oCTe.InfCTe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Emit');
    oCTe.InfCTe.Emit.CNPJ := '00000000000000';
    oCTe.InfCTe.Emit.IE := '9999999999';
    oCTe.InfCTe.Emit.XNome := 'XXXXXX XXXXXX XXXXXX';
    oCTe.InfCTe.Emit.XFant := 'XXXXXX XXXXXX';
    oCTe.InfCTe.Emit.CRT := 3; // CRT.RegimeNormal

    oCTe.InfCTe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderEmit');
    oCTe.InfCTe.Emit.EnderEmit.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oCTe.InfCTe.Emit.EnderEmit.Nro := '11111';
    oCTe.InfCTe.Emit.EnderEmit.XBairro := 'XXXXXXXXXXXXXX';
    oCTe.InfCTe.Emit.EnderEmit.CMun := 4118402;
    oCTe.InfCTe.Emit.EnderEmit.XMun := 'PARANAVAI';
    oCTe.InfCTe.Emit.EnderEmit.CEP := '87700000';
    oCTe.InfCTe.Emit.EnderEmit.UF := 41; // UFBrasil.PR
    oCTe.InfCTe.Emit.EnderEmit.Fone := '04433333333';

    // 5. Criar 'Rem' (Remetente)
    oCTe.InfCTe.Rem := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Rem');
    oCTe.InfCTe.Rem.CNPJ := '00000000000000';
    oCTe.InfCTe.Rem.IE := '9999999999';
    oCTe.InfCTe.Rem.XNome := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oCTe.InfCTe.Rem.XFant := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oCTe.InfCTe.Rem.Fone := '04433333333';

    oCTe.InfCTe.Rem.EnderReme := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderReme');
    oCTe.InfCTe.Rem.EnderReme.XLgr := 'XXXXXXXXXXXXXXXXXX';
    oCTe.InfCTe.Rem.EnderReme.Nro := '9999';
    oCTe.InfCTe.Rem.EnderReme.XBairro := 'XXXXXXXXXXXXXXX';
    oCTe.InfCTe.Rem.EnderReme.CMun := 4118402;
    oCTe.InfCTe.Rem.EnderReme.XMun := 'PARANAVAI';
    oCTe.InfCTe.Rem.EnderReme.CEP := '87700000';
    oCTe.InfCTe.Rem.EnderReme.UF := 41; // UFBrasil.PR
    oCTe.InfCTe.Rem.EnderReme.CPais := 1058;
    oCTe.InfCTe.Rem.EnderReme.XPais := 'BRASIL';

    // 6. Criar 'Dest' (Destinatário)
    oCTe.InfCTe.Dest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Dest');
    oCTe.InfCTe.Dest.CNPJ := '00000000000000';
    oCTe.InfCTe.Dest.IE := 'ISENTO';
    oCTe.InfCTe.Dest.XNome := 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';

    oCTe.InfCTe.Dest.EnderDest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EnderDest');
    oCTe.InfCTe.Dest.EnderDest.XLgr := 'XXXXXXXXXXXXXXXXXXXXXXXXXXX';
    oCTe.InfCTe.Dest.EnderDest.Nro := '55';
    oCTe.InfCTe.Dest.EnderDest.XBairro := 'CENTRO';
    oCTe.InfCTe.Dest.EnderDest.CMun := 3305109;
    oCTe.InfCTe.Dest.EnderDest.XMun := 'SAO JOAO DE MERITI';
    oCTe.InfCTe.Dest.EnderDest.CEP := '25520570';
    oCTe.InfCTe.Dest.EnderDest.UF := 33; // UFBrasil.RJ
    oCTe.InfCTe.Dest.EnderDest.CPais := 1058;
    oCTe.InfCTe.Dest.EnderDest.XPais := 'BRASIL';

    // 7. Criar 'VPrest'
    oCTe.InfCTe.VPrest := CreateOleObject('Unimake.Business.DFe.Xml.CTe.VPrest');
    oCTe.InfCTe.VPrest.VTPrest := 50.00;
    oCTe.InfCTe.VPrest.VRec := 50.00;

    oComp := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Comp');
    oComp.XNome := 'FRETE VALOR';
    oComp.VComp := 50.00;
    oCTe.InfCTe.VPrest.AddComp(IUnknown(oComp)); // Adicionar à lista 'Comp'

    // 8. Criar 'Imp' (Imposto)
    oCTe.InfCTe.Imp := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Imp');

    oCTe.InfCTe.Imp.ICMS := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ICMS');

    oCTe.InfCTe.Imp.ICMS.ICMSSN := CreateOleObject('Unimake.Business.DFe.Xml.CTe.ICMSSN');
    oCTe.InfCTe.Imp.ICMS.ICMSSN.CST := '90';
    oCTe.InfCTe.Imp.ICMS.ICMSSN.IndSN := 1; // SimNao.Sim

    // 9. Criar 'InfCTeNorm'
    oCTe.InfCTe.InfCTeNorm := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCTeNorm');

    // 9a. 'InfCarga'
    oCTe.InfCTe.InfCTeNorm.InfCarga := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfCarga');
    oCTe.InfCTe.InfCTeNorm.InfCarga.VCarga := 6252.96;
    oCTe.InfCTe.InfCTeNorm.InfCarga.ProPred := 'xxxxxxx';

    // InfQ 1
    oInfQ := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfQ');
    oInfQ.CUnid := 01; // CodigoUnidadeMedidaCTe.KG
    oInfQ.TpMed := 'PESO BRUTO';
    oInfQ.QCarga := 320.0000;
    oCTe.InfCTe.InfCTeNorm.InfCarga.AddInfQ(IUnknown(oInfQ));

    // InfQ 2
    oInfQ := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfQ');
    oInfQ.CUnid := 00; // CodigoUnidadeMedidaCTe.UNIDADE
    oInfQ.TpMed := 'UNIDADE';
    oInfQ.QCarga := 1.0000;
    oCTe.InfCTe.InfCTeNorm.InfCarga.AddInfQ(IUnknown(oInfQ));

    // 9b. 'InfDoc' (Documentos)
    oCTe.InfCTe.InfCTeNorm.InfDoc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfDoc');

    oInfNFe := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfNFe');
    oInfNFe.Chave := '41444444444444444444444444444444444444444441';
    oCTe.InfCTe.InfCTeNorm.InfDoc.AddInfNFe(IUnknown(oInfNFe)); // Adicionar NFe à lista InfNFe

    // 9c. 'InfModal'
    oCTe.InfCTe.InfCTeNorm.InfModal := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfModal');
    oCTe.InfCTe.InfCTeNorm.InfModal.VersaoModal := '4.00';

    oCTe.InfCTe.InfCTeNorm.InfModal.Rodo := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Rodo');
    oCTe.InfCTe.InfCTeNorm.InfModal.Rodo.RNTRC := '44444444';

    oOcc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.Occ');
    oOcc.NOcc := 810;
    oOcc.DEmi := Now;

    oOcc.EmiOcc := CreateOleObject('Unimake.Business.DFe.Xml.CTe.EmiOcc');
    oOcc.EmiOcc.CNPJ := '00000000000000';
    oOcc.EmiOcc.CInt := '0000000000';
    oOcc.EmiOcc.IE := '9999999999';
    oOcc.EmiOcc.UF := 41; // UFBrasil.PR
    oOcc.EmiOcc.Fone := '04433333333';

    oCTe.InfCTe.InfCTeNorm.InfModal.Rodo.AddOcc(IUnknown(oOcc)); // Adicionar Occ à lista

    // 10. Criar 'InfRespTec'
    oCTe.InfCTe.InfRespTec := CreateOleObject('Unimake.Business.DFe.Xml.CTe.InfRespTec');
    oCTe.InfCTe.InfRespTec.CNPJ := '00000000000000';
    oCTe.InfCTe.InfRespTec.XContato := 'XXXXXXXXXXXXXXXXXXXXXXX';
    oCTe.InfCTe.InfRespTec.Email := 'teste@gmail.com';
    oCTe.InfCTe.InfRespTec.Fone := '04433333333';

    // --- FIM DA MONTAGEM DO XML ---

    // Recuperar a chave do CTe para usar depois
    chaveCTe := VarToStr(oCTe.InfCTe.Chave);
    ShowMessage('Chave CTe: ' + chaveCTe);

    // 11. Consumir o serviço
    oAutorizacaoSinc := CreateOleObject('Unimake.Business.DFe.Servicos.CTe.AutorizacaoSinc');
    oAutorizacaoSinc.SetXMLConfiguracao(IUnknown(oCTe), IUnknown(oConfiguracao));

    //Recuperar o conteúdo do XML assinado
    cteAssinado := VarToStr(oAutorizacaoSinc.GetConteudoCTeAssinado(0));

    //Exibir o XML assinado
    ShowMessage(cteAssinado);

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oCTe.InfCTe.Chave + '-cte.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(cteAssinado)^, Length(cteAssinado));
    finally
      Free;
    end;

    oAutorizacaoSinc.Executar(IUnknown(oCTe), IUnknown(oConfiguracao));

    // 12. Tratar o Retorno
    ShowMessage('CStat: ' + VarToStr(oAutorizacaoSinc.Result.CStat) + ' - XMotivo: ' + oAutorizacaoSinc.Result.XMotivo);

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
        ShowMessage('CTe Rejeitado: ' + VarToStr(oAutorizacaoSinc.Result.ProtCTe.InfProt.CStat) + ' - ' + oAutorizacaoSinc.Result.ProtCTe.InfProt.XMotivo);
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
