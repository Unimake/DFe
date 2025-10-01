// ------------------------------------------------------------------
// Enviar MDFe no modo síncrono
// ------------------------------------------------------------------

unit EnviarMDFeSincrono;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarMDFeSincrono = class
  public
    procedure Executar;
  end;

implementation

procedure TEnviarMDFeSincrono.Executar;
var
  oConfiguracao: OleVariant;
  oExceptionInterop: OleVariant;

  oMDFe: OleVariant;
  oInfMunCarrega: OleVariant;
  oAutorizacaoSinc: OleVariant;
  oInfContratante: OleVariant;
  oCondutor: OleVariant;
  oInfMunDescarga: OleVariant;
  oInfCTe: OleVariant;
  oInfNFe: OleVariant;
  oInfUnidTransp: OleVariant;
  oLacUnidTransp: OleVariant;
  oInfUnidCarga: OleVariant;
  oLacUnidCarga: OleVariant;
  oLacre: OleVariant;
  oSeg: OleVariant;

  notaAssinada, caminhoArquivo: string;
  xmlRetornado, statusRetorno, docProcMDFe, numeroProtocolo: string;
begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 4; // 4 = MDFe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  // Criar objeto para capturar exceções do lado do C#
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar a tag <MDFe>
    oMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.MDFe');

    // Criar a tag <infMDFe>
    oMDFe.InfMDFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfMDFe');
    oMDFe.InfMDFe.Versao := '3.00';

    // Criar a tag <ide>
    oMDFe.InfMDFe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Ide');
    oMDFe.InfMDFe.Ide.CUF := 41; // PR
    oMDFe.InfMDFe.Ide.TpAmb := 2; // Homologação
    oMDFe.InfMDFe.Ide.TpEmit := 1; // Prestador de serviço
    oMDFe.InfMDFe.Ide.&Mod := 58; // Modelo MDFe
    oMDFe.InfMDFe.Ide.Serie := 1;
    oMDFe.InfMDFe.Ide.NMDF := 861;
    oMDFe.InfMDFe.Ide.CMDF := '01722067';
    oMDFe.InfMDFe.Ide.Modal := 1; // Rodoviário
    oMDFe.InfMDFe.Ide.DhEmi := Now;
    oMDFe.InfMDFe.Ide.TpEmis := 1; // Normal
    oMDFe.InfMDFe.Ide.ProcEmi := 0; // App contribuinte
    oMDFe.InfMDFe.Ide.VerProc := 'UNICO V8.0';
    oMDFe.InfMDFe.Ide.TpTransp := 1; // ETC
    oMDFe.InfMDFe.Ide.UFIni := 41; // PR
    oMDFe.InfMDFe.Ide.UFFim := 35; // SP
    oMDFe.InfMDFe.Ide.DhIniViagem := Now;

    // Criar a tag <infMunCarrega>
    oInfMunCarrega := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfMunCarrega');
    oInfMunCarrega.CMunCarrega := 4118402;
    oInfMunCarrega.XMunCarrega := 'PARANAVAI';
    oMDFe.InfMDFe.Ide.AddInfMunCarrega(IUnknown(oInfMunCarrega));

    // Criar a tag <emit>
    oMDFe.InfMDFe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Emit');
    oMDFe.InfMDFe.Emit.CNPJ := '06117473000150';
    oMDFe.InfMDFe.Emit.IE := '9456656656';
    oMDFe.InfMDFe.Emit.XNome := 'XXXXXX XXXXXX XXXXXX';
    oMDFe.InfMDFe.Emit.XFant := 'XXXXXX XXXXXX';

    // Criar a tag <enderEmit>
    oMDFe.InfMDFe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.EnderEmit');
    oMDFe.InfMDFe.Emit.EnderEmit.XLgr := 'RUA XXXXXXX';
    oMDFe.InfMDFe.Emit.EnderEmit.Nro := '01112';
    oMDFe.InfMDFe.Emit.EnderEmit.XBairro := 'VILA TESTE';
    oMDFe.InfMDFe.Emit.EnderEmit.CMun := 4118402;
    oMDFe.InfMDFe.Emit.EnderEmit.XMun := 'PARANAVAI';
    oMDFe.InfMDFe.Emit.EnderEmit.CEP := '87706000';
    oMDFe.InfMDFe.Emit.EnderEmit.UF := 41;
    oMDFe.InfMDFe.Emit.EnderEmit.Fone := '04433333333';

    // Criar a tag <infModal>
    oMDFe.InfMDFe.InfModal := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfModal');
    oMDFe.InfMDFe.InfModal.VersaoModal := '3.00';

    // Criar a tag <rodo>
    oMDFe.InfMDFe.InfModal.Rodo := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Rodo');
    oMDFe.InfMDFe.InfModal.Rodo.InfANTT := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfANTT');
    oMDFe.InfMDFe.InfModal.Rodo.InfANTT.RNTRC := '44556666';

    // Criar contratante
    oInfContratante := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfContratante');
    oInfContratante.CNPJ := '06117473000150';
    oMDFe.InfMDFe.InfModal.Rodo.InfANTT.AddInfContratante(IUnknown(oInfContratante));

    // Criar veículo de tração
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.VeicTracao');
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.CInt := 'AXF0000';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Placa := 'AXF0000';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.CapKG := 5000;
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.TpRod := 2;
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.TpCar := 2;
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.UF := 41;

    // Criar proprietário do veículo
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Prop');
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.CNPJ := '06117443000150';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.RNTRC := '44556666';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.XNome := 'XXXXXX XXXXXX';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.IE := '5545546656';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.UF := 41;
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.Prop.TpProp := 2;

    // Criar condutor
    oCondutor := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Condutor');
    oCondutor.XNome := 'XXXXXXXXX XXXXX';
    oCondutor.CPF := '02133333333';
    oMDFe.InfMDFe.InfModal.Rodo.VeicTracao.AddCondutor(IUnknown(oCondutor));

    // Criar infDoc -> infMunDescarga -> infCTe/NFe
    oMDFe.InfMDFe.InfDoc := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfDocInfMDFe');

    oInfMunDescarga := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfMunDescarga');
    oInfMunDescarga.CMunDescarga := 3505708;
    oInfMunDescarga.XMunDescarga := 'BARUERI';

    oInfCTe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe');
    oInfCTe.ChCTe := '41000000000000000000000000000000000000000006';
    oInfMunDescarga.AddInfCTe(IUnknown(oInfCTe));

    oInfNFe := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfNFe');
    oInfNFe.ChNFe := '12345678901234567890123456789012345678901234';

    // UnidTransp
    oInfUnidTransp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfUnidTransp');
    oInfUnidTransp.IdUnidTransp := '122';
    oInfUnidTransp.TpUnidTransp := 2;

    oLacUnidTransp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.LacUnidTransp');
    oLacUnidTransp.NLacre := '12334';
    oInfUnidTransp.AddLacUnidTransp(IUnknown(oLacUnidTransp));

    // UnidCarga
    oInfUnidCarga := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfUnidCarga');
    oInfUnidCarga.TpUnidCarga := 1;
    oInfUnidCarga.IdUnidCarga := '123';

    oLacUnidCarga := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.LacUnidCarga');
    oLacUnidCarga.NLacre := '3333333';
    oInfUnidCarga.AddLacUnidCarga(IUnknown(oLacUnidCarga));

    oInfUnidTransp.AddInfUnidCarga(IUnknown(oInfUnidCarga));
    oInfNFe.AddInfUnidTransp(IUnknown(oInfUnidTransp));
    oInfMunDescarga.AddInfNFe(IUnknown(oInfNFe));

    oMDFe.InfMDFe.InfDoc.AddInfMunDescarga(IUnknown(oInfMunDescarga));

    // Seguros
    oSeg := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Seg');
    oSeg.NApol := '033666565656';

    oSeg.InfResp := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfResp');
    oSeg.InfResp.RespSeg := 1;
    oSeg.InfResp.CNPJ := '06117473000150';

    oSeg.InfSeg := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfSeg');
    oSeg.InfSeg.XSeg := 'PORTO SEGURO';
    oSeg.InfSeg.CNPJ := '06117473000150';

    oSeg.AddNAver('0000000000000000000000000000000000000000');
    oSeg.AddNAver('0000000000000000000000000000000000000000');

    oMDFe.InfMDFe.AddSeg(IUnknown(oSeg));

    // Produto predominante + lotação
    oMDFe.InfMDFe.ProdPred := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.ProdPred');
    oMDFe.InfMDFe.ProdPred.TpCarga := 5;
    oMDFe.InfMDFe.ProdPred.XProd := 'TESTE DE PRODUTO PREDOMINANTE';

    oMDFe.InfMDFe.ProdPred.InfLotacao := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfLotacao');
    oMDFe.InfMDFe.ProdPred.InfLotacao.InfLocalCarrega := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfLocalCarrega');
    oMDFe.InfMDFe.ProdPred.InfLotacao.InfLocalCarrega.CEP := '87302080';

    oMDFe.InfMDFe.ProdPred.InfLotacao.InfLocalDescarrega := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfLocalDescarrega');
    oMDFe.InfMDFe.ProdPred.InfLotacao.InfLocalDescarrega.CEP := '25650208';

    // Criar totais
    oMDFe.InfMDFe.Tot := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Tot');
    oMDFe.InfMDFe.Tot.QCTe := 3;
    oMDFe.InfMDFe.Tot.VCarga := 56599.09;
    oMDFe.InfMDFe.Tot.CUnid := 1;
    oMDFe.InfMDFe.Tot.QCarga := 2879.00;

    // Lacres
    oLacre := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Lacre');
    oLacre.NLacre := '1111111';
    oMDFe.InfMDFe.AddLacres(IUnknown(oLacre));

    oLacre := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.Lacre');
    oLacre.NLacre := '2222222';
    oMDFe.InfMDFe.AddLacres(IUnknown(oLacre));

    // Criar infAdic
    oMDFe.InfMDFe.InfAdic := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfAdic');
    oMDFe.InfMDFe.InfAdic.InfCpl := 'DATA/HORA PREVISTA PARA O INICIO DA VIAGEM: ' + DateTimeToStr(Now);

    // Criar respTec
    oMDFe.InfMDFe.InfRespTec := CreateOleObject('Unimake.Business.DFe.Xml.MDFe.InfRespTec');
    oMDFe.InfMDFe.InfRespTec.CNPJ := '99999999999999';
    oMDFe.InfMDFe.InfRespTec.XContato := 'Teste RespTec';
    oMDFe.InfMDFe.InfRespTec.Email := 'teste@teste.com';
    oMDFe.InfMDFe.InfRespTec.Fone := '4499999999';

    // Consumir serviço
    oAutorizacaoSinc := CreateOleObject('Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc');
    oAutorizacaoSinc.SetXMLConfiguracao(IUnknown(oMDFe), IUnknown(oConfiguracao));

    // XML assinado
    notaAssinada := VarToStr(oAutorizacaoSinc.GetConteudoMDFeAssinado());
    ShowMessage(notaAssinada);

    caminhoArquivo := 'd:\testenfe\' + oMDFe.InfMDFe.Chave + '-mdfe.xml';
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
    finally
      Free;
    end;

    // Enviar
    oAutorizacaoSinc.Executar(IUnknown(oMDFe), IUnknown(oConfiguracao));

    xmlRetornado := VarToStr(oAutorizacaoSinc.RetornoWSString);
    ShowMessage(xmlRetornado);

    statusRetorno := IntToStr(oAutorizacaoSinc.Result.CStat) + ' - ' + VarToStr(oAutorizacaoSinc.Result.XMotivo);
    ShowMessage(statusRetorno);

    if oAutorizacaoSinc.Result.CStat = 104 then
    begin
      if oAutorizacaoSinc.Result.ProtMDFe.InfProt.CStat = 100 then
      begin
        numeroProtocolo := VarToStr(oAutorizacaoSinc.Result.ProtMDFe.InfProt.NProt);
        ShowMessage('Protocolo: ' + numeroProtocolo);
        oAutorizacaoSinc.GravarXmlDistribuicao('d:\testenfe');
        docProcMDFe := VarToStr(oAutorizacaoSinc.GetMDFeProcResults(oMDFe.InfMDFe.Chave));
        ShowMessage(docProcMDFe);
      end;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erro Pascal: ' + E.Message);
      ShowMessage(oExceptionInterop.GetMessage());
      ShowMessage(IntToStr(oExceptionInterop.GetErrorCode()));
    end;
  end;
end;

end.
