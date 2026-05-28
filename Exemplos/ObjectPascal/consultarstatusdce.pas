// ------------------------------------------------------------------
// Enviar DCe
// ------------------------------------------------------------------

unit EnviarDCe;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarDCe = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarDCe.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  oDCe: olevariant;
  oDet: olevariant;

  oAutorizacaoSinc: olevariant;
  oAutXML: olevariant;

  notaAssinada: string;
  caminhoArquivo: string;

  xmlRetornado: string;
  statusRetorno: string;
  docProcDCe: string;
  numeroProtocolo: string;

  i: integer;
begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 16; //DCe
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar a tag <DCe>
    oDCe := CreateOleObject('Unimake.Business.DFe.Xml.DCe.DCe');

    // Criar a tag <infDCe>
    oDCe.InfDCe := CreateOleObject('Unimake.Business.DFe.Xml.DCe.InfDCe');
    oDCe.InfDCe.Versao := '1.00';

    // Criar tag Ide
    oDCe.InfDCe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Ide');
    oDCe.InfDCe.Ide.CUF := 41; // UFBrasil.PR
    oDCe.InfDCe.Ide.CDC := '123456';
    oDCe.InfDCe.Ide.&Mod := 99; // ModeloDFe.DCe
    oDCe.InfDCe.Ide.Serie := 0;
    oDCe.InfDCe.Ide.NDC := 1;
    oDCe.InfDCe.Ide.DhEmi := Now;
    oDCe.InfDCe.Ide.TpEmis := 1; // TipoEmissao.Normal
    oDCe.InfDCe.Ide.TpEmit := 2; // TipoEmitenteDCe.EmissorProprio
    oDCe.InfDCe.Ide.NSiteAutoriz := string('0');
    oDCe.InfDCe.Ide.CDV := 6;
    oDCe.InfDCe.Ide.TpAmb := 2; // TipoAmbiente.Homologacao
    oDCe.InfDCe.Ide.VerProc := 'Unimake-Test';

    // Criar tag Emit
    oDCe.InfDCe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Emit');
    oDCe.InfDCe.Emit.CNPJ := '00000000000199';
    oDCe.InfDCe.Emit.XNome := 'Emitente Teste';

    oDCe.InfDCe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.DCe.EnderEmit');
    oDCe.InfDCe.Emit.EnderEmit.XLgr := 'Rua Teste';
    oDCe.InfDCe.Emit.EnderEmit.Nro := '100';
    oDCe.InfDCe.Emit.EnderEmit.XBairro := 'Centro';
    oDCe.InfDCe.Emit.EnderEmit.CMun := '4106902';
    oDCe.InfDCe.Emit.EnderEmit.XMun := 'Curitiba';
    oDCe.InfDCe.Emit.EnderEmit.UF := 41; // UFBrasil.PR
    oDCe.InfDCe.Emit.EnderEmit.CEP := '80010000';
    oDCe.InfDCe.Emit.EnderEmit.CPais := '1058';
    oDCe.InfDCe.Emit.EnderEmit.XPais := 'Brasil';

    // Criar tag Dest
    oDCe.InfDCe.Dest := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Dest');
    oDCe.InfDCe.Dest.CPF := '12345678909';
    oDCe.InfDCe.Dest.XNome := 'Destinatario Teste';

    oDCe.InfDCe.Dest.EnderDest := CreateOleObject('Unimake.Business.DFe.Xml.DCe.EnderDest');
    oDCe.InfDCe.Dest.EnderDest.XLgr := 'Rua Destino';
    oDCe.InfDCe.Dest.EnderDest.Nro := '200';
    oDCe.InfDCe.Dest.EnderDest.XBairro := 'Centro';
    oDCe.InfDCe.Dest.EnderDest.CMun := '4106902';
    oDCe.InfDCe.Dest.EnderDest.XMun := 'Curitiba';
    oDCe.InfDCe.Dest.EnderDest.UF := 41; // UFBrasil.PR
    oDCe.InfDCe.Dest.EnderDest.CEP := '80010000';
    oDCe.InfDCe.Dest.EnderDest.Email := 'destino@teste.com';

    // Criar tag AutXML
    oAutXML := CreateOleObject('Unimake.Business.DFe.Xml.DCe.AutXML');
    oAutXML.CPF := '12345678909';
    oDCe.InfDCe.AddAutXML(IUnknown(oAutXML));

    // Criar tag Det
    oDet := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Det');
    oDet.NItem := 1;

    oDet.Prod := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Prod');
    oDet.Prod.XProd := 'Produto teste';
    oDet.Prod.NCM := '99';
    oDet.Prod.QCom := 1;
    oDet.Prod.VUnCom := 10;
    oDet.Prod.VProd := 10;

    oDet.InfAdProd := 'Item preservado';

    oDCe.InfDCe.AddDet(IUnknown(oDet));

    // Criar tag Total
    oDCe.InfDCe.Total := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Total');
    oDCe.InfDCe.Total.VDC := 10;

    // Criar tag Transp
    oDCe.InfDCe.Transp := CreateOleObject('Unimake.Business.DFe.Xml.DCe.Transp');
    oDCe.InfDCe.Transp.ModTrans := 1;
    oDCe.InfDCe.Transp.CNPJTransp := '00000000000199';

    // Criar tag InfAdic
    oDCe.InfDCe.InfAdic := CreateOleObject('Unimake.Business.DFe.Xml.DCe.InfAdic');
    oDCe.InfDCe.InfAdic.InfCpl := 'Informacao complementar';

    // Criar tag InfDec
    oDCe.InfDCe.InfDec := CreateOleObject('Unimake.Business.DFe.Xml.DCe.InfDec');
    oDCe.InfDCe.InfDec.XObs1 := 'Declaracao 1';
    oDCe.InfDCe.InfDec.XObs2 := 'Declaracao 2';

    ShowMessage(oDCe.InfDCe.Id);
    ShowMessage(oDCe.InfDCe.Chave);

    //Consumir o serviço
    oAutorizacaoSinc := CreateOleObject('Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc');
    oAutorizacaoSinc.SetXMLConfiguracao(IUnknown(oDCe), IUnknown(oConfiguracao));

    //Recuperar o conteúdo do XML assinado
    notaAssinada := VarToStr(oAutorizacaoSinc.GetConteudoXMLAssinado());

    //Exibir o XML assinado
    ShowMessage(notaAssinada);

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + oDCe.InfDCe.Chave + '-dce.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
    try
      WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
    finally
      Free;
    end;

    oAutorizacaoSinc.Executar(IUnknown(oDCe), IUnknown(oConfiguracao));

    // XML Retornado pela SEFAZ
    xmlRetornado := VarToStr(oAutorizacaoSinc.RetornoWSString);
    ShowMessage(xmlRetornado);

    // Código de Status e Motivo
    statusRetorno := Trim(IntToStr(oAutorizacaoSinc.Result.CStat)) + ' ' + VarToStr(oAutorizacaoSinc.Result.XMotivo);
    ShowMessage(statusRetorno);

    // Verifica se o DCe foi autorizado
    if oAutorizacaoSinc.Result.CStat = 100 then
    begin
      if oAutorizacaoSinc.Result.ProtDCe.InfProt.CStat = 100 then
      begin
        // Gravar XML de distribuição em uma pasta (DCe com o protocolo anexado)
        oAutorizacaoSinc.GravarXmlDistribuicao('d:\testenfe');

        // Pegar a string do XML de distribuição
        docProcDCe := VarToStr(oAutorizacaoSinc.GetDCeProcResults(oDCe.InfDCe.Chave));
        ShowMessage(docProcDCe);

        // Pegar o número do protocolo de autorização
        numeroProtocolo := VarToStr(oAutorizacaoSinc.Result.ProtDCe.InfProt.NProt);
        ShowMessage(numeroProtocolo);
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

