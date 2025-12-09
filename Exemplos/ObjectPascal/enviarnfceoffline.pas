// ------------------------------------------------------------------
// Enviar NFCe no modo síncrono
// ------------------------------------------------------------------

unit EnviarNFCeOffline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarNFCeOffline = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarNFCeOffline.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  oEnviNFe: olevariant;
  oNfe: olevariant;
  oInfNFe: olevariant;
  oDet: olevariant;
  oDetPag: olevariant;

  oConteudoNFe: olevariant;
  oConteudoInfNFe: olevariant;
  chaveNFe: string;

  oAutorizacao: olevariant;

  notaAssinada: string;
  caminhoArquivo: string;

  xmlRetornado: string;
  statusRetorno: string;
  docProcNFe: string;
  numeroProtocolo: string;

  i: integer;
  cStat: integer;

  envioOffline: boolean;
  statusEnvio: integer;
  xJust: string;
  conexaoNet: boolean;
  lSair: boolean;
  oConfigDANFe: olevariant;
  oDANFe: olevariant;
begin
  //0-Inicio de tudo
  //2-Gerado DANFE em contingência offline
  //10-Houve tentativa de envio da NFCe foi enviada para SEFAZ
  //90-NFCe autorizada com sucesso
  statusEnvio := 0;
  envioOffline := false;
  conexaoNet := false;
  lSair := False;
  xJust := 'Emitida em contingencia em decorrencia de falhas da infraestrutura de rede/internet';

  if not conexaoNet then
  begin
    envioOffline := true;
    xJust := 'Emitido em contingencia por falta de internet';
  end;

  while not lSair do
  begin

    // Criar objeto de configuração mínima
    oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
    oConfiguracao.TipoDFe := 1; //0=NFCe ###
    if not envioOffline then
    begin
      oConfiguracao.TipoEmissao := 1; //1=Normal
    end
    else
    begin
      oConfiguracao.TipoEmissao := 9; //1=Contingencia offline
    end;
    oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
    oConfiguracao.CertificadoSenha := '12345678';
    oConfiguracao.CSC := 'HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG';  //###
    oConfiguracao.CSCIDToken := 2;      //###

    //Criar objeto para pegar exceção do lado do CSHARP
    oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

    try
      // Criar a tag <enviNFe>
      oEnviNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnviNFe');
      oEnviNFe.Versao := '4.00';
      oEnviNFe.IdLote := '000000000000001';
      oEnviNFe.IndSinc := 1; // 1=Sim 0=Nao ###

      // Criar a tag <NFe>
      oNfe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.NFe');

      if statusEnvio = 0 then
      begin
        // Criar tag InfNfe
        oInfNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfNFe');
        oInfNFe.Versao := '4.00';

        // Cria tag Ide
        oInfNFe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Ide');
        oInfNFe.Ide.CUF := 41; // Brasil.PR
        oInfNFe.Ide.NatOp := 'VENDA PRODUC.DO ESTABELEC';
        oInfNFe.Ide.&Mod := 65; // NFCe ###
        oInfNFe.Ide.Serie := 59;
        oInfNFe.Ide.NNF := 2;
        oInfNFe.Ide.DhEmi := Now;
        oInfNFe.Ide.DhSaiEnt := Now;
        oInfNFe.Ide.TpNF := 1; // Saida
        oInfNFe.Ide.IdDest := 1; // OperacaoEstadual ###
        oInfNFe.Ide.CMunFG := 4118402;
        oInfNFe.Ide.TpImp := 4; // FormatoImpressaoDANFE.DANFENFCe ###
        if not envioOffline then
        begin
          oInfNFe.Ide.TpEmis := 1; // TipoEmissao.Normal
        end
        else
        begin
          oInfNFe.Ide.TpEmis := 9; // TipoEmissao.ContingenciaOffline
          oInfNFe.Ide.XJust := xJust;
          oInfNFe.Ide.DhCont := Now;
        end;
        oInfNFe.Ide.TpAmb := 2; // TipoAmbiente.Homologacao
        oInfNFe.Ide.FinNFe := 1; // FinalidadeNFe.Normal ###
        oInfNFe.Ide.IndFinal := 1; // SimNao.Sim ###
        oInfNFe.Ide.IndPres := 1; // IndicadorPresenca.OperacaoPresencial
        oInfNFe.Ide.ProcEmi := 0; // ProcessoEmissao.AplicativoContribuinte
        oInfNFe.Ide.VerProc := 'TESTE 1.00';


        // criar tag Emit
        oInfNFe.Emit := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Emit');
        oInfNFe.Emit.CNPJ := '06117473000150';
        oInfNFe.Emit.XNome := 'UNIMAKE SOLUCOES CORPORATIVAS LTDA';
        oInfNFe.Emit.XFant := 'UNIMAKE - PARANAVAI';
        oInfNFe.Emit.IE := '9032000301';
        oInfNFe.Emit.IM := '14018';
        oInfNFe.Emit.CNAE := '6202300';
        oInfNFe.Emit.CRT := 1; // CRT.SimplesNacional

        oInfNFe.Emit.EnderEmit := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnderEmit');
        oInfNFe.Emit.EnderEmit.XLgr := 'RUA PAULO ANTONIO COSTA';
        oInfNFe.Emit.EnderEmit.Nro := '575';
        oInfNFe.Emit.EnderEmit.XBairro := 'CENTRO';
        oInfNFe.Emit.EnderEmit.CMun := 4118402;
        oInfNFe.Emit.EnderEmit.XMun := 'PARANAVAI';
        oInfNFe.Emit.EnderEmit.UF := 41; // UFBrasil.PR
        oInfNFe.Emit.EnderEmit.CEP := '87707210';
        oInfNFe.Emit.EnderEmit.Fone := '04431421010';

        // criar tag Dest
        oInfNFe.Dest := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Dest'); //###
        oInfNFe.Dest.CNPJ := '04218457000128';
        oInfNFe.Dest.XNome := 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
        oInfNFe.Dest.IndIEDest := 9; // IndicadorIEDestinatario.NaoContribuinte

        for i := 1 to 3 do
        begin
          // criar tag Det
          oDet := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Det');
          oDet.NItem := i;

          oDet.Prod := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Prod');
          oDet.Prod.CProd := '0000' + TrimRight(IntToStr(i));
          oDet.Prod.CEAN := 'SEM GTIN';
          oDet.Prod.XProd := 'NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL'; //###
          oDet.Prod.NCM := '84714900';
          oDet.Prod.CFOP := '5102';
          oDet.Prod.UCom := 'LU';
          oDet.Prod.QCom := 1.00;
          oDet.Prod.VUnCom := 84.90;
          oDet.Prod.VProd := 84.90;
          oDet.Prod.CEANTrib := 'SEM GTIN';
          oDet.Prod.UTrib := 'LU';
          oDet.Prod.QTrib := 1.00;
          oDet.Prod.VUnTrib := 84.90;
          oDet.Prod.IndTot := 1; // SimNao.Sim
          oDet.Prod.XPed := '300474';
          oDet.Prod.NItemPed := 1;

          // criar tag Imposto
          oDet.Imposto := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Imposto');
          oDet.Imposto.VTotTrib := 12.63;

          // criar tag Icms
          oDet.Imposto.ICMS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ICMS');

          // criar tag ICMSSN101
          oDet.Imposto.ICMS.ICMSSN102 := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ICMSSN102');
          oDet.Imposto.ICMS.ICMSSN102.Orig := 0; // OrigemMercadoria.Nacional

          // criar tag PIS
          oDet.Imposto.PIS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.PIS');

          // criar tag PISOutr
          oDet.Imposto.PIS.PISOutr := CreateOleObject('Unimake.Business.DFe.Xml.NFe.PISOutr');
          oDet.Imposto.PIS.PISOutr.CST := '99';
          oDet.Imposto.PIS.PISOutr.VBC := olevariant(0.00);
          oDet.Imposto.PIS.PISOutr.PPIS := olevariant(0.00);
          oDet.Imposto.PIS.PISOutr.VPIS := olevariant(0.00);

          // criar tag COFINS
          oDet.Imposto.COFINS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.COFINS');

          // criar tag COFINSOutr
          oDet.Imposto.COFINS.COFINSOutr := CreateOleObject('Unimake.Business.DFe.Xml.NFe.COFINSOutr');
          oDet.Imposto.COFINS.COFINSOutr.CST := '99';
          oDet.Imposto.COFINS.COFINSOutr.VBC := olevariant(0.00);
          oDet.Imposto.COFINS.COFINSOutr.PCOFINS := olevariant(0.00);
          oDet.Imposto.COFINS.COFINSOutr.VCOFINS := olevariant(0.00);

          // adicionar a tag Det dentro da tag InfNfe
          oInfNfe.AddDet(IUnknown(oDet));
        end;

        // Criar tag Total
        oInfNfe.Total := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Total');

        // Criar tag ICMSTot
        oInfNfe.Total.ICMSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ICMSTot');
        oInfNfe.Total.ICMSTot.VBC := 0;
        oInfNfe.Total.ICMSTot.VICMS := 0;
        oInfNfe.Total.ICMSTot.VICMSDeson := 0;
        oInfNfe.Total.ICMSTot.VFCP := 0;
        oInfNfe.Total.ICMSTot.VBCST := 0;
        oInfNfe.Total.ICMSTot.VST := 0;
        oInfNfe.Total.ICMSTot.VFCPST := 0;
        oInfNfe.Total.ICMSTot.VFCPSTRet := 0;
        oInfNfe.Total.ICMSTot.VProd := 254.70;
        oInfNfe.Total.ICMSTot.VFrete := 0;
        oInfNfe.Total.ICMSTot.VSeg := 0;
        oInfNfe.Total.ICMSTot.VDesc := 0;
        oInfNfe.Total.ICMSTot.VII := 0;
        oInfNfe.Total.ICMSTot.VIPI := 0;
        oInfNfe.Total.ICMSTot.VIPIDevol := 0;
        oInfNfe.Total.ICMSTot.VPIS := 0;
        oInfNfe.Total.ICMSTot.VCOFINS := 0;
        oInfNfe.Total.ICMSTot.VOutro := 0;
        oInfNfe.Total.ICMSTot.VNF := 254.70;
        oInfNfe.Total.ICMSTot.VTotTrib := 37.89;

        // Criar a tag Transp
        oInfNfe.Transp := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Transp');
        oInfNfe.Transp.ModFrete := 9; // ModalidadeFrete.SemOcorrenciaTransporte ###

        // criar tag Pag
        oInfNFe.Pag := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Pag');

        // criar tag DetPag (pode ter mais que uma, sÃ³ foi criada uma como exemplo)
        oDetPag := CreateOleObject('Unimake.Business.DFe.Xml.NFe.DetPag');
        oDetPag.IndPag := 0; // IndicadorPagamento.PagamentoVista
        oDetPag.TPag := 1; // MeioPagamento.Dinheiro
        oDetPag.VPag := 254.70;

        // adicionar a tag DetPag dentro da tag Tag
        oInfNFe.Pag.AddDetPag(IUnknown(oDetPag));

        // criar tag InfAdic
        oInfNFe.InfAdic := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfAdic');
        oInfNFe.InfAdic.InfCpl := 'Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008';

        // criar tag InfRespTec
        oInfNFe.InfRespTec := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfRespTec');
        oInfNFe.InfRespTec.CNPJ := '06117473000150';
        oInfNFe.InfRespTec.XContato := 'Ze das Couves';
        oInfNFe.InfRespTec.Email := 'zedascouves@gmail.com';
        oInfNFe.InfRespTec.Fone := '04430000000';
        oInfNFe.InfRespTec.IdCSRT := '01';
        oInfNFe.InfRespTec.CSRT := '8WCARAO9D8P00R845TARUPPTGY5CL40WS3J1';

        // adicionar a tag InfNfe dentro da tag Nfe
        oNfe.AddInfNFe(IUnknown(oInfNFe));

        // adiconar a tag nfe dentro da tag EnviNfe
        oEnviNFe.AddNfe(IUnknown(oNfe));

        //Recuperar a chave da NFe
        oConteudoNFe := oEnviNFe.GetNFe(0);
        oConteudoInfNFe := olevariant(oConteudoNFe.GetInfNFe(0));
        chaveNFe := VarToStr(oConteudoInfNFe.Chave);
      end
      else
      begin
        //Desserializar o XML a partir de um arquivo no HD/SSD
        oEnviNFe.AddNFe(IUnknown(oNFe.LoadFromFile('D:\testenfe\wandreytestenfce-nfe.xml')));
      end;

      //Consumir o serviço
      oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.NFCe.Autorizacao'); //###
      oAutorizacao.SetXMLConfiguracao(IUnknown(oEnviNFe), IUnknown(oConfiguracao));

      //Recuperar o conteúdo do XML assinado
      notaAssinada := VarToStr(oAutorizacao.GetConteudoNFeAssinada(0));

      //Definir caminho para salvar o XML assinado no HD/SSD
      caminhoArquivo := 'D:\testenfe\wandreytestenfce-nfe.xml';

      if statusEnvio = 0 then
      begin
        // Excluir arquivo existente, se houver
        if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

        // Gravar o XML assinado no HD
        with TFileStream.Create(caminhoArquivo, fmCreate) do
        try
          WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
        finally
          Free;
        end;
      end;

      //Só enviar se não estiver offline
      if (not envioOffline) and (statusEnvio < 10) then
      begin
        statusEnvio := 10;

        oAutorizacao.Executar(IUnknown(oEnviNFe), IUnknown(oConfiguracao));
      end
      else
      begin
        lSair := True;
      end;

      if not envioOffline then
      begin
        // XML Retornado pela SEFAZ
        xmlRetornado := VarToStr(oAutorizacao.RetornoWSString);

        cStat := oAutorizacao.Result.CStat;

        //Simular alguns status para teste
        //cStat := 108; //Servidor de processamento está paralisado temnporariamente
        //cStat := 109; //Servidor de Processamento está Paralisado sem Previsão de retorno
        //cStat := 104; //Processado com sucesso

        // Verifica se o lote foi processado (CStat = 104)
        if cStat = 104 then
        begin
          cStat := oAutorizacao.Result.ProtNFe.InfProt.CStat;
          //Simular alguns cStat
          cStat := 100;

          // Verifica se a NF-e foi autorizada (CStat = 100)
          if cStat = 100 then
          begin
            // Gravar XML de distribuição em uma pasta (NFe com o protocolo anexado)
            oAutorizacao.GravarXmlDistribuicao('d:\testenfe');

            // Pegar a string do XML de distribuição
            docProcNFe := VarToStr(oAutorizacao.GetNFeProcResults(chaveNFe));
            ShowMessage(docProcNFe);

            // Pegar o número do protocolo de autorização
            numeroProtocolo := VarToStr(oAutorizacao.Result.ProtNFe.InfProt.NProt);
            ShowMessage(numeroProtocolo);
            statusEnvio := 90; //NFCe autorizada (travo a nota e não deixo fazer mais nada)
            lSair := true;
          end
          else
          begin
            // Outra rejeição, voltar o status da nota a zero para permitir o usuário gerar a nota novamente mas já com a correções
            ShowMessage(Trim(IntToStr(oAutorizacao.Result.ProtNFe.InfProt.CStat)) + ' ' + VarToStr(oAutorizacao.Result.ProtNFe.InfProt.XMotivo));
            statusEnvio := 0;
            lSair := true;
          end;
        end
        else
        begin
          if cStat = 108 then //Servidor de processamento está paralisado temnporariamente
          begin
            xJust := 'Servidor de Processamento está Paralisado sem Previsão de retorno - cStat = 108';
            statusEnvio := 0;
            envioOffline := true;
          end
          else
          begin
            if cStat = 109 then //Servidor de Processamento está Paralisado sem Previsão de retorno
            begin
              xJust := 'Servidor de Processamento está Paralisado sem Previsão de retorno - cStat = 109';
              statusEnvio := 0;
              envioOffline := true;
            end
            else
            begin
              // Outra rejeição, voltar o status da nota a zero para permitir o usuário gerar a nota novamente mas já com a correções
              ShowMessage(Trim(IntToStr(oAutorizacao.Result.CStat)) + ' ' + VarToStr(oAutorizacao.Result.XMotivo));
              statusEnvio := 0;
              lSair := True;
            end;
          end;
        end;
      end
      else
      begin
        //Disparar a impressão do DANFE em contingência offline
        oConfigDANFe := CreateOleObject('Unimake.Unidanfe.Configurations.UnidanfeConfiguration');
        oConfigDANFe.Arquivo := notaAssinada; //String da nota assinada
        oConfigDANFe.Visualizar := True;
        oConfigDANFe.Imprimir := False;
        oConfigDANFe.EnviaEmail := False;

        //Disparar a impressao do DANFe
        oDANFe := CreateOleObject('Unimake.Unidanfe.UnidanfeServices');
        oDANFe.Execute(oConfigDANFe);

        lSair := True;
        statusEnvio := 2; // XML gerado e emitido danfe em contingência offline
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
end;

end.
