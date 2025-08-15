// ------------------------------------------------------------------
// Enviar NFe no modo síncrono (Com as tags da reforma tributária)
// ------------------------------------------------------------------

unit EnviarNFeSincronoRTC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Dialogs, Variants;

type
  TEnviarNFeSincronoRTC = class
  private

  public
    procedure Executar;
  end;

implementation

procedure TEnviarNFeSincronoRTC.Executar;
var
  oConfiguracao: olevariant;
  oExceptionInterop: olevariant;

  oEnviNFe: olevariant;
  oNfe: olevariant;
  oInfNFe: olevariant;
  oDet: olevariant;
  oVol: olevariant;
  oDup: olevariant;
  oDetPag: olevariant;

  oConteudoNFe: olevariant;
  oConteudoInfNFe: olevariant;
  chaveNFe: string;

  oAutorizacao: olevariant;

  notaAssinada: string;
  caminhoArquivo: string;

  xmlRetornado: string;
  statusRetorno, motivoRetorno: string;
  docProcNFe: string;
  numeroProtocolo: string;

  i: integer;

begin
  // Criar objeto de configuração mínima
  oConfiguracao := CreateOleObject('Unimake.Business.DFe.Servicos.Configuracao');
  oConfiguracao.TipoDFe := 0; //0=NFe
  oConfiguracao.TipoEmissao := 1; //1=Normal
  oConfiguracao.CertificadoArquivo := 'C:\Projetos\certificados\UnimakePV.pfx';
  oConfiguracao.CertificadoSenha := '12345678';

  //Criar objeto para pegar exceção do lado do CSHARP
  oExceptionInterop := CreateOleObject('Unimake.Exceptions.ThrowHelper');

  try
    // Criar a tag <enviNFe>
    oEnviNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnviNFe');
    oEnviNFe.Versao := '4.00';
    oEnviNFe.IdLote := '000000000000001';
    oEnviNFe.IndSinc := 1; // 1=Sim 0=Nao

    // Criar a tag <NFe>
    oNfe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.NFe');

    // Criar tag InfNfe
    oInfNFe := CreateOleObject('Unimake.Business.DFe.Xml.NFe.InfNFe');
    oInfNFe.Versao := '4.00';

    // Cria tag Ide
    oInfNFe.Ide := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Ide');
    oInfNFe.Ide.CUF := 41; // Brasil.PR
    oInfNFe.Ide.NatOp := 'VENDA PRODUC.DO ESTABELEC';
    oInfNFe.Ide.&Mod := 55; // NFe
    oInfNFe.Ide.Serie := 59;
    oInfNFe.Ide.NNF := 33;
    oInfNFe.Ide.DhEmi := Now;
    oInfNFe.Ide.DhSaiEnt := Now;
    oInfNFe.Ide.TpNF := 1; // Saida
    oInfNFe.Ide.IdDest := 2; // OperacaoInterestadual
    oInfNFe.Ide.CMunFG := 4118402;
    oInfNFe.Ide.TpImp := 1; // FormatoImpressaoDANFE.NormalRetrato
    oInfNFe.Ide.TpEmis := 1; // TipoEmissao.Normal
    oInfNFe.Ide.TpAmb := 2; // TipoAmbiente.Homologacao
    oInfNFe.Ide.FinNFe := 1; // FinalidadeNFe.Normal
    oInfNFe.Ide.IndFinal := 1; // SimNao.Sim
    oInfNFe.Ide.IndPres := 1; // IndicadorPresenca.OperacaoPresencial
    oInfNFe.Ide.ProcEmi := 0; // ProcessoEmissao.AplicativoContribuinte
    oInfNFe.Ide.VerProc := 'TESTE 1.00';

    //RTC
    oInfNFe.Ide.CMunFGIBS := 3543402;
    oInfNFe.Ide.GCompraGov := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GCompraGov');
    oInfNFe.Ide.GCompraGov.PRedutor := 0;
    oInfNFe.Ide.GCompraGov.TpEnteGov := 4; //TipoEnteGovernamental.Municipio,
    oInfNFe.Ide.GCompraGov.TpOperGov := 1; //TipoOperacaoEnteGovernamental.Fornecimento
    oInfNFe.Ide.GPagAntecipado := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GPagAntecipado');
    oInfNFe.Ide.GPagAntecipado.AddRefNFe('00000000000000000000000000000000000000000000');
    oInfNFe.Ide.GPagAntecipado.AddRefNFe('11111111111111111111111111111111111111111111');
    oInfNFe.Ide.TpNFCredito := 2; //TipoNFCredito.ApropriacaoCreditoPresumidoIBSZFM
    oInfNFe.Ide.TpNFDebito := 6; //TipoNFDebito.PagamentoAntecipado

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
    oInfNFe.Dest := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Dest');
    oInfNFe.Dest.CNPJ := '04218457000128';
    oInfNFe.Dest.XNome := 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
    oInfNFe.Dest.IndIEDest := 1; // IndicadorIEDestinatario.ContribuinteICMS
    oInfNFe.Dest.IE := '582614838110';
    oInfNFe.Dest.Email := 'janelaorp@janelaorp.com.br';

    oInfNFe.Dest.EnderDest := CreateOleObject('Unimake.Business.DFe.Xml.NFe.EnderDest');
    oInfNFe.Dest.EnderDest.XLgr := 'AVENIDA DA SAUDADE';
    oInfNFe.Dest.EnderDest.Nro := '1555';
    oInfNFe.Dest.EnderDest.XBairro := 'CAMPOS ELISEOS';
    oInfNFe.Dest.EnderDest.CMun := 3543402;
    oInfNFe.Dest.EnderDest.XMun := 'RIBEIRAO PRETO';
    oInfNFe.Dest.EnderDest.UF := 35; // UFBrasil.SP
    oInfNFe.Dest.EnderDest.CEP := '14080000';
    oInfNFe.Dest.EnderDest.Fone := '01639611500';

    for i := 1 to 3 do
    begin
      // criar tag Det
      oDet := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Det');
      oDet.NItem := i;

      oDet.Prod := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Prod');
      oDet.Prod.CProd := '0000' + TrimRight(IntToStr(i));
      oDet.Prod.CEAN := 'SEM GTIN';
      oDet.Prod.XProd := 'NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL';
      oDet.Prod.NCM := '84714900';
      oDet.Prod.CFOP := '6101';
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
      oDet.Imposto.ICMS.ICMSSN101 := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ICMSSN101');
      oDet.Imposto.ICMS.ICMSSN101.Orig := 0; // OrigemMercadoria.Nacional
      oDet.Imposto.ICMS.ICMSSN101.PCredSN := 2.8255;
      oDet.Imposto.ICMS.ICMSSN101.VCredICMSSN := 2.40;

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

      //RTC
      oDet.Imposto.IBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.IBSCBS');
      oDet.Imposto.IBSCBS.CST := '000';
      oDet.Imposto.IBSCBS.CClassTrib := '000001';

      oDet.Imposto.IBSCBS.GIBSCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSCBS');
      oDet.Imposto.IBSCBS.GIBSCBS.VBC := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GCBS');
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.PCBS := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.VCBS := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDevTrib := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDevTrib');
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDevTrib.VDevTrib := 0;

      //oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDif := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDif');
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDif.PDif := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDif.VDif := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GRed');
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed.PAliqEfet := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed.PRedAliq := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSMun');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.PIBSMun := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.VIBSMun := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDevTrib := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDevTrib');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDevTrib.VDevTrib := 0;

      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDif := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDif');
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDif.PDif := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDif.VDif := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GRed');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed.PAliqEfet := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed.PRedAliq := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSUF');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.PIBSUF := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.VIBSUF := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDevTrib := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDevTrib');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDevTrib.VDevTrib := 0;

      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDif := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GDif');
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDif.PDif := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDif.VDif := 0;

      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GRed');
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed.PAliqEfet := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed.PRedAliq := 0;
      oDet.Imposto.IBSCBS.GIBSCBS.VIBS := 0;

      //oDet.Imposto.IBSCBS.GIBSCBS.GCBSCredPres :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GCBSCredPres');
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBSCredPres.CCredPres := '';
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBSCredPres.PCredPres := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBSCredPres.VCredPres := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GCBSCredPres.VCredPresCondSus := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSCredPres :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSCredPres');
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSCredPres.CCredPres := '';
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSCredPres.PCredPres := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSCredPres.VCredPres := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GIBSCredPres.VCredPresCondSus := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GTribCompraGov');
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.PAliqCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.PAliqIBSMun := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.PAliqIBSUF := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.VTribCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.VTribIBSMun := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribCompraGov.VTribIBSUF := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GTribRegular');
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.CClassTribReg := '000001';
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.CSTReg := '000';
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.PAliqEfetRegCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.PAliqEfetRegIBSMun := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.PAliqEfetRegIBSUF := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.VTribRegCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.VTribRegIBSMun := 0;
      //oDet.Imposto.IBSCBS.GIBSCBS.GTribRegular.VTribRegIBSUF := 0;

      //oDet.Imposto.IBSCBS.GCredPresIBSZFM :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GCredPresIBSZFM');
      //oDet.Imposto.IBSCBS.GCredPresIBSZFM.TpCredPresIBSZFM := 0; //TipoCreditoPresumidoIBSZFM.SemCreditoPresumido
      //oDet.Imposto.IBSCBS.GCredPresIBSZFM.VCredPresIBSZFM := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSCBSMono');
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoDif :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GMonoDif');
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoDif.PDifCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoDif.PDifIBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoDif.VIBSMonoDif := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoDif.VCBSMonoDif := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GMonoPadrao');
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao.QBCMono := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao.AdRemCBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao.AdRemIBS := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao.VIBSMono := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoPadrao.VCBSMono := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GMonoRet');
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet.QBCMonoRet := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet.AdRemCBSRet := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet.AdRemIBSRet := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet.VIBSMonoRet := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoRet.VCBSMonoRet := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GMonoReten');
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten.QBCMonoReten := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten.AdRemCBSReten := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten.AdRemIBSReten := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten.VCBSMonoReten := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.GMonoReten.VIBSMonoReten := 0;
      //
      //oDet.Imposto.IBSCBS.GIBSCBSMono.VTotCBSMonoItem := 0;
      //oDet.Imposto.IBSCBS.GIBSCBSMono.VTotIBSMonoItem := 0;
      //
      //oDet.Imposto.IBSCBS.GTransfCred :=  CreateOleObject('Unimake.Business.DFe.Xml.NFe.GTransfCred');
      //oDet.Imposto.IBSCBS.GTransfCred.VCBS := 0;
      //oDet.Imposto.IBSCBS.GTransfCred.VIBS := 0;

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

    //RTC
    oInfNfe.Total.IBSCBSTot := CreateOleObject('Unimake.Business.DFe.Xml.NFe.IBSCBSTot');
    oInfNFe.Total.IBSCBSTot.VBCIBSCBS := 0;

    oInfNfe.Total.IBSCBSTot.GCBS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GCBSTot');
    oInfNfe.Total.IBSCBSTot.GCBS.VCBS := 0;
    oInfNfe.Total.IBSCBSTot.GCBS.VCredPres := 0;
    oInfNfe.Total.IBSCBSTot.GCBS.VCredPresCondSus := 0;
    oInfNfe.Total.IBSCBSTot.GCBS.VDevTrib := 0;
    oInfNfe.Total.IBSCBSTot.GCBS.VDif := 0;

    oInfNfe.Total.IBSCBSTot.GIBS := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSTot');

    oInfNfe.Total.IBSCBSTot.GIBS.GIBSMun := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSMunTot');
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSMun.VDevTrib := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSMun.VDif := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSMun.VIBSMun := 0;

    oInfNfe.Total.IBSCBSTot.GIBS.GIBSUF := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GIBSUFTot');
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSUF.VDevTrib := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSUF.VDif := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.GIBSUF.VIBSUF := 0;

    oInfNfe.Total.IBSCBSTot.GIBS.VCredPres := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.VCredPresCondSus := 0;
    oInfNfe.Total.IBSCBSTot.GIBS.VIBS := 0;

    oInfNFe.Total.IBSCBSTot.GMono := CreateOleObject('Unimake.Business.DFe.Xml.NFe.GMono');
    oInfNFe.Total.IBSCBSTot.GMono.VCBSMono := 0;
    oInfNFe.Total.IBSCBSTot.GMono.VCBSMonoRet := 0;
    oInfNFe.Total.IBSCBSTot.GMono.VCBSMonoReten := 0;
    oInfNFe.Total.IBSCBSTot.GMono.VIBSMono := 0;
    oInfNFe.Total.IBSCBSTot.GMono.VIBSMonoRet := 0;
    oInfNFe.Total.IBSCBSTot.GMono.VIBSMonoReten := 0;

    oInfNFe.Total.ISTot := CreateOleObject('Unimake.Business.DFe.Xml.NFe.ISTot');
    oInfNFe.Total.ISTot.VIS := 0;

    oInfNFe.Total.VNFTot := 1;

    // Criar a tag Transp
    oInfNfe.Transp := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Transp');
    oInfNfe.Transp.ModFrete := 0; // ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF

    for i := 1 to 3 do
    begin
      // Criar a tag Vol
      oVol := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Vol');
      oVol.QVol := 1;
      oVol.Esp := 'LU';
      oVol.Marca := 'UNIMAKE';
      oVol.PesoL := 0.000;
      oVol.PesoB := 0.000;

      // adicionar a tag Vol na tag Transp
      oInfNfe.Transp.AddVol(IUnknown(oVol));
    end;

    // Criar tag Cobr
    oInfNfe.Cobr := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Cobr');

    // Criar tag Fat
    oInfNfe.Cobr.Fat := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Fat');
    oInfNfe.Cobr.Fat.NFat := '057910';
    oInfNfe.Cobr.Fat.VOrig := 254.70;
    oInfNfe.Cobr.Fat.VDesc := 0;
    oInfNfe.Cobr.Fat.VLiq := 254.70;

    for i := 1 to 2 do
    begin
      // Criar tag Dup
      oDup := CreateOleObject('Unimake.Business.DFe.Xml.NFe.Dup');
      oDup.NDup := '00' + TrimRight(IntToStr(I));
      oDup.DVenc := Date();
      oDup.VDup := 127.35;

      // adicionar a tag Dup dentro da tag Cobr
      oInfNfe.Cobr.AddDup(IUnknown(oDup));
    end;

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

    ShowMessage('Chave NFe: ' + chaveNFe);

    //Recuperar outras informações da NFe
    ShowMessage(VarToStr(oConteudoInfNFe.Ide.cUF));
    ShowMessage(VarToStr(oConteudoInfNFe.Emit.XNome));

    //Consumir o serviço
    oAutorizacao := CreateOleObject('Unimake.Business.DFe.Servicos.NFe.Autorizacao');
    oAutorizacao.SetXMLConfiguracao(IUnknown(oEnviNFe), IUnknown(oConfiguracao));

    //Recuperar o conteúdo do XML assinado
    notaAssinada := VarToStr(oAutorizacao.GetConteudoNFeAssinada(0));

    //Exibir o XML assinado
    ShowMessage(notaAssinada);

    //Definir caminho para salvar o XML assinado no HD/SSD
    caminhoArquivo := 'd:\testenfe\' + chaveNFe + '-nfe.xml';

    // Excluir arquivo existente, se houver
    if FileExists(caminhoArquivo) then DeleteFile(caminhoArquivo);

    // Gravar o XML assinado no HD
    with TFileStream.Create(caminhoArquivo, fmCreate) do
      try
        WriteBuffer(Pointer(notaAssinada)^, Length(notaAssinada));
      finally
        Free;
      end;

    oAutorizacao.Executar(IUnknown(oEnviNFe), IUnknown(oConfiguracao));

    // XML Retornado pela SEFAZ
    xmlRetornado := VarToStr(oAutorizacao.RetornoWSString);
    ShowMessage(xmlRetornado);

    // Código de Status e Motivo
    statusRetorno := Trim(IntToStr(oAutorizacao.Result.CStat)) + ' ' + VarToStr(oAutorizacao.Result.XMotivo);
    ShowMessage(statusRetorno);

    // Verifica se o lote foi processado (CStat = 104)
    if oAutorizacao.Result.CStat = 104 then
    begin
      // Verifica se a NF-e foi autorizada (CStat = 100)
      if oAutorizacao.Result.ProtNFe.InfProt.CStat = 100 then
      begin
        // Gravar XML de distribuição em uma pasta (NFe com o protocolo anexado)
        oAutorizacao.GravarXmlDistribuicao('d:\testenfe');

        // Pegar a string do XML de distribuição
        docProcNFe := VarToStr(oAutorizacao.GetNFeProcResults(chaveNFe));
        ShowMessage(docProcNFe);

        // Pegar o número do protocolo de autorização
        numeroProtocolo := VarToStr(oAutorizacao.Result.ProtNFe.InfProt.NProt);
        ShowMessage(numeroProtocolo);
      end
      else
      begin
        // Rejeitada ou Denegada - Fazer devidos tratamentos
        ShowMessage('NF-e rejeitada ou denegada. Verifique os detalhes.');
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
