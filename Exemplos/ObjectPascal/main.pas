unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComObj,
  ConsultarStatusNFe, EnviarNFeSincrono, EnviarNFeSincronoDuplicidade,
  EnviarNFeSincronoDesserializacao, ConsultarSituacaoNFe, EventoCancelamentoNFe,
  ImprimirDANFEcomUniDANFE, ConsultarDistribuicaoDFe, EnviarEventoManifestacaoNFe,
  EnviarNFCeSincrono, EnviarNFCeSincronoDesserializacao, EventoCancelamentoNFCe,
  EnviarNFeSincronoRTC, DesserializarRetornoConsultaDFe, CertificadoDigital,
  BETHAGerarNFSe, BETHACancelarNFSe, BETHAEnviarLoteRPSSincrono,
  BETHAConsultarNFSeRPS, BETHAConsultarLoteRPS, NACIONALGerarNFSe,
  NACIONALCancelarNFSe, NACIONALConsultarNFSe, NACIONALConsultarNFSeRPS,
  NACIONALConsultarNFSePDF, EventoCCENFe, InutilizacaoNumeroNFe,
  ConsultarStatusNFCom, EnviarNFComSincrono, EnviarMDFeSincrono,
  EnviarEventoCancelamentoMDFe, EnviarEventoEncerramentoMDFe,
  ConsultarStatusMDFe, ConsultarSituacaoMDFe,ConsultarMDFeNaoEncerrado,EventoPagamentoMDFe,
  EnviarEventoAlteracaoPagamentoMDFe,ConsultarSituacaoNFCom,EnviarEventoCancelamentoNFCom,
  ConsultarStatusCte, ConsultarSituacaoCTe, EnviarCteSincrono, EnviarCteOsSincrono,EventoCancelamentoCTe,
  EnviarEventoCancelamentoCTeOS, EventoCCeCTe,InsucessoEntregaCTe,CancelamentoInsucessoEntregaCTe, DesserializandoXmlCTeOS,
  EventoCTeDesacordo, EventoEpecCte, ConsultaConfigGNRE, EnviarXmlGNRe, ConsultaResultadoLoteGNRE,
  EnviarNFCeOffline, EnviarNFeSincronoContingenciaSVC, NACIONALGerarNFSeObjeto,
  NACIONALCancelarNFSeObjeto, NACIONALConsultarNFSeRPSObjeto, NACIONALConsultarNFSeObjeto;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    btnConsultaStatusNFe: TButton;
    btnEnviarNFCeSincrono: TButton;
    btnEnviarNFCeSincronoDesserializacao: TButton;
    btnEnviarNFeSincrono: TButton;
    btnEnviarNFeSincronoDesserializacao: TButton;
    btnEnviarNFeSincronoDuplicidade: TButton;
    btnConsultarSituacaoNFe: TButton;
    btnEnviarNFeSincronoRTC: TButton;
    btnEventoCancelamentoNFe: TButton;
    btnImprimirDANFEcomUniDANFE: TButton;
    BtnCertificadoDigital: TButton;
    BtnBETHAGerarNFSe: TButton;
    BtnBETHACancelarNFSe: TButton;
    BtnBETHAEnviarLoteRPSSincrono: TButton;
    BtnBETHAConsultarLoteRPS: TButton;
    BtnNACIONALGerarNFSe: TButton;
    BtnNACIONALCancelarNFSe: TButton;
    BtnNACIONALConsultarNFSe: TButton;
    BtnNACIONALConsultarNFSePorRPS: TButton;
    BtnNACIONALConsultarPDFNFSe: TButton;
    btnEventoCCENFe: TButton;
    btnInutilizacaoNumeroNFe: TButton;
    BtnEnviarNFComSincrono: TButton;
    BtnEnviarEventoEncerramentoMDFe: TButton;
    BtnNACIONALGerarNFSeObjeto: TButton;
    Btn_CancelamentoInsucessoEntregaCTe: TButton;
    Btn_ConsultarStatusMDFe: TButton;
    Btn_ConsultarSituacaoMDFe: TButton;
    Btn_ConsultaMdfeNaoEncerrado: TButton;
    Btn_EventoPagamentoMDFe: TButton;
    Btn_EnviarEventoAlteracaoPagamentoMDFe: TButton;
    Btn_ConsultaSituacaoNFCom: TButton;
    Btn_EnviarEventoCancelamentoNFCom: TButton;
    Btn_ConsultaStatusCte: TButton;
    Btn_ConsultaSituacaoCTe: TButton;
    Btn_EnviarCteSincrono: TButton;
    Btn_EnviarCteOsSincrono: TButton;
    Btn_EventoCancelamentoCTe: TButton;
    Btn_EnviarEventoCancelamentoCTeOS: TButton;
    Btn_EventoCCeCTe: TButton;
    Btn_InsucessoEntregaCTe: TButton;
    Btn_DesserializandoXmlCTeOS: TButton;
    Btn_EventoCteDesacordo: TButton;
    Btn_EventoEpecCte: TButton;
    Btn_ConsultaResultadoLoteGNRE: TButton;
    btnEnviarNFCeOffline: TButton;
    btnEnviarNFeSincronoContingenciaSVC: TButton;
    BtnNACIONALConsultarNFSePorRPSObjeto: TButton;
    BtnNACIONALConsultarNFSeObjeto: TButton;
    EnviarXmlGNRe: TButton;
    ConsultaConfigGNRE: TButton;
    GroupBox1: TGroupBox;
    btnConsultarDistribuicaoDFe: TToggleBox;
    btnEnviarEventoManifestacaoNFe: TToggleBox;
    GroupBox2: TGroupBox;
    btnEventoCancelamentoNFCe: TToggleBox;
    btnDesserializarRetornoConsultaDFe: TToggleBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    BtnBETHAConsultarNfseRps: TButton;
    GroupBox5: TGroupBox;
    BtnConsultarStatusNFCom: TToggleBox;
    GroupBox6: TGroupBox;
    BtnEnviarMDFeSincrono: TToggleBox;
    BtnEnviarEventoCancelamentoMDFe: TToggleBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    BtnNACIONALCancelarNFSeObjeto: TToggleBox;
    procedure BtnBETHACancelarNFSeClick(Sender: TObject);
    procedure BtnBETHAConsultarLoteRPSClick(Sender: TObject);
    procedure BtnBETHAConsultarNFSeRPSClick(Sender: TObject);
    procedure BtnBETHAEnviarLoteRPSSincronoClick(Sender: TObject);
    procedure BtnBETHAGerarNFSeClick(Sender: TObject);
    procedure BtnCertificadoDigitalClick(Sender: TObject);
    procedure btnConsultarDistribuicaoDFeOnClick(Sender: TObject);
    procedure btnConsultarSituacaoNFeClick(Sender: TObject);
    procedure BtnConsultarStatusNFComClick(Sender: TObject);
    procedure btnConsultaStatusNFeClick(Sender: TObject);
    procedure btnDesserializarRetornoConsultaDFeClick(Sender: TObject);
    procedure BtnEnviarEventoCancelamentoMDFeClick(Sender: TObject);
    procedure BtnEnviarEventoEncerramentoMDFeClick(Sender: TObject);
    procedure btnEnviarEventoManifestacaoNFeClick(Sender: TObject);
    procedure BtnEnviarMDFeSincronoClick(Sender: TObject);
    procedure btnEnviarNFCeOfflineClick(Sender: TObject);
    procedure btnEnviarNFCeSincronoClick(Sender: TObject);
    procedure btnEnviarNFCeSincronoDesserializacaoClick(Sender: TObject);
    procedure BtnEnviarNFComSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoContingenciaSVCClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDuplicidadeClick(Sender: TObject);
    procedure btnEnviarNFeSincronoRTCClick(Sender: TObject);
    procedure btnEventoCancelamentoNFCeClick(Sender: TObject);
    procedure btnEventoCancelamentoNFeClick(Sender: TObject);
    procedure btnEventoCCENFeClick(Sender: TObject);
    procedure btnImprimirDANFEcomUniDANFEClick(Sender: TObject);
    procedure btnInutilizacaoNumeroNFeClick(Sender: TObject);
    procedure BtnNACIONALCancelarNFSeClick(Sender: TObject);
    procedure BtnNACIONALCancelarNFSeObjetoClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSeClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSeObjetoClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSePorRPSClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSePorRPSObjetoClick(Sender: TObject);
    procedure BtnNACIONALConsultarPDFNFSeClick(Sender: TObject);
    procedure BtnNACIONALGerarNFSeClick(Sender: TObject);
    procedure BtnNACIONALGerarNFSeObjetoClick(Sender: TObject);
    procedure Btn_CancelamentoInsucessoEntregaCTeClick(Sender: TObject);
    procedure Btn_ConsultaResultadoLoteGNREClick(Sender: TObject);
    procedure Btn_ConsultaSituacaoCTeClick(Sender: TObject);
    procedure Btn_ConsultaStatusCteClick(Sender: TObject);
    procedure Btn_EnviarEventoCancelamentoCTeOSClick(Sender: TObject);
    procedure Btn_EnviarCteOsSincronoClick(Sender: TObject);
    procedure Btn_EnviarEventoAlteracaoPagamentoMDFeClick(Sender: TObject);
    procedure Btn_ConsultaMdfeNaoEncerradoClick(Sender: TObject);
    procedure Btn_ConsultarStatusMDFeClick(Sender: TObject);
    procedure Btn_ConsultarSituacaoMDFeClick(Sender: TObject);
    procedure Btn_EventoCancelamentoCTeClick(Sender: TObject);
    procedure Btn_EventoCCeCTeClick(Sender: TObject);
    procedure Btn_EventoEpecCteClick(Sender: TObject);
    procedure Btn_EventoPagamentoMDFeClick(Sender: TObject);
    procedure Btn_ConsultaSituacaoNFComClick(Sender: TObject);
    procedure Btn_EnviarEventoCancelamentoNFComClick(Sender: TObject);
    procedure Btn_EnviarCteSincronoClick(Sender: TObject);
    procedure Btn_InsucessoEntregaCTeClick(Sender: TObject);
    procedure Btn_DesserializandoXmlCTeOSClick(Sender: TObject);
    procedure Btn_EventoCteDesacordoClick(Sender: TObject);
    procedure ConsultaConfigGNREClick(Sender: TObject);
    procedure EnviarXmlGNReClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox2Click(Sender: TObject);
    procedure GroupBox3Click(Sender: TObject);
    procedure GroupBox8Click(Sender: TObject);

  private

  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

{$R *.lfm}

{ TfrmPrincipal }

procedure TfrmPrincipal.btnConsultaStatusNFeClick(Sender: TObject);
var
  oServico: TConsultarStatusNFe;
begin
  oServico := TConsultarStatusNFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnDesserializarRetornoConsultaDFeClick(Sender: TObject
  );
var
  oServico: TDesserializarRetornoConsultaDFe;
begin
  oServico := TDesserializarRetornoConsultaDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnEnviarEventoCancelamentoMDFeClick(Sender: TObject);
var
  oServico: TEnviarEventoCancelamentoMDFe;
begin
  oServico := TEnviarEventoCancelamentoMDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnEnviarEventoEncerramentoMDFeClick(Sender: TObject);
var
  oServico: TEnviarEventoEncerramentoMDFe;
begin
  oServico := TEnviarEventoEncerramentoMDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;


procedure TfrmPrincipal.btnEnviarEventoManifestacaoNFeClick(Sender: TObject);
var
  oServico: TEnviarEventoManifestacaoNFe;
begin
  oServico := TEnviarEventoManifestacaoNFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnEnviarMDFeSincronoClick(Sender: TObject);
var
  oServico: TEnviarMDFeSincrono;
begin
  oServico := TEnviarMDFeSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFCeOfflineClick(Sender: TObject);
var
  oServico: TEnviarNFCeOffline;
begin
  oServico := TEnviarNFCeOffline.Create;
  try
    oServico.Executar();
  finally
  end;
end;


procedure TfrmPrincipal.btnEnviarNFCeSincronoClick(Sender: TObject);
var
  oServico: TEnviarNFCeSincrono;
begin
  oServico := TEnviarNFCeSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFCeSincronoDesserializacaoClick(Sender: TObject);
var
  oServico: TEnviarNFCeSincronoDesserializacao;
begin
  oServico := TEnviarNFCeSincronoDesserializacao.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnEnviarNFComSincronoClick(Sender: TObject);
var
  oServico: TEnviarNFComSincrono;
begin
  oServico := TEnviarNFComSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;


procedure TfrmPrincipal.btnConsultarSituacaoNFeClick(Sender: TObject);
var
  oServico: TConsultarSituacaoNFe;
begin
  oServico := TConsultarSituacaoNFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnConsultarStatusNFComClick(Sender: TObject);
var
  oServico: TConsultarStatusNFCom;
begin
  oServico := TConsultarStatusNFCom.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnConsultarDistribuicaoDFeOnClick(Sender: TObject);
var
  oServico: TConsultarDistribuicaoDFe;
begin
  oServico := TConsultarDistribuicaoDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnCertificadoDigitalClick(Sender: TObject);
var
  oServico: TCertificadoDigital;
begin
  oServico := TCertificadoDigital.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnBETHAGerarNFSeClick(Sender: TObject);
var
  oServico: TBETHAGerarNFSe;
begin
  oServico := TBETHAGerarNFSe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnBETHACancelarNFSeClick(Sender: TObject);
var
  oServico: TBETHACancelarNFSe;
begin
  oServico := TBETHACancelarNFSe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnBETHAConsultarLoteRPSClick(Sender: TObject);
var
  oServico: TBETHAConsultarLoteRPS;
begin
  oServico := TBETHAConsultarLoteRPS.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnBETHAConsultarNFSeRPSClick(Sender: TObject);
var
  oServico: TBETHAConsultarNFSeRPS;
begin
  oServico := TBETHAConsultarNFSeRPS.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.BtnBETHAEnviarLoteRPSSincronoClick(Sender: TObject);
var
  oServico: TBETHAEnviarLoteRPSSincrono;
begin
  oServico := TBETHAEnviarLoteRPSSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoClick(Sender: TObject);
var
  oServico: TEnviarNFeSincrono;
begin
  oServico := TEnviarNFeSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoContingenciaSVCClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoContingenciaSVC;
begin
  oServico := TEnviarNFeSincronoContingenciaSVC.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoDesserializacao;
begin
  oServico := TEnviarNFeSincronoDesserializacao.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoDuplicidadeClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoDuplicidade;
begin
  oServico := TEnviarNFeSincronoDuplicidade.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoRTCClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoRTC;
begin
  oServico := TEnviarNFeSincronoRTC.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEventoCancelamentoNFCeClick(Sender: TObject);
var
  oServico: TEventoCancelamentoNFCe;
begin
  oServico := TEventoCancelamentoNFCe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEventoCancelamentoNFeClick(Sender: TObject);
var
  oServico: TEventoCancelamentoNFe;
begin
  oServico := TEventoCancelamentoNFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEventoCCENFeClick(Sender: TObject);
var
  oServico: TEventoCCENFe;
begin
  oServico := TEventoCCENFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnImprimirDANFEcomUniDANFEClick(Sender: TObject);
var
  oServico: TImprimirDANFEcomUniDANFE;
begin
  oServico := TImprimirDANFEcomUniDANFE.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.btnInutilizacaoNumeroNFeClick(Sender: TObject);
var
  oServico: TInutilizacaoNumeroNFe;
begin
  oServico := TInutilizacaoNumeroNFe.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALCancelarNFSeClick(Sender: TObject);
var
  oServico: TNACIONALCancelarNFSe;
begin
  oServico := TNACIONALCancelarNFSe.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALCancelarNFSeObjetoClick(Sender: TObject);
var
  oServico: TNACIONALCancelarNFSeObjeto;
begin
  oServico := TNACIONALCancelarNFSeObjeto.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALConsultarNFSeClick(Sender: TObject);
var
  oServico: TNACIONALConsultarNFSe;
begin
  oServico := TNACIONALConsultarNFSe.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALConsultarNFSeObjetoClick(Sender: TObject);
var
  oServico: TNACIONALConsultarNFSeObjeto;
begin
  oServico := TNACIONALConsultarNFSeObjeto.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALConsultarNFSePorRPSClick(Sender: TObject);
var
  oServico: TNACIONALConsultarNFSeRPS;
begin
  oServico := TNACIONALConsultarNFSeRPS.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALConsultarNFSePorRPSObjetoClick(
  Sender: TObject);
var
  oServico: TNACIONALConsultarNFSeRPSObjeto;
begin
  oServico := TNACIONALConsultarNFSeRPSObjeto.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALConsultarPDFNFSeClick(Sender: TObject);
var
  oServico: TNACIONALConsultarNFSePDF;
begin
  oServico := TNACIONALConsultarNFSePDF.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALGerarNFSeClick(Sender: TObject);
var
  oServico: TNACIONALGerarNFSe;
begin
  oServico := TNACIONALGerarNFSe.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.BtnNACIONALGerarNFSeObjetoClick(Sender: TObject);
var
  oServico: TNACIONALGerarNFSeObjeto;
begin
  oServico := TNACIONALGerarNFSeObjeto.Create;
  try
      oServico.Executar();
    finally
    end;
end;


procedure TfrmPrincipal.Btn_CancelamentoInsucessoEntregaCTeClick(Sender: TObject);
var
  oServico: TCancelamentoInsucessoEntregaCTe;
begin
  oServico := TCancelamentoInsucessoEntregaCTe.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_ConsultaResultadoLoteGNREClick(Sender: TObject);
var
  oServico: TConsultaResultadoLoteGNRE;
begin
  oServico := TConsultaResultadoLoteGNRE.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_ConsultaSituacaoCTeClick(Sender: TObject);
var
  oServico: TConsultarSituacaoCTe;
begin
  oServico := TConsultarSituacaoCTe.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_ConsultaStatusCteClick(Sender: TObject);
var
  oServico: TConsultaStatusCte;
begin
  oServico := TConsultaStatusCte.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_EnviarEventoCancelamentoCTeOSClick(Sender: TObject);
var
  oServico: TEnviarEventoCancelamentoCTeOS;
begin
  oServico := TEnviarEventoCancelamentoCTeOS.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_EnviarCteOsSincronoClick(Sender: TObject);
var
  oServico: TEnviarCteOsSincrono;
begin
  oServico := TEnviarCteOsSincrono.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_EnviarEventoAlteracaoPagamentoMDFeClick(Sender: TObject);
var
  oServico: TEnviarEventoAlteracaoPagamentoMDFe;
begin
  oServico := TEnviarEventoAlteracaoPagamentoMDFe.create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_ConsultaMdfeNaoEncerradoClick(Sender: TObject);
  var
    oServico: TConsultarMDFeNaoEncerrado;
  begin
    oServico := TConsultarMDFeNaoEncerrado.create;
    try
        oServico.Executar();
      finally
      end;
  end;

procedure TfrmPrincipal.Btn_ConsultarStatusMDFeClick(Sender: TObject);
var
  oServico: TConsultarStatusMDFe;
begin
  oServico := TConsultarStatusMDFe.Create;
  try
      oServico.Executar();
    finally
    end;
end;

procedure TfrmPrincipal.Btn_ConsultarSituacaoMDFeClick(Sender: TObject);
var
  oServico: TConsultarSituacaoMDFe;
begin
  oServico := TConsultarSituacaoMDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EventoCancelamentoCTeClick(Sender: TObject);
var
  oServico: TEventoCancelamentoCTe;
begin
  oServico := TEventoCancelamentoCTe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EventoCCeCTeClick(Sender: TObject);
var
  oServico: TEventoCCeCTe;
begin
  oServico := TEventoCCeCTe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EventoEpecCteClick(Sender: TObject);
var
  oServico: TEventoEpecCte;
begin
  oServico := TEventoEpecCte.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EventoPagamentoMDFeClick(Sender: TObject);
var
  oServico: TEventoPagamentoMDFe;
begin
  oServico := TEventoPagamentoMDFe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_ConsultaSituacaoNFComClick(Sender: TObject);
var
  oServico: TConsultaSituacaoNFCom;
begin
  oServico := TConsultaSituacaoNFCom.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EnviarEventoCancelamentoNFComClick(Sender: TObject);
var
  oServico: TEnviarEventoCancelamentoNFCom;
begin
  oServico := TEnviarEventoCancelamentoNFCom.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EnviarCteSincronoClick(Sender: TObject);
var
  oServico: TEnviarCteSincrono;
begin
  oServico := TEnviarCteSincrono.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_InsucessoEntregaCTeClick(Sender: TObject);
var
  oServico: TInsucessoEntregaCTe;
begin
  oServico := TInsucessoEntregaCTe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_DesserializandoXmlCTeOSClick(Sender: TObject);
var
  oServico: TDesserializandoXmlCTeOS;
begin
  oServico := TDesserializandoXmlCTeOS.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.Btn_EventoCteDesacordoClick(Sender: TObject);
var
  oServico: TEventoCteDesacordo;
begin
  oServico := TEventoCteDesacordo.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.ConsultaConfigGNREClick(Sender: TObject);
var
  oServico: TConsultaConfigGNRE;
begin
  oServico := TConsultaConfigGNRE.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.EnviarXmlGNReClick(Sender: TObject);
var
  oServico: TEnviarXmlGNRe;
begin
  oServico := TEnviarXmlGNRe.Create;
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox1Click(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox2Click(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox3Click(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox8Click(Sender: TObject);
begin

end;

end.
