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
  ConsultarStatusNFCom, EnviarNFComSincrono;

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
    procedure btnEnviarEventoManifestacaoNFeClick(Sender: TObject);
    procedure btnEnviarNFCeSincronoClick(Sender: TObject);
    procedure btnEnviarNFCeSincronoDesserializacaoClick(Sender: TObject);
    procedure BtnEnviarNFComSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDuplicidadeClick(Sender: TObject);
    procedure btnEnviarNFeSincronoRTCClick(Sender: TObject);
    procedure btnEventoCancelamentoNFCeClick(Sender: TObject);
    procedure btnEventoCancelamentoNFeClick(Sender: TObject);
    procedure btnEventoCCENFeClick(Sender: TObject);
    procedure btnImprimirDANFEcomUniDANFEClick(Sender: TObject);
    procedure btnInutilizacaoNumeroNFeClick(Sender: TObject);
    procedure BtnNACIONALCancelarNFSeClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSeClick(Sender: TObject);
    procedure BtnNACIONALConsultarNFSePorRPSClick(Sender: TObject);
    procedure BtnNACIONALConsultarPDFNFSeClick(Sender: TObject);
    procedure BtnNACIONALGerarNFSeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox3Click(Sender: TObject);

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

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox3Click(Sender: TObject);
begin

end;

end.
