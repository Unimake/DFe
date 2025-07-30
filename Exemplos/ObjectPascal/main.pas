unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComObj,
  ConsultarStatusNFe, EnviarNFeSincrono, EnviarNFeSincronoDuplicidade,
  EnviarNFeSincronoDesserializacao, ConsultarSituacaoNFe, EventoCancelamentoNFe,
  ImprimirDANFEcomUniDANFE, ConsultarDistribuicaoDFe, EnviarEventoManifestacaoNFe,
  EnviarNFCeSincrono, EnviarNFCeSincronoDesserializacao, EventoCancelamentoNFCe;

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
    btnEventoCancelamentoNFe: TButton;
    btnImprimirDANFEcomUniDANFE: TButton;
    GroupBox1: TGroupBox;
    btnConsultarDistribuicaoDFe: TToggleBox;
    btnEnviarEventoManifestacaoNFe: TToggleBox;
    GroupBox2: TGroupBox;
    btnEventoCancelamentoNFCe: TToggleBox;
    procedure btnConsultarDistribuicaoDFeChange(Sender: TObject);
    procedure btnConsultarSituacaoNFeClick(Sender: TObject);
    procedure btnConsultaStatusNFeClick(Sender: TObject);
    procedure btnEnviarEventoManifestacaoNFeChange(Sender: TObject);
    procedure btnEnviarNFCeSincronoClick(Sender: TObject);
    procedure btnEnviarNFCeSincronoDesserializacaoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDuplicidadeClick(Sender: TObject);
    procedure btnEventoCancelamentoNFCeClick(Sender: TObject);
    procedure btnEventoCancelamentoNFeClick(Sender: TObject);
    procedure btnImprimirDANFEcomUniDANFEClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);

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
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarEventoManifestacaoNFeChange(Sender: TObject);
var
  oServico: TEnviarEventoManifestacaoNFe;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFCeSincronoClick(Sender: TObject);
var
  oServico: TEnviarNFCeSincrono;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFCeSincronoDesserializacaoClick(Sender: TObject);
var
  oServico: TEnviarNFCeSincronoDesserializacao;
begin
  try
    oServico.Executar();
  finally
  end;
end;


procedure TfrmPrincipal.btnConsultarSituacaoNFeClick(Sender: TObject);
var
  oServico: TConsultarSituacaoNFe;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnConsultarDistribuicaoDFeChange(Sender: TObject);
var
  oServico: TConsultarDistribuicaoDFe;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoClick(Sender: TObject);
var
  oServico: TEnviarNFeSincrono;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoDesserializacao;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEnviarNFeSincronoDuplicidadeClick(Sender: TObject);
var
  oServico: TEnviarNFeSincronoDuplicidade;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEventoCancelamentoNFCeClick(Sender: TObject);
var
  oServico: TEventoCancelamentoNFCe;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnEventoCancelamentoNFeClick(Sender: TObject);
var
  oServico: TEventoCancelamentoNFe;
begin
  try
    oServico.Executar();
  finally
  end;
end;

procedure TfrmPrincipal.btnImprimirDANFEcomUniDANFEClick(Sender: TObject);
var
  oServico: TImprimirDANFEcomUniDANFE;
begin
  oServico.Executar();
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox1Click(Sender: TObject);
begin

end;


end.
