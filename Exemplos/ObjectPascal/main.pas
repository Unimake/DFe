unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComObj,
  ConsultarStatusNFe, EnviarNFeSincrono, EnviarNFeSincronoDesserializacao;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    btnConsultaStatusNFe: TButton;
    btnEnviarNFeSincrono: TButton;
    btnEnviarNFeSincronoDesserializacao: TButton;
    GroupBox1: TGroupBox;
    procedure btnConsultaStatusNFeClick(Sender: TObject);
    procedure btnEnviarNFeSincronoClick(Sender: TObject);
    procedure btnEnviarNFeSincronoDesserializacaoClick(Sender: TObject);
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

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin

end;

procedure TfrmPrincipal.GroupBox1Click(Sender: TObject);
begin

end;

end.
