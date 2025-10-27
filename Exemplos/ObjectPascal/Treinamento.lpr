program Treinamento;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, ConsultarStatusNFe, EnviarNFeSincrono,
  EnviarNFeSincronoDesserializacao, ImprimirDANFEcomUniDANFE,
  CertificadoDigital, ConsultarDistribuicaoDFe, ConsultarSituacaoNFe,
  DesserializarRetornoConsultaDFe, EnviarEventoManifestacaoNFe,
  EnviarNFCeSincrono, EnviarNFCeSincronoDesserializacao, BETHAGerarNFSe,
  BETHACancelarNFSe, BETHAEnviarLoteRPSSincrono, BETHAConsultarNFSeRPS,
  BETHAConsultarLoteRPS, NACIONALGerarNFSe, NACIONALCancelarNFSe,
  NACIONALConsultarNFSe, NACIONALConsultarNFSePDF, NACIONALConsultarNFSeRPS,
  EventoCCENFe, InutilizacaoNumeroNFe, ConsultarStatusNFCom,
  EnviarNFComSincrono, EnviarMDFeSincrono, EnviarEventoCancelamentoMDFe,
  EnviarEventoEncerramentoMDFe, ConsultarStatusMDFe, ConsultarSituacaoMDFe,
  ConsultarMDFeNaoEncerrada, EventoPagamentoMDFe,
  EnviarEventoAlteracaoPagamentoMDFe
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.

