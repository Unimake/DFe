# Uso da NFeABI na DLL Unimake.DFe

A versão 1.00 oferece os serviços de consulta de status e autorização síncrona somente no ambiente de homologação. Produção permanece sem endpoint publicado e falha fechada.

## Certificado A1

Carregue o certificado diretamente do arquivo PFX e mantenha caminho e senha fora do código-fonte:

```csharp
var certificado = new Unimake.Business.Security.CertificadoDigital()
    .CarregarCertificadoDigitalA1(caminhoPfx, senhaPfx);
```

## Consultar o status

```csharp
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;

var consulta = new ConsStatServNFeABI
{
    Versao = "1.00",
    TpAmb = TipoAmbiente.Homologacao,
    CUF = UFBrasil.PR,
    XServ = "STATUS"
};

var configuracao = new Configuracao
{
    TipoDFe = TipoDFe.NFeABI,
    TipoEmissao = TipoEmissao.Normal,
    CodigoUF = (int)UFBrasil.PR,
    CertificadoDigital = certificado
};

var servico = new Unimake.Business.DFe.Servicos.NFeABI.StatusServico(consulta, configuracao);
servico.Executar();

RetConsStatServNFeABI retorno = servico.Result;
```

## Autorizar uma NFeABI

```csharp
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;

var documento = new NFeABI().LoadFromFile(caminhoXml);
documento.Signature = null;
documento.InfNFeABI.Ide.TpAmb = TipoAmbiente.Homologacao;

var configuracao = new Configuracao
{
    TipoDFe = TipoDFe.NFeABI,
    TipoEmissao = TipoEmissao.Normal,
    CodigoUF = (int)documento.InfNFeABI.Ide.CUF,
    CertificadoDigital = certificado
};

var servico = new Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc(documento, configuracao);
servico.Executar();

RetNFeABI retorno = servico.Result;
```

Ao alterar qualquer conteúdo de um XML previamente assinado, descarte a assinatura antiga como no exemplo. O documento é assinado novamente e validado pelo pipeline normal da DLL. O sucesso técnico do transporte não significa autorização fiscal: avalie `retorno.CStat` e `retorno.XMotivo`.

## Serviços ainda indisponíveis

- Produção.
- Consulta de protocolo remota.
- Recepção remota de eventos.
- Autorização ZIP.

Não reutilize os endpoints de homologação como produção e não presuma URLs para serviços ainda não publicados.
