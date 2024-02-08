/*
Bom dia, Wandrey, 

Consegui comunicação da Unimake.DFe (DLL) com o PHP.

Ontem tive uns 5 minutinhos de tempo e resolvi testar a comunicação usando o Laravel (Framework PHP) com a DLL. 

Lembrando que para utilização da DLL é indispensável habilitar a extensão PHP_COM_DOTNET nas configurações do PHP.

Vou te mandar o que fiz, lembrando que não aprofundei, mas fiz a consulta de status.
*/

public function configuracao()
{
	$configuracao = new COM("Unimake.Business.DFe.Servicos.Configuracao");
	$configuracao->TipoDFe = 0; //0=NFe
	$configuracao->CertificadoArquivo = 'LOCAL DO ARQUIVO DO CERTIFICADO (PFX)';
	$configuracao->CertificadoSenha = 'SENHA DO CERTIFICADO';

	return $configuracao;
}

public function consStatServ(){
	$consStatServ = new COM("Unimake.Business.DFe.Xml.NFe.ConsStatServ");
	$consStatServ->Versao = '4.00';
	$consStatServ->TpAmb = 2; //2=Homologação
	$consStatServ->CUF = 31; //31=Minas Gerais

	return $consStatServ;
}

public function statusServico(){
	$statusServico = new COM("Unimake.Business.DFe.Servicos.NFe.StatusServico");
	try {
		$statusServico->Executar($this->consStatServ(), $this->configuracao());
		//echo $statusServico->RetornoWSString;
		return $statusServico->Result->CStat . ' - ' . $statusServico->Result->XMotivo;
	}
	catch (Exception $e) {
		return $e->getMessage();
	}

}