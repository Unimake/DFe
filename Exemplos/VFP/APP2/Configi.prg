SET READBORDER ON
SET TALK OFF			&& Elimina a resposta dos comandos para o Vídeo
SET NOTIFY OFF
SET CONSOLE OFF
SET DATE TO DMY			&& Formato de Data para Dia/Mes/Ano
SET POINT TO ','		&& Separador Decimal
SET SEPARATOR TO '.'	&& Separador do Milhar
** Configurações de Rede (MultiUsuário)
SET EXCLUSIVE OFF		&& Permite o compartilhamento dos arquivos de dados
SET REPROCESS TO 1 		&& Quantas tentativas para bloquear um registro
SET REFRESH TO 5		&& Tempo de atualização dos dados no Browse
SET DELETED ON			&& Não apresenta os registros marcados para exclusão
SET CENTURY ON			&& Mostra o ano com 4 dígitos
SET CENTURY TO 19 ROLLOVER 10	&& 05 ou maior é entendido como 19xx. De 0 a 4 como 20xx
SET CURRENCY TO 'R$'	&& Símbolo monetário
SET SAFETY OFF			&& Não avisa para sobreescrever um arquivo

ON KEY LABEL ALT+1 ACTIVATE WINDOW CALCULATOR
ON KEY LABEL ALT+2 ACTIVATE WINDOW CALENDAR