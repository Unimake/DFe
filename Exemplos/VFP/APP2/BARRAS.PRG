***
*  
* Codigo de Barras Facil
*
* Estas funcoes para impressão de codigo de barras devem ser usadas com
* a fonte BARRAS2.TTF (Barras Biro).
*
* Esta fonte possui apenas as seguintes letras:
* f - espaco fino
* F - barra fina
* m - espaco medio
* M - barra media
* g - espaco grosso
* G - barra grossa
* s - espaco super grosso
* S - barra super grossa
* x - espaco extra grosso
* X - barra extra grossa
*
* As funcoes abaixo apenas geram sequencias de fFmMgGsSxX que vao
* formar as barras.
*
* Neste PRG seguem 5 exemplos de funcoes para geracao do codigo de
* barras 2 de 5 industrial, 2 de 5 intercalado, 3 de 9, EAN8 e EAN13.
*
* Para usar as funcoes abaixo basta chamar a funcao, passando o
* codigo desejado. Selecionar a fonte Barras Biro e o tamanho apropriado.
* Se a barra ficar muito estreita crie outra linha identica e cole
* embaixo.
* 
* Exemplos:
*
* Barra25("1234567890")    && Fonte Barras Biro, Tamanho 20
* Barra25i("1234567890")   && Fonte Barras Biro, Tamanho 20
* Barra39("*1234567890*")  && Fonte Barras Biro, Tamanho 20
*
*********************************************************************
* IMPORTANTE:                                                       *
* SEMPRE FACA MUITOS TESTES DE LEITURA COM VARIOS CODIGOS ANTES DE  *
* LIBERAR A IMPRESSAO DE BARRAS PARA SEUS CLIENTES.                 *
*********************************************************************
*
* Esta fonte (BARRAS2.TTF Barras Biro) e as funcoes Barra25, Barra25i,
* Barra39, BarraEAN8 e BarraEAN13 podem ser livremente usadas e
* alteradas.
*
* Claudio Gaspar Martins (cgmartins@biro.com.br)
* Biro Processamento de Dados Ltda
* Novo Hamburgo - RS - Brasil
* Dezembro, 1999
*
*
* Historico das revisoes (YYYY/MM/DD):
* 2001/02/28 - Corrigida tabela A nas funcoes EAN13 e EAN8
*
*


***
*
* Barra 2 de 5 Industrial
*
*

FUNCTION Barra25( tcNumero )

LOCAL lnT, lcRetorno, lnTamanho
LOCAL ARRAY laBarras(10)
lcRetorno  = "GfGfFf"      && Sequencia inicial
lnTamanho  = LEN(tcNumero)

*-- Sequencias fixas
laBarras[1] = "FfFfGfGfFf"
laBarras[2] = "GfFfFfFfGf"
laBarras[3] = "FfGfFfFfGf"
laBarras[4] = "GfGfFfFfFf"
laBarras[5] = "FfFfGfFfGf"
laBarras[6] = "GfFfGfFfFf"
laBarras[7] = "FfGfGfFfFf"
laBarras[8] = "FfFfFfGfGf"
laBarras[9] = "GfFfFfGfFf"
laBarras[10] = "FfGfFfGfFf"

FOR lnT = 1 TO lnTamanho
    lcRetorno = lcRetorno + laBarras[VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
NEXT

*-- Acrescenta sequencia de finalizacao
lcRetorno = lcRetorno + "GfFfGf"

RETURN lcRetorno

***
*
* Barra 2 de 5 Intercalado
*
*

FUNCTION Barra25i( tcNumero )

*-- Variaveis
LOCAL lnT, lnU, lcBarras, lcEspacos, lcRetorno, lnTamanho
LOCAL ARRAY laBarras(10)
lcRetorno = "FfFf"        && Sequencia inicial
lnTamanho = LEN(tcNumero)

*-- Sequencias
laBarras[1] = "00110"
laBarras[2] = "10001"
laBarras[3] = "01001"
laBarras[4] = "11000"
laBarras[5] = "00101"
laBarras[6] = "10100"
laBarras[7] = "01100"
laBarras[8] = "00011"
laBarras[9] = "10010"
laBarras[10] = "01010"

*-- Se quantidade de caracteres nao for par coloca 0 na frente
IF lnTamanho % 2 <> 0
	tcNumero = "0" + tcNumero
ENDIF

FOR lnT = 1 TO lnTamanho STEP 2

    *-- Pega os 0 e 1
    lcBarras  = laBarras[VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
    lcEspacos = laBarras[VAL(SUBSTR(tcNumero, lnT + 1, 1)) + 1]

    *-- Combima as barras e os espacos
    FOR lnU = 1 TO 5
        lcRetorno = lcRetorno + IIF(SUBSTR(lcBarras, lnU, 1) == "0", "F", "G")
        lcRetorno = lcRetorno + IIF(SUBSTR(lcEspacos, lnU, 1) == "0", "f", "g")
    NEXT

NEXT

*-- Acrescenta sequencia de finalizacao
lcRetorno = lcRetorno + "GfFf"

RETURN lcRetorno

***
*
* Barra 3 de 9
* 
* OBSERVACAO: Alguns leitores exigem o * como prefixo e sufixo do codigo
*
*

FUNCTION Barra39( tcNumero )

LOCAL lnT, lnPosicao, lcRetorno, lnTamanho, lcCaracteres
LOCAL ARRAY laBarras(44)
lcRetorno   = ""
lnTamanho   = LEN(tcNumero)
lcCaracteres = "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-.#*$/+%"

*-- Sequencias
laBarras[1] = "GfFgFfFfGf"
laBarras[2] = "FfGgFfFfGf"
laBarras[3] = "GfGgFfFfFf"
laBarras[4] = "FfFgGfFfGf"
laBarras[5] = "GfFgGfFfFf"
laBarras[6] = "FfGgGfFfFf"
laBarras[7] = "FfFgFfGfGf"
laBarras[8] = "GfFgFfGfFf"
laBarras[9] = "FfGgFfGfFf"
laBarras[10] = "FfFgGfGfFf"
laBarras[11] = "GfFfFgFfGf"
laBarras[12] = "FfGfFgFfGf"
laBarras[13] = "GfGfFgFfFf"
laBarras[14] = "FfFfGgFfGf"
laBarras[15] = "GfFfGgFfFf"
laBarras[16] = "FfGfGgFfFf"
laBarras[17] = "FfFfFgGfGf"
laBarras[18] = "GfFfFgGfFf"
laBarras[19] = "FfGfFgGfFf"
laBarras[20] = "FfFfGgGfFf"
laBarras[21] = "GfFfFfFgGf"
laBarras[22] = "FfGfFfFgGf"
laBarras[23] = "GfGfFfFgFf"
laBarras[24] = "FfFfGfFgGf"
laBarras[25] = "GfFfGfFgFf"
laBarras[26] = "FfGfGfFgFf"
laBarras[27] = "FfFfFfGgGf"
laBarras[28] = "GfFfFfGgFf"
laBarras[29] = "FfGfFfGgFf"
laBarras[30] = "FfFfGfGgFf"
laBarras[31] = "GgFfFfFfGf"
laBarras[32] = "FgGfFfFfGf"
laBarras[33] = "GgGfFfFfFf"
laBarras[34] = "FgFfGfFfGf"
laBarras[35] = "GgFfGfFfFf"
laBarras[36] = "FgGfGfFfFf"
laBarras[37] = "FgFfFfGfGf"
laBarras[38] = "GgFfFfGfFf"
laBarras[39] = "FgGfFfGfFf"
laBarras[40] = "FgFfGfGfFf"
laBarras[41] = "FgFgFgFfFf"
laBarras[42] = "FgFgFfFgFf"
laBarras[43] = "FgFfFgFgFf"
laBarras[44] = "FfFgFgFgFf"

FOR lnT = 1 TO lnTamanho

	lnPosicao = AT(SUBSTR(tcNumero, lnT, 1), lcCaracteres)
    IF lnPosicao > 0
        lcRetorno = lcRetorno + laBarras[lnPosicao]
    ENDIF

NEXT

RETURN lcRetorno

***
*
* Barras EAN13
*
*

FUNCTION BarraEAN13( tcNumero )

LOCAL lnT, lcRetorno, lcSequencia
LOCAL ARRAY laBarras(4, 10)
lcRetorno   = "FfF"

*-- Deve sempre ter 13 caracteres
IF LEN(tcNumero) <> 13
	RETURN ""
ENDIF

*-- Tabela A
laBarras[1,  1] = "fffFFfF"
laBarras[1,  2] = "ffFFffF"
laBarras[1,  3] = "ffFffFF"  && Corrigido em 20010228
laBarras[1,  4] = "fFFFFfF"
laBarras[1,  5] = "fFfffFF"
laBarras[1,  6] = "fFFfffF"
laBarras[1,  7] = "fFfFFFF"
laBarras[1,  8] = "fFFFfFF"
laBarras[1,  9] = "fFFfFFF"
laBarras[1, 10] = "fffFfFF"

*-- Tabela B
laBarras[2,  1] = "fFffFFF"
laBarras[2,  2] = "fFFffFF"
laBarras[2,  3] = "ffFFfFF"
laBarras[2,  4] = "fFffffF"
laBarras[2,  5] = "ffFFFfF"
laBarras[2,  6] = "fFFFffF"
laBarras[2,  7] = "ffffFfF"
laBarras[2,  8] = "ffFfffF"
laBarras[2,  9] = "fffFffF"
laBarras[2, 10] = "ffFfFFF"

*-- Tabela C
laBarras[3,  1] = "FFFffFf"
laBarras[3,  2] = "FFffFFf"
laBarras[3,  3] = "FFfFFff"
laBarras[3,  4] = "FffffFf"
laBarras[3,  5] = "FfFFFff"
laBarras[3,  6] = "FffFFFf"
laBarras[3,  7] = "FfFffff"
laBarras[3,  8] = "FfffFff"
laBarras[3,  9] = "FffFfff"
laBarras[3, 10] = "FFFfFff"

*-- Tabela para definir primeiro caracter
laBarras[4,  1] = "111111"
laBarras[4,  2] = "112122"
laBarras[4,  3] = "112212"
laBarras[4,  4] = "112221"
laBarras[4,  5] = "121122"
laBarras[4,  6] = "122112"
laBarras[4,  7] = "122211"
laBarras[4,  8] = "121212"
laBarras[4,  9] = "121221"
laBarras[4, 10] = "122121"

*-- Pega a sequencia de tabela a usar
lcSequencia = laBarras[4, VAL(SUBSTR(tcNumero, 1, 1)) + 1]

*-- Apura do 2' ao 7' caracter
FOR lnT = 2 TO 7
	lcRetorno = lcRetorno +;
		laBarras[VAL(SUBSTR(lcSequencia, lnT - 1, 1)),;
		VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
NEXT

*-- Coloca o meio
lcRetorno = lcRetorno + "fFfFf"

*-- Apura do 8' ao 13'
FOR lnT = 8 TO 13
	lcRetorno = lcRetorno +;
		laBarras[3, VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
NEXT

*-- Acrescenta sequencia de finalizacao
lcRetorno = lcRetorno + "FfF"

RETURN lcRetorno


***
*
* Barras EAN8
*
*

FUNCTION BarraEAN8( tcNumero )

LOCAL lnT, lcRetorno
LOCAL ARRAY laBarras(2, 10)
lcRetorno   = "FfF"

*-- Deve sempre ter 8 caracteres
IF LEN(tcNumero) <> 8
	RETURN ""
ENDIF

*-- Tabela A
laBarras[1,  1] = "fffFFfF"
laBarras[1,  2] = "ffFFffF"
laBarras[1,  3] = "ffFffFF"  && Corrigido em 20010228
laBarras[1,  4] = "fFFFFfF"
laBarras[1,  5] = "fFfffFF"
laBarras[1,  6] = "fFFfffF"
laBarras[1,  7] = "fFfFFFF"
laBarras[1,  8] = "fFFFfFF"
laBarras[1,  9] = "fFFfFFF"
laBarras[1, 10] = "fffFfFF"

*-- Tabela C
laBarras[2,  1] = "FFFffFf"
laBarras[2,  2] = "FFffFFf"
laBarras[2,  3] = "FFfFFff"
laBarras[2,  4] = "FffffFf"
laBarras[2,  5] = "FfFFFff"
laBarras[2,  6] = "FffFFFf"
laBarras[2,  7] = "FfFffff"
laBarras[2,  8] = "FfffFff"
laBarras[2,  9] = "FffFfff"
laBarras[2, 10] = "FFFfFff"

*-- Apura do 1' ao 4' caracter
FOR lnT = 1 TO 4
	lcRetorno = lcRetorno +;
		laBarras[1, VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
NEXT

*-- Coloca o meio
lcRetorno = lcRetorno + "fFfFf"

*-- Apura do 5' ao 8'
FOR lnT = 5 TO 8
	lcRetorno = lcRetorno +;
		laBarras[2, VAL(SUBSTR(tcNumero, lnT, 1)) + 1]
NEXT

*-- Acrescenta sequencia de finalizacao
lcRetorno = lcRetorno + "FfF"

RETURN lcRetorno

*-- Fim