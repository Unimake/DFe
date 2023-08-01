'----------------------------------------------------
'Gerar a chave do DFe (NFe, CTe, CTeOS, MDFe ou NFCe)
'----------------------------------------------------
Dim chaveDFe
Dim oXMLUtilityInterop

Set oXMLUtilityInterop = CreateObject("Unimake.Business.DFe.Utility.XMLUtilityInterop")

'Gerar a chave passando o código numérico randômico
'
'Parâmetros:
'  1 - cUF = Código da UF do emitente (Conteúdo da tag <cUF>)
'  2 - dhEmi = Data de emissão da Nota (Conteúdo da tag <dhEmi>)
'  3 - cnpjcpf = CNPJ ou CPF do emissor do documento fiscal eletrônico (conteúdo da tag <emit><CNPJ>)
'  4 - mod = Código do modelo do documento fiscal eletrônico (conteúdo da tag <mod>)
'  5 - serie = Número da série do documento fiscal eletrônico (conteúdo da tag <serie>)
'  6 - nNF = Número do documento fiscal eletrônico (conteúdo da tag <nNF>)
'  7 - tpEmis = Tipo de emissão (conteúdo da tag <tpEmis>)
'  8 - cNF = Código numérico randômico (conteúdo da tag <cNF>) - Deixe em branco ("") o conteúdo que a DLL gera esta numeração automaticamente, caso deseje.
chaveDFe = oXMLUtilityInterop.MontarChaveDFe(41, Now, "06117473000150", 55, 1, 236, 1, "12345678")

MsgBox chaveDFe

'Gerar a chave deixando a DLL gerar o código numérico randômico
'
'Parâmetros:
'  1 - cUF = Código da UF do emitente (Conteúdo da tag <cUF>)
'  2 - dhEmi = Data de emissão da Nota (Conteúdo da tag <dhEmi>)
'  3 - cnpjcpf = CNPJ ou CPF do emissor do documento fiscal eletrônico (conteúdo da tag <emit><CNPJ>)
'  4 - mod = Código do modelo do documento fiscal eletrônico (conteúdo da tag <mod>)
'  5 - serie = Número da série do documento fiscal eletrônico (conteúdo da tag <serie>)
'  6 - nNF = Número do documento fiscal eletrônico (conteúdo da tag <nNF>)
'  7 - tpEmis = Tipo de emissão (conteúdo da tag <tpEmis>)
'  8 - cNF = Código numérico randômico (conteúdo da tag <cNF>) - Deixe em branco ("") o conteúdo que a DLL gera esta numeração automaticamente, caso deseje.
chaveDFe = oXMLUtilityInterop.MontarChaveDFe(41, Now, "06117473000150", 55, 1, 236, 1, "")

MsgBox chaveDFe