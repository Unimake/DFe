validarSchema = CreateObject("Unimake.Business.DFe.ValidarSchema")
schema = "NFe.nfe_v4.00.xsd"
arquivo = FULLPATH(CURDIR()) + 'Envio\NFe.xml' 

validarSchema.Validar(arquivo, schema ,"")

IF validarSchema.Success
    MESSAGEBOX("XML validado com sucesso.")
ELSE 
    MESSAGEBOX("Code: " + validarSchema.errorCode  + "Message: " + validarSchema.ErrorMessage)
ENDIF 

RELEASE validarSchema 