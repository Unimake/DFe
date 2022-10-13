* CDO2000.prg
 
#DEFINE cdoSendPassword "http://schemas.microsoft.com/cdo/configuration/sendpassword"
#DEFINE cdoSendUserName "http://schemas.microsoft.com/cdo/configuration/sendusername"
#DEFINE cdoSendUsingMethod "http://schemas.microsoft.com/cdo/configuration/sendusing"
#DEFINE cdoSMTPAuthenticate "http://schemas.microsoft.com/cdo/configuration/smtpauthenticate"
#DEFINE cdoSMTPConnectionTimeout "http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout"
#DEFINE cdoSMTPServer "http://schemas.microsoft.com/cdo/configuration/smtpserver"
#DEFINE cdoSMTPServerPort "http://schemas.microsoft.com/cdo/configuration/smtpserverport"
#DEFINE cdoSMTPUseSSL "http://schemas.microsoft.com/cdo/configuration/smtpusessl"
#DEFINE cdoURLGetLatestVersion "http://schemas.microsoft.com/cdo/configuration/urlgetlatestversion"
#DEFINE cdoAnonymous 0	&& Perform no authentication (anonymous)
#DEFINE cdoBasic 1	&& Use the basic (clear text) authentication mechanism.
#DEFINE cdoSendUsingPort 2	&& Send the message using the SMTP protocol over the network.
#DEFINE cdoXMailer "urn:schemas:mailheader:x-mailer"
 
DEFINE CLASS cdo2000 AS Custom
 
	PROTECTED aErrors[1], nErrorCount, oMsg, oCfg, cXMailer
 
	nErrorCount = 0
 
	* Message attributes
	oMsg = Null
 
	cFrom = ""
	cReplyTo = ""
	cTo = ""
	cCC = ""
	cBCC = ""
	cAttachment = ""
 
	cSubject = ""
	cHtmlBody = ""
	cTextBody = ""
	cHtmlBodyUrl = ""
 
	cCharset = ""
 
	* Priority: Normal, High, Low or empty value (Default)
	cPriority = ""
 
	* Configuration object fields values
	oCfg = Null
	cServer = ""
	nServerPort = 25
	* Use SSL connection
	lUseSSL = .F.
	nConnectionTimeout = 30			&& Default 30 sec's
	nAuthenticate = cdoAnonymous
	cUserName = ""
	cPassword = ""
	* Do not use cache for cHtmlBodyUrl
	lURLGetLatestVersion = .T.
 
	* Optional. Creates your own X-MAILER field in the header
	cXMailer = "VFP CDO 2000 mailer Ver 1.1.100 2010"
 
	PROTECTED PROCEDURE Init
		This.ClearErrors()
	ENDPROC
 
	* Send message
	PROCEDURE Send
 
		IF This.GetErrorCount() > 0
			RETURN This.GetErrorCount()
		ENDIF
 
		WITH This
			.ClearErrors()
			.oCfg = CREATEOBJECT("CDO.Configuration")
			.oMsg = CREATEOBJECT("CDO.Message")
			.oMsg.Configuration = This.oCfg
		ENDWITH
 
		* Fill message attributes
		LOCAL lnind, laList[1], loHeader, laDummy[1], lcMailHeader
 
		IF This.SetConfiguration() > 0
			RETURN This.GetErrorCount()
		ENDIF
 
		IF EMPTY(This.cFrom)
			This.AddError("ERROR : From is empty.")
		ENDIF
		IF EMPTY(This.cSubject)
			This.AddError("ERROR : Subject is empty.")
		ENDIF
 
		IF EMPTY(This.cTo) AND EMPTY(This.cCC) AND EMPTY(This.cBCC)
			This.AddError("ERROR : To, CC and BCC are all empty.")
		ENDIF
 
		IF This.GetErrorCount() > 0
			RETURN This.GetErrorCount()
		ENDIF
 
		This.SetHeader()
 
		WITH This.oMsg
 
			.From     = This.cFrom
			.ReplyTo  = This.cReplyTo
 
			.To       = This.cTo
			.CC       = This.cCC
			.BCC      = This.cBCC
			.Subject  = This.cSubject
 
			* Create HTML body from external HTML (file, URL)
			IF NOT EMPTY(This.cHtmlBodyUrl)
				.CreateMHTMLBody(This.cHtmlBodyUrl)
			ENDIF
 
			* Send HTML body. Creates TextBody as well
			IF NOT EMPTY(This.cHtmlBody)
				.HtmlBody = This.cHtmlBody
			ENDIF
 
			* Send Text body. Could be different from HtmlBody, if any
			IF NOT EMPTY(This.cTextBody)
				.TextBody = This.cTextBody
			ENDIF
 
			IF NOT EMPTY(This.cCharset)
				IF NOT EMPTY(.HtmlBody)
					.HtmlBodyPart.Charset = This.cCharset
				ENDIF
 
				IF NOT EMPTY(.TextBody)
					.TextBodyPart.Charset = This.cCharset
				ENDIF
			ENDIF
 
			* Process attachments
			IF NOT EMPTY(This.cAttachment)
				* Accepts comma or semicolon
				* VFP 7.0 and later
				*FOR lnind=1 TO ALINES(laList, This.cAttachment, [,], [;])
				* VFP 6.0 and later compatible
				FOR lnind=1 TO ALINES(laList, CHRTRAN(This.cAttachment, [,;], CHR(13) + CHR(13)))
					lcAttachment = ALLTRIM(laList[lnind])
					* Ignore empty values
					IF EMPTY(laList[lnind])
						LOOP
					ENDIF
 
					* Make sure that attachment exists
					IF ADIR(laDummy, lcAttachment) = 0
						This.AddError("ERROR: Attacment not Found - " + lcAttachment)
					ELSE
						* The full path is required.
						IF 	UPPER(lcAttachment) <> UPPER(FULLPATH(lcAttachment))
							lcAttachment = FULLPATH(lcAttachment)
						ENDIF
						.AddAttachment(lcAttachment)
					ENDIF
				ENDFOR
			ENDIF
 
			IF NOT EMPTY(This.cCharset)
				.BodyPart.Charset = This.cCharset
			ENDIF
 
			* Priority
			IF NOT EMPTY(This.cPriority)
				lcMailHeader = "urn:schemas:mailheader:"
				.Fields(lcMailHeader + "Priority")   = LOWER(This.cPriority)
				.Fields(lcMailHeader + "Importance") = LOWER(This.cPriority)
				DO CASE
				CASE This.cPriority = "High"
					.Fields(lcMailHeader + "X-Priority") = 1 && 5=Low, 3=Normal, 1=High
				CASE This.cPriority = "Normal"
					.Fields(lcMailHeader + "X-Priority") = 3 && 5=Low, 3=Normal, 1=High
				CASE This.cPriority = "Low"
					.Fields(lcMailHeader + "X-Priority") = 5 && 5=Low, 3=Normal, 1=High
				ENDCASE
				.Fields.Update()
			ENDIF
		ENDWITH
 
		IF This.GetErrorCount() > 0
			RETURN This.GetErrorCount()
		ENDIF
 
		This.oMsg.Send()
 
		RETURN This.GetErrorCount()
 
	ENDPROC
 
	* Clear errors collection
	PROCEDURE ClearErrors()
		This.nErrorCount = 0
		DIMENSION This.aErrors[1]
		This.aErrors[1] = Null
		RETURN This.nErrorCount
	ENDPROC
 
	* Return # of errors in the error collection
	PROCEDURE GetErrorCount
		RETURN This.nErrorCount
	ENDPROC
 
	* Return error by index
	PROCEDURE GetError
		LPARAMETERS tnErrorno
		IF	tnErrorno <= This.GetErrorCount()
			RETURN This.aErrors[tnErrorno]
		ELSE
			RETURN Null
		ENDIF
	ENDPROC
 
	* Populate configuration object
	PROTECTED PROCEDURE SetConfiguration
 
		* Validate supplied configuration values
		IF EMPTY(This.cServer)
			This.AddError("ERROR: SMTP Server isn't specified.")
		ENDIF
		IF NOT INLIST(This.nAuthenticate, cdoAnonymous, cdoBasic)
			This.AddError("ERROR: Invalid Authentication protocol ")
		ENDIF
		IF This.nAuthenticate = cdoBasic ;
				AND (EMPTY(This.cUserName) OR EMPTY(This.cPassword))
			This.AddError("ERROR: User name/Password is required for basic authentication")
		ENDIF
 
		IF 	This.GetErrorCount() > 0
			RETURN This.GetErrorCount()
		ENDIF
 
		WITH This.oCfg.Fields
 
			* Send using SMTP server
			.Item(cdoSendUsingMethod) = cdoSendUsingPort
			.Item(cdoSMTPServer) = This.cServer
			.Item(cdoSMTPServerPort) = This.nServerPort
			.Item(cdoSMTPConnectionTimeout) = This.nConnectionTimeout
 
			.Item(cdoSMTPAuthenticate) = This.nAuthenticate
			IF This.nAuthenticate = cdoBasic
				.Item(cdoSendUserName) = This.cUserName
				.Item(cdoSendPassword) = This.cPassword
			ENDIF
			.Item(cdoURLGetLatestVersion) = This.lURLGetLatestVersion
			.Item(cdoSMTPUseSSL) = This.lUseSSL
 
			.Update()
		ENDWITH
 
		RETURN This.GetErrorCount()
 
	ENDPROC
 
	*----------------------------------------------------
	* Add message to the error collection
	PROTECTED PROCEDURE AddError
		LPARAMETERS tcErrorMsg
		This.nErrorCount = This.nErrorCount + 1
		DIMENSION This.aErrors[This.nErrorCount]
		This.aErrors[This.nErrorCount] = tcErrorMsg
		RETURN This.nErrorCount
	ENDPROC
 
	*----------------------------------------------------
	* Format an error message and add to the error collection
	PROTECTED PROCEDURE AddOneError
		LPARAMETERS tcPrefix, tnError, tcMethod, tnLine
		LOCAL lcErrorMsg, laList[1]
		IF INLIST(tnError, 1427,1429)
			AERROR(laList)
			lcErrorMsg = TRANSFORM(laList[7], "@0") + "  " + laList[3]
		ELSE
			lcErrorMsg = MESSAGE()
		ENDIF
		This.AddError(tcPrefix + ":" + TRANSFORM(tnError) + " # " + ;
			tcMethod + " # " + TRANSFORM(tnLine) + " # " + lcErrorMsg)
		RETURN This.nErrorCount
	ENDPROC
 
	*----------------------------------------------------
	* Simple Error handler. Adds VFP error to the objects error collection
	PROTECTED PROCEDURE Error
		LPARAMETERS tnError, tcMethod, tnLine
		This.AddOneError("ERROR: ", tnError, tcMethod, tnLine )
		RETURN This.nErrorCount
	ENDPROC
 
	*-------------------------------------------------------
	* Set mail header fields, if necessary. For now sets X-MAILER, if specified
	PROTECTED PROCEDURE SetHeader
		LOCAL loHeader
		IF NOT EMPTY(This.cXMailer)
			loHeader = This.oMsg.Fields
			WITH loHeader
				.Item(cdoXMailer) =  This.cXMailer
				.Update()
			ENDWITH
		ENDIF
	ENDPROC
 
	*----------------------------------------------------
	*
	PROTECTED PROCEDURE cPriority_assign(tvVal)
		* Check for incorrect values
		IF INLIST("~" + PROPER(tvVal) + "~", "~High~", "~Normal~", "~Low~") OR EMPTY(tvVal)
			This.cPriority = PROPER(ALLTRIM(tvVal))
		ELSE
			This.AddError("ERROR: Invalid value for cPriority property.")	
		ENDIF
	ENDPROC
 
ENDDEFINE



