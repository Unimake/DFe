do TEST_JMAIL01

do TEST_JMAIL02

RETURN
*
PROCEDURE TEST_JMAIL01

local m.server, m.subject, m.body, m.receipt_requested

m.server = "smtp.libero.it"

m.subject = "Test Email"

m.body = "A very short body ...Smile"

* if you need html email
*m.body = filetostr("save\html email.html")

m.receipt_requested = .t.

local m.sender, m.address, m.attachment
m.sender = "io_gianni@libero.it"
m.address = "io.gianni@virgilio.it"
m.attachment = "file01.zip"

m.success = MY_JMAIL(m.server, m.sender, m.address, m.subject, m.body, m.attachment, m.receipt_requested)

if m.success
	messagebox("Email successfully sent")
else
	messagebox("Jmail failed to send mail")
endif

RETURN
*
PROCEDURE TEST_JMAIL02

local m.server, m.subject, m.body, m.receipt_requested

m.server = "smtp.libero.it"

m.subject = "Test Email"

m.body = "A very short body ...Smile"

* if you need html email
*m.body = filetostr("save\html email.html")

m.receipt_requested = .t.

local array a_sender[1, 2]

a_sender[1, 1] = "io_gianni@libero.it"
a_sender[1, 2] = "Gianni"

local array a_address[2, 3]

a_address[1, 1] = "io.gianni@virgilio.it"
a_address[1, 2] = "Giannino Uno"
a_address[1, 3] = "TO"

a_address[2, 1] = "io_gianni@virgilio.it"
a_address[2, 2] = "Giannino Due"
a_address[2, 3] = "CC"

local array a_attachment[2]

a_attachment[1] = "FILE02.pdf"
a_attachment[2] = "FILE03.doc"

m.success = MY_JMAIL(m.server, @a_sender, @a_address, m.subject, m.body, @a_attachment, m.receipt_requested)

if m.success
	messagebox("Email successfully sent")
else
	messagebox("Jmail failed to send mail")
endif

RETURN
*
FUNCTION MY_JMAIL
parameters m.pp_server, m.pp_sender, m.pp_address, m.pp_subject, m.pp_body, m.pp_attachment, m.pp_receipt_requested

if ! file("JMAIL.DLL")
	messagebox("JMAIL NOT installed")
	RETURN .f.
endif

if ! empty(m.pp_server) and occurs("@", m.pp_server) > 1
	* "io_gianni@libero.it:mypass@smtp.libero.it"
	RETURN MY_JMAIL_AUTH()
endif

local s_MailMessage, llExecuted

if type("s_MailMessage") # "O" or isnull(s_MailMessage)
	local m.riprova, m.errore
	m.riprova = .t.
	m.errore = .f.

	do while m.riprova
		try
			s_MailMessage = newobject("JMail.SMTPMail")
			m.riprova = .f.
		catch
			try
				RUN REGSVR32.EXE /s JMAIL.DLL
			catch
				m.riprova = .f.
				m.errore = .t.
			endtry
		endtry
	enddo

	if m.errore
		messagebox("Cannot register JMAIL.DLL")
		RETURN .f.
	endif
endif

with s_MailMessage

	.Logging = .t.

	.ISOEncodeHeaders = .f.

	if ! empty(m.pp_server)
		.ServerAddress = m.pp_server
	endif

	if type("pp_sender[1]") = "C"
		external array pp_sender

		.Sender = pp_sender[1, 1]
		.SenderName = left(.Sender, at("@", .Sender) -1)
		if alen(pp_sender, 2) = 2
			.SenderName = pp_sender[1, 2]
		endif
	else
		.Sender = m.pp_sender
		.SenderName = left(.Sender, at("@", .Sender) -1)
	endif

	if ! empty(m.pp_subject)
		.Subject = alltrim(m.pp_subject)
	else
		* an empty subject generate an email without the subject field
		* and this cause OFFICE OUTLOOK to generate an error if you have asked for ReturnReceipt

		.Subject = " "
	endif

	.Body = alltrim(m.pp_body)

	.ReturnReceipt = m.pp_receipt_requested

	if m.pp_receipt_requested
		* added for OFFICE OUTLOOK that does not understand Return-Receipt-To

		.AddNativeHeader("Disposition-Notification-To", '"' + .SenderName + '" <' + .Sender + ">")
	endif

	.AddNativeHeader("X-Mailer", "Microsoft Outlook Express 6.00.2900.2670")
	.AddNativeHeader("X-MimeOLE", "Produced By Microsoft MimeOLE V6.00.2900.2670")

	if type("pp_address[1]") = "C"
		external array pp_address

		local lnRows, lnColumns, lnIndex, lcAddress, lcDisplayName, lcType
		lnRows = alen(pp_address, 1)
		lnColumns = alen(pp_address, 2)

		for lnIndex = 1 to lnRows
			if lnColumns = 0
				lcAddress = pp_address[lnIndex]
			else
				lcAddress = pp_address[lnIndex, 1]
			endif
			lcDisplayName = left(lcAddress, at("@", lcAddress) -1)
			lcType = "TO"
			if lnColumns >= 2
				lcDisplayName = pp_address[lnIndex, 2]
			endif
			if lnColumns >= 3
				lcType = pp_address[lnIndex, 3]
			endif
			do case
			case lcType == "TO"
				.AddRecipientEx(lcAddress, lcDisplayName)
			case lcType == "CC"
				.AddRecipientCC(lcAddress)
			case lcType == "BCC"
				.AddRecipientBCC(lcAddress)
			endcase
		endfor
	else
		.AddRecipientEx(m.pp_address, left(m.pp_address, at("@", m.pp_address) -1))
	endif

	if type("pp_attachment[1]") = "C"
		external array pp_attachment

		local lnRows, lnColumns, lnIndex, lcPathname, lcName
		lnRows = alen(pp_attachment, 1)
		lnColumns = alen(pp_attachment, 2)

		for lnIndex = 1 to lnRows
			if lnColumns = 0
				lcPathname = pp_attachment[lnIndex]
			else
				lcPathname = pp_attachment[lnIndex, 1]
			endif
			lcName = justfname(lcPathname)
			if lnColumns >= 2
				lcName = pp_attachment[lnIndex, 2]
			endif
			if file(lcPathname)
				.AddAttachment(lcPathname)
			endif
		endfor
	else
		if ! empty(m.pp_attachment) and file(m.pp_attachment)
			.AddAttachment(m.pp_attachment)
		endif
	endif

	.Silent = .t.

	llExecuted = .Execute()

	if ! llExecuted
		local m.msg
		m.msg = ;
			"Email NOT sent!" + chr(13) + chr(10) + chr(13) + chr(10) + ;
			iif(.ErrorCode # 0, ltrim(str(.ErrorCode)) + " " + .ErrorMessage + " " + .ErrorSource + "." + ;
			chr(13) + chr(10) + chr(13) + chr(10), "") + ;
			.Log
		messagebox(m.msg)
	endif

	if llExecuted
		.Close
	endif
endwith

RETURN llExecuted
*
FUNCTION MY_JMAIL_AUTH

local s_MailMessage, llExecuted

if type("s_MailMessage") # "O" or isnull(s_MailMessage)
	local m.riprova, m.errore
	m.riprova = .t.
	m.errore = .f.

	do while m.riprova
		try
			s_MailMessage = newobject("JMail.Message")
			m.riprova = .f.
		catch
			try
				RUN REGSVR32.EXE /s JMAIL.DLL
			catch
				m.riprova = .f.
				m.errore = .t.
			endtry
		endtry
	enddo

	if m.errore
		messagebox("Cannot register JMAIL.DLL")
		RETURN .f.
	endif
endif

local m.server, m.auth_name, m.auth_pw
m.server = m.pp_server
m.auth_name = ""
m.auth_pw = ""

if ! empty(m.server) and occurs("@", m.server) > 1
	* "io_gianni@libero.it:mypass@smtp.libero.it"
	m.auth_name = left(m.server, at(":", m.server) -1)
	m.server = substr(m.server, at(":", m.server) +1)
	m.auth_pw = left(m.server, at("@", m.server) -1)
	m.server = substr(m.server, at("@", m.server) +1)
endif

with s_MailMessage

	.Logging = .t.

	.ISOEncodeHeaders = .f.

	if type("pp_sender[1]") = "C"
		external array pp_sender

		.From = pp_sender[1, 1]
		.FromName = left(.From, at("@", .From) -1)
		if alen(pp_sender, 2) = 2
			.FromName = pp_sender[1, 2]
		endif
	else
		.From = m.pp_sender
		.FromName = left(.From, at("@", .From) -1)
	endif

	if ! empty(m.pp_subject)
		.Subject = alltrim(m.pp_subject)
	else
		* an empty subject generate an email without the subject field
		* and this cause OFFICE OUTLOOK to generate an error if you have asked for ReturnReceipt

		.Subject = " "
	endif

	.Body = alltrim(m.pp_body)

	.ReturnReceipt = m.pp_receipt_requested

	if m.pp_receipt_requested
		* added for OFFICE OUTLOOK that does not understand Return-Receipt-To

		.AddNativeHeader("Disposition-Notification-To", '"' + .FromName + '" <' + .From + ">")
	endif

	.AddNativeHeader("X-Mailer", "Microsoft Outlook Express 6.00.2900.2670")
	.AddNativeHeader("X-MimeOLE", "Produced By Microsoft MimeOLE V6.00.2900.2670")

	if type("pp_address[1]") = "C"
		external array pp_address

		local lnRows, lnColumns, lnIndex, lcAddress, lcDisplayName, lcType
		lnRows = alen(pp_address, 1)
		lnColumns = alen(pp_address, 2)

		for lnIndex = 1 to lnRows
			if lnColumns = 0
				lcAddress = pp_address[lnIndex]
			else
				lcAddress = pp_address[lnIndex, 1]
			endif
			lcDisplayName = left(lcAddress, at("@", lcAddress) -1)
			lcType = "TO"
			if lnColumns >= 2
				lcDisplayName = pp_address[lnIndex, 2]
			endif
			if lnColumns >= 3
				lcType = pp_address[lnIndex, 3]
			endif
			do case
			case lcType == "TO"
				.AddRecipient(lcAddress, lcDisplayName)
			case lcType == "CC"
				.AddRecipientCC(lcAddress)
			case lcType == "BCC"
				.AddRecipientBCC(lcAddress)
			endcase
		endfor
	else
		.AddRecipient(m.pp_address, left(m.pp_address, at("@", m.pp_address) -1))
	endif

	if type("pp_attachment[1]") = "C"
		external array pp_attachment

		local lnRows, lnColumns, lnIndex, lcPathname, lcName
		lnRows = alen(pp_attachment, 1)
		lnColumns = alen(pp_attachment, 2)

		for lnIndex = 1 to lnRows
			if lnColumns = 0
				lcPathname = pp_attachment[lnIndex]
			else
				lcPathname = pp_attachment[lnIndex, 1]
			endif
			lcName = justfname(lcPathname)
			if lnColumns >= 2
				lcName = pp_attachment[lnIndex, 2]
			endif
			if file(lcPathname)
				.AddAttachment(lcPathname)
			endif
		endfor
	else
		if ! empty(m.pp_attachment) and file(m.pp_attachment)
			.AddAttachment(m.pp_attachment)
		endif
	endif

	.Silent = .t.

	if ! empty(m.auth_name) and ! empty(m.auth_pw)
		.MailServerUserName = m.auth_name
		.MailServerPassWord = m.auth_pw
	endif

	if ! empty(m.server)
		llExecuted = .Send(m.server)
	else
		llExecuted = .Send()
	endif

	if ! llExecuted
		local m.msg
		m.msg = ;
			"Email NOT sent!" + chr(13) + chr(10) + chr(13) + chr(10) + ;
			iif(.ErrorCode # 0, ltrim(str(.ErrorCode)) + " " + .ErrorMessage + " " + .ErrorSource + "." + ;
			chr(13) + chr(10) + chr(13) + chr(10), "") + ;
			.Log
		messagebox(m.msg)
	endif

	if llExecuted
		.Close
	endif
endwith

RETURN llExecuted
*