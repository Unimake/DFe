 Action = MsgBox("Location is not on file."+chr(13)+"Do you wish to add it?", 4)
 Action = MsgBox("Are you sure you want to quit?", "\!\<Yes;\?\<Maybe", 5)
MSGBOX("TESTE",5)
*---------------------------------------------------------------------
procedure MSGBOX
parameters Msg, Buttons, Wtitle, Tips, Cor1, Cor2, Cor3
* VFP version
* Parameters:  Msg      Multi-line display string
*              Buttons    0 = Ok           (default if omitted)
*                         1 = Ok, Cancel
*                         2 = Abort, Retry, Ignore
*                         3 = Yes, No, Cancel
*                         4 = Yes, No
*                         5 = Retry, Cancel
*                       optional: string with custom buttons
*              Wtitle   optional: window title
*              Tips     optional: string with custom tips for the buttons
* Returns action number

* Example usage:
* Action = MsgBox("Location is not on file.+chr(13)+"Do you wish to add it?", 4)
* Action = MsgBox("Are you sure you want to quit?", "\!\<Yes;\?\<Maybe", 5)

* 10/18/01 - Get rid of DOS window, use a screen form for custom buttons. DC
*        NB: Custom buttons aren't used often, so the auto-sizing has
*             not been finely tuned - not worth the expense.  It works!
*            Delete color scheme and default action parameters.
* 02/15/01 - "Local all" doesn't work, fix local declarations
* 01/10/01 - De-select current dbf to avoid conflicts with field names
* 12/13/00 - Add CR to force Windows wrapping at a reasonable width.
*            Delete prevision for centered lines delimited by ';'.
*            Multiple lines now must be separated by chr(13).
* 12/23/99 - VFP: use system MessageBox if no special buttons.
* 11/08/95 - provide for automatic wordwrap if no ';' in Msg
* 11/95 - Windows-compatible button sets copied from FoxTools by D.Covill
*         Reference FoxPro Developers Journal, November 1995, Page 9
* Original "ALERT" by Steve Ramsower
*----------------------------------------
set talk off

local OldSel
OldSel = select()		&& 01/10/01
select 0				&& so we don't pick up data fields by mistake

*-- Msg is the only required parameter
if parameters() < 5
	Cor1 = -1
endif
if parameters() < 4
	Tips = ''
endif
if parameters() < 3
	Wtitle = ''
endif
if parameters() < 2
	Buttons = 0
endif
if empty(Wtitle)
	Wtitle = 'Note:'
endif

if vartype(Buttons) = 'N'
	*--We can use the Windows messagebox
	local iButtons, iResult, cTitle
	*-- if just an Ok, then use Info icon, otherwise use Question
	iButtons = iif(Buttons = 0, 64, Buttons + 32)
	cTitle = Wtitle
	*-- Fix the Msg so it doesn't come out as one long line!
	*-- (Windows doesn't wrap it until it gets about 100 chars long)
	iResult = MessageBox(MsgBox2(Msg), iButtons, cTitle)
	*-- now map the result codes back to our (positional) system
	do case
	case iResult = 0          && Escape
		*-- the same
	case Buttons = 1         && Ok=1, Cancel=2
		*-- leave it alone
	case Buttons = 2         && Abort=3, Retry=4, Ignore=5
		iResult = iResult - 2
	case Buttons = 3         && Yes=6, No=7, Cancel=2
		do case
		case iResult = 6
			iResult = 1
		case iResult = 7
			iResult = 2
		case iResult = 2
			iResult = 3
		endcase
	case Buttons = 4         && Yes=6, No=7
		iResult = iResult - 5
	case Buttons = 5         && Retry=4, Cancel=2
		iResult = iif(iResult=4, 1, 2)
	endcase
	select (OldSel)			&& 01/10/01
	return m.iResult
endif

*-- Custom buttons, have to create our own messagebox
*-- Break message into lines
local MaxLen, NumLines, MsgArray, Center
m.MaxLen = 0                 && maximum line length
OldMemo = set("memowidth")
	*-- calculate a reasonable breakdown of lines
	set memowidth to 50
	NumLines = memlines(Msg)
	if NumLines > 5
		set memowidth to 60
		NumLines = memlines(Msg)
	endif
	if NumLines > 10
		set memowidth to 70
		NumLines = memlines(Msg)
	endif
	if NumLines > 20
		set memowidth to 80
		NumLines = memlines(Msg)
	endif
	for I = 1 to NumLines
		MaxLen = max(MaxLen, len(mline(Msg, I)))
	endfor
*-- now stuff CR on end of each line
T2 = ''
for L = 1 to memlines(Msg)
	T2 = T2 + mline(Msg, L) + chr(13)
endfor
Msg = left(T2, len(T2)-1)   && strip off last chr(13)
*-- and reset the memowidth
set memowidth to (OldMemo)
Center = iif(NumLines = 1, .T., .F.)

*-- Create the form and set the editbox size
oBox = createobject("frmMessageBox")
with oBox
	*-- Size the form and the edit box
	.edtMessage.Width = m.MaxLen * 8
	.edtMessage.Height = m.NumLines * 18
	.Width    = .edtMessage.Width  + 35
	.Height   = .edtMessage.Height + 80
	.Caption  = Wtitle
	.cMessage = Msg               && the message text
	IF Cor1 >= 0
		.backcolor = RGB(Cor1, Cor2, Cor3)
	ENDIF
endwith

*-- Parse buttons and calculate longest
if empty(Buttons)
	NumBtns = 1
	Buttons = "OK"
else
	NumBtns  = occurs(';',Buttons) + 1    && number of buttons
ENDIF

*-- Separa os ToolTips dos botões
dimension tButtons[m.NumBtns]
Remain = Tips
MaxWidth = 0
for I = 1 to m.NumBtns
	Break = at(';', m.Remain)
	if m.Break > 0
		tButtons[I] = left(m.Remain, m.Break - 1)
		m.Remain = substr(m.Remain, m.Break + 1)
	ELSE
		tButtons[I] = m.Remain
		IF LEN(alltrim(m.Remain)) > 0
			m.Remain = ""
		ENDIF		
   endif
endfor

*-- Separa os Captions dos botões
dimension aButtons[m.NumBtns]
Remain = Buttons
MaxWidth = 0
for I = 1 to m.NumBtns
	Break = at(';', m.Remain)
	if m.Break > 0
		aButtons[I] = left(m.Remain, m.Break - 1)
		m.Remain = substr(m.Remain, m.Break + 1)
	else
		aButtons[I] = m.Remain
	endif
	*-- get length of buttons without escape chars
	Temp = strtran(aButtons[I], '\', '')
	Temp = strtran(Temp, '<', '')
	MaxWidth = max(MaxWidth, len(Temp))
endfor
BtnWidth = MaxWidth * 9  + 10     && Pixels
BtnWidth = max(BtnWidth, 100)      && Don't make 'em too small
Gap = 10
BtnTotal = BtnWidth * NumBtns + Gap * (NumBtns-1)
if BtnTotal > oBox.Width
	with oBox
		.Width = BtnTotal + 20
		.edtMessage.Width = BtnTotal - 24
	endwith
endif

*-- Now put the buttons on the form
*-- And delete the ones not used
with oBox
	*-- Calculate the gap between buttons
	Bleft = (.Width - BtnTotal) / 2
	for I = 1 to NumBtns
		CmdName = "Command"+str(I,1)
		*-- Pass the button number as a parameter
		*-- On click, the button will set Action to this number
		with oBox.&CmdName
			.Top = oBox.Height - 40
			.Left = Bleft
			.Width = BtnWidth
			.Caption = aButtons[I]
			.ToolTipText = tButtons[I]
			.Visible = .T.
		endwith
		Bleft = Bleft + BtnWidth + Gap
	endfor
	*-- delete the ones we didn't use
	for J = NumBtns+1 to 6
		CmdName = "Command"+str(J,1)
		.RemoveObject(CmdName)
	ENDFOR
	
	IF TYPE('kcriatimer') <> 'N'
		oBox.timer1.enabled = .F.
	ELSE
		oBox.timer1.enabled = .T.
		obox.botaot1 = kcriatimer
	ENDIF
ENDWITH

*-- Display the window
Action = 0                   && default
oBox.AutoCenter = .T.
oBox.Show()                  && Sets Action on exit

select (m.OldSel)
return m.Action

*---------------------------------------------------------------------
DEFINE CLASS frmMessageBox AS form
	borderStyle = 1
	Top = 0
	Left = 0
	Height = 300
	Width = 400
	DoCreate = .T.
	Caption = "Note:"
	WindowType = 1            && modal
	MaxButton  = .F.
	MinButton  = .F.
	Closable   = .F.
	ControlBox = .F.
	ShowTips   = .T.
	cMessage   = ' '            && the message itself
	botaot1    = 1
	Name = "frmMessageBox"

	ADD OBJECT icomsg AS image WITH ;
		BackStyle = 0, ;       && transparent
		picture = "ico32.bmp", ;
		Height = 34, ;
		Left = 10, ;           && leave margin at left
		Top = 12, ;
		Width = 41, ;
		Name = "icoMessage"

	ADD OBJECT edtmessage AS editbox WITH ;
		BackStyle = 0, ;       && transparent
		BorderStyle = 0, ;     && none
		ControlSource = "thisform.cMessage", ;
		Height = 192, ;
		Left = 65, ;           && leave margin at left
		FontSize = 10, ;
		enabled  = .F., ;
		ScrollBars = 0, ;      && none
		TabIndex = 6, ;
		disabledbackcolor = RGB(255,255,255), ;
		disabledforecolor = RGB(0,0,0), ;
		Top = 12, ;
		Width = 348, ;
		Name = "edtMessage"

   *-- Up to 6 command buttons, we'll remove the ones not used
   *-- (Code turns out to be easier than adding new ones.)
	ADD OBJECT command1 AS commandbutton WITH ;
		Top = 216, ;
		Left = 12, ;
		Height = 28, ;
		Width = 60, ;
		FontSize = 10, ;
		Caption = "Cmd1", ;    && will be set by caller
		TabIndex = 1, ;
		Name = "Command1"

	ADD OBJECT command2 AS commandbutton WITH ;
		Top = 216, ;
		Left = 84, ;
		Height = 28, ;
		Width = 60, ;
		FontSize = 10, ;
		Caption = "cmd1", ;
		TabIndex = 2, ;
		Name = "Command2"

	ADD OBJECT command3 AS commandbutton WITH ;
		Top = 216, ;
		Left = 156, ;
		Height = 28, ;
		Width = 60, ;
		FontSize = 10, ;
		Caption = "cmd3", ;
		TabIndex = 3, ;
		Name = "Command3"

	ADD OBJECT command4 AS commandbutton WITH ;
		Top = 216, ;
		Left = 228, ;
		Height = 28, ;
		Width = 60, ;
		FontSize = 10, ;
		Caption = "cmd4", ;
		TabIndex = 4, ;
		Name = "Command4"

	ADD OBJECT command5 AS commandbutton WITH ;
		Top = 216, ;
		Left = 300, ;
		Height = 28, ;
		Width = 60, ;
		Caption = "cmd5", ;
		TabIndex = 5, ;
		Name = "Command5"

	ADD OBJECT command6 AS commandbutton WITH ;
		Top = 216, ;
		Left = 372, ;
		Height = 28, ;
		Width = 60, ;
		Caption = "cmd6", ;
		TabIndex = 6, ;
		Name = "Command6"

	ADD OBJECT timer1 AS timer WITH ;
		interval = 10000, ;
		Name = "timer1"

   *-- Buttons set the Action memvar, then exit
	PROCEDURE command1.Click
		m.Action = 1
		thisform.Release()
	ENDPROC

	PROCEDURE command2.Click
		m.Action = 2
		thisform.Release()
	ENDPROC

	PROCEDURE command3.Click
		m.Action = 3
		thisform.Release()
	ENDPROC

	PROCEDURE command4.Click
		m.Action = 4
		thisform.Release()
	ENDPROC

	PROCEDURE command5.Click
		m.Action = 5
		thisform.Release()
	ENDPROC

	PROCEDURE command6.Click
		m.Action = 6
		thisform.Release()
	ENDPROC

	PROCEDURE timer1.timer
		m.Action = obox.botaot1
		thisform.Release()
	ENDPROC
ENDDEFINE

*---------------------------------------------------------------------
procedure MsgBox2
          lparameters Tx      && Text of message
* Break message into multiple lines so it looks more reasonable
* Windows MessageBox() doesn't wrap the lines until they get nearly
* the entire width of the screen.  We'll insert CR characters at
* reasonable points so the box is more of a rectangle.

* 01/10/01 - Add "m.", remove potential conflict with field names

local OldMemo, T2, L

OldMemo = set("memowidth")
*-- Keep the width small until the number of lines gets too large
set memowidth to 40
if memlines(Tx) > 5
	set memowidth to 50
endif
if memlines(Tx) > 12
	set memowidth to 80
endif
*-- now stuff CR on end of each line
T2 = ''
for L = 1 to memlines(Tx)
	T2 = T2 + mline(Tx, L) + chr(13)
endfor
T2 = left(T2, len(T2)-1)   && strip off last chr(13)
*-- and reset the memowidth
set memowidth to (OldMemo)
return T2
*---------------------------------------------------------------------
