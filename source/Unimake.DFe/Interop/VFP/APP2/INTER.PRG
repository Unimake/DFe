PUBLIC oForm
oForm = CREATEOBJECT("teste")
oForm.windowstate=2
oForm.SHOW(1)
* Definição do Form
DEFINE CLASS teste AS FORM
CAPTION = ""
BACKCOLOR = RGB(255,255,255)
ICON = ""
DESKTOP = .T.
MINBUTTON = .T.
MAXBUTTON = .T.
visible=.t.

ADD OBJECT oleIE AS OLECONTROL WITH ;
TOP = 0, LEFT = 0, WIDTH = 795, HEIGHT = 575, OLECLASS = "Shell.Explorer.2"

PROCEDURE oleIE.REFRESH
NODEFAULT
ENDPROC
**Preste atencao que o endereco aqui e fixo
PROCEDURE INIT
THISFORM.oleIE.navigate("http://www.itau.com.br")
ENDPROC
***********************