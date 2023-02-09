*MESSAGEBOX(drive)
DO LOCFILE("FoxyPreviewer.App") && WITH "c:\YourAppFolderHere\"

* After initializing FoxyPreviewer, all properties become available, such as:
*!* WITH _Screen.oFoxyPreviewer
*!*		.cSuccessor  = ""
*!*		.lQuietMode  = .F.
*!*		.lShowSearch = .F.
*!*		.lShowSetup  = .F.
*!*		.nThermType  = 2
*!*		.cLocalPath  = ""
*!* ENDWITH 

*_Screen.oFoxyPreviewer.cImgPrint = GETPICT("bmp")

* _Screen.oFoxyPreviewer.cSaveDefName = "\\dell3\users\teste1"
* _Screen.oFoxyPreviewer.cDestFile = "\\dell3\users\teste1"

_Screen.oFoxyPreviewer.cTitle     = "Meu titulo customizado"
_Screen.oFoxyPreviewer.cFormicon  = "source\images\pr_mail03.ico"
*!*	_Screen.oFoxyPreviewer.lShowClose = .T.
*!*	_Screen.oFoxypreviewer.nShowToolbar = 1 && 1=Visible, 2=Invisible

_Screen.oFoxyPreviewer.nCanvasCount = 1 && initial nr of pages rendered on the preview form.
&& Valid values are 1 (default), 2, or 4.

_Screen.oFoxyPreviewer.nZoomLevel = 5 && initial zoom level of the preview window. Possible values are:
&& 1-10%, 2-25%, 3-50%, 4-75%, 5-100% default, 6-150% ;
&& 7-200%, 8-300%, 9-500%, 10-whole page

* _screen.oFoxyPreviewer.lPrintVisible = .F. && Use this to hide the Printer button

*REPORT FORM LOCFILE(_Samples + "\Solution\Reports\Colors.frx") PREVIEW && RANGE 2,4
*REPORT FORM LOCFILE("FINANw.frx") PREVIEW

RETURN 



REPORT FORM LOCFILE(_Samples + "\Solution\Reports\Percent.frx")  PREVIEW
REPORT FORM LOCFILE(_Samples + "\Solution\Reports\Invoice.frx")  PREVIEW
REPORT FORM LOCFILE(_Samples + "\Solution\Reports\Colors.frx")   PREVIEW
REPORT FORM LOCFILE(_Samples + "\Solution\Reports\Wrapping.frx") PREVIEW



SET REPORTBEHAVIOR 90 
DO LOCFILE(drive+'FoxyPreviewer.APP') 
cFormato = _Samples + "\Solution\Reports\Wrapping.frx" 
REPORT FORM (cFormato) preview 
DO LOCFILE(DRIVE+'FoxyPreviewer.APP') WITH "RELEASE" 
SET REPORTBEHAVIOR 80 
RETURN