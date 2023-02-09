
<%@ Page Language="VB" AutoEventWireup="true" CodeFile="MPNSabra.aspx.vb" Inherits="_MPNSabra"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Sistema MPNSabra</title>
</head>
<Body Style=Font-Size :"12px" > 
<Form id="Default" method="post" Runat="Server">
<div>
<Table  border= 0>
<tr>
<td align="center">
<span style=font-size : "24px"; color:"#99000cc"; font-family : courier><strong>

<asp:Panel Id="MostraMensagem" Runat="Server"
 style="top:215px;Left:20px;position:absolute;">
<asp:Label type="Text" Id="Mostrou" Runat="Server" BorderStyle="solid"
 Width="241px"  Height="20px"   Align="center" backcolor="Yellow"   />
<asp:Button ID="BotaoMostra" text="OK" backcolor="Green" Runat="Server" 
 OnClick="MostrandoMensagem" />
</asp:Panel>


<asp:label  Id="MeText" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="Small" BackColor="Yellow" ForeColor="Blue"
 Width="728"  Height="20"  Text="Nome da Empresa Usuaria"
 style="top:40px;Left:350px;position:absolute;"/>

<asp:label  Id="LabelNota" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="Small" BackColor="White" ForeColor="Black"
 Width="728"  Height="20"  Text="Chave da Nota Fiscal"
 style="top:80px;Left:350px;position:absolute;"/>

<asp:Textbox type="Text" Id="ChaveNota" Runat="Server"  
Font-Names="Arial Black" Font-Size="Small" BackColor="White" ForeColor="Black"
 Width="500"  Height="20"  Text="Chave da Nota Fiscal"
 style="top:100px;Left:350px;position:absolute;"/>

<asp:label  Id="LabelProt" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="Small" BackColor="White" ForeColor="Black"
 Width="728"  Height="20"  Text="Número do Protocolo"
 style="top:130px;Left:350px;position:absolute;"/>

<asp:Textbox type="Text" Id="Protoca" Runat="Server"  
Font-Names="Arial Black" Font-Size="Small" BackColor="White" ForeColor="Black"
 Width="500"  Height="20"  Text="Número do Protocolo"
 style="top:150px;Left:350px;position:absolute;"/>

<tr>
<td align="center">
<asp:Button  Id="Button1" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Estado Serviço"
style="top:380px;Left:550px;position:absolute;"
 OnServerClick="Button1_ServerClick" OnClientClick="Button1_ServerClick"
 OnClick="Button1_Click" />
  
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button2" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Anular Número"
style="top:380px;Left:650px;position:absolute;"
 OnServerClick="Button2_ServerClick" OnClientClick="Button2_ServerClick"
 OnClick="Button2_Click" />
  
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button3" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Cancelar Nota"
style="top:380px;Left:450px;position:absolute;"
 OnServerClick="Button3_ServerClick" OnClientClick="Button3_ServerClick"
 OnClick="Button3_Click" />
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button4" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Autorizar XML"
style="top:280px;Left:450px;position:absolute;"
 OnClick="Button4_Click" />
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button5" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Autorizar Campos"
style="top:280px;Left:550px;position:absolute;"
 OnServerClick="Button5_ServerClick" OnClientClick="Button5_ServerClick"
 OnClick="Button5_Click" />
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button6" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Evento CCE"
style="top:280px;Left:650px;position:absolute;"
 OnServerClick="Button6_ServerClick" OnClientClick="Button6_ServerClick"
 OnClick="Button6_Click" />
</td></tr>

<tr>
<td align="center">
<asp:Button  Id="Button7" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Consultar Situacao NF"
style="top:180px;Left:450px;position:absolute;"
 OnServerClick="Button7_ServerClick" OnClientClick="Button7_ServerClick"
 OnClick="Button7_Click" />
</td></tr>

	<tr>
<td align="center">
<asp:Button  Id="Button8" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Consultar GTIN"
style="top:180px;Left:550px;position:absolute;"
 OnServerClick="Button8_ServerClick" OnClientClick="Button8_ServerClick"
 OnClick="Button8_Click" />
</td></tr>

	<tr>
<td align="center">
<asp:Button  Id="Button9" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Consultar Distribuicao"
style="top:180px;Left:650px;position:absolute;"
 OnServerClick="Button9_ServerClick" OnClientClick="Button9_ServerClick"
 OnClick="Button9_Click" />
</td></tr>	
	
	<tr>
<td align="center">
<asp:Button  Id="Button10" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="XX-Small" BackColor="Red" ForeColor="White"
 Width="89px"  Height="33px"  Text="Manifestacao NFE"
style="top:180px;Left:750px;position:absolute;"
 OnServerClick="Button10_ServerClick" OnClientClick="Button10_ServerClick"
 OnClick="Button10_Click" />
</td></tr>		
	
	
<tr>
<td align="center">
<asp:label  Id="CopiRait" Runat="Server" BorderStyle="none"
Font-Names="Arial Black" Font-Size="Small" BackColor="Yellow" ForeColor="Blue"
 Width="728"  Height="20"  Text="Copyright 2020 MPN Assessoria e Sistemas Ltda        (21) 2462-2094"
 style="top:480px;Left:350px;position:absolute;"/>
</td></tr>

<asp:Textbox type="Text" Id="CodigoOperador" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Feito" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="File5" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Extens5" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Conectaa" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Empresa" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Natal" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Text3" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Xico" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="LocalPFX" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="SenhaPFX" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Pathe" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="CertificadoValido" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Versao" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="TipoNF" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="CUF" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="TPAmb" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Serie" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Configuracao1" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Certo" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="Errado" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="IDentCSC" Runat="Server"  style="display:none" />
<asp:Textbox type="Text" Id="TokenCSC" Runat="Server"  style="display:none" /> 
<asp:Textbox type="Text" Id="CNPJ" Runat="Server"  style="display:none" /> 
<asp:Textbox type="Text" Id="ContaGrava" Runat="Server"  style="display:none" />
</div>
</form>
</body>
</html>
