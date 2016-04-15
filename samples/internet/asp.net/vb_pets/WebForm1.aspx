<%@ Page Language="vb" AutoEventWireup="false" Inherits="vb_pets.WebForm1" CodeFile="WebForm1.aspx.vb" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML>
	<HEAD>
		<title>PetsWebForm</title>
		<meta name="GENERATOR" content="Microsoft Visual Studio.NET 7.0">
		<meta name="CODE_LANGUAGE" content="Visual Basic 7.0">
		<meta name="vs_defaultClientScript" content="JavaScript">
		<meta name="vs_targetSchema" content="http://schemas.microsoft.com/intellisense/ie5">
	</HEAD>
	<body>
		<form id="Form1" method="post" runat="server">
			<P>
				<asp:Label id="Label1" runat="server" Width="208px" Height="67px">What sound does your pet make?</asp:Label>
				<asp:ListBox id="SoundListBox" runat="server">
					<asp:ListItem Value="woof">woof</asp:ListItem>
					<asp:ListItem Value="quack">quack</asp:ListItem>
					<asp:ListItem Value="meow">meow</asp:ListItem>
				</asp:ListBox></P>
			<P>
				<asp:Button id="GetPetButton" runat="server" Text="Get Pet"></asp:Button></P>
			<P>Your pet is a
				<asp:TextBox id="PetTextBox" runat="server"></asp:TextBox></P>
		</form>
	</body>
</HTML>
