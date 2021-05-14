<%@ LANGUAGE=VBScript %>
<% Option Explicit %>
<HTML>
<HEAD>
<TITLE>Pets ODBC/VB ASP Sample</TITLE>
</HEAD>
<BODY>
<H1>Pets ODBC/VB ASP Sample</H1>

<FORM METHOD="POST" ACTION="pets_odbc.asp">
<P>
What sound does your pet make?
</P>
<INPUT TYPE="RADIO" NAME="sound" VALUE="woof">Woof<BR>
<INPUT TYPE="RADIO" NAME="sound" VALUE="meow">Meow<BR>
<INPUT TYPE="RADIO" NAME="sound" VALUE="quack">Quack<BR>
<P>
<INPUT TYPE="SUBMIT" VALUE="SUBMIT">
</P>
</FORM>

<% Dim Pets
   Dim Sound
   Dim Pet

   Sound = Cstr(Request.Form("sound"))

   If Sound <> "" Then
      Set Pets = Server.CreateObject("pets_odbc.Pets") 
      Pets.Init("c:\InetPub\Scripts\pets_odbc.xpl") 
      Pet = Pets.GetPet(Sound)
      Response.Write "<P>Your pet is a " + Pet + "</P>"
      Pets.Done()
   Else
      Response.Write "<P>(No input yet.)</P>"
   End If
%>

</BODY>
</HTML>
