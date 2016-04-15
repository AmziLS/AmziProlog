<HTML>
<HEAD>
<jsp:useBean id="PetsJSPBeanId" scope="session" class="pets.PetsJSPBean" />
<jsp:setProperty name="PetsJSPBeanId" property="*" />
<TITLE>
Pets JSP
</TITLE>
</HEAD>
<BODY>
<H1>
Pets JSP
</H1>
<FORM method="post">
<P>What sound does the pet make : <INPUT NAME="sound"></P>
<INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Submit">
<INPUT TYPE="RESET" VALUE="Reset">
<P>The pet is a : <jsp:getProperty name="PetsJSPBeanId" property="pet" /></P>
</FORM>
</BODY>
</HTML>
