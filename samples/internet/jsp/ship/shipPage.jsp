<%@ page errorPage="shipErrorPage.jsp" %>
<html>
<head>
<title>
Shipping Advisor
</title>
<script language="JavaScript">
<!--
function SP_openBrWindow(theURL,winName,features) { //v2.0
  window.open(theURL,winName,features);
}
//-->
</script>
</head>
<body>
<jsp:useBean id="shipPageBeanId" scope="session" class="ship.shipPageBean"/>
<jsp:setProperty name="shipPageBeanId" property="*" />
<h1><font color="blue">
Shipping Advisor
</font></h1>
Enter the following information:
<form method="post">
<table>
<tr><td>Destination UPS Zone :</td><td><select name="upsZone">
    <option value="1" selected>1</option>
    <option value="2">2</option>
    <option value="3">3</option>
    <option value="4">4</option>
    <option value="5">5</option>
    <option value="6">6</option>
    <option value="7">7</option>
    <option value="8">8</option>
  </select></td></tr>
<tr><td>Type : </td><td><select name="type">
    <option value="letter" selected>letter</option>
    <option value="brochure">brochure</option>
    <option value="package">package</option>
  </select></td></tr>
<tr><td>Weight [lb:oz] :</td><td><input name="weight" maxlength="7" size="7" value="0:1"></td></tr>
<tr><td>C.O.D. :  </td><td><select name="cod">
    <option value="yes">yes</option>
    <option value="no" selected>no</option>
  </select></td></tr>
<tr><td>Declared Value : </td><td>$<input name="declaredValue" maxlength="5" size="5" value="0"></td></tr>
</table>
<input type="submit" name="Find Shipping Options" value="Submit">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<input type="reset" value="Reset Inputs">
<br>

<%-- Display the shipping options only when the form is posted --%>
<%
if (request.getMethod().equals("POST")) {
%>
<hr>
<h2>Shipping Options:</h2>
<img src="http://www.amzi.com/images/button_java_code.gif" onMouseDown="SP_openBrWindow('http://www.amzi.com/popups/shipPageBean_java.htm','javacode','scrollbars=yes,width=200,height=200')">
&nbsp;&nbsp;&nbsp;&nbsp;
<img src="http://www.amzi.com/images/button_prolog_code.gif" onMouseDown="SP_openBrWindow('http://www.amzi.com/popups/ship_pro.htm','prologcode','scrollbars=yes,width=200,height=200')">
<br>
<jsp:getProperty name="shipPageBeanId" property="options" />
<%
}
%>
</form>
</body>
</html>
