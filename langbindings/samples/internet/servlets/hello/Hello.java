import amzi.ls.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.util.*;

public class Hello extends HttpServlet
   implements SingleThreadModel			// The Amzi! Logic Server is not thread safe
{

   //Initialize global variables
   public void init(ServletConfig config) throws ServletException
   {
      super.init(config);
   }

   // Process the HTTP Get request
   public void doGet (HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {

      ServletOutputStream out = response.getOutputStream();

      // set content type and other response header fields first
      response.setContentType("text/html");
      out.println("<HEAD><TITLE> Hello Java Servlet </TITLE></HEAD><BODY>");
      out.println("<h1><FONT COLOR=BLUE> Hello Java Servlet </FONT></h1>");
      out.println("<form method=post action=\"/amzi/Hello\">");
      out.println("<p>Enter full pathname for HELLO.XPL (use double backslashes-do not include hello.xpl or a trailing slash): ");
      out.println("<input name=\"path\" type=text rows=1 size=50>");
      out.println("<p><input name=submit type=submit></form>");
      out.println("</BODY>");
      out.close();
   }

   //Process the HTTP Post request
   public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      long term;
      String result, path;

      response.setContentType("text/html");
      PrintWriter out = new PrintWriter (response.getOutputStream());
      out.println("<html>");
      out.println("<head><title>Hello</title></head>");

      try
      {
         LogicServer ls = new LogicServer();
         ls.Init("");
         if ( (path = request.getParameter("path")) == null) path = "";
         path = path + "\\hello.xpl";
         ls.Load(path);
         term = ls.ExecStr("hello($Java Programmer$, Response)");
         if (term == 0)
            out.println("Hello Failed");
         else
         {
	         result = ls.GetStrArg(term, 2);
            out.println(result);
	      }
         ls.Close();
      }
      catch (LSException ex)
      {
         out.println("Logic Server Exception: " + ex.getMessage());
      }

      out.println("<body>");
      out.println("</body></html>");
      out.close();
   }

   //Get Servlet information
  public String getServletInfo()
   {
      return "Hello Information";
   }
}
 
