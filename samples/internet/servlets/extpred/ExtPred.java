import amzi.ls.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.io.*;
import java.util.*;

public class ExtPred extends HttpServlet implements SingleThreadModel
{
   LogicServer ls;

   /**Initialize global variables*/
   public void init() throws ServletException
   {
      ls = null;
   }

   /**Process the HTTP Get request*/
   public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      ServletOutputStream out = response.getOutputStream();

      // set content type and other response header fields first
      response.setContentType("text/html");
      out.println("<HEAD><TITLE> Servlet Extended Predicate </TITLE></HEAD><BODY>");
      out.println("<h1><FONT COLOR=BLUE> Servlet Extended Predicate </FONT></h1>");
      out.println("<form method=post action=\"/amzi/ExtPred\">");
      out.println("<p>Enter full pathname for HTMLDOC.XPL (use double backslashes-do not include htmldoc.xpl or a trailing slash): ");
      out.println("<input name=\"path\" type=text rows=1 size=50>");
      out.println("<p><input name=submit type=submit></form>");
      out.println("</BODY>");
      out.close();
   }

   /**Process the HTTP Post request*/
   public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      long term;
      String result, path;

      response.setContentType("text/html");
      PrintWriter out = new PrintWriter (response.getOutputStream());
      out.println("<html>");
      out.println("<head><title>Servlet Extended Predicate</title></head>");

      try
      {
         // Create and initialize a Logic Server
         LogicServer ls = new LogicServer();
         ls.Init("");

         // create an extended predicate library
         // that initializes its own predicates
         AmziPreds ap = new AmziPreds(ls);

         // Get the xpl pathname and load it
         if ( (path = request.getParameter("path")) == null) path = "";
         path = path + "\\htmldoc.xpl";
         ls.Load(path);

         // Call our Prolog predicate to get a part of an HTML document
         // htmldoc calls user_info in AmziPreds
         term = ls.ExecStr("htmldoc(DOC)");
         if (term == 0)
            out.println("htmldoc Failed");
         else
         {
	         result = ls.GetStrArg(term, 1);
            out.println(result);
	      }
         ls.Close();
         ls = null;
      }
      catch (LSException ex)
      {
         try
         {
            if (ls != null) ls.Close();
            ls = null;
         }
         catch (LSException ex2)
         { }
         out.println("Logic Server Exception: " + ex.getMessage());
      }

      out.println("<body>");
      out.println("</body></html>");
      out.close();
   }

   /**Clean up resources*/
   public void destroy()
   {
      try { if (ls != null) ls.Close(); }
      catch (LSException ex) {}
   }
}
