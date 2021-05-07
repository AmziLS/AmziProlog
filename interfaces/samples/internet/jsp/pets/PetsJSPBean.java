package pets;

import amzi.ls.*;

public class PetsJSPBean
{
   private String pet = "";
   private String sound = "";
   private LogicServer ls = null;

   public String getPet()
   {
      long term;

      if (sound == null || sound.length() == 0) return "";

      if (ls == null)
      {
         try
         {
            ls = new LogicServer();
            ls.Init("");
            ls.Load("/Program Files/Apache Tomcat 4.0/webapps/amzi/pets.xpl");
         }
         catch (LSException ex)
         {
            ls = null;
            return "***ERROR*** LSException: " + ex.getMessage() + " Directory=" + System.getProperty("user.dir");
         }
      }

      try
      {
         ls.ExecStr("retractall(sound(_))");
         ls.AssertaStr("sound($"+sound+"$)");
         term = ls.ExecStr("pet(X)");
         if (term == 0)
            pet = "***ERROR*** ExecStr Failed";
         else
            pet = ls.GetStrArg(term, 1);
      }
      catch (LSException ex)
      {
         pet = "***ERROR*** LSException: " + ex.getMessage();
      }

      return pet;
   }

   public void setSound(String newValue)
   {
      if (newValue!=null)
         sound = newValue;
   }

}