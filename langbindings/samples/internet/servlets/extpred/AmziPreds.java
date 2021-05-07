/**
 * AmziPreds.java
 *
 * A template for a Java class that implements a number
 * of non-application-specific extended Prolog predicates.
 * It is used with the Pets example, and includes one
 * simple extended predicate that prompts a user for
 * an answer.
 */

import amzi.ls.*;
import java.io.*;

class AmziPreds
{
	private LogicServer ls;

	AmziPreds(LogicServer tls) throws LSException
	{
      // Save the Logic Server object reference, and register
      // the extended predicates.
		ls = tls;
		ls.AddPred("user_info", 3, "AmziPreds", "user_info", this);
	}

	public boolean user_info()
	{
      try
      {
         // Get the user information
         String name = System.getProperty("user.name");
         String os = System.getProperty("os.name");
         String dir = System.getProperty("user.home");

         // And return it
         ls.UnifyStrParm(1, name);
         ls.UnifyStrParm(2, os);
         ls.UnifyStrParm(3, dir);

         return true;
      }
      catch (LSException e)
      {
         return false;
      }
   }
}