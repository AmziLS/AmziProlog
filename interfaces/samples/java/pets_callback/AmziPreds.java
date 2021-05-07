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

class AmziPreds {
	private LogicServer ls;
   private BufferedReader din;

	AmziPreds(LogicServer tls) throws LSException
	{
      // Initialize a buffered reader for the input prompts
      din = new BufferedReader(new InputStreamReader(System.in));

      // Save the Logic Server object reference, and register
      // the extended predicates.
		ls = tls;
		ls.AddPred("prompt", 2, "AmziPreds", "prompt", this);
	}

	public boolean prompt()
	{
      String answer;
      boolean tf;

      try
      {
         // Prompt with the first parameter
         System.out.println(ls.GetStrParm(1));

         // Use the buffered reader to get an answer
         // for the second, unify it and return true/false
         // depending on the result of the unification.
         return ls.UnifyStrParm(2, din.readLine());
      }
      catch (LSException e)
      {
         System.out.println(e.GetMsg());
         return false;
      }
      catch (IOException e)
      {
         System.out.println(e.getMessage());
         return false;
      }
   }
}