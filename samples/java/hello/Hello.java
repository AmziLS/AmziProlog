import amzi.ls.*;

class Hello {
  public static void main(String args[]) throws LSException {
    long term;
    String result;

    LogicServer ls = new LogicServer();
    ls.Init("");
    ls.Load("hello.xpl");
    term = ls.ExecStr("hello($Java Programmer$, Response)");
    if (term == 0)
	  System.out.println("Hello Failed");
	else
      {
	  result = ls.GetStrArg(term, 2);
      System.out.println(result);
	  }
    ls.Close();
  }
}
