package com.amzi.prolog.core.utils;

import amzi.ls.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class Utils {

   public static String tiltSlashes(String in)
   {
		StringBuffer out;
		out = new StringBuffer(in);
		for (int i=0; i<in.length(); i++)
		{
			if (out.charAt(i) == '\\')
				out.setCharAt(i, '/');
		}
		return new String(out);

/*      String slashed;
      int i, slash;

      slashed = "";
      i = 0;
      while ((slash = s.indexOf("\\", i)) >= 0)
      {
         slashed = slashed + s.substring(i, slash) + "\\\\";
         i = slash+1;
      }
      slashed = slashed + s.substring(i, s.length());
      return slashed;
*/
   }

   	public static synchronized String[] prologListToStringArray(LogicServer ls, long list, int size) throws LSException
   	{
		List array = new ArrayList();

      	// Check for the empty list or an atom
      	long type = ls.GetTermType(list);

      	if (type != LogicServer.pLIST) return new String[0];

      	while (list != 0)
      	{
      		int len = ls.StrTermLen(ls.GetHead(list));
         	array.add(ls.TermToStr(ls.GetHead(list), len+2));

         	list = ls.GetTail(list);
      	}

		String[] result= new String[array.size()];
		array.toArray(result);
		return result;
   }

   public static String doubleSlashes(String s)
   {
	  String slashed;
	  int i, slash;

	  slashed = "";
	  i = 0;
	  while ((slash = s.indexOf("\\", i)) >= 0)
	  {
		 slashed = slashed + s.substring(i, slash) + "\\\\";
		 i = slash+1;
	  }
	  slashed = slashed + s.substring(i, s.length());
	  return slashed;
   }

   public static synchronized Properties prologListToProperties(LogicServer ls, long list, int size) throws LSException
   {
      Properties p = new Properties();
      long element, name, value;

      // Check for the empty list or an atom
      long type = ls.GetTermType(list);
      if (type != LogicServer.pLIST) return p;

      while (list != 0)
      {
         element = ls.GetHead(list);
         name = ls.GetArg(element, 1);
         value = ls.GetArg(element, 2);
         int nlen = ls.StrTermLen(name);
         int vlen = ls.StrTermLen(value);
         if (ls.GetTermType(value) == LogicServer.pLIST)
            p.setProperty(ls.TermToStr(name, nlen+2), ls.TermToStrQ(value, vlen+2));
         else {
			p.setProperty(ls.TermToStr(name, nlen+2), ls.TermToStr(value, vlen+2));
         }

         list = ls.GetTail(list);
      }
      return p;
   }

   public static synchronized Properties prologListToPropertiesQ(LogicServer ls, long list, int size) throws LSException
   {
	  Properties p = new Properties();
	  long element, name, value;

	  // Check for the empty list or an atom
	  long type = ls.GetTermType(list);
	  if (type != LogicServer.pLIST) return p;

	  while (list != 0)
	  {
		element = ls.GetHead(list);
		name = ls.GetArg(element, 1);
		value = ls.GetArg(element, 2);
		int nlen = ls.StrTermLen(name);
		int vlen = ls.StrTermLen(value);
		p.setProperty(ls.TermToStr(name, nlen+2), ls.TermToStrQ(value, vlen+2));
		
		list = ls.GetTail(list);
	  }
	  return p;
	}
}
