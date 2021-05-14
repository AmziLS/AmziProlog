package com.amzi.prolog.core.utils;

import java.io.File;
import java.io.FilenameFilter;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class ExtFilenameFilter implements FilenameFilter {
   String ext;

   public ExtFilenameFilter(String ext)
   {
      this.ext = ext;
   }

   public boolean accept(File dir, String name)
   {
	   int i = name.lastIndexOf('.');
	   if(i > 0 && i < name.length()-1)
      if (name.substring(i+1).toLowerCase().equals(ext)) return true;
      return false;
   }
}