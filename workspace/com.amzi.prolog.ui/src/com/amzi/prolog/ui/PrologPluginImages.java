package com.amzi.prolog.ui;

import java.net.MalformedURLException;
import java.net.URL;

//import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class PrologPluginImages {
	
	// The plugin registry
	private static ImageRegistry imageRegistry = new ImageRegistry();

	// Subdirectory (under the package containing this class) where 16 color images are
	private static URL fgIconBaseURL;
	static {
		try {
			fgIconBaseURL= new URL(PrologUIPlugin.getDefault().getBundle().getEntry("/"), "icons/");
		} catch (MalformedURLException e) {
			PrologUIPlugin.log(e);
		}
	}	
	private static final String NAME_PREFIX= "com.amzi.prolog.ui" + '.';
	private static final int NAME_PREFIX_LENGTH= NAME_PREFIX.length();

	// icons directory images
	public static final String D_ROOT = "";
	
	public static final String IMG_QUESTIONDASH= NAME_PREFIX + "question_dash.gif";
	public static final String IMG_AMZIPAW= NAME_PREFIX + "amzi_paw.gif";
	public static final String IMG_MONITOR= NAME_PREFIX + "monitor.gif";
	public static final String IMG_REMOTE= NAME_PREFIX + "remote.gif";
	public static final String IMG_SEGMENTEDIT= NAME_PREFIX + "segment_edit.gif";
	
	public static final ImageDescriptor DESC_QUESTIONDASH= createManaged(D_ROOT, IMG_QUESTIONDASH);
	public static final ImageDescriptor DESC_AMZIPAW= createManaged(D_ROOT, IMG_AMZIPAW);
	public static final ImageDescriptor DESC_MONITOR= createManaged(D_ROOT, IMG_MONITOR);
	public static final ImageDescriptor DESC_REMOTE= createManaged(D_ROOT, IMG_REMOTE);
	public static final ImageDescriptor DESC_SEGMENTEDIT= createManaged(D_ROOT, IMG_SEGMENTEDIT);
	
	// obj16 directory images
	public static final String D_OBJ= "obj16/";

	public static final String IMG_OBJS_PREDICATE= NAME_PREFIX + "predicate.gif";
	public static final String IMG_OBJS_CLAUSE= NAME_PREFIX + "clause.gif";
	public static final String IMG_OBJS_DIRECTIVES= NAME_PREFIX + "directives.gif";
	public static final String IMG_OBJS_DIRECTIVE= NAME_PREFIX + "directive.gif";
	public static final String IMG_OBJS_STOP= NAME_PREFIX + "stop.gif";
	public static final String IMG_OBJS_START= NAME_PREFIX + "start.gif";
	public static final String IMG_OBJS_USES= NAME_PREFIX + "uses.gif";
	public static final String IMG_OBJS_USEDBY= NAME_PREFIX + "usedby.gif";
	public static final String IMG_OBJS_REFRESH= NAME_PREFIX + "refresh.gif";
	public static final String IMG_OBJS_AZSORT= NAME_PREFIX + "azsort.gif";
	public static final String IMG_OBJS_DIAMOND= NAME_PREFIX + "diamond.gif";
	public static final String IMG_OBJS_COPY= NAME_PREFIX + "copy_edit.gif";
	public static final String IMG_OBJS_MATHWORD= NAME_PREFIX + "mathword.gif";
	public static final String IMG_OBJS_DIRECTWORD= NAME_PREFIX + "directword.gif";
	public static final String IMG_OBJS_CONSTWORD= NAME_PREFIX + "constword.gif";
	public static final String IMG_OBJS_PREDWORD= NAME_PREFIX + "predword.gif";
	
	public static final ImageDescriptor DESC_OBJS_PREDICATE= createManaged(D_OBJ, IMG_OBJS_PREDICATE);
	public static final ImageDescriptor DESC_OBJS_CLAUSE= createManaged(D_OBJ, IMG_OBJS_CLAUSE);
	public static final ImageDescriptor DESC_OBJS_DIRECTIVES= createManaged(D_OBJ, IMG_OBJS_DIRECTIVES);
	public static final ImageDescriptor DESC_OBJS_DIRECTIVE= createManaged(D_OBJ, IMG_OBJS_DIRECTIVE);
	public static final ImageDescriptor DESC_OBJS_STOP= createManaged(D_OBJ, IMG_OBJS_STOP);
	public static final ImageDescriptor DESC_OBJS_START= createManaged(D_OBJ, IMG_OBJS_START);
	public static final ImageDescriptor DESC_OBJS_USES= createManaged(D_OBJ, IMG_OBJS_USES);
	public static final ImageDescriptor DESC_OBJS_USEDBY= createManaged(D_OBJ, IMG_OBJS_USEDBY);
	public static final ImageDescriptor DESC_OBJS_REFRESH= createManaged(D_OBJ, IMG_OBJS_REFRESH);
	public static final ImageDescriptor DESC_OBJS_AZSORT= createManaged(D_OBJ, IMG_OBJS_AZSORT);
	public static final ImageDescriptor DESC_OBJS_DIAMOND= createManaged(D_OBJ, IMG_OBJS_DIAMOND);
	public static final ImageDescriptor DESC_OBJS_COPY= createManaged(D_OBJ, IMG_OBJS_COPY);
	public static final ImageDescriptor DESC_OBJS_MATHWORD= createManaged(D_OBJ, IMG_OBJS_MATHWORD);
	public static final ImageDescriptor DESC_OBJS_DIRECTWORD= createManaged(D_OBJ, IMG_OBJS_DIRECTWORD);
	public static final ImageDescriptor DESC_OBJS_CONSTWORD= createManaged(D_OBJ, IMG_OBJS_CONSTWORD);
	public static final ImageDescriptor DESC_OBJS_PREDWORD= createManaged(D_OBJ, IMG_OBJS_PREDWORD);
	
	public static void initialize() {
		//createManaged(registry, D_OBJ, IMG_OBJS_PREDICATE);
	}
	
	private static ImageDescriptor createManaged(String prefix, String name) {
		return createManaged(imageRegistry, prefix, name);
	}
	
	private static ImageDescriptor createManaged(ImageRegistry registry, String prefix, String name) {
		ImageDescriptor result= ImageDescriptor.createFromURL(makeIconFileURL(prefix, name.substring(NAME_PREFIX_LENGTH)));
		registry.put(name, result);
		return result;
	}
	
	public static Image get(String key) {
		return imageRegistry.get(key);
	}
	
	private static ImageDescriptor create(String prefix, String name) {
		return ImageDescriptor.createFromURL(makeIconFileURL(prefix, name));
	}
	
	private static URL makeIconFileURL(String prefix, String name) {
		StringBuffer buffer= new StringBuffer(prefix);
		buffer.append(name);
		try {
			URL url = new URL(fgIconBaseURL, buffer.toString());
			return url;
		} 
		catch (MalformedURLException e) {
			PrologUIPlugin.log(e);
			return null;
		}
	}
	
	/**
	 * Sets all available image descriptors for the given action.
	 */	
	public static void setImageDescriptors(IAction action, String type, String relPath) {
		relPath= relPath.substring(NAME_PREFIX_LENGTH);
		action.setDisabledImageDescriptor(create("d" + type, relPath));
		action.setHoverImageDescriptor(create("c" + type, relPath));
		action.setImageDescriptor(create("e" + type, relPath));
	}
	
	/**
	 * Helper method to access the image registry from the JavaPlugin class.
	 */
	static ImageRegistry getImageRegistry() {
		return imageRegistry;
	}
}
