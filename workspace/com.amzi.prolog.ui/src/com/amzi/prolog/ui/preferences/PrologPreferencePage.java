package com.amzi.prolog.ui.preferences;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.editor.internal.IPrologColorConstants;

public class PrologPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private IntegerFieldEditor tabSize;
	private ColorFieldEditor codeColor, commentColor, stringColor, predColor, mathColor;
	
	protected Control createContents(Composite parent) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), "com.amzi.prolog.ui.preferences");

		Composite comp = new Composite(parent, SWT.NONE);
		tabSize = new IntegerFieldEditor(IPrologColorConstants.TABSIZE_KEY, "Tab Size", comp, 3);
		codeColor = new ColorFieldEditor(IPrologColorConstants.CODE, "Code Color", comp);	
		commentColor = new ColorFieldEditor(IPrologColorConstants.COMMENT, "Comment Color", comp);
		stringColor = new ColorFieldEditor(IPrologColorConstants.STRING_CONSTANT, "String Color", comp);
		predColor = new ColorFieldEditor(IPrologColorConstants.SYSTEM_PREDICATE, "System Predicate Color", comp);
		mathColor = new ColorFieldEditor(IPrologColorConstants.MATH_SYMBOL_FUNCTION, "Math Symbol Color", comp);
		
		initializeFields();
		
		return comp;
	}

	public void init(IWorkbench workbench) {
		setPreferenceStore(PrologUIPlugin.getDefault().getPreferenceStore());
	}

	/*
	 * @see PreferencePage#performDefaults()
	 */
	protected void performDefaults() {
		super.performDefaults();
		tabSize.setStringValue(getPreferenceStore().getDefaultString(tabSize.getPreferenceName()));
		RGB codeRGB = PreferenceConverter.getDefaultColor(getPreferenceStore(), codeColor.getPreferenceName());
		codeColor.getColorSelector().setColorValue(codeRGB);
		RGB commentRGB = PreferenceConverter.getDefaultColor(getPreferenceStore(), commentColor.getPreferenceName());
		commentColor.getColorSelector().setColorValue(commentRGB);
		RGB stringRGB = PreferenceConverter.getDefaultColor(getPreferenceStore(), stringColor.getPreferenceName());
		stringColor.getColorSelector().setColorValue(stringRGB);
		RGB predRGB = PreferenceConverter.getDefaultColor(getPreferenceStore(), predColor.getPreferenceName());
		predColor.getColorSelector().setColorValue(predRGB);
		RGB mathRGB = PreferenceConverter.getDefaultColor(getPreferenceStore(), mathColor.getPreferenceName());
		mathColor.getColorSelector().setColorValue(mathRGB);
	}
	
    /* (non-Javadoc)
     * Method declared on PreferencePage
     */
    public boolean performOk() {
        storeValues();
		PrologUIPlugin.getDefault().savePluginPreferences();
        return true;
    }

	protected void initializeFields() {
		tabSize.setStringValue(new Integer(getPreferenceStore().getInt(tabSize.getPreferenceName())).toString());
		RGB codeRGB = PreferenceConverter.getColor(getPreferenceStore(), codeColor.getPreferenceName());
		codeColor.getColorSelector().setColorValue(codeRGB);
		RGB commentRGB = PreferenceConverter.getColor(getPreferenceStore(), commentColor.getPreferenceName());
		commentColor.getColorSelector().setColorValue(commentRGB);
		RGB stringRGB = PreferenceConverter.getColor(getPreferenceStore(), stringColor.getPreferenceName());
		stringColor.getColorSelector().setColorValue(stringRGB);
		RGB predRGB = PreferenceConverter.getColor(getPreferenceStore(), predColor.getPreferenceName());
		predColor.getColorSelector().setColorValue(predRGB);
		RGB mathRGB = PreferenceConverter.getColor(getPreferenceStore(), mathColor.getPreferenceName());
		mathColor.getColorSelector().setColorValue(mathRGB);		
	}
	
	private void storeValues() {
        IPreferenceStore store = getPreferenceStore();
        store.setValue(IPrologColorConstants.TABSIZE_KEY, tabSize.getIntValue());
		PreferenceConverter.setValue(store, IPrologColorConstants.CODE, codeColor.getColorSelector().getColorValue());
		PreferenceConverter.setValue(store, IPrologColorConstants.COMMENT, commentColor.getColorSelector().getColorValue());
		PreferenceConverter.setValue(store, IPrologColorConstants.STRING_CONSTANT, stringColor.getColorSelector().getColorValue());
		PreferenceConverter.setValue(store, IPrologColorConstants.SYSTEM_PREDICATE, predColor.getColorSelector().getColorValue());
		PreferenceConverter.setValue(store, IPrologColorConstants.MATH_SYMBOL_FUNCTION, mathColor.getColorSelector().getColorValue());

	}
}
