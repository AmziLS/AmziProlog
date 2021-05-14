package com.amzi.prolog.ui.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;

import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.editor.internal.IPrologColorConstants;

public class PrologPreferenceInitializer extends AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
        IPreferenceStore store = PrologUIPlugin.getDefault().getPreferenceStore();
        store.setDefault(IPrologColorConstants.TABSIZE_KEY, 3);
		PreferenceConverter.setDefault(store, IPrologColorConstants.CODE, new RGB(0, 0, 0));
		PreferenceConverter.setDefault(store, IPrologColorConstants.COMMENT, new RGB(172, 0, 0));
		PreferenceConverter.setDefault(store, IPrologColorConstants.STRING_CONSTANT, new RGB(0, 172, 0));
		PreferenceConverter.setDefault(store, IPrologColorConstants.SYSTEM_PREDICATE, new RGB(0, 128, 255));
		PreferenceConverter.setDefault(store, IPrologColorConstants.MATH_SYMBOL_FUNCTION, new RGB(64, 0, 128));
	}
}
