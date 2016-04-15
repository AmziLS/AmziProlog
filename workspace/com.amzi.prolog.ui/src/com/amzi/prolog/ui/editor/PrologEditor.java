package com.amzi.prolog.ui.editor;

import com.amzi.prolog.ui.editor.actions.BreakpointRulerEditorAction;
import com.amzi.prolog.ui.editor.internal.IPrologColorConstants;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.views.*;

import java.util.Iterator;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.DefaultRangeIndicator;
import org.eclipse.ui.texteditor.ITextEditorActionConstants;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

import org.eclipse.jface.text.source.IAnnotationAccess;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.OverviewRuler;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.eclipse.ui.texteditor.DefaultMarkerAnnotationAccess;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.MarkerAnnotationPreferences;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextOperationAction;

public class PrologEditor extends TextEditor {
	private ColorManager colorManager;
	private PrologContentOutlinePage outlinePage;
	private PrologSourceViewerConfiguration sourceViewerConfig;
	private SourceViewer sourceViewer;
	private PrologDocumentProvider prologDocumentProvider;
//	private IVerticalRuler verticalRuler;
	private IPreferenceStore preferenceStore;
	//private Color originalSelectionBackgroundColor, originalSelectionForegroundColor;
	//private ISelectionChangedListener selectionChangedListener;
	//private boolean listenerShouldIgnoreSelectionChange;
	private MarkerAnnotationPreferences fAnnotationPreferences;
	//private boolean checkBackground = false;
	//private boolean lightBackground;
	
	public PrologEditor() {
		super();
	}

	/*
	 * @see org.eclipse.ui.editors.text.TextEditor#initializeEditor()
	 */
	protected void initializeEditor() {
		super.initializeEditor();
		
		// The font is loaded from the preference store
		// See AbstractTextEditor for other preferences that load automatically
		preferenceStore = PrologUIPlugin.getDefault().getPreferenceStore();
		
		// The second item was preceded by EditorsPlugin.getDefault(). but it
		// generated a warning.
		IPreferenceStore chainedStores[] = {preferenceStore, getPreferenceStore()};
		setPreferenceStore(new ChainedPreferenceStore(chainedStores));
		
		// Create the color manager
		colorManager = new ColorManager();
		
		sourceViewerConfig = new PrologSourceViewerConfiguration(colorManager);
		setSourceViewerConfiguration(sourceViewerConfig);
		
		prologDocumentProvider = new PrologDocumentProvider(colorManager); 
		setDocumentProvider(prologDocumentProvider);

		setRangeIndicator(new DefaultRangeIndicator());
//		setHelpContextId(ITextEditorHelpContextIds.TEXT_EDITOR);
		setHelpContextId("com.amzi.prolog.ui.editor");
	
		setEditorContextMenuId("#PrologEditorContext"); //$NON-NLS-1$
		setRulerContextMenuId("#PrologRulerContext"); //$NON-NLS-1$
		
		fAnnotationPreferences= new MarkerAnnotationPreferences();
	}
	
	/*
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#createSourceViewer(org.eclipse.swt.widgets.Composite, org.eclipse.jface.text.source.IVerticalRuler, int)
	 */
	protected ISourceViewer createSourceViewer(Composite parent, IVerticalRuler ruler, int styles) {
		fAnnotationAccess= createAnnotationAccess();
		
		fOverviewRuler= new OverviewRuler(fAnnotationAccess, VERTICAL_RULER_WIDTH, colorManager);
		Iterator e= fAnnotationPreferences.getAnnotationPreferences().iterator();
		while (e.hasNext()) {
			AnnotationPreference preference= (AnnotationPreference) e.next();
			if (preference.contributesToHeader())
				fOverviewRuler.addHeaderAnnotationType(preference.getAnnotationType());
		}
		
		sourceViewer= new SourceViewer(parent, ruler, fOverviewRuler, isOverviewRulerVisible(), styles);
		fSourceViewerDecorationSupport= new SourceViewerDecorationSupport(sourceViewer, fOverviewRuler, fAnnotationAccess, colorManager);
		configureSourceViewerDecorationSupport();
		
		return sourceViewer;
	}

	/**
	 * Creates the annotation access for this editor.
	 * @return the created annotation access
	 */
	protected IAnnotationAccess createAnnotationAccess() {
		return new DefaultMarkerAnnotationAccess();
	}

	/*
	 * @see org.eclipse.ui.editors.text.TextEditor#configureSourceViewerDecorationSupport()
	 */
	protected void configureSourceViewerDecorationSupport() {
// 3.0		super.configureSourceViewerDecorationSupport();

		Iterator e= fAnnotationPreferences.getAnnotationPreferences().iterator();
		while (e.hasNext())
			fSourceViewerDecorationSupport.setAnnotationPreference((AnnotationPreference) e.next());
/*		fSourceViewerDecorationSupport.setAnnotationPainterPreferenceKeys(DefaultMarkerAnnotationAccess.UNKNOWN, 
			TextEditorPreferenceConstants.EDITOR_UNKNOWN_INDICATION_COLOR, 
			TextEditorPreferenceConstants.EDITOR_UNKNOWN_INDICATION, 
			TextEditorPreferenceConstants.EDITOR_UNKNOWN_INDICATION_IN_OVERVIEW_RULER, 0);
*/		
		fSourceViewerDecorationSupport.setCursorLinePainterPreferenceKeys(AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE, 
			AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE_COLOR);
		fSourceViewerDecorationSupport.setMarginPainterPreferenceKeys(AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN, 
			AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR, 
			AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN);
		fSourceViewerDecorationSupport.setSymbolicFontName(getFontPropertyPreferenceKey());
	}

	/**
	 * Creates the editor's standard actions and connects them with the global
	 * workbench actions.
	 */
	protected void createActions() {
		super.createActions();

//		try {
			// In future, add a content assist action and a goto line action
//			setAction(CONTENT_ASSIST_ACTION_ID, new COBOLContentAssistantAction(COBOLEditorMessages.getResourceBundle(), CONTENT_ASSIST_RESOURCE_KEY_PREFIX, this, ISourceViewer.CONTENTASSIST_PROPOSALS));
//			Action gotoAction = new COBOLGotoLineAction(COBOLEditorMessages.getResourceBundle(),this);
//			if ( gotoAction != null ){
//				setAction(ITextEditorActionConstants.GOTO_LINE, gotoAction);
//			}

			// Breakpoint setting via the ruler menu and double-click
			BreakpointRulerEditorAction action = new BreakpointRulerEditorAction(PrologUIPlugin.getResourceBundle(), 
				"ToggleBreakpoint_", this, getVerticalRuler());
			setAction("ToggleBreakpoint", action);
			setAction(ITextEditorActionConstants.RULER_DOUBLE_CLICK, getAction("ToggleBreakpoint"));
			
			IAction a= new TextOperationAction(PrologUIPlugin.getResourceBundle(), "ContentAssistProposal_", this, ISourceViewer.CONTENTASSIST_PROPOSALS); //$NON-NLS-1$
			a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
			setAction("ContentAssistProposal", a); //$NON-NLS-1$
		
			a= new TextOperationAction(PrologUIPlugin.getResourceBundle(), "ContentAssistTip_", this, ISourceViewer.CONTENTASSIST_CONTEXT_INFORMATION);  //$NON-NLS-1$
			a.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
			setAction("ContentAssistTip", a); //$NON-NLS-1$

//			createStatusLineMessageProvider();

//		} 
//		catch(NullPointerException e) {
//		}
	}

	public void dispose() {
		colorManager.dispose();
		if (outlinePage != null)
			outlinePage.setInput(null);

		super.dispose();
	}

	public void doRevertToSaved() {
		super.doRevertToSaved();
		if (outlinePage != null)
			outlinePage.update();
	}
	
	public void doSave(IProgressMonitor monitor) {
		super.doSave(monitor);
		if (outlinePage != null)
			outlinePage.update();
	}
	
	public void doSaveAs() {
		super.doSaveAs();
		if (outlinePage != null)
			outlinePage.update();
	}
	
	public void doSetInput(IEditorInput input) throws CoreException {
		// Close it if this is a leftover debugging window
//		if (input instanceof PrologEditorInput)
//			if (!((PrologEditorInput)input).isDebuggerActive())
//				close(false);

		super.doSetInput(input);
		if (outlinePage != null)
			outlinePage.setInput(input);
	}
	
	public void editorContextMenuAboutToShow(IMenuManager menu) {
		super.editorContextMenuAboutToShow(menu);
		addAction(menu, "ContentAssistProposal"); //$NON-NLS-1$
//		addAction(menu, "ContentAssistTip"); //$NON-NLS-1$
	}
	
	protected void rulerContextMenuAboutToShow(IMenuManager menu) {
		super.rulerContextMenuAboutToShow(menu);
		
		// Adds the action defined above by setAction
		addAction(menu, "ToggleBreakpoint");
	}
	
	public boolean isEditable() {
//		Object obj = getEditorInput();
//		if (obj instanceof PrologEditorInput)
//			return false;
//		else
			return super.isEditable();
	}
	
	public String getTitle() {
//		Object obj = getEditorInput();
//		if (obj instanceof PrologEditorInput) {
//			String title = super.getTitle();
//			return "! " + title;
//		}
//		else
			return super.getTitle();
	}
	
	public Object getAdapter(Class required) {
		if (IContentOutlinePage.class.equals(required)) {
			if (outlinePage == null) {
				outlinePage= new PrologContentOutlinePage(getDocumentProvider(), this);
				if (getEditorInput() != null)
					outlinePage.setInput(getEditorInput());
			}
			return outlinePage;
		}
		return super.getAdapter(required);
	}
	
	/**
	 * Handles a property change event describing a change
	 * of the editor's preference store and updates the preference
	 * related editor properties.
	 *
	 * @param event the property change event
	 */
	protected void handlePreferenceStoreChanged(PropertyChangeEvent event) {
		// Everything except the items below are automatically updated
		super.handlePreferenceStoreChanged(event);
		
		ISourceViewer sv = getSourceViewer();
	
		try {
			if (sv != null) {
	
				String property= event.getProperty();
				
				// Tabs changed
				if (IPrologColorConstants.TABSIZE_KEY.equals(property)) {
					SourceViewerConfiguration configuration= getSourceViewerConfiguration();
					String[] types= configuration.getConfiguredContentTypes(sv);
					for (int i= 0; i < types.length; i++)
						sv.setIndentPrefixes(configuration.getIndentPrefixes(sv, types[i]), types[i]);
	
					Object value= event.getNewValue();
	
					if (value instanceof Integer) {
						sv.getTextWidget().setTabs(((Integer) value).intValue());
	
					} else if (value instanceof String) {
						sv.getTextWidget().setTabs(Integer.parseInt((String) value));
					}
				}
				
				// Prolog color constants
				if (IPrologColorConstants.CODE.equals(property) || IPrologColorConstants.COMMENT.equals(property) ||
					IPrologColorConstants.STRING_CONSTANT.equals(property) || IPrologColorConstants.SYSTEM_PREDICATE.equals(property)) {
					Object value = event.getNewValue();
					RGB rgb = null;
					if (value instanceof String)
						rgb = StringConverter.asRGB((String)event.getNewValue());
					if (value instanceof RGB)
						rgb = (RGB)value;
					if (rgb != null)
						sourceViewerConfig.updateColor(property, rgb);
					sv.invalidateTextPresentation();
				}
			}
		} 
		finally {
			if (sv != null && affectsTextPresentation(event))
				sv.invalidateTextPresentation();
		}
	}
/*
	private void rememberOriginalSelectionBackgroundColor() {
		if (originalSelectionBackgroundColor ==null) {
			originalSelectionBackgroundColor = getSourceViewer().getTextWidget().getSelectionBackground();
			originalSelectionForegroundColor = getSourceViewer().getTextWidget().getSelectionForeground();
			selectionChangedListener = new ISelectionChangedListener() {
				public void selectionChanged(SelectionChangedEvent e) {
					if (!listenerShouldIgnoreSelectionChange)
						getSourceViewer().getTextWidget().setSelectionBackground(originalSelectionBackgroundColor);
						getSourceViewer().getTextWidget().setSelectionForeground(originalSelectionForegroundColor);
				}
			};
			getSelectionProvider().addSelectionChangedListener(selectionChangedListener);
		}
	}

	private void restoreOriginalCurrentSelectionBackgroundColor() {
		if (originalSelectionBackgroundColor!=null) {
			getSourceViewer().getTextWidget().setSelectionBackground(originalSelectionBackgroundColor);
			getSourceViewer().getTextWidget().setSelectionForeground(originalSelectionForegroundColor);
		}
	}

	public void selectAndReveal(int start, int length) {
		// Check the background color once for Prolog Debugger
		if (!checkBackground) {
			if (getSourceViewer().getTextWidget().getBackground().getRed() > 230 && 
				getSourceViewer().getTextWidget().getBackground().getGreen() > 230 &&
				getSourceViewer().getTextWidget().getBackground().getBlue() > 230)
				lightBackground = true;
			else
				lightBackground = false;
			checkBackground = true;
		}
		
		org.eclipse.ui.IEditorInput input = getEditorInput();
		if (input instanceof PrologEditorInput) {
			PrologEditorInput prologEditorInput = (PrologEditorInput) input;
			if (prologEditorInput.hasSelectionBackgroundColorProvider()) {
				ISourceViewer v = getSourceViewer();
				StyledText styledText= getSourceViewer().getTextWidget();
				rememberOriginalSelectionBackgroundColor();
				styledText.setSelectionBackground(prologEditorInput.getSelectionBackgroundColor());
				styledText.setSelectionForeground(prologEditorInput.getSelectionForegroundColor());
				if (lightBackground)
					styledText.setBackground(colorManager.getColor(new RGB(240,255,255)));
				else
					styledText.setBackground(colorManager.getColor(new RGB(255,255,255)));
			} 
			else {
				restoreOriginalCurrentSelectionBackgroundColor();
			}
		}
		listenerShouldIgnoreSelectionChange = true;
		super.selectAndReveal (start, length);
		listenerShouldIgnoreSelectionChange = false;
	}	
*/
}
