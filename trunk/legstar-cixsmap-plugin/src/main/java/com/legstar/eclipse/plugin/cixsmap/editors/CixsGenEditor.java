/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.eclipse.plugin.cixsmap.editors;


import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.ide.IDE;
import org.eclipse.jface.text.IDocument;

import com.legstar.cixs.gen.model.CixsMappingModel;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.eclipse.plugin.cixsmap.Activator;
import com.legstar.eclipse.plugin.cixsmap.dialogs.LegacyMappingComposite;

import java.io.ByteArrayOutputStream;

/**
 * Legacy mapping editor.:
 * <ul>
 * <li>page 0 contains a nested text editor.
 * <li>page 1 has a mapping dialog
 * </ul>
 */
public class CixsGenEditor
		extends MultiPageEditorPart
		implements IResourceChangeListener {

	/** The text editor used in page 0. */
	private TextEditor editor;

	/** The composite content of the second page. */
	private LegacyMappingComposite mMappingComposite;

	/** The current Web Service name. */
	private String mMappingName;

	/** The legacy Web Service name descriptor file. */
	private IFile mMappingFile;
	
	/**
	 * Creates a multi-page editor.
	 */
	public CixsGenEditor() {
		super();
		ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.part.MultiPageEditorPart#createPages()
	 */
	/**
	 * Creates the pages of the multi-page editor.
	 */
	protected final void createPages() {
		createTextEditorPage();
		createOperationsPage();
		setActivePage(1);
	}

	/**
	 * Creates operations page of the multi-page editor,
	 * which shows a dialog to setup operations.
	 */
	private void createOperationsPage() {

		CixsMappingModel mapping = null;
		try {
			mapping = loadModel();
		} catch (CoreException e) {
            errorDialog("loading model", e);
			return;
		}

		mMappingComposite = new LegacyMappingComposite(
				getContainer(), SWT.NONE, mapping, mMappingFile);

		mMappingComposite.addListener(
		        LegacyMappingComposite.CHG_EVENT, new Listener() {
			public void handleEvent(final Event event) {
				try {
					saveModel();
				} catch (CoreException e) {
				    errorDialog("saving model", e);
				}
			}
		});
		
		mMappingComposite.addListener(
		        LegacyMappingComposite.GEN_EVENT, new Listener() {
			public void handleEvent(final Event event) {
				try {
					generate();
				} catch (CoreException e) {
                    errorDialog("generating", e);
				}
			}
		});
		
		int index = addPage(mMappingComposite);
		setPageText(index, "Operations");
	}

	/**
	 * Creates text editor page of the multi-page editor,
	 * which contains a text editor.
	 */
	private void createTextEditorPage() {
		try {
			editor = new TextEditor();
			int index = addPage(editor, getEditorInput());
			setPageText(index, editor.getTitle());
		} catch (PartInitException e) {
            errorDialog("creating nested text editor", e);
		}
	}

	/**
	 * The <code>MultiPageEditorPart</code> implementation of this 
	 * <code>IWorkbenchPart</code> method disposes all nested editors.
	 * Subclasses may extend.
	 */
	public final void dispose() {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
		super.dispose();
	}

	/**
	 * Saves the multi-page editor's document.
	 * @param monitor for progress
	 */
	public final void doSave(final IProgressMonitor monitor) {
		getEditor(0).doSave(monitor);
	}

	/**
	 * Saves the multi-page editor's document as another file.
	 * Also updates the text for page 1's tab, and updates this multi-page
	 * editor's input to correspond to the nested editor's.
	 */
	public final void doSaveAs() {
		IEditorPart editor1 = getEditor(0);
		editor1.doSaveAs();
		setPageText(0, editor1.getTitle());
		setInput(editor1.getEditorInput());
	}

	/**
	 * Declared in IEditorPart.
	 * @param marker where we need to go
	 */
	public final void gotoMarker(final IMarker marker) {
		setActivePage(0);
		IDE.gotoMarker(getEditor(0), marker);
	}

	/**
	 * The <code>WsGenEditor</code> implementation of this method
	 * checks that the input is an instance of <code>IFileEditorInput</code>.
	 * @see org.eclipse.ui.part.MultiPageEditorPart#init(
	 * org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 * @param site
	 *            The site for which this part is being created; must not be
	 *            <code>null</code>.
	 * @param editorInput
	 *            The input on which this editor should be created; must not be
	 *            <code>null</code>.
	 * @throws PartInitException
	 *             If the initialization of the part fails.
	 */
	public final void init(
			final IEditorSite site,
			final IEditorInput editorInput)
			throws PartInitException {
		
		if (!(editorInput instanceof IFileEditorInput)) {
			throw new PartInitException(
					"Invalid Input: Must be IFileEditorInput");
		}
		super.init(site, editorInput);
		FileEditorInput filee = (FileEditorInput) getEditorInput();
		mMappingFile = filee.getFile();
		mMappingName = filee.getName();
		int dotLoc = mMappingName.lastIndexOf('.');
		if (dotLoc != -1) {
			mMappingName = mMappingName.substring(0, dotLoc);
		}
	}
	
	
	/**
	 * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
     * @return <code>true</code> if "Save As" is supported, and 
     * <code>false</code> if not supported
	 */
	public final boolean isSaveAsAllowed() {
		return true;
	}
	
	/**
	 * Calculates the contents of page 2 when it is activated.
	 * @see org.eclipse.ui.part.MultiPageEditorPart#pageChange(int)
	 * @param newPageIndex
	 *            the index of the activated page
	 */
	protected final void pageChange(
			final int newPageIndex) {
		super.pageChange(newPageIndex);
		/* When switching to operations page from editor page, make sure
		 * the model reflects the latest changes. */
		if (newPageIndex == 1) {
			try {
				mMappingComposite.resetOperations(loadModel());
			} catch (CoreException e) {
	            errorDialog("loading document model", e);
			}
		}
	}

	/**
	 * Creates a properties model file from the content on the text editor.
	 * @return the new properties model file
	 * @throws CoreException if model can't be loaded
	 */
	private CixsMappingModel loadModel() throws CoreException {
		CixsMappingModel mappingModel = new CixsMappingModel();
		String editorText =
			editor.getDocumentProvider().getDocument(
					editor.getEditorInput()).get();
		if (editorText != null && editorText.length() > 0) {
			try {
				mappingModel.load(editorText);
			} catch (CixsModelException e) {
			    throwCoreException(e);
			}
		}
		return mappingModel;
	}

	/**
	 * Saves the text editor content in the model file.
	 * @throws CoreException if storage fails
	 */
	private void saveModel() throws CoreException {
		IDocument edoc = editor.getDocumentProvider().getDocument(
				editor.getEditorInput());
		ByteArrayOutputStream os =
			new ByteArrayOutputStream();
		try {
			edoc.set(mMappingComposite.getMappingModel().serialize());
			os.close();
		} catch (IOException e) {
            throwCoreException(e);
		}
	}

	/**
	 * Initiate the endpoint generation process.
	 * @throws CoreException if generation fails
	 */
	private void generate() throws CoreException {
		doSave(null);

	}
	
	/**
	 * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(
	 * org.eclipse.core.resources.IResourceChangeEvent)
	 * @param event the resource change event
	 */
	public final void resourceChanged(
			final IResourceChangeEvent event) {
		if (event.getType() == IResourceChangeEvent.PRE_CLOSE) {
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					IWorkbenchPage[] pages =
						getSite().getWorkbenchWindow().getPages();
					for (int i = 0; i < pages.length; i++) {
						if (((FileEditorInput) editor.getEditorInput()).
								getFile().
								getProject().equals(event.getResource())) {
							IEditorPart editorPart = pages[i].findEditor(
									editor.getEditorInput());
							pages[i].closeEditor(editorPart, true);
						}
					}
				}            
			});
		}
	}
	
	/**
	 * Report a core exception.
     * @param context what we were trying to do
	 * @param e the core exception
	 */
	private void errorDialog(final String context, final CoreException e) {
        ErrorDialog.openError(getSite().getShell(),
                "Error while " + context + ": " + e.getMessage(),
                null, e.getStatus());
	}

    /**
     * Create a formatted core exception.
     * @param e an exception
     * @throws CoreException the core exception
     */
    private void throwCoreException(final Exception e) throws CoreException {
        IStatus status =
            new Status(IStatus.ERROR,
            		Activator.PLUGIN_ID,
            		IStatus.OK,
            		e.getMessage(),
            		e);
        Activator.getDefault().getLog().log(status);
        throw new CoreException(status);
    }

}
