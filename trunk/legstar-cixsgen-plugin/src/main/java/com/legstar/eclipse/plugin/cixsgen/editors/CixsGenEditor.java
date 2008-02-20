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
package com.legstar.eclipse.plugin.cixsgen.editors;


import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
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
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;

import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.eclipse.plugin.cixsgen.Activator;
import com.legstar.eclipse.plugin.cixsgen.AntCreationException;
import com.legstar.eclipse.plugin.cixsgen.CixsGenDescriptor;
import com.legstar.eclipse.plugin.cixsgen.CixsGenDriver;
import com.legstar.eclipse.plugin.cixsgen.dialogs.LegacyWebServiceDialog;
import com.legstar.eclipse.plugin.common.LegstarReport;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.InvocationTargetException;

/**
 * Legacy Web Service description editor.:
 * <ul>
 * <li>page 0 contains a nested text editor.
 * <li>page 1 has an service maintenance dialog
 * </ul>
 */
public class CixsGenEditor
		extends MultiPageEditorPart
		implements IResourceChangeListener {

	/** The text editor used in page 0. */
	private TextEditor editor;

	/** The composite content of the second page. */
	private LegacyWebServiceDialog mServiceDialog;

	/** Set of properties for the ant script. */
	private CixsGenDescriptor mCixsGenDescriptor;
	
	/** The Java project holding the service descriptor. */
	private IProject mProject;

	/** Project location on disk. */
	private String mProjectLocation;
	
	/** Workspace location on disk. */
	private String mWorkspaceLocation;
	
	/** The current Web Service name. */
	private String mServiceName;

	/** The legacy Web Service name descriptor file. */
	private IFile mServiceFile;
	
	/** The folder where endpoint need to be generated. */
	private String mCixsSrcDir;

	/** The folder where binaries are generated. */
	private String mCixsBinDir;
	
	/** Event type corresponding to an update on operation dialog composite. */
	public static final int CHG_EVENT = 1035;

	/** Event type corresponding to a request to generate the endpoint. */
	public static final int GEN_EVENT = 1036;

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

		CixsJaxwsService service = null;
		try {
			service = loadModel();
		} catch (CoreException e) {
			ErrorDialog.openError(
					getSite().getShell(),
					"Error loading model: " + e.getMessage(),
					null,
					e.getStatus());
			return;
		}

		mServiceDialog = new LegacyWebServiceDialog(
				getContainer(), SWT.NONE, service, mServiceFile);
		mServiceDialog.addListener(CHG_EVENT, new Listener() {
			public void handleEvent(final Event event) {
				try {
					saveModel();
				} catch (CoreException e) {
					ErrorDialog.openError(
							getSite().getShell(),
							"Error saving model: " + e.getMessage(),
							null,
							e.getStatus());
				}
			}

		});
		mServiceDialog.addListener(GEN_EVENT, new Listener() {
			public void handleEvent(final Event event) {
				try {
					generate();
				} catch (CoreException e) {
					ErrorDialog.openError(
							getSite().getShell(),
							"Error generating: " + e.getMessage(),
							null,
							e.getStatus());
				}
			}

		});
		
		int index = addPage(mServiceDialog);
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
			ErrorDialog.openError(
					getSite().getShell(),
					"Error creating nested text editor",
					null,
					e.getStatus());
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
	 *             If the initialization of the part fails -- currently never.
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
		mServiceFile = filee.getFile();
		mServiceName = filee.getName();
		int dotLoc = mServiceName.lastIndexOf('.');
		if (dotLoc != -1) {
			mServiceName = mServiceName.substring(0, dotLoc);
		}
		/* Extract the workspace location, assuming a classical project*/
		mProject = mServiceFile.getProject();
		mProjectLocation = trimPath(mProject.getLocationURI().getPath());
		mWorkspaceLocation = trimLastSegment(mProjectLocation);
		introspectProject();
	}
	
	/**
	 * Examine the project containing the edited file and derive locations
	 * for base project, java sources and binaries.
	 */
	private void introspectProject() {
		
 		IJavaProject jproject = JavaCore.create(mProject);
	
		if (jproject != null) {
			/* If this is a java project, get Java source and output folders 
			 * (assuming there is only one) */
			try {
				IClasspathEntry[]  cpe = jproject.getRawClasspath();
				for (int i = 0; i < cpe.length; i++) {
					if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
						mCixsSrcDir = mWorkspaceLocation
						+ cpe[i].getPath().toOSString();
						if (cpe[i].getOutputLocation() == null) {
							mCixsBinDir = mWorkspaceLocation
							+ jproject.getOutputLocation().toOSString();
						} else {
							mCixsBinDir = mWorkspaceLocation
							+ cpe[i].getOutputLocation().toOSString();
						}
						return;
					}
				}
			} catch (JavaModelException e) {
				e.getCause();
			}
		}
		/* If everything else failed, assume generated artifacts will go to the 
		 * project root. */
		mCixsSrcDir = mProjectLocation;
		mCixsBinDir = mProjectLocation;
	}
	
	/**
	 * Remove any trailing slashes.
	 * @param path the path to trim
	 * @return the trimmed name
	 */
	private String trimPath(final String path) {
		String newPath = null;
		if (path == null) {
			return newPath;
		}
		newPath = path;
		if (newPath.length() < 2) {
			return newPath;
		}
		if (newPath.charAt(newPath.length() - 1) == '/' 
			|| newPath.charAt(newPath.length() - 1) == '\\') {
			newPath = newPath.substring(0, newPath.length() - 1);
		}
		return newPath;
	}
	
	/**
	 * Removes the last segment from a path if any.
	 * @param path the path to remove segment from
	 * @return the new path
	 */
	private String trimLastSegment(final String path) {
		String newPath = null;
		if (path == null) {
			return newPath;
		}
		newPath = path;
		int slashLoc = newPath.lastIndexOf('/');
		if (slashLoc != -1) {
			newPath = newPath.substring(0, slashLoc);
			return newPath;
		}
		slashLoc = newPath.lastIndexOf('\\');
		if (slashLoc != -1) {
			newPath = newPath.substring(0, slashLoc);
			return newPath;
		}
		return newPath;
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
				mServiceDialog.resetOperations(loadModel());
			} catch (CoreException e) {
				ErrorDialog.openError(
						getSite().getShell(),
						"Error loading document model",
						null,
						e.getStatus());
			}
		}
	}

	/**
	 * Creates a properties model file from the content on the text editor.
	 * @return the new properties model file
	 * @throws CoreException if model can't be loaded
	 */
	private CixsJaxwsService loadModel() throws CoreException {
		CixsJaxwsService service = new CixsJaxwsService();
		String editorText =
			editor.getDocumentProvider().getDocument(
					editor.getEditorInput()).get();
		if (editorText != null && editorText.length() > 0) {
			try {
				service.load(editorText);
			} catch (CixsModelException e) {
				LegstarReport.throwCoreException(e, Activator.PLUGIN_ID);
			}
		}
		return service;
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
			edoc.set(mServiceDialog.getService().serialize());
			os.close();
		} catch (IOException e) {
			LegstarReport.throwCoreException(e, Activator.PLUGIN_ID);
		}
	}

	/**
	 * Initiate the endpoint generation process.
	 * @throws CoreException if generation fails
	 */
	private void generate() throws CoreException {
		doSave(null);

		/* Create a set of generation parameters */
		try {
			createGenDescriptor();
		} catch (AntCreationException e) {
			MessageDialog.openError(getSite().getShell(),
					"Error", e.getMessage());
			LegstarReport.logCoreError(e.getMessage(),
					Activator.PLUGIN_ID);
			return;
		}

		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(final IProgressMonitor monitor)
					throws InvocationTargetException {
				try {
					doGenerate(
							mServiceDialog.getService(),
							mCixsGenDescriptor, monitor);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			new ProgressMonitorDialog(getSite().getShell()).run(true, true, op);
		} catch (InterruptedException e) {
			return;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			MessageDialog.openError(getSite().getShell(),
					"Error", realException.getClass().toString() + " "
					+ realException.getMessage());
			LegstarReport.logCoreException(realException, Activator.PLUGIN_ID);
		}
	}
	
	/**
	 * Populates a descriptor object. It is assumed Cixs
	 * Jaxb and Custom code all go to the same binary folder.
	 * @throws AntCreationException if properties are missing
	 */
	private void createGenDescriptor() throws AntCreationException {
		mCixsGenDescriptor = new CixsGenDescriptor();
		mCixsGenDescriptor.setCixsSourcesDir(mCixsSrcDir);
		mCixsGenDescriptor.setCixsBinariesDir(mCixsBinDir);
		mCixsGenDescriptor.setCixsJaxbBinariesDir(mCixsBinDir);
		mCixsGenDescriptor.setCixsCustBinariesDir(mCixsBinDir);
		mCixsGenDescriptor.setCixsAntScriptsDir(
				makeAbsolute(mCixsGenDescriptor.getCixsAntScriptsDir()));
		mCixsGenDescriptor.setCixsWebDescriptorsDir(
				makeAbsolute(mCixsGenDescriptor.getCixsWebDescriptorsDir()));
		mCixsGenDescriptor.setCixsPropertiesDir(
				makeAbsolute(mCixsGenDescriptor.getCixsPropertiesDir()));
	}
	
	/**
	 * Appends the project location if path is not absolute.
	 * @param path the path to check
	 * @return the absolute path
	 */
	private String makeAbsolute(final String path) {
		String newPath = path;
		Path p = new Path(path);
		if (!p.isAbsolute()) {
			Path w = new Path(mProjectLocation);
			newPath = w.append(p).toOSString();
		}
		/* Make sure this folder exists */
		File folder = new File(newPath);
		if (!folder.exists()) {
			folder.mkdirs();
		}
		return newPath;
	}

	/**
	 * Generation process per se.
	 * @param service service descriptor
	 * @param cixsAntPropMap ant script properties
	 * @param monitor progress monitor
	 * @throws CoreException if generation fails
	 */
	private void doGenerate(
			final CixsJaxwsService service,
			final CixsGenDescriptor cixsAntPropMap,
			final IProgressMonitor monitor) throws CoreException {

		monitor.beginTask(
				"Generating web service endpoint for " + mServiceName, 2);
		monitor.worked(1);
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IContainer[] containers = root.findContainersForLocation(
				new Path(mCixsSrcDir));
		if (containers.length == 0) {
			LegstarReport.throwCoreException(
					"Container \"" + mCixsSrcDir + "\" does not exist.",
					Activator.PLUGIN_ID);
		}
		IContainer container = containers[0];
		try {
			CixsGenDriver gen = new CixsGenDriver();
			gen.generate(mServiceName,
					     cixsAntPropMap,
					     service,
					     monitor);
		} catch (AntCreationException e) {
			LegstarReport.throwCoreException(e, Activator.PLUGIN_ID);
		}
		/* Make sure the project is refreshed as the files are created outside
		 * the Eclipse IDE. */
		container.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		monitor.worked(1);
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


}
