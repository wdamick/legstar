/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;

import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.schemagen.COXBSchemaGenerator;
import com.legstar.eclipse.plugin.schemagen.Activator;

/**
 * This is a new XML schema wizard. Its role is to create a new
 * XML schema file in the provided container. If the container resource
 * (a folder or a project) is selected in the workspace 
 * when the wizard is opened, it will accept it as the target
 * container. The wizard creates one file with the extension
 * "xsd"..
 */

public class SchemaGenWizard extends Wizard implements INewWizard {
	
	/** First page in the wizard imports a COBOL file into the workspace. */
	private ImportCobolWizardPage mPageImportCobol;
	
	/** Second page in the wizard, creates the XML schema file. */
	private NewXSDWizardPage mPageCreateXSD;
	
	/** The current selection. */
	private IStructuredSelection mSelection;
	
	/** Page image location. */
	private static final String PAGE_IMG = "icons/legsemlogo.jpg";
	
	/**
	 * Constructor for NewXSDWizard.
	 * @throws CoreException if model cannot be loaded
	 */
	public SchemaGenWizard() throws CoreException {
		super();
		setNeedsProgressMonitor(true);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                		PAGE_IMG);
        setDefaultPageImageDescriptor(image);
	}
	
	/**
	 * Adding the pages to the wizard.
	 */
	@Override
	public final void addPages() {
		mPageImportCobol = new ImportCobolWizardPage(mSelection);
		/* For an unknown reason, this better be initialized */
		mPageImportCobol.setContainerFullPath(new Path("/Turlututu"));
		addPage(mPageImportCobol);
		mPageCreateXSD = new NewXSDWizardPage(mSelection);
		addPage(mPageCreateXSD);
	}

	/**
	 * This method is called when 'Finish' button is pressed in
	 * the wizard. We will create an operation and run it
	 * using wizard as execution context.
	 * {@inheritDoc}
	 */
	public final boolean performFinish() {
		/* First import the cobol file specified on page 1 into the workspace
		 * (replace any previous version). */
		if (!deleteCobolFile()) {
			return false;
		}
		IFile file = mPageImportCobol.createNewFile();
        if (file == null) {
            return false;
        }
		final String xsdCobolName = file.getLocation().toOSString();
		
		/* Get the output from the new XSD page. Take into account that the
		 * user might have pressed "finish" on the first page in which case
		 * fields are not yet initialized on the XSD page. */
		if (mPageCreateXSD.getContainerName() == null 
				|| mPageCreateXSD.getContainerName().length() == 0) {
			mPageCreateXSD.initFieldsFromCobolPage(mPageImportCobol);
		}
		final String xsdContainerName = mPageCreateXSD.getContainerName();
		final String xsdFileName = mPageCreateXSD.getFileName();
		final String xsdNamespace = mPageCreateXSD.getNamespace();
		
		/* Perform the generation per-se in a different thread, monitoring
		 * the progress. */
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(final IProgressMonitor monitor)
							throws InvocationTargetException {
				try {
					doFinish(xsdCobolName,
							xsdContainerName,
							xsdFileName,
							xsdNamespace,
							monitor);
				} catch (CoreException e) {
					throw new InvocationTargetException(e);
				} finally {
					monitor.done();
				}
			}
		};
		try {
			getContainer().run(true, false, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			Activator.logCoreException(realException);
			MessageDialog.openError(getShell(),
					"Error", realException.getClass().toString() + " "
					+ realException.getMessage());
			return false;
		}
		return true;
	}
	
	/**
	 * Remove any previous versions of the Cobol file from the container.
	 * @return false if deletion fails
	 */
	private boolean deleteCobolFile() {
        IPath containerPath = mPageImportCobol.getContainerFullPath();
        IPath newFilePath = containerPath.append(
        		mPageImportCobol.getFileName());
        IFile newFileHandle = ResourcesPlugin.getWorkspace().getRoot().getFile(
        		newFilePath);
        try {
			newFileHandle.delete(IResource.NONE, null);
		} catch (CoreException e) {
			Activator.logCoreException(e);
			MessageDialog.openError(getShell(),
					"Error", e.getClass().toString() + " "
					+ e.getMessage());
			return false;
		}
		return true;
	}
	
	/**
	 * The worker method. It will find the container, create the
	 * file if missing or just replace its contents, and open
	 * the editor on the newly created file.
	 * 
	 * @param xsdCobolName the COBOL file name
	 * @param xsdContainerName target container for XML schema
	 * @param xsdFileName XML schema file name
	 * @param xsdNamespace target namespace
	 * @param monitor for progress monitoring
	 * @throws CoreException if generation fails
	 */
	private void doFinish(
		final String xsdCobolName,
		final String xsdContainerName,
		final String xsdFileName,
		final String xsdNamespace,
		final IProgressMonitor monitor)
		throws CoreException {
		
		/* create a xsd file */
		monitor.beginTask("Creating " + xsdFileName, 2);
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IResource resource = root.findMember(new Path(xsdContainerName));
		if (!resource.exists() || !(resource instanceof IContainer)) {
			Activator.throwCoreException(
					"Container \"" + xsdContainerName + "\" does not exist.");
		}
		IContainer container = (IContainer) resource;
		final IFile file = container.getFile(new Path(xsdFileName));
		generateXSD(
				xsdCobolName, file.getLocation().toOSString(), xsdNamespace);

		/* Make sure the project is refreshed as the file was created outside
        *  theEclipse API. */
        container.refreshLocal(
            IResource.DEPTH_INFINITE, monitor);
		
		monitor.worked(1);
		monitor.setTaskName("Opening file for editing...");
		getShell().getDisplay().asyncExec(new Runnable() {
			public void run() {
				IWorkbenchPage page =
					PlatformUI.getWorkbench().
					getActiveWorkbenchWindow().getActivePage();
				try {
					IDE.openEditor(page, file, true);
				} catch (PartInitException e) {
					return;
				}
			}
		});
		monitor.worked(1);
	}
	
	/**
	 * XSD generation using Legstar XML to Schema classes.
	 * 
	 * @param cobolFileName fully qualified cobol file name
	 * @param xsdFileName fully qualified target XML schema file name
	 * @param xsdNamespace target namespace
	 */
	private void generateXSD(
			final String cobolFileName,
			final String xsdFileName,
			final String xsdNamespace) {
		
		COXBSchemaGenerator gen = new COXBSchemaGenerator();
		gen.setCobolFile(cobolFileName);
		gen.setXSDFile(xsdFileName);
		gen.setNamespace(xsdNamespace);
		gen.execute();
		
	}
	
	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 * Called by Eclipse to provide the wizard with information about the
	 *  workbench.
	 * Override to keep a handle to 
	 * {@inheritDoc}
	 */
	public final void init(
			final IWorkbench workbench,
			final IStructuredSelection selection) {
		mSelection = selection;
	}
}
