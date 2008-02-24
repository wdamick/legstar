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
package com.legstar.eclipse.plugin.cixsgen.jaxws.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.eclipse.plugin.cixsgen.jaxws.Activator;
import com.legstar.eclipse.plugin.cixsgen.jaxws.CixsGenPreferences;
import com.legstar.eclipse.plugin.common.LegstarReport;

import org.eclipse.ui.ide.IDE;


/**
 * Legacy Web Service new wizard. Its role is to create a new legacy web
 * service descriptor file in the provided container. If the container resource
 * (a folder or a project) is selected in the workspace when the wizard is
 * opened, it will accept it as the target container. The wizard creates one
 * file with the extension "cixs". The Web Service multi-page editor will then
 * be opened.
 */

public class NewWSWizard extends Wizard implements INewWizard {
	
	/** This wizard only has one page. */
	private NewWSWizardPage mPage;
	
	/** Current selection. */
	private IStructuredSelection mSelection;
	
	/** The model file extension on disk. */
	private static final String WSFILE_EXT = ".cixs";

	/** Page image location. */
	private static final String PAGE_IMG = "icons/legsemlogo.jpg";
	
	/** Error messages dialog boxes titles. */
	private static final String ERROR_TITLE = "Cixsgen Error";
	
	/** First part of error message for non existent container. */
	private static final String NO_CONTAINER_PART1 = "Container \"";
	
	/** Second part of error message for non existent container. */
	private static final String NO_CONTAINER_PART2 =
		"\" does not exist.";
	
	/** Monitor text when the CIXS file is created. */
	private static final String MONITOR_CIXS_CREATE =
		"Creating ";
	
	/** Monitor text when the CIXS file is opened. */
	private static final String MONITOR_CIXS_OPEN =
		"Opening file for editing...";
	
	/**
	 * Constructor for NewWSWizard.
	 */
	public NewWSWizard() {
		super();
		setNeedsProgressMonitor(true);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID, PAGE_IMG);
        setDefaultPageImageDescriptor(image);
	}
	
	/**
	 * Adding the page to the wizard.
	 */
	public final void addPages() {
		if (mSelection == null) {
			mSelection = new StructuredSelection();
		}
		mPage = new NewWSWizardPage(mSelection);
		addPage(mPage);
	}

	/**
	 * This method is called when 'Finish' button is pressed in
	 * the wizard. We will create an operation and run it
	 * using wizard as execution context.
	 * @return true if success
	 */
	public final boolean performFinish() {
		final String containerName = mPage.getContainerName();
		final String wsName = mPage.getWSName();
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(final IProgressMonitor monitor) 
					throws InvocationTargetException {
				try {
					doFinish(containerName, wsName, monitor);
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
			LegstarReport.logCoreException(realException, Activator.PLUGIN_ID);
			MessageDialog.openError(getShell(),
					ERROR_TITLE, realException.getClass().toString() + " "
					+ realException.getMessage());
			return false;
		}
		return true;
	}
	
	/**
	 * Worker method will save the file and then start an editor on it.
	 * @param containerName where the file should go
	 * @param serviceName the Web Service name
	 * @param monitor to follow progress
	 * @throws CoreException if something goes wrong
	 */
	private void doFinish(
		final String containerName,
		final String serviceName,
		final IProgressMonitor monitor)	throws CoreException {
		
		/* Store the web service description in a file */
		String fileName = serviceName + WSFILE_EXT;
		
		monitor.beginTask(MONITOR_CIXS_CREATE + fileName, 2);
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IResource resource = root.findMember(new Path(containerName));
		if (!resource.exists() || !(resource instanceof IContainer)) {
			LegstarReport.throwCoreException(
					NO_CONTAINER_PART1 + containerName + NO_CONTAINER_PART2,
					Activator.PLUGIN_ID);
		}
		IContainer container = (IContainer) resource;
		final IFile ifile = container.getFile(new Path(fileName));
		final File file =  ifile.getLocation().toFile();
		try {
			OutputStream os =
                new FileOutputStream(file, false);
			CixsJaxwsService service = new CixsJaxwsService();
			CixsGenPreferences cixsgenPref = new CixsGenPreferences();
			service.setName(serviceName);
			service.setPackageName(
					cixsgenPref.getCixsPackagePrefix() + '.' + serviceName);
			service.setTargetNamespace(
					cixsgenPref.getCixsNamespacePrefix() + '/' + serviceName);
            os.write(service.serialize().getBytes());
            os.close();
		} catch (IOException e) {
			LegstarReport.throwCoreException(e, Activator.PLUGIN_ID);
		}
		/* Make sure the project is refreshed as the file was created outside
	     * the Eclipse IDE. */
        container.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		monitor.worked(1);
		/* Schedule a task to display the generated file in the future */
		monitor.setTaskName(MONITOR_CIXS_OPEN);
		getShell().getDisplay().asyncExec(new Runnable() {
			public void run() {
				IWorkbenchPage page1 =
					PlatformUI.getWorkbench().
					getActiveWorkbenchWindow().getActivePage();
				try {
					IDE.openEditor(page1, ifile, true);
				} catch (PartInitException e) {
					return;
				}
			}
		});
		monitor.worked(1);
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
