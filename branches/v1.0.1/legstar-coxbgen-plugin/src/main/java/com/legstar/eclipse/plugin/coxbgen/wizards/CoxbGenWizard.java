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
package com.legstar.eclipse.plugin.coxbgen.wizards;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;

import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.common.LegstarAntRunner;
import com.legstar.eclipse.plugin.common.LegstarReport;
import com.legstar.eclipse.plugin.coxbgen.CoxbgenPreferences;

import java.util.Map;
import java.util.HashMap;


/**
 * This wizard role is to create annotated JAXB classes from an annotated
 * XML schema and then generate binding classes.
 */

public class CoxbGenWizard extends Wizard implements INewWizard {
	
	/** The main page of controls. */
	private CoxbGenWizardPage page;
	
	/** The current workbench selection. */
	private ISelection selection;
	
	/** The current XML schema file. */
	private IFile mXsdFile = null;

	/** Set of common preferences. */
	private CoxbgenPreferences mCoxbgenPref;
	
	/** Relative path to JAXB classes generation XSLT. */
	private static final String JAXB_XSL_LOC = "/ant/build-jaxb.xml";
	
	/** Relative path to COXB classes generation XSLT. */
	private static final String COXB_XSL_LOC = "/ant/build-coxb.xml";
	
	/** Target for JAXB generation (compilation is necessary because COXB
	 * will perform reflection on JAXB generated classes). */
	private static final String JAXB_TARGET = "compileJAXB";
	
	/** Target for COXB generation (avoid compilation). */
	private static final String COXB_TARGET = "generateCOXB";
	
	/** Error message if Coxbgen location has not been set. */
	private static final String GEN_NOT_SET = "Coxbgen location not set in "
		+ "preferences. Use Window-->Preferences to set the LegStar COXB "
		+ "Generator location to a valid Coxbgen install folder (Coxbgen must "
		+ "have been installed prior to using this plugin).";
	
	/** Page image location. */
	private static final String PAGE_IMG = "icons/legsemlogo.jpg";
	
	/** Error messages dialog boxes titles. */
	private static final String ERROR_TITLE = "Coxbgen Error";
	
	/** Monitor text when JAXB generation starts. */
	private static final String MONITOR_JAXB_START =
		"Generating JAXB classes for ";
	
	/** Monitor text when COXB generation starts. */
	private static final String MONITOR_COXB_START =
		"Generating COXB classes for ";
	
	/** First part of error message for non existent container. */
	private static final String NO_CONTAINER_PART1 = "Container \"";
	
	/** Second part of error message for non existent container. */
	private static final String NO_CONTAINER_PART2 =
		"\" does not exist.";
	
	/** Ant script XML Schema file name. */
	private static final String ANT_XSD_FILE = "xsd.file";

	/** Ant script JAXB Sources target location property name. */
	private static final String ANT_JAXB_SRC_DIR = "jaxb.src.dir";

	/** Ant script JAXB binaries target location property name. */
	private static final String ANT_JAXB_BIN_DIR = "jaxb.bin.dir";

	/**
	 * Constructor for CoxbGenWizard.
	 * @throws CoreException if initialization goes wrong 
	 */
	public CoxbGenWizard() throws CoreException {
		super();
		initialize();
		mXsdFile = null;
	}
	
	/**
	 * Constructor for CoxbGenWizard.
	 * @param xsdFile an XML schema file
	 * @throws CoreException if initialization goes wrong 
	 */
	public CoxbGenWizard(final IFile xsdFile) throws CoreException {
		super();
		initialize();
		mXsdFile = xsdFile;
	}
	
	/**
	 * Setup Wizard environment.
	 * @throws CoreException if setup fails
	 */
	private void initialize() throws CoreException {
		setNeedsProgressMonitor(true);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID, PAGE_IMG);
        setDefaultPageImageDescriptor(image);
        mCoxbgenPref = new CoxbgenPreferences();
	}
	
	/**
	 * Adding the page to the wizard.
	 */

	public final void addPages() {
		page = new CoxbGenWizardPage(selection, mXsdFile);
		addPage(page);
	}

	/**
	 * This method is called when 'Finish' button is pressed in the wizard.
	 * We will create an operation and run it using wizard as execution context.
	 * @return true if processing went fine
	 */
	public final boolean performFinish() {
		final IFile xsdFile = page.getXsdFile();
		final String [] rootNames = page.getRootName();
		final String srcdirName = page.getSrcDirName();
		final String bindirName = page.getBinDirName();
		final String jaxbPackageName = page.getJaxbPackageName();
		final String coxbgenDir = mCoxbgenPref.getCoxbgenLocation();
		
		if (coxbgenDir == null || coxbgenDir.length() == 0) {
			MessageDialog.openError(getShell(),
					ERROR_TITLE, GEN_NOT_SET);
			LegstarReport.logCoreError(GEN_NOT_SET,
					Activator.PLUGIN_ID);
			return false;
		}
		
		IRunnableWithProgress op = new IRunnableWithProgress() {
			public void run(final IProgressMonitor monitor)
				throws InvocationTargetException {
				try {
					doFinish(xsdFile, rootNames, srcdirName,
							bindirName, jaxbPackageName, monitor);
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
	 * The worker method. It will collect the bialog data and create an
	 * ebvironment for an ANT script to generate the target classes.
	 * @param xsdFile The XML schema file
	 * @param rootNames the root type names
	 * @param srcdirName the target source directory
	 * @param bindirName the target binary directory
	 * @param jaxbPackageName the target jaxb package name
	 * @param monitor to follow progress
	 * @throws CoreException if something goes wrong
	 */
	private void doFinish(
		final IFile xsdFile,
		final String [] rootNames,
		final String srcdirName,
		final String bindirName,
		final String jaxbPackageName,
		final IProgressMonitor monitor)
		throws CoreException {
		
		int scale = 1000;
		monitor.beginTask(MONITOR_JAXB_START
				+ xsdFile.getName(), (1 + rootNames.length) * scale);
		LegstarAntRunner antRunner = new LegstarAntRunner();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		
		/* Check that we have a valid source container to store our files */
		IResource resource = root.findMember(new Path(srcdirName));
		if (!resource.exists() || !(resource instanceof IContainer)) {
			LegstarReport.throwCoreException(NO_CONTAINER_PART1 + srcdirName
					+ NO_CONTAINER_PART2, Activator.PLUGIN_ID);
		}
		IContainer srcdir = (IContainer) resource;

		/* Check that we have a valid binary container to store our files */
		resource = root.findMember(new Path(bindirName));
		if (!resource.exists() || !(resource instanceof IContainer)) {
			LegstarReport.throwCoreException(NO_CONTAINER_PART1 + bindirName
					+ NO_CONTAINER_PART2, Activator.PLUGIN_ID);
		}
		IContainer bindir = (IContainer) resource;

		/* Parameters expected by ANT task */
		Map < String, String > properties = new HashMap < String, String >();
		properties.put(ANT_XSD_FILE, xsdFile.getLocation().toOSString());
		properties.put(ANT_JAXB_SRC_DIR, srcdir.getLocation().toOSString());
		properties.put(ANT_JAXB_BIN_DIR, bindir.getLocation().toOSString());
		
		/* Generate the JAXB classes */
		String[] jaxbTargets = {JAXB_TARGET};
		antRunner.run(
				mCoxbgenPref.getCoxbgenLocation() + JAXB_XSL_LOC,
				jaxbTargets,
				properties,
				monitor,
				scale);
		
		/* Generate the binding classes */
		monitor.setTaskName(MONITOR_COXB_START	+ xsdFile.getName());
		
		for (int i = 0; i < rootNames.length; i++) {
			String[] coxbTargets = {COXB_TARGET};
			properties.put("jaxb.root.name", rootNames[i]);
			antRunner.run(mCoxbgenPref.getCoxbgenLocation() + COXB_XSL_LOC,
					coxbTargets,
					properties,
					monitor,
					scale);
		}
		
		/* Make sure the project is refreshed as the files were created outside
		 * the Eclipse API. */
        srcdir.refreshLocal(
            IResource.DEPTH_INFINITE, monitor);

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
			final IStructuredSelection sel) {
		this.selection = sel;
	}
	
}
