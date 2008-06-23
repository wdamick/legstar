package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import com.legstar.codegen.models.AbstractAntBuildModel;
import com.legstar.codegen.models.IAntBuildModel;
import com.legstar.eclipse.ant.AntLaunchException;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardRunnable;

/**
 * Background task that performs the actual XSD generation. The process
 * involves 3 steps:
 * <ul>
 *  <li>Build an ant script file using a velocity template</li>
 *  <li>Launching the ant script as a background process</li>
 *  <li>Opening the default editor for Xsd files</li>
 * </ul>
 */
public abstract class AbstractToXsdWizardRunnable
		extends AbstractWizardRunnable {

	/** The target xsd file name. */
	private String mTargetXsdFileName;

	/** Monitor text when XSD generation starts. */
	private static final String MONITOR_XSD_GENERATION_START =
		"Generating ANT script for XML Schema generation";

	/** Monitor text when opening editor on Xsd file. */
	private static final String MONITOR_OPEN_EDITOR_START =
		"Opening file for editing...";

	/** The wizard submitting this runnable. */
	private IWizard mWizard;
	
	/** Where the generated Xsd will go. */
	private String mTargetContainer;

	/** Part of the ant script file name generated. Allows segregating
	 * this ant file from the ones produced by other LegStar wizards. */
	private static final String ANT_FILE_NAME_ID = "schemagen-";

	/**
	 * Instantiate this runnable from UI items. It is important not to attempt
	 * access to UI elements from the background thread.
	 * @param antBuildModel the model object to be passed to velocity templates
	 * @param mainPage the page holding targets
	 * @throws InvocationTargetException if runnable cannot be instantiated
	 */
	public AbstractToXsdWizardRunnable(
			final IAntBuildModel antBuildModel,
			final MainWizardPage mainPage)
	throws InvocationTargetException {
		super(antBuildModel,
				mainPage.getTargetContainerText().getText(),
				getAntScriptFileName(mainPage.getTargetFileText().getText())
				);
		mTargetXsdFileName = mainPage.getTargetFileText().getText();
		mWizard = mainPage.getWizard();
		mTargetContainer = mainPage.getTargetContainerText().getText();
	}

	/** {@inheritDoc} */
	public void run(final IProgressMonitor monitor)
	throws InvocationTargetException, InterruptedException {
		int scale = 1;
		/* 4 tasks because launcher counts as a hidden one*/
		monitor.beginTask(MONITOR_XSD_GENERATION_START + getTargetXsdFileName(),
				4 * scale);
		try {
			/* 1. Create the ant build */
			createBuild(monitor, scale);

			/* 2 & 3. Execute the generated build.*/
			runBuild(monitor, scale);

			/* 4. Open an editor on the generated XML schema. */
			editXsdFile(monitor, scale);

		} finally {
			monitor.done();
		}
	}

	/**
	 * Make sure the project is refreshed as the files were created outside
	 * the Eclipse API.
	 * Request the workbench to open an editor on the newly created
	 * Xsd file.
	 * @param monitor the current monitor
	 * @param scale the scale of progress
	 * @throws InvocationTargetException execution fails
	 */
	protected void editXsdFile(
			final IProgressMonitor monitor,
			final int scale) throws InvocationTargetException {
		monitor.setTaskName(MONITOR_OPEN_EDITOR_START);
		try {
			((IContainer) getProject(mTargetContainer)).refreshLocal(
					IResource.DEPTH_INFINITE,
					new SubProgressMonitor(monitor, 1 * scale));
			if (getXsdFile() == null) {
				Throwable th = new AntLaunchException("Xsd file "
						+ getTargetXsdFileName() + " was not generated."
						+ " Check ant script log for a failure.");
				throw new InvocationTargetException(th);
			}
			Shell shell = mWizard.getContainer().getShell();
			shell.getDisplay().asyncExec(new Runnable() {
				public void run() {
					IWorkbenchPage page =
						PlatformUI.getWorkbench().
						getActiveWorkbenchWindow().getActivePage();
					try {
						IDE.openEditor(page, getXsdFile(), true);
					} catch (PartInitException e) {
						return;
					}
				}
			});
		} catch (CoreException e) {
			throw new InvocationTargetException(e);
		}
	}

	
	/**
	 * Create a valid location for the generated Xsd file. The target
	 * container name is always expected to start with a project name
	 * as the first segment of its path.
	 * @return an existing location
	 */
	protected String getTargetXsdLocation() {
		IPath projectPath = getProject(mTargetContainer).getLocation();
		IPath xsdPath = projectPath.append(
				(new Path(mTargetContainer)).removeFirstSegments(1));
		mkDir(xsdPath);
		return xsdPath.toOSString();
	}

	/**
	 * @return the generated Xsd file ready to be edited
	 */
	protected IFile getXsdFile() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath projectPath = new Path(mTargetContainer);
		IPath xsdPath = projectPath.append(getTargetXsdFileName());
		IResource resource = root.findMember(xsdPath);
		return (IFile) resource;
	}

	/**
	 * @return the Xsd file as a file system object
	 */
	protected File getXsdFileFile() {
		String folder = getTargetXsdLocation();
		String xsdPath = folder + File.separator + getTargetXsdFileName();
		return new File(xsdPath);
	}

	/**
	 * @return the target Xsd file name (not a path)
	 */
	protected String getTargetXsdFileName() {
		return mTargetXsdFileName;
	}

	/**
	 * Generate a valid ant script file name.
	 * @param xsdFileName the source xsd file name.
	 * @return the ant script file name
	 */
	private static String getAntScriptFileName(final String xsdFileName) {
		return AbstractAntBuildModel.ANT_FILE_PREFIX
		+ ANT_FILE_NAME_ID
		+ xsdFileName + '.'
		+ AbstractAntBuildModel.ANT_FILE_SUFFIX;	
	}
}
