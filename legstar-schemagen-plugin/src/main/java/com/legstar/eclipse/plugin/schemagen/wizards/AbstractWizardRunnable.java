package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.osgi.framework.Bundle;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.IAntBuildModel;
import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.preferences.PreferenceConstants;
import com.legstar.eclipse.plugin.schemagen.util.AntLaunchException;
import com.legstar.eclipse.plugin.schemagen.util.AntLaunchHelper;

/**
 * Background task that performs the actual XSD generation. The process
 * involves 3 steps:
 * <ul>
 *  <li>Build an ant script file using a velocity template</li>
 *  <li>Launching the ant script as a background process</li>
 *  <li>Opening the default editor for Xsd files</li>
 * </ul>
 */
public abstract class AbstractWizardRunnable implements IRunnableWithProgress {

	/** The model to build from. */
	private IAntBuildModel mAntBuildModel;

	/** The target container name. */
	private String mTargetContainerName;

	/** The target xsd file name. */
	private String mTargetXsdFileName;

	/** Monitor text when XSD generation starts. */
	private static final String MONITOR_XSD_GENERATION_START =
		"Generating ANT script for XML Schema generation";

	/** Monitor text when ant build generation starts. */
	private static final String MONITOR_ANT_GENERATION_START =
		"Generating ANT script...";

	/** Monitor text when ant execute starts. */
	private static final String MONITOR_ANT_RUN_START =
		"Running ANT script...";

	/** Monitor text when opening editor on Xsd file. */
	private static final String MONITOR_OPEN_EDITOR_START =
		"Opening file for editing...";

	/**  Suffix of generated ANT script file. */
	private static final String ANT_FILE_SUFFIX = "xml";

	/** Prefix of generated ANT script file. */
	private static final String ANT_FILE_PREFIX = "build-";

	/** The wizard submitting this runnable. */
	private IWizard mWizard;

	/** Error message if product location has not been set. */
	public static final String PRODUCT_LOCATION_NOT_SET =
		"Product location not set in "
		+ "preferences. Use Window-->Preferences to set the LegStar product "
		+ "location to a valid install folder (LegStar must "
		+ "have been installed prior to using this plugin).";

	/**
	 * Instantiate this runnable from UI items. It is important not to attempt
	 * access to UI elements from the background thread.
	 * @param antBuildModel the model object to be passed to velocity templates
	 * @param mainPage the page holding targets
	 * @throws InvocationTargetException if runnable cannot be instantiated
	 */
	public AbstractWizardRunnable(
			final IAntBuildModel antBuildModel,
			final MainWizardPage mainPage)
	throws InvocationTargetException {
		mAntBuildModel = antBuildModel;
		mTargetContainerName = mainPage.getTargetContainerText().getText();
		mTargetXsdFileName = mainPage.getTargetFileText().getText();
		mWizard = mainPage.getWizard();
	}

	/** {@inheritDoc} */
	public void run(final IProgressMonitor monitor)
	throws InvocationTargetException, InterruptedException {
		int scale = 1;
		monitor.beginTask(MONITOR_XSD_GENERATION_START + getTargetXsdFileName(),
				3 * scale);
		try {
			/* 1. Create the ant build */
			String scriptFileName = createBuild(monitor, scale);

			/* 2. Execute the generated build */
			runBuild(scriptFileName, monitor, scale);

			/* 3. Open an editor on the generated XML schema. */
			editXsdFile(monitor, scale);

		} finally {
			monitor.done();
		}
	}

	/**
	 * Create a new ANT script file (replace existing one). We create this
	 * one directly under the target container.
	 * @param monitor the current monitor
	 * @param scale the scale of progress
	 * @return a path to the generated script
	 * @throws InvocationTargetException if creation fails
	 */
	protected String createBuild(
			final IProgressMonitor monitor,
			final int scale) throws InvocationTargetException {
		monitor.setTaskName(MONITOR_ANT_GENERATION_START);
		File scriptFile;
		try {
			scriptFile = new File(
					getTargetAntScriptLocation().toOSString()
					+ File.separatorChar 
					+ getTargetAntFileName());

			mAntBuildModel.generateBuild(scriptFile);
			monitor.worked(1 * scale);
		} catch (CodeGenMakeException e) {
			throw new InvocationTargetException(e);
		}

		return scriptFile.getPath();
	}

	/**
	 * Execute an ant build monitoring its progress.
	 * @param scriptFileName the complete path and name of the ant script
	 * @param monitor the current monitor
	 * @param scale the scale of progress
	 * @throws InvocationTargetException execution fails
	 */
	protected void runBuild(
			final String scriptFileName,
			final IProgressMonitor monitor,
			final int scale) throws InvocationTargetException {

		monitor.setTaskName(MONITOR_ANT_RUN_START);
		try {
			removeXsdFile(monitor);
			IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
			IContainer container = root.getContainerForLocation(
					new Path(scriptFileName));
			container.getProject().refreshLocal(IResource.DEPTH_INFINITE, null);
			AntLaunchHelper antHelper = new AntLaunchHelper(getAntFile());
			ILaunch launch = antHelper.execute(
					new SubProgressMonitor(monitor, 1 * scale));
			/* Wait until the async process is finished  */
			for (IProcess process : launch.getProcesses()) {
				while (!process.isTerminated()) {
					if (monitor.isCanceled()) {
						return;
					}
					try {
						Thread.sleep(100);
					} catch (InterruptedException e1) {
						return;
					}
				}
				IStreamsProxy streamsProxy = process.getStreamsProxy();
				if (streamsProxy != null) {
					String errorMessage =
						streamsProxy.getErrorStreamMonitor().getContents();
					if (errorMessage != null && errorMessage.length() > 0) {
						Throwable th = new AntLaunchException(
								"Ant launch for " + scriptFileName
								+ " failed. error message=" + errorMessage);
						throw new InvocationTargetException(th);
					}
				}
				if (process.getExitValue() != 0) {
					Throwable th = new AntLaunchException(
							"Ant launch for " + scriptFileName
							+ " failed. return code=" + process.getExitValue());
					throw new InvocationTargetException(th);
				}
			}
			/* Usual LegStar ant runner does not work with JNI */
			/*
          LegstarAntRunner antRunner = new LegstarAntRunner();
          String[] antTargets = {GENERATE_XSD_TARGET};
          Map < String, String > properties = new HashMap < String, String >();
          antRunner.run(
          scriptFileName,
          antTargets,
          properties,
          monitor,
          scale);
			 */

		} catch (CoreException e) {
			throw new InvocationTargetException(e);
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
			((IContainer) getProjectResource()).refreshLocal(
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
	 * The generated script goes into the same project as the target XML schema
	 * but uses its own folder if a preference exists goes to root otherwise.
	 * @return the location for the ant script
	 */
	protected IPath getTargetAntScriptLocation() {
		IPath antScriptPath = getProjectLocation();
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String antScriptsFolder = store.getString(
				PreferenceConstants.ANT_SCRIPTS_FOLDER);
		if (antScriptsFolder != null && antScriptsFolder.length() != 0) {
			antScriptPath = antScriptPath.append(new Path(antScriptsFolder));
		}
		mkDir(antScriptPath);
		return antScriptPath;
	}

	/**
	 * Create a valid location for the generated Xsd file. The target
	 * container name is always expected to start with a project name
	 * as the first segment of its path.
	 * @return an existing location
	 */
	protected String getTargetXsdLocation() {
		IPath projectPath = getProjectLocation();
		IPath xsdPath = projectPath.append(
				(new Path(getTargetContainerName())).removeFirstSegments(1));
		mkDir(xsdPath);
		return xsdPath.toOSString();
	}

	/**
	 * @return the eclipse resource corresponding to the target project where
	 * the Xsd should be generated.
	 */
	private IResource getProjectResource() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		return root.findMember(new Path(
				getTargetContainerName()));
	}
	/**
	 * @return the physical location of the target project where the Xsd should
	 * be generated.
	 */
	protected IPath getProjectLocation() {
		IResource targetContainer = getProjectResource();
		return targetContainer.getProject().getLocation();
	}

	/**
	 * Make sure a folder physically exists.
	 * @param path a path to the folder (must be absolute)
	 */
	protected void mkDir(final IPath path) {
		File folder = new File(path.toOSString());
		if (!folder.exists()) {
			folder.mkdirs();
		}
	}

	/**
	 * @return the generated Xsd file ready to be edited
	 */
	protected IFile getXsdFile() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath projectPath = new Path(getTargetContainerName());
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
	 * @return the generated ant script file ready to be launched
	 */
	protected IFile getAntFile() {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IPath projectPath = new Path(getTargetContainerName());
		IPath containerPath = projectPath.append("ant");
		IPath filePath = containerPath.append(getTargetAntFileName());
		IResource resource = root.findMember(filePath);
		return (IFile) resource;
	}

	/**
	 * Removes the Xsd file from the workspace and the file system.
	 * @param monitor the current progress monitor
	 * @throws CoreException if resource cannot be deleted
	 */
	private void removeXsdFile(
			final IProgressMonitor monitor) throws CoreException {
		/* If file is already in the workspace, remove it from there */
		if (getXsdFile() != null) {
			getXsdFile().delete(true, true, monitor);
		} else {
			getXsdFileFile().delete();
		}
	}
	
	/**
	 * Determines where this plugin is installed on the file system.
	 * @return the plugin location
	 * @throws InvocationTargetException if location cannot be determined
	 */
	protected String getPluginInstallLocation()
			throws InvocationTargetException {
        Bundle bundle = Platform.getBundle(Activator.PLUGIN_ID);
        Path path = new Path("/");
        URL fileURL = FileLocator.find(bundle, path, null);
        String productLocation = null;
		try {
			productLocation = FileLocator.resolve(fileURL).getPath();
		} catch (IOException e) {
			throw new InvocationTargetException(e);
		}
		return productLocation;
	}

	/**
	 * @return the target container name
	 */
	protected String getTargetContainerName() {
		return mTargetContainerName;
	}

	/**
	 * @return the target ant file same (not a path)
	 */
	protected String getTargetAntFileName() {
		return ANT_FILE_PREFIX + getTargetXsdFileName() + '.'
		+ ANT_FILE_SUFFIX;
	}

	/**
	 * @return the target Xsd file name (not a path)
	 */
	protected String getTargetXsdFileName() {
		return mTargetXsdFileName;
	}

	/**
	 * @return the model to build from
	 */
	public final IAntBuildModel getAntBuildModel() {
		return mAntBuildModel;
	}

	/**
	 * @param antBuildModel the model to build from to set
	 */
	public final void setAntBuildModel(
			final IAntBuildModel antBuildModel) {
		mAntBuildModel = antBuildModel;
	}

}
