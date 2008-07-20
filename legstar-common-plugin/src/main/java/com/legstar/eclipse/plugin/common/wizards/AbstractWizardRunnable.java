/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.common.wizards;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.osgi.util.NLS;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.IAntBuildModel;
import com.legstar.eclipse.ant.AntLaunchException;
import com.legstar.eclipse.ant.AntLaunchHelper;
import com.legstar.eclipse.plugin.common.Activator;
import com.legstar.eclipse.plugin.common.Messages;
import com.legstar.eclipse.plugin.common.preferences.PreferenceConstants;

/**
 * Abstract backend task. Provides common ant launcher capabilities.
 */
public abstract class AbstractWizardRunnable implements IRunnableWithProgress {

	/** The model to build from. */
	private IAntBuildModel mAntBuildModel;

	/** The target project. */
	private IProject mTargetProject;

	/** The target ant file name (without path). */
	private String mTargetAntFileName;

	/** Probe files prefixes. */
	private static final String PROBE_FILE_PREFIX = "probe";

	/** Probe files suffixes. */
	private static final String PROBE_FILE_SUFFIX = "tmp";

	/**
	 * Instantiate this runnable from UI items. It is important not to attempt
	 * access to UI elements from the background thread.
	 * @param antBuildModel the model object to be passed to velocity templates
	 * @param targetContainerRelativePathName where generated objects are stored
	 * @param targetAntFileName the name (no path) of the ant file
	 * @throws InvocationTargetException if runnable cannot be instantiated
	 */
	public AbstractWizardRunnable(
			final IAntBuildModel antBuildModel,
			final String targetContainerRelativePathName,
			final String targetAntFileName)
	throws InvocationTargetException {
		mAntBuildModel = antBuildModel;
		mTargetProject = getProject(targetContainerRelativePathName);
		mTargetAntFileName = targetAntFileName;
	}

	/**
	 * Create a new ANT script file (replace existing one). We create this
	 * one directly under the target container.  The process also creates
	 * a temporary probe file expected to be deleted by the ant script on
	 * successful completion. This serves as a general purpose mechanism to
	 * determine whether the ant run was successful or not.
	 * @param monitor the current monitor
	 * @param scale the scale of progress
	 * @throws InvocationTargetException if creation fails
	 */
	protected void createBuild(
			final IProgressMonitor monitor,
			final int scale) throws InvocationTargetException {
		monitor.setTaskName(Messages.ant_generating_task_label);
		try {
			File antFile = getAntFile();
			mAntBuildModel.setProbeFile(getProbeFile());
			mAntBuildModel.generateBuild(antFile);
			monitor.worked(1 * scale);
		} catch (CodeGenMakeException e) {
			throw new InvocationTargetException(e);
		}
	}

	/**
	 * Execute an ant build monitoring its progress.
	 * @param monitor the current monitor
	 * @param scale the scale of progress
	 * @throws InvocationTargetException execution fails
	 */
	protected void runBuild(
			final IProgressMonitor monitor,
			final int scale) throws InvocationTargetException {

		monitor.setTaskName(Messages.ant_running_task_label);
		try {
			getTargetProject().refreshLocal(IResource.DEPTH_INFINITE, null);
			IFile antFile = getGeneratedAntFile();
			AntLaunchHelper antHelper = new AntLaunchHelper(
					getGeneratedAntFile());
			ILaunch launch = antHelper.execute(
					new SubProgressMonitor(monitor, 1 * scale));
			/* Wait until the async process is finished  */
			for (IProcess process : launch.getProcesses()) {
				if (!process.getLaunch().equals(launch)) {
					continue;
				}
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
								NLS.bind(Messages.ant_failure_stream_msg,
										antFile.getName(), errorMessage));
						throw new InvocationTargetException(th);
					}
				}
				if (process.getExitValue() != 0) {
					Throwable th = new AntLaunchException(
							NLS.bind(Messages.ant_failure_retcode_msg,
									antFile.getName(), process.getExitValue()));
					throw new InvocationTargetException(th);
				}
			}
			
			/* Check that the probe file was successfully removed */
			getTargetProject().refreshLocal(IResource.DEPTH_INFINITE, null);
			if (!checkProbeFile()) {
				Throwable th = new AntLaunchException(
						NLS.bind(Messages.ant_failure_console_msg,
								antFile.getName()));
				throw new InvocationTargetException(th);
			}

		} catch (CoreException e) {
			throw new InvocationTargetException(e);
		} finally {
			monitor.worked(1 * scale);
		}
	}
	
	
	/**
	 * @return the ant file as a file system object
	 */
	protected File getAntFile() {
		File antFile = new File(
				getTargetAntScriptLocation().toOSString()
				+ File.separatorChar 
				+ getTargetAntFileName());
		return antFile;
	}
	
	/**
	 * Create a temporary probe file.
	 * @return the probe file
	 * @throws CodeGenMakeException if file cannot be created
	 */
	protected File getProbeFile() throws CodeGenMakeException {
		try {
			return File.createTempFile(
					PROBE_FILE_PREFIX, PROBE_FILE_SUFFIX);
		} catch (IOException e) {
			throw new CodeGenMakeException(e);
		}
	}
	
	/**
	 * @return true if the probe file was successfully deleted false otherwise
	 */
	protected boolean checkProbeFile() {
		File probeFile = mAntBuildModel.getProbeFile();
		if (probeFile != null) {
			if (probeFile.exists()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * The generated script goes into the target project under a specific
	 * ant folder.
	 * If the ant folder does not yet exist, it is created
	 * @return the location for the ant script
	 */
	protected IPath getTargetAntScriptLocation() {
		IPath antFolder = getAntFolderAbsolutePath();
		mkDir(antFolder);
		return antFolder;
	}
	
	/**
	 * @return the folder where ant scripts need to be stored.
	 */
	protected IPath getAntFolderRelativePath() {
		IPath projectPath = mTargetProject.getFullPath();
		IPath containerPath = projectPath.append(getPreferenceAntFolder());
		return containerPath;
	}
	
	/**
	 * @return the folder where ant scripts need to be stored.
	 */
	protected IPath getAntFolderAbsolutePath() {
		IPath projectPath = mTargetProject.getLocation();
		IPath containerPath = projectPath.append(getPreferenceAntFolder());
		return containerPath;
	}
	
	/**
	 * @return the ant file relative path (relative to the workspace root)
	 */
	protected IPath getAntFileRelativePath() {
		IPath containerPath = getAntFolderRelativePath();
		return containerPath.append(getTargetAntFileName());
	}

	/**
	 * Once the ant file is generated it is retrieved as an IFile.
	 * @return the generated ant script file ready to be launched
	 */
	protected IFile getGeneratedAntFile() {
		IPath filePath = getAntFileRelativePath();
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IResource resource = root.findMember(filePath);
		return (IFile) resource;
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
     * @return the preferred ant script sub folder relative to projects.
     */
    public String getPreferenceAntFolder() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String antScriptsFolder = store.getString(
				PreferenceConstants.ANT_SCRIPTS_FOLDER);
		if (antScriptsFolder == null) {
			return "";
		} else {
			return antScriptsFolder;
		}
    }
    
	/**
	 * Retrieve the parent project from a resource relative path name.
	 * @param relativePathName relative to workspace root
	 * @return the parent project
	 */
	protected static IProject getProject(final String relativePathName) {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IResource resource = root.findMember(relativePathName);
		return resource.getProject();
	}
	

	/**
	 * Determines where the common LegStar plugin is installed on the file
	 *  system.
	 * @param pluginId the plugin identifier for which the location is sought
	 * @return the plugin location
	 * @throws InvocationTargetException if location cannot be determined
	 */
	public static String getPluginInstallLocation(final String pluginId)
			throws InvocationTargetException {
		return Activator.getPluginInstallLocation(pluginId);
	}
	
	/**
	 * @return the target project
	 */
	protected IProject getTargetProject() {
		return mTargetProject;
	}

	/**
	 * @return the target ant file same (not a path)
	 */
	protected String getTargetAntFileName() {
		return mTargetAntFileName;
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
