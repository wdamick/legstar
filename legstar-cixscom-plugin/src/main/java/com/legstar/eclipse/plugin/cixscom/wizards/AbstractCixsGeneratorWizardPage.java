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
package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

import com.legstar.cixs.gen.model.CixsMappingModel;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.cixscom.preferences.PreferenceConstants;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;

/**
 * Abstract page. Collects parameters needed for common Cixs Artifacts
 * generation. Each subclass will ad its own widgets. 
 *
 */
public abstract class AbstractCixsGeneratorWizardPage
extends AbstractWizardPage {

	/** The current mapping file. */
	private IFile mMappingFile = null;

	/** The current mapping model. */
	private CixsMappingModel mMappingModel = null;

	/** Component or service name. */
	private Text mServiceNameText = null;

	/** Generated Java classes package name. */
	private Text mJavaClassesPackageNameText = null;

	/** Where JAXB classes reside. */
	private Text mJaxbBinDirText = null;

	/** Where COXB classes reside. */
	private Text mCoxbBinDirText = null;

	/** Where custom classes reside. */
	private Text mCustBinDirText = null;

	/** Where generated sources reside. */
	private Text mTargetSrcDirText = null;

	/** Where generated classes reside. */
	private Text mTargetBinDirText = null;

	/** Where generated ant scripts reside. */
	private Text mTargetAntDirText = null;

	/** Where generated property files reside. */
	private Text mTargetPropDirText = null;

	/** The host character set. */
	private Text mHostCharsetText = null;
	
	/**
	 * Construct the page.
	 * @param pageName the page name
	 * @param pageTitle the page title
	 * @param pageDesc the page description
	 * @param selection the current workbench selection
	 * @param mappingFile the mapping file
	 */
	protected AbstractCixsGeneratorWizardPage(
			final IStructuredSelection selection,
			final String pageName,
			final String pageTitle,
			final String pageDesc,
			final IFile mappingFile) {
		super(selection, pageName, pageTitle, pageDesc);
		mMappingFile = mappingFile;
	}

	/** {@inheritDoc} */
	public void createExtendedControls(final Composite parent) {

		addCixsGroup(parent);

		final TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
		final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
		groupGridData.horizontalSpan = LAYOUT_COLUMNS;
		tabFolder.setLayoutData(groupGridData);

		addTargetGroup(tabFolder);
		addDeploymentGroup(tabFolder);
		addCoxbGroup(tabFolder);
	}

	/**
	 * Groups parameters that generically describe generated artifacts.
	 * @param container parent container
	 */
	protected void addCixsGroup(final Composite container) {
		Group group = createGroup(container,
				Messages.generation_project_label, 2);
		createLabel(group, Messages.generation_project_name_label + ':');
		mServiceNameText = createText(group);
		mServiceNameText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		createLabel(group, Messages.generation_java_package_label + ':');
		mJavaClassesPackageNameText = createText(group);
		addWidgetsToCixsGroup(group);
	}

	/**
	 * Groups JAXB and COXB classes locations. These are needed for the
	 * generation process.
	 * For convenience we use directoryFieldEditors which are normally used
	 * in preference pages. These controls change the layout of their parent
	 * container. This is why we create canvas within a canvas in order to
	 * preserve the parent canvas layout.
	 * @param tabFolder the parent folder
	 */
	private void addCoxbGroup(final TabFolder tabFolder) {
		Canvas canvas = createCanvasInFolder(tabFolder,
				Messages.structures_binding_classes_label,
				LAYOUT_COLUMNS);
		Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);
		mJaxbBinDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.jaxbBinDir",
				Messages.jaxb_classes_location_label + ':');
		mJaxbBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mCoxbBinDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.coxbBinDir",
				Messages.coxb_classes_location_label + ':');
		mCoxbBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mCustBinDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.custBinDir",
				Messages.cust_classes_location_label + ':');
		mCustBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		addWidgetsToCoxbGroup(canvas2);
	}

	/**
	 * Groups all target folders where artifacts will be generated.
	 * @param tabFolder the parent folder
	 */
	private void addTargetGroup(final TabFolder tabFolder) {
		Canvas canvas = createCanvasInFolder(
				tabFolder, Messages.generation_target_locations,
				LAYOUT_COLUMNS);
		Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);
		mTargetSrcDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.targetSrcDir",
				Messages.java_sources_target_location + ':');
		mTargetSrcDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetBinDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.targetBinDir",
				Messages.java_classes_target_location + ':');
		mTargetBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetAntDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.targetAntDir",
				Messages.ant_scripts_target_location + ':');
		mTargetAntDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetPropDirText = createDirectoryFieldEditor(canvas2,
				"com.legstar.eclipse.plugin.cixscom.targetPropDir",
				Messages.properties_files_target_location + ':');
		mTargetPropDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		addWidgetsToTargetGroup(canvas2);
	}

	/**
	 * Groups the parameters needed to deploy the generated artifacts.
	 * @param tabFolder the parent folder
	 */
	private void addDeploymentGroup(final TabFolder tabFolder) {
		Canvas canvas = createCanvasInFolder(
				tabFolder, Messages.deployment_group_label, 2);
		mHostCharsetText = createTextField(canvas, getCommonStore(),
				com.legstar.eclipse.plugin.common.preferences
				.PreferenceConstants.HOST_CHARSET,
				Messages.mainframe_charset_label + ':');
		mHostCharsetText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		addWidgetsToDeploymentGroup(canvas);
	}

	/**
	 * Initialize all fields.
	 */
	protected void initContents() {
		mMappingModel = loadMappingModel(mMappingFile);
		setServiceName(javaNormalize(mMappingModel.getName()));
		initPackageName(getServiceName());
		initJavaSrcAndBinDirs(mMappingFile.getProject());
		/* Initialized after target bin dir because it depends on it*/
		initCoxbDir();
		initOtherArtifactsDirs(mMappingFile.getProject());
		initExtendedWidgets(mMappingFile.getProject());
	}
	
	/**
	 * TODO Move this to common utility class.
	 * The service name is used in various places as a Java identifier. This
	 * creates a valid Java identifier from a proposed string.
	 * 
	 * @param str the input string
	 * @return a a valid java identifier
	 */
	public static String javaNormalize(final String str)  {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < str.length(); i++) {
			Character c = str.charAt(i);
			if (i == 0) {
				if (Character.isJavaIdentifierStart(c)) {
					sb.append(c);
				} else {
					sb.append("C");
				}
			} else {
				if (Character.isJavaIdentifierPart(c)) {
					sb.append(c);
				}
			}
		}
		return sb.toString();
	}

	/**
	 * Perform validation on data entered so far.
	 */
	protected void dialogChanged() {

		if (getServiceName().length() == 0) {
			updateStatus(Messages.invalid_project_name_msg);
			return;
		}
		for (int i = 0; i < getServiceName().length(); i++) {
			Character c = getServiceName().charAt(i);
			if (i == 0) {
				if (!Character.isJavaIdentifierStart(c)) {
					updateStatus(Messages.invalid_project_name_msg);
					return;
				}
			} else {
				if (!Character.isJavaIdentifierPart(c)) {
					updateStatus(Messages.invalid_project_name_msg);
					return;
				}
			}
		}
		if (!checkDirectory(getJaxbBinDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.jaxb_classes_location_label))) {
			return;
		}

		if (!checkDirectory(getCoxbBinDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.coxb_classes_location_label))) {
			return;
		}

		if (!checkDirectory(getCustBinDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.cust_classes_location_label))) {
			return;
		}

		if (!checkDirectory(getTargetSrcDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.java_sources_target_location))) {
			return;
		}

		if (!checkDirectory(getTargetBinDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.java_classes_target_location))) {
			return;
		}

		if (!checkDirectory(getTargetPropDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.properties_files_target_location))) {
			return;
		}
		if (!checkDirectory(getTargetAntDir(),
				NLS.bind(Messages.invalid_location_msg,
						Messages.ant_scripts_target_location))) {
			return;
		}

		try {
			CodeGenUtil.checkCharset(getHostCharset());
		} catch (CodeGenMakeException e) {
			updateStatus(Messages.invalid_mainframe_charset_msg);
			return;
		}

		if (!validateExtendedWidgets()) {
			return;
		}

		updateStatus(null);

	}

	/**
	 * Entry point for derived classes to add more widgets on the main group.
	 * @param container the parent container
	 */
	public abstract void addWidgetsToCixsGroup(final Composite container);

	/**
	 * Entry point for derived classes to add more widgets on the binding group.
	 * @param container the parent container
	 */
	public abstract void addWidgetsToCoxbGroup(final Composite container);

	/**
	 * Entry point for derived classes to add more widgets on the target group.
	 * @param container the parent container
	 */
	public abstract void addWidgetsToTargetGroup(final Composite container);

	/**
	 * Entry point for derived classes to add more widgets on the deployment
	 *  group.
	 * @param container the parent container
	 */
	public abstract void addWidgetsToDeploymentGroup(final Composite container);

	/**
	 * Give derived classes a chance to initialize their private widgets.
	 * @param project the current Eclipse project
	 */
	public abstract void initExtendedWidgets(final IProject project);

	/**
	 * Give derived classes a chance to validate their private widgets.
	 * @return true if widget contrnt is valid, false otherwise
	 */
	public abstract boolean validateExtendedWidgets();

	/** @return the subclass plugin.*/
	public abstract AbstractCixsActivator getActivator();

	/**
	 * Create a container in a tabfolder.
	 * @param tabFolder the parent tab folder
	 * @param text the tab folder text
	 * @param columns the number of columns this containers layout should have
	 * @return the new container
	 */
	protected static Canvas createCanvasInFolder(
			final TabFolder tabFolder, final String text, final int columns) {
		final TabItem tabItem = new TabItem(tabFolder, SWT.NONE);
		tabItem.setText(text);
		final Canvas canvas = createCanvas(tabFolder, columns);
		tabItem.setControl(canvas);
		return canvas;
	}

	/**
	 * Examine the given project and derive default locations java sources
	 * and binaries.
	 * @param project the current Eclipse project
	 */
	private void initJavaSrcAndBinDirs(final IProject project) {
		IJavaProject javaProject = JavaCore.create(project);
		if (javaProject != null) {
			IPath rootPath =
				javaProject.getProject().getLocation().removeLastSegments(1);

			/* If this is a java project, get first Java source and output
			 * folders. */
			try {
				IClasspathEntry[]  cpe = javaProject.getRawClasspath();
				for (int i = 0; i < cpe.length; i++) {
					if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
						setTargetSrcDir(rootPath.append(
								cpe[i].getPath()).toOSString());
						if (cpe[i].getOutputLocation() == null) {
							setTargetBinDir(rootPath.append(
									javaProject.getOutputLocation()
							).toOSString());
						} else {
							setTargetBinDir(rootPath.append(
									cpe[i].getOutputLocation()).toOSString());
						}
						return;
					}
				}
			} catch (JavaModelException e) {
				AbstractWizard.errorDialog(
						getShell(), Messages.generate_error_dialog_title,
						getPluginId(),
						Messages.java_location_lookup_failure_msg,
						NLS.bind(Messages.invalid_java_project_msg,
								project.getName(), e.getMessage()));
				AbstractWizard.logCoreException(e,
						getPluginId());
			}
		}
		/* If everything else failed, assume generated artifacts will go to the 
		 * project root. */
		setTargetSrcDir(project.getLocation().toOSString());
		setTargetBinDir(project.getLocation().toOSString());
	}

	/**
	 * Package name is built from a prefix stored in preferences and the
	 * name of the target service.
	 * @param serviceName the service name
	 */
	private void initPackageName(final String serviceName) {
		String prefix = getCixscomStore().getString(
				PreferenceConstants.CIXS_PACKAGE_NAME_PREFIX);
		if (prefix == null || prefix.length() == 0) {
			setJavaClassesPackageName(serviceName.toLowerCase());
		} else {
			setJavaClassesPackageName(prefix + '.' + serviceName.toLowerCase());
		}
	}

	/**
	 * Source classes for JAXB, COXB and Custom are all assumed to come
	 * from the same location as the target binary folder.
	 */
	private void initCoxbDir() {
		setJaxbBinDir(getTargetBinDir());
		setCoxbBinDir(getTargetBinDir());
		setCustBinDir(getTargetBinDir());
	}

	/**
	 * Various artifacts locations are driven by preferences.
	 * @param project the target project with the workbench
	 */
	private void initOtherArtifactsDirs(final IProject project) {
		setTargetAntDir(getDefaultTargetDir(getCommonStore(),
				com.legstar.eclipse.plugin.common.preferences
				.PreferenceConstants.ANT_SCRIPTS_FOLDER));

		setTargetPropDir(getDefaultTargetDir(getCixscomStore(),
				PreferenceConstants.CIXS_TARGET_PROP_FOLDER));

	}
	
	/**
	 * Target locations have default values that are built relative to the
	 * project containing the mapping file and preferences taken from a
	 * preference store.
	 * This will make sure the default location exists.
	 * @param store the preference store to use
	 * @param storeKey the preference store key for this target 
	 * @return the corresponding default target location
	 */
	protected String getDefaultTargetDir(
			final IPreferenceStore store, final String storeKey) {
		IPath projectPath = getMappingFile().getProject().getLocation();
		String folder = store.getString(storeKey);
		String defaultValue = projectPath.append(new Path(folder)).toOSString();
		try {
			CodeGenUtil.checkDirectory(defaultValue, true);
			getMappingFile().getProject().refreshLocal(
					IResource.DEPTH_INFINITE, null);
		} catch (IllegalArgumentException e) {
			updateStatus(NLS.bind(Messages.invalid_default_location_msg,
							defaultValue, storeKey));
		} catch (CoreException e) {
			updateStatus(NLS.bind(Messages.location_refresh_failure_msg,
					defaultValue, storeKey));
		}
		return defaultValue;
	}

	/**
	 * The target source folder is part of a Java project. It is assumed
	 * this was tested in the validation step so we can safely infer the
	 * java nature here.
	 * @return the target java project
	 */
	public IJavaProject getTargetJavaProject() {
		IResource resource = ResourcesPlugin.getWorkspace().getRoot()
		.getContainerForLocation(new Path(getTargetSrcDir()));
		return JavaCore.create(resource.getProject());
	}
	
	/**
	 * Checks that a folder exists.
	 * @param dir folder name
	 * @param erorMessage the message to use if folder does not exist
	 * @return true if folder exist, false otherwise
	 */
	protected boolean checkDirectory(
			final String dir, final String erorMessage) {
		try {
			CodeGenUtil.checkDirectory(dir, false);
		} catch (IllegalArgumentException e1) {
			updateStatus(erorMessage);
			return false;
		}
		return true;
	}

	/**
	 * @param serviceName Service name to set
	 */
	public void setServiceName(final String serviceName) {
		mServiceNameText.setText(serviceName);
	}

	/**
	 * @return Service name
	 */
	public String getServiceName() {
		return mServiceNameText.getText();
	}

	/**
	 * @param javaClassesPackageName Generated java classes package name
	 */
	public void setJavaClassesPackageName(
			final String javaClassesPackageName) {
		mJavaClassesPackageNameText.setText(javaClassesPackageName);
	}

	/**
	 * @return Generated java classes package name
	 */
	public String getJavaClassesPackageName() {
		return mJavaClassesPackageNameText.getText();
	}

	/**
	 * @param jaxbBinDirLocation Where JAXB classes reside
	 */
	public void setJaxbBinDir(final String jaxbBinDirLocation) {
		mJaxbBinDirText.setText(jaxbBinDirLocation);
	}

	/**
	 * @return Where JAXB classes reside
	 */
	public String getJaxbBinDir() {
		return mJaxbBinDirText.getText();
	}

	/**
	 * @param coxbBinDirLocation Where Binding classes reside
	 */
	public void setCoxbBinDir(final String coxbBinDirLocation) {
		mCoxbBinDirText.setText(coxbBinDirLocation);
	}

	/**
	 * @return Where Binding classes reside
	 */
	public String getCoxbBinDir() {
		return mCoxbBinDirText.getText();
	}

	/**
	 * @param custBinDirLocation Where custom classes reside
	 */
	public void setCustBinDir(final String custBinDirLocation) {
		mCustBinDirText.setText(custBinDirLocation);
	}

	/**
	 * @return Where custom classes reside
	 */
	public String getCustBinDir() {
		return mCustBinDirText.getText();
	}

	/**
	 * @param targetSrcDirLocation Where generated sources reside
	 */
	public void setTargetSrcDir(final String targetSrcDirLocation) {
		mTargetSrcDirText.setText(targetSrcDirLocation);
	}

	/**
	 * @return Where generated sources reside
	 */
	public String getTargetSrcDir() {
		return mTargetSrcDirText.getText();
	}

	/**
	 * @param targetBinDirLocation Where generated classes reside
	 */
	public void setTargetBinDir(final String targetBinDirLocation) {
		mTargetBinDirText.setText(targetBinDirLocation);
	}

	/**
	 * @return Where generated classes reside
	 */
	public String getTargetBinDir() {
		return mTargetBinDirText.getText();
	}

	/**
	 * @return the mapping model
	 */
	public final CixsMappingModel getMappingModel() {
		return mMappingModel;
	}

	/**
	 * @param targetAntDirLocation Where generated ant scripts reside
	 */
	public void setTargetAntDir(final String targetAntDirLocation) {
		mTargetAntDirText.setText(targetAntDirLocation);
	}

	/**
	 * @return Where generated ant scripts reside
	 */
	public String getTargetAntDir() {
		return mTargetAntDirText.getText();
	}

	/**
	 * @param targetPropDirLocation Where generated Property files reside
	 */
	public void setTargetPropDir(final String targetPropDirLocation) {
		mTargetPropDirText.setText(targetPropDirLocation);
	}

	/**
	 * @return Where generated Property files reside
	 */
	public String getTargetPropDir() {
		return mTargetPropDirText.getText();
	}

	/**
	 * @param hostCharset the mainframe character set
	 */
	public void setHostCharset(final String hostCharset) {
		mHostCharsetText.setText(hostCharset);
	}

	/**
	 * @return the mainframe character set
	 */
	public String getHostCharset() {
		return mHostCharsetText.getText();
	}

	/**
	 * @return the mapping file
	 */
	public final IFile getMappingFile() {
		return mMappingFile;
	}

	/**
	 * Try to load the mapping file into a mapping model.
	 * @param mappingFile the file to load
	 * @return a mapping model that might be empty if load fails
	 */
	private CixsMappingModel loadMappingModel(final IFile mappingFile) {
		CixsMappingModel mappingModel = new CixsMappingModel();
		try {
			mappingModel.load(mappingFile.getLocation().toFile());
		} catch (CixsModelException e) {
			AbstractWizard.errorDialog(getShell(),
					Messages.generate_error_dialog_title,
					getPluginId(),
					Messages.mapping_file_load_failure_short_msg,
					NLS.bind(Messages.mapping_file_load_failure_long_msg,
							mappingFile.getName(), e.getMessage()));
			AbstractWizard.logCoreException(e, getPluginId());
		}
		return mappingModel;

	}

	/**
	 * @return the preference store managed by the common plugin.
	 */
	public IPreferenceStore getCommonStore() {
       return  com.legstar.eclipse.plugin.common.Activator.getDefault()
    	   .getPreferenceStore();
		
	}
	
	/**
	 * @return the preference store managed by the cixscom plugin.
	 */
	public IPreferenceStore getCixscomStore() {
       return  com.legstar.eclipse.plugin.cixscom.Activator.getDefault()
    	   .getPreferenceStore();
		
	}
	
	/**
	 * @return the subclass plugin preference store
	 */
	public IPreferenceStore getStore() {
		return getActivator().getPreferenceStore();
	}

	/**
	 * @return the subclass plugin ID
	 */
	public String getPluginId() {
		return getActivator().getPluginId();
	}

}
