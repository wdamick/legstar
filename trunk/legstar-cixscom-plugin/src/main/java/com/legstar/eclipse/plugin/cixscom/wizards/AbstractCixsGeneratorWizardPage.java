package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.cixs.gen.model.CixsMappingModel;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.eclipse.plugin.cixscom.preferences.PreferenceConstants;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * Abstract page. Collects parameters needed for common Cixs Artifacts
 * generation. Each subclass will ad its own widgets. 
 *
 */
public abstract class AbstractCixsGeneratorWizardPage extends WizardPage {

	/** The main grid layout column number. */
	public static final int LAYOUT_COLUMNS = 3;

	/** The current workbench selection. */
	private ISelection mInitialSelection;

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
			final String pageName,
			final String pageTitle,
			final String pageDesc,
			final ISelection selection,
			final IFile mappingFile) {
		super(pageName);
		mInitialSelection = selection;
		mMappingFile = mappingFile;
		setTitle(pageTitle);
		setDescription(pageDesc);
		ImageDescriptor image =
			AbstractUIPlugin.
			imageDescriptorFromPlugin(
					com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID,
					com.legstar.eclipse.plugin.common.Activator.LOGO_IMG);
		setImageDescriptor(image);
	}

	/** {@inheritDoc} */
	public void createControl(final Composite parent) {
		/* Fit controls in a 3 column grid */
		Composite container = new Composite(parent, SWT.NULL);
		final GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = LAYOUT_COLUMNS;
		container.setLayout(gridLayout);
		setControl(container);

		addCixsGroup(container);

		final TabFolder tabFolder = new TabFolder(container, SWT.NONE);
		final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
		groupGridData.horizontalSpan = LAYOUT_COLUMNS;
		tabFolder.setLayoutData(groupGridData);

		addCoxbGroup(tabFolder);
		addTargetGroup(tabFolder);
		addDeploymentGroup(tabFolder);

		initContents();
		dialogChanged();
	}

	/**
	 * Groups parameters that generically describe generated artifacts.
	 * @param container parent container
	 */
	protected void addCixsGroup(final Composite container) {
		Group group = createGroup(container, "Generated artifacts", 2);
		createLabel(group, "Name:");
		mServiceNameText = createText(group);
		mServiceNameText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		createLabel(group, "Java package:");
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
		Canvas canvas = createCanvasInFolder(
				tabFolder, "Sources location", LAYOUT_COLUMNS);
		Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);
		mJaxbBinDirText = createDirectoryFieldEditor(canvas2,
				"jaxbBinDir", "JAXB classes:");
		mJaxbBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mCoxbBinDirText = createDirectoryFieldEditor(canvas2,
				"coxbBinDir", "Binding classes:");
		mCoxbBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mCustBinDirText = createDirectoryFieldEditor(canvas2,
				"custBinDir", "Custom classes:");
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
				tabFolder, "Targets location", LAYOUT_COLUMNS);
		Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);
		mTargetSrcDirText = createDirectoryFieldEditor(canvas2,
				"targetSrcDir", "Java sources:");
		mTargetSrcDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetBinDirText = createDirectoryFieldEditor(canvas2,
				"targetBinDir", "Java classes:");
		mTargetBinDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetAntDirText = createDirectoryFieldEditor(canvas2,
				"targetAntDir", "Ant scripts:");
		mTargetAntDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		mTargetPropDirText = createDirectoryFieldEditor(canvas2,
				"targetPropDir", "Properties files:");
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
				tabFolder, "Deployment", 2);
		mHostCharsetText = createTextField(canvas, getCommonStore(),
				com.legstar.eclipse.plugin.common.preferences
				.PreferenceConstants.HOST_CHARSET,
				"Mainframe character set:");
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
		setServiceName(mMappingModel.getName());
		initPackageName(mMappingModel.getName());
		initJavaSrcAndBinDirs(mMappingFile.getProject());
		/* Initialized after target bin dir because it depends on it*/
		initCoxbDir();
		initOtherArtifactsDirs(mMappingFile.getProject());
		initExtendedWidgets(mMappingFile.getProject());
	}

	/**
	 * Perform validation on data entered so far.
	 */
	protected void dialogChanged() {

		if (getServiceName().length() == 0) {
			updateStatus("Invalid component name");
			return;
		}
		if (!checkDirectory(getJaxbBinDir(),
		"Invalid JAXB classes location")) {
			return;
		}

		if (!checkDirectory(getCoxbBinDir(),
		"Invalid Binding classes location")) {
			return;
		}

		if (!checkDirectory(getCustBinDir(),
		"Invalid Custom classes location")) {
			return;
		}

		if (!checkDirectory(getTargetSrcDir(),
		"Invalid Target Java sources location")) {
			return;
		}

		if (!checkDirectory(getTargetBinDir(),
		"Invalid Target Java classes location")) {
			return;
		}

		if (!checkDirectory(getTargetPropDir(),
		"Invalid Target properties file location")) {
			return;
		}
		if (!checkDirectory(getTargetAntDir(),
		"Invalid Target ant scripts location")) {
			return;
		}

		try {
			CodeGenUtil.checkCharset(getHostCharset());
		} catch (CodeGenMakeException e) {
			updateStatus("Invalid host character set");
			return;
		}

		if (!validateExtendedWidgets()) {
			return;
		}

		((AbstractWizard) getWizard()).setCanFinish(true);
		updateStatus(null);

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
	 * Create a label SWT widget.
	 * @param container parent container
	 * @param text the labels text
	 * @return the new label
	 */
	protected static Label createLabel(
			final Composite container, final String text) {
		final Label label = new Label(container, SWT.NONE);
		label.setText(text);
		return label;
	}

	/**
	 * Create a textbook SWT widget.
	 * @param container parent container
	 * @return the textbox
	 */
	protected static Text createText(final Composite container) {
		final Text text = new Text(container, SWT.BORDER | SWT.SINGLE);
		final GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		text.setLayoutData(gridData);
		return text;
	}

	/**
	 * Add a new button on a composite.
	 * @param parent the parent composite
	 * @param text text to appear on button
	 * @return the newly created button
	 */
	public static Button createButton(
			final Composite parent, final String text) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText(text);
		return button;
	}

	/**
	 * This type of widget has a textbox and a browse button to lookup a folder
	 * on the file system. The content is tied to a preference store element so
	 * content can be saved and restored.
	 * @param container the parent composite
	 * @param preferenceName the preference store item
	 * @param labelText the label's text appearing before the textbox
	 * @return the composite
	 */
	protected static Text createDirectoryFieldEditor(
			final Composite container,
			final String preferenceName,
			final String labelText) {
		DirectoryFieldEditor editor = new DirectoryFieldEditor(preferenceName,
				labelText, container);
		return editor.getTextControl(container);
	}

	/**
	 * This type of widget is a simple textbox preceded by a label.
	 * The content can be initialized from a preference store.
	 * @param container the parent composite
	 * @param store a preference store
	 * @param preferenceName the preference store item
	 * @param labelText the label's text appearing before the textbox
	 * @return the composite
	 */
	protected static Text createTextField(
			final Composite container,
			final IPreferenceStore store,
			final String preferenceName,
			final String labelText) {
		createLabel(container, labelText);
		Text text = createText(container);
		if (preferenceName != null) {
			text.setText(store.getDefaultString(preferenceName));
		}
		return text;
	}

	/**
	 * Create a group container.
	 * @param container the parent container
	 * @param text the group text
	 * @param columns the number of columns this groups layout should have
	 * @return the new group
	 */
	protected static Group createGroup(
			final Composite container, final String text, final int columns) {
		final Group group = new Group(container, SWT.SHADOW_ETCHED_IN);
		final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
		groupGridData.horizontalSpan = LAYOUT_COLUMNS;
		group.setLayoutData(groupGridData);
		group.setLayout(new GridLayout(columns, false));
		group.setText(text);
		return group;
	}

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
	 * Create a container spanning all columns of parent container grid.
	 * @param container the parent container
	 * @param columns the number of columns this containers layout should have
	 * @return the new container
	 */
	protected static Canvas createCanvas(
			final Composite container, final int columns) {
		final Canvas canvas = new Canvas(container, SWT.NONE);
		final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
		groupGridData.horizontalSpan = LAYOUT_COLUMNS;
		canvas.setLayoutData(groupGridData);
		canvas.setLayout(new GridLayout(columns, false));
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
						getShell(), "Initialization error",
						getPluginId(),
						"Attempt to locate default java source and"
						+ " binary folders failed ",
						"Project " + project.getName()
						+ " is not a valid Java project " + e.getMessage());
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
			setJavaClassesPackageName(serviceName);
		} else {
			setJavaClassesPackageName(prefix + '.' + serviceName);
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
		IPath projectPath = project.getProject().getLocation();

		String antFolder = getCixscomStore().getString(
				PreferenceConstants.CIXS_TARGET_ANT_FOLDER);
		setTargetAntDir(projectPath.append(
				new Path(antFolder)).toOSString());

		String propFolder = getCixscomStore().getString(
				PreferenceConstants.CIXS_TARGET_PROP_FOLDER);
		setTargetPropDir(projectPath.append(
				new Path(propFolder)).toOSString());

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
			AbstractWizard.errorDialog(getShell(), "Load error", getPluginId(),
					"Attempt to load mapping file failed ",
					"Mapping file " 
					+ mappingFile.getName()
					+ " is corrupted " + e.getMessage());
			AbstractWizard.logCoreException(e, getPluginId());
		}
		return mappingModel;

	}

	/**
	 * Invalidate the dialog content if error message is not null and make sure
	 * error message is displayed.
	 * @param errorMessage the text
	 */
	protected void updateStatus(final String errorMessage) {
		setErrorMessage(errorMessage);
		setPageComplete(errorMessage == null);
		((AbstractWizard) getWizard()).setCanFinish(
				(errorMessage == null) ? true : false);
	}

	/**
	 * @return the Initial Selection
	 */
	public final ISelection getInitialSelection() {
		return mInitialSelection;
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
