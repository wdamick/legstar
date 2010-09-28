/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.cixscom.wizards;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
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
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
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

    /** This generator model. */
    private AbstractAntBuildCixsModel _genModel;

    /** The current mapping file. */
    private IFile _mappingFile = null;

    /** Component or service name. */
    private Text _serviceNameText = null;

    /** Generated Java classes package name. */
    private Text _javaClassesPackageNameText = null;

    /** Where JAXB classes reside. */
    private Text _jaxbBinDirText = null;

    /** Where COXB classes reside. */
    private Text _coxbBinDirText = null;

    /** Where custom classes reside. */
    private Text _custBinDirText = null;

    /** Where generated sources reside. */
    private Text _targetSrcDirText = null;

    /** Where generated classes reside. */
    private Text _targetBinDirText = null;

    /** Where generated ant scripts reside. */
    private Text _targetAntDirText = null;

    /** Where generated distribution archives files reside. */
    private Text _targetDistDirText = null;

    /** The host character set. */
    private Text _hostCharsetText = null;

    /**
     * Construct the page.
     * 
     * @param pageName the page name
     * @param pageTitle the page title
     * @param pageDesc the page description
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     * @param genModel the generation model
     */
    protected AbstractCixsGeneratorWizardPage(
            final IStructuredSelection selection,
            final String pageName,
            final String pageTitle,
            final String pageDesc,
            final IFile mappingFile,
            final AbstractAntBuildCixsModel genModel) {
        super(selection, pageName, pageTitle, pageDesc);
        _mappingFile = mappingFile;
        _genModel = genModel;
    }

    /** {@inheritDoc} */
    public void createExtendedControls(final Composite parent) {

        addCixsGroup(parent);

        final TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
        final GridData groupGridData = new GridData(GridData.FILL_HORIZONTAL);
        groupGridData.horizontalSpan = LAYOUT_COLUMNS;
        tabFolder.setLayoutData(groupGridData);

        addDeploymentGroup(tabFolder);
        addTargetGroup(tabFolder);
        addCoxbGroup(tabFolder);
    }

    /**
     * Groups parameters that generically describe generated artifacts.
     * 
     * @param container parent container
     */
    protected void addCixsGroup(final Composite container) {
        Group group = createGroup(container,
                Messages.generation_project_label, 2);
        createLabel(group, Messages.generation_project_name_label + ':');
        _serviceNameText = createText(group);

        createLabel(group, Messages.generation_java_package_label + ':');
        _javaClassesPackageNameText = createText(group);

        addWidgetsToCixsGroup(group);
    }

    /**
     * Groups JAXB and COXB classes locations. These are needed for the
     * generation process.
     * For convenience we use directoryFieldEditors which are normally used
     * in preference pages. These controls change the layout of their parent
     * container. This is why we create canvas within a canvas in order to
     * preserve the parent canvas layout.
     * 
     * @param tabFolder the parent folder
     */
    private void addCoxbGroup(final TabFolder tabFolder) {
        Canvas canvas = createCanvasInFolder(tabFolder,
                Messages.structures_binding_classes_label,
                LAYOUT_COLUMNS);
        Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);

        _jaxbBinDirText = createDirectoryFieldEditor(canvas2,
                Messages.jaxb_classes_location_label + ':');
        _coxbBinDirText = createDirectoryFieldEditor(canvas2,
                Messages.coxb_classes_location_label + ':');
        _custBinDirText = createDirectoryFieldEditor(canvas2,
                Messages.cust_classes_location_label + ':');

        addWidgetsToCoxbGroup(canvas2);
    }

    /**
     * Groups all target folders where artifacts will be generated.
     * 
     * @param tabFolder the parent folder
     */
    private void addTargetGroup(final TabFolder tabFolder) {
        Canvas canvas = createCanvasInFolder(
                tabFolder, Messages.generation_target_locations,
                LAYOUT_COLUMNS);
        Canvas canvas2 = createCanvas(canvas, LAYOUT_COLUMNS);

        _targetSrcDirText = createDirectoryFieldEditor(canvas2,
                Messages.java_sources_target_location + ':');
        _targetBinDirText = createDirectoryFieldEditor(canvas2,
                Messages.java_classes_target_location + ':');
        _targetAntDirText = createDirectoryFieldEditor(canvas2,
                Messages.ant_scripts_target_location + ':');
        _targetDistDirText = createDirectoryFieldEditor(canvas2,
                Messages.distribution_files_target_location + ':');

        addWidgetsToTargetGroup(canvas2);
    }

    /**
     * Groups the parameters needed to deploy the generated artifacts.
     * 
     * @param tabFolder the parent folder
     */
    private void addDeploymentGroup(final TabFolder tabFolder) {
        Canvas canvas = createCanvasInFolder(
                tabFolder, Messages.deployment_group_label, 2);

        _hostCharsetText = createTextField(canvas, getCommonStore(),
                com.legstar.eclipse.plugin.common.preferences
                .PreferenceConstants.HOST_CHARSET,
                Messages.mainframe_charset_label + ':');

        addWidgetsToDeploymentGroup(canvas);
    }

    /**
     * Initialize all fields.
     */
    public void initContents() {
        setServiceName(getInitServiceName());
        setJavaClassesPackageName(getInitJavaClassesPackageName());
        setTargetSrcDir(getInitTargetSrcDir());
        setTargetBinDir(getInitTargetBinDir());

        /* Initialized after target bin dir because they depend on it */
        setJaxbBinDir(getInitJaxbBinDir());
        setCoxbBinDir(getInitCoxbBinDir());
        setCustBinDir(getInitCustBinDir());

        setTargetAntDir(getInitTargetDir(
                getGenModel().getTargetAntDir(),
                getCommonStore(),
                com.legstar.eclipse.plugin.common.preferences.PreferenceConstants.ANT_SCRIPTS_FOLDER));
        setTargetDistDir(getInitTargetDir(getGenModel().getTargetDistDir(),
                PreferenceConstants.DEFAULT_CIXS_TARGET_DIST_FOLDER));

        setHostCharset(getInitHostCharset());

        initExtendedWidgets(getMappingFile().getProject());

        createListeners();
    }

    /**
     * Creation of listeners on controls is separated from control creation.
     * This allows listeners to be created after all fields have been
     * initialized
     * and avoid triggering dialogChanged on initialization.
     */
    public void createListeners() {

        _serviceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _javaClassesPackageNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _jaxbBinDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _coxbBinDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _custBinDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetSrcDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetBinDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetAntDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetDistDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _hostCharsetText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createExtendedListeners();
    }

    /**
     * Creation of listeners on controls is separated from control creation.
     * This allows listeners to be created after all fields have been
     * initialized
     * and avoid triggering dialogChanged on initialization.
     */
    public abstract void createExtendedListeners();

    /**
     * Gets an initial value for the service name.
     * <p/>
     * Value is retrieved from the Model first (The Model will typically hold
     * the last values used) and if not found, a new name is built from the
     * mapping file name:
     * 
     * <pre>
     *  &lt;cixsMapping name="thisname"&gt;
     * </pre>
     * 
     * @return an initial service name
     */
    protected String getInitServiceName() {
        return getGenModel().getCixsService().getName();

    }

    /**
     * Gets an initial value for the generated java classes package name.
     * <p/>
     * Value is retrieved from the Model first (The Model will typically hold
     * the last values used) and if not found, a new name is built from the
     * service name and a preferred prefix.
     * 
     * @return an initial generated java classes package name
     */
    protected String getInitJavaClassesPackageName() {
        String initValue = getGenModel().getCixsService().getPackageName();
        if (initValue == null) {
            String prefix = getStore().getString(
                    PreferenceConstants.DEFAULT_CIXS_PACKAGE_NAME_PREFIX);
            if (prefix == null || prefix.length() == 0) {
                initValue = getServiceName().toLowerCase();
            } else {
                initValue = prefix + '.' + getServiceName().toLowerCase();
            }
        }
        return initValue;
    }

    /**
     * Attempts to retrieve a target source directory from the Model. If none is
     * found, then the target source will be the first source directory of the
     * Eclipse project.
     * 
     * @return an initial source directory
     */
    protected String getInitTargetSrcDir() {
        File initFile = getGenModel().getTargetSrcDir();
        if (initFile == null) {
            initFile = getJavaDir(getMappingFile().getProject(), true);
        }
        return getDisplayPath(initFile);
    }

    /**
     * Attempts to retrieve a target binaries directory from the Model. If none
     * is found, then the target binaries will be the one that is associated
     * with the first source directory of the Eclipse project.
     * 
     * @return an initial binaries directory
     */
    protected String getInitTargetBinDir() {
        File initFile = getGenModel().getTargetBinDir();
        if (initFile == null) {
            initFile = getJavaDir(getMappingFile().getProject(), false);
        }
        return getDisplayPath(initFile);
    }

    /**
     * Attempts to retrieve a JAXB binaries directory from the Model. If none is
     * found, then the target binaries directory is assumed.
     * 
     * @return a JAXB binaries directory
     */
    protected String getInitJaxbBinDir() {
        File initFile = getGenModel().getJaxbBinDir();
        if (initFile == null) {
            return getTargetBinDir();
        }
        return getDisplayPath(initFile);
    }

    /**
     * Attempts to retrieve a COXB binaries directory from the Model. If none is
     * found, then the target binaries directory is assumed.
     * 
     * @return a COXB binaries directory
     */
    protected String getInitCoxbBinDir() {
        File initFile = getGenModel().getCoxbBinDir();
        if (initFile == null) {
            return getTargetBinDir();
        }
        return getDisplayPath(initFile);
    }

    /**
     * Attempts to retrieve a custom binaries directory from the Model. If none
     * is found, then the target binaries directory is assumed.
     * 
     * @return a custom binaries directory
     */
    protected String getInitCustBinDir() {
        File initFile = getGenModel().getCustBinDir();
        if (initFile == null) {
            return getTargetBinDir();
        }
        return getDisplayPath(initFile);
    }

    /**
     * Attempts to retrieve the host character set from the Mode. If none is
     * found the general default one is returned.
     * 
     * @return an initial host character set
     */
    protected String getInitHostCharset() {
        String initValue = getGenModel().getHostCharset();
        if (initValue == null) {
            initValue = getCommonStore().getDefaultString(
                    com.legstar.eclipse.plugin.common.preferences
                    .PreferenceConstants.HOST_CHARSET);
        }
        return initValue;
    }

    /**
     * Perform validation on data entered so far.
     */
    public void dialogChanged() {

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

        if (!checkDirectory(getTargetDistDir(),
                NLS.bind(Messages.invalid_location_msg,
                        Messages.distribution_files_target_location))) {
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
        updateGenModel();

    }

    /**
     * Store the dialog values in the generation Model.
     */
    protected void updateGenModel() {
        getGenModel().getCixsService().setName(getServiceName());
        getGenModel().getCixsService().setPackageName(
                getJavaClassesPackageName());
        getGenModel().setTargetSrcDir(new File(getTargetSrcDir()));
        getGenModel().setTargetBinDir(new File(getTargetBinDir()));

        getGenModel().setJaxbBinDir(new File(getJaxbBinDir()));
        getGenModel().setCoxbBinDir(new File(getCoxbBinDir()));
        getGenModel().setCustBinDir(new File(getCustBinDir()));

        getGenModel().setTargetAntDir(new File(getTargetAntDir()));

        getGenModel().setHostCharset(getHostCharset());

        updateGenModelExtended();

        try {
            getProjectPreferences().flush();
        } catch (BackingStoreException e) {
            AbstractWizard.logCoreException(e,
                    getActivator().getPluginId());
        }
    }

    /**
     * Save the dialog values into the generation Model.
     */
    public abstract void updateGenModelExtended();

    /**
     * Entry point for derived classes to add more widgets on the main group.
     * 
     * @param container the parent container
     */
    public abstract void addWidgetsToCixsGroup(final Composite container);

    /**
     * Entry point for derived classes to add more widgets on the binding group.
     * 
     * @param container the parent container
     */
    public abstract void addWidgetsToCoxbGroup(final Composite container);

    /**
     * Entry point for derived classes to add more widgets on the target group.
     * 
     * @param container the parent container
     */
    public abstract void addWidgetsToTargetGroup(final Composite container);

    /**
     * Entry point for derived classes to add more widgets on the deployment
     * group.
     * 
     * @param container the parent container
     */
    public abstract void addWidgetsToDeploymentGroup(final Composite container);

    /**
     * Give derived classes a chance to initialize their private widgets.
     * 
     * @param project the current Eclipse project
     */
    public abstract void initExtendedWidgets(final IProject project);

    /**
     * Give derived classes a chance to validate their private widgets.
     * 
     * @return true if widget contrnt is valid, false otherwise
     */
    public abstract boolean validateExtendedWidgets();

    /** @return the subclass plugin. */
    public abstract AbstractCixsActivator getActivator();

    /**
     * Create a container in a tabfolder.
     * 
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
     * From a Java nature Eclipse project, this utility method retrieves either
     * a java source folder or an associated java output folder for binaries.
     * <p/>
     * The algorithm stops at the first source directory encountered.
     * 
     * @param project the current Eclipse project
     * @param source true if we are looking for a source folder, false for an
     *            output folder
     * @return a java folder (either source or output)
     */
    protected File getJavaDir(final IProject project, final boolean source) {
        IJavaProject javaProject = JavaCore.create(project);
        if (javaProject != null) {
            IPath rootPath =
                    javaProject.getProject().getLocation()
                            .removeLastSegments(1);

            /*
             * If this is a java project, get first Java source and output
             * folders.
             */
            try {
                IClasspathEntry[] cpe = javaProject.getRawClasspath();
                for (int i = 0; i < cpe.length; i++) {
                    if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
                        if (source) {
                            return rootPath.append(cpe[i].getPath()).toFile();
                        } else {
                            if (cpe[i].getOutputLocation() == null) {
                                return rootPath.append(
                                        javaProject.getOutputLocation())
                                        .toFile();
                            } else {
                                return rootPath.append(
                                        cpe[i].getOutputLocation())
                                        .toFile();
                            }
                        }
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
        /*
         * If everything else failed, assume generated artifacts will go to the
         * project root.
         */
        return project.getLocation().toFile();
    }

    /**
     * Target locations have default values that are built relative to the
     * project containing the mapping file and preferences taken from a
     * preference store.
     * This will make sure the default location exists.
     * 
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
     * Get a location for a target directory. This is the absolute location of
     * the directory parameter received or a default location if that parameter
     * is null or invalid.
     * 
     * @param dir the candidate target directory
     * @param storeKey the preference key to use for default values
     * @return a unique non ambiguous file location or null if File is null
     */
    protected String getInitTargetDir(final File dir, final String storeKey) {
        return getInitTargetDir(dir, getStore(), storeKey);
    }

    /**
     * Get a location for a target directory. This is the absolute location of
     * the directory parameter received or a default location if that parameter
     * is null or invalid.
     * 
     * @param dir the candidate target directory
     * @param store a preference store from which to fetch the default
     * @param storeKey the preference key to use for default values
     * @return a unique non ambiguous file location or null if File is null
     */
    protected String getInitTargetDir(final File dir,
            final IPreferenceStore store, final String storeKey) {
        String dirPath = getDisplayPath(dir);
        if (dirPath == null) {
            dirPath = getDefaultTargetDir(getStore(), storeKey);
        }
        return dirPath;
    }

    /**
     * Turns a file into a displayable path. The objective is that there is no
     * ambiguity as to the location of the file so we avoid relative path
     * locations.
     * 
     * @param file the file whose location is to be displayed
     * @return a unique non ambiguous file location or null if File is null
     */
    protected String getDisplayPath(final File file) {
        if (file == null) {
            return null;
        }
        try {
            return file.getCanonicalPath();
        } catch (IOException e) {
            /* An invalid file is considered non displayable */
            return null;
        }
    }

    /**
     * The target source folder is part of a Java project. It is assumed
     * this was tested in the validation step so we can safely infer the
     * java nature here.
     * 
     * @return the target java project
     */
    public IJavaProject getTargetJavaProject() {
        IResource resource = ResourcesPlugin.getWorkspace().getRoot()
                .getContainerForLocation(new Path(getTargetSrcDir()));
        return JavaCore.create(resource.getProject());
    }

    /**
     * Checks that a folder exists.
     * 
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
        _serviceNameText.setText(serviceName);
    }

    /**
     * @return Service name
     */
    public String getServiceName() {
        return _serviceNameText.getText();
    }

    /**
     * @param javaClassesPackageName Generated java classes package name
     */
    public void setJavaClassesPackageName(
            final String javaClassesPackageName) {
        _javaClassesPackageNameText.setText(javaClassesPackageName);
    }

    /**
     * @return Generated java classes package name
     */
    public String getJavaClassesPackageName() {
        return _javaClassesPackageNameText.getText();
    }

    /**
     * @param jaxbBinDirLocation Where JAXB classes reside
     */
    public void setJaxbBinDir(final String jaxbBinDirLocation) {
        _jaxbBinDirText.setText(jaxbBinDirLocation);
    }

    /**
     * @return Where JAXB classes reside
     */
    public String getJaxbBinDir() {
        return _jaxbBinDirText.getText();
    }

    /**
     * @param coxbBinDirLocation Where Binding classes reside
     */
    public void setCoxbBinDir(final String coxbBinDirLocation) {
        _coxbBinDirText.setText(coxbBinDirLocation);
    }

    /**
     * @return Where Binding classes reside
     */
    public String getCoxbBinDir() {
        return _coxbBinDirText.getText();
    }

    /**
     * @param custBinDirLocation Where custom classes reside
     */
    public void setCustBinDir(final String custBinDirLocation) {
        _custBinDirText.setText(custBinDirLocation);
    }

    /**
     * @return Where custom classes reside
     */
    public String getCustBinDir() {
        return _custBinDirText.getText();
    }

    /**
     * @param targetSrcDirLocation Where generated sources reside
     */
    public void setTargetSrcDir(final String targetSrcDirLocation) {
        _targetSrcDirText.setText(targetSrcDirLocation);
    }

    /**
     * @return Where generated sources reside
     */
    public String getTargetSrcDir() {
        return _targetSrcDirText.getText();
    }

    /**
     * @param targetBinDirLocation Where generated classes reside
     */
    public void setTargetBinDir(final String targetBinDirLocation) {
        _targetBinDirText.setText(targetBinDirLocation);
    }

    /**
     * @return Where generated classes reside
     */
    public String getTargetBinDir() {
        return _targetBinDirText.getText();
    }

    /**
     * @param targetAntDirLocation Where generated ant scripts reside
     */
    public void setTargetAntDir(final String targetAntDirLocation) {
        _targetAntDirText.setText(targetAntDirLocation);
    }

    /**
     * @return Where generated ant scripts reside
     */
    public String getTargetAntDir() {
        return _targetAntDirText.getText();
    }

    /**
     * @param targetDistDirLocation Where generated distribution archives reside
     */
    public void setTargetDistDir(final String targetDistDirLocation) {
        _targetDistDirText.setText(targetDistDirLocation);
    }

    /**
     * @return Where generated generated distribution archives reside
     */
    public String getTargetDistDir() {
        return _targetDistDirText.getText();
    }

    /**
     * @param hostCharset the mainframe character set
     */
    public void setHostCharset(final String hostCharset) {
        _hostCharsetText.setText(hostCharset);
    }

    /**
     * @return the mainframe character set
     */
    public String getHostCharset() {
        return _hostCharsetText.getText();
    }

    /**
     * @return the mapping file
     */
    public IFile getMappingFile() {
        return _mappingFile;
    }

    /**
     * @return the preference store managed by the common plugin.
     */
    public IPreferenceStore getCommonStore() {
        return com.legstar.eclipse.plugin.common.Activator.getDefault()
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

    /**
     * @return the project scope preferences
     */
    public IEclipsePreferences getProjectPreferences() {
        return ((AbstractCixsGeneratorWizard) getWizard())
                .getProjectPreferences();
    }

    /**
     * @return the data model
     */
    public AbstractAntBuildCixsModel getGenModel() {
        return _genModel;
    }
}
