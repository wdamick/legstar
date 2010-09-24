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
package com.legstar.eclipse.plugin.coxbgen.wizards;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.stream.StreamSource;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaException;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.apache.ws.commons.schema.XmlSchemaObjectCollection;
import org.apache.ws.commons.schema.XmlSchemaType;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;

import com.legstar.coxb.gen.CoxbGenException;
import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.coxb.util.Utils;
import com.legstar.eclipse.plugin.common.ClasspathInitializer;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.coxbgen.Messages;

/**
 * This basic version of the generation UI will display some of the parameters
 * that drive the generation process and give a chance to cancel the generation
 * if necessary.
 */

public class CoxbGenWizardPage extends AbstractWizardPage {

    /** XML schema label field. */
    private Label mXsdFileLabel;

    /** XML schema file. */
    private IFile _xsdFile;

    /** JAXB package name. */
    private Text _jaxbPackageName;

    /** COXB package name. */
    private Text _coxbPackageName;

    /** An instance of the generator model. */
    CoxbGenModel _coxbModel;

    /** List of complex types from XML schema. . */
    private List mJaxbRootClassNamesList;

    /** Target source directory for generated sources. */
    private Text mTargetSrcDirText;

    /** Target binaries directory for generated sources. */
    private Label mTargetBinDirLabel;

    /**
     * A pattern to validate characters entered for package names
     * This applies to characters entered, not the complete package
     * string.
     */
    private Pattern _pkgnamePattern = Pattern
            .compile("[a-zA-Z0-9\\.]*");

    /** Page name. */
    private static final String PAGE_NAME =
            "CoxbGenWizardPage";

    /**
     * Constructor for CoxbGenWizardPage from existing XML schema file.
     * By default, the target project is the Xsd file containing project.
     * 
     * @param selection current workbench selection
     * @param coxbModel the data model
     * @param xsdFile XML schema file
     */
    public CoxbGenWizardPage(
            final IStructuredSelection selection,
            final IFile xsdFile,
            final CoxbGenModel coxbModel) {
        super(selection, PAGE_NAME,
                Messages.wizard_page_title, Messages.wizard_page_description);
        _xsdFile = xsdFile;
        _coxbModel = coxbModel;
        _coxbModel.setXsdFile(_xsdFile.getLocation().toFile());
    }

    /** {@inheritDoc} */
    public void createExtendedControls(final Composite container) {
        /* JAXB related parameters */
        Group group = createGroup(container, Messages.jaxb_group_label);
        createLabel(group, Messages.xsd_file_name_label + ':');
        mXsdFileLabel = createLabel(group, "", 2);

        createLabel(group, Messages.jaxb_package_name_label + ':');
        _jaxbPackageName = createText(group);
        _jaxbPackageName.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _jaxbPackageName.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(final VerifyEvent e) {
                verifyPackageName(e);
            }

        });
        final Button jaxbOptionsButton = createButton(group,
                Messages.jaxb_options_button_label);
        jaxbOptionsButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                CoxbGenJaxbOtionsDialog dialog = new CoxbGenJaxbOtionsDialog(
                        getShell(), getCoxbModel().getJaxbXjbModel());
                if (dialog.open() == CoxbGenJaxbOtionsDialog.OK) {
                    handleXJBParameters(dialog);
                }
            }
        });

        /* root name edit box */
        createLabel(container, Messages.root_elements_list_label + ':', 3);
        mJaxbRootClassNamesList = new List(
                container, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = LAYOUT_COLUMNS;
        gd.heightHint = 200;
        mJaxbRootClassNamesList.setLayoutData(gd);
        mJaxbRootClassNamesList.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(final SelectionEvent e) {
                dialogChanged();
            }

            public void widgetSelected(final SelectionEvent e) {
                dialogChanged();
            }
        });
        mJaxbRootClassNamesList.setFocus();

        createLabel(container, Messages.coxb_package_name_label + ':');
        _coxbPackageName = createText(container);
        _coxbPackageName.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _coxbPackageName.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(final VerifyEvent e) {
                verifyPackageName(e);
            }

        });
        final Button coxbOptionsButton = createButton(container,
                Messages.coxb_options_button_label);
        coxbOptionsButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                CoxbGenOptionsDialog dialog = new CoxbGenOptionsDialog(
                        getShell(), getCoxbModel());
                dialog.open();
            }
        });

        /* Target source folder edit box and browse button */
        createLabel(container, Messages.target_source_folder_label + ':');
        mTargetSrcDirText = createText(container);
        mTargetSrcDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createBrowseForContainerButton(container,
                Messages.target_source_folder_select_label,
                mTargetSrcDirText);

        createLabel(container, Messages.target_classes_folder_label + ':');
        mTargetBinDirLabel = createLabel(container, "", 2);

    }

    /**
     * Make sure we have a valid package name.
     * 
     * @param e the package change event
     */
    protected void verifyPackageName(final VerifyEvent e) {
        Matcher m = _pkgnamePattern.matcher(e.text);
        if (!m.matches()) {
            e.doit = false;
        }
    }

    /**
     * Initialize all fields.
     */
    public void initContents() {
        try {
            if (getXsdFile() == null) {
                throwCoreException(Messages.no_xsd_file_msg);
            }
            mXsdFileLabel.setText(getXsdFile().getName());
            initModel(getXsdFile());
            initRootElements(getXsdFile());
            initTargetSrcDir(getXsdFile());
        } catch (CoreException e) {
            errorDialog(getShell(),
                    Messages.generate_error_dialog_title,
                    Activator.PLUGIN_ID,
                    Messages.page_initialization_failure_msg,
                    e.getMessage());

            /* No use continuing if can't even initialize */
            throw new RuntimeException(e);
        }
    }

    /**
     * Create an initial model based on the selected XML Schema.
     * 
     * @param xsdFile the selected XMl Schema
     * @throws CoreException if model cannot be initialized
     */
    protected void initModel(final IFile xsdFile) throws CoreException {
        _coxbModel.setXsdFile(xsdFile.getLocation().toFile());
        try {
            _jaxbPackageName.setText(_coxbModel.getJaxbPackageName());
            _coxbPackageName.setText(_coxbModel.getCoxbPackageName());
        } catch (CoxbGenException e) {
            throwCoreException(e);
        }
    }

    /**
     * Loads a COBOL-annotated XML Schema and populates related widgets.
     * 
     * @param xsdFile the XML schema file from the workspace
     * @throws CoreException if loading fails. XML Schema file is probably
     *             invalid.
     */
    protected void initRootElements(final IFile xsdFile) throws CoreException {
        try {
            mJaxbRootClassNamesList.removeAll();
            InputStream is = new FileInputStream(
                    xsdFile.getLocation().toFile());
            XmlSchemaCollection schemaCol = new XmlSchemaCollection();
            XmlSchema schema = schemaCol.read(new StreamSource(is), null);
            XmlSchemaObjectCollection items = schema.getItems();
            for (int i = 0; i < items.getCount(); i++) {
                XmlSchemaObject obj = items.getItem(i);
                if (obj instanceof XmlSchemaComplexType) {
                    loadXmlSchemaComplexType(xsdFile,
                            (XmlSchemaComplexType) obj);
                }
            }
            for (int i = 0; i < items.getCount(); i++) {
                XmlSchemaObject obj = items.getItem(i);
                if (obj instanceof XmlSchemaElement) {
                    loadXmlSchemaRootElement(xsdFile,
                            (XmlSchemaElement) obj);
                }
            }

        } catch (FileNotFoundException e) {
            throwCoreException(e);
        } catch (XmlSchemaException e) {
            throwCoreException(e);
        }
    }

    /**
     * All complex types are potential root elements. The name displayed is
     * the expected JAXB Class name including any suffix if requested.
     * 
     * @param xsdFile the XML schema file from the workspace
     * @param xsdComplexType the complex type
     */
    protected void loadXmlSchemaComplexType(
            final IFile xsdFile, final XmlSchemaComplexType xsdComplexType) {
        String normalizedName = Utils.toClassName(xsdComplexType.getName());
        if (getCoxbModel().getTypeNamePrefix() != null) {
            normalizedName = getCoxbModel().getTypeNamePrefix()
                    + normalizedName;
        }
        if (getCoxbModel().getTypeNameSuffix() != null) {
            normalizedName += getCoxbModel().getTypeNameSuffix();
        }
        mJaxbRootClassNamesList.add(normalizedName);
    }

    /**
     * Root XSD elements are also potential root elements.
     * If the element type is a named complex type, it is already present in
     * the list so no need to display it. This means we only add elements
     * with anonymous complex types here.
     * The root class name is the JAXB class name.
     * 
     * @param xsdFile the XML schema file from the workspace
     * @param xsdElement element
     */
    protected void loadXmlSchemaRootElement(
            final IFile xsdFile, final XmlSchemaElement xsdElement) {
        XmlSchemaType xsdType = xsdElement.getSchemaType();
        if (xsdType.getName() == null) {
            String normalizedName = Utils.toClassName(xsdElement.getName());
            if (getCoxbModel().getElementNamePrefix() != null) {
                normalizedName = getCoxbModel().getElementNamePrefix()
                        + normalizedName;
            }
            if (getCoxbModel().getElementNameSuffix() != null) {
                normalizedName += getCoxbModel().getElementNameSuffix();
            }
            mJaxbRootClassNamesList.add(normalizedName);
        }
    }

    /**
     * Initially, try to set the target src dir as the first source directory of
     * the project containing the Xsd file. If that project is not a Java
     * project leave the field not initialized.
     * 
     * @param xsdFile the XML schema file from the workspace
     * @throws CoreException if project is invalid
     */
    protected void initTargetSrcDir(final IFile xsdFile) throws CoreException {
        try {
            IJavaProject jproject = JavaCore.create(xsdFile.getProject());
            if (jproject == null) {
                throwCoreException(NLS.bind(
                        Messages.xsd_file_in_invalid_project_msg,
                                xsdFile.getLocation().toOSString()));
            }
            IClasspathEntry[] cpe = jproject.getRawClasspath();
            /* Find the first source location */
            for (int i = 0; i < cpe.length; i++) {
                if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
                    mTargetSrcDirText.setText(cpe[i].getPath().toOSString());
                    break;
                }
            }
        } catch (JavaModelException e) {
            throwCoreException(e);
        }
    }

    /**
     * Perform validation on data entered so far.
     */
    public void dialogChanged() {

        /* Validate the source directory as a Java project source folder */
        if (!isJavaSrcDir(getSrcDirRelativePathName())) {
            updateStatus(Messages.invalid_target_src_folder_msg);
            return;
        }

        /* Make sure at least one root name is selected */
        if (getJaxbRootClassNames().size() == 0) {
            updateStatus(Messages.no_root_elements_selected_msg);
            return;
        }

        /* Make sure the target project has LegStar libraries on its classpath */
        if (!lookupContainerLibrary(getTargetJavaProject(),
                ClasspathInitializer.LIBRARY_NAME)) {
            try {
                setupContainerLibrary(getTargetJavaProject(),
                        ClasspathInitializer.LIBRARY_NAME);
            } catch (JavaModelException e) {
                updateStatus(NLS.bind(
                        Messages.classpath_setup_failure_msg,
                        getSrcDirRelativePathName(), e.getMessage()));
                return;
            }
        }

        updateStatus(null);
        updateCoxbModel();

    }

    /**
     * Update the model with UI field values.
     */
    protected void updateCoxbModel() {
        getCoxbModel().setJaxbPackageName(getValueFromText(_jaxbPackageName));
        getCoxbModel().setCoxbPackageName(getValueFromText(_coxbPackageName));
        getCoxbModel().setJaxbSrcDir(new File(
                getPathName(getSrcDirRelativePathName())));
        getCoxbModel().setJaxbBinDir(new File(
                getPathName(getBinDirRelativePathName())));
        getCoxbModel().setCoxbSrcDir(getCoxbModel().getJaxbSrcDir());
        getCoxbModel().setCoxbBinDir(getCoxbModel().getJaxbBinDir());
        getCoxbModel().setJaxbRootClassNames(getJaxbRootClassNames());
    }

    /**
     * Construct a full path name from a relative one.
     * 
     * @param relativePathName a pathname relative to the workspace root
     * @return a full path name
     */
    protected String getPathName(final String relativePathName) {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        IContainer container = (IContainer) root.findMember(relativePathName);
        return container.getLocation().toOSString();
    }

    /**
     * When JAXB XJB parameters have been changed, we need to reload
     * the class root list because item names in that list reflect
     * the effect of the XJB parameters.
     * 
     * @param dialog the XJB parameters dialog
     */
    protected void handleXJBParameters(final CoxbGenJaxbOtionsDialog dialog) {
        try {
            initRootElements(getXsdFile());
        } catch (CoreException e) {
            errorDialog(getShell(),
                    Messages.generate_error_dialog_title,
                    Activator.PLUGIN_ID,
                    Messages.page_initialization_failure_msg,
                    e.getMessage());
        }

    }

    /**
     * Check if a relative path name is a valid java source directory.
     * Also sets the target binary folder.
     * 
     * @param relativePathName the path name
     * @return true if this is a valid java source folder
     */
    protected boolean isJavaSrcDir(final String relativePathName) {
        IResource resource = ResourcesPlugin.getWorkspace().getRoot()
                .findMember(new Path(relativePathName));
        if (resource != null) {
            IJavaProject jproject = JavaCore.create(resource.getProject());
            if (jproject != null) {
                try {
                    IClasspathEntry[] cpe = jproject.getRawClasspath();
                    /* Lookup the pathname */
                    for (int i = 0; i < cpe.length; i++) {
                        if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
                            if (relativePathName.equals(
                                    cpe[i].getPath().toOSString())) {
                                if (cpe[i].getOutputLocation() != null) {
                                    mTargetBinDirLabel.setText(
                                            cpe[i].getOutputLocation()
                                                    .toOSString());
                                } else {
                                    mTargetBinDirLabel.setText(
                                            jproject.getOutputLocation()
                                                    .toOSString());
                                }
                                return true;
                            }
                        }
                    }
                } catch (JavaModelException e) {
                    return false;
                }
            }
        }
        return false;
    }

    /**
     * The target source folder is part of a Java project. This is validated
     * by <code>isJavaSrcDir</code>.
     * 
     * @return the target java project
     */
    protected IJavaProject getTargetJavaProject() {
        IResource resource = ResourcesPlugin.getWorkspace().getRoot()
                .findMember(new Path(getSrcDirRelativePathName()));
        return JavaCore.create(resource.getProject());
    }

    /**
     * @return the source directory name
     */
    public String getSrcDirRelativePathName() {
        return mTargetSrcDirText.getText();
    }

    /**
     * Retrieves the output location for java classes associated with a
     * given java source folder (assumed to be valid).
     * 
     * @return the binaries output location
     */
    public String getBinDirRelativePathName() {
        return mTargetBinDirLabel.getText();
    }

    /**
     * @return the root type name
     */
    public java.util.List < String > getJaxbRootClassNames() {
        java.util.List < String > result = new java.util.ArrayList < String >();
        for (String className : mJaxbRootClassNamesList.getSelection()) {
            result.add(className);
        }
        return result;
    }

    /**
     * @return the XML Schema file
     */
    public IFile getXsdFile() {
        return _xsdFile;
    }

    /**
     * @return the data model
     */
    public CoxbGenModel getCoxbModel() {
        return _coxbModel;
    }

}
