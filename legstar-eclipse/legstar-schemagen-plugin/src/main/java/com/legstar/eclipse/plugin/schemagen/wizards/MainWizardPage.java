/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.File;
import java.util.Properties;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.SourceToXsdCobolModel;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardRunnable;
import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.Messages;
import com.legstar.eclipse.plugin.schemagen.preferences.PreferenceConstants;

/**
 * This is the first XSD generation wizard page. This is where the user
 * selects the type of source he would like to start from and determines
 * the target location and parameters for the XSD.
 * 
 */
public class MainWizardPage extends AbstractToXsdWizardPage {

    /** Selection of available source types. */
    private Combo _sourceTypeCombo;

    /** Destination container. */
    private Text _targetContainerText;

    /** Destination file name. */
    private Text _targetXSDFileNameText;

    /** Whether the XSD file should be overwritten if it already exist. */
    private boolean _overwriteAllowed = true;

    /** Target XML schema namespace. */
    private Text _targetNamespaceText;

    /** The field values that are permanent. */
    private CommonModel _model;

    /**
     * Will be true is user explicitly modified the namespace. This means we
     * should refrain from trying to generate it automatically.
     */
    private boolean _xsdNamespaceUserChanged;

    /**
     * Constructs the main wizard page.
     * 
     * @param initialSelection the workbench current selection
     */
    public MainWizardPage(final IStructuredSelection initialSelection) {
        super(initialSelection,
                "MainWizardPage",
                Messages.main_wizard_page_title,
                Messages.main_wizard_page_description);
    }

    /** {@inheritDoc} */
    @Override
    public void createExtendedControls(final Composite container) {
        Group groupSource = createGroup(container,
                Messages.source_type_group_label);
        _sourceTypeCombo = createComboFromItemsArray(
                groupSource, new String[] {
                        Messages.cobol_source_type_text,
                        Messages.xsd_source_type_text,
                        Messages.java_source_type_text });

        Group groupTarget = createGroup(container,
                Messages.target_group_label);

        createLabel(groupTarget, Messages.container_label);
        _targetContainerText = createText(groupTarget);

        createBrowseForContainerButton(
                groupTarget, Messages.container_selection_label,
                _targetContainerText);

        createLabel(groupTarget, Messages.xsd_file_name_label);
        _targetXSDFileNameText = createText(groupTarget);
        _targetXSDFileNameText.setFocus();

        createOverwriteAllowedCheckButton(groupTarget,
                Messages.overwrite_button_label);

        createLabel(groupTarget, Messages.namespace_label);
        _targetNamespaceText = createText(groupTarget, 2);

    }

    /** {@inheritDoc} */
    public void initContents() {
        IProject project = initTargetContainer();

        // Whatever project was identified, tell the wizard so that other pages
        // know about it
        setProject(project);

        loadGenModel();
        createListeners();
    }

    /**
     * Initialize widgets from the Model.
     */
    public void loadGenModel() {
        if (getProject() != null) {
            _model = new CommonModel(loadProperties(getProjectPreferences()));
        } else {
            _model = new CommonModel();
        }

        if (_model.getTargetXsdFileName() != null
                && _model.getTargetXsdFileName().length() > 0) {
            _targetXSDFileNameText.setText(_model.getTargetXsdFileName());
        } else {
            _targetXSDFileNameText.setText(".xsd");
        }

        if (_model.getNamespace() != null
                && _model.getNamespace().length() > 0) {
            _targetNamespaceText.setText(_model.getNamespace());
        } else {
            _targetNamespaceText.setText(getDefaultNamespace());
        }

        if (_targetNamespaceText.getText().equals(getDefaultNamespace())) {
            _xsdNamespaceUserChanged = false;
        } else {
            _xsdNamespaceUserChanged = true;
        }

    }

    /** {@inheritDoc} */
    public void initProjectContent() {
        loadGenModel();
        getWizard().getCobolToXsdWizardPage().initProjectContent();
        getWizard().getJavaToXsdWizardPage().initProjectContent();
        getWizard().getXsdToXsdWizardPage().initProjectContent();
    }

    /**
     * Makes sure we start listening on widget changes only after they are
     * initialized.
     */
    public void createListeners() {
        _targetContainerText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetXSDFileNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetXSDFileNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                autoFill();
            }
        });
        _targetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

        // Detects user modifications to the namespace (as opposed to
        // modifications due to autofill)
        _targetNamespaceText.addFocusListener(new FocusListener() {

            String initialValue;

            public void focusGained(final FocusEvent e) {
                initialValue = ((Text) e.widget).getText();
            }

            public void focusLost(final FocusEvent e) {
                String newValue = ((Text) e.widget).getText();
                if (!_xsdNamespaceUserChanged) {
                    _xsdNamespaceUserChanged = !newValue.equals(initialValue);
                }

            }

        });
    }

    /**
     * Set the initial value of the target container field.
     * <p/>
     * If a project is selected in the workspace, then it is considered to be
     * the initial value.
     * <p/>
     * If no project is selected, we attempt to recover the last container used
     * from the default preferences.
     * <p/>
     * We are only interested in the project so if a lower level element is
     * selected we get the first segment of its path.
     * <p/>
     * Since we can't process multiple selections, we deal with the first one
     * only.
     * 
     * @return the selected project in the workbench or null if none is selected
     */
    protected IProject initTargetContainer() {
        IProject project = null;
        if (getInitialSelection() != null
                && !getInitialSelection().isEmpty()
                && getInitialSelection() instanceof TreeSelection) {

            Object obj = ((TreeSelection) getInitialSelection()).getPaths()[0]
                    .getFirstSegment();
            if (obj instanceof IProject) {
                project = (IProject) obj;
            } else if (obj instanceof IJavaProject) {
                project = ((IJavaProject) obj).getProject();
            }
        }
        if (project == null) {
            String lastContainerPath = getDefaultPreferences().getString(
                    PreferenceConstants.LAST_TARGET_CONTAINER);
            if (lastContainerPath.length() > 0) {
                // Make sure it still exist in the workbench
                project = AbstractWizardRunnable
                        .getProject(lastContainerPath);
                if (project != null && project.isOpen()) {
                    _targetContainerText.setText(lastContainerPath);
                } else {
                    project = null;
                }
            }
        } else {
            _targetContainerText.setText(
                    project.getFullPath().toOSString());
        }

        return project;
    }

    /**
     * Create a default namespace using the preferred prefix and the XSD file
     * name.
     * 
     * @return the value used to initialize the text widget
     */
    private String getDefaultNamespace() {
        return getDefaultNamespacePrefix() + getXsdSimpleFileName();
    }

    /**
     * The default namespace prefix comes from the LegStar general preferences.
     * 
     * @return a default prefix terminated with slash
     */
    protected String getDefaultNamespacePrefix() {
        IPreferenceStore store = getDefaultPreferences();
        String str = store.getString(PreferenceConstants.XSD_NAMESPACE_PREFIX);
        if (str != null && str.length() != 0) {
            str = setSeparatorChar(str, '/');
            return str;
        }
        return "";
    }

    /**
     * If the input string is empty or null, return an empty string.
     * If the input string already ends with the segments separator char, return
     * the input string unchanged.
     * Otherwise, append a final segments separator character and return the
     * resulting string.
     * 
     * @param str input string
     * @param c the segments separator character
     * @return the input string with final continuation character
     */
    private String setSeparatorChar(final String str, final char c) {
        if (str == null || str.length() == 0) {
            return "";
        }
        if (str.charAt(str.length() - 1) == c) {
            return str;
        }
        return str + c;
    }

    /**
     * When user types in an xsd name, we can generally infer an appropriate
     * namespace unless the user has taken control of
     * these fields already.
     * Only the last segment of the xsd file name, excluding the extension,
     * contributes to other field values.
     */
    private void autoFill() {
        String str = getXsdSimpleFileName();
        if (!_xsdNamespaceUserChanged) {
            _targetNamespaceText.setText(getDefaultNamespacePrefix() + str);
        }
    }

    /**
     * @return the last segment of the xsd file name without an extension
     */
    private String getXsdSimpleFileName() {
        IPath path = new Path(
                _targetXSDFileNameText.getText()).removeFileExtension();
        if (path.lastSegment() == null) {
            return "";
        }
        return path.lastSegment();
    }

    /**
     * Adds a check button that reflects its state in an associated boolean
     * variable.
     * 
     * @param container parent composite
     * @param text the button text
     * @return a check button
     */
    private Button createOverwriteAllowedCheckButton(
            final Composite container,
            final String text) {
        final Button button = new Button(container, SWT.CHECK);
        button.setText(text);
        button.setSelection(isOverwriteAllowed());
        button.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                setOverwriteAllowed(!isOverwriteAllowed());
                dialogChanged();
            }
        });
        return button;
    }

    /** {@inheritDoc} */
    @Override
    public void dialogChanged() {

        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

        /* Target container should not be empty */
        String targetContainer = getTargetContainer();
        if (targetContainer.length() == 0) {
            updateStatus(Messages.no_target_container_msg);
            return;
        }

        /* Target container must exist and be a valid container */
        IResource containerResource = root.findMember(
                new Path(targetContainer));
        if (containerResource == null
                || !containerResource.exists()
                || !(containerResource instanceof IContainer)) {
            updateStatus(Messages.invalid_target_container_msg);
            return;
        }

        /* Make sure we have a valid project. */
        IProject project = containerResource.getProject();
        if (project == null || !project.isOpen()) {
            updateStatus(Messages.invalid_target_container_msg);
            return;
        }

        /*
         * If this is a different project than the one we had before, ignore
         * other changes to this page. We also offer a chance to the next
         * pages to initialize their project-dependent content
         */
        if (project != getProject()) {
            setProject(project);
            initProjectContent();
        }

        /* Target XML schema file name must not be empty */
        if (getXsdSimpleFileName().length() == 0) {
            updateStatus(Messages.no_target_xsd_file_name_msg);
            return;
        }

        String fileName = getTargetXSDFileName();
        /*
         * If overwrite is prohibited, then XML schema file must not exist in
         * target container
         */
        if (!isOverwriteAllowed()) {
            IResource fileResource = root.findMember(
                    new Path(targetContainer).append(fileName));
            if (fileResource != null && fileResource.exists()) {
                updateStatus(Messages.already_exists_target_xsd_file_msg);
                return;
            }
        }

        updateStatus(null);
        updateGenModel();
    }

    /**
     * Update the model with UI field values.
     */
    protected void updateGenModel() {
        _model.setTargetXsdFileName(getValueFromText(_targetXSDFileNameText));
        _model.setNamespace(getValueFromText(_targetNamespaceText));
    }

    /**
     * Store the selected values in the default scoped preference store.
     */
    public void storeDefaultPreferences() {
        getDefaultPreferences()
                .setValue(
                        PreferenceConstants.LAST_TARGET_CONTAINER,
                        getTargetContainer());
    }

    /**
     * The next page depends on the source type selected. {@inheritDoc}
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    public IWizardPage getNextPage() {
        AbstractWizardPage nextPage = null;
        switch (getSelectedSource()) {
        case 0:
            nextPage = getWizard().getCobolToXsdWizardPage();
            break;
        case 1:
            nextPage = getWizard().getXsdToXsdWizardPage();
            ((XsdToXsdWizardPage) nextPage).setNewTargetNamespace(
                    getTargetNamespace());
            break;
        case 2:
            nextPage = getWizard().getJavaToXsdWizardPage();
            break;
        default:
            break;
        }
        return nextPage;
    }

    /**
     * Returns the zero-relative index of the item which is currently selected
     * in the receiver's list, or -1 if no item is selected.
     * 
     * @return the source type that was selected on the first page.
     */
    public int getSelectedSource() {
        return _sourceTypeCombo.getSelectionIndex();
    }

    /**
     * @return true if the XSD file should be overwritten if it already exist
     */
    public boolean isOverwriteAllowed() {
        return _overwriteAllowed;
    }

    /**
     * @param overwriteAllowed true if the XSD file should be overwritten if it
     *            already exist
     */
    public void setOverwriteAllowed(final boolean overwriteAllowed) {
        _overwriteAllowed = overwriteAllowed;
    }

    /**
     * @return the destination container text
     */
    public String getTargetContainer() {
        return _targetContainerText.getText();
    }

    /**
     * @param destinationContainer the destination container text to set
     */
    public void setDestinationContainer(
            final String destinationContainer) {
        _targetContainerText.setText(destinationContainer);
    }

    /**
     * @return the target XML Schema file name
     */
    public String getTargetXSDFileName() {
        return _targetXSDFileNameText.getText();
    }

    /**
     * @param targetXSDFileName the target XML Schema file name to set
     */
    public void setTargetXSDFileName(
            final String targetXSDFileName) {
        _targetXSDFileNameText.setText(targetXSDFileName);
    }

    /**
     * @return the Target XML schema namespace. Returns null if namespace is
     *         spaces
     */
    public String getTargetNamespace() {
        if (_targetNamespaceText.getText().trim().length() == 0) {
            return null;
        }
        return _targetNamespaceText.getText();
    }

    /**
     * @param targetNamespace the Target XML schema namespace to set
     */
    public void setTargetNamespace(final String targetNamespace) {
        _targetNamespaceText.setText(targetNamespace);
    }

    /**
     * @return the default scope preferences
     */
    public IPreferenceStore getDefaultPreferences() {
        return getWizard().getDefaultPreferences();
    }

    /**
     * @return the project scope preferences
     */
    public IEclipsePreferences getProjectPreferences() {
        return getWizard().getProjectPreferences();
    }

    /**
     * Create a properties file from preferences content.
     * <p/>
     * In case we cannot recover the previous values, just log an error.
     * 
     * @param preferences an Eclipse preference store
     * @return a properties file
     */
    public Properties loadProperties(final IEclipsePreferences preferences) {
        try {
            return getWizard().loadProperties(preferences);
        } catch (BackingStoreException e) {
            logCoreException(e, Activator.PLUGIN_ID);
            return new Properties();
        }
    }

    /**
     * This wizard page is common to various translators (COBOL to XSD, XSD to
     * XSD, Java to XSD). So we don't necessarily know which one the user will
     * end up selecting. Here we define the minimal model that is common to all
     * translators so we can store/load previous values.
     * 
     */
    protected class CommonModel extends SourceToXsdCobolModel {

        /**
         * A no-Arg constructor.
         */
        public CommonModel() {
        }

        /**
         * Construct from a properties file.
         * 
         * @param props the property file
         */
        public CommonModel(final Properties props) {
            super(props);
        }

        /** {@inheritDoc} */
        public void generateBuild(final File arg0) throws CodeGenMakeException {
        }

    }

    /**
     * @return the set of properties that need to be persisted
     */
    public Properties getPersistProperties() {
        return _model.toProperties();
    }

}
