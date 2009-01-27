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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.util.Locale;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

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
    private Combo mSourceTypeCombo;

    /** Destination container. */
    private Text mTargetContainerText;

    /** Destination file name. */
    private Text mTargetXSDFileNameText;

    /** Whether the XSD file should be overwritten if it already exist. */
    private boolean mOverwriteAllowed = true;

    /** Target XML schema namespace. */
    private Text mTargetNamespaceText;

    /** Target jaxb classes package name. */
    private Text mTargetJaxbPackageNameText;

    /** An optional suffix to append on JAXB classes name generated from Complex
     *  types in the XSD.*/
    private Text mJaxbTypeClassesSuffixText;

    /** The final namespace is built starting with this prefix. */
    private String mXsdNamespacePrefix = "";

    /** The final jaxb package is built starting with this prefix. */
    private String mJaxbPackageNamePrefix = "";

    /** Will be true is user explicitly modified the namespace. This means we
     *  should refrain from trying to generate it automatically. */
    private boolean mXsdNamespaceUserChanged;

    /** Will be true is user explicitly modified the namespace. This means we
     *  should refrain from trying to generate it automatically. */
    private boolean mJaxbPackageNameUserChanged;

    /**
     * Constructs the main wizard page.
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
        mSourceTypeCombo = createComboFromItemsArray(
                groupSource, new String [] {
                        Messages.cobol_source_type_text, 
                        Messages.xsd_source_type_text,
                        Messages.java_source_type_text});

        Group groupTarget = createGroup(container,
                Messages.target_group_label);

        createLabel(groupTarget, Messages.container_label);
        mTargetContainerText = createText(groupTarget);
        initTargetContainer();
        mTargetContainerText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createBrowseForContainerButton(
                groupTarget, Messages.container_selection_label,
                mTargetContainerText);

        createLabel(groupTarget, Messages.xsd_file_name_label);
        mTargetXSDFileNameText = createText(groupTarget);
        String lastTargetXSDFileName =
            getDefaultPreferences().getString("TargetXSDFileName");
        if (lastTargetXSDFileName.length() > 0) {
            mTargetXSDFileNameText.setText(lastTargetXSDFileName);
        } else {
            mTargetXSDFileNameText.setText(".xsd");
        }
        mTargetXSDFileNameText.setFocus();
        mTargetXSDFileNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetXSDFileNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                autoFill();
            }
        });
        createOverwriteAllowedCheckButton(groupTarget,
                Messages.overwrite_button_label);

        createLabel(groupTarget, Messages.namespace_label);
        mTargetNamespaceText = createText(groupTarget, 2);
        mXsdNamespacePrefix = getSetText(mTargetNamespaceText, '/',
                PreferenceConstants.XSD_NAMESPACE_PREFIX);
        mXsdNamespaceUserChanged = false;
        mTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                mXsdNamespaceUserChanged = userChanged(
                        mTargetNamespaceText, '/', false);
            }
        });

        createLabel(groupTarget, Messages.jaxb_package_name_label);
        mTargetJaxbPackageNameText = createText(groupTarget, 2);
        mJaxbPackageNamePrefix = getSetText(mTargetJaxbPackageNameText, '.',
                PreferenceConstants.JAXB_PACKAGE_NAME_PREFIX);
        mJaxbPackageNameUserChanged = false;
        mTargetJaxbPackageNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetJaxbPackageNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                mJaxbPackageNameUserChanged = userChanged(
                        mTargetJaxbPackageNameText, '.', true);
            }
        });
        createLabel(groupTarget, Messages.jaxb_classes_suffix_label);
        mJaxbTypeClassesSuffixText = createText(groupTarget, 2);
        mJaxbTypeClassesSuffixText.setText(getDefaultPreferences().getString(
        "JaxbTypeClassesSuffix"));
        mJaxbTypeClassesSuffixText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /**
     * Set the initial value of the target container field.
     * <p/>
     * If a project is selected in the workspace, then it is considered to
     * be the initial value.
     * <p/>
     * If no project is selected, we attempt to recover the last container used
     * from the default preferences.
     */
    private void initTargetContainer() {
        if (getInitialSelection() != null && !getInitialSelection().isEmpty()) {
            Object obj = getInitialSelection().getFirstElement();
            IResource container = null;
            if (obj instanceof IResource) {
                if (obj instanceof IContainer) {
                    container = ((IContainer) obj);
                } else {
                    container = ((IResource) obj).getParent();
                }
            } else if (obj instanceof IJavaProject) {
                container = ((IJavaProject) obj).getProject();
            } else if (obj instanceof IJavaElement) {
                container = ((IJavaElement) obj).getResource();
            }
            if (container != null) {
                mTargetContainerText.setText(
                        container.getFullPath().toOSString());
                return;
            }
        }
        mTargetContainerText.setText(getDefaultPreferences().getString(
        "TargetContainer"));
    }

    /** {@inheritDoc} */
    public void initContents() {
    }

    /**
     * A convenience method to set a Text control to a value from a 
     * preference store and then return that value.
     * @param text the text widget
     * @param separator the type of separator used to terminate a prefix
     * @param keyInStore the value identifier in the preference store 
     * @return the value used to initialize the text widget
     */
    private String getSetText(
            final Text text, final char separator, final String keyInStore) {
        IPreferenceStore store = getDefaultPreferences();
        String str = store.getString(keyInStore);
        if (str != null && str.length() != 0) {
            str = setSeparatorChar(str, separator);
            text.setText(str + getXsdSimpleFileName());
        }
        return str;
    }

    /**
     * If the input string is empty or null, return an empty string.
     * If the input string already ends with the segments separator char, return
     * the input string unchanged.
     * Otherwise, append a final segments separator character and return the
     * resulting string. 
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
     * namespace and jaxb package name unless the user has taken control of
     * these fields already.
     * Only the last segment of the xsd file name, excluding the extension,
     * contributes to other field values.
     */
    private void autoFill() {
        String str = getXsdSimpleFileName();
        if (!mXsdNamespaceUserChanged) {
            mTargetNamespaceText.setText(mXsdNamespacePrefix + str);
        }
        if (!mJaxbPackageNameUserChanged) {
            mTargetJaxbPackageNameText.setText(mJaxbPackageNamePrefix
                    + lowercaseNormalize(str));
        }
    }

    /**
     * To determine if a control text was modified by the user, we compare the
     * last characters to those of the target xsd file name. If they are still
     * the same, we assume no user modification was done.
     * @param text the text to compare
     * @param c the segments separator character 
     * @param isJavaIdentifierPart tells if the text must be a valid java
     *  identifier 
     * @return true if the content of the text field now differs from autofill
     */
    private boolean userChanged(
            final Text text, final char c, final boolean isJavaIdentifierPart) {
        String str = getXsdSimpleFileName();
        String content = lastSegment(text.getText(), c);
        if (str.length() == 0) {
            return !(content == null || content.length() == 0);
        } else {
            if (content == null || content.length() == 0) {
                return true;
            } else {
                if (isJavaIdentifierPart) {
                    return !(lowercaseNormalize(str).equals(content));
                }
                return !(str.equals(content));
            }
        }
    }

    /**
     * This ensures a string can be used as a valid identifier for a 
     * java package name or an XML namespace.
     * <ul>
     * <li>Lowercases all uppercase characters.</li>
     * <li>Replaces all illegal characters by period.</li>
     * </ul>
     * @param str the string to normalize
     * @return the normalized string.
     */
    private String lowercaseNormalize(final String str) {
        StringBuilder sb = new StringBuilder();
        for (char ch : str.toCharArray()) {
            if (Character.isJavaIdentifierPart(ch)) {
                sb.append(ch);
            } else {
                sb.append('.');
            }
        }
        return sb.toString().toLowerCase(Locale.getDefault());
    }

    /**
     * @return the last segment of the xsd file name without an extension
     */
    private String getXsdSimpleFileName() {
        IPath path = new Path(
                mTargetXSDFileNameText.getText()).removeFileExtension();
        if (path.lastSegment() == null) {
            return "";
        }
        return path.lastSegment();
    }

    /**
     * Given a separator character, returns the segment following the last
     * separator or the string itself if no seperators is found.
     * @param str string to extract segment from
     * @param c the separator character
     * @return the last segment of the input string
     */
    private String lastSegment(final String str, final char c) {
        if (str == null || str.length() == 0) {
            return str;
        }
        int i = str.lastIndexOf(c);
        if (i < 0) {
            return str;
        }
        return str.substring(i + 1);
    }

    /**
     * Adds a check button that reflects its state in an associated boolean
     *  variable.
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

        /* Target XML schema file name must not be empty */
        if (getXsdSimpleFileName().length() == 0) {
            updateStatus(Messages.no_target_xsd_file_name_msg);
            return;
        }

        String fileName = getTargetXSDFileName();
        /* If overwrite is prohibited, then XML schema file must not exist in
         * target container */
        if (!isOverwriteAllowed()) {
            IResource fileResource = root.findMember(
                    new Path(targetContainer).append(fileName));
            if (fileResource != null && fileResource.exists()) {
                updateStatus(Messages.already_exists_target_xsd_file_msg);
                return;
            }
        }

        /* Namespace cannot be empty. With Xsd or Wsdl, the original namespace
         * can be kept, therefore it is the only case where we tolerate an
         * empty namespace here.*/
        String namespace = getTargetNamespace();
        if (namespace.length() == 0 && getSelectedSource() != 1) {
            updateStatus(Messages.no_target_namespace_msg);
            return;
        }

        /* Namespace cannot be empty. We might be able to relax this but this is
         * unsafe right now.*/
        String jaxbPackageName = getTargetJaxbPackageName();
        if (jaxbPackageName.length() == 0) {
            updateStatus(Messages.no_target_jaxb_package_msg);
            return;
        }

        updateStatus(null);
    }

    /**
     * Store the selected values in the default scoped preference store.
     */
    public void storeDefaultPreferences() {
        getDefaultPreferences().setValue(
                "TargetContainer", getTargetContainer());
        getDefaultPreferences().setValue(
                "TargetXSDFileName", getTargetXSDFileName());
        getDefaultPreferences().setValue(
                "JaxbTypeClassesSuffix", getJaxbTypeClassesSuffix());
    }

    /**
     * The next page depends on the source type selected.
     * {@inheritDoc}
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    public IWizardPage getNextPage() {
        switch(getSelectedSource()) {
        case 0:
            return ((MainWizard) getWizard()).getCobolToXsdWizardPage();
        case 1:
            XsdToXsdWizardPage xsdToXsdWizardPage = (XsdToXsdWizardPage)
            ((MainWizard) getWizard()).getXsdToXsdWizardPage();
            xsdToXsdWizardPage.setNewTargetNamespace(
                    getTargetNamespace());
            return xsdToXsdWizardPage;
        case 2:
            return ((MainWizard) getWizard()).getJavaToXsdWizardPage();
        default:
            return null;
        }
    }

    /**
     * Returns the zero-relative index of the item which is currently selected
     * in the receiver's list, or -1 if no item is selected.
     * @return the source type that was selected on the first page.
     */
    public int getSelectedSource() {
        return mSourceTypeCombo.getSelectionIndex();
    }

    /**
     * @return true if the XSD file should be overwritten if it already exist
     */
    public final boolean isOverwriteAllowed() {
        return mOverwriteAllowed;
    }

    /**
     * @param overwriteAllowed true if the XSD file should be overwritten if it
     *  already exist
     */
    public final void setOverwriteAllowed(final boolean overwriteAllowed) {
        mOverwriteAllowed = overwriteAllowed;
    }

    /**
     * @return the destination container text
     */
    public final String getTargetContainer() {
        return mTargetContainerText.getText();
    }

    /**
     * @param destinationContainer the destination container text to set
     */
    public final void setDestinationContainer(
            final String destinationContainer) {
        mTargetContainerText.setText(destinationContainer);
    }

    /**
     * @return the target XML Schema file name
     */
    public final String getTargetXSDFileName() {
        return mTargetXSDFileNameText.getText();
    }

    /**
     * @param targetXSDFileName the target XML Schema file name to set
     */
    public final void setTargetXSDFileName(
            final String targetXSDFileName) {
        mTargetXSDFileNameText.setText(targetXSDFileName);
    }

    /**
     * @return the Target XML schema namespace
     */
    public final String getTargetNamespace() {
        return mTargetNamespaceText.getText();
    }

    /**
     * @param targetNamespace the Target XML schema namespace to set
     */
    public final void setTargetNamespace(final String targetNamespace) {
        mTargetNamespaceText.setText(targetNamespace);
    }

    /**
     * @return the Target jaxb classes package name
     */
    public final String getTargetJaxbPackageName() {
        return mTargetJaxbPackageNameText.getText();
    }

    /**
     * @param targetJaxbPackageName the Target jaxb classes package name to
     * set
     */
    public final void setTargetJaxbPackageName(
            final String targetJaxbPackageName) {
        mTargetJaxbPackageNameText.setText(targetJaxbPackageName);
    }

    /**
     * @return the optional suffix to append on JAXB classes name generated from
     *  Complex types in the XSD
     */
    public final String getJaxbTypeClassesSuffix() {
        return mJaxbTypeClassesSuffixText.getText();
    }

    /**
     * @param jaxbTypeClassesSuffix the optional suffix to append on JAXB
     *  classes name generated from Complex types in the XSD
     */
    public final void setJaxbTypeClassesSuffix(
            final String jaxbTypeClassesSuffix) {
        mJaxbTypeClassesSuffixText.setText(jaxbTypeClassesSuffix);
    }

    /**
     * @return the project scope preferences
     */
    public IPreferenceStore getDefaultPreferences() {
        return ((MainWizard) getWizard()).getDefaultPreferences();
    }

}
