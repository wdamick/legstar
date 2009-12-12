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
package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * A wizard page displaying widgets that are specific to the Jaxws to Cixs
 * generation.
 */
public class Jaxws2CixsGeneratorWizardPage
extends AbstractCixsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Jaxws2CixsGeneratorWizardPage";

    /** J2ee folder where web deployment files should be generated. */
    private Text mTargetWDDDirText = null;

    /** J2ee folder where war files should be deployed. */
    private Text mTargetWarDirText = null;

    /** Generated Web service target namespace. */
    private Text mWsdlTargetNamespaceText = null;

    /** Generated Web service name. */
    private Text mWsdlServiceNameText = null;

    /** Generated Web service port name. */
    private Text mWsdlPortNameText = null;

    /**
     * Construct the page.
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     */
    protected Jaxws2CixsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile) {
        super(selection, PAGE_NAME,
                Messages.jaxws_to_cixs_wizard_page_title,
                Messages.jaxws_to_cixs_wizard_page_description,
                mappingFile);
    }

    /** {@inheritDoc} */
    public void addWidgetsToCixsGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {
        mTargetWDDDirText = createDirectoryFieldEditor(container,
                "targetWDDDir",
                Messages.wdd_target_location_label + ':');
    }

    /** {@inheritDoc} */
    public void addWidgetsToCoxbGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {
        mTargetWarDirText = createTextField(container, getStore(),
                PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER,
                Messages.war_deployment_location_label + ':');

        mWsdlTargetNamespaceText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_target_namespace_label + ':');

        mWsdlServiceNameText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_service_name_label + ':');

        mWsdlPortNameText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_port_name_label + ':');

    }

    /** {@inheritDoc} */
    public void createExtendedListeners() {

        mTargetWDDDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetWarDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mWsdlTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mWsdlServiceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mWsdlPortNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        
    }
    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {
        setTargetWDDDir(getDefaultTargetDir(getStore(),
                PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER));
        setDefaultWsdlTargetNamespace(getServiceName());
        setDefaultWsdlServiceName(getServiceName());
        setDefaultWsdlPortName(getServiceName());
    }

    /**
     * Attempt to retrieve last target namespace from the project preferences.
     * If not found, target namespace is built from a prefix stored in preferences and the
     * name of the target service.
     * @param serviceName the service name
     */
    private void setDefaultWsdlTargetNamespace(final String serviceName) {
        String name;
        String prefix = getStore().getString(
                PreferenceConstants.ADAPTER_WSDL_TARGET_NAMESPACE_PREFIX);
        if (prefix == null || prefix.length() == 0) {
            name = serviceName;
        } else {
            name = prefix + '/' + serviceName;
        }
        setWsdlTargetNamespace(getProjectPreferences().get(
                PreferenceConstants.LAST_ADAPTER_WSDL_TARGET_NAMESPACE, name));
    }

    /**
     * Attempt to retrieve last service namespace from the project preferences.
     * If not found, Wsdl service name is built from the name of the target service and
     * a suffix stored in preferences.
     * @param serviceName the service name
     */
    private void setDefaultWsdlServiceName(final String serviceName) {
        String name;
        String suffix = getStore().getString(
                PreferenceConstants.ADAPTER_WSDL_SERVICE_NAME_SUFFIX);
        if (suffix == null || suffix.length() == 0) {
            name = serviceName;
        } else {
            name = serviceName + suffix;
        }
        setWsdlServiceName(getProjectPreferences().get(
                PreferenceConstants.LAST_ADAPTER_WSDL_SERVICE_NAME, name));
    }

    /**
     * Attempt to retrieve last port name from the project preferences.
     * If not found, Wsdl port name is built from the name of the target service and
     * a suffix stored in preferences.
     * @param serviceName the service name
     */
    private void setDefaultWsdlPortName(final String serviceName) {
        String name;
        String suffix = getStore().getString(
                PreferenceConstants.ADAPTER_WSDL_PORT_NAME_SUFFIX);
        if (suffix == null || suffix.length() == 0) {
            name = serviceName;
        } else {
            name = serviceName + suffix;
        }
        setWsdlPortName(getProjectPreferences().get(
                PreferenceConstants.LAST_ADAPTER_WSDL_PORT_NAME, name));
    }

    /** {@inheritDoc} */
    public boolean validateExtendedWidgets() {
        if (!checkDirectory(getTargetWDDDir(),
                Messages.invalid_wdd_target_location_msg)) {
            return false;
        }
        if (getWsdlTargetNamespace().length() == 0) {
            updateStatus(Messages.invalid_wsdl_target_namespace_msg);
            return false;
        }
        if (getWsdlServiceName().length() == 0) {
            updateStatus(Messages.invalid_wsdl_service_name_msg);
            return false;
        }
        if (getWsdlPortName().length() == 0) {
            updateStatus(Messages.invalid_wsdl_port_name_msg);
            return false;
        }

        return true;
    }

    /**
     * Store the selected values in the project scoped preference store.
     */
    public void storeExtendedProjectPreferences() {
        getProjectPreferences().put(
                PreferenceConstants.LAST_ADAPTER_WSDL_TARGET_NAMESPACE, getWsdlTargetNamespace());
        getProjectPreferences().put(
                PreferenceConstants.LAST_ADAPTER_WSDL_SERVICE_NAME, getWsdlServiceName());
        getProjectPreferences().put(
                PreferenceConstants.LAST_ADAPTER_WSDL_PORT_NAME, getWsdlPortName());
    }

    /**
     * @param targetWarDir J2ee folder where war files should be
     *  deployed
     */
    public void setTargetWarDir(final String targetWarDir) {
        mTargetWarDirText.setText(targetWarDir);
    }

    /**
     * @return J2ee folder where war files should be deployed
     */
    public String getTargetWarDir() {
        return mTargetWarDirText.getText();
    }

    /**
     * @param targetWDDDir J2ee folder where web deployment files should
     *  be generated
     */
    public void setTargetWDDDir(final String targetWDDDir) {
        mTargetWDDDirText.setText(targetWDDDir);
    }

    /**
     * @return J2ee folder where web deployment files should be generated
     */
    public String getTargetWDDDir() {
        return mTargetWDDDirText.getText();
    }

    /**
     * @param wsdlTargetNamespace Generated Web services target namespace
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        mWsdlTargetNamespaceText.setText(wsdlTargetNamespace);
    }

    /**
     * @return Generated Web services target namespace
     */
    public String getWsdlTargetNamespace() {
        return mWsdlTargetNamespaceText.getText();
    }

    /**
     * @param wsdlServiceName Generated Web service name
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        mWsdlServiceNameText.setText(wsdlServiceName);
    }

    /**
     * @return Generated Web service name
     */
    public String getWsdlServiceName() {
        return mWsdlServiceNameText.getText();
    }

    /**
     * @param wsdlPortName Generated Web service port name
     */
    public void setWsdlPortName(final String wsdlPortName) {
        mWsdlPortNameText.setText(wsdlPortName);
    }

    /**
     * @return Generated Web service port name
     */
    public String getWsdlPortName() {
        return mWsdlPortNameText.getText();
    }

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

}
