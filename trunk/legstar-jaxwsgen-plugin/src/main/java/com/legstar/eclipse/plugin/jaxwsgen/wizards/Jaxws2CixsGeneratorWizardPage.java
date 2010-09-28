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

import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * A wizard page displaying widgets that are specific to the Jaxws to Cixs
 * generation.
 */
public class Jaxws2CixsGeneratorWizardPage
        extends AbstractCixsJaxwsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Jaxws2CixsGeneratorWizardPage";

    /** This generator model. */
    private AntBuildJaxws2CixsModel _genModel;

    /** Generated Web service target namespace. */
    private Text _wsdlTargetNamespaceText = null;

    /** Generated Web service name. */
    private Text _wsdlServiceNameText = null;

    /** Generated Web service port name. */
    private Text _wsdlPortNameText = null;

    /**
     * Construct the page.
     * 
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     * @param genModel the generation model
     */
    protected Jaxws2CixsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile,
            final AntBuildJaxws2CixsModel genModel) {
        super(selection, PAGE_NAME,
                Messages.jaxws_to_cixs_wizard_page_title,
                Messages.jaxws_to_cixs_wizard_page_description,
                mappingFile,
                genModel);
        _genModel = genModel;
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {

        super.addWidgetsToDeploymentGroup(container);

        _wsdlTargetNamespaceText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_target_namespace_label + ':');

        _wsdlServiceNameText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_service_name_label + ':');

        _wsdlPortNameText = createTextField(container, getStore(),
                null,
                Messages.adapter_wsdl_port_name_label + ':');

    }

    /** {@inheritDoc} */
    public void createExtendedListeners() {

        super.createExtendedListeners();

        _wsdlTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _wsdlServiceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _wsdlPortNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {

        super.initExtendedWidgets(project);

        setWsdlTargetNamespace(getInitWsdlTargetNamespace(getServiceName()));
        setWsdlServiceName(getInitWsdlServiceName(getServiceName()));
        setWsdlPortName(getInitWsdlPortName(getServiceName()));
    }

    /**
     * Attempt to retrieve last target namespace from the project preferences.
     * If not found, target namespace is built from a prefix stored in
     * preferences and the service name.
     * 
     * @param serviceName the service name
     * @return a valid initial value
     */
    protected String getInitWsdlTargetNamespace(final String serviceName) {
        String initValue = getGenModel().getWebServiceParameters()
                .getWsdlTargetNamespace();
        if (initValue == null) {
            String prefix = getStore().getString(
                    PreferenceConstants.ADAPTER_WSDL_TARGET_NAMESPACE_PREFIX);
            if (prefix == null || prefix.length() == 0) {
                initValue = serviceName;
            } else {
                initValue = prefix + '/' + serviceName;
            }
        }
        return initValue;
    }

    /**
     * Attempt to retrieve last wsdl service name from the project preferences.
     * If not found, wsdl service name is built from the service name
     * potentially followed by a preferred suffix.
     * 
     * @param serviceName the service name
     * @return a valid initial value
     */
    protected String getInitWsdlServiceName(final String serviceName) {
        String initValue = getGenModel().getWebServiceParameters()
                .getWsdlServiceName();
        if (initValue == null) {
            String suffix = getStore().getString(
                    PreferenceConstants.ADAPTER_WSDL_SERVICE_NAME_SUFFIX);
            if (suffix == null || suffix.length() == 0) {
                initValue = serviceName;
            } else {
                initValue = serviceName + suffix;
            }
        }
        return initValue;
    }

    /**
     * Attempt to retrieve last wsdl port name from the project preferences.
     * If not found, wsdl port name is built from the service name
     * potentially followed by a preferred suffix.
     * 
     * @param serviceName the service name
     * @return a valid initial value
     */
    protected String getInitWsdlPortName(final String serviceName) {
        String initValue = getGenModel().getWebServiceParameters()
                .getWsdlPortName();
        if (initValue == null) {
            String suffix = getStore().getString(
                    PreferenceConstants.ADAPTER_WSDL_PORT_NAME_SUFFIX);
            if (suffix == null || suffix.length() == 0) {
                initValue = serviceName;
            } else {
                initValue = serviceName + suffix;
            }
        }
        return initValue;
    }

    /** {@inheritDoc} */
    public boolean validateExtendedWidgets() {

        if (!super.validateExtendedWidgets()) {
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

    /** {@inheritDoc} */
    public void updateGenModelExtended() {

        super.updateGenModelExtended();

        getGenModel().getWebServiceParameters().setWsdlTargetNamespace(
                getWsdlTargetNamespace());
        getGenModel().getWebServiceParameters().setWsdlServiceName(
                getWsdlServiceName());
        getGenModel().getWebServiceParameters().setWsdlPortName(
                getWsdlPortName());
    }

    /**
     * @param wsdlTargetNamespace Generated Web services target namespace
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        _wsdlTargetNamespaceText.setText(wsdlTargetNamespace);
    }

    /**
     * @return Generated Web services target namespace
     */
    public String getWsdlTargetNamespace() {
        return _wsdlTargetNamespaceText.getText();
    }

    /**
     * @param wsdlServiceName Generated Web service name
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        _wsdlServiceNameText.setText(wsdlServiceName);
    }

    /**
     * @return Generated Web service name
     */
    public String getWsdlServiceName() {
        return _wsdlServiceNameText.getText();
    }

    /**
     * @param wsdlPortName Generated Web service port name
     */
    public void setWsdlPortName(final String wsdlPortName) {
        _wsdlPortNameText.setText(wsdlPortName);
    }

    /**
     * @return Generated Web service port name
     */
    public String getWsdlPortName() {
        return _wsdlPortNameText.getText();
    }

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

    /**
     * @return the data model
     */
    public AntBuildJaxws2CixsModel getGenModel() {
        return _genModel;
    }
}
