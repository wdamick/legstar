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

import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.cixs.gen.model.options.WebServiceParameters;
import com.legstar.eclipse.plugin.cixscom.Activator;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.cixscom.dialogs.WsdlPortSelectionDialog;
import com.legstar.eclipse.plugin.cixscom.preferences.PreferenceConstants;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.common.wizards.IURLSelectionListener;

/**
 * Holds the controls for a Web Service proxy target.
 * <p/>
 * A Web Service is described by the characteristics of a WSDL.
 *
 */
public class CixsProxyWebServiceTargetGroup extends AbstractCixsControlsGroup {

    /** URL locating target Web service WSDL. */
    private Combo mWsdlUrlCombo = null;

    /** Target Web service WSDL service name. */
    private Text mWsdlServiceNameText = null;

    /** Target Web service WSDL port name. */
    private Text mWsdlPortNameText = null;

    /** Target Web services target namespace. */
    private Text mWsdlTargetNamespaceText = null;

    /**
     * Construct this control holder attaching it to a wizard page.
     * @param wizardPage the parent wizard page
     */
    public CixsProxyWebServiceTargetGroup(final AbstractCixsGeneratorWizardPage wizardPage) {
        super(wizardPage);
    }


    /**
     * {@inheritDoc} 
     */
    public void createButton(final Composite composite) {
        super.createButton(composite, "Web Service");
    }

    /**
     * {@inheritDoc} 
     */
    public void createControls(final Composite composite) {
        super.createControls(composite, Messages.target_web_service_group_label, 3);

        mWsdlUrlCombo = getWizardPage().createUrlComboGroup(
                getGroup(), Messages.target_web_service_wsdl_url_label,
                new ModifyListener() {
                    public void modifyText(final ModifyEvent e) {
                    }
                },
                new URLSelectionAdapter());
        

        mWsdlServiceNameText = AbstractWizardPage.createTextField(getGroup(),
                null, null, Messages.target_web_service_wsdl_service_name_label + ':');

        AbstractWizardPage.createLabel(getGroup(), "");

        mWsdlPortNameText = AbstractWizardPage.createTextField(getGroup(),
                null, null, Messages.target_web_service_wsdl_port_name_label + ':');

        AbstractWizardPage.createLabel(getGroup(), "");

        mWsdlTargetNamespaceText = AbstractWizardPage.createTextField(getGroup(),
                null, null, Messages.target_web_service_wsdl_target_namespace_label + ':');
    }

    /**
     * {@inheritDoc} 
     */
    public void createExtendedListeners() {
        
        mWsdlUrlCombo.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mWsdlServiceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mWsdlPortNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        mWsdlTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
    }
    
    /**
     * {@inheritDoc} 
     */
    public void initExtendedControls() {
        initWsdlUrl();
        setWsdlUrl(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_WS_WSDL_URL, ""));
        setWsdlServiceName(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_WS_WSDL_SERVICE_NAME, ""));
        setWsdlPortName(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_WS_WSDL_PORT_NAME, ""));
        setWsdlTargetNamespace(getProjectPreferences().get(
                PreferenceConstants.PROXY_LAST_WS_WSDL_TARGET_NAMESPACE, ""));
    }

    /**
     * {@inheritDoc} 
     */
    public void storeExtendedProjectPreferences() {

        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_WS_WSDL_URL, getWsdlUrl());
        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_WS_WSDL_SERVICE_NAME, getWsdlServiceName());
        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_WS_WSDL_PORT_NAME, getWsdlPortName());
        getProjectPreferences().put(
                PreferenceConstants.PROXY_LAST_WS_WSDL_TARGET_NAMESPACE, getWsdlTargetNamespace());
    }

    /**
     * {@inheritDoc} 
     */
    public boolean validateControls() {
        if (getWsdlUrl().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_wsdl_url_msg);
            return false;
        }
        if (getWsdlServiceName().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_wsdl_service_name_msg);
            return false;
        }
        if (getWsdlPortName().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_wsdl_port_name_msg);
            return false;
        }
        if (getWsdlTargetNamespace().length() == 0) {
            getWizardPage().updateStatus(Messages.invalid_target_namespace_msg);
            return false;
        }
        return true;
    }

    /**
     *Defines what happens when a URL is selected.
     */
    private class URLSelectionAdapter implements IURLSelectionListener {

        /** {@inheritDoc} */
        public void urlSelected(final String urlString) {
            WsdlPortSelectionDialog dlg =
                new WsdlPortSelectionDialog(
                        getWsdlUrl(), getShell(),
                        Activator.PLUGIN_ID);
            if (Window.OK == dlg.open()) {
                setWsdlTargetNamespace(dlg.getTargetNamespace());
                setWsdlServiceName(dlg.getServiceName());
                setWsdlPortName(dlg.getPortName());
            }
        }
    }

    /**
     * Setup the initial history list attached to wsdl URL combo box.
     */
    private void initWsdlUrl() {
        for (String value : getWizardPage().getUrlHistory().get()) {
            mWsdlUrlCombo.add(value);
        }
    }

    /**
     * @return the URL locating target Web service WSDL
     */
    public String getWsdlUrl() {
        return mWsdlUrlCombo.getText();
    }

    /**
     * @param wsdlUrl the URL locating target Web service WSDL to set
     */
    public void setWsdlUrl(final String wsdlUrl) {
        mWsdlUrlCombo.setText(wsdlUrl);
    }

    /**
     * @return the Target Web service WSDL service name
     */
    public String getWsdlServiceName() {
        return mWsdlServiceNameText.getText();
    }

    /**
     * @param wsdlServiceName the Target Web service WSDL service name to
     *  set
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        mWsdlServiceNameText.setText(wsdlServiceName);
    }

    /**
     * @return the Target Web service WSDL port name
     */
    public String getWsdlPortName() {
        return mWsdlPortNameText.getText();
    }

    /**
     * @param wsdlPortName the Target Web service WSDL port name to set
     */
    public void setWsdlPortName(final String wsdlPortName) {
        mWsdlPortNameText.setText(wsdlPortName);
    }

    /**
     * @param wsdlTargetNamespace the Target Web services target namespace
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        mWsdlTargetNamespaceText.setText(wsdlTargetNamespace);
    }

    /**
     * @return the Target Web services target namespace
     */
    public String getWsdlTargetNamespace() {
        return mWsdlTargetNamespaceText.getText();
    }
    
    /**
     * @return the target Web Service parameters as a formatted Web Service parameters object
     */
    public WebServiceParameters getWebServiceTargetParameters() {
        WebServiceParameters webServiceParameters = new WebServiceParameters();
        webServiceParameters.setWsdlUrl(getWsdlUrl());
        webServiceParameters.setWsdlServiceName(getWsdlServiceName());
        webServiceParameters.setWsdlPortName(getWsdlPortName());
        webServiceParameters.setWsdlTargetNamespace(getWsdlTargetNamespace());
        return webServiceParameters;
    }

}
