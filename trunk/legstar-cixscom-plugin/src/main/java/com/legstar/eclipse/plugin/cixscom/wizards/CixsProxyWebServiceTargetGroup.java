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
    private Combo _wsdlUrlCombo = null;

    /** Target Web service WSDL service name. */
    private Text _wsdlServiceNameText = null;

    /** Target Web service WSDL port name. */
    private Text _wsdlPortNameText = null;

    /** Target Web services target namespace. */
    private Text _wsdlTargetNamespaceText = null;

    /** The data model behind this group. */
    private WebServiceParameters _genModel;

    /**
     * Construct this control holder attaching it to a wizard page.
     * 
     * @param wizardPage the parent wizard page
     * @param genModel the data model
     * @param selected whether this group should initially be selected
     */
    public CixsProxyWebServiceTargetGroup(
            final AbstractCixsGeneratorWizardPage wizardPage,
            final WebServiceParameters genModel,
            final boolean selected) {
        super(wizardPage, selected);
        _genModel = genModel;
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
        super.createControls(composite,
                Messages.target_web_service_group_label, 3);

        _wsdlUrlCombo = getWizardPage().createUrlComboGroup(
                getGroup(), Messages.target_web_service_wsdl_url_label,
                new ModifyListener() {
                    public void modifyText(final ModifyEvent e) {
                    }
                },
                new URLSelectionAdapter());

        _wsdlServiceNameText = AbstractWizardPage.createTextField(getGroup(),
                null, null,
                Messages.target_web_service_wsdl_service_name_label + ':');

        AbstractWizardPage.createLabel(getGroup(), "");

        _wsdlPortNameText = AbstractWizardPage.createTextField(getGroup(),
                null, null,
                Messages.target_web_service_wsdl_port_name_label + ':');

        AbstractWizardPage.createLabel(getGroup(), "");

        _wsdlTargetNamespaceText = AbstractWizardPage.createTextField(
                getGroup(),
                null, null,
                Messages.target_web_service_wsdl_target_namespace_label + ':');
    }

    /**
     * {@inheritDoc}
     */
    public void createExtendedListeners() {

        _wsdlUrlCombo.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _wsdlServiceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _wsdlPortNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                getWizardPage().dialogChanged();
            }
        });
        _wsdlTargetNamespaceText.addModifyListener(new ModifyListener() {
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
        setWsdlUrl(getInitWsdlUrl());
        setWsdlServiceName(getInitWsdlServiceName());
        setWsdlPortName(getInitWsdlPortName());
        setWsdlTargetNamespace(getInitWsdlTargetNamespace());
    }

    /**
     * @return a safe class name initial value
     */
    protected String getInitWsdlUrl() {
        String initValue = _genModel.getWsdlUrl();
        if (initValue == null) {
            initValue = "";
        }
        return initValue;
    }

    /**
     * @return a safe class name initial value
     */
    protected String getInitWsdlServiceName() {
        String initValue = _genModel.getWsdlServiceName();
        if (initValue == null) {
            initValue = "";
        }
        return initValue;
    }

    /**
     * @return a safe class name initial value
     */
    protected String getInitWsdlPortName() {
        String initValue = _genModel.getWsdlPortName();
        if (initValue == null) {
            initValue = "";
        }
        return initValue;
    }

    /**
     * @return a safe class name initial value
     */
    protected String getInitWsdlTargetNamespace() {
        String initValue = _genModel.getWsdlTargetNamespace();
        if (initValue == null) {
            initValue = "";
        }
        return initValue;
    }

    /**
     * {@inheritDoc}
     */
    public void updateGenModelExtended() {
        getGenModel().setWsdlUrl(getWsdlUrl());
        getGenModel().setWsdlServiceName(getWsdlServiceName());
        getGenModel().setWsdlPortName(getWsdlPortName());
        getGenModel().setWsdlTargetNamespace(getWsdlTargetNamespace());
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
            getWizardPage()
                    .updateStatus(Messages.invalid_wsdl_service_name_msg);
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
            _wsdlUrlCombo.add(value);
        }
    }

    /**
     * @return the URL locating target Web service WSDL
     */
    public String getWsdlUrl() {
        return _wsdlUrlCombo.getText();
    }

    /**
     * @param wsdlUrl the URL locating target Web service WSDL to set
     */
    public void setWsdlUrl(final String wsdlUrl) {
        _wsdlUrlCombo.setText(wsdlUrl);
    }

    /**
     * @return the Target Web service WSDL service name
     */
    public String getWsdlServiceName() {
        return _wsdlServiceNameText.getText();
    }

    /**
     * @param wsdlServiceName the Target Web service WSDL service name to
     *            set
     */
    public void setWsdlServiceName(final String wsdlServiceName) {
        _wsdlServiceNameText.setText(wsdlServiceName);
    }

    /**
     * @return the Target Web service WSDL port name
     */
    public String getWsdlPortName() {
        return _wsdlPortNameText.getText();
    }

    /**
     * @param wsdlPortName the Target Web service WSDL port name to set
     */
    public void setWsdlPortName(final String wsdlPortName) {
        _wsdlPortNameText.setText(wsdlPortName);
    }

    /**
     * @param wsdlTargetNamespace the Target Web services target namespace
     */
    public void setWsdlTargetNamespace(final String wsdlTargetNamespace) {
        _wsdlTargetNamespaceText.setText(wsdlTargetNamespace);
    }

    /**
     * @return the Target Web services target namespace
     */
    public String getWsdlTargetNamespace() {
        return _wsdlTargetNamespaceText.getText();
    }

    /**
     * @return the _genModel
     */
    public WebServiceParameters getGenModel() {
        return _genModel;
    }

}
