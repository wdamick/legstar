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
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.legstar.cixs.jaxws.model.ProxyTargetType;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.cixscom.wizards.CixsProxyPojoTargetGroup;
import com.legstar.eclipse.plugin.cixscom.wizards.CixsProxyWebServiceTargetGroup;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * A wizard page displaying widgets that are specific to the Cixs to Jaxws
 * generation.
 */
public class Cixs2JaxwsGeneratorWizardPage
extends AbstractCixsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Cixs2JaxwsGeneratorWizardPage";

    /** Settings for target selection group. */
    private Group mTargetGroup = null;

    /** Settings for deployment parameters group. */
    private Composite mDeploymentGroup = null;

    /** The POJO target controls group. */
    private CixsProxyPojoTargetGroup mPojoTargetGroup;
    
    /** The Web Service target controls group. */
    private CixsProxyWebServiceTargetGroup mWebServiceTargetGroup;
    
    /** Where generated COBOL source reside. */
    private Text mTargetCobolDirText = null;

    /** J2ee folder where web deployment files should be generated. */
    private Text mTargetWDDDirText = null;

    /** J2ee folder where war files should be deployed. */
    private Text mTargetWarDirText = null;

    /** HTTP Proxy client parameters. */
    private Cixs2JaxwsProxyDeployHttpGroup mCixsProxyDeployHttpGroup;

    /**
     * Construct the page.
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     */
    protected Cixs2JaxwsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile) {
        super(selection, PAGE_NAME,
                Messages.cixs_to_jaxws_wizard_page_title,
                Messages.cixs_to_jaxws_wizard_page_description,
                mappingFile);
    }

    /** {@inheritDoc} */
    protected void addCixsGroup(final Composite container) {
        
        mTargetGroup = createGroup(container, Messages.target_selection_group_label, 3);
        createLabel(mTargetGroup, Messages.target_selection_label + ':');
        Composite composite = new Composite(mTargetGroup, SWT.NULL);
        composite.setLayout(new RowLayout());
        
        mWebServiceTargetGroup = new CixsProxyWebServiceTargetGroup(this);
        mPojoTargetGroup = new CixsProxyPojoTargetGroup(this);

        mWebServiceTargetGroup.createButton(composite);
        mPojoTargetGroup.createButton(composite);

        mWebServiceTargetGroup.createControls(mTargetGroup);
        mPojoTargetGroup.createControls(mTargetGroup);

        super.addCixsGroup(container);
    }

    /** {@inheritDoc} */
    public void addWidgetsToCixsGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {

        mTargetWDDDirText = createDirectoryFieldEditor(container,
                "targetWDDDir", Messages.wdd_target_location_label + ':');

        mTargetCobolDirText = createDirectoryFieldEditor(container,
                "targetCobolDir", Messages.cobol_target_location_label + ':');

    }

    /** {@inheritDoc} */
    public void addWidgetsToCoxbGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {
        mDeploymentGroup = container;
        
        mTargetWarDirText = createTextField(container, getStore(),
                "targetJarDir", Messages.war_deployment_location_label + ':');
 
        createLabel(container, Messages.sample_configuration_transport_label + ":");
        Composite composite = new Composite(container, SWT.NULL);
        composite.setLayout(new RowLayout());

        mCixsProxyDeployHttpGroup = new Cixs2JaxwsProxyDeployHttpGroup(this);
        /* Buttons go to the newly created rowdata composite */
        mCixsProxyDeployHttpGroup.createButton(composite);
        
        /* The group goes to the deployment container*/
        mCixsProxyDeployHttpGroup.createControls(container);
    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {
        
        setTargetWDDDir(getDefaultTargetDir(getStore(),
                PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER));
        setTargetCobolDir(getDefaultTargetDir(getStore(),
                PreferenceConstants.COBOL_SAMPLE_FOLDER));
        setTargetWarDir(getStore().getDefaultString(
                PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER));
 
        getWebServiceTargetGroup().initControls();
        getPojoTargetGroup().initControls();
        getCixsProxyDeployHttpGroup().initControls();
        
        /* Make sure one of the target group is visible */
        if (!getWebServiceTargetGroup().getButton().getSelection()
            && !getPojoTargetGroup().getButton().getSelection()) {
            getWebServiceTargetGroup().getButton().setSelection(true);
        }
        
        /* HTTP transport deployment is the only available one */
        if (!getCixsProxyDeployHttpGroup().getButton().getSelection()) {
            getCixsProxyDeployHttpGroup().getButton().setSelection(true);
        }
    }
    
    /** {@inheritDoc} */
    public void createExtendedListeners() {

        /* Everything initialized, start listening on modification events */
        mTargetWDDDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetCobolDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

        mTargetWarDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

        getWebServiceTargetGroup().createListeners();
        getPojoTargetGroup().createListeners();
        getCixsProxyDeployHttpGroup().createListeners();
        
    }

    /** {@inheritDoc} */
    public boolean validateExtendedWidgets() {
        
        getWebServiceTargetGroup().setVisibility();
        getPojoTargetGroup().setVisibility();
        getCixsProxyDeployHttpGroup().setVisibility();
        getShell().layout(new Control[] {mTargetGroup, mDeploymentGroup});
        
        if (getProxyTargetType() == ProxyTargetType.POJO
                && !getPojoTargetGroup().validateControls()) {
            return false;
        }

        if (getProxyTargetType() == ProxyTargetType.WEBSERVICE
                && !getWebServiceTargetGroup().validateControls()) {
            return false;
        }
        
        if (getCixsProxyDeployHttpGroup().getSelection()
                && !getCixsProxyDeployHttpGroup().validateControls()) {
            return false;
        }

        if (!checkDirectory(getTargetWDDDir(),
                Messages.invalid_wdd_target_location_msg)) {
            return false;
        }
        if (!checkDirectory(getTargetCobolDir(),
                Messages.invalid_cobol_target_location_msg)) {
            return false;
        }

        return true;
    }

    /**
     * Store the selected values in the project scoped preference store.
     */
    public void storeExtendedProjectPreferences() {
        getPojoTargetGroup().storeProjectPreferences();
        getWebServiceTargetGroup().storeProjectPreferences();
        getCixsProxyDeployHttpGroup().storeProjectPreferences();
    }
    
    /**
     * @return the selected target type
     */
    public final ProxyTargetType getProxyTargetType() {
        if (getPojoTargetGroup().getSelection()) {
            return ProxyTargetType.POJO;
        }
        if (getWebServiceTargetGroup().getSelection()) {
            return ProxyTargetType.WEBSERVICE;
        }
        return ProxyTargetType.POJO;
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

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

    /**
     * @return where generated COBOL source reside
     */
    public final String getTargetCobolDir() {
        return mTargetCobolDirText.getText();
    }

    /**
     * @param targetCobolDir where generated COBOL source reside to set
     */
    public final void setTargetCobolDir(final String targetCobolDir) {
        mTargetCobolDirText.setText(targetCobolDir);
    }

    /**
     * @return the J2ee folder where war files should be deployed
     */
    public final String getTargetWarDir() {
        return mTargetWarDirText.getText();
    }

    /**
     * @param targetWarDir J2ee folder where war files should be deployed
     */
    public final void setTargetWarDir(final String targetWarDir) {
        mTargetWarDirText.setText(targetWarDir);
    }

    /**
     * @return the POJO target controls group
     */
    public CixsProxyPojoTargetGroup getPojoTargetGroup() {
        return mPojoTargetGroup;
    }

    /**
     * @param pojoTargetGroup the POJO target controls group to set
     */
    public void setPojoTargetGroup(final CixsProxyPojoTargetGroup pojoTargetGroup) {
        mPojoTargetGroup = pojoTargetGroup;
    }

    /**
     * @return the Web Service target controls group
     */
    public CixsProxyWebServiceTargetGroup getWebServiceTargetGroup() {
        return mWebServiceTargetGroup;
    }

    /**
     * @param webServiceTargetGroup the Web Service target controls group to set
     */
    public void setWebServiceTargetGroup(
            final CixsProxyWebServiceTargetGroup webServiceTargetGroup) {
        mWebServiceTargetGroup = webServiceTargetGroup;
    }

    /**
     * @return the HTTP Proxy client parameters
     */
    public Cixs2JaxwsProxyDeployHttpGroup getCixsProxyDeployHttpGroup() {
        return mCixsProxyDeployHttpGroup;
    }

    /**
     * @param cixsProxyDeployHttpGroup the HTTP Proxy client parameters to set
     */
    public void setCixsProxyDeployHttpGroup(
            final Cixs2JaxwsProxyDeployHttpGroup cixsProxyDeployHttpGroup) {
        mCixsProxyDeployHttpGroup = cixsProxyDeployHttpGroup;
    }


}
