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
package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import java.io.File;

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

import com.legstar.cixs.gen.model.options.ProxyTargetType;
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
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
        extends AbstractCixsJaxwsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Cixs2JaxwsGeneratorWizardPage";

    /** This generator model. */
    private AntBuildCixs2JaxwsModel _genModel;

    /** Settings for target selection group. */
    private Group _targetGroup = null;

    /** Settings for deployment parameters group. */
    private Composite _deploymentGroup = null;

    /** The POJO target controls group. */
    private CixsProxyPojoTargetGroup _pojoTargetGroup;

    /** The Web Service target controls group. */
    private CixsProxyWebServiceTargetGroup _webServiceTargetGroup;

    /** Where generated COBOL source reside. */
    private Text _targetCobolDirText = null;

    /** HTTP Proxy client parameters. */
    private Cixs2JaxwsProxyDeployHttpGroup _cixsProxyDeployHttpGroup;

    /**
     * Construct the page.
     * 
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     * @param genModel the generation model
     */
    protected Cixs2JaxwsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile,
            final AntBuildCixs2JaxwsModel genModel) {
        super(selection, PAGE_NAME,
                Messages.cixs_to_jaxws_wizard_page_title,
                Messages.cixs_to_jaxws_wizard_page_description,
                mappingFile,
                genModel);
        _genModel = genModel;
    }

    /** {@inheritDoc} */
    protected void addCixsGroup(final Composite container) {

        _targetGroup = createGroup(container,
                Messages.target_selection_group_label, 3);
        createLabel(_targetGroup, Messages.target_selection_label + ':');
        Composite composite = new Composite(_targetGroup, SWT.NULL);
        composite.setLayout(new RowLayout());

        _webServiceTargetGroup = new CixsProxyWebServiceTargetGroup(
                this,
                getGenModel().getWebServiceTargetParameters(),
                getGenModel().getProxyTargetType() == ProxyTargetType.WEBSERVICE);
        _pojoTargetGroup = new CixsProxyPojoTargetGroup(
                this,
                getGenModel().getPojoTargetParameters(),
                getGenModel().getProxyTargetType() == ProxyTargetType.POJO);

        _webServiceTargetGroup.createButton(composite);
        _pojoTargetGroup.createButton(composite);

        _webServiceTargetGroup.createControls(_targetGroup);
        _pojoTargetGroup.createControls(_targetGroup);

        super.addCixsGroup(container);
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {

        super.addWidgetsToTargetGroup(container);

        _targetCobolDirText = createDirectoryFieldEditor(container,
                "targetCobolDir", Messages.cobol_target_location_label + ':');

    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {

        super.addWidgetsToDeploymentGroup(container);

        _deploymentGroup = container;

        createLabel(container, Messages.sample_configuration_transport_label
                + ":");
        Composite composite = new Composite(container, SWT.NULL);
        composite.setLayout(new RowLayout());

        /* HTTP transport deployment is the only available one */
        _cixsProxyDeployHttpGroup = new Cixs2JaxwsProxyDeployHttpGroup(
                this,
                getGenModel().getHttpTransportParameters(),
                getGenModel().getSampleCobolHttpClientType(),
                true);

        /* Buttons go to the newly created rowdata composite */
        _cixsProxyDeployHttpGroup.createButton(composite);

        /* The group goes to the deployment container */
        _cixsProxyDeployHttpGroup.createControls(container);
    }

    /** {@inheritDoc} */
    public void createExtendedListeners() {

        super.createExtendedListeners();

        _targetCobolDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });

        getWebServiceTargetGroup().createListeners();
        getPojoTargetGroup().createListeners();
        getCixsProxyDeployHttpGroup().createListeners();

    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {

        super.initExtendedWidgets(project);

        setTargetCobolDir(getInitTargetDir(getGenModel().getTargetCobolDir(),
                PreferenceConstants.DEFAULT_COBOL_SAMPLE_FOLDER,
                true));

        getWebServiceTargetGroup().initControls();
        getPojoTargetGroup().initControls();
        getCixsProxyDeployHttpGroup().initControls();

    }

    /** {@inheritDoc} */
    public boolean validateExtendedWidgets() {

        if (!super.validateExtendedWidgets()) {
            return false;
        }

        getWebServiceTargetGroup().setVisibility();
        getPojoTargetGroup().setVisibility();
        getCixsProxyDeployHttpGroup().setVisibility();
        getShell().layout(new Control[] { _targetGroup, _deploymentGroup });

        if (getPojoTargetGroup().isSelected()
                && !getPojoTargetGroup().validateControls()) {
            return false;
        }

        if (getWebServiceTargetGroup().isSelected()
                && !getWebServiceTargetGroup().validateControls()) {
            return false;
        }

        if (getCixsProxyDeployHttpGroup().isSelected()
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
    public void updateGenModelExtended() {

        super.updateGenModelExtended();

        if (getWebServiceTargetGroup().isSelected()) {
            getWebServiceTargetGroup().updateGenModel();
            getGenModel().setProxyTargetType(ProxyTargetType.WEBSERVICE);
        }
        if (getPojoTargetGroup().isSelected()) {
            getPojoTargetGroup().updateGenModel();
            getGenModel().setProxyTargetType(ProxyTargetType.POJO);
        }

        if (getCixsProxyDeployHttpGroup().isSelected()) {
            getCixsProxyDeployHttpGroup().updateGenModel();
            getGenModel().setSampleCobolHttpClientType(
                    getCixsProxyDeployHttpGroup()
                            .getSampleCobolHttpClientType());
        }

        getGenModel().setTargetCobolDir(new File(getTargetCobolDir()));
    }

    /**
     * @return the selected target type
     */
    public ProxyTargetType getProxyTargetType() {
        if (getPojoTargetGroup().getSelection()) {
            return ProxyTargetType.POJO;
        }
        if (getWebServiceTargetGroup().getSelection()) {
            return ProxyTargetType.WEBSERVICE;
        }
        return ProxyTargetType.POJO;
    }

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

    /**
     * @return where generated COBOL source reside
     */
    public String getTargetCobolDir() {
        return _targetCobolDirText.getText();
    }

    /**
     * @param targetCobolDir where generated COBOL source reside to set
     */
    public void setTargetCobolDir(final String targetCobolDir) {
        _targetCobolDirText.setText(targetCobolDir);
    }

    /**
     * @return the POJO target controls group
     */
    public CixsProxyPojoTargetGroup getPojoTargetGroup() {
        return _pojoTargetGroup;
    }

    /**
     * @param pojoTargetGroup the POJO target controls group to set
     */
    public void setPojoTargetGroup(
            final CixsProxyPojoTargetGroup pojoTargetGroup) {
        _pojoTargetGroup = pojoTargetGroup;
    }

    /**
     * @return the Web Service target controls group
     */
    public CixsProxyWebServiceTargetGroup getWebServiceTargetGroup() {
        return _webServiceTargetGroup;
    }

    /**
     * @param webServiceTargetGroup the Web Service target controls group to set
     */
    public void setWebServiceTargetGroup(
            final CixsProxyWebServiceTargetGroup webServiceTargetGroup) {
        _webServiceTargetGroup = webServiceTargetGroup;
    }

    /**
     * @return the HTTP Proxy client parameters
     */
    public Cixs2JaxwsProxyDeployHttpGroup getCixsProxyDeployHttpGroup() {
        return _cixsProxyDeployHttpGroup;
    }

    /**
     * @param cixsProxyDeployHttpGroup the HTTP Proxy client parameters to set
     */
    public void setCixsProxyDeployHttpGroup(
            final Cixs2JaxwsProxyDeployHttpGroup cixsProxyDeployHttpGroup) {
        _cixsProxyDeployHttpGroup = cixsProxyDeployHttpGroup;
    }

    /**
     * @return the data model
     */
    public AntBuildCixs2JaxwsModel getGenModel() {
        return _genModel;
    }
}
