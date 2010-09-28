package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import com.legstar.cixs.jaxws.model.AbstractAntBuildCixsJaxwsModel;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * Wizard page fields that are common to both adapter and proxy generation.
 * 
 */
public abstract class AbstractCixsJaxwsGeneratorWizardPage extends
        AbstractCixsGeneratorWizardPage {

    /** A model that is common to proxies and adapters for JAXWS. */
    private AbstractAntBuildCixsJaxwsModel _genModel;

    /** J2ee folder where web deployment files should be generated. */
    private Text _targetWDDDirText = null;

    /** J2ee folder where war files should be deployed. */
    private Text _targetWarDirText = null;

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
    public AbstractCixsJaxwsGeneratorWizardPage(
            final IStructuredSelection selection,
            final String pageName,
            final String pageTitle,
            final String pageDesc,
            final IFile mappingFile,
            final AbstractAntBuildCixsJaxwsModel genModel) {
        super(selection, pageName, pageTitle, pageDesc, mappingFile, genModel);
        _genModel = genModel;
    }

    /** {@inheritDoc} */
    public void addWidgetsToCixsGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {
        _targetWDDDirText = createDirectoryFieldEditor(container,
                "targetWDDDir",
                Messages.wdd_target_location_label + ':');
    }

    /** {@inheritDoc} */
    public void addWidgetsToCoxbGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {
        _targetWarDirText = createTextField(container, getStore(),
                null,
                Messages.war_deployment_location_label + ':');

    }

    /** {@inheritDoc} */
    public void createExtendedListeners() {

        _targetWDDDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        _targetWarDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {
        setTargetWDDDir(getInitTargetDir(getGenModel().getTargetWDDDir(),
                PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER));
        setTargetWarDir(getInitTargetDir(getGenModel().getTargetWarDir(),
                PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER));
    }

    /** {@inheritDoc} */
    public boolean validateExtendedWidgets() {
        if (!checkDirectory(getTargetWDDDir(),
                Messages.invalid_wdd_target_location_msg)) {
            return false;
        }

        return true;
    }

    /** {@inheritDoc} */
    public void updateGenModelExtended() {
        getGenModel().setTargetWDDDir(new File(getTargetWDDDir()));
        getGenModel().setTargetWarDir(new File(getTargetWarDir()));
    }

    /**
     * @param targetWarDir J2ee folder where war files should be
     *            deployed
     */
    public void setTargetWarDir(final String targetWarDir) {
        _targetWarDirText.setText(targetWarDir);
    }

    /**
     * @return J2ee folder where war files should be deployed
     */
    public String getTargetWarDir() {
        return _targetWarDirText.getText();
    }

    /**
     * @return J2ee folder where web deployment files should be generated
     */
    public String getTargetWDDDir() {
        return _targetWDDDirText.getText();
    }

    /**
     * @param targetWDDDir J2ee folder where web deployment files should
     *            be generated
     */
    public void setTargetWDDDir(final String targetWDDDir) {
        _targetWDDDirText.setText(targetWDDDir);
    }

    /**
     * @return the data model
     */
    public AbstractAntBuildCixsJaxwsModel getGenModel() {
        return _genModel;
    }
}
