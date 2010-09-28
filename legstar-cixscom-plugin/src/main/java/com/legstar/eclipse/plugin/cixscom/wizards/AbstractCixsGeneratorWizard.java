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

import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IWorkbench;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * This abstract wizard is shared by all generators based on a Cixs mapping
 * file.
 */

public abstract class AbstractCixsGeneratorWizard extends AbstractWizard {

    /** The current workbench selection. */
    private IStructuredSelection _initialSelection;

    /** The current mapping file. */
    private IFile _mappingFile = null;

    /** This generator model. */
    private AbstractAntBuildCixsModel _genModel;

    /**
     * Constructor for AbstractCixsGeneratorWizard.
     * 
     * @param mappingFile an mapping file
     * @throws CoreException if initialization goes wrong
     */
    public AbstractCixsGeneratorWizard(
            final IFile mappingFile) throws CoreException {
        super();
        setNeedsProgressMonitor(true);
        _mappingFile = mappingFile;
        setProjectPreferences(mappingFile.getProject());
        _genModel = createGenModel();
    }

    /**
     * Get a generation model for the duration of the wizard.
     * 
     * @return a generation model
     */
    protected AbstractAntBuildCixsModel createGenModel() {
        try {
            AbstractAntBuildCixsModel genModel =
                    createGenModel(loadProperties(getProjectPreferences()));
            genModel.setCixsService(createServiceModel());
            genModel.setProductLocation(getPluginInstallLocation(
                    com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));
            return genModel;
        } catch (BackingStoreException e) {
            AbstractWizard
                    .errorDialog(
                            getShell(),
                            Messages.generate_error_dialog_title,
                            getPluginId(),
                            Messages.project_preferences_access_failure_short_msg,
                            NLS
                                    .bind(
                                            Messages.project_preferences_access_failure_long_msg,
                                            getMappingFile().getProject(), e
                                                    .getMessage()));
            logCoreException(e, getPluginId());
        } catch (InvocationTargetException e) {
            AbstractWizard.errorDialog(getShell(),
                    Messages.generate_error_dialog_title,
                    getPluginId(),
                    Messages.plugin_install_locate_failure_short_msg,
                    NLS.bind(Messages.plugin_install_locate_failure_long_msg,
                            e.getCause()));
            logCoreException(e, getPluginId());
        }
        return null;

    }

    /**
     * Try to load the mapping file into a service model.
     * 
     * @return a mapping model that might be empty if load fails
     */
    protected AbstractCixsService createServiceModel() {
        AbstractCixsService serviceModel = createCixsService();
        try {
            serviceModel.load(getMappingFile().getLocation().toFile());
        } catch (CixsModelException e) {
            AbstractWizard.errorDialog(getShell(),
                    Messages.generate_error_dialog_title,
                    getPluginId(),
                    Messages.mapping_file_load_failure_short_msg,
                    NLS.bind(Messages.mapping_file_load_failure_long_msg,
                            getMappingFile().getName(), e.getMessage()));
            logCoreException(e, getPluginId());
        }
        return serviceModel;

    }

    /**
     * @return the service model for this wizard
     */
    public abstract AbstractCixsService createCixsService();

    /**
     * Generation models are built using properties that were previously saved.
     * 
     * @param props a set of saved properties
     * @return the generation model for this wizard
     */
    public abstract AbstractAntBuildCixsModel createGenModel(
            final Properties props);

    /**
     * We will accept the selection in the workbench to see if
     * we can initialize from it.
     * Called by Eclipse to provide the wizard with information about the
     * workbench. {@inheritDoc}
     */
    public void init(
            final IWorkbench workbench,
            final IStructuredSelection selection) {
        _initialSelection = selection;
    }

    /**
     * @return the Initial Selection
     */
    public IStructuredSelection getInitialSelection() {
        return _initialSelection;
    }

    /**
     * @return the Mapping File
     */
    public IFile getMappingFile() {
        return _mappingFile;
    }

    /**
     * @return the generation model
     */
    public AbstractAntBuildCixsModel getGenModel() {
        return _genModel;
    }

    /** {@inheritDoc} */
    @Override
    public Properties getPersistProperties() {
        return _genModel.toProperties();
    }

}
