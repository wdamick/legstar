/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.cixscom.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.osgi.util.NLS;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.gen.model.CixsMappingModel;
import com.legstar.codegen.models.AbstractAntBuildModel;

import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardRunnable;

/**
 * Background task that performs the actual artifacts generation. The default
 * process involves 2 steps:
 * <ul>
 *  <li>Build an ant script file using a velocity template</li>
 *  <li>Launch the ant script as a background process</li>
 * </ul>
 */
public abstract class AbstractCixsGeneratorWizardRunnable
extends AbstractWizardRunnable {

    /** The current mapping model. */
    private CixsMappingModel mMappingModel = null;

    /**
     * Constructs the backend generation task. 
     * The ant script will be generated under the folder from
     * preferences.
     * TODO allow user to select the ant script location
     * @param cixsGenWizardPage the main wizard page
     * @param antFileNameId a prefix to append before file name.
     * @throws InvocationTargetException if construction fails
     */
    public AbstractCixsGeneratorWizardRunnable(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage,
            final String antFileNameId)
    throws InvocationTargetException {
        super(null,
                cixsGenWizardPage.getMappingFile().getFullPath().toOSString(),
                getAntScriptFileName(antFileNameId,
                        cixsGenWizardPage.getMappingModel().getName()));
        mMappingModel = cixsGenWizardPage.getMappingModel();
        setAntBuildModel(getGenerationModel(cixsGenWizardPage));
    }

    /** {@inheritDoc} */
    public void run(
            final IProgressMonitor monitor) throws InvocationTargetException,
            InterruptedException {
        int scale = 1;
        /* 3 tasks because launcher counts as a hidden one*/
        monitor.beginTask(
                NLS.bind(Messages.ant_generation_task_label,
                        mMappingModel.getName()),
                        3 * scale);
        try {
            /* 1. Create the ant build */
            createBuild(monitor, scale);

            /* 2 & 3. Execute the generated build.*/
            runBuild(monitor, scale);

        } finally {
            monitor.done();
        }
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * @param cixsGenWizardPage the wizard page holding input parameters
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected abstract AbstractAntBuildCixsModel getGenerationModel(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage)
    throws InvocationTargetException;

    /**
     * Move data from wizard page to model.
     * @param cixsGenWizardPage the wizard page
     * @param genModel the target model
     * @throws InvocationTargetException if model cannot be set
     */
    protected void setModel(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage,
            final AbstractAntBuildCixsModel genModel)
    throws InvocationTargetException {

        genModel.setProductLocation(getPluginInstallLocation(
                com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));
        AbstractCixsService cixsService = createCixsService();
        cixsService.setCixsOperations(mMappingModel.getCixsOperations());
        cixsService.setName(cixsGenWizardPage.getServiceName());
        cixsService.setPackageName(
                cixsGenWizardPage.getJavaClassesPackageName());
        genModel.setCixsService(cixsService);
        genModel.setJaxbBinDir(
                new File(cixsGenWizardPage.getJaxbBinDir()));
        genModel.setCoxbBinDir(
                new File(cixsGenWizardPage.getCoxbBinDir()));
        genModel.setCustBinDir(
                new File(cixsGenWizardPage.getCustBinDir()));
        genModel.setTargetSrcDir(
                new File(cixsGenWizardPage.getTargetSrcDir()));
        genModel.setTargetBinDir(
                new File(cixsGenWizardPage.getTargetBinDir()));
        genModel.setTargetAntDir(
                new File(cixsGenWizardPage.getTargetAntDir()));
        genModel.setTargetPropDir(
                new File(cixsGenWizardPage.getTargetPropDir()));
        genModel.setHostCharset(cixsGenWizardPage.getHostCharset());

    }

    /**
     * @return a new service instance
     */
    public abstract AbstractCixsService createCixsService();

    /**
     * Generate a valid ant script file name.
     * @param antFileNameId a prefix to append before file name.
     * @param mappingFileName the mapping file name.
     * @return the ant script file name
     */
    private static String getAntScriptFileName(
            final String antFileNameId,
            final String mappingFileName) {
        return AbstractAntBuildModel.ANT_FILE_PREFIX
        + antFileNameId
        + mappingFileName + '.'
        + AbstractAntBuildModel.ANT_FILE_SUFFIX;
    }

}
