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
package com.legstar.eclipse.plugin.cixscom.wizards;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.osgi.util.NLS;

import com.legstar.codegen.models.AbstractAntBuildModel;

import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardRunnable;

/**
 * Background task that performs the actual artifacts generation. The default
 * process involves 2 steps:
 * <ul>
 * <li>Build an ant script file using a velocity template</li>
 * <li>Launch the ant script as a background process</li>
 * </ul>
 */
public abstract class AbstractCixsGeneratorWizardRunnable
        extends AbstractWizardRunnable {

    /**
     * Constructs the asynchronous generation task.
     * 
     * @param cixsGenWizardPage the main wizard page
     * @param antFileNameId a prefix to append before ant script file name.
     * @throws InvocationTargetException if construction fails
     */
    public AbstractCixsGeneratorWizardRunnable(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage,
            final String antFileNameId)
            throws InvocationTargetException {
        super(null,
                cixsGenWizardPage.getMappingFile().getProject(),
                getAntScriptFileName(antFileNameId,
                        cixsGenWizardPage.getGenModel().getCixsService()
                                .getName()));
        setAntBuildModel(cixsGenWizardPage.getGenModel());
    }

    /** {@inheritDoc} */
    public void run(
            final IProgressMonitor monitor) throws InvocationTargetException,
            InterruptedException {
        int scale = 1;
        /* 3 tasks because launcher counts as a hidden one */
        monitor.beginTask(
                NLS.bind(Messages.ant_generation_task_label,
                        getTargetAntFileName()),
                        3 * scale);
        try {
            /* 1. Create the ant build */
            createBuild(monitor, scale);

            /* 2 & 3. Execute the generated build. */
            runBuild(monitor, scale);

        } finally {
            monitor.done();
        }
    }

    /**
     * Generate a valid ant script file name.
     * 
     * @param antFileNameId a prefix to append before file name.
     * @param serviceName the service name.
     * @return the ant script file name
     */
    protected static String getAntScriptFileName(
            final String antFileNameId,
            final String serviceName) {
        return AbstractAntBuildModel.ANT_FILE_PREFIX
                + antFileNameId
                + serviceName + '.'
                + AbstractAntBuildModel.ANT_FILE_SUFFIX;
    }

}
