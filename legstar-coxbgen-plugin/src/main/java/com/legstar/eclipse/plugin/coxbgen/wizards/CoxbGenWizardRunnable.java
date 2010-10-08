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
package com.legstar.eclipse.plugin.coxbgen.wizards;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;

import com.legstar.codegen.models.AbstractAntBuildModel;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardRunnable;
import com.legstar.eclipse.plugin.coxbgen.Messages;

/**
 * Background task that performs the actual binding generation. The process
 * involves 2 steps:
 * <ul>
 * <li>Build an ant script file using a velocity template</li>
 * <li>Launch the ant script as a background process</li>
 * </ul>
 */
public class CoxbGenWizardRunnable extends AbstractWizardRunnable {

    /**
     * Part of the ant script file name generated. Allows segregating
     * this ant file from the ones produced by other LegStar wizards.
     */
    private static final String ANT_FILE_NAME_ID = "coxb-";

    /**
     * Constructs the backend generation task.
     * The ant script will be generated under the folder from
     * preferences.
     * TODO allow user to select the ant script location
     * 
     * @param coxbGenWizardPage the main wizard page
     * @throws InvocationTargetException if construction fails
     */
    public CoxbGenWizardRunnable(
            final CoxbGenWizardPage coxbGenWizardPage)
            throws InvocationTargetException {
        super(null,
                coxbGenWizardPage.getSrcDirRelativePathName(),
                getAntScriptFileName(coxbGenWizardPage.getXsdFile().getName()));
        setAntBuildModel(coxbGenWizardPage.getGenModel());
    }

    /** {@inheritDoc} */
    public void run(
            final IProgressMonitor monitor) throws InvocationTargetException,
            InterruptedException {
        int scale = 1;
        /* 3 tasks because launcher counts as a hidden one */
        monitor.beginTask(Messages.ant_generating_task_label,
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
     * @param xsdFileName the source xsd file name.
     * @return the ant script file name
     */
    private static String getAntScriptFileName(final String xsdFileName) {
        return AbstractAntBuildModel.ANT_FILE_PREFIX
                + ANT_FILE_NAME_ID
                + xsdFileName + '.'
                + AbstractAntBuildModel.ANT_FILE_SUFFIX;
    }

}
