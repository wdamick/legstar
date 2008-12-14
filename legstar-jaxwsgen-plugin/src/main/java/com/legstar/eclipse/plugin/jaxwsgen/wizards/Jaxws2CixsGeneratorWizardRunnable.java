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
package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.eclipse.plugin.cixscom.wizards
.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.cixscom.wizards
.AbstractCixsGeneratorWizardRunnable;

/**
 * Background task that performs the actual artifacts generation. The process
 * involves 2 steps:
 * <ul>
 *  <li>Build an ant script file using a velocity template</li>
 *  <li>Launch the ant script as a background process</li>
 * </ul>
 */
public class Jaxws2CixsGeneratorWizardRunnable
extends AbstractCixsGeneratorWizardRunnable {

    /** Part of the ant script file name generated. Allows segregating
     * this ant file from the ones produced by other LegStar wizards. */
    private static final String ANT_FILE_NAME_ID = "jaxws-j2c-";

    /**
     * Constructs the backend generation task. 
     * The ant script will be generated under the folder from
     * preferences.
     * TODO allow user to select the ant script location
     * @param jaxws2CixsGenWizardPage the main wizard page
     * @throws InvocationTargetException if construction fails
     */
    public Jaxws2CixsGeneratorWizardRunnable(
            final Jaxws2CixsGeneratorWizardPage jaxws2CixsGenWizardPage)
    throws InvocationTargetException {
        super(jaxws2CixsGenWizardPage, ANT_FILE_NAME_ID);
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * @param cixsGenWizardPage the wizard page holding input parameters
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected AbstractAntBuildCixsModel getGenerationModel(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage)
    throws InvocationTargetException {
        AntBuildJaxws2CixsModel genModel = new AntBuildJaxws2CixsModel();
        Jaxws2CixsGeneratorWizardPage page =
            (Jaxws2CixsGeneratorWizardPage) cixsGenWizardPage;
        setModel(page, genModel);
        genModel.getCixsJaxwsService().setTargetNamespace(
                page.getTargetNamespace());
        genModel.setTargetWarDir(new File(page.getTargetWarDir()));
        genModel.setTargetWDDDir(new File(page.getTargetWDDDir()));
        return genModel;
    }

    /** {@inheritDoc} */
    public AbstractCixsService createCixsService() {
        return new CixsJaxwsService();
    }

}
