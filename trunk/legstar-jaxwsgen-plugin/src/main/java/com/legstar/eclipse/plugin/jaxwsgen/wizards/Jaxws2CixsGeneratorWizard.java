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

import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.operation.IRunnableWithProgress;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizard;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;

/**
 * This wizard role is to create a set of Jaxws artifacts that allows Web
 * Service clients to access a CICS program as a Web Service.
 */
public class Jaxws2CixsGeneratorWizard extends AbstractCixsGeneratorWizard {

    /** What we are trying to generate. */
    public static final String GENERATION_SUBJECT = "JAX-WS Service adapter";

    /** The main page of controls. */
    private Jaxws2CixsGeneratorWizardPage _jaxws2CixsGenPage;

    /**
     * Constructor for Jaxws2CixsGeneratorWizard.
     * 
     * @param mappingFile an mapping file
     * @throws CoreException if initialization goes wrong
     */
    public Jaxws2CixsGeneratorWizard(
            final IFile mappingFile) throws CoreException {
        super(mappingFile);
    }

    /**
     * Adding the page to the wizard.
     */
    public void addPages() {
        _jaxws2CixsGenPage = new Jaxws2CixsGeneratorWizardPage(
                getInitialSelection(), getMappingFile(), getGenModel());
        addPage(_jaxws2CixsGenPage);
    }

    /** {@inheritDoc} */
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /** {@inheritDoc} */
    @Override
    public String getGenerationSubject() {
        return GENERATION_SUBJECT;
    }

    /** {@inheritDoc} */
    @Override
    public AbstractCixsService createCixsService() {
        return new CixsJaxwsService();
    }

    /** {@inheritDoc} */
    @Override
    public AbstractAntBuildCixsModel createGenModel(final Properties props) {
        return new AntBuildJaxws2CixsModel(props);
    }

    /** {@inheritDoc} */
    @Override
    public AntBuildJaxws2CixsModel getGenModel() {
        return (AntBuildJaxws2CixsModel) super.getGenModel();
    }

    /** {@inheritDoc} */
    @Override
    public IRunnableWithProgress getWizardRunnable()
            throws InvocationTargetException {
        return new Jaxws2CixsGeneratorWizardRunnable(_jaxws2CixsGenPage);
    }

}
