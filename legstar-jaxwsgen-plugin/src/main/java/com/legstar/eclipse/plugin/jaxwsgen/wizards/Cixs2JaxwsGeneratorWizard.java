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
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizard;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizardRunnable;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;

/**
 * This wizard role is to create a set of Jaxws artifacts that allows CICS
 * client programs to access a target Web Service.
 */
public class Cixs2JaxwsGeneratorWizard extends AbstractCixsGeneratorWizard {

    /** What we are trying to generate. */
    public static final String GENERATION_SUBJECT = "JAX-WS proxy Service";

    /** The main page of controls. */
    private Cixs2JaxwsGeneratorWizardPage _cixs2JaxwsGenPage;

    /**
     * Constructor for Cixs2JaxwsGeneratorWizard.
     * 
     * @param mappingFile an mapping file
     * @throws CoreException if initialization goes wrong
     */
    public Cixs2JaxwsGeneratorWizard(
            final IFile mappingFile) throws CoreException {
        super(mappingFile);
    }

    /**
     * Adding the page to the wizard.
     */
    public void addPages() {
        _cixs2JaxwsGenPage = new Cixs2JaxwsGeneratorWizardPage(
                getInitialSelection(), getMappingFile(), getGenModel());
        addPage(_cixs2JaxwsGenPage);
    }

    /** {@inheritDoc} */
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /** {@inheritDoc} */
    protected AbstractCixsGeneratorWizardRunnable getRunnable()
            throws InvocationTargetException {
        return new Cixs2JaxwsGeneratorWizardRunnable(_cixs2JaxwsGenPage);
    }

    /** {@inheritDoc} */
    @Override
    public AbstractAntBuildCixsModel createGenModel(final Properties props) {
        return new AntBuildCixs2JaxwsModel(props);
    }

    /** {@inheritDoc} */
    @Override
    public AntBuildCixs2JaxwsModel getGenModel() {
        return (AntBuildCixs2JaxwsModel) super.getGenModel();
    }

    /** {@inheritDoc} */
    @Override
    public String getGenerationSubject() {
        return GENERATION_SUBJECT;
    }

    /** {@inheritDoc} */
    @Override
    public IRunnableWithProgress getWizardRunnable()
            throws InvocationTargetException {
        return new Cixs2JaxwsGeneratorWizardRunnable(_cixs2JaxwsGenPage);
    }

}
