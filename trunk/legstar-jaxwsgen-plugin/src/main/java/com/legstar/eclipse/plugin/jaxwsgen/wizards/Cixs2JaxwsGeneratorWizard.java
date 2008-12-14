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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizard;
import com.legstar.eclipse.plugin.cixscom.wizards.
AbstractCixsGeneratorWizardRunnable;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;

/**
 * This wizard role is to create a set of Jaxws artifacts that allows CICS
 * client programs to access a target Web Service.
 */
public class Cixs2JaxwsGeneratorWizard extends AbstractCixsGeneratorWizard {

    /** The main page of controls. */
    private Cixs2JaxwsGeneratorWizardPage mCixs2JaxwsGenPage;

    /**
     * Constructor for Cixs2JaxwsGeneratorWizard.
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
    public final void addPages() {
        mCixs2JaxwsGenPage = new Cixs2JaxwsGeneratorWizardPage(
                getInitialSelection(), getMappingFile());
        addPage(mCixs2JaxwsGenPage);
    }

    /** {@inheritDoc} */
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /** {@inheritDoc} */
    protected AbstractCixsGeneratorWizardRunnable getRunnable()
    throws InvocationTargetException {
        return new Cixs2JaxwsGeneratorWizardRunnable(mCixs2JaxwsGenPage);
    }

}
