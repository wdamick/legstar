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

import org.eclipse.jface.operation.IRunnableWithProgress;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * This wizard role is to create annotated JAXB classes from a COBOL-annotated
 * XML schema and then generate binding classes.
 */

public class CoxbGenWizard extends AbstractWizard {

    /** What we are trying to generate. */
    public static final String GENERATION_SUBJECT = "Binding classes";

    /** The main page of controls. */
    private CoxbGenWizardPage _coxbGenPage;

    /** The current workbench selection. */
    private IStructuredSelection _initialSelection;

    /** The current XML schema file. */
    private IFile _xsdFile = null;

    /** An instance of the generator model. */
    CoxbGenModel _genModel;

    /**
     * Constructor for CoxbGenWizard.
     * 
     * @throws CoreException if initialization goes wrong
     */
    public CoxbGenWizard() throws CoreException {
        this(null);
    }

    /**
     * Constructor for CoxbGenWizard.
     * 
     * @param xsdFile an XML schema file
     * @throws CoreException if initialization goes wrong
     */
    public CoxbGenWizard(final IFile xsdFile) throws CoreException {
        super();
        setNeedsProgressMonitor(true);
        _xsdFile = xsdFile;
        setProjectPreferences(xsdFile.getProject());
        try {
            _genModel = new CoxbGenModel(
                    loadProperties(getProjectPreferences()));
            _genModel.setProductLocation(getPluginInstallLocation(
                    com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));
        } catch (BackingStoreException e) {
            logCoreException(e, getPluginId());
            _genModel = new CoxbGenModel();
        } catch (InvocationTargetException e) {
            logCoreException(e, getPluginId());
            _genModel = new CoxbGenModel();
        }
    }

    /**
     * We will accept the selection in the workbench to see if
     * we can initialize from it.
     * Called by Eclipse to provide the wizard with information about the
     * workbench.
     * Override to keep a handle to {@inheritDoc}
     */
    public void init(
            final IWorkbench workbench,
            final IStructuredSelection selection) {
        _initialSelection = selection;
    }

    /**
     * Adding the page to the wizard.
     * The model is loaded from the preference store if possible.
     */

    public void addPages() {
        _coxbGenPage = new CoxbGenWizardPage(_initialSelection, _xsdFile,
                _genModel);
        addPage(_coxbGenPage);
    }

    /**
     * @return the XML Schema file
     */
    public IFile getXsdFile() {
        return _xsdFile;
    }

    /** {@inheritDoc} */
    @Override
    public String getGenerationSubject() {
        return GENERATION_SUBJECT;
    }

    /** {@inheritDoc} */
    @Override
    public Properties getPersistProperties() {
        return _genModel.toProperties();
    }

    /** {@inheritDoc} */
    @Override
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /** {@inheritDoc} */
    @Override
    public IRunnableWithProgress getWizardRunnable()
            throws InvocationTargetException {
        return new CoxbGenWizardRunnable(_coxbGenPage);
    }

}
