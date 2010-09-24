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
package com.legstar.eclipse.plugin.coxbgen.wizards;

import org.eclipse.jface.operation.IRunnableWithProgress;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.coxbgen.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * This wizard role is to create annotated JAXB classes from a COBOL-annotated
 * XML schema and then generate binding classes.
 */

public class CoxbGenWizard extends AbstractWizard {

    /** The main page of controls. */
    private CoxbGenWizardPage _coxbGenPage;

    /** The current workbench selection. */
    private IStructuredSelection _initialSelection;

    /** The current XML schema file. */
    private IFile _xsdFile = null;

    /** Set of preferences stored at the project level. */
    private IEclipsePreferences _projectPreferences;

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
        IScopeContext context = new ProjectScope(getXsdFile().getProject());
        _projectPreferences = context.getNode(Activator.PLUGIN_ID);
    }

    /**
     * This method is called when 'Finish' button is pressed in the wizard.
     * We will create an operation and run it using wizard as execution context.
     * 
     * @return true if processing went fine
     */
    public boolean performFinish() {

        try {
            storeProjectPreferences();
            IRunnableWithProgress op = new CoxbGenWizardRunnable(_coxbGenPage);
            getContainer().run(true, true, op);
        } catch (InterruptedException e) {
            return false;
        } catch (InvocationTargetException e) {
            errorDialog(getShell(),
                    Messages.generate_error_dialog_title,
                    Activator.PLUGIN_ID,
                    Messages.generation_failure_short_msg,
                    NLS.bind(Messages.generation_failure_long_msg,
                            _xsdFile.getName(), e.getTargetException()));
            logCoreException(e.getTargetException(), Activator.PLUGIN_ID);
            return false;
        }
        return true;
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
     * Saves the user choices reflected by the model in a preference node.
     */
    public void storeProjectPreferences() {
        storeProperties(getProjectPreferences(), _coxbGenPage.getCoxbModel()
                .toProperties());
        try {
            getProjectPreferences().flush();
        } catch (BackingStoreException e) {
            AbstractWizard.logCoreException(e,
                    Activator.PLUGIN_ID);
        }
    }

    /**
     * Adding the page to the wizard.
     * The model is loaded from the preference store if possible.
     */

    public void addPages() {
        CoxbGenModel coxbModel;
        try {
            coxbModel = new CoxbGenModel(
                    loadProperties(getProjectPreferences()));
        } catch (BackingStoreException e) {
            logCoreException(e, Activator.PLUGIN_ID);
            coxbModel = new CoxbGenModel();
        }
        _coxbGenPage = new CoxbGenWizardPage(_initialSelection, _xsdFile,
                coxbModel);
        addPage(_coxbGenPage);
    }

    /**
     * Loads the last user choices.
     * 
     * @param preferences a preference node
     * @return an initialized model
     * @throws BackingStoreException if access to preference store fails
     */
    public CoxbGenModel loadModel(
            final IEclipsePreferences preferences) throws BackingStoreException {
        return new CoxbGenModel(loadProperties(preferences));
    }

    /**
     * @return the XML Schema file
     */
    public IFile getXsdFile() {
        return _xsdFile;
    }

    /**
     * @return the project scope preferences
     */
    public IEclipsePreferences getProjectPreferences() {
        return _projectPreferences;
    }
}
