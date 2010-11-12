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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.eclipse.plugin.schemagen.Activator;

/**
 * The main wizard orchestrates the various wizard pages.
 */
public class MainWizard extends AbstractWizard implements INewWizard {

    /** What we are trying to generate. */
    public static final String GENERATION_SUBJECT = "XML Schema";

    /** The workbench selection upon entry in wizard. */
    private IStructuredSelection _initialSelection;

    /** The first page of the wizard, common to all targets. */
    private MainWizardPage _mainWizardPage;

    /** The generation from a COBOL fragment page. */
    private CobolToXsdWizardPage _cobolToXsdWizardPage;

    /** The generation from a Xsd or Wsdl page. */
    private XsdToXsdWizardPage _xsdToXsdWizardPage;

    /** The generation from a set of Java classes page. */
    private JavaToXsdWizardPage _javaToXsdWizardPage;

    /** Set of preferences stored at the workbench level. */
    private IPreferenceStore _defaultPreferences;

    /** Eclipse Project to which artifacts are attached. */
    private IProject _project = null;

    /**
     * No arg constructor.
     */
    public MainWizard() {
        super();
        setNeedsProgressMonitor(true);
        _defaultPreferences = Activator.getDefault().getPreferenceStore();
    }

    /** {@inheritDoc} */
    @Override
    public boolean performFinish() {
        _mainWizardPage.storeDefaultPreferences();
        return super.performFinish();
    }

    /**
     * We will accept the selection in the workbench to see if
     * we can initialize from it. {@inheritDoc}
     * 
     * @see IWorkbenchWizard#init(IWorkbench, IStructuredSelection)
     */
    public void init(
            final IWorkbench workbench, final IStructuredSelection selection) {
        _initialSelection = selection;
    }

    /**
     * Adding pages to the wizard.
     * 
     * @see org.eclipse.jface.wizard.Wizard#addPages()
     */
    public void addPages() {
        _mainWizardPage = new MainWizardPage(_initialSelection);
        addPage(_mainWizardPage);
        _cobolToXsdWizardPage = new CobolToXsdWizardPage(_initialSelection);
        addPage(_cobolToXsdWizardPage);
        _xsdToXsdWizardPage = new XsdToXsdWizardPage(_initialSelection);
        addPage(_xsdToXsdWizardPage);
        _javaToXsdWizardPage = new JavaToXsdWizardPage(_initialSelection);
        addPage(_javaToXsdWizardPage);
    }

    /**
     * @return the Cobol To Xsd Wizard Page
     */
    public AbstractToXsdWizardPage getCobolToXsdWizardPage() {
        return _cobolToXsdWizardPage;
    }

    /**
     * @return the Xsd To Xsd Wizard Page
     */
    public AbstractToXsdWizardPage getXsdToXsdWizardPage() {
        return _xsdToXsdWizardPage;
    }

    /**
     * @return the Java To Xsd Wizard Page
     */
    public AbstractToXsdWizardPage getJavaToXsdWizardPage() {
        return _javaToXsdWizardPage;
    }

    /** {@inheritDoc} */
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /**
     * @return the default scope preferences
     */
    public IPreferenceStore getDefaultPreferences() {
        return _defaultPreferences;
    }

    /** {@inheritDoc} */
    @Override
    public String getGenerationSubject() {
        return GENERATION_SUBJECT;
    }

    /** {@inheritDoc} */
    @Override
    public Properties getPersistProperties() {
        Properties props = new Properties();
        props.putAll(_mainWizardPage.getPersistProperties());
        props.putAll(_cobolToXsdWizardPage.getPersistProperties());

        // TODO add other pages
        return props;
    }

    /** {@inheritDoc} */
    @Override
    public IRunnableWithProgress getWizardRunnable()
            throws InvocationTargetException {
        IRunnableWithProgress op = null;
        switch (_mainWizardPage.getSelectedSource()) {
        case 0:
            op = new CobolToXsdWizardRunnable(
                    _mainWizardPage, _cobolToXsdWizardPage);
            break;
        case 1:
            op = new XsdToXsdWizardRunnable(
                    _mainWizardPage, _xsdToXsdWizardPage);
            break;
        case 2:
            op = new JavaToXsdWizardRunnable(
                    _mainWizardPage, _javaToXsdWizardPage);
            break;
        default:
        }
        return op;
    }

    /**
     * @return the Eclipse Project to which artifacts are attached
     */
    public IProject getProject() {
        return _project;
    }

    /**
     * @param project the Eclipse Project to which artifacts are attached to set
     */
    public void setProject(final IProject project) {
        _project = project;
        if (project != null) {
            setProjectPreferences(project);
        }
    }

}
