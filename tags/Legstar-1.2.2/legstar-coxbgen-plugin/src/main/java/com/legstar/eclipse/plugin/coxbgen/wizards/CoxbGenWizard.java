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
package com.legstar.eclipse.plugin.coxbgen.wizards;
import org.eclipse.jface.operation.IRunnableWithProgress;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.coxbgen.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;


/**
 * This wizard role is to create annotated JAXB classes from a COBOL-annotated
 * XML schema and then generate binding classes.
 */

public class CoxbGenWizard extends AbstractWizard {
	
	/** The main page of controls. */
	private CoxbGenWizardPage mCoxbGenPage;
	
	/** The current workbench selection. */
	private IStructuredSelection mInitialSelection;
	
	/** The current XML schema file. */
	private IFile mXsdFile = null;

	/**
	 * Constructor for CoxbGenWizard.
	 * @throws CoreException if initialization goes wrong 
	 */
	public CoxbGenWizard() throws CoreException {
		this(null);
	}
	
	/**
	 * Constructor for CoxbGenWizard.
	 * @param xsdFile an XML schema file
	 * @throws CoreException if initialization goes wrong 
	 */
	public CoxbGenWizard(final IFile xsdFile) throws CoreException {
		super();
        setNeedsProgressMonitor(true);
		mXsdFile = xsdFile;
	}
	
	/**
	 * This method is called when 'Finish' button is pressed in the wizard.
	 * We will create an operation and run it using wizard as execution context.
	 * @return true if processing went fine
	 */
	public final boolean performFinish() {
		
		try {
			IRunnableWithProgress op = new CoxbGenWizardRunnable(mCoxbGenPage);
			getContainer().run(true, true, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
            errorDialog(getShell(),
            		Messages.generate_error_dialog_title,
            		Activator.PLUGIN_ID,
                    Messages.generation_failure_short_msg,
                    NLS.bind(Messages.generation_failure_long_msg,
                    		mXsdFile.getName(), e.getTargetException()));
            logCoreException(e.getTargetException(), Activator.PLUGIN_ID);
            return false;
		}
		return true;
	}
	
	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 * Called by Eclipse to provide the wizard with information about the
	 *  workbench.
	 * Override to keep a handle to 
	 * {@inheritDoc}
	 */
	public final void init(
			final IWorkbench workbench,
			final IStructuredSelection selection) {
		mInitialSelection = selection;
	}
	
	/**
	 * Adding the page to the wizard.
	 */

	public final void addPages() {
		mCoxbGenPage = new CoxbGenWizardPage(mInitialSelection, mXsdFile);
		addPage(mCoxbGenPage);
	}

}
