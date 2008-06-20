/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.eclipse.plugin.coxbgen.wizards;
import org.eclipse.jface.operation.IRunnableWithProgress;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.coxbgen.Activator;
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
            errorDialog(getShell(), "Generation error", Activator.PLUGIN_ID,
                    "Binding classes generation failed ",
                    "The generation attempt for " 
                    + mXsdFile.getName()
                    + " has generated a InvocationTargetException "
                    + e.getTargetException());
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
