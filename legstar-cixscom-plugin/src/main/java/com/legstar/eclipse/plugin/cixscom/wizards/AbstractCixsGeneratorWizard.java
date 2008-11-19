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
package com.legstar.eclipse.plugin.cixscom.wizards;
import org.eclipse.jface.operation.IRunnableWithProgress;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IWorkbench;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * This abstract wizard is shared by all generators based on a Cixs mapping 
 * file.
 */

public abstract class AbstractCixsGeneratorWizard extends AbstractWizard {
	
	/** The current workbench selection. */
	private IStructuredSelection mInitialSelection;
	
	/** The current mapping file. */
	private IFile mMappingFile = null;

	/**
	 * Constructor for AbstractCixsGeneratorWizard.
	 * @param mappingFile an mapping file
	 * @throws CoreException if initialization goes wrong 
	 */
	public AbstractCixsGeneratorWizard(
	        final IFile mappingFile) throws CoreException {
		super();
        setNeedsProgressMonitor(true);
        mMappingFile = mappingFile;
	}
	
    /**
     * @return the subclass plugin ID
     */
    public abstract String getPluginId();
    
	/**
	 * This method is called when 'Finish' button is pressed in the wizard.
	 * We will create an operation and run it using wizard as execution context.
	 * @return true if processing went fine
	 */
	public final boolean performFinish() {
		
		try {
			IRunnableWithProgress op = getRunnable();
			getContainer().run(true, true, op);
		} catch (InterruptedException e) {
			return false;
		} catch (InvocationTargetException e) {
            errorDialog(getShell(),
            		Messages.generate_error_dialog_title,
            		getPluginId(),
                    Messages.generation_failure_short_msg,
                    NLS.bind(Messages.generation_failure_long_msg,
                    		mMappingFile.getName(), e.getTargetException()));
            logCoreException(e.getTargetException(), getPluginId());
            return false;
		}
		return true;
	}
	
	/**
	 * @return a background runnable task to generate ant scripts.
	 * @throws InvocationTargetException if runnable cannot be created
	 */
	protected abstract AbstractCixsGeneratorWizardRunnable getRunnable()
	    throws InvocationTargetException;
	
	/**
	 * We will accept the selection in the workbench to see if
	 * we can initialize from it.
	 * Called by Eclipse to provide the wizard with information about the
	 *  workbench.
	 * {@inheritDoc}
	 */
	public final void init(
			final IWorkbench workbench,
			final IStructuredSelection selection) {
		mInitialSelection = selection;
	}

    /**
     * @return the Initial Selection
     */
    public final IStructuredSelection getInitialSelection() {
        return mInitialSelection;
    }

    /**
     * @return the Mapping File
     */
    public final IFile getMappingFile() {
        return mMappingFile;
    }
	
}