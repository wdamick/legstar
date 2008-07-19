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
package com.legstar.eclipse.plugin.common.wizards;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;

import com.legstar.eclipse.plugin.common.Activator;

/**
 * A generic wizard gathering shared code used by derived wizards.
 *
 */
public abstract class AbstractWizard extends Wizard {

    /** Unlocks the finish button. */
    private boolean mCanFinish = false;
    
	/**
     * Pops an error message.
     * @param shell parent shell
     * @param dialogTitle the error dialog title
     * @param pluginID the parent plugin ID
     * @param shortMessage text of the message
     * @param reason additional explanations
     */
    public static void errorDialog(
            final Shell shell,
            final String dialogTitle,
            final String pluginID,
            final String shortMessage,
            final String reason) { 
        IStatus status = new Status(
                IStatus.ERROR, pluginID,
                IStatus.ERROR, reason, null);         
        ErrorDialog.openError(shell, dialogTitle, shortMessage, status); 
    } 
 
    /**
     * Logs an exception using IStatus.
     * @param innerException the exception to trace
     * @param pluginID plug-in identifier
     */
    public static void logCoreException(
            final Throwable innerException,
            final String pluginID) {
    	Activator.logCoreException(innerException, pluginID);
    }
    
    /**
     * Create a formatted core exception.
     * @param e an exception
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
    		final Exception e) throws CoreException {
    	Activator.throwCoreException(e);
    }

    /**
     * Create a formatted core exception.
     * @param message the error message
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
    		final String message) throws CoreException {
    	Activator.throwCoreException(message);
    }

    /**
     * {@inheritDoc}
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    public boolean canFinish() {
        return mCanFinish;
    }
    
    /**
     * @param canFinish true if finish button should be available
     */
    public void setCanFinish(final boolean canFinish) {
        mCanFinish = canFinish;
    }


}
