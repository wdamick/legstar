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
package com.legstar.eclipse.plugin.common.wizards;

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
     * TODO move to AbstractLegStarWizard
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
     * TODO move to AbstractLegStarWizard
     * Logs an exception using IStatus.
     * @param innerException the exception to trace
     * @param pluginID plug-in identifier
     */
    public static void logCoreException(
            final Throwable innerException,
            final String pluginID) {
        String message = (innerException.getMessage() == null)
                ? innerException.getClass().toString()
                : innerException.getMessage();
        IStatus status =
            new Status(IStatus.ERROR, pluginID,
                    IStatus.OK, message, innerException);
        Activator.getDefault().getLog().log(status);
        return;
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
