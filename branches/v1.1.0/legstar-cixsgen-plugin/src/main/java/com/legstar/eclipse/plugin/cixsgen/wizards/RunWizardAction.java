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
package com.legstar.eclipse.plugin.cixsgen.wizards;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.jface.wizard.WizardDialog;

/**
 * Used to propagate a menu or toolbar action as a new Web Service request.
 *
 */
public class RunWizardAction  extends Action 
	implements IWorkbenchWindowActionDelegate {
	
	/**
	 * Contructor.
	 */
	public RunWizardAction() {
    	super();
    }
	
	/** {@inheritDoc}. */
    public void init(final IWorkbenchWindow window) {
    }

    /** Called when the action is discarded. */
    public void dispose() {
    }

	/** {@inheritDoc}. */
    public final void run(final IAction action) {
        NewWSWizard wizard = new NewWSWizard();
		Shell shell =
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        WizardDialog dialog = new WizardDialog(shell, wizard);
        dialog.create();
        dialog.open();
    }

	/** {@inheritDoc}. */
    public final void selectionChanged(
    		final IAction action,
    		final ISelection selection) {
    }


}
