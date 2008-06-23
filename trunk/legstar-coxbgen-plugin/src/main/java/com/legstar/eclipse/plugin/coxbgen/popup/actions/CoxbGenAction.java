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
package com.legstar.eclipse.plugin.coxbgen.popup.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.eclipse.plugin.coxbgen.Messages;
import com.legstar.eclipse.plugin.coxbgen.wizards.CoxbGenWizard;

/**
 * This action becomes available when an XML schema is selected.
 */
public class CoxbGenAction implements IObjectActionDelegate {

	/** The current selection in the workspace. */
	private ISelection mSelection;
	
	/**
	 * Constructor for GenAction.
	 */
	public CoxbGenAction() {
		super();
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 * @param action
	 *            the action proxy that handles presentation portion of the
	 *            action; must not be <code>null</code>.
	 * @param targetPart
	 *            the new part target; must not be <code>null</code>.
	 */
	public void setActivePart(
			final IAction action, final IWorkbenchPart targetPart) {
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 * @param action
	 *            the action proxy that handles presentation portion of the
	 *            action; must not be <code>null</code>.
	 */
	public final void run(final IAction action) {
        try {
			/* Get us the selected file */
			IFile file = null;
			if (mSelection != null && !mSelection.isEmpty()
					&& mSelection instanceof IStructuredSelection) {
				IStructuredSelection ssel = (IStructuredSelection) mSelection;
				if (ssel.size() > 1) {
					return;
				}
				Object obj = ssel.getFirstElement();
				if (obj instanceof IResource) {
					if (obj instanceof IFile) {
						file = (IFile) obj;
					} else {
	                	AbstractWizard.throwCoreException(
	                			Messages.no_xsd_file_msg);
					}
				}
			}
			CoxbGenWizard wizard = new CoxbGenWizard(file);
			Shell shell =
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			WizardDialog dialog = new WizardDialog(shell, wizard);
			dialog.create();
			dialog.open();
        } catch (CoreException e) {
        	AbstractWizard.logCoreException(e, Activator.PLUGIN_ID);
        }
	}
	
	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
     * @param action the action proxy that handles presentation portion of 
     * 		the action
     * @param selection the current selection, or <code>null</code> if there
     * 		is no selection.
	 */
	public final void selectionChanged(
			final IAction action, final ISelection selection) {
		mSelection = selection;
	}

}
