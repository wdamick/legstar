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
package com.legstar.eclipse.plugin.cixsmap.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import com.legstar.eclipse.plugin.cixsmap.wizards.NewMappingFileWizard;

/**
 * Allows menus and toolbar buttons to open the mapping editor.
 *
 */
public class OpenMappingEditorActionDelegate
implements IWorkbenchWindowActionDelegate {

    /** Top level window in the workbench. */
    private IWorkbenchWindow mWindow;

    /** The current selection. */
    private IStructuredSelection mSelection;

    /** {@inheritDoc} */
    public void init(final IWorkbenchWindow window) {
        mWindow = window;
    }

    /** {@inheritDoc} */
    public void run(final IAction action) {
        if (mWindow == null) {
            return;
        }
        if (mSelection == null) {
            mSelection = new StructuredSelection();
        }
        NewMappingFileWizard wizard = new NewMappingFileWizard();
        wizard.init(mWindow.getWorkbench(), mSelection);
        WizardDialog dialog = new WizardDialog(mWindow.getShell(), wizard);
        dialog.open();

    }

    /** {@inheritDoc} */
    public void selectionChanged(
            final IAction action, final ISelection selection) {
        if (selection instanceof IStructuredSelection) {
            mSelection = (IStructuredSelection) selection;
        } else {
            mSelection = null;
        }

    }

    /** {@inheritDoc} */
    public void dispose() {
    }

}
