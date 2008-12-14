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
package com.legstar.eclipse.plugin.schemagen.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;

import com.legstar.eclipse.plugin.schemagen.wizards.MainWizard;

/**
 * Our sample action implements workbench action delegate.
 * The action proxy will be created by the workbench and
 * shown in the UI. When the user tries to use the action,
 * this delegate will be created and execution will be 
 * delegated to it.
 * @see IWorkbenchWindowActionDelegate
 */
public class SchemaGenAction implements IWorkbenchWindowActionDelegate {

    /** Top level window in the workbench. */
    private IWorkbenchWindow mWindow;

    /** Helps track selection changes in the workbench. */
    private IStructuredSelection mSelection;

    /**
     * The constructor.
     */
    public SchemaGenAction() {
    }

    /**
     * The action has been activated. The argument of the
     * method represents the 'real' action sitting
     * in the workbench UI.
     * @see IWorkbenchWindowActionDelegate#run
     * @param action the action proxy that handles the presentation portion of
     *   the action
     */
    public final void run(
            final IAction action) {
        if (mWindow == null) {
            return;
        }
        MainWizard wizard = new MainWizard();
        wizard.init(PlatformUI.getWorkbench(), mSelection);
        WizardDialog dialog = new WizardDialog(mWindow.getShell(), wizard);
        dialog.open();
    }

    /**
     * Selection in the workbench has been changed. We 
     * can change the state of the 'real' action here
     * if we want, but this can only happen after 
     * the delegate has been created.
     * @see IWorkbenchWindowActionDelegate#selectionChanged
     * @param action the action proxy that handles presentation portion of 
     *  the action
     * @param selection the current selection, or <code>null</code> if there
     *  is no selection.
     */
    public void selectionChanged(
            final IAction action, final ISelection selection) {
        mSelection =
            selection instanceof IStructuredSelection
            ? (IStructuredSelection) selection
                    : null;
    }

    /**
     * We can use this method to dispose of any system
     * resources we previously allocated.
     * @see IWorkbenchWindowActionDelegate#dispose
     */
    public void dispose() {
    }

    /**
     * We will cache window object in order to
     * be able to provide parent shell for the message dialog.
     * @see IWorkbenchWindowActionDelegate#init
     * @param window the window that provides the context for this delegate
     */
    public final void init(
            final IWorkbenchWindow window) {
        mWindow = window;
    }
}
