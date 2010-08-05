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
package com.legstar.eclipse.plugin.cixscom.popup.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import com.legstar.eclipse.plugin.cixscom.Activator;
import com.legstar.eclipse.plugin.cixscom.Messages;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;

/**
 * This action becomes available when a LegStar mapping file is selected.
 */
public abstract class AbstractGeneratorAction implements IObjectActionDelegate {

    /** The current selection in the workspace. */
    private ISelection mSelection;

    /**
     * Constructor for GenAction.
     */
    public AbstractGeneratorAction() {
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
    public void run(final IAction action) {
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
                                Messages.no_mapping_file_msg);
                    }
                }
            }
            startWizard(file);
        } catch (CoreException e) {
            AbstractWizard.logCoreException(e, Activator.PLUGIN_ID);
        }
    }

    /**
     * Subclasses implement the wizard start method.
     * @param mappingFile the mapping file
     * @throws CoreException if wizard fails to start
     */
    public abstract void startWizard(
            final IFile mappingFile) throws CoreException;

    /**
     * @see IActionDelegate#selectionChanged(IAction, ISelection)
     * @param action the action proxy that handles presentation portion of 
     *  the action
     * @param selection the current selection, or <code>null</code> if there
     *  is no selection.
     */
    public void selectionChanged(
            final IAction action, final ISelection selection) {
        mSelection = selection;
    }

}
