/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

/**
 * Implementation of a generator launcher. It conforms to the 
 * <code>ICixsGeneratorWizardLauncher</code> interface which allows subclasses
 * to be dynamically discovered.
 */
public abstract class AbstractCixsGeneratorWizardLauncher
implements ICixsGeneratorWizardLauncher {

    /**
     * {@inheritDoc}
     */
    public void startGenerationWizard(
            final IFile mappingFile) throws CoreException {
        Shell shell =
            PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        WizardDialog dialog = new WizardDialog(shell, getWizard(mappingFile));
        dialog.create();
        dialog.open();
    }

    /**
     * {@inheritDoc}
     */
    protected abstract IWizard getWizard(
            final IFile mappingFile) throws CoreException;

}
