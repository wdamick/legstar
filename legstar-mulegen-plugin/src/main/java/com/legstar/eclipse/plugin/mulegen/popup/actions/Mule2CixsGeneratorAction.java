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
package com.legstar.eclipse.plugin.mulegen.popup.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.cixscom.popup.actions.AbstractGeneratorAction;
import com.legstar.eclipse.plugin.mulegen.wizards
		.Mule2CixsGeneratorWizardLauncher;

/**
 * This action becomes available when a LegStar mapping file is selected.
 */
public class Mule2CixsGeneratorAction extends AbstractGeneratorAction {

    /** {@inheritDoc} */
    @Override
    public void startWizard(final IFile mappingFile) throws CoreException {
        new Mule2CixsGeneratorWizardLauncher().startGenerationWizard(
                mappingFile);
    }

}
