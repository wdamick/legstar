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
package com.legstar.eclipse.plugin.mulegen.wizards;

import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsGeneratorWizard;
import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardRunnable;
import com.legstar.eclipse.plugin.mulegen.Activator;

/**
 * This wizard role is to create a set of Mule artifacts that allows CICS 
 * clients to access a Mule UMO component.
 */

public class Cixs2MuleGeneratorWizard extends AbstractCixsGeneratorWizard {
	
	/** The main page of controls. */
	private Cixs2MuleGeneratorWizardPage mCixs2MuleGenPage;
	
	/**
	 * Constructor for Cixs2MuleGeneratorWizard.
	 * @param mappingFile an mapping file
	 * @throws CoreException if initialization goes wrong 
	 */
	public Cixs2MuleGeneratorWizard(
	        final IFile mappingFile) throws CoreException {
		super(mappingFile);
	}
	
	/**
	 * Adding the page to the wizard.
	 */
	public final void addPages() {
		mCixs2MuleGenPage = new Cixs2MuleGeneratorWizardPage(
		        getInitialSelection(), getMappingFile());
		addPage(mCixs2MuleGenPage);
	}

    /** {@inheritDoc} */
    public String getPluginId() {
        return Activator.PLUGIN_ID;
    }

    /** {@inheritDoc} */
    protected AbstractCixsGeneratorWizardRunnable getRunnable()
            throws InvocationTargetException {
        return new Cixs2MuleGeneratorWizardRunnable(mCixs2MuleGenPage);
    }

}
