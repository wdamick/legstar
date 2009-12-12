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
package com.legstar.eclipse.plugin.cixscom.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

/**
 * All generators wizards which implement this interface will be dynamically
 * discovered using the OSGI registration service.
 * This allows other wizards to discover which generators are available.
 * Implementing classes must also implement a static registration method.
 */
public interface ICixsGeneratorWizardLauncher {
    
    /**
     * @return name by which this generator is identified. Must be unique
     * among all generators.
     */
    String getName();
    
    /**
     * Start a new instance of a generation wizard.
     * @param mappingFile the mapping file
     * @throws CoreException if wizard cannot be instantiated
     */
    void startGenerationWizard(IFile mappingFile) throws CoreException;

}
