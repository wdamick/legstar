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
