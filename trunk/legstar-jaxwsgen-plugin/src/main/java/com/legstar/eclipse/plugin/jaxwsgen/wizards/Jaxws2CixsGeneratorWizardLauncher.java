package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizard;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.legstar.eclipse.plugin.cixscom.wizards
								.AbstractCixsGeneratorWizardLauncher;
import com.legstar.eclipse.plugin.cixscom.wizards.ICixsGeneratorWizardLauncher;

/**
 * Implementation of a component generator which basically hands over
 * control to the Jaxws2Cixs wizard.
  */
public class Jaxws2CixsGeneratorWizardLauncher
		extends AbstractCixsGeneratorWizardLauncher {

    /**
     * Get an instance of the wizard.
     * @param mappingFile the mapping file
     * @return a new instance of the wizard
     * @throws CoreException if wizard cannot be instantiated
     */
    protected IWizard getWizard(
    		final IFile mappingFile) throws CoreException {
    	return new Jaxws2CixsGeneratorWizard(mappingFile);
    	
    }

	/** {@inheritDoc} */
    public String getName() {
		return "Jaxws to CICS Web Service generator";
	}
    
    /**
     * Register an OSGI component generation service that can be discovered
     * dynamically by other plugins (bundles).
     * @param context the current bundle context
     * @return a registration service
     */
    public static ServiceRegistration register(
            final BundleContext context) {
        return context.registerService(
                ICixsGeneratorWizardLauncher.class.getName(),
                new Jaxws2CixsGeneratorWizardLauncher(),
                new Properties());
        
    }
}
