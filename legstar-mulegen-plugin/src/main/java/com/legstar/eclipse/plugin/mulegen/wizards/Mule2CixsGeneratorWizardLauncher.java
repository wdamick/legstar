package com.legstar.eclipse.plugin.mulegen.wizards;

import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizard;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardLauncher;
import com.legstar.eclipse.plugin.cixscom.wizards.ICixsGeneratorWizardLauncher;
import com.legstar.eclipse.plugin.mulegen.Messages;

/**
 * Implementation of a component generator which basically hands over
 * control to the Mule wizard.
  */
public class Mule2CixsGeneratorWizardLauncher
        extends AbstractCixsGeneratorWizardLauncher {

    /** {@inheritDoc} */
    public String getName() {
        return Messages.mule_to_cixs_wizard_page_title;
    }

    /** {@inheritDoc} */
    protected IWizard getWizard(final IFile mappingFile) throws CoreException {
        return new Mule2CixsGeneratorWizard(mappingFile);
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
                new Mule2CixsGeneratorWizardLauncher(),
                new Properties());
        
    }

}
