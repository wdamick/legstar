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
package com.legstar.eclipse.plugin.jaxwsgen;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.jaxwsgen.wizards
.Jaxws2CixsGeneratorWizardLauncher;
import com.legstar.eclipse.plugin.jaxwsgen.wizards
.Cixs2JaxwsGeneratorWizardLauncher;

/**
 * This generator plugin register itself for dynamic discovery.
 * @see com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator.
 */
public class Activator extends AbstractCixsActivator  {

    /** The plug-in ID. */
    public static final String PLUGIN_ID =
        "com.legstar.eclipse.plugin.jaxwsgen";

    /** The shared instance. */
    private static Activator mPlugin;

    /** The result of registering a Jaxws2Cixs generator service. */
    private ServiceRegistration mJaxws2CixsGeneratorService;

    /** The result of registering a Cixs2Jaxws generator service. */
    private ServiceRegistration mCixs2JaxwsGeneratorService;

    /**
     * Default constructor.
     */
    public Activator() {
        super(PLUGIN_ID);
    }

    /**
     * {@inheritDoc}
     */
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        mPlugin = this;
        mJaxws2CixsGeneratorService = 
            Jaxws2CixsGeneratorWizardLauncher.register(context);
        mCixs2JaxwsGeneratorService = 
            Cixs2JaxwsGeneratorWizardLauncher.register(context);
    }

    /**
     * {@inheritDoc}
     */
    public void stop(final BundleContext context) throws Exception {
        if (mJaxws2CixsGeneratorService != null) {
            mJaxws2CixsGeneratorService.unregister();
        }
        if (mCixs2JaxwsGeneratorService != null) {
            mCixs2JaxwsGeneratorService.unregister();
        }
        mPlugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance.
     * @return the shared instance
     */
    public static Activator getDefault() {
        return mPlugin;
    }

}
