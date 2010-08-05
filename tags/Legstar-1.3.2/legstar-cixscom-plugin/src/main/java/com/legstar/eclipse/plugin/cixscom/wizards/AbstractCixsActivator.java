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

import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * An abstract Activator that implements the Startup interface.
 * Subclasses of this plugin is declared for early startup because they register
 * themselves at startup time. Other plugins can discover such plugins
 * via the OSGI registration mechanism. Without early startup, this plugin
 * will not be available for discovery until it has been used once.
 */
public abstract class AbstractCixsActivator extends AbstractUIPlugin
implements IStartup  {

    /** This plugin unique ID. */
    private String mPluginId;

    /**
     * The constructor.
     * @param pluginId the plugin unique ID.
     */
    public AbstractCixsActivator(final String pluginId) {
        mPluginId = pluginId;
    }

    /**
     * Subclasses are expected to override this method in order to
     * register their services.
     * {@inheritDoc}
     */
    public void start(final BundleContext context) throws Exception {
        super.start(context);
    }

    /**
     * Subclasses are expected to override this method in order to
     * unregister their services.
     * {@inheritDoc}
     */
    public void stop(final BundleContext context) throws Exception {
        super.stop(context);
    }

    /**
     * {@inheritDoc}
     */
    public void earlyStartup() {
    }

    /**
     * @return the plugin unique ID
     */
    public String getPluginId() {
        return mPluginId;
    }
}
