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
package com.legstar.eclipse.plugin.cixsmap;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * This plugin is mainly an editor for Mainframe program mapping to methods.
 * Mapping data structures is handled by the coxbgen-plugin and is a 
 * pre-requisite before mapping methods can start.
 */
public class Activator extends AbstractUIPlugin {

    /** The plug-in ID. */
    public static final String PLUGIN_ID = "com.legstar.eclipse.plugin.cixsmap";

    /** The shared instance. */
    private static Activator mPlugin;

    /** Context returned by start is needed for registration services. */
    private BundleContext mContext;

    /**
     * The constructor.
     */
    public Activator() {
    }

    /**
     * {@inheritDoc}
     */
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        mContext = context;
        mPlugin = this;
    }

    /**
     * {@inheritDoc}
     */
    public void stop(final BundleContext context) throws Exception {
        mPlugin = null;
        mContext = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance.
     *
     * @return the shared instance
     */
    public static Activator getDefault() {
        return mPlugin;
    }

    /**
     * Returns an image descriptor for the image file at the given
     * plug-in relative path.
     *
     * @param path the path
     * @return the image descriptor
     */
    public static ImageDescriptor getImageDescriptor(final String path) {
        return imageDescriptorFromPlugin(PLUGIN_ID, path);
    }

    /**
     * @return the bundle context
     */
    public final BundleContext getContext() {
        return mContext;
    }

}
