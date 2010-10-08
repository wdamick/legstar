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
package com.legstar.eclipse.plugin.common;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class Activator extends AbstractUIPlugin {

    /** The plug-in ID. */
    public static final String PLUGIN_ID = "com.legstar.eclipse.plugin.common";

    /** Legstar logo shared by all plugins. */
    public static final String LOGO_IMG = Messages.legstar_logo_icon;

    /** The shared instance. */
    private static Activator plugin;

    /**
     * The constructor.
     */
    public Activator() {
        plugin = this;
    }

    /** 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(
     * org.osgi.framework.BundleContext)
     * @param context bundle contect
     * @throws Exception if something goes wrong
     */
    public void start(final BundleContext context) throws Exception {
        super.start(context);

    }

    /**
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(
     * org.osgi.framework.BundleContext)
     * @param context bundle contect
     * @throws Exception if something goes wrong
     */
    public void stop(final BundleContext context) throws Exception {
        plugin = null;
        super.stop(context);
    }

    /**
     * Returns the shared instance.
     *
     * @return the shared instance
     */
    public static Activator getDefault() {
        return plugin;
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
     * Create a formatted core exception.
     * @param e an exception
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
            final Exception e) throws CoreException {
        IStatus status =
            new Status(IStatus.ERROR,
                    PLUGIN_ID,
                    IStatus.OK,
                    e.getMessage(),
                    e);
        getDefault().getLog().log(status);
        throw new CoreException(status);
    }

    /**
     * Create a formatted core exception.
     * @param message the error message
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
            final String message) throws CoreException {
        IStatus status =
            new Status(IStatus.ERROR,
                    PLUGIN_ID,
                    IStatus.OK,
                    message,
                    null);
        getDefault().getLog().log(status);
        throw new CoreException(status);
    }

    /**
     * Determines where the common LegStar plugin is installed on the file
     *  system.
     * @param pluginId the plugin identifier for which the location is sought
     * @return the plugin location
     * @throws InvocationTargetException if location cannot be determined
     */
    public static String getPluginInstallLocation(final String pluginId)
    throws InvocationTargetException {
        Bundle bundle = Platform.getBundle(pluginId);
        Path path = new Path("/");
        URL fileURL = FileLocator.find(bundle, path, null);
        String productLocation = null;
        try {
            productLocation = FileLocator.resolve(fileURL).getPath();
        } catch (IOException e) {
            throw new InvocationTargetException(e);
        }
        return productLocation;
    }

    /**
     * Logs an exception using IStatus.
     * @param innerException the exception to trace
     * @param pluginID plug-in identifier
     */
    public static void logCoreException(
            final Throwable innerException,
            final String pluginID) {
        String message = (innerException.getMessage() == null)
        ? innerException.getClass().toString()
                : innerException.getMessage();
        IStatus status =
            new Status(IStatus.ERROR, pluginID,
                    IStatus.OK, message, innerException);
        Activator.getDefault().getLog().log(status);
        return;
    }

}
