/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.eclipse.plugin.schemagen;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class Activator extends AbstractUIPlugin {

	/** The plug-in ID. */
	public static final String PLUGIN_ID =
		"com.legstar.eclipse.plugin.schemagen";

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
	 * @param context the bundle context for this plug-in
	 * @throws Exception if this plug-in did not start up properly
	 */
	@Override
	public final void start(
			final BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(
	 * org.osgi.framework.BundleContext)
	 * @param context the bundle context for this plug-in
	 * @throws Exception if this plug-in did not start up properly
	 */
	@Override
	public final void stop(final BundleContext context) throws Exception {
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
	 * Generate an architected exception using IStatus.
	 * @param message the exception description
	 * @throws CoreException the resulting exception
	 */
	public static void throwCoreException(
			final String message) throws CoreException {
		IStatus status =
			new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					IStatus.OK, message, null);
		throw new CoreException(status);
	}

	/**
	 * Rethrows an architected exception using IStatus.
	 * @param innerException the exception to propagate
	 * @throws CoreException the resulting exception
	 */
	public static void throwCoreException(
			final Throwable innerException) throws CoreException {
		IStatus status =
			new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					IStatus.OK, innerException.getMessage(), innerException);
		throw new CoreException(status);
	}

	/**
	 * Logs an exception using IStatus.
	 * @param innerException the exception to trace
	 */
	public static void logCoreException(
			final Throwable innerException) {
		IStatus status =
			new Status(IStatus.ERROR, Activator.PLUGIN_ID,
					IStatus.OK, innerException.getMessage(), innerException);
		getDefault().getLog().log(status);
		return;
	}
}
