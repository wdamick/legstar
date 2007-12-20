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
package com.legstar.eclipse.plugin.common;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * A utility class used by other plugins for exception reporting and
 * instrumentation.
 *
 */
public final class LegstarReport {
	
	/** This is a utility class not meant for instanciation. */
	private LegstarReport() {
		
	}
	
	/**
	 * Generate an architected exception using IStatus.
	 * @param message the exception description
	 * @param pluginID plug-in identifier
	 * @throws CoreException the resulting exception
	 */
	public static void throwCoreException(
			final String message,
			final String pluginID) throws CoreException {
		IStatus status =
			new Status(IStatus.ERROR, pluginID,
					IStatus.OK, message, null);
		throw new CoreException(status);
	}

	/**
	 * Rethrows an architected exception using IStatus.
	 * @param innerException the exception to propagate
	 * @param pluginID plug-in identifier
	 * @throws CoreException the resulting exception
	 */
	public static void throwCoreException(
			final Throwable innerException,
			final String pluginID) throws CoreException {
		IStatus status =
			new Status(IStatus.ERROR, pluginID,
					IStatus.OK, innerException.getMessage(), innerException);
		throw new CoreException(status);
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
	
	/**
	 * Logs an error message using IStatus.
	 * @param message the message to trace
	 * @param pluginID plug-in identifier
	 */
	public static void logCoreError(
			final String message,
			final String pluginID) {
		logCoreMessage(message,	IStatus.ERROR, pluginID);
	}

	/**
	 * Logs an informational message using IStatus.
	 * @param message the message to trace
	 * @param pluginID plug-in identifier
	 */
	public static void logCoreInfo(
			final String message,
			final String pluginID) {
		logCoreMessage(message,	IStatus.INFO, pluginID);
	}

	/**
	 * Logs a message using IStatus.
	 * @param message the message to trace
	 * @param severity the message severity
	 * @param pluginID plug-in identifier
	 */
	public static void logCoreMessage(
			final String message,
			final int severity,
			final String pluginID) {
		IStatus status =
			new Status(severity, pluginID,
					IStatus.OK, message, null);
		Activator.getDefault().getLog().log(status);
		return;
	}

}
