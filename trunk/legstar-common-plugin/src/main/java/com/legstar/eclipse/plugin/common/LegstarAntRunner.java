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

import java.util.Map;

import org.eclipse.ant.core.AntRunner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 * Encapsulates code to run ANT under eclipse.
 *
 */
public class LegstarAntRunner {
	
	/** Set of common preferences. */
	private LegstarPreferences mLegstarPref;
	
	/** No argument constructor. */
	public LegstarAntRunner() {
		mLegstarPref = new LegstarPreferences();
	}
	
	/**
	 * Submit an ANT script for execution.
	 * @param buildFileLocation fully qualified ANT script file location
	 * @param targets arrays of targets
	 * @param properties map of properties/values expected by the ANT script
	 * @param monitor the progress monitor to use
	 * @param scale the progress scaling factor
	 * @throws CoreException if submission fails
	 */
	public final void run(
			final String buildFileLocation,
			final String[] targets,
			final Map < String, String > properties,
			final IProgressMonitor monitor,
			final int scale) throws CoreException {
		AntRunner runner = new AntRunner();
		runner.setBuildFileLocation(buildFileLocation);
		runner.setExecutionTargets(targets);
		runner.addBuildLogger(mLegstarPref.getAntLogger());
		runner.setMessageOutputLevel(mLegstarPref.getAntMessageOutputLevel());
		if (properties != null) {
			runner.addUserProperties(properties);
		}
		runner.run(new SubProgressMonitor(monitor, 1 * scale));
	}
}
