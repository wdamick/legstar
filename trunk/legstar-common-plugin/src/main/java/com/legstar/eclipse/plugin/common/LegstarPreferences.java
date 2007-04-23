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

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 * Legstar prefrences handling class.
 *
 */
public class LegstarPreferences {

	/** The class name of an ant logger class. */
	private String mAntLogger;
	
	/** The verbosity level required. */
	private int mAntMessageOutputLevel;
	
	/** No argument constructor. */
	public LegstarPreferences() {
		IPreferencesService service = Platform.getPreferencesService();
		mAntLogger = service.getString(Activator.PLUGIN_ID,
				LegstarConstants.ANT_LOGGER_PREF,
				LegstarConstants.ANT_DEFAULT_LOGGER_CLASS, null);
		mAntMessageOutputLevel = service.getInt(Activator.PLUGIN_ID,
				LegstarConstants.ANT_MSGLVL_PREF,
				LegstarConstants.ANT_DEFAULT_MSGLVL, null);
	}

	/**
	 * @return the ant logger class
	 */
	public final String getAntLogger() {
		return mAntLogger;
	}

	/**
	 * @return the verbosity level required
	 */
	public final int getAntMessageOutputLevel() {
		return mAntMessageOutputLevel;
	}
	

}
