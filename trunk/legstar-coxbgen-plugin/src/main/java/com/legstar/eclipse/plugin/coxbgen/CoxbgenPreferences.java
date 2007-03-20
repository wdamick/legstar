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
package com.legstar.eclipse.plugin.coxbgen;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 * Legstar Coxbagen preferences handling class.
 *
 */
public class CoxbgenPreferences {

	/** The preference store ID for the coxbgen location. */
	public static final String GEN_LOC_PREF = "CoxbgenLocation";
	
	/** The default coxbgen location. */
	public static final String DEFAULT_GEN_LOC = "";
	
	/** The COXB generator location. */
	private String mCoxbgenLocation;
	
	/** No argument constructor. */
	public CoxbgenPreferences() {
		IPreferencesService service = Platform.getPreferencesService();
		mCoxbgenLocation = service.getString(Activator.PLUGIN_ID,
				GEN_LOC_PREF,
				DEFAULT_GEN_LOC, null);
	}

	/**
	 * @return the coxbgen location
	 */
	public final String getCoxbgenLocation() {
		return mCoxbgenLocation;
	}


}
