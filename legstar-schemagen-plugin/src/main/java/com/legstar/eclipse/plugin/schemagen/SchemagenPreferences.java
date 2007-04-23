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
package com.legstar.eclipse.plugin.schemagen;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 * Legstar Schemagen preferences handling class.
 *
 */
public class SchemagenPreferences {

	/** The preference store ID for the namespace prefix. */
	public static final String NS_PREFIX_PREF = "NsPrefix";
	
	/** The default namespace prefix. */
	public static final String DEFAULT_NS_PREFIX = "http://test.legstar";
	
	/** The XML schema namespace prefix. */
	private String mNsPrefix;
	
	/** No argument constructor. */
	public SchemagenPreferences() {
		IPreferencesService service = Platform.getPreferencesService();
		mNsPrefix = service.getString(Activator.PLUGIN_ID,
				NS_PREFIX_PREF,
				DEFAULT_NS_PREFIX, null);
	}

	/**
	 * @return the ant logger class
	 */
	public final String getNsPrefix() {
		return mNsPrefix;
	}


}
