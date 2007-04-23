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
package com.legstar.eclipse.plugin.cixsgen;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;


/**
 * Legstar Cixsgen preferences handling class.
 *
 */
public class CixsgenPreferences {

	/* ============================ STORE ID SECTION ========================*/

	/** The preference store ID for the CIXS package name prefix. */
	public static final String CIXS_PKG_PFX_PREF = "CixsPackagePrefix";
	
	/** The preference store ID for the CIXS namespace prefix. */
	public static final String CIXS_NS_PFX_PREF = "CixsNamespacePrefix";

	/** The preference store ID for the cixsgen location. */
	public static final String GEN_LOC_PREF = "CixsgenLocation";
	
	/** The preference store ID for the Ant scripts target location. */
	public static final String CIXS_ANT_DIR_PREF = "AntDir";
	
	/** The preference store ID for the Web descriptors target location. */
	public static final String CIXS_WDD_DIR_PREF = "WddDir";
	
	/** The preference store ID for the Properties files target location. */
	public static final String CIXS_PROP_DIR_PREF = "PropDir";
	
	/** The preference store ID for the War files target location. */
	public static final String CIXS_WAR_DIR_PREF = "WarDir";
	
	/* ========================= DEFAULT VALUES SECTION =====================*/

	/** The default CIXS package name prefix. */
	public static final String DEFAULT_CIXS_PKG_PFX = "legstar.test.ws";
	
	/** The default CIXS namespace prefix. */
	public static final String DEFAULT_CIXS_NS_PFX = "http://test.legstar/ws";
	
	/** The default cixsgen location. */
	public static final String DEFAULT_GEN_LOC = "";
	
	/** The default Ant scripts location. */
	public static final String DEFAULT_CIXS_ANT_DIR = "ant";
	
	/** The default Web descriptors location. */
	public static final String DEFAULT_CIXS_WDD_DIR = "WebContent/WEB-INF";
	
	/** The default Properties files location. */
	public static final String DEFAULT_CIXS_PROP_DIR =
		"WebContent/WEB-INF/classes";
	
	/** The default War files location. */
	public static final String DEFAULT_CIXS_WAR_DIR =
		"${env.CATALINA_HOME}/webapps";
	
	/* =========================== PROPERTIES SECTION =======================*/

	/** The CIXS package name prefix. */
	private String mCixsPackagePrefix;
	
	/** The CIXS namespace prefix. */
	private String mCixsNamespacePrefix;
	
	/** The CIXS generator location. */
	private String mCixsgenLocation;
	
	/** The CIXS Ant scripts location. */
	private String mCixsAntDir;
	
	/** The CIXS Web descriptors location. */
	private String mCixsWddDir;
	
	/** The CIXS Properties files location. */
	private String mCixsPropDir;
	
	/** The CIXS War files location. */
	private String mCixsWarDir;
	
	/** No argument constructor. */
	public CixsgenPreferences() {
		IPreferencesService service = Platform.getPreferencesService();
		mCixsgenLocation = service.getString(Activator.PLUGIN_ID,
				GEN_LOC_PREF,
				DEFAULT_GEN_LOC, null);
		mCixsPackagePrefix = service.getString(Activator.PLUGIN_ID,
				CIXS_PKG_PFX_PREF, DEFAULT_CIXS_PKG_PFX, null);
		mCixsNamespacePrefix = service.getString(Activator.PLUGIN_ID,
				CIXS_NS_PFX_PREF, DEFAULT_CIXS_NS_PFX, null);
		mCixsAntDir = service.getString(Activator.PLUGIN_ID,
				CIXS_ANT_DIR_PREF, DEFAULT_CIXS_ANT_DIR, null);
		mCixsWddDir = service.getString(Activator.PLUGIN_ID,
				CIXS_WDD_DIR_PREF, DEFAULT_CIXS_WDD_DIR, null);
		mCixsPropDir = service.getString(Activator.PLUGIN_ID,
				CIXS_PROP_DIR_PREF, DEFAULT_CIXS_PROP_DIR, null);
		mCixsWarDir = service.getString(Activator.PLUGIN_ID,
				CIXS_WAR_DIR_PREF, DEFAULT_CIXS_WAR_DIR, null);
	}

	/**
	 * @return the cixsgen location
	 */
	public final String getCixsgenLocation() {
		return mCixsgenLocation;
	}
	/**
	 * @return the CIXS package name prefix
	 */
	public final String getCixsPackagePrefix() {
		return mCixsPackagePrefix;
	}

	/**
	 * @return the CIXS namespace prefix
	 */
	public final String getCixsNamespacePrefix() {
		return mCixsNamespacePrefix;
	}

	/**
	 * @return the CIXS Ant scripts location
	 */
	public final String getCixsAntDir() {
		return mCixsAntDir;
	}

	/**
	 * @return the CIXS Web descriptors location
	 */
	public final String getCixsWddDir() {
		return mCixsWddDir;
	}

	/**
	 * @return the CIXS Properties files location
	 */
	public final String getCixsPropDir() {
		return mCixsPropDir;
	}

	/**
	 * @return the CIXS War files location
	 */
	public final String getCixsWarDir() {
		return mCixsWarDir;
	}


}
