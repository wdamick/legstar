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
 * LegStar Cixsgen preferences handling class.
 *
 */
public class CixsGenPreferences {

	/* ============================ STORE ID SECTION ========================*/

	/** The preference store ID for the CIXS package name prefix. */
	public static final String CIXS_PKG_PFX_PREF = "CixsPackagePrefix";
	
	/** The preference store ID for the CIXS namespace prefix. */
	public static final String CIXS_NS_PFX_PREF = "CixsNamespacePrefix";

	/** The preference store ID for the core cixsgen install location. */
	public static final String GEN_LOC_PREF = "CixsgenLocation";
	
	/** The preference store ID for the target sources directory. */
	public static final String CIXS_SRCDIR_PREF = "CixsSourcesDir";
	
	/** The preference store ID for the target binaries directory. */
	public static final String CIXS_BINDIR_PREF = "CixsBinariesDir";
	
	/** The preference store ID for the location of JAXB binaries. */
	public static final String JAXB_BINDIR_PREF = "CixsJaxbBinariesDir";
	
	/** The preference store ID for the location of custom classes binaries. */
	public static final String CUST_BINDIR_PREF = "CixsCustBinariesDir";
	
	/** The preference store ID for the Ant scripts target location. */
	public static final String CIXS_ANT_DIR_PREF = "CixsAntScriptsDir";
	
	/** The preference store ID for the Web descriptors target location. */
	public static final String CIXS_WDD_DIR_PREF = "CixsWebDescriptorsDir";
	
	/** The preference store ID for the Properties files target location. */
	public static final String CIXS_PROP_DIR_PREF = "CixsPropertiesDir";
	
	/** The preference store ID for the War files target location. */
	public static final String CIXS_WAR_DIR_PREF = "CixsWarDir";
	
	/* ========================= DEFAULT VALUES SECTION =====================*/

	/** The default CIXS package name prefix. */
	public static final String DEFAULT_CIXS_PKG_PFX = "legstar.test.ws";
	
	/** The default CIXS namespace prefix. */
	public static final String DEFAULT_CIXS_NS_PFX = "http://test.legstar/ws";
	
	/** The default cixsgen location. */
	public static final String DEFAULT_GEN_LOC = "";
	
	/** The default target sources directory. */
	public static final String DEFAULT_SRCDIR = "src";
	
	/** The default target binaries directory. */
	public static final String DEFAULT_BINDIR = "bin";
	
	/** The default location of JAXB binaries. */
	public static final String DEFAULT_JAXBBINDIR = "bin";
	
	/** The default location of custom classes binaries. */
	public static final String DEFAULT_CUSTBINDIR = "bin";
	
	/** The default Ant scripts location. */
	public static final String DEFAULT_CIXS_ANT_DIR = "ant";
	
	/** The default Web descriptors location. */
	public static final String DEFAULT_CIXS_WDD_DIR = "WebContent/WEB-INF";
	
	/** The default Properties files location. */
	public static final String DEFAULT_CIXS_PROP_DIR =
		"WebContent/WEB-INF/classes";
	
	/** The default War files location. */
	public static final String DEFAULT_CIXS_WAR_DIR =
		"${env.CATALINA_BASE}/webapps";
	
	/* =========================== PROPERTIES SECTION =======================*/

	/** The CIXS package name prefix. */
	private String mCixsPackagePrefix;
	
	/** The CIXS namespace prefix. */
	private String mCixsNamespacePrefix;
	
	/** The CIXS generator location. */
	private String mCixsgenLocation;
	
	/** The CIXS target sources directory. */
	private String mCixsSourcesDir;
	
	/** The CIXS target binaries directory. */
	private String mCixsBinariesDir;
	
	/** The CIXS location of JAXB binaries. */
	private String mCixsJaxbBinariesDir;
	
	/** The CIXS location of custom classes binaries. */
	private String mCixsCustBinariesDir;
	
	/** The CIXS Ant scripts location. */
	private String mCixsAntScriptsDir;
	
	/** The CIXS Web descriptors location. */
	private String mCixsWebDescriptorsDir;
	
	/** The CIXS Properties files location. */
	private String mCixsPropertiesDir;
	
	/** The CIXS War files location. */
	private String mCixsWarDir;
	
	/** No argument constructor. */
	public CixsGenPreferences() {
		IPreferencesService service = Platform.getPreferencesService();
		mCixsgenLocation = service.getString(Activator.PLUGIN_ID,
				GEN_LOC_PREF,
				DEFAULT_GEN_LOC, null);
		mCixsPackagePrefix = service.getString(Activator.PLUGIN_ID,
				CIXS_PKG_PFX_PREF, DEFAULT_CIXS_PKG_PFX, null);
		mCixsNamespacePrefix = service.getString(Activator.PLUGIN_ID,
				CIXS_NS_PFX_PREF, DEFAULT_CIXS_NS_PFX, null);
		mCixsSourcesDir = service.getString(Activator.PLUGIN_ID,
				CIXS_SRCDIR_PREF, DEFAULT_SRCDIR, null);
		mCixsBinariesDir = service.getString(Activator.PLUGIN_ID,
				CIXS_BINDIR_PREF, DEFAULT_BINDIR, null);
		mCixsAntScriptsDir = service.getString(Activator.PLUGIN_ID,
				CIXS_ANT_DIR_PREF, DEFAULT_CIXS_ANT_DIR, null);
		mCixsWebDescriptorsDir = service.getString(Activator.PLUGIN_ID,
				CIXS_WDD_DIR_PREF, DEFAULT_CIXS_WDD_DIR, null);
		mCixsPropertiesDir = service.getString(Activator.PLUGIN_ID,
				CIXS_PROP_DIR_PREF, DEFAULT_CIXS_PROP_DIR, null);
		mCixsWarDir = service.getString(Activator.PLUGIN_ID,
				CIXS_WAR_DIR_PREF, DEFAULT_CIXS_WAR_DIR, null);
	}

	/**
	 * @return the core cixsgen install location
	 */
	public final String getCixsgenLocation() {
		return mCixsgenLocation;
	}

	/**
	 * @param cixsgenLocation the core cixsgen install location to set
	 */
	public final void setCixsgenLocation(
			final String cixsgenLocation) {
		mCixsgenLocation = cixsgenLocation;
	}

	/**
	 * @return the CIXS package name prefix
	 */
	public final String getCixsPackagePrefix() {
		return mCixsPackagePrefix;
	}

	/**
	 * @param cixsPackagePrefix the package name prefix to set
	 */
	public final void setCixsPackagePrefix(
			final String cixsPackagePrefix) {
		mCixsPackagePrefix = cixsPackagePrefix;
	}

	/**
	 * @return the CIXS namespace prefix
	 */
	public final String getCixsNamespacePrefix() {
		return mCixsNamespacePrefix;
	}

	/**
	 * @param cixsNamespacePrefix the namespace prefix to set
	 */
	public final void setCixsNamespacePrefix(
			final String cixsNamespacePrefix) {
		mCixsNamespacePrefix = cixsNamespacePrefix;
	}

	/**
	 * @return the CIXS Ant scripts location
	 */
	public final String getCixsAntScriptsDir() {
		return mCixsAntScriptsDir;
	}

	/**
	 * @param cixsAntScriptsDir the Ant scripts location to set
	 */
	public final void setCixsAntScriptsDir(
			final String cixsAntScriptsDir) {
		mCixsAntScriptsDir = cixsAntScriptsDir;
	}

	/**
	 * @return the CIXS Web descriptors location
	 */
	public final String getCixsWebDescriptorsDir() {
		return mCixsWebDescriptorsDir;
	}

	/**
	 * @param cixsWebDescriptorsDir the Web descriptors location to set
	 */
	public final void setCixsWebDescriptorsDir(
			final String cixsWebDescriptorsDir) {
		mCixsWebDescriptorsDir = cixsWebDescriptorsDir;
	}

	/**
	 * @return the CIXS Properties files location
	 */
	public final String getCixsPropertiesDir() {
		return mCixsPropertiesDir;
	}

	/**
	 * @param cixsPropertiesDir the Properties files location to set
	 */
	public final void setCixsPropertiesDir(
			final String cixsPropertiesDir) {
		mCixsPropertiesDir = cixsPropertiesDir;
	}

	/**
	 * @return the CIXS War files location
	 */
	public final String getCixsWarDir() {
		return mCixsWarDir;
	}

	/**
	 * @param cixsWarDir the War files location to set
	 */
	public final void setCixsWarDir(
			final String cixsWarDir) {
		mCixsWarDir = cixsWarDir;
	}

	/**
	 * @return the target binaries directory
	 */
	public final String getCixsBinariesDir() {
		return mCixsBinariesDir;
	}

	/**
	 * @param cixsBinariesDir the target binaries directory to set
	 */
	public final void setCixsBinariesDir(
			final String cixsBinariesDir) {
		mCixsBinariesDir = cixsBinariesDir;
	}

	/**
	 * @return the target sources directory
	 */
	public final String getCixsSourcesDir() {
		return mCixsSourcesDir;
	}

	/**
	 * @param cixsSourcesDir the target sources directory to set
	 */
	public final void setCixsSourcesDir(
			final String cixsSourcesDir) {
		mCixsSourcesDir = cixsSourcesDir;
	}

	/**
	 * @return the location of custom classes binaries
	 */
	public final String getCixsCustBinariesDir() {
		return mCixsCustBinariesDir;
	}

	/**
	 * @param cixsCustBinariesDir the location of custom classes binaries to set
	 */
	public final void setCixsCustBinariesDir(
			final String cixsCustBinariesDir) {
		mCixsCustBinariesDir = cixsCustBinariesDir;
	}

	/**
	 * @return the location of JAXB binaries
	 */
	public final String getCixsJaxbBinariesDir() {
		return mCixsJaxbBinariesDir;
	}

	/**
	 * @param cixsJaxbBinariesDir the location of JAXB binaries to set
	 */
	public final void setCixsJaxbBinariesDir(
			final String cixsJaxbBinariesDir) {
		mCixsJaxbBinariesDir = cixsJaxbBinariesDir;
	}

}
