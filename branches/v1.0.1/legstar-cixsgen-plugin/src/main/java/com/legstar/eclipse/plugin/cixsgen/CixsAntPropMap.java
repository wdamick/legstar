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

import java.util.HashMap;
import java.util.Map;

/**
 * This class maps project properties with a target Ant script properties.
 *
 */
public class CixsAntPropMap {
	
	/** Ant script base directory. */
	private static final String ANT_BASE_DIR = "basedir";

	/** Ant script package prefix property name. */
	private static final String ANT_CIXS_PKG_PFX = "cixs.package.pfx";

	/** Ant script namespace prefix property name. */
	private static final String ANT_CIXS_NS_PFX = "cixs.namespace.pfx";

	/** Ant script CIXS Sources target location property name. */
	private static final String ANT_CIXS_SRC_DIR = "cixs.src.dir";

	/** Ant script CIXS binaries target location property name. */
	private static final String ANT_CIXS_BIN_DIR = "cixs.bin.dir";

	/** Ant script Ant scripts target location property name. */
	private static final String ANT_CIXS_ANT_DIR = "ant.dir";

	/** Ant script Web descriptors target location property name. */
	private static final String ANT_CIXS_WDD_DIR = "wdd.dir";

	/** Ant script Properties files target location property name. */
	private static final String ANT_CIXS_PROP_DIR = "prop.dir";

	/** Ant script War files target location property name. */
	private static final String ANT_CIXS_WAR_DIR = "war.dir";

	/** Ant JAXB and binding classes location property name. */
	private static final String ANT_JAXB_BIN_DIR = "jaxb.bin.dir";

	/** Ant script custom code classes location property name. */
	private static final String ANT_CUST_BIN_DIR = "cust.bin.dir";

	/** Properties formatted as expected by the Ant script. */
	private Map < String, String > mAntProp;

	/** Error message if Cixsgen location has not been set. */
	private static final String GEN_NOT_SET = "Cixsgen location not set in "
		+ "preferences. Use Window-->Preferences to set the LegStar CIXS "
		+ "Generator location to a valid Cixsgen install folder (Cixsgen must "
		+ "have been installed prior to using this plugin).";
	

	/**
	 * Constructor loads preferences.
	 * @throws AntCreationException if preferences are not set
	 */
	public CixsAntPropMap() throws AntCreationException {
		CixsgenPreferences cixsgenPref = new CixsgenPreferences();
		mAntProp = new HashMap < String, String >();
		
		/* Make sure preferences have been setup */
		if (cixsgenPref.getCixsgenLocation() == null
				|| cixsgenPref.getCixsgenLocation().length() == 0) {
			throw (new AntCreationException(GEN_NOT_SET));
		}
		mAntProp.put(ANT_BASE_DIR, cixsgenPref.getCixsgenLocation());
		mAntProp.put(ANT_CIXS_PKG_PFX, cixsgenPref.getCixsPackagePrefix());
		mAntProp.put(ANT_CIXS_NS_PFX, cixsgenPref.getCixsNamespacePrefix());
	}
	
	/**
	 * @return the ant properties map
	 */
	public final Map < String, String > getMap() {
		return mAntProp;
	}
	
	/**
	 * Set base directory value.
	 * @param basedir base directory
	 */
	public final void setBaseDir(final String basedir) {
		mAntProp.put(ANT_BASE_DIR, basedir);
	}
	
	/**
	 * @return base directory
	 */
	public final String getBaseDir() {
		return mAntProp.get(ANT_BASE_DIR);
	}

	/**
	 * Set package prefix value.
	 * @param cixsPackagePrefix package prefix 
	 */
	public final void setCixsPackagePrefix(
			final String cixsPackagePrefix) {
		mAntProp.put(ANT_CIXS_PKG_PFX, cixsPackagePrefix);
	}
	
	/**
	 * @return package prefix 
	 */
	public final String getCixsPackagePrefix() {
		return mAntProp.get(ANT_CIXS_PKG_PFX);
	}

	/**
	 * Set namespace prefix value.
	 * @param cixsNamespacePrefix namespace prefix 
	 */
	public final void setCixsNamespacePrefix(
			final String cixsNamespacePrefix) {
		mAntProp.put(ANT_CIXS_NS_PFX, cixsNamespacePrefix);
	}
	
	/**
	 * @return namespace prefix 
	 */
	public final String getCixsNamespacePrefix() {
		return mAntProp.get(ANT_CIXS_NS_PFX);
	}

	/**
	 * Set CIXS Sources target location value.
	 * @param cixsSrcDir CIXS Sources target location 
	 */
	public final void setCixsSrcDir(final String cixsSrcDir) {
		mAntProp.put(ANT_CIXS_SRC_DIR, cixsSrcDir);
	}
	
	/**
	 * @return CIXS Sources target location 
	 */
	public final String getCixsSrcDir() {
		return mAntProp.get(ANT_CIXS_SRC_DIR);
	}

	/**
	 * Set CIXS binaries target location value.
	 * @param cixsBinDir CIXS binaries target location 
	 */
	public final void setCixsBinDir(final String cixsBinDir) {
		mAntProp.put(ANT_CIXS_BIN_DIR, cixsBinDir);
	}
	
	/**
	 * @return CIXS binaries target location 
	 */
	public final String getCixsBinDir() {
		return mAntProp.get(ANT_CIXS_BIN_DIR);
	}

	/**
	 * Set JAXB binaries target location value.
	 * @param jaxbBinDir JAXB binaries target location 
	 */
	public final void setJaxbBinDir(final String jaxbBinDir) {
		mAntProp.put(ANT_JAXB_BIN_DIR, jaxbBinDir);
	}
	
	/**
	 * @return JAXB binaries target location 
	 */
	public final String getJaxbBinDir() {
		return mAntProp.get(ANT_JAXB_BIN_DIR);
	}

	/**
	 * Set Custom code binaries target location value.
	 * @param custBinDir JAXB binaries target location 
	 */
	public final void setCustBinDir(final String custBinDir) {
		mAntProp.put(ANT_CUST_BIN_DIR, custBinDir);
	}
	
	/**
	 * @return Custom code binaries target location 
	 */
	public final String getCustBinDir() {
		return mAntProp.get(ANT_CUST_BIN_DIR);
	}

	/**
	 * Set generated ant scripts location value.
	 * @param antDir generated ant scripts location 
	 */
	public final void setAntDir(final String antDir) {
		mAntProp.put(ANT_CIXS_ANT_DIR, antDir);
	}
	
	/**
	 * @return generated ant scripts location location 
	 */
	public final String getAntDir() {
		return mAntProp.get(ANT_CIXS_ANT_DIR);
	}

	/**
	 * Set generated web descriptors location value.
	 * @param wddDir generated web descriptors location 
	 */
	public final void setWddDir(final String wddDir) {
		mAntProp.put(ANT_CIXS_WDD_DIR, wddDir);
	}
	
	/**
	 * @return generated web descriptors location location 
	 */
	public final String getWddDir() {
		return mAntProp.get(ANT_CIXS_WDD_DIR);
	}

	/**
	 * Set generated properties files location value.
	 * @param propDir generated properties files location 
	 */
	public final void setPropDir(final String propDir) {
		mAntProp.put(ANT_CIXS_PROP_DIR, propDir);
	}
	
	/**
	 * @return generated properties files location location 
	 */
	public final String getPropDir() {
		return mAntProp.get(ANT_CIXS_PROP_DIR);
	}

	/**
	 * Set generated war files location value.
	 * @param warDir generated war files location 
	 */
	public final void setWarDir(final String warDir) {
		mAntProp.put(ANT_CIXS_WAR_DIR, warDir);
	}
	
	/**
	 * @return generated war files location location 
	 */
	public final String getWarDir() {
		return mAntProp.get(ANT_CIXS_WAR_DIR);
	}
}
