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

/**
 * This class expose all properties that are useful at generation time.
 *
 */
public class CixsGenDescriptor extends CixsGenPreferences {
	
	/** Error message if Cixsgen location has not been set. */
	private static final String GEN_NOT_SET = "Cixsgen location not set in "
		+ "preferences. Use Window-->Preferences to set the LegStar CIXS "
		+ "Generator location to a valid Cixsgen install folder (Cixsgen must "
		+ "have been installed prior to using this plugin).";
	

	/**
	 * Constructor loads preferences.
	 * @throws AntCreationException if preferences are not set
	 */
	public CixsGenDescriptor() throws AntCreationException {
		super();
	
		/* Make sure preferences have been setup */
		if (getCixsgenLocation() == null
				|| getCixsgenLocation().length() == 0) {
			throw (new AntCreationException(GEN_NOT_SET));
		}

	}
}
