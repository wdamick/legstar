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
package com.legstar.cixs.jaxws.model;

import com.legstar.cixs.gen.model.AbstractCixsService;

/**
 * This class adds to <code>AbstractCixsService</code> those properties that
 * are very specific to Jaxws and would have no meaning for other targets.
 * 
 * @author Fady Moussallam
 * 
 */ 
public class CixsJaxwsService extends AbstractCixsService {
	
	/** Service host header class name. */
	private String mHeaderClassName;
	
	/** Default pattern for server URI. Must be kept in sync with
	 * various velocity templates. */
    public static final String DEFAULT_SERVER_URI_TEMPLATE =
    	"http://localhost:8080/c2ws-${service.name}/${service.name}Proxy";
    
	/** Will be appended to implementation class name to form a host header
	 *  class name. */
	private static final String HOST_HEADER_SUFFIX = "HostHeader";
	
	/**
	 * @return the host header class name
	 */
	public final String getHeaderClassName() {
		if (mHeaderClassName == null || mHeaderClassName.length() == 0) {
			return getInterfaceClassName() + HOST_HEADER_SUFFIX;
		}
		return mHeaderClassName;
	}

	/**
	 * @param headerClassName the host header class name to set
	 */
	public final void setHeaderClassName(final String headerClassName) {
		mHeaderClassName = headerClassName;
	}

	/**
	 * @return a goode default URI that the host could use to reach
	 *  the generated service proxy
	 */
	public final String getDefaultServiceURI() {
		return DEFAULT_SERVER_URI_TEMPLATE.replace(
					"${service.name}", getName());
	}
}
