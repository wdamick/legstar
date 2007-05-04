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
package com.legstar.host.invoke;

import com.legstar.messaging.Address;

/**
 * A factory providing a host invoke capability. In this version, only
 * one such capability is provided.
 */
public final class HostInvokeFactory {
	
	/**
	 * This factory is a utility class.
	 */
	private HostInvokeFactory() {
		
	}

	/**
	 * An Invoker is constructed from a configuration file, for a particular
	 * host address and target host program.
	 * @param generalConfigFileName an XML configuration file name
	 * @param address the host address
	 * @param programAttributesFileName the host program attributes properties
	 * file
	 * @return a Host invoke implementation
	 * @throws HostInvokeException in construction fails
	 */
	public static HostInvoke createHostInvoke(
			final String generalConfigFileName,
			final Address address,
			final String programAttributesFileName)
			throws HostInvokeException {
		return new CommareaInvoke(
				generalConfigFileName, address, programAttributesFileName);
	}

}
