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
package com.legstar.messaging;

/**
 * Abstract factory shields users from the transport layer.
 */
public interface ConnectionFactory {
	
	/**
	 * Create a new host connection. This method creates a connection object
	 * but wether or not a physical connection is established is left to
	 * the actual implementation.
	 * 
	 * @param connectionID an identifier for this connection
	 * @param address the host address to connect to
	 * @return the new host connection
	 * @throws ConnectionException if failed to create connection
	 */
	Connection createConnection(
			String connectionID, Address address) throws ConnectionException;
}
