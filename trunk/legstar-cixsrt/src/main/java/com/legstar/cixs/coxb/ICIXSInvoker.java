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
package com.legstar.cixs.coxb;

/**
 * Used to abstract the connectivity details from users.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICIXSInvoker {
	
	/**
	 * Setup invoker permanent parameters.
	 * @param header data passed from client
	 * @param operationName the requested operation
	 * @throws CIXSException if initialization fails
	 */
	void initialize(
			CIXSHeader header,
			String operationName) throws CIXSException;
	
	/**
	 * Returns a new invoke parameter description.
	 * @return the new parameter description
	 */
	CIXSParameter createParameter();
	
	/**
	 * Call a remote host program.
	 * 
	 * @param inParameter input parameters description
	 * @param outParameter output parameters description
	 * @throws CIXSException if call fails
	 */
	void invoke(
			CIXSParameter inParameter,
			CIXSParameter outParameter) throws CIXSException;

}
