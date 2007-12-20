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
package com.legstar.coxb;

import com.legstar.coxb.host.HostException;

/**
 * This interface groups methods that are common to all octet stream elements
 * that are not arrays.
 * 
 * @author Fady Moussallam
 * 
*/
public interface ICobolOctetStreamBinding extends ICobolBinding {
	
	/**
	 * Returns this element value.
	 * @return Element value
	 * @throws HostException if value cannot be retrieved
	 */
	byte[] getByteArrayValue() throws HostException;
	
	
	/**
	 * Sets the element value.
	 * @param value Value to set
	 * @throws HostException if value cannot be set
	 */
	void setByteArrayValue(byte[] value) throws HostException;
	
}
