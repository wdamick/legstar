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
/**
 * 
 */
package com.legstar.coxb.convert;

import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.host.HostException;

/**
 * This class uses the Strategy pattern to shield marshallers/unmarshallers from
 * the specific Java numeric to cobol representation.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolDoubleConverter {
	/**
	 * Converts an element java value to a host representation stored in host
	 * buffer.
	 * @param ce Cobol element descriptor 
	 * @param hostTarget Target host buffer
	 * @param offset Offset in the target host buffer
	 * @return the new offset after host buffer has been updated
	 * @throws HostException if conversion fails
	 */
	int toHost(ICobolDoubleBinding ce, byte[] hostTarget, int offset)
		throws HostException; 
	
	/**
	 * Converts an array of java values to a host representation stored in 
	 * host buffer.
	 * @param ce Cobol array descriptor 
	 * @param hostTarget Target host buffer
	 * @param offset Offset in the target host buffer
	 * @param currentOccurs actual number of items in array
	 * @return the new offset after host buffer has been updated
	 * @throws HostException if conversion fails
	 */
	int toHost(ICobolArrayDoubleBinding ce, byte[] hostTarget, int offset,
			int currentOccurs)
		throws HostException;
	
	/**
	 * Converts an element Cobol value from a host buffer to a java value.
	 * @param ce Cobol element descriptor 
	 * @param hostSource Source host buffer
	 * @param offset Offset in the source host buffer
	 * @return the new offset after host buffer has been read
	 * @throws HostException if conversion fails
	 */
	int fromHost(ICobolDoubleBinding ce, byte[] hostSource, int offset)
		throws HostException;
	
	/**
	 * Converts an array of Cobol values from a host buffer to a java array.
	 * @param ce Cobol array descriptor 
	 * @param hostSource Source host buffer
	 * @param offset Offset in the source host buffer
	 * @param currentOccurs actual number of items in array
	 * @return the new offset after host buffer has been read
	 * @throws HostException if conversion fails
	 */
	int fromHost(ICobolArrayDoubleBinding ce, byte[] hostSource, int offset,
			int currentOccurs)
		throws HostException;
	
}
