/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
/**
 * 
 */
package com.legstar.coxb.convert;

import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.host.HostException;

/**
 * This class uses the Strategy pattern to shield marshallers/unmarshallers from
 * the specific Java numeric to cobol representation.
 *
 * @author Fady Moussallam
 * 
 */
public interface ICobolPackedDecimalConverter {
	/**
	 * Converts an element java value to a host representation stored in host
	 * buffer.
	 * @param ce Cobol element descriptor 
	 * @param hostTarget Target host buffer
	 * @param offset Offset in the target host buffer
	 * @return the new offset after host buffer has been updated
	 * @throws HostException if conversion fails
	 */
	int toHost(ICobolPackedDecimalBinding ce, byte[] hostTarget, int offset)
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
	int toHost(
			ICobolArrayPackedDecimalBinding ce, byte[] hostTarget, int offset,
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
	int fromHost(ICobolPackedDecimalBinding ce, byte[] hostSource, int offset)
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
	int fromHost(
			ICobolArrayPackedDecimalBinding ce, byte[] hostSource, int offset,
			int currentOccurs)
		throws HostException;
}
