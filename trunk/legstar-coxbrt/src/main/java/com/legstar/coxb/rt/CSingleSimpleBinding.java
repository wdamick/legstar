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
package com.legstar.coxb.rt;

import com.legstar.coxb.ICobolBinding;
import com.legstar.host.HostException;

/**
 * This class is a superclass of all other simple element types implementing
 * binding between a java type and cobol.
 *
 * @author Fady Moussallam
 * 
*/
public abstract class CSingleSimpleBinding
	extends CBinding implements ICobolBinding {

	/** Cobol element length in bytes. */
	private int mByteLength = 0;

	/**
	 * Constructor for a cobol simple element to java binding.
	 * 
	 * @param javaName the name of the bound java property
	 * @param javaType the type of the bound java property
	 * @param byteLength the cobol element length in bytes
	 */
	public CSingleSimpleBinding(
			final String javaName,
			final String javaType,
			final int byteLength) {
		super(javaName, javaType);
		mByteLength = byteLength;
		
	}
	
	/** {@inheritDoc} */
	public final int getByteLength() throws HostException {
		return mByteLength;
	}

	/**
	 * @param byteLength the Cobol element length in bytes to set
	 */
	public final void setByteLength(final int byteLength) {
		mByteLength = byteLength;
	}

}
