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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostException;

/**
 * An abstract class representing simple cobol version methods.
 *
 * @author Fady Moussallam
 * 
 */
public abstract class CobolSimpleConverter {

	/** Cobol compiler parameters. */
	private CobolContext mCobolContext;
	
	/**
	 * @param cobolContext the Cobol compiler parameters in effect
	 */
	public CobolSimpleConverter(final CobolContext cobolContext) {
		mCobolContext = cobolContext;
	}

	/**
	 * @return Returns the CobolContext.
	 */
	public final CobolContext getCobolContext() {
		return mCobolContext;
	}

	/**
	 * @param cobolContext The CobolContext to set.
	 */
	public final void setCobolContext(final CobolContext cobolContext) {
		mCobolContext = cobolContext;
	}
	
	/**
	 * Formats a meaningful error message to help track conversion errors.
	 * @param ce the faulty binding element 
	 * @param e the conversion exception
	 * @throws HostException the resulting host exception
	 */
	public final void throwHostException(
			final ICobolBinding ce, 
			final CobolConversionException e)
	        throws HostException {
		throw (new HostException("ConversionException for element:"
				+ ce.getBindingName()
				+ " Cobol name:" + ce.getCobolName()
				+ " Reason:" + e.getMessage()));
		
	}
}
