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
package com.legstar.coxb;

import com.legstar.host.HostContext;

/**
 * 
 * This class encapsulates all Cobol compiler options that might influence
 * the conversion from Cobol representation to Java representation.
 *
 * @author Fady Moussallam
 * 
*/
public class CobolContext extends HostContext {

	/** True if cobol option for extended arithmetics (31 digits) is set. */
	private boolean mArithExtend = false;
	
	/** True if PIC N items are to be handled as DBCS items. */
	private boolean mNsymbolDbcs = false;
	
	/**
	 * This enumeration class represents how binary data is truncated depending
	 * on the number of digits specified in the PICTURE clause.
	 *
	 */
	public enum trunc {
		/** TRUNC(OPT) Leaves it up to the compiler to decide to truncate or
		 *             not, based on performance considerations. */
		OPT,
		/** TRUNC(BIN) No truncation occurs, this is equivalent to COMP-5. */
		BIN,
		/** TRUNC(STD) Data truncated to the number of digits in the 
		 *             PICTURE clause. */
		STD 
	}

	/**
	 * @return Returns the arithExtend.
	 */
	public final boolean isArithExtend() {
		return mArithExtend;
	}

	/**
	 * @param arithExtend The arithExtend to set.
	 */
	public final void setArithExtend(final boolean arithExtend) {
		mArithExtend = arithExtend;
	}

	/**
	 * @return Returns the symbol Dbcs.
	 */
	public final boolean isSymbolDbcs() {
		return mNsymbolDbcs;
	}

	/**
	 * @param nsymbolDbcs The symbol Dbcs to set.
	 */
	public final void setSymbolDbcs(final boolean nsymbolDbcs) {
		mNsymbolDbcs = nsymbolDbcs;
	}

}
