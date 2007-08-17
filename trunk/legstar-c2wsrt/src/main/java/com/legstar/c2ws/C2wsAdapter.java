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
package com.legstar.c2ws;

/**
 * This interface is used to shield invokers from the details of how a
 * target web service gets actually called using raw host data in
 * ebcdic.
 */
public interface C2wsAdapter {

	/**
	 * Provides a correlation is so that traces can be related from end to
	 * end.
	 * @param cxid the correlation id to use
	 */
	void setCorrelationId(String cxid);
	
	/**
	 * This method allows you to specify a host character set to use when
	 * converting host data to unicode.
	 * @param hostCharset a character set from charsets.jar
	 */
	void setHostCharset(String hostCharset);
	
	/**
	 * Invokes a target Web Service with a one host input byte buffer, one 
	 * host output byte buffer and a synchronous exchange pattern.
	 * @param wsd the web service descriptor
	 * @param hostBytes the inbound request host data 
	 * @return the outbound host reply data
	 * @throws C2wsAdapterException in invoke fails
	 */
	byte[] invoke(
			final C2wsWSDescriptor wsd,
			final byte[] hostBytes) throws C2wsAdapterException;

}
