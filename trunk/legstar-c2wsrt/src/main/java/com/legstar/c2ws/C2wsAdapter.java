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
package com.legstar.c2ws;

/**
 * This interface is used to shield invokers from the details of how a
 * target web service gets actually called using raw host data in
 * ebcdic.
 */
public interface C2wsAdapter {
	
	/**
	 * Setup the adapter for a specific target web service.
	 * @param wsd the target web service descriptor
	 * @param hostCharset the a character set from charsets.jar
	 * @throws C2wsConfigurationException if initialization fails
	 */
	void init(
			final C2wsWSDescriptor wsd,
			final String hostCharset) throws C2wsConfigurationException;

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
	 * @param hostBytes the inbound request host data 
	 * @return the outbound host reply data
	 * @throws C2wsAdapterException in invoke fails
	 */
	byte[] invoke(final byte[] hostBytes) throws C2wsAdapterException;

}
