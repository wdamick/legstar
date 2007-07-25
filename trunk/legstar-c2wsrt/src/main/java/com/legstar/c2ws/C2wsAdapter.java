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
