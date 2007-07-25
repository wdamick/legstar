package com.legstar.c2ws.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/** Utility class. */
public final class C2wsUtil {
	
	/** Flood prevention for large data.   */
	private static final int MAX_TRACES_BYTES  = 500;    

	/** Logger. */
	private static final Log LOG =	LogFactory.getLog(C2wsUtil.class);
	
	/** Utility classes are not instanciated. */
	private C2wsUtil() {
		
	}
	
	/**
	 * Produce a dump-like report of a data buffer content.
	 * @param cxid a correlation id
	 * @param data the raw data to trace
	 * @param dataLength the size of the data
	 */
	public static void traceData(
			final String cxid,
			final byte[] data, final long dataLength) {

		/* Enhanced logger with correlation id. */
		C2wsLog log = new C2wsLog(LOG, cxid);
		
		StringBuilder dumpLine = new StringBuilder(); // 128
		String dumpChar; //[5];
		StringBuilder dumpString =  new StringBuilder(); //[17];

		for (int i = 0; i < dataLength && i < MAX_TRACES_BYTES; i++) {
			/* print every 16 byte on a different line */
			dumpChar = String.format("%02X ", data[i] & 0xff);
			dumpLine.append(dumpChar);
			if (Character.isValidCodePoint(data[i])) {
				dumpChar = String.format("%c", data[i]);
			} else {
				dumpChar = "?";
			}
			if (dumpChar.length() > 0) {
				dumpString.append(dumpChar);
			} else {
				dumpString.append(" ");
			}
			if (i % 16 == 15 || i == dataLength - 1) {
				while (i % 16 < 15) {
					dumpLine.append("   ");
					i++;
				}
				dumpLine.append(" -- ");
				dumpLine.append(dumpString);
				log.debug(dumpLine);
				dumpString =  new StringBuilder();
				dumpLine = new StringBuilder();
			}
		}

		if (dataLength > MAX_TRACES_BYTES) {
			dumpLine.append(String.format("...data was truncated at %d bytes",
					MAX_TRACES_BYTES));
			log.debug(dumpLine);
		}
		return;
	}


}
