package com.legstar.messaging;

/** Test utility class. */
public final class Util {
	
	
	/** Cannot be instanciated. */
	private Util() {
		
	}
	
	/**
	 * Utility method to pretty print a byte array content.
	 * @param hostBytes byte array to print
	 * @return a pretty string
	 */
	public static String toHexString(final byte[] hostBytes) {
		
		if (hostBytes == null) {
			return null;
		}
		
		StringBuffer hexString = new StringBuffer("");
		for (int i = 0; i < hostBytes.length; i++) {
			hexString.append(
					Integer.toHexString(
							hostBytes[i] & 0xFF | 0x100).substring(1, 3));
		}
		
		return hexString.toString();
	}
	
	/**
	 * Takes a string of characters representing hex data and converts it
	 * to a byte array.
	 * @param string the hex string
	 * @return the byte array
	 */
	public static byte[] toByteArray(final String string) {
		if (string == null) {
			return null;
		}
		byte[] hostBytes = new byte[string.length() / 2];
		for (int i = 0; i < string.length(); i += 2) {
			hostBytes[i / 2] = 
				(byte) Integer.parseInt(string.substring(i, i + 2), 16);
		}
		return hostBytes;
	}

}
