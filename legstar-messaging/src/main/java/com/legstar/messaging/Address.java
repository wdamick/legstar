package com.legstar.messaging;

import org.apache.commons.configuration.HierarchicalConfiguration;


/**
 * Implements a generic host address including the necessary credentials.
 */
public class Address {
	
	/** Configuration XPath location endpoint. */
	private static final String ENDPOINT_NAME_CFG = "hostEndPoint";
	
	/** Configuration XPath location endpoint name attribute. */
	private static final String ENDPOINT_NAME_ATTR_CFG = "@name";
	
	/** Relative XPath location within enpoint for host character set. */
	private static final String CHARSET_CFG = "hostCharset";
	
	/** Relative XPath location within enpoint for host user ID. */
	private static final String USERID_CFG = "hostUserID";
	
	/** Relative XPath location within enpoint for host password. */
	private static final String PASSWORD_CFG = "hostPassword";

	/** Relative XPath location within enpoint for host trace mode. */
	private static final String TRACEMODE_CFG = "hostTraceMode";
	
	/** Host User ID. */
	private String mHostUserID;

	/** Host Password. */
	private String mHostPassword;
	
	/** A logical name for a host destination. */
	private String mEndPointName;

	/** The host character set to use for this request. */
	private String mHostCharset;
	
	/** Host trace mode. */
	private boolean mHostTraceMode = false;
	
	/**
	 * An address points to an endpoint.
	 * @param endPointName the target endpoint name
	 */
	public Address(final String endPointName) {
		mEndPointName = endPointName;
	}
	
	/**
	 * Construct an Address from a configuration fragment describing
	 * an endpoint.
	 * @param endpointConfig an XML configuration fragment
	 */
	public Address(final HierarchicalConfiguration endpointConfig) {
		mEndPointName = endpointConfig.getString(ENDPOINT_NAME_ATTR_CFG);
		mHostCharset = endpointConfig.getString(CHARSET_CFG);
		mHostUserID = endpointConfig.getString(USERID_CFG);
		mHostPassword = endpointConfig.getString(PASSWORD_CFG);
		mHostTraceMode = endpointConfig.getBoolean(TRACEMODE_CFG, false);
	}
	
	/**
	 * Construct a new address from partial address, complementing missing
	 * attributes with configuration values.
	 * @param address the partial address
	 * @param endpointConfig an XML configuration fragment
	 */
	public Address(
			final Address address,
			final HierarchicalConfiguration endpointConfig) {
		
		this(endpointConfig);
		if (address != null) {
			if (address.getEndPointName() != null
					&&	address.getEndPointName().length() > 0) {
				mEndPointName = address.getEndPointName();
			}
			if (address.getHostCharset() != null
					&&	address.getHostCharset().length() > 0) {
				mHostCharset = address.getHostCharset();
			}
			if (address.getHostUserID() != null
					&&	address.getHostUserID().length() > 0) {
				mHostUserID = address.getHostUserID();
			}
			if (address.getHostPassword() != null
					&&	address.getHostPassword().length() > 0) {
				mHostPassword = address.getHostPassword();
			}
			if (address.isHostTraceMode()) {
				mHostTraceMode = true;
			}
		}
}
	
	/**
	 * Two addresses are considered equal if their non-null attributes
	 * are equal.
	 * @param obj the address to be compared to
	 * @return true if objects are identical
	 */
	public final boolean equals(final Object obj) {
		/* The target has no constraints therefore it equal to any address */
		if (obj == null) {
			return true;
		}
		
		/* If the target is from a different class, it cannot be equal */
	    if (obj.getClass() != Address.class) {
	    	return false;
	    }
	    
	    String ojbEndPointName = ((Address) obj).getEndPointName();
	    if (ojbEndPointName != null && mEndPointName != null
	    		&& !ojbEndPointName.equals(mEndPointName)) {
	    	return false;
	    }
	    
	    String ojbHostCharset = ((Address) obj).getHostCharset();
	    if (ojbHostCharset != null && mHostCharset != null
	    		&& !ojbHostCharset.equals(mHostCharset)) {
	    	return false;
	    }
	    
	    String ojbHostUserID = ((Address) obj).getHostUserID();
	    if (ojbHostUserID != null && mHostUserID != null
	    		&& !ojbHostUserID.equals(mHostUserID)) {
	    	return false;
	    }
	    
	    return true;
	}
	
	/**
	 * @see Object#hashCode() 
	 * {@inheritDoc}
	 */
	public final int hashCode() {
		String hashString = "";
		if (mEndPointName != null) {
			hashString += mEndPointName;
		}
		if (mHostCharset != null) {
			hashString += mHostCharset;
		}
		if (mHostUserID != null) {
			hashString += mHostUserID;
		}
	    return hashString.hashCode();
	}
	
	/**
	 * @return the logical name for a host destination
	 */
	public final String getEndPointName() {
		return mEndPointName;
	}

	/**
	 * @param endPointName the logical name for a host destination to set
	 */
	public final void setEndPointName(final String endPointName) {
		mEndPointName = endPointName;
	}

	/**
	 * @return the Host User ID
	 */
	public final String getHostUserID() {
		return mHostUserID;
	}

	/**
	 * @param hostUserID the Host User ID to set
	 */
	public final void setHostUserID(final String hostUserID) {
		mHostUserID = hostUserID;
	}

	/**
	 * @return the Host Password
	 */
	public final String getHostPassword() {
		return mHostPassword;
	}

	/**
	 * @param password the Host Password to set
	 */
	public final void setHostPassword(final String password) {
		mHostPassword = password;
	}

	/**
	 * @return the host character set
	 */
	public final String getHostCharset() {
		return mHostCharset;
	}

	/**
	 * @param hostCharset the host character set to set
	 */
	public final void setHostCharset(final String hostCharset) {
		mHostCharset = hostCharset;
	}

	/**
	 * @return the host trace mode enabled or or
	 */
	public final boolean isHostTraceMode() {
		return mHostTraceMode;
	}

	/**
	 * @param hostTraceMode the host trace mode to set
	 */
	public final void setHostTraceMode(final boolean hostTraceMode) {
		mHostTraceMode = hostTraceMode;
	}

	/**
	 * Helper to pretty print the address content.
	 * @return formatted address report
	 */
	public final String getReport() {
		String report = "Address:\n"
			+ "  " + ENDPOINT_NAME_CFG + "\t : " + mEndPointName + "\n"
			+ "  " + CHARSET_CFG + "\t : " + mHostCharset + "\n"
			+ "  " + USERID_CFG + "\t : " + mHostUserID + "\n"
			+ "  " + TRACEMODE_CFG + "\t : " + mHostTraceMode;
		return report;
	}
	

}
