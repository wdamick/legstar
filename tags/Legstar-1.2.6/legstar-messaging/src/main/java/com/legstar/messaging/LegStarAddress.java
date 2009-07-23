/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.messaging;



/**
 * Implements a generic host address including the necessary credentials.
 */
public class LegStarAddress {

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
    public LegStarAddress(final String endPointName) {
        mEndPointName = endPointName;
    }

    /**
     * Construct an Address from a configuration fragment describing
     * an endpoint.
     * @param endpoint a host endpoint
     */
    public LegStarAddress(final HostEndpoint endpoint) {
        mEndPointName = endpoint.getName();
        mHostCharset = endpoint.getHostCharset();
        mHostUserID = endpoint.getHostUserID();
        mHostPassword = endpoint.getHostPassword();
        mHostTraceMode = endpoint.isHostTraceMode();
    }

    /**
     * Construct a new address from partial address, complementing missing
     * attributes with configuration values.
     * @param address the partial address
     * @param endpoint a host endpoint
     */
    public LegStarAddress(
            final LegStarAddress address,
            final HostEndpoint endpoint) {

        this(endpoint);
        if (address != null) {
            if (address.getEndPointName() != null && address.getEndPointName().length() > 0) {
                mEndPointName = address.getEndPointName();
            }
            if (address.getHostCharset() != null && address.getHostCharset().length() > 0) {
                mHostCharset = address.getHostCharset();
            }
            if (address.getHostUserID() != null && address.getHostUserID().length() > 0) {
                mHostUserID = address.getHostUserID();
            }
            if (address.getHostPassword() != null && address.getHostPassword().length() > 0) {
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
        if (obj.getClass() != LegStarAddress.class) {
            return false;
        }

        String ojbEndPointName = ((LegStarAddress) obj).getEndPointName();
        if (ojbEndPointName != null && mEndPointName != null
                && !ojbEndPointName.equals(mEndPointName)) {
            return false;
        }

        String ojbHostCharset = ((LegStarAddress) obj).getHostCharset();
        if (ojbHostCharset != null && mHostCharset != null
                && !ojbHostCharset.equals(mHostCharset)) {
            return false;
        }

        String ojbHostUserID = ((LegStarAddress) obj).getHostUserID();
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
        String report = "Address=["
            + HostEndpoint.HOST_ENDPOINT_LABEL + "=" + mEndPointName
            + "," + HostEndpoint.HOST_CHARSET_LABEL + "=" + mHostCharset
            + "," + HostEndpoint.HOST_USERID_LABEL + "=" + mHostUserID
            + "," + HostEndpoint.HOST_TRACE_LABEL + "=" + mHostTraceMode
            + "]";
        return report;
    }
    
    /** {@inheritDoc} */
    public final String toString() {
        return getReport();
    }


}
