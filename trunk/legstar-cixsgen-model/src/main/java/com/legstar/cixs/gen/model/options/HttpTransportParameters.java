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
package com.legstar.cixs.gen.model.options;

import java.util.Locale;
import java.util.Map;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * Set of parameters needed for HTTP transport.
 */
public class HttpTransportParameters {

    /** The HTTP scheme in use. */
    private HttpScheme mScheme = HttpScheme.http;
    
    /** The host on which the HTTP server listens. */
    private String mHost =  CodeGenUtil.getLocalIPAddress();
    
    /** The default port number on which the HTTP server listens. */
    public static final int DEFAULT_PORT = 8080;
    
    /** The port number on which the HTTP server listens. */
    private int mPort = DEFAULT_PORT;
    
    /** User ID for basic authentication. */
    private String mUserId = "";

    /** Password for basic authentication. */
    private String mPassword = "";

    /** The path to the service. */
    private String mPath = "";

    /**
     * HTTP parameters are expected by templates to come from a parameters map.
     * @param parameters a parameters map to which http parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put("httpScheme", getScheme());
        parameters.put("httpHost", getHost());
        parameters.put("httpPort", Integer.toString(getPort()));
        parameters.put("httpPath", getPath());
        parameters.put("httpUserId", getUserId());
        parameters.put("httpPassword", getPassword());
        parameters.put("httpURL", getUrl());
    }

    /**
     * Check that parameters are set correctly.
     * @throws CodeGenMakeException if parameters are missing or wrong
     */
    public void check() throws CodeGenMakeException {
        if (!getScheme().equals(HttpScheme.http)) {
            throw new CodeGenMakeException(
                    "Scheme " + getScheme() + " is not supported at this time");
        }
        if (getHost() == null || getHost().length() == 0) {
            throw new CodeGenMakeException(
            "You must specify an HTTP host");
        }
        if (getPath() == null || getPath().length() == 0) {
            throw new CodeGenMakeException(
            "You must specify an HTTP path");
        }
        if (getPath().charAt(0) != '/') {
            throw new CodeGenMakeException(
            "The HTTP path must start with the / character");
        }
        CodeGenUtil.checkHttpURI(getUrl());
    }
    
    /**
     * @return a String representation of the HTTP URL
     */
    public String getUrl() {
        return getScheme() + "://" + getHost() + ":" + getPort() + getPath();
    }

    /**
     * @return the scheme to use with HTTP
     */
    public HttpScheme getScheme() {
        return mScheme;
    }

    /**
     * @param scheme the scheme to use with HTTP
     */
    public void setScheme(final HttpScheme scheme) {
        mScheme = scheme;
    }

    /**
     * @return the scheme to use with HTTP as a String
     */
    public String getSchemeAsString() {
        return mScheme.toString();
    }

    /**
     * @param scheme the scheme to use with HTTP as a String
     */
    public void setScheme(
            final String scheme) {
        HttpScheme value = HttpScheme.valueOf(
                scheme.toLowerCase(Locale.getDefault()));
        mScheme = value;
    }

    /**
     * @return the host on which the HTTP server listens
     */
    public String getHost() {
        return mHost;
    }

    /**
     * @param host the host on which the HTTP server listens
     */
    public void setHost(final String host) {
        mHost = host;
    }

    /**
     * @return the port number on which the HTTP server listens
     */
    public int getPort() {
        return mPort;
    }

    /**
     * @param port the port number on which the HTTP server listens
     */
    public void setPort(final int port) {
        mPort = port;
    }

    /**
     * @return the path to the service
     */
    public String getPath() {
        return mPath;
    }

    /**
     * @param path the path to the service to set
     */
    public void setPath(final String path) {
        mPath = path;
    }

    /**
     * The http schemes supported.
     */
    public enum HttpScheme {
        /** Http. */
        http, 
        /** Secure Http. */
        https       
    }

    /**
     * @return the user ID for basic authentication
     */
    public String getUserId() {
        return mUserId;
    }

    /**
     * @param userId the user ID for basic authentication to set
     */
    public void setUserId(final String userId) {
        mUserId = userId;
    }

    /**
     * @return the password for basic authentication
     */
    public String getPassword() {
        return mPassword;
    }

    /**
     * @param password the password for basic authentication to set
     */
    public void setPassword(final String password) {
        mPassword = password;
    }
    
}
