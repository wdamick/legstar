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
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.models.AbstractPropertiesModel;

/**
 * Set of parameters needed for HTTP transport.
 */
public class HttpTransportParameters extends AbstractPropertiesModel {

    /* ====================================================================== */
    /* = Constants section = */
    /* ====================================================================== */
    /** Port number when it is not set. */
    public static final int PORT_NOT_SET = -1;

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */
    /** HTTP Scheme. */
    public static final String HTTP_SCHEME = "httpScheme";

    /** HTTP Scheme. */
    public static final String HTTP_HOST = "httpHost";

    /** HTTP Scheme. */
    public static final String HTTP_PORT = "httpPort";

    /** HTTP Scheme. */
    public static final String HTTP_PATH = "httpPath";

    /** HTTP Scheme. */
    public static final String HTTP_USERID = "httpUserId";

    /** HTTP Scheme. */
    public static final String HTTP_PASSWORD = "httpPassword";

    /** HTTP Scheme. */
    public static final String HTTP_URL = "httpURL";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */
    /** The HTTP scheme in use. */
    private HttpScheme _httpScheme = HttpScheme.http;

    /** The host on which the HTTP server listens. */
    private String _httpHost = CodeGenUtil.getLocalIPAddress();

    /** The port number on which the HTTP server listens. */
    private int _httpPort = PORT_NOT_SET;

    /** User ID for basic authentication. */
    private String _httpUserId = "";

    /** Password for basic authentication. */
    private String _httpPassword = "";

    /** The path to the service. */
    private String _httpPath = "";

    /**
     * A no-Arg constructor.
     */
    public HttpTransportParameters() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public HttpTransportParameters(final Properties props) {
        super(props);
        setScheme(getString(props, HTTP_SCHEME, null));
        setHost(getString(props, HTTP_HOST, null));
        setPort(getInt(props, HTTP_PORT, PORT_NOT_SET));
        setPath(getString(props, HTTP_PATH, null));
        setUserId(getString(props, HTTP_USERID, null));
        setPassword(getString(props, HTTP_PASSWORD, null));
    }

    /**
     * HTTP parameters are expected by templates to come from a parameters map.
     * 
     * @param parameters a parameters map to which http parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(HTTP_SCHEME, getScheme());
        parameters.put(HTTP_HOST, getHost());
        parameters.put(HTTP_PORT, Integer.toString(getPort()));
        parameters.put(HTTP_PATH, getPath());
        parameters.put(HTTP_USERID, getUserId());
        parameters.put(HTTP_PASSWORD, getPassword());
        parameters.put(HTTP_URL, getUrl());
    }

    /**
     * Check that parameters are set correctly.
     * 
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
        return _httpScheme;
    }

    /**
     * @param scheme the scheme to use with HTTP
     */
    public void setScheme(final HttpScheme scheme) {
        _httpScheme = scheme;
    }

    /**
     * @return the scheme to use with HTTP as a String
     */
    public String getSchemeAsString() {
        return _httpScheme.toString();
    }

    /**
     * @param scheme the scheme to use with HTTP as a String
     */
    public void setScheme(
            final String scheme) {
        HttpScheme value = HttpScheme.valueOf(
                scheme.toLowerCase(Locale.getDefault()));
        _httpScheme = value;
    }

    /**
     * @return the host on which the HTTP server listens
     */
    public String getHost() {
        return _httpHost;
    }

    /**
     * @param host the host on which the HTTP server listens
     */
    public void setHost(final String host) {
        _httpHost = host;
    }

    /**
     * @return the port number on which the HTTP server listens
     */
    public int getPort() {
        return _httpPort;
    }

    /**
     * @param port the port number on which the HTTP server listens
     */
    public void setPort(final int port) {
        _httpPort = port;
    }

    /**
     * @return the path to the service
     */
    public String getPath() {
        return _httpPath;
    }

    /**
     * @param path the path to the service to set
     */
    public void setPath(final String path) {
        _httpPath = path;
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
        return _httpUserId;
    }

    /**
     * @param userId the user ID for basic authentication to set
     */
    public void setUserId(final String userId) {
        _httpUserId = userId;
    }

    /**
     * @return the password for basic authentication
     */
    public String getPassword() {
        return _httpPassword;
    }

    /**
     * @param password the password for basic authentication to set
     */
    public void setPassword(final String password) {
        _httpPassword = password;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, HTTP_SCHEME, getSchemeAsString());
        putString(props, HTTP_HOST, getHost());
        putInt(props, HTTP_PORT, getPort());
        putString(props, HTTP_PATH, getPath());
        putString(props, HTTP_USERID, getUserId());
        putString(props, HTTP_PASSWORD, getPassword());
        return props;
    }
}
