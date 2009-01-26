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
package com.legstar.c2ws.servlet;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.proxy.invoke.ProxyConfigurationException;
import com.legstar.proxy.invoke.ProxyInvokerException;
import com.legstar.proxy.invoke.ServiceProxy;

/**
 * This proxy servlet receives HTTP requests and hand over control to
 * {@link com.legstar.proxy.invoke.ServiceProxy} which performs the
 * actual call to a remote process.
 * <p/>
 * The web.xml configuration file must provide all the necessary parameters
 * for ServiceProxy to perform its task.
 * <p/>
 * The servlet does not have any knowledge of the binding types or target
 * service. It is only concerned with HTTP transport to/from a mainframe.
 *
 */
public class C2wsProxy extends javax.servlet.http.HttpServlet {

    /** The serial ID. */
    static final long serialVersionUID = 1L;

    /** The old content-type supported by this class. */
    private static final String BINARY_CONTENT_TYPE = "binary/octet-stream";

    /** This content type is more easily understood by regular web servers.
     * Starting from now it will be the preferred content type. Even if we 
     * continue to support binary/octet-stream. */
    private static final String APPLICATION_CONTENT_TYPE = "application/octet-stream";

    /** The special http header for correlation ids. */
    private static final String CORRELATION_ID_HDR = "Correlation-id";

    /** Flood prevention for large data.   */
    private static final int MAX_TRACES_BYTES  = 500;   
    
    /**
     * Configuration parameters collected at initialization time.
     */
    private Map < String, String > mProxyConfig;

    /**
     * Generic proxy service which will invoke the remote process.
     */
    private ServiceProxy mServiceProxy;

    /** Logger. */
    private static final Log LOG =  LogFactory.getLog(C2wsProxy.class);

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    public final void init(
            final ServletConfig config) throws ServletException {
        super.init(config);

        if (LOG.isDebugEnabled()) {
            LOG.debug("Initializing ");
        }
        mProxyConfig = new HashMap < String, String >();
        Enumeration < String > en = getServletConfig().getInitParameterNames();
        while (en.hasMoreElements()) {
            setInitParameter(en.nextElement(), null);
        }

        try {
            mServiceProxy = new ServiceProxy(mProxyConfig);
        } catch (ProxyConfigurationException e) {
            throw new ServletException(e);
        }
    }

    /** {@inheritDoc} */
    protected void doPost(
            final HttpServletRequest request,
            final HttpServletResponse response) throws ServletException {

        long startTime = System.currentTimeMillis();

        /* Use correlation id received in http header. This allows logs on
         * this side to be easily correlated with the mainframe logs. */
        String requestID = request.getHeader(CORRELATION_ID_HDR);
        if (LOG.isDebugEnabled()) {
            LOG.debug("Start proxy request " + requestID 
                    + " from client " + request.getRemoteHost());
        }

        /* Make sure this is a Mainframe LegStar request. */
        if (!isSupportedContentType(request)) {
            throw new ServletException("Content type "
                    + request.getContentType()
                    + " is not supported");
        }

        try {
            int requestBytesLen = request.getContentLength();     
            byte[] requestBytes = new byte[requestBytesLen];        
            request.getInputStream().read(requestBytes, 0, requestBytesLen);
            
            if (LOG.isDebugEnabled()) {
                LOG.debug("Request data from host:");
                traceData(requestID, requestBytes);
            }

            byte[] replyBytes = getServiceProxy().invoke(
                    getProxyConfig(), requestID, requestBytes);
            
            if (LOG.isDebugEnabled()) {
                LOG.debug("Reply data to host:");
                traceData(requestID, replyBytes);
            }
            response.setContentType(request.getContentType());
            response.getOutputStream().write(replyBytes);
            
        } catch (IOException e) {
            throw (new ServletException(e));
        } catch (ProxyInvokerException e) {
            throw (new ServletException(e));
        }

        long endTime = System.currentTimeMillis();
        if (LOG.isDebugEnabled()) {
            LOG.debug("End proxy request " + requestID 
                    + " from client " + request.getRemoteHost()
                    + " serviced in " + (endTime - startTime) + " msecs");
        }
    } 

    /**
     * Check that the client is sending a content type that we understand.
     * @param request the http request
     * @return true if the request has supported encoding
     */
    private boolean isSupportedContentType(final HttpServletRequest request) {
        if (request.getContentType().trim().compareToIgnoreCase(
                BINARY_CONTENT_TYPE) == 0
                || request.getContentType().trim().compareToIgnoreCase(
                        APPLICATION_CONTENT_TYPE) == 0) {
            return true;
        }
        return false;
    }

    /**
     * Locate an initialization parameter from the servlet configuration.
     * If the parameter is not found, a default value is used. The parameter
     * is then copied over to the in memory configuration.
     * @param parameterName the parameter name
     * @param defaultValue the default value if not found
     */
    private void setInitParameter(
            final String parameterName, final String defaultValue) {
        String value = getServletConfig().getInitParameter(parameterName);
        if (value == null || value.length() == 0) {
            value = defaultValue;
            if (LOG.isDebugEnabled()) {
                LOG.debug("Parameter " + parameterName + " not found."
                        + " Using default value: " + value);
            }
        } else {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Parameter " + parameterName + " found."
                        + " Using value: " + value);
            }
        }
        getProxyConfig().put(parameterName.replace("c2ws.", ""), value);
    }

    /**
     * @return the in memory set of configuration parameters
     */
    public Map < String, String > getProxyConfig() {
        return mProxyConfig;
    }

    /**
     * @param proxyConfig the in memory set of configuration parameters to set
     */
    public void setProxyConfig(final Map < String, String > proxyConfig) {
        mProxyConfig = proxyConfig;
    }

    /**
     * @return the proxy service which will invoke the remote process
     */
    public ServiceProxy getServiceProxy() {
        return mServiceProxy;
    }

    /**
     * @param serviceProxy the proxy service which will invoke the remote process to set
     */
    public void setServiceProxy(final ServiceProxy serviceProxy) {
        mServiceProxy = serviceProxy;
    }

    /**
     * Produce a dump-like report of a data buffer content.
     * @param requestID a correlation id
     * @param data the raw data to trace
     */
    public static void traceData(
            final String requestID,
            final byte[] data) {

        StringBuilder dumpLine = new StringBuilder(); // 128
        String dumpChar; //[5];
        StringBuilder dumpString =  new StringBuilder(); //[17];

        for (int i = 0; i < data.length && i < MAX_TRACES_BYTES; i++) {
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
            if (i % 16 == 15 || i == data.length - 1) {
                while (i % 16 < 15) {
                    dumpLine.append("   ");
                    i++;
                }
                dumpLine.append(" -- ");
                dumpLine.append(dumpString);
                LOG.debug(dumpLine);
                dumpString =  new StringBuilder();
                dumpLine = new StringBuilder();
            }
        }

        if (data.length > MAX_TRACES_BYTES) {
            dumpLine.append(String.format("...data was truncated at %d bytes",
                    MAX_TRACES_BYTES));
            LOG.debug(dumpLine);
        }
    }
}
