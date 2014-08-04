/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cixs.gen.model.options;

import java.util.Map;
import java.util.Properties;

import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractPropertiesModel;

/**
 * Set of parameters needed for Websphere MQ transport.
 */
public class WmqTransportParameters extends AbstractPropertiesModel {

    /* ====================================================================== */
    /* = Constants section = */
    /* ====================================================================== */
    /** The default jndi file system directory relative to mule install folder. */
    public static final String DEFAULT_JNDI_FS_DIRECTORY = "file:///JNDI-Directory";

    /**
     * The default context factory class used to do naming lookups for WMQ
     * resources.
     */
    public static final String DEFAULT_JNDI_CONTEXT_FACTORY = "com.sun.jndi.fscontext.RefFSContextFactory";

    /** The default suffix for WMQ Queue name receiving requests. */
    public static final String DEFAULT_REQUEST_QUEUE_SUFFIX = "REQUEST.QUEUE";

    /** The default suffix for WMQ Queue name receiving replies. */
    public static final String DEFAULT_REPLY_QUEUE_SUFFIX = "REPLY.QUEUE";

    /** The default suffix for WMQ Queue name receiving errors. */
    public static final String DEFAULT_ERROR_QUEUE_SUFFIX = "ERROR.QUEUE";

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** WMQ jndi url. */
    public static final String WMQ_JNDI_URL = "wmqJndiUrl";

    /** WMQ jndi context factory. */
    public static final String WMQ_JNDI_CONTEXT_FACTORY = "wmqJndiContextFactory";

    /** WMQ connection factory. */
    public static final String WMQ_CONNECTION_FACTORY = "wmqConnectionFactory";

    /** WMQ zos queue manager. */
    public static final String WMQ_ZOS_QUEUE_MANAGER = "wmqZosQueueManager";

    /** WMQ request queue. */
    public static final String WMQ_REQUEST_QUEUE = "wmqRequestQueue";

    /** WMQ reply queue. */
    public static final String WMQ_REPLY_QUEUE = "wmqReplyQueue";

    /** WMQ error queue. */
    public static final String WMQ_ERROR_QUEUE = "wmqErrorQueue";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** The URL used to do naming lookups for WMQ resources. */
    private String _jndiUrl = DEFAULT_JNDI_FS_DIRECTORY;

    /** The context factory class used to do naming lookups for WMQ resources. */
    private String _jndiContextFactory = DEFAULT_JNDI_CONTEXT_FACTORY;

    /**
     * The connection-factory used to lookup queues/topics in a naming directory
     * (JNDI).
     */
    private String _connectionFactory;

    /** The ZOS WMQ Manager. */
    private String _zosQueueManager;

    /** The WMQ Queue name receiving requests. */
    private String _requestQueue;

    /** The WMQ Queue name receiving replies. */
    private String _replyQueue;

    /** The WMQ Queue name receiving errors. */
    private String _errorQueue;

    /**
     * A no-Arg constructor.
     */
    public WmqTransportParameters() {
        super();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public WmqTransportParameters(final Properties props) {
        super(props);
        setJndiUrl(getString(props, WMQ_JNDI_URL, DEFAULT_JNDI_FS_DIRECTORY));
        setJndiContextFactory(getString(props, WMQ_JNDI_CONTEXT_FACTORY,
                DEFAULT_JNDI_CONTEXT_FACTORY));
        setConnectionFactory(getString(props, WMQ_CONNECTION_FACTORY, null));
        setZosQueueManager(getString(props, WMQ_ZOS_QUEUE_MANAGER, null));
        setRequestQueue(getString(props, WMQ_REQUEST_QUEUE, null));
        setReplyQueue(getString(props, WMQ_REPLY_QUEUE, null));
        setErrorQueue(getString(props, WMQ_ERROR_QUEUE, null));
    }

    /**
     * WMQ parameters are expected by templates to come from a parameters map.
     * 
     * @param parameters a parameters map to which wmq parameters must be added
     */
    public void add(final Map < String, Object > parameters) {
        parameters.put(WMQ_JNDI_URL, getJndiUrl());
        parameters.put(WMQ_JNDI_CONTEXT_FACTORY, getJndiContextFactory());
        parameters.put(WMQ_CONNECTION_FACTORY, getConnectionFactory());
        parameters.put(WMQ_ZOS_QUEUE_MANAGER, getZosQueueManager());
        parameters.put(WMQ_REQUEST_QUEUE, getRequestQueue());
        parameters.put(WMQ_REPLY_QUEUE, getReplyQueue());
        if (getErrorQueue() != null && getErrorQueue().length() > 0) {
            parameters.put(WMQ_ERROR_QUEUE, getErrorQueue());
        }
    }

    /**
     * Check that parameters are set correctly.
     * 
     * @throws CodeGenMakeException if parameters are missing or wrong
     */
    public void check() throws CodeGenMakeException {
        if (getJndiUrl() == null || getJndiUrl().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ JNDI URL");
        }
        if (getJndiContextFactory() == null
                || getJndiContextFactory().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ JNDI context factory");
        }
        if (getConnectionFactory() == null
                || getConnectionFactory().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ JNDI connection factory");
        }
        if (getZosQueueManager() == null || getZosQueueManager().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ ZOS queue Manager");
        }
        if (getRequestQueue() == null || getRequestQueue().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ Target request queue");
        }
        if (getReplyQueue() == null || getReplyQueue().length() == 0) {
            throw new CodeGenMakeException(
                    "You must specify a Websphere MQ Target reply queue");
        }

    }

    /**
     * @return the URL used to do naming lookups for WMQ resources
     */
    public String getJndiUrl() {
        return _jndiUrl;
    }

    /**
     * @param jndiUrl the URL used to do naming lookups for WMQ resources
     */
    public void setJndiUrl(final String jndiUrl) {
        _jndiUrl = jndiUrl;
    }

    /**
     * @return the context factory class the JBossESB will use
     *         to do naming lookups for WMQ resources
     */
    public String getJndiContextFactory() {
        return _jndiContextFactory;
    }

    /**
     * @param jndiContextFactory the context factory class the JBossESB will use
     *            to do naming lookups for WMQ resources
     */
    public void setJndiContextFactory(final String jndiContextFactory) {
        _jndiContextFactory = jndiContextFactory;
    }

    /**
     * @return the connection-factory used to lookup queues/topics in a naming
     *         directory (JNDI)
     */
    public String getConnectionFactory() {
        return _connectionFactory;
    }

    /**
     * @param connectionFactory the connection-factory used to lookup
     *            queues/topics in a naming directory (JNDI)
     */
    public void setConnectionFactory(final String connectionFactory) {
        _connectionFactory = connectionFactory;
    }

    /**
     * @return the WMQ Queue name receiving requests
     */
    public String getRequestQueue() {
        return _requestQueue;
    }

    /**
     * @param requestQueue the WMQ Queue name receiving requests
     */
    public void setRequestQueue(final String requestQueue) {
        _requestQueue = requestQueue;
    }

    /**
     * @return the WMQ Queue name receiving replies
     */
    public String getReplyQueue() {
        return _replyQueue;
    }

    /**
     * @param replyQueue the WMQ Queue name receiving replies
     */
    public void setReplyQueue(final String replyQueue) {
        _replyQueue = replyQueue;
    }

    /**
     * @return the WMQ Queue name receiving errors
     */
    public String getErrorQueue() {
        return _errorQueue;
    }

    /**
     * @param errorQueue the WMQ Queue name receiving errors
     */
    public void setErrorQueue(final String errorQueue) {
        _errorQueue = errorQueue;
    }

    /**
     * @return the ZOS WMQ Manager
     */
    public String getZosQueueManager() {
        return _zosQueueManager;
    }

    /**
     * @param zosQueueManager the ZOS WMQ Manager
     */
    public void setZosQueueManager(final String zosQueueManager) {
        _zosQueueManager = zosQueueManager;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        putString(props, WMQ_JNDI_URL, getJndiUrl());
        putString(props, WMQ_JNDI_CONTEXT_FACTORY, getJndiContextFactory());
        putString(props, WMQ_CONNECTION_FACTORY, getConnectionFactory());
        putString(props, WMQ_ZOS_QUEUE_MANAGER, getZosQueueManager());
        putString(props, WMQ_REQUEST_QUEUE, getRequestQueue());
        putString(props, WMQ_REPLY_QUEUE, getReplyQueue());
        putString(props, WMQ_ERROR_QUEUE, getErrorQueue());
        return props;
    }
}
