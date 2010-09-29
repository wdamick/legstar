package com.legstar.config;

import java.util.ArrayList;
import java.util.List;

import com.legstar.messaging.HostEndpoint;

/**
 * Pooling engine configuration parameters.
 * 
 */
public class PoolingEngineConfig {

    /** Time out (in milliseconds) for initial connect. */
    public static final int DEFAULT_MAXIMUM_REQUESTS = 100000;

    /** Total number of threads in the thread pool. */
    public static final int DEFAULT_THREAD_POOL_SIZE = 25;

    /** Default maximum time (milliseconds) to wait for a pooled connection. */
    public static final int DEFAULT_TAKE_TIMEOUT = 1000;

    /** Maximum number of waiting requests. */
    private int _maxRequests = DEFAULT_MAXIMUM_REQUESTS;

    /** Total number of threads in the thread pool. */
    private int _threadPoolSize = DEFAULT_THREAD_POOL_SIZE;

    /** Work manager can be recovered from JNDI. */
    private String _workManagerJNDILocation;

    /** List of all available host endpoints. */
    private List < HostEndpoint > _hostEndpoints = new ArrayList < HostEndpoint >();

    /**
     * Maximum time (milliseconds) to wait for a pooled connection to become
     * available.
     */
    private int _takeTimeout = DEFAULT_TAKE_TIMEOUT;

    /**
     * @return the maximum number of waiting requests.
     *         When this number is reached, the engine will stop accepting
     *         requests.
     *         If the host system is unresponsive it is not useful to continue
     *         to accept requests.
     */
    public int getMaxRequests() {
        return _maxRequests;
    }

    /**
     * @param maxRequests the maximum number of waiting requests to set
     */
    public void setMaxRequests(final int maxRequests) {
        _maxRequests = maxRequests;
    }

    /**
     * @return the list of all available host endpoints
     */
    public List < HostEndpoint > getHostEndpoints() {
        return _hostEndpoints;
    }

    /**
     * @param endpoints the list of all available host endpoints to set
     */
    public void setHostEndpoints(final List < HostEndpoint > endpoints) {
        _hostEndpoints = endpoints;
    }

    /**
     * @return the work manager can be recovered from JNDI
     */
    public String getWorkManagerJNDILocation() {
        return _workManagerJNDILocation;
    }

    /**
     * @param managerJNDILocation the work manager can be recovered from JNDI to
     *            set
     */
    public void setWorkManagerJNDILocation(final String managerJNDILocation) {
        _workManagerJNDILocation = managerJNDILocation;
    }

    /**
     * The default thread pool works with a fixed number of reusable threads.
     * The engine itself uses one of the threads, then there is one thread per
     * active host connection. This number must be larger than the sum of all
     * pool sizes (defined in hostEndPoints) plus one.
     * 
     * @return the total number of threads in the thread pool
     */
    public int getThreadPoolSize() {
        return _threadPoolSize;
    }

    /**
     * @param poolSize the total number of threads in the thread pool to set
     */
    public void setThreadPoolSize(final int poolSize) {
        _threadPoolSize = poolSize;
    }

    /**
     * @return the maximum time (milliseconds) to wait for a pooled connection
     *         to become available
     */
    public int getTakeTimeout() {
        return _takeTimeout;
    }

    /**
     * @param takeTimeout the maximum time (milliseconds) to wait for a pooled
     *            connection to become available to set
     */
    public void setTakeTimeout(final int takeTimeout) {
        _takeTimeout = takeTimeout;
    }

}
