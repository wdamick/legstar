package com.legstar.pool.manager;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HostEndpoint.AccessStrategy;
import com.legstar.mock.client.MockEndpoint;

import junit.framework.TestCase;

/**
 * Some common test methods.
 * 
 */
public class AbstractConnectionPoolTester extends TestCase {

    /** Maximum connection time (ms). */
    public static final long KEEP_ALIVE_TIME = 300L;

    /** Period between 2 checks of idle connections (ms). */
    public static final long IDLE_TEST_PERIOD = 150L;

    /**
     * Load a configured pool.
     * 
     * @param poolSize the pool size
     * @return a connection pool
     * @throws ConnectionPoolException if pool cannot be created
     */
    public ConnectionPool getConnectionPool(
            final int poolSize) throws ConnectionPoolException {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        ConnectionPool connectionPool = new ConnectionPool(address,
                getPooledHostEndpoint(poolSize));
        return connectionPool;
    }

    /**
     * @param poolSize the pool size
     * @return a pooled host endpoint
     */
    public HostEndpoint getPooledHostEndpoint(final int poolSize) {
        HostEndpoint endpoint = new MockEndpoint();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass(
                "com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostAccessStrategy(AccessStrategy.pooled);
        endpoint.setHostConnectionPoolSize(poolSize);
        endpoint.setPooledInvokeTimeout(2000);
        endpoint.setPooledMaxIdleTime(KEEP_ALIVE_TIME);
        endpoint.setPooledMaxIdleTimeCheckPeriod(IDLE_TEST_PERIOD);
        return endpoint;
    }

    /**
     * @return the formatted time with milliseconds
     */
    public static String now() {
        Calendar cal = Calendar.getInstance();
        SimpleDateFormat sdf = new SimpleDateFormat("H:mm:ss:SSS");
        return sdf.format(cal.getTime());
    }
}
