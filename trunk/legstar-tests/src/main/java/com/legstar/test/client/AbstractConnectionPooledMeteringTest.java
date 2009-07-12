package com.legstar.test.client;

import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.PoolingEngineConfig;
import com.legstar.coxb.host.HostData;
import com.legstar.host.server.EngineHandler;
import com.legstar.host.server.EngineHolder;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;


/**
 * Common code for metering tests using pooling engine.
 * <p/>
 * This test can be run in 2 modes.
 * <ul>
 * <li>Normal single pass JUnit test in which case setUp/tearDown are executed and
 *  oneTimeSetUp/OneTimeTearDown are ignored</li>
 * <li>JMeter JUnit sampler tests in which case setUp/tearDown are ignored and
 *  oneTimeSetUp/OneTimeTearDown are used</li>
 * </ul>
 *
 */
public abstract class AbstractConnectionPooledMeteringTest extends TestCase {
    
    /** Time out (in milliseconds) for invoke. */
    private static final long DEFAULT_INVOKE_TIMEOUT_MSEC = 3000L;
    
    /** The target host endpoint name.*/
    private String _endpointName;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    
    /**
     * Use with JMeter to setup engine only once for a metering session.
     * @throws Exception if setup fails
     */
    public void oneTimeSetUp() throws Exception {
        _log.info("oneTimeSetup");
        PoolingEngineConfig config = getPoolingEngineConfig();
        _endpointName = config.getHostEndpoints().get(0).getName();
        EngineHandler serverHandler = new EngineHandler(config);
        serverHandler.init();
    }
    
    /**
     * @return a pooling engine configuration with the transport endpoints
     * it is important that only one endpoint be defined in this config
     */
    public abstract PoolingEngineConfig getPoolingEngineConfig();

    /**
     * Use with JMeter to shutdown engine only once for a metering session.
     * @throws Exception if setup fails
     */
    public void oneTimeTearDown() throws Exception {
        _log.info("oneTimeTearDown");
        EngineHolder.stop();
    }
    
    /**
     * {@inheritDoc}
     * Make sure JMeter does not run this. This is only useful when run locally.
     */
    public void setUp() throws Exception {
        oneTimeSetUp();
    }
    
    /**
     * {@inheritDoc}
     * Make sure JMeter does not run this. This is only useful when run locally.
     */
    public void tearDown() throws Exception {
        oneTimeTearDown();
    }

    /**
     * Submit a request for LSFILEAE execution, wait for the reply and check it.
     * @throws Exception if test fails
     */
    public void testLsfileae() throws Exception {
        LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                new LegStarAddress(getEndpointName()));
        EngineHolder.getEngine().addRequest(request);
        request.await(DEFAULT_INVOKE_TIMEOUT_MSEC, TimeUnit.MILLISECONDS);
        if (request.getException() != null) {
            throw request.getException();
        } else {
            if (request.getResponseMessage() == null) {
                throw new RequestException(
                        "Timed out waiting for a response for Request:"
                        + request.getID());
            }
        }
        assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
        assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
    }
    
    /**
     * @return the target host endpoint
     */
    public String getEndpointName() {
        return _endpointName;
    }

}
