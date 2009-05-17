package com.legstar.test.client;

import java.util.concurrent.TimeUnit;

import org.apache.commons.configuration.CombinedConfiguration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.DefaultConfigurationBuilder;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
    
    /** Composite configuration file for the engine.*/
    public static final String CONFIG_FILE = "legstar-engine-config.xml";
    
    /** Time out (in milliseconds) for invoke. */
    private static final long DEFAULT_INVOKE_TIMEOUT_MSEC = 3000L;
    
    /** Endpoint to use for metring test. Must be defined in CONFIG_FILE dependencies. */
    private String _endpointName;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());
    
    /**
     * @param endpointName the target mainframe endpoint name
     */
    public AbstractConnectionPooledMeteringTest(final String endpointName) {
        _endpointName = endpointName;
    }

    /**
     * Use with JMeter to setup engine only once for a metering session.
     * @throws Exception if setup fails
     */
    public void oneTimeSetUp() throws Exception {
        _log.info("oneTimeSetup");
        EngineHandler serverHandler = new EngineHandler(loadConfigFile(CONFIG_FILE));
        serverHandler.init();
    }

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
                new LegStarAddress(_endpointName));
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
     * Use the Apache configuration API to retrieve the configuration file.
     * This gives q lot of flexibility to locate the file.
     * 
     * @param configFileName name of the configuration file
     * @return the configuration retrieved
     * @throws ConfigurationException if configuration cannot be retrieved
     */
    private HierarchicalConfiguration loadConfigFile(
            final String configFileName) throws ConfigurationException {
        _log.debug("Attempting to load " + configFileName);
        DefaultConfigurationBuilder dcb = new DefaultConfigurationBuilder();
        dcb.setFileName(configFileName);
        CombinedConfiguration config = (CombinedConfiguration)
        dcb.getConfiguration(true).getConfiguration(
                DefaultConfigurationBuilder.ADDITIONAL_NAME);
        config.setExpressionEngine(new XPathExpressionEngine());
        _log.debug("Load success for " + configFileName);
        return config; 
    }
}
