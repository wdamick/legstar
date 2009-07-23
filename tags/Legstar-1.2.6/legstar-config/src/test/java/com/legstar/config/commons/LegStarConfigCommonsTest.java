package com.legstar.config.commons;

import java.util.List;

import com.legstar.config.LegStarConfigurationException;
import com.legstar.config.PoolingEngineConfig;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.mock.client.MockEndpoint;

import junit.framework.TestCase;

/**
 * Test the LegStarConfig class.
 *
 */
public class LegStarConfigCommonsTest extends TestCase {
    
    /** Configuration file with some errors. */
    private static final String CONFIG_FILE = "config0.xml";

    /** Configuration file with no errors. */
    private static final String VALID_CONFIG_FILE = "config1.xml";

    /** Composite configuration file. */
    private static final String COMPOSITE_CONFIG_FILE = "legstar-engine-config.xml";

    /**
     * See what happens if configuration file is not found.
     */
    public void testConfigFileNotfound() {
        try {
            new LegStarConfigCommons("tarata.tsointsoin");
        } catch (LegStarConfigurationException e) {
            assertEquals("org.apache.commons.configuration.ConfigurationException:"
                    + " Cannot locate configuration source tarata.tsointsoin",
                    e.getMessage());
        }
    }
    
    /**
     * Try a non existing endpoint.
     */
    public void testEndpointNotFound() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            legStarConfig.getHostEndpoint("NotAMainframe");
        } catch (LegStarConfigurationException e) {
            assertEquals("The requested endpoint:NotAMainframe is not defined.",
                    e.getMessage());
        }
    }

    /**
     * Try a an endpoint with invalid configuration.
     */
    public void testEndpointInvalid() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            legStarConfig.getHostEndpoint("INVALID");
        } catch (LegStarConfigurationException e) {
            assertEquals("There are no connection factories in the configuration.",
                    e.getMessage());
        }
    }

    /**
     * Try a an endpoint with invalid connection factory class.
     */
    public void testEndpointInvalidConnectionFactoryClass() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            legStarConfig.getHostEndpoint("INVALID-CONNECTION-FACTORY-CLASS");
        } catch (LegStarConfigurationException e) {
            assertEquals("java.lang.ClassNotFoundException: com.legstar.truc.much.CicsSocketConnectionFactory",
                    e.getMessage());
        }
    }

    /**
     * Try a an endpoint with invalid host access strategy.
     */
    public void testEndpointInvalidHostAccessStrategy() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            legStarConfig.getHostEndpoint("INVALID-HOST-ACCESS-STRATEGY");
            fail();
        } catch (LegStarConfigurationException e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " No enum const class com.legstar.messaging.HostEndpoint$AccessStrategy.bidule",
                    e.getMessage());
        }
    }

    /**
     * Try to get and endoint.
     */
    public void testGetEndpoint() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            HostEndpoint hostEndpoint = legStarConfig.getHostEndpoint("TheMainframe");
            assertTrue(hostEndpoint instanceof MockEndpoint);
            assertEquals("IBM01140", hostEndpoint.getHostCharset());
            assertEquals("P390", hostEndpoint.getHostUserID());
            assertEquals("STREAM2", hostEndpoint.getHostPassword());
            assertEquals("TheMainframe", hostEndpoint.getName());
            assertEquals(5, hostEndpoint.getHostConnectionPoolSize());
            assertEquals(2000, hostEndpoint.getPooledInvokeTimeout());
            assertEquals(true, hostEndpoint.isHostTraceMode());
        } catch (LegStarConfigurationException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Try to get the default endpoint.
     */
    public void testGetDefaultEndpoint() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(CONFIG_FILE);
            HostEndpoint hostEndpoint = legStarConfig.getHostEndpoint(new LegStarAddress(""));
            assertTrue(hostEndpoint instanceof MockEndpoint);
            assertEquals("IBM01140", hostEndpoint.getHostCharset());
            assertEquals("P390", hostEndpoint.getHostUserID());
            assertEquals("STREAM2", hostEndpoint.getHostPassword());
            assertEquals("TheMainframe", hostEndpoint.getName());
            assertEquals(5, hostEndpoint.getHostConnectionPoolSize());
            assertEquals(2000, hostEndpoint.getPooledInvokeTimeout());
            assertEquals(true, hostEndpoint.isHostTraceMode());
        } catch (LegStarConfigurationException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Try to get all endpoints.
     */
    public void testGetAllEndpoints() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(VALID_CONFIG_FILE);
            List < HostEndpoint > endpoints = legStarConfig.getHostEndpoints();
            assertEquals(2, endpoints.size());
            HostEndpoint hostEndpoint = endpoints.get(0);
            assertTrue(hostEndpoint instanceof MockEndpoint);
            assertEquals("IBM01140", hostEndpoint.getHostCharset());
            assertEquals("P390", hostEndpoint.getHostUserID());
            assertEquals("STREAM2", hostEndpoint.getHostPassword());
            assertEquals("TheMainframe", hostEndpoint.getName());
            assertEquals(5, hostEndpoint.getHostConnectionPoolSize());
            assertEquals(2000, hostEndpoint.getPooledInvokeTimeout());
            assertEquals(true, hostEndpoint.isHostTraceMode());
            hostEndpoint = endpoints.get(1);
            assertTrue(hostEndpoint instanceof MockEndpoint);
            assertEquals("IBM01140", hostEndpoint.getHostCharset());
            assertEquals("P390", hostEndpoint.getHostUserID());
            assertEquals("STREAM2", hostEndpoint.getHostPassword());
            assertEquals("CICSTS31", hostEndpoint.getName());
            assertEquals(5, hostEndpoint.getHostConnectionPoolSize());
            assertEquals(2000, hostEndpoint.getPooledInvokeTimeout());
            assertEquals(true, hostEndpoint.isHostTraceMode());
        } catch (LegStarConfigurationException e) {
            fail(e.getMessage());
        }
    }
    
    /**
     * Try to get the pooling engine configuration.
     */
    public void testGetPoolingEngineConfig() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(VALID_CONFIG_FILE);
            PoolingEngineConfig poolingEngineConfig = legStarConfig.getPoolingEngineConfig();
            assertEquals(100005, poolingEngineConfig.getMaxRequests());
            assertEquals(26, poolingEngineConfig.getThreadPoolSize());
            assertEquals(null, poolingEngineConfig.getWorkManagerJNDILocation());
            assertEquals(2, poolingEngineConfig.getHostEndpoints().size());
        } catch (LegStarConfigurationException e) {
            fail(e.getMessage());
        }
    }

   /**
     * Try to get the pooling engine configuration from a composite configuration file.
     */
    public void testGetPoolingEngineConfigFromComposite() {
        try {
            LegStarConfigCommons legStarConfig = new LegStarConfigCommons(COMPOSITE_CONFIG_FILE);
            PoolingEngineConfig poolingEngineConfig = legStarConfig.getPoolingEngineConfig();
            assertEquals(100007, poolingEngineConfig.getMaxRequests());
            assertEquals(27, poolingEngineConfig.getThreadPoolSize());
            assertEquals(null, poolingEngineConfig.getWorkManagerJNDILocation());
            assertEquals(2, poolingEngineConfig.getHostEndpoints().size());
        } catch (LegStarConfigurationException e) {
            fail(e.getMessage());
        }
    }
}
