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
package com.legstar.messaging;

import junit.framework.TestCase;

/**
 * Test the HostEndpoint class.
 * 
 */
public class HostEndpointTest extends TestCase {

    /** Test the toString method. */
    public void testToString() {
        HostEndpoint endpoint = new HostEndpointImpl();
        endpoint.setName("TheMainframe");
        endpoint.setHostConnectionfactoryClass(
                "com.legstar.mock.client.MockConnectionFactory");
        endpoint.setHostCharset("IBM01147");
        endpoint.setHostUserID("MYUSER");
        endpoint.setHostPassword("MYPASS");
        endpoint.setHostTraceMode(true);
        endpoint.setConnectTimeout(45);
        endpoint.setReceiveTimeout(789);
        endpoint.setHostConnectionPoolSize(18);
        endpoint.setPooledInvokeTimeout(456);
        endpoint.setPooledMaxKeepAlive(74);
        endpoint.setPooledIdleTestPeriod(13);
        assertEquals(
                "[hostEndpoint=TheMainframe,"
                        + "hostCharset=IBM01147,"
                        + "hostUserID=MYUSER,"
                        + "hostPassword=********,"
                        + "hostTraceMode=true,"
                        + "connectTimeout=45,"
                        + "receiveTimeout=789,"
                        + "hostConnectionfactoryClass=com.legstar.mock.client.MockConnectionFactory,"
                        + "hostAccessStrategy=direct,"
                        + "hostConnectionPoolSize=18,"
                        + "pooledInvokeTimeout=456,"
                        + "pooledMaxKeepAlive=74,"
                        + "pooledIdleTestPeriod=13]",
                endpoint.toString());
    }

    /**
     * Get connection factory should try to instantiate one if none found.
     */
    public void testGetConnectionFactory() {
        /* First try with an existing factory */
        ConnectionFactory connectionFactory = new MockConnectionFactory();
        HostEndpoint endpoint = new HostEndpointImpl(connectionFactory);
        assertEquals(connectionFactory, endpoint.getHostConnectionfactory());

        /* A class name is needed. */
        endpoint.setHostConnectionfactory(null);
        try {
            endpoint.getHostConnectionfactory();
        } catch (IllegalStateException e) {
            assertEquals("Host endpoint has no connection factory class name",
                    e.getMessage());
        }

        /* The class name must be on the classpath. */
        endpoint.setHostConnectionfactoryClass("complement.bidon");
        try {
            endpoint.getHostConnectionfactory();
        } catch (IllegalStateException e) {
            assertEquals("java.lang.ClassNotFoundException: complement.bidon",
                    e.getMessage());
        }

        /* Now force the endpoint to create a new one. */
        endpoint.setHostConnectionfactory(null);
        endpoint
                .setHostConnectionfactoryClass("com.legstar.messaging.MockConnectionFactory");
        ConnectionFactory connectionFactory2 = endpoint
                .getHostConnectionfactory();
        assertTrue(connectionFactory2 != connectionFactory);
    }

    /**
     * A test implementation of a host endpoint.
     * 
     */
    public class HostEndpointImpl extends HostEndpoint {

        /**
         * No-arg constructor.
         */
        public HostEndpointImpl() {
        }

        /**
         * Constructor using an existing connection factory.
         * 
         * @param connectionFactory an instance of a connection factory
         */
        public HostEndpointImpl(final ConnectionFactory connectionFactory) {
            super(connectionFactory);
        }

        /** {@inheritDoc} */
        public void check() throws ConnectionException {
        }

    }

}
