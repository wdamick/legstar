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
package com.legstar.test.cixs;

/**
 * Tests a Web Service adapter using a raw HTTP Client. This reduces the
 * overhead as opposed to using a JAX-WS client.
 * 
 */
public class HttpClientLsfileae100ITCase extends AbstractAdapterHttpClientTester {

    /** The SOAP request expected by target Web Service (captured by TCPMon). */
    public static final String SOAP_REQUEST = "<?xml version=\"1.0\" ?>"
            + "<S:Envelope xmlns:S=\"http://schemas.xmlsoap.org/soap/envelope/\">"
            + "<S:Header>"
            + "<ns2:LsfileaeHostHeader xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\""
            + " xmlns:ns3=\"http://legstar.com/test/coxb/lsfileae\">"
            + "<ns2:hostUserID>P390</ns2:hostUserID>"
            + "<ns2:hostPassword>STREAM2</ns2:hostPassword>"
            + "<ns2:hostEndPoint>${endpointName}</ns2:hostEndPoint>"
            + "<ns2:hostTraceMode>false</ns2:hostTraceMode>"
            + "</ns2:LsfileaeHostHeader>"
            + "</S:Header>"
            + "<S:Body>"
            + "<ns2:LsfileaeRequest xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\""
            + " xmlns=\"http://legstar.com/test/coxb/lsfileae\">"
            + "<Dfhcommarea>" + "<ComNumber>100</ComNumber>" + "</Dfhcommarea>"
            + "</ns2:LsfileaeRequest>" + "</S:Body>" + "</S:Envelope>";

    /** The SOAP reply produced by target Web Service (captured by TCPMon). */
    public static final String SOAP_REPLY = "<?xml version=\"1.0\" ?>"
            + "<S:Envelope xmlns:S=\"http://schemas.xmlsoap.org/soap/envelope/\">"
            + "<S:Body>"
            + "<ns2:LsfileaeResponse xmlns=\"http://legstar.com/test/coxb/lsfileae\""
            + " xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\">"
            + "<Dfhcommarea>" + "<ComNumber>100</ComNumber>" + "<ComPersonal>"
            + "<ComName>S. D. BORMAN</ComName>"
            + "<ComAddress>SURREY, ENGLAND</ComAddress>"
            + "<ComPhone>32156778</ComPhone>" + "</ComPersonal>"
            + "<ComDate>26 11 81</ComDate>" + "<ComAmount>$0100.11</ComAmount>"
            + "<ComComment>*********</ComComment>" + "</Dfhcommarea>"
            + "</ns2:LsfileaeResponse>" + "</S:Body>" + "</S:Envelope>";

    /**
     * Construct the test.
     */
    public HttpClientLsfileae100ITCase() {
        super("lsfileae", SOAP_REQUEST, SOAP_REPLY);
    }

    /**
     * Order is important to avoid class loading issues.
     * 
     * @return
     */
    public String[] getDeployables() {
        return new String[] { "target/war/cixs-lsfileae.war" };
    }
}
