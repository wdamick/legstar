package com.legstar.test.cixs;


/**
 * Tests a Web Service adapter using a raw HTTP Client. This reduces the overhead as
 * opposed to using a JAX-WS client.
 *
 */
public class HttpClientLsfileae102Test extends AbstractAdapterHttpClientTester {

    /** The SOAP request expected by target Web Service (captured by TCPMon). */
    public static final String SOAP_REQUEST =
        "<?xml version=\"1.0\" ?>"
        + "<S:Envelope xmlns:S=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        + "<S:Header>"
        + "<ns2:HostHeader xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\""
        + " xmlns:ns3=\"http://legstar.com/test/coxb/lsfileae\">"
        + "<ns2:hostUserID>P390</ns2:hostUserID>"
        + "<ns2:hostPassword>STREAM2</ns2:hostPassword>"
        + "<ns2:hostEndPoint>${endpointName}</ns2:hostEndPoint>"
        + "<ns2:hostTraceMode>false</ns2:hostTraceMode>"
        + "</ns2:HostHeader>"
        + "</S:Header>"
        + "<S:Body>"
        + "<ns2:LsfileaeRequest xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\""
        + " xmlns:ns3=\"http://legstar.com/test/coxb/lsfileae\">"
        + "<ns2:Request>"
        + "<ns3:ComNumber>102</ns3:ComNumber>"
        + "</ns2:Request>"
        + "</ns2:LsfileaeRequest>"
        + "</S:Body>"
        + "</S:Envelope>";
    
    /** The SOAP reply produced by target Web Service (captured by TCPMon). */
    public static final String SOAP_REPLY =
        "<?xml version=\"1.0\" ?>"
        + "<S:Envelope xmlns:S=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        + "<S:Body>"
        + "<ns2:LsfileaeResponse xmlns:ns2=\"http://cixs.test.legstar.com/lsfileae\""
        + " xmlns:ns3=\"http://legstar.com/test/coxb/lsfileae\">"
        + "<ns2:Response>"
        + "<ns3:ComNumber>102</ns3:ComNumber>"
        + "<ns3:ComPersonal>"
        + "<ns3:ComName>J. T. CZAYKOWSKI</ns3:ComName>"
        + "<ns3:ComAddress>WARWICK, ENGLAND</ns3:ComAddress>"
        + "<ns3:ComPhone>98356183</ns3:ComPhone>"
        + "</ns3:ComPersonal>"
        + "<ns3:ComDate>26 11 81</ns3:ComDate>"
        + "<ns3:ComAmount>$1111.11</ns3:ComAmount>"
        + "<ns3:ComComment>*********</ns3:ComComment>"
        + "</ns2:Response>"
        + "</ns2:LsfileaeResponse>"
        + "</S:Body>"
        + "</S:Envelope>";
    
    /**
     * Construct the test.
     */
    public HttpClientLsfileae102Test() {
        super("lsfileae", SOAP_REQUEST, SOAP_REPLY);
    }
    
}
