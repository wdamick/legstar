package com.legstar.test.cixs;


/**
 * Tests a Web Service adapter using a raw HTTP Client. This one is targeted at axis2.
 *
 */
public class HttpClientLsfileae100AxisTest extends AbstractAdapterHttpClientTester {

    /** The SOAP request expected by target Web Service (captured by TCPMon). */
    public static final String SOAP_REQUEST =
        "<?xml version=\"1.0\" ?>"
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
        + "<Dfhcommarea>"
        + "<ComNumber>100</ComNumber>"
        + "</Dfhcommarea>"
        + "</ns2:LsfileaeRequest>"
        + "</S:Body>"
        + "</S:Envelope>";
    
    /** The SOAP reply produced by target Web Service (captured by TCPMon). */
    public static final String SOAP_REPLY =
        "<?xml version='1.0' encoding='utf-8'?>"
        + "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">"
        + "<soapenv:Body>"
        + "<LsfileaeResponse xmlns=\"http://cixs.test.legstar.com/lsfileae\""
        + " xmlns:ns2=\"http://legstar.com/test/coxb/lsfileae\">"
        + "<ns2:Dfhcommarea>"
        + "<ns2:ComNumber>100</ns2:ComNumber>"
        + "<ns2:ComPersonal>"
        + "<ns2:ComName>S. D. BORMAN</ns2:ComName>"
        + "<ns2:ComAddress>SURREY, ENGLAND</ns2:ComAddress>"
        + "<ns2:ComPhone>32156778</ns2:ComPhone>"
        + "</ns2:ComPersonal>"
        + "<ns2:ComDate>26 11 81</ns2:ComDate>"
        + "<ns2:ComAmount>$0100.11</ns2:ComAmount>"
        + "<ns2:ComComment>*********</ns2:ComComment>"
        + "</ns2:Dfhcommarea>"
        + "</LsfileaeResponse>"
        + "</soapenv:Body>"
        + "</soapenv:Envelope>";
    
    /**
     * Construct the test.
     */
    public HttpClientLsfileae100AxisTest() {
        super("lsfileae", SOAP_REQUEST, SOAP_REPLY);
    }
    
    /**
     * @return the web service location.
     */
    protected String getServiceURI() {
        return "http://megamouss:8080/axis2/services/" + getWsName() + "Service." + getWsName() + "Port/";
    }
}
