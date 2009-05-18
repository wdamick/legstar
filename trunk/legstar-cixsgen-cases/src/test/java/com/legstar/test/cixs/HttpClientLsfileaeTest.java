package com.legstar.test.cixs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;

import junit.framework.TestCase;

/**
 * Tests a Web Service adapter using a raw HTTP Client. This reduces the overhead as
 * opposed to using a JAX-WS client.
 *
 */
public class HttpClientLsfileaeTest extends TestCase {

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
        + "<ns3:ComNumber>100</ns3:ComNumber>"
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
        + "<ns3:ComNumber>100</ns3:ComNumber>"
        + "<ns3:ComPersonal>"
        + "<ns3:ComName>S. D. BORMAN</ns3:ComName>"
        + "<ns3:ComAddress>SURREY, ENGLAND</ns3:ComAddress>"
        + "<ns3:ComPhone>32156778</ns3:ComPhone>"
        + "</ns3:ComPersonal>"
        + "<ns3:ComDate>26 11 81</ns3:ComDate>"
        + "<ns3:ComAmount>$0100.11</ns3:ComAmount>"
        + "<ns3:ComComment>*********</ns3:ComComment>"
        + "</ns2:Response>"
        + "</ns2:LsfileaeResponse>"
        + "</S:Body>"
        + "</S:Envelope>";
    
    /**
     * CICSTS31 Direct Http.
     */
    public void testRoundTripCICSTS31DirectHttp() {
        testRoundTrip("CICSTS31DirectHttp");
    }
    
    /**
     * CICSTS31 Pooled Http.
     */
    public void testRoundTripCICSTS31PooledHttp() {
        testRoundTrip("CICSTS31PooledHttp");
    }

    /**
     * CICSTS23 Direct Http.
     */
    public void testRoundTripCICSTS23DirectHttp() {
        testRoundTrip("CICSTS23DirectHttp");
    }
    
    /**
     * CICSTS23 Pooled Http.
     */
    public void testRoundTripCICSTS23PooledHttp() {
        testRoundTrip("CICSTS23PooledHttp");
    }

    /**
     * CICSTS23 Direct Socket.
     */
    public void testRoundTripCICSTS23DirectSocket() {
        testRoundTrip("CICSTS23DirectSocket");
    }
    
    /**
     * CICSTS23 Pooled Socket.
     */
    public void testRoundTripCICSTS23PooledSocket() {
        testRoundTrip("CICSTS23PooledSocket");
    }

    /**
     * CICSTS23 Direct MQ.
     */
    public void testRoundTripCICSTS23DirectMQ() {
        testRoundTrip("CICSTS23DirectMQ");
    }
    
    /**
     * CICSTS23 Pooled MQ.
     */
    public void testRoundTripCICSTS23PooledMQ() {
        testRoundTrip("CICSTS23PooledMQ");
    }

    /**
     * CICSTS31 Direct MQ.
     */
    public void testRoundTripCICSTS31DirectMQ() {
        testRoundTrip("CICSTS31DirectMQ");
    }
    
    /**
     * CICSTS31 Pooled MQ.
     */
    public void testRoundTripCICSTS31PooledMQ() {
        testRoundTrip("CICSTS31PooledMQ");
    }

    /**
     * Perform a complete request and check result.
     * @param endpointName the target mainframe endpoint name
     */
    private void testRoundTrip(final String endpointName) {
        try {
            HttpClient httpClient = new HttpClient();
            PostMethod postMethod = new PostMethod("http://localhost:8080/cixs-lsfileae/lsfileae");
            StringRequestEntity requestEntity =
                new StringRequestEntity(SOAP_REQUEST.replace("${endpointName}", endpointName),
                        "text/xml", "utf-8");
            postMethod.setRequestEntity(requestEntity);
            int rc = httpClient.executeMethod(postMethod);
            assertEquals(200, rc);
            assertEquals(SOAP_REPLY, getResponseBodyAsString(
                    postMethod.getResponseBodyAsStream(),
                    postMethod.getResponseCharSet()));
            postMethod.releaseConnection();
        } catch (UnsupportedEncodingException e) {
            fail(e.toString());
        } catch (HttpException e) {
            fail(e.toString());
        } catch (IOException e) {
            fail(e.toString());
        }
    }
    
    /**
     * The HttpClient getResponseBodyAsString returns a systematic warning when the response hes
     * no content-length.
     * @param is the http response as a stream
     * @param charset the response character set
     * @return the result string
     */
    private String getResponseBodyAsString(final InputStream is, final String charset) {
        try {
            Reader reader = new BufferedReader(new InputStreamReader(is, charset));
            StringBuilder sb = new StringBuilder();
            int ch;
            while ((ch = reader.read()) > -1) {
                sb.append((char) ch);
            }
            is.close();
            return sb.toString();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

}
