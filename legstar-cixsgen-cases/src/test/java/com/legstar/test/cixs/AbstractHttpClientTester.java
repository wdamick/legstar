package com.legstar.test.cixs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;

import junit.framework.TestCase;

/**
 * Tests a using a raw HTTP Client.
 *
 */
public abstract class AbstractHttpClientTester extends TestCase {
    
    /** A multithreaded client used throughout the test.*/
    private final HttpClient _httpClient;
    
    /**
     * Use multithreaded client (useful for load testing).
     * Make sure connections do not stay in XLOSE_WAIT too long.
     */
    public AbstractHttpClientTester() {
        MultiThreadedHttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.closeIdleConnections(1000);
        _httpClient = new HttpClient(connectionManager);
    }

    /**
     * Perform a request/reply using text/xml as payload.
     * @param url the target url
     * @param xmlRequest the XML request
     * @return the XML reply
     * @throws Exception if something goes wrong
     */
    public String postXml(final String url, final String xmlRequest) throws Exception {
        PostMethod postMethod = new PostMethod(url);
        StringRequestEntity requestEntity =
            new StringRequestEntity(xmlRequest, "text/xml", "utf-8");
        postMethod.setRequestEntity(requestEntity);
        int rc = _httpClient.executeMethod(postMethod);
        assertEquals(200, rc);
        String xmlReply =  getResponseBodyAsString(
                postMethod.getResponseBodyAsStream(),
                postMethod.getResponseCharSet());
        postMethod.releaseConnection();
        return xmlReply;
    }
    
    /**
     * Perform a request/reply using byte[] application/octet-stream as payload.
     * @param url the target url
     * @param bytesRequest the bytes request
     * @return the bytes reply
     * @throws Exception if something goes wrong
     */
    public byte[] postBytes(final String url, final byte[] bytesRequest) throws Exception {
        PostMethod postMethod = new PostMethod(url);
        ByteArrayRequestEntity requestEntity = new ByteArrayRequestEntity(
                bytesRequest, "application/octet-stream");
        postMethod.setRequestEntity(requestEntity);
        int rc = _httpClient.executeMethod(postMethod);
        assertEquals(200, rc);
        byte[] bytesReply =  postMethod.getResponseBody();
        postMethod.releaseConnection();
        return bytesReply;
    }

    /**
     * The HttpClient getResponseBodyAsString returns a systematic warning when the response has
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
