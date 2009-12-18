package com.legstar.test.cixs;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Properties;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.params.HttpConnectionManagerParams;

import junit.framework.TestCase;

/**
 * Tests a using a raw HTTP Client.
 *
 */
public abstract class AbstractHttpClientTester extends TestCase {

    /** A multi-thread client used throughout the test.*/
    private static final HttpClient HTTPCLIENT = createHttpClient();
    
    /** Maximum number of connections allowed overall. */
    public static final int MAX_TOTAL_CONNECTIONS = 25;
 
    /** Default maximum number of connections allowed for a given host config. */
    public static final int DEFAULT_MAX_CONNECTIONS_PER_HOST = 5;
    
    /** Timeout until a connection is established. A value of zero 
     * means the timeout is not used. The default value is zero.*/
    public static final int CONNECT_TIMEOUT = 5000;
    
    /** Socket timeout (<tt>SO_TIMEOUT</tt>) in milliseconds which is the 
     * timeout for waiting for data.*/
    public static final int SOCKET_TIMEOUT = 5000;
    
    /** Disable Nagle's algorithm (that is enable TCP_NODELAY). */
    public static final boolean TCP_NO_DELAY = true;

    /** Close connections which are idle for longer than this. */
    public static final int IDLE_CONNECTION_TIMEOUT = 1000;

    /**
     * Setup the static http connection pool.
     * @return an http client instance
     */
    private static HttpClient createHttpClient() { 

        HttpConnectionManagerParams connectionManagerParams = new HttpConnectionManagerParams();

        connectionManagerParams.setMaxTotalConnections(MAX_TOTAL_CONNECTIONS);
        connectionManagerParams.setDefaultMaxConnectionsPerHost(DEFAULT_MAX_CONNECTIONS_PER_HOST);
        connectionManagerParams.setTcpNoDelay(TCP_NO_DELAY);
        connectionManagerParams.setSoTimeout(SOCKET_TIMEOUT);
        connectionManagerParams.setConnectionTimeout(CONNECT_TIMEOUT);

        HttpConnectionManager connectionManager = new MultiThreadedHttpConnectionManager();
        connectionManager.setParams(connectionManagerParams);
        connectionManager.closeIdleConnections(IDLE_CONNECTION_TIMEOUT);
        return new HttpClient(connectionManager);
    }

    /**
     * Perform a request/reply using text/xml as payload.
     * @param url the target url
     * @param xmlRequest the XML request
     * @param soapAction the soap action
     * @return the XML reply
     * @throws Exception if something goes wrong
     */
    public String postXml(final String url, final String xmlRequest, final String soapAction) throws Exception {
        PostMethod postMethod = new PostMethod(url);
        if (soapAction != null && soapAction.length() > 0) {
            postMethod.setRequestHeader("SOAPAction", soapAction);
        }
        StringRequestEntity requestEntity =
            new StringRequestEntity(xmlRequest, "text/xml", "utf-8");
        postMethod.setRequestEntity(requestEntity);
        int rc = HTTPCLIENT.executeMethod(postMethod);
        assertEquals(postMethod.getStatusText(), 200, rc);
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
        int rc = HTTPCLIENT.executeMethod(postMethod);
        assertEquals(postMethod.getStatusText(), 200, rc);
        byte[] bytesReply =  postMethod.getResponseBody();
        postMethod.releaseConnection();
        return bytesReply;
    }

    /**
     * @return the J2EE server host.
     */
    public String getJ2EEHost() {
        return findJ2EEHost();
    }

    /**
     * 
     * @return the J2EE server host name. This can come from a properties
     * file or will default to the local machine ip address.
     */
    private static String findJ2EEHost() {
        String defaultJ2EEHost = getLocalIPAddress();
        InputStream stream = AbstractHttpClientTester.class.getResourceAsStream(
                "/devenv.properties");
        if (stream != null) {
            Properties props = new Properties();
            try {
                props.load(stream);
                return props.getProperty("legstar-dev-tomcat-host", defaultJ2EEHost);
            } catch (IOException e) {
                return defaultJ2EEHost;
            }
        } else {
            return defaultJ2EEHost;
        }
        
    }

    /**
     * Assuming the local machine is running JBoss ESB.
     * @return the local machine IP address
     */
    private static String getLocalIPAddress() {
        try {
            InetAddress addr = InetAddress.getLocalHost();
            byte[] ipAddr = addr.getAddress();
            String ipAddrStr = "";
            for (int i = 0; i < ipAddr.length; i++) {
                if (i > 0) {
                    ipAddrStr += ".";
                }
                ipAddrStr += ipAddr[i] & 0xFF;
            }
            return ipAddrStr;
        } catch (UnknownHostException e) {
            return "";
        }

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
