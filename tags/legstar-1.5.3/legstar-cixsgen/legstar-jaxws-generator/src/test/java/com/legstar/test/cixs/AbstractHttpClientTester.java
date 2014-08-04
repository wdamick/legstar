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

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collection;
import java.util.Properties;

import junit.framework.TestCase;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpConnectionManager;
import org.apache.commons.httpclient.MultiThreadedHttpConnectionManager;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.StringRequestEntity;
import org.apache.commons.httpclient.params.HttpConnectionManagerParams;
import org.apache.commons.io.FileUtils;
import org.codehaus.cargo.container.InstalledLocalContainer;
import org.codehaus.cargo.container.deployable.WAR;
import org.codehaus.cargo.container.tomcat.Tomcat6xInstalledLocalContainer;
import org.codehaus.cargo.container.tomcat.Tomcat6xStandaloneLocalConfiguration;

/**
 * Tests a using a raw HTTP Client to call a Web Service (either proxy or
 * Adapter).
 * 
 */
public abstract class AbstractHttpClientTester extends TestCase {

    /** A multi-thread client used throughout the test. */
    private static final HttpClient HTTPCLIENT = createHttpClient();

    /** Maximum number of connections allowed overall. */
    public static final int MAX_TOTAL_CONNECTIONS = 25;

    /** Default maximum number of connections allowed for a given host config. */
    public static final int DEFAULT_MAX_CONNECTIONS_PER_HOST = 5;

    /**
     * Timeout until a connection is established. A value of zero means the
     * timeout is not used. The default value is zero.
     */
    public static final int CONNECT_TIMEOUT = 5000;

    /**
     * Socket timeout (<tt>SO_TIMEOUT</tt>) in milliseconds which is the timeout
     * for waiting for data.
     */
    public static final int SOCKET_TIMEOUT = 5000;

    /** Disable Nagle's algorithm (that is enable TCP_NODELAY). */
    public static final boolean TCP_NO_DELAY = true;

    /** Close connections which are idle for longer than this. */
    public static final int IDLE_CONNECTION_TIMEOUT = 1000;

    /** A Tomcat instance. */
    private InstalledLocalContainer _webapp;

    /**
     * Setup Tomcat.
     */
    public AbstractHttpClientTester() {
    }

    /**
     * Start a Tomcat instance.
     * */
    public void setUp() throws Exception {
        _webapp = getContainer();
        _webapp.start();
    }

    /**
     * Stop the Tomcat instance.
     * */
    public void tearDown() throws Exception {
        _webapp.stop();
    }

    /**
     * Setup the static http connection pool.
     * 
     * @return an http client instance
     */
    protected static HttpClient createHttpClient() {

        HttpConnectionManagerParams connectionManagerParams = new HttpConnectionManagerParams();

        connectionManagerParams.setMaxTotalConnections(MAX_TOTAL_CONNECTIONS);
        connectionManagerParams
                .setDefaultMaxConnectionsPerHost(DEFAULT_MAX_CONNECTIONS_PER_HOST);
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
     * 
     * @param url the target url
     * @param xmlRequest the XML request
     * @param soapAction the soap action
     * @return the XML reply
     * @throws Exception if something goes wrong
     */
    public String postXml(final String url, final String xmlRequest,
            final String soapAction) throws Exception {
        PostMethod postMethod = new PostMethod(url);
        if (soapAction != null && soapAction.length() > 0) {
            postMethod.setRequestHeader("SOAPAction", soapAction);
        }
        StringRequestEntity requestEntity = new StringRequestEntity(xmlRequest,
                "text/xml", "utf-8");
        postMethod.setRequestEntity(requestEntity);
        int rc = HTTPCLIENT.executeMethod(postMethod);
        String xmlReply = getResponseBodyAsString(
                postMethod.getResponseBodyAsStream(),
                postMethod.getResponseCharSet());
        postMethod.releaseConnection();
        assertEquals(postMethod.getStatusText() + " " + xmlReply, 200, rc);
        return xmlReply;
    }

    /**
     * Perform a request/reply using byte[] application/octet-stream as payload.
     * 
     * @param url the target url
     * @param bytesRequest the bytes request
     * @return the bytes reply
     * @throws Exception if something goes wrong
     */
    public byte[] postBytes(final String url, final byte[] bytesRequest)
            throws Exception {
        PostMethod postMethod = new PostMethod(url);
        ByteArrayRequestEntity requestEntity = new ByteArrayRequestEntity(
                bytesRequest, "application/octet-stream");
        postMethod.setRequestEntity(requestEntity);
        int rc = HTTPCLIENT.executeMethod(postMethod);
        assertEquals(postMethod.getStatusText(), 200, rc);
        byte[] bytesReply = postMethod.getResponseBody();
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
     * @return the J2EE server host name. This can come from a properties file
     *         or will default to the local machine ip address.
     */
    protected static String findJ2EEHost() {
        String defaultJ2EEHost = getLocalIPAddress();
        InputStream stream = AbstractHttpClientTester.class
                .getResourceAsStream("/devenv.properties");
        if (stream != null) {
            Properties props = new Properties();
            try {
                props.load(stream);
                return props.getProperty("legstar-dev-tomcat-host",
                        defaultJ2EEHost);
            } catch (IOException e) {
                return defaultJ2EEHost;
            }
        } else {
            return defaultJ2EEHost;
        }

    }

    /**
     * Assuming the local machine is running JBoss ESB.
     * 
     * @return the local machine IP address
     */
    protected static String getLocalIPAddress() {
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
     * The HttpClient getResponseBodyAsString returns a systematic warning when
     * the response has no content-length.
     * 
     * @param is the http response as a stream
     * @param charset the response character set
     * @return the result string
     */
    protected String getResponseBodyAsString(final InputStream is,
            final String charset) {
        try {
            Reader reader = new BufferedReader(new InputStreamReader(is,
                    charset));
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

    /**
     * Start the web container call the web service, check then stop the web
     * container.
     */
    public void performRoundTrip() {
        try {
            callServiceAndCheck();
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.toString());
        }
    }

    /**
     * Call the Web service and check results.
     */
    public abstract void callServiceAndCheck() throws Exception;

    /**
     * Create a Cargo web container deploying a war in it. All dependencies go
     * to shared library.
     * 
     * @return a container ready to start
     */
    @SuppressWarnings("unchecked")
    public InstalledLocalContainer getContainer() {
        Tomcat6xStandaloneLocalConfiguration configuration = new Tomcat6xStandaloneLocalConfiguration(
                "target/tomcat6x");
        configuration.addDeployable(new WAR("target/war/legstar-engine.war"));
        for (String deployable : getDeployables()) {
            configuration.addDeployable(new WAR(deployable));
        }
        Tomcat6xInstalledLocalContainer webapp = new Tomcat6xInstalledLocalContainer(
                configuration);
        Collection < File > jarFiles = FileUtils.listFiles(new File(
                "target/dependency/lib"), new String[] { "jar" }, false);
        for (File jarFile : jarFiles) {
            webapp.addSharedClasspath(jarFile.getAbsolutePath());
        }
        webapp.addSharedClasspath(new File("src/test/resources")
                .getAbsolutePath());
        webapp.setHome(System.getenv("CATALINA_HOME"));
        webapp.setOutput("target/cargo" + getClass().getSimpleName()
                + getName() + ".log");
        return webapp;
    }

    /**
     * Get the war archives that needs to be deployed for an integration test.
     * Order is important to avoid class loading issues.
     * 
     * @return
     */
    public abstract String[] getDeployables();
}
