/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.csok.test;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.UnknownHostException;

import junit.framework.TestCase;
import javax.net.SocketFactory;
import java.net.InetAddress;
import java.net.InetSocketAddress;

/**
 *  Low level tests to investigate delayed ack issue with TS 3.1 and various socket methods.
 *  Only interesting if running wireshark.
 */
public class CicsSocketJavaxFactory extends TestCase {

    /** Mainframe IP address. */
    private static final String HOST = "mainframe";
    
    /** A CICS TS 3.1 TCPIPSERVICE is expected to listen on this port.*/
    private static final int PORT = 4080;
    
    /** Timeout in millisecs. */
    private static final int CONNECT_TIMEOUT = 2000;

    /**
     * Connect using SocketFactory.
     */
    public void testConnect() {

        SocketFactory sf = SocketFactory.getDefault();
        try {
            Socket socket = sf.createSocket();
            socket.connect(new InetSocketAddress(InetAddress.getByName(HOST), PORT), CONNECT_TIMEOUT);
            socket.close();
        } catch (UnknownHostException e) {
            e.printStackTrace();
            fail("testConnect");
        } catch (IOException e) {
            e.printStackTrace();
            fail("testConnect");
        }
    }

    /**
     * Connect and send/receive using SocketFactory.
     */
    public void testConnectSend() {

        SocketFactory sf = SocketFactory.getDefault();
        try {
            Socket socket = sf.createSocket();
            socket.connect(new InetSocketAddress(InetAddress.getByName(HOST), PORT), CONNECT_TIMEOUT);
            socket.setSoTimeout(5000);
            socket.setTcpNoDelay(false);

            int outbuffersize = socket.getSendBufferSize();
            if ((outbuffersize > 2048) || (outbuffersize <= 0)) {
                outbuffersize = 2048;
            }
            int inbuffersize = socket.getReceiveBufferSize();
            if ((inbuffersize > 2048) || (inbuffersize <= 0)) {
                inbuffersize = 2048;
            }
            InputStream inputStream = new BufferedInputStream(socket.getInputStream(), inbuffersize);
            OutputStream outputStream = new BufferedOutputStream(socket.getOutputStream(), outbuffersize);

            outputStream.write(getHttpPayload());
            outputStream.flush();

            byte[] header = new byte[4096];
            inputStream.read(header);

            byte[] content = new byte[4096];
            inputStream.read(content);

            socket.close();
        } catch (UnknownHostException e) {
            e.printStackTrace();
            fail("testConnect");
        } catch (IOException e) {
            e.printStackTrace();
            fail("testConnect");
        }
    }

    /**
     * Send and receive over plain socket.
     */
    public void testConnectSendPlainSocket() {

        try {
            Socket socket = new Socket();
            socket.setSoTimeout(5000);
            socket.setTcpNoDelay(true);
            socket.connect(new InetSocketAddress(InetAddress.getByName(HOST), PORT), CONNECT_TIMEOUT);

            socket.getOutputStream().write(getHttpPayload());

            byte[] header = new byte[4096];
            socket.getInputStream().read(header);

            byte[] content = new byte[4096];
            socket.getInputStream().read(content);

            socket.close();
        } catch (UnknownHostException e) {
            e.printStackTrace();
            fail("testConnect");
        } catch (IOException e) {
            e.printStackTrace();
            fail("testConnect");
        }
    }

    /**
     * Since we are talking to an HTTP listener, send an http payload.
     * @return an http payload
     */
    private byte[] getHttpPayload() {
        StringBuilder sb = new StringBuilder();
        sb.append("POST /CICS/CWBA/LSWEBBIN HTTP/1.1" + "\r\n");
        sb.append("CICSProgram: LSFILEAE" + "\r\n");
        sb.append("CICSLength: 79" + "\r\n");
        sb.append("CICSDataLength: 6" + "\r\n");
        sb.append("Authorization: Basic UDM5MDpTVFJFQU0y" + "\r\n");
        sb.append("User-Agent: Jakarta Commons-HttpClient/3.0.1" + "\r\n");
        sb.append("Host: mainframe:4080" + "\r\n");
        sb.append("Content-Length: 6" + "\r\n");
        sb.append("Content-Type: binary/octet-stream" + "\r\n");
        sb.append("\r\n\r\n");
        sb.append("נננסננ");
        return sb.toString().getBytes();
    }
}
