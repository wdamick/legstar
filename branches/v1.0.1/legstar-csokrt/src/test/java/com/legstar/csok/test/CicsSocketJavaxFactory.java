/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
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

/** Low level tests to investigate delayed ack issue with TS 3.1. */
public class CicsSocketJavaxFactory extends TestCase {
	
	private static final String HOST = "192.168.0.110";
	private static final int PORT = 4080;
	private static final int CONNECT_TIMEOUT = 2000;
	
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
	
	private byte[] getHttpPayload() {
		StringBuilder sb = new StringBuilder();
		sb.append("POST /CICS/CWBA/LSWEBBIN HTTP/1.1" + "\r\n");
		sb.append("CICSProgram: LSFILEAE" + "\r\n");
		sb.append("CICSLength: 79" + "\r\n");
		sb.append("CICSDataLength: 6" + "\r\n");
		sb.append("Authorization: Basic UDM5MDpTVFJFQU0y" + "\r\n");
		sb.append("User-Agent: Jakarta Commons-HttpClient/3.0.1" + "\r\n");
		sb.append("Host: 192.168.0.110:4080" + "\r\n");
		sb.append("Content-Length: 6" + "\r\n");
		sb.append("Content-Type: binary/octet-stream" + "\r\n");
		sb.append("\r\n\r\n");
		sb.append("נננסננ");
		return sb.toString().getBytes();
	}
}
