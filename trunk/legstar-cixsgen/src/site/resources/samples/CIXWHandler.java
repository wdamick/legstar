/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.coxb;

import java.io.IOException;
import java.util.Set;
import java.util.Iterator;

import javax.xml.namespace.QName;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPHeader;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPBody;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.handler.soap.SOAPHandler;
import javax.xml.ws.handler.soap.SOAPMessageContext;
import org.w3c.dom.Attr;

import com.legstar.cixs.http.CICWHost;

/**
 * This is a JAXWS protocol handler. It can be replaced by customers. Its role
 * is to collect host connectivity parameters and pass them on, thru the
 * message context to the invoker.
 *
 * @author Fady Moussallam
 * 
 */
public class CIXWHandler  implements SOAPHandler < SOAPMessageContext > {
    
	/** The Host header XML tag. */
	private static final String  HOST_HEADER_E = "HostHeader";
	
	/** The Host user ID XML tag. */
	private static final String  HOST_USER_E = "hostUser";
	
	/** The Host password XML tag. */
	private static final String  HOST_PWD_E = "hostPassword";
	
	/** The Host TCPIP address XML tag. */
	private static final String  HOST_ADDRESS_E = "hostIPAddress";
	
	/** The Host TCPIP port number XML tag. */
	private static final String  HOST_PORT_E = "hostIPPort";
	
	/** The Host path to the HTTP server XML tag. */
	private static final String  HOST_PATH_E = "hostCICWPath";
	
	/** The generic host connectivity parameters properties file. */
	private static final String  HOST_PROP_FILE = "hostconnection.properties";
	
	/** Describes the CICS host system. */
	private CICWHost mHost;

    
	/** Construct handler. Reads a property file to get default host connection
	 * parameters if the client did not set any.
	 */
	public CIXWHandler() {
		
		try {
			mHost = new CICWHost(HOST_PROP_FILE);
		} catch (IOException e) {
			e.printStackTrace();
			throw (new RuntimeException(e));
		}
	}
	
	/** {@inheritDoc} */
	public final Set < QName > getHeaders() {
        return null;
    }
    
	/** {@inheritDoc} */
   public final boolean handleMessage(final SOAPMessageContext smc) {
    	updateSOAPHeader(smc);
        return true;
    }
    
	/** {@inheritDoc} */
   public final boolean handleFault(final SOAPMessageContext smc) {
        return true;
    }
    
	/** {@inheritDoc} */
   public final void close(final MessageContext messageContext) {
    }
    
    /**
     * Search fot HostHeader elements in the SOAP header. If not found or if
     * values were not set, provide values from properties files.
     * 
     * @param smc the SOAP message context
     */
    private void updateSOAPHeader(final SOAPMessageContext smc) {
        
    	/* If no default properties file was provided, nothing useful can
    	 * be done in this handler. */
    	if (mHost == null) {
    		return;
    	}
    	
    	/* Get current direction */
    	Boolean outboundProperty = (Boolean)
            smc.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY);
        
        /* Only process inbound messages */
        if (outboundProperty.booleanValue()) {
        	return;
        }
        	
        SOAPMessage message = smc.getMessage();
        
		try {
			SOAPElement hostHeader;
			
	    	/* Assume SOAP header elements have same namespace and prefix as
	    	 * the request wrapper in the SOAP body. */
			QName wrapperQName = retrieveQName(message);
			
	    	QName hostHeaderQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_HEADER_E, wrapperQName.getPrefix());
	    	QName hostUserQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_USER_E, wrapperQName.getPrefix());
	    	QName hostPasswordQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_PWD_E, wrapperQName.getPrefix());
	    	QName hostAddressQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_ADDRESS_E, wrapperQName.getPrefix());
	    	QName hostPortQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_PORT_E, wrapperQName.getPrefix());
	    	QName hostPathQName = new QName(wrapperQName.getNamespaceURI(),
	    			HOST_PATH_E, wrapperQName.getPrefix());
	    	
	        /* See if a host header is already there */
	    	SOAPHeader soapHeader = message.getSOAPHeader();
	        Iterator hhi = soapHeader.getChildElements(hostHeaderQName);
	        
	        /* If no Host header is there, add one */
	        if (hhi == null || !hhi.hasNext()) {
	        	hostHeader = soapHeader.addChildElement(hostHeaderQName);
	        } else {
	        	hostHeader = (SOAPElement) hhi.next();
	        	/* If HostHeader is marked as nil, change that otherwise
	        	 * implementation will not get the header */
	        	if (hostHeader.getAttribute("xsi:nil").compareTo("true") == 0) {
	        		Attr nilAttr = hostHeader.getAttributeNode("xsi:nil");
	        		nilAttr.setValue("false");
	        	}
	        }
	        
	        /* Set the host user ID */
	        setElement(hostHeader, hostUserQName, mHost.getHostUser());
	        	
	        /* Set the host password */
	        setElement(hostHeader, hostPasswordQName, mHost.getHostPassword());
	        	
	        /* Set the host address */
	        setElement(hostHeader, hostAddressQName, mHost.getHostIPAddress());
	        
	        /* Set the host port */
	        setElement(hostHeader, hostPortQName,
	        		(new Integer(mHost.getHostIPPort())).toString());
	        
	        /* Set the host path */
	        setElement(hostHeader, hostPathQName, mHost.getHostCICWPath());
	        
		} catch (SOAPException e) {
			e.printStackTrace();
			throw (new RuntimeException(e));
		}
        
    }
    
    /**
     * Search for the qualified name used for the request wrapper. 
     * @param message the soap message
     * @return the qualified name used for the wrapper element
     * @throws SOAPException if namespace cannot be recovered
     */
    private QName retrieveQName(
    		final SOAPMessage message) throws SOAPException {

    	SOAPBody soapBody = message.getSOAPBody();
    	Iterator be = soapBody.getChildElements();
    	if (be == null || !be.hasNext()) {
    		throw (new RuntimeException("SOAP request body is empty."));
    	}
    	SOAPElement subElement = (SOAPElement) be.next();
    	return subElement.getElementQName();
    }
    
    /**
     * Ensure a child element exists and is set to a default value if client
     * did not already set a value.
     * 
     * @param parent parent element
     * @param childQN qualified name of child
     * @param value default value to set child to
     * @throws SOAPException if child element cannot be set
     */
    private void setElement(
    		final SOAPElement parent,
    		final QName childQN,
    		final String value) throws SOAPException {
    	
    	SOAPElement child;
    	
    	/* Look for the requested qualified name in parent element */
        Iterator hi = parent.getChildElements(childQN);	

        /* If no such element is there, add one. */
        if (hi == null || !hi.hasNext()) {
        	child = parent.addChildElement(childQN);
        } else {
        	child = (SOAPElement) hi.next();
        }
        /* If element value is not already set by client, provide default 
         * value */
        if (child.getValue() == null
        		|| child.getValue().length() == 0
        		|| child.getValue().compareTo("0") == 0) {
        	child.setValue(value);
        }
    	
    }
}
