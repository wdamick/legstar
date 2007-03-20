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

package com.legstar.test.cixs.redbotha;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redbotha package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _RedbothaRequest_QNAME = new QName("http://cixs.test.legstar.com/redbotha", "RedbothaRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/redbotha", "HostHeader");
    private final static QName _RedbothaResponse_QNAME = new QName("http://cixs.test.legstar.com/redbotha", "RedbothaResponse");
    private final static QName _RedbothaFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redbotha", "RedbothaFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redbotha
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedbothaFaultInfo }
     * 
     */
    public RedbothaFaultInfo createRedbothaFaultInfo() {
        return new RedbothaFaultInfo();
    }

    /**
     * Create an instance of {@link RedbothaHostHeader }
     * 
     */
    public RedbothaHostHeader createRedbothaHostHeader() {
        return new RedbothaHostHeader();
    }

    /**
     * Create an instance of {@link RedbothaResponse }
     * 
     */
    public RedbothaResponse createRedbothaResponse() {
        return new RedbothaResponse();
    }

    /**
     * Create an instance of {@link RedbothaRequest }
     * 
     */
    public RedbothaRequest createRedbothaRequest() {
        return new RedbothaRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedbothaRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redbotha", name = "RedbothaRequest")
    public JAXBElement<RedbothaRequest> createRedbothaRequest(RedbothaRequest value) {
        return new JAXBElement<RedbothaRequest>(_RedbothaRequest_QNAME, RedbothaRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedbothaHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redbotha", name = "HostHeader")
    public JAXBElement<RedbothaHostHeader> createHostHeader(RedbothaHostHeader value) {
        return new JAXBElement<RedbothaHostHeader>(_HostHeader_QNAME, RedbothaHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedbothaResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redbotha", name = "RedbothaResponse")
    public JAXBElement<RedbothaResponse> createRedbothaResponse(RedbothaResponse value) {
        return new JAXBElement<RedbothaResponse>(_RedbothaResponse_QNAME, RedbothaResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedbothaFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redbotha", name = "RedbothaFaultInfo")
    public JAXBElement<RedbothaFaultInfo> createRedbothaFaultInfo(RedbothaFaultInfo value) {
        return new JAXBElement<RedbothaFaultInfo>(_RedbothaFaultInfo_QNAME, RedbothaFaultInfo.class, null, value);
    }

}
