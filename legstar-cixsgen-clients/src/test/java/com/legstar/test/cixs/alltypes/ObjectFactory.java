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

package com.legstar.test.cixs.alltypes;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.alltypes package. 
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

    private final static QName _AlltypesFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/alltypes", "AlltypesFaultInfo");
    private final static QName _AlltypesRequest_QNAME = new QName("http://cixs.test.legstar.com/alltypes", "AlltypesRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/alltypes", "HostHeader");
    private final static QName _AlltypesResponse_QNAME = new QName("http://cixs.test.legstar.com/alltypes", "AlltypesResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.alltypes
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link AlltypesResponse }
     * 
     */
    public AlltypesResponse createAlltypesResponse() {
        return new AlltypesResponse();
    }

    /**
     * Create an instance of {@link AlltypesRequest }
     * 
     */
    public AlltypesRequest createAlltypesRequest() {
        return new AlltypesRequest();
    }

    /**
     * Create an instance of {@link AlltypesHostHeader }
     * 
     */
    public AlltypesHostHeader createAlltypesHostHeader() {
        return new AlltypesHostHeader();
    }

    /**
     * Create an instance of {@link AlltypesFaultInfo }
     * 
     */
    public AlltypesFaultInfo createAlltypesFaultInfo() {
        return new AlltypesFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AlltypesFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/alltypes", name = "AlltypesFaultInfo")
    public JAXBElement<AlltypesFaultInfo> createAlltypesFaultInfo(AlltypesFaultInfo value) {
        return new JAXBElement<AlltypesFaultInfo>(_AlltypesFaultInfo_QNAME, AlltypesFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AlltypesRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/alltypes", name = "AlltypesRequest")
    public JAXBElement<AlltypesRequest> createAlltypesRequest(AlltypesRequest value) {
        return new JAXBElement<AlltypesRequest>(_AlltypesRequest_QNAME, AlltypesRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AlltypesHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/alltypes", name = "HostHeader")
    public JAXBElement<AlltypesHostHeader> createHostHeader(AlltypesHostHeader value) {
        return new JAXBElement<AlltypesHostHeader>(_HostHeader_QNAME, AlltypesHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AlltypesResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/alltypes", name = "AlltypesResponse")
    public JAXBElement<AlltypesResponse> createAlltypesResponse(AlltypesResponse value) {
        return new JAXBElement<AlltypesResponse>(_AlltypesResponse_QNAME, AlltypesResponse.class, null, value);
    }

}
