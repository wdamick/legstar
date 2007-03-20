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

package com.legstar.test.cixs.binarcht;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binarcht package. 
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

    private final static QName _BinarchtResponse_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtResponse");
    private final static QName _BinarchtRequest_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtRequest");
    private final static QName _BinarchtFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtFaultInfo");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binarcht
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinarchtFaultInfo }
     * 
     */
    public BinarchtFaultInfo createBinarchtFaultInfo() {
        return new BinarchtFaultInfo();
    }

    /**
     * Create an instance of {@link BinarchtHostHeader }
     * 
     */
    public BinarchtHostHeader createBinarchtHostHeader() {
        return new BinarchtHostHeader();
    }

    /**
     * Create an instance of {@link BinarchtResponse }
     * 
     */
    public BinarchtResponse createBinarchtResponse() {
        return new BinarchtResponse();
    }

    /**
     * Create an instance of {@link BinarchtRequest }
     * 
     */
    public BinarchtRequest createBinarchtRequest() {
        return new BinarchtRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtResponse")
    public JAXBElement<BinarchtResponse> createBinarchtResponse(BinarchtResponse value) {
        return new JAXBElement<BinarchtResponse>(_BinarchtResponse_QNAME, BinarchtResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtRequest")
    public JAXBElement<BinarchtRequest> createBinarchtRequest(BinarchtRequest value) {
        return new JAXBElement<BinarchtRequest>(_BinarchtRequest_QNAME, BinarchtRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtFaultInfo")
    public JAXBElement<BinarchtFaultInfo> createBinarchtFaultInfo(BinarchtFaultInfo value) {
        return new JAXBElement<BinarchtFaultInfo>(_BinarchtFaultInfo_QNAME, BinarchtFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "HostHeader")
    public JAXBElement<BinarchtHostHeader> createHostHeader(BinarchtHostHeader value) {
        return new JAXBElement<BinarchtHostHeader>(_HostHeader_QNAME, BinarchtHostHeader.class, null, value);
    }

}
