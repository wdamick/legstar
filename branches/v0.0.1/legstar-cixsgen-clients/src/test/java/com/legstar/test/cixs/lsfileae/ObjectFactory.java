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

package com.legstar.test.cixs.lsfileae;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileae package. 
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

    private final static QName _LsfileaeRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileae", "LsfileaeRequest");
    private final static QName _LsfileaeResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileae", "LsfileaeResponse");
    private final static QName _LsfileaeFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileae", "LsfileaeFaultInfo");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileae", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileae
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileaeFaultInfo }
     * 
     */
    public LsfileaeFaultInfo createLsfileaeFaultInfo() {
        return new LsfileaeFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileaeResponse }
     * 
     */
    public LsfileaeResponse createLsfileaeResponse() {
        return new LsfileaeResponse();
    }

    /**
     * Create an instance of {@link LsfileaeHostHeader }
     * 
     */
    public LsfileaeHostHeader createLsfileaeHostHeader() {
        return new LsfileaeHostHeader();
    }

    /**
     * Create an instance of {@link LsfileaeRequest }
     * 
     */
    public LsfileaeRequest createLsfileaeRequest() {
        return new LsfileaeRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileae", name = "LsfileaeRequest")
    public JAXBElement<LsfileaeRequest> createLsfileaeRequest(LsfileaeRequest value) {
        return new JAXBElement<LsfileaeRequest>(_LsfileaeRequest_QNAME, LsfileaeRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileae", name = "LsfileaeResponse")
    public JAXBElement<LsfileaeResponse> createLsfileaeResponse(LsfileaeResponse value) {
        return new JAXBElement<LsfileaeResponse>(_LsfileaeResponse_QNAME, LsfileaeResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileae", name = "LsfileaeFaultInfo")
    public JAXBElement<LsfileaeFaultInfo> createLsfileaeFaultInfo(LsfileaeFaultInfo value) {
        return new JAXBElement<LsfileaeFaultInfo>(_LsfileaeFaultInfo_QNAME, LsfileaeFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileae", name = "HostHeader")
    public JAXBElement<LsfileaeHostHeader> createHostHeader(LsfileaeHostHeader value) {
        return new JAXBElement<LsfileaeHostHeader>(_HostHeader_QNAME, LsfileaeHostHeader.class, null, value);
    }

}
