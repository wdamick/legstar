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

package com.legstar.test.cixs.binnatsi;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binnatsi package. 
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

    private final static QName _BinnatsiRequest_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiRequest");
    private final static QName _BinnatsiFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiFaultInfo");
    private final static QName _BinnatsiResponse_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiResponse");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binnatsi
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinnatsiResponse }
     * 
     */
    public BinnatsiResponse createBinnatsiResponse() {
        return new BinnatsiResponse();
    }

    /**
     * Create an instance of {@link BinnatsiFaultInfo }
     * 
     */
    public BinnatsiFaultInfo createBinnatsiFaultInfo() {
        return new BinnatsiFaultInfo();
    }

    /**
     * Create an instance of {@link BinnatsiRequest }
     * 
     */
    public BinnatsiRequest createBinnatsiRequest() {
        return new BinnatsiRequest();
    }

    /**
     * Create an instance of {@link BinnatsiHostHeader }
     * 
     */
    public BinnatsiHostHeader createBinnatsiHostHeader() {
        return new BinnatsiHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiRequest")
    public JAXBElement<BinnatsiRequest> createBinnatsiRequest(BinnatsiRequest value) {
        return new JAXBElement<BinnatsiRequest>(_BinnatsiRequest_QNAME, BinnatsiRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiFaultInfo")
    public JAXBElement<BinnatsiFaultInfo> createBinnatsiFaultInfo(BinnatsiFaultInfo value) {
        return new JAXBElement<BinnatsiFaultInfo>(_BinnatsiFaultInfo_QNAME, BinnatsiFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiResponse")
    public JAXBElement<BinnatsiResponse> createBinnatsiResponse(BinnatsiResponse value) {
        return new JAXBElement<BinnatsiResponse>(_BinnatsiResponse_QNAME, BinnatsiResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "HostHeader")
    public JAXBElement<BinnatsiHostHeader> createHostHeader(BinnatsiHostHeader value) {
        return new JAXBElement<BinnatsiHostHeader>(_HostHeader_QNAME, BinnatsiHostHeader.class, null, value);
    }

}
