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

package com.legstar.test.cixs.redsimpt;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redsimpt package. 
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

    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "HostHeader");
    private final static QName _RedsimptRequest_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptRequest");
    private final static QName _RedsimptResponse_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptResponse");
    private final static QName _RedsimptFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redsimpt
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedsimptResponse }
     * 
     */
    public RedsimptResponse createRedsimptResponse() {
        return new RedsimptResponse();
    }

    /**
     * Create an instance of {@link RedsimptFaultInfo }
     * 
     */
    public RedsimptFaultInfo createRedsimptFaultInfo() {
        return new RedsimptFaultInfo();
    }

    /**
     * Create an instance of {@link RedsimptHostHeader }
     * 
     */
    public RedsimptHostHeader createRedsimptHostHeader() {
        return new RedsimptHostHeader();
    }

    /**
     * Create an instance of {@link RedsimptRequest }
     * 
     */
    public RedsimptRequest createRedsimptRequest() {
        return new RedsimptRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "HostHeader")
    public JAXBElement<RedsimptHostHeader> createHostHeader(RedsimptHostHeader value) {
        return new JAXBElement<RedsimptHostHeader>(_HostHeader_QNAME, RedsimptHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptRequest")
    public JAXBElement<RedsimptRequest> createRedsimptRequest(RedsimptRequest value) {
        return new JAXBElement<RedsimptRequest>(_RedsimptRequest_QNAME, RedsimptRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptResponse")
    public JAXBElement<RedsimptResponse> createRedsimptResponse(RedsimptResponse value) {
        return new JAXBElement<RedsimptResponse>(_RedsimptResponse_QNAME, RedsimptResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptFaultInfo")
    public JAXBElement<RedsimptFaultInfo> createRedsimptFaultInfo(RedsimptFaultInfo value) {
        return new JAXBElement<RedsimptFaultInfo>(_RedsimptFaultInfo_QNAME, RedsimptFaultInfo.class, null, value);
    }

}
