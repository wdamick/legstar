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

package com.legstar.test.cixs.arrayscx;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.arrayscx package. 
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

    private final static QName _ArrayscxRequest_QNAME = new QName("http://cixs.test.legstar.com/arrayscx", "ArrayscxRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/arrayscx", "HostHeader");
    private final static QName _ArrayscxResponse_QNAME = new QName("http://cixs.test.legstar.com/arrayscx", "ArrayscxResponse");
    private final static QName _ArrayscxFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/arrayscx", "ArrayscxFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.arrayscx
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ArrayscxFaultInfo }
     * 
     */
    public ArrayscxFaultInfo createArrayscxFaultInfo() {
        return new ArrayscxFaultInfo();
    }

    /**
     * Create an instance of {@link ArrayscxResponse }
     * 
     */
    public ArrayscxResponse createArrayscxResponse() {
        return new ArrayscxResponse();
    }

    /**
     * Create an instance of {@link ArrayscxRequest }
     * 
     */
    public ArrayscxRequest createArrayscxRequest() {
        return new ArrayscxRequest();
    }

    /**
     * Create an instance of {@link ArrayscxHostHeader }
     * 
     */
    public ArrayscxHostHeader createArrayscxHostHeader() {
        return new ArrayscxHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayscxRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayscx", name = "ArrayscxRequest")
    public JAXBElement<ArrayscxRequest> createArrayscxRequest(ArrayscxRequest value) {
        return new JAXBElement<ArrayscxRequest>(_ArrayscxRequest_QNAME, ArrayscxRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayscxHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayscx", name = "HostHeader")
    public JAXBElement<ArrayscxHostHeader> createHostHeader(ArrayscxHostHeader value) {
        return new JAXBElement<ArrayscxHostHeader>(_HostHeader_QNAME, ArrayscxHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayscxResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayscx", name = "ArrayscxResponse")
    public JAXBElement<ArrayscxResponse> createArrayscxResponse(ArrayscxResponse value) {
        return new JAXBElement<ArrayscxResponse>(_ArrayscxResponse_QNAME, ArrayscxResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayscxFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayscx", name = "ArrayscxFaultInfo")
    public JAXBElement<ArrayscxFaultInfo> createArrayscxFaultInfo(ArrayscxFaultInfo value) {
        return new JAXBElement<ArrayscxFaultInfo>(_ArrayscxFaultInfo_QNAME, ArrayscxFaultInfo.class, null, value);
    }

}
