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

package com.legstar.test.cixs.typesmix;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.typesmix package. 
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

    private final static QName _TypesmixFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/typesmix", "TypesmixFaultInfo");
    private final static QName _TypesmixRequest_QNAME = new QName("http://cixs.test.legstar.com/typesmix", "TypesmixRequest");
    private final static QName _TypesmixResponse_QNAME = new QName("http://cixs.test.legstar.com/typesmix", "TypesmixResponse");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/typesmix", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.typesmix
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link TypesmixResponse }
     * 
     */
    public TypesmixResponse createTypesmixResponse() {
        return new TypesmixResponse();
    }

    /**
     * Create an instance of {@link TypesmixFaultInfo }
     * 
     */
    public TypesmixFaultInfo createTypesmixFaultInfo() {
        return new TypesmixFaultInfo();
    }

    /**
     * Create an instance of {@link TypesmixRequest }
     * 
     */
    public TypesmixRequest createTypesmixRequest() {
        return new TypesmixRequest();
    }

    /**
     * Create an instance of {@link TypesmixHostHeader }
     * 
     */
    public TypesmixHostHeader createTypesmixHostHeader() {
        return new TypesmixHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TypesmixFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/typesmix", name = "TypesmixFaultInfo")
    public JAXBElement<TypesmixFaultInfo> createTypesmixFaultInfo(TypesmixFaultInfo value) {
        return new JAXBElement<TypesmixFaultInfo>(_TypesmixFaultInfo_QNAME, TypesmixFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TypesmixRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/typesmix", name = "TypesmixRequest")
    public JAXBElement<TypesmixRequest> createTypesmixRequest(TypesmixRequest value) {
        return new JAXBElement<TypesmixRequest>(_TypesmixRequest_QNAME, TypesmixRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TypesmixResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/typesmix", name = "TypesmixResponse")
    public JAXBElement<TypesmixResponse> createTypesmixResponse(TypesmixResponse value) {
        return new JAXBElement<TypesmixResponse>(_TypesmixResponse_QNAME, TypesmixResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link TypesmixHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/typesmix", name = "HostHeader")
    public JAXBElement<TypesmixHostHeader> createHostHeader(TypesmixHostHeader value) {
        return new JAXBElement<TypesmixHostHeader>(_HostHeader_QNAME, TypesmixHostHeader.class, null, value);
    }

}
