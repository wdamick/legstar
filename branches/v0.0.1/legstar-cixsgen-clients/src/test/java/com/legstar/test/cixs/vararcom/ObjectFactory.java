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

package com.legstar.test.cixs.vararcom;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.vararcom package. 
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

    private final static QName _VararcomRequest_QNAME = new QName("http://cixs.test.legstar.com/vararcom", "VararcomRequest");
    private final static QName _VararcomFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/vararcom", "VararcomFaultInfo");
    private final static QName _VararcomResponse_QNAME = new QName("http://cixs.test.legstar.com/vararcom", "VararcomResponse");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/vararcom", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.vararcom
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link VararcomHostHeader }
     * 
     */
    public VararcomHostHeader createVararcomHostHeader() {
        return new VararcomHostHeader();
    }

    /**
     * Create an instance of {@link VararcomFaultInfo }
     * 
     */
    public VararcomFaultInfo createVararcomFaultInfo() {
        return new VararcomFaultInfo();
    }

    /**
     * Create an instance of {@link VararcomResponse }
     * 
     */
    public VararcomResponse createVararcomResponse() {
        return new VararcomResponse();
    }

    /**
     * Create an instance of {@link VararcomRequest }
     * 
     */
    public VararcomRequest createVararcomRequest() {
        return new VararcomRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link VararcomRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/vararcom", name = "VararcomRequest")
    public JAXBElement<VararcomRequest> createVararcomRequest(VararcomRequest value) {
        return new JAXBElement<VararcomRequest>(_VararcomRequest_QNAME, VararcomRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link VararcomFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/vararcom", name = "VararcomFaultInfo")
    public JAXBElement<VararcomFaultInfo> createVararcomFaultInfo(VararcomFaultInfo value) {
        return new JAXBElement<VararcomFaultInfo>(_VararcomFaultInfo_QNAME, VararcomFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link VararcomResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/vararcom", name = "VararcomResponse")
    public JAXBElement<VararcomResponse> createVararcomResponse(VararcomResponse value) {
        return new JAXBElement<VararcomResponse>(_VararcomResponse_QNAME, VararcomResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link VararcomHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/vararcom", name = "HostHeader")
    public JAXBElement<VararcomHostHeader> createHostHeader(VararcomHostHeader value) {
        return new JAXBElement<VararcomHostHeader>(_HostHeader_QNAME, VararcomHostHeader.class, null, value);
    }

}
