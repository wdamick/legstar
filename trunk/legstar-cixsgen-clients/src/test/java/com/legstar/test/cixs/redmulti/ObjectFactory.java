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

package com.legstar.test.cixs.redmulti;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redmulti package. 
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

    private final static QName _RedmultiResponse_QNAME = new QName("http://cixs.test.legstar.com/redmulti", "RedmultiResponse");
    private final static QName _RedmultiRequest_QNAME = new QName("http://cixs.test.legstar.com/redmulti", "RedmultiRequest");
    private final static QName _RedmultiFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redmulti", "RedmultiFaultInfo");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/redmulti", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redmulti
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedmultiRequest }
     * 
     */
    public RedmultiRequest createRedmultiRequest() {
        return new RedmultiRequest();
    }

    /**
     * Create an instance of {@link RedmultiHostHeader }
     * 
     */
    public RedmultiHostHeader createRedmultiHostHeader() {
        return new RedmultiHostHeader();
    }

    /**
     * Create an instance of {@link RedmultiResponse }
     * 
     */
    public RedmultiResponse createRedmultiResponse() {
        return new RedmultiResponse();
    }

    /**
     * Create an instance of {@link RedmultiFaultInfo }
     * 
     */
    public RedmultiFaultInfo createRedmultiFaultInfo() {
        return new RedmultiFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedmultiResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redmulti", name = "RedmultiResponse")
    public JAXBElement<RedmultiResponse> createRedmultiResponse(RedmultiResponse value) {
        return new JAXBElement<RedmultiResponse>(_RedmultiResponse_QNAME, RedmultiResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedmultiRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redmulti", name = "RedmultiRequest")
    public JAXBElement<RedmultiRequest> createRedmultiRequest(RedmultiRequest value) {
        return new JAXBElement<RedmultiRequest>(_RedmultiRequest_QNAME, RedmultiRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedmultiFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redmulti", name = "RedmultiFaultInfo")
    public JAXBElement<RedmultiFaultInfo> createRedmultiFaultInfo(RedmultiFaultInfo value) {
        return new JAXBElement<RedmultiFaultInfo>(_RedmultiFaultInfo_QNAME, RedmultiFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedmultiHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redmulti", name = "HostHeader")
    public JAXBElement<RedmultiHostHeader> createHostHeader(RedmultiHostHeader value) {
        return new JAXBElement<RedmultiHostHeader>(_HostHeader_QNAME, RedmultiHostHeader.class, null, value);
    }

}
