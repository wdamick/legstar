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

package com.legstar.test.cixs.charsets;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.charsets package. 
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

    private final static QName _CharsetsResponse_QNAME = new QName("http://cixs.test.legstar.com/charsets", "CharsetsResponse");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/charsets", "HostHeader");
    private final static QName _CharsetsFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/charsets", "CharsetsFaultInfo");
    private final static QName _CharsetsRequest_QNAME = new QName("http://cixs.test.legstar.com/charsets", "CharsetsRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.charsets
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link CharsetsRequest }
     * 
     */
    public CharsetsRequest createCharsetsRequest() {
        return new CharsetsRequest();
    }

    /**
     * Create an instance of {@link CharsetsResponse }
     * 
     */
    public CharsetsResponse createCharsetsResponse() {
        return new CharsetsResponse();
    }

    /**
     * Create an instance of {@link CharsetsHostHeader }
     * 
     */
    public CharsetsHostHeader createCharsetsHostHeader() {
        return new CharsetsHostHeader();
    }

    /**
     * Create an instance of {@link CharsetsFaultInfo }
     * 
     */
    public CharsetsFaultInfo createCharsetsFaultInfo() {
        return new CharsetsFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CharsetsResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/charsets", name = "CharsetsResponse")
    public JAXBElement<CharsetsResponse> createCharsetsResponse(CharsetsResponse value) {
        return new JAXBElement<CharsetsResponse>(_CharsetsResponse_QNAME, CharsetsResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CharsetsHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/charsets", name = "HostHeader")
    public JAXBElement<CharsetsHostHeader> createHostHeader(CharsetsHostHeader value) {
        return new JAXBElement<CharsetsHostHeader>(_HostHeader_QNAME, CharsetsHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CharsetsFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/charsets", name = "CharsetsFaultInfo")
    public JAXBElement<CharsetsFaultInfo> createCharsetsFaultInfo(CharsetsFaultInfo value) {
        return new JAXBElement<CharsetsFaultInfo>(_CharsetsFaultInfo_QNAME, CharsetsFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CharsetsRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/charsets", name = "CharsetsRequest")
    public JAXBElement<CharsetsRequest> createCharsetsRequest(CharsetsRequest value) {
        return new JAXBElement<CharsetsRequest>(_CharsetsRequest_QNAME, CharsetsRequest.class, null, value);
    }

}
