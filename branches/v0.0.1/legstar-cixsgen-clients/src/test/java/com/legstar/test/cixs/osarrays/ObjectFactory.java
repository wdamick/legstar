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

package com.legstar.test.cixs.osarrays;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.osarrays package. 
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

    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "HostHeader");
    private final static QName _OsarraysFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysFaultInfo");
    private final static QName _OsarraysResponse_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysResponse");
    private final static QName _OsarraysRequest_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.osarrays
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link OsarraysResponse }
     * 
     */
    public OsarraysResponse createOsarraysResponse() {
        return new OsarraysResponse();
    }

    /**
     * Create an instance of {@link OsarraysHostHeader }
     * 
     */
    public OsarraysHostHeader createOsarraysHostHeader() {
        return new OsarraysHostHeader();
    }

    /**
     * Create an instance of {@link OsarraysRequest }
     * 
     */
    public OsarraysRequest createOsarraysRequest() {
        return new OsarraysRequest();
    }

    /**
     * Create an instance of {@link OsarraysFaultInfo }
     * 
     */
    public OsarraysFaultInfo createOsarraysFaultInfo() {
        return new OsarraysFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "HostHeader")
    public JAXBElement<OsarraysHostHeader> createHostHeader(OsarraysHostHeader value) {
        return new JAXBElement<OsarraysHostHeader>(_HostHeader_QNAME, OsarraysHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysFaultInfo")
    public JAXBElement<OsarraysFaultInfo> createOsarraysFaultInfo(OsarraysFaultInfo value) {
        return new JAXBElement<OsarraysFaultInfo>(_OsarraysFaultInfo_QNAME, OsarraysFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysResponse")
    public JAXBElement<OsarraysResponse> createOsarraysResponse(OsarraysResponse value) {
        return new JAXBElement<OsarraysResponse>(_OsarraysResponse_QNAME, OsarraysResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysRequest")
    public JAXBElement<OsarraysRequest> createOsarraysRequest(OsarraysRequest value) {
        return new JAXBElement<OsarraysRequest>(_OsarraysRequest_QNAME, OsarraysRequest.class, null, value);
    }

}
