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

package com.legstar.test.cixs.floatmix;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.floatmix package. 
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

    private final static QName _FloatmixRequest_QNAME = new QName("http://cixs.test.legstar.com/floatmix", "FloatmixRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/floatmix", "HostHeader");
    private final static QName _FloatmixResponse_QNAME = new QName("http://cixs.test.legstar.com/floatmix", "FloatmixResponse");
    private final static QName _FloatmixFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/floatmix", "FloatmixFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.floatmix
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link FloatmixHostHeader }
     * 
     */
    public FloatmixHostHeader createFloatmixHostHeader() {
        return new FloatmixHostHeader();
    }

    /**
     * Create an instance of {@link FloatmixFaultInfo }
     * 
     */
    public FloatmixFaultInfo createFloatmixFaultInfo() {
        return new FloatmixFaultInfo();
    }

    /**
     * Create an instance of {@link FloatmixRequest }
     * 
     */
    public FloatmixRequest createFloatmixRequest() {
        return new FloatmixRequest();
    }

    /**
     * Create an instance of {@link FloatmixResponse }
     * 
     */
    public FloatmixResponse createFloatmixResponse() {
        return new FloatmixResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FloatmixRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/floatmix", name = "FloatmixRequest")
    public JAXBElement<FloatmixRequest> createFloatmixRequest(FloatmixRequest value) {
        return new JAXBElement<FloatmixRequest>(_FloatmixRequest_QNAME, FloatmixRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FloatmixHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/floatmix", name = "HostHeader")
    public JAXBElement<FloatmixHostHeader> createHostHeader(FloatmixHostHeader value) {
        return new JAXBElement<FloatmixHostHeader>(_HostHeader_QNAME, FloatmixHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FloatmixResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/floatmix", name = "FloatmixResponse")
    public JAXBElement<FloatmixResponse> createFloatmixResponse(FloatmixResponse value) {
        return new JAXBElement<FloatmixResponse>(_FloatmixResponse_QNAME, FloatmixResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FloatmixFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/floatmix", name = "FloatmixFaultInfo")
    public JAXBElement<FloatmixFaultInfo> createFloatmixFaultInfo(FloatmixFaultInfo value) {
        return new JAXBElement<FloatmixFaultInfo>(_FloatmixFaultInfo_QNAME, FloatmixFaultInfo.class, null, value);
    }

}
