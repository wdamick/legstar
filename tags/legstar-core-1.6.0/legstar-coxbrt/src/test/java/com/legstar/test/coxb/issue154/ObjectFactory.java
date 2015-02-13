/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb.issue154;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.issue154 package. 
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

    private final static QName _EnumContainerClass_QNAME = new QName("http://coxb.test.legstar.com/issue154", "enumContainerClass");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.issue154
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link EnumContainerClass }
     * 
     */
    public EnumContainerClass createEnumContainerClass() {
        return new EnumContainerClass();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link EnumContainerClass }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://coxb.test.legstar.com/issue154", name = "enumContainerClass")
    public JAXBElement<EnumContainerClass> createEnumContainerClass(EnumContainerClass value) {
        return new JAXBElement<EnumContainerClass>(_EnumContainerClass_QNAME, EnumContainerClass.class, null, value);
    }

}
