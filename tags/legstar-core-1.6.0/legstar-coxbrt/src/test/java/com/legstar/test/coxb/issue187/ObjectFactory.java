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
package com.legstar.test.coxb.issue187;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the com.legstar.test.coxb.ardo03 package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Ardo03Record_QNAME = new QName(
            "http://legstar.com/test/coxb/ardo03", "Ardo03Record");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package: com.legstar.test.coxb.ardo03
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link OdoSubArray }
     * 
     */
    public OdoSubArray createOdoSubArray() {
        return new OdoSubArray();
    }

    /**
     * Create an instance of {@link Ardo03Record }
     * 
     */
    public Ardo03Record createArdo03Record() {
        return new Ardo03Record();
    }

    /**
     * Create an instance of {@link OdoArray }
     * 
     */
    public OdoArray createOdoArray() {
        return new OdoArray();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Ardo03Record }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/ardo03", name = "Ardo03Record")
    public JAXBElement < Ardo03Record > createArdo03Record(Ardo03Record value) {
        return new JAXBElement < Ardo03Record >(_Ardo03Record_QNAME,
                Ardo03Record.class, null, value);
    }

}
