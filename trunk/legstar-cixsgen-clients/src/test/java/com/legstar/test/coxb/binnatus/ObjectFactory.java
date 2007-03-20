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

package com.legstar.test.coxb.binnatus;

import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.binnatus package. 
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


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.binnatus
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsUnsignedNativeType }
     * 
     */
    public LsUnsignedNativeType createLsUnsignedNativeType() {
        return new LsUnsignedNativeType();
    }

    /**
     * Create an instance of {@link LsHalfwordsType }
     * 
     */
    public LsHalfwordsType createLsHalfwordsType() {
        return new LsHalfwordsType();
    }

    /**
     * Create an instance of {@link LsDoublewordsType }
     * 
     */
    public LsDoublewordsType createLsDoublewordsType() {
        return new LsDoublewordsType();
    }

    /**
     * Create an instance of {@link DfhcommareaType }
     * 
     */
    public DfhcommareaType createDfhcommareaType() {
        return new DfhcommareaType();
    }

    /**
     * Create an instance of {@link LsFullwordsType }
     * 
     */
    public LsFullwordsType createLsFullwordsType() {
        return new LsFullwordsType();
    }

}
