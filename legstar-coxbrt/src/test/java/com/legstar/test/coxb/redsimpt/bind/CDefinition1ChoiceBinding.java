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

package com.legstar.test.coxb.redsimpt.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CChoiceBinding;
import com.legstar.coxb.rt.CStringBinding;
import com.legstar.coxb.rt.CZonedDecimalBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.redsimpt.DfhcommareaType;
import com.legstar.test.coxb.redsimpt.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-01-26T18:17:47.484+01:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class CDefinition1ChoiceBinding 
             extends CChoiceBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "cDefinition1";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "DfhcommareaType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding cDefinition1;
            
    /** Child property CZonedDecimalBinding of simple type. */
    public CZonedDecimalBinding cDefinition2;
            
    /** Java object to which this cobol complex array element is bound. */
    private DfhcommareaType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public CDefinition1ChoiceBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public CDefinition1ChoiceBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public CDefinition1ChoiceBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public CDefinition1ChoiceBinding(
        final ObjectFactory objectFactory,
        final DfhcommareaType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public CDefinition1ChoiceBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final DfhcommareaType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        setUnmarshalChoiceStrategy(
            new com.legstar.coxb.cust.redsimpt.ChoiceSelector());
        
        /* Create binding alternatives instances */
        cDefinition1 = new CStringBinding("cDefinition1", 18, false);
        cDefinition1.setCobolName("C-DEFINITION-1");
        cDefinition1.setIsRedefined(true);
        cDefinition2 = new CZonedDecimalBinding("cDefinition2", 18, 18, 0, false, false, false);
        cDefinition2.setCobolName("C-DEFINITION-2");
        cDefinition2.setRedefines("C-DEFINITION-1");
        
        /* Add children to alternatives list */
            
        getAlternativesList().add(cDefinition1);
        getAlternativesList().add(cDefinition2);
    }
  
   
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getCDefinition1() != null) {
            /* Set value from cDefinition1*/
            cDefinition1.setValue(mJaxbObject.getCDefinition1());
        }
                
        if (mJaxbObject.getCDefinition2() != null) {
            /* Set value from cDefinition2*/
            cDefinition2.setValue(new BigDecimal(mJaxbObject.getCDefinition2()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueCDefinition1();
            break;
        case 1:
            setBoundObjectValueCDefinition2();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCDefinition1() throws HostException {
    
        if (cDefinition1.getValue() != null) {
            /* Set value of cDefinition1*/
            mJaxbObject.setCDefinition1(cDefinition1.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCDefinition2() throws HostException {
    
        if (cDefinition2.getValue() != null) {
            
            /* Set value of cDefinition2*/
            mJaxbObject.setCDefinition2(cDefinition2.getValue().longValueExact());
                
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final DfhcommareaType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final DfhcommareaType jaxbObject) {
        mJaxbObject = jaxbObject;
    }

    /**
     * @return the java object factory for objects creation
     */
    public final ObjectFactory getObjectFactory() {
        return mObjectFactory;
    }

    /**
     * @param objectFactory the java object factory for objects creation to set
     */
    public final void setObjectFactory(final ObjectFactory objectFactory) {
        mObjectFactory = objectFactory;
    }

}
