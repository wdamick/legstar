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

package com.legstar.test.coxb.vararcom.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CBinaryBinding;
import com.legstar.coxb.rt.CStringBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.vararcom.CArrayType;
import com.legstar.test.coxb.vararcom.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-01-26T18:17:51.203+01:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class CArrayTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "cArray";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "CArrayType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding cItem1;
            
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding cItem2;
            
    /** Java object to which this cobol complex array element is bound. */
    private CArrayType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public CArrayTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public CArrayTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public CArrayTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public CArrayTypeBinding(
        final ObjectFactory objectFactory,
        final CArrayType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public CArrayTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final CArrayType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        cItem1 = new CStringBinding("cItem1", 5, false);
        cItem1.setCobolName("C-ITEM-1");
        cItem2 = new CBinaryBinding("cItem2", 2, 4, 0, true);
        cItem2.setCobolName("C-ITEM-2");
        
        /* Add children to children list */
           
        getChildrenList().add(cItem1);
        getChildrenList().add(cItem2);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createCArrayType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getCItem1() != null) {
            /* Set value from cItem1*/
            cItem1.setValue(mJaxbObject.getCItem1());
        }
                
        if (mJaxbObject.getCItem2() != 0) {
            /* Set value from cItem2*/
            cItem2.setValue(new BigDecimal(mJaxbObject.getCItem2()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueCItem1();
            break;
        case 1:
            setBoundObjectValueCItem2();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCItem1() throws HostException {
    
        if (cItem1.getValue() != null) {
            /* Set value of cItem1*/
            mJaxbObject.setCItem1(cItem1.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCItem2() throws HostException {
    
        if (cItem2.getValue() != null) {
            
            /* Set value of cItem2*/
            mJaxbObject.setCItem2(cItem2.getValue().shortValueExact());
                
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final CArrayType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final CArrayType jaxbObject) {
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