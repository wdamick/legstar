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

package com.legstar.test.coxb.fixarnum.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CArrayBinaryBinding;
import com.legstar.coxb.rt.CArrayPackedDecimalBinding;
import com.legstar.coxb.rt.CArrayZonedDecimalBinding;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.test.coxb.fixarnum.DfhcommareaType;
import com.legstar.test.coxb.fixarnum.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-01-26T18:17:32.625+01:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class DfhcommareaTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "DfhcommareaType";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "DfhcommareaType";
    
    /** Child property CArrayPackedDecimalBinding of simple type. */
    public CArrayPackedDecimalBinding cArrayPd;
            
    /** Child property CArrayZonedDecimalBinding of simple type. */
    public CArrayZonedDecimalBinding cArrayZd;
            
    /** Child property CArrayZonedDecimalBinding of simple type. */
    public CArrayZonedDecimalBinding cArrayZi;
            
    /** Child property CArrayBinaryBinding of simple type. */
    public CArrayBinaryBinding cArrayBi;
            
    /** Child property CArrayBinaryBinding of simple type. */
    public CArrayBinaryBinding cArrayNi;
            
    /** Java object to which this cobol complex array element is bound. */
    private DfhcommareaType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public DfhcommareaTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public DfhcommareaTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public DfhcommareaTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public DfhcommareaTypeBinding(
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
    public DfhcommareaTypeBinding(
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
    
        /* Create binding children instances */
        cArrayPd = new CArrayPackedDecimalBinding("cArrayPd", this, 4, 7, 2, false, 3, 3);
        cArrayPd.setCobolName("C-ARRAY-PD");
        cArrayZd = new CArrayZonedDecimalBinding("cArrayZd", this, 6, 6, 3, false, false, false, 3, 3);
        cArrayZd.setCobolName("C-ARRAY-ZD");
        cArrayZi = new CArrayZonedDecimalBinding("cArrayZi", this, 4, 4, 0, false, false, false, 3, 3);
        cArrayZi.setCobolName("C-ARRAY-ZI");
        cArrayBi = new CArrayBinaryBinding("cArrayBi", this, 4, 9, 0, false, 3, 3);
        cArrayBi.setCobolName("C-ARRAY-BI");
        cArrayNi = new CArrayBinaryBinding("cArrayNi", this, 8, 18, 0, false, 3, 3);
        cArrayNi.setCobolName("C-ARRAY-NI");
        
        /* Add children to children list */
           
        getChildrenList().add(cArrayPd);
        getChildrenList().add(cArrayZd);
        getChildrenList().add(cArrayZi);
        getChildrenList().add(cArrayBi);
        getChildrenList().add(cArrayNi);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createDfhcommareaType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getCArrayPd() != null) {
            /* Set value from cArrayPd*/
              cArrayPd.setValue(mJaxbObject.getCArrayPd());
        
        }
                
        if (mJaxbObject.getCArrayZd() != null) {
            /* Set value from cArrayZd*/
              cArrayZd.setValue(mJaxbObject.getCArrayZd());
        
        }
                
        if (mJaxbObject.getCArrayZi() != null) {
            /* Set value from cArrayZi*/
      
            if (cArrayZi.getList() != null) {
                cArrayZi.getList().clear();
            } else {
                cArrayZi.setList(new java.util.ArrayList < BigDecimal >());
            }
            for (Integer item : mJaxbObject.getCArrayZi()) {
                cArrayZi.getList().add(new BigDecimal(item));
            }
        
        }
                
        if (mJaxbObject.getCArrayBi() != null) {
            /* Set value from cArrayBi*/
      
            if (cArrayBi.getList() != null) {
                cArrayBi.getList().clear();
            } else {
                cArrayBi.setList(new java.util.ArrayList < BigDecimal >());
            }
            for (Long item : mJaxbObject.getCArrayBi()) {
                cArrayBi.getList().add(new BigDecimal(item));
            }
        
        }
                
        if (mJaxbObject.getCArrayNi() != null) {
            /* Set value from cArrayNi*/
      
            if (cArrayNi.getList() != null) {
                cArrayNi.getList().clear();
            } else {
                cArrayNi.setList(new java.util.ArrayList < BigDecimal >());
            }
            for (BigInteger item : mJaxbObject.getCArrayNi()) {
                cArrayNi.getList().add(new BigDecimal(item));
            }
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueCArrayPd();
            break;
        case 1:
            setBoundObjectValueCArrayZd();
            break;
        case 2:
            setBoundObjectValueCArrayZi();
            break;
        case 3:
            setBoundObjectValueCArrayBi();
            break;
        case 4:
            setBoundObjectValueCArrayNi();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCArrayPd() throws HostException {
    
        if (cArrayPd.getValue() != null) {
            
            /* Set value of cArrayPd*/
            mJaxbObject.getCArrayPd().clear();
            mJaxbObject.getCArrayPd().addAll(cArrayPd.getValue());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCArrayZd() throws HostException {
    
        if (cArrayZd.getValue() != null) {
            
            /* Set value of cArrayZd*/
            mJaxbObject.getCArrayZd().clear();
            mJaxbObject.getCArrayZd().addAll(cArrayZd.getValue());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCArrayZi() throws HostException {
    
        if (cArrayZi.getValue() != null) {
            
            /* Set value of cArrayZi*/
            mJaxbObject.getCArrayZi().clear();
            for (BigDecimal item : cArrayZi.getValue()) {
                mJaxbObject.getCArrayZi().add(new Integer(item.intValueExact()));
            }
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCArrayBi() throws HostException {
    
        if (cArrayBi.getValue() != null) {
            
            /* Set value of cArrayBi*/
            mJaxbObject.getCArrayBi().clear();
            for (BigDecimal item : cArrayBi.getValue()) {
                mJaxbObject.getCArrayBi().add(new Long(item.longValueExact()));
            }
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCArrayNi() throws HostException {
    
        if (cArrayNi.getValue() != null) {
            
            /* Set value of cArrayNi*/
            mJaxbObject.getCArrayNi().clear();
            for (BigDecimal item : cArrayNi.getValue()) {
               mJaxbObject.getCArrayNi().add(item.toBigInteger());
            }
                
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
