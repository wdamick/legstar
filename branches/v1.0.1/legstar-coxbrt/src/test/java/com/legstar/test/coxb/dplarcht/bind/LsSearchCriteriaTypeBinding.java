
package com.legstar.test.coxb.dplarcht.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CStringBinding;
import com.legstar.coxb.rt.CPackedDecimalBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.dplarcht.LsSearchCriteriaType;
import com.legstar.test.coxb.dplarcht.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:18.546+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class LsSearchCriteriaTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "lsSearchCriteria";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "LsSearchCriteriaType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding lsStartwith;
            
    /** Child property CPackedDecimalBinding of simple type. */
    public CPackedDecimalBinding lsStartwithLen;
            
    /** Java object to which this cobol complex array element is bound. */
    private LsSearchCriteriaType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public LsSearchCriteriaTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public LsSearchCriteriaTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public LsSearchCriteriaTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsSearchCriteriaTypeBinding(
        final ObjectFactory objectFactory,
        final LsSearchCriteriaType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsSearchCriteriaTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final LsSearchCriteriaType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        lsStartwith = new CStringBinding("lsStartwith", 8, false);
        lsStartwith.setCobolName("LS-STARTWITH");
        lsStartwithLen = new CPackedDecimalBinding("lsStartwithLen", 5, 9, 0, false);
        lsStartwithLen.setCobolName("LS-STARTWITH-LEN");
        
        /* Add children to children list */
           
        getChildrenList().add(lsStartwith);
        getChildrenList().add(lsStartwithLen);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createLsSearchCriteriaType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getLsStartwith() != null) {
            /* Set value from lsStartwith*/
            lsStartwith.setValue(mJaxbObject.getLsStartwith());
        }
                
        if (mJaxbObject.getLsStartwithLen() != 0L) {
            /* Set value from lsStartwithLen*/
            lsStartwithLen.setValue(new BigDecimal(mJaxbObject.getLsStartwithLen()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueLsStartwith();
            break;
        case 1:
            setBoundObjectValueLsStartwithLen();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsStartwith() throws HostException {
    
        if (lsStartwith.getValue() != null) {
            /* Set value of lsStartwith*/
            mJaxbObject.setLsStartwith(lsStartwith.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsStartwithLen() throws HostException {
    
        if (lsStartwithLen.getValue() != null) {
            
            /* Set value of lsStartwithLen*/
            mJaxbObject.setLsStartwithLen(lsStartwithLen.getValue().longValueExact());
                
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final LsSearchCriteriaType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final LsSearchCriteriaType jaxbObject) {
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
