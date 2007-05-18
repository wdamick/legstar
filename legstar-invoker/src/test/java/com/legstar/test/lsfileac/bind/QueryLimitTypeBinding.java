
package com.legstar.test.lsfileac.bind;

import com.legstar.host.HostException;
import com.legstar.test.lsfileac.ObjectFactory;
import com.legstar.test.lsfileac.QueryLimitType;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CPackedDecimalBinding;
import java.math.BigDecimal;


/**
 * This class was generated by COXB version 1.0.
 * 2007-05-14T17:10:52.078+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class QueryLimitTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "QueryLimitType";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "QueryLimitType";
    
    /** Child property CPackedDecimalBinding of simple type. */
    public CPackedDecimalBinding maxItemsRead;
            
    /** Child property CPackedDecimalBinding of simple type. */
    public CPackedDecimalBinding maxElapseTime;
            
    /** Java object to which this cobol complex array element is bound. */
    private QueryLimitType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public QueryLimitTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public QueryLimitTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public QueryLimitTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public QueryLimitTypeBinding(
        final ObjectFactory objectFactory,
        final QueryLimitType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public QueryLimitTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final QueryLimitType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        maxItemsRead = new CPackedDecimalBinding("maxItemsRead", 5, 8, 0, false);
        maxItemsRead.setCobolName("MAX-ITEMS-READ");
        maxElapseTime = new CPackedDecimalBinding("maxElapseTime", 5, 8, 0, false);
        maxElapseTime.setCobolName("MAX-ELAPSE-TIME");
        
        /* Add children to children list */
           
        getChildrenList().add(maxItemsRead);
        getChildrenList().add(maxElapseTime);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createQueryLimitType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getMaxItemsRead() != 0L) {
            /* Set value from maxItemsRead*/
            maxItemsRead.setValue(new BigDecimal(mJaxbObject.getMaxItemsRead()));
        
        }
                
        if (mJaxbObject.getMaxElapseTime() != 0L) {
            /* Set value from maxElapseTime*/
            maxElapseTime.setValue(new BigDecimal(mJaxbObject.getMaxElapseTime()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueMaxItemsRead();
            break;
        case 1:
            setBoundObjectValueMaxElapseTime();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueMaxItemsRead() throws HostException {
    
        if (maxItemsRead.getValue() != null) {
            
            /* Set value of maxItemsRead*/
            mJaxbObject.setMaxItemsRead(maxItemsRead.getValue().longValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueMaxElapseTime() throws HostException {
    
        if (maxElapseTime.getValue() != null) {
            
            /* Set value of maxElapseTime*/
            mJaxbObject.setMaxElapseTime(maxElapseTime.getValue().longValueExact());
                
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final QueryLimitType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final QueryLimitType jaxbObject) {
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
