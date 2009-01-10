
package com.legstar.test.coxb.dplarcht.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CBinaryBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.dplarcht.LsReplyDataType;
import com.legstar.test.coxb.dplarcht.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:18.984+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class LsReplyDataTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "lsReplyData";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "LsReplyDataType";
    
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding lsItemsCount;
            
    /** Child property LsItemsArrayTypeWrapperBinding of complexArray type. */
    public LsItemsArrayTypeWrapperBinding lsItemsArray;
            
    /** Java object to which this cobol complex array element is bound. */
    private LsReplyDataType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public LsReplyDataTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public LsReplyDataTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public LsReplyDataTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsReplyDataTypeBinding(
        final ObjectFactory objectFactory,
        final LsReplyDataType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsReplyDataTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final LsReplyDataType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        lsItemsCount = new CBinaryBinding("lsItemsCount", 4, 9, 0, false);
        lsItemsCount.setCobolName("LS-ITEMS-COUNT");
        lsItemsCount.setIsODOObject(true);
        lsItemsArray = new LsItemsArrayTypeWrapperBinding(this);
        
        /* Add children to children list */
           
        getChildrenList().add(lsItemsCount);
        getChildrenList().add(lsItemsArray);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createLsReplyDataType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
        /* Pass on the JAXB factory to child lsItemsArray  */
        lsItemsArray.setObjectFactory(mObjectFactory);

    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getLsItemsCount() != 0L) {
            /* Set value from lsItemsCount*/
            lsItemsCount.setValue(new BigDecimal(mJaxbObject.getLsItemsCount()));
        
        }
                
        if (mJaxbObject.getLsItemsArray() != null) {
            /* Set value from lsItemsArray*/
            lsItemsArray.setJaxbObject(mJaxbObject.getLsItemsArray());
            
        } else {
            lsItemsArray.createBoundObject();
        }
                
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueLsItemsCount();
            break;
        case 1:
            setBoundObjectValueLsItemsArray();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsItemsCount() throws HostException {
    
        if (lsItemsCount.getValue() != null) {
            
            /* Set value of lsItemsCount*/
            mJaxbObject.setLsItemsCount(lsItemsCount.getValue().longValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsItemsArray() throws HostException {
    
        /* Set value of complex array child lsItemsArray*/
        mJaxbObject.getLsItemsArray().clear();
        mJaxbObject.getLsItemsArray().addAll(lsItemsArray.getJaxbObject());
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final LsReplyDataType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final LsReplyDataType jaxbObject) {
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