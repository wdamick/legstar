
package com.legstar.test.coxb.dplarcht.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CChoiceBinding;
import com.legstar.coxb.rt.CStringBinding;
import com.legstar.coxb.rt.CZonedDecimalBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.dplarcht.LsRequestType;
import com.legstar.test.coxb.dplarcht.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:18.421+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class LsAllItemsChoiceBinding 
             extends CChoiceBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "lsAllItems";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "LsRequestType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding lsAllItems;
            
    /** Child property CZonedDecimalBinding of simple type. */
    public CZonedDecimalBinding lsMaxItems;
            
    /** Java object to which this cobol complex array element is bound. */
    private LsRequestType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public LsAllItemsChoiceBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public LsAllItemsChoiceBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public LsAllItemsChoiceBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsAllItemsChoiceBinding(
        final ObjectFactory objectFactory,
        final LsRequestType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsAllItemsChoiceBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final LsRequestType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding alternatives instances */
        lsAllItems = new CStringBinding("lsAllItems", 4, false);
        lsAllItems.setCobolName("LS-ALL-ITEMS");
        lsAllItems.setIsRedefined(true);
        lsMaxItems = new CZonedDecimalBinding("lsMaxItems", 4, 4, 0, false, false, false);
        lsMaxItems.setCobolName("LS-MAX-ITEMS");
        lsMaxItems.setRedefines("LS-ALL-ITEMS");
        
        /* Add children to alternatives list */
            
        getAlternativesList().add(lsAllItems);
        getAlternativesList().add(lsMaxItems);
    }
  
   
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getLsAllItems() != null) {
            /* Set value from lsAllItems*/
            lsAllItems.setValue(mJaxbObject.getLsAllItems());
        }
                
        if (mJaxbObject.getLsMaxItems() != null) {
            /* Set value from lsMaxItems*/
            lsMaxItems.setValue(new BigDecimal(mJaxbObject.getLsMaxItems()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueLsAllItems();
            break;
        case 1:
            setBoundObjectValueLsMaxItems();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsAllItems() throws HostException {
    
        if (lsAllItems.getValue() != null) {
            /* Set value of lsAllItems*/
            mJaxbObject.setLsAllItems(lsAllItems.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsMaxItems() throws HostException {
    
        if (lsMaxItems.getValue() != null) {
            
            /* Set value of lsMaxItems*/
            mJaxbObject.setLsMaxItems(lsMaxItems.getValue().intValueExact());
                
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final LsRequestType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final LsRequestType jaxbObject) {
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
