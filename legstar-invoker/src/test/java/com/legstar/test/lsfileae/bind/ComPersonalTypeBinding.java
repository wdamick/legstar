
package com.legstar.test.lsfileae.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CStringBinding;

import com.legstar.test.lsfileae.ComPersonalType;
import com.legstar.test.lsfileae.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-03-09T09:09:56.191+01:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class ComPersonalTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "comPersonal";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "ComPersonalType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding comName;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding comAddress;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding comPhone;
            
    /** Java object to which this cobol complex array element is bound. */
    private ComPersonalType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public ComPersonalTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public ComPersonalTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public ComPersonalTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public ComPersonalTypeBinding(
        final ObjectFactory objectFactory,
        final ComPersonalType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public ComPersonalTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final ComPersonalType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        comName = new CStringBinding("comName", 20, false);
        comName.setCobolName("COM-NAME");
        comAddress = new CStringBinding("comAddress", 20, false);
        comAddress.setCobolName("COM-ADDRESS");
        comPhone = new CStringBinding("comPhone", 8, false);
        comPhone.setCobolName("COM-PHONE");
        
        /* Add children to children list */
           
        getChildrenList().add(comName);
        getChildrenList().add(comAddress);
        getChildrenList().add(comPhone);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createComPersonalType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getComName() != null) {
            /* Set value from comName*/
            comName.setValue(mJaxbObject.getComName());
        }
                
        if (mJaxbObject.getComAddress() != null) {
            /* Set value from comAddress*/
            comAddress.setValue(mJaxbObject.getComAddress());
        }
                
        if (mJaxbObject.getComPhone() != null) {
            /* Set value from comPhone*/
            comPhone.setValue(mJaxbObject.getComPhone());
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueComName();
            break;
        case 1:
            setBoundObjectValueComAddress();
            break;
        case 2:
            setBoundObjectValueComPhone();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueComName() throws HostException {
    
        if (comName.getValue() != null) {
            /* Set value of comName*/
            mJaxbObject.setComName(comName.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueComAddress() throws HostException {
    
        if (comAddress.getValue() != null) {
            /* Set value of comAddress*/
            mJaxbObject.setComAddress(comAddress.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueComPhone() throws HostException {
    
        if (comPhone.getValue() != null) {
            /* Set value of comPhone*/
            mJaxbObject.setComPhone(comPhone.getValue());
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final ComPersonalType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final ComPersonalType jaxbObject) {
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
