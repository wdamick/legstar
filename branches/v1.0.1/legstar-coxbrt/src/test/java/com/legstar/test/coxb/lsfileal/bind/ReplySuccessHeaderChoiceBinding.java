
package com.legstar.test.coxb.lsfileal.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CChoiceBinding;

import com.legstar.test.coxb.lsfileal.ReplyDataType;
import com.legstar.test.coxb.lsfileal.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:45.781+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class ReplySuccessHeaderChoiceBinding 
             extends CChoiceBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "replySuccessHeader";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "ReplyDataType";
    
    /** Child property ReplySuccessHeaderTypeBinding of complex type. */
    public ReplySuccessHeaderTypeBinding replySuccessHeader;
            
    /** Child property ReplyErrorHeaderTypeBinding of complex type. */
    public ReplyErrorHeaderTypeBinding replyErrorHeader;
            
    /** Java object to which this cobol complex array element is bound. */
    private ReplyDataType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public ReplySuccessHeaderChoiceBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public ReplySuccessHeaderChoiceBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public ReplySuccessHeaderChoiceBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public ReplySuccessHeaderChoiceBinding(
        final ObjectFactory objectFactory,
        final ReplyDataType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public ReplySuccessHeaderChoiceBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final ReplyDataType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding alternatives instances */
        replySuccessHeader = new ReplySuccessHeaderTypeBinding(getParentBinding());
        replySuccessHeader.setIsRedefined(true);
        replyErrorHeader = new ReplyErrorHeaderTypeBinding(getParentBinding());
        replyErrorHeader.setRedefines("REPLY-SUCCESS-HEADER");
        
        /* Add children to alternatives list */
            
        getAlternativesList().add(replySuccessHeader);
        getAlternativesList().add(replyErrorHeader);
    }
  
   
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
        /* Pass on the JAXB factory to child replySuccessHeader  */
        replySuccessHeader.setObjectFactory(mObjectFactory);

        /* Pass on the JAXB factory to child replyErrorHeader  */
        replyErrorHeader.setObjectFactory(mObjectFactory);

    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getReplySuccessHeader() != null) {
            /* Set value from replySuccessHeader*/
            replySuccessHeader.setJaxbObject(mJaxbObject.getReplySuccessHeader());
            
        }
                            
        if (mJaxbObject.getReplyErrorHeader() != null) {
            /* Set value from replyErrorHeader*/
            replyErrorHeader.setJaxbObject(mJaxbObject.getReplyErrorHeader());
            
        }
                
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueReplySuccessHeader();
            break;
        case 1:
            setBoundObjectValueReplyErrorHeader();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueReplySuccessHeader() throws HostException {
    
        /* Set value of complex child replySuccessHeader*/
        mJaxbObject.setReplySuccessHeader(replySuccessHeader.getJaxbObject());
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueReplyErrorHeader() throws HostException {
    
        /* Set value of complex child replyErrorHeader*/
        mJaxbObject.setReplyErrorHeader(replyErrorHeader.getJaxbObject());
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final ReplyDataType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final ReplyDataType jaxbObject) {
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
