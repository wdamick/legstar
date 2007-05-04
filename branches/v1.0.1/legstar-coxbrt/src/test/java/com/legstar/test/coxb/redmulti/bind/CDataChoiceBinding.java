
package com.legstar.test.coxb.redmulti.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CChoiceBinding;
import com.legstar.coxb.rt.CStringBinding;

import com.legstar.test.coxb.redmulti.DfhcommareaType;
import com.legstar.test.coxb.redmulti.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:36.359+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class CDataChoiceBinding 
             extends CChoiceBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "cData";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "DfhcommareaType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding cData;
            
    /** Child property Filler35TypeBinding of complex type. */
    public Filler35TypeBinding filler35;
            
    /** Child property Filler38TypeBinding of complex type. */
    public Filler38TypeBinding filler38;
            
    /** Java object to which this cobol complex array element is bound. */
    private DfhcommareaType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public CDataChoiceBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public CDataChoiceBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public CDataChoiceBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public CDataChoiceBinding(
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
    public CDataChoiceBinding(
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
            new com.legstar.coxb.cust.redmulti.ChoiceSelector());
        
        /* Create binding alternatives instances */
        cData = new CStringBinding("cData", 200, false);
        cData.setCobolName("C-DATA");
        cData.setIsRedefined(true);
        filler35 = new Filler35TypeBinding(getParentBinding());
        filler35.setRedefines("C-DATA");
        filler38 = new Filler38TypeBinding(getParentBinding());
        filler38.setRedefines("C-DATA");
        
        /* Add children to alternatives list */
            
        getAlternativesList().add(cData);
        getAlternativesList().add(filler35);
        getAlternativesList().add(filler38);
    }
  
   
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
        /* Pass on the JAXB factory to child filler35  */
        filler35.setObjectFactory(mObjectFactory);

        /* Pass on the JAXB factory to child filler38  */
        filler38.setObjectFactory(mObjectFactory);

    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getCData() != null) {
            /* Set value from cData*/
            cData.setValue(mJaxbObject.getCData());
        }
                
        if (mJaxbObject.getFiller35() != null) {
            /* Set value from filler35*/
            filler35.setJaxbObject(mJaxbObject.getFiller35());
            
        }
                            
        if (mJaxbObject.getFiller38() != null) {
            /* Set value from filler38*/
            filler38.setJaxbObject(mJaxbObject.getFiller38());
            
        }
                
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueCData();
            break;
        case 1:
            setBoundObjectValueFiller35();
            break;
        case 2:
            setBoundObjectValueFiller38();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCData() throws HostException {
    
        if (cData.getValue() != null) {
            /* Set value of cData*/
            mJaxbObject.setCData(cData.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueFiller35() throws HostException {
    
        /* Set value of complex child filler35*/
        mJaxbObject.setFiller35(filler35.getJaxbObject());
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueFiller38() throws HostException {
    
        /* Set value of complex child filler38*/
        mJaxbObject.setFiller38(filler38.getJaxbObject());
        
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
