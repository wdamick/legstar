
package com.legstar.test.coxb.dplarcht.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CBinaryBinding;
import com.legstar.coxb.rt.CStringBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.dplarcht.LsProgramsDataType;
import com.legstar.test.coxb.dplarcht.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:18.781+02:00
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. No reflection or annotations are used which makes this
 * class suitable for heavy loads.
 */

public class LsProgramsDataTypeBinding 
             extends CComplexBinding
             implements ICobolBinding {
  
    /** Name of java property to which this cobol element is bound. */
    private static final String JAVA_NAME = "lsProgramsData";
  
    /** Type of java property to which this cobol element is bound. */
    private static final String JAVA_TYPE = "LsProgramsDataType";
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding lsProgramName;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding lsProgramType;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding lsProgramLanguage;
            
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding lsProgramLength;
            
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding lsProgramUsecount;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding filler113;
            
    /** Java object to which this cobol complex array element is bound. */
    private LsProgramsDataType mJaxbObject;
  
    /** Java object factory for objects creation. */
    private ObjectFactory mObjectFactory;
  
    /**
     * No argument constructor.
     */
    public LsProgramsDataTypeBinding() {
        this(null, null, null);
    }
  
    /**
     * Constructor for a child complex binding which be later bound to a JAXB
     * object.
     * @param parentObject the parent binding
     */
    public LsProgramsDataTypeBinding(
        final CComplexBinding parentObject) {
        this(parentObject, null, null);
    }
  
    /**
     * Constructor for a root complex binding without an initial bound object.
     * @param objectFactory the java factory to use to create children instances
     */
    public LsProgramsDataTypeBinding(
        final ObjectFactory objectFactory) {
        this(null, objectFactory, null);
    }

    /**
     * Constructor for a root complex binding from an existing java object.
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsProgramsDataTypeBinding(
        final ObjectFactory objectFactory,
        final LsProgramsDataType jaxbObject) {
        this(null, objectFactory, jaxbObject);
    }

    /**
     * Constructor for a child complex binding with immediate binding to a JAXB
     * object.
     * @param parentObject the parent binding
     * @param objectFactory the java factory to use to create children instances
     * @param jaxbObject the java object to which this element is bound
     */
    public LsProgramsDataTypeBinding(
        final CComplexBinding parentObject,
        final ObjectFactory objectFactory,
        final LsProgramsDataType jaxbObject) {
        
        super(JAVA_NAME, JAVA_TYPE, parentObject);
        mJaxbObject = jaxbObject;
        mObjectFactory = objectFactory;
        initChildren();
    }
  
    /** Creates a binding property for each child. */
    public final void initChildren() {
    
        /* Create binding children instances */
        lsProgramName = new CStringBinding("lsProgramName", 8, false);
        lsProgramName.setCobolName("LS-PROGRAM-NAME");
        lsProgramType = new CStringBinding("lsProgramType", 12, false);
        lsProgramType.setCobolName("LS-PROGRAM-TYPE");
        lsProgramLanguage = new CStringBinding("lsProgramLanguage", 12, false);
        lsProgramLanguage.setCobolName("LS-PROGRAM-LANGUAGE");
        lsProgramLength = new CBinaryBinding("lsProgramLength", 4, 9, 0, true);
        lsProgramLength.setCobolName("LS-PROGRAM-LENGTH");
        lsProgramUsecount = new CBinaryBinding("lsProgramUsecount", 4, 9, 0, true);
        lsProgramUsecount.setCobolName("LS-PROGRAM-USECOUNT");
        filler113 = new CStringBinding("filler113", 24, false);
        filler113.setCobolName("FILLER-113");
        
        /* Add children to children list */
           
        getChildrenList().add(lsProgramName);
        getChildrenList().add(lsProgramType);
        getChildrenList().add(lsProgramLanguage);
        getChildrenList().add(lsProgramLength);
        getChildrenList().add(lsProgramUsecount);
        getChildrenList().add(filler113);
    }
  
   
    /** {@inheritDoc} */
    public final void createBoundObject() throws HostException {
        setJaxbObject(mObjectFactory.createLsProgramsDataType());
    }
       
    /** {@inheritDoc} */
    public final void prepareChildren() throws HostException {
    
    }
  
   
    /** {@inheritDoc} */
    public final void getValuesFromBoundObject() throws HostException {
        /* Set this binding properties from java object property values */
                     
        if (mJaxbObject.getLsProgramName() != null) {
            /* Set value from lsProgramName*/
            lsProgramName.setValue(mJaxbObject.getLsProgramName());
        }
                
        if (mJaxbObject.getLsProgramType() != null) {
            /* Set value from lsProgramType*/
            lsProgramType.setValue(mJaxbObject.getLsProgramType());
        }
                
        if (mJaxbObject.getLsProgramLanguage() != null) {
            /* Set value from lsProgramLanguage*/
            lsProgramLanguage.setValue(mJaxbObject.getLsProgramLanguage());
        }
                
        if (mJaxbObject.getLsProgramLength() != 0) {
            /* Set value from lsProgramLength*/
            lsProgramLength.setValue(new BigDecimal(mJaxbObject.getLsProgramLength()));
        
        }
                
        if (mJaxbObject.getLsProgramUsecount() != 0) {
            /* Set value from lsProgramUsecount*/
            lsProgramUsecount.setValue(new BigDecimal(mJaxbObject.getLsProgramUsecount()));
        
        }
                
        if (mJaxbObject.getFiller113() != null) {
            /* Set value from filler113*/
            filler113.setValue(mJaxbObject.getFiller113());
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueLsProgramName();
            break;
        case 1:
            setBoundObjectValueLsProgramType();
            break;
        case 2:
            setBoundObjectValueLsProgramLanguage();
            break;
        case 3:
            setBoundObjectValueLsProgramLength();
            break;
        case 4:
            setBoundObjectValueLsProgramUsecount();
            break;
        case 5:
            setBoundObjectValueFiller113();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsProgramName() throws HostException {
    
        if (lsProgramName.getValue() != null) {
            /* Set value of lsProgramName*/
            mJaxbObject.setLsProgramName(lsProgramName.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsProgramType() throws HostException {
    
        if (lsProgramType.getValue() != null) {
            /* Set value of lsProgramType*/
            mJaxbObject.setLsProgramType(lsProgramType.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsProgramLanguage() throws HostException {
    
        if (lsProgramLanguage.getValue() != null) {
            /* Set value of lsProgramLanguage*/
            mJaxbObject.setLsProgramLanguage(lsProgramLanguage.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsProgramLength() throws HostException {
    
        if (lsProgramLength.getValue() != null) {
            
            /* Set value of lsProgramLength*/
            mJaxbObject.setLsProgramLength(lsProgramLength.getValue().intValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueLsProgramUsecount() throws HostException {
    
        if (lsProgramUsecount.getValue() != null) {
            
            /* Set value of lsProgramUsecount*/
            mJaxbObject.setLsProgramUsecount(lsProgramUsecount.getValue().intValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueFiller113() throws HostException {
    
        if (filler113.getValue() != null) {
            /* Set value of filler113*/
            mJaxbObject.setFiller113(filler113.getValue());
        }
        
    }

    /** {@inheritDoc} */
    public final Object getValue() throws HostException {
        return mJaxbObject;
    }

    /**
     * @return the java object to which this cobol complex element is bound
     */
    public final LsProgramsDataType getJaxbObject() {
        return mJaxbObject;
    }

    /**
     * @param jaxbObject the java object to which this cobol complex element
     * is bound
     */
    public final void setJaxbObject(
        final LsProgramsDataType jaxbObject) {
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
