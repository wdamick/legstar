
package com.legstar.test.coxb.typesmix.bind;

import com.legstar.host.HostException;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.rt.CComplexBinding;

import com.legstar.coxb.rt.CBinaryBinding;
import com.legstar.coxb.rt.CDoubleBinding;
import com.legstar.coxb.rt.CFloatBinding;
import com.legstar.coxb.rt.COctetStreamBinding;
import com.legstar.coxb.rt.CStringBinding;
import com.legstar.coxb.rt.CNationalBinding;
import com.legstar.coxb.rt.CPackedDecimalBinding;
import com.legstar.coxb.rt.CZonedDecimalBinding;
import java.math.BigDecimal;

import com.legstar.test.coxb.typesmix.DfhcommareaType;
import com.legstar.test.coxb.typesmix.ObjectFactory;

/**
 * This class was generated by COXB version 1.0.
 * 2007-04-23T17:01:42.14+02:00
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
    
    /** Child property CStringBinding of simple type. */
    public CStringBinding cAlphabetic;
            
    /** Child property CNationalBinding of simple type. */
    public CNationalBinding cNational;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cDbcs;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cAlphanumericEdited;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cAlphanumeric;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cOctetString;
            
    /** Child property CFloatBinding of simple type. */
    public CFloatBinding cSingleFloat;
            
    /** Child property CDoubleBinding of simple type. */
    public CDoubleBinding cDoubleFloat;
            
    /** Child property CPackedDecimalBinding of simple type. */
    public CPackedDecimalBinding cPackedDecimal;
            
    /** Child property CZonedDecimalBinding of simple type. */
    public CZonedDecimalBinding cZonedDecimal;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cNumericEdited1;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cNumericEdited2;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cNumericEdited3;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cNumericEdited4;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cIndex;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cPointer;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cProcPointer;
            
    /** Child property COctetStreamBinding of simple type. */
    public COctetStreamBinding cFuncPointer;
            
    /** Child property CStringBinding of simple type. */
    public CStringBinding cExternalFloating;
            
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding cBinary;
            
    /** Child property CBinaryBinding of simple type. */
    public CBinaryBinding cNativeBinary;
            
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
        cAlphabetic = new CStringBinding("cAlphabetic", 5, false);
        cAlphabetic.setCobolName("C-ALPHABETIC");
        cNational = new CNationalBinding("cNational", 18, false);
        cNational.setCobolName("C-NATIONAL");
        cDbcs = new COctetStreamBinding("cDbcs", 8);
        cDbcs.setCobolName("C-DBCS");
        cAlphanumericEdited = new CStringBinding("cAlphanumericEdited", 14, false);
        cAlphanumericEdited.setCobolName("C-ALPHANUMERIC-EDITED");
        cAlphanumeric = new CStringBinding("cAlphanumeric", 7, false);
        cAlphanumeric.setCobolName("C-ALPHANUMERIC");
        cOctetString = new COctetStreamBinding("cOctetString", 8);
        cOctetString.setCobolName("C-OCTET-STRING");
        cSingleFloat = new CFloatBinding("cSingleFloat");
        cSingleFloat.setCobolName("C-SINGLE-FLOAT");
        cDoubleFloat = new CDoubleBinding("cDoubleFloat");
        cDoubleFloat.setCobolName("C-DOUBLE-FLOAT");
        cPackedDecimal = new CPackedDecimalBinding("cPackedDecimal", 9, 17, 2, true);
        cPackedDecimal.setCobolName("C-PACKED-DECIMAL");
        cZonedDecimal = new CZonedDecimalBinding("cZonedDecimal", 14, 14, 0, true, false, false);
        cZonedDecimal.setCobolName("C-ZONED-DECIMAL");
        cNumericEdited1 = new CStringBinding("cNumericEdited1", 8, false);
        cNumericEdited1.setCobolName("C-NUMERIC-EDITED-1");
        cNumericEdited2 = new CStringBinding("cNumericEdited2", 16, false);
        cNumericEdited2.setCobolName("C-NUMERIC-EDITED-2");
        cNumericEdited3 = new CStringBinding("cNumericEdited3", 10, false);
        cNumericEdited3.setCobolName("C-NUMERIC-EDITED-3");
        cNumericEdited4 = new CStringBinding("cNumericEdited4", 11, false);
        cNumericEdited4.setCobolName("C-NUMERIC-EDITED-4");
        cIndex = new COctetStreamBinding("cIndex", 4);
        cIndex.setCobolName("C-INDEX");
        cPointer = new COctetStreamBinding("cPointer", 4);
        cPointer.setCobolName("C-POINTER");
        cProcPointer = new COctetStreamBinding("cProcPointer", 8);
        cProcPointer.setCobolName("C-PROC-POINTER");
        cFuncPointer = new COctetStreamBinding("cFuncPointer", 4);
        cFuncPointer.setCobolName("C-FUNC-POINTER");
        cExternalFloating = new CStringBinding("cExternalFloating", 10, false);
        cExternalFloating.setCobolName("C-EXTERNAL-FLOATING");
        cBinary = new CBinaryBinding("cBinary", 4, 9, 0, true);
        cBinary.setCobolName("C-BINARY");
        cNativeBinary = new CBinaryBinding("cNativeBinary", 2, 4, 0, false);
        cNativeBinary.setCobolName("C-NATIVE-BINARY");
        
        /* Add children to children list */
           
        getChildrenList().add(cAlphabetic);
        getChildrenList().add(cNational);
        getChildrenList().add(cDbcs);
        getChildrenList().add(cAlphanumericEdited);
        getChildrenList().add(cAlphanumeric);
        getChildrenList().add(cOctetString);
        getChildrenList().add(cSingleFloat);
        getChildrenList().add(cDoubleFloat);
        getChildrenList().add(cPackedDecimal);
        getChildrenList().add(cZonedDecimal);
        getChildrenList().add(cNumericEdited1);
        getChildrenList().add(cNumericEdited2);
        getChildrenList().add(cNumericEdited3);
        getChildrenList().add(cNumericEdited4);
        getChildrenList().add(cIndex);
        getChildrenList().add(cPointer);
        getChildrenList().add(cProcPointer);
        getChildrenList().add(cFuncPointer);
        getChildrenList().add(cExternalFloating);
        getChildrenList().add(cBinary);
        getChildrenList().add(cNativeBinary);
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
                     
        if (mJaxbObject.getCAlphabetic() != null) {
            /* Set value from cAlphabetic*/
            cAlphabetic.setValue(mJaxbObject.getCAlphabetic());
        }
                
        if (mJaxbObject.getCNational() != null) {
            /* Set value from cNational*/
            cNational.setValue(mJaxbObject.getCNational());
        }
                
        if (mJaxbObject.getCDbcs() != null) {
            /* Set value from cDbcs*/
            cDbcs.setValue(mJaxbObject.getCDbcs());
        }
                
        if (mJaxbObject.getCAlphanumericEdited() != null) {
            /* Set value from cAlphanumericEdited*/
            cAlphanumericEdited.setValue(mJaxbObject.getCAlphanumericEdited());
        }
                
        if (mJaxbObject.getCAlphanumeric() != null) {
            /* Set value from cAlphanumeric*/
            cAlphanumeric.setValue(mJaxbObject.getCAlphanumeric());
        }
                
        if (mJaxbObject.getCOctetString() != null) {
            /* Set value from cOctetString*/
            cOctetString.setValue(mJaxbObject.getCOctetString());
        }
                
        if (mJaxbObject.getCSingleFloat() != 0f) {
            /* Set value from cSingleFloat*/
            cSingleFloat.setValue(mJaxbObject.getCSingleFloat());
        }
                
        if (mJaxbObject.getCDoubleFloat() != 0d) {
            /* Set value from cDoubleFloat*/
            cDoubleFloat.setValue(mJaxbObject.getCDoubleFloat());
        }
                
        if (mJaxbObject.getCPackedDecimal() != null) {
            /* Set value from cPackedDecimal*/
            cPackedDecimal.setValue(mJaxbObject.getCPackedDecimal());
        
        }
                
        if (mJaxbObject.getCZonedDecimal() != 0L) {
            /* Set value from cZonedDecimal*/
            cZonedDecimal.setValue(new BigDecimal(mJaxbObject.getCZonedDecimal()));
        
        }
                
        if (mJaxbObject.getCNumericEdited1() != null) {
            /* Set value from cNumericEdited1*/
            cNumericEdited1.setValue(mJaxbObject.getCNumericEdited1());
        }
                
        if (mJaxbObject.getCNumericEdited2() != null) {
            /* Set value from cNumericEdited2*/
            cNumericEdited2.setValue(mJaxbObject.getCNumericEdited2());
        }
                
        if (mJaxbObject.getCNumericEdited3() != null) {
            /* Set value from cNumericEdited3*/
            cNumericEdited3.setValue(mJaxbObject.getCNumericEdited3());
        }
                
        if (mJaxbObject.getCNumericEdited4() != null) {
            /* Set value from cNumericEdited4*/
            cNumericEdited4.setValue(mJaxbObject.getCNumericEdited4());
        }
                
        if (mJaxbObject.getCIndex() != null) {
            /* Set value from cIndex*/
            cIndex.setValue(mJaxbObject.getCIndex());
        }
                
        if (mJaxbObject.getCPointer() != null) {
            /* Set value from cPointer*/
            cPointer.setValue(mJaxbObject.getCPointer());
        }
                
        if (mJaxbObject.getCProcPointer() != null) {
            /* Set value from cProcPointer*/
            cProcPointer.setValue(mJaxbObject.getCProcPointer());
        }
                
        if (mJaxbObject.getCFuncPointer() != null) {
            /* Set value from cFuncPointer*/
            cFuncPointer.setValue(mJaxbObject.getCFuncPointer());
        }
                
        if (mJaxbObject.getCExternalFloating() != null) {
            /* Set value from cExternalFloating*/
            cExternalFloating.setValue(mJaxbObject.getCExternalFloating());
        }
                
        if (mJaxbObject.getCBinary() != 0) {
            /* Set value from cBinary*/
            cBinary.setValue(new BigDecimal(mJaxbObject.getCBinary()));
        
        }
                
        if (mJaxbObject.getCNativeBinary() != 0) {
            /* Set value from cNativeBinary*/
            cNativeBinary.setValue(new BigDecimal(mJaxbObject.getCNativeBinary()));
        
        }
    
    }
       
    /** {@inheritDoc} */
    public final void setBoundObjectValue(final int index) throws HostException {
        /* Set the JAXB object property value from binding object */
        switch (index) {
        case 0:
            setBoundObjectValueCAlphabetic();
            break;
        case 1:
            setBoundObjectValueCNational();
            break;
        case 2:
            setBoundObjectValueCDbcs();
            break;
        case 3:
            setBoundObjectValueCAlphanumericEdited();
            break;
        case 4:
            setBoundObjectValueCAlphanumeric();
            break;
        case 5:
            setBoundObjectValueCOctetString();
            break;
        case 6:
            setBoundObjectValueCSingleFloat();
            break;
        case 7:
            setBoundObjectValueCDoubleFloat();
            break;
        case 8:
            setBoundObjectValueCPackedDecimal();
            break;
        case 9:
            setBoundObjectValueCZonedDecimal();
            break;
        case 10:
            setBoundObjectValueCNumericEdited1();
            break;
        case 11:
            setBoundObjectValueCNumericEdited2();
            break;
        case 12:
            setBoundObjectValueCNumericEdited3();
            break;
        case 13:
            setBoundObjectValueCNumericEdited4();
            break;
        case 14:
            setBoundObjectValueCIndex();
            break;
        case 15:
            setBoundObjectValueCPointer();
            break;
        case 16:
            setBoundObjectValueCProcPointer();
            break;
        case 17:
            setBoundObjectValueCFuncPointer();
            break;
        case 18:
            setBoundObjectValueCExternalFloating();
            break;
        case 19:
            setBoundObjectValueCBinary();
            break;
        case 20:
            setBoundObjectValueCNativeBinary();
            break;
        }
    }
            
    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCAlphabetic() throws HostException {
    
        if (cAlphabetic.getValue() != null) {
            /* Set value of cAlphabetic*/
            mJaxbObject.setCAlphabetic(cAlphabetic.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNational() throws HostException {
    
        if (cNational.getValue() != null) {
            /* Set value of cNational*/
            mJaxbObject.setCNational(cNational.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCDbcs() throws HostException {
    
        if (cDbcs.getValue() != null) {
            /* Set value of cDbcs*/
            mJaxbObject.setCDbcs(cDbcs.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCAlphanumericEdited() throws HostException {
    
        if (cAlphanumericEdited.getValue() != null) {
            /* Set value of cAlphanumericEdited*/
            mJaxbObject.setCAlphanumericEdited(cAlphanumericEdited.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCAlphanumeric() throws HostException {
    
        if (cAlphanumeric.getValue() != null) {
            /* Set value of cAlphanumeric*/
            mJaxbObject.setCAlphanumeric(cAlphanumeric.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCOctetString() throws HostException {
    
        if (cOctetString.getValue() != null) {
            /* Set value of cOctetString*/
            mJaxbObject.setCOctetString(cOctetString.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCSingleFloat() throws HostException {
    
        if (cSingleFloat.getValue() != null) {
            /* Set value of cSingleFloat*/
            mJaxbObject.setCSingleFloat(cSingleFloat.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCDoubleFloat() throws HostException {
    
        if (cDoubleFloat.getValue() != null) {
            /* Set value of cDoubleFloat*/
            mJaxbObject.setCDoubleFloat(cDoubleFloat.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCPackedDecimal() throws HostException {
    
        if (cPackedDecimal.getValue() != null) {
            
            /* Set value of cPackedDecimal*/
            mJaxbObject.setCPackedDecimal(cPackedDecimal.getValue());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCZonedDecimal() throws HostException {
    
        if (cZonedDecimal.getValue() != null) {
            
            /* Set value of cZonedDecimal*/
            mJaxbObject.setCZonedDecimal(cZonedDecimal.getValue().longValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNumericEdited1() throws HostException {
    
        if (cNumericEdited1.getValue() != null) {
            /* Set value of cNumericEdited1*/
            mJaxbObject.setCNumericEdited1(cNumericEdited1.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNumericEdited2() throws HostException {
    
        if (cNumericEdited2.getValue() != null) {
            /* Set value of cNumericEdited2*/
            mJaxbObject.setCNumericEdited2(cNumericEdited2.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNumericEdited3() throws HostException {
    
        if (cNumericEdited3.getValue() != null) {
            /* Set value of cNumericEdited3*/
            mJaxbObject.setCNumericEdited3(cNumericEdited3.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNumericEdited4() throws HostException {
    
        if (cNumericEdited4.getValue() != null) {
            /* Set value of cNumericEdited4*/
            mJaxbObject.setCNumericEdited4(cNumericEdited4.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCIndex() throws HostException {
    
        if (cIndex.getValue() != null) {
            /* Set value of cIndex*/
            mJaxbObject.setCIndex(cIndex.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCPointer() throws HostException {
    
        if (cPointer.getValue() != null) {
            /* Set value of cPointer*/
            mJaxbObject.setCPointer(cPointer.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCProcPointer() throws HostException {
    
        if (cProcPointer.getValue() != null) {
            /* Set value of cProcPointer*/
            mJaxbObject.setCProcPointer(cProcPointer.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCFuncPointer() throws HostException {
    
        if (cFuncPointer.getValue() != null) {
            /* Set value of cFuncPointer*/
            mJaxbObject.setCFuncPointer(cFuncPointer.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCExternalFloating() throws HostException {
    
        if (cExternalFloating.getValue() != null) {
            /* Set value of cExternalFloating*/
            mJaxbObject.setCExternalFloating(cExternalFloating.getValue());
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCBinary() throws HostException {
    
        if (cBinary.getValue() != null) {
            
            /* Set value of cBinary*/
            mJaxbObject.setCBinary(cBinary.getValue().intValueExact());
                
        }
        
    }

    /** Set corresponding JAXB object property value. */
    private final void setBoundObjectValueCNativeBinary() throws HostException {
    
        if (cNativeBinary.getValue() != null) {
            
            /* Set value of cNativeBinary*/
            mJaxbObject.setCNativeBinary(cNativeBinary.getValue().intValueExact());
                
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
