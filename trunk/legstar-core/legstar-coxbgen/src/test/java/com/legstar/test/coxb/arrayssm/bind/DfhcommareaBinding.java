


package com.legstar.test.coxb.arrayssm.bind;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.common.CComplexBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.CobolBindingFactory;
import com.legstar.coxb.ICobolBindingFactory;
import java.util.List;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.test.coxb.arrayssm.TableComplex;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.test.coxb.arrayssm.TableComplex2;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.host.HostException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.test.coxb.arrayssm.Dfhcommarea;
import com.legstar.test.coxb.arrayssm.ObjectFactory;

/**
 * LegStar Binding for Complex element :
 *   Dfhcommarea.
 * 
 * This class was generated by LegStar Binding generator.
 */
public class DfhcommareaBinding 
             extends CComplexBinding {

    /** Value object to which this cobol complex element is bound. */
    private Dfhcommarea mValueObject;
  
    /** Indicates that the associated Value object just came from the constructor
     * and doesn't need to be recreated. */
    private boolean mUnusedValueObject = false;
    
    /** Maximum host bytes size for this complex object. */
    private static final int BYTE_LENGTH = 54;
    
    /** Child bound to value object property TableSimple(String). */
    public ICobolArrayStringBinding _tableSimple;
    /** Child bound to value object property TableComplex(TableComplex). */
    public ICobolArrayComplexBinding _tableComplexWrapper;
    /** Binding item for complex array binding TableComplex. */
    public ICobolComplexBinding _tableComplexWrapperItem;
    /** Child bound to value object property TableComplex2(TableComplex2). */
    public ICobolComplexBinding _tableComplex2;
    /** Child bound to value object property TableSimpleNumeric(Integer). */
    public ICobolArrayZonedDecimalBinding _tableSimpleNumeric;
            
    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Binding factory. */
    private static final ICobolBindingFactory BF
        = CobolBindingFactory.getBindingFactory();
 
    /** Static reference to Value object factory to be used as default. */
    private static final ObjectFactory JF = new ObjectFactory();
    
    /** Current Value object factory (Defaults to the static one but can be
     *  changed). */
    private ObjectFactory mValueObjectFactory = JF;
    
    /**
     * Constructor for a root Complex element without a bound Value object.
     */
    public DfhcommareaBinding() {
        this(null);
    }

    /**
     * Constructor for a root Complex element with a bound Value object.
     * 
     * @param valueObject the concrete Value object instance bound to this
     *        complex element
     */
    public DfhcommareaBinding(
            final Dfhcommarea valueObject) {
        this("", "", null, valueObject);
    }

    /**
    * Constructor for a Complex element as a child of another element and
    * an associated Value object.
    * 
    * @param bindingName the identifier for this binding
    * @param fieldName field name in parent Value object
    * @param valueObject the concrete Value object instance bound to this
    *        complex element
    * @param parentBinding a reference to the parent binding
    */
    public DfhcommareaBinding(
            final String bindingName,
            final String fieldName,
            final ICobolComplexBinding parentBinding,
            final Dfhcommarea valueObject) {
        
        super(bindingName, fieldName, Dfhcommarea.class, null, parentBinding);
        mValueObject = valueObject;
        if (mValueObject != null) {
            mUnusedValueObject = true;
        }
        initChildren();
        setByteLength(BYTE_LENGTH);
    }

    /** Creates a binding property for each child. */
    private void initChildren() {
        if (_log.isDebugEnabled()) {
            _log.debug("Initializing started");
        }
        /* Create binding children instances */

        _tableSimple = BF.createArrayStringBinding("TableSimple",
               "TableSimple", String.class, this);
        _tableSimple.setCobolName("TABLE-SIMPLE");
        _tableSimple.setByteLength(6);
        _tableSimple.setItemByteLength(3);
        _tableSimple.setMinOccurs(2);
        _tableSimple.setMaxOccurs(2);
        _tableComplexWrapperItem = new TableComplexBinding("TableComplexWrapperItem",
               "TableComplex", this, null);
        _tableComplexWrapper = new TableComplexWrapperBinding("TableComplexWrapper",
               "TableComplex", this, _tableComplexWrapperItem);
        _tableComplexWrapper.setCobolName("TABLE-COMPLEX");
        _tableComplexWrapper.setByteLength(15);
        _tableComplexWrapper.setItemByteLength(5);
        _tableComplexWrapper.setMinOccurs(3);
        _tableComplexWrapper.setMaxOccurs(3);
        _tableComplex2 = new TableComplex2Binding("TableComplex2",
               "TableComplex2", this, null);
        _tableComplex2.setCobolName("TABLE-COMPLEX-2");
        _tableComplex2.setByteLength(28);
        _tableSimpleNumeric = BF.createArrayZonedDecimalBinding("TableSimpleNumeric",
               "TableSimpleNumeric", Integer.class, this);
        _tableSimpleNumeric.setCobolName("TABLE-SIMPLE-NUMERIC");
        _tableSimpleNumeric.setByteLength(5);
        _tableSimpleNumeric.setItemByteLength(1);
        _tableSimpleNumeric.setTotalDigits(1);
        _tableSimpleNumeric.setMinOccurs(5);
        _tableSimpleNumeric.setMaxOccurs(5);

        /* Add children to children list */
        getChildrenList().add(_tableSimple);
        getChildrenList().add(_tableComplexWrapper);
        getChildrenList().add(_tableComplex2);
        getChildrenList().add(_tableSimpleNumeric);
 
        if (_log.isDebugEnabled()) {
            _log.debug("Initializing successful");
        }
    }
    
    /** {@inheritDoc} */
    public void createValueObject() throws HostException {
        /* Since this complex binding has a constructor that takes a
         * Value object, we might already have a Value object that
         * was not used yet. */
        if (mUnusedValueObject && mValueObject != null) {
            mUnusedValueObject = false;
            return;
        }
        mValueObject = mValueObjectFactory.createDfhcommarea();
    }

    /** {@inheritDoc} */
    public void setChildrenValues() throws HostException {

         /* Make sure there is an associated Value object*/
        if (mValueObject == null) {
            createValueObject();
        }
        /* Get Value object property _tableSimple */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_tableSimple"
                    + " value=" + mValueObject.getTableSimple());
        }
        _tableSimple.setObjectValue(mValueObject.getTableSimple());
        /* Get Value object property _tableComplexWrapper */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_tableComplexWrapper"
                    + " value=" + mValueObject.getTableComplex());
        }
        _tableComplexWrapper.setObjectValue(mValueObject.getTableComplex());
        /* Get Value object property _tableComplex2 */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_tableComplex2"
                    + " value=" + mValueObject.getTableComplex2());
        }
        _tableComplex2.setObjectValue(mValueObject.getTableComplex2());
        /* Get Value object property _tableSimpleNumeric */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_tableSimpleNumeric"
                    + " value=" + mValueObject.getTableSimpleNumeric());
        }
        _tableSimpleNumeric.setObjectValue(mValueObject.getTableSimpleNumeric());
     }

    /** {@inheritDoc} */
    public void setPropertyValue(final int index) throws HostException {
 
        ICobolBinding child = getChildrenList().get(index);
        
       /* Children that are not bound to a value object are ignored.
        * This includes Choices and dynamically generated counters
        * for instance.  */
        if (!child.isBound()) {
            return;
        }
        
        /* Set the Value object property value from binding object */
        Object bindingValue = null;
        switch (index) {
        case 0:
            bindingValue = child.getObjectValue(String.class);
            List < String > listTableSimple = cast(bindingValue);
            mValueObject.getTableSimple().clear();
            mValueObject.getTableSimple().addAll(listTableSimple);
            break;
        case 1:
            bindingValue = child.getObjectValue(TableComplex.class);
            List < TableComplex > listTableComplexWrapper = cast(bindingValue);
            mValueObject.getTableComplex().clear();
            mValueObject.getTableComplex().addAll(listTableComplexWrapper);
            break;
        case 2:
            bindingValue = child.getObjectValue(TableComplex2.class);
            mValueObject.setTableComplex2((TableComplex2) bindingValue);
            break;
        case 3:
            bindingValue = child.getObjectValue(Integer.class);
            List < Integer > listTableSimpleNumeric = cast(bindingValue);
            mValueObject.getTableSimpleNumeric().clear();
            mValueObject.getTableSimpleNumeric().addAll(listTableSimpleNumeric);
            break;
         default:
            break;
        }
        if (_log.isDebugEnabled()) {
            _log.debug("Setting value of Value object property "
                    + child.getJaxbName()
                    + " value=" + bindingValue);
        }
    }

    /** {@inheritDoc} */
    public Object getObjectValue(
            final Class < ? > type) throws HostException {
        if (type.equals(Dfhcommarea.class)) {
            return mValueObject;
        } else {
            throw new HostException("Attempt to get binding " + getBindingName()
                    + " as an incompatible type " + type);
        }
    }

    /** {@inheritDoc} */
    public void setObjectValue(
            final Object bindingValue) throws HostException {
        if (bindingValue == null) {
            mValueObject = null;
            return;
        }
        if (bindingValue.getClass().equals(Dfhcommarea.class)) {
            mValueObject = (Dfhcommarea) bindingValue;
        } else {
            throw new HostException("Attempt to set binding " + getBindingName()
                    + " from an incompatible value " + bindingValue);
        }
    }

    /**
     * @return the java object factory for objects creation
     */
    public ObjectFactory getObjectFactory() {
        return mValueObjectFactory;
    }

    /**
     * @param valueObjectFactory the java object factory for objects creation 
     */
    public void setObjectFactory(final Object valueObjectFactory) {
        mValueObjectFactory = (ObjectFactory) valueObjectFactory;
    }

    /** {@inheritDoc} */
    public boolean isSet() {
        return (mValueObject != null);
    }

    /**
     * @return the bound Value object
     */
    public Dfhcommarea getDfhcommarea() {
        return mValueObject;
    }
    
    /**
     * The COBOL complex element maximum length in bytes.
     * 
     * @return COBOL complex element maximum length in bytes
     */
    public int getByteLength() {
        return BYTE_LENGTH;
    }
}
