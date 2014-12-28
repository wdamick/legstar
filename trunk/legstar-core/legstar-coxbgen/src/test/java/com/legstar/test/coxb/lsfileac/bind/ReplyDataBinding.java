


package com.legstar.test.coxb.lsfileac.bind;

import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.common.CComplexBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.CobolBindingFactory;
import com.legstar.coxb.ICobolBindingFactory;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.test.coxb.lsfileac.ReplyItem;
import java.util.List;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.test.coxb.lsfileac.ReplyData;
import com.legstar.test.coxb.lsfileac.ObjectFactory;

/**
 * LegStar Binding for Complex element :
 *   ReplyData.
 * 
 * This class was generated by LegStar Binding generator.
 */
public class ReplyDataBinding 
             extends CComplexBinding {

    /** Value object to which this cobol complex element is bound. */
    private ReplyData mValueObject;
  
    /** Indicates that the associated Value object just came from the constructor
     * and doesn't need to be recreated. */
    private boolean mUnusedValueObject = false;
    
    /** Maximum host bytes size for this complex object. */
    public static final int BYTE_LENGTH = 7905;
    
    /** Child bound to value object property ReplyItemscount(Long). */
    public ICobolPackedDecimalBinding _replyItemscount;
    /** Child bound to value object property ReplyItem(ReplyItem). */
    public ICobolArrayComplexBinding _replyItemWrapper;
    /** Binding item for complex array binding ReplyItem. */
    public ICobolComplexBinding _replyItemWrapperItem;
            
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
    public ReplyDataBinding() {
        this(null);
    }

    /**
     * Constructor for a root Complex element with a bound Value object.
     * 
     * @param valueObject the concrete Value object instance bound to this
     *        complex element
     */
    public ReplyDataBinding(
            final ReplyData valueObject) {
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
    public ReplyDataBinding(
            final String bindingName,
            final String fieldName,
            final ICobolComplexBinding parentBinding,
            final ReplyData valueObject) {
        
        super(bindingName, fieldName, ReplyData.class, null, parentBinding);
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

        _replyItemscount = BF.createPackedDecimalBinding("ReplyItemscount",
               "ReplyItemscount", Long.class, this);
        _replyItemscount.setCobolName("REPLY-ITEMSCOUNT");
        _replyItemscount.setByteLength(5);
        _replyItemscount.setTotalDigits(8);
        _replyItemscount.setIsODOObject(true);
        _replyItemWrapperItem = new ReplyItemBinding("ReplyItemWrapperItem",
               "ReplyItem", this, null);
        _replyItemWrapper = new ReplyItemWrapperBinding("ReplyItemWrapper",
               "ReplyItem", this, _replyItemWrapperItem);
        _replyItemWrapper.setCobolName("REPLY-ITEM");
        _replyItemWrapper.setByteLength(7900);
        _replyItemWrapper.setItemByteLength(79);
        _replyItemWrapper.setMinOccurs(1);
        _replyItemWrapper.setMaxOccurs(100);
        _replyItemWrapper.setDependingOn("REPLY-ITEMSCOUNT");

        /* Add children to children list */
        getChildrenList().add(_replyItemscount);
        getChildrenList().add(_replyItemWrapper);
 
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
        mValueObject = mValueObjectFactory.createReplyData();
    }

    /** {@inheritDoc} */
    public void setChildrenValues() throws HostException {

         /* Make sure there is an associated Value object*/
        if (mValueObject == null) {
            createValueObject();
        }
        /* Get Value object property _replyItemscount */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_replyItemscount"
                    + " value=" + mValueObject.getReplyItemscount());
        }
        _replyItemscount.setObjectValue(mValueObject.getReplyItemscount());
        /* Get Value object property _replyItemWrapper */
        if (_log.isDebugEnabled()) {
            _log.debug("Getting value from Value object property "
                    + "_replyItemWrapper"
                    + " value=" + mValueObject.getReplyItem());
        }
        _replyItemWrapper.setObjectValue(mValueObject.getReplyItem());
        /* For variable size array or list, we make sure any
         * associated counter is updated */
        setCounterValue(_replyItemWrapper.getDependingOn(),
                ((List < ? >) mValueObject.getReplyItem()).size());
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
            bindingValue = child.getObjectValue(Long.class);
            mValueObject.setReplyItemscount((Long) bindingValue);
            break;
        case 1:
            bindingValue = child.getObjectValue(ReplyItem.class);
            List < ReplyItem > listReplyItemWrapper = cast(bindingValue);
            mValueObject.getReplyItem().clear();
            mValueObject.getReplyItem().addAll(listReplyItemWrapper);
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
        if (type.equals(ReplyData.class)) {
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
        if (bindingValue.getClass().equals(ReplyData.class)) {
            mValueObject = (ReplyData) bindingValue;
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
    public ReplyData getReplyData() {
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

