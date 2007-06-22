
package com.legstar.coxb.impl.reflect;

import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;
import com.legstar.binding.CobolElement;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.impl.CComplexBinding;
import com.legstar.coxb.impl.CStringBinding;
import com.legstar.coxb.impl.CArrayStringBinding;
import com.legstar.coxb.impl.CArrayNationalBinding;
import com.legstar.coxb.impl.CNationalBinding;
import com.legstar.coxb.impl.CArrayPackedDecimalBinding;
import com.legstar.coxb.impl.CPackedDecimalBinding;
import com.legstar.coxb.impl.CArrayZonedDecimalBinding;
import com.legstar.coxb.impl.CZonedDecimalBinding;
import com.legstar.coxb.impl.COctetStreamBinding;
import com.legstar.coxb.impl.CArrayOctetStreamBinding;
import com.legstar.coxb.impl.CArrayBinaryBinding;
import com.legstar.coxb.impl.CBinaryBinding;
import com.legstar.coxb.impl.CFloatBinding;
import com.legstar.coxb.impl.CArrayFloatBinding;
import com.legstar.coxb.impl.CDoubleBinding;
import com.legstar.coxb.impl.CArrayDoubleBinding;
import com.legstar.coxb.impl.RedefinesMap;

import java.lang.reflect.Field;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * This class implements a bi-directional binding between a cobol structure and
 * a java object. Visitors can use this class to visit each element of the
 * structure in turn. Reflexion is used on the Java object to infer a 
 * list of children.
 */

public class CComplexReflectBinding extends CComplexBinding {
  
	/** Reference to a JAXB object factory. This is needed because this class
	 * might need to create JAXB objects. */
	private Object mJaxbObjectFactory;
	
    /** Java object to which this cobol complex element is bound. */
    private Object mJaxbObject;
  
    /** Indicates that the associated Jaxb object just came from the constructor
     * and doesn't need to be recreated. */
    private boolean mUnusedJaxbObject = false;
    
	/** Logger. */
	private static final Log LOG
		= LogFactory.getLog(CComplexReflectBinding.class);
	
	/**
	 * Constructor for a root Complex element with a bound JAXB object.
	 * 
	 * @param jaxbObjectFactory the JAXB object factory
	 * @param jaxbObject the concrete JAXB object instance bound to this
	 *        complex element
	 * @throws ReflectBindingException if construction fails
	 */
	public CComplexReflectBinding(
			final Object jaxbObjectFactory,
			final Object jaxbObject)
		throws ReflectBindingException {
		
		this(jaxbObjectFactory, jaxbObject, null, "");
	}

	/**
	 * Constructor for a Complex element as a child of another element and
	 * an associated JAXB object.
	 * 
	 * @param jaxbObjectFactory the JAXB object factory
	 * @param jaxbObject the concrete JAXB object instance bound to this
	 *        complex element
	 * @param parentBinding a reference to the parent binding
	 * @param jaxbName name of field in parent JAXB object
	 * @throws ReflectBindingException if construction fails
	 */
	public CComplexReflectBinding(
			final Object jaxbObjectFactory,
			final Object jaxbObject,
			final CComplexReflectBinding parentBinding,
			final String jaxbName)
		throws ReflectBindingException {
		
		super(jaxbName, null, parentBinding);
		mJaxbObject = jaxbObject;
		mUnusedJaxbObject = true;
		mJaxbObjectFactory = jaxbObjectFactory;
		initComplexElement(jaxbObject.getClass());
	}

	/**
	 * Constructor for a root Complex element knowing the bound JAXB class
	 * name.
	 * 
	 * @param jaxbObjectFactory the JAXB object factory
	 * @param jaxbType JAXB type of complex field
	 * @throws ReflectBindingException if construction fails
	 */
	public CComplexReflectBinding(
			final Object jaxbObjectFactory,
			final Class jaxbType)
		throws ReflectBindingException {
		
		this(jaxbObjectFactory, jaxbType, null, "");
	}

	/**
	 * Constructor for a child Complex element knowing the bound JAXB class
	 * name.
	 * 
	 * @param jaxbObjectFactory the JAXB object factory
	 * @param jaxbType JAXB type of complex field
	 * @param parentBinding a reference to the parent binding
	 * @param jaxbName name of field in parent JAXB object
	 * @throws ReflectBindingException if construction fails
	 */
	public CComplexReflectBinding(
			final Object jaxbObjectFactory,
			final Class jaxbType,
			final CComplexReflectBinding parentBinding,
			final String jaxbName)
		throws ReflectBindingException {
		
		super(jaxbName, jaxbType, parentBinding);
		mJaxbObjectFactory = jaxbObjectFactory;
		initComplexElement(jaxbType);
	}

	/**
	 * Helper method. JAXB Types are annotated with an XmlType which gives
	 * an ordered list of properties
	 * @param jaxbClass the JAXB Class with annotations
	 * @throws ReflectBindingException if initialisation fails
	 */
	@SuppressWarnings("unchecked")
	private void initComplexElement(
			final Class jaxbClass) throws ReflectBindingException {

		if (LOG.isDebugEnabled()) {
			LOG.debug("Initializing Complex binding for " + jaxbClass);
		}
		XmlType xmlType = (XmlType) jaxbClass.getAnnotation(XmlType.class);
		if (xmlType == null) {
			throw new ReflectBindingException(
					"No jaxb annotations found in " + jaxbClass);
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Found JAXB annotations: " + xmlType.toString());
		}
		setJavaType(jaxbClass);
		/* If no name was assigned (root element) use type name as name */
		if (getJavaName() == null || getJavaName().length() == 0) {
			setJavaName(xmlType.name());
		}
		initChildren(jaxbClass, xmlType);

		if (LOG.isDebugEnabled()) {
			LOG.debug(
				"Complex binding sucessfully initialized for: " + jaxbClass);
		}
	}
  
    /**
     * Creates a binding property for each child. 
	 * @param jaxbClass the JAXB Class with annotations
	 * @param xmlType the JAXB annotations
     * @throws ReflectBindingException if children bindings fail
     *  */
    public final void initChildren(
    		final Class jaxbClass,
    		final XmlType xmlType) throws ReflectBindingException {
    
		if (LOG.isDebugEnabled()) {
			LOG.debug("Initializing children of: " + xmlType.name());
		}
		/* Map of choice elements for redefined elements */
		RedefinesMap redefinesMap = new RedefinesMap();
		
		/* Process each property of this complex type in the predefined order */
		for (String prop : xmlType.propOrder()) {

			/* Get a reference to this property field and type */
			Field hostField;
			try {
				hostField = jaxbClass.getDeclaredField(prop);
			} catch (SecurityException e) {
				throw new ReflectBindingException(e);
			} catch (NoSuchFieldException e) {
				throw new ReflectBindingException(e);
			}

			/* Get the cobol annotations for that field */
			CobolElement cobolAnnotations =
				hostField.getAnnotation(CobolElement.class);
			if (cobolAnnotations == null) {
				throw new ReflectBindingException(
						"No cobol annotations found for field "
						+ hostField.getName());
			}
			
			if (LOG.isDebugEnabled()) {
				LOG.debug("Processing Cobol annotations for: "
						+ cobolAnnotations.cobolName());
				LOG.debug("Cobol annotations: " + cobolAnnotations);
			}
			
			/* Get the xml annotations for that field. This is necessary because
			 * the name on the Xml annotation corresponds to the JAXB getter/
			 * setter methods while the property name, returned by
			 * xmlType.propOrder() might be different. */
			XmlElement xmlAnnotation =
				hostField.getAnnotation(XmlElement.class);
			if (xmlAnnotation == null) {
				throw new ReflectBindingException(
						"No xml annotations found for field "
						+ hostField.getName());
			}
			ICobolBinding binding = createBinding(
					xmlAnnotation.name(), getJavaClass(hostField),
					cobolAnnotations, redefinesMap);
			/* In the case of redefines, no actual binding is created for a
			 * redefining item since all alternatives share the same choice
			 * binding. Hence the possibility of a null. */
			if (binding != null) {
				getChildrenList().add(binding);
			}
			
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Children sucessfully initialized for: "
					+ xmlType.name());
		}
    }
    
    /**
     * This method determines the relevant java type to be stored in a binding
     * element. In case of list items, we seek the items types rather than the
     * generic java.util.List type.
     * @param hostField field from which java type is extracted
     * @return the java type class
     * @throws ReflectBindingException if class cannot be determined
     */
    private Class getJavaClass(
    		final Field hostField) throws ReflectBindingException {
    	Class javaClass = hostField.getType();
    	if (javaClass.getName().compareTo("java.util.List") == 0) {
    		String javaTypeName = hostField.getGenericType().toString();
    		javaTypeName = javaTypeName.substring(
    				javaTypeName.indexOf("<") + 1, javaTypeName.length() - 1);
    		try {
				javaClass = Class.forName(javaTypeName);
			} catch (ClassNotFoundException e) {
				throw new ReflectBindingException(e);
			}
    	}
    	return javaClass;
    	
    }
  
	/**
	 * Based on the cobol annotations for that field, we create a corresponding
	 * element type. If this element is redefined we create a choice element
	 * and return it instead of the the redefined element itself. 
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param redefinesMap the current list of redefined items
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations,
			final RedefinesMap redefinesMap)
		throws ReflectBindingException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Binding cobol element "
					+ cobolAnnotations.cobolName()
					+ '(' + cobolAnnotations.type() + ')'
					+ " to java property " + jaxbName
					+ '(' + jaxbType.getName() + ')');
		}
		ICobolBinding cobolElement = null;
		
		switch (cobolAnnotations.type()) {
		case GROUP_ITEM:
			cobolElement = createComplexBinding(
					jaxbName, jaxbType,	cobolAnnotations);
			break;
		case ALPHABETIC_ITEM:
		case ALPHANUMERIC_EDITED_ITEM:
		case ALPHANUMERIC_ITEM:
		case NUMERIC_EDITED_ITEM:
		case EXTERNAL_FLOATING_ITEM:
			cobolElement = createStringBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case NATIONAL_ITEM:
			cobolElement = createNationalBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case PACKED_DECIMAL_ITEM:
			cobolElement = createPackedDecimalBinding(
					jaxbName, jaxbType,	cobolAnnotations);
			break;
		case ZONED_DECIMAL_ITEM:
			cobolElement = createZonedDecimalBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case DBCS_ITEM:
		case OCTET_STREAM_ITEM:
		case INDEX_ITEM:
		case POINTER_ITEM:
		case PROC_POINTER_ITEM:
		case FUNC_POINTER_ITEM:
			cobolElement = createOctetStreamBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case BINARY_ITEM:
		case NATIVE_BINARY_ITEM:
			cobolElement = createBinaryBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case SINGLE_FLOAT_ITEM:
			cobolElement = createFloatBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		case DOUBLE_FLOAT_ITEM:
			cobolElement = createDoubleBinding(
					jaxbName, jaxbType, cobolAnnotations);
			break;
		default:
			throw (new ReflectBindingException(
					"Unrecognized cobol type for field "
					+ cobolAnnotations.cobolName()));
		}
		
		/* If this element is part of a redefinition group (either redefines
		 * another element or is redefined by another element) we need
		 * further processing. */
		String redefines = cobolAnnotations.redefines();
		if ((cobolAnnotations.isRedefined())
				|| (redefines != null
						&& redefines.length() > 0)) {
			return createChoiceBinding(jaxbName, jaxbType,
					cobolAnnotations, cobolElement, redefinesMap);
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Binding created");
		}
		return cobolElement;
	}
   
	/**
	 * Create a group element type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createComplexBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			/* A single complex binding is used for all items */ 
			ICobolComplexBinding item = new CComplexReflectBinding(
					mJaxbObjectFactory, jaxbType, this, jaxbName);
			return new CArrayComplexReflectBinding(
					mJaxbObjectFactory, jaxbName, jaxbType,
					cobolAnnotations, item);
		} else {
			return new CComplexReflectBinding(
					mJaxbObjectFactory, jaxbType, this, jaxbName);
		}
	}
	
	/**
	 * Create a choice element type which will group all alternatives. The
	 * choice element replaces the redefined element in the parent hierarchy.
	 * All alternative (redefining elements) are then added to the choice.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param cobolElement the cobol descriptor for this element
	 * @param redefinesMap the current list of redefined items
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol binding cannot be created
	 */
	private ICobolBinding createChoiceBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolBinding cobolElement,
			final RedefinesMap redefinesMap)
		throws ReflectBindingException {
		
		if (LOG.isDebugEnabled()) {
			if (cobolAnnotations.isRedefined()) {
				LOG.debug("Creating Choice binding for redefined Cobol element "
						+ cobolAnnotations.cobolName());
			} else {
				LOG.debug("Adding "	+ cobolAnnotations.cobolName()
						+ " to Choice binding for Cobol element "
						+ cobolAnnotations.redefines());
			}
		}
		/* If this is a redefined item, then we need to create a special choice
		 * element which will group all alternatives */
		if (cobolAnnotations.isRedefined()) {
			
			CChoiceReflectBinding choice = new CChoiceReflectBinding(
						jaxbName, jaxbType, this, cobolAnnotations);
			
			/* Add the redefined item as the first alternative in the
			 * choice element */
			choice.addAlternative(cobolElement);
			
			/* Add this choice element in the redefines map */
			redefinesMap.updateChoiceElement(
					cobolAnnotations.cobolName(), choice);
			
			if (LOG.isDebugEnabled()) {
				LOG.debug("Choice binding created");
			}
			/* Return the choice as the current element for caller */
			return choice;
			
		}
			
		/* This is a redefinition of an existing element, we need to add
		 * this alternative to the redefined element list of alternatves */
		/* lookup the redefines Map to locate the choice element we are
		 * part of */
		ICobolChoiceBinding choice =
			redefinesMap.getChoiceElement(cobolAnnotations.redefines());
		if (choice == null) {
			/* If the redefined element has not been processed, this means
			 * it was not created by the caller (he already made a choice).
			 * In this case, the current element is the first of the possible
			 * alternatives. */
			choice =
				new CChoiceReflectBinding(
						jaxbName, jaxbType, this, cobolAnnotations);
			
			/* Add the redefined item as the first alternative in the
			 * choice element */
			choice.addAlternative(cobolElement);
			
			/* Add this choice element in the redefines map under the name of
			 * the redefined element.  */
			redefinesMap.updateChoiceElement(
					cobolAnnotations.redefines(), choice);
			
			/* Return the choice as the current element for caller */
			return choice;
		}
		
		/* Add the redefining item to the alternative list in the
		 * choice element */
		choice.addAlternative(cobolElement);
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Choice binding updated");
		}
		/* Since choice element is already part of parent children,
		 * return null to avoid adding it twice */
		return null;
		
	}
	
	/**
	 * Create a String binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createStringBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayStringBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CStringBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}
	
	/**
	 * Create a OctetStream binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createOctetStreamBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayOctetStreamBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new COctetStreamBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}

	/**
	 * Create a National binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createNationalBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayNationalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CNationalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}
	
	/**
	 * Create a PackedDecimal binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding  createPackedDecimalBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayPackedDecimalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CPackedDecimalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}
	
	/**
	 * Create a ZonedDecimal binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding  createZonedDecimalBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayZonedDecimalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CZonedDecimalBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}

	/**
	 * Create a Binary binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding  createBinaryBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayBinaryBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CBinaryBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}
	
	/**
	 * Create a Float binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createFloatBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayFloatBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CFloatBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}

	/**
	 * Create a Double binding type.
	 * 
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws ReflectBindingException if cobol description cannot be created
	 */
	private ICobolBinding createDoubleBinding(
			final String jaxbName,
			final Class jaxbType,
			final CobolElement cobolAnnotations)
		throws ReflectBindingException {
		
		if (cobolAnnotations.maxOccurs() > 0) { 
			return new CArrayDoubleBinding(
					jaxbName, jaxbType, cobolAnnotations);
		} else {
			return new CDoubleBinding(
					jaxbName, jaxbType, cobolAnnotations);
		}
	}

	/** {@inheritDoc} */
    public final void createJaxbObject() throws HostException {
    	/* Since this complex binding has a constructor that takes a
    	 * JAXB object, we might already have a Jaxb object that
    	 * was not used yet. */
    	if (mUnusedJaxbObject && mJaxbObject != null) {
    		mUnusedJaxbObject = false;
    		return;
    	}
    	mJaxbObject = JaxbUtil.createComplexProperty(mJaxbObjectFactory,
    			getJavaType().getName());
    }
       
    /** {@inheritDoc} */
    public final void setChildrenValues() throws HostException {
    	
        /* Make sure there is an associated JAXB object*/
    	if (mJaxbObject == null) {
    		createJaxbObject();
    	}
    	
    	/* Set this binding properties from java object property values */
        for (ICobolBinding child : getChildrenList()) {
         	/* Choice children are a special case. They directly set 
         	 * their parent object depending on the chosen choice
         	 * strategy. */
        	if (child instanceof ICobolChoiceBinding) {
        		continue;
        	} else {
	        	Object value = JaxbUtil.invokeGetProperty(mJaxbObject,
	        			child.getJavaName());
        		if (LOG.isDebugEnabled()) {
        			LOG.debug("Getting value from JAXB property "
        					+ child.getJavaName()
        					+ " value=" + value);
        		}
        		child.setObjectValue(value);
        	}
        }
    }
    
    /** {@inheritDoc} */
    public final void setJaxbPropertyValue(
    		final int index) throws HostException {
    	
     	ICobolBinding child = getChildrenList().get(index);
     	
     	/* Choice children are a special case. They directly set 
     	 * their parent object depending on the chosen choice
     	 * strategy. */
    	if (child instanceof ICobolChoiceBinding) {
    		return;
    	}
    	
		Object value = child.getObjectValue(child.getJavaType());
    	if (LOG.isDebugEnabled()) {
			LOG.debug("Setting value of JAXB property "
					+ child.getJavaName()
					+ " value=" + value);
		}
		JaxbUtil.invokeSetProperty(mJaxbObject, child.getJavaName(),
				value, child.getJavaType());
    }
            

    /** {@inheritDoc} */
    public final Object getObjectValue(final Class type) throws HostException {
    	if (type.equals(getJavaType())) {
    		return mJaxbObject;
		} else {
			throw new HostException("Attempt to get binding " + getJavaName()
					+ " as an incompatible type " + type);
    	}
    }

    /** {@inheritDoc} */
    public final void setObjectValue(final Object value) throws HostException {
    	if (value == null) {
    		mJaxbObject = null;
    		return;
    	}
    	if (value.getClass().equals(getJavaType())) {
    		mJaxbObject = value;
    	} else {
			throw new HostException("Attempt to set binding " + getJavaName()
					+ " from an incompatible value " + value);
    	}
    }

    /**
     * @return the java object factory for objects creation
     */
    public final Object getObjectFactory() {
        return mJaxbObjectFactory;
    }

    /**
     * @param jaxbObjectFactory the java object factory for objects creation 
     */
    public final void setObjectFactory(final Object jaxbObjectFactory) {
    	mJaxbObjectFactory = jaxbObjectFactory;
    }

    /** {@inheritDoc} */
	public final boolean isSet() {
		return (mJaxbObject != null);
	}

}
