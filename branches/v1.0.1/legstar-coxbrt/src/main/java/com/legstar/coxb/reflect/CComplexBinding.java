/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.reflect;

import java.lang.reflect.Field;
import java.math.BigDecimal;


import javax.xml.bind.annotation.XmlType; 

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.annotation.CobolElement;
import com.legstar.host.HostException;
import com.legstar.util.JaxbUtil;
import com.legstar.util.RedefinesMap;

/**
 * Cobol/JAXB implementation of a complex (record) data description. This class
 * holds a reference to a JAXB object and implements an 'accept' method 
 * conformant to the visitor pattern.
 *
 * @author Fady Moussallam
 * 
 */
public class CComplexBinding
		extends CBinding
		implements ICobolComplexBinding {
	
	/** Reference to a JAXB object factory. This is needed because this class
	 * might need to create JAXB objects. */
	private Object mJaxbObjectFactory;
	
	/** Reference to a JAXB XmlType annotation for this type. */
	private XmlType mXmlType;
	
	/** Ordered list of child elements. */
	private java.util.List < ICobolBinding > mChildren;
	
	/** When element bound belongs to a hierachy, this references the parent 
	 * binding. */
	private CComplexBinding mParentBinding;
	
	/**
	 * Constructor for a root Complex element.
	 * 
	 * @param objectFactory the JAXB object factory
	 * @param jaxbObject the concrete JAXB object instance bound to this
	 *        complex element
	 * @throws HostException if construction fails
	 */
	public CComplexBinding(
			final Object objectFactory,
			final Object jaxbObject)
		throws HostException {
		
		/* Name and type will be determined later. */
		super("", "", jaxbObject, null);
		mParentBinding = null;
		mJaxbObjectFactory = objectFactory;
		initComplexElement();
	}
	
	/**
	 * Constructor for a Complex element as a child of another element.
	 * 
	 * @param jaxbName name of field in parent JAXB object
	 * @param parentBinding a reference to the parent binding
	 * @param objectFactory the JAXB object factory
	 * @param jaxbObject the concrete JAXB object instance bound to this
	 *        complex element
	 * @param cobolAnnotations the cobol annotations for this element
	 * @throws HostException if construction fails
	 */
	public CComplexBinding(
			final String jaxbName,
			final CComplexBinding parentBinding,
			final Object objectFactory,
			final Object jaxbObject,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		super(jaxbName, "", jaxbObject, cobolAnnotations);
		mParentBinding = parentBinding;
		mJaxbObjectFactory = objectFactory;
		initComplexElement();
	}

	/**
	 * Helper method. JAXB Types are annotated with an XmlType which gives
	 * an ordered list of properties
	 * @throws HostException if initialisation fails
	 */
	private void initComplexElement() throws HostException {

		mXmlType = getJaxbObject().getClass().getAnnotation(XmlType.class);
		if (mXmlType == null) {
			throw new HostException(
					"No jaxb annotations found in class "
					+ getJaxbObject().getClass());
		}
		setType(mXmlType.name());
		/* If no name was assigned (root element) use type name as name */
		if (getJavaName() == null || getJavaName().length() == 0) {
			setName(mXmlType.name());
		}
	}
	
	/** {@inheritDoc} */
	public final void createBoundObject() throws HostException {
		/* This is a request to build bound objects so we build the children
		 * list with 'create' intent. The creation of bound objects is part
		 * of the build list process. */
		if (mChildren == null) {
			mChildren = buildChildrenList(true);
		}
	}
	
	/** {@inheritDoc} */
	public final void prepareChildren() throws HostException {
	}
	
	/** {@inheritDoc} */
	public final void getValuesFromBoundObject() throws HostException {
		/* This is a request to update values from existing bound objects. 
		 * This dynamic version does not duplicate the properties values
		 * internally so all we need to do is to make sure we have an
		 * updated list of children.*/
		if (mChildren == null) {
			mChildren = buildChildrenList(false);
		}
	}
	
	/** {@inheritDoc} */
	@Override
	public final void accept(final CobolElementVisitor cev)
		throws HostException {
		cev.visit(this);
	}
	
	/** {@inheritDoc} */
	public final void setBoundObjectValue(
			final int index) throws HostException {
		/* In this dynamic version, bound objects are initialized as part
		 * of the children list building. */
	}
	
	/** Returns the corresponding host byte size by recursively calling
	 *  bytelength on each sub-element. */
	/** {@inheritDoc} */
	public final int getByteLength() throws HostException {

		int byteLength = 0;
		
		/* Create an ordered list of properties in this complex element */
		java.util.List < ICobolBinding > children =
			buildChildrenList(true);
		
		for (ICobolBinding child : children) {
			byteLength += child.getByteLength();
		}
		return byteLength;
	}
	
	/**
	 * @return Returns the JAXB Object Factory.
	 */
	public final Object getObjectFactory() {
		return mJaxbObjectFactory;
	}
	
	/**
	 * This will dynamically build children binding elements by reflecting
	 * on cobol annotations within the JAXB object.
	 * 
	 * @param createJaxbObjects true if we need to create missing JAXB objects
	 * @return list of children
	 * @throws HostException if children list cannot be built
	 */
	private java.util.List < ICobolBinding > buildChildrenList(
			final boolean createJaxbObjects)
		throws HostException {
		
		/* List of children */
		java.util.ArrayList < ICobolBinding > childrenList =
			new java.util.ArrayList < ICobolBinding >(); 
		
		/* Map of choice elements for redefined elements */
		RedefinesMap redefinesMap = new RedefinesMap();
		
		/* Process each property of this complex type in the predefined order */
		for (String prop : getXmlType().propOrder()) {
			try {
				/* Get a reference to this property field and type */
				Field hostField =
					getJaxbObject().getClass().getDeclaredField(prop);
				
				/* Get the complete type including genericity,
				 * trim initial "class" */ 
				String jaxbType = hostField.getGenericType().toString();
				if (jaxbType.startsWith("class ")) {
					jaxbType = jaxbType.substring(6);
				}
				
				/* Pass on the field name  */
				String jaxbName = prop;
				
				/* Get the cobol annotations for that field */
				CobolElement cobolAnnotations =
					hostField.getAnnotation(CobolElement.class);
				if (cobolAnnotations == null) {
					throw new HostException(
							"No cobol annotations found for field "
							+ hostField.getName());
				}
				
				/* Add child element to list */
				ICobolBinding ce =
					createCobolJAXBElement(getJaxbObject(), jaxbName,
						                 jaxbType, cobolAnnotations,
						                 createJaxbObjects, redefinesMap);
				if (ce != null) {
					childrenList.add(ce);
				}
			} catch (NoSuchFieldException e) {
				throw (new HostException(
						"NoSuchFieldException " + e.getMessage()));
			}
		}
		return childrenList;
	}
	
	/**
	 * Based on the cobol annotations for that field, we create a corresponding
	 * element type. If this element is redefined we create a choice element
	 * and return it instead of the the redefined element itself. 
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param createJaxbObjects true when bound JAXB objects do not pre-exist
	 * @param redefinesMap the current list of redefined items
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createCobolJAXBElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations,
			final boolean createJaxbObjects,
			final RedefinesMap redefinesMap)
		throws HostException {
		
		ICobolBinding cobolElement = null;
		
		switch (cobolAnnotations.type()) {
		case GROUP_ITEM:
			cobolElement = createComplexElement(
					jaxbObject, jaxbName, jaxbType,
					cobolAnnotations, createJaxbObjects);
			break;
		case ALPHABETIC_ITEM:
		case ALPHANUMERIC_EDITED_ITEM:
		case ALPHANUMERIC_ITEM:
		case NUMERIC_EDITED_ITEM:
		case EXTERNAL_FLOATING_ITEM:
			cobolElement = createStringElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case NATIONAL_ITEM:
			cobolElement = createNationalElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case PACKED_DECIMAL_ITEM:
			cobolElement = createPackedDecimalElement(
					jaxbObject, jaxbName, jaxbType,	cobolAnnotations);
			break;
		case ZONED_DECIMAL_ITEM:
			cobolElement = createZonedDecimalElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case DBCS_ITEM:
		case OCTET_STREAM_ITEM:
		case INDEX_ITEM:
		case POINTER_ITEM:
		case PROC_POINTER_ITEM:
		case FUNC_POINTER_ITEM:
			cobolElement = createOctetStreamElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case BINARY_ITEM:
		case NATIVE_BINARY_ITEM:
			cobolElement = createBinaryElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case SINGLE_FLOAT_ITEM:
			cobolElement = createFloatElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		case DOUBLE_FLOAT_ITEM:
			cobolElement = createDoubleElement(
					jaxbObject, jaxbName, jaxbType, cobolAnnotations);
			break;
		default:
			throw (new HostException(
					"Unrecognized cobol type for field "
					+ cobolAnnotations.cobolName()));
		}
		
		/* If we can't create that element, we can't continue */
		if (cobolElement == null) {
			return null;
		}
		
		/* If this element is part of a redefinition group (either redefines
		 * another element or is redefined by another element) we need
		 * further processing. */
		String redefines = cobolAnnotations.redefines();
		if ((cobolAnnotations.isRedefined())
				|| (redefines != null
						&& redefines.length() > 0)) {
			return createChoiceElement(jaxbObject, jaxbName, jaxbType,
					cobolAnnotations, cobolElement, redefinesMap);
		}
		
		return cobolElement;
	}

	/**
	 * Create a choice element type which will group all alternatives. The
	 * choice element replaces the redefined element in the parent hierarchy.
	 * All alternative (redefining elements) are then added to the choice.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param cobolElement the cobol descriptor for this element
	 * @param redefinesMap the current list of redefined items
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createChoiceElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations,
			final ICobolBinding cobolElement,
			final RedefinesMap redefinesMap)
		throws HostException {
		
		/* If this is a redefined item, then we need to create a special choice
		 * element which will group all alternatives */
		if (cobolAnnotations.isRedefined()) {
			
			CChoiceBinding choice =
				new CChoiceBinding(
						jaxbName, jaxbType, jaxbObject, this, cobolAnnotations);
			
			/* Add the redefined item as the first alternative in the
			 * choice element */
			choice.addAlternative(cobolElement);
			
			/* Add this choice element in the redefines map */
			redefinesMap.updateChoiceElement(
					cobolAnnotations.cobolName(), choice);
			
			/* Return the choice as the current element for caller */
			return choice;
			
		}
			
		/* This is a redefinition of an existing element, we need to add
		 * this alternative to the redefined element list of alternatves */
		/* lookup the redefines Map to locate the choice element we are
		 * part of */
		CChoiceBinding choice =
			redefinesMap.getChoiceElement(cobolAnnotations.redefines());
		if (choice == null) {
			/* If the redefined element has not been processed, this means
			 * it was not created by the caller (he already made a choice).
			 * In this case, the current element is the first of the possible
			 * alternatives. */
			 choice =
				new CChoiceBinding(
						jaxbName, jaxbType, jaxbObject, this, cobolAnnotations);
			
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
		
		/* Since choice element is already part of parent children,
		 * return null to avoid adding it twice */
		return null;
		
	}
	

	/**
	 * Create a group element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @param createJaxbObjects true when bound JAXB objects do not pre-exist
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createComplexElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations,
			final boolean createJaxbObjects)
		throws HostException {
		
		ICobolBinding ce;
		
		/* First get the corresponding JAXB object for the child group item */
		Object group = JaxbUtil.invokeGetProperty(jaxbObject, jaxbName);
		
		if (group == null) {
			/* This is the normal situation in unmarshalling mode */
			if (createJaxbObjects) {
				group = JaxbUtil.addComplexProperty(
						getObjectFactory(),  jaxbObject, jaxbName, jaxbType);
			} else {
				/* This is marshaling mode.
				 * If this element is involved in a redefinition, null means
				 * this alternative is not present so send back this
				 *  information. */
				if (cobolAnnotations.isRedefined()
						|| 	(cobolAnnotations.redefines().length() > 0)) {
						return null;
				} else {
					/* Send back a default value */	
					group = JaxbUtil.addComplexProperty(
							getObjectFactory(), jaxbObject, jaxbName, jaxbType);
				}
			}
		}
		
		/* Complex children which are arrays are stored once for efficiency */ 
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayComplexBinding(
					getObjectFactory(), jaxbName, jaxbType, group,
					cobolAnnotations);
		} else {
			ce = new CComplexBinding(
					jaxbName, this, getObjectFactory(),
					group, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a String element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createStringElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayStringBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CStringBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a National element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createNationalElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayNationalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CNationalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a Octet stream element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createOctetStreamElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayOctetStreamBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new COctetStreamBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a PackedDecimal element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding  createPackedDecimalElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayPackedDecimalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CPackedDecimalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		return ce;
	}
	
	/**
	 * Create a ZonedDecimal element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createZonedDecimalElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayZonedDecimalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CZonedDecimalBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a Binary element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createBinaryElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayBinaryBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CBinaryBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	/**
	 * Create a Float element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createFloatElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayFloatBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CFloatBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}
	
	/**
	 * Create a Double element type.
	 * 
	 * @param jaxbObject the parent JAXB object tree
	 * @param jaxbName the java property name
	 * @param jaxbType the java property type
	 * @param cobolAnnotations the cobol annotations for this element
	 * @return the new cobol element description
	 * @throws HostException if cobol description cannot be created
	 */
	private ICobolBinding createDoubleElement(
			final Object jaxbObject,
			final String jaxbName,
			final String jaxbType,
			final CobolElement cobolAnnotations)
		throws HostException {
		
		ICobolBinding ce;
		if (cobolAnnotations.maxOccurs() > 0) { 
			ce = new CArrayDoubleBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		} else {
			ce = new CDoubleBinding(
					jaxbName, jaxbType, jaxbObject, cobolAnnotations);
		}
		
		return ce;
	}

	/** 
	 * Returns the JAXB XmlType annotation corresponding to this
	 * complex type.
	 *  @return JAXB XmlType annotation
	 */
	public final XmlType getXmlType() {
		return mXmlType;
	}

	/** {@inheritDoc} */
	public final BigDecimal getNumericValue() throws HostException {
		throw (new HostException("Element " + getJavaName()
				+ "cannot return a numeric value"));
	}

	/** {@inheritDoc} */
	public final Object getValue() throws HostException {
		return getJaxbObject();
	}

	/** {@inheritDoc} */
	public final java.util.List < ICobolBinding > getChildrenList() {
		return mChildren;
	}

	/**
	 * @param children the children list to set
	 */
	public final void setChildrenList(
			final java.util.List < ICobolBinding > children) {
		mChildren = children;
	}

	/**
	 * @return the parent binding element
	 */
	public final CComplexBinding getParentBinding() {
		return mParentBinding;
	}

	/**
	 * @param parentBinding the arent binding element to set
	 */
	public final void setParentBinding(
			final CComplexBinding parentBinding) {
		mParentBinding = parentBinding;
	}


}
