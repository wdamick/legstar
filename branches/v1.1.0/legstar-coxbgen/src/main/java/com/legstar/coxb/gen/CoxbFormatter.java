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
package com.legstar.coxb.gen;

import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.util.JaxbUtil;

/**
 * This class groups methods to format COXB generator XML files content.
 * 
 * @author Fady Moussallam
 * 
 */
public final class CoxbFormatter {

	/* ====================================================================== */
	/* = XML work file markup section                                       = */
	/* ====================================================================== */
	/** Assume XML content will be UTF-8 encoded. */
	private static final String XML_DECL =
        "<?xml version='1.0' encoding='UTF-8'?>";

	/** Tag enclosing a complex element description. */
	private static final String COXB_TYPE = "coxb-type";
	
	/** Tag describing a property within a complex element. */
	private static final String COXB_PROP = "coxb-property";
	
	/** Name attribute. */
	private static final String NAME_ATTR = "name=";

	/** Type attribute. */
	private static final String TYPE_ATTR = "type=";

	/** Complex type. */
	private static final String COMPLEX_TYPE = "complex";
	
	/** Choice type. */
	private static final String CHOICE_TYPE = "choice";
	
	/** Complex array type. */
	private static final String COMPLEX_ARRAY_TYPE = "complexArray";
	
	/** Simple type. */
	private static final String SIMPLE_TYPE = "simple";
	
	/** Enum type. */
	private static final String ENUM_TYPE = "enum";
	
	/** Property name in bound JAXB object attribute. */
	private static final String JAXB_NAME_ATTR = "jaxb-name=";
	
	/** Property type in bound JAXB object attribute. */
	private static final String JAXB_TYPE_ATTR = "jaxb-type=";
	
	/** JAXB package name. */
	private static final String JAXB_PACKAGE_ATTR = "jaxb-package=";
	
	/** Name suitable as a java variable name (first char lowercase). */
	private static final String VAR_NAME_ATTR = "var-name=";
	
	/** Name that identifies this binding element. */
	private static final String BIND_NAME_ATTR = "binding-name=";
	
	/** Binding class this property derives from. */
	private static final String BIND_TYPE_ATTR = "binding-type=";
	
	/** Parent Jaxb class (useful for choices). */
	private static final String PARENT_JAXB_TYPE_ATTR = "parent-jaxb-type=";
	
	/** Item Jaxb class (useful for arrays). */
	private static final String ITEM_JAXB_TYPE_ATTR = "item-jaxb-type=";
	
	/** Binding class a complex array item derives from. */
	private static final String ITEM_BIND_TYPE_ATTR = "item-binding-type=";
	
	/** The cobol name of the variable. */
	private static final String COBOL_NAME_ATTR = "cobol-name=";
	
	/** Cobol element size in bytes. */
	private static final String BYTELEN_ATTR = "cobol-byteLength=";
	
	/** Cobol element number of digits (if numeric). */
	private static final String TOTDIGITS_ATTR = "cobol-totalDigits=";
	
	/** Cobol element number of fractional digits (if numeric). */
	private static final String FRACDIGITS_ATTR = "cobol-fractionDigits=";
	
	/** True if Cobol element number is signed (if numeric). */
	private static final String ISSIGNED_ATTR = "cobol-isSigned=";
	
	/** True if Cobol element sign is in first byte (if numeric). */
	private static final String ISLEADING_ATTR = "cobol-isSignLeading=";
	
	/** True if Cobol element sign is in its own byte (if numeric). */
	private static final String ISSEPARATE_ATTR = "cobol-isSignSeparate=";
	
	/** True if Cobol element is justified to the right (if string). */
	private static final String ISRIGHTJUST_ATTR = "cobol-isJustifiedRight=";
	
	/** Minimum number of items (if array). */
	private static final String MINOCCURS_ATTR = "cobol-minOccurs=";
	
	/** Maximum number of items (if array). */
	private static final String MAXOCCURS_ATTR = "cobol-maxOccurs=";
	
	/** Variable name giving array size (if array). */
	private static final String DEPENDING_ON_ATTR = "cobol-dependingOn=";
	
	/**This variable gives an array its size. */
	private static final String IS_ODOOBJECT_ATTR = "cobol-isODOObject=";
	
	/** This variable gives which cobol variable this one redefines. */
	private static final String REDEFINES_ATTR = "cobol-redefines=";
	
	/** Element is redefined by at least one other element. */
	private static final String IS_REDEFINED_ATTR = "cobol-isRedefined=";
	
	/**This variable gives an array its size. */
	private static final String IS_CUSTOM_VARIABLE_ATTR =
        "cobol-isCustomVariable=";
	
	/** A marshal choice strategy class name. */
	private static final String MARSHAL_CHOICE_STRATEGY_ATTR =
        "marshalChoiceStrategyClassName=";
	
	/** An unmarshal choice strategy class name. */
	private static final String UNMARSHAL_CHOICE_STRATEGY_ATTR =
        "unmarshalChoiceStrategyClassName=";
	
	/* ====================================================================== */
	/* = Binding types section                                              = */
	/* ====================================================================== */
	/** The java string type. */
	private static final String STRING_BIND_TYPE = "ICobolStringBinding";
	
	/** The java string array type. */
	private static final String ARRAY_STRING_BIND_TYPE =
		"ICobolArrayStringBinding";
	
	/** The java national type. */
	private static final String NATIONAL_BIND_TYPE = "ICobolNationalBinding";
	
	/** The java national array type. */
	private static final String ARRAY_NATIONAL_BIND_TYPE =
		"ICobolArrayNationalBinding";
	
	/** The java binding for binary data type. */
	private static final String OCTET_STREAM_BIND_TYPE =
		"ICobolOctetStreamBinding";
	
	/** The java binding for binary array data type. */
	private static final String ARRAY_OCTET_STREAM_BIND_TYPE =
        "ICobolArrayOctetStreamBinding";
	
	/** The java binding for binary numeric data type. */
	private static final String BINARY_BIND_TYPE = "ICobolBinaryBinding";
	
	/** The java binding for binary numeric array data type. */
	private static final String ARRAY_BINARY_BIND_TYPE =
		"ICobolArrayBinaryBinding";
	
	/** The java binding for packed decimal numeric data type. */
	private static final String PACKED_BIND_TYPE =
		"ICobolPackedDecimalBinding";
	
	/** The java binding for packed decimal numeric array data type. */
	private static final String ARRAY_PACKED_BIND_TYPE =
        "ICobolArrayPackedDecimalBinding";

	/** The java binding for zoned decimal data type. */
	private static final String ZONED_BIND_TYPE =
		"ICobolZonedDecimalBinding";
	
	/** The java binding for zoned decimal array data type. */
	private static final String ARRAY_ZONED_BIND_TYPE =
        "ICobolArrayZonedDecimalBinding";
	
	/** The java binding for double data type. */
	private static final String DOUBLE_BIND_TYPE = "ICobolDoubleBinding";
	
	/** The java binding for double array data type. */
	private static final String ARRAY_DOUBLE_BIND_TYPE =
		"ICobolArrayDoubleBinding";
	
	/** The java binding for float data type. */
	private static final String FLOAT_BIND_TYPE = "ICobolFloatBinding";
	
	/** The java binding for float array data type. */
	private static final String ARRAY_FLOAT_BIND_TYPE =
		"ICobolArrayFloatBinding";
	
	/** Line seperator for readability. */
	private static final String LINE_SEP = System.getProperty("line.separator");

    /**
     * Private constructor to stop anyone from instantiating
     * this class - the static methods should be used
     * explicitly.
     */
    private CoxbFormatter()  {
        
    }
    
	/**
	 * Format a complex element XML opening elements.
     * 
	 * @param ce the complex element description
	 * @param packageName the target packet for the generated binding class
     * @return part of the XML output
	 */
	public static String formatComplexProlog(
			final ICobolComplexBinding ce,
			final String packageName) {
		
		StringBuffer out = new StringBuffer();
		
		/* <?xml version='1.0' encoding='UTF-8'?> */
		out.append(XML_DECL);
		out.append(LINE_SEP);
		
		/* <coxb-type name="DfhcommareaTypeBinding" type="complex"> */
		out.append('<');
		out.append(COXB_TYPE);
		addAttribute(out, NAME_ATTR, JaxbUtil.getCoxbTypeName(ce));
		addAttribute(out, TYPE_ATTR, COMPLEX_TYPE);
		addAttribute(out, JAXB_TYPE_ATTR, JaxbUtil.getJaxbTypeName(ce));
		addAttribute(out, JAXB_PACKAGE_ATTR, packageName);
		out.append('>');
		out.append(LINE_SEP);
		
		return out.toString();
	}
	
	/**
	 * Format a choice element XML opening elements.
     * 
	 * @param ce the complex element description
	 * @param packageName the target packet for the generated binding class
     * @return part of the XML output
	 * @throws HostException if formatting fails
	 */
	public static String formatComplexProlog(
			final ICobolChoiceBinding ce,
			final String packageName) throws HostException {
		
		StringBuffer out = new StringBuffer();
		
		/* A choice creates its own separate XML to describe the alternatives.*/
		
		/* <?xml version='1.0' encoding='UTF-8'?> */
		out.append(XML_DECL);
		out.append(LINE_SEP);
		
		/* <coxb-type name="DfhcommareaTypeBinding" type="complex"> */
		out.append('<');
		out.append(COXB_TYPE);
		addAttribute(out, NAME_ATTR, JaxbUtil.getCoxbTypeName(ce));
		addAttribute(out, TYPE_ATTR, CHOICE_TYPE);
		addAttribute(out, PARENT_JAXB_TYPE_ATTR,
				ce.getParentBinding().getJaxbType().getSimpleName());
		addAttribute(out, JAXB_PACKAGE_ATTR, packageName);
		/* The first alternative might hold class names for alternative
		 * selection. */
		if (ce.getAlternativesList().size() > 0) {
			ICobolBinding firstAlt = ce.getAlternativesList().get(0);
			String strategy = firstAlt.getMarshalChoiceStrategyClassName();
			if (strategy != null && strategy.length() > 0) {
				addAttribute(out, MARSHAL_CHOICE_STRATEGY_ATTR, strategy);
			}
			strategy = firstAlt.getUnmarshalChoiceStrategyClassName();
			if (strategy != null && strategy.length() > 0) {
				addAttribute(out, UNMARSHAL_CHOICE_STRATEGY_ATTR, strategy);
			}
		}
		out.append('>');
		out.append(LINE_SEP);
		
		return out.toString();
		
	}

	/**
	 * Format a complex array element XML opening elements.
     * 
	 * @param ce the complex element description
	 * @param packageName the target packet for the generated binding class
     * @return part of the XML output
	 * @throws HostException if getMaxOccurs fails
	 */
	public static String formatComplexProlog(
			final ICobolArrayComplexBinding ce,
			final String packageName) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* <?xml version='1.0' encoding='UTF-8'?> */
		out.append(XML_DECL);
		out.append(LINE_SEP);
		
		/* <coxb-type name="DfhcommareaTypeBinding" type="complex"> */
		out.append('<');
		out.append(COXB_TYPE);
		/* Complex arrays are always lists in JAXB, here we need to reference
         * the complex object that occurs multiple times. */
		addAttribute(out, NAME_ATTR, JaxbUtil.getCoxbTypeName(ce));
		addAttribute(out, TYPE_ATTR, COMPLEX_ARRAY_TYPE);
		addAttribute(out, ITEM_JAXB_TYPE_ATTR, JaxbUtil.getJaxbTypeName(ce));
		addAttribute(out, JAXB_PACKAGE_ATTR, packageName);
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());
		if (ce.getDependingOn() != null && ce.getDependingOn().length() > 0) {
			addAttribute(out, DEPENDING_ON_ATTR, ce.getDependingOn());
		}
		out.append('>');
		out.append(LINE_SEP);
		
		return out.toString();
	}
	
	/**
	 * Format a complex element XML closing tage.
     * 
     * @return part of the XML output
	 */
	public static String formatComplexEpilog() {
		
		StringBuffer out = new StringBuffer();
		
		/* </coxb-type> */
		out.append("</");
		out.append(COXB_TYPE);
		out.append('>');
		out.append(LINE_SEP);
		
		return out.toString();
		
	}
	
	/**
	 * A complex property generates an element in its parent complex XML.
	 * 
	 * @param ce the complex binding description
     * @return part of the XML output
	 */
	public static String formatProperty(
			final ICobolComplexBinding ce) {
		
		StringBuffer out = new StringBuffer();
		
		/* <coxb-property jaxb-name="comPersonal" type="complex"
         *  jaxb-type="ComPersonalType"
         *  binding-type="ComPersonalTypeBinding"/> */
		out.append('<');
		out.append(COXB_PROP);
		addAttribute(out, VAR_NAME_ATTR, JaxbUtil.getFieldName(ce));
		addAttribute(out, BIND_NAME_ATTR, ce.getBindingName());
		addAttribute(out, JAXB_NAME_ATTR, ce.getJaxbName());
		addAttribute(out, TYPE_ATTR, COMPLEX_TYPE);
		addAttribute(out, JAXB_TYPE_ATTR, JaxbUtil.getJaxbTypeName(ce));
		addAttribute(out, BIND_TYPE_ATTR, JaxbUtil.getCoxbTypeName(ce));
		if (ce.getRedefines() != null && ce.getRedefines().length() > 0) {
			addAttribute(out, REDEFINES_ATTR, ce.getRedefines());
		}
		if (ce.isRedefined()) {
			addAttribute(out, IS_REDEFINED_ATTR, ce.isRedefined());
		}
		out.append("/>");
		out.append(LINE_SEP);
		
		return out.toString();
		
	}

	/**
	 * A choice generates a property in its parent complex XML.
	 * 
	 * @param ce the choice binding description
     * @return part of the XML output
	 */
	public static String formatProperty(
			final ICobolChoiceBinding ce) {
		
		StringBuffer out = new StringBuffer();
		
		/* <coxb-property jaxb-name="cDefinition1" type="choice"
         *  binding-type="CDefinition1Binding"/> */
		out.append('<');
		out.append(COXB_PROP);
		addAttribute(out, VAR_NAME_ATTR, JaxbUtil.getFieldName(ce));
		addAttribute(out, BIND_NAME_ATTR, ce.getBindingName());
		addAttribute(out, TYPE_ATTR, CHOICE_TYPE);
		addAttribute(out, BIND_TYPE_ATTR, JaxbUtil.getCoxbTypeName(ce));
		if (ce.getMarshalChoiceStrategyClassName() != null
				&& ce.getMarshalChoiceStrategyClassName().length() > 0) {
			addAttribute(out, MARSHAL_CHOICE_STRATEGY_ATTR,
					ce.getMarshalChoiceStrategyClassName());
		}
		if (ce.getUnmarshalChoiceStrategyClassName() != null
				&& ce.getUnmarshalChoiceStrategyClassName().length() > 0) {
			addAttribute(out, UNMARSHAL_CHOICE_STRATEGY_ATTR,
					ce.getUnmarshalChoiceStrategyClassName());
		}
		out.append("/>");
		out.append(LINE_SEP);
		
		return out.toString();
		
	}
	
	/**
	 * A complex array generates a property in its parent complex XML.
	 * 
	 * @param ce the complex array binding description
     * @return part of the XML output
	 */
	public static String formatProperty(
			final ICobolArrayComplexBinding ce) {
		
		StringBuffer out = new StringBuffer();
		
		/* <coxb-property jaxb-name="cDefinition1" type="complexArray"
         *  jaxb-type="CArrayType" binding-type="CArrayWrapperTypeBinding"/> */
		out.append('<');
		out.append(COXB_PROP);
		addAttribute(out, VAR_NAME_ATTR, JaxbUtil.getFieldName(ce));
		addAttribute(out, BIND_NAME_ATTR, ce.getBindingName());
		addAttribute(out, JAXB_NAME_ATTR, ce.getJaxbName());
		addAttribute(out, TYPE_ATTR, COMPLEX_ARRAY_TYPE);
		/* Complex arrays are always lists in JAXB, here we need to reference
         * the complex object that occurs multiple times. */
		addAttribute(out, JAXB_TYPE_ATTR, JaxbUtil.getJaxbTypeName(ce));
		addAttribute(out, BIND_TYPE_ATTR, JaxbUtil.getCoxbTypeName(ce));
		addAttribute(out, ITEM_BIND_TYPE_ATTR, JaxbUtil.getCoxbTypeName(
				ce.getComplexItemBinding()));
		out.append("/>");
		out.append(LINE_SEP);
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolStringBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, STRING_BIND_TYPE);
		addAttribute(out, ISRIGHTJUST_ATTR, ce.isJustifiedRight());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayStringBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_STRING_BIND_TYPE);
		addAttribute(out, ISRIGHTJUST_ATTR, ce.isJustifiedRight());
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolNationalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, NATIONAL_BIND_TYPE);
		addAttribute(out, ISRIGHTJUST_ATTR, ce.isJustifiedRight());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayNationalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_NATIONAL_BIND_TYPE);
		addAttribute(out, ISRIGHTJUST_ATTR, ce.isJustifiedRight());
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolOctetStreamBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, OCTET_STREAM_BIND_TYPE);

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayOctetStreamBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_OCTET_STREAM_BIND_TYPE);
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolBinaryBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, BINARY_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayBinaryBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_BINARY_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolPackedDecimalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, PACKED_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayPackedDecimalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_PACKED_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolZonedDecimalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ZONED_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());
		addAttribute(out, ISLEADING_ATTR, ce.isSignLeading());
		addAttribute(out, ISSEPARATE_ATTR, ce.isSignSeparate());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayZonedDecimalBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_ZONED_BIND_TYPE);
		addAttribute(out, TOTDIGITS_ATTR, ce.getTotalDigits());
		addAttribute(out, FRACDIGITS_ATTR, ce.getFractionDigits());
		addAttribute(out, ISSIGNED_ATTR, ce.isSigned());
		addAttribute(out, ISLEADING_ATTR, ce.isSignLeading());
		addAttribute(out, ISSEPARATE_ATTR, ce.isSignSeparate());
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolDoubleBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, DOUBLE_BIND_TYPE);

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayDoubleBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_DOUBLE_BIND_TYPE);
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}
	
	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolFloatBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, FLOAT_BIND_TYPE);

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatProperty(
			final ICobolArrayFloatBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* Common attributes to all properties */
		out.append(formatSimplePropertyProlog(ce));
		
		/* Set specific attributes */
		addAttribute(out, BYTELEN_ATTR, ce.getByteLength());
		addAttribute(out, BIND_TYPE_ATTR, ARRAY_FLOAT_BIND_TYPE);
		addAttribute(out, MINOCCURS_ATTR, ce.getMinOccurs());
		addAttribute(out, MAXOCCURS_ATTR, ce.getMaxOccurs());

		out.append(formatSimplePropertyEpilog());
		
		return out.toString();
		
	}

	/**
	 * Format the first part of a property element XML element.
     * 
	 * @param ce the element description
     * @return part of the XML output
	 * @throws HostException if byte length cannot be retrieved
	 */
	public static String formatSimplePropertyProlog(
			final ICobolBinding ce) throws HostException {

		StringBuffer out = new StringBuffer();
		
		/* <coxb-property jaxb-name="comComment" jaxb-type="..."... */
		out.append('<');
		out.append(COXB_PROP);
		addAttribute(out, VAR_NAME_ATTR, JaxbUtil.getFieldName(ce));
		addAttribute(out, BIND_NAME_ATTR, ce.getBindingName());
		addAttribute(out, JAXB_NAME_ATTR, ce.getJaxbName());
		String jaxbType = JaxbUtil.getJaxbTypeName(ce);
		addAttribute(out, JAXB_TYPE_ATTR, jaxbType);
		/* TODO: Here we consider that if the type of this element is not one
		 * of the known primitive types, then this must be an enumeration. In
		 * the future this should be replaced by a more robust mechanism. */
		if (isEnum(jaxbType)) {
			addAttribute(out, TYPE_ATTR, ENUM_TYPE);
		} else {
			addAttribute(out, TYPE_ATTR, SIMPLE_TYPE);
		}
		addAttribute(out, COBOL_NAME_ATTR, ce.getCobolName());
		if (ce.getDependingOn() != null && ce.getDependingOn().length() > 0) {
			addAttribute(out, DEPENDING_ON_ATTR, ce.getDependingOn());
		}
		if (ce.isODOObject()) {
			addAttribute(out, IS_ODOOBJECT_ATTR, ce.isODOObject());
		}
		if (ce.getRedefines() != null && ce.getRedefines().length() > 0) {
			addAttribute(out, REDEFINES_ATTR, ce.getRedefines());
		}
		if (ce.isRedefined()) {
			addAttribute(out, IS_REDEFINED_ATTR, ce.isRedefined());
		}
		if (ce.isCustomVariable()) {
			addAttribute(out, IS_CUSTOM_VARIABLE_ATTR, ce.isCustomVariable());
		}
		
		return out.toString();
		
	}
	
	/**
	 * Format the last part of a property element XML element.
     * 
     * @return part of the XML output
	 */
	public static String formatSimplePropertyEpilog() {

		StringBuffer out = new StringBuffer();
		
		/* ... /> */
		out.append("/>");
		out.append(LINE_SEP);
		
		return out.toString();
		
	}
	/**
	 * Helper method to add an XML attribute.
     * 
	 * @param s a stringBuffer used for output
	 * @param attributeName the name of the attribute followed by equal sign
	 * @param attributeValue the value to set
	 */
	public static void addAttribute(
			final StringBuffer s,
			final String attributeName,
			final String attributeValue) {

		s.append(' ');
		s.append(attributeName);
		s.append('"');
		s.append(attributeValue);
		s.append('"');
		
	}

	/**
	 * Helper method to add an XML attribute for a numeric.
     * 
	 * @param s a stringBuffer used for output
	 * @param attributeName the name of the attribute followed by equal sign
	 * @param attributeValue the value to set
	 */
	public static void addAttribute(
			final StringBuffer s,
			final String attributeName,
			final int attributeValue) {
		
		addAttribute(s, attributeName,
				(new Integer(attributeValue)).toString());
		
	}

	/**
	 * Helper method to add an XML attribute for a booleans.
     * 
	 * @param s a stringBuffer used for output
	 * @param attributeName the name of the attribute followed by equal sign
	 * @param attributeValue the value to set
	 */
	public static void addAttribute(
			final StringBuffer s,
			final String attributeName,
			final boolean attributeValue) {
		
		addAttribute(s, attributeName,
				(new Boolean(attributeValue)).toString());
		
	}
	
	/**
	 * This will determine if a type name for a simple element is one of
	 * java's native types and assume it is an enum otherwise.
	 * @param type the element java type
	 * @return true if it should be considered an enum
	 */
	private static boolean isEnum(final String type) {
		if (type == null) {
			return false;
		}
		if (type.compareToIgnoreCase("String") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("[B") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("byte[]") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("short") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("int") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("Integer") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("long") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("float") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("double") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("BigInteger") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("BigDecimal") == 0) {
			return false;
		} else if (type.compareToIgnoreCase("byte") == 0) {
			return false;
		}
		return true;
	}
	

}
