/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.jaxb.plugin;

import org.xml.sax.ErrorHandler;

import com.sun.tools.xjc.Options;
import com.sun.tools.xjc.Plugin;
import com.sun.tools.xjc.outline.Outline;
import com.sun.tools.xjc.outline.ClassOutline;
import com.sun.tools.xjc.outline.FieldOutline;
import com.sun.tools.xjc.model.CPluginCustomization;
import com.sun.tools.xjc.model.CElementInfo;
import com.sun.tools.xjc.model.CElement;
import com.sun.tools.xjc.model.CReferencePropertyInfo;
import com.sun.codemodel.JFieldVar;
import com.sun.codemodel.JAnnotationUse;
import java.util.List;
import java.util.Collections;
import javax.xml.bind.annotation.XmlSchemaType;

import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


/**
 * This is an extension to the JAXB XJC plugin. It is being invoked by the JAXB
 * XML to Java compilation and injects supplementary cobol annotations into the 
 * generated Java classes.
 * Add -Dcom.sun.tools.xjc.Options.findServices=true to VM arguments to help
 * solve classpath issues.
 *
 */
public class CobolJAXBAnnotator extends Plugin {
	
	/**
	 * Constant values used throughout the annotator.
	 */
	private final class Const {
		/** Namespace for cobol annotations. */
		public static final String NS = "http://www.legsem.com/xml/ns/coxb";
		/** Cobol annotation class. */
		public static final  String ELEMENT = "cobolElement";
		/** Cobol value appears inline in cobolElement. */
		public static final  String ELEMENT_VALUE = "value";
		/** Option passed to XJC to enable this cobol plugin. */
		public static final  String OPTIONNAME = "Xlegstar-code";
		/** Command line help for cobol plugin XJC option. */
		public static final  String USAGE =
			"  -Xlegstar-code      :  inject cobol binding annotation into the "
			 + "generated code";
		
		/* XSD annotation tags mapped to java cobol annotations */
		/** Cobol level number. */
		public static final  String LEVEL_NUMBER = "levelNumber";
		/** Cobol variable name. */
		public static final  String COBOL_NAME = "cobolName";
		/** Cobol variable type. */
		public static final  String TYPE = "type";
		/** Cobol picture clause. */
		public static final  String PICTURE = "picture";
		/** Cobol variable size in bytes. */
		public static final  String BYTE_LENGTH = "byteLength";
		/** Cobol right or left justification. */
		public static final  String IS_JUSTIFIED_RIGHT = "justifiedRight";
		/** Cobol numeric sign indicator. */
		public static final  String IS_SIGNED = "signed";
		/** Cobol numeric total number of digits. */
		public static final  String TOTAL_DIGITS = "totalDigits";
		/** Cobol fractional number of digits. */
		public static final  String FRACTION_DIGITS = "fractionDigits";
		/** Cobol sign position. */
		public static final  String IS_SIGN_LEADING = "signLeading";
		/** Cobol sign in its own byte. */
		public static final  String IS_SIGN_SEPARATE = "signSeparate";
		/** Cobol array minimum number of occurences. */
		public static final  String MIN_OCCURS = "minOccurs";
		/** Cobol array maximum number of occurences. */
		public static final  String MAX_OCCURS = "maxOccurs";
		/** Cobol array size dependency on another variable value. */
		public static final  String DEPENDING_ON = "dependingOn";
		/** Cobol array with variable size indicator. */
		public static final  String IS_ODO_OBJECT = "isODOObject";
		/** Cobol variable redefining another one. */
		public static final  String REDEFINES = "redefines";
		/** Cobol variable is redefined by at least one other variable. */
		public static final  String IS_REDEFINED = "isRedefined";
		/** Identifies this element as used in custom code. */
		public static final  String IS_CUSTOM_VARIABLE = "customVariable";
		/** Name of class providing logic to help with alternative selection
		 * when marshaling (Java to Host). */
		public static final  String MARSHAL_CHOICE_STRATEGY =
			"marshalChoiceStrategyClassName";
		/** Name of class providing logic to help with alternative selection
		 * when unmarshaling (Host to Java). */
		public static final  String UNMARSHAL_CHOICE_STRATEGY =
			"unmarshalChoiceStrategyClassName";
		/** Original source file location of the Cobol description. */
		public static final  String SRCE_LINE = 	"srceLine";
	}
	
	/** Since debugging an XJC plugin is somehow tricky, this variable
	 * will produce some debugging help. */
	private static boolean mDebug = false;
	
	/** {@inheritDoc} */
	@Override
	/** This will let XJC know that this plugin supports this argument*/
	public final String getOptionName() {
		return Const.OPTIONNAME;
	}
	
	/** {@inheritDoc} */
	@Override
	/** Just in case XJC requires a friendly comment on this plugin */
	public final String getUsage() {
		return Const.USAGE;
	}
	
	/** {@inheritDoc} */
	@Override
	/** This lets XJC know what are the namespaces to watch for in the source
	 *  schema */
	public final List < String > getCustomizationURIs() {
		return Collections.singletonList(Const.NS);
	}
	
	/** {@inheritDoc} */
	@Override
	/** Just to be extra sure, XJC will call us on each element from the source
	 * schema that seems to belong to the namespaces to watch for. We need to 
	 * tell XJC whether this is an actual supported customization. */
	public final boolean isCustomizationTagName(
			final String nsUri,
			final String localName) {
		
		return (nsUri.equals(Const.NS)
				&& (localName.equals(Const.ELEMENT)
				|| localName.equals(Const.ELEMENT_VALUE)));
	}
	
	/** {@inheritDoc} */
	@Override
	/** 
	 * This is where the real action takes place. XJC has done its job of
	 * building an in-memory model of the soon-to-be generated java classes.
	 * We are given a chance to change that model so that the generated
	 * classes will include the extra annotations that we need.
	 *  */
	public final boolean run(
			final Outline model,
			final Options opt,
			final ErrorHandler errorHandler) {
		
		long start = System.currentTimeMillis();

		/* Each simpleType at the root level in the source schema will become
		 * a JAXBElement in the ObjectFactory class. .*/
		for (CElementInfo eo : model.getModel().getAllElements()) {
			
			if (mDebug) {
				System.out.println(
						"CobolJAXBAnnotator::run::CElementInfo::"
						+ eo.fullName());
			}
			CPluginCustomization c =
				eo.getCustomizations().find(Const.NS, Const.ELEMENT);
			if (c == null) {
				continue;   // no customization --- nothing to inject here
			}

			/* Mark the annotation as acknowledged*/
			c.markAsAcknowledged();
			
		}
		
		/* Each complexType in the source schema will result in a class outline
		 * and its own implementation class. */
		for (ClassOutline co : model.getClasses()) {
			
			if (mDebug) {
				System.out.println(
						"CobolJAXBAnnotator::run::ClassOutline::"
						+ co.implClass);
			}
			
			for (FieldOutline fo : co.getDeclaredFields()) {
				
				if (mDebug) {
					System.out.println(
							"CobolJAXBAnnotator::run::FieldOutline::"
							+ fo.getPropertyInfo().getName(false));
				}
				
				/* Get the customization depending on whether this is a direct
				 * element or a reference to an element.Elements such as arrays
		         * of hexBinary will result in a CReferencePropertyInfo */
				CPluginCustomization c = null;
				if (fo.getPropertyInfo() instanceof CReferencePropertyInfo) {
					if (mDebug) {
						System.out.println(
								"FieldOutline is CReferencePropertyInfo");
					}
					
					for (CElement ce
							: ((CReferencePropertyInfo) fo.getPropertyInfo()).
							getElements()) {
						c = ce.getCustomizations().find(
								Const.NS, Const.ELEMENT);
					}
				} else {
					c = fo.getPropertyInfo().getCustomizations().find(
								Const.NS, Const.ELEMENT);
				}
				
				if (c == null) {
					continue;   // no customization --- nothing to inject here
				}
				if (mDebug) {
					String javaType = fo.getRawType().name();
					System.out.println(
							"CobolJAXBAnnotator::run::ClassOutline::"
							+ c.element.getLocalName()
							+ " type=" + javaType);
				}
				
				
				c.markAsAcknowledged();
				
				/* Inject a cobol annotation on this field. */
				JFieldVar jf =
					co.implClass.fields().get(
							fo.getPropertyInfo().getName(false));
				JAnnotationUse ce = jf.annotate(CobolElement.class);
				
				mapAnnotations(c, ce);
				
				/* HexBinary items are missing a JAXB annotation that
				 * we inject here */
				if (fo.getRawType().name().compareTo("byte[]") == 0) {
					JAnnotationUse xmlSchemaType =
						jf.annotate(XmlSchemaType.class);
					xmlSchemaType.param("name", "hexBinary");
				}
				
			}
		}
		
		long end = System.currentTimeMillis();
  		System.out.println("Cobol annotation success.");
    	System.out.println("Duration=" + (end - start) + " ms");
		
		return true;
	}
	
	/**
	 * Each annotation is extracted from the XML schema customization and
	 * injected back into the JAXB class code.
	 * 
	 * @param c the XML Schema annotation element
	 * @param ce the Java code Cobol annotation
	 */
	private void mapAnnotations(
			final CPluginCustomization c,
			final JAnnotationUse ce) {
		
		ce.param("cobolName",
				c.element.getAttribute(Const.COBOL_NAME).toString());
		
		ce.param("type", CobolType.valueOf(c.element.getAttribute(
				Const.TYPE).toString()));
		
		String sByteLength =
			c.element.getAttribute(Const.BYTE_LENGTH).toString();
		if (sByteLength != null && sByteLength.length() > 0) {
			ce.param("byteLength", Integer.parseInt(sByteLength));
		}
		
		boolean bIsJustifiedRight =
			Boolean.valueOf(c.element.getAttribute(Const.IS_JUSTIFIED_RIGHT));
		ce.param("isJustifiedRight", bIsJustifiedRight);
		
		boolean bIsSigned =
			Boolean.valueOf(c.element.getAttribute(Const.IS_SIGNED));
		ce.param("isSigned", bIsSigned);
		
		boolean bIsSignLeading =
			Boolean.valueOf(c.element.getAttribute(Const.IS_SIGN_LEADING));
		ce.param("isSignLeading", bIsSignLeading);
		
		boolean bIsSignSeparate =
			Boolean.valueOf(c.element.getAttribute(Const.IS_SIGN_SEPARATE));
		ce.param("isSignSeparate", bIsSignSeparate);
		
		String sDigits =
			c.element.getAttribute(Const.TOTAL_DIGITS).toString();
		if (sDigits != null && sDigits.length() > 0) {
			ce.param("totalDigits", Integer.parseInt(sDigits));
		}
		
		String sDecPos =
			c.element.getAttribute(Const.FRACTION_DIGITS).toString();
		if (sDecPos != null && sDecPos.length() > 0) {
			ce.param("fractionDigits", Integer.parseInt(sDecPos));
		}
		
		String sMinOccurs = c.element.getAttribute(Const.MIN_OCCURS).toString();
		if (sMinOccurs != null && sMinOccurs.length() > 0) {
			ce.param("minOccurs", Integer.parseInt(sMinOccurs));
		}
		
		String sMaxOccurs = c.element.getAttribute(Const.MAX_OCCURS).toString();
		if (sMaxOccurs != null && sMaxOccurs.length() > 0) {
			ce.param("maxOccurs", Integer.parseInt(sMaxOccurs));
		}
		
		String sDependingOn =
			c.element.getAttribute(Const.DEPENDING_ON).toString();
		if (sDependingOn != null && sDependingOn.length() > 0) {
			ce.param("dependingOn", sDependingOn);
		}
		
		boolean bIsODOObject =
			Boolean.valueOf(c.element.getAttribute(Const.IS_ODO_OBJECT));
		if (bIsODOObject) {
			ce.param("isODOObject", bIsODOObject);
		}

		String sRedefines =
			c.element.getAttribute(Const.REDEFINES).toString();
		if (sRedefines != null && sRedefines.length() > 0) {
			ce.param("redefines", sRedefines);
		}
		
		boolean bIsRedefined =
			Boolean.valueOf(c.element.getAttribute(Const.IS_REDEFINED));
		if (bIsRedefined) {
			ce.param("isRedefined", bIsRedefined);
		}

		boolean bIsCustomVariable =
			Boolean.valueOf(c.element.getAttribute(Const.IS_CUSTOM_VARIABLE));
		if (bIsCustomVariable) {
			ce.param("isCustomVariable", bIsCustomVariable);
		}

		String sMarshalChoiceStrategy =
			c.element.getAttribute(Const.MARSHAL_CHOICE_STRATEGY).toString();
		if (sMarshalChoiceStrategy != null
				&& sMarshalChoiceStrategy.length() > 0) {
			ce.param(Const.MARSHAL_CHOICE_STRATEGY, sMarshalChoiceStrategy);
		}
		
		String sUnmarshalChoiceStrategy =
			c.element.getAttribute(Const.UNMARSHAL_CHOICE_STRATEGY).toString();
		if (sUnmarshalChoiceStrategy != null
				&& sUnmarshalChoiceStrategy.length() > 0) {
			ce.param(Const.UNMARSHAL_CHOICE_STRATEGY, sUnmarshalChoiceStrategy);
		}
		
	}
}
