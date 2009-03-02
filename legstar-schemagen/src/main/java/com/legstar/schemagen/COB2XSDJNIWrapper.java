/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.schemagen;

/**
 * This class is used to generate a JNI wrapper for the COB2XSD C module.
 * COB2XSD transforms a Cobol source into an annotated XML schema.
 *
 * @author Fady Moussallam
 * 
 */
public class COB2XSDJNIWrapper {

    /**
     * Native interface to CB2XSD main entry point. 
     * 
     * @param inVars a Class encapsulating input parameters
     * @param outVars a Class encapsulating output parameters
     * @return the native call return code 
     * */
    public final native int cob2xsd(final InVars inVars, final OutVars outVars);
    static 
    {
        System.loadLibrary("COB2XSDJNIWrapper"); 
    }

    /**
     * Class encapsulating COB2XSD input parameters.
     */
    public class InVars {
        /** True generate verbose traces on stdout. */
        public boolean debugMode;

        /** Cobol source. */
        public String inFile;

        /** Name of XSD root element. Null means ignore any upper level simple
         *  types. */
        public String inRootName; 

        /** Cobol compiler options in effect. */
        public CobolOptions cobolOptions; 

        /** XML schema generation options. */
        public XsdOptions xsdOptions;

        /** Output XML schema file name. */
        public String outFile;           
    }

    /**
     * Class encapsulating COB2XSD output parameters.
     */
    public class OutVars {
        /** Message returned from COB2XSD. */
        public String message;
    }

    /** Cobol compiler related options options. */
    public class CobolOptions {
        /** true = DEBUG lines are to be parsed.      */
        public boolean includeDebugLines = false; 
        /** true = TRUNC(BIN) option is in effect.          */
        public boolean truncBin = false;  
        /** The currency symbol used in numerics.      */
        public String currencySign = "$"; 
        /** true = Comma is the decimal separator.   */
        public boolean decimalPointIsComma = false;
        /** true = NSYMBOL(DBCS) option is in effect.       */
        public boolean nsymbolDbcs = false; 
        /** true = QUOTE option in effect otherwise APOST.  */
        public boolean quote = false;          
    }

    /** XSD generation options. */
    public class XsdOptions {
        /** True = Replace - sign from cobol names. */
        public boolean replaceMinus = false; 

        /** Char to use to replace - sign.           */
        public String replaceMinusChar = "_"; 

        /** True = Remove - sign from cobol names.  */
        public boolean removeMinus = true; 

        /** True = Comma is the decimal separator.  */
        public boolean uppercaseToLower = true;

        /** True = Uppercase first char and chars following minus sign.  */
        public boolean firstcharUpper = true; 

        /** Used to build complex type names.       */
        public String typeSuffix = "Type";

        /** Xml schema namespace prefix.              */
        public String xsPrefix = "xs"; 

        /** XML Schema namespace. */
        public String xsNs = "http://www.w3.org/2001/XMLSchema"; 

        /** Target namespace prefix.                 */
        public String xsnsPrefix = "xsns"; 

        /** Target namespace.           */
        public String xsnsNs = "http://tempuri/schemagen";

        /** Xml schema Cobol appinfo prefix.          */
        public String xscbPrefix = "cb"; 

        /** The Cobol appinfo schema namespace.   */
        public String xscbNs = "http://www.legsem.com/legstar/xml/cobol-binding-1.0.1.xsd"; 

        /** Jaxb namespace prefix .                   */
        public String xsjaxbPrefix = "jaxb"; 

        /** Jaxb namespace.     */
        public String xsjaxbNs = "http://java.sun.com/xml/ns/jaxb";

        /** Jaxb version.                             */
        public String xsjaxbVersion = "2.0"; 

        /** Jaxb package used for generated classes.     */
        public String xsjaxbPackage = "com.tempuri"; 

        /** Add schema definitions.               */
        public boolean xsdHeaderFooter = true;   
    } 
}
