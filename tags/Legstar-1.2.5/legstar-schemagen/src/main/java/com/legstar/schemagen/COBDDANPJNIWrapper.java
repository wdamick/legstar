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
 * This class is used to generate a JNI wrapper for the COBDDANP C module.
 * COBDDANP analyzes a Cobol picture clause and parses its elements into
 * individual properties readily available to java code. 
 *
 * @author Fady Moussallam
 * 
 */
public class COBDDANPJNIWrapper {

    /**
     * Native interface to COBDDANP main entry point. 
     * 
     * @param inVars a Class encapsulating input parameters
     * @param outVars a Class encapsulating output parameters
     * @return the native call return code 
     * */
    public final native int anapict(final InVars inVars, final OutVars outVars);
    static 
    {
        System.loadLibrary("COBDDANPJNIWrapper"); 
    }

    /**
     * Class encapsulating COBDDANP input parameters.
     */
    public class InVars {
        /** True generate verbose traces on stdout. */
        public boolean debugMode;

        /** The Cobol usage clause. */
        public String usage;

        /** The Cobol picture clause. */
        public String picture;

        /** The Cobol sign is separate clause. */
        public boolean signSeparate;
    }

    /**
     * Class encapsulating COBDDANP output parameters.
     */
    public class OutVars {

        /** The cobol data element type. */
        public int dataType;

        /** The cobol total number of digits for numerics. */
        public int totalDigits;

        /** The cobol fractional number of digits for numerics. */
        public int fractionDigits;

        /** Whether the cobol numeric is signed. */
        public int sign;

        /** The size of the cobol element in bytes. */
        public int byteLength;

        /** The native method return message. */
        public String message;
    }
}
