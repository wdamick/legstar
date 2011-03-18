/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb;

/**
 * These are the COBOL usage attribute that are derived from the original COBOL
 * statement and then propagated as attributes in XML Schema and java annototations.
 *
 * @author Fady Moussallam
 * 
 */
public final class CobolUsage {
    
    /**
     * Utility class.
     */
    private CobolUsage() {
        
    }
    
    /** An character string. */ 
    public static final String  DISPLAY = "DISPLAY";
    /** An double byte character string. */ 
    public static final String  DISPLAY_1 = "DISPLAY-1";
    /** A UTF16-BE character string. */ 
    public static final String  NATIONAL = "NATIONAL";
    /** A binary numeric. */ 
    public static final String  BINARY = "BINARY";
    /** A native binary numeric. */ 
    public static final String  COMP_5 = "COMP-5";
    /** A packed numeric. */ 
    public static final String  PACKED_DECIMAL = "PACKED-DECIMAL";
    /** A single float. */ 
    public static final String  COMP_1 = "COMP-1";
    /** A double float. */ 
    public static final String  COMP_2 = "COMP-2";
    /** An index. */ 
    public static final String  INDEX = "INDEX";
    /** A pointer. */ 
    public static final String  POINTER = "POINTER";
    /** A pointer to a procedure. */ 
    public static final String  PROCEDURE_POINTER = "PROCEDURE-POINTER";
    /** A pointer to a function. */ 
    public static final String  FUNCTION_POINTER = "FUNCTION-POINTER";
}
