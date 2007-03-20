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
package com.legstar.coxb;

/**
 * This is the list of all the cobol item types that are supported.
 *
 * @author Fady Moussallam
 * 
*/
public enum CobolType {
	/** A complex element. */ 
	GROUP_ITEM,
	/** Contains only alphabetic characters. */ 
     ALPHABETIC_ITEM,
 	/** A UTF-16 character string. */ 
     NATIONAL_ITEM,
  	/** A DBCS character string. */ 
    DBCS_ITEM,
  	/** An alphanumeric string with editing characters. */ 
     ALPHANUMERIC_EDITED_ITEM,
   	/** An alphanumeric string. */ 
     ALPHANUMERIC_ITEM,
	/** A string of binary data (not translated). */ 
     OCTET_STREAM_ITEM,
 	/** A 4 bytes hexadecimal floating point numeric. */ 
     SINGLE_FLOAT_ITEM,
  	/** An 8 bytes hexadecimal floating point numeric. */ 
     DOUBLE_FLOAT_ITEM,
   	/** An packed decimal numeric. */ 
     PACKED_DECIMAL_ITEM,
	/** A string of digits also known as zoned decimal. */ 
     ZONED_DECIMAL_ITEM,
 	/** A numeric string with editing characters. */ 
     NUMERIC_EDITED_ITEM,
  	/** An array index. */ 
     INDEX_ITEM,
   	/** An pointer to a memory location. */ 
     POINTER_ITEM ,
	/** An pointer to a procedure. */ 
     PROC_POINTER_ITEM,
 	/** An pointer to a function. */ 
     FUNC_POINTER_ITEM,
  	/** A reference to an object. */ 
     OBJECT_ITEM,
   	/** A string of characters representing a floating point numeric. */ 
     EXTERNAL_FLOATING_ITEM,
	/** A binary item with fixed number of digits. */ 
     BINARY_ITEM,
 	/** A binary item . */ 
     NATIVE_BINARY_ITEM
}
