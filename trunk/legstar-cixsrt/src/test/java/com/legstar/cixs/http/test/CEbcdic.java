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
package com.legstar.cixs.http.test;

/**
 * Simplistic class used to display binary content and translate into ASCII and EBCDIC
  *
 */
public class CEbcdic {
    private static final int SIZE = 95;
    private static final int MAX_TRACES_BYTES = 500;  // Limit the size printed
    
    private static final int ASCII[] = {
            0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 0x0028,
            0x0029, 0x002a, 0x002b, 0x002c, 0x002d, 0x002e, 0x002f,
            0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 0x0038,
            0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f,
            0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048,
            0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f,
            0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058,
            0x0059, 0x005a, 0x005b, 0x005c, 0x005d, 0x005e, 0x005f,
            0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 0x0068,
            0x0069, 0x006a, 0x006b, 0x006c, 0x006d, 0x006e, 0x006f,
            0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 0x0078,
            0x0079, 0x007a, 0x007b, 0x007c, 0x007d, 0x007e
    };      
            private static final int EBCDIC[] = {
            0x0040, 0x005a, 0x007f, 0x007b, 0x005b, 0x006c, 0x0050, 0x007d, 0x004d,
            0x005d, 0x005c, 0x004e, 0x006b, 0x0060, 0x004b, 0x0061,
            0x00f0, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7, 0x00f8,
            0x00f9, 0x007a, 0x005e, 0x004c, 0x007e, 0x006e, 0x006f,
            0x007c, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x00c7, 0x00c8,
            0x00c9, 0x00d1, 0x00d2, 0x00d3, 0x00d4, 0x00d5, 0x00d6,
            0x00d7, 0x00d8, 0x00d9, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7,
            0x00e8, 0x00e9, 0x00ad, 0x00e0, 0x00bd, 0x005f, 0x006d,
            0x0079, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088,
            0x0089, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096,
            0x0097, 0x0098, 0x0099, 0x00a2, 0x00a3, 0x00a4, 0x00a5, 0x00a6, 0x00a7,
            0x00a8, 0x00a9, 0x00c0, 0x006a, 0x00d0, 0x00a1
    };      

/**
* Translates a int, either EBCDIC to ASCII
* @param int the int to be translated
* @return int the translated int
* @exception
**/                                                                    
public final static int translateByte(int i) {
  for (int j = 0; j<SIZE; j++) {
      if (i == EBCDIC[j]) {
          return ASCII[j];
      }
  }               
  return i;
}
/**
 * Utility to display binary content with both ASCII and EBCDIC decoding
 * @param b the byte array to print
 */
public final static void  printHex(byte[] b) {
	System.out.println("     - -- -- -- -- -- binary content -- -- -- -- -- --  - -----ASCII------ - -----EBCDIC-----");
    if (b == null) {
    	System.out.println("     ... null content");
    	return;
    }
	for (int i = 0; i < b.length && i < MAX_TRACES_BYTES; ++i) {
        /* print every 16 byte on a different line */
    	if (i % 16 == 0) {
            System.out.print (Integer.toHexString ((i & 0xFFFF) | 0x10000).substring(1,5) + " - ");
        }
    	/* display binary values as hex */
    	System.out.print (Integer.toHexString((b[i]&0xFF) | 0x100).substring(1,3) + " ");
        if (i % 16 == 15 || i == b.length - 1)
        {
            int j;
            for (j = 16 - i % 16; j > 1; --j)
                System.out.print ("   ");
            System.out.print (" - ");
            int start = (i / 16) * 16;
            int end = (b.length < i + 1) ? b.length : (i + 1);
            /* decode as ascii */
            for (j = start; j < end; ++j)
                if (b[j] >= 32 && b[j] <= 126)
                    System.out.print ((char)b[j]);
                else
                    System.out.print (".");

            /* decode as ebcdic */
            for (j = 16 - i % 16; j > 1; --j)
                System.out.print (" ");
            System.out.print (" - ");
            int e = 0;
            for (j = start; j < end; ++j) {
                e = translateByte(b[j]&0xFF);
                if (e >= 32 && e <= 126)
                    System.out.print ((char)e);
                else
                    System.out.print (".");
            }
            System.out.println ();
        }
    }
    
    if (b.length > MAX_TRACES_BYTES ) {
        System.out.println ();
        System.out.println ("...data was truncated at 500 bytes");
    }
}


}
