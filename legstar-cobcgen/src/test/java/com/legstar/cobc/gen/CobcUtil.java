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
package com.legstar.cobc.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public final class CobcUtil {

	/**
	 *  Reads a complete source file into a string.
	 *  @param inFile the file to read
	 *  @param debug true if file content should be traced
	 * @throws IOException if file cannot be read
	 *  */
	public static String getSource(
			File inFile, boolean debug) throws IOException {
		/* Read the resulting output source*/
    	BufferedReader in = new BufferedReader(new FileReader(inFile));
        StringBuffer resStr = new StringBuffer();
        String str = in.readLine();
        while (str != null) {
        	if (debug) {
        		System.out.println(str);
        	}
        	resStr.append(str);
        	str = in.readLine();
        }
        in.close();
		return resStr.toString();
	}
}
