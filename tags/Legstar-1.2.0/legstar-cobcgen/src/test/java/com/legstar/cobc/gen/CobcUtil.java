/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
