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
package com.legstar.coxb.rt.misc.test;

import junit.framework.TestCase;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;

public class CharsetsCases extends TestCase {
	
	public void testAllCharsets() {
		SortedMap <String, Charset> cs = Charset.availableCharsets();
        Iterator it = cs.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry me = (Map.Entry)it.next();
            String key = (String) me.getKey();
            Charset value = (Charset) me.getValue();
            System.out.println(key);
            System.out.print("  ");
            Iterator ita = value.aliases().iterator();
            while (ita.hasNext()) {
                String alias = (String)ita.next();
                System.out.print(alias);
                System.out.print(" ");
            }
            System.out.println();
        }
	}

}
