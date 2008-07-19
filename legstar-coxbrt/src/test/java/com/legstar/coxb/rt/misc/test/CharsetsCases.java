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
package com.legstar.coxb.rt.misc.test;

import junit.framework.TestCase;
import java.nio.charset.Charset;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;

public class CharsetsCases extends TestCase {
	
	@SuppressWarnings("unchecked")
	public void testAllCharsets() {
		SortedMap <String, Charset> cs = Charset.availableCharsets();
        Iterator < ? > it = cs.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry me = (Map.Entry)it.next();
            String key = (String) me.getKey();
            Charset value = (Charset) me.getValue();
            System.out.println(key);
            System.out.print("  ");
            Iterator < ? > ita = value.aliases().iterator();
            while (ita.hasNext()) {
                String alias = (String)ita.next();
                System.out.print(alias);
                System.out.print(" ");
            }
            System.out.println();
        }
	}

}
