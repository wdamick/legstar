/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

/**
 * Arrays or byte arrays.
 * <p/>
 * 
 * @see https://code.google.com/p/legstar/issues/detail?id=182
 * 
 *      <pre>
 * 01 COMMAREA.
 *     05 A-BINARY   OCCURS 2 PIC X(4).
 * </pre>
 * 
 */
public class MarshalIssue82Test extends AbstractTestMarshal {

    public void testIssue82() throws Exception {
        com.legstar.test.coxb.issue182.Commarea commarea = new com.legstar.test.coxb.issue182.Commarea();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue185.ObjectFactory(), commarea);
        assertNotNull(ccem);
    }

}
