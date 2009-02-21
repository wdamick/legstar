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
package com.legstar.coxb.transform;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.vararcom.CArray;

import junit.framework.TestCase;

/**
 * Test AbstractHostToJavaTransformer.
 */
public class AbstractJavaToHostTransformerTest extends TestCase {
    
    /** A raw zos serialization with french encoded char. */
    private static final String RAW_LSFILEAE_DATA_IBM01147 =
       /* 0 0 0 1 0 0 T O T O                                 L A B A S ç S T R E E T                */
        "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e2e0e2e3d9c5c5e34040404040404040"
       /* 8 8 9 9 3 3 1 4 1 0 0 4 5 8     0 0 1 0 0 . 3 5 A   V O I R      */
     +  "f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";

    /** A raw zos serialization with US encoded char. */
    private static final String RAW_LSFILEAE_DATA_IBM01140 =
       /* 0 0 0 1 0 0 T O T O                                 L A B A S ç S T R E E T                */
        "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e248e2e3d9c5c5e34040404040404040"
       /* 8 8 9 9 3 3 1 4 1 0 0 4 5 8     0 0 1 0 0 . 3 5 A   V O I R      */
     +  "f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";

    /**
     * Test transforming a java object to a byte array (using reflection binding).
     * @throws HostTransformException if transformation fails
     */
    public void testTransform() throws HostTransformException {
        JavaToHostLsfileaeTransformer javaToHostTransformer = new JavaToHostLsfileaeTransformer();
        byte[] hostData = javaToHostTransformer.transform(getLsfileaeObject());
        assertTrue(null != hostData);
        assertEquals(79, hostData.length);
        assertEquals(RAW_LSFILEAE_DATA_IBM01140, HostData.toHexString(hostData));
        
        hostData = javaToHostTransformer.transform(getLsfileaeObject(), "IBM01147");
        assertTrue(null != hostData);
        assertEquals(79, hostData.length);
        assertEquals(RAW_LSFILEAE_DATA_IBM01147, HostData.toHexString(hostData));
    }
    
    /**
     * Test transforming a java object to a byte array of variable size (using reflection binding).
     * @throws HostTransformException if transformation fails 
     */
    public void testTransformVariable() throws HostTransformException {
        JavaToHostVararcomTransformer javaToHostTransformer = new JavaToHostVararcomTransformer();
        byte[] hostData = javaToHostTransformer.transform(getVararcomObject((short) 0));
        assertTrue(null != hostData);
        assertEquals(2, hostData.length);
        assertEquals("0000", HostData.toHexString(hostData));

        hostData = javaToHostTransformer.transform(getVararcomObject((short) 3));
        assertTrue(null != hostData);
        assertEquals(23, hostData.length);
        /*               3A B J A D    0A B J A D    7A B J A D   14 */
        assertEquals("0003c1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000e", HostData.toHexString(hostData));
    }
    
    /**
     * This method creates an instance of a value object and sets its properties.
     * @return a value object object
     */
    public static com.legstar.test.coxb.lsfileae.Dfhcommarea getLsfileaeObject() {
        com.legstar.test.coxb.lsfileae.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.lsfileae.Dfhcommarea();
        dfhcommarea.setComNumber(100);
        dfhcommarea.setComDate("100458");
        dfhcommarea.setComAmount("00100.35");
        dfhcommarea.setComComment("A VOIR");
        ComPersonal personal = new ComPersonal();
        personal.setComName("TOTO");
        personal.setComAddress("LABASçSTREET");
        personal.setComPhone("88993314");
        dfhcommarea.setComPersonal(personal);
        return dfhcommarea;
    }

    /**
     * This method creates an instance of a value object and sets its properties.
     * @param nbItems the number of variable items requested
     * @return a value object object
     */
    public static com.legstar.test.coxb.vararcom.Dfhcommarea getVararcomObject(final short nbItems) {
        com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.vararcom.Dfhcommarea();
        dfhcommarea.setCItemsNumber(nbItems);
        for (int i = 0; i < nbItems; i++) {
            CArray item = new CArray();
            item.setCItem1("ABJAD");
            item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
            dfhcommarea.getCArray().add(item);
        }
        return dfhcommarea;
    }
}
