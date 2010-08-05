package com.legstar.coxb.transform;

import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Generic code useful for test classes in this package.
 *
 */
public class AbstractTestTransformers extends TestCase {

    /** A character set to use for string conversions. */
    public static final String STRING_FRENCH_CHARSET = "IBM01147";
    
    /** A character set to use for string conversions. */
    public static final String STRING_US_CHARSET = "IBM01140";
    
    /** A raw zos serialization with french encoded char. */
    public static final String RAW_LSFILEAE_DATA_IBM01147 =
       /* 0 0 0 1 0 0 T O T O                                 L A B A S ç S T R E E T                */
        "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e2e0e2e3d9c5c5e34040404040404040"
       /* 8 8 9 9 3 3 1 4 1 0 0 4 5 8     0 0 1 0 0 . 3 5 A   V O I R      */
     +  "f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";

    /** A raw zos serialization with US encoded char. */
    public static final String RAW_LSFILEAE_DATA_IBM01140 =
       /* 0 0 0 1 0 0 T O T O                                 L A B A S ç S T R E E T                */
        "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e248e2e3d9c5c5e34040404040404040"
       /* 8 8 9 9 3 3 1 4 1 0 0 4 5 8     0 0 1 0 0 . 3 5 A   V O I R      */
     +  "f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";

    /**
     * Check all member variables from value object with French character set.
     * @param dfhcommarea the value object
     */
    public void checkLsfileaeIBM01147(final Dfhcommarea dfhcommarea) {
        assertEquals("00100.35", dfhcommarea.getComAmount());
        assertEquals("A VOIR", dfhcommarea.getComComment());
        assertEquals("100458", dfhcommarea.getComDate());
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("LABASçSTREET", dfhcommarea.getComPersonal().getComAddress());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone());
    }

    /**
     * Check all member variables from value object with US character set.
     * @param dfhcommarea the value object
     */
    public void checkLsfileaeIBM01140(final Dfhcommarea dfhcommarea) {
        assertEquals("00100.35", dfhcommarea.getComAmount());
        assertEquals("A VOIR", dfhcommarea.getComComment());
        assertEquals("100458", dfhcommarea.getComDate());
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("LABAS\\STREET", dfhcommarea.getComPersonal().getComAddress());
        assertEquals("TOTO", dfhcommarea.getComPersonal().getComName());
        assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone());
    }

    /**
     * This method creates an instance of a value object and sets its properties.
     * @return a value object object
     */
    public static Dfhcommarea getLsfileaeObject() {
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

}
