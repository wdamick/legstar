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
package com.legstar.coxb.convert.simple;

import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.host.HostData;

import junit.framework.TestCase;

/**
 * Test the COBOL COMP-1 TYPE.
 *
 */
public class FloatTest extends TestCase {

    /**
     * Generic conversion from java float to COBOL.
     * @param byteLength the COBOL receiving field size 
     * @param inputValue the input value as a string
     * @param expectedValue the expected value as a hex string
     */
    public static void toHost(
            final int byteLength,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = new byte[byteLength];
            Float javaFloat = new Float(inputValue);
            assertEquals(byteLength, CobolFloatSimpleConverter.toHostSingle(
                    javaFloat, hostBytes, 0));
            assertEquals(expectedValue, HostData.toHexString(hostBytes));
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Generic conversion from COBOL to java float.
     * @param byteLength the COBOL receiving field size 
     * @param inputValue the input value as a hex string
     * @param expectedValue the expected value as a string
     */
    public static void fromHost(
            final int byteLength,
            final String inputValue,
            final String expectedValue) {
        try {
            byte[] hostBytes = HostData.toByteArray(inputValue);
            Float javaFloat = CobolFloatSimpleConverter.fromHostSingle(
                    byteLength, hostBytes, 0);
            assertEquals(expectedValue, javaFloat.toString());
        } catch (CobolConversionException e) {
            fail(e.getMessage());
        }
    }

    /**
     * COMP-1 small value.
     */
    public void testToHost1234() {
        toHost(4, "+1234.0f", "434d2000");
    }

    /**
     * COMP-1 small value.
     */
    public void testFromHost1234() {
        fromHost(4, "434d2000", "1234.0");
    }

    /**
     * COMP-1 zero value.
     */
    public void testToHost0() {
        toHost(4, "+0.0f", "00000000");
    }

    /**
     * COMP-1 zero value.
     */
    public void testFromHost0() {
        fromHost(4, "00000000", "0.0");
    }

    /**
     * COMP-1 1 value.
     */
    public void testToHost1() {
        toHost(4, "+1.0f", "41100000");
    }

    /**
     * COMP-1 1 value.
     */
    public void testFromHost1() {
        fromHost(4, "41100000", "1.0");
    }

    /**
     * COMP-1 no exponent value.
     */
    public void testToHost345006p5678() {
        toHost(4, "+345006.5678f", "45543ae9");
    }

    /**
     * COMP-1 no exponent value.
     */
    public void testFromHost345006p5678() {
        fromHost(4, "45543ae9", "345006.56");
    }

    /**
     * COMP-1 negative exponent value.
     */
    public void testToHost798p20067em16() {
        toHost(4, "+798.20067e-16f", "361677a4");
    }

    /**
     * COMP-1 negative exponent value.
     */
    public void testFromHost798p20067em16() {
        fromHost(4, "361677a4", "7.982005E-14");
    }

    /**
     * Case where we are past the offset. Not enough data sent from the mainframe.
     */
    public void testFromHostPastOffset() {
        fromHost(4, "3616", "0.0");
    }

    /**
     * COMP-1 max exponent value.
     */
    public void testToHost3p40282347ep38() {
        toHost(4, "3.40282347e+38f", "60ffffff");
    }

    /**
     * COMP-1 max exponent value.
     */
    public void testFromHost3p40282347ep38() {
        fromHost(4, "60ffffff", "3.4028235E38");
    }

    /**
     * COMP-1 Tomkins Lost precision that can be explained (both ways).
     */
    public void testTompkins() {
        toHost(4, "25431.1234f", "4463571f");
        fromHost(4, "4463571f", "25431.121");
    }

    /**
     * COMP-1 Amit cases.
     */
    public void testAmitTotallyOff() {
        toHost(4, "0.070859075f", "401223d2");
        fromHost(4, "401223d2", "0.070859075");
    }

    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost1() {
        toHost(4, "0.2050727f", "40347fa5");
        fromHost(4, "40347fa5", "0.2050727");
    }
    
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost2() {
        toHost(4, "0.21563375f", "403733c6");
        fromHost(4, "403733c6", "0.21563375");
    }
    
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost3() {
        toHost(4, "0.86346835f", "40dd0c43");
        fromHost(4, "40dd0c43", "0.86346835");
    }

    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost4() {
        toHost(4, "0.31136322f", "404fb580");
        fromHost(4, "404fb580", "0.31136322");
    }
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost5() {
        toHost(4, "0.63990897f", "40a3d113");
        fromHost(4, "40a3d113", "0.63990897");
    }
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost6() {
        toHost(4, "0.7430732f", "40be3a0c");
        fromHost(4, "40be3a0c", "0.7430732");
    }
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost7() {
        toHost(4, "0.8783575f", "40e0dc0a");
        fromHost(4, "40e0dc0a", "0.8783575");
    }
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost8() {
        toHost(4, "0.03406465f", "3f8b8760");
        fromHost(4, "3f8b8760", "0.03406465");
    }
    /**
     * COMP-1 Amit cases.
     */
    public void testAmitPrecisionLost9() {
        toHost(4, "0.99642426f", "40ff15a9");
        fromHost(4, "40ff15a9", "0.99642426");
    }
}
