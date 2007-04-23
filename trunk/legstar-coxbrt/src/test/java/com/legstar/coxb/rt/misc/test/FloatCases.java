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

public class FloatCases extends TestCase {
	
	public void testFloat1() {
		System.out.println("=================================================");
		System.out.println("Java 1.234e3f");
		int javaIntBits = Float.floatToIntBits(1.234e3f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
		System.out.println("Host +1234");
		int hostIntBits = Integer.parseInt("434D2000",16);
		System.out.println(Integer.toString(hostIntBits));
		System.out.println(Integer.toHexString(hostIntBits));
		System.out.println(Integer.toBinaryString(hostIntBits));
	}
	
	public void testFloat2() {
		System.out.println("=================================================");
		System.out.println("Java +456790.00675");
		int javaIntBits = Float.floatToIntBits(+456790.00675f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	
	public void testFloat3() {
		System.out.println("=================================================");
		System.out.println("Java +0");
		int javaIntBits = Float.floatToIntBits(0f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	
	public void testFloat4() {
		System.out.println("=================================================");
		System.out.println("Java +1");
		int javaIntBits = Float.floatToIntBits(1f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat5() {
		System.out.println("=================================================");
		System.out.println("Java +2");
		int javaIntBits = Float.floatToIntBits(2f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat5b() {
		System.out.println("=================================================");
		System.out.println("Java +3");
		int javaIntBits = Float.floatToIntBits(3f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat5c() {
		System.out.println("=================================================");
		System.out.println("Java +4");
		int javaIntBits = Float.floatToIntBits(4f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat5d() {
		System.out.println("=================================================");
		System.out.println("Java +5");
		int javaIntBits = Float.floatToIntBits(5f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat6() {
		System.out.println("=================================================");
		System.out.println("Java +1000");
		int javaIntBits = Float.floatToIntBits(1000f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat7() {
		System.out.println("=================================================");
		System.out.println("Java +1000000");
		int javaIntBits = Float.floatToIntBits(1000000f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat8() {
		System.out.println("=================================================");
		System.out.println("Java +8388608");
		int javaIntBits = Float.floatToIntBits(8388608f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	
	public void testFloat9() {
		System.out.println("=================================================");
		System.out.println("Java +16777216");
		int javaIntBits = Float.floatToIntBits(16777216f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	
	public void testFloat10() {
		System.out.println("=================================================");
		System.out.println("Java -1");
		int javaIntBits = Float.floatToIntBits(-1f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	
	public void testFloat11() {
		System.out.println("=================================================");
		System.out.println("Java -2");
		int javaIntBits = Float.floatToIntBits(-2f);
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat12() {
		System.out.println("=================================================");
		System.out.println("Java 1234");
		int javaIntBits = Float.floatToIntBits(1234f);
		System.out.println(Float.parseFloat("0.1234e4f"));
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat14() {
		System.out.println("=================================================");
		int javaInt = 8388608;
		System.out.println(javaInt);
		System.out.println(Integer.toHexString(javaInt));
		System.out.println(Integer.toBinaryString(javaInt));
		int s24 = (javaInt << 16);
		System.out.println(s24);
		System.out.println(Integer.toBinaryString(s24));
		
	}
	
	public void testFloat13() {
		System.out.println("=================================================");
		System.out.println("Java 1234.5678");
		int javaIntBits = Float.floatToIntBits(1234.5678f);
		System.out.println(Float.parseFloat("1234.5678f"));
		System.out.println(javaIntBits);
		System.out.println(Integer.toHexString(javaIntBits));
		System.out.println(Integer.toBinaryString(javaIntBits));
		
	}
	public void testFloat15() {
		System.out.println("=================================================");
		System.out.println("Java 1234");
		JavaFloat cF = parseJavaFloat(+1234f);
		System.out.println(Float.parseFloat("0.1234e4f"));
		System.out.println("sign=" + Integer.toString(cF.sign));
		System.out.println("expo=" + Integer.toString(cF.exponent));
		System.out.println("mant=" + Integer.toString(cF.mantissa));
		System.out.println("result=" + Integer.toString(cF.toInt()));
	}

	public void testFloat16() {
		System.out.println("=================================================");
		System.out.println("Java 1234");
		JavaFloat cF = new JavaFloat();
		cF.sign = 0;
		cF.exponent = 10;
		cF.mantissa = 10108928;
		float f = createJavaFloat(cF);
		System.out.println(binaryString(Float.floatToIntBits(f), 32));
		System.out.println(f);
	}
	
	public void testDouble1() {
		System.out.println("=================================================");
		System.out.println(Double.doubleToLongBits(1.234e3d));
		Double var = new Double(Double.longBitsToDouble(4653142004841054208l));
		System.out.println(var.toString());
		System.out.println(Long.toHexString(Double.doubleToLongBits(1.234e3d)));
		System.out.println(Long.toBinaryString(Double.doubleToLongBits(1.234e3d)));
		System.out.println(Long.toString(Long.parseLong("4093480000000000",16)));
		/*                                                    12345678123456781234567812345678*/
		System.out.println(Long.toString(Long.parseLong("100000010010011010010000000000000000000000000000000000000000000",2)));
		System.out.println(Long.toString(Integer.parseInt("01000000",2)));
		System.out.println(Long.toString(Long.parseLong("10010011010010000000000000000000000000000000000000000000",2)));
		System.out.println(Long.toString(Integer.parseInt("10011010010",2)));
		System.out.println(Long.toBinaryString(Integer.parseInt("1234",10)));
		
		System.out.println("host +1234");
		System.out.println(Long.toString(Long.parseLong("434D200000000000",16)));
		System.out.println(Long.toBinaryString(Long.parseLong("434D200000000000",16)));
		Double varHost = new Double(Double.longBitsToDouble(4849567558119981056l));
		System.out.println(varHost.toString());
		System.out.println(Long.toString(Integer.parseInt("1000011",2)));
		System.out.println(Long.toString(Long.parseLong("01001101001000000000000000000000000000000000000000000000",2)));
		System.out.println(Long.toString(Integer.parseInt("1000011010011010010000000000000",2)));
	}
	
	public String binaryString(int number, int minDigits) {
		//	assert that digits <= 31
		//	Will need to use long if digits > 31
		
		int orMask = 1 << minDigits;
		
		if (number < 0 || (orMask > 0 &&	number >= orMask)) { // result will be longer
			return Integer.toBinaryString(number);
		} else	{
			String s = Integer.toBinaryString(number | orMask);
			return s.substring(1);
		}
		
	}
	
	public class JavaFloat {
		public int sign;
		public int exponent;
		public int mantissa;
		
		public int toInt() {
			
			if (exponent >= 0 && exponent < 23)
				return mantissa >>> (23 - exponent);
			else
				return mantissa;
		}
	}
	
	/**
	 * Extracts the sign, exponent and mantissa from a Java float.
	 * @param f a float
	 * @return a float structure
	 */
	public JavaFloat parseJavaFloat(float f) {
		JavaFloat cF = new JavaFloat();
		int javaIntBits = Float.floatToIntBits(f);
		
		/* First bit left (bit 31) is the sign: 0 = positive, 1 = negative */
		cF.sign = (javaIntBits & 0x80000000) >>> 31;
		
		/* Bits 30-23 (8 bits) represents the exponent offset by 127, this number is called excess
		 * so you get the exponent as E= excess - 127 */
		int excess = (javaIntBits & 0x7f800000) >>> 23;
		cF.exponent = (excess - 127);
		
		/* Bits 22-0 (23 bits) represents the mantissa in a form called "1-plus" normalized. This means
		 * that the real mantissa is actually 1.b(22)b(23)...b(0) where the intiial "1" is implicit. */
		int orMask = 1 << 23;
		cF.mantissa = javaIntBits & 0x007fffff | orMask;
		
		return cF;
	}
	
	/**
	 * Reconstruct a java float from its sign, exponent and mantissa components.
	 * @param cF the float sctructure
	 * @return a float
	 */
	public float createJavaFloat(JavaFloat cF) {
		int javaIntBits = cF.mantissa;
		javaIntBits = javaIntBits | ((cF.exponent + 127) << 23);
		javaIntBits = javaIntBits | (cF.sign << 31);
		return Float.intBitsToFloat(javaIntBits);
	}
}
