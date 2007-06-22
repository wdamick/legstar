package com.legstar.test.coxb;



import java.math.BigDecimal;
import java.math.BigInteger;

import com.legstar.test.coxb.fixarnum.DfhcommareaType;

import junit.framework.TestCase;

public class MarshalFixarnumTest extends TestCase {

	private final static String SCHEMA_NAME = "fixarnum";
	
	public void testFixarnum() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);

		dfhcommareaType.getCArrayPd().add(new BigDecimal("16534.23"));
		dfhcommareaType.getCArrayPd().add(new BigDecimal("1.5"));
		dfhcommareaType.getCArrayPd().add(new BigDecimal("184"));
		
		dfhcommareaType.getCArrayZd().add(new BigDecimal("534.236"));
		dfhcommareaType.getCArrayZd().add(new BigDecimal("45.007"));
		dfhcommareaType.getCArrayZd().add(new BigDecimal("1.95"));
		
		dfhcommareaType.getCArrayZi().add(new Integer("9998"));
		dfhcommareaType.getCArrayZi().add(new Integer("0"));
		dfhcommareaType.getCArrayZi().add(new Integer("178"));

		dfhcommareaType.getCArrayBi().add(new Long("999899998"));
		dfhcommareaType.getCArrayBi().add(new Long("676767"));
		dfhcommareaType.getCArrayBi().add(new Long("36789013"));

		dfhcommareaType.getCArrayNi().add(new BigInteger("123456789012345678"));
		dfhcommareaType.getCArrayNi().add(new BigInteger("6767679998"));
		dfhcommareaType.getCArrayNi().add(new BigInteger("36789184"));

		//		      <--C-ARRAY-PD-COMP-3-- ><-------C-ARRAY-ZD-DISPLAY---------><--C-ARRAY-ZI-DISPLAY--><---C-ARRAY-BI-COMP----><------------C-ARRAY-NI-COMP-5----------------->
		//		      <------><------><------><----------><----------><----------><------><------><------><------><------><------><--------------><--------------><-------------->
		//		      1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		      1653423+0000150+0018400+5 3 4 2 3 6 0 4 5 0 0 7 0 0 1 9 5 0 9 9 9 8 0 0 0 0 0 1 7 89998999980067676736789013
		assertEquals("1653423f0000150f0018400ff5f3f4f2f3f6f0f4f5f0f0f7f0f0f1f9f5f0f9f9f9f8f0f0f0f0f0f1f7f83b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 78));
	}
}
