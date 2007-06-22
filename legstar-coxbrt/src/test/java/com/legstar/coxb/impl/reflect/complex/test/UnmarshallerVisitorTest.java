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
package com.legstar.coxb.impl.reflect.complex.test;

import java.math.BigInteger;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.host.HostData;
import com.legstar.host.HostException;

import junit.framework.TestCase;

/** Unmarshaller Visitor Test cases. */
public class UnmarshallerVisitorTest  extends TestCase {

	/**
	 * Unmarshal Lsfileae.
	 * @throws HostException if anything goes wrong
	 */
	public final void testLsfileae() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		            <----------><--------------------------------------><--------------------------------------><--------------><--------------><--------------><---------------->
		//		            1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 9 10111213141516171819201 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 9
		//		            0 0 0 1 0 0 T O T O                                 L A B A S   S T R E E T                 8 8 9 9 3 3 1 4 1 0 0 4 5 8 0 0 1 0 0 . 3 5 A   V O I R
		String hexString = "f0f0f0f1f0f0e3d6e3d640404040404040404040404040404040d3c1c2c1e240e2e3d9c5c5e34040404040404040f8f8f9f9f3f3f1f4f1f0f0f4f5f84040f0f0f1f0f04bf3f5c140e5d6c9d9404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileae.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.lsfileae.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.lsfileae.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(100, dfhcommarea.getComNumber());
		assertEquals("TOTO", dfhcommarea.getComPersonal().getComName().trim());
		assertEquals("LABAS STREET", dfhcommarea.getComPersonal().getComAddress().trim());
		assertEquals("88993314", dfhcommarea.getComPersonal().getComPhone().trim());
		assertEquals("100458", dfhcommarea.getComDate().trim());
		assertEquals("00100.35", dfhcommarea.getComAmount().trim());
		assertEquals("A VOIR", dfhcommarea.getComComment().trim());
	}
	
	/**
	 * Unmarshal Fixarsim.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarsim() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <---------------------------->
		//		              1 2 3 4 5 6 7 8 9 101112131415
		//		              P R E M I D E U X I T R O I S             
		String hexString   = "d7d9c5d4c9c4c5e4e7c9e3d9d6c9e2";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarsim.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarsim.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.fixarsim.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.fixarsim.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals("PREMI", dfhcommarea.getCArray().get(0).trim());
		assertEquals("DEUXI", dfhcommarea.getCArray().get(1).trim());
		assertEquals("TROIS", dfhcommarea.getCArray().get(2).trim());
	}

	/**
	 * Unmarshal Fixarnum.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarnum() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <--C-ARRAY-PD-COMP-3-- ><-------C-ARRAY-ZD-DISPLAY---------><--C-ARRAY-ZI-DISPLAY--><---C-ARRAY-BI-COMP----><------------C-ARRAY-NI-COMP-5----------------->
		//		              <------><------><------><----------><----------><----------><------><------><------><------><------><------><--------------><--------------><-------------->
		//		              1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 
		//		              1653423+0000150+0018400+5 3 4 2 3 6 0 4 5 0 0 7 0 0 1 9 5 0 9 9 9 8 0 0 0 0 0 1 7 89998999980067676736789013
		String hexString   = "1653423f0000150f0018400ff5f3f4f2f3f6f0f4f5f0f0f7f0f0f1f9f5f0f9f9f9f8f0f0f0f0f0f1f7f83b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.fixarnum.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.fixarnum.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		assertEquals("16534.23", dfhcommarea.getCArrayPd().get(0).toString());
		assertEquals("1.50", dfhcommarea.getCArrayPd().get(1).toString());
		assertEquals("184.00", dfhcommarea.getCArrayPd().get(2).toString());
		
		assertEquals("534.236", dfhcommarea.getCArrayZd().get(0).toString());
		assertEquals("45.007", dfhcommarea.getCArrayZd().get(1).toString());
		assertEquals("1.950", dfhcommarea.getCArrayZd().get(2).toString());
		
		assertEquals("9998", dfhcommarea.getCArrayZi().get(0).toString());
		assertEquals("0", dfhcommarea.getCArrayZi().get(1).toString());
		assertEquals("178", dfhcommarea.getCArrayZi().get(2).toString());

		assertEquals("999899998", dfhcommarea.getCArrayBi().get(0).toString());
		assertEquals("676767", dfhcommarea.getCArrayBi().get(1).toString());
		assertEquals("36789013", dfhcommarea.getCArrayBi().get(2).toString());

		assertEquals("123456789012345678", dfhcommarea.getCArrayNi().get(0).toString());
		assertEquals("6767679998", dfhcommarea.getCArrayNi().get(1).toString());
		assertEquals("36789184", dfhcommarea.getCArrayNi().get(2).toString());
	}

	/**
	 * Unmarshal Fixarcom.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarcom() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <------------><------------><------------><------------><------------><------------><------------>
		//		              1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7 
		//		              A B J A 0    0A B J A 1    7A B J A 0   14A B J A 3   21A B J A 4   28A B J A 5   35A B J A 6   42             
		String hexString   = "c1c2d1c1f00000c1c2d1c1f10007c1c2d1c1f2000ec1c2d1c1f30015c1c2d1c1f4001cc1c2d1c1f50023c1c2d1c1f6002a";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.fixarcom.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarcom.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.fixarcom.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.fixarcom.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		for(int i=0;i< 7;i++) {
			com.legstar.test.coxb.fixarcom.CArrayType item = dfhcommarea.getCArray().get(i);
			assertEquals("ABJA" + Integer.toString(i),item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}

	/**
	 * Unmarshal Vararcom with size 0.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize0() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <-->
		//		              1 2   
		//		              0000             
		String hexString   = "0000";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.vararcom.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.vararcom.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(0, dfhcommarea.getCItemsNumber());
	}

	/**
	 * Unmarshal Vararcom with size 1.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize1() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <--><------------>
		//		              1 2 1 2 3 4 5 6 7  
		//		              0001A B J A D    0             
		String hexString   = "0001c1c2d1c1c40000";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.vararcom.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.vararcom.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(1, dfhcommarea.getCItemsNumber());
		for(int i=0;i< 1;i++) {
			com.legstar.test.coxb.vararcom.CArrayType item = dfhcommarea.getCArray().get(i);
			assertEquals("ABJAD",item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}

	/**
	 * Unmarshal Vararcom with size 3.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize3() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		//		              <--><------------><------------><------------>
		//		              1 2 1 2 3 4 5 6 7 1 2 3 4 5 6 7 1 2 3 4 5 6 7  
		//		              0003A B J A D    0A B J A D    7A B J A D    e             
		String hexString   = "0003c1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000e";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.vararcom.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.vararcom.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(3, dfhcommarea.getCItemsNumber());
		for(int i=0;i< 3;i++) {
			com.legstar.test.coxb.vararcom.CArrayType item = dfhcommarea.getCArray().get(i);
			assertEquals("ABJAD",item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}

	/**
	 * Unmarshal Redsimpt.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedsimpt() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "f0f1f2f3f4f5f6f7f8f9f1f2f3f4f5f6f7f8";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redsimpt.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.redsimpt.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		/* The choice selector effect is to produce both alternatives */
		assertEquals(new Long(12345678912345678l), dfhcommarea.getCDefinition2());
		assertEquals(null, dfhcommarea.getCDefinition1());
	}

	/**
	 * Unmarshal Redsimpt second choice case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedsimptOtherChoice() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "c1c2c3c4c5d1d2d3d4d5c1c2c3c4c5d1d2d3";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redsimpt.DfhcommareaType dfhcommarea = new com.legstar.test.coxb.redsimpt.DfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		/* The choice selector effect is to produce both alternatives */
		assertEquals("ABCDEJKLMNABCDEJKL", dfhcommarea.getCDefinition1());
		assertEquals(null, dfhcommarea.getCDefinition2());
	}

	/**
	 * Unmarshal Redbotha both choices case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedbothaBothChoices() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "c1c2";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redbotha.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(49602, dfhcommarea.getCNumeric().intValue());
		assertEquals("A", dfhcommarea.getFiller22().getCLeftByte());
		assertEquals("B", dfhcommarea.getFiller22().getCRightByte());
	}

	/**
	 * In this situation, the backend is expected to send back the output layout.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedinout() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "0023f1f2f3f4f5f6f7f8c1c2c3c4c5c1c2c3c4c5c1c2c3c4c5d5c2";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redinout.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(35, dfhcommarea.getCNumeric());
		assertEquals(12345678, dfhcommarea.getCParaout().getCSomeOutput());
		assertEquals(null, dfhcommarea.getCParain());
	}

	/**
	 * Unmarshal Typesmix.
	 * @throws HostException if anything goes wrong
	 */
	public final void testTypesmix() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "c1c2c3c4c50041004200430044004500200020002000200e4040404040400f404040404040404040404040404040404040404040000000000000000000000000000000000000000000000000000000000cf0f0f0f0f0f0f0f0f0f0f0f0f0c0f040404040404040f0404040404040404040404040404040f0404040404040404040f04040404040404040404000000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0000000000000";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.typesmix.ObjectFactory objectFactory = new com.legstar.test.coxb.typesmix.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.typesmix.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals("ABCDE", dfhcommarea.getCAlphabetic());
		assertEquals("ABCDE    ", dfhcommarea.getCNational());
		assertEquals("0e4040404040400f",HostData.toHexString(dfhcommarea.getCDbcs()));
		assertEquals("              ", dfhcommarea.getCAlphanumericEdited());
		assertEquals("       ", dfhcommarea.getCAlphanumeric());
		byte[] cOctetString = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cOctetString),HostData.toHexString(dfhcommarea.getCOctetString()));
		assertEquals("0.00", dfhcommarea.getCPackedDecimal().toString());
		assertEquals("0       ", dfhcommarea.getCNumericEdited1());
		assertEquals("0               ", dfhcommarea.getCNumericEdited2());
		assertEquals("0         ", dfhcommarea.getCNumericEdited3());
		assertEquals("0          ", dfhcommarea.getCNumericEdited4());
		byte[] cIndex = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cIndex),HostData.toHexString(dfhcommarea.getCIndex()));
		byte[] cPointer = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cPointer),HostData.toHexString(dfhcommarea.getCPointer()));
		byte[] cProcPointer = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cProcPointer),HostData.toHexString(dfhcommarea.getCProcPointer()));
		byte[] cFuncPointer = {0x00,0x00,0x00,0x00};
		assertEquals(HostData.toHexString(cFuncPointer),HostData.toHexString(dfhcommarea.getCFuncPointer()));
	}

	/**
	 * Unmarshal Binpkdus.
	 * @throws HostException if anything goes wrong
	 */
	public final void testBinpkdus() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "0f3f012f0032769f0123456789012345678f1234567890123456789f1234567890123456789012345678901f";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.binpkdus.ObjectFactory objectFactory = new com.legstar.test.coxb.binpkdus.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.binpkdus.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(3, dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1());
		assertEquals(123456789012345678l, dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X18());
		assertEquals(0, dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1Null());
		assertEquals(12, dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X2());
		assertEquals(32769, dfhcommarea.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X7());
		assertEquals(new BigInteger("1234567890123456789"), dfhcommarea.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X19());
		assertEquals(new BigInteger("1234567890123456789012345678901"), dfhcommarea.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X31());
	}

	/**
	 * Unmarshal Dplarcht Files choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtFilesChoice() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "00005c4040404040404040404040000000000f000000000001c1c2c3c4c1c2c3c4c4404040404040404040c4404040404040404040c4404040404040404040c440404040404040404040404040c1c1c1c1c2c2c2c2c3c3c3c3";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		
		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("ABCDABCD", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData().getLsFileName());
		assertEquals("D         D         D         D             ", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData().getLsFileDsname());
		assertEquals("AAAABBBBCCCC", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData().getLsFileEnablestatus());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}

	/**
	 * Unmarshal Dplarcht Programs choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtProgramsChoice() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "00015c4040404040404040404040000000000f000100000001c1c1c1c1c1c1c1c1c2c2c2c2c2c2c2c2c2c2c2c2c3c3c3c3c3c3c3c3c3c3c3c30000000C00000000404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("CCCCCCCCCCCC", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(12, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("AAAAAAAA", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("BBBBBBBBBBBB", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals(0, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramUsecount());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}

	/**
	 * Unmarshal Dplarcht Transactions choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtTransactionsChoice() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString   = "00025c4040404040404040404040000000000f000100000001c1c1c1c1c1c1c1c1c2c2c2c2c2c2c2c2c3c3c3c3c3c3c3c3c3c3c3c3404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("AAAAAAAA", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData().getLsTransactionName());
		assertEquals("BBBBBBBB", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData().getLsTransactionProgram());
		assertEquals("CCCCCCCCCCCC", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData().getLsTransactionStatus());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData());
	}

	/**
	 * Unmarshal Dplarcht Program choice second case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtProgramsChoice2() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString = "00015C4040404040404040404040000000000F000000000001C2C9D5C1D9C3C8E3D7D9D6C7D9C1D44040404040D5D6E3C4C5C6C9D5C5C44040000016A000000002000000000000000000000000000000000000000000000000C2C9D5D5C1E3E2";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals(1, dfhcommarea.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("NOTDEFINED  ", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(5792, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("BINARCHT", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("PROGRAM     ", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals(2, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramUsecount());
		assertEquals("                        ", dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null, dfhcommarea.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}

	/**
	 * Unmarshal Redmulti Normal choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedmultiNormal() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString = "959699948193c1c2d1c1c4c8c1d6e4c1e9404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redmulti.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals("normal", dfhcommarea.getCOutputType());
		assertEquals("ABJADHAOUAZ                   ", dfhcommarea.getFiller35().getCString());
	}

	/**
	 * Unmarshal Redmulti Error choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedmultiError() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString = "859999969940f0f0f7f5c1c2d6d4c9d5c1c2d3c5404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040404040";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.redmulti.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals("error ", dfhcommarea.getCOutputType());
		assertEquals(75, dfhcommarea.getFiller38().getCErrorNum());
		assertEquals("ABOMINABLE                                                                                                                                                                                          ", dfhcommarea.getFiller38().getCErrorDescription());
	}

	/**
	 * Unmarshal with charset support.
	 * @throws HostException if anything goes wrong
	 */
	public final void testCharsets() throws HostException {
		// Create a cobol context 
		CobolContext cobolContext = new CobolContext();
		cobolContext.setHostCharsetName("IBM01147");
		// Select a conversion strategy 
		CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
		String hexString = "e08140837d85a2a340a495409799968293d094854040404040404040404040400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e9006c00e9006d0065006e00740061006900720065002000e00020007200e90073006f00750064007200650020002000200020002000200020002000200020";
		byte[] hostBytes = HostData.toByteArray(hexString);

		// Create a concrete visitor
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
		
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.charsets.ObjectFactory objectFactory = new com.legstar.test.coxb.charsets.ObjectFactory();
		// Create an initial empty instance of an object
		com.legstar.test.coxb.charsets.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();

		// Traverse the object structure, visiting each node with the visitor
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		
		assertEquals("ça c'est un problème            ", dfhcommarea.getComLocal());
		assertEquals("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",HostData.toHexString(dfhcommarea.getComDbcs()));
		assertEquals("élémentaire à résoudre          ", dfhcommarea.getComNational());
	}

}
