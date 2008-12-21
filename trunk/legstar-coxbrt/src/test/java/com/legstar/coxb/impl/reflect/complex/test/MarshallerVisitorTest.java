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
package com.legstar.coxb.impl.reflect.complex.test;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.host.*;
import com.legstar.test.coxb.BinnatusCases;
import com.legstar.test.coxb.CharsetsCases;
import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.FixarcomCases;
import com.legstar.test.coxb.FixarnumCases;
import com.legstar.test.coxb.FixarsimCases;
import com.legstar.test.coxb.JvmqueryWsCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.RedbothaCases;
import com.legstar.test.coxb.RedinoutCases;
import com.legstar.test.coxb.RedmultiCases;
import com.legstar.test.coxb.RedsimptCases;
import com.legstar.test.coxb.TypesmixCases;
import com.legstar.test.coxb.VararcomCases;

import junit.framework.TestCase;

public class MarshallerVisitorTest  extends TestCase {

	/** Cobol converters. */
    private CobolSimpleConverters mCobolConverters;
    
    public void setUp() {
        CobolContext cobolContext = new CobolContext();
        mCobolConverters = new CobolSimpleConverters(cobolContext);
	}
    
    /**
     * Marshal Lsfileae (Reflection)
     * @throws HostException if marshaling fails
     */
    public void testLsfileae() throws HostException{
		byte[] hostBytes = new byte[79];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.Dfhcommarea dfhcommarea = LsfileaeCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(LsfileaeCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Fixarsim (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testFixarsim() throws HostException{
		byte[] hostBytes = new byte[15];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarsim.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarsim.ObjectFactory();
		com.legstar.test.coxb.fixarsim.Dfhcommarea dfhcommarea = FixarsimCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(FixarsimCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Fixarnum (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testFixarnum() throws HostException{
		byte[] hostBytes = new byte[78];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();
		com.legstar.test.coxb.fixarnum.Dfhcommarea dfhcommarea = FixarnumCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(FixarnumCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Fixarnum (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testFixarnumEmpty() throws HostException{
		byte[] hostBytes = new byte[78];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();
		com.legstar.test.coxb.fixarnum.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(FixarnumCases.getHostBytesHexEmpty(), HostData.toHexString(hostBytes));
	}
	
    /**
     * Marshal Fixarcom (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testFixarcom() throws HostException{
		byte[] hostBytes = new byte[49];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarcom.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarcom.ObjectFactory();
		com.legstar.test.coxb.fixarcom.Dfhcommarea dfhcommarea = FixarcomCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(FixarcomCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Vararcom (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testVararcomSize0() throws HostException{
		byte[] hostBytes = new byte[2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectEmpty();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(VararcomCases.getHostBytesHexEmpty(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Vararcom (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testVararcomSize10() throws HostException{
		byte[] hostBytes = new byte[VararcomCases.getHostBytesHexSome().length() / 2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectSome();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(VararcomCases.getHostBytesHexSome(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Vararcom (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testVararcomSize250() throws HostException{
		byte[] hostBytes = new byte[VararcomCases.getHostBytesHexFull().length() / 2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectFull();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(VararcomCases.getHostBytesHexFull(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redsimpt (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedsimpt() throws HostException{
		byte[] hostBytes = new byte[18];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.Dfhcommarea dfhcommarea = RedsimptCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedsimptCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redsimpt (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedsimptOtherChoice() throws HostException{
		byte[] hostBytes = new byte[18];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.Dfhcommarea dfhcommarea = RedsimptCases.getJavaObjectSecondChoice();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedsimptCases.getHostBytesHexSecondChoice(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redbotha (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedbotha() throws HostException{
		byte[] hostBytes = new byte[2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		com.legstar.test.coxb.redbotha.Dfhcommarea dfhcommarea = RedbothaCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedbothaCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redbotha (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedbothaOtherChoice() throws HostException{
		byte[] hostBytes = new byte[2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		com.legstar.test.coxb.redbotha.Dfhcommarea dfhcommarea = RedbothaCases.getJavaObjectSecondChoice();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedbothaCases.getHostBytesHexSecondChoice(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redinout (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedinout() throws HostException{
		byte[] hostBytes = new byte[17];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		com.legstar.test.coxb.redinout.Dfhcommarea dfhcommarea = RedinoutCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedinoutCases.getHostBytesHex(), HostData.toHexString(hostBytes));
        assertEquals(502,ccem.calcByteLength());
	}

    /**
     * Marshal Redinout (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedinoutSecondChoice() throws HostException{
		byte[] hostBytes = new byte[RedinoutCases.getHostBytesHexSecondChoice().length() / 2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		com.legstar.test.coxb.redinout.Dfhcommarea dfhcommarea = RedinoutCases.getJavaObjectSecondChoice();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
        assertEquals(RedinoutCases.getHostBytesHexSecondChoice(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Typesmix (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testTypesmix() throws HostException{
		byte[] hostBytes = new byte[176];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.typesmix.ObjectFactory objectFactory = new com.legstar.test.coxb.typesmix.ObjectFactory();
		com.legstar.test.coxb.typesmix.Dfhcommarea dfhcommarea = TypesmixCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(TypesmixCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Dplarcht (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testDplarchtNoAlternatives() throws HostException{
		byte[] hostBytes = new byte[32025];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		com.legstar.test.coxb.dplarcht.LsRequest request = objectFactory.createLsRequest();
		dfhcommarea.setLsRequest(request);
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		try {
			ccem.accept(mv);
			fail("Control on no alternative provided failed");
		} catch (HostException he) {
			assertEquals("No alternative found for choice element LsAllItemsChoice", he.getMessage());
		}
	}

    /**
     * Marshal Dplarcht (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testDplarcht() throws HostException{
		byte[] hostBytes = new byte[DplarchtCases.getHostBytesHex().length() / 2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = DplarchtCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(DplarchtCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Binatus (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testBinatus() throws HostException{
		byte[] hostBytes = new byte[56];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.binnatus.ObjectFactory objectFactory = new com.legstar.test.coxb.binnatus.ObjectFactory();
		com.legstar.test.coxb.binnatus.Dfhcommarea dfhcommarea = BinnatusCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(BinnatusCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redmulti (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedmultiNormal() throws HostException{
		byte[] hostBytes = new byte[206];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		com.legstar.test.coxb.redmulti.Dfhcommarea dfhcommarea = RedmultiCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedmultiCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Redmulti (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testRedmultiError() throws HostException{
		byte[] hostBytes = new byte[206];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		com.legstar.test.coxb.redmulti.Dfhcommarea dfhcommarea = RedmultiCases.getJavaObjectError();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(RedmultiCases.getHostBytesHexError(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Charsets (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testCharsets() throws HostException{
	    mCobolConverters.getCobolContext().setHostCharsetName("IBM01147");
		byte[] hostBytes = new byte[160];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.charsets.ObjectFactory objectFactory = new com.legstar.test.coxb.charsets.ObjectFactory();
		com.legstar.test.coxb.charsets.Dfhcommarea dfhcommarea = CharsetsCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(mv);
		assertEquals(CharsetsCases.getHostBytesHex(), HostData.toHexString(hostBytes));
	}

    /**
     * Marshal Jvmquery (Reflection)
     * @throws HostException if marshaling fails
     */
	public void testJvmqueryWs() throws HostException{
        mCobolConverters.getCobolContext().setHostCharsetName("IBM01147");
		byte[] hostBytes = new byte[JvmqueryWsCases.getHostBytesHex().length() / 2];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.ws.jvmquery.ObjectFactory objectFactory = new com.legstar.test.coxb.ws.jvmquery.ObjectFactory();
		com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse jvmResponse = JvmqueryWsCases.getJavaObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, jvmResponse);
		ccem.accept(mv);
		assertEquals(JvmqueryWsCases.getHostBytesHex().substring(0, 131),
		        HostData.toHexString(hostBytes).substring(0, 131));
	}
}
