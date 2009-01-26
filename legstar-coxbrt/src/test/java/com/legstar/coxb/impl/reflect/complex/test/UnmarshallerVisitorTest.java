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
package com.legstar.coxb.impl.reflect.complex.test;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.BinpkdusCases;
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

/** Unmarshaller Visitor Test cases. */
public class UnmarshallerVisitorTest  extends TestCase {

    /** Cobol converters. */
    private CobolSimpleConverters mCobolConverters;
    
    public void setUp() {
        CobolContext cobolContext = new CobolContext();
        mCobolConverters = new CobolSimpleConverters(cobolContext);
    }
	/**
	 * Unmarshal Lsfileae.
	 * @throws HostException if anything goes wrong
	 */
	public final void testLsfileae() throws HostException {
		String hexString = LsfileaeCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileae.ObjectFactory();
		com.legstar.test.coxb.lsfileae.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.lsfileae.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		LsfileaeCases.checkJavaObject(dfhcommarea);
	}
	
	/**
	 * Unmarshal Fixarsim.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarsim() throws HostException {
		String hexString   = FixarsimCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarsim.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarsim.ObjectFactory();
		com.legstar.test.coxb.fixarsim.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.fixarsim.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		FixarsimCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Fixarnum.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarnum() throws HostException {
		String hexString   = FixarnumCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarnum.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarnum.ObjectFactory();
		com.legstar.test.coxb.fixarnum.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.fixarnum.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		FixarnumCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Fixarcom.
	 * @throws HostException if anything goes wrong
	 */
	public final void testFixarcom() throws HostException {
		String hexString   = FixarcomCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.fixarcom.ObjectFactory objectFactory = new com.legstar.test.coxb.fixarcom.ObjectFactory();
		com.legstar.test.coxb.fixarcom.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.fixarcom.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		FixarcomCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Vararcom with size 0.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize0() throws HostException {
		String hexString   = VararcomCases.getHostBytesHexEmpty();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.vararcom.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		VararcomCases.checkJavaObjectEmpty(dfhcommarea);
	}

	/**
	 * Unmarshal Vararcom with size 10.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize10() throws HostException {
		String hexString   = VararcomCases.getHostBytesHexSome();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.vararcom.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		VararcomCases.checkJavaObjectSome(dfhcommarea);
	}

	/**
	 * Unmarshal Vararcom with size 250.
	 * @throws HostException if anything goes wrong
	 */
	public final void testVararcomSize250() throws HostException {
		String hexString   = VararcomCases.getHostBytesHexFull();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.vararcom.ObjectFactory objectFactory = new com.legstar.test.coxb.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.vararcom.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		VararcomCases.checkJavaObjectFull(dfhcommarea);
	}

	/**
	 * Unmarshal Redsimpt.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedsimpt() throws HostException {
		String hexString   = RedsimptCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.redsimpt.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedsimptCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Redsimpt second choice case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedsimptOtherChoice() throws HostException {
		String hexString   = RedsimptCases.getHostBytesHexSecondChoice();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory = new com.legstar.test.coxb.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.redsimpt.Dfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedsimptCases.checkJavaObjectSecondChoice(dfhcommarea);
	}

	/**
	 * Unmarshal Redbotha both choices case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedbothaBothChoices() throws HostException {
		String hexString   = RedbothaCases.getHostBytesHexSecondChoice();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redbotha.ObjectFactory objectFactory = new com.legstar.test.coxb.redbotha.ObjectFactory();
		com.legstar.test.coxb.redbotha.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedbothaCases.checkJavaObjectSecondChoice(dfhcommarea);
	}

	/**
	 * In this situation, the backend is expected to send back the output layout.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedinout() throws HostException {
		String hexString   = RedinoutCases.getHostBytesHexSecondChoice();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redinout.ObjectFactory objectFactory = new com.legstar.test.coxb.redinout.ObjectFactory();
		com.legstar.test.coxb.redinout.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedinoutCases.checkJavaObjectSecondChoice(dfhcommarea);
	}

	/**
	 * Unmarshal smix.
	 * @throws HostException if anything goes wrong
	 */
	public final void testTypesmixCases() throws HostException {
		String hexString   = TypesmixCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.typesmix.ObjectFactory objectFactory = new com.legstar.test.coxb.typesmix.ObjectFactory();
		com.legstar.test.coxb.typesmix.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		TypesmixCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Binpkdus.
	 * @throws HostException if anything goes wrong
	 */
	public final void testBinpkdus() throws HostException {
		String hexString   = BinpkdusCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.binpkdus.ObjectFactory objectFactory = new com.legstar.test.coxb.binpkdus.ObjectFactory();
		com.legstar.test.coxb.binpkdus.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		BinpkdusCases.checkJavaObject(dfhcommarea);
	}

	/**
	 * Unmarshal Dplarcht Files choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtFilesChoice() throws HostException {
		String hexString   = DplarchtCases.getHostBytesHex1File();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		DplarchtCases.checkJavaObject1File(dfhcommarea);
	}

	/**
	 * Unmarshal Dplarcht Transactions choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtTransactionsChoice() throws HostException {
		String hexString   = DplarchtCases.getHostBytesHex1Transaction();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		DplarchtCases.checkJavaObject1Transaction(dfhcommarea);
	}

	/**
	 * Unmarshal Dplarcht Program choice second case.
	 * @throws HostException if anything goes wrong
	 */
	public final void testDplarchtProgramsChoice() throws HostException {
		String hexString = DplarchtCases.getHostBytesHex1Program();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		com.legstar.test.coxb.dplarcht.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		DplarchtCases.checkJavaObject1Program(dfhcommarea);
	}

	/**
	 * Unmarshal Redmulti Normal choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedmultiNormal() throws HostException {
		String hexString = RedmultiCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		com.legstar.test.coxb.redmulti.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedmultiCases.getHostBytesHex();
	}

	/**
	 * Unmarshal Redmulti Error choice.
	 * @throws HostException if anything goes wrong
	 */
	public final void testRedmultiError() throws HostException {
		String hexString = RedmultiCases.getHostBytesHexError();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.redmulti.ObjectFactory objectFactory = new com.legstar.test.coxb.redmulti.ObjectFactory();
		com.legstar.test.coxb.redmulti.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		RedmultiCases.getHostBytesHexError();
	}

	/**
	 * Unmarshal with charset support.
	 * @throws HostException if anything goes wrong
	 */
	public final void testCharsets() throws HostException {
	    mCobolConverters.getCobolContext().setHostCharsetName("IBM01147");
		String hexString = CharsetsCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.charsets.ObjectFactory objectFactory = new com.legstar.test.coxb.charsets.ObjectFactory();
		com.legstar.test.coxb.charsets.Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, dfhcommarea);
		ccem.accept(uv);
		CharsetsCases.checkJavaObject(dfhcommarea);
	}
	
	/**
     * Unmarshal jvmquery.
	 * @throws HostException if anything goes wrong
	 */
	public final void testJvmqueryWs() throws HostException {
        //mCobolConverters.getCobolContext().setHostCharsetName("IBM01147");
		String hexString = JvmqueryWsCases.getHostBytesHex();
		byte[] hostBytes = HostData.toByteArray(hexString);
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0, mCobolConverters);
		com.legstar.test.coxb.ws.jvmquery.ObjectFactory objectFactory = new com.legstar.test.coxb.ws.jvmquery.ObjectFactory();
		com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse queryJvmResponse = objectFactory.createQueryJvmResponse();
		CComplexReflectBinding ccem = new CComplexReflectBinding(objectFactory, queryJvmResponse);
		ccem.accept(uv);
		JvmqueryWsCases.checkJavaObject(queryJvmResponse);
	}
	

}
