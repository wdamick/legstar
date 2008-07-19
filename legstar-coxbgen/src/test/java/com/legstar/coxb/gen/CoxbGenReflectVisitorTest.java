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
package com.legstar.coxb.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JaxbUtil;

import junit.framework.TestCase;

public class CoxbGenReflectVisitorTest extends TestCase {
	
	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CoxbGenReflectVisitorTest.class);

	public void testDplarcht() throws Exception {
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory
			= new com.legstar.test.coxb.dplarcht.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.dplarcht.DfhcommareaType"));

		CoxbGenModel coxbGenContext = new CoxbGenModel();
		coxbGenContext.setCoxbSrcDir(new File(GEN_SRC_DIR));
		coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.dplarcht");
		coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.dplarcht.bind");
		
		CoxbGenReflectVisitor visitor = new CoxbGenReflectVisitor(coxbGenContext);
		
		visitor.visit(ce);
		
		/* Check DfhcommareaTypeBinding */
		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/DfhcommareaTypeBinding.java"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsRequest = new LsRequestTypeBinding(\"LsRequest\","));
		assertTrue(resStr.contains("\"LsRequest\", this, null);"));
		assertTrue(resStr.contains("lsRequest.setCobolName(\"LS-REQUEST\");"));
		assertTrue(resStr.contains("lsReply = new LsReplyTypeBinding(\"LsReply\","));
		assertTrue(resStr.contains("\"LsReply\", this, null);"));
		assertTrue(resStr.contains("lsReply.setCobolName(\"LS-REPLY\");"));
		
		/* Check LsAllItemsChoiceBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsAllItemsChoiceBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsAllItems = BF.createStringBinding(\"LsAllItems\","));
		assertTrue(resStr.contains("\"LsAllItems\", String.class, getParentBinding());"));
		assertTrue(resStr.contains("lsAllItems.setCobolName(\"LS-ALL-ITEMS\");"));
		assertTrue(resStr.contains("lsAllItems.setByteLength(4);"));
		assertTrue(resStr.contains("lsMaxItems = BF.createZonedDecimalBinding(\"LsMaxItems\","));
		assertTrue(resStr.contains("\"LsMaxItems\", Integer.class, getParentBinding());"));
		assertTrue(resStr.contains("lsMaxItems.setCobolName(\"LS-MAX-ITEMS\");"));
		assertTrue(resStr.contains("lsMaxItems.setByteLength(4);"));
		assertTrue(resStr.contains("lsMaxItems.setTotalDigits(4);"));
		assertTrue(resStr.contains("lsMaxItems.setRedefines(\"LS-ALL-ITEMS\");"));
		
		/* Check LsFilesDataChoiceBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsFilesDataChoiceBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));
		assertTrue(resStr.contains("lsFilesData = new LsFilesDataTypeBinding(\"LsFilesData\","));
		assertTrue(resStr.contains("\"LsFilesData\", getParentBinding(), null);"));
		assertTrue(resStr.contains("lsFilesData.setCobolName(\"LS-FILES-DATA\");"));
		assertTrue(resStr.contains("lsFilesData.setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));
		assertTrue(resStr.contains("lsProgramsData = new LsProgramsDataTypeBinding(\"LsProgramsData\","));
		assertTrue(resStr.contains("\"LsProgramsData\", getParentBinding(), null);"));
		assertTrue(resStr.contains("lsProgramsData.setCobolName(\"LS-PROGRAMS-DATA\");"));
		assertTrue(resStr.contains("lsProgramsData.setRedefines(\"LS-FILES-DATA\");"));
		assertTrue(resStr.contains("lsTransactionsData = new LsTransactionsDataTypeBinding(\"LsTransactionsData\","));
		assertTrue(resStr.contains("\"LsTransactionsData\", getParentBinding(), null);"));
		assertTrue(resStr.contains("lsTransactionsData.setCobolName(\"LS-TRANSACTIONS-DATA\");"));
		assertTrue(resStr.contains("lsTransactionsData.setRedefines(\"LS-FILES-DATA\");"));
		
		/* Check LsFilesDataTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsFilesDataTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsFileName = BF.createStringBinding(\"LsFileName\","));
		assertTrue(resStr.contains("\"LsFileName\", String.class, this);"));
		assertTrue(resStr.contains("lsFileName.setCobolName(\"LS-FILE-NAME\");"));
		assertTrue(resStr.contains("lsFileName.setByteLength(8);"));
		assertTrue(resStr.contains("lsFileDsname = BF.createStringBinding(\"LsFileDsname\","));
		assertTrue(resStr.contains("\"LsFileDsname\", String.class, this);"));
		assertTrue(resStr.contains("lsFileDsname.setCobolName(\"LS-FILE-DSNAME\");"));
		assertTrue(resStr.contains("lsFileDsname.setByteLength(44);"));
		assertTrue(resStr.contains("lsFileEnablestatus = BF.createStringBinding(\"LsFileEnablestatus\","));
		assertTrue(resStr.contains("\"LsFileEnablestatus\", String.class, this);"));
		assertTrue(resStr.contains("lsFileEnablestatus.setCobolName(\"LS-FILE-ENABLESTATUS\");"));
		assertTrue(resStr.contains("lsFileEnablestatus.setByteLength(12);"));

		/* Check LsItemsArrayTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsItemsArrayTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsFilesDataChoice = new LsFilesDataChoiceBinding(\"LsFilesDataChoice\", this);"));
		assertTrue(resStr.contains("lsFilesDataChoice.setCobolName(\"LS-FILES-DATA\");"));
		assertTrue(resStr.contains("lsFilesDataChoice.setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));

		/* Check LsItemsArrayTypeWrapperBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsItemsArrayTypeWrapperBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("setMinOccurs(1);"));
		assertTrue(resStr.contains("setMaxOccurs(500);"));
		assertTrue(resStr.contains("setDependingOn(\"LS-ITEMS-COUNT\");"));
		assertTrue(resStr.contains("mValueObject.add((LsItemsArrayType) getComplexItemBinding()."));
		assertTrue(resStr.contains("getObjectValue(LsItemsArrayType.class));"));
		/* Check LsItemsArrayTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsItemsArrayTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsFilesDataChoice = new LsFilesDataChoiceBinding(\"LsFilesDataChoice\", this);"));
		assertTrue(resStr.contains("lsFilesDataChoice.setCobolName(\"LS-FILES-DATA\");"));
		assertTrue(resStr.contains("lsFilesDataChoice.setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));

		/* Check LsProgramsDataTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsProgramsDataTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsProgramLength = BF.createBinaryBinding(\"LsProgramLength\","));
		assertTrue(resStr.contains("\"LsProgramLength\", Integer.class, this);"));
		assertTrue(resStr.contains("lsProgramLength.setCobolName(\"LS-PROGRAM-LENGTH\");"));
		assertTrue(resStr.contains("lsProgramLength.setByteLength(4);"));
		assertTrue(resStr.contains("lsProgramLength.setTotalDigits(9);"));
		assertTrue(resStr.contains("lsProgramLength.setIsSigned(true);"));

		/* Check LsReplyDataTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsReplyDataTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsItemsCount = BF.createBinaryBinding(\"LsItemsCount\","));
		assertTrue(resStr.contains("\"LsItemsCount\", Long.class, this);"));
		assertTrue(resStr.contains("lsItemsCount.setCobolName(\"LS-ITEMS-COUNT\");"));
		assertTrue(resStr.contains("lsItemsCount.setByteLength(4);"));
		assertTrue(resStr.contains("lsItemsCount.setTotalDigits(9);"));
		assertTrue(resStr.contains("lsItemsCount.setIsODOObject(true);"));
		assertTrue(resStr.contains("lsItemsArrayWrapperItem = new LsItemsArrayTypeBinding(\"LsItemsArrayWrapperItem\","));
		assertTrue(resStr.contains("\"LsItemsArray\", this, null);"));
		assertTrue(resStr.contains("lsItemsArrayWrapper = new LsItemsArrayTypeWrapperBinding(\"LsItemsArrayWrapper\","));
		assertTrue(resStr.contains("\"LsItemsArray\", this, lsItemsArrayWrapperItem);"));
		assertTrue(resStr.contains("lsItemsArrayWrapper.setCobolName(\"LS-ITEMS-ARRAY\");"));
		assertTrue(resStr.contains("lsItemsArrayWrapper.setMinOccurs(1);"));
		assertTrue(resStr.contains("lsItemsArrayWrapper.setMaxOccurs(500);"));
		assertTrue(resStr.contains("lsItemsArrayWrapper.setDependingOn(\"LS-ITEMS-COUNT\");"));

		/* Check LsReplyTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsReplyTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsReplyType = BF.createBinaryBinding(\"LsReplyType\","));
		assertTrue(resStr.contains("\"LsReplyType\", Integer.class, this);"));
		assertTrue(resStr.contains("lsReplyType.setCobolName(\"LS-REPLY-TYPE\");"));
		assertTrue(resStr.contains("lsReplyType.setByteLength(2);"));
		assertTrue(resStr.contains("lsReplyType.setTotalDigits(4);"));
		assertTrue(resStr.contains("lsReplyData = new LsReplyDataTypeBinding(\"LsReplyData\","));
		assertTrue(resStr.contains("\"LsReplyData\", this, null);"));
		assertTrue(resStr.contains("lsReplyData.setCobolName(\"LS-REPLY-DATA\");"));

		/* Check LsRequestTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsRequestTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsRequestType = BF.createBinaryBinding(\"LsRequestType\","));
		assertTrue(resStr.contains("\"LsRequestType\", Integer.class, this);"));
		assertTrue(resStr.contains("lsRequestType.setCobolName(\"LS-REQUEST-TYPE\");"));
		assertTrue(resStr.contains("lsRequestType.setByteLength(2);"));
		assertTrue(resStr.contains("lsRequestType.setTotalDigits(4);"));
		assertTrue(resStr.contains("lsRequestType.setIsCustomVariable(true);"));
		assertTrue(resStr.contains("lsAllItemsChoice = new LsAllItemsChoiceBinding(\"LsAllItemsChoice\", this);"));
		assertTrue(resStr.contains("lsAllItemsChoice.setCobolName(\"LS-ALL-ITEMS\");"));
		assertTrue(resStr.contains("lsAllItemsChoice.setByteLength(4);"));
		assertTrue(resStr.contains("lsSearchCriteria = new LsSearchCriteriaTypeBinding(\"LsSearchCriteria\","));
		assertTrue(resStr.contains("\"LsSearchCriteria\", this, null);"));
		assertTrue(resStr.contains("lsSearchCriteria.setCobolName(\"LS-SEARCH-CRITERIA\");"));

		/* Check LsSearchCriteriaTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsSearchCriteriaTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsStartwith = BF.createStringBinding(\"LsStartwith\","));
		assertTrue(resStr.contains("\"LsStartwith\", String.class, this);"));
		assertTrue(resStr.contains("lsStartwith.setCobolName(\"LS-STARTWITH\");"));
		assertTrue(resStr.contains("lsStartwith.setByteLength(8);"));
		assertTrue(resStr.contains("lsStartwithLen = BF.createPackedDecimalBinding(\"LsStartwithLen\","));
		assertTrue(resStr.contains("\"LsStartwithLen\", Long.class, this);"));
		assertTrue(resStr.contains("lsStartwithLen.setCobolName(\"LS-STARTWITH-LEN\");"));
		assertTrue(resStr.contains("lsStartwithLen.setByteLength(5);"));
		assertTrue(resStr.contains("lsStartwithLen.setTotalDigits(9);"));

		/* Check LsTransactionsDataTypeBinding */
		in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/dplarcht/bind/LsTransactionsDataTypeBinding.java"));
		resStr = "";
		str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("lsTransactionName = BF.createStringBinding(\"LsTransactionName\","));
		assertTrue(resStr.contains("\"LsTransactionName\", String.class, this);"));
		assertTrue(resStr.contains("lsTransactionName.setCobolName(\"LS-TRANSACTION-NAME\");"));
		assertTrue(resStr.contains("lsTransactionName.setByteLength(8);"));
		assertTrue(resStr.contains("lsTransactionProgram = BF.createStringBinding(\"LsTransactionProgram\","));
		assertTrue(resStr.contains("\"LsTransactionProgram\", String.class, this);"));
		assertTrue(resStr.contains("lsTransactionProgram.setCobolName(\"LS-TRANSACTION-PROGRAM\");"));
		assertTrue(resStr.contains("lsTransactionProgram.setByteLength(8);"));
	}

}
