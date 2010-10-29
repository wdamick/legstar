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
package com.legstar.coxb.gen;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.util.ClassUtil;

/**
 * Test CoxbGenReflectVisitor.
 * 
 */
public class CoxbGenReflectVisitorTest extends AbstractTestTemplate {

    /**
     * Dplarcht is one of the most complex samples that we have. Good way to
     * check that all artifacts are
     * produced by the visitor.
     * 
     * @throws Exception if generation fails
     */
    public void testDplarcht() throws Exception {
        com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                ClassUtil
                        .loadClass("com.legstar.test.coxb.dplarcht.Dfhcommarea"));

        getCoxbGenModel().setJaxbPackageName("com.legstar.test.coxb.dplarcht");
        getCoxbGenModel().setCoxbPackageName(
                "com.legstar.test.coxb.dplarcht.bind");

        CoxbGenReflectVisitor visitor = new CoxbGenReflectVisitor(
                getCoxbGenModel(), getOutputFolder());

        visitor.visit(ce);

        checkDfhcommareaBinding();
        checkLsAllItemsChoiceBinding();
        checkLsFilesDataChoiceBinding();
        checkLsFilesDataTypeBinding();
        checkLsItemsArrayTypeBinding();
        checkLsItemsArrayTypeWrapperBinding();
        checkLsProgramsDataTypeBinding();
        checkLsReplyDataTypeBinding();
        checkLsReplyBinding();
        checkLsRequestBinding();
        checkLsSearchCriteriaTypeBinding();
        checkLsTransactionsDataTypeBinding();
    }

    /**
     * check binding.
     */
    public void checkDfhcommareaBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/DfhcommareaBinding.java");
        assertTrue(resStr
                .contains("lsRequest = new LsRequestBinding(\"LsRequest\","));
        assertTrue(resStr.contains("\"LsRequest\", this, null);"));
        assertTrue(resStr.contains("lsRequest.setCobolName(\"LS-REQUEST\");"));
        assertTrue(resStr.contains("lsReply = new LsReplyBinding(\"LsReply\","));
        assertTrue(resStr.contains("\"LsReply\", this, null);"));
        assertTrue(resStr.contains("lsReply.setCobolName(\"LS-REPLY\");"));

    }

    /**
     * check binding.
     */
    public void checkLsAllItemsChoiceBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsAllItemsChoiceBinding.java");
        assertTrue(resStr
                .contains("lsAllItems = BF.createStringBinding(\"LsAllItems\","));
        assertTrue(resStr
                .contains("\"LsAllItems\", String.class, getParentBinding());"));
        assertTrue(resStr
                .contains("lsAllItems.setCobolName(\"LS-ALL-ITEMS\");"));
        assertTrue(resStr.contains("lsAllItems.setByteLength(4);"));
        assertTrue(resStr
                .contains("lsMaxItems = BF.createZonedDecimalBinding(\"LsMaxItems\","));
        assertTrue(resStr
                .contains("\"LsMaxItems\", Integer.class, getParentBinding());"));
        assertTrue(resStr
                .contains("lsMaxItems.setCobolName(\"LS-MAX-ITEMS\");"));
        assertTrue(resStr.contains("lsMaxItems.setByteLength(4);"));
        assertTrue(resStr.contains("lsMaxItems.setTotalDigits(4);"));
        assertTrue(resStr
                .contains("lsMaxItems.setRedefines(\"LS-ALL-ITEMS\");"));

    }

    /**
     * check binding.
     */
    public void checkLsFilesDataChoiceBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsFilesDataChoiceBinding.java");
        assertTrue(resStr.contains("setUnmarshalChoiceStrategyClassName("));
        assertTrue(resStr
                .contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));
        assertTrue(resStr
                .contains("lsFilesData = new LsFilesDataBinding(\"LsFilesData\","));
        assertTrue(resStr
                .contains("\"LsFilesData\", getParentBinding(), null);"));
        assertTrue(resStr
                .contains("lsFilesData.setCobolName(\"LS-FILES-DATA\");"));
        assertTrue(resStr
                .contains("lsFilesData.setUnmarshalChoiceStrategyClassName("));
        assertTrue(resStr
                .contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));
        assertTrue(resStr
                .contains("lsProgramsData = new LsProgramsDataBinding(\"LsProgramsData\","));
        assertTrue(resStr
                .contains("\"LsProgramsData\", getParentBinding(), null);"));
        assertTrue(resStr
                .contains("lsProgramsData.setCobolName(\"LS-PROGRAMS-DATA\");"));
        assertTrue(resStr
                .contains("lsProgramsData.setRedefines(\"LS-FILES-DATA\");"));
        assertTrue(resStr
                .contains("lsTransactionsData = new LsTransactionsDataBinding(\"LsTransactionsData\","));
        assertTrue(resStr
                .contains("\"LsTransactionsData\", getParentBinding(), null);"));
        assertTrue(resStr
                .contains("lsTransactionsData.setCobolName(\"LS-TRANSACTIONS-DATA\");"));
        assertTrue(resStr
                .contains("lsTransactionsData.setRedefines(\"LS-FILES-DATA\");"));

    }

    /**
     * check binding.
     */
    public void checkLsFilesDataTypeBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsFilesDataBinding.java");
        assertTrue(resStr
                .contains("lsFileName = BF.createStringBinding(\"LsFileName\","));
        assertTrue(resStr.contains("\"LsFileName\", String.class, this);"));
        assertTrue(resStr
                .contains("lsFileName.setCobolName(\"LS-FILE-NAME\");"));
        assertTrue(resStr.contains("lsFileName.setByteLength(8);"));
        assertTrue(resStr
                .contains("lsFileDsname = BF.createStringBinding(\"LsFileDsname\","));
        assertTrue(resStr.contains("\"LsFileDsname\", String.class, this);"));
        assertTrue(resStr
                .contains("lsFileDsname.setCobolName(\"LS-FILE-DSNAME\");"));
        assertTrue(resStr.contains("lsFileDsname.setByteLength(44);"));
        assertTrue(resStr
                .contains("lsFileEnablestatus = BF.createStringBinding(\"LsFileEnablestatus\","));
        assertTrue(resStr
                .contains("\"LsFileEnablestatus\", String.class, this);"));
        assertTrue(resStr
                .contains("lsFileEnablestatus.setCobolName(\"LS-FILE-ENABLESTATUS\");"));
        assertTrue(resStr.contains("lsFileEnablestatus.setByteLength(12);"));

    }

    /**
     * check binding.
     */
    public void checkLsItemsArrayTypeBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsItemsArrayBinding.java");
        assertTrue(resStr
                .contains("lsFilesDataChoice = new LsFilesDataChoiceBinding(\"LsFilesDataChoice\", this);"));
        assertTrue(resStr
                .contains("lsFilesDataChoice.setCobolName(\"LS-FILES-DATA\");"));
        assertTrue(resStr
                .contains("lsFilesDataChoice.setUnmarshalChoiceStrategyClassName("));
        assertTrue(resStr
                .contains("\"com.legstar.coxb.cust.dplarcht.ChoiceSelector\");"));

    }

    /**
     * check binding.
     */
    public void checkLsItemsArrayTypeWrapperBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsItemsArrayWrapperBinding.java");
        assertTrue(resStr.contains("setMinOccurs(1);"));
        assertTrue(resStr.contains("setMaxOccurs(500);"));
        assertTrue(resStr.contains("setDependingOn(\"LS-ITEMS-COUNT\");"));
        assertTrue(resStr
                .contains("mValueObject.add((LsItemsArray) getComplexItemBinding()."));
        assertTrue(resStr.contains("getObjectValue(LsItemsArray.class));"));

    }

    /**
     * check binding.
     */
    public void checkLsProgramsDataTypeBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsProgramsDataBinding.java");
        assertTrue(resStr
                .contains("lsProgramLength = BF.createBinaryBinding(\"LsProgramLength\","));
        assertTrue(resStr
                .contains("\"LsProgramLength\", Integer.class, this);"));
        assertTrue(resStr
                .contains("lsProgramLength.setCobolName(\"LS-PROGRAM-LENGTH\");"));
        assertTrue(resStr.contains("lsProgramLength.setByteLength(4);"));
        assertTrue(resStr.contains("lsProgramLength.setTotalDigits(9);"));
        assertTrue(resStr.contains("lsProgramLength.setIsSigned(true);"));

    }

    /**
     * check binding.
     */
    public void checkLsReplyDataTypeBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsReplyDataBinding.java");
        assertTrue(resStr
                .contains("lsItemsCount = BF.createBinaryBinding(\"LsItemsCount\","));
        assertTrue(resStr.contains("\"LsItemsCount\", Long.class, this);"));
        assertTrue(resStr
                .contains("lsItemsCount.setCobolName(\"LS-ITEMS-COUNT\");"));
        assertTrue(resStr.contains("lsItemsCount.setByteLength(4);"));
        assertTrue(resStr.contains("lsItemsCount.setTotalDigits(9);"));
        assertTrue(resStr.contains("lsItemsCount.setIsODOObject(true);"));
        assertTrue(resStr
                .contains("lsItemsArrayWrapperItem = new LsItemsArrayBinding(\"LsItemsArrayWrapperItem\","));
        assertTrue(resStr.contains("\"LsItemsArray\", this, null);"));
        assertTrue(resStr
                .contains("lsItemsArrayWrapper = new LsItemsArrayWrapperBinding(\"LsItemsArrayWrapper\","));
        assertTrue(resStr
                .contains("\"LsItemsArray\", this, _lsItemsArrayWrapperItem);"));
        assertTrue(resStr
                .contains("lsItemsArrayWrapper.setCobolName(\"LS-ITEMS-ARRAY\");"));
        assertTrue(resStr.contains("lsItemsArrayWrapper.setMinOccurs(1);"));
        assertTrue(resStr.contains("lsItemsArrayWrapper.setMaxOccurs(500);"));
        assertTrue(resStr
                .contains("lsItemsArrayWrapper.setDependingOn(\"LS-ITEMS-COUNT\");"));

    }

    /**
     * check binding.
     */
    public void checkLsReplyBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsReplyBinding.java");
        assertTrue(resStr
                .contains("_lsReplyType = BF.createBinaryBinding(\"LsReplyType\","));
        assertTrue(resStr.contains("\"LsReplyType\", Integer.class, this);"));
        assertTrue(resStr
                .contains("_lsReplyType.setCobolName(\"LS-REPLY-TYPE\");"));
        assertTrue(resStr.contains("_lsReplyType.setByteLength(2);"));
        assertTrue(resStr.contains("_lsReplyType.setTotalDigits(4);"));
        assertTrue(resStr
                .contains("_lsReplyData = new LsReplyDataBinding(\"LsReplyData\","));
        assertTrue(resStr.contains("\"LsReplyData\", this, null);"));
        assertTrue(resStr
                .contains("lsReplyData.setCobolName(\"LS-REPLY-DATA\");"));

    }

    /**
     * check binding.
     */
    public void checkLsRequestBinding() {
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsRequestBinding.java");
        assertTrue(resStr
                .contains("_lsRequestType = BF.createBinaryBinding(\"LsRequestType\","));
        assertTrue(resStr.contains("\"LsRequestType\", Integer.class, this);"));
        assertTrue(resStr
                .contains("_lsRequestType.setCobolName(\"LS-REQUEST-TYPE\");"));
        assertTrue(resStr.contains("_lsRequestType.setByteLength(2);"));
        assertTrue(resStr.contains("_lsRequestType.setTotalDigits(4);"));
        assertTrue(resStr.contains("_lsRequestType.setIsCustomVariable(true);"));
        assertTrue(resStr
                .contains("lsAllItemsChoice = new LsAllItemsChoiceBinding(\"LsAllItemsChoice\", this);"));
        assertTrue(resStr
                .contains("lsAllItemsChoice.setCobolName(\"LS-ALL-ITEMS\");"));
        assertTrue(resStr.contains("lsAllItemsChoice.setByteLength(4);"));
        assertTrue(resStr
                .contains("lsSearchCriteria = new LsSearchCriteriaBinding(\"LsSearchCriteria\","));
        assertTrue(resStr.contains("\"LsSearchCriteria\", this, null);"));
        assertTrue(resStr
                .contains("lsSearchCriteria.setCobolName(\"LS-SEARCH-CRITERIA\");"));

    }

    /**
     * check binding.
     */
    public void checkLsSearchCriteriaTypeBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsSearchCriteriaBinding.java");
        assertTrue(resStr
                .contains("lsStartwith = BF.createStringBinding(\"LsStartwith\","));
        assertTrue(resStr.contains("\"LsStartwith\", String.class, this);"));
        assertTrue(resStr
                .contains("lsStartwith.setCobolName(\"LS-STARTWITH\");"));
        assertTrue(resStr.contains("lsStartwith.setByteLength(8);"));
        assertTrue(resStr
                .contains("lsStartwithLen = BF.createPackedDecimalBinding(\"LsStartwithLen\","));
        assertTrue(resStr.contains("\"LsStartwithLen\", Long.class, this);"));
        assertTrue(resStr
                .contains("lsStartwithLen.setCobolName(\"LS-STARTWITH-LEN\");"));
        assertTrue(resStr.contains("lsStartwithLen.setByteLength(5);"));
        assertTrue(resStr.contains("lsStartwithLen.setTotalDigits(9);"));

    }

    /**
     * check binding.
     */
    public void checkLsTransactionsDataTypeBinding() {
        String resStr = getSource(
                GEN_SRC_DIR,
                "/com/legstar/test/coxb/dplarcht/bind/LsTransactionsDataBinding.java");
        assertTrue(resStr
                .contains("lsTransactionName = BF.createStringBinding(\"LsTransactionName\","));
        assertTrue(resStr
                .contains("\"LsTransactionName\", String.class, this);"));
        assertTrue(resStr
                .contains("lsTransactionName.setCobolName(\"LS-TRANSACTION-NAME\");"));
        assertTrue(resStr.contains("lsTransactionName.setByteLength(8);"));
        assertTrue(resStr
                .contains("lsTransactionProgram = BF.createStringBinding(\"LsTransactionProgram\","));
        assertTrue(resStr
                .contains("\"LsTransactionProgram\", String.class, this);"));
        assertTrue(resStr
                .contains("lsTransactionProgram.setCobolName(\"LS-TRANSACTION-PROGRAM\");"));
        assertTrue(resStr.contains("lsTransactionProgram.setByteLength(8);"));
    }

}
