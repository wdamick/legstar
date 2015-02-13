/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.cobol.gen;

import com.legstar.cobc.AbstractTest;
import com.legstar.cobol.model.CobolDataItem;
import com.legstar.cobol.model.CobolDataItem.Range;
import com.legstar.coxb.CobolUsage;
import com.legstar.coxb.CobolUsage.Usage;

public class CopybookGeneratorTest extends AbstractTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    public boolean isCreateReferences() {
        return CREATE_REFERENCES;
    }

    public void testEmptyGroup() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testGroupWithSingleChild() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem childDataDataItem = new CobolDataItem(5, "CUSTOMER-NAME");
        childDataDataItem.setPicture("X");
        cobolDataItem.getChildren().add(childDataDataItem);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testHierarchy() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "TOP-ITEM");
        CobolDataItem depth1Item1 = new CobolDataItem(5, "DEPTH-1-ITEM-1");
        CobolDataItem depth1Item1Depth2Item1 = new CobolDataItem(10,
                "DEPTH-2-ITEM-1");
        depth1Item1Depth2Item1.setPicture("X");
        CobolDataItem depth1Item1Depth2Item2 = new CobolDataItem(10,
                "DEPTH-2-ITEM-2");
        depth1Item1Depth2Item2.setPicture("9");
        depth1Item1.getChildren().add(depth1Item1Depth2Item1);
        depth1Item1.getChildren().add(depth1Item1Depth2Item2);

        CobolDataItem depth1Item2 = new CobolDataItem(5, "DEPTH-1-ITEM-2");
        CobolDataItem depth1Item2Depth2Item1 = new CobolDataItem(10,
                "DEPTH-2-ITEM-1");
        depth1Item2Depth2Item1.setPicture("X");
        CobolDataItem depth1Item2Depth2Item2 = new CobolDataItem(10,
                "DEPTH-2-ITEM-2");
        depth1Item2Depth2Item2.setPicture("9");
        depth1Item2.getChildren().add(depth1Item2Depth2Item1);
        depth1Item2.getChildren().add(depth1Item2Depth2Item2);

        cobolDataItem.getChildren().add(depth1Item1);
        cobolDataItem.getChildren().add(depth1Item2);

        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testRedefines() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem alphanumItem = new CobolDataItem(5, "CUSTOMER-ALPHANUM");
        alphanumItem.setPicture("X");
        CobolDataItem numItem = new CobolDataItem(5, "CUSTOMER-NUM");
        numItem.setPicture("9");
        numItem.setRedefines("CUSTOMER-ALPHANUM");
        cobolDataItem.getChildren().add(alphanumItem);
        cobolDataItem.getChildren().add(numItem);

        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testOccurs() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem alphanumItem = new CobolDataItem(5, "CUSTOMER-ALPHANUM");
        alphanumItem.setPicture("X");
        alphanumItem.setMinOccurs(5);
        alphanumItem.setMaxOccurs(5);
        cobolDataItem.getChildren().add(alphanumItem);

        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testOccursDependingOn() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem numItem = new CobolDataItem(5, "CUSTOMER-NUM");
        numItem.setPicture("9");
        CobolDataItem alphanumItem = new CobolDataItem(5, "CUSTOMER-ALPHANUM");
        alphanumItem.setPicture("X");
        alphanumItem.setMinOccurs(0);
        alphanumItem.setMaxOccurs(5);
        alphanumItem.setDependingOn("CUSTOMER-NUM");
        cobolDataItem.getChildren().add(numItem);
        cobolDataItem.getChildren().add(alphanumItem);

        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testUsageNoPicture() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setUsage(CobolUsage.Usage.DOUBLEFLOAT);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testUsageAndPicture() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        cobolDataItem.setUsage(CobolUsage.Usage.BINARY);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithShortNumericValue() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        cobolDataItem.setUsage(CobolUsage.Usage.BINARY);
        cobolDataItem.setValue("-5");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithShortStringValue() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setValue("'IPSUM LOREM'");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithShortStringValueEscaped() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setValue("\"THIS ISN\"\"T WRONG\"");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithLongStringValue() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(160)");
        cobolDataItem
                .setValue("'In COBOL, alphanumeric literals cannot exceed one hundred and sixty characters. This is unlike most recent languages which do not put limits on strings length .'");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithMultipleLongStringValue() {
        CobolDataItem rootDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem firstDataItem = new CobolDataItem(5,
                "CUSTOMER-DATA-FIRST");
        firstDataItem.setPicture("X(160)");
        firstDataItem
                .setValue("'In COBOL, alphanumeric literals cannot exceed one hundred and sixty characters. This is unlike most recent languages which do not put limits on strings length .'");
        CobolDataItem secondDataItem = new CobolDataItem(5,
                "CUSTOMER-DATA-SECOND");
        secondDataItem.setPicture("X(160)");
        secondDataItem
                .setValue("'In COBOL, alphanumeric literals cannot exceed one hundred and sixty characters. This is unlike most recent languages which do not put limits on strings length .'");
        rootDataItem.getChildren().add(firstDataItem);
        rootDataItem.getChildren().add(secondDataItem);
        check(CopybookGenerator.generate(rootDataItem));
    }

    public void testWithLongStringValueAndEscapedDelimiter() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(160)");
        cobolDataItem
                .setValue("'In COBOL, alphanumeric literals cannot exceed ''one hundred'' and sixty characters. This is unlike most recent languages which do not put limits on strings length'");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithLongNumericValue() {
        CobolDataItem cobolDataItem = new CobolDataItem(1,
                "DATA-WITH-UNUSUALLY-LONG-NAME");
        cobolDataItem.setPicture("9(18)");
        cobolDataItem.setUsage(Usage.DISPLAY);
        cobolDataItem.setValue("123456789012345678");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithHexadecimalValue() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setValue("X\"B1F2CDEF\"");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithBlankWhenZero() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setBlankWhenZero(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithExternal() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setExternal(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithGlobal() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setGlobal(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithJustifiedRight() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setJustifiedRight(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithSignLeading() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setSignLeading(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithSignTrailing() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setSignLeading(false);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithSignTrailingSeparate() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setSignLeading(false);
        cobolDataItem.setSignSeparate(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithSynchronized() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("X(11)");
        cobolDataItem.setSynchronized(true);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithConditionLiterals() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        CobolDataItem cond1DataItem = new CobolDataItem(88, "PRESENT");
        cond1DataItem.getConditionLiterals().add("0");
        cobolDataItem.getChildren().add(cond1DataItem);
        CobolDataItem cond2DataItem = new CobolDataItem(88, "NOT-PRESENT");
        cond2DataItem.getConditionLiterals().add("1");
        cobolDataItem.getChildren().add(cond2DataItem);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithConditionMultipleLiterals() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        CobolDataItem condDataItem = new CobolDataItem(88, "PRESENT");
        condDataItem.getConditionLiterals().add("0");
        condDataItem.getConditionLiterals().add("3");
        condDataItem.getConditionLiterals().add("4");
        cobolDataItem.getChildren().add(condDataItem);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithConditionRangeLiteral() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        CobolDataItem condDataItem = new CobolDataItem(88, "PRESENT");
        condDataItem.getConditionRanges().add(new Range("0", "5"));
        cobolDataItem.getChildren().add(condDataItem);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithConditionMultipleRangeLiterals() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        cobolDataItem.setPicture("9(4)");
        CobolDataItem cond1DataItem = new CobolDataItem(88, "PRESENT");
        cond1DataItem.getConditionRanges().add(new Range("0", "5"));
        cond1DataItem.getConditionRanges().add(new Range("17", "95"));
        cobolDataItem.getChildren().add(cond1DataItem);
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithRenames() {
        CobolDataItem cobolDataItem = new CobolDataItem(66, "CUSTOMER-DATA");
        cobolDataItem.setRenamesSubject("OLD-NAME");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithRenamesRange() {
        CobolDataItem cobolDataItem = new CobolDataItem(66, "CUSTOMER-DATA");
        cobolDataItem.setRenamesSubjectRange(new Range("OLD-FIRST-NAME",
                "OLD-LAST-NAME"));
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testWithDateFormat() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "DATE-FIELD");
        cobolDataItem.setPicture("9(6)");
        cobolDataItem.setDateFormat("YYXXXX");
        check(CopybookGenerator.generate(cobolDataItem));
    }

    public void testGroupWithSingleChildWithoutHeader() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CUSTOMER-DATA");
        CobolDataItem childDataDataItem = new CobolDataItem(5, "CUSTOMER-NAME");
        childDataDataItem.setPicture("X");
        cobolDataItem.getChildren().add(childDataDataItem);
        check(CopybookGenerator.generate(cobolDataItem, false));
    }

    public void testRootItemHasLevelGreaterThanOne() {
        CobolDataItem cobolDataItem = new CobolDataItem(3, "CUSTOMER-DATA");
        CobolDataItem childDataDataItem = new CobolDataItem(5, "CUSTOMER-NAME");
        childDataDataItem.setPicture("X");
        cobolDataItem.getChildren().add(childDataDataItem);
        check(CopybookGenerator.generate(cobolDataItem, false));
    }

    public void testItemWithLevel88() {
        CobolDataItem cobolDataItem = new CobolDataItem(1, "CONDITION-DATA");
        cobolDataItem.setPicture("9(4)");
        cobolDataItem.setUsage(Usage.BINARY);
        CobolDataItem childDataDataItem = new CobolDataItem(88, "TRUE");
        childDataDataItem.addConditionLiterals("0");
        cobolDataItem.getChildren().add(childDataDataItem);
        check(CopybookGenerator.generate(cobolDataItem, false));
    }

    public void testIndentationAfterDeepNode() {
        CobolDataItem item1 = new CobolDataItem(1, "ITEM1");
        CobolDataItem item1_5 = new CobolDataItem(5, "ITEM-1-5");
        CobolDataItem item1_5_10 = new CobolDataItem(10, "ITEM-1-5-10");
        CobolDataItem item1_5_10_15 = new CobolDataItem(15, "ITEM-1-5-10-15");
        CobolDataItem item2_5 = new CobolDataItem(5, "ITEM-2-5");

        item1.getChildren().add(item1_5);
        item1_5.getChildren().add(item1_5_10);
        item1_5_10.getChildren().add(item1_5_10_15);
        item1.getChildren().add(item2_5);

        check(CopybookGenerator.generate(item1));
    }

}
