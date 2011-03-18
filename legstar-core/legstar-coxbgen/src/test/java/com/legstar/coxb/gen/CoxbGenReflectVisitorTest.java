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
package com.legstar.coxb.gen;

import com.legstar.coxb.ICobolComplexBinding;

/**
 * Test CoxbGenReflectVisitor.
 * 
 */
public class CoxbGenReflectVisitorTest extends AbstractCoxbGenTest {

    /**
     * Dplarcht is one of the most complex samples that we have. Good way to
     * check that all artifacts are produced by the visitor.
     * 
     * @throws Exception if generation fails
     */
    public void testDplarcht() throws Exception {
        CoxbGenModel coxbContext = createModel("dplarcht");
        coxbContext.setXmlTransformers(true);
        coxbContext.setJsonTransformers(true);

        ICobolComplexBinding ce = getComplexBinding("dplarcht", "Dfhcommarea");

        CoxbGenReflectVisitor visitor = new CoxbGenReflectVisitor(coxbContext,
                getTargetFolder("dplarcht"));

        visitor.visit(ce);

        check("dplarcht", "DfhcommareaBinding.java");
        check("dplarcht", "LsAllItemsChoiceBinding.java");
        check("dplarcht", "LsFilesDataChoiceBinding.java");
        check("dplarcht", "LsFilesDataBinding.java");
        check("dplarcht", "LsItemsArrayBinding.java");
        check("dplarcht", "LsItemsArrayWrapperBinding.java");
        check("dplarcht", "LsProgramsDataBinding.java");
        check("dplarcht", "LsReplyDataBinding.java");
        check("dplarcht", "LsReplyBinding.java");
        check("dplarcht", "LsRequestBinding.java");
        check("dplarcht", "LsSearchCriteriaBinding.java");
        check("dplarcht", "LsTransactionsDataBinding.java");
    }

}
