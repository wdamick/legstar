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

import java.io.File;

import org.apache.commons.io.FileUtils;

import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;

/**
 * Test the code generation writer.
 * 
 */
public class CoxbGenWriterTest extends AbstractCoxbGenTest {

    /** True when references should be created. */
    private static final boolean CREATE_REFERENCES = false;

    /** @{inheritDoc */
    public void setUp() throws Exception {
        super.setUp();
        setCreateReferences(CREATE_REFERENCES);
    }

    /**
     * A complex type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenAlltypes() throws Exception {
        genSourceAndCheck("alltypes", "Dfhcommarea", "DfhcommareaBinding.java");
    }

    /**
     * A complex type containing a redefine case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenRedsimpt() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("redsimpt", "Dfhcommarea");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        genSource("redsimpt", cc);
        check("redsimpt", "CDefinition1ChoiceBinding.java");
    }

    /**
     * A complex array type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenArrayssm() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("arrayssm", "Dfhcommarea");
        ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce
                .getChildrenList().get(1);
        genSource("arrayssm", ca);
        check("arrayssm", "TableComplexWrapperBinding.java");

    }

    /**
     * A choice strategy type case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenChoiceStrategy() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("redsimpt", "Dfhcommarea");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        CoxbGenWriter writer = new CoxbGenWriter(createModel("redsimpt"),
                getTargetFolder("redsimpt"));

        /* Do it twice to check the backup mechanism */
        writer.writeChoiceStrategy(cc, "Unmarshal",
                "com.legstar.coxb.cust.redsimpt.ChoiceSelector");
        writer.writeChoiceStrategy(cc, "Unmarshal",
                "com.legstar.coxb.cust.redsimpt.ChoiceSelector");

        File resultFolder = new File(GEN_SRC_DIR,
                "com/legstar/coxb/cust/redsimpt");
        File refFolder = new File(REF_DIR, getClass().getSimpleName());
        check("redsimpt", "ChoiceSelector.java.new", refFolder, resultFolder);

    }

    /**
     * Generate a host to java transformer case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenHostToJavaTransformer() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("lsfileae", "Dfhcommarea");
        CoxbGenWriter writer = new CoxbGenWriter(createModel("lsfileae"),
                getTargetFolder("lsfileae"));
        writer.writeHostToJavaTransformer(ce);
        check("lsfileae", "DfhcommareaHostToJavaTransformer.java");
    }

    /**
     * Generate a java to host transformer case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenJavaToHostTransformer() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("lsfileae", "Dfhcommarea");
        CoxbGenWriter writer = new CoxbGenWriter(createModel("lsfileae"),
                getTargetFolder("lsfileae"));
        writer.writeJavaToHostTransformer(ce);
        check("lsfileae", "DfhcommareaJavaToHostTransformer.java");

    }

    /**
     * Generate a transformer provider case.
     * 
     * @throws Exception if generation fails
     */
    public void testGenTransformers() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("lsfileae", "Dfhcommarea");
        CoxbGenWriter writer = new CoxbGenWriter(createModel("lsfileae"),
                getTargetFolder("lsfileae"));
        writer.writeTransformers(ce);
        check("lsfileae", "DfhcommareaTransformers.java");

    }

    /**
     * A choice strategy that is passed as a parameter (rather than in XML
     * schema).
     * 
     * @throws Exception if generation fails
     */
    public void testGenChoiceStrategyParameter() throws Exception {
        ICobolComplexBinding ce = getComplexBinding("redsimpt", "Dfhcommarea");
        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList()
                .get(0);
        CoxbGenModel coxbContext = createModel("redsimpt");
        coxbContext.addUnmarshalChoiceStrategy(new UnmarshalChoiceStrategy(
                "C-DEFINITION-1:another.UnmarshalChoiceStrategy"));
        CoxbGenWriter writer = new CoxbGenWriter(coxbContext,
                getTargetFolder("redsimpt"));
        writer.write(cc);
        File resultFolder = new File(GEN_SRC_DIR, "another");
        File refFolder = new File(REF_DIR, getClass().getSimpleName());
        check("redsimpt", "UnmarshalChoiceStrategy.java", refFolder,
                resultFolder);
    }

    /**
     * Generate a source from a template and check against reference.
     * 
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @param fileName the target file name
     * @throws Exception usually if test not set properly
     */
    protected void genSourceAndCheck(final String schemaName,
            final String rootName, final String fileName) throws Exception {
        genSource(schemaName, rootName);
        check(schemaName, fileName);
    }

    /**
     * Generate a source from a template for a root complex type.
     * 
     * @param schemaName the originating XSD name
     * @param rootName JAXB root class name
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String schemaName, final String rootName)
            throws Exception {
        genSource(schemaName, getComplexBinding(schemaName, rootName));
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param schemaName the originating XSD name
     * @param binding the type COBOL binding
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String schemaName,
            final ICobolBinding binding) throws Exception {

        genSource(schemaName, binding, createModel(schemaName));
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param schemaName the schema name
     * @param binding the type COBOL binding
     * @param coxbContext parameter set
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final String schemaName,
            final ICobolBinding binding, CoxbGenModel coxbContext)
            throws Exception {

        genSource(binding, coxbContext, getTargetFolder(schemaName));
    }

    /**
     * Generate a source from a template for any type.
     * 
     * @param binding the type COBOL binding
     * @param coxbContext parameter set
     * @param targetFolder where to store the generated content
     * @throws Exception usually if test not set properly
     */
    protected void genSource(final ICobolBinding binding,
            final CoxbGenModel coxbContext, final File targetFolder)
            throws Exception {

        FileUtils.forceMkdir(targetFolder);
        CoxbGenWriter writer = new CoxbGenWriter(coxbContext, targetFolder);
        if (binding instanceof ICobolComplexBinding) {
            writer.write((ICobolComplexBinding) binding);
        } else if (binding instanceof ICobolArrayComplexBinding) {
            writer.write((ICobolArrayComplexBinding) binding);
        } else if (binding instanceof ICobolChoiceBinding) {
            writer.write((ICobolChoiceBinding) binding);
        }
    }

}
