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
package com.legstar.cixs.gen.test;

import java.io.File;
import java.io.IOException;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;


/**
 * Test cases for Jaxws2CixsGenerator.
 */
public class Jaxws2CixsGeneratorTest extends AbstractTestTemplate {

	private Jaxws2CixsGenerator mGenerator;

	/** General location for generated artifacts. */
	private static final String GEN_DIR = "src/test/gen";

	/** Ant scripts will be generated here. */
	private static final String GEN_ANT_DIR = "ant";

	/** Web descriptors will be generated here. */
	private static final String GEN_WEB_DIR = "WebContent/WEB-INF";

	/** Property files will be generated here. */
	private static final String GEN_PROP_DIR = "WebContent/WEB-INF/classes";
	

    public void setUp() {
        mGenerator = new Jaxws2CixsGenerator();
        mGenerator.init();
        mGenerator.setTargetSrcDir(GEN_SRC_DIR);
    }
	
	/**
     * Check controls on input make file.
     */
    public final void testInputValidation() {
        Jaxws2CixsGenerator generator = new Jaxws2CixsGenerator();
        try {
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing service description parameter",
                    e.getCause().getMessage());
        }
        CixsJaxwsService cixsJaxwsService = new CixsJaxwsService();
        cixsJaxwsService.setName("jaxwsServiceName");
        try {
            generator.setCixsJaxwsService(cixsJaxwsService);
            generator.execute();
            fail();
        } catch (Exception e) {
            assertEquals("java.lang.IllegalArgumentException:"
                    + " No directory name was specified",
                    e.getCause().getMessage());
        }
       
    }
    
    private void initJaxwsService(CixsJaxwsService cixsJaxwsService) {
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
        mGenerator.setTargetAntDir(
        		new File(GEN_DIR + '/' + cixsJaxwsService.getName() + '/' + GEN_ANT_DIR));
        mGenerator.setTargetWDDDir(
        		new File(GEN_DIR + '/' + cixsJaxwsService.getName() + '/' + GEN_WEB_DIR));
        mGenerator.setTargetPropDir(
        		new File(GEN_DIR + '/' + cixsJaxwsService.getName() + '/' + GEN_PROP_DIR));
    }
    
    /**
     * Check generation when no operations are specified.
     * @throws Exception if generation fails
     */
    public final void testGenerateClassesNoOperations() throws Exception {
    	
        
        CixsJaxwsService cixsJaxwsService = new CixsJaxwsService();
        cixsJaxwsService.setName("jaxwsServiceName");
        cixsJaxwsService.setInterfaceClassName("JaxwsService");
        cixsJaxwsService.setImplementationClassName("JaxwsServiceImpl");
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        String resStr = getSource(GEN_SRC_DIR + "/JaxwsService.java");
        assertTrue(resStr.contains("public interface JaxwsService {"));
        
        File interfaceFile = new File(GEN_SRC_DIR + "/JaxwsService.java");
        interfaceFile.delete();
        File implementationFile = new File(GEN_SRC_DIR + "/JaxwsServiceImpl.java");
        implementationFile.delete();
        File headerFile = new File(GEN_SRC_DIR + "/JaxwsServiceHeader.java");
        headerFile.delete();
        
    }

    /**
     * Check generation for operation with identical input and output structures.
     * @throws Exception if generation fails
     */
    public final void testLsfileaeGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileae();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "Lsfileae");
        checkOperationResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "lsfileae", "Lsfileae");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileae", "lsfileae", "Lsfileae", "Lsfileae", "Response");
    }
    
    /**
     * Check generation for operation with different input and output structures.
     * @throws Exception if generation fails
     */
    public final void testgetLsfilealGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileal();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfileal", "lsfileal", "Lsfileal");
        checkOperationResult("/com/legstar/test/cixs/Lsfileal", "lsfileal", "lsfileal", "Lsfileal");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal", "lsfileal", "Lsfileal", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/lsfileal", "lsfileal", "lsfileal", "Lsfileal", "Response");
    }

    /**
     * Check generation for CICS containers target component.
     * @throws Exception if generation fails
     */
    public final void testLsfileacGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileac();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "Lsfileac");
        checkOperationResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileac", "lsfileac", "lsfileac", "Lsfileac", "Response");
    }
    
    /**
     * Check generation for multiple operations components.
     * @throws Exception if generation fails
     */
    public final void testLsfileaxGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileax();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "Lsfileax");
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileae", "Lsfileae");
        checkOperationResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "lsfileac", "Lsfileac", "Response");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "Lsfileae", "Lsfileae", "Request");
        checkWebWrapperResult("/com/legstar/test/cixs/Lsfileax", "lsfileax", "Lsfileae", "Lsfileae", "Response");
    }
    
    /**
     * Check generation for service in default package.
     * @throws Exception if generation fails
     */
    public final void testLsfileapGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileap();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("", "lsfileap", "Lsfileap");
        checkOperationResult("", "lsfileap", "lsfileae", "Lsfileae");
    }
    
    /**
     * Check generation for service with different operation package.
     * @throws Exception if generation fails
     */
    public final void testLsfileanGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfilean();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfilean", "lsfilean", "Lsfilean");
        checkOperationResult("/com/legstar/test/cixs/oper/Lsfilean", "lsfilean", "lsfileae", "Lsfileae");
    }

    /**
     * Check generation for service with different a single structure per container.
     * @throws Exception if generation fails
     */
    public final void testLsfileaqGenerateClasses() throws Exception {
        CixsJaxwsService cixsJaxwsService = TestCases.getLsfileaq();
        initJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "Lsfileaq");
        checkOperationResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileac", "Lsfileac");
        checkHolderResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileaq", "Lsfileac", "Request");
        checkHolderResult("/com/legstar/test/cixs/Lsfileaq", "lsfileaq", "lsfileaq", "Lsfileac", "Response");
    }

    private void checkComponentResult(
    		String relativeLoc, String service, String ClassName) throws IOException {
        String resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
        				".java");
        assertTrue(resStr.contains(
                "public interface " + ClassName +
                " {"));

        resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/"  + ClassName +
        				"Impl.java");
        assertTrue(resStr.contains(
                "public class "  + ClassName +
                "Impl implements " + ClassName +
                " {"));

        resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
        				"HostHeader.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "HostHeader {"));

        resStr = getSource(
        		GEN_DIR + '/' + service + '/' + GEN_ANT_DIR + "/"
                + "build.xml");
        assertTrue(resStr.contains(
                "<war warfile=\"${targetWarDir}/cixs-" + service +
                ".war\""));
        
        resStr = getSource(
        		GEN_DIR + '/' + service + '/' + GEN_WEB_DIR + "/"
                + "web.xml");
        assertTrue(resStr.contains(
                "<servlet-name>" + service + "Service</servlet-name>"));
        
        resStr = getSource(
        		GEN_DIR + '/' + service + '/' + GEN_WEB_DIR + "/"
                + "sun-jaxws.xml");
        assertTrue(resStr.contains(
                "<endpoint name=\"" + service + "Service\""));
        
    }
    
    public void checkOperationResult(
    		String relativeLoc, String service, String operation, String ClassName) throws IOException {
    	String resStr = getSource(
    			GEN_DIR + '/' + service + '/' + GEN_PROP_DIR
                + "/" + operation + ".properties");
        assertTrue(resStr.contains(
                "CICSProgramName=" + operation.toUpperCase() ));
        resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
                "Fault.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "Fault"));
        
        resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
                "FaultInfo.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "FaultInfo {"));
        
    }
    
    public void checkHolderResult(
    		String relativeLoc, String service, String operation, String ClassName, String propertyName) throws IOException {
        String resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName + propertyName +
                "Holder.java");
        assertTrue(resStr.contains(
                "public class " + ClassName + propertyName +
                "Holder {"));
        
    }
    
    public void checkWebWrapperResult(
    		String relativeLoc, String service, String operation, String ClassName, String propertyName) throws IOException {
        String resStr = getSource(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName + propertyName +
                ".java");
        assertTrue(resStr.contains(
                "public class " + ClassName + propertyName +
                " {"));
        
    }
    
}
