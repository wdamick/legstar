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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.CixsJaxwsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;

import junit.framework.TestCase;

/**
 * Test cases for CixsJaxwsGenerator.
 */
public class CixsJaxwsGeneratorTest extends TestCase {

    /** Logger. */
	private static final Log LOG = LogFactory.getLog(CixsJaxwsGeneratorTest.class);
	
	/** Java Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	/** Ant scripts will be generated here. */
	private static final String GEN_ANT_DIR = "src/test/gen/ant";

	/** Web descriptors will be generated here. */
	private static final String GEN_WEB_DIR = "src/test/gen/WebContent/WEB-INF";

	/** Property files will be generated here. */
	private static final String GEN_PROP_DIR = "src/test/gen/WebContent/WEB-INF/classes";
	
	private CixsJaxwsGenerator mGenerator;

    public void setUp() {
        mGenerator = new CixsJaxwsGenerator();
        mGenerator.init();
        mGenerator.setTargetSrcDir(GEN_SRC_DIR);
        mGenerator.setTargetAntDir(GEN_ANT_DIR);
        mGenerator.setTargetWDDDir(GEN_WEB_DIR);
        mGenerator.setTargetPropDir(GEN_PROP_DIR);
    }
	
	/**
     * Check controls on input make file.
     */
    public final void testInputValidation() {
        CixsJaxwsGenerator generator = new CixsJaxwsGenerator();
        try {
            generator.execute();
        } catch (Exception e) {
            assertEquals("Missing cixs Jaxws service parameter",
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
    
    /**
     * Check generation when no operations are specified.
     * @throws Exception if generation fails
     */
    public final void testGenerateClassesNoOperations() throws Exception {
    	
        CixsJaxwsService cixsJaxwsService = new CixsJaxwsService();
        cixsJaxwsService.setName("jaxwsServiceName");
        cixsJaxwsService.setInterfaceClassName("JaxwsService");
        cixsJaxwsService.setImplementationClassName("JaxwsServiceImpl");
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        String resStr = getContent(GEN_SRC_DIR + "/JaxwsService.java");
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
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
        mGenerator.setCixsJaxwsService(cixsJaxwsService);
        mGenerator.execute();
        checkComponentResult("/com/legstar/test/cixs/Lsfilean", "lsfilean", "Lsfilean");
        checkOperationResult("/com/legstar/test/cixs/oper/Lsfilean", "lsfilean", "lsfileae", "Lsfileae");
    }

    private void checkComponentResult(
    		String relativeLoc, String service, String ClassName) throws IOException {
        String resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
        				".java");
        assertTrue(resStr.contains(
                "public interface " + ClassName +
                " {"));

        resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/"  + ClassName +
        				"Impl.java");
        assertTrue(resStr.contains(
                "public class "  + ClassName +
                "Impl implements " + ClassName +
                " {"));

        resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
        				"HostHeader.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "HostHeader {"));

        resStr = getContent(
        		GEN_ANT_DIR + "/"  + service +
        				"/"
                + "build.xml");
        assertTrue(resStr.contains(
                "<war warfile=\"${warDir}/cixs-" + service +
                ".war\""));
        
        resStr = getContent(
        		GEN_WEB_DIR + "/" + service + "/"
                + "web.xml");
        assertTrue(resStr.contains(
                "<servlet-name>" + service + "Service</servlet-name>"));
        
        resStr = getContent(
        		GEN_WEB_DIR + "/" + service + "/"
                + "sun-jaxws.xml");
        assertTrue(resStr.contains(
                "<endpoint name=\"" + service + "Service\""));
        
    }
    
    public void checkOperationResult(
    		String relativeLoc, String service, String operation, String ClassName) throws IOException {
    	String resStr = getContent(
        		GEN_PROP_DIR
                + "/" + operation + ".properties");
        assertTrue(resStr.contains(
                "CICSProgramName=" + operation.toUpperCase() ));
        resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
                "Fault.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "Fault"));
        
        resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName +
                "FaultInfo.java");
        assertTrue(resStr.contains(
                "public class " + ClassName +
                "FaultInfo {"));
        
    }
    
    public void checkHolderResult(
    		String relativeLoc, String service, String operation, String ClassName, String propertyName) throws IOException {
        String resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName + propertyName +
                "Holder.java");
        assertTrue(resStr.contains(
                "public class " + ClassName + propertyName +
                "Holder {"));
        
    }
    
    public void checkWebWrapperResult(
    		String relativeLoc, String service, String operation, String ClassName, String propertyName) throws IOException {
        String resStr = getContent(
        		GEN_SRC_DIR + relativeLoc +
        				"/" + ClassName + propertyName +
                ".java");
        assertTrue(resStr.contains(
                "public class " + ClassName + propertyName +
                " {"));
        
    }
    
    /**
     * Reads the content of a file in a string.
     * @param fileName name of the file
     * @return a string with the file content
     * @throws IOException if fails to read file
     */
    private static String getContent(final String fileName) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(fileName));
        StringBuilder resStr = new StringBuilder();
        String str = in.readLine();
        while (str != null) {
            LOG.debug(str);
            resStr.append(str);
            str = in.readLine();
        }
        in.close();
        return resStr.toString();
    }

}
