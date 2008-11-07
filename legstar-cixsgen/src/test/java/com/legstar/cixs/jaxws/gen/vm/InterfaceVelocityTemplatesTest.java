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
package com.legstar.cixs.jaxws.gen.vm;

import java.io.File;

import com.legstar.cixs.gen.AbstractTestTemplate;
import com.legstar.cixs.gen.TestCases;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

public class InterfaceVelocityTemplatesTest extends AbstractTestTemplate {
	
	private static final String CIXS_JAXWS_GENERATOR_NAME =
		"LegStar Jaxws service Generator";
	
	public void testCommonPackage() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileae();
        String resStr = genSource(model,
        		CIXS_JAXWS_GENERATOR_NAME,
        		"vlc/j2c-service-common-package.vm",
        		GEN_SRC_DIR,
        		"test.txt");
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
	}
	
	public void testInterfaceCommareainEqCommareaout() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileae();

		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");
        
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileae;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfileaePort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("public interface Lsfileae {"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileaeRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileae.LsfileaeRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileaeResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileae.LsfileaeResponse\")"));
        assertTrue(resStr.contains("Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileae\")"));
        assertTrue(resStr.contains("LsfileaeHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeFault;"));
	}
	
	public void testInterfaceCommareainNeqCommareaout() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileal();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");
		
        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileal;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.RequestParms;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileal.ReplyData;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfilealPort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\")"));
        assertTrue(resStr.contains("public interface Lsfileal {"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfilealRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileal.LsfilealRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfilealResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileal.LsfilealResponse\")"));
        assertTrue(resStr.contains("ReplyData lsfileal("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("RequestParms request,"));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileal\")"));
        assertTrue(resStr.contains("LsfilealHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfilealFault;"));
	}
	
	public void testInterfaceContainer() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileac();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileac;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfileacPort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileac\")"));
        assertTrue(resStr.contains("public interface Lsfileac {"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileacRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileac\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileac.LsfileacRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileacResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileac\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileac.LsfileacResponse\")"));
        assertTrue(resStr.contains("LsfileacResponseHolder lsfileac("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("LsfileacRequestHolder request,"));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileac\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileac\")"));
        assertTrue(resStr.contains("LsfileacHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileacFault;"));
	}

	public void testInterfaceMultiOperation() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileax();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfileax;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfileaxPort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\")"));
        assertTrue(resStr.contains("public interface Lsfileax {"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileaeRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileax.LsfileaeRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileaeResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileax.LsfileaeResponse\")"));
        assertTrue(resStr.contains("Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\")"));
        assertTrue(resStr.contains("Dfhcommarea request,"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\")"));
        assertTrue(resStr.contains("LsfileaxHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeFault;"));
        
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileacRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileax.LsfileacRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileacResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.lsfileax.LsfileacResponse\")"));
        assertTrue(resStr.contains("LsfileacResponseHolder lsfileac("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("LsfileacRequestHolder request,"));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileax\")"));
        assertTrue(resStr.contains("LsfileaxHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileacFault;"));
	}

	public void testInterfaceOperationWithDifferentNamespace() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfilean();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");

        assertTrue(resStr.contains("package com.legstar.test.cixs.lsfilean;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfileanPort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfilean\")"));
        assertTrue(resStr.contains("public interface Lsfilean {"));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/oper/lsfilean\")"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileaeRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/oper/lsfilean\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.oper.lsfilean.LsfileaeRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileaeResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/oper/lsfilean\","));
        assertTrue(resStr.contains("className = \"com.legstar.test.cixs.oper.lsfilean.LsfileaeResponse\")"));
        assertTrue(resStr.contains("Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/oper/lsfilean\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/oper/lsfilean\")"));
        assertTrue(resStr.contains("LsfileanHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeFault;"));
	}
	
	public void testInterfaceOperationWithNoPackage() throws Exception {
		
		CixsJaxwsService model = TestCases.getLsfileap();
    	
		File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				GEN_SRC_DIR, model.getPackageName(), true);
		Jaxws2CixsGenerator.generateInterface(
				model, getParameters(), componentClassFilesDir);
        String resStr = getSource(
        		componentClassFilesDir,
        		model.getInterfaceClassName() + ".java");

        assertFalse(resStr.contains("package com.legstar.test.cixs.lsfileap;"));
        assertTrue(resStr.contains("import com.legstar.test.coxb.lsfileae.Dfhcommarea;"));
        assertTrue(resStr.contains("@WebService(name = \"lsfileapPort\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\")"));
        assertTrue(resStr.contains("public interface Lsfileap {"));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\")"));
        assertTrue(resStr.contains("@RequestWrapper(localName = \"LsfileaeRequest\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\","));
        assertTrue(resStr.contains("className = \"LsfileaeRequest\")"));
        assertTrue(resStr.contains("@ResponseWrapper(localName = \"LsfileaeResponse\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\","));
        assertTrue(resStr.contains("className = \"LsfileaeResponse\")"));
        assertTrue(resStr.contains("Dfhcommarea lsfileae("));
        assertTrue(resStr.contains("@WebParam(name = \"Request\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\")"));
        assertTrue(resStr.contains("@WebParam(name = \"HostHeader\", header = true, partName = \"HostHeader\","));
        assertTrue(resStr.contains("targetNamespace = \"http://cixs.test.legstar.com/lsfileap\")"));
        assertTrue(resStr.contains("LsfileapHostHeader hostHeader)"));
        assertTrue(resStr.contains("throws LsfileaeFault;"));
	}
	
}
