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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;

import junit.framework.TestCase;

public class CoxbGenModelTest extends TestCase {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CoxbGenModelTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/ant";

	public void testBuildCoxb() throws Exception {

		CoxbGenModel model = new CoxbGenModel();
		
		List < String > jaxbRootClassNames = new ArrayList < String > ();
		jaxbRootClassNames.add("class1");
		jaxbRootClassNames.add("class2");

		model.setProductLocation("/Users/Fady/sandbox/legstar-1.2.0");
		model.setJaxbSrcDir(new File("jaxb/src"));
		model.setJaxbBinDir(new File("jaxb/bin"));
		model.setXsdFile(new File("myXsd.xsd"));
		model.setJaxbXjcBindingDir(new File("/Users/Fady/sandbox/legstar-1.2.0/xjb"));
		model.setCoxbSrcDir(new File("coxb/src"));
		model.setCoxbBinDir(new File("coxb/bin"));
		model.setJaxbRootClassNames(jaxbRootClassNames);
		model.setProbeFile(new File("probe.file.tmp"));

		model.generateBuild(CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));
		
		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("<project basedir=\"/Users/Fady/sandbox/legstar-1.2.0\" default=\"signalSuccess\" name=\"generate-COXB-classes\">"));
		assertTrue(resStr.contains("<dirset dir=\"jaxb\\bin\"/>"));
		assertTrue(resStr.contains("<mkdir dir=\"jaxb\\bin\"/>"));
		assertTrue(resStr.contains("<mkdir dir=\"jaxb\\src\"/>"));
		assertTrue(resStr.contains("<echo message=\"Generating JAXB classes for myXsd.xsd\" />"));
		assertTrue(resStr.contains("<xjc schema=\"myXsd.xsd\" destdir=\"jaxb\\src\" extension=\"true\" removeOldOutput=\"yes\">"));
		assertTrue(resStr.contains("<arg value=\"\\Users\\Fady\\sandbox\\legstar-1.2.0\\xjb\" />"));
		assertTrue(resStr.contains("<javac srcdir=\"jaxb\\src\""));
		assertTrue(resStr.contains("destdir=\"jaxb\\bin\""));
		assertTrue(resStr.contains("<echo message=\"Generating binding classes for myXsd.xsd\" />"));
		assertTrue(resStr.contains("xsdFile=\"myXsd.xsd\""));
		assertTrue(resStr.contains("targetDir=\"coxb\\src\""));
		assertTrue(resStr.contains("<jaxbRootClass name=\"class1\"/>"));
		assertTrue(resStr.contains("<jaxbRootClass name=\"class2\"/>"));
		assertTrue(resStr.contains("<javac srcdir=\"coxb\\src\""));
		assertTrue(resStr.contains("destdir=\"coxb\\bin\""));
		assertTrue(resStr.contains("<delete file=\"probe.file.tmp\" quiet=\"true\"/>"));
	}
}
