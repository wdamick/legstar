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
package com.legstar.cixs.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cixs.gen.model.options.HttpTransportParameters;
import com.legstar.cixs.gen.model.options.WebServiceParameters;
import com.legstar.cixs.gen.model.options.WmqTransportParameters;
import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.gen.CoxbHelper;

import junit.framework.TestCase;

/**
 * This is code common to all junit tests that exercise the velocity
 * templates.
 */
public class AbstractTestTemplate extends TestCase {

    /** Parent generation folder. */
    public static final File GEN_DIR = new File("target/src/gen");

    /** Location of JAXB classes. */
    public static final File JAXB_BIN_DIR = new File("target/classes");

    /** Code will be generated here. */
    public static final File GEN_SRC_DIR = new File("target/src/gen/java");

    /** Configuration files will be generated here. */
    public static final File GEN_CONF_DIR = new File("target/src/gen/conf");

    /** Cobol files will be generated here. */
    public static final File GEN_COBOL_DIR = new File("target/src/gen/cobol");

    /** Web descriptors files will be generated here. */
    public static final File GEN_WDD_DIR = new File("target/src/gen/webapp");

    /** Web descriptors files will be generated here. */
    public static final File GEN_ANT_DIR = new File("target/src/gen/ant");

    /** Properties files will be generated here. */
    public static final File GEN_PROP_DIR = new File("target/src/gen/prop");

    /** Reference to binaries location. */
    public static final File GEN_BIN_DIR = new File("target/src/gen/target/classes");

    /** Reference to war files location. */
    public static final File GEN_WAR_DIR = new File("${env.CATALINA_BASE}/webapp");

    /** Additional parameter set passed to templates. */
    private Map < String, Object > mParameters;

    /** Logger. */
    private final Log _log = LogFactory.getLog(
            AbstractTestTemplate.class);

    /** @{inheritDoc}*/
    @Override
    public void setUp() {
        try {
            emptyDir(GEN_DIR);
            CodeGenUtil.initVelocity();
            CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
            CodeGenUtil.checkDirectory(GEN_CONF_DIR, true);
            CodeGenUtil.checkDirectory(GEN_COBOL_DIR, true);
            CodeGenUtil.checkDirectory(GEN_WDD_DIR, true);
            CodeGenUtil.checkDirectory(GEN_ANT_DIR, true);
            mParameters = new HashMap < String, Object >();
            CodeGenHelper helper = new CodeGenHelper();
            mParameters.put("helper", helper);
            mParameters.put("coxbHelper", new CoxbHelper());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    /**
     * Apply a velocity template and return the generated source.
     * @param model model to use
     * @param generatorName the name to appear as generator 
     * @param templateName the velocity template to apply
     * @param dir the folder where generated source should go
     * @param genSourceName the generate file name
     * @return the source code generated
     * @throws Exception if something goes wrong
     */
    public String genSource(
            final CixsJaxwsService model,
            final String generatorName,
            final String templateName,
            final File dir,
            final String genSourceName) throws Exception {
        CodeGenUtil.processTemplate(
                generatorName,
                templateName,
                "model", model,
                getParameters(),
                CodeGenUtil.getFile(dir, genSourceName));
        return getSource(dir, genSourceName);
    }

    /**
     * A general purpose reader that gets the file content into a string.
     * @param srcDir the location of the source artifact
     * @param srcName the source artifact name
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(
            final File srcDir, final String srcName) throws IOException {
        return getSource(new File(srcDir, srcName));
    }

    /**
     * A general purpose reader that gets the file content into a string.
     * @param fileName the name of the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(final String fileName) throws IOException {
        return getSource(new File(fileName));
    }
    /**
     * A general purpose reader that gets the file content into a string.
     * @param file the file pointing to source
     * @return a string containing the generated source
     * @throws IOException if something goes wrong
     */
    public String getSource(final File file) throws IOException {
        BufferedReader in = new BufferedReader(
                new FileReader(file));
        String resStr = "";
        String str = in.readLine();
        while (str != null) {
            _log.debug(str);
            resStr += str;
            str = in.readLine();
        }
        in.close();
        return resStr;
    }


    /**
     * @return the mParameters
     */
    public final Map < String, Object > getParameters() {
        return mParameters;
    }

    /**
     * Recreates a folder after emptying its content.
     * @param dir the folder to empy
     */
    public void emptyDir(final File dir) {
        deleteDir(dir);
        dir.mkdirs();
    }

    /**
     * Destroys a folder and all of its content.
     * @param dir the folder to destroy
     */
    public void deleteDir(final File dir) {
        if (dir.exists()) {
            for (File file : dir.listFiles()) {
                if (file.isDirectory()) {
                    deleteDir(file);
                }
                file.delete();
            }
        }
    }
    
    /**
     * Get Web Service default parameters.
     * @param service the service
     * @return the default set of parameters
     */
    public WebServiceParameters getDefaultWebServiceParameters(
            final CixsJaxwsService service) {
        WebServiceParameters webServiceParameters = new WebServiceParameters();
        webServiceParameters.setWsdlPortName(service.getName()
                + Jaxws2CixsGenerator.DEFAULT_WSDL_PORT_NAME_SUFFIX);
        webServiceParameters.setWsdlServiceName(service.getName()
                + Jaxws2CixsGenerator.DEFAULT_WSDL_SERVICE_NAME_SUFFIX);
        webServiceParameters.setWsdlTargetNamespace(
                Jaxws2CixsGenerator.DEFAULT_WSDL_TARGET_NAMESPACE_PREFIX
                + '/' + service.getName());
        return webServiceParameters;
        
    }

    /**
     * Add Web Service default parameters.
     * @param service the service
     * @param parameters the set of parameters
     */
    public void addWebServiceParameters(
            final CixsJaxwsService service,
            final Map < String, Object > parameters) {
        WebServiceParameters webServiceParameters =
            getDefaultWebServiceParameters(service);
        webServiceParameters.add(parameters);
        
    }

    /**
     * Produce a set of http parameters.
     * @param model the service
     * @return a default set of http parameters
     */
    public HttpTransportParameters getDefaultHttpParameters(final CixsJaxwsService model) {
        HttpTransportParameters httpTransportParameters = new HttpTransportParameters();
        httpTransportParameters.setPath(
                Cixs2JaxwsGenerator.DEFAULT_SERVER_PATH_TEMPLATE.replace(
                        "${service.name}", model.getName()));
        return httpTransportParameters;
    }
 
    /**
     * Add Web Service default parameters.
     * @param service the service
     * @param parameters the set of parameters
     */
    public void addHttpTransportParameters(
            final CixsJaxwsService service,
            final Map < String, Object > parameters) {
        HttpTransportParameters httpTransportParameters =
            getDefaultHttpParameters(service);
        httpTransportParameters.add(parameters);
        
    }

    /**
     * Produce a set of WMQ parameters.
     * @param model the service
     * @return a default set of WMQ parameters
     */
    public WmqTransportParameters getDefaultWmqParameters(final CixsJaxwsService model) {
        WmqTransportParameters wmqTransportParameters = new WmqTransportParameters();
        wmqTransportParameters.setConnectionFactory("MyQCF");
        wmqTransportParameters.setZosQueueManager("CSQ1");
        wmqTransportParameters.setRequestQueue("REQUEST.QUEUE");
        wmqTransportParameters.setReplyQueue("REPLY.QUEUE");
        wmqTransportParameters.setErrorQueue("ERROR.QUEUE");
        return wmqTransportParameters;
    }
 
    /**
     * Add Web Service default parameters.
     * @param service the service
     * @param parameters the set of parameters
     */
    public void addWmqTransportParameters(
            final CixsJaxwsService service,
            final Map < String, Object > parameters) {
        WmqTransportParameters wmqTransportParameters =
            getDefaultWmqParameters(service);
        wmqTransportParameters.add(parameters);
        
    }

}
