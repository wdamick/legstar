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
package com.legstar.codegen;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * This Ant task iteratively calls the Velocity template engine to create
 * a list of artifacts described in an xml file.
 * The xml file typically describes a target as a sequential list of steps.
 * Each step is associated with a velocity template and produces an artifact.
 * TODO describe the format of xml make in an xsd
 */
public class CodeGenMake extends Task {

    /** The name of the calling generator. */
    private String mGeneratorName;

    /** The cixs make file name. */
    private String mCodeGenMakeFileName;

    /**
     * Velocity templates are applied against a model.
     * This is the model name.
     */
    private String mModelName;

    /**
     * Velocity templates are applied against a model.
     * This is the model itself.
     */
    private Object mModel;

    /** XML element representing a code generation target. */
    public static final String CODEGEN_TARGET_XML_E = "target";

    /** XML attribute representing a code generation target name. */
    public static final String CODEGEN_TARGET_NAME_XML_A = "name";

    /** XML attribute representing a code generation target directory. */
    public static final String CODEGEN_TARGET_DIR_XML_A = "dir";

    /** XML element representing a code generation step with a template. */
    public static final String CODEGEN_STEP_XML_E = "step";

    /** XML attribute representing a code generation template name. */
    public static final String CODEGEN_TEMPLATE_NAME_XML_A = "templateName";

    /** XML attribute representing a code generation target file. */
    public static final String CODEGEN_TEMPLATE_TARGET_FILE_XML_A = "targetFile";

    /** XML attribute representing a code generation target character set. */
    public static final String CODEGEN_TEMPLATE_TARGET_CHARSETNAME_XML_A = "targetCharsetName";

    /** Logger. */
    private final Log _log = LogFactory.getLog(CodeGenMake.class);

    /** @{inheritDoc  */
    @Override
    public void init() {
        try {
            _log.info("Initializing Code generation make processor");
            CodeGenUtil.initVelocity();
        } catch (Exception e) {
            throw new BuildException(e.getMessage());
        }
    }

    /** @{inheritDoc */
    @Override
    public void execute() {
        long start = System.currentTimeMillis();
        try {
            _log.debug("Start make processor");
            Document doc = getInput();
            if (_log.isDebugEnabled()) {
                _log.debug("Make file name   = " + mCodeGenMakeFileName);
                _log.debug("Model name       = " + mModelName);
            }
            processTargets(doc);
        } catch (CodeGenMakeException e) {
            _log.error("Make processor failure", e);
            throw new BuildException(e.getMessage());
        }
        long end = System.currentTimeMillis();
        _log.info("Make processing success for " + mCodeGenMakeFileName);
        _log.info("Duration = " + (end - start) + " ms");
    }

    /**
     * Check input values and load the xml make file.
     * 
     * @return an in-memory DOM xml make
     * @throws CodeGenMakeException if initialization fails
     */
    private Document getInput() throws CodeGenMakeException {
        if (mCodeGenMakeFileName == null || mCodeGenMakeFileName.length() == 0) {
            throw new CodeGenMakeException("Missing make file parameter");
        }
        if (mModelName == null || mModelName.length() == 0) {
            throw new CodeGenMakeException("Missing model name parameter");
        }
        if (mModel == null) {
            throw new CodeGenMakeException("Missing model parameter");
        }
        File makeFile = new File(mCodeGenMakeFileName);

        if (!makeFile.exists()) {
            throw new CodeGenMakeException("Code generation make file "
                    + makeFile + " does not exist");
        }
        return load(makeFile);
    }

    /**
     * Process targets sequentially, invoking the velocity engine for each
     * target.
     * 
     * @param doc a valid DOM for a cixs make
     * @throws CodeGenMakeException if processing fails
     */
    private void processTargets(final Document doc) throws CodeGenMakeException {
        NodeList listOfElements = doc.getElementsByTagName(
                CODEGEN_TARGET_XML_E);
        if (listOfElements == null || listOfElements.getLength() == 0) {
            throw (new CodeGenMakeException(
                    "Empty or invalid code generation make file"));
        }
        Element targetElement = (Element) listOfElements.item(0);
        String targetName = targetElement
                .getAttribute(CODEGEN_TARGET_NAME_XML_A);
        if (targetName == null || targetName.length() == 0) {
            throw new CodeGenMakeException("Missing name attribute for "
                    + CODEGEN_TARGET_XML_E + " element");
        }
        String targetDir = targetElement.getAttribute(CODEGEN_TARGET_DIR_XML_A);
        if (targetDir != null && targetDir.length() > 0) {
            try {
                CodeGenUtil.checkDirectory(targetDir, true);
            } catch (IllegalArgumentException e) {
                throw new CodeGenMakeException(e);
            }
        }
        listOfElements = targetElement.getElementsByTagName(
                CODEGEN_STEP_XML_E);

        if (_log.isDebugEnabled()) {
            _log.debug("Processing target");
            _log.debug("Target name      = " + targetName);
            _log.debug("Target directory = " + targetDir);
        }
        for (int i = 0; i < listOfElements.getLength(); i++) {
            Element templateEl = (Element) listOfElements.item(i);
            String templateName = templateEl.getAttribute(
                    CODEGEN_TEMPLATE_NAME_XML_A);
            if (templateName == null || templateName.length() == 0) {
                throw new CodeGenMakeException(
                        "Missing template name attribute for "
                                + CODEGEN_STEP_XML_E + " element");
            }
            String templateTargetFileName = templateEl.getAttribute(
                    CODEGEN_TEMPLATE_TARGET_FILE_XML_A);
            if (templateTargetFileName == null
                    || templateTargetFileName.length() == 0) {
                throw new CodeGenMakeException(
                        "Missing template target file name attribute for "
                                + CODEGEN_STEP_XML_E + " element");
            }
            String templateTargetCharsetName = templateEl.getAttribute(
                    CODEGEN_TEMPLATE_TARGET_CHARSETNAME_XML_A);
            if (templateTargetCharsetName.length() == 0) {
                templateTargetCharsetName = null;
            }
            CodeGenUtil.processTemplate(
                    mGeneratorName,
                    templateName,
                    mModelName, mModel,
                    getParameters(templateEl),
                    CodeGenUtil.getFile(targetDir, templateTargetFileName),
                    templateTargetCharsetName);
        }
    }

    /**
     * Template elements might have children. These are used to pass additional
     * parameters to the template generation process.
     * Children are expected to have a single value attribute otherwise, they
     * are ignored.
     * 
     * @param templateEl the current template element
     * @return a Map of parameters/values
     */
    private Map < String, Object > getParameters(final Element templateEl) {
        Map < String, Object > parameters = new HashMap < String, Object >();
        NodeList childs = templateEl.getChildNodes();
        for (int i = 0; i < childs.getLength(); i++) {
            if (childs.item(i) instanceof Element) {
                Element parmEl = (Element) childs.item(i);
                String value = parmEl.getAttribute("value");
                if (value != null) {
                    parameters.put(parmEl.getNodeName(), value);
                }
            }
        }
        return parameters;
    }

    /**
     * Loads code generation make file from a serialized XML.
     * 
     * @param makeFile the serialized file
     * @return an in-memory DOM code generation make
     * @throws CodeGenMakeException if load fails
     */
    public Document load(final File makeFile) throws CodeGenMakeException {
        DocumentBuilderFactory docBuilderFactory =
                DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder;
        try {
            docBuilderFactory.setNamespaceAware(false);
            docBuilder = docBuilderFactory.newDocumentBuilder();
            Document doc = docBuilder.parse(makeFile);
            return doc;
        } catch (ParserConfigurationException e) {
            throw (new CodeGenMakeException(e));
        } catch (SAXException e) {
            throw (new CodeGenMakeException(e));
        } catch (IOException e) {
            throw (new CodeGenMakeException(e));
        }
    }

    /**
     * @return the code generation Make File name
     */
    public String getCodeGenMakeFileName() {
        return mCodeGenMakeFileName;
    }

    /**
     * @param codeGenMakeFileName the code generation Make File name to set
     */
    public void setCodeGenMakeFileName(final String codeGenMakeFileName) {
        mCodeGenMakeFileName = codeGenMakeFileName;
    }

    /**
     * @return the model name
     */
    public String getModelName() {
        return mModelName;
    }

    /**
     * @param modelName the model name to set
     */
    public void setModelName(final String modelName) {
        mModelName = modelName;
    }

    /**
     * @return the model used to apply templates to
     */
    public Object getModel() {
        return mModel;
    }

    /**
     * @param model the model to set
     */
    public void setModel(final Object model) {
        mModel = model;
    }

    /**
     * @return the name of the calling generator
     */
    public String getGeneratorName() {
        return mGeneratorName;
    }

    /**
     * @param generatorName the name of the calling generator to set
     */
    public void setGeneratorName(final String generatorName) {
        mGeneratorName = generatorName;
    }

}
