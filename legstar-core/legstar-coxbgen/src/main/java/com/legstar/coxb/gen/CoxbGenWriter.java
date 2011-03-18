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

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.codegen.CodeGenVelocityException;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;

/**
 * Encapsulates file management and velocity template generation.
 */
public class CoxbGenWriter {

    /** Velocity template for complex elements. */
    public static final String COMPLEX_VLC_TEMPLATE = "vlc/coxb-bind-complex.vm";

    /** Velocity template for choice elements. */
    public static final String CHOICE_VLC_TEMPLATE = "vlc/coxb-bind-choice.vm";

    /** Velocity template for choice strategy sample. */
    public static final String CHOICE_STRATEGY_VLC_TEMPLATE = "vlc/coxb-bind-choice-strategy.vm";

    /** Velocity template for complex arrays elements. */
    public static final String COMPLEX_ARRAY_VLC_TEMPLATE = "vlc/coxb-bind-complex-array.vm";

    /** Velocity template for host to java transformer. */
    public static final String HOST_TO_JAVA_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-host-to-java-transformer.vm";

    /** Velocity template for java to host transformer. */
    public static final String JAVA_TO_HOST_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-java-to-host-transformer.vm";

    /** Velocity template for transformer provider. */
    public static final String HOST_XFORMERS_VLC_TEMPLATE = "vlc/coxb-bind-transformers.vm";

    /** Velocity template for host to XML transformer. */
    public static final String HOST_TO_XML_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-host-to-xml-transformer.vm";

    /** Velocity template for xml to host transformer. */
    public static final String XML_TO_HOST_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-xml-to-host-transformer.vm";

    /** Velocity template for xml transformer provider. */
    public static final String HOST_XML_XFORMERS_VLC_TEMPLATE = "vlc/coxb-bind-xml-transformers.vm";

    /** Velocity template for host to JSON transformer. */
    public static final String HOST_TO_JSON_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-host-to-json-transformer.vm";

    /** Velocity template for json to host transformer. */
    public static final String JSON_TO_HOST_XFORMER_VLC_TEMPLATE = "vlc/coxb-bind-json-to-host-transformer.vm";

    /** Velocity template for json transformer provider. */
    public static final String HOST_JSON_XFORMERS_VLC_TEMPLATE = "vlc/coxb-bind-json-transformers.vm";

    /** A set of methods to simplify the velocity templates. */
    private CodeGenHelper _codeGenHelper;

    /** Simplifying methods specific to coxb. */
    private CoxbHelper _coxbHelper;

    /** Container for all parameters to move around. */
    private CoxbGenModel _coxbGenModel;

    /** Folder where generated files are created. */
    private File _outputFolder;

    /** This generator name. */
    public static final String BINDING_GENERATOR_NAME = "LegStar Binding generator";

    /**
     * Constructor from an existing directory.
     * 
     * @param coxbGenModel set of parameters
     * @param outputFolder where files need to be generated
     * @throws CoxbGenException if velocity engine failed to initialize
     */
    public CoxbGenWriter(final CoxbGenModel coxbGenModel,
            final File outputFolder) throws CoxbGenException {
        _coxbGenModel = coxbGenModel;
        try {
            CodeGenUtil.initVelocity();
            _outputFolder = outputFolder;
        } catch (CodeGenVelocityException e) {
            throw new CoxbGenException(e);
        }
        _codeGenHelper = new CodeGenHelper();
        _coxbHelper = new CoxbHelper();
    }

    /**
     * Produces a binding class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void write(final ICobolComplexBinding ce) throws CoxbGenException {
        writeGeneric(ce, COMPLEX_VLC_TEMPLATE, _coxbHelper.getCoxbTypeName(ce)
                + ".java");
    }

    /**
     * Produces a binding class for a complex array element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void write(final ICobolArrayComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, COMPLEX_ARRAY_VLC_TEMPLATE,
                _coxbHelper.getCoxbTypeName(ce) + ".java");
    }

    /**
     * Produces a binding class for a choice element. Also generates samples for
     * strategy classes.
     * <p/>
     * The first alternative is bound to the REDEFINED COBOL item. If an
     * unmarshal strategy was specified for this COBOL item, override strategy
     * name that was specified in XML schema.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void write(final ICobolChoiceBinding ce) throws CoxbGenException {

        if (_coxbGenModel.getUnmarshalChoiceStrategies() != null) {
            try {
                ICobolBinding firstAlternative = ce.getAlternativesList()
                        .get(0);
                for (UnmarshalChoiceStrategy strategy : _coxbGenModel
                        .getUnmarshalChoiceStrategies()) {
                    if (strategy.getRedefinedCobolItem().equals(
                            firstAlternative.getCobolName())) {
                        ce.setUnmarshalChoiceStrategyClassName(strategy
                                .getUnmarshalChoiceStrategyClassName());
                        break;
                    }
                }

            } catch (HostException e) {
                throw new CoxbGenException(e);
            }
        }

        writeGeneric(ce, CHOICE_VLC_TEMPLATE, _coxbHelper.getCoxbTypeName(ce)
                + ".java");

        if (ce.getMarshalChoiceStrategyClassName() != null
                && ce.getMarshalChoiceStrategyClassName().length() > 0) {
            writeChoiceStrategy(ce, "Marshal",
                    ce.getMarshalChoiceStrategyClassName());

        }
        if (ce.getUnmarshalChoiceStrategyClassName() != null
                && ce.getUnmarshalChoiceStrategyClassName().length() > 0) {
            writeChoiceStrategy(ce, "Unmarshal",
                    ce.getUnmarshalChoiceStrategyClassName());

        }
    }

    /**
     * Produces a host to java transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeHostToJavaTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_TO_JAVA_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "HostToJavaTransformer.java");
    }

    /**
     * Produces a java to host transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeJavaToHostTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, JAVA_TO_HOST_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "JavaToHostTransformer.java");
    }

    /**
     * Produces a transformer provider class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeTransformers(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_XFORMERS_VLC_TEMPLATE, ce.getJaxbName()
                + "Transformers.java");
    }

    /**
     * Produces a host to XML transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeHostToXmlTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_TO_XML_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "HostToXmlTransformer.java");
    }

    /**
     * Produces an XML to host transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeXmlToHostTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, XML_TO_HOST_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "XmlToHostTransformer.java");
    }

    /**
     * Produces an XML transformer provider class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeXmlTransformers(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_XML_XFORMERS_VLC_TEMPLATE, ce.getJaxbName()
                + "XmlTransformers.java");
    }

    /**
     * Produces a host to JSON transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeHostToJsonTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_TO_JSON_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "HostToJsonTransformer.java");
    }

    /**
     * Produces an JSON to host transformer class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeJsonToHostTransformer(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, JSON_TO_HOST_XFORMER_VLC_TEMPLATE, ce.getJaxbName()
                + "JsonToHostTransformer.java");
    }

    /**
     * Produces an JSON transformer provider class for a complex element.
     * 
     * @param ce the binding element
     * @throws CoxbGenException if generation fails
     */
    public void writeJsonTransformers(final ICobolComplexBinding ce)
            throws CoxbGenException {
        writeGeneric(ce, HOST_JSON_XFORMERS_VLC_TEMPLATE, ce.getJaxbName()
                + "JsonTransformers.java");
    }

    /**
     * Applies a velocity template to a binding class producing a file.
     * 
     * @param ce the binding element
     * @param template the velocity template
     * @param fileName the generated file name
     * @throws CoxbGenException if generation fails
     */
    private void writeGeneric(final ICobolBinding ce, final String template,
            final String fileName) throws CoxbGenException {
        try {
            Map < String, Object > parameters = createParameters(ce);

            CodeGenUtil.processTemplate(BINDING_GENERATOR_NAME, template,
                    "binding", ce, parameters,
                    CodeGenUtil.getFile(_outputFolder, fileName));
        } catch (CodeGenMakeException e) {
            throw new CoxbGenException(e);
        }
    }

    /**
     * Produces a sample choice strategy class for a choice element. If previous
     * code exists at the target location, the sample gets an extra extension in
     * order not to overwrite any existing code.
     * 
     * @param ce the binding element
     * @param strategyType either Unmarshal or Marshal
     * @param strategyClassName a fully qualified class name for the strategy
     * @throws CoxbGenException if generation fails
     */
    public void writeChoiceStrategy(final ICobolChoiceBinding ce,
            final String strategyType, final String strategyClassName)
            throws CoxbGenException {
        try {
            Map < String, Object > parameters = createParameters(ce);
            parameters.put("choice-strategy-type", strategyType);
            parameters.put("choice-strategy-qualified-class-name",
                    strategyClassName);

            String dir = _coxbGenModel.getCoxbSrcDir().getAbsolutePath()
                    + '/'
                    + CodeGenUtil.relativeLocation(_codeGenHelper
                            .getPackageName(strategyClassName,
                                    _coxbGenModel.getCoxbPackageName()));
            CodeGenUtil.checkDirectory(dir, true);

            /* Check for previous code */
            File targetFile = CodeGenUtil.getFile(dir,
                    _codeGenHelper.getClassName(strategyClassName) + ".java");
            if (targetFile.exists()) {
                targetFile = CodeGenUtil.getFile(dir,
                        _codeGenHelper.getClassName(strategyClassName)
                                + ".java.new");
            }

            CodeGenUtil.processTemplate(BINDING_GENERATOR_NAME,
                    CHOICE_STRATEGY_VLC_TEMPLATE, "binding", ce, parameters,
                    targetFile);

        } catch (CodeGenMakeException e) {
            throw new CoxbGenException(e);
        }
    }

    /**
     * @param binding the binding element being processed
     * @return a set of parameters that velocity templates can use.
     */
    private Map < String, Object > createParameters(final ICobolBinding binding) {
        Map < String, Object > parameters = new HashMap < String, Object >();

        parameters.put("helper", _codeGenHelper);
        parameters.put("coxbContext", _coxbGenModel);
        parameters.put("coxbHelper", _coxbHelper);
        parameters.put("binding-class-name",
                _coxbHelper.getCoxbTypeName(binding));
        return parameters;
    }
}
