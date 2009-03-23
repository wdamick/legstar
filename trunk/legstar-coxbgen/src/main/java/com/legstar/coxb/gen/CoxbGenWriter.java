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

/**
 * Encapsulates file management and velocity template generation.
 */
public class CoxbGenWriter {

    /** Velocity template for complex elements. */
    public static final String COMPLEX_VLC_TEMPLATE =
        "vlc/coxb-bind-complex.vm";

    /** Velocity template for choice elements. */
    public static final String CHOICE_VLC_TEMPLATE =
        "vlc/coxb-bind-choice.vm";

    /** Velocity template for choice strategy sample. */
    public static final String CHOICE_STRATEGY_VLC_TEMPLATE =
        "vlc/coxb-bind-choice-strategy.vm";

    /** Velocity template for complex arrays elements. */
    public static final String COMPLEX_ARRAY_VLC_TEMPLATE =
        "vlc/coxb-bind-complex-array.vm";

    /** Velocity template for host to java transformer. */
    public static final String HOST_TO_JAVA_XFORMER_VLC_TEMPLATE =
        "vlc/coxb-bind-host-to-java-transformer.vm";

    /** Velocity template for java to host transformer. */
    public static final String JAVA_TO_HOST_XFORMER_VLC_TEMPLATE =
        "vlc/coxb-bind-java-to-host-transformer.vm";

    /** Velocity template for transformer provider. */
    public static final String HOST_XFORMERS_VLC_TEMPLATE =
        "vlc/coxb-bind-transformers.vm";

    /** Velocity template for host to XML transformer. */
    public static final String HOST_TO_XML_XFORMER_VLC_TEMPLATE =
        "vlc/coxb-bind-host-to-xml-transformer.vm";

    /** Velocity template for xml to host transformer. */
    public static final String XML_TO_HOST_XFORMER_VLC_TEMPLATE =
        "vlc/coxb-bind-xml-to-host-transformer.vm";

    /** Velocity template for xml transformer provider. */
    public static final String HOST_XML_XFORMERS_VLC_TEMPLATE =
        "vlc/coxb-bind-xml-transformers.vm";

    /** A set of methods to simplify the velocity templates. */
    private CodeGenHelper mHelper;

    /** Simplifying methods specific to coxb. */
    private CoxbHelper mCoxbHelper;

    /** Container for all parameters to move around. */
    private CoxbGenModel mCoxbGenContext;

    /** This generator name. */
    public static final String BINDING_GENERATOR_NAME =
        "LegStar Binding generator";

    /**
     * Constructor from an existing directory.
     * 
     * @param coxbGenContext set of parameters
     * @throws CodeGenException if velocity engine failed to initialize 
     */
    public CoxbGenWriter(
            final CoxbGenModel coxbGenContext) throws CodeGenException {
        mCoxbGenContext = coxbGenContext;
        try {
            CodeGenUtil.initVelocity();
            CodeGenUtil.checkDirectory(
                    mCoxbGenContext.getCoxbSrcDir().getAbsolutePath(), false);
        } catch (CodeGenVelocityException e) {
            throw new CodeGenException(e);
        }
        mHelper = new CodeGenHelper();
        mCoxbHelper = new CoxbHelper();
    }

    /**
     * Produces a binding class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void write(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, COMPLEX_VLC_TEMPLATE,
                mCoxbHelper.getCoxbTypeName(ce) + ".java");
    }

    /**
     * Produces a binding class for a complex array element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void write(
            final ICobolArrayComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, COMPLEX_ARRAY_VLC_TEMPLATE,
                mCoxbHelper.getCoxbTypeName(ce) + ".java");
    }

    /**
     * Produces a binding class for a choice element.
     * Also generates samples for strategy classes.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void write(
            final ICobolChoiceBinding ce) throws CodeGenException {

        writeGeneric(ce, CHOICE_VLC_TEMPLATE,
                mCoxbHelper.getCoxbTypeName(ce) + ".java");

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
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeHostToJavaTransformer(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, HOST_TO_JAVA_XFORMER_VLC_TEMPLATE,
                ce.getJaxbName() + "HostToJavaTransformer.java");
    }

    /**
     * Produces a java to host transformer class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeJavaToHostTransformer(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, JAVA_TO_HOST_XFORMER_VLC_TEMPLATE,
                ce.getJaxbName() + "JavaToHostTransformer.java");
    }

    /**
     * Produces a transformer provider class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeTransformers(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, HOST_XFORMERS_VLC_TEMPLATE,
                ce.getJaxbName() + "Transformers.java");
    }

    /**
     * Produces a host to XML transformer class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeHostToXmlTransformer(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, HOST_TO_XML_XFORMER_VLC_TEMPLATE,
                ce.getJaxbName() + "HostToXmlTransformer.java");
    }

    /**
     * Produces an XML to host transformer class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeXmlToHostTransformer(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, XML_TO_HOST_XFORMER_VLC_TEMPLATE,
                ce.getJaxbName() + "XmlToHostTransformer.java");
    }

    /**
     * Produces an XML transformer provider class for a complex element.
     * @param ce the binding element
     * @throws CodeGenException if generation fails
     */
    public final void writeXmlTransformers(
            final ICobolComplexBinding ce) throws CodeGenException {
        writeGeneric(ce, HOST_XML_XFORMERS_VLC_TEMPLATE,
                ce.getJaxbName() + "XmlTransformers.java");
    }

    /**
     * Applies a velocity template to a binding class producing a file.
     * @param ce the binding element
     * @param template the velocity template
     * @param fileName the generated file name
     * @throws CodeGenException if generation fails
     */
    private void writeGeneric(
            final ICobolBinding ce,
            final String template,
            final String fileName) throws CodeGenException {
        try {
            Map < String, Object > parameters = createParameters(ce);

            String dir = mCoxbGenContext.getCoxbSrcDir().getAbsolutePath() + '/'
            + CodeGenUtil.relativeLocation(
                    mCoxbGenContext.getCoxbPackageName());
            CodeGenUtil.checkDirectory(dir, true);

            CodeGenUtil.processTemplate(
                    BINDING_GENERATOR_NAME,
                    template,
                    "binding", ce,
                    parameters,
                    CodeGenUtil.getFile(dir, fileName));
        } catch (CodeGenMakeException e) {
            throw new CodeGenException(e);
        }
    }

    /**
     * Produces a sample choice strategy class for a choice element.
     * If previous code exists at the target location, the sample gets
     * an extra extension in order not to overwrite any existing code.
     * @param ce the binding element
     * @param strategyType either Unmarshal or Marshal
     * @param strategyClassName a fully qualified class name for the strategy
     * @throws CodeGenException if generation fails
     */
    public final void writeChoiceStrategy(
            final ICobolChoiceBinding ce,
            final String strategyType,
            final String strategyClassName) throws CodeGenException {
        try {
            Map < String, Object > parameters = createParameters(ce);
            parameters.put("choice-strategy-type", strategyType);
            parameters.put("choice-strategy-qualified-class-name",
                    strategyClassName);

            String dir = mCoxbGenContext.getCoxbSrcDir().getAbsolutePath() + '/'
            + CodeGenUtil.relativeLocation(
                    mHelper.getPackageName(strategyClassName,
                            mCoxbGenContext.getCoxbPackageName()));
            CodeGenUtil.checkDirectory(dir, true);

            /* Check for previous code */
            File targetFile = CodeGenUtil.getFile(dir,
                    mHelper.getClassName(strategyClassName) + ".java");
            if (targetFile.exists()) {
                targetFile = CodeGenUtil.getFile(dir,
                        mHelper.getClassName(strategyClassName) + ".java.new");
            }

            CodeGenUtil.processTemplate(
                    BINDING_GENERATOR_NAME,
                    CHOICE_STRATEGY_VLC_TEMPLATE,
                    "binding", ce,
                    parameters,
                    targetFile);

        } catch (CodeGenMakeException e) {
            throw new CodeGenException(e);
        }
    }

    /**
     * @param binding the binding element being processed
     * @return a set of parameters that velocity templates can use.
     */
    private Map < String, Object > createParameters(
            final ICobolBinding binding) {
        Map < String, Object > parameters =
            new HashMap < String, Object >();

        parameters.put("helper", mHelper);
        parameters.put("coxbContext", mCoxbGenContext);
        parameters.put("coxbHelper", mCoxbHelper);
        parameters.put("binding-class-name",
                mCoxbHelper.getCoxbTypeName(binding));
        return parameters;
    }
}
