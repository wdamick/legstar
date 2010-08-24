package com.legstar.jaxb.gen;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.listener.CommonsLoggingListener;
import org.apache.tools.ant.types.Commandline;

import com.legstar.codegen.CodeGenMakeException;
import com.sun.tools.xjc.XJC2Task;

/**
 * A lightweight ant Task which augments the standard JAXB XJC Task.
 * <p/>
 * The basic idea is to collect customization parameters and generate a
 * temporary JAXB external customization file.
 * <p/>
 * This is not a replacement for XJC, just a more restricted way of using it for
 * legstar to relieve upfront modules, such as cob2xsd, from adding JAXB
 * customizations to the XML SChema.
 * 
 */
public class CobolJAXBGenerator extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * From XJC, this is the schema file to be compiled. A file name
     * (can be relative to the build script base directory), or an URL.
     * Required parameter.
     */
    private String _schema;

    /**
     * From XJC. If specified, generated code will be placed under
     * this Java package. This option is equivalent to the "-p" command-line
     * switch.
     * Optional parameter.
     */
    private String _package;

    /** From XJC. Generated code will be written under this directory. */
    private File _destdir;

    /** The model used for XJB generation. */
    private CobolJAXBXJBModel _xjbModel = new CobolJAXBXJBModel();

    /** Injects the JAXB custom bindings in an XMl SChema. */
    private CobolJAXBCustomizer _customizer;

    /** Whether internal bindings or and external binding should be used. */
    private boolean _internalBindings = true;

    /**
     * The ant method. Generates JAXB classes by invoking XJC.
     */
    public void execute() {
        _log.info("JAXB generation started.");

        checkInput();

        // Recreate an XJC task each time because schemas and xjb files are
        // cumulative

        XJC2Task xjcTask = new XJC2Task();
        xjcTask.setProject(getProject());
        xjcTask.setTaskName("xjcTask");

        // Use external or internal customization
        String xsdLocation = getSchema();
        if (_internalBindings) {
            xsdLocation = createInternalCustomization(getSchema());
        } else {
            xjcTask.setBinding(createExternalCustomization(getSchema()));
        }

        xjcTask.setSchema(xsdLocation);
        xjcTask.setPackage(getPackage());
        xjcTask.setDestdir(getDestdir());

        xjcTask.setExtension(true);
        xjcTask.setRemoveOldOutput(true);
        Commandline.Argument arg1 = xjcTask.createArg();
        arg1.setValue("-Xlegstar-code");
        Commandline.Argument arg2 = xjcTask.createArg();
        arg2.setValue("-nv");

        xjcTask.execute();

        _log.info("JAXB generation ended.");
    }

    /**
     * Checks that properties set are valid.
     */
    protected void checkInput() {

        try {
            if (_log.isDebugEnabled()) {
                _log.debug("checkInput started");
                _log.debug("Schema: " + getSchema());
                _log.debug("Destdir: " + getDestdir());
                _log.debug("Package name: " + getPackage());
                _log.debug("xjbModel: " + _xjbModel);
            }
            if (getSchema() == null) {
                throw (new BuildException(
                        "You must specify an XML schema file name"));
            } else {
                File schemaFile = new File(getSchema());
                if (schemaFile.exists()) {
                    if (_log.isDebugEnabled()) {
                        for (Object line : FileUtils.readLines(schemaFile)) {
                            _log.debug(line);
                        }
                    }
                } else {
                    throw (new BuildException(
                            "XML schema file " + getSchema()
                                    + " does not exist"));
                }
            }
            if (getDestdir() == null) {
                throw (new BuildException(
                        "You must specify a destination directory"));
            }
        } catch (IOException e) {
            throw new BuildException(e);
        }
    }

    /**
     * Creates a temporary XML Schema which holds JAXB customization
     * bindings.
     * 
     * @param xsdLocation the original XML schema location
     * @return the location of the customized XML Schema
     */
    protected String createInternalCustomization(final String xsdLocation) {
        try {
            File tempXsdFile = File.createTempFile("jaxb-schema", ".xsd");
            tempXsdFile.deleteOnExit();

            _customizer.customize(new File(xsdLocation), tempXsdFile);

            return tempXsdFile.getAbsolutePath();
        } catch (IOException e) {
            throw new BuildException(e);
        }

    }

    /**
     * Creates a temporary binding file holding the parameters chosen.
     * 
     * @param xsdLocation the XML Schema location
     * @return the location of the temporary XJB file
     */
    public String createExternalCustomization(final String xsdLocation) {
        try {
            File tempXJBFile = File.createTempFile("jaxb-xjb", ".xml");
            tempXJBFile.deleteOnExit();

            // The schema location needs to be a valid URI
            URI xsdURILocation = (new File(xsdLocation)).toURI();
            System.out.println(xsdURILocation.toString());
            _xjbModel.setXsdLocation(xsdURILocation.toString());
            _xjbModel.generateXjb(tempXJBFile);

            return tempXJBFile.getAbsolutePath();
        } catch (IOException e) {
            throw new BuildException(e);
        } catch (CodeGenMakeException e) {
            throw new BuildException(e);
        }

    }

    /**
     * This is the schema file to be compiled. A file name
     * (can be relative to the build script base directory), or an URL.
     * 
     * @return the schema file name
     */
    public String getSchema() {
        return _schema;
    }

    /**
     * This is the schema file to be compiled. A file name
     * (can be relative to the build script base directory), or an URL.
     * 
     * @param schema file name
     */
    public void setSchema(final String schema) {
        _schema = schema;
    }

    /**
     * If specified, generated code will be placed under
     * this Java package.
     * 
     * @return Java package name
     */
    public String getPackage() {
        return _package;
    }

    /**
     * If specified, generated code will be placed under this Java package.
     * 
     * @param package1 Java package name
     */
    public void setPackage(final String package1) {
        _package = package1;
    }

    /**
     * Generated code will be written under this directory.
     * 
     * @return destination directory
     */
    public File getDestdir() {
        return _destdir;
    }

    /**
     * Generated code will be written under this directory.
     * 
     * @param destdir destination directory
     */
    public void setDestdir(final File destdir) {
        _destdir = destdir;
    }

    /**
     * @return if IsSet Methods should be generated
     */
    public boolean isGenerateIsSetMethod() {
        return _xjbModel.isGenerateIsSetMethod();
    }

    /**
     * @param generateIsSetMethod if IsSet Methods should be generated
     */
    public void setGenerateIsSetMethod(final boolean generateIsSetMethod) {
        _xjbModel.setGenerateIsSetMethod(generateIsSetMethod);
    }

    /**
     * @return the serialization unique ID. (All JAXB classes must be
     *         serializable for LegStar)
     */
    public long getSerializableUid() {
        return _xjbModel.getSerializableUid();
    }

    /**
     * @param serializableUid the serialization unique ID. (All JAXB classes
     *            must be serializable for LegStar)
     */
    public void setSerializableUid(final long serializableUid) {
        _xjbModel.setSerializableUid(serializableUid);
    }

    /**
     * @return the prefix to add to type names
     */
    public String getTypeNamePrefix() {
        return _xjbModel.getTypeNamePrefix();
    }

    /**
     * @param typeNamePrefix the prefix to add to type names
     */
    public void setTypeNamePrefix(final String typeNamePrefix) {
        _xjbModel.setTypeNamePrefix(typeNamePrefix);
    }

    /**
     * @return the suffix to add to type names
     */
    public String getTypeNameSuffix() {
        return _xjbModel.getTypeNameSuffix();
    }

    /**
     * @param typeNameSuffix the suffix to add to type names
     */
    public void setTypeNameSuffix(final String typeNameSuffix) {
        _xjbModel.setTypeNameSuffix(typeNameSuffix);
    }

    /**
     * @return the prefix to add to element names
     */
    public String getElementNamePrefix() {
        return _xjbModel.getElementNamePrefix();
    }

    /**
     * @param elementNamePrefix the prefix to add to element names
     */
    public void setElementNamePrefix(final String elementNamePrefix) {
        _xjbModel.setElementNamePrefix(elementNamePrefix);
    }

    /**
     * @return the suffix to add to element names
     */
    public String getElementNameSuffix() {
        return _xjbModel.getElementNameSuffix();
    }

    /**
     * @param elementNameSuffix the suffix to add to element names
     */
    public void setElementNameSuffix(final String elementNameSuffix) {
        _xjbModel.setElementNameSuffix(elementNameSuffix);
    }

    /**
     * Initialize a commons logging logger so that XJC logs gets merged
     * with the legstar ones.
     * 
     * @see org.apache.tools.ant.Task#init()
     */
    @Override
    public void init() {
        try {
            BuildListener logger = new CommonsLoggingListener();
            getProject().addBuildListener(logger);

            _customizer = new CobolJAXBCustomizer(_xjbModel);

            super.init();
        } catch (ParserConfigurationException e) {
            throw new BuildException(e);
        }
    }

    /**
     * @return whether internal bindings or and external binding should be used
     */
    public boolean isInternalBindings() {
        return _internalBindings;
    }

    /**
     * @param internalBindings whether internal bindings or and external binding
     *            should be used
     */
    public void setInternalBindings(final boolean internalBindings) {
        _internalBindings = internalBindings;
    }
}
