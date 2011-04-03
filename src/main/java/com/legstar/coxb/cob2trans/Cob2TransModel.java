package com.legstar.coxb.cob2trans;

import java.util.Properties;

import com.legstar.cob2xsd.Cob2XsdModel;
import com.legstar.codegen.models.AbstractPropertiesModel;
import com.legstar.coxb.gen.CoxbGenModel;

/**
 * Parameter set for a Transformers generation.
 * 
 */
public class Cob2TransModel extends AbstractPropertiesModel {

    /* ====================================================================== */
    /* Following are default field values. = */
    /* ====================================================================== */

    /** Default name for generated XSD target folder. */
    public static final String DEFAULT_XSD_FOLDER_NAME = "xsd";

    /** Default name for generated JAVA sources target folder. */
    public static final String DEFAULT_SRC_FOLDER_NAME = "src";

    /** Default name for generated JAVA binaries target folder. */
    public static final String DEFAULT_BIN_FOLDER_NAME = "bin";

    /** Default name for generated distribution archives target folder. */
    public static final String DEFAULT_DIST_FOLDER_NAME = "dist";

    /** Default behavior: don't clean folders before generation. */
    public static final boolean DEFAULT_CLEAN_FOLDERS = false;

    /* ====================================================================== */
    /* Following are key identifiers for this model persistence. = */
    /* ====================================================================== */

    /** Physical location of the XML schema. */
    public static final String XSD_FOLDER_NAME = "xsdFolderName";

    /** Physical location of the generated sources. */
    public static final String SRC_FOLDER_NAME = "srcFolderName";

    /** Physical location of the generated binaries. */
    public static final String BIN_FOLDER_NAME = "binFolderName";

    /** Physical location of the distribution archive. */
    public static final String DIST_FOLDER_NAME = "distFolderName";

    /** Whether to clean the output folders before generation. */
    public static final String CLEAN_FOLDERS = "cleanFolders";

    /* ====================================================================== */
    /* Following are this class fields that are persistent. = */
    /* ====================================================================== */

    /** Storage for generated XSDs. */
    private String _xsdFolderName = DEFAULT_XSD_FOLDER_NAME;

    /** Storage for generated JAVA sources. */
    private String _srcFolderName = DEFAULT_SRC_FOLDER_NAME;

    /** Storage for generated JAVA binaries. */
    private String _binFolderName = DEFAULT_BIN_FOLDER_NAME;

    /** Storage for generated distribution archives. */
    private String _distFolderName = DEFAULT_DIST_FOLDER_NAME;

    /** Whether to clean target folders before generation. */
    private boolean _cleanFolders = DEFAULT_CLEAN_FOLDERS;

    /** COBOL to XSD parameter set. */
    private Cob2XsdModel _cob2XsdModel;

    /** COXB generator parameter set. */
    private CoxbGenModel _coxbgenModel;

    /**
     * A no-Arg constructor.
     */
    public Cob2TransModel() {
        _cob2XsdModel = new Cob2XsdModel();
        _coxbgenModel = new CoxbGenModel();
    }

    /**
     * Construct from a properties file.
     * 
     * @param props the property file
     */
    public Cob2TransModel(final Properties props) {
        setXsdFolderName(getString(props, XSD_FOLDER_NAME,
                DEFAULT_XSD_FOLDER_NAME));
        setSrcFolderName(getString(props, SRC_FOLDER_NAME,
                DEFAULT_SRC_FOLDER_NAME));
        setBinFolderName(getString(props, BIN_FOLDER_NAME,
                DEFAULT_BIN_FOLDER_NAME));
        setDistFolderName(getString(props, DIST_FOLDER_NAME,
                DEFAULT_DIST_FOLDER_NAME));
        setCleanFolders(getBoolean(props, CLEAN_FOLDERS, DEFAULT_CLEAN_FOLDERS));
        _cob2XsdModel = new Cob2XsdModel(props);
        _coxbgenModel = new CoxbGenModel(props);
    }

    /**
     * @return the COBOL to XSD options set
     */
    public Cob2XsdModel getCob2XsdModel() {
        return _cob2XsdModel;
    }

    /**
     * @param cob2XsdModel the COBOL to XSD options set
     */
    public void setCob2XsdModel(final Cob2XsdModel cob2XsdModel) {
        _cob2XsdModel = cob2XsdModel;
    }

    /**
     * @return the COXB generator options set
     */
    public CoxbGenModel getCoxbGenModel() {
        return _coxbgenModel;
    }

    /**
     * @param coxbgenModel the COXB generator options set
     */
    public void setCoxbGenModel(final CoxbGenModel coxbgenModel) {
        _coxbgenModel = coxbgenModel;
    }

    /**
     * @return the storage for generated XSDs
     */
    public String getXsdFolderName() {
        return _xsdFolderName;
    }

    /**
     * @param xsdFolderName the storage for generated XSDs to set
     */
    public void setXsdFolderName(final String xsdFolderName) {
        _xsdFolderName = xsdFolderName;
    }

    /**
     * @return the storage for generated JAVA sources
     */
    public String getSrcFolderName() {
        return _srcFolderName;
    }

    /**
     * @param srcFolderName the storage for generated JAVA sources to set
     */
    public void setSrcFolderName(final String srcFolderName) {
        _srcFolderName = srcFolderName;
    }

    /**
     * @return the storage for generated JAVA binaries
     */
    public String getBinFolderName() {
        return _binFolderName;
    }

    /**
     * @param binFolderName the storage for generated JAVA binaries to set
     */
    public void setBinFolderName(final String binFolderName) {
        _binFolderName = binFolderName;
    }

    /**
     * @return the storage for generated distribution archives
     */
    public String getDistFolderName() {
        return _distFolderName;
    }

    /**
     * @param distFolderName the storage for generated distribution archives to
     *            set
     */
    public void setDistFolderName(final String distFolderName) {
        _distFolderName = distFolderName;
    }

    /**
     * @return whether to clean target folders before generation
     */
    public boolean isCleanFolders() {
        return _cleanFolders;
    }

    /**
     * @param cleanFolders whether to clean target folders before generation
     */
    public void setCleanFolders(final boolean cleanFolders) {
        _cleanFolders = cleanFolders;
    }

    /**
     * @return a properties file holding the values of this object fields
     */
    public Properties toProperties() {
        Properties props = super.toProperties();
        if (getXsdFolderName() != null) {
            putString(props, XSD_FOLDER_NAME, getXsdFolderName());
        }
        if (getSrcFolderName() != null) {
            putString(props, SRC_FOLDER_NAME, getSrcFolderName());
        }
        if (getBinFolderName() != null) {
            putString(props, BIN_FOLDER_NAME, getBinFolderName());
        }
        if (getDistFolderName() != null) {
            putString(props, DIST_FOLDER_NAME, getDistFolderName());
        }
        putBoolean(props, CLEAN_FOLDERS, isCleanFolders());

        props.putAll(getCob2XsdModel().toProperties());
        props.putAll(getCoxbGenModel().toProperties());

        return props;
    }

}
