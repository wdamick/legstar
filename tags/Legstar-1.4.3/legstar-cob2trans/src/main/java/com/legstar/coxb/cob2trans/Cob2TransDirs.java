package com.legstar.coxb.cob2trans;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

/**
 * Set of target folders for Transformers generation.
 * 
 */
public class Cob2TransDirs {

    /** XML Schema folder. */
    private File _xsdDir;

    /** JAVA Sources folder. */
    private File _srcDir;

    /** JAVA binaries folder. */
    private File _binDir;

    /** Distribution archives folder. */
    private File _distDir;

    /**
     * Creates the set of folders if they don't exist yet.
     * 
     * @throws IOException if creation fails
     */
    public void create() throws IOException {
        FileUtils.forceMkdir(_xsdDir);
        FileUtils.forceMkdir(_srcDir);
        FileUtils.forceMkdir(_binDir);
        FileUtils.forceMkdir(_distDir);
    }

    /**
     * Delete any previous content from folders.
     * 
     * @throws IOException if cleanup fails
     */
    public void clean() throws IOException {
        FileUtils.deleteQuietly(_xsdDir);
        FileUtils.deleteQuietly(_srcDir);
        FileUtils.deleteQuietly(_binDir);
        FileUtils.deleteQuietly(_distDir);
        create();

    }

    /**
     * @return the XML Schema folder
     */
    public File getXsdDir() {
        return _xsdDir;
    }

    /**
     * @param xsdDir the XML Schema folder to set
     */
    public void setXsdDir(final File xsdDir) {
        _xsdDir = xsdDir;
    }

    /**
     * @return the JAVA Sources folder
     */
    public File getSrcDir() {
        return _srcDir;
    }

    /**
     * @param srcDir the JAVA Sources folder to set
     */
    public void setSrcDir(final File srcDir) {
        _srcDir = srcDir;
    }

    /**
     * @return the JAVA binaries folder
     */
    public File getBinDir() {
        return _binDir;
    }

    /**
     * @param binDir the JAVA binaries folder to set
     */
    public void setBinDir(final File binDir) {
        _binDir = binDir;
    }

    /**
     * @return the Distribution archives folder
     */
    public File getDistDir() {
        return _distDir;
    }

    /**
     * @param distDir the Distribution archives folder to set
     */
    public void setDistDir(final File distDir) {
        _distDir = distDir;
    }

}
