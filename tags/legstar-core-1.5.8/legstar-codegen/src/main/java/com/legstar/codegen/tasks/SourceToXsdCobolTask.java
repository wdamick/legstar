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
package com.legstar.codegen.tasks;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.StringTokenizer;
import java.util.Vector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.codegen.models.SourceToXsdCobolModel;

/**
 * This abstract ant task is used by derived generates which generate
 * an XML schema with COBOL annotation from different type of sources.
 */
public abstract class SourceToXsdCobolTask extends Task {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Groups all the data needed to generate an annotated XML schema. */
    private SourceToXsdCobolModel mModel;

    /**
     * Checks that common properties set are valid.
     * 
     * @param xsdFileNameMandatory where an xsd file name is mandatory
     * @param namespaceMandatory where a target namespace is mandatory
     */
    public void checkInput(
            final boolean xsdFileNameMandatory,
            final boolean namespaceMandatory) {

        if (_log.isDebugEnabled()) {
            _log.debug("checkInput started");
            _log.debug("   Target namespace name    = " + getNamespace());
            _log.debug("   Target directory         = " + getTargetDir());
            _log.debug("   Target Xsd file name     = "
                    + getTargetXsdFileName());
        }

        if (getModel() == null) {
            throw (new BuildException("You must specify a model"));
        }

        /* Check that we have a valid target directory. */
        if (getTargetDir() == null) {
            throw (new BuildException(
                    "You must provide a target directory"));
        }
        if (!getTargetDir().exists()) {
            throw (new BuildException(
                    "Directory " + getTargetDir() + " does not exist"));
        }
        if (!getTargetDir().isDirectory() || !getTargetDir().canWrite()) {
            throw (new BuildException(
                    getTargetDir() + " is not a directory or is not writable"));
        }

        /* Set a valid target annotated XSD file name */
        if (xsdFileNameMandatory) {
            if (getTargetXsdFileName() == null
                    || getTargetXsdFileName().length() == 0) {
                throw (new BuildException(
                        "You must provide a target xsd file name"));
            }
            if (getTargetXsdFileName().contains(File.separator)
                    || getTargetXsdFileName().contains("/")) {
                throw (new BuildException(
                        "Xsd file name should not specify a path (use targetDir for path)"));
            }
        }

        if (namespaceMandatory) {
            if (getNamespace() == null || getNamespace().length() == 0) {
                throw (new BuildException(
                        "You must specify an output XML schema namespace"));
            }
        }

        /*
         * If we have a namespace check it.
         */
        if (getNamespace() != null && getNamespace().length() > 0) {
            try {
                URI nURI = new URI(getNamespace());
                if (nURI.isOpaque()) {
                    throw (new BuildException(
                            "Namespace " + getNamespace()
                                    + " is not a hierarchical URI"));
                }
            } catch (URISyntaxException e) {
                throw new BuildException(e);
            }
        }

    }

    /**
     * Converts a URI into a package name. We assume a hierarchical,
     * server-based URI with the following syntax:
     * [scheme:][//host[:port]][path][?query][#fragment]
     * The package name is derived from host, path and fragment.
     * 
     * @param namespaceURI the input namespace URI
     * @return the result package name
     */
    public static String packageFromURI(final URI namespaceURI) {

        StringBuilder result = new StringBuilder();
        URI nURI = namespaceURI.normalize();
        boolean firstToken = true;

        /*
         * First part of package name is built from host with tokens in
         * reverse order.
         */
        if (nURI.getHost() != null && nURI.getHost().length() != 0) {
            Vector < String > v = new Vector < String >();
            StringTokenizer t = new StringTokenizer(nURI.getHost(), ".");
            while (t.hasMoreTokens()) {
                v.addElement(t.nextToken());
            }

            for (int i = v.size(); i > 0; i--) {
                if (!firstToken) {
                    result.append('.');
                } else {
                    firstToken = false;
                }
                result.append(v.get(i - 1));
            }
        }

        /* Next part of package is built from the path tokens */
        if (nURI.getPath() != null && nURI.getPath().length() != 0) {
            Vector < String > v = new Vector < String >();
            StringTokenizer t = new StringTokenizer(nURI.getPath(), "/");
            while (t.hasMoreTokens()) {
                v.addElement(t.nextToken());
            }

            for (int i = 0; i < v.size(); i++) {
                String token = v.get(i);
                /* ignore situations such as /./../ */
                if (token.equals(".") || token.equals("..")) {
                    continue;
                }
                if (!firstToken) {
                    result.append('.');
                } else {
                    firstToken = false;
                }
                result.append(v.get(i));
            }
        }

        /* Finally append any fragment */
        if (nURI.getFragment() != null && nURI.getFragment().length() != 0) {
            if (!firstToken) {
                result.append('.');
            } else {
                firstToken = false;
            }
            result.append(nURI.getFragment());
        }

        /*
         * By convention, namespaces are lowercase and should not contain
         * invalid Java identifiers
         */
        String s = result.toString().toLowerCase();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            Character c = s.charAt(i);
            if (Character.isJavaIdentifierPart(c) || c.equals('.')) {
                sb.append(c);
            } else {
                sb.append("_");
            }
        }
        return sb.toString();
    }

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return getModel().getTargetDir();
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        getModel().setTargetDir(targetDir);
    }

    /**
     * @return the target annotated XSD file name
     */
    public String getTargetXsdFileName() {
        return getModel().getTargetXsdFileName();
    }

    /**
     * @param targetXsdFileName the target annotated XSD file name to set
     */
    public void setTargetXsdFileName(final String targetXsdFileName) {
        getModel().setTargetXsdFileName(targetXsdFileName);
    }

    /**
     * @return the target schema namespace
     */
    public String getNamespace() {
        return getModel().getNamespace();
    }

    /**
     * @param namespace the target schema namespace to set
     */
    public void setNamespace(final String namespace) {
        getModel().setNamespace(namespace);
    }

    /**
     * @return the generation model
     */
    public SourceToXsdCobolModel getModel() {
        return mModel;
    }

    /**
     * @param model the generation model to set
     */
    public void setModel(final SourceToXsdCobolModel model) {
        mModel = model;
    }
}
