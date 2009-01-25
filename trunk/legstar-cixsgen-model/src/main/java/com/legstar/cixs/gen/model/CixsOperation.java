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
package com.legstar.cixs.gen.model;
import java.util.ArrayList;
import java.util.List;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.legstar.codegen.CodeGenUtil;

/**
 * This class describes a service operation and its binding to JAXB and CICS.
 */
public class CixsOperation {

    /** Operation name. */
    private String mName;

    /** Host program name. */
    private String mCicsProgramName;

    /** CICS Channel name. */
    private String mCicsChannel;

    /** Operation-related classes package name. */
    private String mPackageName;

    /** Class name for request wrapper. */
    private String mRequestWrapperType;

    /** Class name for response wrapper. */
    private String mResponseWrapperType;

    /** Class name for request holder (groups multiple input parts). */
    private String mRequestHolderType;

    /** Class name for response holder (groups multiple output parts). */
    private String mResponseHolderType;

    /** Class name for faults. */
    private String mFaultType;

    /** Class name for fault descriptors. */
    private String mFaultInfoType;

    /** Input structures. */
    private List < CixsStructure > mInput =
        new ArrayList < CixsStructure >();

    /** Output structures. */
    private List < CixsStructure > mOutput =
        new ArrayList < CixsStructure >();

    /** XML element representing a CIXS operation definition. */
    public static final String CIXS_OPERATION_XML_E = "cixsOperation";

    /** XML attribute representing an operation name. */
    public static final String CIXS_OPERATION_NAME_XML_A = "name";

    /** XML attribute representing a CICS program name. */
    public static final String CIXS_CICS_PROGRAM_NAME_XML_A =
        "cicsProgramName";

    /** XML attribute representing a CICS Channel name. */
    public static final String CIXS_CICS_CHANNEL_XML_A
    = "cicsChannel";

    /** Operation-related classes package name. */
    public static final String CIXS_OP_PACKAGE_XML_A
    = "packageName";

    /** Request wrapper class name. */
    public static final String CIXS_OP_REQ_WRAPPER_XML_A
    = "requestWrapperType";

    /** Response wrapper class name. */
    public static final String CIXS_OP_RES_WRAPPER_XML_A
    = "responseWrapperType";

    /** Request holder class name. */
    public static final String CIXS_OP_REQ_HOLDER_XML_A
    = "requestHolderType";

    /** Response holder class name. */
    public static final String CIXS_OP_RES_HOLDER_XML_A
    = "responseHolderType";

    /** Fault wrapper class name. */
    public static final String CIXS_OP_FAULT_XML_A
    = "faultType";

    /** Fault descriptor class name. */
    public static final String CIXS_OP_FAULT_INFO_XML_A
    = "faultInfoType";

    /** XML element representing an input structure. */
    public static final String CIXS_INPUT_STRUCTURE_XML_E
    = "input";

    /** XML element representing an output structure. */
    public static final String CIXS_OUTPUT_STRUCTURE_XML_E
    = "output";

    /** Default suffix for exception class names. */
    private static final String DEFAULT_FAULT_SUFFIX = "Fault";

    /** Default suffix for exception info class names. */
    private static final String DEFAULT_FAULT_INFO_SUFFIX = "FaultInfo";

    /** Default suffix for request wrapper class names. */
    private static final String DEFAULT_REQUEST_WRAPPER_SUFFIX = "Request";

    /** Default suffix for response wrapper class names. */
    private static final String DEFAULT_RESPONSE_WRAPPER_SUFFIX = "Response";

    /** Default suffix for multi-input request holder class names. */
    private static final String DEFAULT_REQUEST_HOLDER_SUFFIX = "RequestHolder";

    /** Default suffix for multi-input response holder class names. */
    private static final String DEFAULT_RESPONSE_HOLDER_SUFFIX =
        "ResponseHolder";

    /**
     * @return the service operation name
     */
    public final String getName() {
        return mName;
    }

    /**
     * @param name the service operation name to set
     */
    public final void setName(final String name) {
        mName = name;
    }

    /**
     * @return the host program name
     */
    public final String getCicsProgramName() {
        return mCicsProgramName;
    }

    /**
     * @param cicsProgramName the host program name to set
     */
    public final void setCicsProgramName(final String cicsProgramName) {
        mCicsProgramName = cicsProgramName;
    }

    /**
     * @return the CICS Channel name
     */
    public final String getCicsChannel() {
        return mCicsChannel;
    }

    /**
     * @param cicsChannel the CICS Channel name to set
     */
    public final void setCicsChannel(final String cicsChannel) {
        mCicsChannel = cicsChannel;
    }

    /**
     * @return the Input structures
     */
    public final List < CixsStructure > getInput() {
        return mInput;
    }

    /**
     * @param input the Input structures to set
     */
    public final void setInput(
            final List < CixsStructure > input) {
        mInput = input;
    }

    /**
     * @param structure the structure to add
     */
    public final void addInput(final CixsStructure structure) {
        mInput.add(structure);
    }

    /**
     * @return the Output structures
     */
    public final List < CixsStructure > getOutput() {
        return mOutput;
    }

    /**
     * @param output the Output structures to set
     */
    public final void setOutput(
            final List < CixsStructure > output) {
        mOutput = output;
    }

    /**
     * @param structure the structure to add
     */
    public final void addOutput(final CixsStructure structure) {
        mOutput.add(structure);
    }

    /**
     * Create an XML usable as input for and ant task.
     * @return the XML
     */
    public final String serialize() {
        StringBuffer result = new StringBuffer();
        result.append("<" + CIXS_OPERATION_XML_E + " "
                + CIXS_OPERATION_NAME_XML_A + "=" + '\"'
                + getName() + '\"');
        result.append(" " + CIXS_CICS_PROGRAM_NAME_XML_A + "=" + '\"'
                + getCicsProgramName() + '\"');
        if (getCicsChannel() != null && getCicsChannel().length() > 0) {
            result.append(" " + CIXS_CICS_CHANNEL_XML_A + "=" + '\"'
                    + getCicsChannel() + '\"');
        }
        if (getFaultType() != null && getFaultType().length() > 0) {
            result.append(" " + CIXS_OP_FAULT_XML_A + "=" + '\"'
                    + getFaultType() + '\"');
        }
        if (getFaultInfoType() != null && getFaultInfoType().length() > 0) {
            result.append(" " + CIXS_OP_FAULT_INFO_XML_A + "=" + '\"'
                    + getFaultInfoType() + '\"');
        }
        if (getPackageName() != null && getPackageName().length() > 0) {
            result.append(" " + CIXS_OP_PACKAGE_XML_A + "=" + '\"'
                    + getPackageName() + '\"');
        }
        if (getRequestHolderType() != null
                && getRequestHolderType().length() > 0) {
            result.append(" " + CIXS_OP_REQ_HOLDER_XML_A + "=" + '\"'
                    + getRequestHolderType() + '\"');
        }
        if (getRequestWrapperType() != null
                && getRequestWrapperType().length() > 0) {
            result.append(" " + CIXS_OP_REQ_WRAPPER_XML_A + "=" + '\"'
                    + getRequestWrapperType() + '\"');
        }
        if (getResponseHolderType() != null
                && getResponseHolderType().length() > 0) {
            result.append(" " + CIXS_OP_RES_HOLDER_XML_A + "=" + '\"'
                    + getResponseHolderType() + '\"');
        }
        if (getResponseWrapperType() != null
                && getResponseWrapperType().length() > 0) {
            result.append(" " + CIXS_OP_RES_WRAPPER_XML_A + "=" + '\"'
                    + getResponseWrapperType() + '\"');
        }
        result.append('>' +  CodeGenUtil.CRLF);
        for (CixsStructure structure : getInput()) {
            result.append(structure.serialize(CIXS_INPUT_STRUCTURE_XML_E));
            result.append(CodeGenUtil.CRLF);
        }
        for (CixsStructure structure : getOutput()) {
            result.append(structure.serialize(CIXS_OUTPUT_STRUCTURE_XML_E));
            result.append(CodeGenUtil.CRLF);
        }
        result.append("</" + CIXS_OPERATION_XML_E + ">");
        return result.toString();
    }

    /**
     * Loads the CIXS Operation from an XML node element.
     * @param operationNode the operation node
     * @throws CixsModelException if load fails
     */
    public final void load(final Node operationNode) throws CixsModelException {
        Element operationElement = (Element) operationNode;
        mName = operationElement.getAttribute(CIXS_OPERATION_NAME_XML_A);
        if (mName == null || mName.length() == 0) {
            throw new CixsModelException("Operation must have a name");
        }
        mCicsProgramName = operationElement.getAttribute(
                CIXS_CICS_PROGRAM_NAME_XML_A);
        if (mCicsProgramName == null || mCicsProgramName.length() == 0) {
            throw new CixsModelException(
            "Operation must have an associated program name");
        }
        mCicsChannel =  operationElement.getAttribute(
                CIXS_CICS_CHANNEL_XML_A);
        mPackageName =  operationElement.getAttribute(
                CIXS_OP_PACKAGE_XML_A);
        mRequestWrapperType =  operationElement.getAttribute(
                CIXS_OP_REQ_WRAPPER_XML_A);
        mResponseWrapperType =  operationElement.getAttribute(
                CIXS_OP_RES_WRAPPER_XML_A);
        mRequestHolderType =  operationElement.getAttribute(
                CIXS_OP_REQ_HOLDER_XML_A);
        mResponseHolderType =  operationElement.getAttribute(
                CIXS_OP_RES_HOLDER_XML_A);
        mFaultType =  operationElement.getAttribute(
                CIXS_OP_FAULT_XML_A);
        mFaultInfoType =  operationElement.getAttribute(
                CIXS_OP_FAULT_INFO_XML_A);

        mInput = new ArrayList < CixsStructure >();
        NodeList listOfElements = operationElement.getElementsByTagName(
                CIXS_INPUT_STRUCTURE_XML_E);
        for (int i = 0; i < listOfElements.getLength(); i++) {
            CixsStructure structure = new CixsStructure();
            structure.load(listOfElements.item(i));
            mInput.add(structure);
        }

        mOutput = new ArrayList < CixsStructure >();
        listOfElements = operationElement.getElementsByTagName(
                CIXS_OUTPUT_STRUCTURE_XML_E);
        for (int i = 0; i < listOfElements.getLength(); i++) {
            CixsStructure structure = new CixsStructure();
            structure.load(listOfElements.item(i));
            mOutput.add(structure);
        }
    }

    /**
     * @return this operation property values as a string array. This helps
     * inserting the structure as an item in an array.
     */
    public final String[] getAsStringArray() {
        String[] array = {getName(),
                getCicsProgramName(),
                getCicsChannel(),
                Integer.toString(getInput().size()),
                Integer.toString(getOutput().size())};
        return array;
    }

    /**
     * @see Object#hashCode() 
     * {@inheritDoc}
     */
    public final int hashCode() {
        return getName().hashCode();
    }

    /**
     * Indicates whether some other operation is "equal to" this one.
     *
     * @param obj Object to be compared.
     * @return true if this object is the same as the obj argument; false
     *         otherwise..
     */
    public final boolean equals(final Object obj) {
        return (obj != null) && (obj.getClass() == CixsOperation.class)
        && ((CixsOperation) obj).getName().equals(getName());
    }

    /**
     * Compares this object with the specified object for order. Returns a
     * negative integer, zero, or a positive integer as this object is less
     * than, equal to, or greater than the specified object.
     *
     * @param o Object to be compared.
     * @return A negative integer, zero, or a positive integer as this object
     *         is less than, equal to, or greater than the specified object.
     */
    public final int compareTo(final Object o) {
        if (o.getClass() != CixsOperation.class) {
            throw new ClassCastException(o.getClass().getName());
        } else {
            return ((CixsOperation) o).getName().compareTo(getName());
        }
    }

    /**
     * A convenience method to return a valid class name built from this
     * operation name.
     * @return a valid class name built from the operation name.
     */
    public final String getClassName() {
        return CodeGenUtil.classNormalize(getName());
    }

    /**
     * @return the Class name for faults
     */
    public final String getFaultType() {
        if (mFaultType == null || mFaultType.length() == 0) {
            return getClassName() + DEFAULT_FAULT_SUFFIX;
        }
        return mFaultType;
    }

    /**
     * @param faultType the Class name for faults to set
     */
    public final void setFaultType(final String faultType) {
        mFaultType = faultType;
    }

    /**
     * @return the Class name for fault descriptors
     */
    public final String getFaultInfoType() {
        if (mFaultInfoType == null || mFaultInfoType.length() == 0) {
            return getClassName() + DEFAULT_FAULT_INFO_SUFFIX;
        }
        return mFaultInfoType;
    }

    /**
     * @param faultInfoType the Class name for fault descriptors to set
     */
    public final void setFaultInfoType(final String faultInfoType) {
        mFaultInfoType = faultInfoType;
    }

    /**
     * @return the operation-related classes package name
     */
    public final String getPackageName() {
        return mPackageName;
    }

    /**
     * @param packageName the operation-related classes package name to set
     */
    public final void setPackageName(final String packageName) {
        mPackageName = packageName;
    }

    /**
     * @return the Class name for request holder (groups multiple input parts
     * which happens only if channel/containers are supported)
     * for single input operations this is the input type
     */
    public final String getRequestHolderType() {
        if (mRequestHolderType == null || mRequestHolderType.length() == 0) {
            if (hasChannel()) {
                return getClassName() + DEFAULT_REQUEST_HOLDER_SUFFIX;
            } else {
                if (mInput.size() > 0) {
                    return mInput.get(0).getJaxbType();
                } else {
                    return null;
                }
            }
        }
        return mRequestHolderType;
    }

    /**
     * @param requestHolderType the Class name for request holder (groups
     *  multiple input parts) to set
     */
    public final void setRequestHolderType(final String requestHolderType) {
        mRequestHolderType = requestHolderType;
    }

    /**
     * @return the Class name for request wrapper. If none was specified,
     * build a name using operation class name and a suffix.
     */
    public final String getRequestWrapperType() {
        if (mRequestWrapperType == null || mRequestWrapperType.length() == 0) {
            return getClassName() + DEFAULT_REQUEST_WRAPPER_SUFFIX;
        }
        return mRequestWrapperType;
    }

    /**
     * @param requestWrapperType the Class name for request wrapper to set
     */
    public final void setRequestWrapperType(
            final String requestWrapperType) {
        mRequestWrapperType = requestWrapperType;
    }

    /**
     * @return the Class name for response holder ((groups multiple output parts
     * which happens only if channel/containers are supported)
     */
    public final String getResponseHolderType() {
        if (mResponseHolderType == null || mResponseHolderType.length() == 0) {
            if (hasChannel()) {
                return getClassName() + DEFAULT_RESPONSE_HOLDER_SUFFIX;
            } else {
                if (mOutput.size() > 0) {
                    return mOutput.get(0).getJaxbType();
                } else {
                    return null;
                }
            }
        }
        return mResponseHolderType;
    }

    /**
     * @param responseHolderType the Class name for response holder (groups
     *  multiple output parts) to set
     */
    public final void setResponseHolderType(
            final String responseHolderType) {
        mResponseHolderType = responseHolderType;
    }

    /**
     * @return the Class name for response wrapper
     */
    public final String getResponseWrapperType() {
        if (mResponseWrapperType == null
                || mResponseWrapperType.length() == 0) {
            return getClassName() + DEFAULT_RESPONSE_WRAPPER_SUFFIX;
        }
        return mResponseWrapperType;
    }

    /**
     * @param responseWrapperType the Class name for response wrapper to set
     */
    public final void setResponseWrapperType(
            final String responseWrapperType) {
        mResponseWrapperType = responseWrapperType;
    }

    /**
     * @return true if this operation uses Channel/Containers
     */
    public final boolean hasChannel() {
        return (mCicsChannel != null && mCicsChannel.length() > 0);
    }
}
