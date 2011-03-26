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
package com.legstar.host.invoke.model;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONStringer;

import com.legstar.config.Constants;

/**
 * Provides the properties of a target host program to be invoked by a LegStar
 * adapter.
 */
public class HostProgram {

    /** The host program name. */
    private String _name;

    /** The size of the commarea. */
    private int _maxDataLength;

    /** The size of the input data. */
    private int _dataLength;

    /** The remote CICS ID. */
    private String _sysID;

    /** Syncpoint forced on return. */
    private Boolean _syncOnReturn;

    /** The remote CICS transaction ID to use. */
    private String _transID;

    /** The CICS Channel used to link to this program. */
    private String _channelName;

    /** The list of input containers names and their max host byte size. */
    private List < HostContainer > _inputContainer = new ArrayList < HostContainer >();

    /** The list of output containers names and their max host byte size. */
    private List < HostContainer > _outputContainers = new ArrayList < HostContainer >();

    /**
     * The largest commarea size supported.
     * 
     * @return returns the largest commarea size supported
     */
    public int getMaxDataLength() {
        return _maxDataLength;
    }

    /**
     * The largest commarea size supported.
     * 
     * @param maxDataLength the largest commarea size supported
     */
    public void setMaxDataLength(final int maxDataLength) {
        _maxDataLength = maxDataLength;
    }

    /**
     * @deprecated replaced by {@link getMaxDataLength()}
     * @return Returns the size of the commarea.
     */
    public int getLength() {
        return _maxDataLength;
    }

    /**
     * @deprecated replaced by {@link setMaxDataLength()}
     * @param length the size of the commarea.
     */
    public void setLength(final int length) {
        _maxDataLength = length;
    }

    /**
     * The size of the input data.
     * 
     * @return the size of the input data
     */
    public int getDataLength() {
        return _dataLength;
    }

    /**
     * The size of the input data.
     * 
     * @param dataLength the size of the input data
     */
    public void setDataLength(final int dataLength) {
        _dataLength = dataLength;
    }

    /**
     * The host program name.
     * 
     * @return the host program name
     */
    public String getName() {
        return _name;
    }

    /**
     * The host program name.
     * 
     * @param name host program name
     */
    public void setName(final String name) {
        _name = name;
    }

    /**
     * True if CICS should automatically syncpoint on each call.
     * 
     * @return true if CICS should automatically syncpoint on each call
     */
    public Boolean getSyncOnReturn() {
        return _syncOnReturn;
    }

    /**
     * True if CICS should automatically syncpoint on each call.
     * 
     * @param syncOnReturn true if CICS should automatically syncpoint on each
     *            call
     */
    public void setSyncOnReturn(final boolean syncOnReturn) {
        _syncOnReturn = syncOnReturn;
    }

    /**
     * True if CICS should automatically syncpoint on each call.
     * 
     * @return true if CICS should automatically syncpoint on each call
     */
    public boolean isSyncOnReturn() {
        return _syncOnReturn;
    }

    /**
     * The remote CICS ID.
     * 
     * @return the remote CICS ID
     */
    public String getSysID() {
        return _sysID;
    }

    /**
     * The remote CICS ID.
     * 
     * @param sysID remote CICS ID
     */
    public void setSysID(final String sysID) {
        _sysID = sysID;
    }

    /**
     * The remote CICS transaction ID to use.
     * 
     * @return the remote CICS transaction ID to use
     */
    public String getTransID() {
        return _transID;
    }

    /**
     * The remote CICS transaction ID to use.
     * 
     * @param transID remote CICS transaction ID to use
     */
    public void setTransID(final String transID) {
        _transID = transID;
    }

    /**
     * The CICS Channel name.
     * 
     * @return the CICS Channel
     */
    public String getChannelName() {
        return _channelName;
    }

    /**
     * The CICS Channel name.
     * 
     * @param channelName the CICS Channel to set
     */
    public void setChannelName(final String channelName) {
        _channelName = channelName;
    }

    /**
     * @deprecated replaced by {@link getChannelName()}
     * @return the CICS Channel
     */
    public String getChannel() {
        return _channelName;
    }

    /**
     * @deprecated replaced by {@link setChannelName()}
     * @param channel the CICS Channel to set
     */
    public void setChannel(final String channel) {
        _channelName = channel;
    }

    /**
     * @deprecated replaced by {@link getInputContainers()}
     * @return the input Containers list
     */
    public List < HostContainer > getInContainers() {
        return _inputContainer;
    }

    /**
     * @deprecated replaced by {@link setInputContainers()}
     * @param inContainers the input Containers list to set
     */
    public void setInContainers(final List < HostContainer > inContainers) {
        _inputContainer = inContainers;
    }

    /**
     * The CICS input Containers list.
     * 
     * @return the input Containers list
     */
    public List < HostContainer > getInputContainers() {
        return _inputContainer;
    }

    /**
     * The CICS input Containers list.
     * 
     * @param inputContainers the input Containers list to set
     */
    public void setInputContainers(final List < HostContainer > inputContainers) {
        _inputContainer = inputContainers;
    }

    /**
     * Add a CICS input container.
     * 
     * @param container the new container to add
     */
    public void addInputContainer(final HostContainer container) {
        _inputContainer.add(container);
    }

    /**
     * The CICS output Containers list.
     * 
     * @return the output Containers list
     */
    public List < HostContainer > getOutputContainers() {
        return _outputContainers;
    }

    /**
     * The CICS output Containers list.
     * 
     * @param outputContainers the output Containers list to set
     */
    public void setOutputContainers(
            final List < HostContainer > outputContainers) {
        _outputContainers = outputContainers;
    }

    /**
     * Add a CICS output container.
     * 
     * @param container the new container to add
     */
    public void addOutputContainer(final HostContainer container) {
        _outputContainers.add(container);
    }

    /**
     * @deprecated replaced by {@link getOutputContainers()}
     * @return the output Containers list
     */
    public List < HostContainer > getOutContainers() {
        return _outputContainers;
    }

    /**
     * @deprecated replaced by {@link setOutputContainers()}
     * @param outContainers the output Containers list to set
     */
    public void setOutContainers(final List < HostContainer > outContainers) {
        _outputContainers = outContainers;
    }

    /**
     * Perform sanity check on attributes.
     * 
     * @throws HostProgramException if attributes are inconsistent
     */
    public void check() throws HostProgramException {
        if (getName() == null || getName().length() == 0) {
            throw new HostProgramException("Program name must be specified.");
        }
        if (getDataLength() > getMaxDataLength()) {
            throw new HostProgramException("Data length cannot exceed length.");
        }
    }

    /**
     * @return true if this program uses channel/containers
     */
    public boolean hasChannel() {
        return (getChannelName() != null && getChannelName().length() > 0);
    }

    /**
     * Creates a valid JSON representation of this object.
     * 
     * @return a string serialization of the JSON object
     * @see java.lang.Object#toString()
     */
    public String toString() {
        try {
            JSONStringer stringer = new JSONStringer();
            stringer.object();
            stringer.key(Constants.CICS_PROGRAM_NAME_KEY);
            stringer.value(getName());
            if (hasChannel()) {
                stringer.key(Constants.CICS_CHANNEL_KEY);
                stringer.value(getChannelName());
                if (getInputContainers() != null
                        && getInputContainers().size() > 0) {
                    stringer.key(Constants.CICS_IN_CONTAINERS_KEY);
                    stringer.value(toJSON(getInputContainers()));
                }
                if (getOutputContainers() != null
                        && getOutputContainers().size() > 0) {
                    stringer.key(Constants.CICS_OUT_CONTAINERS_KEY);
                    stringer.value(toJSON(getOutputContainers()));
                }
            } else {
                stringer.key(Constants.CICS_LENGTH_KEY);
                stringer.value(getMaxDataLength());
                stringer.key(Constants.CICS_DATALEN_KEY);
                stringer.value(getDataLength());
            }
            if (getSysID() != null) {
                stringer.key(Constants.CICS_SYSID_KEY);
                stringer.value(getSysID());
            }
            if (getSyncOnReturn() != null) {
                stringer.key(Constants.CICS_SYNCONRET_KEY);
                stringer.value(getSyncOnReturn());
            }
            if (getTransID() != null) {
                stringer.key(Constants.CICS_TRANSID_KEY);
                stringer.value(getTransID());
            }
            stringer.endObject();
            return stringer.toString();
        } catch (JSONException e) {
            return "Unable to string object. Got " + e;
        }
    }

    /**
     * Host program properties are sent to the host as a JSON serialization
     * converted to a basic host character set.
     * <p/>
     * Mainframe programs in charge of reading that JSON serialization are not
     * full fledged JSON parser so we simplify things here.
     * 
     * @return a JSON serialization in host character set
     * @throws HostProgramException if something is wrong with the attributes
     */
    public String toJSONHost() throws HostProgramException {
        try {
            JSONStringer stringer = new JSONStringer();
            stringer.object();
            stringer.key(Constants.CICS_PROGRAM_NAME_KEY);
            stringer.value(getName());
            if (hasChannel()) {
                stringer.key(Constants.CICS_CHANNEL_KEY);
                stringer.value(getChannelName());
                /*
                 * Host has no interest in input containers (there is enough
                 * info in the message data parts). It also has no interest in
                 * output containers maximum length.
                 */
                if (getOutputContainers() != null
                        && getOutputContainers().size() > 0) {
                    stringer.key(Constants.CICS_OUT_CONTAINERS_KEY);
                    stringer.value(toJSONNames(getOutputContainers()));
                }
            } else {
                stringer.key(Constants.CICS_LENGTH_KEY);
                /* Host is not expecting int types, only strings */
                stringer.value(Integer.toString(getMaxDataLength()));
                stringer.key(Constants.CICS_DATALEN_KEY);
                stringer.value(Integer.toString(getDataLength()));
            }
            if (getSysID() != null) {
                stringer.key(Constants.CICS_SYSID_KEY);
                stringer.value(getSysID());
            }
            if (getSyncOnReturn() != null) {
                stringer.key(Constants.CICS_SYNCONRET_KEY);
                /*
                 * Host is not expecting boolean types like "true" or "false",
                 * rather is expects "1" for true and "0" for false
                 */
                stringer.value(isSyncOnReturn() ? "1" : "0");
            }
            if (getTransID() != null) {
                stringer.key(Constants.CICS_TRANSID_KEY);
                stringer.value(getTransID());
            }
            stringer.endObject();
            return stringer.toString();
        } catch (JSONException e) {
            throw new HostProgramException(e);
        }
    }

    /**
     * @param containers the list of host containers
     * @return a JSON array where items are JSON serializations of containers
     * @throws JSONException if JSON failure
     */
    private JSONArray toJSON(final List < HostContainer > containers)
            throws JSONException {
        JSONArray jContainers = new JSONArray();
        for (HostContainer container : containers) {
            jContainers.put(container.toJSON());
        }
        return jContainers;
    }

    /**
     * @param containers the list of host containers
     * @return a JSON array where items are containers names
     * @throws JSONException if JSON failure
     */
    private JSONArray toJSONNames(final List < HostContainer > containers)
            throws JSONException {
        JSONArray jContainers = new JSONArray();
        for (HostContainer container : containers) {
            jContainers.put(container.getName());
        }
        return jContainers;
    }
}
