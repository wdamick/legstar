package com.legstar.host.invoke.model;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONStringer;

import com.legstar.config.Constants;

/**
 * Provides the properties of a target host program to be invoked by a
 * LegStar adapter.
 */
public class HostProgram {

    /** The host program name. */
    private String _name;

    /** The size of the commarea. */
    private int _length;

    /** The size of the input data. */
    private int _dataLength;

    /** The remote CICS ID. */
    private String _sysID;

    /** Syncpoint forced on return. */
    private Boolean _syncOnReturn;

    /** The remote CICS transaction ID to use. */
    private String _transID;

    /** The CICS Channel used to link to this program. */
    private String _channel;

    /** The list of input containers names and their max host byte size. */
    private List < HostContainer > _inContainers = new ArrayList < HostContainer >();

    /** The list of output containers names and their max host byte size. */
    private List < HostContainer > _outContainers = new ArrayList < HostContainer >();

    /**
     * @return Returns the size of the commarea.
     */
    public int getLength() {
        return _length;
    }

    /**
     * @param length the size of the commarea.
     */
    public void setLength(final int length) {
        _length = length;
    }

    /**
     * @return Returns the size of the input data.
     */
    public int getDataLength() {
        return _dataLength;
    }

    /**
     * @param dataLength the size of the input data.
     */
    public void setDataLength(final int dataLength) {
        _dataLength = dataLength;
    }

    /**
     * @return Returns the host program name.
     */
    public String getName() {
        return _name;
    }

    /**
     * @param name host program name.
     */
    public void setName(final String name) {
        _name = name;
    }

    /**
     * @return Returns the Syncpoint forced on return mode.
     */
    public Boolean getSyncOnReturn() {
        return _syncOnReturn;
    }

    /**
     * @param syncOnReturn Syncpoint forced on return mode.
     */
    public void setSyncOnReturn(final boolean syncOnReturn) {
        _syncOnReturn = syncOnReturn;
    }

    /**
     * @return Returns the remote CICS ID.
     */
    public String getSysID() {
        return _sysID;
    }

    /**
     * @param sysID remote CICS ID.
     */
    public void setSysID(final String sysID) {
        _sysID = sysID;
    }

    /**
     * @return Returns the remote CICS transaction ID to use.
     */
    public String getTransID() {
        return _transID;
    }

    /**
     * @param transID remote CICS transaction ID to use.
     */
    public void setTransID(final String transID) {
        _transID = transID;
    }

    /**
     * @return the CICS Channel
     */
    public String getChannel() {
        return _channel;
    }

    /**
     * @param channel the CICS Channel to set
     */
    public void setChannel(final String channel) {
        _channel = channel;
    }

    /**
     * @return the input Containers list
     */
    public List < HostContainer > getInContainers() {
        return _inContainers;
    }

    /**
     * @param inContainers the input Containers list to set
     */
    public void setInContainers(
            final List < HostContainer > inContainers) {
        _inContainers = inContainers;
    }
    /**
     * @return the output Containers list
     */
    public List < HostContainer > getOutContainers() {
        return _outContainers;
    }

    /**
     * @param outContainers the output Containers list to set
     */
    public void setOutContainers(
            final List < HostContainer > outContainers) {
        _outContainers = outContainers;
    }

    /**
     * Perform sanity check on attributes.
     * @throws HostProgramException if attributes are inconsistent
     */
    public void check() throws HostProgramException {
        if (getName() == null || getName().length() == 0) {
            throw new HostProgramException("Program name must be specified.");
        }
        if (getDataLength() > getLength()) {
            throw new HostProgramException("Data length cannot exceed length.");
        }
    }

    /**
     * @return true if this program uses channel/containers
     */
    public boolean hasChannel() {
        return (getChannel() != null && getChannel().length() > 0);
    }

    /**
     * Creates a valid JSON representation of this object.
     * @return  a string serialization of the JSON object
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
                stringer.value(getChannel());
                if (getInContainers() != null && getInContainers().size() > 0) {
                    stringer.key(Constants.CICS_IN_CONTAINERS_KEY);
                    stringer.value(toJSON(getInContainers()));
                }
                if (getOutContainers() != null && getOutContainers().size() > 0) {
                    stringer.key(Constants.CICS_OUT_CONTAINERS_KEY);
                    stringer.value(toJSON(getOutContainers()));
                }
            } else {
                stringer.key(Constants.CICS_LENGTH_KEY);
                stringer.value(getLength());
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
     * Mainframe programs in charge of reading that JSON serialization are
     * not full fledged JSON parser so we simplify things here.
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
                stringer.value(getChannel());
                /* Host has no interest in input containers (there is enough info
                 * in the message data parts). It also has no interest in output
                 * containers maximum length. */
                if (getOutContainers() != null && getOutContainers().size() > 0) {
                    stringer.key(Constants.CICS_OUT_CONTAINERS_KEY);
                    stringer.value(toJSONNames(getOutContainers()));
                }
            } else {
                stringer.key(Constants.CICS_LENGTH_KEY);
                /* Host is not expecting int types, only strings*/
                stringer.value(Integer.toString(getLength()));
                stringer.key(Constants.CICS_DATALEN_KEY);
                stringer.value(Integer.toString(getDataLength()));
            }
            if (getSysID() != null) {
                stringer.key(Constants.CICS_SYSID_KEY);
                stringer.value(getSysID());
            }
            if (getSyncOnReturn() != null) {
                stringer.key(Constants.CICS_SYNCONRET_KEY);
                /* Host is not expecting boolean types, only strings*/
                stringer.value(getSyncOnReturn().toString());
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
    private JSONArray toJSON(
            final List < HostContainer > containers) throws JSONException {
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
    private JSONArray toJSONNames(
            final List < HostContainer > containers) throws JSONException {
        JSONArray jContainers = new JSONArray();
        for (HostContainer container : containers) {
            jContainers.put(container.getName());
        }
        return jContainers;
    }
}
