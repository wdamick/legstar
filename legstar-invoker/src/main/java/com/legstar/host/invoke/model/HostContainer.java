package com.legstar.host.invoke.model;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;

/**
 * Provides the properties of a host container. Containers are named and are similar to blobs
 * (their content is untyped).
 */
public class HostContainer {

    /** JSON identifier for container name.*/
    public static final String CONTAINER_NAME_LABEL = "containerName";

    /** JSON identifier for container size.*/
    public static final String CONTAINER_SIZE_LABEL = "containerLength";

    /** The container name. */
    private String _name;

    /** The container data size. */
    private int _length;

    /**
     * @return the container name.
     */
    public final String getName() {
        return _name;
    }

    /**
     * @param name the container name to set.
     */
    public final void setName(final String name) {
        _name = name;
    }

    /**
     * @return the container data size.
     */
    public final int getLength() {
        return _length;
    }

    /**
     * @param length the container data size to set.
     */
    public final void setLength(final int length) {
        _length = length;
    }

    /**
     * Creates a valid JSON representation of this object.
     * @return  a string serialization of the JSON object
     * @see java.lang.Object#toString()
     */
    public String toString() {
        try {
            JSONStringer stringer = new JSONStringer();
            stringer.object()
                .key(CONTAINER_NAME_LABEL)
                .value(getName())
                .key(CONTAINER_SIZE_LABEL)
                .value(getLength())
            .endObject();
            return stringer.toString();
        } catch (JSONException e) {
            return "Unable to string object. Got " + e;
        }
    }
    
    /**
     * @return this instance as a JSON object
     * @throws JSONException if JSONification fails
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject o = new JSONObject();
        o.put(CONTAINER_NAME_LABEL, getName());
        o.put(CONTAINER_SIZE_LABEL, getLength());
        return o;
    }

}
