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
package com.legstar.coxb.transform;

import java.io.Reader;
import java.io.Writer;

/**
 * A generic class that provides transformer capabilities for a given structure.
 * <p/>
 * A structure maps to an XSD and a COBOL structure. This class does not
 * implement the transformers, it acts as a container.
 * <p/>
 * Classes derived from this one will typically implement a constructor that
 * creates the directional transformers, JSON to host and host to JSON.
 * 
 */
public abstract class AbstractJsonTransformers implements IHostJsonTransformers {

    /** Transformer that turns an JSON into host data. */
    private IJsonToHostTransformer _jsonToHost;

    /** Transformer that turns host data into an JSON. */
    private IHostToJsonTransformer _hostToJson;

    /**
     * No arg constructor. Caller is responsible for setting the internal
     * transformers.
     */
    public AbstractJsonTransformers() {

    }

    /**
     * Creates a provider with its directional transformers.
     * 
     * @param jsonToHost JSON to host transformer
     * @param hostToJson host to JSON transformer
     */
    public AbstractJsonTransformers(
            final IJsonToHostTransformer jsonToHost,
            final IHostToJsonTransformer hostToJson) {
        _jsonToHost = jsonToHost;
        _hostToJson = hostToJson;
    }

    /**
     * @return the transformer that turns a JSON into host data
     */
    public IJsonToHostTransformer getJsonToHost() {
        return _jsonToHost;
    }

    /**
     * @param jsonToHost the transformer that turns an JSON into host data to
     *            set
     */
    public void setJsonToHost(final IJsonToHostTransformer jsonToHost) {
        _jsonToHost = jsonToHost;
    }

    /**
     * @return the transformer that turns host data into an JSON
     */
    public IHostToJsonTransformer getHostToJson() {
        return _hostToJson;
    }

    /**
     * @param hostToJson the transformer that turns host data into an JSON to
     *            set
     */
    public void setHostToJson(final IHostToJsonTransformer hostToJson) {
        _hostToJson = hostToJson;
    }

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Reader reader, final String hostCharset)
            throws HostTransformException {
        return getJsonToHost().transform(reader, hostCharset);
    }

    /**
     * Transforms JSON to host data.
     * 
     * @param reader the JSON reader to unmarshal JSON data from
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Reader reader)
            throws HostTransformException {
        return getJsonToHost().transform(reader);
    }

    /**
     * Transforms host data to JSON with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void toJson(final byte[] hostData, final Writer writer,
            final String hostCharset)
            throws HostTransformException {
        getHostToJson().transform(hostData, writer, hostCharset);
    }

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void toJson(final byte[] hostData, final Writer writer)
            throws HostTransformException {
        getHostToJson().transform(hostData, writer);
    }

    /**
     * Transforms JSON source to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Reader reader, final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        return getJsonToHost().transform(reader, hostCharset, status);
    }

    /**
     * Transforms JSON source to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] toHost(final Reader reader, final HostTransformStatus status)
            throws HostTransformException {
        return getJsonToHost().transform(reader, status);
    }

    /**
     * Transforms host data to JSON using a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void toJson(final byte[] hostData, final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        getHostToJson().transform(hostData, writer, hostCharset, status);
    }

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void toJson(final byte[] hostData, final Writer writer,
            final HostTransformStatus status) throws HostTransformException {
        getHostToJson().transform(hostData, writer, status);
    }

}
