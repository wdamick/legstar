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
package com.legstar.coxb.transform;

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

}
