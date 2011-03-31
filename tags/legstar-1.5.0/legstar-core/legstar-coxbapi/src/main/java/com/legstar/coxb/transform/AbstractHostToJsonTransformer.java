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
package com.legstar.coxb.transform;

import java.io.IOException;
import java.io.Writer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;

/**
 * Generic methods to transform host data to JSON.
 */
public abstract class AbstractHostToJsonTransformer implements
        IHostToJsonTransformer {

    /** Logger. */
    private final Log _log = LogFactory
            .getLog(AbstractHostToJsonTransformer.class);

    /** A Host to Java object transformer. */
    private IHostToJavaTransformer _hostToJavaTransformer;

    /** Jackson object mapper. */
    private ObjectMapper _jsonMapper;

    /**
     * Create an Host to JSON transformer using a Host to Java transformer.
     * 
     * @param hostToJavaTransformer the host to java transformer
     * @throws HostTransformException if transformer cannot be created
     */
    public AbstractHostToJsonTransformer(
            final IHostToJavaTransformer hostToJavaTransformer)
            throws HostTransformException {
        _hostToJavaTransformer = hostToJavaTransformer;
        _jsonMapper = new ObjectMapper();
        AnnotationIntrospector introspector = new JaxbAnnotationIntrospector();
        _jsonMapper.getDeserializationConfig().setAnnotationIntrospector(
                    introspector);
        _jsonMapper.getSerializationConfig().setAnnotationIntrospector(
                    introspector);
    }

    /**
     * Transforms host data to JSON with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        if (_log.isDebugEnabled()) {
            _log.debug("Transforming host data to JSON:");
        }
        Object valueObject = getHostToJavaTransformer().transform(hostData,
                offset, hostCharset, status);
        getJsonFromObject(valueObject, writer);
    }

    /**
     * Transforms host data to JSON with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final String hostCharset) throws HostTransformException {
        transform(hostData, offset, writer, hostCharset,
                new HostTransformStatus());
    }

    /**
     * Transforms host data to JSON with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset) throws HostTransformException {
        transform(hostData, 0, writer, hostCharset);
    }

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer) throws HostTransformException {
        transform(hostData, 0, writer, (String) null);
    }

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer JSON will be sent to this writer.
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer) throws HostTransformException {
        transform(hostData, offset, writer, (String) null);
    }

    /**
     * Transforms host data to JSON with a specific host character set.
     * 
     * @param hostData a byte array containing host data
     * @param writer JSON will be sent to this writer.
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final String hostCharset,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, 0, writer, hostCharset, status);
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
    public void transform(
            final byte[] hostData,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, 0, writer, (String) null, status);
    }

    /**
     * Transforms host data to JSON.
     * 
     * @param hostData a byte array containing host data
     * @param offset index of first byte to process in hostData
     * @param writer JSON will be sent to this writer.
     * @param status will contain information on the transformation after it is
     *            executed
     * @throws HostTransformException if transformation fails
     */
    public void transform(
            final byte[] hostData,
            final int offset,
            final Writer writer,
            final HostTransformStatus status) throws HostTransformException {
        transform(hostData, offset, writer, (String) null, status);
    }

    /**
     * Marshal JAXB value object to get the JSON.
     * <p/>
     * Root elements can be marshalled directly while non-root elements must be
     * encapsulated in a JAXBElement before they can be marshalled.
     * 
     * @param valueObject the JAXB value object
     * @param writer JSON will be sent to this writer.
     * @throws HostTransformException if transformation fails
     * */
    public void getJsonFromObject(
            final Object valueObject, final Writer writer)
            throws HostTransformException {
        try {
            _jsonMapper.writeValue(writer, valueObject);
        } catch (JsonGenerationException e) {
            throw new HostTransformException(e);
        } catch (JsonMappingException e) {
            throw new HostTransformException(e);
        } catch (IOException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * @return the Host to Java transformer
     */
    public IHostToJavaTransformer getHostToJavaTransformer() {
        return _hostToJavaTransformer;
    }

}
