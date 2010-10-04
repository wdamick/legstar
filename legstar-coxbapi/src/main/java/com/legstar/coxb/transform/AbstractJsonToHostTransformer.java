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

import java.io.IOException;
import java.io.Reader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;

import com.legstar.coxb.CobolBindingException;

/**
 * Generic methods to transform JSON to host data.
 */
public abstract class AbstractJsonToHostTransformer implements
        IJsonToHostTransformer {

    /** Logger. */
    private final Log _log = LogFactory
            .getLog(AbstractJsonToHostTransformer.class);

    /** A Java object to Host transformer. */
    private IJavaToHostTransformer _javaToHostTransformer;

    /** Jackson object mapper. */
    private ObjectMapper _jsonMapper;

    /**
     * Create an JSON to Host transformer using a Java to Host transformer.
     * 
     * @param javaToHostTransformer the java to host transformer
     * @throws HostTransformException if transformer cannot be created
     */
    public AbstractJsonToHostTransformer(
            final IJavaToHostTransformer javaToHostTransformer)
            throws HostTransformException {
        _javaToHostTransformer = javaToHostTransformer;
        _jsonMapper = new ObjectMapper();
        AnnotationIntrospector introspector = new JaxbAnnotationIntrospector();
        _jsonMapper.getDeserializationConfig().setAnnotationIntrospector(
                        introspector);
        _jsonMapper.getSerializationConfig().setAnnotationIntrospector(
                        introspector);
    }

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Reader reader, final String hostCharset,
            final HostTransformStatus status)
            throws HostTransformException {
        if (_log.isDebugEnabled()) {
            _log.debug("Transforming JSON to host data:");
        }
        return getJavaToHostTransformer().transform(getObjectFromJson(reader),
                hostCharset, status);
    }

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param status will contain information on the transformation after it is
     *            executed
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Reader reader,
            final HostTransformStatus status)
            throws HostTransformException {
        return transform(reader, (String) null, status);
    }

    /**
     * Transforms JSON to host data with a specific host character set.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @param hostCharset the host character set
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Reader reader, final String hostCharset)
            throws HostTransformException {
        return transform(reader, hostCharset, new HostTransformStatus());
    }

    /**
     * Transforms JSON to host data.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from
     * @return a byte array with host data
     * @throws HostTransformException if transformation fails
     */
    public byte[] transform(final Reader reader) throws HostTransformException {
        return transform(reader, (String) null);
    }

    /**
     * Unmarshal an JSON to get the JAXB value object.
     * 
     * @param reader the JSON Reader to unmarshal JSON data from (such as
     *            SAXSource, DOMSource, and StreamSource)
     * @return a JAXB value object
     * @throws HostTransformException if transformation fails
     */
    public Object getObjectFromJson(final Reader reader)
            throws HostTransformException {
        try {
            return _jsonMapper.readValue(reader, getJavaToHostTransformer()
                    .getBinding().getJaxbType());
        } catch (CobolBindingException e) {
            throw new HostTransformException(e);
        } catch (JsonParseException e) {
            throw new HostTransformException(e);
        } catch (JsonMappingException e) {
            throw new HostTransformException(e);
        } catch (IOException e) {
            throw new HostTransformException(e);
        }
    }

    /**
     * @return the Java to Host transformer
     */
    public IJavaToHostTransformer getJavaToHostTransformer() {
        return _javaToHostTransformer;
    }

}
