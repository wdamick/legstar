/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.visitor;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.ICobolArrayBinaryBinding;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolArrayDbcsBinding;
import com.legstar.coxb.ICobolArrayDoubleBinding;
import com.legstar.coxb.ICobolArrayFloatBinding;
import com.legstar.coxb.ICobolArrayNationalBinding;
import com.legstar.coxb.ICobolArrayOctetStreamBinding;
import com.legstar.coxb.ICobolArrayPackedDecimalBinding;
import com.legstar.coxb.ICobolArrayStringBinding;
import com.legstar.coxb.ICobolArrayZonedDecimalBinding;
import com.legstar.coxb.ICobolBinaryBinding;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.ICobolDbcsBinding;
import com.legstar.coxb.ICobolDoubleBinding;
import com.legstar.coxb.ICobolFloatBinding;
import com.legstar.coxb.ICobolNationalBinding;
import com.legstar.coxb.ICobolOctetStreamBinding;
import com.legstar.coxb.ICobolPackedDecimalBinding;
import com.legstar.coxb.ICobolStringBinding;
import com.legstar.coxb.ICobolZonedDecimalBinding;
import com.legstar.coxb.convert.ICobolConverters;
import com.legstar.coxb.host.HostException;

/**
 * This visitor builds a flat map of key/value pairs while unmarshaling
 * mainframe data.
 * <p/>
 * 
 */
public class FlatCobolUnmarshalVisitor extends CobolUnmarshalVisitor {

    /** Unmarshaler populates this flat ordered map. */
    private Map < String, Object > _keyValues = new LinkedHashMap < String, Object >();

    /**
     * Contextual suffix to uniquely identify a map entry (used for array
     * items).
     */
    private String _suffix = "";

    /**
     * Contextual prefix to uniquely identify a map entry (used for name
     * conflict resolution)
     */
    private String _prefix = "";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Visitor constructor.
     * 
     * @param hostBytes host buffer used by visitor
     * @param offset offset in host buffer
     * @param cobolConverters set of converters to use for cobol elements
     */
    public FlatCobolUnmarshalVisitor(byte[] hostBytes, int offset,
            ICobolConverters cobolConverters) {
        super(hostBytes, offset, cobolConverters);
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolComplexBinding ce) throws HostException {
        String prevPrefix = _prefix;
        _prefix = _prefix + "_" + ce.getJaxbName();
        super.visit(ce);
        _prefix = prevPrefix;
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayComplexBinding ce) throws HostException {
        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling started for array of complex bindings "
                    + ce.getBindingName());
        }
        /*
         * Ask complex array binding to initialize bound array so that it is
         * ready for unmarshaling.
         */
        ce.createValueObject();

        String prevSuffix = _suffix;

        /*
         * Visit each item of the array in turn creating a contextual suffix for
         * each item.
         */
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            _suffix = _suffix + "_" + Integer.toString(i);
            ICobolBinding itemDesc = ce.getComplexItemBinding();
            itemDesc.accept(this);
            ce.addPropertyValue(i);
            _suffix = prevSuffix;
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Unmarshaling successful for array of complex bindings "
                    + ce.getBindingName());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolStringBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getStringValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayStringBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getStringList().get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolNationalBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getStringValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayNationalBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getStringList().get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolDbcsBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getStringValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayDbcsBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getStringList().get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolZonedDecimalBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getObjectValue(ce.getJaxbType()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayZonedDecimalBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix),
                    ((List < ? >) ce.getObjectValue(ce.getJaxbType())).get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolPackedDecimalBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getObjectValue(ce.getJaxbType()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayPackedDecimalBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix),
                    ((List < ? >) ce.getObjectValue(ce.getJaxbType())).get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolBinaryBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getObjectValue(ce.getJaxbType()));
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayBinaryBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix),
                    ((List < ? >) ce.getObjectValue(ce.getJaxbType())).get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolFloatBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getFloatValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayFloatBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getFloatList().get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolDoubleBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getDoubleValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayDoubleBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getDoubleList().get(i));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolOctetStreamBinding ce) throws HostException {
        super.visit(ce);
        _keyValues.put(uniqueName(ce.getJaxbName() + _suffix),
                ce.getByteArrayValue());
    }

    /** {@inheritDoc} */
    @Override
    public void visit(ICobolArrayOctetStreamBinding ce) throws HostException {
        super.visit(ce);
        for (int i = 0; i < ce.getCurrentOccurs(); i++) {
            String newSuffix = _suffix + "_" + Integer.toString(i);
            _keyValues.put(uniqueName(ce.getJaxbName() + newSuffix), ce
                    .getByteArrayList().get(i));
        }
    }

    /**
     * Creates a unique column name based on a proposed name.
     * <p/>
     * The proposed name is as simple as possible but we need to avoid any name
     * conflicts (a column must have a unique name) so we check for any conflict
     * and use a prefix system to disambiguate names.
     * 
     * @param proposedName the proposed name
     * @return a unique name for a column
     */
    protected String uniqueName(final String proposedName) {

        String[] prefixes = _prefix.split("_");
        int pos = prefixes.length - 1;

        String uniqueName = proposedName;
        while (_keyValues.containsKey(uniqueName) && pos > -1) {
            uniqueName = prefixes[pos] + '_' + uniqueName;
            pos--;
        }

        return uniqueName;

    }

    /**
     * @return the key/value pairs built from mainframe data
     */
    public Map < String, Object > getKeyValues() {
        return _keyValues;
    }

}
