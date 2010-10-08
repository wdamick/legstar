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
package com.legstar.mock.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.CobolConversionException;
import com.legstar.coxb.convert.simple.CobolStringSimpleConverter;
import com.legstar.coxb.convert.simple.CobolZonedDecimalSimpleConverter;

/**
 * Simulates the FILEA VSAM file with predefined access types.
 *
 */
public class MockFILEA {
    
    /** A character file with formatted data.*/
    public static final String TEXT_DATA_FILE = "/dfh$fain";
    
    /** Store host data per customer Id. */
    private Map < BigDecimal,  byte[] > _hostCustomersList;

    /** Logger. */
    private final Log _log = LogFactory.getLog(MockFILEA.class);

    /**
     * At construction time, we load a text file data content into a map of host byte arrays.
     */
    public MockFILEA() {
        try {
            _hostCustomersList = new LinkedHashMap < BigDecimal,  byte[] >();
            InputStream is = getClass().getResourceAsStream(TEXT_DATA_FILE);
            BufferedReader in = new BufferedReader(new InputStreamReader(is));
            String record = null;
            while ((record = in.readLine()) != null) {
                _log.debug(record);
                byte[] hostRecord = getHostRecord(record);
                if (hostRecord != null) {
                    _hostCustomersList.put(new BigDecimal(record.substring(1, 7)), hostRecord);
                }
            }
            in.close();
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
    }
    
    /**
     * Retrieve customer data in host format.
     * @param customerId the customer Id in host format
     * @return the customer record in host format
     */
    public byte[] getCustomer(final byte[] customerId) {
        try {
            BigDecimal bigId = CobolZonedDecimalSimpleConverter.fromHostSingle(
                    6, 6, 0, false, false, false, customerId, 0,
                    CobolContext.getDefaultHostCharsetName());
            return _hostCustomersList.get(bigId);
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return null;
        }
    }
    
    /**
     * @return the total number of customers
     */
    public int getCustomersNumber() {
        return _hostCustomersList.size();
    }
    
    /**
     * The list will match customers by name. If name pattern is null all
     * customers are returned otherwise the pattern can contain *.
     * @param namePattern the pattern that a name must match.
     * @return a list of customers whose name matches a certain (simple) pattern
     */
    public List < byte[] > getCustomers(
            final String namePattern) {
        return getCustomers(namePattern, -1, _hostCustomersList.size());
    }

    /**
     * The list will match customers by name. If name pattern is null all
     * customers are returned otherwise the pattern can contain *.
     * @param namePattern the pattern that a name must match.
     * @param maxItems the maximum number of items to examine for a match
     * @return a list of customers whose name matches a certain (simple) pattern
     */
    public List < byte[] > getCustomers(
            final String namePattern, final long maxItems) {
        return getCustomers(namePattern, -1, maxItems);
    }

    /**
     * The list will match customers by name. If name pattern is null all
     * customers are returned otherwise the pattern can contain *.
     * @param namePattern the pattern that a name must match.
     * @param maxItems the maximum number of items to examine for a match (-1 for no limit)
     * @param maxReplies the maximum number of matches to return (-1 for no limit)
     * @return a list of customers whose name matches a certain (simple) pattern
     */
    public List < byte[] > getCustomers(
            final String namePattern, final long maxItems, final long maxReplies) {
        List < byte[] > list = new ArrayList < byte[] >();
        int itemsExamined = 0;
        for (byte[] dfhcommarea : _hostCustomersList.values()) {
            boolean match = true;
            String name = getCustomerName(dfhcommarea);
            if (namePattern != null) {
                for (int i = 0; i < namePattern.length(); i++) {
                    if (namePattern.charAt(i) == '*') {
                        break;
                    } else {
                        if (namePattern.charAt(i) != name.charAt(i)) {
                            match = false;
                            break;
                        }
                    }
                }
            }
            if (match) {
                if (maxReplies > -1 && list.size() == maxReplies) {
                    break;
                }
                list.add(dfhcommarea);
            }
            itemsExamined++;
            if (maxItems > -1 && itemsExamined > maxItems) {
                break;
            }
        }
        return list;
    }
    
    /**
     * Get the name from a host byte array.
     * @param dfhcommarea the host byte array
     * @return the customer name
     */
    private String getCustomerName(final byte[] dfhcommarea) {
        try {
            return CobolStringSimpleConverter.fromHostSingle(
                    CobolContext.getDefaultHostCharsetName(), 20, dfhcommarea, 6);
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return null;
        }
    }
    
    /**
     * Create a host record from a text record.
     * On CICS when the VSAM file is loaded, the comment gets replaced by all stars.
     * @param record from text file
     * @return a new host record or null if record is invalid
     */
    private byte[] getHostRecord(final String record) {
        if (record.length() != 80) {
            return null;
        }
        int offset = 0;
        byte[] hostRecord = new byte[79];
        String hostCharsetName = CobolContext.getDefaultHostCharsetName();

        try {
            /* number */
            CobolZonedDecimalSimpleConverter.toHostSingle(
                    new BigDecimal(record.substring(1, 7)),
                    6, 6, 0, false, false, false, hostRecord, offset, hostCharsetName);
            offset += 6;

            /* name */
            CobolStringSimpleConverter.toHostSingle(
                    record.substring(7, 27),
                    hostCharsetName, 20, false, hostRecord, offset);
            offset += 20;
            
            /* address */
            CobolStringSimpleConverter.toHostSingle(
                    record.substring(27, 47),
                    hostCharsetName, 20, false, hostRecord, offset);
            offset += 20;
            
            /* phone */
            CobolStringSimpleConverter.toHostSingle(
                    record.substring(47, 55),
                    hostCharsetName, 8, false, hostRecord, offset);
            offset += 8;
            
            /* date */
            CobolStringSimpleConverter.toHostSingle(
                    record.substring(55, 63),
                    hostCharsetName, 8, false, hostRecord, offset);
            offset += 8;

            /* amount */
            CobolStringSimpleConverter.toHostSingle(
                    record.substring(63, 71),
                    hostCharsetName, 8, false, hostRecord, offset);
            offset += 8;

            /* comment */
            CobolStringSimpleConverter.toHostSingle(
                    "*********",
                    hostCharsetName, 9, false, hostRecord, offset);
            offset += 9;
        } catch (CobolConversionException e) {
            e.printStackTrace();
            return null;
        }

        return hostRecord;
    }

}
