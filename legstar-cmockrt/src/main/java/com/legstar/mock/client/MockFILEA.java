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
package com.legstar.mock.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

/**
 * Simulates the FILEA VSAM file with predefined access types.
 *
 */
public class MockFILEA {
    
    /** A character file with formatted data.*/
    public static final String TEXT_DATA_FILE = "/dfh$fain";
    
    /** Store Dfhcommarea objects mapping the COBOL layout of a VSAM record. */
    private Map < Long,  Dfhcommarea > mCustomersList;
    
    /** Logger. */
    private final Log _log = LogFactory.getLog(MockFILEA.class);

    /**
     * At construction time, we load a text file data content into a map of objects.
     * @throws IOException  if text file cannot be read
     */
    public MockFILEA() throws IOException {
        mCustomersList = new LinkedHashMap < Long,  Dfhcommarea >();
        InputStream is = getClass().getResourceAsStream(TEXT_DATA_FILE);
        BufferedReader in = new BufferedReader(new InputStreamReader(is));
        String record = null;
        while ((record = in.readLine()) != null) {
            _log.debug(record);
            Dfhcommarea dfhcommarea = splitRecord(record);
            if (dfhcommarea != null) {
                mCustomersList.put(new Long(dfhcommarea.getComNumber()), dfhcommarea);
            }
        }
        in.close();
    }
    
    /**
     * Retrieve a customer data.
     * @param customerId the customer ID
     * @return the customer data of null if not found
     */
    public Dfhcommarea getCustomer(final long customerId) {
        return getCustomersList().get(customerId);
    }
    
    /**
     * The list will match customers by name. If name pattern is null all
     * customers are returned otherwise the pattern can contain *.
     * @param namePattern the pattern that a name must match.
     * @param maxItems the maximum number of items to examine for a match
     * @return a list of customers whose name matches a certain (simple) pattern
     */
    public List < Dfhcommarea > getCustomers(
            final String namePattern, final long maxItems) {
        List < Dfhcommarea > list = new ArrayList < Dfhcommarea >();
        int itemsExamined = 0;
        for (Dfhcommarea dfhcommarea : getCustomersList().values()) {
            boolean match = true;
            String name = dfhcommarea.getComPersonal().getComName();
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
                list.add(dfhcommarea);
            }
            itemsExamined++;
            if (itemsExamined > maxItems) {
                break;
            }
        }
        return list;
    }
    
    /**
     * Create a Dfhcommarea object from a text record.
     * On CICS when the VSAM file is loaded, the comment gets replaced by all stars.
     * @param record from text file
     * @return a new Dfhcommarea object or null if record is invalid
     */
    private Dfhcommarea splitRecord(final String record) {
        if (record.length() != 80) {
            return null;
        }
        
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setComNumber(Long.parseLong(record.substring(1, 7)));
        ComPersonal comPersonal = new ComPersonal();
        comPersonal.setComName(record.substring(7, 27));
        comPersonal.setComAddress(record.substring(27, 47));
        comPersonal.setComPhone(record.substring(47, 55));
        dfhcommarea.setComDate(record.substring(55, 63));
        dfhcommarea.setComAmount(record.substring(63, 71));
        dfhcommarea.setComComment("*********");
        
        dfhcommarea.setComPersonal(comPersonal);
        return dfhcommarea;
    }

    /**
     * @return the customers list
     */
    public Map < Long, Dfhcommarea > getCustomersList() {
        return mCustomersList;
    }

}
