/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.host.invoke;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.legstar.config.Constants;


/** 
 * This data object holds the characteristics of a given CICS program.
 * Properties are initially loaded from a Properties file and can be
 * modified.
 *
 * @author Fady Moussallam
 * 
 */
public class ProgramAttributes {
	
	/** If no commarea length is provided. */
	private static final int DEFAULT_COMMAREA_LEN = 0;
	
	/** If no data length is provided. */
	private static final int DEFAULT_DATA_LEN = 0;
	
	/** If no data sync on return is provided. */
	private static final boolean DEFAULT_SYNC_ON_RETURN = false;
	
	/** The CICS program name. */
	private String mProgram;
	
	/** The size of the commarea. */
	private int mLength;
	
	/** The size of the input data. */
	private int mDataLength;
	
	/** The remote CICS ID. */
	private String mSysID;
	
	/** Syncpoint forced on return. */
	private boolean mSyncOnReturn;
	
	/** The remote CICS transaction ID to use. */
	private String mTransID;
	
	/**
	 * Constructor from a properties file.
	 * @param programAttributesFileName the name of the property file
	 * @throws ProgramAttributesException if property file cannot be read
	 */
	public ProgramAttributes(final String programAttributesFileName)
		throws ProgramAttributesException {
		
		/* load the program properties file */
		Properties programProperties = null;
		try {
			programProperties = loadFromPropFile(programAttributesFileName);
		} catch (IOException e) {
			throw new ProgramAttributesException(e);
		}
		
		/* Set individual properties */
		mProgram = programProperties.getProperty(Constants.CICS_PROGRAM_KEY);
		if (mProgram == null || mProgram.length() == 0) {
			throw new ProgramAttributesException(
					"Program name must be specified.");
		}
		String strLength = programProperties.getProperty(
				Constants.CICS_LENGTH_KEY);
		if (strLength == null) {
			mLength = DEFAULT_COMMAREA_LEN;
		} else {
			mLength = Integer.parseInt(strLength);
		}
		String strDataLength = programProperties.getProperty(
				Constants.CICS_DATALEN_KEY);
		if (strDataLength == null) {
			mDataLength = DEFAULT_DATA_LEN;
		} else {
			mDataLength = Integer.parseInt(strDataLength);
		}
		if (mDataLength > mLength) {
			throw new ProgramAttributesException(
					"Data length cannot exceed length.");
		}
		mSysID = programProperties.getProperty(Constants.CICS_SYSID_KEY);
		String strSyncOnReturn = programProperties.getProperty(
				Constants.CICS_SYNCONRET_KEY);
		if (strSyncOnReturn != null && strSyncOnReturn.length() > 0) {
			mSyncOnReturn = Boolean.parseBoolean(strSyncOnReturn);
		} else {
			mSyncOnReturn = DEFAULT_SYNC_ON_RETURN;
		}
		mTransID = programProperties.getProperty(Constants.CICS_TRANSID_KEY);
	}
	
	/**
	 * Loads a properties file from classpath into memory.
	 * @param propFileName the properties file name
	 * @return the in-memory properties file
	 * @throws IOException if file cannot be loaded
	 */
	public final Properties loadFromPropFile(
			final String propFileName) throws IOException {
		
		/* load the program properties files */
		Properties properties = new Properties();
		
		/* We use the thread loader rather than the class loader because this
		 * class will live in a different jar file than the calling
		 * application. */
		InputStream in =
			Thread.currentThread().getContextClassLoader().
			getResourceAsStream(propFileName);
		if (in == null) {
			throw (new FileNotFoundException(propFileName));
		}
		properties.load(in);
		in.close();
		return properties;
	}
	
	/**
	 * Returns a map of the program attributes using key names that are
	 * compatible with the legstar messaging protocol.
	 * 
	 * @return map of program attributes
	 */
	public final Map < String, String > getProgramAttrMap() {
		HashMap < String, String > map = new HashMap < String, String >();
		
		/* Add mandatory keys */
		map.put(Constants.CICS_PROGRAM_KEY, mProgram);
		map.put(Constants.CICS_LENGTH_KEY, Integer.toString(mLength));
		map.put(Constants.CICS_DATALEN_KEY, Integer.toString(mDataLength));

		/* Add optional keys */
		if (mSysID != null && mSysID.length() > 0) {
			map.put(Constants.CICS_SYSID_KEY, mSysID);
		}
		if (mSyncOnReturn != DEFAULT_SYNC_ON_RETURN) {
			map.put(Constants.CICS_SYNCONRET_KEY, new Boolean(
					mSyncOnReturn).toString());
		}
		if (mTransID != null && mTransID.length() > 0) {
			map.put(Constants.CICS_TRANSID_KEY, mTransID);
		}
		return map;
	}

	/**
	 * @return Returns the size of the commarea.
	 */
	public final int getLength() {
		return mLength;
	}
	
	/**
	 * @param length the size of the commarea.
	 */
	public final void setLength(final int length) {
		mLength = length;
	}

	/**
	 * @return Returns the size of the input data.
	 */
	public final int getDataLength() {
		return mDataLength;
	}

	/**
	 * @param dataLength the size of the input data.
	 */
	public final void setDataLength(final int dataLength) {
		mDataLength = dataLength;
	}

	/**
	 * @return Returns the CICS program name.
	 */
	public final String getProgram() {
		return mProgram;
	}

	/**
	 * @param program CICS program name.
	 */
	public final void setProgram(final String program) {
		mProgram = program;
	}

	/**
	 * @return Returns the Syncpoint forced on return mode.
	 */
	public final boolean getSyncOnReturn() {
		return mSyncOnReturn;
	}

	/**
	 * @param syncOnReturn Syncpoint forced on return mode.
	 */
	public final void setSyncOnReturn(final boolean syncOnReturn) {
		mSyncOnReturn = syncOnReturn;
	}

	/**
	 * @return Returns the remote CICS ID.
	 */
	public final String getSysID() {
		return mSysID;
	}

	/**
	 * @param sysID remote CICS ID.
	 */
	public final void setSysID(final String sysID) {
		mSysID = sysID;
	}

	/**
	 * @return Returns the remote CICS transaction ID to use.
	 */
	public final String getTransID() {
		return mTransID;
	}

	/**
	 * @param transID remote CICS transaction ID to use.
	 */
	public final void setTransID(final String transID) {
		mTransID = transID;
	}
}
