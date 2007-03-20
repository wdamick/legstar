/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.coxb;

import java.io.IOException;


/** 
 * This data object holds the characteristics of a given CICS program.
 * Properties are loaded from a Properties file or set individually. 
 *
 * @author Fady Moussallam
 * 
 */
public class CIXSProgram {
	
	/** The CICS program name. */
	private String mProgramName = null;
	
	/** The maximum time to wait for a reply. */
	private int mExecuteTimeout = 10;
	
	/** The size of the commarea. */
	private int mCommareaLength = 0;
	
	/** The size of the input data. */
	private int mDataLength = 0;
	
	/** The remote CICS ID. */
	private String mSysID = null;
	
	/** Syncpoint forced on return. */
	private String mSyncOnReturn = null;
	
	/** The remote CICS transaction ID to use. */
	private String mTransID = null;
	
	/** Default no-arg constructor. */
	public CIXSProgram() {
		
	}

	/**
	 * Constructor from a properties file.
	 * @param propertiesFileName the name of the property file
	 * @throws IOException if property file cannot be read
	 */
	public CIXSProgram(final String propertiesFileName)
		throws IOException {
		
		/* load the program properties files */
		java.util.Properties programProperties = new java.util.Properties();
		
		/* We use the thread loader rather than the class loader because this
		 * class will live in a different jar file than the calling
		 * application. */
		java.io.InputStream in =
			Thread.currentThread().getContextClassLoader().
			getResourceAsStream(propertiesFileName);
		if (in == null) {
			throw (new java.io.FileNotFoundException(propertiesFileName));
		}
		programProperties.load(in);
		in.close();
		
		/* Set individual properties */
		mProgramName = programProperties.getProperty("ProgramName");
		String strExecuteTimeout =
			programProperties.getProperty("ExecuteTimeout");
		if (strExecuteTimeout != null) {
			mExecuteTimeout = Integer.parseInt(strExecuteTimeout);
		}
		String strCommareaLength =
			programProperties.getProperty("CommareaLength");
		if (strCommareaLength != null) {
			mCommareaLength =
				Integer.parseInt(
						programProperties.getProperty("CommareaLength"));
		}
		String strDataLength = programProperties.getProperty("DataLength");
		if (strDataLength != null) {
			mDataLength =
				Integer.parseInt(programProperties.getProperty("DataLength"));
		}
		mSysID = programProperties.getProperty("SysID");
		mSyncOnReturn = programProperties.getProperty("SyncOnReturn");
		mTransID = programProperties.getProperty("TransID");
	}

	/**
	 * @return Returns the size of the commarea.
	 */
	public final int getCommareaLength() {
		return mCommareaLength;
	}

	/**
	 * @param commareaLength The size of the commarea to set.
	 */
	public final void setCommareaLength(final int commareaLength) {
		mCommareaLength = commareaLength;
	}

	/**
	 * @return Returns the size of the input data.
	 */
	public final int getDataLength() {
		return mDataLength;
	}

	/**
	 * @param dataLength The size of the input data to set.
	 */
	public final void setDataLength(final int dataLength) {
		mDataLength = dataLength;
	}

	/**
	 * @return Returns the maximum time to wait for a reply.
	 */
	public final int getExecuteTimeout() {
		return mExecuteTimeout;
	}

	/**
	 * @param executeTimeout The maximum time to wait for a reply to set.
	 */
	public final void setExecuteTimeout(final int executeTimeout) {
		mExecuteTimeout = executeTimeout;
	}

	/**
	 * @return Returns the CICS program name.
	 */
	public final String getProgramName() {
		return mProgramName;
	}

	/**
	 * @param programName The CICS program name to set.
	 */
	public final void setProgramName(final String programName) {
		mProgramName = programName;
	}

	/**
	 * @return Returns the Syncpoint forced on return.
	 */
	public final String getSyncOnReturn() {
		return mSyncOnReturn;
	}

	/**
	 * @param syncOnReturn The Syncpoint forced on return to set.
	 */
	public final void setSyncOnReturn(final String syncOnReturn) {
		mSyncOnReturn = syncOnReturn;
	}

	/**
	 * @return Returns the remote CICS ID.
	 */
	public final String getSysID() {
		return mSysID;
	}

	/**
	 * @param sysID The remote CICS ID to set.
	 */
	public final void setSysID(final String sysID) {
		mSysID = sysID;
	}

	/**
	 * @return Returns the The remote CICS transaction ID to use.
	 */
	public final String getTransID() {
		return mTransID;
	}

	/**
	 * @param transID The The remote CICS transaction ID to use to set.
	 */
	public final void setTransID(final String transID) {
		mTransID = transID;
	}
}
