/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.invoke;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
public class CicsProgram {
	
	/** If no commarea length is provided. */
	private static final int DEFAULT_COMMAREA_LEN = 0;
	
	/** If no data length is provided. */
	private static final int DEFAULT_DATA_LEN = 0;
	
	/** If no data sync on return is provided. */
	private static final boolean DEFAULT_SYNC_ON_RETURN = false;
	
	/** The CICS program name. */
	private String mName;
	
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
	
	/** The CICS Channel used to link to this program. */
	private String mChannel;
	
	/** The list of input containers names and their max host byte size. */
	private Map < String, Integer > mInContainers;
	
	/** The list of output containers names and their max host byte size. */
	private Map < String, Integer > mOutContainers;
	
	/**
	 * Constructor from a properties file.
	 * @param programAttributesFileName the name of the property file
	 * @throws CicsProgramException if property file cannot be read
	 */
	public CicsProgram(final String programAttributesFileName)
		throws CicsProgramException {
		
		/* load the program properties file */
		Properties programProperties = null;
		try {
			programProperties = loadFromPropFile(programAttributesFileName);
		} catch (IOException e) {
			throw new CicsProgramException(e);
		}
		
		/* Set individual properties */
		mName = programProperties.getProperty(Constants.CICS_PROGRAM_NAME_KEY);
		if (mName == null || mName.length() == 0) {
			throw new CicsProgramException(
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
			throw new CicsProgramException(
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
		
		/* When channel is used, there is a list of input containers and a
		 * list of output containers. Each container has a name and a max size
		 * (host bytes).
		 *  */
		mChannel = programProperties.getProperty(Constants.CICS_CHANNEL_KEY);
		if (mChannel != null && mChannel.length() > 0) {
			mInContainers = new LinkedHashMap < String, Integer >();
			loadContainer(programProperties, mInContainers,
					Constants.CICS_IN_CONTAINERS_KEY,
					Constants.CICS_IN_CONTAINERS_LEN_KEY);
			mOutContainers = new LinkedHashMap < String, Integer >();
			loadContainer(programProperties, mOutContainers,
					Constants.CICS_OUT_CONTAINERS_KEY,
					Constants.CICS_OUT_CONTAINERS_LEN_KEY);
		}
	}
	
	/**
	 * Create a map with container names and associated max size from entries
	 * in a property file.
	 * List of items are expected to be stored as a set of properties suffixed
	 * with _n where n in the item rank. 
	 * @param programProperties program attributes as properties 
	 * @param containers an empty map for containers names and sizes
	 * @param nameKey the properties key for container name
	 * @param lengthKey the properties key for container size
	 * @throws CicsProgramException if failed to create map
	 */
	private void loadContainer(
			final Properties programProperties,
			final Map < String, Integer > containers,
			final String nameKey,
			final String lengthKey)
			throws CicsProgramException {
		int i = 1;
		String container = programProperties.getProperty(nameKey + '_' + i);
		while (container != null && container.length() > 0) {
			Integer size = new Integer(0);
			String containerSize = programProperties.getProperty(
					lengthKey + '_' + i);
			if (containerSize != null && containerSize.length() > 0) {
				size = Integer.parseInt(containerSize);
			}
			containers.put(container, size);
			container = programProperties.getProperty(nameKey + '_' + ++i);
		}
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
	public final Map < String, Object > getProgramAttrMap() {
		HashMap < String, Object > map = new HashMap < String, Object >();
		
		/* Add mandatory keys */
		map.put(Constants.CICS_PROGRAM_NAME_KEY, mName);
		
		/* Pass on Channel or Commarea mandatory parameters */
		if (mChannel == null || mChannel.length() == 0) {
			map.put(Constants.CICS_LENGTH_KEY, Integer.toString(mLength));
			map.put(Constants.CICS_DATALEN_KEY, Integer.toString(mDataLength));
		} else {
			map.put(Constants.CICS_CHANNEL_KEY, mChannel);
			/* Pass output containers as a string array */
			if (mOutContainers != null && mOutContainers.size() > 0) {
				String[] outContainers = new String[mOutContainers.size()];
				mOutContainers.keySet().toArray(outContainers);
				map.put(Constants.CICS_OUT_CONTAINERS_KEY, outContainers);
			}
		}

		/* Add optional keys */
		if (mSysID != null && mSysID.length() > 0) {
			map.put(Constants.CICS_SYSID_KEY, mSysID);
		}
		if (mSyncOnReturn != DEFAULT_SYNC_ON_RETURN) {
			map.put(Constants.CICS_SYNCONRET_KEY, Boolean.valueOf(
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
	public final String getName() {
		return mName;
	}

	/**
	 * @param name CICS program name.
	 */
	public final void setName(final String name) {
		mName = name;
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

	/**
	 * @return the CICS Channel
	 */
	public final String getChannel() {
		return mChannel;
	}

	/**
	 * @param channel the CICS Channel to set
	 */
	public final void setChannel(final String channel) {
		mChannel = channel;
	}

	/**
	 * @return the input Containers list
	 */
	public final Map < String, Integer > getInContainers() {
		return mInContainers;
	}

	/**
	 * @param inContainers the input Containers list to set
	 */
	public final void setInContainers(
			final Map < String, Integer > inContainers) {
		mInContainers = inContainers;
	}
	/**
	 * @return the output Containers list
	 */
	public final Map < String, Integer > getOutContainers() {
		return mOutContainers;
	}

	/**
	 * @param outContainers the output Containers list to set
	 */
	public final void setOutContainers(
			final Map < String, Integer > outContainers) {
		mOutContainers = outContainers;
	}
}
