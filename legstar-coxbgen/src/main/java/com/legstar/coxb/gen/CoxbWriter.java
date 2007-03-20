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
package com.legstar.coxb.gen;

import java.io.File;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

import com.legstar.host.HostException;

/**
 * This class manages a LIFO stack of temporary files. The last file opened is
 * used for all write operations until a request to close or open arrives. 
 *
 * @author Fady Moussallam
 * 
*/
public class CoxbWriter {
	
	/** This is the folder where all generated files will be created. */
	private File mTargetDir;
	
	/** The list of active file names. The last one in the stack is used for
	 * write operations. */
	private List < String > mFileStack;
	
	/** The corresponding list of writers. */
	private List < BufferedWriter > mWritersStack;
	
	/** Pattern for temporary files. */
	private static final String TEMP_PATTERN = "legstar-td-";
	
	/** Suffix for temporary files. */
	private static final String TEMP_SUFFIX = ".tmp";
	
	/**
	 * Constructor from an existing directory.
     * 
	 * @param targetDir an existing directory where new files are to be created
	 * @throws HostException if directory is invalid
	 */
	public CoxbWriter(final File targetDir) throws HostException {
	
		if (!targetDir.exists()) {
			throw (new HostException(targetDir.getPath() + " does not exist"));
		}
		if (!targetDir.isDirectory()) {
			throw (new HostException(targetDir.getPath()
					+ " is not a directory"));
		}
		if (!targetDir.canWrite()) {
			throw (new HostException("Directory " + targetDir.getPath() 
					+ " is not writable"));
		}
		mTargetDir = targetDir;
		mFileStack = new ArrayList < String >();
		mWritersStack = new ArrayList < BufferedWriter >();
	}
	
	/**
	 * Create a temporary file and writes content to it. Store the new
	 * file as the last one in the stack.
	 * 
	 * @param content the data to write
	 * @return the name of the temporary file
	 * @throws HostException if file cannot be created
	 */
	public final String openWrite(
			final String content) throws HostException {
		
		File temp;
		try {
			/* Create a new temporary file */
			temp = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
			
	        /* Delete temporary file when program exits.*/
	        temp.deleteOnExit();
	        
			/* Add the file to the stack of opened files */
	        BufferedWriter out = new BufferedWriter(new FileWriter(temp));
			mFileStack.add(temp.getPath());
			mWritersStack.add(out);
			
			/* Write the initial content */
			out.write(content);
		} catch (IOException e) {
			throw (new HostException(e.getMessage()));
		}
		return temp.getPath();
	}
	
	/**
	 * Writes content to the last file in the stack.
     * 
	 * @param content the data to write
	 * @return the name of the temporary file
	 * @throws HostException if there are no open files
	 */
	public final String write(final String content) throws HostException {
		
		String fileName = null;
		if (mFileStack.size() == 0) {
			throw (new HostException("No current open file on stack"));
		}
		try {
			BufferedWriter out = mWritersStack.get(mWritersStack.size() - 1);
			out.write(content);
			fileName = mFileStack.get(mFileStack.size() - 1);
		} catch (IOException e) {
			throw (new HostException(e.getMessage()));
		}
		return fileName;
	}
	
	/**
	 * Write data to the last file in the stack and close it. removes the file
	 * from the stack.
	 * 
	 * @param content the data to write
	 * @return the name of the temporary file
	 * @throws HostException if there are no open files or write fails
	 */
	public final String writeClose(final String content) throws HostException {
		
		String fileName = null;
		try {
			write(content);
			BufferedWriter out = mWritersStack.get(mWritersStack.size() - 1);
			out.close();
			fileName = mFileStack.get(mFileStack.size() - 1);
		} catch (IOException e) {
			throw (new HostException(e.getMessage()));
		}
		mFileStack.remove(mFileStack.size() - 1);
		mWritersStack.remove(mWritersStack.size() - 1);

		return fileName;
	}
	
	/**
	 * Close all files from the stack and empty it.
     * 
	 * @throws IOException if close operation fails
	 */
	public final void closeAll() throws IOException {
		for (BufferedWriter out : mWritersStack) {
			out.close();
		}
		mFileStack = new ArrayList < String >();
		mWritersStack = new ArrayList < BufferedWriter >();
	}

	/**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mTargetDir;
	}

	/**
	 * @return the current target directory location name
	 */
	public final String getTargetDirName() {
		return mTargetDir.getAbsolutePath();
	}
	
	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTarget(final File targetDir) {
		mTargetDir = targetDir;
	}

	/**
	 * @return the stack of temporary file names
	 */
	public final List < String > getFileStack() {
		return mFileStack;
	}

	/**
	 * @return the stack of opened writers
	 */
	public final List < BufferedWriter > getWritersStack() {
		return mWritersStack;
	}

}
