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
package com.legstar.c2ws.util;

import org.apache.commons.logging.Log;


/**
 * This is a simple wrapper on an Apache logging logger. It allows a
 * correlation id to show up on each trace produced.
 *
 */
public class C2wsLog implements Log {
	
	/** A reference to a static logger. */
	private Log mLog;
	
	/** A correlation ID to show on traces. */
	private String mCxid;
	
	/**
	 * Creates an instance wrapping an Apache logger and specifying a
	 * correlation id.
	 * @param log the Apache logger
	 * @param cxid a correlation id used for end to end tracing
	 */
	public C2wsLog(final Log log, final String cxid) {
		mLog = log;
		mCxid = cxid;
	}
	
	/**
	 * Creates an instance wrapping an Apache logger.
	 * @param log the Apache logger
	 */
	public C2wsLog(final Log log) {
		this(log, null);
	}

	/**
	 * @return the Correlation Id
	 */
	public final String getCorrelationId() {
		return mCxid;
	}

	/**
	 * @param cxid the Correlation Id to set
	 */
	public final void setCorrelationId(final String cxid) {
		mCxid = cxid;
	}

	/**
	 * @return the logger
	 */
	public final Log getLog() {
		return mLog;
	}

	/**
	 * @return the Correlation Id formatted
	 */
	public final String getCorrelationIdTrace() {
		if (mCxid == null || mCxid.length() == 0) {
			return "";
		}
		return " - " + mCxid;
	}

	/**
	 * @param log the logger to set
	 */
	public final void setLog(final Log log) {
		mLog = log;
	}

	/** {@inheritDoc}*/
	public final void debug(final Object arg0) {
		mLog.debug(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void debug(final Object arg0, final Throwable arg1) {
		mLog.debug(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

	/** {@inheritDoc}*/
	public final void error(final Object arg0) {
		mLog.error(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void error(final Object arg0, final Throwable arg1) {
		mLog.error(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

	/** {@inheritDoc}*/
	public final void fatal(final Object arg0) {
		mLog.fatal(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void fatal(final Object arg0, final Throwable arg1) {
		mLog.fatal(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

	/** {@inheritDoc}*/
	public final void info(final Object arg0) {
		mLog.info(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void info(final Object arg0, final Throwable arg1) {
		mLog.info(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

	/** {@inheritDoc}*/
	public final boolean isDebugEnabled() {
		return mLog.isDebugEnabled();
	}

	/** {@inheritDoc}*/
	public final boolean isErrorEnabled() {
		return mLog.isErrorEnabled();
	}

	/** {@inheritDoc}*/
	public final boolean isFatalEnabled() {
		return mLog.isFatalEnabled();
	}

	/** {@inheritDoc}*/
	public final boolean isInfoEnabled() {
		return mLog.isInfoEnabled();
	}

	/** {@inheritDoc}*/
	public final boolean isTraceEnabled() {
		return mLog.isTraceEnabled();
	}

	/** {@inheritDoc}*/
	public final boolean isWarnEnabled() {
		return mLog.isWarnEnabled();
	}

	/** {@inheritDoc}*/
	public final void trace(final Object arg0) {
		mLog.trace(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void trace(final Object arg0, final Throwable arg1) {
		mLog.trace(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

	/** {@inheritDoc}*/
	public final void warn(final Object arg0) {
		mLog.warn(arg0.toString() + getCorrelationIdTrace());
	}

	/** {@inheritDoc}*/
	public final void warn(final Object arg0, final Throwable arg1) {
		mLog.warn(arg0.toString() + getCorrelationIdTrace(), arg1);
	}

}
