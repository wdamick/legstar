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
