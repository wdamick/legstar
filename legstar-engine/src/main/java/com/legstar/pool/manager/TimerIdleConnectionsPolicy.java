package com.legstar.pool.manager;

import java.util.Timer;
import java.util.TimerTask;

/**
 * At regular time intervals, this policy checks for idle connections that have
 * been inactive for longer than a specified interval.
 * <p/>
 * Connections locks up expensive resources on the mainframe side so we want to
 * close them if they are not used.
 * 
 */
public class TimerIdleConnectionsPolicy {

    /** A timer used to schedule checks at regular time intervals. */
    private Timer _timer;

    /** The task scheduled for regular executions. */
    private CheckIdleConnectionsTask _timerTask;

    /** Clear identifier for the Timer thread. */
    private static final String TIMER_THREAD_NAME = "LegstarIdleConnectionsTimer";

    /**
     * Policy is attached to a connection pool.
     * <p/>
     * If the check period or the maximum idle time is negative or zero, nothing
     * is scheduled.
     * 
     * @param connectionPool the connection pool to monitor
     * @param period time between checks
     * @param maxIdleTime the maximum idle time for a connection
     */
    public TimerIdleConnectionsPolicy(
            final ConnectionPool connectionPool,
            final long period,
            final long maxIdleTime) {
        if (period > 0 && maxIdleTime > 0) {
            _timer = new Timer(TIMER_THREAD_NAME, true);
            _timerTask = new CheckIdleConnectionsTask(
                    connectionPool,
                    maxIdleTime);
            _timer.schedule(_timerTask, period, period);
        }
    }

    /**
     * Stop monitoring idle connections.
     */
    public synchronized void stop() {
        if (_timer != null) {
            _timer.cancel();
            _timer.purge();
            _timer = null;
        }
    }

    /**
     * @return the Task exception if any (null otherwise)
     */
    public Throwable getException() {
        if (_timerTask != null) {
            return _timerTask.getException();
        }
        return null;
    }

    /**
     * A task that is scheduled to close idle connections.
     * 
     */
    private static class CheckIdleConnectionsTask extends TimerTask {

        /** The connection pool to check for idle connections. */
        private ConnectionPool _connectionPool;

        /** Maximum idle time in milliseconds. */
        private long _maxIdleTime;

        /** Any exception that was asynchronously raised. */
        private Throwable _exception;

        /**
         * @param connectionPool the connection pool to monitor
         * @param maxIdleTime the maximum idle time for a connection
         */
        public CheckIdleConnectionsTask(final ConnectionPool connectionPool,
                final long maxIdleTime) {
            _connectionPool = connectionPool;
            _maxIdleTime = maxIdleTime;
        }

        /** {@inheritDoc} */
        @Override
        public void run() {
            try {
                _connectionPool.closeObsoleteConnections(_maxIdleTime);
            } catch (ConnectionPoolException e) {
                _exception = e;
                cancel();
            }
        }

        /**
         * @return the Exception
         */
        public Throwable getException() {
            return _exception;
        }

    }

    /**
     * @return the timer used to schedule checks at regular time intervals
     */
    public Timer getTimer() {
        return _timer;
    }
}
