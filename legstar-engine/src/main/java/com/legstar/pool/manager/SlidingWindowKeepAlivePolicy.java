package com.legstar.pool.manager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.RequestException;

/**
 * A simplistic sliding window that allow us keep track of opened connections in a given timespan.
 * <p/>
 * The windows slides when a new connection is added. When it slides, connections that are obsolete
 * are closed and removed from the sliding window.
 * <p/>
 *
 */
public class SlidingWindowKeepAlivePolicy {

    /** Sliding window time span in milliseconds. */
    private long _timespan;

    /** List of events occurring within sliding window. */
    private final List < OpenedConnection > _eventsInWindow =
        Collections.synchronizedList(new ArrayList < OpenedConnection >());

    /**
     * Construct a sliding window.
     * @param timespan time span in milliseconds
     */
    public SlidingWindowKeepAlivePolicy(final long timespan) {
        _timespan = timespan;
    }

    /**
     * Add a new opened connection to the sliding window.
     * <p/>
     * This will close and expel old connections which fell off the sliding window.
     * @param connection the new opened connection
     * @throws ConnectionPoolException if treatment of obsolete connections fails
     */
    public void add(final LegStarConnection connection) throws ConnectionPoolException {
        if (getTimespan() <= 0) {
            return;
        }
        long now = System.currentTimeMillis();
        removeObsoleteConnections(now);
        _eventsInWindow.add(new OpenedConnection(connection, now));
    }

    /**
     * @return the number of events in the sliding window
     */
    public int getEventsInWindow() {
        return _eventsInWindow.size();
    }

    /**
     * @return the events ratio as the number of events that occurred
     *  in a second
     */
    public double getEventsRatio() {
        return (getEventsInWindow() * 1000.0) / _timespan;
    }

    /**
     * Remove events which are now obsolete.
     * @param now the current time
     * @throws ConnectionPoolException if obsolete connections cannot be closed
     */
    private void removeObsoleteConnections(
            final Long now) throws ConnectionPoolException {
        try {
            synchronized (_eventsInWindow) {
                Iterator < OpenedConnection > iter = _eventsInWindow.iterator();
                while (iter.hasNext()) {
                    OpenedConnection openedConnection = iter.next();
                    if ((now - openedConnection.getTimeOpened()) > _timespan) {
                        openedConnection.getConnection().close();
                        iter.remove();
                    }
                }
            }
        } catch (RequestException e) {
            throw new ConnectionPoolException(e);
        }
    }

    /**
     * @return sliding window time span in milliseconds
     */
    public long getTimespan() {
        return _timespan;
    }

    /**
     * A class to associate a connection with its open time.
     *
     */
    public final class OpenedConnection {

        /** Actual connection object. */
        private LegStarConnection _connection;

        /** Time when connection was opened.*/
        private long _timeOpened;

        /**
         * Construct the opened connection.
         * @param connection actual connection
         * @param timeOpened time when connection was opened
         */
        public OpenedConnection(
                final LegStarConnection connection,
                final long timeOpened) {
            _connection = connection;
            _timeOpened = timeOpened;
        }

        /**
         * @return the actual connection object
         */
        public LegStarConnection getConnection() {
            return _connection;
        }

        /**
         * @return the time when connection was opened
         */
        public long getTimeOpened() {
            return _timeOpened;
        }
    }


}
