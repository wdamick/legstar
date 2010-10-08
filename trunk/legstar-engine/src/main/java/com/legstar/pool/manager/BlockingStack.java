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
package com.legstar.pool.manager;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/**
 * A poor mans bounded LIFO queue. This implements a stack with blocking poll
 * capabilities similar to ArrayBlockinhQueue (which is FIFO).
 * <p/>
 * Hacked from org.apache.tomcat.jdbc.pool.FairBlockingQueue.
 * To be replaced with Java 1.6 Deque.
 * @param <T> the stack object type
 *
 */
public class BlockingStack < T > {
    
    /** Stack maximum capacity. */
    private int _capacity;
    
    /** Stack elements are organized as a linked list internally.
     * Linked lists are not synchronized. */
    private LinkedList < T > _list = new LinkedList < T >();
    
    /** Synchronization lock. */
    private final ReentrantLock _lock = new ReentrantLock();
    
    /**
     * Threads waiting for a stack element to become available will
     * have each an associated count down latch in the order where
     * they started waiting.
     */
    private LinkedList < ExchangeCountDownLatch < T > > _waiters =
        new LinkedList < ExchangeCountDownLatch < T > >();

    
    /**
     * Creates a blocking stack with the specified maximum size.
     * @param size maximum stack capacity
     */
    public BlockingStack(final int size) {
        _capacity = size;
    }
    
    /**
     * Adds the specified element to this stack.
     * If consumers are waiting, new element is handed over to the oldest
     * waiter rather than being added to the stack.
     * @param element object to become head
     * @return true if add operation successful
     */
    public boolean add(final T element) {
        if (_list.size() == _capacity) {
            throw new IllegalStateException("Stack is full.");
        }
        final ReentrantLock lock = _lock;
        lock.lock();
        ExchangeCountDownLatch < T > latch = null;
        try {
            if (_waiters.size() > 0) {
                latch = _waiters.removeFirst();
                latch.setItem(element);
            } else {
                _list.addFirst(element);
            }
        } finally {
            lock.unlock();
        }
        if (latch != null) {
            latch.countDown();
        }
        return true;
    }
    
    /**
     * @param timeout how long to wait (specified in unit)
     * @param unit the timeout unit
     * @return the head of this stack, or null if timed out. 
     * @throws InterruptedException if thread is interrupted
     */
    public T poll(final long timeout, final TimeUnit unit) throws InterruptedException {
        T result = null;
        final ReentrantLock lock = _lock;
        boolean error = true;
        lock.lock();
        try {
            if (_list.size() == 0 && timeout > 0) {
                ExchangeCountDownLatch < T > latch = new ExchangeCountDownLatch < T >(1);
                _waiters.addLast(latch);
                lock.unlock();
                if (!latch.await(timeout, unit)) {
                    lock.lock();
                    _waiters.remove(latch);
                    lock.unlock();
                }
                result = latch.getItem();
            } else {
                result = _list.removeFirst();
                lock.unlock();
            }
            error = false;
        } finally {
            if (error && lock.isHeldByCurrentThread()) {
                lock.unlock();
            }
        }
        return result;
    }
    
    /**
     * @return the initial stack capacity minus the current size
     */
    public int remainingCapacity() {
        return _capacity - _list.size();
    }
    
    /**
     * @return the stack size
     */
    public int size() {
        return _list.size();
    }

    /**
     * A class which associates an object item with the count down latch. This allows
     * the item to passed (exchanged) from the adder thread to the polling thread.
     */
    protected class ExchangeCountDownLatch < E > extends CountDownLatch {

        /** The item object to be passed from thread to thread. */
        protected volatile E _item;
        
        /**
         * @param i the initial latch counter
         */
        public ExchangeCountDownLatch(final int i) {
            super(i);
        }
        
        /**
         * @return the item object to be passed from thread to thread
         */
        public E getItem() {
            return _item;
        }
        
        /**
         * @param item the item object to be passed from thread to thread
         */
        public void setItem(final E item) {
            this._item = item;
        }
    }

    /**
     * Iterator safe from concurrent modification exceptions.
     *
     */
    protected class FairIterator implements Iterator < T > {
        /** A copy of all elements in memory. */
        private T[] elements = null;
        
        /** The current index. */
        private int index;
        
        /** The current element.*/
        private T element = null;

        /**
         * At construction time the content of the list is copied over in
         * a thread safe fashion. This is the only moment when the in-memory 
         * array is guaranteed to reflect the stack content.
         */
        @SuppressWarnings("unchecked")
        public FairIterator() {
            final ReentrantLock lock = BlockingStack.this._lock;
            lock.lock();
            try {
                elements = (T[]) new Object[BlockingStack.this._list.size()];
                BlockingStack.this._list.toArray(elements);
                index = 0;
            } finally {
                lock.unlock();
            }
        }
        
        /** {@inheritDoc} */
        public boolean hasNext() {
            return index < elements.length;
        }

        /** {@inheritDoc} */
        public T next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            element = elements[index++];
            return element;
        }

        /** {@inheritDoc} */
        public void remove() {
            final ReentrantLock lock = BlockingStack.this._lock;
            lock.lock();
            try {
                if (element != null) {
                    BlockingStack.this._list.remove(element);
                }
            } finally {
                lock.unlock();
            }
        }

    }
    /**
     * @return stack maximum size
     */
    public int getCapacity() {
        return _capacity;
    }

    /**
     * @return the internal list of stack elements
     */
    public LinkedList < T > getElementsList() {
        return _list;
    }
    
    
    
    /**
     * @return an iterator through the stack
     */
    public Iterator < T > iterator() {
        return new FairIterator();
    }
    
    /**
     * Check if stack contains an element.
     * @param element element to check
     * @return true if element is in stack
     */
    public boolean contains(final T element) {
        final ReentrantLock lock = _lock;
        lock.lock();
        try {
            return getElementsList().contains(element);
        } finally {
            lock.unlock();
        }
    }

    /**
     * @return waiters list
     */
    public LinkedList < ExchangeCountDownLatch < T > > getWaiters() {
        return _waiters;
    }

}
