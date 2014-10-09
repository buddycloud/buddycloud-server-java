package org.buddycloud.channelserver.db;

import java.util.Iterator;

/**
 * Wraps an {@link Iterator} to provide a {@link CloseableIterator} interface with an empty
 * {@link #close()} method.
 *
 * @param <T> the type of the iterator
 */
public class ClosableIteratorImpl<T> implements CloseableIterator<T> {

    private Iterator<T> iterator;

    public ClosableIteratorImpl(final Iterator<T> iterator) {
        this.iterator = iterator;
    }

    @Override
    public boolean hasNext() {
        return iterator.hasNext();
    }

    @Override
    public T next() {
        return iterator.next();
    }

    @Override
    public void remove() {
        iterator.remove();
    }

    @Override
    public void close() {
        // Do nothing
    }

}
