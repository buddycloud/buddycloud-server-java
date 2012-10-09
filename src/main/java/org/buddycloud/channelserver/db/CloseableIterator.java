package org.buddycloud.channelserver.db;

import java.util.Iterator;

/**
 * An extension to iterator which provides a mechanism to free up resources once the calling code
 * has finished using the iterator.
 * <p>The {@link #close()} method will be implicitly called when the iterator reaches the end.
 * <p><b>NOTE: If the calling code does not reach the end of the iterator then {@link #close()} SHOULD be called explicitly in
 * order to free up resources as soon as possible.</b>
 *
 * @param <T> the class of objects to be returned by the iterator.
 */
public interface CloseableIterator<T> extends Iterator<T> {
	/**
	 * Closes the iterator and frees up any resources which the iterator my be holding open.
	 * <p>Will be implicitly called if the iterator reaches the end.
	 * <p>It is safe to call this method even if the resources have already been closed. In this case the method will have no effect.
	 */
	void close();
}
