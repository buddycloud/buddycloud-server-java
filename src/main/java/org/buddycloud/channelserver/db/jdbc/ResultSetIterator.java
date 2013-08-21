package org.buddycloud.channelserver.db.jdbc;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.apache.log4j.Logger;
import org.buddycloud.channelserver.db.CloseableIterator;

/**
 * Wraps a {@link ResultSet} to provide an {@link Iterator}, converting each row
 * using a given converter object.
 * <p>
 * The {@link ResultSet} and its {@link Statement} will be automatically closed
 * when the iterator returns the last item.
 * <p>
 * <b>NOTE: If the calling code does not reach the end of the iterator then
 * {@link #close()} MUST be called explicitly in order to free up database
 * resources</b>
 * 
 * @param <T>
 *            the return type for each row.
 */
class ResultSetIterator<T> implements CloseableIterator<T> {

	private static final Logger logger = Logger
			.getLogger(ResultSetIterator.class);

	/**
	 * An object to get the data from the current row of a {@link ResultSet} and
	 * return an object of type T.
	 * 
	 * @param <T>
	 */
	public interface RowConverter<T> {
		T convertRow(ResultSet rs) throws SQLException;
	}

	private ResultSet resultSet;
	private RowConverter<T> converter;

	private Boolean hasNextItem = null;

	public ResultSetIterator(final ResultSet resultSet,
			final RowConverter<T> converter) {
		this.resultSet = resultSet;
		this.converter = converter;
	}

	@Override
	public boolean hasNext() {
		if (hasNextItem == null) {
			try {
				hasNextItem = resultSet.next();

				if (!hasNextItem) {
					close();
				}
			} catch (SQLException e) {
				logger.error(
						"SQLException thrown while determining if next item available",
						e);
				hasNextItem = false;
			}
		}

		return hasNextItem;
	}

	@Override
	public T next() {
		if (!hasNext()) {
			throw new NoSuchElementException("No more results");
		}

		try {
			T item = converter.convertRow(resultSet);

			hasNextItem = null;

			return item;
		} catch (SQLException e) {
			throw new NoSuchElementException("No more results");
		}
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}

	public void close() {
		try {
			if (false == resultSet.isClosed())
				resultSet.getStatement().close();
		} catch (SQLException e) {
			logger.error("SQLException thrown while closing statement", e);
		}
	}
}
