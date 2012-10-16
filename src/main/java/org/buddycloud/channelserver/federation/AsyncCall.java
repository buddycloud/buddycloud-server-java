package org.buddycloud.channelserver.federation;

/**
 * An interface for classes which define code which can be called asyncronously.
 * @author ashleyw
 *
 * @param <T>
 */
public interface AsyncCall<T> {
	public interface ResultHandler<T> {
		void onSuccess(T result);
		void onError(Throwable t);
	}
	
	/**
	 * Initiates the request and will call the appropriate method in the handler when the request has completed.
	 */
	void call(ResultHandler<T> handler);
}
