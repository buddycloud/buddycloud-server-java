package org.buddycloud.channelserver.db.jedis;

import java.util.Iterator;

class CastingIterator<F, T> implements Iterator<T>
{
	Iterator<F> fromIt;
     
     public CastingIterator(Iterator<F> fromIt) {
          this.fromIt = fromIt;
     }
     
     @Override
     public boolean hasNext() {
          return fromIt.hasNext();
     }

     @Override
     public T next() {
          return (T) fromIt.next();
     }

     @Override
     public void remove() {
          fromIt.remove();
     }
}