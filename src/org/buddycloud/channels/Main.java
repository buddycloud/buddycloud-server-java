package org.buddycloud.channels;

import org.xmpp.component.ComponentException;

public class Main {
	
	public static void main(String[] args) {
		
		try {
	
			XmppComponent xmppComponent = new XmppComponent("127.0.0.1",
										  					5347, 
								  							"channels.localhost", 
										  					"julie");
			xmppComponent.run();
			
		} catch (ComponentException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		while (true) {
			try {
				Thread.sleep(5000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}
	
}
