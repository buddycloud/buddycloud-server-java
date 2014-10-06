package org.buddycloud.channelserver.db.jdbc;

import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;

import junit.framework.Assert;

import org.buddycloud.channelserver.packetHandler.iq.IQTestHandler;
import org.junit.Test;
import org.xmpp.packet.JID;

public class JDBCNodeStoreOnlineUserTest extends JDBCNodeStoreAbstract {

    private JID jid1 = new JID("user@server/resource");
    private JID jid2 = new JID("user@server/other-resource");
    private JID jid3 = new JID("user2@server/resource");

    public JDBCNodeStoreOnlineUserTest() throws SQLException, IOException, ClassNotFoundException {
        dbTester = new DatabaseTester();
        IQTestHandler.readConf();
    }


    @Test
    public void canAddAnOnlineJid() throws Exception {

        store.jidOnline(jid1);

        HashMap<String, Object> expected = new HashMap<String, Object>();

        expected.put("user", jid1.toFullJID());

        dbTester.assertions().assertTableContains("online_users", expected);
    }

    @Test
    public void addingTheSameJidDoesntResultInTwoEntries() throws Exception {

        store.jidOnline(jid1);
        store.jidOnline(jid1);

        HashMap<String, Object> expected = new HashMap<String, Object>();

        expected.put("user", jid1.toFullJID());

        dbTester.assertions().assertTableContains("online_users", expected);
    }

    @Test
    public void canTakeUserOffline() throws Exception {
        store.jidOnline(jid1);
        store.jidOffline(jid1);

        HashMap<String, Object> expected = new HashMap<String, Object>();

        expected.put("user", jid1.toFullJID());

        dbTester.assertions().assertTableContains("online_users", expected, 0);
    }

    @Test
    public void canGetListOfOnlineJids() throws Exception {

        store.jidOnline(jid1);
        Thread.sleep(1);
        store.jidOnline(jid2);
        store.jidOnline(jid3);

        Assert.assertEquals(2, store.onlineJids(jid1).size());
        Assert.assertEquals(2, store.onlineJids(jid2).size());
        Assert.assertEquals(1, store.onlineJids(jid3).size());

        ArrayList<JID> onlineJids = store.onlineJids(jid1);

        Assert.assertEquals(jid1, onlineJids.get(1));
        Assert.assertEquals(jid2, onlineJids.get(0));
        Assert.assertEquals(jid3, store.onlineJids(jid3).get(0));
    }
}
