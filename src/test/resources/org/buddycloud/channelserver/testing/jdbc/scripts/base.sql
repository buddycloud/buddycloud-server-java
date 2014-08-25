CREATE TABLE "nodes" ("node" TEXT NOT NULL PRIMARY KEY);
CREATE TABLE "node_config" ("node" TEXT NOT NULL REFERENCES "nodes" ("node") ON DELETE CASCADE,
       	     		  "key" TEXT,
			  "value" TEXT,
			  "updated" TIMESTAMP,
			  PRIMARY KEY ("node", "key"));
CREATE TABLE "items" ("node" TEXT REFERENCES "nodes" ("node") ON DELETE CASCADE,
       	     	    "id" TEXT NOT NULL,
		    "updated" TIMESTAMP,
		    "xml" TEXT,
		    "in_reply_to" TEXT,
		    PRIMARY KEY ("node", "id"));
CREATE INDEX "items_updated" ON "items" ("updated");
CREATE INDEX "items_in_reply_to" ON "items" ("node", "in_reply_to");

CREATE TABLE "subscriptions" ("node" TEXT REFERENCES "nodes" ("node") ON DELETE CASCADE,
       	     		    "user" TEXT,
			    "listener" TEXT,
			    "subscription" TEXT,          
 			    "updated" TIMESTAMP,
 			    "temporary" BOOLEAN DEFAULT FALSE,
                "invited_by" TEXT,
			    PRIMARY KEY ("node", "user"));
CREATE INDEX "subscriptions_updated" ON "subscriptions" ("updated");
CREATE TABLE "affiliations" ("node" TEXT REFERENCES "nodes" ("node") ON DELETE CASCADE,
       	     		   "user" TEXT,
			   "affiliation" TEXT,
 			   "updated" TIMESTAMP,
			   PRIMARY KEY ("node", "user"));
CREATE INDEX "affiliations_updated" ON "affiliations" ("updated");


CREATE VIEW "open_nodes" AS
       SELECT DISTINCT "node" FROM "node_config"
       	      WHERE "key"='accessModel'
	        AND "value"='open';

CREATE TABLE online_users ("user" TEXT NOT NULL REFERENCES nodes (node),
			  updated TIMESTAMP);

-- Upgrade 2

-- MIXED IN ABOVE

-- Upgrade 3

-- MIXED IN ABOVE

-- Upgrade 4

-- MIXED IN ABOVE

-- Upgrade 7

-- MIXED IN ABOVE
