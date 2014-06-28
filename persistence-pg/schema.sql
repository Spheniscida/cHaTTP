
-- users table.
CREATE TABLE chattp_users
(
    user_id SERIAL NOT NULL,
    user_name VARCHAR(50) NOT NULL,
    user_email VARCHAR(50) NOT NULL,
    user_password VARCHAR(72) NOT NULL, -- max. bf password length
    user_settings TEXT,

    PRIMARY KEY (user_id),
    UNIQUE (user_id),
    UNIQUE (user_name)
);

-- We select mainly over user_name
CREATE INDEX chattp_users_user_name_ix ON chattp_users (user_name);

CREATE TABLE chattp_messages
(
    receiver INTEGER NOT NULL,
    sender INTEGER NOT NULL,
    timestamp VARCHAR(40) NOT NULL, -- The timestamp is handled only by the client, we don't need to parse it
    group_message BOOLEAN,
    body TEXT,

    FOREIGN KEY (receiver) REFERENCES chattp_users(user_id),
    FOREIGN KEY (sender) REFERENCES chattp_users(user_id)
);

-- We select mainly over receiver
CREATE INDEX chattp_messages_receiver_ix ON chattp_messages (receiver);

CREATE TABLE chattp_brokers
(
    broker_id SERIAL NOT NULL,
    broker_name VARCHAR(75) NOT NULL,

    PRIMARY KEY (broker_id),
    UNIQUE (broker_id),
    UNIQUE (broker_name)
);

-- Our main constraint will be the name
CREATE INDEX chattp_brokers_broker_name_ix ON chattp_brokers (broker_name);

CREATE TABLE chattp_locations
(
    user_id INTEGER NOT NULL,
    broker_id INTEGER NOT NULL,
    channel_id CHARACTER(64) NOT NULL,
    last_heartbeat TIMESTAMP WITH TIMEZONE DEFAULT CURRENT_TIMESTAMP(),

    FOREIGN KEY (user_id) REFERENCES chattp_users (user_id),
    FOREIGN KEY (broker_id) REFERENCES chattp_brokers (broker_id),
    UNIQUE (channel_id)
);

CREATE INDEX chattp_locations_user_id_ix ON chattp_locations (user_id);
-- Channel_ID index is created automatically (UNIQUE constraint)

