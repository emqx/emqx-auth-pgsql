DROP TABLE IF EXISTS mqtt_acl;

CREATE TABLE mqtt_acl (
                           id SERIAL primary key,
                           allow integer,
                           ipaddr character varying(60),
                           username character varying(100),
                           clientid character varying(100),
                           access  integer,
                           topic character varying(100));

INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
                   VALUES
                   (1,1,'127.0.0.1','u1','c1',1,'t1'),
                   (2,0,'127.0.0.1','u2','c2',1,'t1'),
                   (3,1,'10.10.0.110','u1','c1',1,'t1'),
                   (4,1,'127.0.0.1','u3','c3',3,'t1');

DROP TABLE IF EXISTS mqtt_user;

CREATE TABLE mqtt_user (
                            id SERIAL primary key,
                            is_superuser boolean,
                            username character varying(100),
                            password character varying(100),
                            salt character varying(40));

INSERT INTO mqtt_user (id, is_superuser, username, password, salt)
                     VALUES
                     (1, true, 'plain', 'plain', 'salt'),
                     (2, false, 'md5', '1bc29b36f623ba82aaf6724fd3b16718', 'salt'),
                     (3, false, 'sha', 'd8f4590320e1343a915b6394170650a8f35d6926', 'salt'),
                     (4, false, 'sha256', '5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e', 'salt'),
                     (5, false, 'pbkdf2_password', 'cdedb5281bb2f801565a1122b2563515', 'ATHENA.MIT.EDUraeburn'),
                     (6, false, 'bcrypt_foo', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.HbUIOdlQI0iS22Q5rd5z.JVVYH6sfm6', '$2a$12$sSS8Eg.ovVzaHzi1nUHYK.'),
                     (7, false, 'bcrypt', '$2y$16$rEVsDarhgHYB0TGnDFJzyu5f.T.Ha9iXMTk9J36NCMWWM7O16qyaK', 'salt');