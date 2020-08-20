CREATE DATABASE IF NOT EXISTS userdb_test;
CREATE USER IF NOT EXISTS 'userdb_test'@'%' IDENTIFIED BY 'userdb_test';
GRANT ALL PRIVILEGES ON userdb_test.* TO 'userdb_test'@'%';
FLUSH PRIVILEGES;
