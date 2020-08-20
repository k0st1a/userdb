CREATE DATABASE IF NOT EXISTS userdb_test;
USE userdb_test;
CREATE TABLE IF NOT EXISTS user (
    id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    user char(25) NOT NULL,
    password char(25) NOT NULL,
    UNIQUE (user)
) ENGINE=InnoDB;
