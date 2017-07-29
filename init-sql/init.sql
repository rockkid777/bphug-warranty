create database warranties;
connect warranties
create table warranties (uuid BINARY(16) NOT NULL primary key,
    name VARCHAR(54), expiry_date DATE, price INT);
