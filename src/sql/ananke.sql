create table users (
  btc_addr varchar(34) primary key
);

create table users_trusted (
  id serial primary key,
  btc_addr varchar(34) references users (btc_addr) not null,
  trust_interval interval not null
)

create type event_type as enum ('start_work', 'stop_work');

create table timelog (
  id serial primary key,
  btc_addr varchar(34) references users (btc_addr) not null,
  log_time timestamp without time zone not null,
  log_type event_type not null
)
