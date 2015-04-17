create table users (
  id serial primary key,
  handle text not null,
  btc_addr text not null,
  email text not null  
);

create table projects (
  id serial primary key,
  project_name text not null,
  inception_date timestamp with time zone not null,
  initiator_id integer references users (id) not null
);

create table project_companions (
  project_id integer references projects(id) not null,
  companion_id integer references users(id) not null
);

create type event_t as enum ('start', 'stop');

create table work_events (
  id serial primary key,
  project_id integer references projects(id) not null,
  user_id integer references users(id) not null,
  btc_addr text not null,
  event_type event_t not null,
  event_time timestamp with time zone not null,
  event_metadata json not null
);

create table auctions (
  id serial primary key,
  project_id integer references projects(id) not null,
  initiator_id integer references users (id) not null,
  raise_amount numeric not null,
  end_time timestamp with time zone not null
);

create table bids (
  id serial primary key,
  auction_id integer references projects (id) not null,
  bidder_id integer references users (id) not null,
  bid_seconds integer not null,
  bid_amount numeric not null,
  bid_time timestamp with time zone not null
);
