-- create extension if not exists "uuid-ossp";

create table users (
  id uuid primary key default uuid_generate_v4(),
  handle text not null,
  btc_addr text not null,
  email text not null  
);

create table projects (
  id uuid primary key default uuid_generate_v4(),
  project_name text not null,
  inception_date timestamp with time zone not null,
  initiator_id uuid references users (id) not null
);

create table project_companions (
  project_id uuid references projects(id) not null,
  user_id uuid references users(id) not null
);

create type event_t as enum ('start', 'stop');

create table work_events (
  id uuid primary key default uuid_generate_v4(),
  project_id uuid references projects(id) not null,
  user_id uuid references users(id) not null,
  btc_addr text not null,
  event_type event_t not null,
  event_time timestamp with time zone not null,
  event_metadata json not null
);

create table auctions (
  id uuid primary key default uuid_generate_v4(),
  project_id uuid references projects(id) not null,
  initiator_id uuid references users (id) not null,
  raise_amount numeric not null,
  end_time timestamp with time zone not null
);

create table bids (
  id uuid primary key default uuid_generate_v4(),
  auction_id uuid references projects (id) not null,
  bidder_id uuid references users (id) not null,
  bid_seconds integer not null,
  bid_amount numeric not null,
  bid_time timestamp with time zone not null
);
