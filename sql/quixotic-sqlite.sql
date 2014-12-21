CREATE TABLE work_events (btc_addr TEXT, event_type TEXT, event_time INTEGER);
CREATE TABLE auctions (raise_amount INTEGER, end_time INTEGER);
CREATE TABLE bids (auction_id INTEGER, user_id INTEGER, bid_seconds INTEGER, bid_amount INTEGER, bid_time INTEGER);
create table users (btc_addr TEXT, email TEXT);
