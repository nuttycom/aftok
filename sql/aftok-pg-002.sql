alter table projects add depreciation_fn json not null default '{"type": "LinearDepreciation", "arguments": {"undep": 6, "dep": 60}}';

alter table project_companions add invited_by uuid references users(id);
update project_companions set invited_by = user_id; --
alter table project_companions alter column invited_by set not null;

alter table project_companions 
add joined_at timestamp with time zone not null 
default (now() at time zone 'UTC');
