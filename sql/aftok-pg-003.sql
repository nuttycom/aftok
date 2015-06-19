create table invitations (
  id uuid primary key default uuid_generate_v4(),
  project_id uuid references projects(id) not null,
  invitor_id uuid references users (id) not null,
  invitee_email text not null,
  invitation_key text not null,
  invitation_time timestamp with time zone not null default (now() at time zone 'UTC'),
  acceptance_time timestamp with time zone
);
