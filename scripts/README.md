# Aftok API Scripts

Bash scripts for exercising the Aftok API endpoints.

## Setup

1. Copy `.env.example` to `.env` and configure for your environment:
   ```bash
   cp .env.example .env
   ```

2. Edit `.env` to set your server host:
   ```bash
   # For local minikube development:
   AFTOK_HOST="$(minikube ip):$(kubectl get svc aftok-dev-nginx -n aftok-dev -o jsonpath='{.spec.ports[0].nodePort}')"

   # For local docker-compose:
   AFTOK_HOST="localhost:8000"

   # For production:
   AFTOK_HOST="aftok.com"
   ```

## Authentication

The scripts use cookie-based authentication. You must log in first before using authenticated endpoints.

### Login
```bash
# Set credentials in .env or enter interactively
./login.sh
```

This saves session cookies to `cookies.txt`. All subsequent scripts will use these cookies for authentication.

### Logout
```bash
./logout.sh
```

This clears the session and removes `cookies.txt`.

## Available Scripts

### Unauthenticated Endpoints
- `create_user.sh` - Register a new user account
- `check_zaddr.sh` - Validate a Zcash address

### Project Management
- `list_projects.sh` - List all projects for the authenticated user
- `get_project.sh` - Get project details by ID
- `get_project_detail.sh` - Get detailed project info including contributors
- `create_project.sh` - Create a new project

### Time Logging
- `log_start.sh` - Start logging time on a project
- `log_end.sh` - Stop logging time on a project
- `amend.sh` - Amend a logged event's timestamp
- `list_user_events.sh` - List your logged events for a project
- `list_user_intervals.sh` - List your work intervals for a project
- `list_project_intervals.sh` - List all work intervals for a project

### Billing
- `create_project_billable.sh` - Create a billable item for a project
- `list_project_billables.sh` - List billables for a project
- `create_payment_request.sh` - Create a payment request for a billable
- `list_project_payouts.sh` - List payouts for a project

### Auctions
- `create_auction.sh` - Create a fundraising auction for a project

### Invitations
- `invite.sh` - Invite a user to join a project

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `AFTOK_HOST` | Server hostname (without protocol) | `aftok.com` |
| `AFTOK_USER` | Username for login | (prompted) |
| `AFTOK_PASS` | Password for login | (prompted) |
| `ALLOW_INSECURE` | Set to `-k` for self-signed certs | (empty) |
| `PID` | Default project UUID | (prompted) |

## Example Workflow

```bash
# 1. Configure environment
cp .env.example .env
vim .env  # Set AFTOK_HOST for your environment

# 2. Create an account (if needed)
./create_user.sh

# 3. Log in
./login.sh

# 4. Create a project
./create_project.sh

# 5. List your projects to get the project ID
./list_projects.sh

# 6. Start logging time (set PID env var or enter interactively)
export PID="your-project-uuid"
./log_start.sh

# 7. Stop logging time
./log_end.sh

# 8. View your logged intervals
./list_user_intervals.sh

# 9. Log out when done
./logout.sh
```
