Local Builds with Nix
=====================

The simplest way to get the server building is to use [nix](https://nixos.org/manual/nixos/stable/).

Once you've installed nix, from the root of the project, run:

~~~bash
nix build
~~~

This will download and compile all dependencies, the aftok source, and create a server docker image (located in the `result`
file). To load the resulting docker file into docker, run `docker load < result`. For convenience, this set of steps is
already defined for the `build-server-image` target in the `Makefile`.

Local Development with Helm
===========================

Local development uses Helm and minikube for running the full aftok stack
(server, client, site, PostgreSQL, nginx). All the deployment tooling lives in
the public [aftok-chart](https://github.com/aftok/aftok-chart) repository,
which provides a Nix flake with kubectl, helm, minikube, and helper scripts.

Prerequisites
-------------

- [Nix](https://nixos.org/download/) with flakes enabled
- [Docker](https://docs.docker.com/get-docker/) running

Setting Up the Chart Repository
--------------------------------

Clone the chart repo alongside the server repo. The rebuild scripts expect a
specific directory layout, so place it as a sibling:

~~~
aftok/
├── server/canon/       # this repo (server worktree)
├── client/work/        # client worktree (if doing frontend work)
├── aftok.com/work/     # static site worktree (if doing site work)
└── aftok-chart/        # <-- clone this
~~~

~~~bash
# From the aftok/ parent directory:
git clone https://github.com/aftok/aftok-chart.git
~~~

Setting Up a Local Cluster
--------------------------

~~~bash
cd aftok-chart

# Enter the development shell (provides kubectl, helm, minikube, k9s, etc.)
nix develop

# Start minikube and enable required addons
setup-local-cluster
~~~

This starts a minikube cluster with Docker driver, 4 CPUs, and 8GB RAM, and
enables the ingress, metrics-server, and dashboard addons. It will also print
instructions for configuring `/etc/hosts` so you can access the app at
`http://aftok.local`.

Deploying to the Local Cluster
------------------------------

~~~bash
# From the aftok-chart/ directory, inside `nix develop`:

# Deploy with local dev defaults (builds chart deps, configures PostgreSQL, etc.)
deploy-dev
~~~

This starts minikube if needed, builds chart dependencies, and deploys the full
stack with sensible local defaults (dev passwords, `imagePullPolicy: Never`,
NodePort service on port 30080, mailpit for email capture). After deployment it
prints the URL to access the application.

For custom configuration, create a `values-local.yaml` file and deploy with
helm directly:

~~~bash
build-chart
helm upgrade --install aftok-dev ./aftok \
  --namespace aftok-dev \
  --create-namespace \
  --values values-local.yaml \
  --wait
~~~

See `examples/values-example.yaml` in the chart repo for a complete reference.

Rebuilding After Code Changes
-----------------------------

The chart repo's dev shell provides `rebuild-*` commands that handle the full
cycle: staging git changes (so nix can see them), building images, loading them
into minikube's Docker daemon, and restarting pods.

~~~bash
# From the aftok-chart/ directory, inside `nix develop`:

# Rebuild and redeploy server
rebuild-server ../server/canon aftok-dev

# Rebuild and redeploy client (if working on frontend)
rebuild-client ../client/work aftok-dev

# Rebuild and redeploy static site
rebuild-site ../aftok.com/work aftok-dev

# Rebuild all components
rebuild-all aftok-dev
~~~

The paths above assume the sibling directory layout described in "Setting Up the
Chart Repository". Adjust the first argument if your layout differs.

**Why use `rebuild-*` instead of raw kubectl?** Raw `kubectl delete pod`
commands are unreliable for picking up new images because minikube's Docker
cache may serve stale images. The `rebuild-*` commands solve this by building
fresh images, loading them directly into minikube's Docker daemon, and
force-deleting pods.

Useful Commands
---------------

These are all available inside `nix develop` in the chart repo:

~~~bash
# View application logs
show-logs aftok-dev server
show-logs aftok-dev nginx

# Backup database
backup-db aftok-dev

# Restore database from backup
restore-db aftok-dev ./backups/aftok-backup-aftok-dev-20240101-120000.sql

# Clean up the dev deployment entirely
cleanup-dev

# Open Kubernetes dashboard
minikube dashboard

# Terminal-based Kubernetes UI
k9s
~~~

Database Migrations
-------------------

Database migrations are applied **automatically** when the server starts up.
The server uses the `dbmigrations` library to check for and apply any pending
migrations from the bundled `migrations/` directory before accepting requests.

New migrations can be created with the `moo-postgresql` tool available in
the nix development shell:

~~~bash
nix develop

moo-postgresql new \
  --config-file ./local/server/conf/aftok-server-migrations.cfg \
  kebab-case-descriptive-name
~~~

Migration files use YAML format with `Created`, `Description`, `Depends`,
`Apply`, and `Revert` fields. Files are stored in the `migrations/` directory
with timestamp prefixes.
