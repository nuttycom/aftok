Client Project Setup
====================

This is the very minimal set of steps that is necessary to build the
JavaScript artifact for the client site:

```

sudo npm install -g purescript pulp bower
npm install virtual-dom
pulp dep install
pulp browserify --optimise --to dist/aftok.js

```


