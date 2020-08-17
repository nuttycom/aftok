# Aftok Client Webapp

### Initial Setup

To install the toolchain locally (reads `devDependencies` from `package.json`):

```sh
npm install
```
### Building

You can now build the PureScript source code with:

```sh
spago build
```

### Development Cycle

To create a minified bundle for deployment, run: 

```sh
npm run build-prod
```

Parcel output appears in the `./dist/` directory.
