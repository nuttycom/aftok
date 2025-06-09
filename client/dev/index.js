// Use dynamic import to load the PureScript module
import("../output/Main/index.js").then(module => {
  module.main();
});
