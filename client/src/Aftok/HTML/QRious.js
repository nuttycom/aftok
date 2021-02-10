"use strict"

var QRious = require('qrious');

exports.renderQRInternal = options => () => {
  var qrious = new QRious(options);
  return qrious.toDataURL();
}
