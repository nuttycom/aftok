"use strict"

import { QRious } from 'qrious';

export const renderQRInternal = options => () => {
  var qrious = new QRious(options);
  return qrious.toDataURL();
}
