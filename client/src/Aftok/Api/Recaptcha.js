"use strict";

exports.getRecaptchaResponseInternal = useElemId => elemId => () => {
  if (useElemId) {
    return grecaptcha.getResponse(elemId);
  } else {
    return grecaptcha.getResponse();
  }
}
