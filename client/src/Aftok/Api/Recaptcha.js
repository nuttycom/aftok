"use strict";

exports.getRecaptchaResponseInternal = useElemId => elemId => () => {
  if (useElemId) {
    return grecaptcha.getResponse(elemId);
  } else {
    return grecaptcha.getResponse();
  }
}

exports.recaptchaRenderInternal = siteKey => elemId => () => {
  grecaptcha.render(
    document.getElementById(elemId), 
    { 'sitekey': siteKey }
  );
}
