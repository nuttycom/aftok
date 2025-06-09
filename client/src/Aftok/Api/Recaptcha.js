"use strict";

export const getRecaptchaResponseInternal = useElemId => elemId => () => {
  if (useElemId) {
    return grecaptcha.getResponse(elemId);
  } else {
    return grecaptcha.getResponse();
  }
}

export const recaptchaRenderInternal = siteKey => elemId => () => {
  grecaptcha.render(
    document.getElementById(elemId), 
    { 'sitekey': siteKey }
  );
}
