exports.renderQRInternal = selector => content => () => {
  $('#' + selector).kjua(content)
}
