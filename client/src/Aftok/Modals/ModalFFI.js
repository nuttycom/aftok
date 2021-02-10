exports.toggleModalInternal = modalId => toggle => () => {
  $('#' + modalId).modal(toggle)
}
