export const toggleModalInternal = modalId => toggle => () => {
  $('#' + modalId).modal(toggle)
}
