function detect_click(el) {
  Shiny.onInputChange('clicked', el.innerText);
}