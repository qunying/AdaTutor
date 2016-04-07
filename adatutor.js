function adat_toggle () {
  var i;
  var el = document.getElementById(arguments[0]);
  el.style.display = el.style.display != 'none' ? 'none' : '';
  if (arguments.length > 1) {
    for (i = 1; i < arguments.length; ++i) {
      el = document.getElementById(arguments[i]);
      el.style.display = 'none';
    }
  }
}
