
function Init() {
  var mainDiv = document.getElementById('main');
  page = Elm.embed(Elm.Main, mainDiv, {});

  // Prevent the backspace key from navigating back.
  // http://stackoverflow.com/questions/1495219/how-can-i-prevent-the-backspace-key-from-navigating-back
  $(document).unbind('keydown').bind('keydown', function (event) {
      var doPrevent = false;
      if (event.keyCode === 8) {
          var d = event.srcElement || event.target;
          if ((d.tagName.toUpperCase() === 'INPUT' &&
               (
                   d.type.toUpperCase() === 'TEXT' ||
                   d.type.toUpperCase() === 'PASSWORD' ||
                   d.type.toUpperCase() === 'FILE' ||
                   d.type.toUpperCase() === 'EMAIL' ||
                   d.type.toUpperCase() === 'SEARCH' ||
                   d.type.toUpperCase() === 'DATE' )
               ) ||
               d.tagName.toUpperCase() === 'TEXTAREA') {
              doPrevent = d.readOnly || d.disabled;
          }
          else {
              doPrevent = true;
          }
      }
      if (doPrevent) {
          event.preventDefault();
      }
  });
}