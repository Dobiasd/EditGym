//source: http://stackoverflow.com/questions/11582512/how-to-get-url-parameters-with-javascript
function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}

function getURLParameterDef(name, def) {
  var val = getURLParameter(name);
  if (val)
    return val;
  return def;
}

// Prevent the backspace key from navigating back.
// http://stackoverflow.com/questions/1495219/how-can-i-prevent-the-backspace-key-from-navigating-back
function DisableBackspaceNavigation() {
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

function Init() {
  var page = getURLParameterDef("page", "start");

  var mainDiv = document.getElementById('main');

  if (page == "levels")
    elmContent = Elm.embed(Elm.Levels, mainDiv, {});
  else if (page == "highscores")
    elmContent = Elm.embed(Elm.Highscores, mainDiv, {});
  else if (page == "game")
  {
    DisableBackspaceNavigation();
    elmContent = Elm.embed(Elm.Game, mainDiv, {level : getURLParameterDef("level", "")});
  }
  else if (page == "help")
    elmContent = Elm.embed(Elm.Help, mainDiv, {});
  else if (page == "contact")
    elmContent = Elm.embed(Elm.Contact, mainDiv, {});
  else if (page == "faq")
    elmContent = Elm.embed(Elm.FAQ, mainDiv, {});
  else
  {
    elmContent = Elm.embed(Elm.Start, mainDiv, {});
    var baseURL = document.location.href.split('?')[0];
    history.replaceState({}, "Edit Gym", baseURL);
  }
}