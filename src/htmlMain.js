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

// http://stackoverflow.com/questions/13562041/jquery-how-to-stop-propagation-of-ctrl-a
function DisableCtrlAAndTab() {
  jQuery(document).keydown(function(e) {
    if (e.ctrlKey) {
      if (e.keyCode == 65 || e.keyCode == 97 ) { // 'A' or 'a'
        e.preventDefault();
      }
    }
    else
    {
      if ( e.keyCode == 9 ) { // or tab
        e.preventDefault();
      }
    }
  });
}


$(document).keydown(function(objEvent) {
    if (objEvent.keyCode == 9) {  //tab pressed
        objEvent.preventDefault(); // stops its action
    }
})



function Init() {
  var page = getURLParameterDef("page", "start");

  var mainDiv = document.getElementById('main');

  if (page == "levels")
    elmContent = Elm.embed(Elm.Levels, mainDiv, {});
  else if (page == "highscores")
    elmContent = Elm.embed(Elm.Highscores, mainDiv, {});
  else if (page == "game")
  {
    level = getURLParameterDef("level", "");
    loadingTextStart = "loading ...";
    loadingTextGoal = "... loading";
    elmContent = Elm.embed(Elm.Game, mainDiv,
                            { start : loadingTextStart
                            , goal : loadingTextGoal });
    elmContent.ports.start.send(loadingTextStart);
    elmContent.ports.goal.send(loadingTextGoal);

    levelBaseUrl = "levels/" + level + "/";
    startUrl = levelBaseUrl + "start.txt";
    goalUrl = levelBaseUrl + "goal.txt";

    // Gives "syntax error" in firefox console when running locally.
    // http://stackoverflow.com/questions/6470567/jquery-load-txt-file-and-insert-into-div
    $.ajax({
      dataType: "text",
      url: goalUrl,
      success: function(data) {
        data = data.replace(/(\r\n)/gm,"\n");
        data = data.replace(/(\r)/gm,"\n");
        elmContent.ports.goal.send(data);
      },
      error: function(jqXHR, textStatus, errorThrown) {
        msg = "Unable to load " + goalUrl + "\n"
                + textStatus + "\n" + errorThrown;
        elmContent.ports.goal.send(msg);
      }
    });

    $.ajax({
      dataType: "text",
      url: startUrl,
      success: function(data) {
        data = data.replace(/(\r)/gm,"");
        elmContent.ports.start.send(data);
      },
      error: function(jqXHR, textStatus, errorThrown) {
        msg = "Unable to load " + startUrl + "\n"
                + textStatus + "\n" + errorThrown;
        elmContent.ports.start.send(msg);
      }
    });

    DisableBackspaceNavigation();
    DisableCtrlAAndTab();
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