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

//http://stackoverflow.com/questions/1056562/how-do-i-prevent-scrolling-with-arrow-keys-but-not-the-mouse
function DisableCtrlAAndTab() {
  jQuery(document).keydown(function(e) {
    if (e.ctrlKey) {
      if (e.keyCode == 65 || e.keyCode == 97 ) { // 'A' or 'a'
        e.preventDefault();
      }
    }
    // prevent scrolling (mainly in IE) with pgUp, pgDown, end, pos1 and arrows
    if (e.keyCode >= 33 && e.keyCode <= 40 ) {
      e.preventDefault();
    }
    if ( e.keyCode == 9 ) { // tab
      e.preventDefault();
    }
  });
}


$(document).keydown(function(objEvent) {
    if (objEvent.keyCode == 9) {  //tab pressed
        objEvent.preventDefault(); // stops its action
    }
})

function CleanTextData(data) {
  data = data.replace(/(\r\n)/gm,"\n");
  data = data.replace(/(\r)/gm,"\n");
  data = data.replace(/\t/g,"    ");
  return data;
}

// http://stackoverflow.com/questions/6470567/jquery-load-txt-file-and-insert-into-div
// In chrome you have to start with the following flag to make it work: --allow-file-access-from-files
function LoadAndForward(dataUrl, doClean, dest) {
  $.ajax({
    dataType: "text",
    mimeType: "text/plain",
    url: dataUrl,
    success: function(data) {
      if (doClean) {
        data = CleanTextData(data);
      }
      dest.send(data);
    },
    error: function(jqXHR, textStatus, errorThrown) {
      msg = "Unable to load " + dataUrl + "\n"
              + textStatus + "\n" + errorThrown;
      dest.send(msg);
    }
  });
}

function Init() {
  var page = getURLParameterDef("page", "start");

  var mainDiv = document.getElementById('main');

  if (page == "exercises")
    elmContent = Elm.embed(Elm.Exercises, mainDiv, {});
  else if (page == "highscores")
    elmContent = Elm.embed(Elm.Highscores, mainDiv, {});
  else if (page == "newsletter")
    elmContent = Elm.embed(Elm.Newsletter, mainDiv, {});
  else if (page == "create_exercise")
    elmContent = Elm.embed(Elm.CreateExercise, mainDiv, {});
  else if (page == "game")
  {
    exercise = getURLParameterDef("exercise", "");
    loadingTextStart = "loading ...";
    loadingTextGoal = "... loading";
    loadingTextCoach = "...";
    elmContent = Elm.embed(Elm.Game, mainDiv,
                            { start : loadingTextStart
                            , goal : loadingTextGoal
                            , coach : loadingTextCoach });
    elmContent.ports.start.send(loadingTextStart);
    elmContent.ports.goal.send(loadingTextGoal);
    elmContent.ports.coach.send(loadingTextCoach);

    exerciseBaseUrl = "exercises/" + exercise + "/";
    startUrl = exerciseBaseUrl + "start.txt";
    goalUrl = exerciseBaseUrl + "goal.txt";
    coachUrl = exerciseBaseUrl + "coach.txt";

    LoadAndForward(startUrl, true, elmContent.ports.start)
    LoadAndForward(goalUrl, true, elmContent.ports.goal)
    LoadAndForward(coachUrl, true, elmContent.ports.coach)

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