var dimension = [0, 0];

$( document ).ready(function() {

  Shiny.addCustomMessageHandler('showid', function(what) {
    $("#" + what).show()
  });

  Shiny.addCustomMessageHandler('hideid', function(what) {
    $("#" + what).hide()
  });

  Shiny.addCustomMessageHandler('showclass', function(what) {
    $("." + what).show()
  });

  Shiny.addCustomMessageHandler('hideclass', function(what) {
    $("." + what).hide()
  });

  Shiny.addCustomMessageHandler('showhref', function(what) {
    $("a[href*=" + what).show()
  });

  Shiny.addCustomMessageHandler('hidehref', function(what) {
    $("a[href*=" + what).hide()
  });

  Shiny.addCustomMessageHandler('clickon', function(what) {
    $(what).click()
  });

  Shiny.addCustomMessageHandler('disable', function(what) {
    $(what).attr('disabled', 'disabled')
  });

  Shiny.addCustomMessageHandler('reable', function(what) {
    $(what).removeAttr('disabled')
  });

  window.onresize = function() {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
  };


});

$( document ).on({


    'shiny:connected': function(event) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    },

    'shiny:value': function(event) {
      Shiny.onInputChange(event.name + "_rendered", [Math.random()]);
    },

    // 'shiny:recalculated': function(event) {
    //   Shiny.onInputChange(event.target.id + "_recalculated", [Math.random()]);
    // },

    // 'shiny:visualchange': function(event) {
    //   Shiny.onInputChange("visualchange", [event.target.id, Math.random()]);
    //   Shiny.onInputChange(event.target.id + "_visualchange", [Math.random()]);
    // },

    'shiny:bound': function(event) {
      if(event.bindingType == 'output') {
        if(event.target.id != 'ui') {
          Shiny.onInputChange(event.target.id + "_bound", [Math.random()]);
        }
      }
      // Shiny.onInputChange(event.target.id + "_bound", [Math.random()]);
    }

    // 'shiny:unbound': function(event) {
    // }

});
