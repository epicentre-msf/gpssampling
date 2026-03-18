function(el, x) {

  var map = this;

  var hues = [
    '#eff3ff',
    '#bdd7e7',
    '#6baed6',
    '#3182bd',
    '#08519c'];

  Shiny.addCustomMessageHandler('addAttributionControl',
    function(msg) {
      L.control.attribution().addTo(map);
    }
  );

  Shiny.addCustomMessageHandler('removeAttributionControl',
    function(msg) {
      L.control.attribution().remove();
    }
  );

  Shiny.addCustomMessageHandler('setIndicator',
    function(msg) {

      map.eachLayer(function(layer) {

        if(layer.feature) {

          var value = layer.feature.properties[msg.property];

          if(value < 10) {var division = 5;}
          if(value < 5) {var division = 4;}
          if(value < 4) {var division = 3;}
          if(value < 3) {var division = 2;}
          if(value < 2) {var division = 1;}

          layer.setStyle({
            fillColor: hues[division-1],
            fillOpacity: 0.8,
            weight: 0.5
          });

          var content =
            '<table><th><td>' + layer.feature.properties['adm.1.name'] + '<\/td><\/th>' +
            '<tr><td>Week 2018-W' + msg.property + '<\/td><\/tr>' +
            '<tr><td>CMR: ' + value+ '<\/td><\/tr><\/table>';

          // layer.bindPopup(content);

          var content =
            '<table><th><td>' + layer.feature.properties['adm.1.name'] + '<\/td><\/th>' +
            '<tr><td>Week 2018-W' + msg.property + '<\/td><\/tr>' +
            '<tr><td>CMR: ' + value+ '<\/td><\/tr><\/table>';

          layer.bindLabel(content);

          // layer.bindTooltip('<p style="height:200px; width:200px">static content</p>');
        }

      });

    }
  );

  Shiny.addCustomMessageHandler('redrawMap',
    function(msg) {
      map.eachLayer(function(layer) {
        // layer.redraw();
      }
    )}
  );

}
