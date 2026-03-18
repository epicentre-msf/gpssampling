LeafletWidget.methods.addPaintPolygonControl = function(options) {
  var map = this;

  map.paintPolygonControl =  L.control.paintPolygon(options);
  map.paintPolygonControl.addTo(map);
};

LeafletWidget.methods.startDraw = function(options) {
  var map = this;
  if(map.paintPolygonControl) {
    map.paintPolygonControl.options.drawOptions.color = options.color;
    map.paintPolygonControl.options.layerOptions.color = options.color;
    map.paintPolygonControl.startDraw();
  }
};

LeafletWidget.methods.stopDraw = function() {
  var map = this;
  if(map.paintPolygonControl) {
    map.paintPolygonControl.stop();
    var data = map.paintPolygonControl.getData();
    Shiny.onInputChange(map.id+'_paint_data', data);
  }
};

LeafletWidget.methods.startErase = function() {
  var map = this;
  if(map.paintPolygonControl) {
    map.paintPolygonControl.startErase();
  }
};

LeafletWidget.methods.eraseAll = function() {
  var map = this;
  if(map.paintPolygonControl) {
    map.paintPolygonControl.eraseAll();
  }
};

LeafletWidget.methods.setData = function(data) {
  var map = this;
  if(map.paintPolygonControl) {
    map.paintPolygonControl.setData(data.features[[0]]);
  }
};
