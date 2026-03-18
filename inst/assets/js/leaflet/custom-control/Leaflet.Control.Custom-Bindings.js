LeafletWidget.methods.addCustomControl = function(options) {
  var map = this;
  L.control.custom(options).addTo(map);
};