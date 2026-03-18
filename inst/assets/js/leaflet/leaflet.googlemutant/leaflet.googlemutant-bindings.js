LeafletWidget.methods.addGoogleTiles = function(layerId, group, options) {
  (function(){
    var map = this;

		var google = L.gridLayer.googleMutant({
			maxZoom: 24,
			type: options.type
		});

		map.layerManager.addLayer(google, "tile", layerId, group);

  }).call(this);
};
