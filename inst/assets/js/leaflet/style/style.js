LeafletWidget.methods.setStyle = function (group, styles, labels) {
  var offset = arguments.length <= 3 || arguments[3] === undefined ? 0 : arguments[3];

  window.map = this;
  var layers = this.layerManager.getLayerGroup(group).getLayers();

  if (styles) {
    for (var i = 0; i < styles.length; i++) {
      layers[i + offset].setStyle(styles[i]);
    }
  }
  if (labels) {
    for (var _i = 0; _i < styles.length; _i++) {
      layers[_i + offset].bindTooltip(labels[_i]);
    }
  }
};

/** Much more performant way to style loaded geometry */
LeafletWidget.methods.setStyleFast = function (group, colors, weights, labels, popups, strokes, fills, fill_opacities, radiuses, visibilities) {
  window.map = this;
  var layers = this.layerManager.getLayerGroup(group).getLayers();

  if (labels) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].bindTooltip(labels[i]);
    }
  }

  if (popups) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].bindPopup(popups[i]);
    }
  }

  if (colors) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setStyle({ color: colors[i], fillColor: colors[i] });
    }
  }

  if (weights) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setStyle({ weight: weights[i] });
    }
  }

  if (strokes) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setStyle({ color: strokes[i] });
    }
  }

  if (fills) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setStyle({ fillColor: fills[i] });
    }
  }

  if (fill_opacities) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setStyle({ fillOpacity: fill_opacities[i] });
    }
  }

  if (radiuses) {
    for (var i = 0; i < layers.length; i++) {
      layers[i].setRadius(radiuses[i]);
    }
  }

  if (visibilities) {
    for (var i = 0; i < layers.length; i++) {
      if(!visibilities[i]) {
        layers[i].getElement().classList.add("hided");
      } else {
        layers[i].getElement().classList.remove("hided");
      }
    }
  }
};
