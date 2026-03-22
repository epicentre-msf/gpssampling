// Custom leaflet-pm methods for programmatic control
// Replaces fork-only functions from s-balandine/leafpm
// These are invoked from R via leaflet::invokeMethod()
//
// Deferred: LeafletWidget may not be available when this script first loads
// (depends on htmlwidgets loading order). We wait until it exists.

(function() {
  function register() {
    LeafletWidget.methods.pmToggleDrawMode = function(shape, options) {
      var map = this;
      if (shape === false || shape === null) {
        map.pm.disableDraw();
      } else {
        map.pm.enableDraw(shape, options || {});
      }
    };

    LeafletWidget.methods.pmToggleEditMode = function(targetGroup, options) {
      var map = this;
      map.eachLayer(function(layer) {
        if (layer.pm && layer.options && layer.options.group === targetGroup) {
          if (options === false) {
            layer.pm.disable();
          } else {
            layer.pm.enable(options || {});
          }
        }
      });
    };

    LeafletWidget.methods.pmToggleRemovalMode = function(targetGroup, enabled) {
      var map = this;
      if (enabled === false) {
        map.pm.disableGlobalRemovalMode();
      } else {
        map.pm.enableGlobalRemovalMode();
      }
    };

    LeafletWidget.methods.pmEditFeature = function(targetGroup, targetId, editOptions) {
      var map = this;
      map.eachLayer(function(layer) {
        if (layer.options &&
            layer.options.group === targetGroup &&
            layer.options.layerId === targetId) {
          layer.pm.enable(editOptions || {});
        }
      });
    };

    LeafletWidget.methods.pmClearFeatures = function() {
      var map = this;
      var toRemove = [];
      map.eachLayer(function(layer) {
        if (layer.pm && layer._pmTempLayer) {
          toRemove.push(layer);
        }
      });
      toRemove.forEach(function(layer) {
        map.removeLayer(layer);
      });
    };

    LeafletWidget.methods.pmEditStop = function(targetGroup) {
      var map = this;
      map.eachLayer(function(layer) {
        if (layer.pm && layer.options && layer.options.group === targetGroup) {
          layer.pm.disable();
        }
      });
    };
  }

  if (typeof LeafletWidget !== "undefined") {
    register();
  } else {
    // Wait for LeafletWidget to become available
    var attempts = 0;
    var timer = setInterval(function() {
      attempts++;
      if (typeof LeafletWidget !== "undefined") {
        clearInterval(timer);
        register();
      } else if (attempts > 50) {
        clearInterval(timer);
        console.warn("leafpm-bridge: LeafletWidget not available after 5s");
      }
    }, 100);
  }
})();
