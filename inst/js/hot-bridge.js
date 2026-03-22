// Custom Shiny-to-Handsontable message handlers
// Replaces fork-only functions from s-balandine/rhandsontable

Shiny.addCustomMessageHandler('hot-load-data', function(msg) {
  var el = document.getElementById(msg.id);
  if (!el) return;
  var widget = HTMLWidgets.find('#' + msg.id);
  if (widget && widget.hot) {
    widget.hot.loadData(msg.data);
  }
});

Shiny.addCustomMessageHandler('hot-render', function(msg) {
  var el = document.getElementById(msg.id);
  if (!el) return;
  var widget = HTMLWidgets.find('#' + msg.id);
  if (widget && widget.hot) {
    widget.hot.render();
  }
});

Shiny.addCustomMessageHandler('hot-select-cell', function(msg) {
  var widget = HTMLWidgets.find('#' + msg.id);
  if (widget && widget.hot) {
    widget.hot.selectCell(msg.row, msg.col);
  }
});

Shiny.addCustomMessageHandler('hot-filter', function(msg) {
  var widget = HTMLWidgets.find('#' + msg.id);
  if (widget && widget.hot) {
    var plugin = widget.hot.getPlugin('filters');
    if (plugin) {
      plugin.clearConditions(msg.column);
      if (msg.filter && msg.filter !== '') {
        plugin.addCondition(msg.column, 'contains', [msg.filter]);
      }
      plugin.filter();
    }
  }
});
