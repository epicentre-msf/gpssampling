LeafletWidget.methods.addEasyButtonBar = function(buttons, position, id) {

  var options = {};

  options.position = position;

  // only add ID if provided
  if(id) {
    options.id = id;
  }

  var easyButtons = [];
  for(var i=0; i < buttons.length; i++) {
    easyButtons[i] = getEasyButton(buttons[i]);
  }
  L.easyBar(easyButtons, options).addTo(this);

};
