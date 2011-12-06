google.load("earth", "1");

var ge = null;

function init() {
  google.earth.createInstance("myGE", initCallback, failureCallback);
}

function initCallback(object) {
  ge = object;
  ge.getWindow().setVisibility(true);
}

function failureCallback(object) { alert("Failed to start Google Earth"); }
