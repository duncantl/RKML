google.load("earth", "1");

var ge = null;

function init() {
  google.earth.createInstance("myGE", initCallback, failureCallback);
}

function initCallback(object) {
  ge = object;
  ge.getWindow().setVisibility(true);
  ge.getNavigationControl().setVisibility(ge.VISIBILITY_AUTO);
  google.earth.fetchKml(ge, 'http://www.omegahat.org/RKMLDevice/boxplots.kmz', fetchCallback);
}

function fetchCallback(obj) {
    alert("Should be loading " + obj);
    ge.getFeatures().appendChild(obj);
}

function failureCallback(object) { alert("Failed to start Google Earth"); }
