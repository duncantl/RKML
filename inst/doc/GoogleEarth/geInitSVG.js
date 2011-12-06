google.load("earth", "1");

var ge = null;

var kmzFile = 'http://www.omegahat.org/RKML/Examples/CityTemperatures.kmz';
//var kmzFile = 'http://localhost/CityTemperatures.kmz';

function init() {
  google.earth.createInstance("myGE", initCallback, failureCallback);
}

function initCallback(object) {
  ge = object;
  ge.getWindow().setVisibility(true);
  ge.getNavigationControl().setVisibility(ge.VISIBILITY_AUTO);
  google.earth.fetchKml(ge, kmzFile, fetchCallback);
}

function fetchCallback(obj) {
 alert("loaded " + kmzFile);
    ge.getFeatures().appendChild(obj);
    setView(-112, 37);
}

function failureCallback(object) { alert("Failed to start Google Earth"); }

function setView(long, lat) {
   var lookAt = ge.getView().copyAsLookAt(ge.ALTITUDE_ABSOLUTE);/* ALTITUDE_RELATIVE_TO_GROUND */
   lookAt.setLongitude(long);
   lookAt.setLatitude(lat);
   lookAt.setAltitude(100); /* metres */
   ge.getView().setAbstractView(lookAt);
}
