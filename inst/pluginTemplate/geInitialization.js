google.load("earth", "1");

var ge = null;

// Called from the onload attribute in the <body>
function init() {
  google.earth.createInstance("myGE", initCallback, failureCallback);
}

// the callback when the GE instance is created. This loads the 
// KML/KMZ files we want.
function initCallback(object) {
  ge = object;
  ge.getWindow().setVisibility(true);
  ge.getNavigationControl().setVisibility(ge.VISIBILITY_AUTO);

  google.earth.fetchKml(ge, 'XXX', fetchCallback);

/* Add one or more calls go google.earth.fetchKml() to load kmz or kml files.  Supply a callback
   function that is invoked when .
  google.earth.fetchKml(ge, 'http://www.stat.berkeley.edu/users/nolan/data/KML/cityHousePrices.kmz', fetchCallback);
  google.earth.fetchKml(ge, 'http://www.stat.berkeley.edu/users/nolan/data/KML/HousesSold.kml', fetchCallback);
*/
}

// called if creating the Google Earth instance fails.
function failureCallback(object) { 
   alert("Failed to start Google Earth"); 
}

// This is called when the KML/KMZ file is actually downloaded and loaded.
function fetchCallback(obj) {
   // alert("Should be loading " + obj);
   ge.getFeatures().appendChild(obj);
}



/*********************************************************************/

function addPlacemark(long, lat) {
  newPlacemark = ge.createPlacemark('');
  var point = ge.createPoint('');
  point.setLatitude(lat); 
  point.setLongitude(long); 
  newPlacemark.setGeometry(point);
  newPlacemark.setName(" ");
  newPlacemark.setStyleSelector(style);
  myFolder.getFeatures().appendChild(newPlacemark);
}

function removePlacemarks() {
/* This function removes the extra red placemarks from GE */
/* They have been placed in a specil folder to make it easy to remove */
    features = myFolder.getFeatures();
    while (features.getFirstChild())
       features.removeChild(features.getFirstChild());
}
