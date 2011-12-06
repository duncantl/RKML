/* This version is used for multiple GE instances. */

google.load("earth", "1");

var ges = {}; //new Array();

var KMLDocs = { "myGE": 'http://www.stat.berkeley.edu/users/nolan/data/KML/cityHousePrices.kmz', "myGE2": 'http://www.stat.berkeley.edu/users/nolan/data/KML/HousesSold.kml'};

/* This function returns a new function that will then be used as the initialization
   callback. It will carry the id of the HTML element into which the GE instance is being placed
   and also the URL of the KML file to be loaded. */
function makeClosure(id, url) {
    return( function(obj) { 
        initCallback(obj, url, id);
    });
}

// Called from the onload attribute in the <body>
function init() {
    for(var id in KMLDocs) {
	google.earth.createInstance(id, makeClosure(id, kmlDocs[id]), failureCallback);
    }
}

// the callback when the GE instance is created. This loads the 
// KML/KMZ files we want.
function initCallback(object, url, id) {
  var ge = object;
  ge.getWindow().setVisibility(true);
  ge.getNavigationControl().setVisibility(ge.VISIBILITY_AUTO);

  ges[id] = ge;

/* Add one or more calls go google.earth.fetchKml() to load kmz or kml files.  Supply a callback
   function that is invoked when . We load the URL specified in the arguments to this function.
     google.earth.fetchKml(ge, 'http://www.stat.berkeley.edu/users/nolan/data/KML/cityHousePrices.kmz',
                             fetchCallback);
*/
  google.earth.fetchKml(ge, url, fetchCallback);

       // The view has the following events:
       // viewchangebegin, viewchange and viewchangeend.
       // We can add event handlers on the Window, Globe
   google.earth.addEventListener(ge.getView(), 'viewchangeend', function() { setViewCB(ge)});
//  google.earth.addEventListener(ge.getView(), 'viewchangeend', clickCB);
//  google.earth.addEventListener(ge.getWindow(), 'click', windowCB);
//  google.earth.addEventListener(ge.getGlobe(), 'mousemove', globeCB);



}

var timer;
function setViewCB(ge)
{
//    realSetView(ge);
    if(timer) {
	clearTimeout(timer);
    }
    timer = setTimeout(function() { realSetView(ge); }, 50);
}

function realSetView(obj)
{
    if(false) {
	var txt = "";
	for(i = 0; i < ges.length; i++) {
	    txt = txt + " " + ges[i];
	}
	alert("foo: " + obj + " " + ges.length + " " + txt);
    }

    var la = obj.getView().copyAsLookAt(obj.ALTITUDE_ABSOLUTE);

    var nla = ges[[1]].createLookAt('');
    nla.set(la.getLatitude(), la.getLongitude(), la.getAltitude(), la.getAltitudeMode(), la.getHeading(), la.getTilt(), la.getRange());
    ges[[1]].getView().setAbstractView(nla);
}

function clickCB(obj)
{
    alert("clickCB: " + obj);
}

function windowCB(obj)
{
    alert("windowCB: " + obj);
}

function globeCB(obj)
{
    alert("globeCB: " + obj);
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
