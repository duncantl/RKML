google.load("earth", "1");

var ge = null;

//var kmzFile = 'file://localhost/Users/duncan/Books/XMLTechnologies/Rpackages/RKML/inst/samples/EmbeddedGE/GEEvents/quakes.kml';
var kmzFile = 'http://www.omegahat.org/quakes.kml';

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
  // obj is a KmlDocument
  // alert("loaded " + kmzFile + " " + obj.getType());
  ge.getFeatures().appendChild(obj);
  setView(-105, -4.5);
  addEventHandlers(ge);
}

function showSVGPoint(event) {
    var id =  event.getTarget().getId();
//    alert("showSVGPoint "+ event + " " + id + " " + parent.document);
//    var e = parent.document.embeds[1]; //.getSVGDocument();
    var e = parent.document.getElementById('PlotID');

//    var e = parent.document.embeds[1].getSVGDocument();
//    alert("SVG Doc = " + e.getSVGDocument() + ", contentDocument = " + e.contentDocument);
//    alert("Seeing into the SVG doc's highlighPoint(): " + e.getSVGDocument().highlightPoint);

//    var sdoc = e.getSVGDocument();

    alert("hi = " + parent.hi);
    if(parent.hi) {
	parent.hi(id);
    } else {
       alert("SVG doc " + parent.document.embeds.length + " " + e + " id = '" + e.id + "'"); // + e.getSVGDocument());
    }
     // don't popup the usual window for the placemark.
    event.preventDefault();
}

function addEventHandlers(gobj)
{
       // here we loop over the KmlPlacemark objects
       // and add an event handler to each for the click event
   var placemarks = ge.getElementsByType('KmlPlacemark');
   var i;
   // alert( " # of placemarks " + placemarks.getLength());
   for (i = 0; i < placemarks.getLength(); ++i) {
      var pl = placemarks.item(i);
       // if(i == 0)
       //    alert("i = 0 " + pl + " " + pl.getType() + " " + google.earth.addEventListener);
       google.earth.addEventListener(pl, 'click', showSVGPoint);
   }


    var marker;
/*
    var fs = gobj.getFeatures();
    var tmp = fs.getChildNodes();
    var count;
    count = tmp.item(0);
// .getFirstChild().getLength();
    alert("adding event handlers to " + count + " elements");
    alert("?" + count.getChildNodes() + " !");
    for(marker in tmp.getChildNodes()) {
	google.maps.event.addListener(marker, 'click', showSVGPoint)
    }
*/

/*
    var i, id;
    var el;
    alert("adding event handlers");
    for(i = 1; i <= 1047; i++) {
        id = "quake_" + i;
	el = ge.getElementById(id);
	if(i == 1) {
	    alert(id + " -> " + el);
	}
        if(el)
   	   google.earth.addEventListener(el, 'click', showSVGPoint);
    }
    alert("Finished adding event handlers " + i);
*/


//   alert("Finished adding event handlers " + i);

}

function failureCallback(object) { alert("Failed to start Google Earth"); }

function setView(long, lat) {
   var lookAt = ge.getView().copyAsLookAt(ge.ALTITUDE_ABSOLUTE);/* ALTITUDE_RELATIVE_TO_GROUND */
   lookAt.setLongitude(long);
   lookAt.setLatitude(lat);
   lookAt.setAltitude(10); /* metres */
   ge.getView().setAbstractView(lookAt);
}
