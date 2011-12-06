// alert("defining highlightPoint");

function highlightPoint(id)
{
//    alert("In SVG highlightPoint " + id);
   
    var nid;
    nid = id.replace(/quake/, "pt");
    var el = document.getElementById(nid);
    alert("element " + el);
    try {el.setAttribute("fill", "red")); } catch(err) {alert("Failed to change fill" + err);}
}

 // make this accessible as the variable hi in the HTML document.
parent.hi = highlightPoint;
// alert("SVG loaded");

