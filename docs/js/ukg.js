/* ================= Set up background ==================== */
bg = {
    shown: new L.tileLayer(''), // the tile layer being shown
    initialise: function(){
	var rb = document.querySelectorAll('input[name="bgRadio"]:checked')[0]
	//console.log(rb);
	this.change(rb);
    },
    change: function(e){
        //console.log(e);
        map.removeLayer(this.shown);
        switch (e.value) {
        case 'osmbw':
	    this.shown = new L.tileLayer("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
                                  {"maxZoom": 17,
                                   "attribution": "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors, &copy; <a href='https://cartodb.com/attributions'>CartoDB</a>"
                                  });
	    break;
	case 'osmtopo':
	    this.shown = new L.tileLayer("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                                    {"maxZoom": 17,
				     "attribution": "Map data: &copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a>, <a href='http://viewfinderpanoramas.org'>SRTM</a> | Map style: &copy; <a href='https://opentopomap.org'>OpenTopoMap</a> (<a href='https://creativecommons.org/licenses/by-sa/3.0/'>CC-BY-SA</a>)"
				    });
	    break;
	case "esri":
	    this.shown = new L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
					 {"attribution": "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"});
	    break;
	}
	//console.log(this.shown)
	map.addLayer(this.shown);
    }
}

/* ========================================== Set up marker layers ========================== */
var mrk = {
    icon: {},
	// "u": new L.icon({
	//     iconUrl: "im/warning_0.svg",
	//     iconSize: [24, 28],iconAnchor: [12, 28],popupAnchor: [0, -25]}),
	// "n": new L.icon({
	//     iconUrl: "im/warning_0.svg",
	//     iconSize: [24, 28],iconAnchor: [12, 28],popupAnchor: [0, -25]}),
	// "d": new L.icon({
	//     iconUrl: "im/warning_1.svg",
	//     iconSize: [24, 28],iconAnchor: [12, 28],popupAnchor: [0, -25]}),
	// "w": new L.icon({
	//     iconUrl: "im/warning_2.svg",
    //     iconSize: [24, 28],iconAnchor: [12, 28],popupAnchor: [0, -25]})},
    stage: L.geoJson(null, {
	filter: function(feat) { return feat.properties.type === 'stage'},
	pointToLayer: function (feature, latlng) {
	    //console.log(feature.properties.stationReference)
	    return L.marker(latlng,{		
		riseOnHover: true,
		title: feature.properties.town})
	},
	onEachFeature: function(feature, layer) {   
	    // add onclick event
	    pstr = '<h4>' + '<a href=series.html?mid='+feature.properties.mid + ' target=_blank>' + feature.properties.town + '</a></h4>';
	    pstr = pstr + '<p><b>River</b>: ' + feature.properties.featureName + '</p>';
	    pstr = pstr + '<p><b>Last Warning</b>: ' + feature.properties.lastWarning + '</p>';
	    pstr = pstr + '<p><b>Last Forecast Warning</b>: ' + feature.properties.lastForecastWarning + '</p>';
	    pstr = pstr + '<p><b>Series</b>: ' + feature.properties.mid + '</p>';
	    layer.bindPopup(pstr);
	}
    }),
    tidal: L.geoJson(null, {
	filter: function(feat) { return feat.properties.type === 'tidal'},
	pointToLayer: function (feature, latlng) {
	    //console.log(feature.properties.stationReference)
	    return L.circleMarker(latlng,{		
		riseOnHover: true,
		title: feature.properties.town})
	},
	onEachFeature: function(feature, layer) {   
	    // add onclick event
	    pstr = '<h4>' + '<a href=./series?mid='+feature.properties.mid + ' target=_blank> Tide Gauge </a></h4>';
	    pstr = pstr + '<p><b>Last Warning</b>: ' + feature.properties.lastWarning + '</p>';
	    pstr = pstr + '<p><b>Last Forecast Warning</b>: ' + feature.properties.lastForecastWarning + '</p>';
	    pstr = pstr + '<p><b>Series</b>: ' + feature.properties.mid + '</p>';
	    layer.bindPopup(pstr);
	}
    }),
    tbr: L.geoJson(null, {
	filter: function(feat) { return feat.properties.type === 'tbr'},
	pointToLayer: function (feature, latlng) {
	    //console.log(feature.properties.stationReference)
	    return L.circleMarker(latlng,{
		radius: 8,
		fillColor: "#ff7800",
		color: "#000",
		weight: 1,
		opacity: 1,
		fillOpacity: 1,
		riseOnHover: true,
		title: feature.properties.town})
	},
	onEachFeature: function(feature, layer) {   
	    // add onclick event
	    pstr = '<h4>' + '<a href=series.html?mid='+feature.properties.mid + ' target=_blank> Rain Gauge </a></h4>';
	    pstr = pstr + '<p><b>Last Warning</b>: ' + feature.properties.lastWarning + '</p>';
	    pstr = pstr + '<p><b>Last Forecast Warning</b>: ' + feature.properties.lastForecastWarning + '</p>';
	    pstr = pstr + '<p><b>Series</b>: ' + feature.properties.mid + '</p>';
	    layer.bindPopup(pstr);
	}
    }),
    update: function(e){
	console.log(e)
	if(e.checked){
	    map.addLayer(mrk[e.value])
	}else{
	    map.removeLayer(mrk[e.value])
	}
	//console.log(e)
    },
    initialise: function(){
	const isOk = response => response.ok ? response.json() : Promise.reject(new Error('Failed to load data from server'))
	fetch("https://flood.waternumbers.com/series") //test_data/stations.json')
	    .then(isOk) // <= Use `isOk` function here
	    .then(function (data) {
		mrk.stage.addData(data)
		mrk.tbr.addData(data)
		mrk.tidal.addData(data)
	    })
	    //.then(data => mrk.waterLevel.addData(data))
	    .catch(error => console.error("station loading error " + error))
	//mrk.change() // get data
	
	var rb = document.querySelectorAll('input[name="mrkInput"]:checked');
	//console.log(rb)
	rb.forEach(function(z){
	    mrk.update(z);
	});
    }
};

/* --------------------------------------------------------*/
/* Code to initialise the map */

var map = L.map('map',{
    zoom: 7,
    maxZoom: 16,
    center: [52.5, -2.0],
    layers: [bg.shown],//, lyr.shown],//,wms.shown,markers.shown],
    zoomControl: false
}) ;
//map.createPane('lyrPane');
//map.getPane('lyrPane').style.zIndex = 300;

bg.initialise();
mrk.initialise();
