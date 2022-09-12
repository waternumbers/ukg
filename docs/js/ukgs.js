/* --------------------------------------------------------*/
/* Code to initialise the map */
const params = new URLSearchParams(window.location.search);
const mid = params.get("mid");
console.log(mid);

var map = L.map('map',{
    zoom: 7,
    maxZoom: 16,
    center: [52.5, -2.0],
    layers: new L.tileLayer("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
                            {"maxZoom": 17,
                             "attribution": "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors, &copy; <a href='https://cartodb.com/attributions'>CartoDB</a>"
                            }),//, lyr.shown],//,wms.shown,markers.shown],
    zoomControl: false
}) ;

const isOk = response => response.ok ? response.json() : Promise.reject(new Error('Failed to load data from server'))
const url = "https://flood.waternumbers.com/series/"+mid;

//setInterval(()=>{
const handlefetch =() =>{
    fetch(url)
	.then(isOk) // <= Use `isOk` function here
	.then(function (feature) {
	    // add to map
	    L.geoJson(feature).addTo(map);
	    
	    // populate div
	    if( feature.properties.town ){
		document.getElementById("seriesName").innerHTML=feature.properties.town;
	    }else{
		switch (feature.properties.type) {
		case "stage": document.getElementById("seriesName").innerHTML='River Gauge';
		case "tidal": document.getElementById("seriesName").innerHTML='Tidal Gauge';
		case "tbr": document.getElementById("seriesName").innerHTML='Rain Gauge';
		};
	    }

	pstr = '<p><b>River</b>: ' + feature.properties.featureName + '</p>';
	pstr = pstr + '<p><b>Last Observation</b>: ' + feature.properties.lastObs + '</p>';
	pstr = pstr + '<p><b>Last Forecast Issued</b>: ' + feature.properties.lastObs + '</p>';
	
	pstr = pstr + '<p><b>Last Warning</b>: ' + feature.properties.lastWarning + '</p>';
	pstr = pstr + '<p><b>Last Forecast Warning</b>: ' + feature.properties.lastForecastWarning + '</p>';
	pstr = pstr + '<p><b>Series</b>: ' + feature.properties.mid + '</p>';
	document.getElementById("seriesInfo").innerHTML=pstr;

	var plotData =[];
	    for (let i = 0; i < feature.properties.data.length; i++) {
	    
            plotData.push([new Date(feature.properties.data[i].dateTime),
			   [null,feature.properties.data[i].obs,null],
			   [feature.properties.data[i].fcst_lower,
			    feature.properties.data[i].fcst,
			    feature.properties.data[i].fcst_upper],
			   [null,feature.properties.typicalHigh,null]
			  ])
			   // [feature.properties.data[i].fcst_lower,
			   //  feature.properties.data[i].fcst,
			   //  feature.properties.data[i].fcst_higher]])
	}
	    //console.log(feature.properties.data);
	    //console.log(plotData)
	g = new Dygraph(
            document.getElementById("div_g"),
            plotData, {
		customBars: true,
		labels: ['Time', 'Observed', 'Forecast',"Possible Flooding"]
            }
	);


	})
	.catch(error => console.error("station loading error " + error))
};
handlefetch();
setInterval(()=>handlefetch(),60000);
	    

