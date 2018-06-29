//************** Variables pour la carte ******************************
var map;
var currentProdHR;
var barburl;
var barbOverlay;
var conturl;
var contOverlay;
var drawLayer;
var maxF = 10;
var lat = 38.5;
var lon = -87;
var fzoom = 8;
var barbs = "10wbrb";
var cont = "mslpct";
var prod = "2MTMP";
var model = "nowcast";
var ffs = "01";
var rstamp = new Date(new Date().toUTCString());
var error = false;
var currentcoord;
var modelrun;

// Lire les paramètres sauvegardés dans les cookies
var windVisible = (Cookies.get("WindVisibility") == undefined ? "false" : Cookies.get("WindVisibility"));
var isobarVisible = (Cookies.get("IsobarsVisibility") == undefined ? "false" : Cookies.get("IsobarsVisibility"));	
var productVisibility = (Cookies.get("ProductVisibility") == undefined ? "true" : Cookies.get("ProductVisibility"));


// ****************** Variables pour les controles *********************
var selectedElement;
var source = new ol.source.Vector();
var shapeStyle = new ol.style.Style({
		fill : new ol.style.Fill({
			color : "rgba(255, 0, 0, 0.05)"
		}),
		stroke : new ol.style.Stroke({
			color : "rgba(255, 0, 0, 1)",
			width : 2
		}),
		image : new ol.style.Circle({
			radius : 3,
			fill : new ol.style.Fill({
				color : "rgba(255, 0, 0, 1)"
				})
		}),
		dash: undefined,
	});

// Executer quand le document est tout chargé.
$(function() {

	// Lire les données du serveur
	$.ajax(
		{
		   method : "GET",
		   url : "ajax/get-data.php",
		   dataType : "json",
		   data : { model : model},
		   success : AjaxDone,
		   error : AjaxFail,
		   async : true
		}); 
   
	
   	$.contextMenu({
   		selector: '#map',
   		callback: function(key,options){
   			
   		},
   		items:{
   			"tephi": {
   				name: "Get sounding at this location", 
   				callback: function(key,opt) {
					var init = modelrun.split("_")[1];
					var coord = $('#coordinates').html();
					$.ajax({
						type: "GET",
						url: "ajax/get-skew.php",
						data:{
							'model':model,
							'lat':currentcoord[1],
							'lon':currentcoord[0],
							'ffs':ffs,
							'init':init,
							},	
						success: displayImage,
						error: function(data)
						{
							alert(data);
						}
					});
   					
   				},
   			},
   			"sep1": "---------",
   			"editfeature": {
   				name : "Edit feature colors...", 
   				disabled: function(key,opt){ return (selectedElement != undefined ? false : true);},
   				callback: function(key, opt){
   					changeFeatureStyle();	
   				},
   			},
   			"deletefeature": {
   				name : "Delete feature", 
   				disabled: function(key,opt){ return (selectedElement != undefined ? false : true);},
   				callback: function(key,opt){
   					DeleteFeature();
   				},
   			},
   			"sep2": "----------",
   			"clearfeature" : {
   				name: "Clear all shapes",
   				callback: function(key,opt){
					$('<div></div>').appendTo('body')
					    .html('<div><h6>Are you sure you want to delete all shapes ?</h6></div>')
					    .dialog({
					        modal: true,
					        title: 'Confirmation',
					        zIndex: 10000,
					        autoOpen: true,
					        width: 'auto',
					        resizable: false,
					        buttons: {
					            Yes: function () {
				   					if(interaction != undefined)
				   					{
				   						map.removeInteraction(interaction);
				   					}
				   					source.clear();

				   					changeInteraction();

					                $(this).dialog("close");
					            },
					            No: function () {
					                $(this).dialog("close");
					            }
					        },
					        close: function (event, ui) {
					            $(this).remove();
					        }
					    });
   				},
   				disabled: function(key,opt){
   					if(source.getFeatures().length > 0)
   					{
   						return false;
   					}
   					else
   					{
   						return true;
   					}
   				},
   			},
    	}
   	});
   	
		var selectSingleClick = new ol.interaction.Select();
	// Initialiser l'héritage des boutons de control de la carte.
	ol.inherits(SaveImageToFile, ol.control.Control);
	ol.inherits(DrawingToolbar, ol.control.Control);


	// extents de la carte
//	var e_minx = -14089280;// coté gauche 
//	var e_miny = 2783677;  // bas
//	var e_maxx = -7312353;// coté droit
//	var e_maxy = 6715818; // haut

	// Si le niveau de zoom est disponible  le lire
	
	fzoom = (Cookies.get("zoom") != undefined ? Cookies.get("zoom") : fzoom);
	
	// si la longitude et la latitude est disponible la lire
	if(Cookies.get("lat") != undefined && Cookies.get("lon") != undefined)
	{
		lat = Cookies.get("lat");
		lon = Cookies.get("lon");
	}

	// si le produit est disponible le lire
	prod = (Cookies.get("Product") != undefined ? Cookies.get("Product") : prod);
		
	// si le frame est disponible le lire
	//ffs = (Cookies.get("ffs") != undefined ? Cookies.get("ffs") : ffs);

	// si les barbs sont disponible le lire
	barbs = (Cookies.get("barbs") != undefined ? Cookies.get("Winds") : barbs);

	// Lire l'heure actuelle et enlever 1hre pour le dernier modèle.	
	//rstamp.setHours(rstamp.getHours() -1);
	
	/****************************************************************************************/
    	
	modelrun = model + "_" + FormatRunDateTime(rstamp);
	//modelrun = model + "_18061614";
	
	/*************************************** Initialiser la map *****************************************/
	
	var mousePosition = new ol.control.MousePosition({
		coordinateFormat: ol.coordinate.createStringXY(4),
		projection: 'EPSG:4326',
		target: document.getElementById("coordinates"),
		undefinedHTML : '------',
		className:'coordinates'
	});

	var map_controls = 
	[
		new ol.control.ScaleLine(),
		new ol.control.Zoom(),
		new ol.control.LayerSwitcher(),
		new SaveImageToFile(),
		new DrawingToolbar(),
		mousePosition,
    ];

    map = new ol.Map(
    {
    	target: "map",
		renderer : 'canvas',
		loadTilesWhileAnimating: true,
		loadTilesWhileInteracting : true,
	    view: new ol.View(
    		{
    			center: ol.proj.fromLonLat([-75.5255,45.498]),
				zoom:8,
				maxZoom:10,
				minZoom:8,
				projection:"EPSG:3857",
  		//		extent:[e_minx,e_miny,e_maxx,e_maxy],
  				loadTilesWhileInteracting:true,
  				loadTilesWhileAnimating:true,
				enableRotation: false,
    		}),
    	layers: [
    		new ol.layer.Group(
    		{
    			title: "Base maps",
    			layers:[
			  		// Layer Blanc sans villes
			    	new ol.layer.Tile(
			  		{
			  			source : new ol.source.XYZ (
			  				{
			  					attribution: '&copy; <a href="https://www.quebecvortex.com</a>',
			  					url:"http://korona.geog.uni-heidelberg.de/tiles/asterh/x={x}&y={y}&z={z}",
			  				}),
						preload: Infinity,
						type: "base",
						zIndex:8,
						title: "White with no labels",
						visible: true
			  		}),
			  		// Layer Blanc avec villes
			  		new ol.layer.Tile(
			  		{
			  			source : new ol.source.XYZ (
			  				{
			  					attribution: '&copy; <a href="https://www.quebecvortex.com</a>',
			  					url:"http://korona.geog.uni-heidelberg.de/tiles/asterh/x={x}&y={y}&z={z}",
			  				}),
						preload: Infinity,
						type: "base",
						zIndex:7,
						title: "White with labels",
						visible: false,
				
			  		}),
    			]
    		})
    	],
    	controls: map_controls,		
    });
	
	map.on("pointermove", function(e) {
		currentcoord = ol.proj.transform(e.coordinate, "EPSG:3857",'EPSG:4326');
	});

    /******************************************** LAYER ADMIN ***********************************/	
		
	admin = new ol.layer.Tile(
		{
			title: "Administrative",
			source : new ol.source.XYZ(
				{
					url: "https://korona.geog.uni-heidelberg.de/tiles/adminb/x={x}&y={y}&z={z}",
					crossOrigin: "Anonymous",
					maxZoom:14,
				}),
			opacity:1.0,
			zIndex:6,
			//preload:Infinity,
			visible: true,
			//updateWhileAnimating: true, // optional, for instant visual feedback
			//updateWhileInteracting: true, // optional, for instant visual feedback

		});		

   /******************************************** LAYER DES ROADS ***********************************/

	roads = new ol.layer.Tile(
		{
			title: "Roads",
			source : new ol.source.XYZ(
				{
					url: "http://korona.geog.uni-heidelberg.de/tiles/hybrid/x={x}&y={y}&z={z}",
					crossOrigin: "Anonymous",
					maxZoom:14,
				}),
			opacity:1.0,
			zIndex:5,
			//preload:Infinity,
			visible: true,
			//updateWhileAnimating: true, // optional, for instant visual feedback
			//updateWhileInteracting: true, // optional, for instant visual feedback

		});				
	
	/******************************************** LAYER DE DESSIN ***********************************/

	drawLayer = new ol.layer.Vector({
		source : source,
		style : shapeStyle,
		visible: true,
		wrapX: false,
		updateWhileAnimating: true, // optional, for instant visual feedback
  		updateWhileInteracting: true // optional, for instant visual feedback
	});
	
	/******************************************** EVENEMENTS DE LA MAP ****************************/
	
	map.getView().on('change:resolution', function(e) {
        document.getElementById('zoomlvllabel').innerHTML = "Zoom : " + map.getView().getZoom();
		Cookies.set("zoom",map.getView().getZoom(), { expires : 9999});
    });
		
	map.getViewport().addEventListener("contextmenu",function(e){	
		e.preventDefault();
		
		//var coord = ol.proj.transform(e.coordinate, "EPSG:3857", "EPSG:4326");
		//var stringifyFunc = ol.coordinate.createStringXY(4);
		//var lonlat = stringifyFunc(coord);

		e.return = false;
	});
   
   	/******************************************** GROUP DE LAYERS CACHABLEs **************************/

	var baselayers = new ol.layer.Group(
		{
			title: "Base Layers",
			layers: [drawLayer,admin,roads]
		});	

	map.addLayer(baselayers);
   
});
	

function AjaxDone(data)
{
	// Lire les donnees reçu du serveur.
	var listmodels = data;

	/********************************************** LAYER DU PRODUIT *************************************/
	
	currentProdHR = new ol.layer.Tile(
		{
			title: "Product",
			source : new ol.source.XYZ(
				{
					url: "http://nowcast.quebecvortex.com/couches/"+model+"/"+modelrun+"/"+ffs+"/"+prod+"/{z}/{x}/{y}.png",
					crossOrigin: "Anonymous",
					maxZoom:14,
				}),
			preload:Infinity,
			zIndex:0,
			opacity:0.8,
			useInterimTilesOnError: true,
			visible: (productVisibility == "true" ? true: false) ,
			updateWhileAnimating: true, // optional, for instant visual feedback
			updateWhileInteracting: true // optional, for instant visual feedback
		});
	
		currentProdHR.on("change:visible", function(e){
		Cookies.set("ProductVisibility",currentProdHR.getVisible(), {expires:9999});
	});

	/********************************* LAYER DES VENTS ********************************************/
		
	barbOverlay = new ol.layer.Tile(
		{
			title: "Wind barbs",
			source : new ol.source.XYZ(
				{
					url: "http://nowcast.quebecvortex.com/couches/"+model+"/"+modelrun+"/"+ffs+"/"+barbs+"/{z}/{x}/{y}.png",
					crossOrigin: "Anonymous",
					maxZoom:14,
				}),
			opacity:0.5,
			//preload:Infinity,
			useInterimTilesOnError: true,
			visible: (windVisible == "true" ? true : false),
			updateWhileAnimating: true, // optional, for instant visual feedback
			updateWhileInteracting: true // optional, for instant visual feedback
		});	
	
		barbOverlay.on("change:visible", function(e) {
		Cookies.set("WindVisibility",barbOverlay.getVisible(), {expires:9999});
	});

	/***************************** LAYER ISOBARRES **************************************************/
	
	contOverlay = new ol.layer.Tile(
		{
			title: "Isobars",
			source : new ol.source.XYZ(
				{
					url: "http://nowcast.quebecvortex.com/couches/"+model+"/"+modelrun+"/"+ffs+"/"+cont+"/{z}/{x}/{y}.png",
					//url: "https://www.hiresweather.com/mapcache/gmaps/mslp@GoogleMapsCompatible/{z}/{x}/{y}.png",
					crossOrigin: "Anonymous",
					maxZoom:14,
					
				}),
			opacity:0.5,
			//preload:Infinity,
			visible: (isobarVisible == "true" ? true : false),

			//updateWhileAnimating: true, // optional, for instant visual feedback
			//updateWhileInteracting: true, // optional, for instant visual feedback

		});	
	
		contOverlay.on("change:visible", function(e){
		Cookies.set("IsobarsVisibility", contOverlay.getVisible(), {expires : 9999});
	});

	var overlay = new ol.layer.Group(
		{
			title: "Overlays",
			layers: [currentProdHR,barbOverlay,contOverlay]
		}
	
	);
	
	map.addLayer(overlay);

  	/******************************************** INITIALISATION DES ELEMENTS DE LA CARTE ****************/
	
	BuildModelMenu(listmodels);
	
	// Construire le player
	//if(valprod != null) BuildFrameSelector(valf[modelrun]);

	$("#zoomlvllavel").html("Zoom : " + fzoom);
	$("#currprod").html("Product : " +  prod);
	
	DetectAdblocker();
	changeInteraction();
    setSlider($("#" + modelrun).attr('data-hours').split(','),ffs);
}

function AjaxFail(request,errortype,excpt)
{
	// Si il y a une erreur vider la carte et afficher un message d'erreur
	document.getElementById("main").innerHTML = "Sorry but an error occurend -> " + errortype;
}


function DetectAdblocker()
{

	if(window.canRunAds == undefined)
	{
		var blockmap = document.createElement("div");
		blockmap.id = "mapblocker";
		blockmap.innerHTML = "Under the terms of this website you are required to disable adblock in order to use the app freely. Please disable Adblock and refresh the page.";
		document.body.appendChild(blockmap);
		$("#mapblocker").dialog({
			modal:true,
			title:"Error",
			draggable:false,
			resizable:false,
			closeOnEscape:false,
			dialogClass:"no-select",
			open: function(e,u) 
			{
				$(this).parent().children().children('.ui-dialog-titlebar-close').hide();
			}
		});
		
	}
	else
	{	
		$("#advertising").remove();
	}
}



