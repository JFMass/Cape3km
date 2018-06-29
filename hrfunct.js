function BuildModelMenu(listmodel)
{
	var modelsUL = document.getElementById("modelsmenu");
	var modelsubmenu;
	
	for (var model in listmodel)
	{
		modelsubmenu = document.createElement('li');		
		var amodelname = document.createElement('a');
		amodelname.innerHTML = model;
		modelsubmenu.appendChild(amodelname);
		var modelsubdays = document.createElement('ul');
		var obj = listmodel[model];
		$.each(obj,function(index,value)
		{
			var listitem = document.createElement('li');
			var alink = document.createElement('a');
			
			alink.setAttribute('href','#');
			alink.setAttribute("data-hours",value.join());
			alink.id = index;
			addclickhandler(alink,index);
			alink.innerHTML = index;
			listitem.appendChild(alink);

			modelsubdays.appendChild(listitem);	
		});
		
		modelsubmenu.appendChild(modelsubdays);
		modelsUL.appendChild(modelsubmenu);	
	}
						
	$('#main-menu').smartmenus();
}

function addclickhandler(link, i)
{
    link.addEventListener('click', function(e){
        ChangeModel(i);
    }, false);
}

function setSlider(hours,value)
{
	var lasthour = hours.length -1;
    var actualval = (value == undefined ? 1 : value);
 
	$("#slider").slider(
		{
			step:1,
			min:1,
			max:lasthour,
			value:actualval,
			change: function(event,ui)
			{
				ChangeF(ui.value);
			},
		}).each(function() {
			
			$("#slider").find("label").remove();

			
			// Position the labels
			for (var i = 0; i <= lasthour; i++) {
		
				// Create a new element and position it with percentages
				var el = $('<label>' + (i + 0) + '</label>').css('left', (i/lasthour*100) + '%');
		
				// Add the element inside #slider
				$("#slider").append(el);
		
			}
	
		});	
	
}

function ChangeModel(newrun)
{
	modelrun = newrun;
	ffs = 01;
	ChangeF(1);
	var newurl = "http://nowcast.quebecvortex.com/couches/"+ model + "/" + newrun + "/" + ffs + "/" + prod + "/{z}/{x}/{y}.png";
	var source = new ol.source.XYZ({ url:newurl, crossOrigin:"anonymous"});	 
	currentProdHR.setSource(source);
	
	var hours = $('#'+modelrun).attr('data-hours').split(',');
	
	setSlider(hours,hours[hours.length-1]);
	Cookies.set("model",newrun,{expires:9999});
}

function ChangeBarb(newbarb)
{
	barbs = newbarb;
	var newurl = "http://nowcast.quebecvortex.com/couches/"+model+"/"+modelrun+"/"+ffs+"/"+newbarb+"/{z}/{x}/{y}.png"
	var source = new ol.source.XYZ({ url:newurl, crossOrigin:"anonymous"});	 
	barbOverlay.setSource(source);
	Cookies.set("barbs", newbarb, {expires:9999});
}

function ChangeCont(newcont)
{
	cont = newcont;
	var newurl = "http://nowcast.quebecvortex.com/couches/"+model+"/"+modelrun+"/"+ffs+"/"+newcont+"/{z}/{x}/{y}.png"
	var source = new ol.source.XYZ({ url:newurl, crossOrigin:"anonymous"});	 
	contOverlay.setSource(source);
	Cookies.set("conts", newcont, {expires:9999});
}


function ChangeProd(title,newprod){
	prod = newprod;
	var newurl = "http://nowcast.quebecvortex.com/couches/"+ model + "/" + modelrun + "/" + ffs + "/" + newprod + "/{z}/{x}/{y}.png";
	var source = new ol.source.XYZ({ url:newurl, crossOrigin:"anonymous"});	 
	currentProdHR.setSource(source);
	
	// Linking barboverlay so we get associated barb with new product
	var assobr = newprod+"br";
	if (newprod == "mlcape" || newprod == "sbcape") {
		assobr = "shear6br";
	}	
	else
	{
		assobr = "sfcwindbr";
	}

	ChangeBarb(assobr);
	
	if(newprod != undefined)
	{
    	document.getElementById("currprod").innerHTML = "Product : " + newprod ;
    	prod = newprod;
		Cookies.set("Product",newprod, {expires:9999});
	}
	
}
function ChangeF(newf)
{
	var newval;
	if(newf < 10)
	{
		newval = "0" + newf;		
	}
	else
	{
		newval = newf;	
	}
	
	ffs = newval;
	var newurl = "http://nowcast.quebecvortex.com/couches/"+ model + "/" + modelrun + "/" + ffs + "/" + prod + "/{z}/{x}/{y}.png";
	var source = new ol.source.XYZ({ url:newurl, crossOrigin:"anonymous"});	 
	currentProdHR.setSource(source);
	ChangeBarb(barbs);
	ChangeCont(cont);
	Cookies.set("ffs",ffs, {expires :9999});	
}

function FormatRunDateTime(runtime)
{	

	var month = runtime.getMonth() + 1;
	var hour = runtime.getUTCHours();
	var day = runtime.getDate();
	
	var result;
	
	result = runtime.getFullYear().toString();
	
	if(hour < 10)
	{
		hour = "0" + hour;
	}
	if(month < 10)
	{
		month = "0" + month;
	}
	if(day < 10)
	{ 
	    day = "0" + day;
	}
	result = result + month + day + hour;
	
	return result;
}

