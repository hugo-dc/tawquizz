var goto = function(page)
{
    window.location = page;
}

var createUnit = function()
{
    var unit = document.getElementById("unit");
    if(unit.value === ""){
	console.log("Empty");
	document.getElementById('error').innerHTML = "Please provide a Unit name";
	return;
    }

    window.location = "/create-unit/" + unit.value;
}

var loadUnits = function(data)
{
    var json = JSON.parse(data);
    var ul = document.getElementById("units"); 
    console.log(JSON.stringify(json));
    for(i=0;i < json.length; i++){
        console.log(JSON.stringify(json[i]));
    }
}

var callBackend = function(url, fun)
{
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function()
    {
	if(xhttp.readyState == 4 && xhttp.status == 200) {
	    console.log("ENters here");
	    console.log(xhttp);
	    fun(xhttp.response);
	}
    }
    xhttp.open("GET", url, true);
    xhttp.send();
}


