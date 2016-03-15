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

var callBackend = function(url, fun)
{
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function()
    {
	if(xhttp.readyState == 4 && xhttp.status == 200) {
	    fun(xhttp.responseJSON);
	}
    }
    xhttp.open("GET", url, true);
    xhttp.send();
}

var loadUnits = function(data)
{
    var ul = document.getElementById("units"); 
    for(i=0;i < data.length; i++){
        console.log(JSON.stringify(data[i]));
    }
}

