var goto = function(page){
    window.location = page;
}

var createUnit = function(){
    var unit = document.getElementById("unit");
    if(unit.value === ""){
	console.log("Empty");
	document.getElementById('error').innerHTML = "Please provide a Unit name";
	return;
    }
    window.location = "/create-unit/" + unit.value;
}

var loadUnits = function(data){
    var json = JSON.parse(data);
    var ul = document.getElementById("units"); 
    console.log(JSON.stringify(json));
    for(i=0;i < json.length; i++){
        console.log(JSON.stringify(json[i]));
	var li = document.createElement("LI");
	var tx = document.createTextNode(json[i].unName);
	var a = document.createElement("a");
	a.appendChild(tx);
	a.href = "http://localhost:3000/unit/" + json[i].unId;
	li.appendChild(a);
	ul.appendChild(li);
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

var getUnitId = function(url)
{
    console.log(url);
    return 1;
}

// Create Answers
var createAnswers = function()
{
}

// Create Question and Answers in DataBase
var createQuestion = function(unit)
{
    
    var q = $('#question').val();
    var a = new Array();
    var text = $("#expl").val();
    a[0] = $('#ans1').val();
    a[1] = $('#ans2').val();
    a[2] = $('#ans3').val();
    a[3] = $('#ans4').val();
    a[4] = $('#ans5').val();
    a[5] = $('#ans6').val();
    a[6] = $('#ans7').val();

    if (q !== "" &&
	a[0] !== "" )
    {
	console.log(q);
	console.log(a[0]);
	console.log(text);
	console.log(unit);
	console.log("Done");

/*
	$.post("../save-question",
	       {
		   cqUnit : unit,
		   cqText : text,
		   cqExpl : expl
	       },
	       function(data, status)
	       {
		   console.log(data);
		   console.log(status);
	       });
*/
	var formData =  {
		code: "main = do",
		lang: "haskell"
	};

	console.log(JSON.stringify(formData));
	
	$.ajax({
	    method: "POST",
	    url: "http://localhost:3000/test-me",
	    dataType: "json",
	    data: JSON.stringify(formData),
	    success: function(msg) {
		alert("Data Saved: " + msg);
	    },

	    contentType: "application/json"
	});
    }
}

var checkNext = function(n)
{
    var id = '#ans' + n;
    var val = $(id)[0].value;
    var fansid = "#fans" + (n+1);    
    if(val !== ""){
	$(fansid).show(1000);
	$(fansid + "c").show(1000);
    }else{
	var nextId = "#ans" + ( n + 1);
	if( $(nextId).val() !== ""){
	    $(id)[0].value = $(nextId).val();
	    $(nextId)[0].value = "";
	}else{
	    $(fansid).hide(1000);
	    $(fansid + "c").hide(1000);
	}
    }
}
