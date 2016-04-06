
var app = angular.module('tawquizz',[]);

app.controller('MainController', function($scope, $http) {
    
}

var d = "";

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

var loadQuestions = function(data){
    var json = JSON.parse(data);
    var ul = document.getElementById("questions");
    console.log(JSON.stringify(json));
    for(var i=0; i < json.length; i++){
	console.log(JSON.stringify(json[i]));
	var li = document.createElement("LI");
	var tx = document.createTextNode(json[i].quText);
	var a = document.createElement("a");
	a.appendChild(tx);
	a.href="http://localhost:3000/edit/" + json[i].quId;
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
	    console.log("Enters here");
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
    var a = new Array();
    var c = new Array();

    var q = $('#question').val();
    var expl = $("#expl").val();

    a[0] = $('#ans1').val();
    a[1] = $('#ans2').val();
    a[2] = $('#ans3').val();
    a[3] = $('#ans4').val();
    a[4] = $('#ans5').val();
    a[5] = $('#ans6').val();

    c[0] = $('#cor1').is(':checked');
    c[1] = $('#cor2').is(':checked');
    c[2] = $('#cor3').is(':checked');
    c[3] = $('#cor4').is(':checked');
    c[4] = $('#cor5').is(':checked');
    c[5] = $('#cor6').is(':checked');

    if (q !== "" && a[0] !== "" ){
	      var formData =  {
            cqUnit : unit,
            cqText : q,
            cqExpl : expl,
            cqAns1 : a[0],
            cqCor1 : c[0],
            cqAns2 : a[1],
            cqCor2 : c[1],
            cqAns3 : a[2],
            cqCor3 : c[2],
            cqAns4 : a[3],
            cqCor4 : c[3],
            cqAns5 : a[4],
            cqCor5 : c[4],
            cqAns6 : a[5],
            cqCor6 : c[5]
	      };

	      console.log(JSON.stringify(formData));

	      $.ajax({
	          method: "POST",
	          url: "http://localhost:3000/save-question",
	          dataType: "json",
	          data: JSON.stringify(formData),
	          success: function(msg) {
		            alert("Data Saved: " + msg);
                window.location = "/unit/" + unit;
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

var loadQuestion = function(unit, question) {
    if (question === 0) {
	callBackend('/get-first-question/' + unit, questionLoaded);
    } else {
	callBackend('/get-question/' + unit + '/' + question, questionLoaded);
    }
}

var questionLoaded = function(data){
    var d = JSON.parse(data);
    var tx = document.createTextNode(d.quText);
    var question = document.getElementById("question");
    question.appendChild(tx);
    callBackend('/get-answers/' + d.quId, loadOptions);
    console.log('Gets here...');
}

var loadOptions = function(data) {
    var op = JSON.parse(data);
    
}
