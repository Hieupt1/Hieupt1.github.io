
var Doan_van = document.getElementsByClassName("doan_van");

//Bài 1:


function changeFontSize(x) { 
    Doan_van[0].style.fontSize = x +"px";
    Doan_van[1].style.fontSize = x +"px";
    Doan_van[2].style.fontSize = x +"px";

}

//Bài 2:

function increaseFontSize(y) {
    var Size = Doan_van[0].style.fontSize;
    Size = (Number(Size.slice(0, Size.length - 2)) + y) + "px";
    Doan_van[0].style.fontSize = Size ;
    Doan_van[1].style.fontSize = Size ;
    Doan_van[2].style.fontSize = Size ;
}

//Bài 3:

function decreaseFontSize(z) {
    var Size = Doan_van[0].style.fontSize;
    Size = (Number(Size.slice(0, Size.length - 2)) - z) + "px";
    Doan_van[0].style.fontSize = Size ;
    Doan_van[1].style.fontSize = Size ;
    Doan_van[2].style.fontSize = Size ;
}

//Bài 4:

function changeColor() {
    Doan_van[0].style.color = "blue";
    Doan_van[1].style.color = "yellow";
    Doan_van[2].style.color = "red";
}

//Bài 5:

function changeBgColor() {
    Doan_van[0].style.backgroundColor = "green";
    Doan_van[1].style.backgroundColor = "green";
    Doan_van[2].style.backgroundColor = "green";
}

//Bài 6:

function copyContent() {
    Doan_van[0].innerHTML = Doan_van[1].innerHTML;
}

